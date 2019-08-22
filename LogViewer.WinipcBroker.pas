{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit LogViewer.WinipcBroker;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes,

  Spring, Spring.Collections,

  ZeroMQ,

  LogViewer.Interfaces;

type
  TAddSubscriberEvent = procedure(
    Sender          : TObject;
    const AEndpoint : string;
    ASourceId       : UInt32
  ) of object;

type
 TWinipcBroker = class(TThread)
  private
    FWnd             : HWND;
    FWndClass        : WNDCLASS;
    FMsgData         : TBytesStream;
    FBuffer          : TStringStream;
    FZmq             : Weak<IZeroMQ>;
    FPublishers      : IDictionary<string, IZMQPair>;
    FOnAddSubscriber : TAddSubscriberEvent;

    procedure WndProc(var AMessage: TMessage);

  protected
    {$REGION 'TThread overrides'}
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure TerminatedSet; override;
    {$ENDREGION}

    procedure DoAddSubscriber(
      const AEndpoint : string;
      ASourceId       : UInt32
    );

  public
    constructor Create(const AZeroMQ: IZeroMQ);
    destructor Destroy; override;

    property OnAddSubscriber: TAddSubscriberEvent
      read FOnAddSubscriber write FOnAddSubscriber;
  end;

const
// old name maintained for backwards compatibility
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls';
  SERVER_WINDOWNAME : PChar = 'ipc_log_server';

resourcestring
  SFailedToRegisterWindowClass = 'Failed to register message window class';
  SFailedToCreateWindow        = 'Failed to create message window %s';

implementation

uses
  System.SysUtils,

  DDuce.Logger,

  LogViewer.Subscribers.Winipc;

{$REGION 'non-interfaced routines'}
function TWndThreadWindowProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  LBroker  : TWinipcBroker;
  LMessage : TMessage;
begin
  LBroker := TWinipcBroker(GetWindowLongPtr(hWnd, GWL_USERDATA));
  if LBroker <> nil then
  begin
    LMessage.Msg    := uMsg;
    LMessage.WParam := wParam;
    LMessage.LParam := lParam;
    LMessage.Result := 0;
    LBroker.WndProc(LMessage);
    Result := LMessage.Result;
  end
  else
    Result := DefWindowProc(hWnd, uMsg, wParam, lParam);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TWinipcBroker.Create(const AZeroMQ: IZeroMQ);
begin
  Logger.Track(Self, 'Create');
  inherited Create(True);
  FMsgData := TBytesStream.Create;
  FillChar(FWndClass, SizeOf(FWndClass), 0);
  FWndClass.lpfnWndProc   := @TWndThreadWindowProc;
  FWndClass.hInstance     := HInstance;
  FWndClass.lpszClassName := MSG_WND_CLASSNAME;
  FZmq                    := AZeroMQ;
  FBuffer                 := TStringStream.Create('', TEncoding.ANSI);
  Logger.Send('ThreadId', TThread.CurrentThread.ThreadID);
end;

destructor TWinipcBroker.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FPublishers  := nil;
  FZmq         := nil;
  FreeAndNil(FMsgData);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TWinipcBroker.WndProc(var AMessage: TMessage);
var
  LEndPoint        : string;
  LPublisher       : IZMQPair;
  CDS              : PCopyDataStruct;
  LClientProcessId : Integer;
begin
  if AMessage.Msg = WM_COPYDATA then
  begin
    CDS := PCopyDataStruct(AMessage.LParam);
    FMsgData.Clear;
    FMsgData.Seek(0, soFrombeginning);
    FMsgData.WriteBuffer(CDS^.lpData^, CDS^.cbData);
    LClientProcessId := CDS.dwData;
    LEndPoint := Format('inproc://%d', [LClientProcessId]);
    if not FPublishers.TryGetValue(LEndPoint, LPublisher) then
    begin
      LPublisher := FZmq.Target.Start(ZMQSocket.Publisher);
      Guard.CheckTrue(LPublisher.Bind(LEndPoint) = 0, 'Bind failed');
      FPublishers.AddOrSetValue(LEndPoint, LPublisher);
      Synchronize(
        procedure
        begin
          DoAddSubscriber(LEndPoint, LClientProcessId);
        end
      );
    end;
    FBuffer.Clear;
    FBuffer.LoadFromStream(FMsgData);
    LPublisher.SendString(FBuffer.DataString);
    Exit;
  end;
  AMessage.Result :=
    DefWindowProc(FWnd, AMessage.Msg, AMessage.WParam, AMessage.LParam);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TWinipcBroker.Execute;
var
  LMsg       : TMsg;
  LPublisher : IZMQPair;
begin
  NameThreadForDebugging('WinipcBroker');
  try
    FPublishers             := TCollections.CreateDictionary<string, IZMQPair>;
    if Winapi.Windows.RegisterClass(FWndClass) = 0 then
      Exit;
    FWnd := CreateWindow(
      FWndClass.lpszClassName,
      PChar(SERVER_WINDOWNAME),
      WS_DLGFRAME, 0, 0, 0, 0, 0, 0,
      HInstance,
      nil
    );
    if FWnd = 0 then
      Exit;
    SetWindowLongPtr(FWnd, GWL_USERDATA, ULONG_PTR(Self));

    while GetMessage(LMsg, 0, 0, 0) and (not Terminated) do
    begin
      TranslateMessage(LMsg);
      DispatchMessage(LMsg);
    end;
  finally
    for LPublisher in FPublishers.Values do
    begin
      LPublisher.Close;
    end;
    FPublishers := nil;
  end;
end;

procedure TWinipcBroker.TerminatedSet;
begin
  Logger.Track(Self, 'TerminatedSet');
  inherited TerminatedSet;
  PostThreadMessage(ThreadID, WM_NULL, 0, 0);
end;

procedure TWinipcBroker.DoAddSubscriber(const AEndpoint: string;
  ASourceId: UInt32);
begin
  Logger.Track(Self, 'DoAddSubscriber');
  if Assigned(OnAddSubscriber) then
    OnAddSubscriber(Self, AEndpoint, ASourceId);
end;

procedure TWinipcBroker.DoTerminate;
begin
  if FWnd <> 0 then
    DestroyWindow(FWnd);
  Winapi.Windows.UnregisterClass(FWndClass.lpszClassName, FWndClass.hInstance);
  inherited DoTerminate;
end;
{$ENDREGION}

end.
