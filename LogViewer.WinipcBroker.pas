{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Uses a inproc ZMQ publisher. }

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes,

  Spring, Spring.Collections,

  ZeroMQ,

  LogViewer.Interfaces;

type
  TAddSubscriberEvent = procedure(
    Sender             : TObject;
    const AEndpoint    : string;
    AProcessId         : UInt32;
    const AProcessName : string
  ) of object;

type
  TWinipcBroker = class(TThread)
  private
    // Contains the window class attributes that are registered by the
    // RegisterClass function.
    FWndClass        : WNDCLASS;
    // window handle
    FWnd             : HWND;

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
      const AEndpoint    : string;
      AProcessId         : UInt32;
      const AProcessName : string
    );

  public
    constructor Create(const AZeroMQ: IZeroMQ);
    destructor Destroy; override;

    property OnAddSubscriber: TAddSubscriberEvent
      read FOnAddSubscriber write FOnAddSubscriber;
  end;

const
// old names are maintained for backwards compatibility
  MSG_WND_CLASSNAME : PChar = 'FPCMsgWindowCls'; // window class name
  SERVER_WINDOWNAME : PChar = 'ipc_log_server';  // window instance name

resourcestring
  SFailedToRegisterWindowClass = 'Failed to register message window class';
  SFailedToCreateWindow        = 'Failed to create message window %s';

implementation

uses
  System.SysUtils,
  Vcl.Forms,

  DDuce.Logger, DDuce.Utils.Winapi,

  LogViewer.Subscribers.Winipc;

{$REGION 'non-interfaced routines'}
{ This is the window procedure or 'window proc'. It defines most of the behavior
  of the associated window. }

function ThreadWindowProc(AHWnd: HWND; AMsg: UINT; AWParam: WPARAM;
  ALParam: LPARAM): LRESULT; stdcall;
var
  LBroker  : TWinipcBroker;
  LMessage : TMessage;
begin
  LBroker := TWinipcBroker(GetWindowLongPtr(AHWnd, GWL_USERDATA));
  if LBroker <> nil then
  begin
    LMessage.Msg    := AMsg;
    LMessage.WParam := AWParam;
    LMessage.LParam := ALParam;
    LMessage.Result := 0;
    LBroker.WndProc(LMessage);
    Result := LMessage.Result;
  end
  else
    Result := DefWindowProc(AHWnd, AMsg, AWParam, ALParam);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TWinipcBroker.Create(const AZeroMQ: IZeroMQ);
begin
  Logger.Track(Self, 'Create');
  inherited Create(True);
  FMsgData := TBytesStream.Create;
  FZmq     := AZeroMQ;
  FBuffer  := TStringStream.Create('', TEncoding.ANSI);

  { register the window class by filling in the WNDCLASS structure }
  // initialize the WNDCLASS structure
  FillChar(FWndClass, SizeOf(FWndClass), 0);
  // a pointer to the window procedure
  FWndClass.lpfnWndProc   := @ThreadWindowProc;
  // the handle to the application instance
  FWndClass.hInstance     := HInstance;
  // string that identifies the window class
  FWndClass.lpszClassName := MSG_WND_CLASSNAME;
end;

destructor TWinipcBroker.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FPublishers := nil;
  FZmq        := nil;
  FreeAndNil(FMsgData);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TWinipcBroker.WndProc(var AMessage: TMessage);
var
  CDS                : PCopyDataStruct;
  LEndPoint          : string;
  LPublisher         : IZMQPair;
  LClientProcessId   : Integer;
  LClientProcessName : string;
begin
  if AMessage.Msg = WM_COPYDATA then
  begin
    CDS := PCopyDataStruct(AMessage.LParam);
    FMsgData.Clear;
    FMsgData.Seek(0, soFrombeginning);
    FMsgData.WriteBuffer(CDS^.lpData^, CDS^.cbData);
    LClientProcessId := CDS.dwData;
    if LClientProcessId = 0 then
    begin
      LClientProcessId := GetWindowThreadProcessId(AMessage.WParam);
    end;
    LEndPoint := Format('inproc://%d', [LClientProcessId]);
    if not FPublishers.TryGetValue(LEndPoint, LPublisher) then
    begin
      LPublisher := FZmq.Target.Start(ZMQSocket.Publisher);
      Guard.CheckTrue(LPublisher.Bind(LEndPoint) = 0, 'Bind failed');
      FPublishers.AddOrSetValue(LEndPoint, LPublisher);
      Queue(
        procedure
        begin
          LClientProcessName := GetExenameForProcess(LClientProcessId);
          DoAddSubscriber(LEndPoint, LClientProcessId, LClientProcessName);
        end
      );
    end;
    FBuffer.Clear;
    FBuffer.LoadFromStream(FMsgData);
    LPublisher.SendString(FBuffer.DataString);
  end
  else
   AMessage.Result :=
    DefWindowProc(FWnd, AMessage.Msg, AMessage.WParam, AMessage.LParam);
end;
{$ENDREGION}

{$REGION 'protected methods'}
{$REGION 'TThread overrides'}
procedure TWinipcBroker.Execute;
var
  LMsg       : TMsg;
  LPublisher : IZMQPair;
begin
  NameThreadForDebugging('WinipcBroker');
  try
    FPublishers := TCollections.CreateDictionary<string, IZMQPair>;
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
    while not Terminated and GetMessage(LMsg, 0, 0, 0) do
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
  AProcessId: UInt32; const AProcessName: string);
begin
  Logger.Track(Self, 'DoAddSubscriber');
  if Assigned(OnAddSubscriber) then
    OnAddSubscriber(Self, AEndpoint, AProcessId, AProcessName);
end;

procedure TWinipcBroker.DoTerminate;
begin
  if FWnd <> 0 then
    DestroyWindow(FWnd);
  Winapi.Windows.UnregisterClass(FWndClass.lpszClassName, FWndClass.hInstance);
  inherited DoTerminate;
end;
{$ENDREGION}
{$ENDREGION}

end.
