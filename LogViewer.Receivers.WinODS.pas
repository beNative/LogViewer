{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Receivers.WinODS;

{ WinODS channel receiver. }

interface

{$REGION 'documentation'}
{ Receives messages posted by the OutputDebugString Windows API routine. The
  OutputDebugString messages are fetched in a thread and queued as TLogMessage
  compatible stream. }

{ TODO :  Notification when a ProcessId/ProcessName does not exist anymore }
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils,

  LogViewer.Interfaces,

  Spring, Spring.Collections,

  LogViewer.Receivers.Base, LogViewer.WinODS.Settings;

type
  TODSMessage = class
    Id          : UInt32;
    TimeStamp   : TDateTime;
    MsgText     : AnsiString; // ODS messages are always AnsiStrings.
    ProcessId   : Integer;
    ProcessName : UTF8String;
  end;

  { Thread instance that captures OutputDebugString content }

  TODSThread = class(TThread)
  private
    FLastChildOrder   : UInt32;
    FODSQueue         : IQueue<TODSMessage>;
    FCloseEventHandle : THandle;

  protected
    procedure Execute; override;

  public
    constructor Create(AODSQueue: IQueue<TODSMessage>);
  end;

type
  TWinODSChannelReceiver = class(TChannelReceiver, IChannelReceiver)
  private
    FBuffer           : TMemoryStream;
    FODSQueue         : IQueue<TODSMessage>;
    //FODSThread        : TODSThread;

    procedure FODSQueueChanged(
      Sender     : TObject;
      const Item : TODSMessage;
      Action     : TCollectionChangedAction
    );

  protected
    function GetSettings: TWinODSSettings;

    procedure SettingsChanged(Sender: TObject); override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Settings: TWinODSSettings
      read GetSettings;

  end;

implementation

uses
  Winapi.PsAPI, Winapi.TlHelp32,
  System.SyncObjs,
  Vcl.Dialogs,

  Spring.Helpers,

  DDuce.Logger.Interfaces, DDuce.Utils.WinApi,

  LogViewer.WinIPC.Settings;

{$REGION 'construction and destruction'}
procedure TWinODSChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FBuffer := TMemoryStream.Create;
  FODSQueue := TCollections.CreateQueue<TODSMessage>(True);
  FODSQueue.OnChanged.Add(FODSQueueChanged);
  //FODSThread := TODSThread.Create(FODSQueue);
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TWinODSChannelReceiver.BeforeDestruction;
begin
  //FODSThread.Terminate;
  FBuffer.Free;
  FODSQueue.Clear;
  //FODSThread.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWinODSChannelReceiver.GetSettings: TWinODSSettings;
begin
  Result := Manager.Settings.WinODSSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinODSChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
end;

procedure TWinODSChannelReceiver.FODSQueueChanged(Sender: TObject;
  const Item: TODSMessage; Action: TCollectionChangedAction);
const
  ZERO_BUF : Integer = 0;
var
  LTextSize : Integer;
  LMsgType  : Integer;
//  LDataSize : Integer;
begin
  if Enabled and (Action = caAdded) then
  begin
    FBuffer.Clear;
    LMsgType := Integer(lmtText);
    LTextSize := Length(Item.MsgText);
    FBuffer.Seek(0, soFromBeginning);
    FBuffer.WriteBuffer(LMsgType);
    FBuffer.WriteBuffer(Item.TimeStamp);
    FBuffer.WriteBuffer(LTextSize);
    FBuffer.WriteBuffer(Item.MsgText[1], LTextSize);
//      LDataSize := SizeOf(Item.ProcessInfo);
//      FBuffer.WriteBuffer(LDataSize, SizeOf(Integer));
//      FBuffer.WriteBuffer(Item.ProcessInfo, LDataSize);
    FBuffer.WriteBuffer(ZERO_BUF);
//      LTextSize := Length(Item.ProcessName);
//      FBuffer.WriteBuffer(Item.ProcessName[1], LTextSize);
      //ShowMessage(Item.ProcessInfo.ProcessName);
    DoReceiveMessage(FBuffer, Item.ProcessId, 0, string(Item.ProcessName));
  end;
end;
{$ENDREGION}

{$REGION 'TODSThread'}
{$REGION 'construction and destruction'}
constructor TODSThread.Create(AODSQueue: IQueue<TODSMessage>);
begin
  inherited Create;
  FODSQueue := AODSQueue;
  FCloseEventHandle := CreateEvent(nil, True, False, nil);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TODSThread.Execute;
var
  LAckEvent         : THandle;
  LReadyEvent       : THandle;
  LSharedFile       : THandle;
  LSharedMem        : Pointer;
  LReturnCode       : DWORD;
  LODSMessage       : TODSMessage;
  LHandlesToWaitFor : array [0..1] of THandle;
  SA                : SECURITY_ATTRIBUTES;
  SD                : SECURITY_DESCRIPTOR;
begin
  SA.nLength              := SizeOf(SECURITY_ATTRIBUTES);
  SA.bInheritHandle       := TRUE;
  SA.lpSecurityDescriptor := @SD;

  if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;

  if not SetSecurityDescriptorDacl(@SD, TRUE, nil { (PACL)NULL } , False) then
    Exit;

  LAckEvent := CreateEvent(@SA, False, TRUE, 'DBWIN_BUFFER_READY');
  if LAckEvent = 0 then
    Exit;

  LReadyEvent := CreateEvent(@SA, False, False, 'DBWIN_DATA_READY');
  if LReadyEvent = 0 then
    Exit;

  LSharedFile := CreateFileMapping(
    THandle(-1),
    @SA,
    PAGE_READWRITE,
    0,
    4096,
    'DBWIN_BUFFER'
  );
  if LSharedFile = 0 then
    Exit;

  LSharedMem := MapViewOfFile(LSharedFile, FILE_MAP_READ, 0, 0, 512);
  if not Assigned(LSharedMem) then
    Exit;

  while not Terminated do
  begin
    LHandlesToWaitFor[0] := FCloseEventHandle;
    LHandlesToWaitFor[1] := LReadyEvent;

    SetEvent(LAckEvent);
    LReturnCode := WaitForMultipleObjects(
      2,
      @LHandlesToWaitFor,
      False { bWaitAll },
      3000 { INFINITE }
    );

    case LReturnCode of
      WAIT_TIMEOUT:
        Continue;
      WAIT_OBJECT_0:
        Break;
      WAIT_OBJECT_0 + 1:
      begin
        LODSMessage             := TODSMessage.Create;
        LODSMessage.TimeStamp   := Now;
        LODSMessage.ProcessId   := LPDWORD(LSharedMem)^;
        //'$' + inttohex (pThisPid^,2)
//        LODSMessage.ProcessName :=
//          UTF8String(GetExenameForProcess(LODSMessage.ProcessId));
        // The native version of OutputDebugString is ASCII. result is always
        // AnsiString
        LODSMessage.MsgText :=
          AnsiString(PAnsiChar(LSharedMem) + SizeOf(DWORD));

        LODSMessage.Id := FLastChildOrder;
        Inc(FLastChildOrder);
        if Trim(string(LODSMessage.MsgText)) <> '' then
        begin
          Queue(procedure
            begin
              FODSQueue.Enqueue(LODSMessage);
            end
          );
        end;
      end;
      WAIT_FAILED:
        Continue;
    end;
  end;
  UnmapViewOfFile(LSharedMem);
  CloseHandle(LSharedFile);
end;
{$ENDREGION}
{$ENDREGION}

end.
