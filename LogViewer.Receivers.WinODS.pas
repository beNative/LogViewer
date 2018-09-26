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

uses
  Winapi.Windows,
  System.Classes,

  Spring, Spring.Collections,

  LogViewer.Interfaces, LogViewer.Receivers.Base, LogViewer.WinODS.Settings;

{
      https://stackoverflow.com/questions/
        509498/in-delphi-is-outputdebugstring-thread-safe?rq=1

  When OutputDebugString() is called by an application, it takes these steps.
  Note that a failure at any point abandons the whole thing and treats the
  debugging request as a no-op (the string isn't sent anywhere).

  1. Open DBWinMutex and wait until we have exclusive access to it.
  2. Map the DBWIN_BUFFER segment into memory: if it's not found, there is no
     debugger running so the entire request is ignored.
  3. Open the DBWIN_BUFFER_READY and DBWIN_DATA_READY events. As with the shared
     memory segment, missing objects mean that no debugger is available.
  4. Wait for the DBWIN_BUFFER_READY event to be signaled: this says that the
     memory buffer is no longer in use. Most of the time, this event will be
     signaled immediately when it's examined, but it won't wait longer than 10
     seconds for the buffer to become ready (a timeout abandons the request).
  5. Copy up to about 4kbytes of data to the memory buffer, and store the
     current process ID there as well. Always put a NUL byte at the end of the
     string.
  6. Tell the debugger that the buffer is ready by setting the DBWIN_DATA_READY
     event. The debugger takes it from there.
  7. Release the mutex
  8. Close the Event and Section objects, though we keep the handle to the mutex
     around for later.
}

type
  PDbWinBuffer = ^DbWinBuffer;
  DbWinBuffer = record
    dwProcessId: DWORD;
    data: array[0..(4096-sizeof(DWORD))-1] of AnsiChar;
  end;

  TODSMessageReceivedEvent = procedure(
    const AString : AnsiString;
    AProcessId    : UInt32
  ) of object;

  TWinDebugMonitor = class
  private
    FOnMessageReceived       : TODSMessageReceivedEvent;
    m_hDBWinMutex            : THandle;
    m_hDBMonBuffer           : THandle;
    m_hEventBufferReady      : THandle;
    m_hEventDataReady        : THandle;
    m_hWinDebugMonitorThread : THandle;
    m_bWinDebugMonStopped    : Boolean;
    m_pDBBuffer              : PDbWinBuffer;

    function Initialize: DWORD;
    procedure Uninitialize;
    function WinDebugMonitorProcess: DWORD;

  protected
    procedure DoMessageReceived(
      const AString : AnsiString;
      AProcessId    : UInt32
    );

  public
    constructor Create;
    destructor Destroy; override;

    property OnMessageReceived : TODSMessageReceivedEvent
      read FOnMessageReceived write FOnMessageReceived;
  end;

type
  TWinODSChannelReceiver = class(TChannelReceiver, IChannelReceiver, IWinODS)
  private
     FDebugMonitor : TWinDebugMonitor;
     FBuffer       : TMemoryStream;

    function GetSettings: TWinODSSettings;

    procedure SettingsChanged(Sender: TObject);

    procedure FDebugMonitorMessageReceived(
      const AString : AnsiString;
      AProcessId    : UInt32
    );

  protected
    function CreateSubscriber(
      ASourceId         : UInt32;
      AThreadId         : UInt32;
      const ASourceName : string
    ): ISubscriber; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Settings: TWinODSSettings
      read GetSettings;
  end;

implementation

uses
  System.SysUtils,

  Spring.Helpers,

  DDuce.Logger.Interfaces, DDuce.Utils.Winapi,

  LogViewer.Subscribers.WinODS;


{$REGION 'TWinDebugMonitor'}
// ----------------------------------------------------------------------------
//  PROPERTIES OF OBJECTS
// ----------------------------------------------------------------------------
//  NAME        |   DBWinMutex      DBWIN_BUFFER_READY      DBWIN_DATA_READY
// ----------------------------------------------------------------------------
//  TYPE        |   Mutex           Event                   Event
//  ACCESS      |   All             All                     Sync
//  INIT STATE  |   ?               Signaled                Nonsignaled
//  PROPERTY    |   ?               Auto-Reset              Auto-Reset
// ----------------------------------------------------------------------------

constructor TWinDebugMonitor.Create;
begin
  inherited;
  if Initialize <> 0 then
  begin
    OutputDebugString('TWinDebugMonitor.Initialize failed.'#10);
  end;
end;

destructor TWinDebugMonitor.Destroy;
begin
  Uninitialize;
  inherited;
end;

procedure TWinDebugMonitor.DoMessageReceived(const AString: AnsiString;
  AProcessId: UInt32);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(AString, AProcessId);
end;

function WinDebugMonitorThread(pData: Pointer): DWORD; stdcall;
var
  WDB : TWinDebugMonitor;
begin
  WDB := TWinDebugMonitor(pData);

  if WDB <> nil then
  begin
    while not WDB.m_bWinDebugMonStopped do
    begin
      WDB.WinDebugMonitorProcess;
    end;
  end;

  Result := 0;
end;

function TWinDebugMonitor.Initialize: DWORD;
var
  LThreadId : DWORD;
begin
  SetLastError(0);

  // Mutex: DBWin
  // ---------------------------------------------------------
  m_hDBWinMutex := OpenMutex(MUTEX_ALL_ACCESS, False, 'DBWin');
  if m_hDBWinMutex = 0 then
  begin
    m_hDBWinMutex := CreateMutex(nil, LongBool(1), 'DBWin');
    if m_hDBWinMutex = 0 then
    begin
      Result := GetLastError;
      Exit;
    end;
  end;

  // Event: buffer ready
  // ---------------------------------------------------------
  m_hEventBufferReady := OpenEvent(EVENT_ALL_ACCESS, False, 'DBWIN_BUFFER_READY');
  if m_hEventBufferReady = 0 then begin
    m_hEventBufferReady := CreateEvent(nil, False, TRUE, 'DBWIN_BUFFER_READY');
    if m_hEventBufferReady = 0 then begin
      Result := GetLastError;
      Exit;
    end;
  end;

  // Event: data ready
  // ---------------------------------------------------------
  m_hEventDataReady := OpenEvent(SYNCHRONIZE, False, 'DBWIN_DATA_READY');
  if m_hEventDataReady = 0 then begin
    m_hEventDataReady := CreateEvent(nil, False, False, 'DBWIN_DATA_READY');
    if m_hEventDataReady = 0 then begin
      Result := GetLastError;
    end;
  end;

  // Shared memory
  // ---------------------------------------------------------
  m_hDBMonBuffer := OpenFileMapping(FILE_MAP_READ, False, 'DBWIN_BUFFER');
  if m_hDBMonBuffer = 0 then begin
  begin
    m_hDBMonBuffer := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, SizeOf(DbWinBuffer), 'DBWIN_BUFFER');
    if m_hDBMonBuffer = 0 then begin
      Result := GetLastError;
      Exit;
    end;
  end;

  m_pDBBuffer := PDbWinBuffer(MapViewOfFile(m_hDBMonBuffer, SECTION_MAP_READ, 0, 0, 0));
  if m_pDBBuffer = nil then begin
    Result := GetLastError;
    Exit;
  end;

  // Monitoring thread
  // ---------------------------------------------------------
  m_bWinDebugMonStopped := False;

  m_hWinDebugMonitorThread := CreateThread(nil, 0, @WinDebugMonitorThread, Pointer(Self), 0, LThreadId);
  if m_hWinDebugMonitorThread = 0 then begin
    m_bWinDebugMonStopped := True;
    Result := GetLastError;
    Exit;
  end;

  // set monitor thread priority to highest
  // ---------------------------------------------------------
  SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
  SetThreadPriority(m_hWinDebugMonitorThread, THREAD_PRIORITY_TIME_CRITICAL);

  Result := 0;
  end;
end;

procedure TWinDebugMonitor.Uninitialize;
begin
  if m_hWinDebugMonitorThread <> 0 then begin
    m_bWinDebugMonStopped := True;
    WaitForSingleObject(m_hWinDebugMonitorThread, INFINITE);
    CloseHandle(m_hWinDebugMonitorThread);
    m_hWinDebugMonitorThread := 0;
  end;

  if m_hDBWinMutex <> 0 then begin
    CloseHandle(m_hDBWinMutex);
    m_hDBWinMutex := 0;
  end;

  if m_pDBBuffer <> nil then begin
    UnmapViewOfFile(m_pDBBuffer);
    m_pDBBuffer := nil;
  end;

  if m_hDBMonBuffer <> 0 then begin
    CloseHandle(m_hDBMonBuffer);
    m_hDBMonBuffer := 0;
  end;

  if m_hEventBufferReady <> 0  then begin
    CloseHandle(m_hEventBufferReady);
    m_hEventBufferReady := 0;
  end;

  if m_hEventDataReady <> 0 then begin
    CloseHandle(m_hEventDataReady);
    m_hEventDataReady := 0;
  end;
end;

function TWinDebugMonitor.WinDebugMonitorProcess: DWORD;
const
  TIMEOUT_WIN_DEBUG = 100;
begin
  // wait for data ready
  Result := WaitForSingleObject(m_hEventDataReady, TIMEOUT_WIN_DEBUG);

  if Result = WAIT_OBJECT_0 then
  begin
    TThread.CurrentThread.Queue(
      TThread.CurrentThread,
      procedure
      begin
        DoMessageReceived(m_pDBBuffer^.data, m_pDBBuffer.dwProcessId);
      end
    );
    // signal buffer ready
    SetEvent(m_hEventBufferReady);
  end;
end;
{$ENDREGION}


{$REGION 'TWinODSChannelReceiver'}
procedure TWinODSChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FBuffer := TMemoryStream.Create;
  FDebugMonitor := TWinDebugMonitor.Create;
  FDebugMonitor.OnMessageReceived := FDebugMonitorMessageReceived;

  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TWinODSChannelReceiver.BeforeDestruction;
begin
  FDebugMonitor.Free;
  FBuffer.Free;
  inherited BeforeDestruction;
end;

function TWinODSChannelReceiver.CreateSubscriber(ASourceId, AThreadId: UInt32;
  const ASourceName: string): ISubscriber;
begin
  Result := TWinODSSubscriber.Create(Self, ASourceId, '', ASourceName, True);
end;

procedure TWinODSChannelReceiver.FDebugMonitorMessageReceived(
  const AString: AnsiString; AProcessId: UInt32);
const
  ZERO_BUF : Integer = 0;
var
  LTextSize    : Integer;
  LText        : AnsiString;
  LMsgType     : Byte;
  LDummy       : Byte;
  LProcessName : string;
begin
  if Enabled then
  begin
    LDummy := 0;
    begin
      FBuffer.Clear;
      LText := #13#10 + AString; //??
      LMsgType := Integer(lmtText);
      LTextSize := Length(LText);
      FBuffer.Seek(0, soFromBeginning);
      FBuffer.WriteBuffer(LMsgType);
      FBuffer.WriteBuffer(LDummy);
      FBuffer.WriteBuffer(LDummy);
      FBuffer.WriteBuffer(LDummy);
      FBuffer.WriteBuffer(Now);
      FBuffer.WriteBuffer(LTextSize);
      FBuffer.WriteBuffer(LText[1], LTextSize);
      FBuffer.WriteBuffer(ZERO_BUF);
      if not Processes.TryGetValue(AProcessId, LProcessName) then
      begin
        LProcessName := GetExenameForProcess(AProcessId);
        Processes.AddOrSetValue(AProcessId, LProcessName);
      end;
      DoReceiveMessage(FBuffer, AProcessId, 0, LProcessName);
    end;
  end;
end;

function TWinODSChannelReceiver.GetSettings: TWinODSSettings;
begin
  Result := Manager.Settings.WinODSSettings;
end;

procedure TWinODSChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
end;
{$ENDREGION}

end.
