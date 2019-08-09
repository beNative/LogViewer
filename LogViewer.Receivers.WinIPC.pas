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

unit LogViewer.Receivers.Winipc;

{ Winipc channel receiver. }

interface

{$REGION 'documentation'}
{ Receives logmessages through WinIPC (WM_COPYDATA) messages.

  The communication with the message source is synchronous, so when the source
  application sends a message, it blocks until it is received by the receiver.

  REMARK:
   - the sending application and the logviewer need to be started with the same
     windows user credentials.
}
{$ENDREGION}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  DDuce.WinIPC.Server,

  LogViewer.Receivers.Base, LogViewer.Interfaces,
  LogViewer.Receivers.WinIPC.Settings;

type
  TWinipcChannelReceiver = class(TChannelReceiver, IChannelReceiver, IWinipc)
  private
    FIpcServer : TWinipcServer;

  protected
    {$REGION 'property access methods'}
    function GetWindowName: string;
    function GetMsgWindowClassName: string;
    function GetSettings: TWinipcSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure FIpcServerMessage(
      Sender    : TObject;
      ASourceId : UInt32;
      AData     : TStream
    );

    function CreateSubscriber(
      ASourceId         : UInt32;
      AThreadId         : UInt32;
      const ASourceName : string
    ): ISubscriber; override;

    procedure SettingsChanged(Sender: TObject);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Settings: TWinipcSettings
      read GetSettings;

    property WindowName: string
      read GetWindowName;

    property MsgWindowClassName: string
      read GetMsgWindowClassName;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Utils.Winapi, DDuce.Logger,

  LogViewer.Subscribers.Winipc;

{$REGION 'construction and destruction'}
procedure TWinipcChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FIpcServer           := TWinipcServer.Create;
  FIpcServer.OnMessage := FIpcServerMessage;
  FIpcServer.Active    := True;
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TWinipcChannelReceiver.BeforeDestruction;
begin
  FIpcServer.Active := False;
  FIpcServer.Free;
  inherited BeforeDestruction;
end;

function TWinipcChannelReceiver.CreateSubscriber(ASourceId, AThreadId: UInt32;
  const ASourceName: string): ISubscriber;
begin
  Result := TWinIPCSubscriber.Create(Self, ASourceId, '', ASourceName, True);
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TWinipcChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Value then
    FIpcServer.OnMessage := FIpcServerMessage
  else
    FIpcServer.OnMessage := nil;
  FIpcServer.Active := Value;
end;

function TWinipcChannelReceiver.GetMsgWindowClassName: string;
begin
  Result := FIpcServer.MsgWindowClassName;
end;

function TWinipcChannelReceiver.GetSettings: TWinipcSettings;
begin
  Result := Manager.Settings.WinIPCSettings;
end;

function TWinipcChannelReceiver.GetWindowName: string;
begin
  Result := FIpcServer.WindowName;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinipcChannelReceiver.FIpcServerMessage(Sender: TObject;
  ASourceId: UInt32; AData: TStream);
var
  LProcessName : string;
begin
  if not Processes.TryGetValue(ASourceId, LProcessName) then
  begin
    LProcessName := GetExenameForProcess(ASourceId);
    Processes.AddOrSetValue(ASourceId, LProcessName);
  end;
  DoReceiveMessage(AData, ASourceId, 0, LProcessName);
end;

procedure TWinipcChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
end;
{$ENDREGION}

end.
