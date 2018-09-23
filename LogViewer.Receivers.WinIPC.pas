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

unit LogViewer.Receivers.WinIPC;

{ WinIPC channel receiver. }

interface

{$REGION 'documentation'}
{ Receives logmessages through WinIPC (WM_COPYDATA) messages.

  The communication with the message source is synchronous, so when the source
  application sends a message, it blocks until it is received by the receiver.

  REMARK:
   - the sending application and the logviewer need to be started with the same
     windows user credentials.
}

{ TODO :  Notification when a ProcessId/ProcessName does not exist anymore }
{$ENDREGION}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  DDuce.WinIPC.Server,

  LogViewer.Receivers.Base, LogViewer.Interfaces, LogViewer.WinIPC.Settings;

type
  TWinIPCChannelReceiver = class(TChannelReceiver, IChannelReceiver, IWinIPC)
  private
     FIPCServer : TWinIPCServer;

  protected
    {$REGION 'property access methods'}
    function GetSettings: TWinIPCSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure FIPCServerMessage(
      Sender    : TObject;
      ASourceId : Integer;
      AData     : TStream
    );

    function CreateSubscriber(
      ASourceId         : Integer;
      AThreadId         : Integer;
      const ASourceName : string
    ): ISubscriber; override;

    procedure SettingsChanged(Sender: TObject);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Settings: TWinIPCSettings
      read GetSettings;

  end;

implementation

uses
  System.SysUtils,

  DDuce.Utils, DDuce.Utils.Winapi, DDuce.Logger,

  LogViewer.Subscribers.WinIPC;

{$REGION 'construction and destruction'}
procedure TWinIPCChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FIPCServer           := TWinIPCServer.Create;
  FIPCServer.OnMessage := FIPCServerMessage;
  FIPCServer.Active    := True;
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TWinIPCChannelReceiver.BeforeDestruction;
begin
  FIPCServer.Active := False;
  FIPCServer.Free;
  inherited BeforeDestruction;
end;

function TWinIPCChannelReceiver.CreateSubscriber(ASourceId, AThreadId: Integer;
  const ASourceName: string): ISubscriber;
begin
  Result := TWinIPCSubscriber.Create(Self, ASourceId, '', ASourceName, True);
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TWinIPCChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Value then
    FIPCServer.OnMessage := FIPCServerMessage
  else
    FIPCServer.OnMessage := nil;
  FIPCServer.Active := Value;
end;

function TWinIPCChannelReceiver.GetSettings: TWinIPCSettings;
begin
  Result := Manager.Settings.WinIPCSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinIPCChannelReceiver.FIPCServerMessage(Sender: TObject;
  ASourceId: Integer; AData: TStream);
begin
  DoReceiveMessage(AData, ASourceId, 0, GetExenameForProcess(ASourceId));
end;

procedure TWinIPCChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
end;
{$ENDREGION}

end.
