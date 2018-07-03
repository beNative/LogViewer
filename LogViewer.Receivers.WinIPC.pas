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

interface

{ Receives logmessages through WinIPC (WM_COPYDATA) messages. }

{ TODO :  Notification when a ProcessId/ProcessName does not exist anymore }

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  DDuce.WinIPC.Server,

  LogViewer.Receivers.Base, LogViewer.Interfaces, LogViewer.WinIPC.Settings;

type
  TWinIPChannelReceiver = class(TChannelReceiver, IChannelReceiver)
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

    procedure SettingsChanged(Sender: TObject); override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Settings: TWinIPCSettings
      read GetSettings;

  end;

implementation

uses
  System.SysUtils,

  DDuce.Utils, DDuce.Utils.Winapi;

{$REGION 'construction and destruction'}
procedure TWinIPChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FIPCServer           := TWinIPCServer.Create;
  FIPCServer.OnMessage := FIPCServerMessage;
  FIPCServer.Active    := True;
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TWinIPChannelReceiver.BeforeDestruction;
begin
  FIPCServer.Active := False;
  FIPCServer.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TWinIPChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Value then
    FIPCServer.OnMessage := FIPCServerMessage
  else
    FIPCServer.OnMessage := nil;
  FIPCServer.Active := Value;
end;

function TWinIPChannelReceiver.GetSettings: TWinIPCSettings;
begin
  Result := Manager.Settings.WinIPCSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinIPChannelReceiver.FIPCServerMessage(Sender: TObject;
  ASourceId: Integer; AData: TStream);
begin
  DoReceiveMessage(AData, ASourceId, 0, GetExenameForProcess(ASourceId));
end;

procedure TWinIPChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
end;
{$ENDREGION}

end.
