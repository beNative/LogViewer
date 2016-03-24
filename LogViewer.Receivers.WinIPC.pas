{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Receives logmessages from through WinIPC (WM_COPYDATA) messages. }

uses
  System.Classes,
  Vcl.ExtCtrls,

  DDuce.WinIPC.Server,

  LogViewer.Interfaces;

type
  TWinIPChannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private
     FEnabled   : Boolean;
     FIPCServer : TWinIPCServer;

  protected
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);

    procedure FIPCServerMessage(Sender: TObject);

    procedure DoReceiveMessage(AStream : TStream);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TWinIPChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FIPCServer := TWinIPCServer.Create;
  FIPCServer.OnMessage := FIPCServerMessage;
  FIPCServer.Active := True;
end;

procedure TWinIPChannelReceiver.BeforeDestruction;
begin
  FIPCServer.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWinIPChannelReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TWinIPChannelReceiver.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    if Value then
      FIPCServer.OnMessage := FIPCServerMessage
    else
      FIPCServer.OnMessage := nil;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWinIPChannelReceiver.DoReceiveMessage(AStream: TStream);
begin
//
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinIPChannelReceiver.FIPCServerMessage(Sender: TObject);
begin
  DoReceiveMessage(TWinIPCServer(Sender).MsgData);
end;
{$ENDREGION}

end.
