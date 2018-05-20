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


{ TODO :
  Need to handle multiple queues which each can be connected to a dedicated
  message viewer

  A channel receiver maintains a list of listeners which are associated with
  the processId of the originating process.

}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  DDuce.WinIPC.Server,

  LogViewer.Receivers.Base, LogViewer.Interfaces;

type
  TWinIPChannelReceiver = class(TChannelReceiver, IChannelReceiver)
  private
     FIPCServer        : TWinIPCServer;

  protected
    {$REGION 'property access methods'}
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure FIPCServerMessage(
      Sender    : TObject;
      ASourceId : Integer;
      AData     : TStream
    );

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.SysUtils,

  DDuce.Utils;

{$REGION 'construction and destruction'}
procedure TWinIPChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FIPCServer           := TWinIPCServer.Create;
  FIPCServer.OnMessage := FIPCServerMessage;
  FIPCServer.Active    := True;
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
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinIPChannelReceiver.FIPCServerMessage(Sender: TObject;
  ASourceId: Integer; AData: TStream);
begin
  DoReceiveMessage(ASourceId, AData);
end;
{$ENDREGION}

end.
