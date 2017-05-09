{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  DDuce.WinIPC.Server,

  LogViewer.Interfaces;

type
  TWinIPChannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private class var
     FCounter : Integer;
  private
     FEnabled          : Boolean;
     FIPCServer        : TWinIPCServer;
     FOnReceiveMessage : Event<TReceiveMessageEvent>;
     FName             : string;

    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
    function GetName: string;
    procedure SetName(const Value: string);

  protected
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);

    procedure FIPCServerMessage(Sender: TObject);

    procedure DoReceiveMessage(AStream : TStream);

  public
    constructor Create(const AName: string); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property Name: string
      read GetName write SetName;

    property OnReceiveMessage: IEvent<TReceiveMessageEvent>
      read GetOnReceiveMessage;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
constructor TWinIPChannelReceiver.Create(const AName: string);
begin
  inherited Create;  
  if AName = '' then
  begin
    FName := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  end
  else
    FName := AName;
end;

procedure TWinIPChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  Inc(FCounter);
  FIPCServer := TWinIPCServer.Create;
  FIPCServer.OnMessage := FIPCServerMessage;
  FIPCServer.Active := True;
end;

procedure TWinIPChannelReceiver.BeforeDestruction;
begin
  FIPCServer.Active := False;
  FIPCServer.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWinIPChannelReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TWinIPChannelReceiver.GetName: string;
begin
  Result := FName;
end;

procedure TWinIPChannelReceiver.SetName(const Value: string);
begin
  FName := Value;
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
    FIPCServer.Active := Value;
  end;
end;

function TWinIPChannelReceiver.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWinIPChannelReceiver.DoReceiveMessage(AStream: TStream);
begin
  FOnReceiveMessage.Invoke(Self, AStream);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TWinIPChannelReceiver.FIPCServerMessage(Sender: TObject);
begin
  DoReceiveMessage(TWinIPCServer(Sender).MsgData);
end;
{$ENDREGION}

end.
