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

unit LogViewer.Receivers.ZeroMQ;

interface

{ Receives logmessages from one or more ZMQ publisher through a subscriber
  socket.

  A ZeroMQChannelReceiver can receive messages from multiple channels that are
  bound to one or more ZMQ publisher sockets.

  TODO:
    - list of connectionstrings to subscribe to?
}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  ZeroMQ,

  LogViewer.Interfaces;

const
  ZQM_DEFAULT_ADDRESS = 'tcp://localhost:5555';
//  tcp://GANYMEDES:5555
//  tcp://EUROPA:5555

type
  TZeroMQChannelReceiver = class(TInterfacedObject, IChannelReceiver)
  private class var
    FCounter : Integer;
  private
    FOnReceiveMessage : Event<TReceiveMessageEvent>;
    FZMQStream        : TStringStream;
    FZMQ              : IZeroMQ;
    FSubscriber       : IZMQPair;
    FPoll             : IZMQPoll;
    FTimer            : TTimer;
    FEnabled          : Boolean;
    FName             : string;
    FAddress          : string;

    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
    function GetName: string;
    procedure SetName(const Value: string);

    function ConnectSubscriber: Boolean;
    procedure CloseSubscriber;

  protected
    procedure DoReceiveMessage(AStream : TStream);

    procedure FTimerTimer(Sender: TObject);

  public
    constructor Create(
      const AName    : string = '';
      const AAddress : string = ''
    );
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
constructor TZeroMQChannelReceiver.Create(const AName: string;
  const AAddress: string);
begin
  inherited Create;
  if AName = '' then
  begin
    FName := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  end
  else
    FName := AName;
  FAddress := AAddress;
end;

procedure TZeroMQChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FOnReceiveMessage.UseFreeNotification := False;
  Inc(FCounter);
  if FAddress = '' then
    FAddress := ZQM_DEFAULT_ADDRESS;
  FTimer         := TTimer.Create(nil);
  FTimer.OnTimer := FTimerTimer;
  FZMQ           := TZeroMQ.Create;
  FEnabled       := ConnectSubscriber;
  FZMQStream     := TStringStream.Create;
end;

procedure TZeroMQChannelReceiver.BeforeDestruction;
begin
  CloseSubscriber;
  FTimer.Free;
  FZMQStream.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZeroMQChannelReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TZeroMQChannelReceiver.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    if Value then
      FEnabled := ConnectSubscriber
    else
      CloseSubscriber;
    FTimer.Enabled := FEnabled;
  end;
end;

function TZeroMQChannelReceiver.GetName: string;
begin
  Result := FName;
end;

procedure TZeroMQChannelReceiver.SetName(const Value: string);
begin
  FName := Value;
end;

function TZeroMQChannelReceiver.GetOnReceiveMessage: IEvent<TReceiveMessageEvent>;
begin
  Result := FOnReceiveMessage;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TZeroMQChannelReceiver.DoReceiveMessage(AStream: TStream);
begin
  FOnReceiveMessage.Invoke(Self, Self as IChannelReceiver, AStream);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TZeroMQChannelReceiver.FTimerTimer(Sender: TObject);
begin
  if Assigned(FPoll) then
  begin
    while FPoll.PollOnce(50) > 0 do
      FPoll.FireEvents;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TZeroMQChannelReceiver.CloseSubscriber;
begin
  if Assigned(FSubscriber) then
  begin
    FSubscriber.Close;
    FSubscriber := nil;
    FPoll       := nil;
  end;
end;

function TZeroMQChannelReceiver.ConnectSubscriber: Boolean;
var
  N : Integer;
begin
  FSubscriber := FZMQ.Start(ZMQSocket.Subscriber);
  N := FSubscriber.Connect(FAddress);
  // '' as Filter means all ?
  //FSubscriber.Subscribe(Name); // required!!
  FSubscriber.Subscribe(''); // required!!
  FPoll := FZMQ.Poller;
  FPoll.RegisterPair(FSubscriber, [PollEvent.PollIn],
    procedure(Event: PollEvents)
    begin
      FZMQStream.WriteString(FSubscriber.ReceiveString);
      DoReceiveMessage(FZMQStream);
      FZMQStream.Clear;
    end
  );
  Result := True;
end;
{$ENDREGION}

end.
