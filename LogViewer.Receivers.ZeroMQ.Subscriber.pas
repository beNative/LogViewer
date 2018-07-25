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

unit LogViewer.Receivers.ZeroMQ.Subscriber;

interface

uses
  System.Classes,

  ZeroMQ,

  Spring,

  LogViewer.Interfaces;

type
  TZMQSubscriber = class(TInterfacedObject)
  private
    FReceiver   : Weak<IChannelReceiver>; // weak reference!
    FZMQ        : IZeroMQ;
    FSubscriber : IZMQPair;
    FPoll       : IZMQPoll;
    FAddress    : string;
    FPort       : string;
    FZMQStream  : TStringStream;

  protected
    procedure CreateSubscriber;

  public
    constructor Create(
      AReceiver : IChannelReceiver;
      AZMQ      : IZeroMQ;
      AAddress  : string;
      APort     : string
    );
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Poll;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TZMQSubscriber.AfterConstruction;
begin
  inherited AfterConstruction;
  FZMQStream := TStringStream.Create;
end;

procedure TZMQSubscriber.BeforeDestruction;
begin
  FZMQStream.Free;
  FReceiver   := nil;
  FPoll       := nil;
  FSubscriber := nil;
  FZMQ        := nil;
  inherited BeforeDestruction;
end;

constructor TZMQSubscriber.Create(AReceiver: IChannelReceiver; AZMQ: IZeroMQ;
  AAddress, APort: string);
begin
  FReceiver := AReceiver;
  FZMQ      := AZMQ;
  FAddress  := AAddress;
  FPort     := APort;
  CreateSubscriber;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZMQSubscriber.CreateSubscriber;
begin
  FSubscriber := FZMQ.Start(ZMQSocket.Subscriber);
  FSubscriber.Connect(Format('tcp://%s:%s', [FAddress, FPort]));
  FSubscriber.Subscribe('');
  FPoll := FZMQ.Poller;
  FPoll.RegisterPair(FSubscriber, [PollEvent.PollIn],
    procedure(Event: PollEvents)
    begin
      FZMQStream.WriteString(FSubscriber.ReceiveString);
      FReceiver.Target.DoReceiveMessage(
        FZMQStream, Integer(FSubscriber), 0, FSubscriber.LastEndPoint
      );
      FZMQStream.Clear;
    end
  );
end;

procedure TZMQSubscriber.Poll;
begin
  while FPoll.PollOnce(10) > 0 do
    FPoll.FireEvents;
end;
{$ENDREGION}

end.