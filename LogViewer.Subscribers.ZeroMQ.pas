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

unit LogViewer.Subscribers.ZeroMQ;

interface

uses
  System.Classes,

  ZeroMQ,

  Spring,

  LogViewer.Interfaces, LogViewer.Subscribers.Base;

type
  TZMQSubscriber = class(TSubscriber, ISubscriber)
  private
    FZMQ        : IZeroMQ;
    FSubscriber : IZMQPair;
    FPoll       : IZMQPoll;
    FZMQStream  : TStringStream;

    procedure CreateSubscriberSocket(const AEndPoint: string);

  protected
    {$REGION 'property access methods'}
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure Poll;

  public
    constructor Create(
      AReceiver       : IChannelReceiver;
      AZMQ            : IZeroMQ;
      const AEndPoint : string;
      AEnabled        : Boolean
    ); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
constructor TZMQSubscriber.Create(AReceiver: IChannelReceiver; AZMQ: IZeroMQ;
  const AEndPoint: string; AEnabled: Boolean);
begin
  inherited Create(AReceiver, 0, AEndPoint, '', False);
  FZMQ      := AZMQ;
  CreateSubscriberSocket(AEndPoint);
  Enabled := AEnabled;
end;

procedure TZMQSubscriber.AfterConstruction;
begin
  inherited AfterConstruction;
  FZMQStream := TStringStream.Create;
end;

procedure TZMQSubscriber.BeforeDestruction;
begin
  FZMQStream.Free;
  FPoll       := nil;
  FSubscriber := nil;
  FZMQ        := nil;
  inherited BeforeDestruction;
end;

{$ENDREGION}

{$REGION 'property access methods'}
procedure TZMQSubscriber.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    inherited SetEnabled(Value);
    if Value then
      FSubscriber.Subscribe('')
    else
      FSubscriber.UnSubscribe('');
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZMQSubscriber.CreateSubscriberSocket(const AEndPoint: string);
begin
  FSubscriber := FZMQ.Start(ZMQSocket.Subscriber);
  FSubscriber.Connect(AEndPoint);
  FPoll := FZMQ.Poller;
  FPoll.RegisterPair(FSubscriber, [PollEvent.PollIn],
    procedure(Event: PollEvents)
    begin
      FZMQStream.WriteString(FSubscriber.ReceiveString);
      Receiver.DoReceiveMessage(
        FZMQStream, Integer(FSubscriber), 0, FSubscriber.LastEndPoint
      );
      FZMQStream.Clear;
    end
  );
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZMQSubscriber.Poll;
begin
  if Enabled then
  begin
    while FPoll.PollOnce(10) > 0 do
      FPoll.FireEvents;
  end;
end;
{$ENDREGION}

end.
