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
  TZMQSubscriber = class(TSubscriber, ISubscriber, IZMQ)
  private
    FZMQ        : IZeroMQ;
    FSubscriber : IZMQPair;
    FPoll       : IZMQPoll;
    FZMQStream  : TStringStream;

    procedure CreateSubscriberSocket(const AEndPoint: string);

  protected
    {$REGION 'property access methods'}
    procedure SetEnabled(const Value: Boolean); override;
    function GetSourceId: UInt32; override;
    {$ENDREGION}

    procedure Poll; override;

  public
    constructor Create(
      const AReceiver   : IChannelReceiver;
      const AZMQ        : IZeroMQ;
      const AEndPoint   : string;
      ASourceId         : UInt32;
      const AKey        : string;
      const ASourceName : string;
      AEnabled          : Boolean
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TZMQSubscriber.Create(const AReceiver: IChannelReceiver; const AZMQ:
  IZeroMQ; const AEndPoint: string; ASourceId: UInt32; const AKey: string;
  const ASourceName: string; AEnabled: Boolean);
begin
  Guard.CheckNotNull(AZMQ, 'AZMQ');
  FZMQ := AZMQ;
  CreateSubscriberSocket(AEndPoint);
  inherited Create(AReceiver, ASourceId, AKey, ASourceName, AEnabled);
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
    if Enabled then
      FSubscriber.Subscribe('')
    else
      FSubscriber.UnSubscribe('');
  end;
end;

function TZMQSubscriber.GetSourceId: UInt32;
begin
  Result := UInt32(Self);
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Creates a ZMQ subscriber socket, connects it to the given endpoint and
  registers a dedicated poller to handle incomming messages. }

procedure TZMQSubscriber.CreateSubscriberSocket(const AEndPoint: string);
begin
  FSubscriber := FZMQ.Start(ZMQSocket.Subscriber);
  FSubscriber.Connect(AEndPoint);
  FPoll := FZMQ.Poller;
  FPoll.RegisterPair(
    FSubscriber,
    [PollEvent.PollIn],
    procedure(Event: PollEvents)
    begin
      FZMQStream.WriteString(FSubscriber.ReceiveString);
      Receiver.DoReceiveMessage(
        FZMQStream, SourceId, 0, FSubscriber.LastEndPoint
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
    begin
      FPoll.FireEvents;
    end;
  end;
end;
{$ENDREGION}

end.
