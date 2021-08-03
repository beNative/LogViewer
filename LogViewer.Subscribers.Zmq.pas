{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Subscribers.Zmq;

interface

uses
  System.Classes,

  ZeroMQ,

  Spring,

  DDuce.Logger.Interfaces,

  LogViewer.Interfaces, LogViewer.Subscribers.Base;

type
  TZmqSubscriber = class(TSubscriber,
    ISubscriber, IZmq, ILogMessageSubscriptionFilter
  )
  private
    FEndPoint   : string;
    FZmq        : IZeroMQ;
    FSubscriber : IZMQPair;
    FPoll       : IZMQPoll;
    FZmqStream  : TStringStream;

    procedure CreateSubscriberSocket(const AEndPoint: string);

  protected
    {$REGION 'property access methods'}
    procedure SetEnabled(const Value: Boolean); override;
    function GetSourceId: UInt32; override;
    procedure SetLogMessageLevels(const Value: TLogMessageLevels); override;
    procedure SetLogMessageTypes(const Value: TLogMessageTypes); override;
    {$ENDREGION}

    procedure Subscribe;
    procedure UnSubscribe;

    procedure Poll; override;

  public
    constructor Create(
      const AReceiver   : IChannelReceiver;
      const AZmq        : IZeroMQ;
      const AEndPoint   : string;
      ASourceId         : UInt32;
      const AKey        : string;
      const ASourceName : string;
      AEnabled          : Boolean
    ); reintroduce; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TZmqSubscriber.Create(const AReceiver: IChannelReceiver; const AZmq:
  IZeroMQ; const AEndPoint: string; ASourceId: UInt32; const AKey: string;
  const ASourceName: string; AEnabled: Boolean);
begin
  Guard.CheckNotNull(AZmq, 'AZmq');
  FZmq := AZmq;
  FZmqStream := TStringStream.Create('', TEncoding.ANSI);
  CreateSubscriberSocket(AEndPoint);
  inherited Create(AReceiver, ASourceId, AKey, ASourceName, AEnabled);
end;

destructor TZmqSubscriber.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FreeAndNil(FZmqStream);
  FSubscriber.Close;
  FPoll       := nil;
  FSubscriber := nil;
  FZmq        := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TZmqSubscriber.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    inherited SetEnabled(Value);
    if Enabled then
    begin
      Subscribe;
    end
    else
    begin
      UnSubscribe;
    end;
  end;
end;

procedure TZmqSubscriber.SetLogMessageLevels(const Value: TLogMessageLevels);
begin
  if Value <> LogMessageLevels then
  begin
    inherited SetLogMessageLevels(Value);
    if Enabled then
      Subscribe;
    DoChange;
  end;
end;

procedure TZmqSubscriber.SetLogMessageTypes(const Value: TLogMessageTypes);
begin
  if Value <> LogMessageTypes then
  begin
    inherited SetLogMessageTypes(Value);
    if Enabled then
      Subscribe;
    DoChange;
  end;
end;

function TZmqSubscriber.GetSourceId: UInt32;
begin
  Result := UInt32(Self);
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Creates a ZMQ subscriber socket, connects it to the given endpoint and
  registers a dedicated poller to handle incomming messages. }

procedure TZmqSubscriber.CreateSubscriberSocket(const AEndPoint: string);
begin
  FEndPoint := AEndPoint;
  FSubscriber := FZmq.Start(ZMQSocket.Subscriber);
  FSubscriber.Connect(AEndPoint);
  FPoll := FZmq.Poller;
  FPoll.RegisterPair(
    FSubscriber,
    [PollEvent.PollIn],
    procedure(Event: PollEvents)
    begin
      FZmqStream.Clear;
      FZmqStream.Position := 0;
      var S := FSubscriber.ReceiveString;

      FZmqStream.WriteString(S);
      DoReceiveMessage(FZmqStream);
      FZmqStream.Clear;
    end
  );
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZmqSubscriber.Poll;
begin
  if Enabled then
  begin
    while FPoll.PollOnce(1) > 0 do
    begin
      FPoll.FireEvents;
    end;
  end;
end;

procedure TZmqSubscriber.Subscribe;
var
  LLogMessageType  : TLogMessageType;
  LLogMessageLevel : TLogMessageLevel;
  LTopic           : UTF8String;
begin
  Logger.Track(Self, 'Subscribe');
  UnSubscribe;
  for LLogMessageType := Low(TLogMessageType) to High(TLogMessageType) do
  begin
    if LLogMessageType in LogMessageTypes then
    begin
      for LLogMessageLevel := Low(TLogMessageLevel) to High(TLogMessageLevel) do
      begin
        if LLogMessageLevel in LogMessageLevels then
        begin
          LTopic := AnsiChar(Byte(LLogMessageType))
            + AnsiChar(LLogMessageLevel);
          FSubscriber.Subscribe(LTopic);
        end;
      end;
    end
  end;
end;

procedure TZmqSubscriber.UnSubscribe;
begin
  Logger.Track(Self, 'UnSubscribe');
  // simplest way to unsubscribe from all subscrîption is just to create a new
  // socket.
  // See answer of Pieter Hintjens on this discussion:
  // https://grokbase.com/t/zeromq/zeromq-dev/165zwq8dxx/unsubscribe-from-all-topics
  CreateSubscriberSocket(FEndPoint);
end;
{$ENDREGION}

end.
