{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

  DDuce.Logger.Interfaces,

  LogViewer.Interfaces, LogViewer.Subscribers.Base;

type
  TZmqSubscriber = class(TSubscriber,
    ISubscriber, IZmq, ILogMessageSubscriptionFilter
  )
  private
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

    procedure SubscribeToMessages;
    procedure UnSubscribeToMessages;

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
  FZmqStream.Free;
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
      SubscribeToMessages;
    end
    else
    begin
      UnSubscribeToMessages;
    end;
  end;
end;

procedure TZmqSubscriber.SetLogMessageLevels(const Value: TLogMessageLevels);
begin
  inherited SetLogMessageLevels(Value);
  if Value <> LogMessageLevels then
  begin
    if Enabled then
      SubscribeToMessages;
    DoChange;
  end;
end;

procedure TZmqSubscriber.SetLogMessageTypes(const Value: TLogMessageTypes);
begin
  inherited SetLogMessageTypes(Value);
  if Value <> LogMessageTypes then
  begin
    if Enabled then
      SubscribeToMessages;
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
  FSubscriber := FZmq.Start(ZMQSocket.Subscriber);
  FSubscriber.Connect(AEndPoint);
  FPoll := FZmq.Poller;
  FZmqStream.Clear;
  FPoll.RegisterPair(
    FSubscriber,
    [PollEvent.PollIn],
    procedure(Event: PollEvents)
    begin
      FZmqStream.WriteString(FSubscriber.ReceiveString);
      DoReceiveMessage(FZmqStream);
      FZmqStream.Clear;
    end
  );
end;

procedure TZmqSubscriber.SubscribeToMessages;
var
  LLogMessageType  : TLogMessageType;
  LLogMessageLevel : TLogMessageLevel;
  LTopic           : RawByteString;
  N : Integer;
begin
  Logger.Track(Self, 'SubscribeToMessages');
  //UnSubscribeToMessages;
  //FSubscriber.Subscribe('');
  N := 0;
  for LLogMessageType := Low(TLogMessageType) to High(TLogMessageType) do
  begin
    if LLogMessageType in LogMessageTypes then
    begin
      for LLogMessageLevel := Low(TLogMessageLevel) to High(TLogMessageLevel) do
      begin
        if LLogMessageLevel in LogMessageLevels then
        begin
          LTopic := AnsiChar(Byte(LLogMessageType)) + AnsiChar(LLogMessageLevel);
          FSubscriber.Subscribe(LTopic);
          Logger.Info('LMT %s/%d',  [LogMessageTypeNameOf(LLogMessageType), LLogMessageLevel]);
         Logger.Send('N', N);
         Inc(N);
        end;
      end;
    end
    else
    begin
      Logger.Warn('%d not in LogMessageTypes', [Integer(LLogMessageType)]);
    end;
  end;
end;

procedure TZmqSubscriber.UnSubscribeToMessages;
var
  I : TLogMessageType;
  R : RawByteString;
  J : Byte;
begin
  for I := Low(TLogMessageType) to High(TLogMessageType) do
  begin
    for J := Low(Byte) to High(Byte) do
    begin
      R := AnsiChar(Byte(I)) + AnsiChar(J);
      //Logger.Send('UnSubscribe', R);
      FSubscriber.UnSubscribe(R);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZmqSubscriber.Poll;
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
