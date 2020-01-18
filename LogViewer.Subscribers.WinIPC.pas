{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Subscribers.Winipc;

{ WinIPC subscriber. }

interface

uses
  System.Classes,

  Spring, ZeroMQ,

  LogViewer.Interfaces, LogViewer.Subscribers.Base;

type
  TWinipcSubscriber = class(TSubscriber, ISubscriber, IWinipc)
  private
    FZmq        : Weak<IZeroMQ>;
    FSubscriber : IZMQPair;
    FPoll       : IZMQPoll;
    FZmqStream  : TStringStream;

  protected
    procedure CreateSubscriberSocket(const AEndPoint: string);
    procedure Close; override;
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
constructor TWinipcSubscriber.Create(const AReceiver: IChannelReceiver;
  const AZmq: IZeroMQ; const AEndPoint: string; ASourceId: UInt32; const AKey,
  ASourceName: string; AEnabled: Boolean);
begin
  Logger.Track(Self, 'Create');
  FZmq := AZmq;
  FZmqStream := TStringStream.Create;
  CreateSubscriberSocket(AEndPoint);
  inherited Create(AReceiver, ASourceId, AKey, ASourceName, AEnabled);
end;

destructor TWinipcSubscriber.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FZmqStream.Free;
  FSubscriber.Close;
  FPoll       := nil;
  FSubscriber := nil;
  FZmq        := nil;
  inherited Destroy;
end;

procedure TWinipcSubscriber.CreateSubscriberSocket(const AEndPoint: string);
begin
  FSubscriber := FZmq.Target.Start(ZMQSocket.Subscriber);
  Guard.CheckTrue(
    FSubscriber.Connect(AEndPoint) = 0,
    Format('Connect to %s failed.', [AEndPoint])
  );
  Guard.CheckTrue(FSubscriber.Subscribe('') = 0, 'Subscribe failed');
  FPoll := FZmq.Target.Poller;
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
{$ENDREGION}

{$REGION 'protected methods'}
procedure TWinipcSubscriber.Poll;
begin
  if Enabled then
  begin
    while FPoll.PollOnce(10) > 0 do
    begin
      FPoll.FireEvents;
    end;
  end;
end;

procedure TWinipcSubscriber.Close;
begin
  inherited Close;
  FSubscriber.Close;
end;
{$ENDREGION}

end.
