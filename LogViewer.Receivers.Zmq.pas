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

unit LogViewer.Receivers.Zmq;

{ ZeroMQ channel receiver. }

interface

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  ZeroMQ,

  LogViewer.Interfaces,  LogViewer.Receivers.Base,
  LogViewer.Receivers.Zmq.Settings;

{$REGION 'documentation'}
{ Receives logmessages from one or more ZMQ publisher through a subscriber
  socket.

  A ZeroMQChannelReceiver can receive messages from multiple channels that are
  bound to one or more ZMQ publisher sockets.

  In this setup the source application (sender of log messages) acts as a server
  which binds to a ZeroMQ socket (publisher).
  Multiple subscribers (TZeroMQChannelReceiver instances) can connect to the
  same publisher.

  REMARKS:
    - communication with ZeroMQ sockets is asynchronious. So the source
      application is never blocked as all messages are queued.
    - when the receiver cannot keep up with the publisher, messages are thrown
      away by the ZeroMQ subscriber.

  TODO:
    - list of connectionstrings to subscribe to?
}
{$ENDREGION}

type
  TZmqChannelReceiver = class(TChannelReceiver, IChannelReceiver, IZmq)
  private
    FZmq : IZeroMQ;

  protected
    {$REGION 'property access methods'}
    function GetSettings: TZmqSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure SettingsChanged(Sender: TObject);

  public
    procedure AfterConstruction; override;

    constructor Create(
      AManager    : ILogViewerManager;
      AZmq        : IZeroMQ;
      const AName : string
    ); reintroduce;
    destructor Destroy; override;

    property Settings: TZmqSettings
      read GetSettings;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Forms;

{$REGION 'construction and destruction'}
procedure TZmqChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  PollTimer.OnTimer  := PollTimerTimer;
  PollTimer.Interval := Settings.PollingInterval;
  Settings.OnChanged.Add(SettingsChanged);
end;

constructor TZmqChannelReceiver.Create(AManager: ILogViewerManager; AZmq:
  IZeroMQ; const AName: string);
begin
  inherited Create(AManager, AName);
  FZmq := AZmq;
end;

destructor TZmqChannelReceiver.Destroy;
begin
  PollTimer.Enabled := False;
  Settings.OnChanged.RemoveAll(Self);
  FZmq := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TZmqChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled(Value);
  PollTimer.Enabled := Value;
end;

function TZmqChannelReceiver.GetSettings: TZmqSettings;
begin
  Result := Manager.Settings.ZmqSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TZmqChannelReceiver.SettingsChanged(Sender: TObject);
var
  LSubscriber : ISubscriber;
begin
  Enabled            := Settings.Enabled;
  PollTimer.Interval := Settings.PollingInterval;
  for LSubscriber in SubscriberList.Values do
  begin
    (LSubscriber as ILogMessageSubscriptionFilter).LogMessageTypes :=
      Settings.LogMessageTypes;
    (LSubscriber as ILogMessageSubscriptionFilter).LogMessageLevels :=
      Settings.LogMessageLevels;
  end;
end;
{$ENDREGION}

end.
