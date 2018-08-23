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

{ ZeroMQ channel receiver. }

interface

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

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring, Spring.Collections,

  ZeroMQ,

  DDuce.DynamicRecord,

  LogViewer.Interfaces,  LogViewer.Receivers.Base, LogViewer.ZeroMQ.Settings;

//const
  //ZQM_DEFAULT_ADDRESS = 'tcp://192.168.0.226:5555';
  //ZQM_DEFAULT_ADDRESS = 'tcp://192.168.0.226:*';
//  tcp://GANYMEDES:5555
//  tcp://EUROPA:5555

// LogQueue is always filled with messages of the same kind, and is not specific
// for a Receiver instance.

// Here the subscribers are stored in a specific list, and they only will write
// to a queue if it is enabled and is receiving messages.

type
  TZeroMQChannelReceiver = class(TChannelReceiver, IChannelReceiver)
  private
    FTimer : TTimer;
    FZMQ   : IZeroMQ;

  protected
    {$REGION 'property access methods'}
    function GetSettings: TZeroMQSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure FTimerTimer(Sender: TObject);
    procedure SettingsChanged(Sender: TObject); override;

    function CreateSubscriber(
      ASourceId         : Integer;
      AThreadId         : Integer;
      const ASourceName : string
    ): ISubscriber; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    constructor Create(
      AManager    : ILogViewerManager;
      AZMQ        : IZeroMQ;
      const AName : string
    ); reintroduce;

    property Settings: TZeroMQSettings
      read GetSettings;

  end;

implementation

uses
  System.SysUtils,
  Vcl.Forms,

  LogViewer.Subscribers.ZeroMQ;

{$REGION 'construction and destruction'}
procedure TZeroMQChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FTimer          := TTimer.Create(nil);
  FTimer.OnTimer  := FTimerTimer;
  FTimer.Interval := 100;
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TZeroMQChannelReceiver.BeforeDestruction;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  inherited BeforeDestruction;
end;

constructor TZeroMQChannelReceiver.Create(AManager: ILogViewerManager; AZMQ:
  IZeroMQ; const AName: string);
begin
  inherited Create(AManager, AName);
  FZMQ := AZMQ;
end;

function TZeroMQChannelReceiver.CreateSubscriber(ASourceId, AThreadId: Integer;
  const ASourceName: string): ISubscriber;
begin
  Result := TZMQSubscriber.Create(Self, FZMQ, '', False);
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TZeroMQChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited SetEnabled(Value);
  FTimer.Enabled := Value;
end;

function TZeroMQChannelReceiver.GetSettings: TZeroMQSettings;
begin
  Result := Manager.Settings.ZeroMQSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TZeroMQChannelReceiver.SettingsChanged(Sender: TObject);
begin
  Enabled := Settings.Enabled;
end;

procedure TZeroMQChannelReceiver.FTimerTimer(Sender: TObject);
var
  LSubscriber : ISubscriber;
begin
  FTimer.Enabled := False;
  for LSubscriber in SubscriberList.Values do
  begin
    LSubscriber.Poll;
  end;
  FTimer.Enabled := True;
end;
{$ENDREGION}

end.
