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

  In this setup the source application (sender of log messages) acts as a server
  which binds to a ZeroMQ socket (publisher).
  Multiple subscribers (TZeroMQChannelReceiver instances) can connect to the
  same publisher.

  TODO:
    - list of connectionstrings to subscribe to?
}

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
    FZMQ            : IZeroMQ;
    FTimer          : TTimer;

  protected
    {$REGION 'property access methods'}
    function GetSettings: TZeroMQSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    function AddSubscriber(AKeyValues : IDynamicRecord): Boolean; override;

    procedure FTimerTimer(Sender: TObject);
    procedure SettingsChanged(Sender: TObject); override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Settings: TZeroMQSettings
      read GetSettings;

  end;

implementation

uses
  System.SysUtils,

  LogViewer.Receivers.ZeroMQ.Subscriber;

{$REGION 'construction and destruction'}
procedure TZeroMQChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  FTimer          := TTimer.Create(nil);
  FTimer.OnTimer  := FTimerTimer;
  FTimer.Interval := 100;
  FZMQ            := TZeroMQ.Create;
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TZeroMQChannelReceiver.BeforeDestruction;
begin
  FTimer.Free;
  inherited BeforeDestruction;
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
  for LSubscriber in SubscriberList do
  begin
    LSubscriber.Poll;
  end;
  FTimer.Enabled := True;
end;
{$ENDREGION}

{$REGION 'private methods'}
//procedure TZeroMQChannelReceiver.CloseSubscriber;
//begin
//  if Assigned(FSubscriber) then
//  begin
//    FSubscriber.Close;
//    FSubscriber := nil;
//    FPoll       := nil;
//  end;
//end;

(*
function TZeroMQChannelReceiver.ConnectSubscriber: Boolean;
var
  N : Integer;
begin
  FSubscriber := FZMQ.Start(ZMQSocket.Subscriber);

  //FAddress := Format('tcp://192.168.0.226:%d', [FSubscriber.GetPort]);
  FAddress := 'tcp://localhost:5555';

  N := FSubscriber.Connect(FAddress);
  if N = 0 then
  begin
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
  end
  else
    Result := False;
end;

*)
{$ENDREGION}

{$REGION 'protected methods'}
function TZeroMQChannelReceiver.AddSubscriber(
  AKeyValues: IDynamicRecord): Boolean;
var
  S : ISubscriber;
begin
  S := TZMQSubscriber.Create(
    Self,
    FZMQ ,
    AKeyValues.ToString('Address'),
    AKeyValues.ToString('Port')
  );
  SubscriberList.Add(S);
end;
{$ENDREGION}

end.
