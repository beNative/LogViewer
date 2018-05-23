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
    - many optimizations are possible
}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring,

  ZeroMQ,

  LogViewer.Interfaces,  LogViewer.Receivers.Base, LogViewer.ZeroMQ.Settings;

const
  ZQM_DEFAULT_ADDRESS = 'tcp://localhost:5555';
//  tcp://GANYMEDES:5555
//  tcp://EUROPA:5555

type
  TZeroMQChannelReceiver = class(TChannelReceiver, IChannelReceiver)
  private
    FZMQStream  : TStringStream;
    FZMQ        : IZeroMQ;
    FSubscriber : IZMQPair;
    FPoll       : IZMQPoll;
    FTimer      : TTimer;
    FAddress    : string;

    function ConnectSubscriber: Boolean;
    procedure CloseSubscriber;

  protected
    {$REGION 'property access methods'}
    function GetSettings: TZeroMQSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure FTimerTimer(Sender: TObject);
    procedure SettingsChanged(Sender: TObject); override;

  public
    constructor Create(
      AManager       : ILogViewerManager;
      const AName    : string = '';
      const AAddress : string = ''
    ); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Settings: TZeroMQSettings
      read GetSettings;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
constructor TZeroMQChannelReceiver.Create(AManager: ILogViewerManager;
  const AName: string; const AAddress: string);
begin
  inherited Create(AManager, AName);
  FAddress := AAddress;
end;

procedure TZeroMQChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
  if FAddress = '' then
    FAddress := ZQM_DEFAULT_ADDRESS;
  FTimer          := TTimer.Create(nil);
  FTimer.OnTimer  := FTimerTimer;
  FTimer.Interval := 5;
  FZMQ            := TZeroMQ.Create;
  FZMQStream      := TStringStream.Create;
  Settings.OnChanged.Add(SettingsChanged);
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
procedure TZeroMQChannelReceiver.SetEnabled(const Value: Boolean);
var
  B : Boolean;
begin
  B := False;
  if Value then
  begin
    B := ConnectSubscriber;
  end
  else
    CloseSubscriber;
  inherited SetEnabled(B);
  FTimer.Enabled := B;
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
{$ENDREGION}

end.
