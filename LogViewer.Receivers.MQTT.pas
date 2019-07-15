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

unit LogViewer.Receivers.Mqtt;

{ MQTT channel receiver. }

interface

{$REGION 'documentation'}
{ In contrast to ZMQ, MQTT requires a message broker to route messages between
  servers (publishers) and clients (subscribers).
  There are many online message brokers available to connect to, and you can run
  your own one using Mosquitto for example.
}
{$ENDREGION}

uses
  System.Classes,
  Vcl.ExtCtrls,

  Spring, Spring.Collections,

  MQTT,

  DDuce.DynamicRecord,

  LogViewer.Interfaces,  LogViewer.Receivers.Base,
  LogViewer.Receivers.MQTT.Settings;

{$REGION 'documentation'}

{$ENDREGION}

type
  TMqttChannelReceiver = class(TChannelReceiver, IChannelReceiver, IMqtt)
  private
    FMQTT : TMQTT;

    procedure Connect;

  protected
    {$REGION 'property access methods'}
    function GetMQTT: TMQTT;
    function GetSettings: TMQTTSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    procedure SettingsChanged(Sender: TObject);
    // event called when message is received
    procedure FMQTTPublish(
      Sender   : TObject;
      ATopic   : UTF8String;
      APayload : UTF8String
    );

    function CreateSubscriber(
      ASourceId         : UInt32;
      AThreadId         : UInt32;
      const ASourceName : string
    ): ISubscriber; override;

    property MQTT: TMQTT
      read GetMQTT;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property Settings: TMQTTSettings
      read GetSettings;

  end;

implementation

uses
  System.SysUtils,

  LogViewer.Subscribers.MQTT,

  DDuce.Logger, DDuce.Logger.Interfaces;

{$REGION 'construction and destruction'}
procedure TMqttChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;
//  FMQTT.Create(function: TMQTT
//    begin
//      Result := TMQTT.Create(Settings.Broker, Settings.Port);
//      // some brokers require these to have a value
//      Result.WillTopic := 'a';
//      Result.WillMsg   := 'a';
//      Result.OnPublish := FMQTTPublish;
//    end
//  );
end;

destructor TMqttChannelReceiver.Destroy;
begin
//  if FMQTT.IsValueCreated then
//  begin
//    FMQTT.Value.Free; // we need to do an explicit call to Free
//  end;
  if Assigned(FMQTT) then
  begin
    FMQTT.OnPublish := nil;
    FreeAndNil(FMQTT);
  end;
  //FMQTT.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TMqttChannelReceiver.FMQTTPublish(Sender: TObject; ATopic,
  APayload: UTF8String);
begin
//  Logger.Send('ATopic', ATopic);
//  Logger.Send('APayload', APayload);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TMqttChannelReceiver.GetMQTT: TMQTT;
begin
  Result := FMQTT;
end;

procedure TMqttChannelReceiver.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    if Value {and not MQTT.Connected }then
    begin
      Connect;
      MQTT.Subscribe('#', 0);
    end
    else if not Value {and MQTT.Connected} then
    begin
      if Assigned(FMQTT) then
      begin
        MQTT.Disconnect;
      end;
    end;
  end;
  inherited SetEnabled(Value);
  //inherited SetEnabled(MQTT.Connected);
end;

function TMqttChannelReceiver.GetSettings: TMQTTSettings;
begin
  Result := Manager.Settings.MQTTSettings;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TMqttChannelReceiver.SettingsChanged(Sender: TObject);
begin

end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TMqttChannelReceiver.Connect;
begin
  if Assigned(FMQTT) then
  begin
    FreeAndNil(FMQTT);
  end;
  FMQTT := TMQTT.Create(Settings.Broker, Settings.Port);
  FMQTT.OnPublish := FMQTTPublish;
  FMQTT.WillTopic := 'a'; // required by some brokers
  FMQTT.WillMsg := 'a';   // required by some brokers
  if FMQTT.Connect then
  begin
//    FLogTree.Log('Socket connected to broker.', llInfo);
//    FLogTree.Header.AutoFitColumns;
//    if chkSubscribeToAllTopics.Checked then
//      SubscribeToAllTopics;
  end;
end;

function TMqttChannelReceiver.CreateSubscriber(ASourceId, AThreadId: UInt32;
  const ASourceName: string): ISubscriber;
begin
  //Result := TMQTTS WinODSSubscriber.Create(Self, ASourceId, '', ASourceName, True);
end;
{$ENDREGION}

end.
