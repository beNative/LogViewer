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

unit LogViewer.Receivers.MQTT;

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

  LogViewer.Interfaces,  LogViewer.Receivers.Base;

{$REGION 'documentation'}
{$ENDREGION}

type
  TMQTTChannelReceiver = class(TChannelReceiver, IChannelReceiver, IMQTT)
  private
    FMQTT: TMQTT;

  protected
    {$REGION 'property access methods'}
    //function GetSettings: TMQTTSettings;
    procedure SetEnabled(const Value: Boolean); override;
    {$ENDREGION}

    //procedure SettingsChanged(Sender: TObject);

    function CreateSubscriber(
      ASourceId         : UInt32;
      AThreadId         : UInt32;
      const ASourceName : string
    ): ISubscriber; override;

  public
    procedure AfterConstruction; override;

    constructor Create(
      AManager    : ILogViewerManager;
      AMQTT       : TMQTT;
      const AName : string
    ); reintroduce;
//
//    property Settings: TMQTTSettings
//      read GetSettings;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TMQTTChannelReceiver.AfterConstruction;
begin
  inherited AfterConstruction;

end;
{$ENDREGION}

{$REGION 'protected methods'}
constructor TMQTTChannelReceiver.Create(AManager: ILogViewerManager;
  AMQTT: TMQTT; const AName: string);
begin
  inherited Create(AManager, AName);
  FMQTT := AMQTT;
end;

function TMQTTChannelReceiver.CreateSubscriber(ASourceId, AThreadId: UInt32;
  const ASourceName: string): ISubscriber;
begin

end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TMQTTChannelReceiver.SetEnabled(const Value: Boolean);
begin
  inherited;

end;
{$ENDREGION}

end.



