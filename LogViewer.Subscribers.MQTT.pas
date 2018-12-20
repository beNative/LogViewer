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


unit LogViewer.Subscribers.MQTT;

interface

uses
  System.Classes,

  MQTT,

  Spring,

  LogViewer.Interfaces, LogViewer.Subscribers.Base;

type
  TMQTTSubscriber = class(TSubscriber, ISubscriber, IMQTT)
  private
    FMQTT : TMQTT;

    //procedure CreateSubscriberSocket(const AEndPoint: string);

  protected
    {$REGION 'property access methods'}
    procedure SetEnabled(const Value: Boolean); override;
    function GetSourceId: UInt32; override;
    {$ENDREGION}

    procedure Poll; override;

  public
    constructor Create(
      const AReceiver   : IChannelReceiver;
      AMQTT             : TMQTT;
      ASourceId         : UInt32;
      const AKey        : string;
      const ASourceName : string;
      AEnabled          : Boolean
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TMQTTSubscriber.AfterConstruction;
begin
  inherited AfterConstruction;

end;

procedure TMQTTSubscriber.BeforeDestruction;
begin
  inherited BeforeDestruction;

end;

constructor TMQTTSubscriber.Create(const AReceiver: IChannelReceiver;
  AMQTT: TMQTT; ASourceId: UInt32; const AKey, ASourceName: string;
  AEnabled: Boolean);
begin
  inherited Create(AReceiver, ASourceId, AKey, ASourceName, AEnabled);
  FMQTT := AMQTT;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TMQTTSubscriber.SetEnabled(const Value: Boolean);
begin
  inherited;

end;
{$ENDREGION}

{$REGION 'protected methods'}
function TMQTTSubscriber.GetSourceId: UInt32;
begin

end;

procedure TMQTTSubscriber.Poll;
begin
  inherited;

end;
{$ENDREGION}

end.
