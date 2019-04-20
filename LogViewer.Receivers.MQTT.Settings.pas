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

unit LogViewer.Receivers.MQTT.Settings;

{ Persistable settings for MQTT receiver. }

interface

uses
  System.Classes,

  Spring;

type
  TMQTTSettings = class(TPersistent)
  const
    DEFAULT_BROKER = 'localhost';
    DEFAULT_PORT   = 1883;

  private
    FOnChanged : Event<TNotifyEvent>;
    FEnabled   : Boolean;
    FEndpoints : TStrings;
    FBroker    : string;
    FPort      : Integer;

    procedure Changed;

    {$REGION 'property access methods'}
    function GetEnabled: Boolean;
    function GetOnChanged: IEvent<TNotifyEvent>;
    procedure SetEnabled(const Value: Boolean);
    function GetEndpoints: TStrings;
    function GetBroker: string;
    function GetPort: Integer;
    procedure SetBroker(const Value: string);
    procedure SetPort(const Value: Integer);
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Broker: string
      read GetBroker write SetBroker;

    property Port: Integer
      read GetPort write SetPort default DEFAULT_PORT;

    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property Endpoints: TStrings
      read GetEndpoints;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TMQTTSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FEndpoints := TStringList.Create;
  FPort      := DEFAULT_PORT;
  FBroker    := DEFAULT_BROKER;
end;

procedure TMQTTSettings.BeforeDestruction;
begin
  FEndpoints.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TMQTTSettings.GetBroker: string;
begin
  Result := FBroker;
end;

procedure TMQTTSettings.SetBroker(const Value: string);
begin
  if Value <> Broker then
  begin
    FBroker := Value;
    Changed;
  end;
end;

function TMQTTSettings.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TMQTTSettings.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

function TMQTTSettings.GetEndpoints: TStrings;
begin
  Result := FEndpoints;
end;

function TMQTTSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TMQTTSettings.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TMQTTSettings.SetPort(const Value: Integer);
begin
  if Value <> Port then
  begin
    FPort := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TMQTTSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TMQTTSettings.Assign(Source: TPersistent);
var
  LSettings: TMQTTSettings;
begin
  if Source is TMQTTSettings then
  begin
    LSettings       := TMQTTSettings(Source);
    Enabled         := LSettings.Enabled;
    Port            := LSettings.Port;
    Broker          := LSettings.Broker;
    //PollingInterval := LSettings.PollingInterval;
    FEndpoints.Assign(LSettings.Endpoints);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
