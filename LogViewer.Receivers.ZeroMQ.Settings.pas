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

unit LogViewer.Receivers.ZeroMQ.Settings;

{ Persistable settings for ZeroMQ receiver. }

interface

uses
  System.Classes,

  Spring;

type
  TZeroMQSettings = class(TPersistent)
  const
    DEFAULT_POLLING_INTERVAL = 100;

  private
    FOnChanged       : Event<TNotifyEvent>;
    FEnabled         : Boolean;
    FEndpoints       : TStrings;
    FPollingInterval : Integer;

  protected
    {$REGION 'property access methods'}
    function GetPollingInterval: Integer;
    procedure SetPollingInterval(const Value: Integer);
    function GetEndpoints: TStrings;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    {$ENDREGION}

    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property PollingInterval: Integer // in ms
      read GetPollingInterval write SetPollingInterval
      default DEFAULT_POLLING_INTERVAL;

    property Endpoints: TStrings
      read GetEndpoints;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TZeroMQSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FEndpoints   := TStringList.Create;
  FPollingInterval := DEFAULT_POLLING_INTERVAL;
end;

procedure TZeroMQSettings.BeforeDestruction;
begin
  FEndpoints.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZeroMQSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TZeroMQSettings.GetEndpoints: TStrings;
begin
  Result := FEndpoints;
end;

function TZeroMQSettings.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

procedure TZeroMQSettings.SetPollingInterval(const Value: Integer);
begin
  if Value <> PollingInterval then
  begin
    FPollingInterval := Value;
    Changed;
  end;
end;

function TZeroMQSettings.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TZeroMQSettings.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZeroMQSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TZeroMQSettings.Assign(Source: TPersistent);
var
  LSettings: TZeroMQSettings;
begin
  if Source is TZeroMQSettings then
  begin
    LSettings       := TZeroMQSettings(Source);
    Enabled         := LSettings.Enabled;
    PollingInterval := LSettings.PollingInterval;
    FEndpoints.Assign(LSettings.Endpoints);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
