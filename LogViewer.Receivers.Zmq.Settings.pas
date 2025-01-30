{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Receivers.Zmq.Settings;

{ Persistable settings for ZeroMQ receiver. }

interface

uses
  System.Classes,

  Spring,

  DDuce.Logger.Interfaces;

type
  TZmqSettings = class(TPersistent)
  const
    DEFAULT_POLLING_INTERVAL = 100;
    DEFAULT_POLLING_TIMEOUT  = 10;

  private
    FOnChanged        : Event<TNotifyEvent>;
    FEnabled          : Boolean;
    FEndpoints        : TStrings;
    FPollingInterval  : Integer;
    FPollingTimeout   : Integer;
    FLogMessageTypes  : TLogMessageTypes;
    FLogMessageLevels : TLogMessageLevels;

  protected
    {$REGION 'property access methods'}
    function GetPollingTimeout: Integer;
    procedure SetPollingTimeout(const Value: Integer);
    function GetPollingInterval: Integer;
    procedure SetPollingInterval(const Value: Integer);
    function GetEndpoints: TStrings;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetLogMessageLevels: TLogMessageLevels;
    function GetLogMessageTypes: TLogMessageTypes;
    procedure SetLogMessageLevels(const Value: TLogMessageLevels);
    procedure SetLogMessageTypes(const Value: TLogMessageTypes);
    {$ENDREGION}

    procedure Changed;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property LogMessageTypes: TLogMessageTypes
      read GetLogMessageTypes write SetLogMessageTypes;

    property LogMessageLevels: TLogMessageLevels
      read GetLogMessageLevels write SetLogMessageLevels;

  published
    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property PollingInterval: Integer // in ms
      read GetPollingInterval write SetPollingInterval
      default DEFAULT_POLLING_INTERVAL;

    property PollingTimeout: Integer // in ms
      read GetPollingTimeout write SetPollingTimeout
      default DEFAULT_POLLING_TIMEOUT;

    property Endpoints: TStrings
      read GetEndpoints;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TZmqSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FEndpoints        := TStringList.Create;
  FPollingInterval  := DEFAULT_POLLING_INTERVAL;
  FPollingTimeout   := DEFAULT_POLLING_TIMEOUT;
  FLogMessageTypes  := AllMessages;
  FLogMessageLevels := AllLevels;
end;

destructor TZmqSettings.Destroy;
begin
  FEndpoints.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZmqSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TZmqSettings.GetEndpoints: TStrings;
begin
  Result := FEndpoints;
end;

function TZmqSettings.GetLogMessageLevels: TLogMessageLevels;
begin
  Result := FLogMessageLevels;
end;

procedure TZmqSettings.SetLogMessageLevels(const Value: TLogMessageLevels);
begin
  if Value <> LogMessageLevels then
  begin
    FLogMessageLevels := Value;
    Changed;
  end;
end;

function TZmqSettings.GetLogMessageTypes: TLogMessageTypes;
begin
  Result := FLogMessageTypes;
end;

procedure TZmqSettings.SetLogMessageTypes(const Value: TLogMessageTypes);
begin
  if Value <> LogMessageTypes then
  begin
    FLogMessageTypes := Value;
    Changed;
  end;
end;

function TZmqSettings.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

procedure TZmqSettings.SetPollingInterval(const Value: Integer);
begin
  if Value <> PollingInterval then
  begin
    FPollingInterval := Value;
    Changed;
  end;
end;

function TZmqSettings.GetPollingTimeout: Integer;
begin
  Result := FPollingTimeout;
end;

procedure TZmqSettings.SetPollingTimeout(const Value: Integer);
begin
  if Value <> PollingTimeout then
  begin
    FPollingTimeout := Value;
    Changed;
  end;
end;

function TZmqSettings.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TZmqSettings.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZmqSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TZmqSettings.Assign(Source: TPersistent);
var
  LSettings: TZmqSettings;
begin
  if Source is TZmqSettings then
  begin
    LSettings        := TZmqSettings(Source);
    Enabled          := LSettings.Enabled;
    PollingInterval  := LSettings.PollingInterval;
    PollingTimeout   := LSettings.PollingTimeout;
    LogMessageTypes  := LSettings.LogMessageTypes;
    LogMessageLevels := LSettings.LogMessageLevels;
    FEndpoints.Assign(LSettings.Endpoints);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
