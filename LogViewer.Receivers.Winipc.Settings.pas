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

unit LogViewer.Receivers.Winipc.Settings;

{ Persistable settings for WinIPC receivers. }

interface

uses
  System.Classes,

  Spring;

type
  TWinipcSettings = class(TPersistent)
  const
    DEFAULT_POLLING_INTERVAL = 10;

  private
    FOnChanged       : Event<TNotifyEvent>;
    FEnabled         : Boolean;
    FPollingInterval : Integer;

  protected
    {$REGION 'property access methods'}
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetPollingInterval: Integer;
    procedure SetPollingInterval(const Value: Integer);
    {$ENDREGION}

    procedure Changed;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Enabled: Boolean
      read GetEnabled write SetEnabled;

    property PollingInterval: Integer // in ms
      read GetPollingInterval write SetPollingInterval
      default DEFAULT_POLLING_INTERVAL;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TWinipcSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FPollingInterval := DEFAULT_POLLING_INTERVAL;
  FOnChanged.UseFreeNotification := False;
end;

destructor TWinipcSettings.Destroy;
begin
  FOnChanged.RemoveAll(Self);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWinipcSettings.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TWinipcSettings.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

function TWinipcSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TWinipcSettings.GetPollingInterval: Integer;
begin
  Result := FPollingInterval;
end;

procedure TWinipcSettings.SetPollingInterval(const Value: Integer);
begin
  if Value <> PollingInterval then
  begin
    FPollingInterval := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TWinipcSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWinipcSettings.Assign(Source: TPersistent);
var
  LSettings: TWinipcSettings;
begin
  if Source is TWinipcSettings then
  begin
    LSettings := TWinipcSettings(Source);
    Enabled := LSettings.Enabled;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}
end.
