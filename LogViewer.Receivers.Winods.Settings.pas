{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Receivers.Winods.Settings;

{ Persistable settings for Winods receivers. }

interface

uses
  System.Classes,

  Spring;

type
  TWinodsSettings = class(TPersistent)
  private
    FOnChanged   : Event<TNotifyEvent>;
    FProcessId   : Integer;
    FProcessName : string;
    FEnabled     : Boolean;

    {$REGION 'property access methods'}
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetProcessId: Integer;
    function GetProcessName: string;
    procedure SetProcessId(const Value: Integer);
    procedure SetProcessName(const Value: string);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    {$ENDREGION}

  protected
    procedure Changed;

  public
    procedure Assign(Source: TPersistent); override;

    property ProcessName: string
      read GetProcessName write SetProcessName;

    property ProcessId: Integer
      read GetProcessId write SetProcessId;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Enabled: Boolean
      read GetEnabled write SetEnabled;
  end;

implementation

uses
  DDuce.Utils.Winapi;

{$REGION 'property access methods'}
function TWinodsSettings.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TWinodsSettings.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

function TWinodsSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TWinodsSettings.GetProcessId: Integer;
begin
  Result := FProcessId;
end;


procedure TWinodsSettings.SetProcessId(const Value: Integer);
begin
  if Value <> ProcessId then
  begin
    FProcessId := Value;
    Changed;
  end;
end;

function TWinodsSettings.GetProcessName: string;
begin
  Result := FProcessName;
end;

procedure TWinodsSettings.SetProcessName(const Value: string);
begin
  if Value <> ProcessName then
  begin
    FProcessName := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TWinodsSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWinodsSettings.Assign(Source: TPersistent);
var
  LSettings: TWinodsSettings;
begin
  if Source is TWinodsSettings then
  begin
    LSettings   := TWinodsSettings(Source);
    ProcessName := LSettings.ProcessName;
    ProcessId   := LSettings.ProcessId;
    Enabled     := LSettings.Enabled;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}
end.
