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

unit LogViewer.WinIPC.Settings;

interface

uses
  System.Classes,

  Spring;

type
  TWinIPCSettings = class(TPersistent)
  private
    FOnChanged : Event<TNotifyEvent>;
    FEnabled   : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    {$ENDREGION}

    procedure Changed;

  public
    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Enabled: Boolean
      read GetEnabled write SetEnabled;
  end;

implementation

{$REGION 'property access methods'}
function TWinIPCSettings.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TWinIPCSettings.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

function TWinIPCSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TWinIPCSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWinIPCSettings.Assign(Source: TPersistent);
var
  LSettings: TWinIPCSettings;
begin
  if Source is TWinIPCSettings then
  begin
    LSettings := TWinIPCSettings(Source);
    Enabled := LSettings.Enabled;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}
end.
