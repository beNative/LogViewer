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

unit LogViewer.ZeroMQ.Settings;

interface

uses
  System.Classes,

  Spring;

type
  TZeroMQSettings = class(TPersistent)
  private
    FOnChanged : Event<TNotifyEvent>;
    FAddress   : string;
    FEnabled   : Boolean;

  protected
    {$REGION 'property access methods'}
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetAddress: string;
    procedure SetAddress(const Value: string);
    {$ENDREGION}

    procedure Changed;

  public
    procedure Assign(Source: TPersistent); override;

    property Address: string
      read GetAddress write SetAddress;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Enabled: Boolean
      read GetEnabled write SetEnabled;
  end;

implementation

{$REGION 'property access methods'}
function TZeroMQSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TZeroMQSettings.GetAddress: string;
begin
  Result := FAddress;
end;

procedure TZeroMQSettings.SetAddress(const Value: string);
begin
  if Value <> Address then
  begin
    FAddress := Value;
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
    LSettings := TZeroMQSettings(Source);
    Address  := LSettings.Address;
    Enabled  := LSettings.Enabled;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
