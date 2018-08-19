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

{ Persistable settings for ZeroMQ receivers. }

interface

uses
  System.Classes,

  Spring;

type
  TZeroMQSettings = class(TPersistent)
  private
    FOnChanged     : Event<TNotifyEvent>;
    FAddress       : string;
    FEnabled       : Boolean;
    FPort          : Integer;
    FSubscriptions : TStrings;

  protected
    {$REGION 'property access methods'}
    function GetSubscriptions: TStrings;
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetAddress: string;
    procedure SetAddress(const Value: string);
    {$ENDREGION}

    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Assign(Source: TPersistent); override;

    property Subscriptions: TStrings
      read GetSubscriptions;

    property Address: string
      read GetAddress write SetAddress;

    property Port: Integer
      read GetPort write SetPort;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Enabled: Boolean
      read GetEnabled write SetEnabled;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TZeroMQSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FSubscriptions := TStringList.Create;
end;

procedure TZeroMQSettings.BeforeDestruction;
begin
  FSubscriptions.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZeroMQSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TZeroMQSettings.GetPort: Integer;
begin
  Result := FPort;
end;

function TZeroMQSettings.GetSubscriptions: TStrings;
begin
  Result := FSubscriptions;
end;

procedure TZeroMQSettings.SetPort(const Value: Integer);
begin
  if Value <> Port then
  begin
    FPort := Value;
    Changed;
  end;
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
    Address   := LSettings.Address;
    Port      := LSettings.Port;
    Enabled   := LSettings.Enabled;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
