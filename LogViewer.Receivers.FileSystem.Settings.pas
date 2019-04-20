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

unit LogViewer.Receivers.FileSystem.Settings;

interface

uses
  System.Classes,

  Spring;

type
  TFileSystemSettings = class(TPersistent)
  private
    FOnChanged : Event<TNotifyEvent>;
    FEnabled   : Boolean;
    FPathNames : TStrings;

  protected
    {$REGION 'property access methods'}
    function GetPathNames: TStrings;
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

    property PathNames: TStrings
      read GetPathNames;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TFileSystemSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FPathNames := TStringList.Create;
end;

procedure TFileSystemSettings.BeforeDestruction;
begin
  FPathNames.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TFileSystemSettings.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TFileSystemSettings.SetEnabled(const Value: Boolean);
begin
  if Value <> Enabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

function TFileSystemSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TFileSystemSettings.GetPathNames: TStrings;
begin
  Result := FPathNames;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TFileSystemSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TFileSystemSettings.Assign(Source: TPersistent);
var
  LSettings: TFileSystemSettings;
begin
  if Source is TFileSystemSettings then
  begin
    LSettings := TFileSystemSettings(Source);
    Enabled   := LSettings.Enabled;
    FPathNames.Assign(LSettings.PathNames);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}
end.
