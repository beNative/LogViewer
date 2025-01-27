{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.LogLevels.Settings;

{ Settings for log levels. }

interface

uses
  System.Classes, System.UITypes,

  Spring, Spring.Collections,

  DDuce.Logger.Interfaces;

type
  TLogLevelProperties = class(TPersistent)
  private
    FLevel     : TLogMessageLevel;
    FAlias     : string;
    FColor     : TColor;
    FOnChanged : Event<TNotifyEvent>;

  protected
    {$REGION 'property access methods'}
    procedure SetLevel(const Value: TLogMessageLevel);
    function GetColorName: string;
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetLevel: TLogMessageLevel;
    function GetAlias: string;
    procedure SetAlias(const Value: string);
    {$ENDREGION}

    procedure Changed;

  public
    constructor Create(
      ALevel       : TLogMessageLevel;
      AColor       : TColor = TColors.SysNone;
      const AAlias : string = ''
    );

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property ColorName: string
      read GetColorName;

  published
    property Alias: string
      read GetAlias write SetAlias;

    property Color: TColor
      read GetColor write SetColor;

    property Level: TLogMessageLevel
      read GetLevel write SetLevel;
  end;

  TLogLevelSettings = class(TPersistent)
  private
    FLogLevels : IList<TLogLevelProperties>;
    FOnChanged : Event<TNotifyEvent>;

    procedure FLogLevelChanged(Sender: TObject);

    function GetOnChanged: IEvent<TNotifyEvent>;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property LogLevels: IList<TLogLevelProperties>
      read FLogLevels;
  end;

implementation

uses
  Vcl.Graphics, Vcl.GraphUtil,

  DDuce.Logger;

{$REGION 'TLogLevel'}
{$REGION 'construction and destruction'}
constructor TLogLevelProperties.Create(ALevel: TLogMessageLevel; AColor: TColor;
  const AAlias: string);
begin
  FLevel := ALevel;
  FColor := AColor;
  FAlias := AAlias;
  FOnChanged.UseFreeNotification := False;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogLevelProperties.GetAlias: string;
begin
  Result := FAlias;
end;

procedure TLogLevelProperties.SetAlias(const Value: string);
begin
  if Value <> Alias then
  begin
    FAlias := Value;
    Changed;
  end;
end;

function TLogLevelProperties.GetColor: TColor;
begin
  Result := FColor;
end;

procedure TLogLevelProperties.SetColor(const Value: TColor);
begin
  if Color <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

function TLogLevelProperties.GetColorName: string;
begin
  Result := ColorToWebColorName(FColor);
end;

function TLogLevelProperties.GetLevel: TLogMessageLevel;
begin
  Result := FLevel;
end;

procedure TLogLevelProperties.SetLevel(const Value: TLogMessageLevel);
begin
  if Level <> Value then
  begin
    FLevel := Value;
    Changed;
  end;
end;

function TLogLevelProperties.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogLevelProperties.Assign(Source: TPersistent);
var
  LLogLevel: TLogLevelProperties;
begin
  if Source is TLogLevelProperties then
  begin
    LLogLevel := TLogLevelProperties(Source);
    Level     := LLogLevel.Level;
    Alias     := LLogLevel.Alias;
    FColor    := LLogLevel.Color;
  end
  else
    inherited Assign(Source);
end;

procedure TLogLevelProperties.Changed;
begin
  if FOnChanged.CanInvoke then
    FOnChanged.Invoke(Self);
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TLogLevelSettings'}
{$REGION 'construction and destruction'}
procedure TLogLevelSettings.AfterConstruction;
var
  LL  : TLogLevelProperties;
  LML : TLogMessageLevel;
begin
  inherited AfterConstruction;
  FLogLevels := TCollections.CreateObjectList<TLogLevelProperties>;
  FOnChanged.UseFreeNotification := False;
  for LML := Low(TLogMessageLevel) to High(TLogMessageLevel) do
  begin
    LL := TLogLevelProperties.Create(LML);
    LL.OnChanged.Add(FLogLevelChanged);
    FLogLevels.Add(LL);
  end;
end;

destructor TLogLevelSettings.Destroy;
var
  LML : TLogMessageLevel;
  LL  : TLogLevelProperties;
begin
  for LML := Low(TLogMessageLevel) to High(TLogMessageLevel) do
  begin
    LL := LogLevels[LML];
    LL.OnChanged.Remove(FLogLevelChanged);
  end;
  FLogLevels := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogLevelSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TLogLevelSettings.FLogLevelChanged(Sender: TObject);
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}
{$ENDREGION}

end.
