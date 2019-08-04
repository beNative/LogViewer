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

unit LogViewer.LogLevels.Settings;

{ Settings for log levels. }

interface

uses
  System.Classes, System.UITypes,

  Spring, Spring.Collections;

type
  TLogLevel = class(TPersistent)
  private
    FLevel     : Byte;
    FAlias     : string;
    FColor     : TColor;
    FOnChanged : Event<TNotifyEvent>;

  protected
    {$REGION 'property access methods'}
    function GetColorName: string;
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetLevel: Byte;
    function GetAlias: string;
    procedure SetAlias(const Value: string);
    {$ENDREGION}

    procedure Changed;

  public
    constructor Create(
      ALevel       : Byte;
      AColor       : TColor = TColors.SysNone;
      const AAlias : string = ''
    );

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property Level: Byte
      read GetLevel;

    property ColorName: string
      read GetColorName;

  published
    property Alias: string
      read GetAlias write SetAlias;

    property Color: TColor
      read GetColor write SetColor;
  end;

  TLogLevelSettings = class(TPersistent)
  private
    FLogLevels : IList<TLogLevel>;
    FOnChanged : Event<TNotifyEvent>;

    procedure FLogLevelChanged(Sender: TObject);

    function GetOnChanged: IEvent<TNotifyEvent>;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property LogLevels: IList<TLogLevel>
      read FLogLevels;
  end;

implementation

uses
  Vcl.Graphics, Vcl.GraphUtil,

  DDuce.Logger;

{$REGION 'TLogLevel'}
{$REGION 'construction and destruction'}
constructor TLogLevel.Create(ALevel: Byte; AColor: TColor;
  const AAlias: string);
begin
  FLevel := ALevel;
  FColor := AColor;
  FAlias := AAlias;
  FOnChanged.UseFreeNotification := False;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogLevel.GetAlias: string;
begin
  Result := FAlias;
end;

procedure TLogLevel.SetAlias(const Value: string);
begin
  if Value <> Alias then
  begin
    FAlias := Value;
    Changed;
  end;
end;

function TLogLevel.GetColor: TColor;
begin
  Result := FColor;
end;

procedure TLogLevel.SetColor(const Value: TColor);
begin
  if Color <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

function TLogLevel.GetColorName: string;
begin
  //Result := ColorToString(FColor);
  Result := ColorToWebColorName(FColor);
end;

function TLogLevel.GetLevel: Byte;
begin
  Result := FLevel;
end;

function TLogLevel.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TLogLevel.Changed;
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
  B  : Byte;
  LL : TLogLevel;
begin
  inherited AfterConstruction;
  FLogLevels := TCollections.CreateObjectList<TLogLevel>;
  FOnChanged.UseFreeNotification := False;
  for B := 0 to 255 do
  begin
    LL := TLogLevel.Create(B);
    LL.OnChanged.Add(FLogLevelChanged);
    FLogLevels.Add(LL);
  end;
end;

destructor TLogLevelSettings.Destroy;
var
  B  : Byte;
  LL : TLogLevel;
begin
  for B := 0 to 255 do
  begin
    LL := LogLevels[B];
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
  if FOnChanged.CanInvoke then
  begin
    Logger.Send('Invoked');
    FOnChanged.Invoke(Self);
  end;
end;
{$ENDREGION}

{$ENDREGION}

end.
