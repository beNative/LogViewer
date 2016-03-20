{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Settings;

interface

uses
  System.Classes,

  DDuce.FormSettings;

type
  TLogViewerSettings = class(TPersistent)
  private
    FFormSettings    : TFormSettings;
    FFileName        : string;
    FLeftPanelWidth  : Integer;
    FRightPanelWidth : Integer;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Load;
    procedure Save;

    property FormSettings: TFormSettings
      read FFormSettings;

  published
    property LeftPanelWidth: Integer
      read FLeftPanelWidth write FLeftPanelWidth;

    property RightPanelWidth: Integer
      read FRightPanelWidth write FRightPanelWidth;
  end;

implementation

uses
  System.SysUtils,

  JsonDataObjects;

{$REGION 'construction and destruction'}
procedure TLogViewerSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FFileName := 'settings.json';
  FFormSettings := TFormSettings.Create;
end;

procedure TLogViewerSettings.BeforeDestruction;
begin
  FreeAndNil(FFormSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TLogViewerSettings.Load;
var
  JDO : TJsonDataValueHelper;
  JO  : TJsonObject;
begin
  if FileExists(FFileName) then
  begin
    JO := TJsonObject.Create;
    try
      JO.LoadFromFile(FFileName);
      JO['FormSettings'].ObjectValue.ToSimpleObject(FFormSettings);
      JO.ToSimpleObject(Self);
    finally
      JO.Free;
    end;
  end;
end;

procedure TLogViewerSettings.Save;
var
  JO  : TJsonObject;
begin
  JO := TJsonObject.Create;
  try
    JO.FromSimpleObject(Self);
    JO['FormSettings'].ObjectValue.FromSimpleObject(FFormSettings);

    JO.SaveToFile(FFileName, False);
  finally
    JO.Free;
  end;
end;
{$ENDREGION}

end.
