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

unit LogViewer.Settings;

{ Application settings data. }

interface

uses
  System.Classes,

  DDuce.FormSettings,

  LogViewer.MessageList.Settings, LogViewer.Watches.Settings,
  LogViewer.ComPort.Settings, LogViewer.WinODS.Settings,
  LogViewer.WinIPC.Settings;

type
  TLogViewerSettings = class(TPersistent)
  private
    FFormSettings        : TFormSettings;
    FFileName            : string;
    FLeftPanelWidth      : Integer;
    FRightPanelWidth     : Integer;
    FMessageListSettings : TMessageListSettings;
    FWinODSSettings      : TWinODSSettings;
    FWinIPCSettings      : TWinIPCSettings;
    FComPortSettings     : TComPortSettings;
    FWatchSettings       : TWatchSettings;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Load;
    procedure Save;

    property FormSettings: TFormSettings
      read FFormSettings;

    property WinODSSettings: TWinODSSettings
      read FWinODSSettings;

    property WinIPCSettings: TWinIPCSettings
      read FWinIPCSettings;

    property ComPortSettings: TComPortSettings
      read FComPortSettings;

    property WatchSettings: TWatchSettings
      read FWatchSettings;

    property MessageListSettings: TMessageListSettings
      read FMessageListSettings;

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
  FMessageListSettings := TMessageListSettings.Create;
  FWinODSSettings := TWinODSSettings.Create;
  FWinIPCSettings := TWinIPCSettings.Create;
  FComPortSettings := TComPortSettings.Create;
  FWatchSettings := TWatchSettings.Create;
end;

procedure TLogViewerSettings.BeforeDestruction;
begin
  FreeAndNil(FFormSettings);
  FreeAndNil(FMessageListSettings);
  FreeAndNil(FWinODSSettings);
  FreeAndNil(FWinIPCSettings);
  FreeAndNil(FComPortSettings);
  FreeAndNil(FWatchSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TLogViewerSettings.Load;
var
  JO  : TJsonObject;
begin
  if FileExists(FFileName) then
  begin
    JO := TJsonObject.Create;
    try
      JO.LoadFromFile(FFileName);
      JO['FormSettings'].ObjectValue.ToSimpleObject(FFormSettings);
      JO['MessageListSettings'].ObjectValue.ToSimpleObject(FMessageListSettings);
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
    JO['MessageListSettings'].ObjectValue.FromSimpleObject(FMessageListSettings);
    JO.SaveToFile(FFileName, False);
  finally
    JO.Free;
  end;
end;
{$ENDREGION}

end.
