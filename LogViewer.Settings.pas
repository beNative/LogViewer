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

unit LogViewer.Settings;

{ Persistable application settings. }

interface

uses
  System.Classes,

  Spring,

  DDuce.Settings.Form,

  LogViewer.MessageList.Settings, LogViewer.Watches.Settings,
  LogViewer.Receivers.ComPort.Settings, LogViewer.Receivers.WinODS.Settings,
  LogViewer.Receivers.WinIPC.Settings, LogViewer.Receivers.ZeroMQ.Settings,
  LogViewer.Receivers.MQTT.Settings, LogViewer.Receivers.FileSystem.Settings,
  LogViewer.DisplayValues.Settings;

type
  TLogViewerSettings = class(TPersistent)
  private
    FFormSettings          : TFormSettings;
    FFileName              : string;
    FMessageListSettings   : TMessageListSettings;
    FWinODSSettings        : TWinODSSettings;
    FWinIPCSettings        : TWinIPCSettings;
    FComPortSettings       : TComPortSettings;
    FZeroMQSettings        : TZeroMQSettings;
    FMQTTSettings          : TMQTTSettings;
    FFileSystemSettings    : TFileSystemSettings;
    FWatchSettings         : TWatchSettings;
    FDisplayValuesSettings : TDisplayValuesSettings;
    FOnChanged             : Event<TNotifyEvent>;

    procedure FormSettingsChanged(Sender: TObject);
    procedure DisplayValuesSettingsChanged(Sender: TObject);

  protected
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetFileName: string;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Changed;
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

    property ZeroMQSettings: TZeroMQSettings
      read FZeroMQSettings;

    property MQTTSettings: TMQTTSettings
      read FMQTTSettings;

    property FileSystemSettings: TFileSystemSettings
      read FFileSystemSettings;

    property WatchSettings: TWatchSettings
      read FWatchSettings;

    property MessageListSettings: TMessageListSettings
      read FMessageListSettings;

    property DisplayValuesSettings: TDisplayValuesSettings
      read FDisplayValuesSettings;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property FileName: string
      read GetFileName;
  end;

implementation

uses
  System.SysUtils,

  JsonDataObjects;

{$REGION 'construction and destruction'}
procedure TLogViewerSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FFileName              := 'settings.json';
  FFormSettings          := TFormSettings.Create;
  FFormSettings.OnChanged.Add(FormSettingsChanged);
  FMessageListSettings   := TMessageListSettings.Create;
  FWinODSSettings        := TWinODSSettings.Create;
  FWinIPCSettings        := TWinIPCSettings.Create;
  FComPortSettings       := TComPortSettings.Create;
  FZeroMQSettings        := TZeroMQSettings.Create;
  FMQTTSettings          := TMQTTSettings.Create;
  FFileSystemSettings    := TFileSystemSettings.Create;
  FWatchSettings         := TWatchSettings.Create;
  FDisplayValuesSettings := TDisplayValuesSettings.Create;
  FDisplayValuesSettings.OnChanged.Add(DisplayValuesSettingsChanged);
end;

procedure TLogViewerSettings.BeforeDestruction;
begin
  FDisplayValuesSettings.OnChanged.Remove(DisplayValuesSettingsChanged);
  FreeAndNil(FDisplayValuesSettings);
  FreeAndNil(FWatchSettings);
  FreeAndNil(FZeroMQSettings);
  FreeAndNil(FMQTTSettings);
  FreeAndNil(FFileSystemSettings);
  FreeAndNil(FComPortSettings);
  FreeAndNil(FWinIPCSettings);
  FreeAndNil(FWinODSSettings);
  FreeAndNil(FMessageListSettings);
  FFormSettings.OnChanged.Remove(FormSettingsChanged);
  FreeAndNil(FFormSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogViewerSettings.GetFileName: string;
begin
  Result := FFileName;
end;

function TLogViewerSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TLogViewerSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TLogViewerSettings.DisplayValuesSettingsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TLogViewerSettings.FormSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TLogViewerSettings.Load;
var
  JO : TJsonObject;
begin
  if FileExists(FFileName) then
  begin
    JO := TJsonObject.Create;
    try
      JO.LoadFromFile(FFileName);
      JO['FormSettings'].ObjectValue.ToSimpleObject(FFormSettings);
      JO['MessageListSettings'].ObjectValue.ToSimpleObject(FMessageListSettings);
      JO['WinODSSettings'].ObjectValue.ToSimpleObject(FWinODSSettings);
      JO['WinIPCSettings'].ObjectValue.ToSimpleObject(FWinIPCSettings);
      JO['ZeroMQSettings'].ObjectValue.ToSimpleObject(FZeroMQSettings);
      FZeroMQSettings.Endpoints.Text :=
        JO['ZeroMQSettings'].ObjectValue['Endpoints'].Value;
      JO['MQTTSettings'].ObjectValue.ToSimpleObject(FMQTTSettings);
      FMQTTSettings.Endpoints.Text :=
        JO['MQTTSettings'].ObjectValue['Endpoints'].Value;
      JO['FileSystemSettings'].ObjectValue.ToSimpleObject(FFileSystemSettings);
      FFileSystemSettings.PathNames.Text :=
        JO['FileSystemSettings'].ObjectValue['PathNames'].Value;
      JO['DisplayValueSettings'].ObjectValue['Id'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Id);
      JO['DisplayValueSettings'].ObjectValue['Info'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Info);
      JO['DisplayValueSettings'].ObjectValue['Warning'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Warning);
      JO['DisplayValueSettings'].ObjectValue['Error'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Error);
      JO['DisplayValueSettings'].ObjectValue['TimeStamp'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.TimeStamp);
      JO['DisplayValueSettings'].ObjectValue['ValueName'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.ValueName);
      JO['DisplayValueSettings'].ObjectValue['ValueType'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.ValueType);
      JO['DisplayValueSettings'].ObjectValue['Value'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Value);
      JO['DisplayValueSettings'].ObjectValue['CheckPoint'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.CheckPoint);
      JO['DisplayValueSettings'].ObjectValue['Counter'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Counter);
      JO['DisplayValueSettings'].ObjectValue['Tracing'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Tracing);
      JO['DisplayValueSettings'].ObjectValue['Enter'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Enter);
      JO['DisplayValueSettings'].ObjectValue['Leave'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Leave);
      JO['DisplayValueSettings'].ObjectValue['Conditional'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Conditional);
      JO.ToSimpleObject(Self);
    finally
      JO.Free;
    end;
  end;
end;

procedure TLogViewerSettings.Save;
var
  JO : TJsonObject;
begin
  JO := TJsonObject.Create;
  try
    JO.FromSimpleObject(Self);
    JO['FormSettings'].ObjectValue.FromSimpleObject(FFormSettings);
    JO['MessageListSettings'].ObjectValue.FromSimpleObject(FMessageListSettings);
    JO['WinODSSettings'].ObjectValue.FromSimpleObject(FWinODSSettings);
    JO['WinIPCSettings'].ObjectValue.FromSimpleObject(FWinIPCSettings);
    JO['ZeroMQSettings'].ObjectValue.FromSimpleObject(FZeroMQSettings);
    JO['ZeroMQSettings'].ObjectValue['Endpoints'].Value :=
      FZeroMQSettings.Endpoints.Text;
    JO['MQTTSettings'].ObjectValue.FromSimpleObject(FMQTTSettings);
    JO['MQTTSettings'].ObjectValue['Endpoints'].Value :=
      FMQTTSettings.Endpoints.Text;
    JO['FileSystemSettings'].ObjectValue.FromSimpleObject(FFileSystemSettings);
    JO['FileSystemSettings'].ObjectValue['PathNames'].Value :=
      FileSystemSettings.PathNames.Text;
    JO['DisplayValueSettings'].ObjectValue['Id'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Id);
    JO['DisplayValueSettings'].ObjectValue['Info'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Info);
    JO['DisplayValueSettings'].ObjectValue['Warning'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Warning);
    JO['DisplayValueSettings'].ObjectValue['Error'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Error);
    JO['DisplayValueSettings'].ObjectValue['TimeStamp'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.TimeStamp);
    JO['DisplayValueSettings'].ObjectValue['ValueName'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.ValueName);
    JO['DisplayValueSettings'].ObjectValue['ValueType'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.ValueType);
    JO['DisplayValueSettings'].ObjectValue['Value'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Value);
    JO['DisplayValueSettings'].ObjectValue['CheckPoint'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.CheckPoint);
    JO['DisplayValueSettings'].ObjectValue['Counter'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Counter);
    JO['DisplayValueSettings'].ObjectValue['Tracing'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Tracing);
    JO['DisplayValueSettings'].ObjectValue['Enter'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Enter);
    JO['DisplayValueSettings'].ObjectValue['Leave'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Leave);
    JO['DisplayValueSettings'].ObjectValue['Conditional'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Conditional);
    JO.SaveToFile(FFileName, False);
  finally
    JO.Free;
  end;
end;
{$ENDREGION}

end.
