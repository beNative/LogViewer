{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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
  LogViewer.CallStack.Settings, LogViewer.DisplayValues.Settings,
  LogViewer.Receivers.ComPort.Settings, LogViewer.Receivers.Winods.Settings,
  LogViewer.Receivers.Winipc.Settings, LogViewer.Receivers.ZeroMQ.Settings,
  LogViewer.Receivers.FileSystem.Settings, LogViewer.LogLevels.Settings;

type
  TLogViewerSettings = class(TPersistent)
  private
    FFileName              : string;
    FFormSettings          : TFormSettings;
    FMessageListSettings   : TMessageListSettings;
    FDebugMode             : Boolean;
    FEmitLogMessages       : Boolean;

    FWinodsSettings        : TWinodsSettings;
    FWinipcSettings        : TWinipcSettings;
    FComPortSettings       : TComPortSettings;
    FZeroMQSettings        : TZeroMQSettings;
    FFileSystemSettings    : TFileSystemSettings;

    FWatchSettings         : TWatchSettings;
    FCallStackSettings     : TCallStackSettings;
    FDisplayValuesSettings : TDisplayValuesSettings;
    FLogLevelSettings      : TLogLevelSettings;

    FOnChanged             : Event<TNotifyEvent>;

    procedure FormSettingsChanged(Sender: TObject);
    procedure DisplayValuesSettingsChanged(Sender: TObject);
    function GetEmitLogMessages: Boolean;
    procedure SetEmitLogMessages(const Value: Boolean);

  protected
    {$REGION 'property access methods'}
    function GetDebugMode: Boolean;
    procedure SetDebugMode(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetFileName: string;
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Changed;
    procedure Load;
    procedure Save;

    property FormSettings: TFormSettings
      read FFormSettings;

    property WinodsSettings: TWinodsSettings
      read FWinodsSettings;

    property WinipcSettings: TWinipcSettings
      read FWinipcSettings;

    property ComPortSettings: TComPortSettings
      read FComPortSettings;

    property ZeroMQSettings: TZeroMQSettings
      read FZeroMQSettings;

    property FileSystemSettings: TFileSystemSettings
      read FFileSystemSettings;

    property WatchSettings: TWatchSettings
      read FWatchSettings;

    property CallStackSettings: TCallStackSettings
      read FCallStackSettings;

    property MessageListSettings: TMessageListSettings
      read FMessageListSettings;

    property DisplayValuesSettings: TDisplayValuesSettings
      read FDisplayValuesSettings;

    property LogLevelSettings: TLogLevelSettings
      read FLogLevelSettings;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property FileName: string
      read GetFileName;

  published
    property DebugMode: Boolean
      read GetDebugMode write SetDebugMode;

    property EmitLogMessages: Boolean
      read GetEmitLogMessages write SetEmitLogMessages;

  end;

implementation

uses
  System.SysUtils,

  JsonDataObjects,

  DDuce.Logger;

{$REGION 'construction and destruction'}
procedure TLogViewerSettings.AfterConstruction;
begin
  Logger.Track(Self, 'AfterConstruction');
  inherited AfterConstruction;
  FFileName              := 'settings.json';
  FFormSettings          := TFormSettings.Create;
  FFormSettings.OnChanged.Add(FormSettingsChanged);
  FMessageListSettings   := TMessageListSettings.Create;
  FWinodsSettings        := TWinodsSettings.Create;
  FWinipcSettings        := TWinipcSettings.Create;
  FComPortSettings       := TComPortSettings.Create;
  FZeroMQSettings        := TZeroMQSettings.Create;
  FFileSystemSettings    := TFileSystemSettings.Create;
  FWatchSettings         := TWatchSettings.Create;
  FCallStackSettings     := TCallStackSettings.Create;
  FDisplayValuesSettings := TDisplayValuesSettings.Create;
  FDisplayValuesSettings.OnChanged.Add(DisplayValuesSettingsChanged);
  FLogLevelSettings      := TLogLevelSettings.Create;
end;

procedure TLogViewerSettings.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FDisplayValuesSettings.OnChanged.RemoveAll(Self);
  FFormSettings.OnChanged.RemoveAll(Self);
  FreeAndNil(FDisplayValuesSettings);
  FreeAndNil(FWatchSettings);
  FreeAndNil(FCallStackSettings);
  FreeAndNil(FZeroMQSettings);
  FreeAndNil(FFileSystemSettings);
  FreeAndNil(FComPortSettings);
  FreeAndNil(FWinipcSettings);
  FreeAndNil(FWinodsSettings);
  FreeAndNil(FMessageListSettings);
  FreeAndNil(FFormSettings);
  FreeAndNil(FLogLevelSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogViewerSettings.GetDebugMode: Boolean;
begin
  Result := FDebugMode;
end;

procedure TLogViewerSettings.SetDebugMode(const Value: Boolean);
begin
  if Value <> DebugMode then
  begin
    FDebugMode := Value;
    Changed;
  end;
end;

function TLogViewerSettings.GetEmitLogMessages: Boolean;
begin
  Result := FEmitLogMessages;
end;

procedure TLogViewerSettings.SetEmitLogMessages(const Value: Boolean);
begin
  if Value <> EmitLogMessages then
  begin
    FEmitLogMessages := Value;
    Changed;
  end;
end;

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
  I  : Integer;
  S  : string;
begin
  Logger.Track(Self, 'Load');
  if FileExists(FFileName) then
  begin
    JO := TJsonObject.Create;
    try
      JO.LoadFromFile(FFileName);
      JO['FormSettings'].ObjectValue.ToSimpleObject(FFormSettings);
      JO['MessageListSettings'].ObjectValue.ToSimpleObject(FMessageListSettings);
      for I := 0 to JO['MessageListSettings']
        .ObjectValue['HorizontalPanelPositions'].Count - 1 do
      begin
        FMessageListSettings.HorizontalPanelPositions[I] :=
          JO['MessageListSettings']
            .ObjectValue['HorizontalPanelPositions'].ArrayValue[I];
      end;
      for I := 0 to JO['MessageListSettings']
        .ObjectValue['LeftVerticalPanelPositions'].Count - 1 do
      begin
        FMessageListSettings.LeftVerticalPanelPositions[I] :=
          JO['MessageListSettings']
            .ObjectValue['LeftVerticalPanelPositions'].ArrayValue[I];
      end;
      // TODO MessageListSettings.VisibleMessageTypes
      // TODO MessageListSettings.VisibleValueTypes
      // TODO MessageListSettings.VisibleMessageLevels

      JO['WinodsSettings'].ObjectValue.ToSimpleObject(FWinodsSettings);
      JO['WinipcSettings'].ObjectValue.ToSimpleObject(FWinipcSettings);
      JO['ZeroMQSettings'].ObjectValue.ToSimpleObject(FZeroMQSettings);
      FZeroMQSettings.Endpoints.Text :=
        JO['ZeroMQSettings'].ObjectValue['Endpoints'].Value;
      JO['FileSystemSettings'].ObjectValue.ToSimpleObject(FFileSystemSettings);
      FFileSystemSettings.PathNames.Text :=
        JO['FileSystemSettings'].ObjectValue['PathNames'].Value;
      S := 'DisplayValuesSettings';
      JO[S].ObjectValue['Id'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Id);
      JO[S].ObjectValue['Info'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Info);
      JO[S].ObjectValue['Warning'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Warning);
      JO[S].ObjectValue['Error'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Error);
      JO[S].ObjectValue['TimeStamp'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.TimeStamp);
      JO[S].ObjectValue['ValueName'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.ValueName);
      JO[S].ObjectValue['ValueType'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.ValueType);
      JO[S].ObjectValue['Value'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Value);
      JO[S].ObjectValue['CheckPoint'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.CheckPoint);
      JO[S].ObjectValue['Counter'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Counter);
      JO[S].ObjectValue['Tracing'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Tracing);
      JO[S].ObjectValue['Enter'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Enter);
      JO[S].ObjectValue['Leave'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Leave);
      JO[S].ObjectValue['Conditional'].ObjectValue
        .ToSimpleObject(FDisplayValuesSettings.Conditional);
      JO['WatchSettings'].ObjectValue.ToSimpleObject(FWatchSettings);
      JO['CallStackSettings'].ObjectValue.ToSimpleObject(FCallStackSettings);
      JO['LogLevelSettings'].ObjectValue.ToSimpleObject(FLogLevelSettings);
      for I := 0 to JO['LogLevelSettings']
        .ObjectValue['LogLevels'].Count - 1 do
      begin
        JO['LogLevelSettings'].ObjectValue['LogLevels'].ArrayValue[I]
          .ObjectValue.ToSimpleObject(FLogLevelSettings.LogLevels[I]);
      end;
      JO.ToSimpleObject(Self);
    finally
      JO.Free;
    end;
  end;
end;

procedure TLogViewerSettings.Save;
var
  JO : TJsonObject;
  I  : Integer;
  S  : string;
begin
  Logger.Track(Self, 'Save');
  JO := TJsonObject.Create;
  try
    JO.FromSimpleObject(Self);
    JO['FormSettings'].ObjectValue.FromSimpleObject(FFormSettings);

    S := 'MessageListSettings';
    JO[S].ObjectValue.FromSimpleObject(FMessageListSettings);
    for I := 0 to FMessageListSettings.HorizontalPanelPositions.Count - 1 do
    begin
      JO[S].A['HorizontalPanelPositions']
        .Add(FMessageListSettings.HorizontalPanelPositions[I]);
    end;
    for I := 0 to FMessageListSettings.LeftVerticalPanelPositions.Count - 1 do
    begin
      JO[S].A['LeftVerticalPanelPositions']
        .Add(FMessageListSettings.LeftVerticalPanelPositions[I]);
    end;

    JO['WinodsSettings'].ObjectValue.FromSimpleObject(FWinodsSettings);
    JO['WinipcSettings'].ObjectValue.FromSimpleObject(FWinipcSettings);

    JO['ZeroMQSettings'].ObjectValue.FromSimpleObject(FZeroMQSettings);
    JO['ZeroMQSettings'].ObjectValue['Endpoints'].Value :=
      FZeroMQSettings.Endpoints.Text;

    S := 'FileSystemSettings';
    JO[S].ObjectValue.FromSimpleObject(FFileSystemSettings);
    JO[S].ObjectValue['PathNames'].Value := FileSystemSettings.PathNames.Text;

    S := 'DisplayValuesSettings';
    JO[S].ObjectValue['Id'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Id);
    JO[S].ObjectValue['Info'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Info);
    JO[S].ObjectValue['Warning'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Warning);
    JO[S].ObjectValue['Error'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Error);
    JO[S].ObjectValue['TimeStamp'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.TimeStamp);
    JO[S].ObjectValue['ValueName'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.ValueName);
    JO[S].ObjectValue['ValueType'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.ValueType);
    JO[S].ObjectValue['Value'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Value);
    JO[S].ObjectValue['CheckPoint'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.CheckPoint);
    JO[S].ObjectValue['Counter'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Counter);
    JO[S].ObjectValue['Tracing'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Tracing);
    JO[S].ObjectValue['Enter'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Enter);
    JO[S].ObjectValue['Leave'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Leave);
    JO[S].ObjectValue['Conditional'].ObjectValue
      .FromSimpleObject(FDisplayValuesSettings.Conditional);

    JO['WatchSettings'].ObjectValue.FromSimpleObject(FWatchSettings);
    JO['CallStackSettings'].ObjectValue.FromSimpleObject(FCallStackSettings);
    S := 'LogLevelSettings';
    JO[S].ObjectValue.FromSimpleObject(FLogLevelSettings);
    for I := 0 to FLogLevelSettings.LogLevels.Count - 1 do
    begin
      JO[S].A['LogLevels'].AddObject
        .FromSimpleObject(FLogLevelSettings.LogLevels[I]);
    end;
    JO.SaveToFile(FFileName, False);
  finally
    JO.Free;
  end;
end;
{$ENDREGION}

end.
