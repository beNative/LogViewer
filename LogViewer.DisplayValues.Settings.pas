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

unit LogViewer.DisplayValues.Settings;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics,

  Spring,

  DDuce.Settings.TextFormat;

type
  TDisplayValuesSettings = class(TPersistent)
  private
    FTimeStamp   : TTextFormatSettings;
    FValueName   : TTextFormatSettings;
    FValueType   : TTextFormatSettings;
    FValue       : TTextFormatSettings;
    FId          : TTextFormatSettings;
    FWarning     : TTextFormatSettings;
    FInfo        : TTextFormatSettings;
    FError       : TTextFormatSettings;
    FCheckPoint  : TTextFormatSettings;
    FCounter     : TTextFormatSettings;
    FTracing     : TTextFormatSettings;
    FConditional : TTextFormatSettings;
    FOnChanged   : Event<TNotifyEvent>;

    function GetOnChanged: IEvent<TNotifyEvent>;

    procedure FormatSettingsChanged(Sender: TObject);

  protected
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property TimeStamp: TTextFormatSettings
      read FTimeStamp;

    property ValueName: TTextFormatSettings
      read FValueName;

    property ValueType: TTextFormatSettings
      read FValueType;

    property Value: TTextFormatSettings
      read FValue;

    property Id: TTextFormatSettings
      read FId;

    property Warning: TTextFormatSettings
      read FWarning;

    property Info: TTextFormatSettings
      read FInfo;

    property Error: TTextFormatSettings
      read FError;

    property CheckPoint: TTextFormatSettings
      read FCheckPoint;

    property Counter: TTextFormatSettings
      read FCounter;

    property Tracing: TTextFormatSettings
      read FTracing;

    property Conditional: TTextFormatSettings
      read FConditional;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TDisplayValuesSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FTimeStamp := TTextFormatSettings.Create;
  FTimeStamp.Name := 'TimeStamp';
  FTimeStamp.OnChanged.Add(FormatSettingsChanged);
  FValueName := TTextFormatSettings.Create;
  FValueName.Name := 'ValueName';
  FValueName.OnChanged.Add(FormatSettingsChanged);
  FValueType := TTextFormatSettings.Create;
  FValueType.Name := 'ValueType';
  FValueType.OnChanged.Add(FormatSettingsChanged);
  FValue := TTextFormatSettings.Create;
  FValue.Name := 'Value';
  FValue.OnChanged.Add(FormatSettingsChanged);
  FId := TTextFormatSettings.Create;
  FId.Name := 'Id';
  FId.OnChanged.Add(FormatSettingsChanged);
  FInfo := TTextFormatSettings.Create;
  FInfo.Name := 'Info';
  FInfo.OnChanged.Add(FormatSettingsChanged);
  FWarning := TTextFormatSettings.Create;
  FWarning.Name := 'Warning';
  FWarning.OnChanged.Add(FormatSettingsChanged);
  FError := TTextFormatSettings.Create;
  FError.Name := 'Error';
  FError.OnChanged.Add(FormatSettingsChanged);
  FCheckPoint := TTextFormatSettings.Create;
  FCheckPoint.Name := 'CheckPoint';
  FCheckPoint.OnChanged.Add(FormatSettingsChanged);
  FCounter := TTextFormatSettings.Create;
  FCounter.Name := 'Counter';
  FCounter.OnChanged.Add(FormatSettingsChanged);
  FTracing := TTextFormatSettings.Create;
  FTracing.Name := 'Tracing';
  FTracing.OnChanged.Add(FormatSettingsChanged);
  FConditional := TTextFormatSettings.Create;
  FConditional.Name := 'Conditional';
  FConditional.OnChanged.Add(FormatSettingsChanged);
end;

procedure TDisplayValuesSettings.BeforeDestruction;
begin
  FTimeStamp.Free;
  FValueName.Free;
  FValueType.Free;
  FValue.Free;
  FId.Free;
  FInfo.Free;
  FWarning.Free;
  FError.Free;
  FCheckPoint.Free;
  FCounter.Free;
  FTracing.Free;
  FConditional.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TDisplayValuesSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TDisplayValuesSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TDisplayValuesSettings.FormatSettingsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

end.
