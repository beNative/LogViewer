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

{ Persistable settings for display values. }

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
    FEnter       : TTextFormatSettings;
    FLeave       : TTextFormatSettings;
    FConditional : TTextFormatSettings;
    FAction      : TTextFormatSettings;
    FOnChanged   : Event<TNotifyEvent>;

    function GetOnChanged: IEvent<TNotifyEvent>;

    procedure FormatSettingsChanged(Sender: TObject);

  protected
    procedure Changed;
    procedure InitializeObjects;

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

    property Enter: TTextFormatSettings
      read FEnter;

    property Leave: TTextFormatSettings
      read FLeave;

    property Conditional: TTextFormatSettings
      read FConditional;

    property Action: TTextFormatSettings
      read FAction;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TDisplayValuesSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeObjects;
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
  FAction.Free;
  FEnter.Free;
  FLeave.Free;
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

{$REGION 'protected methods'}
procedure TDisplayValuesSettings.InitializeObjects;
begin
  FTimeStamp               := TTextFormatSettings.Create;
  FTimeStamp.Name          := 'TimeStamp';
  FTimeStamp.Font.Color    := clBlue;
  FTimeStamp.OnChanged.Add(FormatSettingsChanged);

  FValueName               := TTextFormatSettings.Create;
  FValueName.Name          := 'ValueName';
  FValueName.Font.Color    := clMaroon;
  FValueName.Font.Style    := [fsUnderline];
  FValueName.OnChanged.Add(FormatSettingsChanged);

  FValueType               := TTextFormatSettings.Create;
  FValueType.Name          := 'ValueType';
  FValueType.Font.Color    := clNavy;
  FValueType.OnChanged.Add(FormatSettingsChanged);

  FValue                   := TTextFormatSettings.Create;
  FValue.Name              := 'Value';
  FValue.Font.Color        := clBlack;
  FValue.OnChanged.Add(FormatSettingsChanged);

  FId                      := TTextFormatSettings.Create;
  FId.Name                 := 'Id';
  FId.Font.Color           := clGray;
  FId.OnChanged.Add(FormatSettingsChanged);

  FInfo                    := TTextFormatSettings.Create;
  FInfo.Name               := 'Info';
  FInfo.Font.Color         := clBlue;
  FInfo.Font.Style         := [fsBold];
  FInfo.OnChanged.Add(FormatSettingsChanged);

  FWarning                 := TTextFormatSettings.Create;
  FWarning.Font.Color      := clWebOrange;
  FWarning.Font.Style      := [fsBold];
  FWarning.Name            := 'Warning';
  FWarning.OnChanged.Add(FormatSettingsChanged);

  FError                   := TTextFormatSettings.Create;
  FError.Name              := 'Error';
  FError.Font.Color        := clRed;
  FError.Font.Style        := [fsBold];
  FError.OnChanged.Add(FormatSettingsChanged);

  FConditional             := TTextFormatSettings.Create;
  FConditional.Font.Color  := clTeal;
  FConditional.Name        := 'Conditional';
  FConditional.OnChanged.Add(FormatSettingsChanged);

  FCheckPoint              := TTextFormatSettings.Create;
  FCheckPoint.Name         := 'CheckPoint';
  FCheckPoint.Font.Color   := clGreen;
  FCheckPoint.OnChanged.Add(FormatSettingsChanged);

  FCounter                 := TTextFormatSettings.Create;
  FCounter.Name            := 'Counter';
  FCounter.Font.Color      := clPurple;
  FCounter.OnChanged.Add(FormatSettingsChanged);

  FAction                 := TTextFormatSettings.Create;
  FAction.Name            := 'Action';
  FAction.Font.Color      := clMaroon;
  FAction.OnChanged.Add(FormatSettingsChanged);

  // used when collapsed (TODO)
  FTracing                 := TTextFormatSettings.Create;
  FTracing.Name            := 'Tracing';
  FTracing.BackgroundColor := clSilver;
  FTracing.OnChanged.Add(FormatSettingsChanged);

  // used when expanded
  FEnter                   := TTextFormatSettings.Create;
  FEnter.Font.Color        := clGreen;
  FEnter.BackgroundColor   := clSilver;
  FEnter.Name              := 'Enter';
  FEnter.OnChanged.Add(FormatSettingsChanged);

  // used when expanded
  FLeave                   := TTextFormatSettings.Create;
  FLeave.Name              := 'Leave';
  FLeave.Font.Color        := clRed;
  FLeave.BackgroundColor   := clSilver;
  FLeave.OnChanged.Add(FormatSettingsChanged);  
end;
{$ENDREGION}

end.
