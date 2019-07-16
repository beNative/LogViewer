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

unit LogViewer.MessageList.Settings;

{ Persistable settings associated with the messagelist viewer. }

interface

{$REGION 'documentation'}
{ TODO:
   - column settings for
        - TimeStamp columns
        - Name columns
        - Value columns
   - default filtered messages
 }
{$ENDREGION}

uses
  System.Classes,

  Spring,

  DDuce.Logger.Interfaces,

  LogViewer.Resources, LogViewer.Watches.Settings;

type
  TLogMessageTypes = set of TLogMessageType;

  TMessageListSettings = class(TPersistent)
  private
    FAutoScrollMessages         : Boolean;
    FAutoFilterMessages         : Boolean;
    FSmartTimeStamps            : Boolean;
    FDynamicAutoSizeColumns     : Boolean;
    FHideColumnHeaders          : Boolean;
    FOnChanged                  : Event<TNotifyEvent>;
    FVisibleMessageTypes        : TLogMessageTypes;
    FVisibleValueTypes          : TStringList;
    FHorizontalPanelPositions   : Vector<Double>;
    FLeftVerticalPanelPositions : Vector<Double>;

  protected
    {$REGION 'property access methods'}
    function GetSmartTimeStamps: Boolean;
    procedure SetSmartTimeStamps(const Value: Boolean);
    function GetVisibleValueTypes: TStrings;
    function GetAutoScrollMessages: Boolean;
    procedure SetAutoScrollMessages(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetVisibleMessageTypes: TLogMessageTypes;
    procedure SetVisibleMessageTypes(const Value: TLogMessageTypes);
    function GetAutoFilterMessages: Boolean;
    procedure SetAutoFilterMessages(const Value: Boolean);
    function GetDynamicAutoSizeColumns: Boolean;
    procedure SetDynamicAutoSizeColumns(const Value: Boolean);
    function GetHideColumnHeaders: Boolean;
    procedure SetHideColumnHeaders(const Value: Boolean);
    {$ENDREGION}

    procedure FVisibleValueTypesChange(Sender: TObject);

    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property VisibleMessageTypes: TLogMessageTypes
      read GetVisibleMessageTypes write SetVisibleMessageTypes;

    property VisibleValueTypes: TStrings
      read GetVisibleValueTypes;

    property HorizontalPanelPositions: Vector<Double>
      read FHorizontalPanelPositions;

    property LeftVerticalPanelPositions: Vector<Double>
      read FLeftVerticalPanelPositions;

  published
    property AutoScrollMessages: Boolean
      read GetAutoScrollMessages write SetAutoScrollMessages;

    property AutoFilterMessages: Boolean
      read GetAutoFilterMessages write SetAutoFilterMessages;

    property DynamicAutoSizeColumns: Boolean
      read GetDynamicAutoSizeColumns write SetDynamicAutoSizeColumns;

    property SmartTimeStamps: Boolean
      read GetSmartTimeStamps write SetSmartTimeStamps;

    property HideColumnHeaders: Boolean
      read GetHideColumnHeaders write SetHideColumnHeaders;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TMessageListSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FOnChanged.UseFreeNotification := False;
  FVisibleValueTypes            := TStringList.Create;
  FVisibleValueTypes.OnChange   := FVisibleValueTypesChange;
  FVisibleValueTypes.Sorted     := True;
  FVisibleValueTypes.Duplicates := dupIgnore;
  FVisibleMessageTypes := ALL_MESSAGES;
  // defaults
  FHorizontalPanelPositions.Add(0.2);
  FHorizontalPanelPositions.Add(0.7);
  FHorizontalPanelPositions.Add(1);
  FLeftVerticalPanelPositions.Add(0.3);
  FLeftVerticalPanelPositions.Add(1);
end;

procedure TMessageListSettings.BeforeDestruction;
begin
  FVisibleValueTypes.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TMessageListSettings.GetAutoFilterMessages: Boolean;
begin
  Result := FAutoFilterMessages;
end;

procedure TMessageListSettings.SetAutoFilterMessages(const Value: Boolean);
begin
  if Value <> AutoFilterMessages then
  begin
    FAutoFilterMessages := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetAutoScrollMessages: Boolean;
begin
  Result := FAutoScrollMessages;
end;

procedure TMessageListSettings.SetAutoScrollMessages(const Value: Boolean);
begin
  if Value <> AutoScrollMessages then
  begin
    FAutoScrollMessages := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetDynamicAutoSizeColumns: Boolean;
begin
  Result := FDynamicAutoSizeColumns;
end;

function TMessageListSettings.GetHideColumnHeaders: Boolean;
begin
  Result := FHideColumnHeaders;
end;

procedure TMessageListSettings.SetHideColumnHeaders(const Value: Boolean);
begin
  if Value <> HideColumnHeaders then
  begin
    FHideColumnHeaders := Value;
    Changed;
  end;
end;

procedure TMessageListSettings.SetDynamicAutoSizeColumns(const Value: Boolean);
begin
  if Value <> DynamicAutoSizeColumns then
  begin
    FDynamicAutoSizeColumns := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetVisibleMessageTypes: TLogMessageTypes;
begin
  Result := FVisibleMessageTypes;
end;

procedure TMessageListSettings.SetVisibleMessageTypes(
  const Value: TLogMessageTypes);
begin
  if Value <> VisibleMessageTypes then
  begin
    FVisibleMessageTypes := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetVisibleValueTypes: TStrings;
begin
  Result := FVisibleValueTypes;
end;

function TMessageListSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TMessageListSettings.GetSmartTimeStamps: Boolean;
begin
  Result := FSmartTimeStamps;
end;

procedure TMessageListSettings.SetSmartTimeStamps(const Value: Boolean);
begin
  if Value <> SmartTimeStamps then
  begin
    FSmartTimeStamps := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TMessageListSettings.FVisibleValueTypesChange(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TMessageListSettings.Changed;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TMessageListSettings.Assign(Source: TPersistent);
var
  LSettings: TMessageListSettings;
begin
  if Source is TMessageListSettings then
  begin
    LSettings := TMessageListSettings(Source);
    AutoScrollMessages     := LSettings.AutoScrollMessages;
    AutoFilterMessages     := LSettings.AutoFilterMessages;
    VisibleMessageTypes    := LSettings.VisibleMessageTypes;
    DynamicAutoSizeColumns := LSettings.DynamicAutoSizeColumns;
    HideColumnHeaders      := LSettings.HideColumnHeaders;
    HorizontalPanelPositions.Assign(LSettings.HorizontalPanelPositions.Data);
    LeftVerticalPanelPositions.Assign(LSettings.LeftVerticalPanelPositions.Data);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
