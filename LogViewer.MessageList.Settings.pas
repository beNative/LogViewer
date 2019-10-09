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

  LogViewer.Resources;

type
  TLogMessageTypes = set of TLogMessageType;

  TMessageListSettings = class(TPersistent)
  private
    FAutoScrollMessages         : Boolean;
    FAutoFilterMessages         : Boolean;
    FSmartTimeStamps            : Boolean;
    FDynamicAutoSizeColumns     : Boolean;
    FColumnHeadersVisible       : Boolean;
    FOnChanged                  : Event<TNotifyEvent>;
    FVisibleMessageTypes        : TLogMessageTypes;
    FVisibleMessageLevels       : TLogMessageLevels;
    FVisibleValueTypes          : TStringList;
    FHorizontalPanelPositions   : Vector<Double>;
    FLeftVerticalPanelPositions : Vector<Double>;
    FMessageDetailsVisible      : Boolean;

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
    function GetMessageDetailsVisible: Boolean;
    procedure SetMessageDetailsVisible(const Value: Boolean);
    function GetColumnHeadersVisible: Boolean;
    procedure SetColumnHeadersVisible(const Value: Boolean);
    function GetVisibleMessageLevels: TLogMessageLevels;
    procedure SetVisibleMessageLevels(const Value: TLogMessageLevels);
    {$ENDREGION}

    procedure FVisibleValueTypesChange(Sender: TObject);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Changed;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property VisibleMessageTypes: TLogMessageTypes
      read GetVisibleMessageTypes write SetVisibleMessageTypes;

    property VisibleMessageLevels: TLogMessageLevels
      read GetVisibleMessageLevels write SetVisibleMessageLevels;

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

    property ColumnHeadersVisible: Boolean
      read GetColumnHeadersVisible write SetColumnHeadersVisible;

    property DynamicAutoSizeColumns: Boolean
      read GetDynamicAutoSizeColumns write SetDynamicAutoSizeColumns;

    property MessageDetailsVisible: Boolean
      read GetMessageDetailsVisible write SetMessageDetailsVisible;

    property SmartTimeStamps: Boolean
      read GetSmartTimeStamps write SetSmartTimeStamps;
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
  FVisibleMessageTypes  := AllMessages;
  FVisibleMessageLevels := AllLevels;
  // defaults
  FHorizontalPanelPositions.Add(0.2);
  FHorizontalPanelPositions.Add(0.7);
  FHorizontalPanelPositions.Add(1);
  FLeftVerticalPanelPositions.Add(0.3);
  FLeftVerticalPanelPositions.Add(1);
end;

destructor TMessageListSettings.Destroy;
begin
  FVisibleValueTypes.Free;
  inherited Destroy;
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

function TMessageListSettings.GetColumnHeadersVisible: Boolean;
begin
  Result := FColumnHeadersVisible;
end;

procedure TMessageListSettings.SetColumnHeadersVisible(const Value: Boolean);
begin
  if Value <> ColumnHeadersVisible then
  begin
    FColumnHeadersVisible := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetDynamicAutoSizeColumns: Boolean;
begin
  Result := FDynamicAutoSizeColumns;
end;

procedure TMessageListSettings.SetDynamicAutoSizeColumns(const Value: Boolean);
begin
  if Value <> DynamicAutoSizeColumns then
  begin
    FDynamicAutoSizeColumns := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetMessageDetailsVisible: Boolean;
begin
  Result := FMessageDetailsVisible;
end;

procedure TMessageListSettings.SetMessageDetailsVisible(const Value: Boolean);
begin
  if Value <> MessageDetailsVisible then
  begin
    FMessageDetailsVisible := Value;
    Changed;
  end;
end;

function TMessageListSettings.GetVisibleMessageLevels: TLogMessageLevels;
begin
  Result := FVisibleMessageLevels;
end;

procedure TMessageListSettings.SetVisibleMessageLevels(
  const Value: TLogMessageLevels);
begin
  if Value <> VisibleMessageLevels then
  begin
    FVisibleMessageLevels := Value;
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
    VisibleMessageLevels   := LSettings.VisibleMessageLevels;
    DynamicAutoSizeColumns := LSettings.DynamicAutoSizeColumns;
    ColumnHeadersVisible   := LSettings.ColumnHeadersVisible;
    MessageDetailsVisible  := LSettings.MessageDetailsVisible;
    HorizontalPanelPositions.Assign(LSettings.HorizontalPanelPositions.Data);
    LeftVerticalPanelPositions.Assign(LSettings.LeftVerticalPanelPositions.Data);
    VisibleValueTypes.Assign(LSettings.VisibleValueTypes);
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.
