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

unit LogViewer.MessageList.View;

interface

{ Message viewer responsible for displaying all messages from an associated
  log channel (IChannelReceiver receiver instance) }

{
  TODO:
    - auto show message details, watches window and callstack (WinODS)
}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ImgList, Vcl.Grids, Vcl.DBGrids,
  Data.DB,

  FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Comp.DataSet,
  FireDAC.Stan.StorageBin,

  VirtualTrees,

  Spring.Collections,

  DDuce.Editor.Interfaces, DDuce.Logger.Interfaces, DDuce.Components.ValueList,
  DDuce.Components.DBGridView,

  LogViewer.Messages.Data, LogViewer.Watches.Data, LogViewer.Watches.View,
  LogViewer.Interfaces, LogViewer.CallStack.Data, LogViewer.CallStack.View,
  LogViewer.MessageList.Settings, LogViewer.MessageList.LogNode,
  LogViewer.DisplayValues.Settings;

type
  TfrmMessageList = class(TForm, ILogViewer)
    {$REGION 'designer controls'}
    btnFilterMessages : TButton;
    chkAutoFilter     : TCheckBox;
    edtHandleType     : TLabeledEdit;
    edtHeight         : TLabeledEdit;
    edtMessageFilter  : TLabeledEdit;
    edtMessageType    : TLabeledEdit;
    edtPixelFormat    : TLabeledEdit;
    edtTimeStamp      : TLabeledEdit;
    edtValue          : TLabeledEdit;
    edtValueName      : TLabeledEdit;
    edtValueType      : TLabeledEdit;
    edtWidth          : TLabeledEdit;
    imlMessageTypes   : TImageList;
    pgcMessageDetails : TPageControl;
    pnlCallStack      : TPanel;
    pnlCallStackTitle : TPanel;
    pnlCallStackWatch : TPanel;
    pnlColor          : TPanel;
    pnlFilter         : TPanel;
    pnlImageViewer    : TPanel;
    pnlLeft           : TPanel;
    pnlLeftBottom     : TPanel;
    pnlMessageContent : TPanel;
    pnlMessageDetails : TGridPanel;
    pnlMessages       : TPanel;
    pnlMessageType    : TPanel;
    pnlRawMessageData : TPanel;
    pnlRight          : TPanel;
    pnlTextViewer     : TPanel;
    pnlTimeStamp      : TPanel;
    pnlValue          : TPanel;
    pnlValueName      : TPanel;
    pnlValueType      : TPanel;
    pnlWatches        : TPanel;
    splLeftHorizontal : TSplitter;
    splLeftVertical   : TSplitter;
    splVertical       : TSplitter;
    tsImageViewer     : TTabSheet;
    tsTextViewer      : TTabSheet;
    tsValueList       : TTabSheet;
    sbxImage          : TScrollBox;
    imgBitmap         : TImage;
    tsDataSet         : TTabSheet;
    dscMain           : TDataSource;
    {$ENDREGION}

    procedure edtMessageFilterChange(Sender: TObject);
    procedure edtMessageFilterKeyDown(
      Sender  : TObject;
      var Key : Word;
      Shift   : TShiftState
    );
    procedure edtMessageFilterKeyUp(
      Sender  : TObject;
      var Key : Word;
      Shift   : TShiftState
    );
    procedure chkAutoFilterClick(Sender: TObject);
    procedure pnlMessagesClick(Sender: TObject);

  private class var
    FCounter : Integer;

  private
    FUpdate          : Boolean;
    FMessageCount    : Int64;
    FCurrentMsg      : TLogMessage;
    FCallStack       : IList<TCallStackData>;
    FWatches         : TWatchList;
    FLogTreeView     : TVirtualStringTree;
    FSubscriber      : ISubscriber;
    FCallStackView   : TfrmCallStackView;
    FWatchesView     : TfrmWatchesView;
    FManager         : ILogViewerManager;
    FEditorView      : IEditorView;
    FExpandParents   : Boolean;
    FLastParent      : PVirtualNode;
    FLastNode        : PVirtualNode;
    FVKPressed       : Boolean;
    FSettings        : TMessageListSettings;
    FAutoSizeColumns : Boolean;
    FValueList       : TValueList;
    FDataSet         : TFDMemTable;
    FDBGridView      : TDBGridView;

    {$REGION 'property access methods'}
    function GetManager: ILogViewerManager;
    function GetActions: ILogViewerActions;
    function GetSubscriber: ISubscriber;
    function GetForm: TCustomForm;
    function GetSettings: TMessageListSettings;
    function GetDisplayValuesSettings: TDisplayValuesSettings;
    {$ENDREGION}

    procedure FSettingsChanged(Sender: TObject);

    {$REGION 'FLogTreeView event handlers'}
    procedure FLogTreeViewFilterCallback(
      Sender    : TBaseVirtualTree;
      Node      : PVirtualNode;
      Data      : Pointer;
      var Abort : Boolean
    );
    procedure FLogTreeViewClick(Sender: TObject);
    procedure FLogTreeViewFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FLogTreeViewFocusChanging(
      Sender      : TBaseVirtualTree;
      OldNode     : PVirtualNode;
      NewNode     : PVirtualNode;
      OldColumn   : TColumnIndex;
      NewColumn   : TColumnIndex;
      var Allowed : Boolean
    );
    procedure FLogTreeViewFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FLogTreeViewGetImageIndex(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      Kind           : TVTImageKind;
      Column         : TColumnIndex;
      var Ghosted    : Boolean;
      var ImageIndex : TImageIndex
    );
    procedure FLogTreeViewGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FLogTreeViewInitNode(
      Sender            : TBaseVirtualTree;
      ParentNode        : PVirtualNode;
      Node              : PVirtualNode;
      var InitialStates : TVirtualNodeInitStates
    );
    procedure FLogTreeViewKeyPress(
      Sender  : TObject;
      var Key : Char
    );
    procedure FLogTreeViewBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );
    procedure FLogTreeViewBeforeItemPaint(
      Sender         : TBaseVirtualTree;
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      ItemRect       : TRect;
      var CustomDraw : Boolean
    );
    procedure FLogTreeViewAfterItemPaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas;
      Node         : PVirtualNode;
      ItemRect     : TRect
    );
    procedure FLogTreeViewPaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure FLogTreeViewGetHint(
      Sender             : TBaseVirtualTree;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      var LineBreakStyle : TVTTooltipLineBreakStyle;
      var HintText       : string
    );
    procedure FLogTreeViewGetHintKind(
      Sender   : TBaseVirtualTree;
      Node     : PVirtualNode;
      Column   : TColumnIndex;
      var Kind : TVTHintKind
    );
    procedure FLogTreeViewHotChange(
      Sender  : TBaseVirtualTree;
      OldNode : PVirtualNode;
      NewNode : PVirtualNode
    );
    {$ENDREGION}

  protected
    procedure FSubscriberReceiveMessage(
      Sender  : TObject;
      AStream : TStream
    );

    procedure Clear;
    procedure AutoFitColumns;

    procedure ApplySettings;

    procedure ProcessMessage(AStream: TStream);

    procedure AddMessageToTree(const AMessage: TLogMessage);

    procedure UpdateCallStack(ALogNode: TLogNode);
    procedure UpdateMessageDetails(ALogNode: TLogNode);
    procedure UpdateComponentDisplay(ALogNode: TLogNode);
    procedure UpdateBitmapDisplay(ALogNode: TLogNode);
    procedure UpdateDataSetDisplay(ALogNode: TLogNode);
    procedure UpdateTextDisplay(ALogNode: TLogNode);
    procedure UpdateTextStreamDisplay(ALogNode: TLogNode);
    procedure UpdateColorDisplay(ALogNode: TLogNode);
    procedure UpdateValueDisplay(ALogNode: TLogNode);

    procedure UpdateLogTreeView;

    procedure ClearMessageDetailsControls;

    procedure CreateLogTreeView;
    procedure CreateEditor;
    procedure CreateCallStackView;
    procedure CreateWatchesView;

    procedure CollapseAll;
    procedure ExpandAll;

    procedure Activate; override;
    procedure UpdateActions; override;
    procedure UpdateView;

    procedure GotoFirst;
    procedure GotoLast;

    property Manager: ILogViewerManager
      read GetManager;

    property Actions: ILogViewerActions
      read GetActions;

    property Subscriber: ISubscriber
      read GetSubscriber;

    property Settings: TMessageListSettings
      read GetSettings;

    property DisplayValuesSettings: TDisplayValuesSettings
      read GetDisplayValuesSettings;

    property Form: TCustomForm
      read GetForm;

  public
    constructor Create(
      AOwner      : TComponent;
      AManager    : ILogViewerManager;
      ASubscriber : ISubscriber;
      ASettings   : TMessageListSettings
    ); reintroduce; virtual;

    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.StrUtils, System.UITypes, System.DateUtils, System.Math,
  System.UIConsts, System.Threading,

  Spring, Spring.Helpers,

  DDuce.Factories.VirtualTrees, DDuce.Editor.Factories,
  DDuce.Reflect, DDuce.Utils, DDuce.Factories.GridView,

  DDuce.ObjectInspector.zObjectInspector, DDuce.DynamicRecord,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  LogViewer.Factories, LogViewer.Resources;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmMessageList.Create(AOwner: TComponent; AManager
  : ILogViewerManager; ASubscriber: ISubscriber; ASettings: TMessageListSettings);
begin
  inherited Create(AOwner);
  FSubscriber := ASubscriber;
  FManager  := AManager;
  FSettings := ASettings;
end;

procedure TfrmMessageList.AfterConstruction;
begin
  inherited AfterConstruction;
  Inc(FCounter);
  btnFilterMessages.Action := FManager.Actions.Items['actFilterMessages'];
  FExpandParents           := False;
  CreateEditor;
  CreateLogTreeView;
  CreateWatchesView;
  CreateCallStackView;
  FSubscriber.OnReceiveMessage.Add(FSubscriberReceiveMessage);
  Caption := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  FSettings.OnChanged.Add(FSettingsChanged);
  FLogTreeView.PopupMenu := Manager.Menus.LogTreeViewerPopupMenu;

  FValueList        := TValueList.Create(Self);
  FValueList.Parent := tsValueList;
  FValueList.Align  := alClient;

  FDataSet := TFDMemTable.Create(Self);
  FDBGridView := TGridViewFactory.CreateDBGridView(Self, tsDataSet, dscMain);

  ApplySettings;
end;

procedure TfrmMessageList.BeforeDestruction;
begin
  FSettings.LeftPanelWidth  := pnlLeft.Width;
  FSettings.RightPanelWidth := pnlRight.Width;
  FSubscriber.OnReceiveMessage.Remove(FSubscriberReceiveMessage);
  FSettings.OnChanged.Remove(FSettingsChanged);
  FSubscriber := nil;
  FManager  := nil;
  FSettings := nil;
  inherited BeforeDestruction;
end;

procedure TfrmMessageList.CreateCallStackView;
begin
  FCallStack     := TCollections.CreateObjectList<TCallStackData>;
  FCallStackView := TLogViewerFactories.CreateCallStackView(
    Self,
    pnlCallStack,
    FCallStack as IObjectList,
    DisplayValuesSettings
  );
end;

procedure TfrmMessageList.CreateEditor;
begin
  FEditorView := TEditorFactories.CreateView(
    pnlTextViewer,
    Manager.EditorManager
  );
  FEditorView.Settings.EditorOptions.WordWrapEnabled := True;
end;

procedure TfrmMessageList.CreateLogTreeView;
var
  C : TVirtualTreeColumn;
begin
  FLogTreeView := TVirtualStringTreeFactory.CreateTreeList(Self, pnlMessages);
  FLogTreeView.AlignWithMargins := False;
  FLogTreeView.TreeOptions.AutoOptions := FLogTreeView.TreeOptions.AutoOptions +
    [toAutoSpanColumns];
  FLogTreeView.TreeOptions.PaintOptions := [
    toHideFocusRect, toHotTrack, toPopupMode, toShowBackground, toShowButtons,
    toShowDropmark, toStaticBackground, toShowRoot, toShowVertGridLines,
    toThemeAware, toUseBlendedImages, toUseBlendedSelection,
    toStaticBackground, toUseExplorerTheme
  ];

  FLogTreeView.NodeDataSize := SizeOf(TLogNode);
  FLogTreeView.Images       := imlMessageTypes;

  FLogTreeView.OnBeforeItemPaint := FLogTreeViewBeforeItemPaint;
  FLogTreeView.OnAfterItemPaint  := FLogTreeViewAfterItemPaint;

  FLogTreeView.OnBeforeCellPaint := FLogTreeViewBeforeCellPaint;

  FLogTreeView.OnFocusChanged    := FLogTreeViewFocusChanged;
  FLogTreeView.OnFocusChanging   := FLogTreeViewFocusChanging;
  FLogTreeView.OnClick           := FLogTreeViewClick;

  FLogTreeView.OnHotChange       := FLogTreeViewHotChange;

  FLogTreeView.OnInitNode        := FLogTreeViewInitNode;
  FLogTreeView.OnFreeNode        := FLogTreeViewFreeNode;

  FLogTreeView.OnGetHint         := FLogTreeViewGetHint;
  FLogTreeView.OnGetHintKind     := FLogTreeViewGetHintKind;

  FLogTreeView.OnGetText         := FLogTreeViewGetText;
  FLogTreeView.OnPaintText       := FLogTreeViewPaintText;
  FLogTreeView.OnGetImageIndex   := FLogTreeViewGetImageIndex;

  FLogTreeView.OnKeyPress        := FLogTreeViewKeyPress;
  FLogTreeView.HintMode          := hmHint;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := '';
  C.Options  := C.Options + [coFixed];
  C.Width    := 120;
  C.MinWidth := 120;
  C.MaxWidth := 150;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := SName;
  C.Options  := C.Options + [coSmartResize, coAutoSpring];
  C.Width    := 150;
  C.MinWidth := 100;
  C.MaxWidth := 2000;

  C := FLogTreeView.Header.Columns.Add;
  C.Text      := SType;
  C.Options   := C.Options + [coSmartResize, coAutoSpring];
  C.Width     := 150;
  C.MinWidth  := 50;
  C.MaxWidth  := 2000;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := SValue;
  C.Options  := C.Options + [coAutoSpring];
  C.Width    := 150;
  C.MinWidth := 80;
  C.MaxWidth := 2000;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := STimestamp;
  C.Width    := 80;
  C.MinWidth := 80;
  C.MaxWidth := 80;

  FLogTreeView.Header.AutoSizeIndex := 3;
end;

procedure TfrmMessageList.CreateWatchesView;
begin
  FWatches := TWatchList.Create;
  FWatchesView := TLogViewerFactories.CreateWatchesView(
    Self,
    pnlLeftBottom,
    FWatches,
    FSettings.WatchSettings,
    DisplayValuesSettings
  );
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMessageList.GetActions: ILogViewerActions;
begin
  Result := Manager as ILogViewerActions;
end;

function TfrmMessageList.GetDisplayValuesSettings: TDisplayValuesSettings;
begin
  if Assigned(Manager) and Assigned(Manager.Settings) then
    Result := Manager.Settings.DisplayValuesSettings
  else
    Result := nil;
end;

function TfrmMessageList.GetForm: TCustomForm;
begin
  Result := Self;
end;

function TfrmMessageList.GetManager: ILogViewerManager;
begin
  Result := FManager;
end;

function TfrmMessageList.GetSubscriber: ISubscriber;
begin
  Result := FSubscriber;
end;

function TfrmMessageList.GetSettings: TMessageListSettings;
begin
  Result := FSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'FLogTreeView'}
procedure TfrmMessageList.FLogTreeViewBeforeItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var CustomDraw: Boolean);
//var
//  LN : TLogNode;
//  DVS : TDisplayValuesSettings;
//  S : string;
begin
//  LN := Sender.GetNodeData<TLogNode>(Node);
//  DVS := Manager.Settings.DisplayValuesSettings;
//  if LN.MessageType in [lmtEnterMethod, lmtLeaveMethod] then
//  begin
    //CustomDraw := True;
    //DVS.Tracing.AssignTo(TargetCanvas);
    //TargetCanvas.Brush.Color := $00EEEEEE;
//    TargetCanvas.FillRect(ItemRect);
//    S := LN.Text;
//    TargetCanvas.TextRect(ItemRect, S);
  //end;
end;

procedure TfrmMessageList.FSubscriberReceiveMessage(Sender: TObject;
  AStream: TStream);
begin
  ProcessMessage(AStream);
end;

procedure TfrmMessageList.FLogTreeViewAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
begin
//
end;

procedure TfrmMessageList.FLogTreeViewClick(Sender: TObject);
begin
  AutoFitColumns;
end;

procedure TfrmMessageList.FLogTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LN  : TLogNode;
  DVS : TDisplayValuesSettings;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  DVS := Manager.Settings.DisplayValuesSettings;
  if LN.MessageType in [lmtEnterMethod, lmtLeaveMethod] then
  begin
    DVS.Tracing.AssignTo(TargetCanvas);
    //TargetCanvas.Brush.Color := $00EEEEEE;
    TargetCanvas.FillRect(CellRect);
  end
end;

procedure TfrmMessageList.FLogTreeViewFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  LN : TLogNode;
begin
  //Todo: merge with Changed?
  //The CallStack is only updated if the parent changes
  Allowed := OldNode <> NewNode;
  if Allowed and (
    (OldNode = nil) or (NewNode = nil) or (OldNode.Parent <> NewNode.Parent)
  ) then
  begin
    LN := Sender.GetNodeData<TLogNode>(NewNode);
    Guard.CheckNotNull(LN, 'LogNode');
    UpdateCallStack(LN);
  end;
end;

procedure TfrmMessageList.FLogTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LN : TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'LogNode');
  UpdateMessageDetails(LN);
end;

procedure TfrmMessageList.FLogTreeViewFilterCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  LN : TLogNode;
  B  : Boolean;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'LogNode');
  B := LN.MessageType in Settings.VisibleMessageTypes;
  if edtMessageFilter.Text <> '' then
    B := B and
      (ContainsText(LN.Text, edtMessageFilter.Text) or
       ContainsText(LN.ValueName, edtMessageFilter.Text) or
       ContainsText(LN.Value, edtMessageFilter.Text));

  Sender.IsVisible[Node] := B;
end;

procedure TfrmMessageList.FLogTreeViewGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  LN: TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'ND');
  //HintText := Reflect.Fields(LN).ToString;
end;

procedure TfrmMessageList.FLogTreeViewGetHintKind(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Kind: TVTHintKind);
begin
  Kind := vhkText;
end;

procedure TfrmMessageList.FLogTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  ND: TLogNode;
begin
  ND := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(ND, 'ND');
  if (Kind in [ikNormal, ikSelected]) and (Column = COLUMN_MAIN) then
  begin
    if Integer(ND.MessageType) < imlMessageTypes.Count then
    begin
      ImageIndex := Integer(ND.MessageType);
    end
    else if ND.MessageType = lmtDataSet then
    begin
      ImageIndex := 20;
    end
    else if ND.MessageType = lmtText then
    begin
      ImageIndex := 8;
    end
    else
    begin
      ImageIndex := 0;
    end;
  end;
end;

procedure TfrmMessageList.FLogTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LN : TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'ND');
  if Column = COLUMN_MAIN then
  begin
    case LN.MessageType of
      lmtValue:
        CellText := SValue;
      lmtAlphaColor, lmtColor:
        CellText := SColor;
      lmtBitmap:
        CellText := SBitmap;
      lmtComponent:
        CellText := SComponent;
      lmtObject:
        CellText := SObject;
      lmtPersistent:
        CellText := SPersistent;
      lmtInterface:
        CellText := SInterface;
      lmtStrings:
        CellText := SStrings;
      lmtCheckpoint:
        CellText := SCheckpoint;
      lmtDataSet:
        CellText := SDataSet;
      lmtScreenShot:
        CellText := SScreenShot;
      lmtText:
        CellText := SText;
      else
        CellText := LN.Text
    end;
  end
  else if Column = COLUMN_VALUENAME then
  begin
    CellText := LN.ValueName;
  end
  else if Column = COLUMN_VALUETYPE then
  begin
    CellText := LN.ValueType;
  end
  else if Column = COLUMN_VALUE then
  begin
    if LN.Value.Contains(#13#10)
      or (LN.Value.Length > MAX_TEXTLENGTH_VALUECOLUMN) then
      CellText := '...'
    else
      CellText := LN.Value;
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    if YearOf(LN.TimeStamp) = YearOf(Now) then // sanity check
      CellText := FormatDateTime('hh:nn:ss:zzz',  LN.TimeStamp);
  end;
end;

procedure TfrmMessageList.FLogTreeViewHotChange(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode);
var
  LN1 : TLogNode;
  LN2 : TLogNode;
  N   : Int64;
begin
  LN1 := Sender.GetNodeData<TLogNode>(NewNode);
  LN2 := Sender.GetNodeData<TLogNode>(Sender.FocusedNode);
  if Assigned(LN1) and Assigned(LN2) then
  begin
    N := MilliSecondsBetween(LN1.TimeStamp, LN2.TimeStamp);
    pnlMessageContent.Caption := Format('Delta = %d ms', [N]);
    //Sender.Hint := Format('Delta = %d ms', [N]);
  end;
end;

procedure TfrmMessageList.FLogTreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LN : TLogNode; // class type
  I  : Integer;
  S  : string;
  SL : TStringList;
begin
  LN := TLogNode.Create;
  Node.SetData(LN);
  LN.MessageData := FCurrentMsg.Data;
  LN.TimeStamp   := FCurrentMsg.TimeStamp;
  LN.MessageType := TLogMessageType(FCurrentMsg.MsgType);
  LN.VTNode      := Node;
  LN.Id          := FMessageCount;
  case LN.MessageType of
    lmtValue, lmtComponent, lmtStrings, lmtPersistent, lmtObject, lmtInterface:
    begin
      S := string(FCurrentMsg.Text);
      I := S.IndexOf('=');
      LN.ValueName := Copy(S, 1, I);
      LN.Value := Copy(S, I + 3, S.Length); // ' = '
      if LN.Value.StartsWith(#13#10) then // multiline values
        LN.Value := Copy(LN.Value, 3, LN.Value.Length);
      LN.ValueType := ExtractText(LN.ValueName, '(', ')');
      I := S.IndexOf('(');
      if I > 1 then
        LN.ValueName := Copy(S, 1, I);
      LN.Text := '';
    end;
    lmtDataSet:
    begin
      LN.ValueName := FCurrentMsg.Text;
      LN.ValueType := 'TDataSet';

    end;
    lmtBitmap, lmtScreenShot:
    begin
      LN.ValueName := FCurrentMsg.Text;
      LN.ValueType := 'TBitmap';
    end;
    lmtAlphaColor, lmtColor:
    begin
      S := string(FCurrentMsg.Text);
      I := S.IndexOf('=');
      LN.ValueName := Copy(S, 1, I);
      LN.Value := Copy(S, I + 3, S.Length);
      LN.ValueType := ExtractText(LN.ValueName, '(', ')');
      I := S.IndexOf('(');
      if I > 1 then
        LN.ValueName := Copy(S, 1, I);
      LN.Text := '';
    end;
    lmtEnterMethod, lmtLeaveMethod,
    lmtInfo, lmtWarning, lmtError, lmtConditional:
    begin
      LN.Text := string(FCurrentMsg.Text);
    end;
    lmtText:
    begin
      SL := TStringList.Create;
      try
        SL.Text := string(FCurrentMsg.Text);
        if SL.Count > 0 then
        begin
          S := SL[0];
          I := S.IndexOf('(');
          if I > 1 then
          begin
            LN.ValueName := Copy(S, 1, I);
            LN.ValueType := ExtractText(S, '(', ')');
          end;
          SL.Delete(0);
        end;
        LN.Value := SL.Text;
      finally
        SL.Free;
      end;
    end;
    lmtCheckpoint:
    begin
      S := string(FCurrentMsg.Text);
      I := S.IndexOf('#');
      LN.ValueName := Copy(S, 1, I);
      LN.Value := Copy(S, I + 2, S.Length);
      LN.ValueType := 'Checkpoint';
    end
  end;
  //Show only what matches filter criterias
  Sender.IsVisible[Node] := (LN.MessageType in [lmtEnterMethod, lmtLeaveMethod]) or
      (LN.MessageType in Settings.VisibleMessageTypes);
    //and IsWild(Text, FTitleFilter, True));
end;

procedure TfrmMessageList.FLogTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LN: TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  LN.Free;
end;

procedure TfrmMessageList.FLogTreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  if not edtMessageFilter.Focused then
  begin
    edtMessageFilter.SetFocus;
    PostMessage(edtMessageFilter.Handle, WM_CHAR, Ord(Key), 0);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;

{ Used to apply custom settings to TargetCanvas.Font. }

procedure TfrmMessageList.FLogTreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  LN  : TLogNode;
  DVS : TDisplayValuesSettings;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'ND');
  DVS := Manager.Settings.DisplayValuesSettings;
  if not Assigned(Manager) then
    Exit;
  if Column = COLUMN_MAIN then
  begin
    case LN.MessageType of
      lmtInfo:
      begin
        DVS.Info.AssignTo(TargetCanvas.Font);
      end;
      lmtWarning:
      begin
        DVS.Warning.AssignTo(TargetCanvas.Font);
      end;
      lmtError:
      begin
        DVS.Error.AssignTo(TargetCanvas.Font);
      end;
      lmtValue, lmtComponent, lmtAlphaColor, lmtColor, lmtBitmap, lmtStrings,
      lmtObject, lmtPersistent, lmtInterface:
      begin
        TargetCanvas.Font.Color := clDkGray;
      end;
      lmtEnterMethod, lmtLeaveMethod:
      begin
        DVS.Tracing.AssignTo(TargetCanvas.Font);
      end;
      lmtCheckpoint:
      begin
        DVS.CheckPoint.AssignTo(TargetCanvas.Font);
      end;
    end;
  end
  else if Column = COLUMN_VALUENAME then
  begin
    DVS.ValueName.AssignTo(TargetCanvas.Font);
  end
  else if Column = COLUMN_VALUETYPE then
  begin
    DVS.ValueType.AssignTo(TargetCanvas.Font);
  end
  else if Column = COLUMN_VALUE then
  begin
    DVS.Value.AssignTo(TargetCanvas.Font);
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    DVS.TimeStamp.AssignTo(TargetCanvas.Font);
  end;
end;
{$ENDREGION}

{$REGION 'edtMessageFilter'}
procedure TfrmMessageList.edtMessageFilterChange(Sender: TObject);
begin
  if edtMessageFilter.Text <> '' then
  begin
    edtMessageFilter.Font.Style := [fsBold];
    edtMessageFilter.Color := clYellow;
  end
  else
  begin
    edtMessageFilter.Font.Style := [];
    edtMessageFilter.Color := clWhite;
  end;

  if Settings.AutoFilterMessages then
    UpdateLogTreeView;
end;

procedure TfrmMessageList.edtMessageFilterKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  A : Boolean;
  B : Boolean;
  C : Boolean;
  D : Boolean;
  E : Boolean;
  F : Boolean;
  G : Boolean;
  H : Boolean;
begin
  // SHIFTED and ALTED keycombinations
  A := (ssAlt in Shift) or (ssShift in Shift);
  { Single keys that need to be handled by the edit control like all displayable
    characters but also HOME and END }
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  { CTRL-keycombinations that need to be handled by the edit control like
    CTRL-C for clipboard copy. }
  C := (Key in VK_CTRL_EDIT_KEYS) {and (Shift = [ssCtrlOS])};
  { SHIFT-keycombinations that need to be handled by the edit control for
    uppercase characters but also eg. SHIFT-HOME for selections. }
  D := (Key in VK_SHIFT_EDIT_KEYS) and (Shift = [ssShift]);
  { Only CTRL key is pressed. }
  E := (Key = VK_CONTROL) {and (Shift = [ssCtrlOS])};
  { Only SHIFT key is pressed. }
  F := (Key = VK_SHIFT) and (Shift = [ssShift]);
  { Only (left) ALT key is pressed. }
  G := (Key = VK_MENU) and (Shift = [ssAlt]);
  { ESCAPE }
  H := Key = VK_ESCAPE;
  if not (A or B or C or D or E or F or G or H) then
  begin
    FVKPressed := True;
    Key := 0;
  end
  { Prevents jumping to the application's main menu which happens by default
    if ALT is pressed. }
  else if G then
  begin
    Key := 0;
  end;
end;

procedure TfrmMessageList.edtMessageFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FVKPressed and FLogTreeView.Enabled then
  begin
    FLogTreeView.Perform(WM_KEYDOWN, Key, 0);
    if Visible and FLogTreeView.CanFocus then
      FLogTreeView.SetFocus;
  end;
  FVKPressed := False;
end;
{$ENDREGION}

procedure TfrmMessageList.chkAutoFilterClick(Sender: TObject);
begin
  Settings.AutoFilterMessages := (Sender as TCheckBox).Checked;
end;

procedure TfrmMessageList.FSettingsChanged(Sender: TObject);
begin
  ApplySettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessageList.ApplySettings;
begin
  chkAutoFilter.Checked := Settings.AutoFilterMessages;
  pnlLeft.Width         := Settings.LeftPanelWidth;
  pnlRight.Width        := Settings.RightPanelWidth;
  DisplayValuesSettings.TimeStamp.AssignTo(edtTimeStamp.Font);
  edtTimeStamp.Alignment := DisplayValuesSettings.TimeStamp.HorizontalAlignment;
  DisplayValuesSettings.ValueName.AssignTo(edtValueName.Font);
  edtValueName.Alignment := DisplayValuesSettings.ValueName.HorizontalAlignment;
  DisplayValuesSettings.ValueType.AssignTo(edtValueType.Font);
  edtValueType.Alignment := DisplayValuesSettings.ValueType.HorizontalAlignment;
  DisplayValuesSettings.Value.AssignTo(edtValue.Font);
  edtValue.Alignment := DisplayValuesSettings.Value.HorizontalAlignment;
  UpdateView;
end;

procedure TfrmMessageList.AutoFitColumns;
begin
  FLogTreeView.BeginUpdate;
  //FLogTreeView.Header.AutoFitColumns(False, smaUseColumnOption, -1, -1);
  FLogTreeView.Header.AutoFitColumns(False, smaUseColumnOption, 1, 2);
  FAutoSizeColumns := True;
  FLogTreeView.EndUpdate;
end;

procedure TfrmMessageList.Activate;
begin
  inherited Activate;
  Manager.ActiveView := Self as ILogViewer;
end;

{ Parses message data from the TLogMessage record. }

procedure TfrmMessageList.AddMessageToTree(const AMessage: TLogMessage);
begin
  FLogTreeView.BeginUpdate;
  try
    case TLogMessageType(AMessage.MsgType) of
      lmtEnterMethod:
      begin
        FLastNode := FLogTreeView.AddChild(FLastParent, nil);
        if FExpandParents then
          FLogTreeView.Expanded[FLastParent] := True;
        FLastParent := FLastNode;
      end;
      lmtLeaveMethod:
      begin
        if (FLastParent = nil)
          or (FLastParent^.Parent = FLogTreeView.RootNode) then
        begin
          FLastNode   := FLogTreeView.AddChild(nil, nil);
          FLastParent := nil;
        end
        else
        begin
          FLastNode   := FLogTreeView.AddChild(FLastParent.Parent, nil);
          FLastParent := FLastNode.Parent;
        end;
      end
      else
      begin
        FLastNode := FLogTreeView.AddChild(FLastParent, nil);
      end;
    end; // case
    if FExpandParents then
    begin
      FLogTreeView.Expanded[FLastParent] := True;
    end;
    FAutoSizeColumns := False;
  finally
    FLogTreeView.EndUpdate;
  end;
end;

procedure TfrmMessageList.Clear;
begin
  FLogTreeView.Clear;
  FWatches.Clear;
  FEditorView.Clear;
  FCallStack.Clear;
  ClearMessageDetailsControls;
  FMessageCount := 0;
  FLastNode     := nil;
  FLastParent   := nil;
  FUpdate := True;
end;

procedure TfrmMessageList.ClearMessageDetailsControls;
begin
  imgBitmap.Picture   := nil;
  edtWidth.Text       := '';
  edtHeight.Text      := '';
  edtValue.Text       := '';
  edtPixelFormat.Text := '';
  edtHandleType.Text  := '';
  edtMessageType.Text := '';
  edtTimeStamp.Text   := '';
  edtValueName.Text   := '';
  edtValueType.Text   := '';
  pnlColor.Color      := clBtnFace;
  FDataSet.Active     := False;
  FEditorView.Clear;
  FValueList.Clear;
end;

procedure TfrmMessageList.CollapseAll;
begin
  FLogTreeView.FullCollapse;
end;

procedure TfrmMessageList.ExpandAll;
begin
  FLogTreeView.FullExpand;
end;

procedure TfrmMessageList.GotoFirst;
begin
  if FWatchesView.HasFocus then
  begin
    FWatchesView.GotoFirst;
  end
  else
  begin
    FLogTreeView.FocusedNode := FLogTreeView.GetFirst;
    FLogTreeView.Selected[FLogTreeView.FocusedNode] := True;
  end;
end;

procedure TfrmMessageList.GotoLast;
begin
  if FWatchesView.HasFocus then
  begin
    FWatchesView.GotoLast;
  end
  else
  begin
    FLogTreeView.FocusedNode := FLogTreeView.GetLast;
    FLogTreeView.Selected[FLogTreeView.FocusedNode] := True;
  end;
end;

procedure TfrmMessageList.pnlMessagesClick(Sender: TObject);
begin

end;

{ Reads the received message stream from the active logchannel . }

{
   Message layout in stream
     1. Message type (4 byte)          => TLogMessage.MsgType (TLogMessageType)
     2. Timestamp    (8 byte)          => TLogMessage.TimeStamp
     3. Text size    (4 byte)
     4. Text         (variable size)   => TLogMessage.Text
     5. Data size    (4 byte)
     6. Data         (variable size)   => TLogMessage.Data

    TLogMessage = packed record
      MsgType   : Integer; // (TLogMessageType)
      TimeStamp : TDateTime;
      Text      : UTF8String;
      Data      : TStream;
    end;
}

procedure TfrmMessageList.ProcessMessage(AStream: TStream);
var
  LTextSize : Integer;
  LDataSize : Integer;
  S         : string;
  I         : Integer;
  LName     : string;
  LType     : string;
  LValue    : string;
begin
  Guard.CheckNotNull(AStream, 'AStream');
  LTextSize := 0;
  LDataSize := 0;
  Inc(FMessageCount);
  AStream.Seek(0, soFromBeginning);
  AStream.ReadBuffer(FCurrentMsg.MsgType);
  AStream.ReadBuffer(FCurrentMsg.TimeStamp);
  AStream.ReadBuffer(LTextSize);
  if LTextSize > 0 then
  begin
    SetLength(FCurrentMsg.Text, LTextSize);
    AStream.ReadBuffer(FCurrentMsg.Text[1], LTextSize);
  end;
  AStream.ReadBuffer(LDataSize);
  if LDataSize > 0 then
  begin
    FCurrentMsg.Data          := TMemoryStream.Create;
    FCurrentMsg.Data.Size     := 0;
    FCurrentMsg.Data.Position := 0;
    FCurrentMsg.Data.CopyFrom(AStream, LDataSize);
  end
  else
    FCurrentMsg.Data := nil;

  case TLogMessageType(FCurrentMsg.MsgType) of
    lmtCounter:
    begin
      S := string(FCurrentMsg.Text);
      I := S.IndexOf('=');
      LName  := Copy(S, 1, I);
      LValue := Copy(S, I + 2, S.Length);
      LType  := 'Counter';
      FWatches.Add(
        LName,
        LType,
        LValue,
        FMessageCount,
        FCurrentMsg.TimeStamp,
        True,
        True
      );
      FWatchesView.UpdateView(FMessageCount);
    end;
    lmtWatch:
    begin
      S := string(FCurrentMsg.Text);
      I := S.IndexOf('=');
      LName := Copy(S, 1, I);
      LValue := Copy(S, I + 2, S.Length);
      LType := ExtractText(LName, '(', ')');
      I := S.IndexOf('(');
      if I > 1 then
        LName := Copy(S, 1, I);
      FWatches.Add(
        LName,
        LType,
        LValue,
        FMessageCount,
        FCurrentMsg.TimeStamp,
        True,
        False
      );
      FWatchesView.UpdateView(FMessageCount);
    end;
    lmtClear:
    begin
      Clear;
    end
    else
    begin
      AddMessageToTree(FCurrentMsg);
    end;
  end; // case

  if Settings.AutoScrollMessages then
  begin
    FLogTreeView.FocusedNode := FLogTreeView.GetLast;
    FLogTreeView.Selected[FLogTreeView.FocusedNode] := True;
  end;
  FUpdate := True;
end;

procedure TfrmMessageList.UpdateActions;
var
  B : Boolean;
begin
  if FUpdate then
  begin
    B := Focused;
    if not B and Assigned(Parent) then
    begin
      if Parent.Focused then
        B := True;
    end;
    if B then
    begin
      Activate;
    end;
    UpdateLogTreeView;
    if Assigned(Actions) then
      Actions.UpdateActions;

    if Settings.DynamicAutoSizeColumns and not FAutoSizeColumns then
      AutoFitColumns;
    FUpdate := False;
  end;

  inherited UpdateActions;
end;

procedure TfrmMessageList.UpdateBitmapDisplay(ALogNode: TLogNode);
begin
  if Assigned(ALogNode.MessageData) then
  begin
    ALogNode.MessageData.Position := 0;
    imgBitmap.Picture.Bitmap.LoadFromStream(ALogNode.MessageData);
    pgcMessageDetails.ActivePage := tsImageViewer;
    with imgBitmap.Picture do
    begin
      edtWidth.Text       := Bitmap.Width.ToString;
      edtHeight.Text      := Bitmap.Height.ToString;
      edtPixelFormat.Text := Reflect.EnumName(Bitmap.PixelFormat);
      edtHandleType.Text  := Reflect.EnumName(Bitmap.HandleType);
    end;
  end;
end;

procedure TfrmMessageList.UpdateCallStack(ALogNode: TLogNode);
var
  I   : Integer;
  CSD : TCallStackData;
  LN  : TLogNode;
  VN  : PVirtualNode;
begin
  FCallStack.Clear;
  VN := ALogNode.VTNode;
  I := FLogTreeView.GetNodeLevel(VN);
  while I > 0 do
  begin
    LN := FLogTreeView.GetNodeData<TLogNode>(VN.Parent);
    CSD := TCallStackData.Create;
    CSD.Title := LN.Text;
    CSD.Level := I;
    FCallStack.Add(CSD);
    VN := VN.Parent;
    Dec(I);
  end;
end;

procedure TfrmMessageList.UpdateColorDisplay(ALogNode: TLogNode);
var
  I : Integer;
  S : string;
begin
  I := ALogNode.Value.IndexOf('(');
  if I > 1 then
  begin
    S := Copy(ALogNode.Value, 1, I - 1);
  end
  else
    S := Trim(ALogNode.Value);
  if ALogNode.MessageType = lmtAlphaColor then
    // First byte in Alphacolors is the transparancy channel
    pnlColor.Color := AlphaColorToColor(S.ToInt64)
  else
    pnlColor.Color := S.ToInteger;
end;

procedure TfrmMessageList.UpdateComponentDisplay(ALogNode: TLogNode);
var
  LStream : TStringStream;
begin
  if Assigned(ALogNode.MessageData) then
  begin
    LStream := TStringStream.Create('', TEncoding.ANSI);
    try
      pgcMessageDetails.ActivePage := tsTextViewer;
      ALogNode.MessageData.Position := 0;
      ObjectBinaryToText(ALogNode.MessageData, LStream);
      LStream.Position := 0;
      FEditorView.Text := LStream.DataString;
      FEditorView.HighlighterName := 'DFM';
    finally
      FreeAndNil(LStream);
    end;
  end
  else
  begin
    FEditorView.Text := ALogNode.Value;
  end;
end;

procedure TfrmMessageList.UpdateDataSetDisplay(ALogNode: TLogNode);
begin
  pgcMessageDetails.ActivePage := tsDataSet;
  ALogNode.MessageData.Position := 0;
  FDataSet.LoadFromStream(ALogNode.MessageData);
  dscMain.DataSet := FDataSet;
  FDBGridView.AutoSizeCols;
end;

procedure TfrmMessageList.UpdateMessageDetails(ALogNode: TLogNode);
begin
  ClearMessageDetailsControls;
  FWatchesView.UpdateView(ALogNode.Id);
  case ALogNode.MessageType of
    lmtCallStack, {lmtException,} lmtHeapInfo, lmtCustomData:
      UpdateTextStreamDisplay(ALogNode);
    lmtAlphaColor, lmtColor:
      UpdateColorDisplay(ALogNode);
    lmtComponent:
      UpdateComponentDisplay(ALogNode);
    lmtBitmap:
      UpdateBitmapDisplay(ALogNode);
    lmtMemory:
    begin
      //edtHex.OpenStream(ALogNode.MessageData);
//        pgcMessageDetails.ActivePageIndex := 3;
    end;
    lmtEnterMethod, lmtLeaveMethod, lmtText,
    lmtInfo, lmtWarning, lmtError, lmtConditional:
      UpdateTextDisplay(ALogNode);
    lmtValue, lmtObject, lmtPersistent, lmtInterface, lmtStrings, lmtCheckpoint:
      UpdateValueDisplay(ALogNode);
    lmtDataSet:
      UpdateDataSetDisplay(ALogNode);
  end;
  edtMessageType.Text := LogMessageTypeNameOf(ALogNode.MessageType);
  edtTimeStamp.Text   :=
    FormatDateTime('dd:mm:yyyy hh:nn:ss:zzz', ALogNode.TimeStamp);
  edtValue.Text       := ALogNode.Value;
  edtValueName.Text   := ALogNode.ValueName;
  edtValueType.Text   := ALogNode.ValueType;
end;

procedure TfrmMessageList.UpdateTextDisplay(ALogNode: TLogNode);
var
  S : string;
begin
  pgcMessageDetails.ActivePage := tsTextViewer;
  FEditorView.Text := ALogNode.Value;
  S := Trim(ALogNode.ValueName);
  if S <> '' then
   FEditorView.HighlighterName := S;
end;

procedure TfrmMessageList.UpdateTextStreamDisplay(ALogNode: TLogNode);
var
  LStream : TStringStream;
begin
  if ALogNode.MessageData = nil then
  begin
    FEditorView.Text := '';
  end
  else
  begin
    ALogNode.MessageData.Position := 0;
    LStream := TStringStream.Create('', TEncoding.ANSI);
    try
      pgcMessageDetails.ActivePage := tsTextViewer;
      LStream.Position := 0;
      LStream.CopyFrom(ALogNode.MessageData, ALogNode.MessageData.Size);
      LStream.Position := 0;
      FEditorView.Text := LStream.DataString;
      FEditorView.HighlighterName := 'TXT';
    finally
      LStream.Free;
    end;
  end;
end;

procedure TfrmMessageList.UpdateLogTreeView;
begin
  FLogTreeView.BeginUpdate;
  try
    FLogTreeView.IterateSubtree(nil, FLogTreeViewFilterCallback, nil);
    FLogTreeView.Header.AutoFitColumns;
  finally
    FLogTreeView.EndUpdate;
  end;
end;

procedure TfrmMessageList.UpdateValueDisplay(ALogNode: TLogNode);
var
  DR : DynamicRecord;
  SL : TStringList;
begin
  FEditorView.Text := ALogNode.Value;
  FEditorView.HighlighterName := 'TXT';
  if ALogNode.Value.Contains('=') then
  begin
    pgcMessageDetails.ActivePage := tsValueList;
    DR.FromString(ALogNode.Value);
    FValueList.Data := DR;
  end
  else if ALogNode.Value.Contains(#13#10) then
  begin
    SL := TStringList.Create;
    try
     SL.Text := ALogNode.Value;
     DR.FromArray<string>(SL.ToStringArray, True);
    finally
      SL.Free;
    end;
    pgcMessageDetails.ActivePage := tsValueList;
    FValueList.Data := DR;
  end
  else
  begin
    pgcMessageDetails.ActivePage := tsTextViewer;
  end;
end;

procedure TfrmMessageList.UpdateView;
begin
  FUpdate := True;
end;
{$ENDREGION}
end.
