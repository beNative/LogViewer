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

unit LogViewer.MessageList.View;

{ Log message viewer form. }

interface

{$REGION 'documentation'}
{ Message viewer responsible for displaying all messages from an associated
  log channel (IChannelReceiver receiver instance) }

{
  TODO:
    - auto show message details, watches window and callstack (WinODS)
}
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ImgList, Vcl.Grids, Vcl.DBGrids,
  Data.DB,

  FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Comp.DataSet,
  FireDAC.Stan.StorageBin,

  VirtualTrees,

  Spring, Spring.Collections,

  DDuce.Editor.Interfaces, DDuce.Logger.Interfaces, DDuce.Components.ValueList,
  DDuce.Components.DBGridView,

  LogViewer.Messages.Data, LogViewer.Watches.Data, LogViewer.Watches.View,
  LogViewer.Interfaces, LogViewer.CallStack.Data, LogViewer.CallStack.View,
  LogViewer.ValueList.View, LogViewer.MessageList.Settings,
  LogViewer.MessageList.LogNode, LogViewer.DisplayValues.Settings;

type
  TfrmMessageList = class(TForm, ILogViewer)
    {$REGION 'designer controls'}
    dscMain           : TDataSource;
    edtHandleType     : TLabeledEdit;
    edtHeight         : TLabeledEdit;
    edtMessageFilter  : TButtonedEdit;
    edtPixelFormat    : TLabeledEdit;
    edtWidth          : TLabeledEdit;
    imgBitmap         : TImage;
    imlMessageTypes   : TImageList;
    pgcMessageData    : TPageControl;
    pgcMessageDetails : TPageControl;
    pnlCallStack      : TPanel;
    pnlCallStackTitle : TPanel;
    pnlCallStackWatch : TPanel;
    pnlFilter         : TPanel;
    pnlImageViewer    : TPanel;
    pnlLeft           : TPanel;
    pnlLeftBottom     : TPanel;
    pnlMessageContent : TPanel;
    pnlMessages       : TPanel;
    pnlRawMessageData : TPanel;
    pnlRight          : TPanel;
    pnlTextViewer     : TPanel;
    pnlWatches        : TPanel;
    sbxImage          : TScrollBox;
    splLeftHorizontal : TSplitter;
    splLeftVertical   : TSplitter;
    splVertical       : TSplitter;
    tsDataSet         : TTabSheet;
    tsImageViewer     : TTabSheet;
    tsMessageView     : TTabSheet;
    tsRawData         : TTabSheet;
    tsTextViewer      : TTabSheet;
    tsValueList       : TTabSheet;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtMessageFilterMouseEnter(Sender: TObject);
    procedure edtMessageFilterMouseLeave(Sender: TObject);
    procedure pnlMessagesResize(Sender: TObject);

  private class var
    FCounter : Integer;

  private
    FUpdate                      : Boolean; // trigger UpdateActions of Manager
    FMessageCount                : Int64;
    FCurrentMsg                  : TLogMessage;
    FCallStack                   : IList<TCallStackData>;
    FWatches                     : TWatchList;
    FLogTreeView                 : TVirtualStringTree;
    FSubscriber                  : ISubscriber;
    FCallStackView               : TfrmCallStackView;
    FWatchesView                 : TfrmWatchesView;
    FManager                     : ILogViewerManager;
    FEditorView                  : IEditorView;
    FExpandParents               : Boolean;
    FLastParent                  : PVirtualNode;
    FLastNode                    : PVirtualNode;
    FVKPressed                   : Boolean;
    FSettings                    : TMessageListSettings;
    FAutoSizeColumns             : Boolean;
    FValueList                   : TfrmValueList;
    FDataSet                     : TFDMemTable;
    FDBGridView                  : TDBGridView;
    FMiliSecondsBetweenSelection : Integer;

    {$REGION 'property access methods'}
    function GetManager: ILogViewerManager;
    function GetActions: ILogViewerActions;
    function GetSubscriber: ISubscriber;
    function GetForm: TCustomForm;
    function GetSettings: TMessageListSettings;
    function GetDisplayValuesSettings: TDisplayValuesSettings;
    function GetIsActiveView: Boolean;
    function GetEditorView: IEditorView;
    {$ENDREGION}

    {$REGION 'event handlers'}
    {$REGION 'FLogTreeView event handlers'}
    procedure FLogTreeViewFilterCallback(
      Sender    : TBaseVirtualTree;
      Node      : PVirtualNode;
      Data      : Pointer;
      var Abort : Boolean
    );
    procedure FLogTreeViewDblClick(Sender: TObject);
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
    procedure FLogTreeViewAfterCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellRect        : TRect
    );
    procedure FLogTreeViewBeforeItemPaint(
      Sender         : TBaseVirtualTree;
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      ItemRect       : TRect;
      var CustomDraw : Boolean
    );
    procedure FLogTreeViewPaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure FLogTreeViewHotChange(
      Sender  : TBaseVirtualTree;
      OldNode : PVirtualNode;
      NewNode : PVirtualNode
    );
    procedure FLogTreeViewMeasureItem(
      Sender: TBaseVirtualTree;
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      var NodeHeight : Integer
    );
    procedure FLogTreeViewCollapsed(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FLogTreeViewExpanded(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    {$ENDREGION}

    procedure FSettingsChanged(Sender: TObject);
    procedure FSubscriberReceiveMessage(
      Sender  : TObject;
      AStream : TStream
    );
    {$ENDREGION}

    function IsCollapsedTracingNode(
      ATree : TBaseVirtualTree;
      ANode : PVirtualNode
    ): Boolean;
    function GetMilliSecondsBetweenSelection: Integer;
    procedure EnsureIsActiveViewIfFocused;

  protected

    procedure Clear;
    procedure AutoFitColumns;
    procedure ApplySettings;

    procedure ProcessMessage(AStream: TStream);

    procedure AddMessageToTree(const AMessage: TLogMessage);

    procedure UpdateCallStackDisplay(ALogNode: TLogNode);
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
    procedure CreateValueListView;

    procedure CollapseAll;
    procedure ExpandAll;
    procedure SetFocusToFilter;
    procedure SelectAll;
    procedure ClearSelection;

    procedure Activate; override;
    procedure UpdateActions; override;
    procedure UpdateView;
    procedure UpdateTreeColumns;

    procedure GotoFirst;
    procedure GotoLast;

    property IsActiveView: Boolean
      read GetIsActiveView;

    property EditorView: IEditorView
      read GetEditorView;

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

    property MilliSecondsBetweenSelection: Integer
      read GetMilliSecondsBetweenSelection;

  public
    constructor Create(
      AOwner      : TComponent;
      AManager    : ILogViewerManager;
      ASubscriber : ISubscriber;
      ASettings   : TMessageListSettings
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.StrUtils, System.UITypes, System.DateUtils, System.Math,
  System.UIConsts,

  Spring.Helpers,

  DDuce.Factories.VirtualTrees, DDuce.Editor.Factories, DDuce.Reflect,
  DDuce.Utils, DDuce.Factories.GridView, DDuce.Logger,

  DDuce.ObjectInspector.zObjectInspector, DDuce.DynamicRecord,

  LogViewer.Manager, LogViewer.Factories, LogViewer.Resources;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmMessageList.Create(AOwner: TComponent; AManager
  : ILogViewerManager; ASubscriber: ISubscriber; ASettings: TMessageListSettings);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  Guard.CheckNotNull(ASubscriber, 'ASubscriber');
  Guard.CheckNotNull(ASettings, 'ASettings');
  FManager    := AManager;
  FSubscriber := ASubscriber;
  FSettings   := ASettings;
end;

procedure TfrmMessageList.AfterConstruction;
begin
  inherited AfterConstruction;
  Inc(FCounter);
  FMiliSecondsBetweenSelection := -1;
  edtMessageFilter.OnRightButtonClick :=
    FManager.Actions.Items['actFilterMessages'].OnExecute;
  FExpandParents           := True;
  CreateEditor;
  CreateLogTreeView;
  CreateWatchesView;
  CreateCallStackView;
  CreateValueListView;
  pgcMessageData.ActivePage := tsMessageView;
  // TEMP TSI
  tsRawData.TabVisible      := True;
//  tsRawData.TabVisible      := False;
//  tsMessageView.TabVisible  := False;
//  tsValueList.TabVisible    := False;
//  tsImageViewer.TabVisible  := False;
//  tsDataSet.TabVisible      := False;
//  tsTextViewer.TabVisible   := False;

  Caption := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  Logger.Info('Creating new message viewer (%s)', [Caption]);

  Subscriber.OnReceiveMessage.Add(FSubscriberReceiveMessage);
  FSettings.OnChanged.Add(FSettingsChanged);

  FLogTreeView.PopupMenu := Manager.Menus.LogTreeViewerPopupMenu;

  FDataSet    := TFDMemTable.Create(Self);
  FDBGridView := TGridViewFactory.CreateDBGridView(Self, tsDataSet, dscMain);
  FDBGridView.BorderStyle := bsNone;

  ApplySettings;
end;

procedure TfrmMessageList.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  if Assigned(FSettings) then
  begin
    FSettings.LeftPanelWidth  := pnlLeft.Width;
    FSettings.RightPanelWidth := pnlRight.Width;
    FSettings.OnChanged.Remove(FSettingsChanged);
    FSettings := nil;
  end;
  if Assigned(FSubscriber) then
  begin
    FSubscriber.OnReceiveMessage.Remove(FSubscriberReceiveMessage);
    FSubscriber := nil;
  end;
  FCallStack  := nil;
  FreeAndNil(FValueList);
  FreeAndNil(FWatches);
  FreeAndNIl(FWatchesView);
  FreeAndNil(FCallStackView);
  FreeAndNil(FLogTreeView);
  FreeAndNil(FDataSet);
  FreeAndNil(FDBGridView);
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
  EditorView.Settings.EditorOptions.WordWrapEnabled := True;
end;

{ Creates and initializes the TVirtualStringTree component. }

procedure TfrmMessageList.CreateLogTreeView;
var
  C : TVirtualTreeColumn;
  B : Boolean;
begin
  FLogTreeView := TVirtualStringTreeFactory.CreateTreeList(Self, pnlMessages);
  FLogTreeView.AlignWithMargins := False;
  FLogTreeView.TreeOptions.AutoOptions := FLogTreeView.TreeOptions.AutoOptions +
    [toAutoSpanColumns];
  FLogTreeView.TreeOptions.PaintOptions := [
    toHideFocusRect, toHotTrack, toPopupMode, toShowBackground, toShowButtons,
    toShowDropmark, toStaticBackground, toShowRoot, toShowVertGridLines,
    toThemeAware, toUseBlendedImages, toUseBlendedSelection {, toShowTreeLines},
    toStaticBackground, toUseExplorerTheme
  ];
//  FLogTreeView.TreeOptions.MiscOptions := FLogTreeView.TreeOptions.MiscOptions
//    +  [toVariableNodeHeight];

  FLogTreeView.NodeDataSize := SizeOf(TLogNode);
  FLogTreeView.Images       := imlMessageTypes;
  FLogTreeView.HintMode     := hmTooltip;
  FLogTreeView.ShowHint     := True;
  FLogTreeView.LineMode     := lmBands;

  FLogTreeView.OnBeforeItemPaint := FLogTreeViewBeforeItemPaint;
  FLogTreeView.OnBeforeCellPaint := FLogTreeViewBeforeCellPaint;
  FLogTreeView.OnAfterCellPaint  := FLogTreeViewAfterCellPaint;
  FLogTreeView.OnFocusChanged    := FLogTreeViewFocusChanged;
  FLogTreeView.OnFocusChanging   := FLogTreeViewFocusChanging;
  FLogTreeView.OnDblClick        := FLogTreeViewDblClick;
  FLogTreeView.OnHotChange       := FLogTreeViewHotChange;
  FLogTreeView.OnInitNode        := FLogTreeViewInitNode;
  FLogTreeView.OnFreeNode        := FLogTreeViewFreeNode;
  FLogTreeView.OnGetText         := FLogTreeViewGetText;
  FLogTreeView.OnPaintText       := FLogTreeViewPaintText;
  FLogTreeView.OnGetImageIndex   := FLogTreeViewGetImageIndex;
  FLogTreeView.OnKeyPress        := FLogTreeViewKeyPress;
  FLogTreeView.OnMeasureItem     := FLogTreeViewMeasureItem;
  FLogTreeView.OnCollapsed       := FLogTreeViewCollapsed;
  FLogTreeView.OnExpanded        := FLogTreeViewExpanded;

  B := Supports(Subscriber, IWinIPC) or Supports(Subscriber, IZMQ);

  C := FLogTreeView.Header.Columns.Add; // logging level
  C.Text     := '';
  C.Margin   := 0;
  C.Spacing  := 0;
  C.Width    := 10;
  C.MinWidth := 10;
  C.MaxWidth := 10;

  C := FLogTreeView.Header.Columns.Add; // message type
  C.Text     := '';
  C.Options  := C.Options - [coSmartResize, coAutoSpring];
  C.Width    := 100;
  C.MinWidth := 100;
  C.MaxWidth := 2000;

  C := FLogTreeView.Header.Columns.Add; // name
  C.Text     := SName;
  C.Options  := C.Options + [coSmartResize, coAutoSpring];
  if not B then
    C.Options  := C.Options - [coVisible];
  C.Width    := 100;
  C.MinWidth := 100;
  C.MaxWidth := 2000;

  C := FLogTreeView.Header.Columns.Add; // value
  C.Text     := SValue;
  C.Options  := C.Options + [coSmartResize, coAutoSpring];
  C.Width    := 100;
  C.MinWidth := 100;
  C.MaxWidth := 2000;

  C := FLogTreeView.Header.Columns.Add;
  C.Text    := SType;
  C.Options := C.Options + [coSmartResize, coAutoSpring];
  if not B then
    C.Options  := C.Options - [coVisible];
  C.Width     := 70;
  C.MinWidth  := 75;
  C.MaxWidth  := 2000;

  C := FLogTreeView.Header.Columns.Add; // timestamp
  C.Text     := STimestamp;
  C.Width    := 80;
  C.MinWidth := 80;
  C.MaxWidth := 80;
  C.Alignment := taRightJustify;

  FLogTreeView.Header.AutoSizeIndex := COLUMN_MAIN;
  FLogTreeView.Header.Options       :=
    FLogTreeView.Header.Options + [hoFullRepaintOnResize];
end;

procedure TfrmMessageList.CreateValueListView;
begin
  FValueList := TfrmValueList.Create(Self);
  AssignFormParent(FValueList, tsValueList);
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

function TfrmMessageList.GetEditorView: IEditorView;
begin
  Result := FEditorView;
end;

function TfrmMessageList.GetForm: TCustomForm;
begin
  Result := Self;
end;

function TfrmMessageList.GetIsActiveView: Boolean;
begin
  Result := Assigned(Manager.ActiveView)
    and (Manager.ActiveView = (Self as ILogViewer));
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
    edtMessageFilter.Color := clBtnFace;
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

procedure TfrmMessageList.edtMessageFilterMouseEnter(Sender: TObject);
begin
  if not edtMessageFilter.Focused then
    edtMessageFilter.Color := clWhite;
end;

procedure TfrmMessageList.edtMessageFilterMouseLeave(Sender: TObject);
begin
  if not edtMessageFilter.Focused then
    edtMessageFilter.Color := clBtnFace;
end;
{$ENDREGION}

{$REGION 'FLogTreeView'}
procedure TfrmMessageList.FLogTreeViewBeforeItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var CustomDraw: Boolean);
//var
//  LN  : TLogNode;
//  DVS : TDisplayValuesSettings;
//  S   : string;
begin
//  LN := Sender.GetNodeData<TLogNode>(Node);
//  DVS := Manager.Settings.DisplayValuesSettings;
//  if LN.MessageType in [lmtEnterMethod, lmtLeaveMethod] then
//  begin
//    CustomDraw := True;
//    DVS.Tracing.AssignTo(TargetCanvas);
//    TargetCanvas.Brush.Color := $00EEEEEE;
//    TargetCanvas.FillRect(ItemRect);
//    S := LN.Text;
//    TargetCanvas.TextRect(ItemRect, S);
//  end;
end;

{ Do not show Enter and Leave nodes when collapsed. }

procedure TfrmMessageList.FLogTreeViewCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LN : TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  if Assigned(LN.VTNode.NextSibling) then
  begin
    Sender.IsVisible[LN.VTNode.NextSibling] := False;
  end
end;

procedure TfrmMessageList.FLogTreeViewDblClick(Sender: TObject);
begin
  UpdateTreeColumns;
  AutoFitColumns;
end;

{ Show both Enter and Leave nodes when expanded. }

procedure TfrmMessageList.FLogTreeViewExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LN : TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  if Assigned(LN.VTNode.NextSibling) then
  begin
    Sender.IsVisible[LN.VTNode.NextSibling] := True;
  end
end;

procedure TfrmMessageList.FLogTreeViewAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  LN : TLogNode;
  PN : TLogNode;
  N  : Int64;
  S  : string;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  if Column = COLUMN_TIMESTAMP then
  begin
    S := Format('<fc=clBlue>%s</fc>',
            [FormatDateTime('hh:nn:ss:zzz',  LN.TimeStamp)]);
    if not (LN.MessageType in [lmtEnterMethod, lmtLeaveMethod]) then
    begin
      if Assigned(LN.VTNode.PrevSibling) then
        PN := Sender.GetNodeData<TLogNode>(LN.VTNode.PrevSibling)
      else if Assigned(LN.VTNode.Parent) and (LN.VTNode.Parent <> Sender.RootNode) then
        PN := Sender.GetNodeData<TLogNode>(LN.VTNode.Parent)
      else
        PN := nil;
      if Assigned(PN) then
      begin
        if MilliSecondOf(PN.TimeStamp) <= MilliSecondOf(LN.TimeStamp) then
        begin
          N := MilliSecondsBetween(PN.TimeStamp, LN.TimeStamp);
          if N < 1000 then
          begin
            S := Format('<fc=clSilver>%s:</fc><fc=clBlue>%s</fc>',
            [FormatDateTime('hh:nn:ss',  LN.TimeStamp),
             FormatDateTime('zzz',  LN.TimeStamp)]);
          end;
        end;
      end;
    end;
    DrawFormattedText(CellRect, TargetCanvas, S);
  end;
end;

procedure TfrmMessageList.FLogTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LN      : TLogNode;
  PN      : TLogNode;
  N       : Int64;
  DVS     : TDisplayValuesSettings;
  LIndent : Integer;
  LNode   : PVirtualNode;
  S       : string;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  DVS := Manager.Settings.DisplayValuesSettings;
  if (LN.MessageType in [lmtEnterMethod, lmtLeaveMethod])
    and ((Column = COLUMN_MAIN) or (Column = COLUMN_TIMESTAMP)) then
  begin
    DVS.Tracing.AssignTo(TargetCanvas);
    //TargetCanvas.FillRect(ContentRect);
    TargetCanvas.FillRect(CellRect);
    if IsCollapsedTracingNode(Sender, Node) then
    begin
      Sender.IsVisible[Node.NextSibling] := False;
    end;
  end;
  if Column = COLUMN_LEVEL then
  begin
    // TODO : logging level colors
  end;
  // draw indentation lines
  if Column = Sender.Header.MainColumn then
  begin
    LIndent := Sender.GetNodeLevel(Node) * FLogTreeView.Indent;
    Inc(CellRect.Left, LIndent);
    LIndent := -Integer(FLogTreeView.Indent);
//    TargetCanvas.Brush.Color := clYellow;
//    TargetCanvas.FillRect(CellRect);
    CellRect.Right := CellRect.Left + Integer(FLogTreeView.Indent);
    Inc(CellRect.Bottom);
    LNode := Node;
    repeat
      TargetCanvas.Brush.Color := DVS.Tracing.BackgroundColor;
      TargetCanvas.FillRect(CellRect);
      LNode := LNode.Parent;
      if not Assigned(LNode) or (LNode = Sender.RootNode) then
        Break;
      Inc(CellRect.Left, LIndent);
      Inc(CellRect.Right, LIndent);
      //DoGetBackColor(ANode, C);
    until False;
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
    UpdateCallStackDisplay(LN);
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
  S  : string;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  S  := edtMessageFilter.Text;
  if Assigned(LN) then
  begin
    if (LN.MessageType = lmtText) and (LN.Highlighter <> '') then
    begin
      B := Settings.VisibleValueTypes.IndexOf(LN.Highlighter) <> -1;
    end
    else
    begin
      B := LN.MessageType in Settings.VisibleMessageTypes;
    end;
    if S <> '' then
    begin
      B := B and (ContainsText(LN.Text, S) or ContainsText(LN.ValueName, S) or
       ContainsText(LN.Value, S));
    end;
    Sender.IsVisible[Node] := B;
  end;
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
      if not (vsExpanded in Node.States) and (ND.MessageType = lmtEnterMethod)
        and (Node.ChildCount > 0) and Assigned(ND.VTNode.NextSibling) then
      begin
        ImageIndex := 9;
      end;
    end
    else if ND.MessageType = lmtDataSet then
    begin
      ImageIndex := 20;
    end
    else if ND.MessageType = lmtAction then
    begin
      ImageIndex := 21;
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
  LN     : TLogNode;
  S      : string;
  LDelim : array of string;
  LTrim  : TArray<string>;
  PN     : TLogNode;
  N      : Int64;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'ND');
  if Column = COLUMN_LEVEL then
  begin
    CellText := LN.LogLevel.ToString;
  end
  else if Column = COLUMN_MAIN then
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
      lmtAction:
        CellText := SAction;
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
    LDelim := [sLineBreak];
    LTrim := LN.Value.Split(LDelim, 1, TStringSplitOptions.ExcludeEmpty);
    if Length(LTrim) > 0 then
      S := LTrim[0];
    if (S.Length > MAX_TEXTLENGTH_VALUECOLUMN) then
      S := S.Substring(0, MAX_TEXTLENGTH_VALUECOLUMN);
    CellText := S;
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    CellText := '';
//    if not (LN.MessageType in [lmtEnterMethod, lmtLeaveMethod]) then
//    begin
//      if Assigned(LN.VTNode.PrevSibling) then
//        PN := Sender.GetNodeData<TLogNode>(LN.VTNode.PrevSibling)
//      else if Assigned(LN.VTNode.Parent) and (LN.VTNode.Parent <> Sender.RootNode) then
//        PN := Sender.GetNodeData<TLogNode>(LN.VTNode.Parent)
//      else
//        PN := nil;
//      if Assigned(PN) then
//      begin
//        if MilliSecondOf(PN.TimeStamp) <= MilliSecondOf(LN.TimeStamp) then
//        begin
//          N := MilliSecondsBetween(PN.TimeStamp, LN.TimeStamp);
//          if N < 1000 then
//          begin
//            CellText := FormatDateTime('zzz',  LN.TimeStamp);
//          end;
//        end;
//      end;
//    end;
//    if CellText.IsEmpty and (YearOf(LN.TimeStamp) = YearOf(Now)) then // sanity check
//    begin
//      CellText := FormatDateTime('hh:nn:ss:zzz',  LN.TimeStamp);
//    end;
  end;
end;

{ Measures time difference between selected node and hot node. }

procedure TfrmMessageList.FLogTreeViewHotChange(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode);
var
  LN1 : TLogNode;
  LN2 : TLogNode;
begin
  LN1 := Sender.GetNodeData<TLogNode>(NewNode);
  LN2 := Sender.GetNodeData<TLogNode>(Sender.FocusedNode);
  if Assigned(LN1) and Assigned(LN2) then
  begin
    FMiliSecondsBetweenSelection :=
      MilliSecondsBetween(LN1.TimeStamp, LN2.TimeStamp);
  end
  else
    FMiliSecondsBetweenSelection := -1;
end;

procedure TfrmMessageList.FLogTreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LN    : TLogNode;
  I     : Integer;
  S     : string;
  SL    : TStringList;
  LText : string;
begin
  LN := TLogNode.Create;
  Node.SetData(LN);
  LN.MessageData := FCurrentMsg.Data;
  LN.TimeStamp   := FCurrentMsg.TimeStamp;
  LN.MessageType := TLogMessageType(FCurrentMsg.MsgType);
  LN.VTNode      := Node;
  LN.LogLevel    := FCurrentMsg.LogLevel;
  LN.Id          := FMessageCount;
  LText          := string(FCurrentMsg.Text);
  case LN.MessageType of
    lmtValue, lmtComponent, lmtStrings, lmtPersistent, lmtObject, lmtInterface:
    begin
      S := LText;
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
      LN.ValueName := LText;
      LN.ValueType := 'TDataSet';
    end;
    lmtAction:
    begin
      LN.ValueName := LText;
      LN.ValueType := 'TAction';
    end;
    lmtBitmap, lmtScreenShot:
    begin
      LN.ValueName := LText;
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
        SL.Text := Trim(string(FCurrentMsg.Text));
        if SL.Count > 0 then
        begin
          S := SL[0];
          I := S.IndexOf('(');
          if I > 1 then
          begin
            LN.ValueName := Copy(S, 1, I);
            LN.Highlighter := Trim(ExtractText(S, '(', ')'));
          end;
          if I <> -1 then
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
    end
  end; // case LN.MessageType of
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

procedure TfrmMessageList.FLogTreeViewMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
//var
//  LN  : TLogNode;
//  PN  : TLogNode;
begin
//  LN := Sender.GetNodeData<TLogNode>(Node);
//  if (LN.MessageType in [lmtLeaveMethod]) and Assigned(LN.VTNode.PrevSibling) then
//  begin
//    PN := Sender.GetNodeData<TLogNode>(LN.VTNode.PrevSibling);
//    if not (vsExpanded in PN.VTNode.States) then
//    begin
//      NodeHeight := 0;
//    end;
//  end;

//  if Assigned(LN.VTNode.PrevSibling) and Sender.GetNodeData<TLogNode>(Node)
//  if LN.MessageType in [lmtLeaveMethod] and then
//
//if  then
//   vsExpanded
end;

{ Triggered before either normal or fixed text is painted to allow finer
  customization (kind of sub cell painting). Hereit is used to apply custom
  settings to TargetCanvas.Font. }

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
        TargetCanvas.Font.Size := 7;
      end;
      lmtEnterMethod:
      begin
        if IsCollapsedTracingNode(Sender, Node) then
          DVS.Tracing.AssignTo(TargetCanvas.Font)
        else
          DVS.Enter.AssignTo(TargetCanvas.Font);
      end;
      lmtLeaveMethod:
      begin
        DVS.Leave.AssignTo(TargetCanvas.Font);
      end;
      lmtCheckpoint:
      begin
        DVS.CheckPoint.AssignTo(TargetCanvas.Font);
      end;
      lmtAction:
      begin
        DVS.Action.AssignTo(TargetCanvas.Font);
      end;
    end;
  end
  else if Column = COLUMN_LEVEL then
  begin
    TargetCanvas.Font.Color := clDkGray;
    TargetCanvas.Font.Size := 6;
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

procedure TfrmMessageList.pnlMessagesResize(Sender: TObject);
begin
  FUpdate          := True;
  FAutoSizeColumns := False;
end;
{$ENDREGION}

procedure TfrmMessageList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmMessageList.FSubscriberReceiveMessage(Sender: TObject;
  AStream: TStream);
begin
  ProcessMessage(AStream);
end;

procedure TfrmMessageList.FSettingsChanged(Sender: TObject);
begin
  ApplySettings;
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Makes sure the active view is the current view when it is focused. }

procedure TfrmMessageList.EnsureIsActiveViewIfFocused;
var
  B : Boolean;
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
end;

function TfrmMessageList.GetMilliSecondsBetweenSelection: Integer;
begin
  Result := FMiliSecondsBetweenSelection;
end;

function TfrmMessageList.IsCollapsedTracingNode(ATree: TBaseVirtualTree;
  ANode: PVirtualNode): Boolean;
var
  LN : TLogNode;
begin
  if not (vsExpanded in ANode.States) and (ANode.ChildCount > 0) and
    Assigned(ANode.NextSibling) then
  begin
    LN := ATree.GetNodeData<TLogNode>(ANode);
    Result := LN.MessageType = lmtEnterMethod;
  end
  else
    Result := False;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessageList.ApplySettings;
begin
//  chkAutoFilter.Checked := Settings.AutoFilterMessages;
  pnlLeft.Width         := Settings.LeftPanelWidth;
  pnlRight.Width        := Settings.RightPanelWidth;
//  DisplayValuesSettings.TimeStamp.AssignTo(edtTimeStamp.Font);
//  edtTimeStamp.Alignment := DisplayValuesSettings.TimeStamp.HorizontalAlignment;
//  DisplayValuesSettings.ValueName.AssignTo(edtValueName.Font);
//  edtValueName.Alignment := DisplayValuesSettings.ValueName.HorizontalAlignment;
//  DisplayValuesSettings.ValueType.AssignTo(edtValueType.Font);
//  edtValueType.Alignment := DisplayValuesSettings.ValueType.HorizontalAlignment;
//  DisplayValuesSettings.Value.AssignTo(edtValue.Font);
//  edtValue.Alignment := DisplayValuesSettings.Value.HorizontalAlignment;
  FUpdate := True;
end;

procedure TfrmMessageList.AutoFitColumns;
begin
  Logger.Track(Self, 'AutoFitColumns');
  FLogTreeView.Header.AutoFitColumns(False, smaUseColumnOption, 2);
  FAutoSizeColumns := True;
end;

procedure TfrmMessageList.Activate;
begin
  inherited Activate;
  Manager.ActiveView := Self as ILogViewer;
  Manager.EditorManager.ActiveView := FEditorView;
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
    if AutoScroll then
      FAutoSizeColumns := False
    else
     FAutoSizeColumns := True;
  finally
    FLogTreeView.EndUpdate;
  end;
end;

procedure TfrmMessageList.ClearMessageDetailsControls;
begin
  imgBitmap.Picture   := nil;
  edtWidth.Text       := '';
  edtHeight.Text      := '';
  edtPixelFormat.Text := '';
  edtHandleType.Text  := '';
  FDataSet.Active     := False;
  EditorView.Clear;
  if Assigned(FValueList) then
    FValueList.Clear;
end;

{ Reads the received message stream from the active logchannel. }

{$REGION 'documentation'}
{
   Message layout in stream
     1. Message type (1 byte)          => TLogMessage.MsgType (TLogMessageType)
     2. Log level    (1 byte)
     3. Reserved     (1 byte)
     4. Reserved     (1 byte)
     5. Timestamp    (8 byte)          => TLogMessage.TimeStamp
     6. Text size    (4 byte)
     7. Text         (variable size)   => TLogMessage.Text
     8. Data size    (4 byte)
     9. Data         (variable size)   => TLogMessage.Data

    TLogMessage = packed record
      MsgType   : Byte; // (TLogMessageType)
      Level     : Byte;
      Reserved  : Byte;
      Reserved  : Byte;
      TimeStamp : TDateTime;
      Text      : UTF8String;
      Data      : TStream;
    end;
}
{$ENDREGION}

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
  AStream.ReadBuffer(FCurrentMsg.LogLevel);
  AStream.ReadBuffer(FCurrentMsg.Reserved1);
  AStream.ReadBuffer(FCurrentMsg.Reserved2);
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
      if Settings.AutoScrollMessages then
      begin
        FLogTreeView.FocusedNode := FLogTreeView.GetLast;
        FLogTreeView.Selected[FLogTreeView.FocusedNode] := True;
      end;
      FUpdate := True;
    end;
  end; // case
end;

{$REGION 'Commands'}
procedure TfrmMessageList.Clear;
begin
  ClearMessageDetailsControls;
  FWatches.Clear;
  EditorView.Clear;
  FCallStack.Clear;
  FLogTreeView.Clear;
  FMessageCount := 0;
  FLastNode     := nil;
  FLastParent   := nil;
  FUpdate       := True;
end;

procedure TfrmMessageList.ClearSelection;
begin
  FLogTreeView.ClearSelection;
end;

procedure TfrmMessageList.CollapseAll;
begin
  FLogTreeView.FullCollapse;
  FUpdate := True;
  FAutoSizeColumns := False;
end;

procedure TfrmMessageList.ExpandAll;
begin
  FLogTreeView.FullExpand;
  FUpdate := True;
  FAutoSizeColumns := False;
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
    FUpdate := True;
    FAutoSizeColumns := False;
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
    FUpdate := True;
    FAutoSizeColumns := False;
  end;
end;

procedure TfrmMessageList.SelectAll;
begin
  FLogTreeView.SelectAll(False);
end;

procedure TfrmMessageList.SetFocusToFilter;
begin
  if edtMessageFilter.CanFocus then
    edtMessageFilter.SetFocus;
end;
{$ENDREGION}

{$REGION 'Display updating'}
procedure TfrmMessageList.UpdateBitmapDisplay(ALogNode: TLogNode);
begin
  if Assigned(ALogNode.MessageData) then
  begin
    tsValueList.TabVisible   := False;
    tsImageViewer.TabVisible := False;
    tsDataSet.TabVisible     := False;
    tsTextViewer.TabVisible  := False;
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

procedure TfrmMessageList.UpdateCallStackDisplay(ALogNode: TLogNode);
var
  I   : Integer;
  CSD : TCallStackData;
  LN  : TLogNode;
  LN2 : TLogNode;
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
    LN2 :=  FLogTreeView.GetNodeData<TLogNode>(LN.VTNode.NextSibling);
    if Assigned(LN2) then
    begin
      CSD.Duration := MilliSecondsBetween(LN2.TimeStamp, LN.TimeStamp);
    end;
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
//  if ALogNode.MessageType = lmtAlphaColor then
//    // First byte in Alphacolors is the transparancy channel
//    pnlColor.Color := AlphaColorToColor(S.ToInt64)
//  else
//    pnlColor.Color := S.ToInteger;
end;

procedure TfrmMessageList.UpdateComponentDisplay(ALogNode: TLogNode);
var
  LStream : TStringStream;
begin
  if Assigned(ALogNode.MessageData) then
  begin
    LStream := TStringStream.Create('', TEncoding.ANSI);
    try
      tsValueList.TabVisible   := False;
      tsImageViewer.TabVisible := False;
      tsDataSet.TabVisible     := False;
      tsTextViewer.TabVisible  := False;
      pgcMessageDetails.ActivePage := tsTextViewer;
      ALogNode.MessageData.Position := 0;
      ObjectBinaryToText(ALogNode.MessageData, LStream);
      LStream.Position := 0;
      EditorView.Text := LStream.DataString;
      EditorView.HighlighterName := 'DFM';
    finally
      FreeAndNil(LStream);
    end;
  end
  else
  begin
    EditorView.Text := ALogNode.Value;
  end;
end;

procedure TfrmMessageList.UpdateDataSetDisplay(ALogNode: TLogNode);
begin
  tsValueList.TabVisible   := False;
  tsImageViewer.TabVisible := False;
  tsDataSet.TabVisible     := False;
  tsTextViewer.TabVisible  := False;
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
    lmtBitmap, lmtScreenShot:
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
//  edtMessageType.Text := LogMessageTypeNameOf(ALogNode.MessageType);
//  edtTimeStamp.Text   :=
//    FormatDateTime('dd:mm:yyyy hh:nn:ss:zzz', ALogNode.TimeStamp);
//  edtValue.Text       := ALogNode.Value;
//  edtValueName.Text   := ALogNode.ValueName;
//  edtValueType.Text   := ALogNode.ValueType;
end;

procedure TfrmMessageList.UpdateTextDisplay(ALogNode: TLogNode);
var
  S : string;
begin
  tsValueList.TabVisible   := False;
  tsTextViewer.TabVisible  := False;
  tsImageViewer.TabVisible := False;
  tsDataSet.TabVisible     := False;
  pgcMessageDetails.ActivePage := tsTextViewer;
  EditorView.Text := ALogNode.Value;
  S := ALogNode.Highlighter;
  if S <> '' then
   EditorView.HighlighterName := S;
end;

procedure TfrmMessageList.UpdateTextStreamDisplay(ALogNode: TLogNode);
var
  LStream : TStringStream;
begin
  tsValueList.TabVisible   := False;
  tsImageViewer.TabVisible := False;
  tsDataSet.TabVisible     := False;
  tsTextViewer.TabVisible  := False;
  pgcMessageDetails.ActivePage := tsTextViewer;
  if ALogNode.MessageData = nil then
  begin
    EditorView.Text := '';
  end
  else
  begin
    ALogNode.MessageData.Position := 0;
    LStream := TStringStream.Create('', TEncoding.ANSI);
    try
      LStream.Position := 0;
      LStream.CopyFrom(ALogNode.MessageData, ALogNode.MessageData.Size);
      LStream.Position := 0;
      EditorView.Text := LStream.DataString;
      EditorView.HighlighterName := 'TXT';
    finally
      LStream.Free;
    end;
  end;
end;

procedure TfrmMessageList.UpdateTreeColumns;
var
  LTotalWidth : Integer;
  LColumn     : TVirtualTreeColumn;
begin
  // This can cause focus to be set on the grid if ComponentState is does not
  // include csLoading. Otherwise it can cause an exception ('cannot focus a
  // disabled or invisible window'). This is the reason why this property is
  // not assigned right after creation of the treeview.
  FLogTreeView.Header.MainColumn := COLUMN_MAIN;

  LTotalWidth := FLogTreeView.ClientWidth;
  LColumn := FLogTreeView.Header.Columns[COLUMN_MAIN];
  LColumn.MinWidth := LTotalWidth div 7;
  LColumn := FLogTreeView.Header.Columns[COLUMN_VALUETYPE];
  LColumn.MaxWidth := LTotalWidth div 4;
  LColumn.MinWidth := LTotalWidth div 7;
  LColumn := FLogTreeView.Header.Columns[COLUMN_VALUE];
  LColumn.MaxWidth := LTotalWidth div 4 * 3;
  LColumn.MinWidth := LTotalWidth div 4;
  LColumn := FLogTreeView.Header.Columns[COLUMN_VALUENAME];
  LColumn.MaxWidth := LTotalWidth div 2;
  LColumn.MinWidth := LTotalWidth div 6;
end;

procedure TfrmMessageList.UpdateLogTreeView;
begin
  FLogTreeView.BeginUpdate;
  try
    FLogTreeView.IterateSubtree(nil, FLogTreeViewFilterCallback, nil);
    UpdateTreeColumns;
    if not FAutoSizeColumns then
      AutoFitColumns;
  finally
    FLogTreeView.EndUpdate;
  end;
end;

procedure TfrmMessageList.UpdateValueDisplay(ALogNode: TLogNode);
var
  DR : DynamicRecord;
  SL : TStringList;
begin
  EditorView.Text := ALogNode.Value;
  EditorView.HighlighterName  := 'INI';
  pgcMessageDetails.ActivePage := tsTextViewer;

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
{$ENDREGION}

procedure TfrmMessageList.UpdateActions;
begin
  EnsureIsActiveViewIfFocused;
  if IsActiveView and FUpdate then
  begin
    UpdateLogTreeView;
    if Assigned(Actions) then
    begin
      Actions.UpdateActions;
    end;
    FUpdate := False;
  end;
  inherited UpdateActions;
end;

{ Force an update of the message viewer. }

procedure TfrmMessageList.UpdateView;
begin
  FUpdate := True;
  FAutoSizeColumns := False;
end;
{$ENDREGION}
end.
