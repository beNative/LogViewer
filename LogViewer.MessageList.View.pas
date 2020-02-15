{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ImgList,

  VirtualTrees, kcontrols, kpagecontrol, OMultiPanel,

  Spring, Spring.Collections,

  DDuce.Editor.Interfaces, DDuce.Logger.Interfaces,

  LogViewer.Watches.Data, LogViewer.Watches.View, LogViewer.Interfaces,
  LogViewer.CallStack.Data, LogViewer.CallStack.View, LogViewer.ValueList.View,
  LogViewer.MessageList.Settings, LogViewer.MessageList.LogNode,
  LogViewer.DisplayValues.Settings, LogViewer.DataSet.View,
  LogViewer.Image.View, LogViewer.RawData.View, LogViewer.MessageData.View;

type
  TfrmMessageList = class(TForm, ILogViewer)
    {$REGION 'designer controls'}
    chkShowDetails             : TCheckBox;
    chkShowWatchHistory        : TCheckBox;
    chkSyncWithSelectedMessage : TCheckBox;
    edtMessageFilter           : TButtonedEdit;
    imlMessageTypes            : TImageList;
    pgcMessageData             : TKPageControl;
    pgcMessageDetails          : TKPageControl;
    pnlCallStack               : TPanel;
    pnlCallStackTitle          : TPanel;
    pnlFilter                  : TPanel;
    pnlLeft                    : TOMultiPanel;
    pnlMain                    : TOMultiPanel;
    pnlMessageContent          : TPanel;
    pnlMessageData             : TPanel;
    pnlMessages                : TPanel;
    pnlRight                   : TPanel;
    pnlTextViewer              : TPanel;
    pnlWatches                 : TPanel;
    pnlWatchTitle              : TPanel;
    splRightHorizontal         : TSplitter;
    tsDataSet                  : TKTabSheet;
    tsImageViewer              : TKTabSheet;
    tsMessageView              : TKTabSheet;
    tsRawData                  : TKTabSheet;
    tsTextViewer               : TKTabSheet;
    tsValueList                : TKTabSheet;
    {$ENDREGION}

    procedure chkShowWatchHistoryClick(Sender: TObject);
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
    procedure edtMessageFilterMouseEnter(Sender: TObject);
    procedure edtMessageFilterMouseLeave(Sender: TObject);
    procedure chkShowDetailsClick(Sender: TObject);
    procedure chkSyncWithSelectedMessageClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pnlMainSplitterMoved(Sender: TObject);
    procedure pnlMessagesResize(Sender: TObject);
    procedure tsRawDataShow(Sender: TObject);
    procedure pnlMainCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure edtMessageFilterExit(Sender: TObject);

  private class var
    FCounter : Integer;

  private
    FManager                     : ILogViewerManager;
    FUpdating                    : Boolean;

    // state information
    FUpdate                      : Boolean; // trigger UpdateActions of Manager
    FSubscriber                  : ISubscriber;
    FLastParent                  : PVirtualNode;
    FLastNode                    : PVirtualNode;
    FVKPressed                   : Boolean;
    FMiliSecondsBetweenSelection : Integer;

    // data
    FWatches    : TWatchList;
    FCurrentMsg : TLogMessage;
    FCallStack  : IList<TCallStackData>;

    // views
    FLogTreeView     : TVirtualStringTree;
    FCallStackView   : TfrmCallStackView;
    FWatchesView     : TfrmWatchesView;
    FMessageDataView : TfrmMessageDataView;
    FValueList       : TfrmValueListView;
    FDataSetView     : TfrmDataSetView;
    FImageView       : TfrmImageView;
    FRawDataView     : TfrmRawDataView;
    FEditorView      : IEditorView;

    // settings
    FSettings         : TMessageListSettings;
    FExpandParents    : Boolean;
    FAutoSizeColumns  : Boolean;
    FScrollToLastNode : Boolean;

    {$REGION 'property access methods'}
    function GetManager: ILogViewerManager;
    function GetActions: ILogViewerActions;
    function GetSubscriber: ISubscriber;
    function GetForm: TCustomForm;
    function GetSettings: TMessageListSettings;
    function GetDisplayValuesSettings: TDisplayValuesSettings;
    function GetIsActiveView: Boolean;
    function GetEditorView: IEditorView;
    function GetSelectedLogNode: TLogNode;
    procedure SetSelectedLogNode(const Value: TLogNode);
    function GetMessageCount: Int64;
    function GetMilliSecondsBetweenSelection: Integer;
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
    procedure FLogTreeViewAfterItemPaint(
      Sender       : TBaseVirtualTree;
      TargetCanvas : TCanvas;
      Node         : PVirtualNode;
      ItemRect     : TRect);
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
    procedure FLogTreeViewCollapsed(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FLogTreeViewExpanded(
      Sender : TBaseVirtualTree;
      Node   :  PVirtualNode
    );
    procedure FLogTreeViewExpanding(
      Sender      : TBaseVirtualTree;
      Node        : PVirtualNode;
      var Allowed : Boolean
    );
    {$ENDREGION}

    procedure FSettingsChanged(Sender: TObject);
    procedure FSubscriberReceiveMessage(
      Sender  : TObject;
      AStream : TStream
    );
    procedure WatchSettingsChanged(Sender: TObject);
    {$ENDREGION}

    function IsCollapsedTracingNode(
      ATree : TBaseVirtualTree;
      ANode : PVirtualNode
    ): Boolean;

    procedure EnsureCurrentViewIsActiveViewWhenFocused;
    function ParseValue(const AString: string): Tuple<string, string, string>;

  protected
    function GetLeftPanelVisible: Boolean;
    procedure SetLeftPanelVisible(const Value: Boolean);
    function GetRightPanelVisible: Boolean;
    procedure SetRightPanelVisible(const Value: Boolean);


    procedure Clear;
    procedure AutoFitColumns;
    procedure ApplySettings;
    procedure ApplyFilter;
    procedure LoadPanelSettings;
    procedure SavePanelSettings;

    procedure ProcessMessage(AStream: TStream);
    procedure AddMessageToTree(const AMessage: TLogMessage);
    procedure Modified;

    procedure UpdateCallStackDisplay(ALogNode: TLogNode);
    procedure UpdateMessageDetails(ALogNode: TLogNode);
    procedure UpdateComponentDisplay(ALogNode: TLogNode);
    procedure UpdateBitmapDisplay(ALogNode: TLogNode);
    procedure UpdateDataSetDisplay(ALogNode: TLogNode);
    procedure UpdateTextDisplay(ALogNode: TLogNode);
    procedure UpdateTextStreamDisplay(ALogNode: TLogNode);
    procedure UpdateRawDataDisplay(ALogNode: TLogNode);
    procedure UpdateColorDisplay(ALogNode: TLogNode);
    procedure UpdateValueDisplay(ALogNode: TLogNode);

    procedure UpdateLogTreeView;

    procedure ClearMessageDetailsControls;

    procedure CreateLogTreeView;
    procedure CreateEditor;
    procedure CreateCallStackView;
    procedure CreateWatchesView;
    procedure CreateValueListView;
    procedure CreateDataSetView;
    procedure CreateImageView;
    procedure CreateRawDataView;
    procedure CreateMessageDataView;

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

    property SelectedLogNode: TLogNode
      read GetSelectedLogNode write SetSelectedLogNode;

    property MessageCount: Int64
      read GetMessageCount;

    property LeftPanelVisible: Boolean
      read GetLeftPanelVisible write SetLeftPanelVisible;

    property RightPanelVisible: Boolean
      read GetRightPanelVisible write SetRightPanelVisible;

  public
    constructor Create(
      AOwner      : TComponent;
      AManager    : ILogViewerManager;
      ASubscriber : ISubscriber;
      ASettings   : TMessageListSettings
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils, System.UITypes, System.DateUtils, System.Math,
  Vcl.GraphUtil,

  Spring.Helpers,

  DDuce.Factories.VirtualTrees, DDuce.Editor.Factories, DDuce.Utils,
  DDuce.Logger, DDuce.DynamicRecord,

  LogViewer.Manager, LogViewer.Factories, LogViewer.Resources;

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
  chkShowWatchHistory.Checked :=
    Manager.Settings.WatchSettings.WatchHistoryVisible;
  chkSyncWithSelectedMessage.Checked :=
    Manager.Settings.WatchSettings.SyncWithSelection;
  chkShowDetails.Checked     := Settings.MessageDetailsVisible;
  splRightHorizontal.Visible := Settings.MessageDetailsVisible;
  pnlMessageData.Visible     := Settings.MessageDetailsVisible;
  FExpandParents := True;
  CreateEditor;
  CreateLogTreeView;
  CreateWatchesView;
  CreateCallStackView;
  CreateValueListView;
  CreateDataSetView;
  CreateImageView;
  CreateRawDataView;
  CreateMessageDataView;
  pgcMessageData.ActivePage := tsMessageView;
  Caption := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  Logger.Info('Creating new message viewer (%s)', [Caption]);
  ApplySettings;
  Subscriber.OnReceiveMessage.Add(FSubscriberReceiveMessage);
  FSettings.OnChanged.Add(FSettingsChanged);
  Manager.Settings.WatchSettings.OnChanged.Add(WatchSettingsChanged);
  FLogTreeView.PopupMenu := Manager.Menus.LogTreeViewerPopupMenu;
end;

destructor TfrmMessageList.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  Manager.Settings.WatchSettings.OnChanged.RemoveAll(Self);
  if Assigned(FSubscriber) then
  begin
    FSubscriber.OnReceiveMessage.RemoveAll(Self);
    FSubscriber := nil;
  end;
  if Assigned(FSettings) then
  begin
    FSettings.OnChanged.RemoveAll(Self);
    SavePanelSettings;
    FSettings := nil;
  end;
  FEditorView.Form.Free; // prevents the instance to be freed by the owning
                         // manager instance or the parent control.
  FEditorView := nil;
  FCallStack  := nil;
  FreeAndNil(FCurrentMsg.Data);
  FreeAndNil(FValueList);
  FreeAndNil(FDataSetView);
  FreeAndNil(FImageView);
  FreeAndNil(FRawDataView);
  FreeAndNIl(FWatchesView);
  FreeAndNil(FCallStackView);
  FreeAndNil(FLogTreeView);
  FreeAndNil(FWatches);
  inherited Destroy;
end;

procedure TfrmMessageList.CreateCallStackView;
begin
  FCallStack     := TCollections.CreateObjectList<TCallStackData>;
  FCallStackView := TLogViewerFactories.CreateCallStackView(
    Self,
    pnlCallStack,
    FCallStack as IObjectList,
    Manager.Settings.CallStackSettings,
    DisplayValuesSettings
  );
end;

procedure TfrmMessageList.CreateDataSetView;
begin
  FDataSetView := TfrmDataSetView.Create(Self);
  AssignFormParent(FDataSetView, tsDataSet);
end;

procedure TfrmMessageList.CreateEditor;
begin
  FEditorView := TEditorFactories.CreateView(
    pnlTextViewer,
    Manager.EditorManager
  );
  EditorView.Settings.EditorOptions.WordWrapEnabled := True;
end;

procedure TfrmMessageList.CreateImageView;
begin
  FImageView := TfrmImageView.Create(Self);
  AssignFormParent(FImageView, tsImageViewer);
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
    toHotTrack, toPopupMode, toShowBackground, toShowButtons, toShowDropmark,
    toStaticBackground, toShowRoot, toThemeAware, toUseBlendedImages,
    toUseBlendedSelection, toStaticBackground, toUseExplorerTheme
  ];
  FLogTreeView.NodeDataSize := SizeOf(TLogNode);
  FLogTreeView.Images       := imlMessageTypes;
  FLogTreeView.HintMode     := hmTooltip;
  FLogTreeView.ShowHint     := True;
  FLogTreeView.LineMode     := lmBands;

  FLogTreeView.OnBeforeCellPaint := FLogTreeViewBeforeCellPaint;
  FLogTreeView.OnAfterItemPaint  := FLogTreeViewAfterItemPaint;
  FLogTreeView.OnFocusChanged    := FLogTreeViewFocusChanged;
  FLogTreeView.OnDblClick        := FLogTreeViewDblClick;
  FLogTreeView.OnHotChange       := FLogTreeViewHotChange;
  FLogTreeView.OnInitNode        := FLogTreeViewInitNode;
  FLogTreeView.OnFreeNode        := FLogTreeViewFreeNode;
  FLogTreeView.OnGetText         := FLogTreeViewGetText;
  FLogTreeView.OnPaintText       := FLogTreeViewPaintText;
  FLogTreeView.OnGetImageIndex   := FLogTreeViewGetImageIndex;
  FLogTreeView.OnKeyPress        := FLogTreeViewKeyPress;
  FLogTreeView.OnCollapsed       := FLogTreeViewCollapsed;
  FLogTreeView.OnExpanded        := FLogTreeViewExpanded;
  FLogTreeView.OnExpanding       := FLogTreeViewExpanding;

  B := Supports(Subscriber, IWinIpc) or Supports(Subscriber, IZmq);

  C := FLogTreeView.Header.Columns.Add; // logging level
  if not B then
    C.Options  := C.Options - [coVisible];
  C.Text     := '';
  C.Margin   := 0;
  C.Spacing  := 0;
  C.Width    := 10;
  C.MinWidth := 10;
  C.MaxWidth := 10;

  C := FLogTreeView.Header.Columns.Add; // message type
  C.Text     := '';
  C.Options  := C.Options - [coSmartResize, coAutoSpring];
  if B then
  begin
    C.Width    := 100;
    C.MinWidth := 100;
    C.MaxWidth := 2000;
  end
  else
  begin
    C.Width    := 20;
    C.MinWidth := 20;
    C.MaxWidth := 40;
  end;

  C := FLogTreeView.Header.Columns.Add; // name
  C.Text     := SName;
  if not B then
    C.Options  := C.Options - [coVisible, coSmartResize, coAutoSpring]
  else
    C.Options  := C.Options + [coSmartResize, coAutoSpring];

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

  if not B then
    C.Options  := C.Options - [coVisible, coSmartResize, coAutoSpring]
  else
    C.Options := C.Options + [coSmartResize, coAutoSpring];
  C.Width     := 70;
  C.MinWidth  := 75;
  C.MaxWidth  := 2000;

  C := FLogTreeView.Header.Columns.Add; // timestamp
  C.Text     := STimestamp;
  C.MinWidth := 10 * DisplayValuesSettings.TimeStamp.Font.Size;
  C.Width    := C.MinWidth;
  C.MaxWidth := C.MinWidth;
  C.Alignment := taRightJustify;
  FLogTreeView.Header.AutoSizeIndex := COLUMN_MAIN;
  FLogTreeView.Header.Options       :=
    FLogTreeView.Header.Options + [hoFullRepaintOnResize];
end;

procedure TfrmMessageList.CreateMessageDataView;
begin
  FMessageDataView := TfrmMessageDataView.Create(Self, DisplayValuesSettings);
  AssignFormParent(FMessageDataView, pnlMessageData);
end;

procedure TfrmMessageList.CreateRawDataView;
begin
  FRawDataView := TfrmRawDataView.Create(Self);
  AssignFormParent(FRawDataView, tsRawData);
end;

procedure TfrmMessageList.CreateValueListView;
begin
  FValueList := TfrmValueListView.Create(Self, DisplayValuesSettings);
  AssignFormParent(FValueList, tsValueList);
end;

procedure TfrmMessageList.CreateWatchesView;
begin
  FWatches := TWatchList.Create;
  FWatchesView := TLogViewerFactories.CreateWatchesView(
    Self,
    pnlWatches,
    FWatches,
    Manager.Settings.WatchSettings,
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

function TfrmMessageList.GetLeftPanelVisible: Boolean;
begin
  Result := pnlLeft.Visible;
end;

procedure TfrmMessageList.SetLeftPanelVisible(const Value: Boolean);
begin
  if Value <> LeftPanelVisible then
  begin
    pnlMain.PanelCollection[0].Visible := Value;
  end;
end;

function TfrmMessageList.GetManager: ILogViewerManager;
begin
  Result := FManager;
end;

function TfrmMessageList.GetMessageCount: Int64;
begin
  if Assigned(FSubscriber) then
    Result := FSubscriber.MessageCount
  else
    Result := 0;
end;

function TfrmMessageList.GetSubscriber: ISubscriber;
begin
  Result := FSubscriber;
end;

function TfrmMessageList.GetSelectedLogNode: TLogNode;
begin
  if Assigned(FLogTreeView) and Assigned(FLogTreeView.FocusedNode)
    and FLogTreeView.Selected[FLogTreeView.FocusedNode]
  then
  begin
    Result := FLogTreeView.GetNodeData<TLogNode>(FLogTreeView.FocusedNode);
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TfrmMessageList.SetSelectedLogNode(const Value: TLogNode);
begin
  if Assigned(Value) and Assigned(Value.VTNode) then
  begin
    if Value <> SelectedLogNode then
    begin
      FLogTreeView.FocusedNode := Value.VTNode;
      FLogTreeView.Selected[Value.VTNode] := True;
      FLogTreeView.ScrollIntoView(Value.VTNode, True, True);
    end;
//    FLogTreeView.Expanded[Value.VTNode] :=
//        not FLogTreeView.Expanded[Value.VTNode];
  end;
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
    edtMessageFilter.Color      := clYellow;
  end
  else
  begin
    edtMessageFilter.Font.Style := [];
    edtMessageFilter.Color      := clWhite;
  end;

  if Settings.AutoFilterMessages then
  begin
    FUpdate := True;
  end;
end;

procedure TfrmMessageList.edtMessageFilterExit(Sender: TObject);
begin
  ApplyFilter;
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
  { SHIFT and ALT keycombinations }
  A := (ssAlt in Shift) or (ssShift in Shift);
  { Single keys that need to be handled by the edit control like all displayable
    characters but also HOME and END }
  B := (Key in VK_EDIT_KEYS) and (Shift = []);
  { CTRL keycombinations that need to be handled by the edit control like
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
    edtMessageFilter.Color := clYellow;
end;

procedure TfrmMessageList.edtMessageFilterMouseLeave(Sender: TObject);
begin
  if not edtMessageFilter.Focused then
    edtMessageFilter.Color := clWhite;
end;
{$ENDREGION}

{$REGION 'FLogTreeView'}
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

procedure TfrmMessageList.FLogTreeViewExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
  Allowed := True;
end;

{ Draws custom focus rectangle. }

procedure TfrmMessageList.FLogTreeViewAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
begin
  if FLogTreeView.FocusedNode = Node then
  begin
    TargetCanvas.Pen.Color := clBlue;
    TargetCanvas.Pen.Width := 2;
    ItemRect.Inflate(-1, -1);
    TargetCanvas.MoveTo(ItemRect.Left, ItemRect.Top);
    TargetCanvas.LineTo(ItemRect.Right, ItemRect.Top);
    TargetCanvas.MoveTo(ItemRect.Left, ItemRect.Top);
    TargetCanvas.LineTo(ItemRect.Left, ItemRect.Bottom);
    TargetCanvas.MoveTo(ItemRect.Left, ItemRect.Bottom);
    TargetCanvas.LineTo(ItemRect.Right, ItemRect.Bottom);
    TargetCanvas.MoveTo(ItemRect.Right, ItemRect.Top);
    TargetCanvas.LineTo(ItemRect.Right, ItemRect.Bottom);
  end;
end;

{ This handler takes care of the following things:
  - Coloring the loglevel column background color.
  - Drawing collapsed tracing nodes as one node (using IsCollapsedTracingNode). }

procedure TfrmMessageList.FLogTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LN      : TLogNode;
  DVS     : TDisplayValuesSettings;
  LIndent : Integer;
  LRect   : TRect;
  LNode   : PVirtualNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  DVS := Manager.Settings.DisplayValuesSettings;
  if (LN.MessageType in TracingMessages)
    and ((Column = COLUMN_MAIN) or (Column = COLUMN_TIMESTAMP)) then
  begin
    DVS.Tracing.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
    if IsCollapsedTracingNode(Sender, Node) then
    begin
      Sender.IsVisible[Node.NextSibling] := False;
    end;
  end;
  if Column = COLUMN_LEVEL then
  begin
    Manager.Settings.LogLevelSettings.LogLevels[LN.LogLevel].Color;
    TargetCanvas.FillRect(CellRect);
  end
  else if Column = COLUMN_VALUE then
  begin
    DVS.Value.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
  end
  else if Column = COLUMN_VALUENAME then
  begin
    DVS.ValueName.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
  end
  else if Column = COLUMN_VALUETYPE then
  begin
    DVS.ValueType.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    DVS.TimeStamp.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
  end;

  if Column = Sender.Header.MainColumn then
  begin
    // draw indentation background
    LIndent := Sender.GetNodeLevel(Node) * FLogTreeView.Indent;
    LRect   := CellRect;
    Inc(LRect.Left, LIndent);
    LIndent := -Integer(FLogTreeView.Indent);
    LRect.Right := LRect.Left + Integer(FLogTreeView.Indent);
    Inc(LRect.Bottom);
    LNode := Node;
    repeat
      TargetCanvas.Brush.Color := DVS.Tracing.BackgroundColor;
      TargetCanvas.FillRect(LRect);
      LNode := LNode.Parent;
      if not Assigned(LNode) or (LNode = Sender.RootNode) then
        Break;
      Inc(LRect.Left, LIndent);
      Inc(LRect.Right, LIndent);
    until False;
    if LN.MessageType in NotificationMessages then
    begin
      LIndent := (Sender.GetNodeLevel(Node) + 1) * FLogTreeView.Indent;
      LRect := CellRect;
      Inc(LRect.Left, LIndent);
      case LN.MessageType of
        lmtInfo:
          DVS.Info.AssignTo(TargetCanvas);
        lmtError:
          DVS.Error.AssignTo(TargetCanvas);
        lmtWarning:
          DVS.Warning.AssignTo(TargetCanvas);
        lmtConditional:
          DVS.Conditional.AssignTo(TargetCanvas);
        lmtCheckpoint:
          DVS.CheckPoint.AssignTo(TargetCanvas);
      end;
      TargetCanvas.FillRect(LRect);
    end;
  end
end;

procedure TfrmMessageList.FLogTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FUpdate then
    UpdateActions
  else
    Modified;
end;

procedure TfrmMessageList.FLogTreeViewFilterCallback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  LN      : TLogNode;
  B       : Boolean;
  LFilter : string;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  LFilter := edtMessageFilter.Text;
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
    B := B and (LN.LogLevel in Settings.VisibleMessageLevels);
    if LFilter <> '' then
    begin
      B := B and (
       ContainsText(LN.Text, LFilter) or ContainsText(LN.ValueName, LFilter) or
       ContainsText(LN.Value, LFilter) or (LN.MessageType in TracingMessages)
      );
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
        and Assigned(ND.VTNode.NextSibling) then
      begin
        ImageIndex := 9;
      end;
    end;
    if ND.MessageType = lmtDataSet then
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
    else if ND.MessageType in [lmtColor, lmtAlphaColor] then
    begin
      ImageIndex := 22;
    end
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
  Guard.CheckNotNull(LN, 'LN');
  if Column = COLUMN_MAIN then
  begin
    if LN.MessageType in NotificationMessages + TracingMessages then
    begin
      CellText := LN.Text
    end
    else
    begin
      CellText := ' ';
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
    if S.Length > MAX_TEXTLENGTH_VALUECOLUMN then
      S := S.Substring(0, MAX_TEXTLENGTH_VALUECOLUMN);
    CellText := S;
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    if Settings.SmartTimeStamps then
    begin
      CellText := '';
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
              CellText := FormatDateTime('zzz',  LN.TimeStamp);
            end;
          end;
        end;
      end;
      if CellText.IsEmpty then
      begin
        CellText := FormatDateTime('hh:nn:ss:zzz',  LN.TimeStamp);
      end;
    end
    else
      CellText := FormatDateTime('hh:nn:ss:zzz',  LN.TimeStamp);
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
  LN     : TLogNode;
  I      : Integer;
  S      : string;
  SL     : TStringList;
  LText  : string;
  LName  : string;
  LValue : string;
  LType  : string;
begin
  LN := TLogNode.Create;
  Node.SetData(LN);
  if Assigned(FCurrentMsg.Data) then
  begin
    FCurrentMsg.Data.Position := 0;
    LN.MessageData.CopyFrom(FCurrentMsg.Data, FCurrentMsg.Data.Size);
    FreeAndNil(FCurrentMsg.Data);
  end;
  LN.TimeStamp   := FCurrentMsg.TimeStamp;
  LN.MessageType := TLogMessageType(FCurrentMsg.MsgType);
  LN.VTNode      := Node;
  LN.LogLevel    := FCurrentMsg.LogLevel;
  LN.Id          := MessageCount;
  LN.TextSize    := Length(FCurrentMsg.Text); // correct as it is a UTF8String
  LText          := string(FCurrentMsg.Text);
  case LN.MessageType of
    lmtValue, lmtComponent, lmtStrings, lmtPersistent, lmtObject, lmtInterface,
    lmtAlphaColor, lmtColor:
    begin
      ParseValue(LText).Unpack(LName, LValue, LType);
    end;
    lmtDataSet:
    begin
      LName := LText;
      LType := 'TDataSet';
    end;
    lmtAction:
    begin
      LName := LText;
      LType := 'TAction';
    end;
    lmtBitmap, lmtScreenShot:
    begin
      LName := LText;
      LType := 'TBitmap';
    end;
    lmtEnterMethod, lmtLeaveMethod,
    lmtInfo, lmtWarning, lmtError, lmtConditional:
    begin
      LN.Text := LText;
    end;
    lmtText:
    begin
      SL := TStringList.Create;
      try
        SL.Text := Trim(LText);
        if SL.Count > 0 then
        begin
          S := SL[0];
          I := S.IndexOf('(');
          if I > 1 then
          begin
            LName := Copy(S, 1, I);
            LN.Highlighter := Trim(ExtractText(S, '(', ')'));
          end;
          if I <> -1 then
            SL.Delete(0);
        end;
        LValue := SL.Text;
      finally
        SL.Free;
      end;
    end;
    lmtCheckpoint:
    begin
      LN.Text := LText;
    end
  end; // case LN.MessageType of
  LN.ValueName := LName;
  LN.Value     := LValue;
  LN.ValueType := LType;
end;

procedure TfrmMessageList.FLogTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LN : TLogNode;
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

{ Triggered before either normal or fixed text is painted to allow finer
  customization (kind of sub cell painting). Here it is used to apply custom
  font settings. }

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
        DVS.Info.AssignTo(TargetCanvas);
      lmtWarning:
        DVS.Warning.AssignTo(TargetCanvas);
      lmtError:
        DVS.Error.AssignTo(TargetCanvas);
      lmtConditional:
        DVS.Conditional.AssignTo(TargetCanvas);
      lmtEnterMethod:
      begin
        if IsCollapsedTracingNode(Sender, Node) then
          DVS.Tracing.AssignTo(TargetCanvas)
        else
          DVS.Enter.AssignTo(TargetCanvas);
      end;
      lmtLeaveMethod:
        DVS.Leave.AssignTo(TargetCanvas);
      lmtCheckpoint:
        DVS.CheckPoint.AssignTo(TargetCanvas);
      lmtAction:
        DVS.Action.AssignTo(TargetCanvas);
    end;
  end
  else if Column = COLUMN_VALUENAME then
  begin
    DVS.ValueName.AssignTo(TargetCanvas);
  end
  else if Column = COLUMN_VALUETYPE then
  begin
    DVS.ValueType.AssignTo(TargetCanvas);
  end
  else if Column = COLUMN_VALUE then
  begin
    DVS.Value.AssignTo(TargetCanvas);
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    DVS.TimeStamp.AssignTo(TargetCanvas);
  end;
end;
{$ENDREGION}

procedure TfrmMessageList.pnlMainCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := pnlMain.CanFocus;
end;

procedure TfrmMessageList.pnlMainSplitterMoved(Sender: TObject);
begin
  Modified;
end;

procedure TfrmMessageList.pnlMessagesResize(Sender: TObject);
begin
  if IsActiveView then
  begin
    Modified;
    FAutoSizeColumns := False;
  end;
end;

procedure TfrmMessageList.chkShowDetailsClick(Sender: TObject);
begin
  Settings.MessageDetailsVisible := (Sender as TCheckBox).Checked;
end;

procedure TfrmMessageList.chkShowWatchHistoryClick(Sender: TObject);
begin
  Manager.Settings.WatchSettings.WatchHistoryVisible :=
    (Sender as TCheckBox).Checked;
end;

procedure TfrmMessageList.chkSyncWithSelectedMessageClick(Sender: TObject);
begin
  Manager.Settings.WatchSettings.SyncWithSelection :=
    (Sender as TCheckBox).Checked;
  if Assigned(SelectedLogNode) then
  begin
    if not chkSyncWithSelectedMessage.Checked then
      FWatchesView.UpdateView(MessageCount)
    else
      FWatchesView.UpdateView(SelectedLogNode.Id);
  end;
end;

procedure TfrmMessageList.tsRawDataShow(Sender: TObject);
begin
  if Assigned(SelectedLogNode) then
    UpdateRawDataDisplay(SelectedLogNode);
end;

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

procedure TfrmMessageList.WatchSettingsChanged(Sender: TObject);
begin
  chkShowWatchHistory.Checked :=
    Manager.Settings.WatchSettings.WatchHistoryVisible;
  chkSyncWithSelectedMessage.Checked :=
    Manager.Settings.WatchSettings.SyncWithSelection;
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Makes sure the active view is the current view when it is focused. }

procedure TfrmMessageList.EnsureCurrentViewIsActiveViewWhenFocused;
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

function TfrmMessageList.GetRightPanelVisible: Boolean;
begin
  Result := pnlRight.Visible;
end;

procedure TfrmMessageList.SetRightPanelVisible(const Value: Boolean);
begin
  if Value <> RightPanelVisible then
  begin
    pnlMain.PanelCollection[2].Visible := Value;
    //pnlRight.Visible := Value;
  end;
end;

function TfrmMessageList.IsCollapsedTracingNode(ATree: TBaseVirtualTree;
  ANode: PVirtualNode): Boolean;
var
  LN : TLogNode;
begin
  if not (vsExpanded in ANode.States) and
    Assigned(ANode.NextSibling) then
  begin
    LN := ATree.GetNodeData<TLogNode>(ANode);
    Result := LN.MessageType = lmtEnterMethod;
  end
  else
    Result := False;
end;

{ Called when UpdateLogTreeView needs to be called. }

procedure TfrmMessageList.Modified;
begin
  FUpdate := True;
end;

{ Parses the given string into name, value and type. }

function TfrmMessageList.ParseValue(
  const AString: string): Tuple<string, string, string>;
var
  I      : Integer;
  LName  : string;
  LValue : string;
  LType  : string;
begin
  I := AString.IndexOf('=');
  LName := Copy(AString, 1, I);
  LValue := Copy(AString, I + 3, AString.Length); // ' = '
  if LValue.StartsWith(#13#10) then // multiline values
    LValue := Copy(LValue, 3, LValue.Length);
  LType := ExtractText(LName, '(', ')');
  I := AString.IndexOf('(');
  if I > 1 then
    LName := Copy(AString, 1, I);
  Result.Create(LName, LValue, LType);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessageList.LoadPanelSettings;
var
  I : Integer;
begin
  for I := 0 to pnlMain.PanelCollection.Count - 2 do
  begin
    pnlMain.PanelCollection[I].Position := Settings.HorizontalPanelPositions[I];
  end;
  for I := 0 to pnlLeft.PanelCollection.Count - 2 do
  begin
    pnlLeft.PanelCollection[I].Position := Settings.LeftVerticalPanelPositions[I];
  end;
end;

procedure TfrmMessageList.SavePanelSettings;
var
  I : Integer;
begin
  for I := 0 to pnlMain.PanelCollection.Count - 2 do
  begin
    FSettings.HorizontalPanelPositions[I] := pnlMain.PanelCollection[I].Position;
  end;
  for I := 0 to pnlLeft.PanelCollection.Count - 2 do
  begin
    FSettings.LeftVerticalPanelPositions[I] := pnlLeft.PanelCollection[I].Position;
  end;
end;

procedure TfrmMessageList.ApplyFilter;
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

procedure TfrmMessageList.ApplySettings;
//var
//  LFilter : ILogMessageSubscriptionFilter;
begin
  Logger.Track(Self, 'ApplySettings');
  if Settings.ColumnHeadersVisible then
    FLogTreeView.Header.Options := FLogTreeView.Header.Options + [hoVisible]
  else
    FLogTreeView.Header.Options := FLogTreeView.Header.Options - [hoVisible];
  splRightHorizontal.Visible := Settings.MessageDetailsVisible;
  pnlMessageData.Visible     := Settings.MessageDetailsVisible;

  // TODO
//  if Supports(Subscriber, ILogMessageSubscriptionFilter, LFilter) then
//  begin
//    LFilter.LogMessageTypes := Settings.VisibleMessageTypes;
//    LFilter.LogMessageLevels := Settings.VisibleMessageLevels;
//    Logger.Send('LogMessageTypes', TValue.From(LFilter.LogMessageTypes));
//    Logger.Send('LogMessageLevels', TValue.From(LFilter.LogMessageLevels));
//  end;
  LoadPanelSettings;
  ApplyFilter;
  Modified;
end;

procedure TfrmMessageList.AutoFitColumns;
begin
  FLogTreeView.Header.AutoFitColumns(False, smaUseColumnOption, 2);
  FAutoSizeColumns := True;
end;

{ Makes the current view the active view in the Manager. }

procedure TfrmMessageList.Activate;
begin
  Logger.Track(Self, 'Activate');
  Manager.ActiveView := Self as ILogViewer;
  Manager.EditorManager.ActiveView := FEditorView;
  inherited Activate;
end;

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
  EditorView.Clear;
  if Assigned(FValueList) then
    FValueList.Clear;
  FRawDataView.Clear;
  FDataSetView.Clear;
  FImageView.Clear;
end;

{ Reads the received message stream from the active logchannel. }

{$REGION 'documentation'}
{
   Message layout in the received binary stream
     1. Message type (1 byte)          => TLogMessage.MsgType (TLogMessageType)
     2. Log level    (1 byte)          => TLogMessage.LogLevel (0-255)
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
  LName     : string;
  LType     : string;
  LValue    : string;
  LText     : string;
begin
  Guard.CheckNotNull(AStream, 'AStream');
  LTextSize := 0;
  LDataSize := 0;
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
    LText := string(FCurrentMsg.Text);
  end;
  AStream.ReadBuffer(LDataSize);
  if LDataSize > 0 then
  begin
    if not Assigned(FCurrentMsg.Data) then
      FCurrentMsg.Data          := TMemoryStream.Create;
    FCurrentMsg.Data.Size     := 0;
    FCurrentMsg.Data.Position := 0;
    FCurrentMsg.Data.CopyFrom(AStream, LDataSize);
  end
  else
  begin
    FreeAndNil(FCurrentMsg.Data);
  end;
  case TLogMessageType(FCurrentMsg.MsgType) of
    lmtCounter:
    begin
      ParseValue(LText).Unpack(LName, LValue, LType);
      FWatches.Add(
        LName, LType, LValue, lmtCounter, MessageCount, FCurrentMsg.TimeStamp,
        True, True
      );
      if not chkSyncWithSelectedMessage.Checked then
        FWatchesView.UpdateView(MessageCount)
      else
        FWatchesView.UpdateView;
    end;
    lmtWatch:
    begin
      ParseValue(LText).Unpack(LName, LValue, LType);
      FWatches.Add(
        LName, LType, LValue, lmtWatch, MessageCount, FCurrentMsg.TimeStamp,
        True, False
      );
      if not chkSyncWithSelectedMessage.Checked then
        FWatchesView.UpdateView(MessageCount)
      else
        FWatchesView.UpdateView;
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
        FScrollToLastNode := True;
      end;
      Modified;
    end;
  end; // case
end;

{$REGION 'Commands'}
{ Clear all received messages. }

procedure TfrmMessageList.Clear;
begin
  ClearMessageDetailsControls;
  FWatchesView.Clear;
  EditorView.Clear;
  FCallStack.Clear;
  FLogTreeView.Clear;
  FMessageDataView.Clear;
  FSubscriber.Reset;
  FLastNode   := nil;
  FLastParent := nil;
  FUpdate     := True;
end;

procedure TfrmMessageList.ClearSelection;
begin
  FLogTreeView.ClearSelection;
end;

procedure TfrmMessageList.CollapseAll;
begin
  FLogTreeView.FullCollapse;
  Modified;
  FAutoSizeColumns := False;
end;

procedure TfrmMessageList.ExpandAll;
begin
  FLogTreeView.FullExpand;
  Modified;
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
    Modified;
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
    Modified;
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
  FImageView.LoadFromStream(ALogNode.MessageData);
end;

{ Calculates and prepares the callstack display for the selected node. }

procedure TfrmMessageList.UpdateCallStackDisplay(ALogNode: TLogNode);
var
  I   : Integer;
  LN  : TLogNode;
  LN2 : TLogNode;
  VN  : PVirtualNode;
begin
  Guard.CheckNotNull(ALogNode, 'ALogNode');
  FCallStack.Clear;
  LN2 := nil;
  VN  := ALogNode.VTNode;
  I   := 1;
  while (I > 0) and Assigned(VN) do
  begin
    LN := FLogTreeView.GetNodeData<TLogNode>(VN);
    I  := FLogTreeView.GetNodeLevel(VN);
    if LN.MessageType = lmtEnterMethod then
    begin
      if Assigned(LN.VTNode.NextSibling) then
      begin
        LN2 := FLogTreeView.GetNodeData<TLogNode>(LN.VTNode.NextSibling);
        if Assigned(VN.Parent) then
        begin
          VN := VN.Parent;
        end
        else
          VN := nil;
      end
      else
        VN := nil;
    end
    else if LN.MessageType = lmtLeaveMethod then
    begin
      if Assigned(VN.PrevSibling) then
      begin
        LN2 := LN;
        LN := FLogTreeView.GetNodeData<TLogNode>(VN.PrevSibling);
        if Assigned(VN.Parent) then
          VN := VN.Parent
        else
          VN := nil;
      end
      else
        VN := nil;
    end
    else if I > 0 then
    begin
      if Assigned(VN.Parent) then
      begin
        VN := VN.Parent;
      end
      else
      begin
        VN := nil;
      end;
    end
    else
    begin
      VN := nil;
    end;
    if Assigned(LN2) then
      FCallStack.Add(
        TCallStackData.Create(
          LN, LN2, LN.Text, I, MilliSecondsBetween(LN2.TimeStamp, LN.TimeStamp)
        )
      );
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
  FDataSetView.LoadFromStream(ALogNode.MessageData);
end;

{ Updates all information related to the selected message node. }

procedure TfrmMessageList.UpdateMessageDetails(ALogNode: TLogNode);
begin
  ClearMessageDetailsControls;
  FMessageDataView.LogNode := ALogNode;
  case ALogNode.MessageType of
    lmtCallStack, {lmtException,} lmtHeapInfo, lmtCustomData:
      UpdateTextStreamDisplay(ALogNode);
    lmtAlphaColor, lmtColor:
      UpdateColorDisplay(ALogNode);
    lmtComponent:
    begin
      UpdateComponentDisplay(ALogNode);
      pgcMessageDetails.ActivePage := tsTextViewer;
    end;
    lmtBitmap, lmtScreenShot:
    begin
      UpdateBitmapDisplay(ALogNode);
      pgcMessageDetails.ActivePage := tsImageViewer;
    end;
    lmtMemory:
    begin
      UpdateRawDataDisplay(ALogNode);
      pgcMessageDetails.ActivePage := tsRawData;
    end;
    lmtEnterMethod, lmtLeaveMethod, lmtText,
    lmtInfo, lmtWarning, lmtError, lmtConditional:
    begin
      //UpdateTextStreamDisplay(ALogNode);
      UpdateTextDisplay(ALogNode);
      pgcMessageDetails.ActivePage := tsTextViewer;
    end;
    lmtValue, lmtObject, lmtPersistent, lmtInterface, lmtStrings, lmtCheckpoint:
    begin
      UpdateValueDisplay(ALogNode);
      pgcMessageDetails.ActivePage := tsValueList;
    end;
    lmtDataSet:
    begin
      UpdateDataSetDisplay(ALogNode);
      pgcMessageDetails.ActivePage := tsDataSet;
    end;
  end;
  if chkSyncWithSelectedMessage.Checked then
    FWatchesView.UpdateView(ALogNode.Id);
end;

procedure TfrmMessageList.UpdateRawDataDisplay(ALogNode: TLogNode);
begin
  if Assigned(ALogNode.MessageData) then
    FRawDataView.LoadFromStream(ALogNode.MessageData);
end;

procedure TfrmMessageList.UpdateTextDisplay(ALogNode: TLogNode);
var
  S : string;
begin
  if ALogNode.Value.IsEmpty then
    EditorView.Text := ALogNode.Text
  else
    EditorView.Text := ALogNode.Value;
  S := ALogNode.Highlighter;
  if S <> '' then
   EditorView.HighlighterName := S;
end;

{ Updates text display with text stream data stored in MessageData. }

procedure TfrmMessageList.UpdateTextStreamDisplay(ALogNode: TLogNode);
var
  LStream : TStringStream;
begin
  if not Assigned(ALogNode.MessageData) then
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

procedure TfrmMessageList.UpdateValueDisplay(ALogNode: TLogNode);
var
  DR : DynamicRecord;
  SL : TStringList;
begin
  EditorView.Text := ALogNode.Value;
  EditorView.HighlighterName  := 'INI';
  if ALogNode.Value.Contains('=') then
  begin
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
    FValueList.Data := DR;
  end
  else
  begin
    pgcMessageDetails.ActivePage := tsTextViewer;
  end;
end;
{$ENDREGION}

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
  LColumn.MinWidth := LTotalWidth div 6;
  LColumn := FLogTreeView.Header.Columns[COLUMN_VALUENAME];
  LColumn.MaxWidth := LTotalWidth div 2;
  LColumn.MinWidth := LTotalWidth div 10;
  LColumn := FLogTreeView.Header.Columns[COLUMN_VALUE];
  LColumn.MaxWidth := LTotalWidth div 2;
  LColumn.MinWidth := LTotalWidth div 6;
  LColumn := FLogTreeView.Header.Columns[COLUMN_VALUETYPE];
  LColumn.MaxWidth := LTotalWidth div 6;
  LColumn.MinWidth := LTotalWidth div 10;
  LColumn := FLogTreeView.Header.Columns[COLUMN_TIMESTAMP];
  LColumn.MinWidth := 10 * DisplayValuesSettings.TimeStamp.Font.Size;
  LColumn.Width    := LColumn.MinWidth;
  LColumn.MaxWidth := LColumn.MinWidth;

  FAutoSizeColumns := True;
end;

procedure TfrmMessageList.UpdateLogTreeView;
var
  LN : TLogNode;
begin
  if not FUpdating then
  begin
    FUpdating := True;
    if edtMessageFilter.Focused then
    begin
      ApplyFilter;
    end
    else if Assigned(SelectedLogNode) then
    begin
      LN := SelectedLogNode;
      UpdateMessageDetails(LN);
      UpdateCallStackDisplay(LN);
      ApplyFilter;
    end;
    if FScrollToLastNode then
    begin
      FLogTreeView.FocusedNode := FLogTreeView.GetLast;
      FLogTreeView.Selected[FLogTreeView.FocusedNode] := True;
      FScrollToLastNode := False;
    end;
    FUpdating := False;
  end;
end;

procedure TfrmMessageList.UpdateActions;
begin
  if FUpdate and not IsActiveView then
    EnsureCurrentViewIsActiveViewWhenFocused;
  if IsActiveView and FUpdate then
  begin
    Logger.IncCounter(Name);
    UpdateLogTreeView;
    if Assigned(Actions) then
    begin
      Actions.UpdateActions;
    end;
    SavePanelSettings;
    FUpdate := False;
  end;
  inherited UpdateActions;
end;

{ Force an update of the message viewer. }

procedure TfrmMessageList.UpdateView;
begin
  Modified;
  FAutoSizeColumns := False;
end;
{$ENDREGION}

end.
