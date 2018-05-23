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
  https://stackoverflow.com/questions/5365365/tree-like-datastructure-for-use-with-virtualtreeview
}

{
  TODO:
    - auto show message details, watches window and callstack (WinODS)
}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ImgList,

  VirtualTrees,

  Spring.Collections,

  DDuce.Editor.Interfaces, DDuce.Logger.Interfaces,

  LogViewer.Messages.Data, LogViewer.Watches.Data, LogViewer.Watches.View,
  LogViewer.Interfaces, LogViewer.CallStack.Data, LogViewer.CallStack.View,
  LogViewer.MessageList.Settings, LogViewer.MessageList.LogNode;

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
    imgBitmap         : TImage;
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
    pnlMessages       : TPanel;
    pnlRawMessageData : TPanel;
    pnlRight          : TPanel;
    pnlTextViewer     : TPanel;
    pnlWatches        : TPanel;
    splLeftHorizontal : TSplitter;
    splLeftVertical   : TSplitter;
    splVertical       : TSplitter;
    tsImageViewer     : TTabSheet;
    tsTextViewer      : TTabSheet;
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

  private class var
    FCounter : Integer;

  private
    FMessageCount   : Int64;
    FCurrentMsg     : TLogMessage;
    FCallStack      : IList<TCallStackData>;
    FWatches        : TWatchList;
    FLogTreeView    : TVirtualStringTree;
    FLogQueue       : ILogQueue;
    FCallStackView  : TfrmCallStackView;
    FWatchesView    : TfrmWatchesView;
    FManager        : ILogViewerManager;
    FEditorManager  : IEditorManager;
    FEditorSettings : IEditorSettings;
    FEditorView     : IEditorView;
    FExpandParents  : Boolean;
    FLastParent     : PVirtualNode;
    FLastNode       : PVirtualNode;
    FVKPressed      : Boolean;
    FSettings       : TMessageListSettings;

    {$REGION 'property access methods'}
    function GetManager: ILogViewerManager;
    function GetActions: ILogViewerActions;
    function GetLogQueue: ILogQueue;
    function GetForm: TCustomForm;
    function GetSettings: TMessageListSettings;
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
    {$ENDREGION}

  protected
    procedure FLogQueueReceiveMessage(
      Sender  : TObject;
      AStream : TStream
    );

    procedure Clear;

    procedure ProcessMessage(AStream: TStream);

    procedure AddMessageToTree(const AMessage: TLogMessage);

    procedure UpdateCallStack(ALogNode: TLogNode);
    procedure UpdateMessageDetails(ALogNode: TLogNode);
    procedure UpdateComponentDisplay(ALogNode: TLogNode);
    procedure UpdateBitmapDisplay(ALogNode: TLogNode);
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

    property LogQueue: ILogQueue
      read GetLogQueue;

    property Settings: TMessageListSettings
      read GetSettings;

    property Form: TCustomForm
      read GetForm;

  public
    constructor Create(
      AOwner    : TComponent;
      AManager  : ILogViewerManager;
      ALogQueue : ILogQueue;
      ASettings : TMessageListSettings
    ); reintroduce; virtual;

    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.StrUtils, System.UITypes, System.DateUtils, System.Math,
  System.UIConsts,

  Spring, Spring.Helpers,

  DDuce.Factories.VirtualTrees, DDuce.Editor.Factories,
  DDuce.Reflect, DDuce.Utils,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  LogViewer.Factories, LogViewer.Resources;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmMessageList.Create(AOwner: TComponent; AManager
  : ILogViewerManager; ALogQueue : ILogQueue; ASettings: TMessageListSettings);
begin
  inherited Create(AOwner);
  FLogQueue := ALogQueue;
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
  FLogQueue.OnReceiveMessage.Add(FLogQueueReceiveMessage);
  Caption := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  FSettings.OnChanged.Add(FSettingsChanged);
  FLogTreeView.PopupMenu  := Manager.Menus.LogTreeViewerPopupMenu;
  edtTimeStamp.Font.Color := TIMESTAMP_FONTCOLOR;
  edtValueName.Font.Color := VALUENAME_FONTCOLOR;
  edtValueType.Font.Color := VALUETYPE_FONTCOLOR;
  edtValue.Font.Color     := VALUE_FONTCOLOR;
end;

procedure TfrmMessageList.BeforeDestruction;
begin
  FLogQueue.OnReceiveMessage.Remove(FLogQueueReceiveMessage);
  FSettings.OnChanged.Remove(FSettingsChanged);
  FLogQueue := nil;
  FManager  := nil;
  FSettings := nil;
  inherited BeforeDestruction;
end;

procedure TfrmMessageList.chkAutoFilterClick(Sender: TObject);
begin
  Settings.AutoFilterMessages := (Sender as TCheckBox).Checked;
end;

procedure TfrmMessageList.CreateCallStackView;
begin
  FCallStack     := TCollections.CreateObjectList<TCallStackData>;
  FCallStackView := TLogViewerFactories.CreateCallStackView(
    Self,
    pnlCallStack,
    FCallStack as IObjectList
  );
end;

procedure TfrmMessageList.CreateEditor;
begin
  FEditorSettings := TEditorFactories.CreateSettings(Self, 'settings.xml');
  FEditorManager  := TEditorFactories.CreateManager(Self, FEditorSettings);
  FEditorView     := TEditorFactories.CreateView(pnlTextViewer, FEditorManager);
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

  FLogTreeView.NodeDataSize := SizeOf(TLogNode);
  FLogTreeView.Images       := imlMessageTypes;

  FLogTreeView.OnBeforeItemPaint := FLogTreeViewBeforeItemPaint;
  FLogTreeView.OnAfterItemPaint  := FLogTreeViewAfterItemPaint;

  FLogTreeView.OnBeforeCellPaint := FLogTreeViewBeforeCellPaint;

  FLogTreeView.OnFocusChanged    := FLogTreeViewFocusChanged;
  FLogTreeView.OnFocusChanging   := FLogTreeViewFocusChanging;
  FLogTreeView.OnClick           := FLogTreeViewClick;

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
  C.MaxWidth := 200;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := SName;
  C.Options  := C.Options + [coSmartResize];
  C.Width    := 100;
  C.MinWidth := 100;
  C.MaxWidth := 200;

  C := FLogTreeView.Header.Columns.Add;
  C.Text      := SType;
  C.Options  := C.Options + [coSmartResize];
  C.Width     := 50;
  C.MinWidth  := 50;
  C.MaxWidth  := 200;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := SValue;
  C.Options  := C.Options + [coAutoSpring];
  C.Width    := 100;
  C.MinWidth := 50;
  C.MaxWidth := 1024;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := STimestamp;
  C.Width    := 80;
  C.MinWidth := 80;
  C.MaxWidth := 80;
end;

procedure TfrmMessageList.CreateWatchesView;
begin
  FWatches := TWatchList.Create;
  FWatchesView := TLogViewerFactories.CreateWatchesView(
    Self,
    pnlLeftBottom,
    FWatches
  );
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMessageList.GetActions: ILogViewerActions;
begin
  Result := Manager as ILogViewerActions;
end;

function TfrmMessageList.GetForm: TCustomForm;
begin
  Result := Self;
end;

function TfrmMessageList.GetManager: ILogViewerManager;
begin
  Result := FManager;
end;

function TfrmMessageList.GetLogQueue: ILogQueue;
begin
  Result := FLogQueue;
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
begin
//
end;

procedure TfrmMessageList.FLogQueueReceiveMessage(Sender: TObject;
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
  FLogTreeView.Header.AutoFitColumns(False, smaUseColumnOption, -1, -1);
  //FLogTreeView.Header.AutoFitColumns(False, smaAllColumns, -1, -1);
end;

procedure TfrmMessageList.FLogTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LN : TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  if LN.MessageType in [lmtEnterMethod, lmtLeaveMethod] then
  begin
    TargetCanvas.Brush.Color := $00EEEEEE;
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
    else
      ImageIndex := 0;
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
        CellText := 'Value';
      lmtAlphaColor, lmtColor:
        CellText := 'Color';
      lmtBitmap:
        CellText := 'Bitmap';
      lmtComponent:
        CellText := 'Component';
      lmtStrings:
        CellText := 'Strings';
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

procedure TfrmMessageList.FLogTreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LN : TLogNode; // class type
  I  : Integer;
  S  : string;
  A  : TArray<string>;
begin
  LN := TLogNode.Create;
  Node.SetData(LN);
  LN.MessageData := FCurrentMsg.Data;
  LN.TimeStamp   := FCurrentMsg.TimeStamp;
  LN.MessageType := TLogMessageType(FCurrentMsg.MsgType);
  LN.VTNode      := Node;
  LN.Id          := FMessageCount;
  case LN.MessageType of
    lmtValue, lmtComponent, lmtStrings, lmtBitmap:
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
//    lmtComponent:
//    begin
//      LN.Text := '';
//    end;
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
    lmtInfo, lmtWarning, lmtError, lmtConditional,
    lmtText:
    begin
      LN.Text := string(FCurrentMsg.Text);
    end;
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
  LN : TLogNode;
begin
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'ND');
  if Column = COLUMN_MAIN then
  begin
    case LN.MessageType of
      lmtInfo:
      begin
        TargetCanvas.Font.Color := clBlue;
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      end;
      lmtWarning:
      begin
        TargetCanvas.Font.Color := clRed;
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      end;
      lmtError:
      begin
        TargetCanvas.Font.Color := clRed;
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      end;
      lmtValue, lmtComponent, lmtAlphaColor, lmtColor, lmtBitmap, lmtStrings:
      begin
        TargetCanvas.Font.Color := clDkGray;
      end;
    end;
  end
  else if Column = COLUMN_VALUENAME then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end
  else if Column = COLUMN_VALUETYPE then
  begin
    TargetCanvas.Font.Color := VALUETYPE_FONTCOLOR;
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end
  else if Column = COLUMN_VALUE then
  begin
    TargetCanvas.Font.Color := VALUE_FONTCOLOR;
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    TargetCanvas.Font.Color := TIMESTAMP_FONTCOLOR;
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

procedure TfrmMessageList.FSettingsChanged(Sender: TObject);
begin
  chkAutoFilter.Checked := Settings.AutoFilterMessages;
  UpdateView;
end;
{$ENDREGION}

{$REGION 'protected methods'}
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
  FMessageCount       := 0;
  FLastNode           := nil;
  FLastParent         := nil;
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
  edtValueType.Text   := '';
  pnlColor.Color      := clBtnFace;
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
  SetLength(FCurrentMsg.Text, LTextSize);
  AStream.ReadBuffer(FCurrentMsg.Text[1], LTextSize);
  AStream.ReadBuffer(LDataSize);
  if LDataSize > 0 then
  begin
    FCurrentMsg.Data := TMemoryStream.Create;
    FCurrentMsg.Data.Size := 0;
    FCurrentMsg.Data.Position := 0;
    FCurrentMsg.Data.CopyFrom(AStream, LDataSize);
  end
  else
    FCurrentMsg.Data := nil;

  case TLogMessageType(FCurrentMsg.MsgType) of
    lmtWatch, lmtCounter:
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
        FCurrentMsg.MsgType = Integer(lmtCounter) // SkipOnNewValue
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
end;

procedure TfrmMessageList.UpdateActions;
var
  B: Boolean;
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
  if Assigned(Actions) then
    Actions.UpdateActions;

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
    pnlColor.Color := AlphaColorToColor(StrToInt(S))
  else
    pnlColor.Color := StrToInt(S);
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

procedure TfrmMessageList.UpdateMessageDetails(ALogNode: TLogNode);
begin
  ClearMessageDetailsControls;
  FWatchesView.UpdateView(ALogNode.Id);
  case ALogNode.MessageType of
    lmtStrings, lmtCallStack, {lmtException,} lmtHeapInfo, lmtCustomData:
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
    lmtValue:
      UpdateValueDisplay(ALogNode);
  end;
  edtMessageType.Text := LogMessageTypeNameOf(ALogNode.MessageType);
  edtTimeStamp.Text   :=
    FormatDateTime('dd:mm:yyyy hh:nn:ss:zzz', ALogNode.TimeStamp);
  edtValue.Text       := ALogNode.Value;
  edtValueName.Text   := ALogNode.ValueName;
  edtValueType.Text   := ALogNode.ValueType;
end;

procedure TfrmMessageList.UpdateTextDisplay(ALogNode: TLogNode);
begin
  pgcMessageDetails.ActivePage := tsTextViewer;
  FEditorView.Text := ALogNode.Text;
  FEditorView.HighlighterName := 'TXT';
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
begin
  pgcMessageDetails.ActivePage := tsTextViewer;
  FEditorView.Text := ALogNode.Value;
  FEditorView.HighlighterName := 'TXT';
end;

procedure TfrmMessageList.UpdateView;
begin
  UpdateLogTreeView;
end;
{$ENDREGION}
end.
