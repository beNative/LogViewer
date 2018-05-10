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
  LogViewer.MessageList.Settings;

type
  TfrmMessageList = class(TForm, ILogViewer)
    {$REGION 'designer controls'}
    btnFilterMessages : TButton;
    chkAutoFilter     : TCheckBox;
    edtMessageFilter  : TLabeledEdit;
    imlMessageTypes   : TImageList;
    pnlCallStack      : TPanel;
    pnlCallStackTitle : TPanel;
    pnlCallStackWatch : TPanel;
    pnlFilter         : TPanel;
    pnlLeft           : TPanel;
    pnlLeftBottom     : TPanel;
    pnlMessageContent : TPanel;
    pnlMessages       : TPanel;
    pnlRight          : TPanel;
    pnlWatches        : TPanel;
    splLeftHorizontal : TSplitter;
    splLeftVertical   : TSplitter;
    splVertical       : TSplitter;
    pgcMessageContent : TPageControl;
    tsTextViewer      : TTabSheet;
    tsImageViewer     : TTabSheet;
    pnlTextViewer     : TPanel;
    tsRawMessageData  : TTabSheet;
    edtMessageType    : TLabeledEdit;
    edtTimeStamp      : TLabeledEdit;
    mmoMessageText    : TMemo;
    pnlColor          : TPanel;
    edtValueName      : TLabeledEdit;
    edtValueType      : TLabeledEdit;
    edtValue          : TLabeledEdit;
    imgBitmap: TImage;
    edtWidth: TLabeledEdit;
    edtHeight: TLabeledEdit;
    edtPixelFormat: TLabeledEdit;
    edtHandleType: TLabeledEdit;
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

  private class var
    FCounter : Integer;

  private
    FMessageCount   : Int64;
    FCurrentMsg     : TLogMessage;
    FCallStack      : IList<TCallStackData>;
    FWatches        : TWatchList;
    FLogTreeView    : TVirtualStringTree;
    FReceiver       : IChannelReceiver;
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
    function GetReceiver: IChannelReceiver;
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
    procedure FReceiverReceiveMessage(
      Sender    : TObject;
      AReceiver : IChannelReceiver;
      AStream   : TStream
    );

    procedure Clear;

    procedure ProcessMessage(AStream: TStream);

    procedure AddMessageToTree(const AMessage: TLogMessage);

    procedure UpdateCallStack(var ANode: PVirtualNode);
    procedure UpdateLogTreeView;

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

    property Receiver: IChannelReceiver
      read GetReceiver;

    property Settings: TMessageListSettings
      read GetSettings;

    property Form: TCustomForm
      read GetForm;

  public
    constructor Create(
      AOwner    : TComponent;
      AManager  : ILogViewerManager;
      AReceiver : IChannelReceiver;
      ASettings : TMessageListSettings
    ); reintroduce; virtual;

    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.StrUtils, System.UITypes, System.DateUtils,

  Spring, Spring.Helpers,

  DDuce.Factories.VirtualTrees, DDuce.Editor.Factories,
  DDuce.Reflect, DDuce.Utils,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  LogViewer.Factories, LogViewer.Resources, LogViewer.MessageList.LogNode;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmMessageList.Create(AOwner: TComponent; AManager
  : ILogViewerManager; AReceiver: IChannelReceiver; ASettings: TMessageListSettings);
begin
  inherited Create(AOwner);
  FReceiver := AReceiver;
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
  FReceiver.OnReceiveMessage.Add(FReceiverReceiveMessage);
  Caption := Copy(ClassName, 2, Length(ClassName)) + IntToStr(FCounter);
  FSettings.OnChanged.Add(FSettingsChanged);
  FLogTreeView.PopupMenu := Manager.Menus.LogTreeViewerPopupMenu;
  edtTimeStamp.Font.Color := TIMESTAMP_FONTCOLOR;
  edtValueName.Font.Color := VALUENAME_FONTCOLOR;
  edtValueType.Font.Color := VALUETYPE_FONTCOLOR;
  edtValue.Font.Color     := VALUE_FONTCOLOR;
end;

procedure TfrmMessageList.BeforeDestruction;
begin
  FReceiver.Enabled := False;
  FReceiver.OnReceiveMessage.Remove(FReceiverReceiveMessage);
  FReceiver := nil;
  FManager := nil;
  FSettings.OnChanged.Remove(FSettingsChanged);
  FSettings := nil;
  inherited BeforeDestruction;
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
  C.Text     := STitle;
  C.Options  := C.Options + [coFixed, coAutoSpring];
  C.Width    := 100;
  C.MinWidth := 100;
  C.MaxWidth := 1024;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := SName;
  C.Width    := 100;
  C.MinWidth := 100;
  C.MaxWidth := 200;

  C := FLogTreeView.Header.Columns.Add;
  C.Text      := SType;
  C.Width     := 80;
  C.MinWidth  := 50;
  C.MaxWidth  := 200;

  C := FLogTreeView.Header.Columns.Add;
  C.Text     := SValue;
  C.Options  := C.Options + [coAutoSpring];
  C.Width    := 300;
  C.MinWidth := 80;
  C.MaxWidth := 800;

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

function TfrmMessageList.GetReceiver: IChannelReceiver;
begin
  Result := FReceiver;
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

procedure TfrmMessageList.FLogTreeViewClick(Sender: TObject);
begin
  FLogTreeView.Header.AutoFitColumns(False, smaUseColumnOption, -1, -1);
end;

procedure TfrmMessageList.FLogTreeViewAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
begin
//
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
begin
  //Todo: merge with Changed?
  //The CallStack is only updated if the parent changes
  Allowed := OldNode <> NewNode;
  if Allowed and ((OldNode = nil) or (NewNode = nil) or
    (OldNode^.Parent <> NewNode^.Parent)) then
    UpdateCallStack(NewNode);
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

procedure TfrmMessageList.FLogTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LStream : TStringStream;
  S       : string;
  LN      : TLogNode;
begin
  LStream := TStringStream.Create('', TEncoding.ANSI);
  LN := Sender.GetNodeData<TLogNode>(Node);
  Guard.CheckNotNull(LN, 'LogNode');
  FWatchesView.UpdateView(LN.Id);
  try
    if LN.MessageData = nil then
    begin
      FEditorView.Text := '';
    end
    else
      LN.MessageData.Position := 0;

    case LN.MessageType of
      {lmtStrings,} lmtCallStack, {lmtException,} lmtHeapInfo, lmtCustomData:
      begin
        LStream.Position := 0;
        LStream.CopyFrom(LN.MessageData, LN.MessageData.Size);
        LStream.Position := 0;
        S := LStream.DataString;
        FEditorView.Text := S;
        FEditorView.HighlighterName := 'INI';
      end;
      lmtColor:
      begin
        //LN.Value
      end;
      lmtAlphaColor:
      begin

      end;
      lmtComponent:
      begin
        if Assigned(LN.MessageData) then // component
        begin
          LN.MessageData.Position := 0;
          ObjectBinaryToText(LN.MessageData, LStream);
          LStream.Position := 0;
          S := LStream.DataString;
          FEditorView.Text := S;
          FEditorView.HighlighterName := 'DFM';
        end
        else
        begin
          FEditorView.Text := LN.Value;
        end;
      end;
      lmtBitmap:
      begin
        imgBitmap.Picture.Bitmap.LoadFromStream(LN.MessageData);
        pgcMessageContent.ActivePage := tsImageViewer;
        with imgBitmap.Picture do
        begin
          edtWidth.Text       := Bitmap.Width.ToString;
          edtHeight.Text      := Bitmap.Height.ToString;
          edtPixelFormat.Text := Reflect.EnumName(Bitmap.PixelFormat);
          edtHandleType.Text  := Reflect.EnumName(Bitmap.HandleType);
          //Color := '$' + IntToHex(TransparentColor, 8);
        end;
      end;
      lmtMemory:
      begin
        //edtHex.OpenStream(LN.MessageData);
//        pgcMessageDetails.ActivePageIndex := 3;
      end;
      else
      begin
        FEditorView.Text := LN.Value;
       end;
    end;
    edtMessageType.Text := LogMessageTypeNameOf(LN.MessageType);
    edtTimeStamp.Text   := DateTimeToStr(LN.TimeStamp);
    edtValue.Text       := LN.Value;
    edtValueName.Text   := LN.ValueName;
    edtValueType.Text   := LN.ValueType;
    UpdateCallStack(Node);
  finally
    LStream.Free;
  end;
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
    CellText := LN.Text;
  end
  else if Column = COLUMN_NAME then
  begin
    CellText := LN.ValueName;
  end
  else if Column = COLUMN_TYPE then
  begin
    CellText := LN.ValueType;
  end
  else if Column = COLUMN_VALUE then
  begin
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
  //LN := Sender.GetNodeData<TLogNode>(Node);
  LN.MessageData := FCurrentMsg.Data;
  LN.TimeStamp   := FCurrentMsg.TimeStamp;
  LN.MessageType := TLogMessageType(FCurrentMsg.MsgType);
  if not (LN.MessageType in [lmtValue, lmtComponent, lmtStrings, lmtText]) then
  begin
    LN.Text := string(FCurrentMsg.Text);
  end;
  S := string(FCurrentMsg.Text);

  if (LN.MessageType in  [lmtValue, lmtText]) then
  begin
    I := S.IndexOf('=');
    LN.ValueName := Copy(S, 1, I);
    LN.Value := Copy(S, I + 2, S.Length);
    LN.ValueType := ExtractText(LN.ValueName, '(', ')');
    I := S.IndexOf('(');
    if I > 1 then
      LN.ValueName := Copy(S, 1, I);

    LN.Text := '';
  end;
  LN.Id := FMessageCount;
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
    end;
  end
  else if Column = COLUMN_NAME then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end
  else if Column = COLUMN_TYPE then
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

  if chkAutoFilter.Checked then
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

procedure TfrmMessageList.FReceiverReceiveMessage(Sender: TObject;
  AReceiver: IChannelReceiver; AStream: TStream);
begin
  ProcessMessage(AStream);
end;

procedure TfrmMessageList.FSettingsChanged(Sender: TObject);
begin
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
          FLastNode   := FLogTreeView.AddChild(FLastParent^.Parent, nil);
          FLastParent := FLastNode^.Parent;
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
  FMessageCount := 0;
  FLastNode     := nil;
  FLastParent   := nil;
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

procedure TfrmMessageList.UpdateCallStack(var ANode: PVirtualNode);
var
  I   : Integer;
  CSD : TCallStackData;
  LN  : TLogNode;
begin
  FCallStack.Clear;
  I := FLogTreeView.GetNodeLevel(ANode);
  while I > 0 do
  begin
    LN := FLogTreeView.GetNodeData<TLogNode>(ANode^.Parent);
    CSD := TCallStackData.Create;
    CSD.Title := LN.Text;
    CSD.Level := I;
    FCallStack.Add(CSD);
    ANode := ANode^.Parent;
    Dec(I);
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

procedure TfrmMessageList.UpdateView;
begin
  UpdateLogTreeView;
end;
{$ENDREGION}
end.
