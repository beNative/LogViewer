{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Message viewer that can be used to display all messages from an associated
  log channel (IChannelReceiver receiver instance) }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ImgList,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates,

  DDuce.Editor.Interfaces, DDuce.Logger.Interfaces,

  LogViewer.Messages.Data, LogViewer.Watches.Data, LogViewer.Watches.View,
  LogViewer.Interfaces, LogViewer.CallStack.Data, LogViewer.CallStack.View,
  LogViewer.MessageList.Settings;

type
  TfrmMessageList = class(TForm, ILogViewerMessagesView)
    {$REGION 'designer controls'}
    btnCollapseAll    : TSpeedButton;
    btnExpandAll      : TSpeedButton;
    btnFilterMessages : TButton;
    chkAutoFilter     : TCheckBox;
    edtMessageFilter  : TLabeledEdit;
    imgViewer         : TImage;
    pgcMessageDetails : TPageControl;
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
    tsHexEditor       : TTabSheet;
    tsImageViewer     : TTabSheet;
    tsInspector       : TTabSheet;
    tsTextViewer      : TTabSheet;
    imlMessageTypes   : TImageList;

    procedure edtMessageFilterChange(Sender: TObject);
    procedure edtMessageFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtMessageFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    {$ENDREGION}

  private
//    FMessages        : IList<TLogMessageData>;
    FMessageCount    : Integer;
    FCurrentMsg      : TLogMessage;
    FCallStack       : IList<TCallStackData>;
    FWatches         : TWatchList;
    FLogTreeView     : TVirtualStringTree;
    FReceiver        : IChannelReceiver;
    FCallStackView   : TfrmCallStackView;
    FWatchesView     : TfrmWatchesView;
    FManager         : ILogViewerManager;

    FEditorManager   : IEditorManager;
    FEditorSettings  : IEditorSettings;
    FEditorView      : IEditorView;
    FExpandParent    : Boolean;
    FLastParent      : PVirtualNode;
    FLastNode        : PVirtualNode;
    FVKPressed       : Boolean;

    {$REGION 'FLogTreeView event handlers'}
    procedure FLogTreeViewFilterCallback(
      Sender    : TBaseVirtualTree;
      Node      : PVirtualNode;
      Data      : Pointer;
      var Abort : Boolean
    );

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
      var ImageIndex : LongInt
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
    function GetManager: ILogViewerManager;
    function GetActions: ILogViewerActions;
    {$ENDREGION}

  protected
    procedure FReceiverReceiveMessage(Sender: TObject; AStream: TStream);

    procedure Clear;

    procedure ProcessMessage(AStream: TStream);

    procedure AddMessageToTree(const AMessage: TLogMessage);

    procedure UpdateCallStack(var ANode: PVirtualNode);
    procedure UpdateLogTreeView;

    procedure CreateLogTreeView;
    procedure CreateEditor;
    procedure CreateCallStackView;
    procedure CreateWatchesView;

    procedure Activate; override;
    procedure UpdateActions; override;
    procedure UpdateView;

    property Manager: ILogViewerManager
      read GetManager;

    property Actions: ILogViewerActions
      read GetActions;

  public
    constructor Create(
      AOwner    : TComponent;
      AManager  : ILogViewerManager;
      AReceiver : IChannelReceiver
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.StrUtils,

  Spring,

  DDuce.Factories, DDuce.Reflect,
  DDuce.Editor.Factories,

  DSharp.Windows.ColumnDefinitions.ControlTemplate,

  LogViewer.Factories, LogViewer.Resources;

{$R *.dfm}

const
  COLUMN_MAIN      = 0;
  COLUMN_NAME      = 1;
  COLUMN_VALUE     = 2;
  COLUMN_TIMESTAMP = 3;

{$REGION 'construction and destruction'}
constructor TfrmMessageList.Create(AOwner: TComponent; AManager
  : ILogViewerManager; AReceiver: IChannelReceiver);
begin
  inherited Create(AOwner);
  FReceiver := AReceiver;
  FManager := AManager;
  CreateEditor;
  CreateLogTreeView;
  CreateWatchesView;
  CreateCallStackView;
  FReceiver.OnReceiveMessage.Add(FReceiverReceiveMessage);
end;

procedure TfrmMessageList.BeforeDestruction;
begin
  FReceiver.Enabled := False;
  FReceiver.OnReceiveMessage.Remove(FReceiverReceiveMessage);
  FReceiver := nil;
  FManager := nil;
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
  FEditorView     := TEditorFactories.CreateView(tsTextViewer, FEditorManager);
end;

procedure TfrmMessageList.CreateLogTreeView;
var
  C : TVirtualTreeColumn;
begin
//  FMessages := TCollections.CreateObjectList<TLogMessageData>;
  FLogTreeView := TFactories.CreateVirtualStringTree(Self, pnlMessages);
  FLogTreeView.TreeOptions.AutoOptions := FLogTreeView.TreeOptions.AutoOptions +
    [toAutoSpanColumns];

  FLogTreeView.NodeDataSize := SizeOf(TNodeData);
  FLogTreeView.Images       := imlMessageTypes;

  FLogTreeView.OnBeforeItemPaint := FLogTreeViewBeforeItemPaint;
  FLogTreeView.OnAfterItemPaint  := FLogTreeViewAfterItemPaint;

  FLogTreeView.OnBeforeCellPaint := FLogTreeViewBeforeCellPaint;

  FLogTreeView.OnFocusChanged    := FLogTreeViewFocusChanged;
  FLogTreeView.OnFocusChanging   := FLogTreeViewFocusChanging;

  FLogTreeView.OnInitNode        := FLogTreeViewInitNode;
  FLogTreeView.OnFreeNode        := FLogTreeViewFreeNode;

  FLogTreeView.OnGetHint         := FLogTreeViewGetHint;
  FLogTreeView.OnGetHintKind     := FLogTreeViewGetHintKind;

  FLogTreeView.OnGetText         := FLogTreeViewGetText;
  FLogTreeView.OnPaintText       := FLogTreeViewPaintText;
  FLogTreeView.OnGetImageIndex   := FLogTreeViewGetImageIndex;

  FLogTreeView.OnKeyPress        := FLogTreeViewKeyPress;

  C := FLogTreeView.Header.Columns.Add;
  C.Text := STitle;
  C.Options := C.Options + [coFixed, coAutoSpring];
  C.Width := 50;
  C.MinWidth := 50;

  C := FLogTreeView.Header.Columns.Add;
  C.Text := SName;
  //C.Alignment := taCenter;
  C.Width := 100;
  C.MinWidth := 100;

  C := FLogTreeView.Header.Columns.Add;
  C.Text := SValue;

  C.Width := 300;
  C.MinWidth := 150;

  C := FLogTreeView.Header.Columns.Add;
  C.Text := STimestamp;
  C.Width    := 80;
  C.MinWidth := 80;


//  FTVPMessages := TFactories.CreateTreeViewPresenter(
//    Self,
//    FLogTreeView,
//    FMessages as IObjectList
//  );
//  FTVPMessages.View.ItemTemplate := TLogTemplate.Create(
//    FTVPMessages.ColumnDefinitions,
//    FMessages
//  );
//  FTVPMessages.OnSelectionChanged := FTVPMessagesSelectionChanged;
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

{$REGION 'property access methods'}
function TfrmMessageList.GetActions: ILogViewerActions;
begin
  Result := Manager as ILogViewerActions;
end;

function TfrmMessageList.GetManager: ILogViewerManager;
begin
  Result := FManager;
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

procedure TfrmMessageList.FLogTreeViewAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
begin
//
end;

procedure TfrmMessageList.FLogTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  ND : PNodeData;
begin
  ND := PNodeData(Sender.GetNodeData(Node));
  if ND.MsgType in [lmtEnterMethod, lmtLeaveMethod] then
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
  ND : PNodeData;
  B  : Boolean;
begin
  ND := Sender.GetNodeData(Node);
  B := ND.MsgType in Manager.VisibleMessageTypes;
  if edtMessageFilter.Text <> '' then
    B := B and
    (ContainsText(ND.Title, edtMessageFilter.Text) or
     ContainsText(ND.Name, edtMessageFilter.Text) or
     ContainsText(ND.Value, edtMessageFilter.Text));

  Sender.IsVisible[Node] := B;
end;

procedure TfrmMessageList.FLogTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LStream : TStringStream;
  S       : string;
  ND      : PNodeData;
begin
  LStream := TStringStream.Create('', TEncoding.ANSI);
  ND := PNodeData(Sender.GetNodeData(Node));
  FWatchesView.UpdateView(ND.Index);
  try
    if ND.MsgData = nil then
    begin
      pgcMessageDetails.ActivePage := tsTextViewer;
      FEditorView.Text := '';
    end
    else
      ND.MsgData.Position := 0;

    case ND.MsgType of
      lmtStrings, lmtCallStack, lmtException, lmtHeapInfo, lmtCustomData:
      begin
        LStream.Position := 0;
        LStream.CopyFrom(ND.MsgData, ND.MsgData.Size);
        LStream.Position := 0;
        S := LStream.DataString;
        FEditorView.Text := S;
        FEditorView.HighlighterName := 'INI';
        pgcMessageDetails.ActivePage := tsTextViewer;
      end;
      lmtObject:
      begin
        if Assigned(ND.MsgData) then // component
        begin
          ND.MsgData.Position := 0;
          ObjectBinaryToText(ND.MsgData, LStream);
          LStream.Position := 0;
          S := LStream.DataString;
          FEditorView.Text := S;
          FEditorView.HighlighterName := 'DFM';
          pgcMessageDetails.ActivePage := tsTextViewer;
        end
        else
        begin
          S := ND.Title;
          ND.Name := Copy(S, 1, Pos(sLineBreak, S));
          FEditorView.Text := Copy(S, Pos(sLineBreak, S) + 2, Length(S));
          pgcMessageDetails.ActivePageIndex := 0;
        end;
      end;
      lmtBitmap:
      begin
        imgViewer.Picture.Bitmap.LoadFromStream(ND.MsgData);
        pgcMessageDetails.ActivePage := tsImageViewer;
        // Index := 1;
        //ShowBitmapInfo(imgViewer.Picture.Bitmap);
      end;
      lmtMemory:
      begin
        //edtHex.OpenStream(ND.MsgData);
        pgcMessageDetails.ActivePageIndex := 3;
      end;
      else
      begin
        S := ND.Title;
        if S.Contains('=') then
        begin
          S := Copy(S, Pos('=', S) + 2, Length(S));
          FEditorView.Text := S;
        end;
        pgcMessageDetails.ActivePageIndex := 0;
      end;
    end;
    UpdateCallStack(Node);
  finally
    LStream.Free;
  end;
end;

procedure TfrmMessageList.FLogTreeViewGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  ND: PNodeData;
begin
  ND := Sender.GetNodeData(Node);
  HintText := Reflect.Fields(ND).ToString;
end;

procedure TfrmMessageList.FLogTreeViewGetHintKind(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Kind: TVTHintKind);
begin
  Kind := vhkText;
end;

procedure TfrmMessageList.FLogTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  ND: PNodeData;
begin
  ND := Sender.GetNodeData(Node);
  if Column = COLUMN_MAIN then
    ImageIndex := Integer(ND.MsgType);
end;

procedure TfrmMessageList.FLogTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  ND : PNodeData;
begin
  ND := Sender.GetNodeData(Node);
  if Column = COLUMN_MAIN then
  begin
    CellText := ND.Title;
  end
  else if Column = COLUMN_NAME then
  begin
    CellText := ND.Name;
  end
  else if Column = COLUMN_VALUE then
  begin
    CellText := ND.Value;
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    CellText := TimeToStr(ND.MsgTime);
  end;
end;

procedure TfrmMessageList.FLogTreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  ND: PNodeData;
  I : Integer;
begin
  ND := Sender.GetNodeData(Node);
  ND.Title   := string(FCurrentMsg.Text);
  ND.MsgData := FCurrentMsg.Data;
  ND.MsgTime := FCurrentMsg.TimeStamp;
  ND.MsgType := TLogMessageType(FCurrentMsg.MsgType);
  if (ND.MsgType = lmtValue) and not (ND.Title.Contains(sLineBreak)) then
  begin
    I := ND.Title.IndexOf('=');
    ND.Name := Copy(ND.Title, 1, I - 1);
    ND.Value := Copy(ND.Title, I + 2, ND.Title.Length);
    ND.Title := '';
  end;
  ND.Index := FMessageCount;
  //Show only what matches filter criterias
  Sender.IsVisible[Node] := (ND.MsgType in [lmtEnterMethod, lmtLeaveMethod]) or
      (ND.MsgType in Manager.VisibleMessageTypes);
    //and IsWild(Title, FTitleFilter, True));
end;

procedure TfrmMessageList.FLogTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  ND: PNodeData;
begin
  ND := Sender.GetNodeData(Node);
  ND.Title := '';
  if Assigned(ND.MsgData) then
    FreeAndNil(ND.MsgData);
end;

procedure TfrmMessageList.FLogTreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  if not edtMessageFilter.Focused then
  begin
    edtMessageFilter.SetFocus;
    PostMessage(edtMessageFilter.Handle, WM_CHAR, Ord(Key), 0);
    //edtMessageFilter.SelStart := Length(TitleFilter);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;

{ Used to apply custom settings to TargetCanvas.Font. }

procedure TfrmMessageList.FLogTreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  ND : PNodeData;
begin
  ND := PNodeData(Sender.GetNodeData(Node));
  if Column = COLUMN_MAIN then
  begin
    case ND.MsgType of
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
  else if Column = COLUMN_VALUE then
  begin
    TargetCanvas.Font.Color := clNavy;
  end
  else if Column = COLUMN_TIMESTAMP then
  begin
    TargetCanvas.Font.Color := clBlue;
  end;
end;
{$ENDREGION}

procedure TfrmMessageList.FReceiverReceiveMessage(Sender: TObject;
  AStream: TStream);
begin
  ProcessMessage(AStream);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessageList.Activate;
begin
  inherited Activate;
  Manager.ActiveView := Self as ILogViewerMessagesView;
end;

procedure TfrmMessageList.AddMessageToTree(const AMessage: TLogMessage);
begin
  FLogTreeView.BeginUpdate;
  try
    case TLogMessageType(FCurrentMsg.MsgType) of
      lmtEnterMethod:
      begin
        FLastNode := FLogTreeView.AddChild(FLastParent, nil);
        if FExpandParent then
          FLogTreeView.Expanded[FLastParent] := True
        else
          FExpandParent := True;
        FLastParent := FLastNode;
      end;
      lmtLeaveMethod:
      begin
        if (FLastParent = nil)
          or (FLastParent^.Parent = FLogTreeView.RootNode) then
        begin
          FLastNode := FLogTreeView.AddChild(nil, nil);
          FLastParent := nil;
        end
        else
        begin
          FLastNode := FLogTreeView.AddChild(FLastParent^.Parent, nil);
          FLastParent := FLastNode^.Parent;
        end;
      end
      else
      begin
        FLastNode := FLogTreeView.AddChild(FLastParent, nil);
      end;
    end; // case
    FLogTreeView.ValidateNode(FLastNode, False);
    if FExpandParent then
    begin
      FLogTreeView.Expanded[FLastParent] := True;
      FExpandParent := False;
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
  pgcMessageDetails.ActivePageIndex := 0;
  FMessageCount := 0;
  FLastNode     := nil;
  FLastParent   := nil;
end;

{ Reads the received message from the active logchannel. }

procedure TfrmMessageList.ProcessMessage(AStream: TStream);
var
  LTextSize : Integer;
  LDataSize : Integer;
begin
  Guard.CheckNotNull(AStream, 'AStream');
  LTextSize := 0;
  LDataSize := 0;

  Inc(FMessageCount);
  AStream.Seek(0, soFromBeginning);
  AStream.ReadBuffer(FCurrentMsg.MsgType, SizeOf(Integer));
  AStream.ReadBuffer(FCurrentMsg.TimeStamp, SizeOf(TDateTime));
  AStream.ReadBuffer(LTextSize, SizeOf(Integer));
  SetLength(FCurrentMsg.Text, LTextSize);
  AStream.ReadBuffer(FCurrentMsg.Text[1], LTextSize);
  AStream.ReadBuffer(LDataSize, SizeOf(Integer));

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
      FWatches.Add(
        string(FCurrentMsg.Text),
        FMessageCount,
        FCurrentMsg.TimeStamp,
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

//  if actAutoScroll.Checked then
//  begin
//    FLogTreeView.FocusedNode := FLogTreeView.GetLast;
//  end;

{
var
  LTextSize : Integer;
  LDataSize : Integer;
  LMD       : TLogMessageData;
  LTime     : TDateTime;
  LType     : Integer;
  LText     : AnsiString;
begin
  Guard.CheckNotNull(AStream, 'AStream');
  LMD := TLogMessageData.Create;
  LMD.Index := FMessages.Count;
  LTextSize := 0;
  LDataSize := 0;
  AStream.Seek(0, soFromBeginning);
  AStream.ReadBuffer(LType, SizeOf(Integer));
  LMD.MessageType := TLogMessageType(LType);
  AStream.ReadBuffer(LTime, SizeOf(TDateTime));
  LMD.TimeStamp := LTime;
  AStream.ReadBuffer(LTextSize, SizeOf(Integer));
  SetLength(LText, LTextSize);
  AStream.ReadBuffer(LText[1], LTextSize);
  LMD.Text := LText;
  AStream.ReadBuffer(LDataSize, SizeOf(Integer));
  if LDataSize > 0 then
  begin
    LMD.Data.CopyFrom(AStream, LDataSize);
  end;
  if LMD.MessageType = lmtLeaveMethod then
  begin
    LMD.Level  := FCurrentMessage.Level - 1;
    if Assigned(FCurrentMessage.Parent) then
      LMD.Parent := FCurrentMessage.Parent.Parent;
  end
  else
  begin
    if Assigned(FCurrentMessage) then
    begin
      if FCurrentMessage.MessageType = lmtEnterMethod then
      begin
        LMD.Level  := FCurrentMessage.Level + 1;
        LMD.Parent := FCurrentMessage;
        LMD.Parent.Children.Add(LMD);
      end
  //    else if LMD.MsgType = lmtLeaveMethod then
  //    begin
  //      LMD.MsgLevel := FCurrentMessage.MsgLevel - 1;
  //    end
      else
      begin
        LMD.Level  := FCurrentMessage.Level;
        LMD.Parent := FCurrentMessage.Parent;
        if Assigned(LMD.Parent) then
          LMD.Parent.Children.Add(LMD);
      end;
    end
    else
    begin
      LMD.Level := 0;
    end;
  end;
  FMessages.Add(LMD);
  FCurrentMessage := LMD;
}

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
begin
  FCallStack.Clear;
  I := FLogTreeView.GetNodeLevel(ANode);
  while I > 0 do
  begin
    CSD := TCallStackData.Create;
    CSD.Title := PNodeData(FLogTreeView.GetNodeData(ANode^.Parent))^.Title;
    CSD.Level := I;
    FCallStack.Add(CSD);
    ANode := ANode^.Parent;
    Dec(I);
  end;
  //FTVPCallStack.TreeView.Header.AutoFitColumns;
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
