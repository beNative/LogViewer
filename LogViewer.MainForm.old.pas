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

unit LogViewer.MainForm.old;

{ Viewer for Multilog messages

  Copyright (C) 2006 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR actClearMessages PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{ Modified by Tim Sinaeve }

{
  Added support for Spring.Logging
  Supported channels:
    - WinIPC
    - File
    - Serial
    - ZeroMQ

  Dependencies:
    - Spring4D
    - DSharp
    - DDuce
    - Delphi-ZeroMQ binding
    - JsonDataObjects

}

interface
uses
  System.Classes, System.Actions, System.ImageList, System.SysUtils,
  Vcl.ActnList, Vcl.ImgList, Vcl.Controls, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics,
  Vcl.Dialogs, Vcl.Menus,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates,

  DDuce.WinIPC.Server, DDuce.Logger.Interfaces, DDuce.Editor.Interfaces,
  DDuce.Components.GridView, DDuce.Components.Inspector,

  ZeroMQ,

  LogViewer.Watches.Data, LogViewer.Settings, LogViewer.Messages.Data,
  LogViewer.CallStack.Data, LogViewer.Interfaces, LogViewer.Factories;

type
  TBitmap     = Vcl.Graphics.TBitmap;

  TfrmMainOld = class(TForm)
    {$REGION 'designer controls'}
    aclMain              : TActionList;
    actBitmap            : TAction;
    actCallStack         : TAction;
    actCheckPoint        : TAction;
    actClearMessages     : TAction;
    actConditional       : TAction;
    actCustomData        : TAction;
    actError             : TAction;
    actException         : TAction;
    actFilterMessages    : TAction;
    actHeapInfo          : TAction;
    actMemory            : TAction;
    actMethodTraces      : TAction;
    actObject            : TAction;
    actOpen              : TAction;
    actSave              : TAction;
    actSelectAll         : TAction;
    actSelectNone        : TAction;
    actSetFocusToFilter  : TAction;
    actStop              : TAction;
    actStrings           : TAction;
    actToggleAlwaysOnTop : TAction;
    actToggleFullscreen  : TAction;
    actToggleInfo        : TAction;
    actToggleWarning     : TAction;
    actValue             : TAction;
    actWinIPCChannel     : TAction;
    actZeroMQChannel     : TAction;
    btnSeperator1        : TToolButton;
    btnSeperator2        : TToolButton;
    btnBitmap            : TToolButton;
    btnCallStack         : TToolButton;
    btnCheckPoint        : TToolButton;
    btnClearMessages     : TToolButton;
    btnConditional       : TToolButton;
    btnCustomData        : TToolButton;
    btnError             : TToolButton;
    btnException         : TToolButton;
    btnFilterMessages    : TButton;
    btnHeapInfo          : TToolButton;
    btnMemory            : TToolButton;
    btnMethodTraces      : TToolButton;
    btnObject            : TToolButton;
    btnSelectAll         : TToolButton;
    btnSelectNone        : TToolButton;
    btnStop              : TToolButton;
    btnStrings           : TToolButton;
    btnToggleAlwaysOnTop : TToolButton;
    btnToggleInfo        : TToolButton;
    btnToggleWarning     : TToolButton;
    btnValue             : TToolButton;
    btnWinIPCChannel     : TToolButton;
    btnZeroMQChannel     : TToolButton;
    cbxWatchHistory      : TComboBox;
    chkAutoFilter        : TCheckBox;
    edtMessageFilter     : TLabeledEdit;
    imgViewer            : TImage;
    imlMain              : TImageList;
    imlMessageTypes      : TImageList;
    pgcMessageDetails    : TPageControl;
    pgcWatches           : TPageControl;
    pnlCallStack         : TPanel;
    pnlCallStackTitle    : TPanel;
    pnlCallStackWatch    : TPanel;
    pnlFilter            : TPanel;
    pnlLeft              : TPanel;
    pnlLeftBottom        : TPanel;
    pnlMessageContent    : TPanel;
    pnlMessages          : TPanel;
    pnlRight             : TPanel;
    pnlWatches           : TPanel;
    sbrMain              : TStatusBar;
    splLeftHorizontal    : TSplitter;
    splLeftVertical      : TSplitter;
    splVertical          : TSplitter;
    tlbMain              : TToolBar;
    tlbMessages          : TToolBar;
    tmrPoll              : TTimer;
    tsHexEditor          : TTabSheet;
    tsHistory            : TTabSheet;
    tsImageViewer        : TTabSheet;
    tsInspector          : TTabSheet;
    tsLatest             : TTabSheet;
    tsSelected           : TTabSheet;
    tsTextViewer         : TTabSheet;
    actODSChannel        : TAction;
    btnODSChannel        : TToolButton;
    actCollapseAll       : TAction;
    actExpandAll         : TAction;
    btnExpandAll         : TSpeedButton;
    btnCollapseAll       : TSpeedButton;
    actAutoScroll        : TAction;
    btnAutoScroll        : TToolButton;
    {$ENDREGION}

    procedure actBitmapExecute(Sender: TObject);
    procedure actCallStackExecute(Sender: TObject);
    procedure actCheckPointExecute(Sender: TObject);
    procedure actClearMessagesExecute(Sender: TObject);
    procedure actConditionalExecute(Sender: TObject);
    procedure actCustomDataExecute(Sender: TObject);
    procedure actErrorExecute(Sender: TObject);
    procedure actExceptionExecute(Sender: TObject);
    procedure actHeapInfoExecute(Sender: TObject);
    procedure actMemoryExecute(Sender: TObject);
    procedure actMethodTracesExecute(Sender: TObject);
    procedure actObjectExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectNoneExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStringsExecute(Sender: TObject);
    procedure actToggleAlwaysOnTopExecute(Sender: TObject);
    procedure actToggleInfoExecute(Sender: TObject);
    procedure actToggleWarningExecute(Sender: TObject);
    procedure actValueExecute(Sender: TObject);
    procedure actFilterMessagesExecute(Sender: TObject);
    procedure actZeroMQChannelExecute(Sender: TObject);
    procedure actWinIPCChannelExecute(Sender: TObject);
    procedure actSetFocusToFilterExecute(Sender: TObject);
    procedure actToggleFullscreenExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);

    procedure FLogTreeViewFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FLogTreeViewFocusChanging(
      Sender     : TBaseVirtualTree;
      OldNode    : PVirtualNode;
      NewNode    : PVirtualNode;
      OldColumn  : TColumnIndex;
      NewColumn  : TColumnIndex;
      var Allowed: Boolean
    );
    procedure FLogTreeViewFreeNode(
      Sender: TBaseVirtualTree;
      Node  : PVirtualNode
    );
    procedure FLogTreeViewGetImageIndex(
      Sender        : TBaseVirtualTree;
      Node          : PVirtualNode;
      Kind          : TVTImageKind;
      Column        : TColumnIndex;
      var Ghosted   : Boolean;
      var ImageIndex: TImageIndex
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

    procedure FWatchesUpdateWatch(const AVariable, AValue: string);
    procedure FWatchesNewWatch(const AVariable: string; AId: Int64);

    procedure FWatchHistoryInspectorGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FSelectedWatchInspectorGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FLatestWatchInspectorGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FIPCServerMessage(Sender: TObject);

    procedure pgcWatchesChange(Sender: TObject);
    procedure edtMessageFilterChange(Sender: TObject);
    procedure edtMessageFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtMessageFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbxWatchHistorySelect(Sender: TObject);
    procedure tmrPollTimer(Sender: TObject);
    procedure actAutoScrollExecute(Sender: TObject);

  private
    FSettings       : TLogViewerSettings;
    FLogTreeView    : TVirtualStringTree;
    FMessageCount   : Integer;
    FCurrentMsg     : TLogMessage;
    FLastParent     : PVirtualNode;
    FLastNode       : PVirtualNode;
    FIPCServer      : TWinIPCServer;
    FWatches        : TWatchList;
    FExpandParent   : Boolean;
    FEditorManager  : IEditorManager;
    FEditorSettings : IEditorSettings;
    FEditorView     : IEditorView;
    FVKPressed      : Boolean;
    FTVPCallStack   : TTreeViewPresenter;
    FVSTCallStack   : TVirtualStringTree;
    FCallStack      : IList<TCallStackData>;
    FZMQStream      : TStringStream;

    FZMQ        : IZeroMQ;
    FSubscriber : IZMQPair;
    FPoll       : IZMQPoll;

    FLatestWatchInspector   : TInspector;
    FSelectedWatchInspector : TInspector;
    FWatchHistoryInspector  : TInspector;

    function GetEditor: IEditorView;
    function GetTitleFilter: string;

    procedure FilterCallback(
      Sender    : TBaseVirtualTree;
      Node      : PVirtualNode;
      Data      : Pointer;
      var Abort : Boolean
    );
    procedure UpdateActiveMessages(
      const AMessageType : TLogMessageType;
      const Sender       : TObject;
      const AToggle      : Boolean = True
    );

    procedure CreateEditor;
    procedure CreateLogTreeView;
    procedure CreateWatches;
    procedure CreateWatchInspectors;
    procedure CreateIPCServer;
    procedure CreateCallStackViewer;
    procedure CreateZMQSubscriber;

    procedure UpdateMessageDisplay;
    procedure UpdateCallStack(var ANode: PVirtualNode);
    procedure UpdateWatches;
    procedure UpdateWatchHistory;
    procedure ClearMessages;
    procedure ZMQPoll;

    procedure ShowBitmapInfo(ABitmap: TBitmap);

  protected
    procedure UpdateActions; override;
    procedure ProcessMessage(AStream: TStream);

    procedure LoadSettings;
    procedure SaveSettings;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Editor: IEditorView
      read GetEditor;

    property TitleFilter: string
      read GetTitleFilter;
  end;

var
  frmMainOld: TfrmMainOld;

const
  COLUMN_MAIN      = 0;
  COLUMN_NAME      = 1;
  COLUMN_VALUE     = 2;
  COLUMN_TIMESTAMP = 3;

implementation

{$R *.dfm}

uses
  Winapi.Windows, Winapi.Messages,
  System.StrUtils, System.UITypes,

  Spring,

  DDuce.Factories, DDuce.Factories.VirtualTrees, DDuce.Components.Factories,
  DDuce.Editor.Factories,
  DDuce.Logger, DDuce.ScopedReference, DDuce.ObjectInspector, DDuce.Reflect,

  LogViewer.Resources, LogViewer.Receivers.WinODS;

{$REGION 'construction and destruction'}
procedure TfrmMainOld.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TLogViewerSettings.Create;
  FZMQStream := TStringStream.Create;
  CreateEditor;
  CreateLogTreeView;
  CreateWatches;
  CreateWatchInspectors;
  CreateCallStackViewer;
  CreateIPCServer;
  CreateZMQSubscriber;
//  FReceiver := TWinODSReceiver.Create;
//  FReceiver.OnReceiveMessage.Add(FReceiverReceiveMessage);
//  LoadSettings;
//  InspectComponent(FLogTreeView);
end;

procedure TfrmMainOld.BeforeDestruction;
begin
//  FReceiver.OnReceiveMessage.Remove(FReceiverReceiveMessage);
//  FReceiver := nil;
  SaveSettings;
  tmrPoll.Enabled := False;
  FIPCServer.Free;
  FWatches.Free;
  FSubscriber := nil;
  FZMQ  := nil;
  FZMQStream := nil;
  FSettings.Free;
  inherited BeforeDestruction;
end;

procedure TfrmMainOld.CreateCallStackViewer;
begin
  FCallStack    := TCollections.CreateObjectList<TCallStackData>;
  FVSTCallStack := TVirtualStringTreeFactory.CreateGrid(Self, pnlCallStack);
  FTVPCallStack := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTCallStack,
    FCallStack as IObjectList
  );
  FTVPCallStack.ShowHeader := False;
end;

procedure TfrmMainOld.CreateIPCServer;
begin
  FIPCServer := TWinIPCServer.Create;
  FIPCServer.OnMessage := FIPCServerMessage;
  FIPCServer.Active := True;
end;

procedure TfrmMainOld.CreateWatchInspectors;
begin
  FLatestWatchInspector := TDDuceComponents.CreateInspector(Self, tsLatest);
  FLatestWatchInspector.OnGetCellText := FLatestWatchInspectorGetCellText;
  FLatestWatchInspector.ReadOnly  := True;
  FLatestWatchInspector.AllowEdit := False;
  FLatestWatchInspector.ThemingEnabled := True;

  FSelectedWatchInspector := TDDuceComponents.CreateInspector(Self, tsSelected);
  FSelectedWatchInspector.OnGetCellText := FSelectedWatchInspectorGetCellText;
  FSelectedWatchInspector.ReadOnly := True;
  FSelectedWatchInspector.AllowEdit := False;

  FWatchHistoryInspector := TDDuceComponents.CreateInspector(Self, tsHistory);
  FWatchHistoryInspector.OnGetCellText := FWatchHistoryInspectorGetCellText;
  FWatchHistoryInspector.ReadOnly  := True;
  FWatchHistoryInspector.AllowEdit := False;
end;

procedure TfrmMainOld.CreateZMQSubscriber;
begin
  FZMQ := TZeroMQ.Create;
  FSubscriber := FZMQ.Start(ZMQSocket.Subscriber);
  FSubscriber.Connect('tcp://GANYMEDES:5555');
  FSubscriber.Connect('tcp://localhost:5555');
  FSubscriber.Subscribe(''); // required!!
  FPoll := FZMQ.Poller;
  FPoll.RegisterPair(FSubscriber, [PollEvent.PollIn],
    procedure(Event: PollEvents)
    begin
      FZMQStream.WriteString(FSubscriber.ReceiveString);
      ProcessMessage(FZMQStream);
      FZMQStream.Clear;
    end
  );
  tmrPoll.Enabled := True;
end;

procedure TfrmMainOld.CreateWatches;
begin
  FWatches := TWatchList.Create;
  FWatches.OnUpdateWatch := FWatchesUpdateWatch;
  FWatches.OnNewWatch    := FWatchesNewWatch;
end;

procedure TfrmMainOld.CreateLogTreeView;
var
  C : TVirtualTreeColumn;
begin
  FLogTreeView := TFactories.CreateVirtualStringTree(Self, pnlMessages);
  FLogTreeView.NodeDataSize      := SizeOf(TNodeData);
  FLogTreeView.OnFocusChanged    := FLogTreeViewFocusChanged;
  FLogTreeView.OnFocusChanging   := FLogTreeViewFocusChanging;
  FLogTreeView.OnFreeNode        := FLogTreeViewFreeNode;
  FLogTreeView.OnInitNode        := FLogTreeViewInitNode;
  FLogTreeView.OnGetImageIndex   := FLogTreeViewGetImageIndex;
  FLogTreeView.OnGetText         := FLogTreeViewGetText;
  FLogTreeView.OnKeyPress        := FLogTreeViewKeyPress;
  FLogTreeView.OnBeforeCellPaint := FLogTreeViewBeforeCellPaint;

  FLogTreeView.OnPaintText       := FLogTreeViewPaintText;
  FLogTreeView.OnGetHintKind     := FLogTreeViewGetHintKind;
  FLogTreeView.OnGetHint         := FLogTreeViewGetHint;
  FLogTreeView.Images            := imlMessageTypes;
  FLogTreeView.ShowHint          := True;
  //FLogTreeView.Header.Options  := FLogTreeView.Header.Options - [hoVisible];
  FLogTreeView.TreeOptions.AutoOptions := FLogTreeView.TreeOptions.AutoOptions +
    [toAutoSpanColumns];

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
end;

procedure TfrmMainOld.CreateEditor;
begin
  FEditorSettings := TEditorFactories.CreateSettings(Self, 'settings.xml');
  FEditorManager  := TEditorFactories.CreateManager(Self, FEditorSettings);
  FEditorView     := TEditorFactories.CreateView(tsTextViewer, FEditorManager);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMainOld.actClearMessagesExecute(Sender: TObject);
begin
  ClearMessages;
end;

procedure TfrmMainOld.actCollapseAllExecute(Sender: TObject);
begin
  FLogTreeView.FullCollapse;
end;

procedure TfrmMainOld.actConditionalExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtConditional, Sender);
end;

procedure TfrmMainOld.actCustomDataExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtCustomData, Sender);
end;

procedure TfrmMainOld.actErrorExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtError, Sender);
end;

procedure TfrmMainOld.actExceptionExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtException, Sender);
end;

procedure TfrmMainOld.actExpandAllExecute(Sender: TObject);
begin
  FLogTreeView.FullExpand;
end;

procedure TfrmMainOld.actFilterMessagesExecute(Sender: TObject);
begin
  UpdateMessageDisplay;
end;

procedure TfrmMainOld.actHeapInfoExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtHeapInfo, Sender);
end;

procedure TfrmMainOld.actMemoryExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtMemory, Sender);
end;

procedure TfrmMainOld.actMethodTracesExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtEnterMethod, Sender);
  UpdateActiveMessages(lmtLeaveMethod, Sender, False);
end;

procedure TfrmMainOld.actObjectExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtObject, Sender);
end;

procedure TfrmMainOld.actCallStackExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtCallStack, Sender);
end;

procedure TfrmMainOld.actAutoScrollExecute(Sender: TObject);
begin
//
end;

procedure TfrmMainOld.actBitmapExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtBitmap, Sender);
end;

procedure TfrmMainOld.actCheckPointExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtCheckpoint, Sender);
end;

procedure TfrmMainOld.actSelectAllExecute(Sender: TObject);
begin
    UpdateMessageDisplay;
end;

procedure TfrmMainOld.actSelectNoneExecute(Sender: TObject);
begin
  UpdateMessageDisplay;
end;

procedure TfrmMainOld.actSetFocusToFilterExecute(Sender: TObject);
begin
  edtMessageFilter.SetFocus;
end;

procedure TfrmMainOld.actStopExecute(Sender: TObject);
begin
  if actStop.Checked then
  begin
    FIPCServer.OnMessage := nil;
    tmrPoll.Enabled := False;
  end
  else
  begin
    FIPCServer.OnMessage := FIPCServerMessage;
    tmrPoll.Enabled := True;
  end;
end;

procedure TfrmMainOld.actStringsExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtStrings, Sender);
end;

procedure TfrmMainOld.actToggleAlwaysOnTopExecute(Sender: TObject);
begin
  if actToggleAlwaysOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TfrmMainOld.actToggleFullscreenExecute(Sender: TObject);
begin
  if WindowState = wsNormal then
    WindowState := wsMaximized
  else
    WindowState := wsNormal;
end;

procedure TfrmMainOld.actToggleInfoExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtInfo, Sender);
end;

procedure TfrmMainOld.actToggleWarningExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtWarning, Sender);
end;

procedure TfrmMainOld.actValueExecute(Sender: TObject);
begin
  UpdateActiveMessages(lmtValue, Sender);
end;
procedure TfrmMainOld.actWinIPCChannelExecute(Sender: TObject);
begin
  FIPCServer.Active := actWinIPCChannel.Checked;
end;

procedure TfrmMainOld.actZeroMQChannelExecute(Sender: TObject);
begin
  if actZeroMQChannel.Checked then
  begin
    if Assigned(FSubscriber) then
    begin
      FSubscriber.Close;
      tmrPoll.Enabled := False;
    end;
    FSubscriber := nil;
    FPoll := nil;
    FZMQ := nil;
    CreateZMQSubscriber;
  end
  else
  begin
    FSubscriber.Close;
    tmrPoll.Enabled := False;
    FSubscriber := nil;
    FPoll := nil;
    FZMQ := nil;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'edtMessageFilter'}
procedure TfrmMainOld.edtMessageFilterChange(Sender: TObject);
begin
  if TitleFilter <> '' then
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
    UpdateMessageDisplay;
end;

procedure TfrmMainOld.edtMessageFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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

procedure TfrmMainOld.edtMessageFilterKeyUp(Sender: TObject; var Key: Word;
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

{$REGION 'FWatches'}
procedure TfrmMainOld.FWatchesNewWatch(const AVariable: string; AId: Int64);
begin
  cbxWatchHistory.Items.Add(AVariable);
end;

procedure TfrmMainOld.FWatchesUpdateWatch(const AVariable, AValue: string);
begin
  FLatestWatchInspector.Rows.Count   := FWatches.Count;
  FSelectedWatchInspector.Rows.Count := FWatches.Count;
  FLatestWatchInspector.Invalidate;
  FSelectedWatchInspector.Invalidate;
end;
{$ENDREGION}

{$REGION 'FLogTreeView'}
procedure TfrmMainOld.FLogTreeViewFocusChanging(Sender: TBaseVirtualTree;
  OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  //Todo: merge with Changed?
  //The CallStack is only updated if the parent changes
  Allowed := OldNode <> NewNode;
  if Allowed and ((OldNode = nil) or (NewNode = nil) or
    (OldNode^.Parent <> NewNode^.Parent)) then
    UpdateCallStack(NewNode);
end;

procedure TfrmMainOld.FLogTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
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

procedure TfrmMainOld.FLogTreeViewPaintText(Sender: TBaseVirtualTree;
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

procedure TfrmMainOld.FLogTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LStream : TStringStream;
  S       : string;
  ND      : PNodeData;
begin
  UpdateWatches;
  LStream := TStringStream.Create('', TEncoding.ANSI);
  ND := PNodeData(Sender.GetNodeData(Node));
  try
    if ND.MsgData = nil then
    begin
      pgcMessageDetails.ActivePage := tsTextViewer;
      Editor.Text := '';
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
        Editor.Text := S;
        Editor.HighlighterName := 'INI';
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
          Editor.Text := S;
          Editor.HighlighterName := 'DFM';
          pgcMessageDetails.ActivePage := tsTextViewer;
        end
        else
        begin
          S := ND.Title;
          ND.Name := Copy(S, 1, Pos(sLineBreak, S));
          Editor.Text := Copy(S, Pos(sLineBreak, S) + 2, Length(S));
          pgcMessageDetails.ActivePageIndex := 0;
        end;
      end;
      lmtBitmap:
      begin
        imgViewer.Picture.Bitmap.LoadFromStream(ND.MsgData);
        pgcMessageDetails.ActivePage := tsImageViewer;
        // Index := 1;
        ShowBitmapInfo(imgViewer.Picture.Bitmap);
      end;
      lmtMemory:
      begin
        //edtHex.OpenStream(ND.MsgData);
{


        }

        pgcMessageDetails.ActivePageIndex := 3;
      end;
      else
      begin
        S := ND.Title;
        if S.Contains('=') then
        begin
          S := Copy(S, Pos('=', S) + 2, Length(S));
          Editor.Text := S;
        end;
        pgcMessageDetails.ActivePageIndex := 0;
      end;
    end;
  finally
    LStream.Free;
  end;
end;

procedure TfrmMainOld.FLogTreeViewGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle:
  TVTTooltipLineBreakStyle; var HintText: string);
var
  ND: PNodeData;
begin
  ND := Sender.GetNodeData(Node);
  HintText := Reflect.Fields(ND).ToString;
end;

procedure TfrmMainOld.FLogTreeViewGetHintKind(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Kind: TVTHintKind);
begin
  Kind := vhkText;
end;

procedure TfrmMainOld.FLogTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  ND: PNodeData;
begin
  ND := Sender.GetNodeData(Node);
  if Column = COLUMN_MAIN then
    ImageIndex := Integer(ND.MsgType);
end;

procedure TfrmMainOld.FLogTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  ND  : PNodeData;
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
    //CellText := DateTimeToStr(ND.MsgTime);
//    NDP := Sender.GetNodeData(Node.PrevSibling);
//    if Assigned(NDP) then
//    begin
//      S := TimeToStr(NDP.MsgTime);
//      if TimeToStr(ND.MsgTime) <> S then
//        CellText := TimeToStr(ND.MsgTime)
//      else
//      begin
//        CellText := '';
//      end;
//    end
//    else
      CellText := TimeToStr(ND.MsgTime);
  end;
end;

procedure TfrmMainOld.FLogTreeViewInitNode(Sender: TBaseVirtualTree;
  ParentNode: PVirtualNode; Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
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
  Sender.IsVisible[Node] := True;
  //(MsgType in [lmtEnterMethod, lmtExitMethod]) or
//      ((MsgType in FActiveMessages);
    //and IsWild(Title, FTitleFilter, True));

  UpdateWatches;
end;

procedure TfrmMainOld.FLogTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  ND: PNodeData;
begin
  ND := Sender.GetNodeData(Node);
  ND.Title := '';
  if Assigned(ND.MsgData) then
    FreeAndNil(ND.MsgData);
end;

{ Processes keyboard input when treeview has focus. }

procedure TfrmMainOld.FLogTreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  if not edtMessageFilter.Focused then
  begin
    edtMessageFilter.SetFocus;
    PostMessage(edtMessageFilter.Handle, WM_CHAR, Ord(Key), 0);
    edtMessageFilter.SelStart := Length(TitleFilter);
    // required to prevent the invocation of accelerator keys!
    Key := #0;
  end;
end;
{$ENDREGION}

{$REGION 'Watch viewers'}
procedure TfrmMainOld.FWatchHistoryInspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
    Value := FWatches.Items[cbxWatchHistory.ItemIndex].Name
  else
    Value := FWatches.Items[cbxWatchHistory.ItemIndex].Values[Cell.Row];
end;

procedure TfrmMainOld.FLatestWatchInspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
  begin
    Value := FWatches.Items[Cell.Row].Name;
  end
  else
  begin
    Value := FWatches.Items[Cell.Row].Value
  end;
end;

procedure TfrmMainOld.FSelectedWatchInspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
    Value := FWatches.Items[Cell.Row].Name
  else
    Value := FWatches.Items[Cell.Row].Value;
end;

procedure TfrmMainOld.pgcWatchesChange(Sender: TObject);
begin
  UpdateWatches;
end;

procedure TfrmMainOld.cbxWatchHistorySelect(Sender: TObject);
begin
  UpdateWatchHistory;
end;
{$ENDREGION}

{$REGION 'Receiver events'}
procedure TfrmMainOld.FIPCServerMessage(Sender: TObject);
begin
  ProcessMessage(TWinIPCServer(Sender).MsgData);
end;
{$ENDREGION}

procedure TfrmMainOld.tmrPollTimer(Sender: TObject);
begin
  ZMQPoll;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMainOld.GetEditor: IEditorView;
begin
  Result := FEditorView;
end;

function TfrmMainOld.GetTitleFilter: string;
begin
  Result := Trim(edtMessageFilter.Text);
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMainOld.ZMQPoll;
begin
  if Assigned(FPoll) then
  begin
    while FPoll.PollOnce(50) > 0 do
      FPoll.FireEvents;
  end;
end;

procedure TfrmMainOld.ClearMessages;
begin
  FLogTreeView.Clear;
  FWatches.Clear;
  cbxWatchHistory.Clear;
  Editor.Lines.Clear;
  pgcMessageDetails.ActivePageIndex := 0;
  FMessageCount := 0;
  FLastNode     := nil;
  FLastParent   := nil;
  FLatestWatchInspector.Rows.Count   := 0;
  FSelectedWatchInspector.Rows.Count := 0;
  FWatchHistoryInspector.Rows.Count  := 0;
end;

procedure TfrmMainOld.FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
var
  ND : PNodeData;
  B  : Boolean;
begin
  ND := Sender.GetNodeData(Node);
//  B := ND.MsgType in FActiveMessages;
  if TitleFilter <> '' then
    B := B and ContainsText(ND.Title, TitleFilter);
  Sender.IsVisible[Node] := B;
end;

procedure TfrmMainOld.UpdateActiveMessages(const AMessageType: TLogMessageType;
  const Sender: TObject; const AToggle: Boolean);
var
  A : TAction;
begin
  A := Sender as TAction;
  if AToggle then
    A.Checked := not A.Checked;
  UpdateMessageDisplay;
end;

procedure TfrmMainOld.UpdateCallStack(var ANode: PVirtualNode);
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
  FTVPCallStack.TreeView.Header.AutoFitColumns;
end;

procedure TfrmMainOld.UpdateMessageDisplay;
begin
  FLogTreeView.BeginUpdate;
  try
    FLogTreeView.IterateSubtree(nil, FilterCallback, nil);
    FLogTreeView.Header.AutoFitColumns;
  finally
    FLogTreeView.EndUpdate;
  end;
end;

procedure TfrmMainOld.UpdateWatches;
var
  LTempIndex: LongWord;
begin
  if (pgcWatches.ActivePage = tsLatest)
    or (pgcWatches.ActivePage = tsSelected) then
  begin
    if pgcWatches.ActivePage = tsLatest then
    begin
      LTempIndex := FMessageCount;
    end
    else
    begin
      if FLogTreeView.FocusedNode <> nil then
        LTempIndex := PNodeData(
          FLogTreeView.GetNodeData(FLogTreeView.FocusedNode)
        )^.Index
      else
        LTempIndex := 0;
    end;
    FWatches.Update(LTempIndex);
  end
  else if pgcWatches.ActivePage = tsHistory then
  begin
    UpdateWatchHistory;
  end;
end;

procedure TfrmMainOld.UpdateWatchHistory;
var
  I: Integer;
begin
  if cbxWatchHistory.ItemIndex = -1 then
    Exit;
  I := Integer(cbxWatchHistory.Items.Objects[cbxWatchHistory.ItemIndex]);
  FWatchHistoryInspector.Rows.Count := FWatches[I].Count;
end;

procedure TfrmMainOld.ShowBitmapInfo(ABitmap: TBitmap);
begin
  with {StringGridBitmap, }ABitmap do
  begin
    //Cells[1, 0] := IntToStr(Height);
    //Cells[1, 1] := IntToStr(Width);
    //Cells[1, 2] := PixelFormatNames[PixelFormat];
    //Cells[1, 3] := HandleTypeNames[HandleType];
    //Cells[1, 4] := '$' + IntToHex(TransparentColor, 8);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Decodes message stream and assigns it to FCurrentMsg: TLogMessage, and updates
  the corresponding message viewers (logdata, callstack, watches, etc.). }

procedure TfrmMainOld.ProcessMessage(AStream: TStream);
var
  LTextSize : Integer;
  LDataSize : Integer;
begin
  Guard.CheckNotNull(AStream, 'AStream');
  LTextSize := 0;
  LDataSize := 0;
  FLogTreeView.BeginUpdate;
  try
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

//    AStream.ReadBuffer(LTextSize, SizeOf(Integer));
//    SetLength(FCurrentMsg.ProcessName, LTextSize);
//    AStream.ReadBuffer(FCurrentMsg.ProcessName[1], LTextSize);

    case TLogMessageType(FCurrentMsg.MsgType) of
      lmtEnterMethod:
      begin
        FLastNode := FLogTreeView.AddChild(FLastParent, nil);
        if FExpandParent then
          FLogTreeView.Expanded[FLastParent] := True
        else
          FExpandParent := True;
        FLastParent := FLastNode;
        FLogTreeView.ValidateNode(FLastNode, False);
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
        FLogTreeView.ValidateNode(FLastNode, False);
      end;
//      lmtValue:
//      begin
//        FLastNode := FLogTreeView.AddChild(FLastParent, nil);
//      end;
     lmtWatch, lmtCounter:
      begin
        FWatches.Add(
          string(FCurrentMsg.Text),
          FMessageCount,
          FCurrentMsg.TimeStamp,
          FCurrentMsg.MsgType = Integer(lmtCounter) // SkipOnNewValue
        );
        UpdateWatches;
      end;
      lmtClear:
      begin
        ClearMessages;
      end
      else
      begin
        FLastNode := FLogTreeView.AddChild(FLastParent, nil);
      end;
    end;
    FLogTreeView.ValidateNode(FLastNode, False);
    if FExpandParent then
    begin
      FLogTreeView.Expanded[FLastParent] := True;
      FExpandParent := False;
    end;
  finally
    FLogTreeView.EndUpdate;
  end;
  if actAutoScroll.Checked then
  begin
    FLogTreeView.FocusedNode := FLogTreeView.GetLast;
  end;
end;

procedure TfrmMainOld.UpdateActions;
var
  B: Boolean;
begin

  B := not actStop.Checked;
  actBitmap.Enabled        := B;
  actCallStack.Enabled     := B;
  actCheckPoint.Enabled    := B;
  actConditional.Enabled   := B;
  actToggleInfo.Enabled    := B;
  actToggleWarning.Enabled := B;
  actValue.Enabled         := B;
  actError.Enabled         := B;
  actMethodTraces.Enabled  := B;
  actException.Enabled     := B;
  actObject.Enabled        := B;
  actHeapInfo.Enabled      := B;
  actCustomData.Enabled    := B;
  actStrings.Enabled       := B;
  actMemory.Enabled        := B;
  
  actToggleAlwaysOnTop.Checked := FormStyle = fsStayOnTop;

  actFilterMessages.Enabled := not chkAutoFilter.Checked and (TitleFilter <> '');
  sbrMain.SimpleText := Format('%d messages received.', [FMessageCount]);
  inherited UpdateActions;
end;

procedure TfrmMainOld.LoadSettings;
begin
  FSettings.Load;
  FSettings.FormSettings.AssignTo(Self);
  pnlCallStackWatch.Width := FSettings.LeftPanelWidth;
  pnlRight.Width := FSettings.RightPanelWidth;
end;

procedure TfrmMainOld.SaveSettings;
begin
  FSettings.LeftPanelWidth := pnlCallStackWatch.Width;
  FSettings.RightPanelWidth := pnlRight.Width;
  FSettings.FormSettings.Assign(Self);
  FSettings.Save;
end;
{$ENDREGION}

end.
