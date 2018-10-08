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

unit LogViewer.Dashboard.View;

interface

{ Provides an overview of all active channels. }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.UITypes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ImgList, Vcl.Menus,

  Spring.Collections,

  VirtualTrees,

  ZeroMQ,

  synaser,

  LogViewer.Interfaces, LogViewer.Dashboard.Node,
  LogViewer.ComPort.Settings.View;

  {
    TODO:
      - indicate amount of data received per channel
      - editable treeview in which we can add nodes for every channel we want to
        subscribe to.
  }

type
  TfrmDashboard = class(TForm)
    {$REGION 'designer controls'}
    aclMain                   : TActionList;
    actAddZeroMQNode          : TAction;
    actAddZMQNodeForLogViewer : TAction;
    actAddZMQNodeLocalHost    : TAction;
    actCloseSubscriber        : TAction;
    actInspectTreeview        : TAction;
    actSubscribeToList        : TAction;
    btnAddZeroMQNode          : TButton;
    btnAddZMQNodeForLogViewer : TButton;
    btnAddZMQNodeLocalHost    : TButton;
    btnSubscribeToList        : TButton;
    edtAddress                : TLabeledEdit;
    edtPort                   : TLabeledEdit;
    imlMain                   : TImageList;
    mmoZMQEndPoints           : TMemo;
    mniCloseSsubscriber       : TMenuItem;
    pgcMain                   : TPageControl;
    pnlCOMPortTitle           : TPanel;
    pnlLeft                   : TPanel;
    pnlLogChannels            : TPanel;
    pnlRight                  : TPanel;
    pnlWinIPCTitle            : TPanel;
    pnlWinODSTitle            : TPanel;
    pnlZeroMQTitle            : TPanel;
    ppmMain                   : TPopupMenu;
    splVertical               : TSplitter;
    tsCOMPort                 : TTabSheet;
    tsWinIPC                  : TTabSheet;
    tsWinODS                  : TTabSheet;
    tsZeroMQ                  : TTabSheet;
    {$ENDREGION}

    procedure actAddZeroMQNodeExecute(Sender: TObject);
    procedure actAddZMQNodeLocalHostExecute(Sender: TObject);
    procedure actInspectTreeviewExecute(Sender: TObject);
    procedure actAddZMQNodeForLogViewerExecute(Sender: TObject);
    procedure actSubscribeToListExecute(Sender: TObject);

    procedure edtAddressExit(Sender: TObject);
    procedure actCloseSubscriberExecute(Sender: TObject);

  private
    FManager             : ILogViewerManager;
    FTreeView            : TVirtualStringTree;
    FZeroMQ              : IZeroMQ;
    FZeroMQReceiver      : IChannelReceiver;
    FWinIPCReceiver      : IChannelReceiver;
    FComPortReceiver     : IChannelReceiver;
    FWinODSReceiver      : IChannelReceiver;
    FZeroMQNode          : TDashboardNode;
    FWinIPCNode          : TDashboardNode;
    FComPortNode         : TDashboardNode;
    FWinODSNode          : TDashboardNode;
    FComPortSettingsForm : TfrmComPortSettings;

    procedure FTreeViewFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FTreeViewInitChildren(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      var ChildCount : Cardinal
    );
    procedure FTreeViewGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FTreeViewBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );
    procedure FTreeViewDblClick(Sender: TObject);
    procedure FTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FTreeViewFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FTreeViewGetImageIndex(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      Kind           : TVTImageKind;
      Column         : TColumnIndex;
      var Ghosted    : Boolean;
      var ImageIndex : TImageIndex
    );

    procedure FWinIPCReceiverSubscriberListChanged(
      Sender     : TObject;
      const AKey : UInt32;
      Action     : TCollectionChangedAction
    );
    procedure FZeroMQReceiverSubscriberListChanged(
      Sender     : TObject;
      const AKey : UInt32;
      Action     : TCollectionChangedAction
    );
    procedure FWinODSReceiverSubscriberListChanged(
      Sender     : TObject;
      const AKey : UInt32;
      Action     : TCollectionChangedAction
    );
    procedure FComPortReceiverSubscriberListChanged(
      Sender     : TObject;
      const AKey : UInt32;
      Action     : TCollectionChangedAction
    );

  protected
    procedure InitializeTreeView;
    procedure InitializeControls;
    procedure CreateChannelReceivers;
    procedure UpdateActions; override;

    procedure AddNodesToTree(AParent: PVirtualNode; ANode: TDashboardNode);

  public
    constructor Create(
      AOwner   : TComponent;
      AManager : ILogViewerManager
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring,

  DDuce.Utils, DDuce.Utils.Winapi, DDuce.Logger,
  DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,
  DDuce.ObjectInspector.zObjectInspector,

  LogViewer.Manager, LogViewer.Factories, LogViewer.Resources,
  LogViewer.Subscribers.ZeroMQ;

{$REGION 'construction and destruction'}
constructor TfrmDashboard.Create(AOwner: TComponent;
  AManager: ILogViewerManager);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
end;

procedure TfrmDashboard.AfterConstruction;
begin
  inherited AfterConstruction;
  FTreeView := TVirtualStringTreeFactory.CreateTreeList(Self, pnlRight);
  FTreeView.AlignWithMargins := False;
  InitializeTreeView;
  InitializeControls;
  CreateChannelReceivers;
end;

procedure TfrmDashboard.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FZeroMQReceiver  := nil;
  FWinIPCReceiver  := nil;
  FWinODSReceiver  := nil;
  FComPortReceiver := nil;
  FManager         := nil;
  FZeroMQ          := nil;
  FTreeView.Clear;
  FTreeView.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDashboard.actAddZeroMQNodeExecute(Sender: TObject);
var
  S : ISubscriber;
begin
  S := TZMQSubscriber.Create(
    FZeroMQReceiver,
    FZeroMQ,
    Format('tcp://%s:%s', [edtAddress.Text, edtPort.Text]),
    Integer(S),
    '',
    'Source',
    FZeroMQReceiver.Enabled
  );
  FZeroMQReceiver.SubscriberList.Add(0, S);
end;

procedure TfrmDashboard.actAddZMQNodeForLogViewerExecute(Sender: TObject);
var
  SL : TStringList;
  S  : ISubscriber;
begin
  SL := TStringList.Create;
  try
    GetIPAddresses(SL);
    if SL.Count > 0 then
    begin
      S := TZMQSubscriber.Create(
        FZeroMQReceiver,
        FZeroMQ,
        Format('tcp://%s:%s', [SL[0], IntToStr(LOGVIEWER_ZMQ_PORT)]),
        GetCurrentProcessId,
        '',
        Application.ExeName,
        FZeroMQReceiver.Enabled
      );
      Logger.Channels.Clear;
      FZeroMQReceiver.SubscriberList.Add(0, S);
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmDashboard.actAddZMQNodeLocalHostExecute(Sender: TObject);
var
  SL : TStringList;
  S  : ISubscriber;
begin
  SL := TStringList.Create;
  try
    GetIPAddresses(SL);
    if SL.Count > 0 then
    begin
      S := TZMQSubscriber.Create(
        FZeroMQReceiver,
        FZeroMQ,
        Format('tcp://%s:%s', [SL[0], '5555']),
        0,
        '',
        'localhost source',
        FZeroMQReceiver.Enabled
      );
      FZeroMQReceiver.SubscriberList.Add(0, S);
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmDashboard.actCloseSubscriberExecute(Sender: TObject);
begin
//
end;

procedure TfrmDashboard.actInspectTreeviewExecute(Sender: TObject);
begin
  InspectComponent(FTreeView);
end;

procedure TfrmDashboard.actSubscribeToListExecute(Sender: TObject);
var
  S    : string;
  LSub : ISubscriber;
begin
  for S in mmoZMQEndPoints.Lines do
  begin
    LSub := TZMQSubscriber.Create(
      FZeroMQReceiver,
      FZeroMQ,
      S,
      0,
      '',
      '',
      FZeroMQReceiver.Enabled
    );
    LSub.Enabled := True;
    FZeroMQReceiver.SubscriberList.Add(0, LSub);
  end;
  FManager.Settings.ZeroMQSettings.Subscriptions.Assign(mmoZMQEndPoints.Lines);
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'FTreeView'}
procedure TfrmDashboard.FTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  DN : TDashboardNode;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    if DN.Receiver.Enabled then
      Node.CheckState := csCheckedNormal
    else
      Node.CheckState := csUncheckedNormal;
  end;
end;

procedure TfrmDashboard.FTreeViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  DN : TDashboardNode;
  B  : Boolean;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    B := Node.CheckState = csCheckedNormal;
    if Supports(DN.Receiver, IWinIPC) then
    begin
      FManager.Settings.WinIPCSettings.Enabled := B;
    end
    else if Supports(DN.Receiver, IZMQ) then
    begin
      FManager.Settings.ZeroMQSettings.Enabled := B;
    end
    else if Supports(DN.Receiver, IWinODS) then
    begin
      FManager.Settings.WinODSSettings.Enabled := B;
    end
    else if Supports(DN.Receiver, IComPort) then
    begin
      //FManager.Settings.ComPortSettings.
    end;
  end
  else
  begin
    DN.Subscriber.Enabled := Node.CheckState = csCheckedNormal;
  end;
end;

procedure TfrmDashboard.FTreeViewDblClick(Sender: TObject);
var
  DN : TDashboardNode;
  V  : ILogViewer;
begin
  DN := FTreeView.GetNodeData<TDashboardNode>(FTreeView.FocusedNode);
  for V in FManager.Views do
  begin
    if V.Subscriber = DN.Subscriber then
    begin
      FManager.ActiveView := V;
    end;
  end;
end;

procedure TfrmDashboard.FTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  DN : TDashboardNode;
begin
  if not Assigned(Node) then
  begin
    Exit;
  end;
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    DN := Sender.GetNodeData<TDashboardNode>(Node);
  end
  else
    DN := Sender.GetNodeData<TDashboardNode>(Node.Parent);
  if Supports(DN.Receiver, IWinIPC) then
  begin
    pgcMain.ActivePage := tsWinIPC;
  end
  else if Supports(DN.Receiver, IZMQ) then
  begin
    pgcMain.ActivePage := tsZeroMQ;
  end
  else if Supports(DN.Receiver, IWinODS) then
  begin
    pgcMain.ActivePage := tsWinODS;
  end
  else if Supports(DN.Receiver, IComPort) then
  begin
    pgcMain.ActivePage := tsCOMPort;
  end;
end;

procedure TfrmDashboard.FTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  DN : TDashboardNode;
begin
  Logger.Track(Self, 'FTreeViewFreeNode');
  if not Assigned(Node) then
    Exit;

  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Assigned(DN) then
  begin
    Logger.SendObject('DN', DN);
    DN.Free;
  end;
end;

procedure TfrmDashboard.FTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  DN : TDashboardNode;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if (Sender.GetNodeLevel(Node) = 0) and (Kind in [ikNormal, ikSelected]) then
  begin
    if DN.Receiver = FZeroMQReceiver then
    begin
      ImageIndex := 1
    end
    else if DN.Receiver = FComPortReceiver then
    begin
      ImageIndex := 2
    end
    else
    begin
      ImageIndex := 0
    end;
  end;
end;

procedure TfrmDashboard.FTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  DN : TDashboardNode;
begin
  CellText := '';
  if not Assigned(Node) then
   Exit;

  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Assigned(DN) then
  begin
    if Sender.GetNodeLevel(Node) = 0 then
    begin
      if Column =  0 then
        CellText := DN.Caption
      else
        CellText := '';
    end
    else
    begin
      if Assigned(DN.Subscriber) then
      begin
        if Column =  0 then
          CellText := DN.Caption
        else if Column =  1 then
          CellText := DN.Subscriber.Key
        else if Column =  2 then
          CellText := DN.Subscriber.SourceId.ToString
        else if Column =  3 then
          CellText := DN.Subscriber.MessageCount.ToString;
      end
    end;
  end;
end;

procedure TfrmDashboard.FTreeViewInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  DN : TDashboardNode;
  N  : TDashboardNode;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  ChildCount := DN.Count;
  if ChildCount > 0 then
  begin
    for N in DN.Nodes.Values do
    begin
      N.VTNode := Sender.AddChild(Node, N);
    end;
  end;
end;

procedure TfrmDashboard.FWinIPCReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete : TDashboardNode;
begin
  if Action = caRemoved then
  begin
    LDelete := FWinIPCNode.Nodes.GetValueOrDefault(AKey);
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VTNode);
      FWinIPCNode.Nodes.Remove(AKey);
    end;
  end;
end;

procedure TfrmDashboard.FWinODSReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete : TDashboardNode;
begin
  if Action = caRemoved then
  begin
    LDelete := FWinODSNode.Nodes.GetValueOrDefault(AKey);
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VTNode);
      FWinODSNode.Nodes.Remove(AKey);
    end;
  end;
end;

procedure TfrmDashboard.FZeroMQReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete : TDashboardNode;
begin
  if Action = caRemoved then
  begin
    LDelete := FZeroMQNode.Nodes.GetValueOrDefault(AKey);
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VTNode);
      FZeroMQNode.Nodes.Remove(AKey);
    end;
  end;
end;

procedure TfrmDashboard.FComPortReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete : TDashboardNode;
begin
  if Action = caRemoved then
  begin
    LDelete := FComPortNode.Nodes.GetValueOrDefault(AKey);
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VTNode);
      FComPortNode.Nodes.Remove(AKey);
    end;
  end;
end;
{$ENDREGION}

procedure TfrmDashboard.edtAddressExit(Sender: TObject);
begin
  edtAddress.Hint := GetIP(edtAddress.Text);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDashboard.AddNodesToTree(AParent: PVirtualNode;
  ANode: TDashboardNode);
var
  LSubNode : TDashboardNode;
  LVTNode  : PVirtualNode;
begin
  LVTNode := FTreeView.AddChild(AParent, ANode);
  ANode.VTNode := LVTNode;
  for LSubNode in ANode.Nodes.Values do
    AddNodesToTree(LVTNode, LSubNode);
end;

procedure TfrmDashboard.CreateChannelReceivers;
begin
  FZeroMQ := TZeroMQ.Create;
  FWinIPCReceiver := TLogViewerFactories.CreateWinIPCReceiver(FManager);
  FManager.AddReceiver(FWinIPCReceiver);
  FWinIPCReceiver.SubscriberList.OnKeyChanged.Add(FWinIPCReceiverSubscriberListChanged);
  FWinIPCReceiver.Enabled := FManager.Settings.WinIPCSettings.Enabled;
  FWinIPCNode := TDashboardNode.Create(nil, FTreeView, FWinIPCReceiver, nil);
  AddNodesToTree(FTreeView.RootNode, FWinIPCNode);
  FWinIPCNode.VTNode.CheckType := ctCheckBox;
  if FWinIPCReceiver.Enabled then
    FWinIPCNode.VTNode.CheckState := csCheckedNormal
  else
    FWinIPCNode.VTNode.CheckState := csUncheckedNormal;

  FWinODSReceiver := TLogViewerFactories.CreateWinODSReceiver(FManager);
  FManager.AddReceiver(FWinODSReceiver);
  FWinODSReceiver.SubscriberList.OnKeyChanged.Add(FWinODSReceiverSubscriberListChanged);
  FWinODSReceiver.Enabled := FManager.Settings.WinODSSettings.Enabled;
  FWinODSNode := TDashboardNode.Create(nil, FTreeView, FWinODSReceiver, nil);
  AddNodesToTree(FTreeView.RootNode, FWinODSNode);
  FWinODSNode.VTNode.CheckType := ctCheckBox;
  if FWinODSReceiver.Enabled then
    FWinODSNode.VTNode.CheckState := csCheckedNormal
  else
    FWinODSNode.VTNode.CheckState := csUncheckedNormal;

  FZeroMQReceiver := TLogViewerFactories.CreateZeroMQReceiver(FManager, FZeroMQ);
  FManager.AddReceiver(FZeroMQReceiver);
  FZeroMQReceiver.SubscriberList.OnKeyChanged.Add(FZeroMQReceiverSubscriberListChanged);
  FZeroMQReceiver.Enabled := FManager.Settings.ZeroMQSettings.Enabled;
  FZeroMQNode := TDashboardNode.Create(nil, FTreeView, FZeroMQReceiver, nil);
  AddNodesToTree(FTreeView.RootNode, FZeroMQNode);
  FZeroMQNode.VTNode.CheckType := ctCheckBox;
  if FZeroMQReceiver.Enabled then
    FZeroMQNode.VTNode.CheckState := csCheckedNormal
  else
    FZeroMQNode.VTNode.CheckState := csUncheckedNormal;

  FComPortReceiver := TLogViewerFactories.CreateComPortReceiver(
    FManager,FManager.Settings.ComPortSettings
  );
  FManager.AddReceiver(FComPortReceiver);
  FComPortReceiver.SubscriberList.OnKeyChanged.Add(FComPortReceiverSubscriberListChanged);
  //FComPortReceiver.Enabled := FManager.Settings.ComPortSettings.Enabled;
  FComPortNode := TDashboardNode.Create(nil, FTreeView, FComPortReceiver, nil);
  AddNodesToTree(FTreeView.RootNode, FComPortNode);
  FComPortNode.VTNode.CheckType := ctCheckBox;
  if FComPortReceiver.Enabled then
    FComPortNode.VTNode.CheckState := csCheckedNormal
  else
    FComPortNode.VTNode.CheckState := csUncheckedNormal;
  FComPortReceiver.Enabled := True;
end;

procedure TfrmDashboard.InitializeControls;
var
  I : Integer;
begin
  for I := 0 to pgcMain.PageCount - 1 do
  begin
    pgcMain.Pages[I].TabVisible := False;
  end;
  FComPortSettingsForm := TfrmComPortSettings.Create(
    Self, FManager.Settings.ComPortSettings
  );
  AssignFormParent(FComPortSettingsForm, tsCOMPort);
  pgcMain.ActivePage := tsWinIPC;
end;

procedure TfrmDashboard.InitializeTreeView;
begin
  FTreeView.OnBeforeCellPaint := FTreeViewBeforeCellPaint;
  FTreeView.OnFreeNode        := FTreeViewFreeNode;
  FTreeView.OnInitChildren    := FTreeViewInitChildren;
  FTreeView.OnGetText         := FTreeViewGetText;
  FTreeView.OnChecked         := FTreeViewChecked;
  FTreeView.OnFocusChanged    := FTreeViewFocusChanged;
  FTreeView.OnDblClick        := FTreeViewDblClick;
  FTreeView.OnGetImageIndex   := FTreeViewGetImageIndex;
  FTreeView.DefaultNodeHeight := 30;
  FTreeView.Images            := imlMain;

  with FTreeView do
  begin
    with Header.Columns.Add do
    begin
      Color    := clWhite;
      MaxWidth := 400;
      MinWidth := 200;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus,
        coEditable];
      Position := 0;
      Indent   := 8;
      Width    := 300;
      Text := 'Name';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 800;
      MinWidth := 200;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 1;
      Width    := 100;
      Text := 'Value';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 800;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 2;
      Width    := 100;
      Text := 'Id';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 800;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
        coSmartResize, coAllowFocus, coEditable];
      Position := 3;
      Width    := 100;
      Text := 'Messagecount';
    end;
    Header.MainColumn := 0;
    TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoSpanColumns];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowTreeLines];
  end;
  FTreeView.Indent            := 30;
end;

procedure TfrmDashboard.UpdateActions;
begin
//  FManager.Actions.UpdateActions;  // optimize for performance
  inherited UpdateActions;
end;
{$ENDREGION}

end.
