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
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.ButtonGroup,

  VirtualTrees,

  LogViewer.Interfaces, LogViewer.Dashboard.View.Node, Vcl.ComCtrls;

  {
    TODO:
      - indicate amount of data received per channel
      - editable treeview in which we can add nodes for every channel we want to
        subscribe to.
  }

type
  TfrmDashboard = class(TForm)
    {$REGION 'designer controls'}
    aclMain          : TActionList;
    pnlLogChannels   : TPanel;
    pnlLeft          : TPanel;
    splVertical      : TSplitter;
    pnlRight         : TPanel;
    pgcMain          : TPageControl;
    tsWinIPC         : TTabSheet;
    tsWinODS         : TTabSheet;
    tsZeroMQ         : TTabSheet;
    tsCOMPort        : TTabSheet;
    edtAddress       : TLabeledEdit;
    edtPort          : TLabeledEdit;
    actAddZeroMQNode : TAction;
    btnAddZeroMQNode : TButton;
    {$ENDREGION}

    procedure actAddZeroMQNodeExecute(Sender: TObject);

  private
    FManager        : ILogViewerManager;
    FTreeView       : TVirtualStringTree;
    FZeroMQReceiver : IChannelReceiver;

    procedure FTreeViewInitNode(
      Sender            : TBaseVirtualTree;
      ParentNode,
      Node              : PVirtualNode;
      var InitialStates : TVirtualNodeInitStates
    );
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

    procedure FChannelReceiverNewLogQueue(
      Sender    : TObject;
      ALogQueue : ILogQueue
    );

  protected
    procedure InitializeTreeView;
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
  Spring, Spring.Collections,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,
  DDuce.ObjectInspector.zObjectInspector, DDuce.DynamicRecord,

  LogViewer.Manager, LogViewer.Receivers.ZeroMQ.Subscriber, LogViewer.Factories;

{$REGION 'construction and destruction'}
constructor TfrmDashboard.Create(AOwner: TComponent;
  AManager: ILogViewerManager);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  FManager := AManager;
end;

procedure TfrmDashboard.AfterConstruction;
var
  R : IChannelReceiver;
begin
  inherited AfterConstruction;
  FTreeView := TVirtualStringTreeFactory.CreateTreeList(Self, pnlLeft);
  FTreeView.AlignWithMargins := False;
  InitializeTreeView;
//  InspectComponent(FTreeView);
end;

procedure TfrmDashboard.BeforeDestruction;
begin
  FTreeView.Clear;
  FTreeView.Free;
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDashboard.actAddZeroMQNodeExecute(Sender: TObject);
var
  //DN : TDashboardNode;
  R : DynamicRecord;
begin
  R['Address'] := edtAddress.Text;
  R['Port']    := edtPort.Text;
  FZeroMQReceiver.AddSubscriber(R);


  //DN := TDashboardNode.Create(nil, FTreeView, FZMQReceiver, ALogQueue);
//  DN.VTNode := FTreeView.AddChild(FVTNode, DN);
//   Nodes.Add(DN);
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
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    DN.Receiver.Enabled := Node.CheckState = csCheckedNormal;
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
    if V.LogQueue = DN.LogQueue then
    begin
      FManager.ActiveView := V;
    end;
  end;

  //ShowMessage(DN.Subscriber.Address);
end;

procedure TfrmDashboard.FTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  DN : TDashboardNode;
begin

  if Sender.GetNodeLevel(Node) = 0 then
  begin
    DN := Sender.GetNodeData<TDashboardNode>(Node);
    if DN.Receiver.Name = 'WinIPCChannelReceiver' then
    begin
      pgcMain.ActivePage := tsWinIPC;
    end
    else if DN.Receiver.Name = 'ZeroMQChannelReceiver' then
    begin
      pgcMain.ActivePage := tsZeroMQ;
    end;
  end;
end;

procedure TfrmDashboard.FTreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  DN : TDashboardNode;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  DN.Free;
end;

procedure TfrmDashboard.FTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  DN : TDashboardNode;
begin
  CellText := '';
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    if Column =  0 then
      CellText := DN.Receiver.Name
    else
      CellText := '';
  end
  else
  begin
    if Assigned(DN.LogQueue) then
    begin
      if Column =  1 then
        CellText := DN.LogQueue.SourceName
      else if Column =  2 then
        CellText := DN.LogQueue.SourceId.ToString
      else if Column =  3 then
        CellText := DN.LogQueue.MessageCount.ToString;
    end
    else if Assigned(DN.Subscriber) then
    begin
      if Column =  1 then
        CellText := DN.Subscriber.Address
      else if Column =  2 then
        CellText := DN.Subscriber.Port

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
    for N in DN.Nodes do
    begin
      N.VTNode := Sender.AddChild(Node, N);
    end;
  end;
end;

procedure TfrmDashboard.FTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
//var
//  DN : TDashboardNode;
begin
//  DN := Sender.GetNodeData<TDashboardNode>(Node);
//  DN.Free;
//
//  if ParentNode = nil then
//    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
//
//  Node.SetData<TDashboardNode>(
//    TDashboardNode.Create(Node, FTreeView, FManager.Receivers[Node.Index])
//  );
//  if Sender.GetNodeLevel(Node) = 0 then
//  begin
//    Node.CheckType := ctCheckBox;
//    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
//  end;
end;
{$ENDREGION}

procedure TfrmDashboard.FChannelReceiverNewLogQueue(Sender: TObject;
  ALogQueue: ILogQueue);
begin
//  FTreeView.Refresh;
//  FTreeView.ReinitNode(FTreeView.RootNode, True);

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
  for LSubNode in ANode.Nodes do
    AddNodesToTree(LVTNode, LSubNode);
end;

procedure TfrmDashboard.InitializeTreeView;
var
  LNode : TDashboardNode;
  R     : IChannelReceiver;
begin
  FTreeView.OnBeforeCellPaint := FTreeViewBeforeCellPaint;
  FTreeView.OnInitNode        := FTreeViewInitNode;
  FTreeView.OnFreeNode        := FTreeViewFreeNode;
  FTreeView.OnInitChildren    := FTreeViewInitChildren;
  FTreeView.OnGetText         := FTreeViewGetText;
  FTreeView.OnChecked         := FTreeViewChecked;
  FTreeView.OnFocusChanged    := FTreeViewFocusChanged;
  FTreeView.OnDblClick        := FTreeViewDblClick;
  with FTreeView do
  begin
    with Header.Columns.Add do
    begin
      Color    := clWhite;
      MaxWidth := 200;
      MinWidth := 100;
      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
        coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus,
        coEditable];
      Position := 0;
      Indent   := 8;
      Width    := 200;
      Text := 'Name';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 800;
      MinWidth := 100;
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
  end;

  R := TLogViewerFactories.CreateWinIPCChannelReceiver(FManager);
  FManager.AddReceiver(R);
  //R.OnNewLogQueue.Add(FChannelReceiverNewLogQueue);
  R.Enabled := FManager.Settings.WinIPCSettings.Enabled;
  LNode := TDashboardNode.Create(nil, FTreeView, R, nil);
  AddNodesToTree(FTreeView.RootNode, LNode);
  LNode.VTNode.CheckType := ctCheckBox;
  if R.Enabled then
    LNode.VTNode.CheckState := csCheckedNormal
  else
    LNode.VTNode.CheckState := csUncheckedNormal;



//  R := TLogViewerFactories.CreateWinODSChannelReceiver(FManager);
//  FManager.AddReceiver(R);
//  R.Enabled := FManager.Settings.WinODSSettings.Enabled;

  R := TLogViewerFactories.CreateZeroMQChannelReceiver(FManager);
  FManager.AddReceiver(R);
  //R.OnNewLogQueue.Add(FChannelReceiverNewLogQueue);
  R.Enabled := FManager.Settings.ZeroMQSettings.Enabled;
  LNode := TDashboardNode.Create(nil, FTreeView, R, nil);
  AddNodesToTree(FTreeView.RootNode, LNode);
  LNode.VTNode.CheckType := ctCheckBox;
  FZeroMQReceiver := R;
  if R.Enabled then
    LNode.VTNode.CheckState := csCheckedNormal
  else
    LNode.VTNode.CheckState := csUncheckedNormal;



  //FTreeView.RootNodeCount := FManager.Receivers.Count;
end;

procedure TfrmDashboard.UpdateActions;
begin
  FManager.Actions.UpdateActions;
  FTreeView.Invalidate;
  inherited UpdateActions;
end;
{$ENDREGION}

end.


