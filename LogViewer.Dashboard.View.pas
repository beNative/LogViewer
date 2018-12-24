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

{ Provides an overview of all active channels and statistics. }

interface

{$REGION 'documentation'}
{ This form provides an overview of all supported channel receivers and allows
  to configure and activate user defined subscriptions.

TODO:
    - indicate amount of data received per channel
    - editable treeview in which we can add nodes for every channel we want to
      subscribe to.
}
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.UITypes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ImgList, Vcl.Menus, Vcl.Grids, Vcl.ToolWin,

  Spring.Collections,

  VirtualTrees,

  ZeroMQ, MQTT,

  synaser,

  DDuce.Components.VirtualTrees.Node, DDuce.Components.ValueList,
  DDuce.Components.Factories, DDuce.DynamicRecord,

  LogViewer.Interfaces, LogViewer.Dashboard.Data,
  LogViewer.ComPort.Settings.View;

type
  TDashboardNode = TVTNode<TDashboardData>;

type
  TfrmDashboard = class(TForm)
    {$REGION 'designer controls'}
    aclMain                    : TActionList;
    actAddEndpoint             : TAction;
    actAddSubscribeToLogViewer : TAction;
    actCloseSubscriber         : TAction;
    actCopyEndpoint            : TAction;
    actDeleteEndpoint          : TAction;
    actInspectTreeview         : TAction;
    actMoveDownEndpoint        : TAction;
    actMoveUpEndpoint          : TAction;
    actSubscribeToLocalHost    : TAction;
    actSubscribeToSelection    : TAction;
    btn1                       : TToolButton;
    btnAdd                     : TToolButton;
    btnAddZMQNodeForLogViewer  : TButton;
    btnAddZMQNodeLocalHost     : TButton;
    btnDelete                  : TToolButton;
    btnDuplicate               : TToolButton;
    btnMoveDown                : TToolButton;
    btnMoveUp                  : TToolButton;
    btnSpacer1                 : TToolButton;
    btnSpacer2                 : TToolButton;
    btnSubscribeToSelection    : TToolButton;
    imlMain                    : TImageList;
    lblWinIPCDescription       : TLabel;
    lblWinODSDescription       : TLabel;
    mniAddEndpoint             : TMenuItem;
    mniCloseSsubscriber        : TMenuItem;
    mniCopyEndpoint            : TMenuItem;
    mniDeleteEndpoint          : TMenuItem;
    mniMoveDownEndpoint        : TMenuItem;
    mniMoveUpEndpoint          : TMenuItem;
    mniN1                      : TMenuItem;
    mniN2                      : TMenuItem;
    mniN3                      : TMenuItem;
    mniSubscribeToSelection    : TMenuItem;
    pgcMain                    : TPageControl;
    pnlCOMPortTitle            : TPanel;
    pnlLeft                    : TPanel;
    pnlLogChannels             : TPanel;
    pnlRight                   : TPanel;
    pnlWinIPCTitle             : TPanel;
    pnlWinODSTitle             : TPanel;
    pnlZeroMQButtons           : TGridPanel;
    pnlZeroMQTitle             : TPanel;
    pnlZMQEndpoints            : TPanel;
    ppmEndpoints               : TPopupMenu;
    ppmMain                    : TPopupMenu;
    splVertical                : TSplitter;
    tlbZMQEndpoints            : TToolBar;
    tsCOMPort                  : TTabSheet;
    tsWinIPC                   : TTabSheet;
    tsWinODS                   : TTabSheet;
    tsZeroMQ                   : TTabSheet;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actSubscribeToLocalHostExecute(Sender: TObject);
    procedure actInspectTreeviewExecute(Sender: TObject);
    procedure actAddSubscribeToLogViewerExecute(Sender: TObject);
    procedure actSubscribeToSelectionExecute(Sender: TObject);
    procedure actCloseSubscriberExecute(Sender: TObject);
    procedure actAddEndpointExecute(Sender: TObject);
    procedure actDeleteEndpointExecute(Sender: TObject);
    procedure actCopyEndpointExecute(Sender: TObject);
    procedure actMoveDownEndpointExecute(Sender: TObject);
    procedure actMoveUpEndpointExecute(Sender: TObject);
    {$ENDREGION}

  private
    FUpdate              : Boolean;
    FManager             : ILogViewerManager;
    FTreeView            : TVirtualStringTree;
    FValueList           : TValueList;
    FZeroMQ              : IZeroMQ;
    FMQTT                : TMQTT;
    FZeroMQReceiver      : IChannelReceiver;
    FMQTTReceiver        : IChannelReceiver;
    FWinIPCReceiver      : IChannelReceiver;
    FComPortReceiver     : IChannelReceiver;
    FWinODSReceiver      : IChannelReceiver;
    FFileSystemReceiver  : IChannelReceiver;
    FZeroMQNode          : TDashboardNode;
    FMQTTNode            : TDashboardNode;
    FWinIPCNode          : TDashboardNode;
    FComPortNode         : TDashboardNode;
    FWinODSNode          : TDashboardNode;
    FFileSystemNode      : TDashboardNode;
    FComPortSettingsForm : TfrmComPortSettings;
    FZMQEndpoints        : DynamicRecord;

    {$REGION 'event handlers'}
    procedure FTreeViewFreeNode(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
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
    procedure FFileSystemReceiverSubscriberListChanged(
      Sender     : TObject;
      const AKey : UInt32;
      Action     : TCollectionChangedAction
    );
    procedure FReceiverChange(Sender : TObject);
    procedure FSubscriberChange(Sender : TObject);

    procedure FValueListExit(Sender : TObject);
    {$ENDREGION}

  protected
    procedure Modified;
    procedure InitializeTreeView;
    procedure InitializeControls;
    procedure CreateChannelReceivers;

    function AddNode(
      AParentNode : TDashboardNode;
      AReceiver   : IChannelReceiver;
      ASubscriber : ISubscriber
    ): TDashboardNode;

    function CanMoveUp: Boolean;
    function CanMoveDown: Boolean;
    procedure SaveEndpoints;
    procedure UpdateActions; override;

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
  FValueList             := TValueList.Create(Self);
  FValueList.Parent      := pnlZMQEndpoints;
  FValueList.Align       := alClient;
  FValueList.ShowGutter  := False;
  FValueList.MultiSelect := True;
  FValueList.OnExit      := FValueListExit;

  InitializeTreeView;
  InitializeControls;
  CreateChannelReceivers;
end;

procedure TfrmDashboard.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FZeroMQReceiver  := nil;
  FMQTTReceiver    := nil;
  FWinIPCReceiver  := nil;
  FWinODSReceiver  := nil;
  FComPortReceiver := nil;
  FManager         := nil;
  FZeroMQ          := nil;
  FMQTT.Free;
  FTreeView.Clear;
  FTreeView.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDashboard.actAddEndpointExecute(Sender: TObject);
begin
  FValueList.Data['New'] := 'tcp://';
  FValueList.Repaint;
end;

procedure TfrmDashboard.actAddSubscribeToLogViewerExecute(Sender: TObject);
var
  SL          : TStringList;
  LSubscriber : ISubscriber;
  LEndPoint   : string;
  LName       : string;
begin
  SL := TStringList.Create;
  try
    GetIPAddresses(SL);
    if SL.Count > 0 then
    begin
      LEndPoint := Format('tcp://%s:%s', [SL[0], IntToStr(LOGVIEWER_ZMQ_PORT)]);
      LName     := Application.ExeName;
      LSubscriber := TZMQSubscriber.Create(
        FZeroMQReceiver,
        FZeroMQ,
        LEndPoint,
        GetCurrentProcessId,
        LEndPoint,
        LName,
        FZeroMQReceiver.Enabled
      );
      Logger.Channels.Clear;
      FZeroMQReceiver.SubscriberList.Add(LSubscriber.SourceId, LSubscriber);
      Modified;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmDashboard.actSubscribeToLocalHostExecute(Sender: TObject);
var
  SL          : TStringList;
  LSubscriber : ISubscriber;
  LEndPoint   : string;
  LName       : string;
begin
  SL := TStringList.Create;
  try
    GetIPAddresses(SL);
    if SL.Count > 0 then
    begin
      LEndPoint   := Format('tcp://%s:%s', [SL[0], '5555']);
      LName       := 'localhost';
      LSubscriber := TZMQSubscriber.Create(
        FZeroMQReceiver,
        FZeroMQ,
        LEndPoint,
        0,
        LEndPoint,
        LName,
        FZeroMQReceiver.Enabled
      );
      FZeroMQReceiver.SubscriberList.Add(LSubscriber.SourceId, LSubscriber);
      Modified;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmDashboard.actCloseSubscriberExecute(Sender: TObject);
begin
//
end;

procedure TfrmDashboard.actCopyEndpointExecute(Sender: TObject);
begin
//
end;

procedure TfrmDashboard.actDeleteEndpointExecute(Sender: TObject);
begin
  FValueList.DeleteSelectedNodes;
end;

procedure TfrmDashboard.actInspectTreeviewExecute(Sender: TObject);
begin
  InspectComponent(FTreeView);
end;

procedure TfrmDashboard.actMoveDownEndpointExecute(Sender: TObject);
var
  LNode : TValueListNode;
begin
  if Assigned(FValueList.FocusedField) then
  begin
    LNode := FValueList.GetFirstSelectedNodeData<TValueListNode>;
    LNode.Data.Index := LNode.Data.Index + 1;
    FValueList.MoveTo(LNode.VNode, LNode.VNode.NextSibling, amInsertAfter, False);
  end;
end;

procedure TfrmDashboard.actMoveUpEndpointExecute(Sender: TObject);
var
  LNode : TValueListNode;
begin
  if Assigned(FValueList.FocusedField) then
  begin
    LNode := FValueList.GetFirstSelectedNodeData<TValueListNode>;
    LNode.Data.Index := LNode.Data.Index - 1;
    FValueList.MoveTo(LNode.VNode, LNode.VNode.PrevSibling, amInsertBefore, False);
  end;
end;

procedure TfrmDashboard.actSubscribeToSelectionExecute(Sender: TObject);
var
  LSubscriber : ISubscriber;
  LEndPoint   : string;
  LName       : string;
  LNode       : TValueListNode;
begin
  FZeroMQReceiver.SubscriberList.Clear;
  for LNode in FValueList.GetSelectedData<TValueListNode> do
  begin
    //FZeroMQReceiver.SubscriberList.ContainsKey()
    LEndPoint := LNode.Data.Value.AsString;
    LName     := LNode.Data.Name;
    LSubscriber := TZMQSubscriber.Create(
      FZeroMQReceiver,
      FZeroMQ,
      LEndPoint,
      0,
      LEndPoint,
      LName,
      FZeroMQReceiver.Enabled
    );
    LSubscriber.Enabled := True;
    FZeroMQReceiver.SubscriberList.Add(LSubscriber.SourceId, LSubscriber);
    Modified;
  end;
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
    if DN.Data.Receiver.Enabled then
      Node.CheckState := csCheckedNormal
    else
      Node.CheckState := csUncheckedNormal;
  end
  else if Sender.GetNodeLevel(Node) = 1 then
  begin
    if DN.Data.Subscriber.Enabled then
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
  if Assigned(DN) then
  begin
    if Sender.GetNodeLevel(Node) = 0 then
    begin
      B := Node.CheckState = csCheckedNormal;
      DN.Data.Receiver.Enabled := B;
      if Supports(DN.Data.Receiver, IWinIPC) then
      begin
        FManager.Settings.WinIPCSettings.Enabled := B;
      end
      else if Supports(DN.Data.Receiver, IZMQ) then
      begin
        FManager.Settings.ZeroMQSettings.Enabled := B;
      end
      else if Supports(DN.Data.Receiver, IWinODS) then
      begin
        FManager.Settings.WinODSSettings.Enabled := B;
      end
      else if Supports(DN.Data.Receiver, IComPort) then
      begin
        //
        //FManager.Settings.ComPortSettings.E
      end;
    end
    else
    begin
      DN.Data.Subscriber.Enabled := Node.CheckState = csCheckedNormal;
      if DN.Data.Subscriber.Enabled and
        not DN.Data.Subscriber.Receiver.Enabled then
      begin
        DN.Data.Subscriber.Receiver.Enabled := True;
        UpdateActions;
      end;
    end;
  end;
  Modified;
end;

procedure TfrmDashboard.FTreeViewDblClick(Sender: TObject);
var
  DN : TDashboardNode;
  V  : ILogViewer;
begin
  DN := FTreeView.GetNodeData<TDashboardNode>(FTreeView.FocusedNode);
  for V in FManager.Views do
  begin
    if V.Subscriber = DN.Data.Subscriber then
    begin
      FManager.ActiveView := V;
    end;
  end;
end;

procedure TfrmDashboard.FTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  DN        : TDashboardNode;
  LReceiver : IChannelReceiver;
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
  LReceiver := DN.Data.Receiver;
  if Supports(LReceiver, IWinIPC) then
  begin
    pgcMain.ActivePage := tsWinIPC;
  end
  else if Supports(LReceiver, IZMQ) then
  begin
    pgcMain.ActivePage := tsZeroMQ;
  end
  else if Supports(LReceiver, IWinODS) then
  begin
    pgcMain.ActivePage := tsWinODS;
  end
  else if Supports(LReceiver, IComPort) then
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
  DN        : TDashboardNode;
  LReceiver : IChannelReceiver;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  LReceiver := DN.Data.Receiver;
  if (Sender.GetNodeLevel(Node) = 0) and (Kind in [ikNormal, ikSelected]) then
  begin
    if LReceiver = FZeroMQReceiver then
    begin
      ImageIndex := 1
    end
    else if LReceiver = FComPortReceiver then
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
  DN          : TDashboardNode;
  LSubscriber : ISubscriber;
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
        CellText := DN.Data.Caption
      else
        CellText := '';
    end
    else
    begin
      if Assigned(DN.Data.Subscriber) then
      begin
        LSubscriber := DN.Data.Subscriber;
        if Column =  0 then
          CellText := LSubscriber.SourceName
        else if Column =  1 then
          CellText := LSubscriber.Key
        else if Column =  2 then
          CellText := LSubscriber.SourceId.ToString
        else if Column =  3 then
          CellText := LSubscriber.MessageCount.ToString;
      end
    end;
  end;
end;
{$ENDREGION}

procedure TfrmDashboard.FValueListExit(Sender: TObject);
begin
  SaveEndpoints;
end;

procedure TfrmDashboard.FWinIPCReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete     : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LDelete := nil;
  if Action = caRemoved then
  begin
    for LDelete in FWinIPCNode do
    begin
      if LDelete.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FWinIPCReceiver.SubscriberList[AKey];
    AddNode(FWinIPCNode, nil, LSubscriber);
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;

procedure TfrmDashboard.FWinODSReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete     : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LDelete := nil;
  if Action = caRemoved then
  begin
    for LDelete in FWinODSNode do
    begin
      if LDelete.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FWinODSReceiver.SubscriberList[AKey];
    AddNode(FWinODSNode, nil, LSubscriber);
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;

procedure TfrmDashboard.FZeroMQReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete     : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LDelete := nil;
  if Action = caRemoved then
  begin
    for LDelete in FZeroMQNode do
    begin
      if LDelete.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FZeroMQReceiver.SubscriberList[AKey];
    AddNode(FZeroMQNode, nil, LSubscriber);
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;

procedure TfrmDashboard.FComPortReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete     : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LDelete := nil;
  if Action = caRemoved then
  begin
    for LDelete in FComPortNode do
    begin
      if LDelete.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FComPortReceiver.SubscriberList[AKey];
    AddNode(FComPortNode, nil, LSubscriber);
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;

procedure TfrmDashboard.FFileSystemReceiverSubscriberListChanged(
  Sender: TObject; const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete     : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LDelete := nil;
  if Action = caRemoved then
  begin
    for LDelete in FFileSystemNode do
    begin
      if LDelete.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LDelete) then
    begin
      FTreeView.DeleteNode(LDelete.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FFileSystemReceiver.SubscriberList[AKey];
    AddNode(FFileSystemNode, nil, LSubscriber);
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;

procedure TfrmDashboard.FReceiverChange(Sender: TObject);
begin
  Modified;
end;

procedure TfrmDashboard.FSubscriberChange(Sender: TObject);
begin
  Modified;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TfrmDashboard.AddNode(AParentNode: TDashboardNode;
  AReceiver: IChannelReceiver; ASubscriber: ISubscriber): TDashboardNode;
begin
  if Assigned(AParentNode) then
    Logger.SendObject('AParentNode', AParentNode);
  if Assigned(AReceiver) then
    Logger.SendInterface('AReceiver', AReceiver);
  if Assigned(ASubscriber) then
    Logger.SendInterface('ASubscriber', ASubscriber);
  if Assigned(AParentNode) then
  begin
    Result := AParentNode.Add(TDashboardData.Create(AReceiver, ASubscriber));
    Result.CheckType  := ctCheckBox;
    Result.CheckState := csCheckedNormal;
  end
  else
  begin
    Result := TDashboardNode.Create(
      FTreeView,
      TDashboardData.Create(AReceiver, ASubscriber)
    );
  end;
  FTreeView.FullExpand;
end;

function TfrmDashboard.CanMoveDown: Boolean;
begin
  if Assigned(FValueList.FocusedNode) then
    Result := Assigned(FValueList.FocusedNode.NextSibling)
  else
    Result := False;
end;

function TfrmDashboard.CanMoveUp: Boolean;
begin
  if Assigned(FValueList.FocusedNode) then
    Result := Assigned(FValueList.FocusedNode.PrevSibling)
  else
    Result := False;
end;

procedure TfrmDashboard.CreateChannelReceivers;
begin
  FZeroMQ := TZeroMQ.Create;
  FWinIPCReceiver := TLogViewerFactories.CreateWinIPCReceiver(FManager);
  FManager.AddReceiver(FWinIPCReceiver);
  FWinIPCReceiver.OnChange.Add(FReceiverChange);
  FWinIPCReceiver.SubscriberList.OnKeyChanged.Add(FWinIPCReceiverSubscriberListChanged);
  FWinIPCReceiver.Enabled := FManager.Settings.WinIPCSettings.Enabled;
  FWinIPCNode := AddNode(nil, FWinIPCReceiver, nil);
  FWinIPCNode.CheckType := ctCheckBox;
  if FWinIPCReceiver.Enabled then
    FWinIPCNode.CheckState := csCheckedNormal
  else
    FWinIPCNode.CheckState := csUncheckedNormal;

  FWinODSReceiver := TLogViewerFactories.CreateWinODSReceiver(FManager);
  FManager.AddReceiver(FWinODSReceiver);
  FWinODSReceiver.OnChange.Add(FReceiverChange);
  FWinODSReceiver.SubscriberList.OnKeyChanged.Add(FWinODSReceiverSubscriberListChanged);
  FWinODSReceiver.Enabled := FManager.Settings.WinODSSettings.Enabled;
  FWinODSNode := AddNode(nil, FWinODSReceiver, nil);
  FWinODSNode.CheckType := ctCheckBox;
  if FWinODSReceiver.Enabled then
    FWinODSNode.CheckState := csCheckedNormal
  else
    FWinODSNode.CheckState := csUncheckedNormal;

  FZeroMQReceiver := TLogViewerFactories.CreateZeroMQReceiver(FManager, FZeroMQ);
  FManager.AddReceiver(FZeroMQReceiver);
  FZeroMQReceiver.OnChange.Add(FReceiverChange);
  FZeroMQReceiver.SubscriberList.OnKeyChanged.Add(FZeroMQReceiverSubscriberListChanged);
  FZeroMQReceiver.Enabled := FManager.Settings.ZeroMQSettings.Enabled;
  FZeroMQNode := AddNode(nil, FZeroMQReceiver, nil);
  FZeroMQNode.CheckType := ctCheckBox;
  if FZeroMQReceiver.Enabled then
    FZeroMQNode.CheckState := csCheckedNormal
  else
    FZeroMQNode.CheckState := csUncheckedNormal;

  FMQTT := TMQTT.Create('localhost', 1833);
  FMQTTReceiver := TLogViewerFactories.CreateMQTTReceiver(FManager, FMQTT);
  FManager.AddReceiver(FMQTTReceiver);
  FMQTTReceiver.OnChange.Add(FReceiverChange);
  FMQTTNode := AddNode(nil, FMQTTReceiver, nil);
  FMQTTNode.CheckType := ctCheckBox;
  if FMQTTReceiver.Enabled then
    FMQTTNode.CheckState := csCheckedNormal;

  FComPortReceiver := TLogViewerFactories.CreateComPortReceiver(
    FManager, FManager.Settings.ComPortSettings
  );
  FManager.AddReceiver(FComPortReceiver);
  FComPortReceiver.OnChange.Add(FReceiverChange);
  FComPortReceiver.SubscriberList.OnKeyChanged.Add(FComPortReceiverSubscriberListChanged);
//  FComPortReceiver.Enabled := FManager.Settings.ComPortSettings.Enabled;
  FComPortNode := AddNode(nil, FComPortReceiver, nil);
  FComPortNode.CheckType := ctCheckBox;
  if FComPortReceiver.Enabled then
    FComPortNode.CheckState := csCheckedNormal
  else
    FComPortNode.CheckState := csUncheckedNormal;
  FComPortReceiver.Enabled := False;

  FFileSystemReceiver := TLogViewerFactories.CreateFileSystemReceiver(
    FManager, ''
  );
  FFileSystemReceiver.SubscriberList.OnKeyChanged.Add(FFileSystemReceiverSubscriberListChanged);
  FFileSystemNode := AddNode(nil, FFileSystemReceiver, nil);
  FFileSystemReceiver.OnChange.Add(FReceiverChange);
  FFileSystemNode.CheckType := ctCheckBox;
  if FFileSystemReceiver.Enabled then
    FFileSystemNode.CheckState := csCheckedNormal
  else
    FFileSystemNode.CheckState := csUncheckedNormal;
  FFileSystemReceiver.Enabled := False;

  FTreeView.FullExpand;
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

  FZMQEndpoints.FromStrings(FManager.Settings.ZeroMQSettings.Endpoints);
  FValueList.Data      := FZMQEndpoints;
  FValueList.PopupMenu := ppmEndpoints;
end;

procedure TfrmDashboard.InitializeTreeView;
begin
  FTreeView.DefaultNodeHeight := 30;
  FTreeView.Images            := imlMain;
  FTreeView.AlignWithMargins  := False;
  FTreeView.OnBeforeCellPaint := FTreeViewBeforeCellPaint;
  FTreeView.OnFreeNode        := FTreeViewFreeNode;
  FTreeView.OnGetText         := FTreeViewGetText;
  FTreeView.OnChecked         := FTreeViewChecked;
  FTreeView.OnFocusChanged    := FTreeViewFocusChanged;
  FTreeView.OnDblClick        := FTreeViewDblClick;
  FTreeView.OnGetImageIndex   := FTreeViewGetImageIndex;
  with FTreeView do
  begin
    with Header.Columns.Add do
    begin
      Color    := clWhite;
      MaxWidth := 800;
      MinWidth := 100;
      Options  := [coAllowClick, coEnabled, coParentBidiMode, coResizable,
        coVisible, coSmartResize, coAllowFocus];
      Position := 0;
      Indent   := 8;
      Width    := 100;
      Text := 'Name';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 400;
      MinWidth := 150;
      Options  := [coAllowClick, coEnabled, coParentBidiMode, coResizable,
        coVisible, coSmartResize, coAllowFocus];
      Position := 1;
      Width    := 200;
      Text := 'Value';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 100;
      MinWidth := 100;
      Options  := [coAllowClick, coEnabled, coParentBidiMode, coVisible,
        coAutoSpring, coAllowFocus];
      Position := 2;
      Width    := 100;
      Text := 'Id';
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 100;
      MinWidth := 100;
      Options  := [coAllowClick, coEnabled, coParentBidiMode, coShowDropMark,
        coVisible, coAllowFocus];
      Position := 3;
      Width    := 100;
      Text := 'Messagecount';
    end;
    Header.MainColumn := 0;
    TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoSpanColumns];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowTreeLines];
  end;
  FTreeView.Header.AutoSizeIndex := 0;
  FTreeView.Indent := 30;
end;

{ Called when actions in the manager need to be updated. }

procedure TfrmDashboard.Modified;
begin
  FUpdate := True;
end;

procedure TfrmDashboard.SaveEndpoints;
var
  LStrings : Shared<TStrings>;
begin
  LStrings := TStringList.Create;
  FValueList.Data.ToStrings(LStrings);
  FManager.Settings.ZeroMQSettings.Endpoints.Assign(LStrings.Value);
end;

procedure TfrmDashboard.UpdateActions;
begin
  inherited UpdateActions;
  actMoveUpEndpoint.Enabled   := CanMoveUp;
  actMoveDownEndpoint.Enabled := CanMoveDown;
  if FUpdate then
  begin
    FManager.Actions.UpdateActions;
    FUpdate := False;
    FTreeView.Repaint;
  end;
end;
{$ENDREGION}

end.
