{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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
  System.SysUtils, System.Variants, System.Classes, System.Actions, System.Rtti,
  System.UITypes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.ImgList, Vcl.Menus,

  Spring.Collections,

  VirtualTrees, VirtualTrees.BaseTree, ZeroMQ, OMultiPanel,
  kcontrols, kpagecontrol,

  DDuce.Components.VirtualTrees.Node, DDuce.Components.ValueList,
  DDuce.DynamicRecord, DDuce.EditList,

  LogViewer.Interfaces, LogViewer.Dashboard.Data;

type
  TDashboardNode = TVTNode<TDashboardData>;

type
  TfrmDashboard = class(TForm)
    {$REGION 'designer controls'}
    aclMain                    : TActionList;
    actAddEndpoint             : TAction;
    actAddSubscribeToLogViewer : TAction;
    actCopyEndpoint            : TAction;
    actDeleteEndpoint          : TAction;
    actInspectTreeview         : TAction;
    actMoveDownEndpoint        : TAction;
    actMoveUpEndpoint          : TAction;
    actSubscribeToLocalHost    : TAction;
    btnAddSubscribeToLogViewer : TButton;
    btnSubscribeToLocalHost    : TButton;
    imlMain                    : TImageList;
    lblWinIPC                  : TLabel;
    lblWinods                  : TLabel;
    pnlZeroMQTitle             : TPanel;
    pgcMain                    : TKPageControl;
    pnlButtons                 : TGridPanel;
    pnlComPorts                : TPanel;
    pnlComPortTitle            : TPanel;
    pnlFileSystemTitle         : TPanel;
    pnlFSLocations             : TPanel;
    pnlLeft                    : TPanel;
    pnlMain                    : TOMultiPanel;
    pnlRight                   : TPanel;
    pnlWinIPCTitle             : TPanel;
    pnlWinodsTitle             : TPanel;
    pnlZmqEndpoints            : TPanel;
    ppmSubscriber              : TPopupMenu;
    tsComPort                  : TKTabSheet;
    tsFileSystem               : TKTabSheet;
    tsWinIpc                   : TKTabSheet;
    tsWinOds                   : TKTabSheet;
    tsZeroMQ                   : TKTabSheet;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actSubscribeToLocalHostExecute(Sender: TObject);
    procedure actInspectTreeviewExecute(Sender: TObject);
    procedure actAddSubscribeToLogViewerExecute(Sender: TObject);
    {$ENDREGION}

  private
    FUpdate             : Boolean;
    FManager            : ILogViewerManager;
    FTreeView           : TVirtualStringTree;
    FZeroMQ             : IZeroMQ;
    FZmqReceiver        : IChannelReceiver;
    FWinipcReceiver     : IChannelReceiver;
    FComPortReceiver    : IChannelReceiver;
    FWinodsReceiver     : IChannelReceiver;
    FFileSystemReceiver : IChannelReceiver;
    FZmqNode            : TDashboardNode;
    FWinipcNode         : TDashboardNode;
    FComPortNode        : TDashboardNode;
    FWinodsNode         : TDashboardNode;
    FFileSystemNode     : TDashboardNode;
    FZmqEndpoints       : TEditList;
    FComPorts           : TEditList;
    FFSLocations        : TEditList;

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
    procedure FTreeViewGetPopupMenu(
      Sender        : TBaseVirtualTree;
      Node          : PVirtualNode;
      Column        : TColumnIndex;
      const P       : TPoint;
      var AskParent : Boolean;
      var PopupMenu : TPopupMenu
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
    procedure FTreeViewChecked(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
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
    procedure FTreeViewPaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure FZmqEndpointsAdd(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FZmqEndpointsItemExecute(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FFSLocationsAdd(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FFSLocationsItemExecute(
      ASender    : TObject;
      var AName  : string;
      var AValue : TValue
    );
    procedure FWinipcReceiverSubscriberListChanged(
      Sender     : TObject;
      const AKey : UInt32;
      Action     : TCollectionChangedAction
    );
    procedure FZmqReceiverSubscriberListChanged(
      Sender     : TObject;
      const AKey : UInt32;
      Action     : TCollectionChangedAction
    );
    procedure FWinodsReceiverSubscriberListChanged(
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
    {$ENDREGION}

  protected
    procedure CreateWinipcReceiver;
    procedure CreateWinodsReceiver;
    procedure CreateZmqReceiver;
    procedure CreateComPortReceiver;
    procedure CreateFileSystemReceiver;

    procedure Modified;
    procedure InitializeTreeView;
    procedure InitializeControls;
    procedure CreateChannelReceivers;

    function AddNode(
      AParentNode : TDashboardNode;
      AReceiver   : IChannelReceiver;
      ASubscriber : ISubscriber
    ): TDashboardNode;

    procedure SaveSettings;
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner   : TComponent;
      AManager : ILogViewerManager
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Update; override;
  end;

implementation

{$R *.dfm}

uses
  System.DateUtils,

  VirtualTrees.Types, VirtualTrees.Header,

  Spring,

  DDuce.Utils, DDuce.Utils.Winapi, DDuce.Logger,
  DDuce.Factories.VirtualTrees,
  DDuce.ObjectInspector.zObjectInspector,

  LogViewer.Factories, LogViewer.Resources,
  LogViewer.Subscribers.Zmq, LogViewer.Subscribers.FileSystem,
  LogViewer.Receivers.Base;

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
  FZmqEndpoints := TEditList.Create(Self, pnlZMQEndpoints);
  FZmqEndpoints.OnAdd.Add(FZmqEndpointsAdd);
  FZmqEndpoints.OnItemExecute.Add(FZmqEndpointsItemExecute);
  FZmqEndpoints.OnExecute.Add(FZmqEndpointsItemExecute);
  FZmqEndpoints.ValueList.BorderStyle := bsNone;
  // TODO: implement a better way to save changes.
  //FZMQEndpoints.ValueList.OnExit := FValueListExit;
  FZmqEndpoints.ActionExecute.Caption := SSubscribe;

  FComPorts := TEditList.Create(Self, pnlCOMPorts);
  FComPorts.ValueList.BorderStyle := bsNone;

  FFSLocations := TEditList.Create(Self, pnlFSLocations);
  FFSLocations.OnAdd.Add(FFSLocationsAdd);
  FFSLocations.OnItemExecute.Add(FFSLocationsItemExecute);
  FFSLocations.OnExecute.Add(FFSLocationsItemExecute);
  FFSLocations.ValueList.BorderStyle := bsNone;
  FFSLocations.ActionExecute.Caption := SSubscribe;
  //FFSLocations.ValueList.OnExit := FValueListExit;

  InitializeTreeView;
  InitializeControls;
  CreateChannelReceivers;
end;

destructor TfrmDashboard.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  // required as this event can be called after FManager is released!

  FZmqEndpoints.OnExecute.RemoveAll(Self);
  FZmqEndpoints.OnItemExecute.RemoveAll(Self);
  FZmqEndpoints.OnAdd.RemoveAll(Self);
  FZmqEndpoints.OnDelete.RemoveAll(Self);

  FFSLocations.OnExecute.RemoveAll(Self);
  FFSLocations.OnItemExecute.RemoveAll(Self);
  FFSLocations.OnAdd.RemoveAll(Self);
  FFSLocations.OnDelete.RemoveAll(Self);

  FZmqEndpoints.ValueList.OnExit := nil;
  FZmqReceiver.OnChange.RemoveAll(Self);
  FWinipcReceiver.OnChange.RemoveAll(Self);
//  FComPortReceiver.OnChange.RemoveAll(Self);

  SaveSettings;
  FZmqReceiver        := nil;
  FWinipcReceiver     := nil;
  FWinodsReceiver     := nil;
//  FComPortReceiver    := nil;
  FFileSystemReceiver := nil;
  FManager            := nil;
  FZeroMQ             := nil;
  Logger.Info('Dashboard TreeView clear');
  FTreeView.Clear;
  FTreeView.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'action handlers'}
{ Used to debug the application itself. }

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
        FZmqReceiver,
        FZeroMQ,
        LEndPoint,
        GetCurrentProcessId,
        LEndPoint,
        LName,
        FZmqReceiver.Enabled
      );
      Logger.Channels.Clear;
      FZmqReceiver.SubscriberList.Add(LSubscriber.SourceId, LSubscriber);
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
        FZmqReceiver,
        FZeroMQ,
        LEndPoint,
        0,
        LEndPoint,
        LName,
        FZmqReceiver.Enabled
      );
      FZmqReceiver.SubscriberList.Add(LSubscriber.SourceId, LSubscriber);
      Modified;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmDashboard.actInspectTreeviewExecute(Sender: TObject);
begin
  InspectComponent(FTreeView);
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
  if DN.Level = 0 then
  begin
    if DN.Data.Receiver.Enabled then
      Node.CheckState := csCheckedNormal
    else
      Node.CheckState := csUncheckedNormal;
    TargetCanvas.Font.Style := [fsBold];
  end
  else if DN.Level = 1 then
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
    if DN.Level = 0 then
    begin
      B := Node.CheckState = csCheckedNormal;
      DN.Data.Receiver.Enabled := B;
      if Supports(DN.Data.Receiver, IWinipc) then
      begin
        FManager.Settings.WinipcSettings.Enabled := B;
      end
      else if Supports(DN.Data.Receiver, IZmq) then
      begin
        FManager.Settings.ZmqSettings.Enabled := B;
      end
      else if Supports(DN.Data.Receiver, IWinods) then
      begin
        FManager.Settings.WinodsSettings.Enabled := B;
      end
      else if Supports(DN.Data.Receiver, IComPort) then
      begin
        //FManager.Settings.ComPortSettings.Enabled := B;
      end
      else if Supports(DN.Data.Receiver, IFileSystem)  then
      begin
        FManager.Settings.FileSystemSettings.Enabled := B;
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
    Logger.SendInterface('V.Subscriber', V.Subscriber);
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
  if Supports(LReceiver, IWinipc) then
  begin
    pgcMain.ActivePage := tsWinIPC;
  end
  else if Supports(LReceiver, IZmq) then
  begin
    pgcMain.ActivePage := tsZeroMQ;
  end
  else if Supports(LReceiver, IWinods) then
  begin
    pgcMain.ActivePage := tsWinODS;
  end
  else if Supports(LReceiver, IComPort) then
  begin
    pgcMain.ActivePage := tsCOMPort;
  end
  else if Supports(LReceiver, IFileSystem) then
  begin
    pgcMain.ActivePage := tsFileSystem;
  end
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
  if (DN.Level = 0) and (Column = 0) and (Kind in [ikNormal, ikSelected]) then
  begin
    if LReceiver = FZmqReceiver then
    begin
      ImageIndex := 1;
    end
    else if LReceiver = FComPortReceiver then
    begin
      ImageIndex := 2;
    end
    else
      ImageIndex := 0;
  end;
end;

procedure TfrmDashboard.FTreeViewGetPopupMenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: Boolean; var PopupMenu: TPopupMenu);
var
  DN : TDashboardNode;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Assigned(DN) and (DN.Level = 1) then
  begin
    PopupMenu := FManager.Menus.SubscriberPopupMenu;
  end;
end;

procedure TfrmDashboard.FTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  DN          : TDashboardNode;
  LSubscriber : ISubscriber;

  function DateTimeToText(ADateTime: TDateTime): string;
  begin
    if Double(ADateTime).SpecialType = fsZero then
      Result := ' '
    else
      Result := DateTimeToStr(ADateTime);
  end;

begin
  CellText := '';
  if not Assigned(Node) then
   Exit;

  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Assigned(DN) then
  begin
    if DN.Level = 0 then
    begin
      if Column = COLUMN_SOURCENAME then
        CellText := DN.Data.Caption
      else
        CellText := '';
    end
    else
    begin
      if Assigned(DN.Data.Subscriber) then
      begin
        LSubscriber := DN.Data.Subscriber;
        if Column = COLUMN_SOURCENAME then
          CellText := LSubscriber.SourceName
        else if Column = COLUMN_KEY then
          CellText := LSubscriber.Key
        else if Column = COLUMN_SOURCEID then
          CellText := LSubscriber.SourceId.ToString
        else if Column = COLUMN_MESSAGECOUNT then
        begin
          if LSubscriber.MessageCount > 0 then
            CellText := LSubscriber.MessageCount.ToString
          else
            CellText := ' ';
        end
        else if Column = COLUMN_BYTES_RECEIVED then
        begin
          if LSubscriber.BytesReceived > 0 then
            CellText :=  FormatBytes(LSubscriber.BytesReceived)
          else
            CellText := ' ';
        end
        else if Column = COLUMN_TIMESTAMP_FIRST then
          CellText := DateTimeToText(LSubscriber.TimeStampFirst)
        else if Column = COLUMN_TIMESTAMP_LAST then
          CellText := DateTimeToText(LSubscriber.TimeStampLast);
      end
    end;
  end;
end;

procedure TfrmDashboard.FTreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  DN : TDashboardNode;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Assigned(DN) then
  begin
    if DN.Level = 0 then
      TargetCanvas.Font.Style := [fsBold]
    else if DN.Level = 1 then
    begin
      if Column < 2 then
      begin
        TargetCanvas.Font.Style := [fsBold];
      end
      else
      begin
        TargetCanvas.Font.Style := [];
      end;
      if DN.Data.Subscriber.IsSourceActive then
      begin
        if DN.Data.Subscriber.Enabled then
          TargetCanvas.Font.Color := clGreen
        else
          TargetCanvas.Font.Color := clRed;
      end
      else
        TargetCanvas.Font.Color := clGray;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'Subscriber lists'}
procedure TfrmDashboard.FWinipcReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LNode       : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LNode := nil;
  if Action = caRemoved then
  begin
    for LNode in FWinipcNode do
    begin
      if LNode.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LNode) then
    begin
      FTreeView.DeleteNode(LNode.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FWinipcReceiver.SubscriberList[AKey];
    LNode := AddNode(FWinipcNode, nil, LSubscriber);
    LNode.Selected := True;
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;

procedure TfrmDashboard.FWinodsReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LNode       : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LNode := nil;
  if Action = caRemoved then
  begin
    for LNode in FWinodsNode do
    begin
      if LNode.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LNode) then
    begin
      FTreeView.DeleteNode(LNode.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FWinodsReceiver.SubscriberList[AKey];
    LNode := AddNode(FWinodsNode, nil, LSubscriber);
    LNode.Selected := True;
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;

procedure TfrmDashboard.FZmqReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LNode       : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  LNode := nil;
  if Action = caRemoved then
  begin
    for LNode in FZmqNode do
    begin
      if LNode.Data.Subscriber.SourceId = AKey then
        Break;
    end;
    if Assigned(LNode) then
    begin
      FTreeView.DeleteNode(LNode.VNode);
    end;
  end
  else if Action = caAdded then
  begin
    LSubscriber := FZmqReceiver.SubscriberList[AKey];
    LNode := AddNode(FZmqNode, nil, LSubscriber);
    LNode.Selected := True;
    LSubscriber.OnChange.Add(FSubscriberChange);
  end;
end;
{$ENDREGION}

{$REGION 'FZMQEndpoints'}
procedure TfrmDashboard.FZmqEndpointsAdd(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  AValue := 'tcp://';
end;

procedure TfrmDashboard.FZmqEndpointsItemExecute(ASender: TObject;
  var AName: string; var AValue: TValue);
var
  LSubscriber : ISubscriber;
  LEndPoint   : string;
  LName       : string;
begin
  LEndPoint   := AValue.ToString;
  LName       := AName;
  LSubscriber := TZMQSubscriber.Create(
    FZmqReceiver,
    FZeroMQ,
    LEndPoint,
    0,         // ASourceId
    LEndPoint, // AKey
    LName,     // ASourceName
    FZmqReceiver.Enabled
  );
  FZmqReceiver.SubscriberList.Add(LSubscriber.SourceId, LSubscriber);
  Modified;
end;

procedure TfrmDashboard.FComPortReceiverSubscriberListChanged(Sender: TObject;
  const AKey: UInt32; Action: TCollectionChangedAction);
var
  LDelete     : TDashboardNode;
  LSubscriber : ISubscriber;
begin
  Logger.Track(Self, 'FComPortReceiverSubscriberListChanged');
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
  Logger.Track(Self, 'FFileSystemReceiverSubscriberListChanged');
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
    if Assigned(LSubscriber) then
    begin
      AddNode(FFileSystemNode, nil, LSubscriber);
      LSubscriber.OnChange.Add(FSubscriberChange);
    end;
  end;
end;

procedure TfrmDashboard.FFSLocationsAdd(ASender: TObject; var AName: string;
  var AValue: TValue);
begin
  AName  := 'New';
  AValue := '';
end;

procedure TfrmDashboard.FFSLocationsItemExecute(ASender: TObject;
  var AName: string; var AValue: TValue);
var
  LSubscriber : ISubscriber;
  LName       : string;
begin
  LName       := AName;
  LSubscriber := TFileSystemSubscriber.Create(
    FFileSystemReceiver,
    10,
    AName,
    AName,
    FFileSystemReceiver.Enabled
  );
  FFileSystemReceiver.SubscriberList.Add(LSubscriber.SourceId, LSubscriber);
  Modified;
end;
{$ENDREGION}

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

procedure TfrmDashboard.CreateChannelReceivers;
begin
  Logger.Track(Self, 'CreateChannelReceivers');
  CreateWinipcReceiver;
  CreateZmqReceiver;
//  CreateWinodsReceiver;
//  CreateComPortReceiver;
//  CreateFileSystemReceiver;
  FTreeView.FullExpand;
end;

procedure TfrmDashboard.CreateFileSystemReceiver;
begin
  FFileSystemReceiver :=
    TLogViewerFactories.CreateFileSystemReceiver(FManager, '');
  FManager.AddReceiver(FFileSystemReceiver);
  FFileSystemReceiver.SubscriberList.OnKeyChanged.Add(
    FFileSystemReceiverSubscriberListChanged
  );
  FFileSystemNode := AddNode(nil, FFileSystemReceiver, nil);
  FFileSystemReceiver.OnChange.Add(FReceiverChange);
  FFileSystemNode.CheckType := ctCheckBox;
  FFileSystemReceiver.Enabled := FManager.Settings.FileSystemSettings.Enabled;
  if FFileSystemReceiver.Enabled then
    FFileSystemNode.CheckState := csCheckedNormal
  else
    FFileSystemNode.CheckState := csUncheckedNormal;
end;

procedure TfrmDashboard.CreateComPortReceiver;
begin
  FComPortReceiver := TLogViewerFactories.CreateComPortReceiver(
    FManager, FManager.Settings.ComPortSettings
  );
  FManager.AddReceiver(FComPortReceiver);
  FComPortReceiver.OnChange.Add(FReceiverChange);
  FComPortReceiver.SubscriberList.OnKeyChanged.Add(
    FComPortReceiverSubscriberListChanged
  );
  FComPortNode := AddNode(nil, FComPortReceiver, nil);
  FComPortNode.CheckType := ctCheckBox;
  if FComPortReceiver.Enabled then
    FComPortNode.CheckState := csCheckedNormal
  else
    FComPortNode.CheckState := csUncheckedNormal;
  FComPortReceiver.Enabled := False;
end;

procedure TfrmDashboard.CreateZmqReceiver;
begin
  FZeroMQ := TZeroMQ.Create;
  FZmqReceiver := TLogViewerFactories.CreateZmqReceiver(FManager, FZeroMQ);
  FManager.AddReceiver(FZmqReceiver);
  FZmqReceiver.OnChange.UseFreeNotification := True;
  FZmqReceiver.OnChange.Add(FReceiverChange);
  FZmqReceiver.SubscriberList.OnKeyChanged.Add(FZmqReceiverSubscriberListChanged);
  FZmqReceiver.Enabled := FManager.Settings.ZmqSettings.Enabled;
  FZmqNode := AddNode(nil, FZmqReceiver, nil);
  FZmqNode.CheckType := ctCheckBox;
  if FZmqReceiver.Enabled then
    FZmqNode.CheckState := csCheckedNormal
  else
    FZmqNode.CheckState := csUncheckedNormal;
end;

procedure TfrmDashboard.CreateWinodsReceiver;
begin
  FWinodsReceiver := TLogViewerFactories.CreateWinodsReceiver(FManager);
  FManager.AddReceiver(FWinodsReceiver);
  FWinodsReceiver.OnChange.UseFreeNotification := False;
  FWinodsReceiver.OnChange.Add(FReceiverChange);
  FWinodsReceiver.SubscriberList.OnKeyChanged.Add(FWinodsReceiverSubscriberListChanged);
  FWinodsReceiver.Enabled := FManager.Settings.WinodsSettings.Enabled;
  FWinodsNode := AddNode(nil, FWinodsReceiver, nil);
  FWinodsNode.CheckType := ctCheckBox;
  if FWinodsReceiver.Enabled then
    FWinodsNode.CheckState := csCheckedNormal
  else
    FWinodsNode.CheckState := csUncheckedNormal;
end;

procedure TfrmDashboard.CreateWinipcReceiver;
begin
  FWinipcReceiver := TLogViewerFactories.CreateWinipcReceiver(FManager);
  FManager.AddReceiver(FWinipcReceiver);
  FWinipcReceiver.OnChange.UseFreeNotification := False;
  FWinipcReceiver.OnChange.Add(FReceiverChange);
  FWinipcReceiver.SubscriberList.OnKeyChanged.Add(FWinipcReceiverSubscriberListChanged);
  FWinipcReceiver.Enabled := FManager.Settings.WinipcSettings.Enabled;
  FWinipcNode := AddNode(nil, FWinipcReceiver, nil);
  FWinipcNode.CheckType := ctCheckBox;
  if FWinipcReceiver.Enabled then
    FWinipcNode.CheckState := csCheckedNormal
  else
    FWinipcNode.CheckState := csUncheckedNormal;
end;

procedure TfrmDashboard.InitializeControls;
var
  I : Integer;
begin
  for I := 0 to pgcMain.PageCount - 1 do
  begin
    //pgcMain.Pages[I].TabVisible := False;
  end;
//  FComPortSettingsForm := TfrmComPortSettings.Create(
//    Self, FManager.Settings.ComPortSettings
//  );
//  AssignFormParent(FComPortSettingsForm, tsCOMPort);
  pgcMain.ActivePage := tsWinIPC;
  FZmqEndpoints.Data.FromStrings(FManager.Settings.ZmqSettings.Endpoints);
  FFSLocations.Data.FromStrings(FManager.Settings.FileSystemSettings.PathNames);
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
  FTreeView.OnGetPopupMenu    := FTreeViewGetPopupMenu;
  FTreeView.OnPaintText       := FTreeViewPaintText;
  with FTreeView do
  begin
    with Header.Columns.Add do
    begin
      Color    := clWhite;
      MaxWidth := 800;
      MinWidth := 100;
      Options  := [coAllowClick, coEnabled, coParentBidiMode, coResizable,
        coVisible, coSmartResize, coAllowFocus];
      Position := COLUMN_SOURCENAME;
      Indent   := 8;
      Width    := 100;
      Text     := SName;
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 400;
      MinWidth := 150;
      Options  := [coAllowClick, coEnabled, coParentBidiMode, coResizable,
        coVisible, coSmartResize, coAllowFocus];
      Position := COLUMN_KEY;
      Width    := 200;
      Text     := SValue;
    end;
    with Header.Columns.Add do
    begin
      MaxWidth  := 80;
      MinWidth  := 80;
      Options   := [coAllowClick, coEnabled, coParentBidiMode, coVisible,
        coAutoSpring, coAllowFocus];
      Position  := COLUMN_SOURCEID;
      Width     := 80;
      Alignment := taCenter;
      Text      := SId;
    end;
    with Header.Columns.Add do
    begin
      MaxWidth  := 90;
      MinWidth  := 90;
      Options   := [coAllowClick, coEnabled, coParentBidiMode,
        coVisible, coAllowFocus];
      Position  := COLUMN_MESSAGECOUNT;
      Width     := 90;
      Alignment := taCenter;
      Text      := SMessageCount;
    end;
    with Header.Columns.Add do
    begin
      MaxWidth  := 80;
      MinWidth  := 80;
      Options   := [coAllowClick, coEnabled, coParentBidiMode,
        coVisible, coAllowFocus];
      Position  := COLUMN_BYTES_RECEIVED;
      Width     := 80;
      Alignment := taCenter;
      Text      := SBytesReceived;
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 120;
      MinWidth := 120;
      Options  := [coAllowClick, coEnabled, coParentBidiMode, coVisible,
        coAllowFocus];
      Position := COLUMN_TIMESTAMP_FIRST;
      Width    := 120;
      Text     := STimeStampFirst;
    end;
    with Header.Columns.Add do
    begin
      MaxWidth := 120;
      MinWidth := 120;
      Options  := [coAllowClick, coEnabled, coParentBidiMode,
        coVisible, coAllowFocus];
      Position := COLUMN_TIMESTAMP_LAST;
      Width    := 120;
      Text     := STimeStampLast;
    end;
    Header.MainColumn := 0;
    TreeOptions.AutoOptions  := TreeOptions.AutoOptions + [toAutoSpanColumns];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowTreeLines];
  end;
  FTreeView.Header.AutoSizeIndex := 0;
  FTreeView.Indent := 30;
end;

{ Called when actions in the manager need to be updated. }

procedure TfrmDashboard.Modified;
begin
  FUpdate := True;
  Logger.Watch('FUpdate', FUpdate);
end;

procedure TfrmDashboard.SaveSettings;
var
  LStrings : Shared<TStrings>;
begin
  Logger.Track(Self, 'SaveSettings');
  LStrings := TStringList.Create;
  FZmqEndpoints.Data.ToStrings(LStrings);
  FManager.Settings.ZmqSettings.Endpoints.Assign(LStrings.Value);
  FFSLocations.Data.ToStrings(LStrings);
  FManager.Settings.FileSystemSettings.PathNames.Assign(LStrings.Value);
end;

procedure TfrmDashboard.UpdateActions;
begin
  inherited UpdateActions;
  if FUpdate then
  begin
    FManager.Actions.UpdateActions;
    FUpdate := False;
    Logger.Watch('FUpdate', FUpdate);
    FTreeView.Repaint;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmDashboard.Update;
begin
  inherited Update;
  Modified;
  UpdateActions;
end;
{$ENDREGION}

end.
