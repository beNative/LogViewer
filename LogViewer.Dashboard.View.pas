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
  Vcl.ActnList,

  VirtualTrees,

  LogViewer.Interfaces;

type
  TfrmDashboard = class(TForm)
    {$REGION 'designer controls'}
    aclMain           : TActionList;
    chkComPortEnabled : TCheckBox;
    chkWinIPCEnabled  : TCheckBox;
    chkWinODSEnabled  : TCheckBox;
    chkZeroMQEnabled  : TCheckBox;
    pnlLogChannels    : TPanel;
    {$ENDREGION}

    procedure chkWinIPCEnabledClick(Sender: TObject);
    procedure chkWinODSEnabledClick(Sender: TObject);
    procedure chkZeroMQEnabledClick(Sender: TObject);
    procedure chkComPortEnabledClick(Sender: TObject);

  private
    FManager  : ILogViewerManager;
    FTreeView : TVirtualStringTree;

    procedure FTreeViewInitNode(
      Sender            : TBaseVirtualTree;
      ParentNode,
      Node              : PVirtualNode;
      var InitialStates : TVirtualNodeInitStates
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

    procedure FTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

//    procedure FChannelReceiverNewLogQueue(
//      Sender    : TObject;
//      ALogQueue : ILogQueue
//    );

  protected
    procedure InitializeTreeView;
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner   : TComponent;
      AManager : ILogViewerManager
    ); reintroduce; virtual;
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring, Spring.Collections,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,

  DDuce.ObjectInspector.zObjectInspector,

  LogViewer.Factories, LogViewer.Dashboard.View.Node;

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
  FTreeView := TVirtualStringTreeFactory.CreateTreeList(Self, pnlLogChannels);
  FTreeView.AlignWithMargins := False;

  R := TLogViewerFactories.CreateWinIPCChannelReceiver(FManager);
  FManager.AddReceiver(R);
  //R.OnNewLogQueue.Add(FChannelReceiverNewLogQueue);
  R.Enabled := FManager.Settings.WinIPCSettings.Enabled;

//  R := TLogViewerFactories.CreateWinODSChannelReceiver(FManager);
//  FManager.AddReceiver(R);
//  R.Enabled := FManager.Settings.WinODSSettings.Enabled;

  R := TLogViewerFactories.CreateZeroMQChannelReceiver(FManager);
  FManager.AddReceiver(R);
  //R.OnNewLogQueue.Add(FChannelReceiverNewLogQueue);
  R.Enabled := FManager.Settings.ZeroMQSettings.Enabled;

  //InitializeTreeView;
  //InspectComponent(FTreeView);
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'FTreeView'}
//procedure TfrmDashboard.FChannelReceiverNewLogQueue(Sender: TObject;
//  ALogQueue: ILogQueue);
//begin
//  FTreeView.Refresh;
//  FTreeView.ReinitNode(FTreeView.RootNode, True);
//end;

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
  DN.Receiver.Enabled := Node.CheckState = csCheckedNormal;
end;

procedure TfrmDashboard.FTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  DN : TDashboardNode;
begin
  DN := Sender.GetNodeData<TDashboardNode>(Node);
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    CellText := DN.Receiver.Name;
  end
  else
  begin
    if Assigned(DN.LogQueue) then
      CellText := DN.LogQueue.SourceName;
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
begin
  if ParentNode = nil then
    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
  Node.SetData<TDashboardNode>(TDashboardNode.Create(
    Node, FManager.Receivers[Node.Index], nil)
  );

  if Sender.GetNodeLevel(Node) = 0 then
    Node.CheckType := ctCheckBox;
end;
{$ENDREGION}

procedure TfrmDashboard.chkComPortEnabledClick(Sender: TObject);
begin
  //FManager.Settings.ComPortSettings.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmDashboard.chkWinIPCEnabledClick(Sender: TObject);
begin
  FManager.Settings.WinIPCSettings.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmDashboard.chkWinODSEnabledClick(Sender: TObject);
begin
  FManager.Settings.WinODSSettings.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TfrmDashboard.chkZeroMQEnabledClick(Sender: TObject);
begin
  FManager.Settings.ZeroMQSettings.Enabled := (Sender as TCheckBox).Checked;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDashboard.InitializeTreeView;
begin
//  FTreeView.OnBeforeCellPaint := FTreeViewBeforeCellPaint;
//  FTreeView.OnInitNode        := FTreeViewInitNode;
//  FTreeView.OnInitChildren    := FTreeViewInitChildren;
//  FTreeView.OnGetText         := FTreeViewGetText;
//  FTreeView.OnChecked         := FTreeViewChecked;
//  with FTreeView do
//  begin
//    with Header.Columns.Add do
//    begin
//      Color    := clWhite;
//      MaxWidth := 200;
//      MinWidth := 100;
//      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
//        coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus,
//        coEditable];
//      Position := 0;
//      Indent   := 8;
//      Width    := 200;
//      Text := 'Name';
//    end;
//    with Header.Columns.Add do
//    begin
//      MaxWidth := 800;
//      MinWidth := 100;
//      Options  := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
//        coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring,
//        coSmartResize, coAllowFocus, coEditable];
//      Position := 1;
//      Width    := 100;
//      Text := 'Value';
//    end;
//    Header.MainColumn := 0;
//  end;

  //FTreeView.RootNodeCount := FManager.Receivers.Count;
end;

procedure TfrmDashboard.UpdateActions;
begin
  chkWinIPCEnabled.Checked := FManager.Settings.WinIPCSettings.Enabled;
  chkWinODSEnabled.Checked := FManager.Settings.WinODSSettings.Enabled;
  chkZeroMQEnabled.Checked := FManager.Settings.ZeroMQSettings.Enabled;
//  chkComPortEnabled.Checked := FManager.Settings.ComPortSettings.Enabled;
  FManager.Actions.UpdateActions;

  inherited UpdateActions;
end;
{$ENDREGION}

end.

