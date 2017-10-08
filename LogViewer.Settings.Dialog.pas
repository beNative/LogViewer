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

unit LogViewer.Settings.Dialog;

{ Application settings. }

{
  View settings
    Callstack
    Watches
}

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ActnList, Vcl.StdCtrls,

  VirtualTrees,

  LogViewer.Settings,
  LogViewer.Settings.Dialog.ConfigNode,

  LogViewer.Comport.Settings.View, LogViewer.WinIPC.Settings.View,
  LogViewer.Watches.Settings.View, LogViewer.WinODS.Settings.View;

type
  TfrmLogViewerSettings = class(TForm)
    pnlConfigTree : TPanel;
    pgcMain       : TPageControl;
    tvConfig      : TTreeView;
    tsWatches     : TTabSheet;
    tsCallstack   : TTabSheet;
    tsWinIPC      : TTabSheet;
    tsWinODS      : TTabSheet;
    tsComport     : TTabSheet;
    tsZeroMQ      : TTabSheet;
    pnlBottom     : TPanel;
    aclMain       : TActionList;
    actClose      : TAction;
    btnClose      : TButton;
    btn2          : TButton;

    procedure tvConfigChange(Sender: TObject; Node: TTreeNode);

    procedure actCloseExecute(Sender: TObject);

  private
    FViewSettings        : TConfigNode;
    FConfigTree          : TVirtualStringTree;
    FSettings            : TLogViewerSettings;
    FComportSettingsForm : TfrmComPortSettings;
    FWatchSettingsForm   : TfrmWatchSettings;
    FWinIPCSettingsForm  : TfrmWinIPCSettings;
    FWinODSSettingsForm  : TfrmWinODSSettings;

    procedure FConfigTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );

  protected
    procedure BuildConfigNodes;
    procedure AddNodesToTree(AParent: PVirtualNode; ANode: TConfigNode);

    procedure InitializeControls;
    procedure CreateSettingsForms;

  public

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      AOwner    : TComponent;
      ASettings : TLogViewerSettings
    ); reintroduce;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Factories;

{$REGION 'construction and destruction'}

procedure TfrmLogViewerSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeControls;
//  FConfigTree := TFactories.CreateVirtualStringTree(Self, pnlConfigTree);
//  FConfigTree.OnGetText := FConfigTreeGetText;
//  FConfigTree.TreeOptions.PaintOptions :=
//    FConfigTree.TreeOptions.PaintOptions + [toShowTreeLines];
//
//  FConfigTree.TreeOptions.AutoOptions := FConfigTree.TreeOptions.AutoOptions
//    + [toAutoExpand];

//  FConfigTree.NodeDataSize := SizeOf(TConfigNode);
  BuildConfigNodes;
end;

procedure TfrmLogViewerSettings.BeforeDestruction;
begin
  //FConfigRoot.Free;
  FConfigTree.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLogViewerSettings.FConfigTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  CN : TConfigNode;
begin
  CN := Sender.GetNodeData<TConfigNode>(Node);
  CellText := CN.Text;
end;

procedure TfrmLogViewerSettings.InitializeControls;
var
  I, J: Integer;
  B : Boolean;
  LNode: TTreeNode;
begin
  // store index of corresponding page in Data property of treenode.
  for I := 0 to tvConfig.Items.Count - 1 do
  begin
    LNode := tvConfig.Items[I];
    B := False;
    J := 0;
    while not B and (J < pgcMain.PageCount) do
    begin
      if pgcMain.Pages[J].Caption = LNode.Text then
      begin
        pgcMain.Pages[J].TabVisible := False;
        B := True;
        LNode.Data := Pointer(J);
      end;
      Inc(J);
    end;
  end;

  CreateSettingsForms;
  tvConfig.FullExpand;

end;

procedure TfrmLogViewerSettings.tvConfigChange(Sender: TObject;
  Node: TTreeNode);
begin
  pgcMain.ActivePageIndex := Integer(Node.Data);
end;

{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmLogViewerSettings.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmLogViewerSettings.AddNodesToTree(AParent: PVirtualNode;
  ANode: TConfigNode);
var
  LSubNode: TConfigNode;
  LVTNode : PVirtualNode;
begin
  LVTNode := FConfigTree.AddChild(AParent, ANode);
  ANode.VTNode := LVTNode;
  for LSubNode in ANode.Nodes do
    AddNodesToTree(LVTNode, LSubNode);
end;

procedure TfrmLogViewerSettings.BuildConfigNodes;
begin
//  FViewSettings := TConfigNode.Create('View settings');
//  AddNodesToTree(nil, FViewSettings);
//  AddNodesToTree(FViewSettings.VTNode, TConfigNode.Create('Watches'));
//  AddNodesToTree(FViewSettings.VTNode, TConfigNode.Create('Callstack'));
//  AddNodesToTree(nil, TConfigNode.Create('Channel settings'));
//  AddNodesToTree(nil, TConfigNode.Create('General settings'));
  //FConfigRoot.Nodes.Add();
end;
constructor TfrmLogViewerSettings.Create(AOwner: TComponent;
  ASettings: TLogViewerSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmLogViewerSettings.CreateSettingsForms;
begin
  FComportSettingsForm := TfrmComPortSettings.Create(Self, FSettings.ComPortSettings);
  FComportSettingsForm.Parent      := tsComport;
  FComportSettingsForm.Align       := alClient;
  FComportSettingsForm.BorderStyle := bsNone;
  FComportSettingsForm.Visible     := True;

  FWatchSettingsForm := TfrmWatchSettings.Create(Self, FSettings.WatchSettings);
  FWatchSettingsForm.Parent      := tsWatches;
  FWatchSettingsForm.Align       := alClient;
  FWatchSettingsForm.BorderStyle := bsNone;
  FWatchSettingsForm.Visible     := True;

  FWinIPCSettingsForm := TfrmWinIPCSettings.Create(Self, FSettings.WinIPCSettings);
  FWinIPCSettingsForm.Parent      := tsWinIPC;
  FWinIPCSettingsForm.Align       := alClient;
  FWinIPCSettingsForm.BorderStyle := bsNone;
  FWinIPCSettingsForm.Visible     := True;

  FWinODSSettingsForm := TfrmWinODSSettings.Create(Self, FSettings.WinODSSettings);
  FWinODSSettingsForm.Parent      := tsWinODS;
  FWinODSSettingsForm.Align       := alClient;
  FWinODSSettingsForm.BorderStyle := bsNone;
  FWinODSSettingsForm.Visible     := True;
end;
{$ENDREGION}

end.
