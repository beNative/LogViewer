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

unit LogViewer.Settings.Dialog;

{ Application settings. }

{
  View settings
    Callstack
    Watches
}

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.ActnList, Vcl.StdCtrls,

  VirtualTrees,

  LogViewer.Settings,
  LogViewer.Settings.Dialog.ConfigNode,

  LogViewer.Comport.Settings.View, LogViewer.WinIPC.Settings.View,
  LogViewer.Watches.Settings.View, LogViewer.WinODS.Settings.View,
  LogViewer.DisplayValues.Settings.View;

type
  TfrmLogViewerSettings = class(TForm)
    pnlConfigTree : TPanel;
    pgcMain       : TPageControl;
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
    tsDisplayValuesSettings: TTabSheet;

    procedure actCloseExecute(Sender: TObject);

  private
    FConfigTree                : TVirtualStringTree;
    FSettings                  : TLogViewerSettings;
    FComportSettingsForm       : TfrmComPortSettings;
    FWatchSettingsForm         : TfrmWatchSettings;
    FWinIPCSettingsForm        : TfrmWinIPCSettings;
    FWinODSSettingsForm        : TfrmWinODSSettings;
    FDisplayValuesSettingsForm : TfrmDisplayValuesSettings;

    procedure FConfigTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );

    procedure FConfigTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );

    procedure FConfigTreeFreeNode(
      Sender: TBaseVirtualTree;
      Node: PVirtualNode
    );

  protected
    procedure BuildConfigNodes;
    procedure AddNodesToTree(AParent: PVirtualNode; ANode: TConfigNode);

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
  DDuce.Factories.VirtualTrees;

{$REGION 'construction and destruction'}
constructor TfrmLogViewerSettings.Create(AOwner: TComponent;
  ASettings: TLogViewerSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmLogViewerSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateSettingsForms;
  FConfigTree := TVirtualStringTreeFactory.CreateTreeList(Self, pnlConfigTree);
  FConfigTree.OnGetText      := FConfigTreeGetText;
  FConfigTree.OnFreeNode     := FConfigTreeFreeNode;
  FConfigTree.OnFocusChanged := FConfigTreeFocusChanged;
  FConfigTree.TreeOptions.PaintOptions :=
    FConfigTree.TreeOptions.PaintOptions + [toShowTreeLines];

  FConfigTree.NodeDataSize := SizeOf(TConfigNode);
  BuildConfigNodes;
end;

procedure TfrmLogViewerSettings.BeforeDestruction;
begin
  FConfigTree.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLogViewerSettings.FConfigTreeFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  CN: TConfigNode;
begin
  CN := Sender.GetNodeData<TConfigNode>(Node);
  if Assigned(CN.TabSheet) then
    pgcMain.ActivePage := CN.TabSheet;
end;

procedure TfrmLogViewerSettings.FConfigTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  CN : TConfigNode;
begin
  CN := Sender.GetNodeData<TConfigNode>(Node);
  FreeAndNil(CN);
end;

procedure TfrmLogViewerSettings.FConfigTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  CN : TConfigNode;
begin
  CN := Sender.GetNodeData<TConfigNode>(Node);
  CellText := CN.Text;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLogViewerSettings.actCloseExecute(Sender: TObject);
begin
  Close;
end;
{$ENDREGION}

{$REGION 'protected methods'}
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
var
  LNode : TConfigNode;
begin
  LNode := TConfigNode.Create('View settings');
  AddNodesToTree(nil, LNode);
  AddNodesToTree(LNode.VTNode, TConfigNode.Create('Watches', tsWatches));
  AddNodesToTree(LNode.VTNode, TConfigNode.Create('Callstack', tsCallstack));
  LNode := TConfigNode.Create('Channel settings');
  AddNodesToTree(nil, LNode);
  AddNodesToTree(LNode.VTNode, TConfigNode.Create('WinIPC', tsWinIPC));
  AddNodesToTree(LNode.VTNode, TConfigNode.Create('OutputDebugString API', tsWinODS));
  AddNodesToTree(LNode.VTNode, TConfigNode.Create('Serial port', tsComport));
  AddNodesToTree(LNode.VTNode, TConfigNode.Create('ZeroMQ', tsZeroMQ));
  LNode := TConfigNode.Create('General settings');
  AddNodesToTree(nil, LNode);
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

  FDisplayValuesSettingsForm := TfrmDisplayValuesSettings.Create(
    Self, FSettings.DisplayValuesSettings
  );
  FDisplayValuesSettingsForm.Parent      := tsDisplayValuesSettings;
  FDisplayValuesSettingsForm.Align       := alClient;
  FDisplayValuesSettingsForm.BorderStyle := bsNone;
  FDisplayValuesSettingsForm.Visible     := True;
end;
{$ENDREGION}

end.
