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
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  VirtualTrees,

  LogViewer.Settings.Dialog.ConfigNode;

type
  TfrmLogViewerSettings = class(TForm)
    pnlConfigTree: TPanel;

  private
    FViewSettings : TConfigNode;
    FConfigTree   : TVirtualStringTree;

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

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Factories;

{$REGION 'construction and destruction'}

procedure TfrmLogViewerSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FConfigTree := TFactories.CreateVirtualStringTree(Self, pnlConfigTree);
  FConfigTree.OnGetText := FConfigTreeGetText;
  FConfigTree.TreeOptions.PaintOptions :=
    FConfigTree.TreeOptions.PaintOptions + [toShowTreeLines];
//
//  FConfigTree.TreeOptions.AutoOptions := FConfigTree.TreeOptions.AutoOptions
//    + [toAutoExpand];

  FConfigTree.NodeDataSize := SizeOf(TConfigNode);
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
begin
  FViewSettings := TConfigNode.Create('View settings');
  AddNodesToTree(nil, FViewSettings);
  AddNodesToTree(FViewSettings.VTNode, TConfigNode.Create('Watches'));
  AddNodesToTree(FViewSettings.VTNode, TConfigNode.Create('Callstack'));
  AddNodesToTree(nil, TConfigNode.Create('Channel settings'));
  AddNodesToTree(nil, TConfigNode.Create('General settings'));
  //FConfigRoot.Nodes.Add();
end;
{$ENDREGION}

end.
