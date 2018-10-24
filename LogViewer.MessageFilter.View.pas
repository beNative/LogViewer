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

unit LogViewer.MessageFilter.View;

{ User interface for message filter setup. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  VirtualTrees,

  DDuce.Logger.Interfaces,
  DDuce.Components.VirtualTrees.Node,

  LogViewer.MessageList.Settings, LogViewer.MessageFilter.Data;

type
  TFilterNode = TVTNode<TFilterData>;

type
  TfrmMessageFilter = class(TForm)
    pnlMessageFilter : TPanel;
  private
    FTree      : TVirtualStringTree;
    FImageList : TImageList;
    FSettings  : TMessageListSettings;

  protected
    procedure BuildTree;

    {$REGION 'event handlers'}
    procedure FTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FTreeGetImageIndex(
      Sender         : TBaseVirtualTree;
      Node           : PVirtualNode;
      Kind           : TVTImageKind;
      Column         : TColumnIndex;
      var Ghosted    : Boolean;
      var ImageIndex : TImageIndex
    );
    procedure FTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FTreeFreeNode(
      Sender: TBaseVirtualTree;
      Node: PVirtualNode
    );
    procedure FTreeChecked(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );

    procedure FSettingsChanged(Sender: TObject);

    {$ENDREGION}

  public
    constructor Create(
      AOwner     : TComponent;
      ASettings  : TMessageListSettings;
      AImageList : TImageList
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring,

  DDuce.Factories.VirtualTrees;

{$REGION 'construction and destruction'}
procedure TfrmMessageFilter.AfterConstruction;
begin
  inherited AfterConstruction;
  FTree := TVirtualStringTreeFactory.CreateTree(Self, pnlMessageFilter);
  FTree.OnGetText       := FTreeGetText;
  FTree.OnGetImageIndex := FTreeGetImageIndex;
  FTree.OnFreeNode      := FTreeFreeNode;
  FTree.OnFocusChanged  := FTreeFocusChanged;
  FTree.OnChecked       := FTreeChecked;

  FTree.Header.Options := FTree.Header.Options - [hoVisible];
  FTree.TreeOptions.PaintOptions := FTree.TreeOptions.PaintOptions
    + [toShowTreeLines];
  FTree.TreeOptions.SelectionOptions := FTree.TreeOptions.SelectionOptions
    + [toMultiSelect];
  FTree.Margins.Right := 0;
  FTree.Images        := FImageList;
  FTree.StateImages   := FImageList;
  BuildTree;
end;

constructor TfrmMessageFilter.Create(AOwner: TComponent;
  ASettings: TMessageListSettings; AImageList: TImageList);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(ASettings, 'ASettings');
  Guard.CheckNotNull(AImageList, 'AImageList');
  FSettings  := ASettings;
  FImageList := AImageList;
end;

procedure TfrmMessageFilter.BeforeDestruction;
begin
  FTree.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMessageFilter.FSettingsChanged(Sender: TObject);
begin
//
end;

procedure TfrmMessageFilter.FTreeChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  FN : TFilterNode;
begin
  FN := Sender.GetNodeData<TFilterNode>(Node);
  if FN.VNode.CheckState.IsChecked then
    FSettings.VisibleMessageTypes := FSettings.VisibleMessageTypes +
      [FN.Data.MessageType]
  else
    FSettings.VisibleMessageTypes := FSettings.VisibleMessageTypes -
      [FN.Data.MessageType]
end;

procedure TfrmMessageFilter.FTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
//
end;

procedure TfrmMessageFilter.FTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  FN : TFilterNode;
begin
  FN := Sender.GetNodeData<TFilterNode>(Node);
  FN.Free;
end;

procedure TfrmMessageFilter.FTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  FN : TFilterNode;
begin
  if Kind in [ikNormal, ikSelected] then
  begin
    FN := Sender.GetNodeData<TFilterNode>(Node);
    if Assigned(FN) and Assigned(FN.Data) then
      ImageIndex := FN.ImageIndex;
  end;
end;

procedure TfrmMessageFilter.FTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  FN : TFilterNode;
begin
  FN := Sender.GetNodeData<TFilterNode>(Node);
  if Assigned(FN) and Assigned(FN.Data) then
    CellText := FN.Data.Caption;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessageFilter.BuildTree;
var
  LNode : TFilterNode;

  function AddNode(
    ACaption     : string;
    AImageIndex  : Integer = -1;
    AMessageType : TLogMessageType = lmtNone
  ): TFilterNode;
  begin
    if Assigned(LNode) then
    begin
      Result := LNode.Add(TFilterData.Create(ACaption, AMessageType));
    end
    else
    begin
      Result := TFilterNode.Create(FTree, TFilterData.Create(ACaption));
    end;
    Result.ImageIndex := AImageIndex;
    Result.CheckType  := ctCheckBox;
    if (AMessageType = lmtNone) or (AMessageType in FSettings.VisibleMessageTypes) then
      Result.CheckState := csCheckedNormal;
  end;

begin
  LNode := nil;
  LNode := AddNode('Notification messages');
  AddNode('Info', 0, lmtInfo);
  AddNode('Warning', 2, lmtWarning);
  AddNode('Error', 1, lmtError);
  LNode := nil;
  LNode := AddNode('Value messages');
  AddNode('Value', 19, lmtValue);
  AddNode('Strings', 8, lmtStrings);
  AddNode('Components', 10, lmtComponent);
  AddNode('Color', 22, lmtColor);
  AddNode('AlphaColor', 22, lmtAlphaColor);
  AddNode('Persistent', 16, lmtPersistent);
  AddNode('Interface', 17, lmtInterface);
  AddNode('Object', 18, lmtObject);
  AddNode('DataSet', 20, lmtDataSet);
  AddNode('Action', 21, lmtAction);
  AddNode('Bitmap', 12, lmtBitmap);
  AddNode('Screenshot', 12, lmtScreenshot);
  AddNode('Exception', 11, lmtException);

  LNode := nil;
  LNode := AddNode('Text messages', 24);
  AddNode('SQL', 0, lmtText);
  AddNode('XML', 0, lmtText);
  AddNode('INI', 0, lmtText);
  AddNode('JSON', 0, lmtText);

  LNode := nil;
  LNode := AddNode('Trace messages');
  AddNode('Checkpoint', 7, lmtCheckpoint);
  AddNode('Counter', 23, lmtCounter);
  LNode := AddNode('Track method', 9);
  AddNode('Enter', 4, lmtEnterMethod);
  AddNode('Leave', 5, lmtLeaveMethod);
  FTree.FullExpand;
end;
{$ENDREGION}

end.
