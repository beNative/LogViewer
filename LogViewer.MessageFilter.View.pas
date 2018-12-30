{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

{ User interface for message filter treeview. }

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

    function UpdateChildren(
      ANode       : TFilterNode;
      ACheckState : TCheckState
    ): Boolean;

    function UpdateParent(
      ANode       : TFilterNode;
      ACheckState : TCheckState
    ): Boolean;

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
  System.StrUtils,

  Spring,

  DDuce.Factories.VirtualTrees,

  DDuce.Logger,

  LogViewer.Resources;

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
  I  : Integer;
begin
  FN := Sender.GetNodeData<TFilterNode>(Node);
  if FN.VNode.CheckState.IsChecked then
  begin
    FSettings.VisibleMessageTypes := FSettings.VisibleMessageTypes +
      FN.Data.MessageTypes;
    if MatchText(FN.Data.Caption, ['SQL', 'INI', 'JSON']) then
      FSettings.VisibleValueTypes.Add(FN.Data.Caption);
  end
  else
  begin
    FSettings.VisibleMessageTypes := FSettings.VisibleMessageTypes -
      FN.Data.MessageTypes;
    if MatchText(FN.Data.Caption, ['SQL', 'INI', 'JSON']) then
    begin
      I := FSettings.VisibleValueTypes.IndexOf(FN.Data.Caption);
      if I <> -1 then
      begin
        FSettings.VisibleValueTypes.Delete(I);
      end;
    end;
  end;
  Logger.SendStrings(FSettings.VisibleValueTypes);
  UpdateChildren(FN, FN.VNode.CheckState);
  UpdateParent(FN, FN.VNode.CheckState);
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
    ACaption      : string;
    AImageIndex   : Integer = -1;
    AMessageTypes : TLogMessageTypes = []
  ): TFilterNode;
  begin
    if Assigned(LNode) then
    begin
      Result := LNode.Add(TFilterData.Create(ACaption, AMessageTypes));
    end
    else
    begin
      Result := TFilterNode.Create(FTree, TFilterData.Create(ACaption, AMessageTypes));
    end;
    Result.ImageIndex := AImageIndex;
    Result.CheckType  := ctCheckBox;
    if AMessageTypes * FSettings.VisibleMessageTypes = AMessageTypes then
      Result.CheckState := csCheckedNormal;
  end;

begin
  LNode := nil;
  LNode := AddNode(SNotificationMessages, -1, [lmtInfo, lmtWarning, lmtError]);
  AddNode(SInfo, 0, [lmtInfo]);
  AddNode(SWarning, 2, [lmtWarning]);
  AddNode(SError, 1, [lmtError]);

  LNode := nil;
  LNode := AddNode(SValueMessages, - 1, [lmtValue, lmtStrings, lmtComponent,
    lmtColor, lmtAlphaColor, lmtPersistent, lmtInterface, lmtObject, lmtDataSet,
    lmtAction, lmtBitmap, lmtScreenshot, lmtException]);
  AddNode(SValue, 19, [lmtValue]);
  AddNode(SStrings, 8, [lmtStrings]);
  AddNode(SComponent, 10, [lmtComponent]);
  AddNode(SColor, 22, [lmtColor, lmtAlphaColor]);
  AddNode(SPersistent, 16, [lmtPersistent]);
  AddNode(SInterface, 17, [lmtInterface]);
  AddNode(SObject, 18, [lmtObject]);
  AddNode(SDataSet, 20, [lmtDataSet]);
  AddNode(SAction, 21, [lmtAction]);
  AddNode(SBitmap, 12, [lmtBitmap]);
  AddNode(SScreenshot, 12, [lmtScreenshot]);
  AddNode(SException, 11, [lmtException]);

  LNode := nil;
  LNode := AddNode(STextMessages, 24);
  AddNode('SQL', 27, [lmtText]);
  AddNode('XML', 28, [lmtText]);
  AddNode('INI', 0, [lmtText]);
  AddNode('JSON', 26, [lmtText]);
  FSettings.VisibleValueTypes.Add('SQL');
  FSettings.VisibleValueTypes.Add('XML');
  FSettings.VisibleValueTypes.Add('INI');
  FSettings.VisibleValueTypes.Add('JSON');

  Logger.SendStrings(FSettings.VisibleValueTypes);

  LNode := nil;
  LNode := AddNode(
    STraceMessages,
    25,
    [lmtCheckpoint, lmtCounter, lmtEnterMethod, lmtLeaveMethod]
  );
  AddNode(SCheckpoint, 7, [lmtCheckpoint]);
  AddNode(SCounter, 23, [lmtCounter]);
  LNode := AddNode(STrackMethod, 9, [lmtEnterMethod, lmtLeaveMethod]);
  AddNode(SEnter, 4, [lmtEnterMethod]);
  AddNode(SLeave, 5, [lmtLeaveMethod]);
  FTree.FullExpand;
end;

{ Updates checkbox state of children if applicable. }

function TfrmMessageFilter.UpdateChildren(ANode: TFilterNode;
  ACheckState: TCheckState): Boolean;
var
  N : TFilterNode;
  I : Integer;
begin
  if ANode.ChildCount > 0 then
  begin
    for I := 0 to ANode.ChildCount - 1 do
    begin
      N := ANode.Items[I];
      N.CheckState := ACheckState;
      FTree.RepaintNode(N.VNode);
      UpdateChildren(N, ACheckState);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function TfrmMessageFilter.UpdateParent(ANode: TFilterNode;
  ACheckState: TCheckState): Boolean;
var
  LFirstChild : PVirtualNode;
  LLastChild  : PVirtualNode;
  N           : PVirtualNode;
  B           : Boolean;
begin
  Logger.Track(Self, 'UpdateParent');
  if Assigned(ANode) and Assigned(ANode.VNode) and Assigned(ANode.VNode.Parent) then
  begin
    LFirstChild := ANode.VNode.Parent.FirstChild;
    LLastChild  := ANode.VNode.Parent.LastChild;
    N := LFirstChild;
    B := Assigned(N) and (N.CheckState = ACheckState);
    while B and (N <> LLastChild) do
    begin
      N := N.NextSibling;
      B := B and Assigned(N) and (N.CheckState = ACheckState);
    end;
    if B then
    begin
      ANode.VNode.Parent.CheckState := ACheckState;
    end
    else
    begin
      ANode.VNode.Parent.CheckState := csMixedNormal;
    end;
    FTree.RepaintNode(ANode.VNode.Parent);
    Result := B;
  end
  else
    Result := False;
end;
{$ENDREGION}

end.
