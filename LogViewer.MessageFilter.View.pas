{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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
  System.SysUtils, System.Variants, System.Classes, System.UITypes,
  System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ImageCollection, Vcl.ImgList, Vcl.VirtualImageList,

  VirtualTrees, VirtualTrees.BaseTree, kpagecontrol, kcontrols,

  DDuce.Logger.Interfaces, DDuce.Components.VirtualTrees.Node,

  LogViewer.MessageList.Settings, LogViewer.MessageFilter.Data;

type
  TFilterData = LogViewer.MessageFilter.Data.TFilterData;
  TFilterNode = TVTNode<TFilterData>;

type
  TfrmMessageFilter = class(TForm)
    pnlMessageFilter : TPanel;
    pgcMain          : TKPageControl;
    tsClientSide     : TKTabSheet;
    tsSourceSide     : TKTabSheet;
    imlMain          : TVirtualImageList;

  private
    FCSTree    : TVirtualStringTree;
    FSSTree    : TVirtualStringTree;
    FImageList : TVirtualImageList;
    FSettings  : TMessageListSettings;

  protected
    procedure BuildTree(
      ATree         : TVirtualStringTree;
      AIsSourceSide : Boolean = False
    );

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
    procedure FTreeFreeNode(
      Sender: TBaseVirtualTree;
      Node: PVirtualNode
    );
    procedure FTreeChecked(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );
    procedure FTreeBeforeGetCheckState(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode
    );

    procedure FSettingsChanged(Sender: TObject);
    {$ENDREGION}

    function UpdateChildren(
      ATree       : TBaseVirtualTree;
      ANode       : TFilterNode;
      ACheckState : TCheckState
    ): Boolean;

    function UpdateParent(
      ATree       : TBaseVirtualTree;
      ANode       : TFilterNode;
      ACheckState : TCheckState
    ): Boolean;

  public
    constructor Create(
      AOwner           : TComponent;
      ASettings        : TMessageListSettings;
      AImageCollection : TImageCollection
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,

  VirtualTrees.Types, VirtualTrees.Header,

  Spring,

  DDuce.Factories.VirtualTrees,
  DDuce.Logger,

  LogViewer.Resources;

{$REGION 'construction and destruction'}
procedure TfrmMessageFilter.AfterConstruction;
begin
  inherited AfterConstruction;
  FCSTree := TVirtualStringTreeFactory.CreateTree(Self, tsClientSide);
  FCSTree.OnGetText             := FTreeGetText;
  FCSTree.OnGetImageIndex       := FTreeGetImageIndex;
  FCSTree.OnFreeNode            := FTreeFreeNode;
  FCSTree.OnChecked             := FTreeChecked;
  FCSTree.OnBeforeGetCheckState := FTreeBeforeGetCheckState;

  FCSTree.Header.Options               := FCSTree.Header.Options - [hoVisible];
  FCSTree.TreeOptions.PaintOptions     := FCSTree.TreeOptions.PaintOptions
    + [toShowTreeLines];
  FCSTree.TreeOptions.SelectionOptions := FCSTree.TreeOptions.SelectionOptions
    + [toMultiSelect];
  FCSTree.Margins.Right := 0;
  FCSTree.Images        := FImageList;
  FCSTree.StateImages   := FImageList;
  BuildTree(FCSTree);

  FSSTree := TVirtualStringTreeFactory.CreateTree(Self, tsSourceSide);
  FSSTree.OnGetText             := FTreeGetText;
  FSSTree.OnGetImageIndex       := FTreeGetImageIndex;
  FSSTree.OnFreeNode            := FTreeFreeNode;
  FSSTree.OnChecked             := FTreeChecked;
  FSSTree.OnBeforeGetCheckState := FTreeBeforeGetCheckState;

  FSSTree.Header.Options               := FSSTree.Header.Options - [hoVisible];
  FSSTree.TreeOptions.PaintOptions     := FSSTree.TreeOptions.PaintOptions
    + [toShowTreeLines];
  FSSTree.TreeOptions.SelectionOptions := FSSTree.TreeOptions.SelectionOptions
    + [toMultiSelect];
  FSSTree.Margins.Right := 0;
  FSSTree.Images        := FImageList;
  FSSTree.StateImages   := FImageList;
  BuildTree(FSSTree, True);
end;

constructor TfrmMessageFilter.Create(AOwner: TComponent;
  ASettings: TMessageListSettings; AImageCollection: TImageCollection);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(ASettings, 'ASettings');
  Guard.CheckNotNull(AImageCollection, 'AImageList');
  FSettings  := ASettings;
  FSettings.OnChanged.Add(FSettingsChanged);
  FImageList := imlMain;
  FImageList.ImageCollection := AImageCollection;
end;

destructor TfrmMessageFilter.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FSettings.OnChanged.RemoveAll(Self);
  FSettings := nil;
  FCSTree.Free;
  FSSTree.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMessageFilter.FSettingsChanged(Sender: TObject);
begin
  if not FCSTree.Focused then
  begin
    FCSTree.Refresh;
  end;
end;

{$REGION 'FTree'}
procedure TfrmMessageFilter.FTreeBeforeGetCheckState(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  FN            : TFilterNode;
  I             : Integer;
  LUpdateParent : Boolean;
begin
  LUpdateParent := True;
  FN := Sender.GetNodeData<TFilterNode>(Node);
  if FN.Level = 1 then
  begin
    if FN.Data.IsMessageTypeFilter then
    begin
      if MatchText(FN.Data.Caption, ['SQL', 'XML', 'INI', 'JSON']) then
      begin
        I := FSettings.VisibleValueTypes.IndexOf(FN.Data.Caption);
        if I <> -1 then
        begin
          Node.CheckState := csCheckedNormal
        end
        else
        begin
          Node.CheckState := csUncheckedNormal;
        end;
        LUpdateParent := False;
      end
      else if FN.Data.MessageTypes * FSettings.VisibleMessageTypes
        = FN.Data.MessageTypes then
        Node.CheckState := csCheckedNormal
      else
        Node.CheckState := csUncheckedNormal;
    end;
    if FN.Data.IsMessageLevelFilter then
    begin
      if FN.Data.MessageLevels * FSettings.VisibleMessageLevels
        = FN.Data.MessageLevels then
        Node.CheckState := csCheckedNormal
      else
        Node.CheckState := csUncheckedNormal;
    end;
    if LUpdateParent then
      UpdateParent(Sender, FN, Node.CheckState);
  end;
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
    if MatchText(FN.Data.Caption, ['SQL', 'XML', 'INI', 'JSON']) then
    begin
      FSettings.VisibleValueTypes.Add(FN.Data.Caption);
    end
    else
    begin
      FSettings.VisibleMessageTypes := FSettings.VisibleMessageTypes +
        FN.Data.MessageTypes;
    end;
    FSettings.VisibleMessageLevels := FSettings.VisibleMessageLevels +
      FN.Data.MessageLevels;
  end
  else
  begin
    if MatchText(FN.Data.Caption, ['SQL', 'XML', 'INI', 'JSON']) then
    begin
      I := FSettings.VisibleValueTypes.IndexOf(FN.Data.Caption);
      if I <> -1 then
      begin
        FSettings.VisibleValueTypes.Delete(I);
      end;
    end
    else
    begin
      FSettings.VisibleMessageTypes := FSettings.VisibleMessageTypes -
        FN.Data.MessageTypes;
    end;
    FSettings.VisibleMessageLevels := FSettings.VisibleMessageLevels -
      FN.Data.MessageLevels;
  end;
  FSettings.Changed; // not automatically triggered!
  UpdateChildren(Sender, FN, FN.VNode.CheckState);
  if FN.Level > 0 then
    UpdateParent(Sender, FN, FN.VNode.CheckState);
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
  begin
    CellText := FN.Data.Caption;
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMessageFilter.BuildTree(ATree: TVirtualStringTree;
  AIsSourceSide: Boolean);
var
  LNode            : TFilterNode;
  LLogMessageLevel : TLogMessageLevel;

  function AddMessageTypeNode(
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
      Result := TFilterNode.Create(
        ATree, TFilterData.Create(ACaption, AMessageTypes)
      );
    end;
    Result.ImageIndex := AImageIndex;
    Result.CheckType  := ctCheckBox;
    if AMessageTypes * FSettings.VisibleMessageTypes = AMessageTypes then
      Result.CheckState := csCheckedNormal
    else
      Result.CheckState := csUncheckedNormal;

    if Result.Level = 1 then
    begin
      UpdateParent(ATree, Result, Result.CheckState);
    end;
  end;

  function AddMessageLevelNode(
    ACaption       : string;
    AColor         : TColor;
    AMessageLevels : TLogMessageLevels = []
  ): TFilterNode;
  begin
    if Assigned(LNode) then
    begin
      Result := LNode.Add(TFilterData.Create(ACaption, AMessageLevels));
    end
    else
    begin
      Result := TFilterNode.Create(
        ATree, TFilterData.Create(ACaption, AMessageLevels)
      );
    end;
    Result.Data.Color := AColor;
    Result.CheckType  := ctCheckBox;
    if AColor = clNone then // root node
      Result.ImageIndex := 29
    else
      Result.ImageIndex := -1;
    if AMessageLevels * FSettings.VisibleMessageLevels = AMessageLevels then
      Result.CheckState := csCheckedNormal
    else
      Result.CheckState := csUncheckedNormal;

    if Result.Level = 1 then
    begin
      UpdateParent(ATree, Result, Result.CheckState);
    end;
  end;

begin
  LNode := nil;
  LNode := AddMessageTypeNode(
    SNotificationMessages, -1, [lmtInfo, lmtWarning, lmtError]
  );
  AddMessageTypeNode(SInfo, 0, [lmtInfo]);
  AddMessageTypeNode(SWarning, 2, [lmtWarning]);
  AddMessageTypeNode(SError, 1, [lmtError]);

  LNode := nil;
  LNode := AddMessageTypeNode(SValueMessages, - 1, [lmtValue, lmtStrings,
    lmtComponent, lmtColor, lmtAlphaColor, lmtPersistent, lmtInterface,
    lmtObject, lmtDataSet, lmtAction, lmtBitmap, lmtScreenshot, lmtException]);
  AddMessageTypeNode(SValue, 19, [lmtValue]);
  AddMessageTypeNode(SStrings, 8, [lmtStrings]);
  AddMessageTypeNode(SComponent, 10, [lmtComponent]);
  AddMessageTypeNode(SColor, 22, [lmtColor, lmtAlphaColor]);
  AddMessageTypeNode(SPersistent, 16, [lmtPersistent]);
  AddMessageTypeNode(SInterface, 17, [lmtInterface]);
  AddMessageTypeNode(SObject, 18, [lmtObject]);
  AddMessageTypeNode(SDataSet, 20, [lmtDataSet]);
  AddMessageTypeNode(SAction, 21, [lmtAction]);
  AddMessageTypeNode(SBitmap, 12, [lmtBitmap]);
  AddMessageTypeNode(SScreenshot, 12, [lmtScreenshot]);
  AddMessageTypeNode(SException, 11, [lmtException]);

  LNode := nil;
  LNode := AddMessageTypeNode(STextMessages, 24, [lmtText]);
  if not AIsSourceSide then
  begin
    AddMessageTypeNode('SQL', 27, [lmtText]);
    AddMessageTypeNode('XML', 28, [lmtText]);
    AddMessageTypeNode('INI', 30, [lmtText]);
    AddMessageTypeNode('JSON', 26, [lmtText]);
    FSettings.VisibleValueTypes.Add('SQL');
    FSettings.VisibleValueTypes.Add('XML');
    FSettings.VisibleValueTypes.Add('INI');
    FSettings.VisibleValueTypes.Add('JSON');
  end;

  LNode := nil;
  LNode := AddMessageTypeNode(
    STraceMessages,
    25,
    [lmtCheckpoint, lmtCounter, lmtEnterMethod, lmtLeaveMethod]
  );
  AddMessageTypeNode(SCheckpoint, 7, [lmtCheckpoint]);
  LNode := AddMessageTypeNode(STrackMethod, 9, [lmtEnterMethod, lmtLeaveMethod]);
  AddMessageTypeNode(SEnter, 4, [lmtEnterMethod]);
  AddMessageTypeNode(SLeave, 5, [lmtLeaveMethod]);

  LNode := nil;
  LNode := AddMessageLevelNode(SLogLevels, clNone, AllLevels);
  for LLogMessageLevel := Low(TLogMessageLevel) to High(TLogMessageLevel) do
  begin
    AddMessageLevelNode(
      Integer(LLogMessageLevel).ToString, clRed, [LLogMessageLevel]
    );
  end;

  ATree.FullExpand;
end;

{ Updates checkbox state of children if applicable. }

function TfrmMessageFilter.UpdateChildren(ATree: TBaseVirtualTree;
  ANode: TFilterNode; ACheckState: TCheckState): Boolean;
var
  N : TFilterNode;
  I : Integer;
begin
  if ANode.ChildCount > 0 then
  begin
    for I := 0 to ANode.ChildCount - 1 do
    begin
      N := TFilterNode(ANode.Items[I]);
//      Guard.CheckNotNull(N, 'N');
//      Guard.CheckNotNull(N.VNode, 'N.VNode');
      N.CheckState := ACheckState;
      //ATree.InvalidateNode(N.VNode);
      UpdateChildren(ATree, N, ACheckState);
    end;
    Result := True;
  end
  else
    Result := False;
end;

{ Update checkstate of parent node between csChecked, csUnchecked and
  csMixedNormal. }

function TfrmMessageFilter.UpdateParent(ATree: TBaseVirtualTree;
  ANode: TFilterNode; ACheckState: TCheckState): Boolean;
var
  LFirstChild : PVirtualNode;
  LLastChild  : PVirtualNode;
  N           : PVirtualNode;
  B           : Boolean;
begin
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
    ATree.InvalidateNode(ANode.VNode.Parent);
    Result := B;
  end
  else
    Result := False;
end;
{$ENDREGION}

end.
