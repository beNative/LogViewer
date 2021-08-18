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
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  VirtualTrees, kpagecontrol, kcontrols,

  DDuce.Logger.Interfaces, DDuce.Components.VirtualTrees.Node,

  LogViewer.MessageList.Settings, LogViewer.MessageFilter.Data;

type
  TFilterNode = TVTNode<TFilterData>;

type
  TfrmMessageFilter = class(TForm)
    pnlMessageFilter : TPanel;
    pgcMain          : TKPageControl;
    tsClientSide     : TKTabSheet;
    tsSourceSide     : TKTabSheet;

  private
    FCSTree    : TVirtualStringTree;
    FSSTree    : TVirtualStringTree;
    FImageList : TImageList;
    FSettings  : TMessageListSettings;

  protected
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

    procedure BuildTree(
      ATree         : TVirtualStringTree;
      AIsSourceSide : Boolean = False
    );
    function AddMessageTypeNode(
      ATree         : TVirtualStringTree;
      ACaption      : string;
      AImageIndex   : Integer = -1;
      AMessageTypes : TLogMessageTypes = [];
      AParentNode   : TFilterNode = nil
    ): TFilterNode;
    function AddMessageLevelNode(
      ATree         : TVirtualStringTree;
      ACaption       : string;
      AColor         : TColor;
      AMessageLevels : TLogMessageLevels = [];
      AParentNode    : TFilterNode = nil
    ): TFilterNode;

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
      AOwner     : TComponent;
      ASettings  : TMessageListSettings;
      AImageList : TImageList
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
  ASettings: TMessageListSettings; AImageList: TImageList);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(ASettings, 'ASettings');
  Guard.CheckNotNull(AImageList, 'AImageList');
  FSettings  := ASettings;
  FSettings.OnChanged.Add(FSettingsChanged);
  FImageList := AImageList;
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
      if MatchText(FN.Data.Caption, [SSql, SXml, SIni, SJson]) then
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
    if MatchText(FN.Data.Caption, [SSql, SXml, SIni, SJson]) then
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
    if MatchText(FN.Data.Caption, [SSql, SXml, SIni, SJson]) then
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
function TfrmMessageFilter.AddMessageLevelNode(ATree: TVirtualStringTree;
  ACaption: string; AColor: TColor; AMessageLevels: TLogMessageLevels;
  AParentNode: TFilterNode): TFilterNode;
begin
  if Assigned(AParentNode) then
  begin
    Result := AParentNode.Add(TFilterData.Create(ACaption, AMessageLevels));
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

function TfrmMessageFilter.AddMessageTypeNode(ATree: TVirtualStringTree;
  ACaption: string; AImageIndex: Integer; AMessageTypes: TLogMessageTypes;
  AParentNode: TFilterNode): TFilterNode;
begin
  if Assigned(AParentNode) then
  begin
    Result := AParentNode.Add(TFilterData.Create(ACaption, AMessageTypes));
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

procedure TfrmMessageFilter.BuildTree(ATree: TVirtualStringTree;
  AIsSourceSide: Boolean);
var
  LNode            : TFilterNode;
  LLogMessageLevel : TLogMessageLevel;
begin
  LNode := AddMessageTypeNode(
    ATree,
    SNotificationMessages, -1, [lmtInfo, lmtWarning, lmtError]
  );
  AddMessageTypeNode(ATree, SInfo, IMGIDX_INFO, [lmtInfo], LNode);
  AddMessageTypeNode(ATree, SWarning, IMGIDX_WARNING, [lmtWarning], LNode);
  AddMessageTypeNode(ATree, SError, IMGIDX_ERROR, [lmtError], LNode);

  LNode := AddMessageTypeNode(
    ATree,
    SValueMessages,
    - 1,
    [lmtValue, lmtStrings, lmtComponent, lmtColor, lmtAlphaColor, lmtPersistent,
     lmtInterface, lmtObject, lmtDataSet, lmtAction, lmtBitmap, lmtScreenshot,
     lmtException]
  );
  AddMessageTypeNode(ATree, SValue, IMGIDX_VALUE, [lmtValue], LNode);
  AddMessageTypeNode(ATree, SStrings, IMGIDX_STRINGS, [lmtStrings], LNode);
  AddMessageTypeNode(
    ATree, SComponent, IMGIDX_COMPONENT, [lmtComponent], LNode
  );
  AddMessageTypeNode(
    ATree, SColor, IMGIDX_COLOR, [lmtColor, lmtAlphaColor], LNode
  );
  AddMessageTypeNode(
    ATree, SPersistent, IMGIDX_PERSISTENT, [lmtPersistent], LNode
  );
  AddMessageTypeNode(
    ATree, SInterface, IMGIDX_INTERFACE, [lmtInterface], LNode
  );
  AddMessageTypeNode(ATree, SObject, IMGIDX_OBJECT, [lmtObject], LNode);
  AddMessageTypeNode(ATree, SDataSet, IMGIDX_DATASET, [lmtDataSet], LNode);
  AddMessageTypeNode(ATree, SAction, IMGIDX_ACTION, [lmtAction], LNode);
  AddMessageTypeNode(ATree, SBitmap, IMGIDX_BITMAP, [lmtBitmap], LNode);
  AddMessageTypeNode(
    ATree, SScreenshot, IMGIDX_SCREENSHOT, [lmtScreenshot], LNode
  );
  AddMessageTypeNode(
    ATree, SException, IMGIDX_EXCEPTION, [lmtException], LNode
  );

  LNode := AddMessageTypeNode(ATree, STextMessages, 24, [lmtText]);
  if not AIsSourceSide then
  begin
    AddMessageTypeNode(ATree, SSql, IMGIDX_SQL, [lmtText], LNode);
    AddMessageTypeNode(ATree, SXml, IMGIDX_XML, [lmtText], LNode);
    AddMessageTypeNode(ATree, SIni, IMGIDX_INI, [lmtText], LNode);
    AddMessageTypeNode(ATree, SJson, IMGIDX_JSON, [lmtText], LNode);
    FSettings.VisibleValueTypes.Add(SSql);
    FSettings.VisibleValueTypes.Add(SXml);
    FSettings.VisibleValueTypes.Add(SIni);
    FSettings.VisibleValueTypes.Add(SJson);
  end;

  LNode := AddMessageTypeNode(
    ATree,
    STraceMessages,
    IMGIDX_TRACE,
    [lmtCheckpoint, lmtCounter, lmtEnterMethod, lmtLeaveMethod]
  );
  AddMessageTypeNode(
    ATree, SCheckpoint, IMGIDX_CHECKPOINT, [lmtCheckpoint], LNode
  );

  LNode := AddMessageTypeNode(
    ATree, STrackMethod, IMGIDX_TRACKMETHOD, [lmtEnterMethod, lmtLeaveMethod]
  );
  AddMessageTypeNode(ATree, SEnter, IMGIDX_ENTER, [lmtEnterMethod], LNode);
  AddMessageTypeNode(ATree, SLeave, IMGIDX_LEAVE, [lmtLeaveMethod], LNode);

  LNode := AddMessageLevelNode(ATree, SLogLevels, clNone, AllLevels);
  for LLogMessageLevel := Low(TLogMessageLevel) to High(TLogMessageLevel) do
  begin
    AddMessageLevelNode(
      ATree, Integer(LLogMessageLevel).ToString, clRed, [LLogMessageLevel], LNode
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
      N := ANode.Items[I];
      N.CheckState := ACheckState;
      ATree.InvalidateNode(N.VNode);
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
  if Assigned(ANode) and Assigned(ANode.VNode)
    and Assigned(ANode.VNode.Parent) then
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
