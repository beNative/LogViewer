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

unit LogViewer.Dashboard.View.Node;

interface

uses
  System.Classes, System.Generics.Collections,
  Vcl.Forms,

  Spring, Spring.Collections,

  VirtualTrees,

  DDuce.Logger.Interfaces,

  LogViewer.Interfaces;

{$REGION 'documentation'}
{
  See this topic for more information on creating a datastructure for tree
  nodes:

  https://stackoverflow.com/questions/5365365/tree-like-datastructure-for-use-with-virtualtreeview
}
{$ENDREGION}

type
  TDashboardNode = class
  private
    FVTNode     : PVirtualNode;
    FNodes      : Lazy<IList<TDashboardNode>>;
    FReceiver   : IChannelReceiver;
    FSubscriber : ISubscriber;
    FVTree      : TVirtualStringTree;
    FTreeForm   : TForm; // form owning the treeview control

  protected
    {$REGION 'property access methods'}
    function GetSubscriber: ISubscriber;
    procedure SetSubscriber(const Value: ISubscriber);
    function GetVTree: TVirtualStringTree;
    function GetCount: Integer;
    function GetReceiver: IChannelReceiver;
    procedure SetReceiver(const Value: IChannelReceiver);
    function GetNodes: IList<TDashboardNode>;
    function GetVTNode: PVirtualNode;
    procedure SetVTNode(const Value: PVirtualNode);
    {$ENDREGION}

  public
    constructor Create(
      AVTNode     : PVirtualNode;
      AVTree      : TVirtualStringTree;
      AReceiver   : IChannelReceiver;
      ASubscriber : ISubscriber = nil
    );
    procedure BeforeDestruction; override;

    procedure FReceiverSubscriberListChanged(
      Sender     : TObject;
      const Item : ISubscriber;
      Action     : TCollectionChangedAction
    );

    procedure FSubscriberReceiveMessage(
      Sender    : TObject;
      AStream   : TStream
    );

    property Nodes: IList<TDashboardNode>
      read GetNodes;

    property Count: Integer
      read GetCount;

    property Receiver: IChannelReceiver
      read GetReceiver write SetReceiver;

    property Subscriber: ISubscriber
      read GetSubscriber write SetSubscriber;

    property VTNode: PVirtualNode
      read GetVTNode write SetVTNode;

    property VTree: TVirtualStringTree
      read GetVTree;
  end;

implementation

uses
  System.SysUtils,

  DDuce.Logger, DDuce.Utils;

{$REGION 'construction and destruction'}
constructor TDashboardNode.Create(AVTNode: PVirtualNode;
  AVTree: TVirtualStringTree; AReceiver: IChannelReceiver;
  ASubscriber: ISubscriber);
begin
  FNodes.Create(function: IList<TDashboardNode>
    begin
      Result := TCollections.CreateObjectList<TDashboardNode>(False);
    end
  );
  FVTNode   := AVTNode;
  FVTree    := AVTree;
  if Assigned(FVTree) and Assigned(FVTree.Owner) and (FVTree.Owner is TForm) then
    FTreeForm := TForm(FVTree.Owner);
  if not Assigned(ASubscriber) then
    Receiver  := AReceiver;
  Subscriber := ASubscriber;
end;

procedure TDashboardNode.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FVTree      := nil;
  FVTNode     := nil;
  FReceiver   := nil;
  FSubscriber := nil;
  FTreeForm   := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TDashboardNode.GetCount: Integer;
begin
  if FNodes.IsValueCreated then
    Result := Nodes.Count
  else
    Result := 0;
end;

function TDashboardNode.GetNodes: IList<TDashboardNode>;
begin
  Result := FNodes.Value;
end;

function TDashboardNode.GetReceiver: IChannelReceiver;
begin
  Result := FReceiver;
end;

procedure TDashboardNode.SetReceiver(const Value: IChannelReceiver);
begin
  if Value <> Receiver then
  begin
    Nodes.Clear;
    FReceiver := Value;
    if Assigned(FReceiver) then
    begin
      FReceiver.SubscriberList.OnValueChanged.Add(FReceiverSubscriberListChanged);
    end;
  end;
end;

function TDashboardNode.GetSubscriber: ISubscriber;
begin
  Result := FSubscriber;
end;

procedure TDashboardNode.SetSubscriber(const Value: ISubscriber);
begin
  FSubscriber := Value;
  if Assigned(FSubscriber) then
  begin
    FSubscriber.OnReceiveMessage.Add(FSubscriberReceiveMessage);
  end;
end;

function TDashboardNode.GetVTNode: PVirtualNode;
begin
  Result := FVTNode;
end;

procedure TDashboardNode.SetVTNode(const Value: PVirtualNode);
begin
  FVTNode := Value;
end;

function TDashboardNode.GetVTree: TVirtualStringTree;
begin
  Result := FVTree;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TDashboardNode.FReceiverSubscriberListChanged(Sender: TObject;
  const Item: ISubscriber; Action: TCollectionChangedAction);
var
  DN : TDashboardNode;
begin

  if Action = caAdded then
  begin
//    for DN in Nodes do
//    begin
//      if Item.SourceName.Contains(DN.Subscriber.Port)
//      and Item.SourceName.Contains(DN.Subscriber.Address) then
//        DN.FLogQueue := Item;
//    end;

    DN := TDashboardNode.Create(nil, FVTree, FReceiver, Item);
    DN.VTNode := FVTree.AddChild(FVTNode, DN);
    DN.VTNode.CheckType := ctCheckBox;
    if Item.Enabled then
      DN.VTNode.CheckState := csCheckedNormal
    else
      DN.VTNode.CheckState := csUncheckedNormal;
    Nodes.Add(DN);
    FVTree.FullExpand;
  end;
end;

procedure TDashboardNode.FSubscriberReceiveMessage(Sender: TObject;
  AStream: TStream);
begin
  // this is a optimalisation to redraw treeview nodes only when needed.
  if Assigned(FTreeForm) and ContainsFocus(FTreeForm) then
  begin
    FVTree.InvalidateNode(FVTNode);
  end;
end;
{$ENDREGION}

end.

