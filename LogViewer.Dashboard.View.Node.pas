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
    FLogQueue   : ILogQueue;
    FSubscriber : ISubscriber;
    FVTree      : TVirtualStringTree;

  protected
    {$REGION 'property access methods'}
    function GetSubscriber: ISubscriber;
    procedure SetSubscriber(const Value: ISubscriber);
    function GetVTree: TVirtualStringTree;
    function GetCount: Integer;
    function GetLogQueue: ILogQueue;
    procedure SetLogQueue(const Value: ILogQueue);
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
      ASubscriber : ISubscriber = nil;
      ALogQueue   : ILogQueue = nil
    );
    procedure BeforeDestruction; override;

    procedure FReceiverSubscriberListChanged(
      Sender     : TObject;
      const Item : ISubscriber;
      Action     : TCollectionChangedAction
    );

    procedure FReceiverLogQueueListChanged(
      Sender     : TObject;
      const Item : ILogQueue;
      Action     : TCollectionChangedAction
    );

    property Nodes: IList<TDashboardNode>
      read GetNodes;

    property Count: Integer
      read GetCount;

    property Receiver: IChannelReceiver
      read GetReceiver write SetReceiver;

    property LogQueue: ILogQueue
      read GetLogQueue write SetLogQueue;

    property Subscriber: ISubscriber
      read GetSubscriber write SetSubscriber;

    property VTNode: PVirtualNode
      read GetVTNode write SetVTNode;

    property VTree: TVirtualStringTree
      read GetVTree;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
constructor TDashboardNode.Create(AVTNode: PVirtualNode;
  AVTree: TVirtualStringTree; AReceiver: IChannelReceiver;
  ASubscriber: ISubscriber; ALogQueue: ILogQueue);
begin
  FNodes.Create(function: IList<TDashboardNode>
    begin
      Result := TCollections.CreateObjectList<TDashboardNode>(False);
    end
  );
  FVTNode   := AVTNode;
  FVTree    := AVTree;
  if not Assigned(ALogQueue) and not Assigned(ASubscriber) then
    Receiver  := AReceiver;
  FLogQueue   := ALogQueue;
  FSubscriber := ASubscriber;
end;

procedure TDashboardNode.BeforeDestruction;
begin
//  FVTree    := nil;
//  FVTNode   := nil;
//  FLogQueue := nil;
//  FReceiver := nil;
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

function TDashboardNode.GetLogQueue: ILogQueue;
begin
  Result := FLogQueue;
end;

procedure TDashboardNode.SetLogQueue(const Value: ILogQueue);
begin
  FLogQueue := Value;
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
      FReceiver.LogQueueList.OnValueChanged.Add(FReceiverLogQueueListChanged);
      FReceiver.SubscriberList.OnChanged.Add(FReceiverSubscriberListChanged);
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
procedure TDashboardNode.FReceiverLogQueueListChanged(Sender: TObject;
  const Item : ILogQueue; Action: TCollectionChangedAction);
var
  DN : TDashboardNode;
begin
  if Action = caAdded then
  begin
    for DN in Nodes do
    begin
      if Item.SourceName.Contains(DN.Subscriber.Port)
      and Item.SourceName.Contains(DN.Subscriber.Address) then
        DN.FLogQueue := Item;


    end;

    //DN := TDashboardNode.Create(nil, FVTree, FReceiver, nil, Item);
    //DN.VTNode := FVTree.AddChild(FVTNode, DN);
    //Nodes.Add(DN);
  end;
end;

procedure TDashboardNode.FReceiverSubscriberListChanged(Sender: TObject;
  const Item: ISubscriber; Action: TCollectionChangedAction);
var
  DN : TDashboardNode;
begin
  if Action = caAdded then
  begin
    DN := TDashboardNode.Create(nil, FVTree, FReceiver, Item);
    DN.VTNode := FVTree.AddChild(FVTNode, DN);

   DN.VTNode.CheckType := ctCheckBox;
  if Item.Enabled then
    DN.VTNode.CheckState := csCheckedNormal
  else
    DN.VTNode.CheckState := csUncheckedNormal;


    Nodes.Add(DN);
  end;
end;

{$ENDREGION}

end.

//[dcc32 Error] LogViewer.Dashboard.View.Node.pas(216): E2010 Incompatible types: 'ILogQueue' and
//'System.Generics.Collections.TPair<System.Integer,LogViewer.Interfaces.ILogQueue>'
