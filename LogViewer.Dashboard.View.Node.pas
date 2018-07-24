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
  System.Classes,

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
    FVTNode   : PVirtualNode;
    FNodes    : Lazy<IList<TDashboardNode>>;
    FReceiver : IChannelReceiver;
    FLogQueue : ILogQueue;
    FVTree    : TVirtualStringTree;

  protected
    {$REGION 'property access methods'}
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
      AVTNode   : PVirtualNode;
      AVTree    : TVirtualStringTree;
      AReceiver : IChannelReceiver;
      ALogQueue : ILogQueue = nil
    );
    procedure BeforeDestruction; override;

    procedure FChannelReceiverNewLogQueue(
      Sender    : TObject;
      ALogQueue : ILogQueue
    );

    property Nodes: IList<TDashboardNode>
      read GetNodes;

    property Count: Integer
      read GetCount;

    property Receiver: IChannelReceiver
      read GetReceiver write SetReceiver;

    property LogQueue: ILogQueue
      read GetLogQueue write SetLogQueue;

    property VTNode: PVirtualNode
      read GetVTNode write SetVTNode;

    property VTree: TVirtualStringTree
      read GetVTree;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TDashboardNode.Create(AVTNode: PVirtualNode;
  AVTree: TVirtualStringTree; AReceiver: IChannelReceiver; ALogQueue: ILogQueue);
begin
  FNodes.Create(function: IList<TDashboardNode>
    begin
      Result := TCollections.CreateObjectList<TDashboardNode>(False);
    end
  );
  FVTNode   := AVTNode;
  FVTree    := AVTree;
  if not Assigned(ALogQueue) then
    Receiver  := AReceiver;
  FLogQueue := ALogQueue;
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
      FReceiver.OnNewLogQueue.Add(FChannelReceiverNewLogQueue);
    end;
  end;
end;

function TDashboardNode.GetVTNode: PVirtualNode;
begin
  Result := FVTNode;
end;

function TDashboardNode.GetVTree: TVirtualStringTree;
begin
  Result := FVTree;
end;

procedure TDashboardNode.SetVTNode(const Value: PVirtualNode);
begin
  FVTNode := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TDashboardNode.FChannelReceiverNewLogQueue(Sender: TObject;
  ALogQueue: ILogQueue);
var
  DN : TDashboardNode;
begin
  DN := TDashboardNode.Create(nil, FVTree, FReceiver, ALogQueue);
  DN.VTNode := FVTree.AddChild(FVTNode, DN);
  Nodes.Add(DN);



//end;


end;
{$ENDREGION}

end.
