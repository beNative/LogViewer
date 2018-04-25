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

{
  See this topic for more information:
  https://stackoverflow.com/questions/5365365/tree-like-datastructure-for-use-with-virtualtreeview
}

unit LogViewer.MessageList.LogNode;

interface

uses
  Spring.Collections,

  VirtualTrees;

type
  TLogNode = class
  private
    FText   : string;
    FVTNode : PVirtualNode;
    FNodes  : IList<TLogNode>;

  protected
    function GetNodes: IList<TLogNode>;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetVTNode: PVirtualNode;
    procedure SetVTNode(const Value: PVirtualNode);

  public
    constructor Create(AText: string = '');

    procedure AfterConstruction; override;

    property Nodes: IList<TLogNode>
      read GetNodes;

    property Text: string
      read GetText write SetText;

    property VTNode: PVirtualNode
      read GetVTNode write SetVTNode;
  end;


implementation

{$REGION 'construction and destruction'}
procedure TLogNode.AfterConstruction;
begin
  inherited AfterConstruction;

end;

constructor TLogNode.Create(AText: string);
begin
//
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogNode.GetNodes: IList<TLogNode>;
begin
  if not Assigned(FNodes) then
    FNodes := TCollections.CreateObjectList<TLogNode>;
  Result := FNodes;
end;

function TLogNode.GetText: string;
begin

end;

procedure TLogNode.SetText(const Value: string);
begin

end;

function TLogNode.GetVTNode: PVirtualNode;
begin

end;

procedure TLogNode.SetVTNode(const Value: PVirtualNode);
begin

end;
{$ENDREGION}

end.
