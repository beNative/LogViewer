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

unit LogViewer.Settings.Dialog.ConfigNode;

interface

{
  See this topic for more information:
  https://stackoverflow.com/questions/5365365/tree-like-datastructure-for-use-with-virtualtreeview
}

uses
  Vcl.ComCtrls,
  Spring, Spring.Collections,

  VirtualTrees;

type
  TConfigNode = class
  private
    FText      : string;
    FVTNode    : PVirtualNode;
    FNodes     : Lazy<IList<TConfigNode>>;
    FTabSheet  : TTabSheet;

  protected
    {$REGION 'property access methods'}
    function GetNodes: IList<TConfigNode>;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetVTNode: PVirtualNode;
    procedure SetVTNode(const Value: PVirtualNode);
    {$ENDREGION}

  public
    constructor Create(
      const AText : string = '';
      ATabSheet   : TTabSheet = nil
    );

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Nodes: IList<TConfigNode>
      read GetNodes;

    property Text: string
      read GetText write SetText;

    property TabSheet: TTabSheet
      read FTabSheet write FTabSheet;

    property VTNode : PVirtualNode
      read GetVTNode write SetVTNode;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TConfigNode.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes.Create(
    function: IList<TConfigNode>
    begin
      Result := TCollections.CreateObjectList<TConfigNode>(False);
    end,
    True
  );
end;

procedure TConfigNode.BeforeDestruction;
begin
  FNodes := nil;
  FTabSheet := nil;
  inherited BeforeDestruction;
end;

constructor TConfigNode.Create(const AText: string; ATabSheet: TTabSheet);
begin
  inherited Create;
  FText     := AText;
  FTabSheet := ATabSheet;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TConfigNode.GetNodes: IList<TConfigNode>;
begin
  Result := FNodes;
end;

function TConfigNode.GetText: string;
begin
  Result := FText;
end;

procedure TConfigNode.SetText(const Value: string);
begin
  FText := Value;
end;

function TConfigNode.GetVTNode: PVirtualNode;
begin
  Result := FVTNode;
end;

procedure TConfigNode.SetVTNode(const Value: PVirtualNode);
begin
  FVTNode := Value;
end;
{$ENDREGION}

end.
