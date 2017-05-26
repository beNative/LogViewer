unit LogViewer.Settings.Dialog.ConfigNode;

interface

{
  See this topic for more information:
  https://stackoverflow.com/questions/5365365/tree-like-datastructure-for-use-with-virtualtreeview }


uses
  Spring.Collections,

  VirtualTrees;

type
  TConfigNode = class
  private
    FText   : string;
    FVTNode : PVirtualNode;
    FNodes  : IList<TConfigNode>;

  protected
    function GetNodes: IList<TConfigNode>;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetVTNode: PVirtualNode;
    procedure SetVTNode(const Value: PVirtualNode);

  public
    constructor Create(AText: string = '');

    procedure AfterConstruction; override;

    property Nodes: IList<TConfigNode>
      read GetNodes;

    property Text: string
      read GetText write SetText;

    property VTNode : PVirtualNode
      read GetVTNode write SetVTNode;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TConfigNode.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TCollections.CreateObjectList<TConfigNode>;
end;

constructor TConfigNode.Create(AText: string);
begin
  inherited Create;
  FText := AText;
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

function TConfigNode.GetVTNode: PVirtualNode;
begin
  Result := FVTNode;
end;

procedure TConfigNode.SetVTNode(const Value: PVirtualNode);
begin
  FVTNode := Value;
end;

procedure TConfigNode.SetText(const Value: string);
begin
  FText := Value;;
end;
{$ENDREGION}

end.
