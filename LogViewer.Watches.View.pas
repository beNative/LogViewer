{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Watches.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates,

  DDuce.Components.GridView, DDuce.Components.Inspector,

  LogViewer.Messages.Data, LogViewer.Watches.Data;

type
  TfrmWatchesView = class(TForm)
    pgcWatches      : TPageControl;
    tsLatest        : TTabSheet;
    tsSelected      : TTabSheet;
    tsHistory       : TTabSheet;
    cbxWatchHistory : TComboBox;

  private
    FWatches                : TWatchList;
    FLatestWatchInspector   : TInspector;
    FSelectedWatchInspector : TInspector;

    FMessages : IList<TLogMessageData>;

    FTVPHistory : TTreeViewPresenter;
    FVSTHistory : TVirtualStringTree;

    procedure FMessagesChanged(
      Sender     : TObject;
      const Item : TLogMessageData;
      Action     : TCollectionChangedAction
    );

    procedure FWatchesUpdate(const AVariable, AValue: string);
    procedure FWatchesNewVariable(const AVariable: string; AIndex: Integer);

    procedure FWatchHistoryInspectorGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FSelectedWatchInspectorGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FLatestWatchInspectorGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );

  public
    constructor Create(
      AOwner    : TComponent;
      AData     : TWatchList;
      AMessages : IList<TLogMessageData>
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

    procedure UpdateView;

  end;

implementation

uses
  DDuce.Components.Factories, DDuce.Factories, DDuce.Logger.Interfaces;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmWatchesView.Create(AOwner: TComponent; AData: TWatchList;
  AMessages : IList<TLogMessageData>);
begin
  inherited Create(AOwner);
  FMessages := AMessages;
  FMessages.OnChanged.Add(FMessagesChanged);
  FWatches  := AData;
  FWatches.OnUpdate := FWatchesUpdate;
  FWatches.OnNewVariable := FWatchesNewVariable;
  FLatestWatchInspector := TDDuceComponents.CreateInspector(Self, tsLatest);
  FLatestWatchInspector.OnGetCellText := FLatestWatchInspectorGetCellText;
  FLatestWatchInspector.ReadOnly  := True;
  FLatestWatchInspector.AllowEdit := False;
  FLatestWatchInspector.ThemingEnabled := True;

  FSelectedWatchInspector := TDDuceComponents.CreateInspector(Self, tsSelected);
  FSelectedWatchInspector.OnGetCellText := FSelectedWatchInspectorGetCellText;
  FSelectedWatchInspector.ReadOnly := True;
  FSelectedWatchInspector.AllowEdit := False;

  FVSTHistory := TFactories.CreateVirtualStringTree(Self, tsHistory);
  FTVPHistory := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTHistory
  );
end;

procedure TfrmWatchesView.BeforeDestruction;
begin
  FMessages.OnChanged.Remove(FMessagesChanged);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmWatchesView.FLatestWatchInspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
  begin
    Value := FWatches.Items[Cell.Row].Name;
  end
  else
  begin
    Value := FWatches.Items[Cell.Row].CurrentValue
  end;
end;

procedure TfrmWatchesView.FMessagesChanged(Sender: TObject;
  const Item: TLogMessageData; Action: TCollectionChangedAction);
begin
  if (Action = caAdded) and (Item.MessageType in [lmtWatch, lmtCounter]) then
  begin
    FWatches.Add(
      Item.Text,
      FMessages.Count,
      Item.MessageType = lmtCounter
    );
    UpdateView;
  end;
end;

procedure TfrmWatchesView.FSelectedWatchInspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
    Value := FWatches.Items[Cell.Row].Name
  else
    Value := FWatches.Items[Cell.Row].CurrentValue;
end;

procedure TfrmWatchesView.FWatchesNewVariable(const AVariable: string;
  AIndex: Integer);
begin
  cbxWatchHistory.Items.Add(AVariable);
end;

procedure TfrmWatchesView.FWatchesUpdate(const AVariable, AValue: string);
begin
  FLatestWatchInspector.Rows.Count   := FWatches.Count;
  FSelectedWatchInspector.Rows.Count := FWatches.Count;
//  FWatchHistoryInspector.Refresh;
//  FSelectedWatchInspector.Refresh;
//  FLatestWatchInspector.Refresh;

end;
procedure TfrmWatchesView.FWatchHistoryInspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  if Cell.Col = 0 then
    Value := FWatches.Items[cbxWatchHistory.ItemIndex].Name
  else
    Value := FWatches.Items[cbxWatchHistory.ItemIndex].Values[Cell.Row];
end;

procedure TfrmWatchesView.UpdateView;
var
  LTempIndex: Integer;
begin
  case pgcWatches.ActivePageIndex of
    0{Latest}, 1{Selected}:
    begin
      if pgcWatches.ActivePageIndex = 0 then
      begin
        LTempIndex := FMessages.Count;
      end
      else
      begin
//        if FLogTreeView.FocusedNode <> nil then
//          LTempIndex := PNodeData(FLogTreeView.GetNodeData(
//            FLogTreeView.FocusedNode))^.Index
//        else
//          LTempIndex := 0;
      end;
      FWatches.Update(LTempIndex);
    end;
    2:
    begin
      //UpdateWatchHistory;
    end;
  end;
end;

{$ENDREGION}

end.
