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

{ View showing watch values and history. }

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

  LogViewer.Messages.Data, LogViewer.Watches.Data;

type
  TfrmWatchesView = class(TForm)
    pgcWatches      : TPageControl;
    tsLatest        : TTabSheet;
    tsSelected      : TTabSheet;
    tsHistory       : TTabSheet;
    cbxWatchHistory : TComboBox;

    procedure pgcWatchesChanging(
      Sender          : TObject;
      var AllowChange : Boolean
    );
    procedure cbxWatchHistoryChange(Sender: TObject);

  private
    FWatches                : TWatchList;
    FVSTLastWatchValues     : TVirtualStringTree;
    FVSTSelectedWatchValues : TVirtualStringTree;
    FVSTWatchHistory        : TVirtualStringTree;

    FTVPLastWatchValues     : TTreeViewPresenter;
    FTVPSelectedWatchValues : TTreeViewPresenter;
    FTVPWatchHistory        : TTreeViewPresenter;

    procedure FWatchesUpdateWatch(const AName, AValue: string);
    procedure FWatchesNewWatch(const AName: string; AIndex: Integer);

    function FCDSTimeStampGetText(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject
    ): string;

  public
    constructor Create(
      AOwner : TComponent;
      AData  : TWatchList
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

    procedure UpdateView;

  end;

implementation

uses
  DDuce.Components.Factories, DDuce.Factories, DDuce.Logger.Interfaces;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmWatchesView.Create(AOwner: TComponent; AData: TWatchList);
var
  CDS : IColumnDefinitions;
  CD  : TColumnDefinition;
begin
  inherited Create(AOwner);
  FWatches := AData;
  FWatches.OnUpdateWatch := FWatchesUpdateWatch;
  FWatches.OnNewWatch    := FWatchesNewWatch;
  FVSTLastWatchValues := TFactories.CreateVirtualStringTree(Self, tsLatest);
  CDS  := TFactories.CreateColumnDefinitions;
  CDS.Add('Name').ValuePropertyName  := 'Name';
  CDS.Add('Value').ValuePropertyName := 'Value';
  CD := CDS.Add('TimeStamp');
  CD.ValuePropertyName := 'TimeStamp';
  CD.OnGetText := FCDSTimeStampGetText;
  FTVPLastWatchValues := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTLastWatchValues,
    FWatches.List as IObjectList,
    CDS
  );
  FVSTSelectedWatchValues := TFactories.CreateVirtualStringTree(Self, tsSelected);
  FTVPSelectedWatchValues := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTSelectedWatchValues
  );
  FVSTWatchHistory := TFactories.CreateVirtualStringTree(Self, tsHistory);
  FTVPWatchHistory := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTWatchHistory
  );
end;

procedure TfrmWatchesView.BeforeDestruction;
begin
  FWatches := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmWatchesView.cbxWatchHistoryChange(Sender: TObject);
begin
  TFactories.InitializeTVP(
    FTVPWatchHistory,
    FVSTWatchHistory,
    FWatches.Items[FWatches.IndexOf(cbxWatchHistory.Text)].List as IObjectList
  );
  FTVPWatchHistory.ColumnDefinitions.Items[2].OnGetText := FCDSTimeStampGetText;
end;

function TfrmWatchesView.FCDSTimeStampGetText(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject): string;
begin
  if Item is TWatch then
    Result := FormatDateTime('hh:nn:ss:zzz',  TWatch(Item).TimeStamp)
  else
  begin
    Result := FormatDateTime('hh:nn:ss:zzz',  TWatchValue(Item).TimeStamp)
  end;
end;

procedure TfrmWatchesView.FWatchesNewWatch(const AName: string;
  AIndex: Integer);
begin
  cbxWatchHistory.Items.Add(AName);
end;

procedure TfrmWatchesView.FWatchesUpdateWatch(const AName, AValue: string);
begin
  if (pgcWatches.ActivePage = tsHistory) and (AName = cbxWatchHistory.Text) then
  begin
    FTVPWatchHistory.Refresh;
    FVSTWatchHistory.FocusedNode := FVSTWatchHistory.GetLast;
  end;
end;

procedure TfrmWatchesView.pgcWatchesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  UpdateView;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmWatchesView.UpdateView;
var
  LTempIndex: Integer;
begin
  case pgcWatches.ActivePageIndex of
    0{Latest}, 1{Selected}:
    begin
      if pgcWatches.ActivePageIndex = 0 then
      begin
//      LTempIndex := FMessages.Count;
        FTVPLastWatchValues.Refresh;
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

    end;
  end;
end;
{$ENDREGION}

end.
