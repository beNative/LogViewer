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
  Vcl.ComCtrls, Vcl.ExtCtrls,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Core.DataTemplates,

  LogViewer.Messages.Data, LogViewer.Watches.Data;

type
  TfrmWatchesView = class(TForm)
    pnlWatches      : TPanel;
    splHorizontal   : TSplitter;
    pnlWatchHistory : TPanel;

  private
    FWatches         : TWatchList;
    FVSTWatchValues  : TVirtualStringTree;
    FVSTWatchHistory : TVirtualStringTree;

    FTVPWatchValues  : TTreeViewPresenter;
    FTVPWatchHistory : TTreeViewPresenter;
    FSelectedWatch   : TWatch;

    procedure FWatchesUpdateWatch(const AName, AValue: string);
    procedure FWatchesNewWatch(const AName: string; AIndex: Integer);

    procedure FTVPWatchValuesSelectionChanged(Sender: TObject);

    function FCDSTimeStampGetText(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject
    ): string;
    procedure UpdateWatchHistory;

  public
    constructor Create(
      AOwner : TComponent;
      AData  : TWatchList
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

    procedure UpdateView(AMessageID: Int64 = 0);

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
  FVSTWatchValues := TFactories.CreateVirtualStringTree(Self, pnlWatches);
  CDS := TFactories.CreateColumnDefinitions;
  CDS.Add('Name').ValuePropertyName  := 'Name';
  CDS.Add('Value').ValuePropertyName := 'Value';
  CD := CDS.Add('TimeStamp');
  CD.ValuePropertyName := 'TimeStamp';
  CD.OnGetText := FCDSTimeStampGetText;
  FTVPWatchValues := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTWatchValues,
    FWatches.List as IObjectList,
    CDS
  );
  FTVPWatchValues.OnSelectionChanged := FTVPWatchValuesSelectionChanged;
  FVSTWatchHistory := TFactories.CreateVirtualStringTree(Self, pnlWatchHistory);
  FTVPWatchHistory := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTWatchHistory
  );
end;

procedure TfrmWatchesView.BeforeDestruction;
begin
  FSelectedWatch := nil;
  FWatches := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
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

procedure TfrmWatchesView.FTVPWatchValuesSelectionChanged(Sender: TObject);
var
  LWatch : TWatch;
begin
  LWatch := FTVPWatchValues.SelectedItem as TWatch;
  if LWatch <> FSelectedWatch then
  begin
    FSelectedWatch := LWatch;
    UpdateWatchHistory;
  end;
end;

procedure TfrmWatchesView.FWatchesNewWatch(const AName: string;
  AIndex: Integer);
begin
  //
end;

procedure TfrmWatchesView.FWatchesUpdateWatch(const AName, AValue: string);
begin
//  if Assigned(FSelectedWatch) and (FSelectedWatch.Name = AName) then
//  begin
//    FVSTWatchHistory.FocusedNode := FVSTWatchHistory.GetLast;
//    FVSTWatchHistory.Selected[FVSTWatchHistory.FocusedNode] := True;
//  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmWatchesView.UpdateView(AMessageID: Int64);
begin
  FTVPWatchValues.Refresh;
  FWatches.Update(AMessageId);
end;

procedure TfrmWatchesView.UpdateWatchHistory;
begin
  TFactories.InitializeTVP(
    FTVPWatchHistory,
    FVSTWatchHistory,
    FSelectedWatch.List as IObjectList
  );
  FTVPWatchHistory.ColumnDefinitions.Items[2].OnGetText := FCDSTimeStampGetText;
  FVSTWatchHistory.FocusedNode := FVSTWatchHistory.GetLast;
  FVSTWatchHistory.Selected[FVSTWatchHistory.FocusedNode] := True;
end;
{$ENDREGION}

end.
