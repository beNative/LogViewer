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

unit LogViewer.Watches.View;

{ View displaying watch values and value history as a master-detail. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,

  LogViewer.Messages.Data, LogViewer.Watches.Data,
  LogViewer.Watches.Settings, LogViewer.DisplayValues.Settings;

type
  TfrmWatchesView = class(TForm)
    {$REGION 'designer controls'}
    pnlWatches      : TPanel;
    splHorizontal   : TSplitter;
    pnlWatchHistory : TPanel;
    {$ENDREGION}

  private
    FMessageId                     : Int64;
    FWatches                       : TWatchList;
    FVSTWatchValues                : TVirtualStringTree;
    FVSTWatchHistory               : TVirtualStringTree;
    FDisplayValuesSettings         : TDisplayValuesSettings;
    FWatchHistoryColumnDefinitions : IColumnDefinitions;
    FTVPWatchValues                : TTreeViewPresenter;
    FTVPWatchHistory               : TTreeViewPresenter;
    FSettings                      : TWatchSettings;

    function GetSelectedWatch: TWatch;

    {$REGION 'event handlers'}
    procedure FTVPWatchValuesSelectionChanged(Sender: TObject);
    function FCDTimeStampGetText(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject
    ): string;
    function FCDTimeStampCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    function FCDTypeCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    function FCDValueCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    function FCDNameCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    function FCDIdCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    {$ENDREGION}

    procedure ConnectWatchHistoryCDEvents;
    procedure ConnectWatchValuesCDEvents;
    procedure CreateObjects;

  protected
    procedure UpdateWatchHistory;

    property SelectedWatch: TWatch
      read GetSelectedWatch;

  public
    constructor Create(
      AOwner                 : TComponent;
      AWatches               : TWatchList;
      ASettings              : TWatchSettings;
      ADisplayValuesSettings : TDisplayValuesSettings
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateView(AMessageId: Int64 = 0);
    procedure GotoFirst;
    procedure GotoLast;

    function HasFocus: Boolean;

  end;

implementation

{$R *.dfm}

uses
  System.UITypes,

  DSharp.Windows.ControlTemplates,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,
  DDuce.Logger,

  LogViewer.Resources;

{$REGION 'construction and destruction'}
constructor TfrmWatchesView.Create(AOwner: TComponent; AWatches: TWatchList;
  ASettings: TWatchSettings; ADisplayValuesSettings: TDisplayValuesSettings);
begin
  inherited Create(AOwner);
  FWatches := AWatches;
  FSettings              := ASettings;
  FDisplayValuesSettings := ADisplayValuesSettings;
  CreateObjects;
end;

procedure TfrmWatchesView.AfterConstruction;
begin
  inherited AfterConstruction;
  Height                 := FSettings.Height;
  if FSettings.WatchHistoryPanelHeight < Height then
    pnlWatchHistory.Height := FSettings.WatchHistoryPanelHeight
  else
    pnlWatchHistory.Height := Height div 3;
end;

procedure TfrmWatchesView.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FSettings.WatchHistoryPanelHeight := pnlWatchHistory.Height;
  FSettings.Height                  := Height;
  FWatches := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmWatchesView.GetSelectedWatch: TWatch;
begin
  if Assigned(FTVPWatchValues.SelectedItem) then
    Result := FTVPWatchValues.SelectedItem as TWatch
  else
    Result := nil;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmWatchesView.FCDIdCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmPaintText then
  begin
    FDisplayValuesSettings.Id.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

function TfrmWatchesView.FCDNameCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmPaintText then
  begin
    FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

function TfrmWatchesView.FCDTimeStampCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmPaintText then
  begin
    FDisplayValuesSettings.TimeStamp.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

function TfrmWatchesView.FCDValueCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmPaintText then
  begin
    FDisplayValuesSettings.Value.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

function TfrmWatchesView.FCDTimeStampGetText(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject): string;
begin
  if Item is TWatch then
    Result := FormatDateTime('hh:nn:ss:zzz',  TWatch(Item).TimeStamp)
  else
  begin
    Result := FormatDateTime('hh:nn:ss:zzz',  TWatchValue(Item).TimeStamp)
  end;
end;

function TfrmWatchesView.FCDTypeCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmPaintText then
  begin
    if TWatch(Item).ValueType = 'Counter' then
    begin
      FDisplayValuesSettings.Counter.AssignTo(TargetCanvas.Font);
    end
    else
    begin
      FDisplayValuesSettings.ValueType.AssignTo(TargetCanvas.Font);
    end;
  end;
  Result := True;
end;

procedure TfrmWatchesView.FTVPWatchValuesSelectionChanged(Sender: TObject);
begin
  UpdateWatchHistory;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmWatchesView.CreateObjects;
var
  CDS : IColumnDefinitions;
  CD  : TColumnDefinition;
begin
  FVSTWatchValues := TVirtualStringTreeFactory.CreateList(Self, pnlWatches);
  FVSTWatchValues.AlignWithMargins := False;
  CDS                  := TFactories.CreateColumnDefinitions;
  CD                   := CDS.Add('Name');
  CD.ValuePropertyName := 'Name';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnCustomDraw      := FCDNameCustomDraw;

  CD                   := CDS.Add('Value');
  CD.ValuePropertyName := 'Value';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.MinWidth          := 50;
  CD.OnCustomDraw      := FCDValueCustomDraw;

  CD                   := CDS.Add('ValueType');
  CD.ValuePropertyName := 'ValueType';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Caption           := 'Type';
  CD.MinWidth          := 0;
  CD.AutoSize          := True;
  CD.OnCustomDraw      := FCDTypeCustomDraw;

  CD                   := CDS.Add('TimeStamp');
  CD.MinWidth          := 80;
  CD.Width             := 80;
  CD.ValuePropertyName := 'TimeStamp';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnGetText         := FCDTimeStampGetText;
  CD.OnCustomDraw      := FCDTimeStampCustomDraw;

  FWatchHistoryColumnDefinitions := TFactories.CreateColumnDefinitions;
  CD                   := FWatchHistoryColumnDefinitions.Add('Id');
  CD.AutoSize          := True;
  CD.ValuePropertyName := 'Id';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Width             := 50;
  CD.OnCustomDraw      := FCDIdCustomDraw;

  CD                   := FWatchHistoryColumnDefinitions.Add('Value');
  CD.ValuePropertyName := 'Value';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.AutoSize          := True; // Test
  CD.OnCustomDraw      := FCDValueCustomDraw;

  CD                   := FWatchHistoryColumnDefinitions.Add('Timestamp');
  CD.ValuePropertyName := 'TimeStamp';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Alignment         := taCenter;
  CD.Width             := 80;
  CD.OnGetText         := FCDTimeStampGetText;
  CD.OnCustomDraw      := FCDTimeStampCustomDraw;

  FTVPWatchValues := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTWatchValues,
    FWatches.List as IObjectList,
    CDS
  );
  ConnectWatchValuesCDEvents;
  FTVPWatchValues.OnSelectionChanged := FTVPWatchValuesSelectionChanged;
  FVSTWatchHistory := TVirtualStringTreeFactory.CreateList(Self, pnlWatchHistory);
  FVSTWatchHistory.AlignWithMargins := False;
  FTVPWatchHistory := TFactories.CreateTreeViewPresenter(Self, FVSTWatchHistory);
end;

procedure TfrmWatchesView.ConnectWatchHistoryCDEvents;
var
  CD : TColumnDefinition;
  I  : Integer;
begin
  for I := 0 to FTVPWatchHistory.ColumnDefinitions.Count - 1 do
  begin
    CD := FTVPWatchHistory.ColumnDefinitions[I];
    if CD.ValuePropertyName = 'TimeStamp' then
    begin
      CD.OnCustomDraw := FCDTimeStampCustomDraw;
      CD.OnGetText    := FCDTimeStampGetText;
    end
    else if CD.ValuePropertyName = 'Value' then
    begin
      CD.OnCustomDraw := FCDValueCustomDraw;
    end;
  end;
end;

procedure TfrmWatchesView.ConnectWatchValuesCDEvents;
var
  CD : TColumnDefinition;
  I  : Integer;
begin
  for I := 0 to FTVPWatchValues.ColumnDefinitions.Count - 1 do
  begin
    CD := FTVPWatchValues.ColumnDefinitions[I];
    if CD.ValuePropertyName = 'TimeStamp' then
    begin
      CD.OnCustomDraw := FCDTimeStampCustomDraw;
      CD.OnGetText    := FCDTimeStampGetText;
    end
    else if CD.ValuePropertyName = 'Name' then
    begin
      CD.OnCustomDraw := FCDNameCustomDraw;
    end
    else if CD.ValuePropertyName = 'ValueType' then
    begin
      CD.OnCustomDraw := FCDTypeCustomDraw;
    end
    else if CD.ValuePropertyName = 'Value' then
    begin
      CD.OnCustomDraw := FCDValueCustomDraw;
    end;
  end;
end;

procedure TfrmWatchesView.UpdateView(AMessageId: Int64);
begin
  FMessageId := AMessageId;
  FTVPWatchValues.Refresh;
  FWatches.Update(AMessageId);
  UpdateWatchHistory;
end;

procedure TfrmWatchesView.UpdateWatchHistory;
begin
  if Assigned(SelectedWatch) then
  begin
    TFactories.InitializeTVP(
      FTVPWatchHistory,
      FVSTWatchHistory,
      SelectedWatch.List as IObjectList,
      FWatchHistoryColumnDefinitions
    );
    ConnectWatchHistoryCDEvents;
    if FMessageId = 0 then
    begin
      FTVPWatchHistory.SelectedItem := SelectedWatch.List.Last;
    end
    else
    begin
      FTVPWatchHistory.SelectedItem := SelectedWatch.CurrentWatchValue;
    end;
  end
  else
  begin
    FVSTWatchHistory.Clear;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmWatchesView.GotoFirst;
begin
  if FVSTWatchValues.Focused then
    FTVPWatchValues.View.MoveCurrentToFirst
  else if FVSTWatchHistory.Focused then
    FTVPWatchHistory.View.MoveCurrentToFirst;
end;

procedure TfrmWatchesView.GotoLast;
begin
  if FVSTWatchValues.Focused then
    FTVPWatchValues.View.MoveCurrentToLast
  else if FVSTWatchHistory.Focused then
    FTVPWatchHistory.View.MoveCurrentToLast;
end;

{ Returns true if this form has the focused control. }

function TfrmWatchesView.HasFocus: Boolean;
begin
  Result := Assigned(Screen.ActiveControl)
    and (Screen.ActiveControl.Owner = Self);
end;
{$ENDREGION}

end.
