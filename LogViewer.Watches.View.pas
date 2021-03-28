{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

{ TODO: synchronise selected record in history list in sync mode. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,

  VirtualTrees, OMultiPanel,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,

  LogViewer.Watches.Data, LogViewer.Watches.Settings,
  LogViewer.DisplayValues.Settings;

type
  TfrmWatchesView = class(TForm)
    pnlMain         : TOMultiPanel;
    pnlWatches      : TPanel;
    pnlWatchHistory : TPanel;

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

    {$REGION 'property access methods'}
    function GetSelectedWatch: TWatch;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure FSettingsChanged(Sender: TObject);
    procedure FTVPWatchValuesSelectionChanged(Sender: TObject);
    procedure FTVPWatchValuesDoubleClick(Sender: TObject);
    procedure FTVPWatchHistoryDoubleClick(Sender: TObject);
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
    procedure UpdateActions; override;

    property SelectedWatch: TWatch
      read GetSelectedWatch;

  public
    constructor Create(
      AOwner                 : TComponent;
      AWatches               : TWatchList;
      ASettings              : TWatchSettings;
      ADisplayValuesSettings : TDisplayValuesSettings
    ); reintroduce; virtual;
    destructor Destroy; override;

    procedure UpdateView(AMessageId: Int64 = 0);
    procedure GotoFirst;
    procedure GotoLast;
    procedure Clear;

    function HasFocus: Boolean;
  end;

implementation

{$R *.dfm}

uses
  System.UITypes,

  DSharp.Windows.ControlTemplates,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,
  DDuce.Logger, DDuce.Logger.Interfaces,

  LogViewer.Resources;

{$REGION 'construction and destruction'}
constructor TfrmWatchesView.Create(AOwner: TComponent; AWatches: TWatchList;
  ASettings: TWatchSettings; ADisplayValuesSettings: TDisplayValuesSettings);
begin
  inherited Create(AOwner);
  FWatches := AWatches;
  FSettings              := ASettings;
  FSettings.OnChanged.Add(FSettingsChanged);
  FDisplayValuesSettings := ADisplayValuesSettings;
  CreateObjects;
end;

destructor TfrmWatchesView.Destroy;
begin
  FSettings.OnChanged.RemoveAll(Self);
  FTVPWatchValues.View.ItemsSource  := nil;
  FTVPWatchHistory.View.ItemsSource := nil;
  FTVPWatchValues.Free;
  FTVPWatchHistory.Free;
  FWatches.Clear;
  FWatches := nil;
  inherited Destroy;
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
  end
  else if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.Id.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
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
  end
  else if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
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
  end
  else if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.TimeStamp.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
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
  end
  else if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.Value.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
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
    if TWatch(Item).MessageType = lmtCounter then
    begin
      FDisplayValuesSettings.Counter.AssignTo(TargetCanvas.Font);
    end
    else
    begin
      FDisplayValuesSettings.ValueType.AssignTo(TargetCanvas.Font);
    end;
  end
  else if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.ValueType.AssignTo(TargetCanvas);
    TargetCanvas.FillRect(CellRect);
  end;
  Result := True;
end;

procedure TfrmWatchesView.FTVPWatchHistoryDoubleClick(Sender: TObject);
begin
  FTVPWatchHistory.TreeView.Header.AutoFitColumns(False, smaAllColumns);
end;

procedure TfrmWatchesView.FTVPWatchValuesDoubleClick(Sender: TObject);
begin
  FTVPWatchValues.TreeView.Header.AutoFitColumns(False, smaAllColumns);
end;

procedure TfrmWatchesView.FTVPWatchValuesSelectionChanged(Sender: TObject);
begin
  UpdateWatchHistory;
end;

procedure TfrmWatchesView.FSettingsChanged(Sender: TObject);
begin
  FTVPWatchValues.ShowHeader  := FSettings.ColumnHeadersVisible;
  FTVPWatchHistory.ShowHeader := FSettings.ColumnHeadersVisible;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmWatchesView.CreateObjects;
var
  CDS : IColumnDefinitions;
  CD  : TColumnDefinition;
begin
  FVSTWatchValues := TVirtualStringTreeFactory.CreateList(Self, pnlWatches);
  FVSTWatchValues.Name := 'FVSTWatchValues'; // for debugging
  FVSTWatchValues.AlignWithMargins := False;
  CDS                  := TFactories.CreateColumnDefinitions;
  CD                   := CDS.Add(COLUMNNAME_NAME);
  CD.ValuePropertyName := COLUMNNAME_NAME;
  CD.MinWidth          := 60;
  CD.Width             := 100;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnCustomDraw      := FCDNameCustomDraw;

  CD                   := CDS.Add(COLUMNNAME_VALUE);
  CD.ValuePropertyName := COLUMNNAME_VALUE;
  CD.HintPropertyName  := CD.ValuePropertyName;
  // Only one column in the header can have AutoSize = True. This property
  // determines the value of the Header.AutoSizeIndex property of the treeview.
  // This property will cause the columns to fit to the width of the control,
  // and will reduce the width of the AutoSizeIndex column if needed when the
  // list width is adjusted.
  CD.AutoSize          := True;
  CD.OnCustomDraw      := FCDValueCustomDraw;

  CD                   := CDS.Add(COLUMNNAME_VALUETYPE);
  CD.ValuePropertyName := COLUMNNAME_VALUETYPE;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.MinWidth          := 0;
  CD.Width             := 60;
  CD.Caption           := SType;
  CD.OnCustomDraw      := FCDTypeCustomDraw;

  CD                   := CDS.Add(COLUMNNAME_TIMESTAMP);
  CD.ValuePropertyName := COLUMNNAME_TIMESTAMP;
  CD.MinWidth          := 80;
  CD.Width             := 80;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Alignment         := taCenter;
  CD.OnGetText         := FCDTimeStampGetText;
  CD.OnCustomDraw      := FCDTimeStampCustomDraw;

  FWatchHistoryColumnDefinitions := TFactories.CreateColumnDefinitions;
  CD                   := FWatchHistoryColumnDefinitions.Add(COLUMNNAME_ID);
  CD.ValuePropertyName := COLUMNNAME_ID;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Width             := 100;
  CD.OnCustomDraw      := FCDIdCustomDraw;

  CD                   := FWatchHistoryColumnDefinitions.Add(COLUMNNAME_VALUE);
  CD.ValuePropertyName := COLUMNNAME_VALUE;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.AutoSize          := True;
  CD.OnCustomDraw      := FCDValueCustomDraw;
  CD.ColumnOptions     := CD.ColumnOptions - [coSortable];

  CD                   := FWatchHistoryColumnDefinitions.Add(COLUMNNAME_TIMESTAMP);
  CD.MinWidth          := 80;
  CD.Width             := 80;
  CD.ValuePropertyName := COLUMNNAME_TIMESTAMP;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Alignment         := taCenter;
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
  FTVPWatchValues.OnDoubleClick      := FTVPWatchValuesDoubleClick;
  FVSTWatchHistory := TVirtualStringTreeFactory.CreateList(Self, pnlWatchHistory);
  FVSTWatchHistory.Name := 'FVSTWatchHistory'; // for debugging
  FVSTWatchHistory.AlignWithMargins := False;
  FTVPWatchHistory := TFactories.CreateTreeViewPresenter(Self, FVSTWatchHistory);
  FTVPWatchHistory.OnDoubleClick := FTVPWatchHistoryDoubleClick;
  FTVPWatchValues.ShowHeader  := FSettings.ColumnHeadersVisible;
  FTVPWatchHistory.ShowHeader := FSettings.ColumnHeadersVisible;
end;

procedure TfrmWatchesView.Clear;
begin
  FWatches.Clear;
  FTVPWatchValues.Refresh;
  UpdateWatchHistory;
end;

procedure TfrmWatchesView.ConnectWatchHistoryCDEvents;
var
  CD : TColumnDefinition;
  I  : Integer;
begin
  for I := 0 to FTVPWatchHistory.ColumnDefinitions.Count - 1 do
  begin
    CD := FTVPWatchHistory.ColumnDefinitions[I];
    if CD.ValuePropertyName = COLUMNNAME_TIMESTAMP then
    begin
      CD.OnCustomDraw := FCDTimeStampCustomDraw;
      CD.OnGetText    := FCDTimeStampGetText;
    end
    else if CD.ValuePropertyName = COLUMNNAME_VALUE then
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
    if CD.ValuePropertyName = COLUMNNAME_TIMESTAMP then
    begin
      CD.OnCustomDraw := FCDTimeStampCustomDraw;
      CD.OnGetText    := FCDTimeStampGetText;
    end
    else if CD.ValuePropertyName = COLUMNNAME_NAME then
    begin
      CD.OnCustomDraw := FCDNameCustomDraw;
    end
    else if CD.ValuePropertyName = COLUMNNAME_VALUETYPE then
    begin
      CD.OnCustomDraw := FCDTypeCustomDraw;
    end
    else if CD.ValuePropertyName = COLUMNNAME_VALUE then
    begin
      CD.OnCustomDraw := FCDValueCustomDraw;
    end;
  end;
end;

procedure TfrmWatchesView.UpdateActions;
begin
  inherited UpdateActions;
  pnlMain.PanelCollection[1].Visible := FSettings.WatchHistoryVisible;
end;

procedure TfrmWatchesView.UpdateView(AMessageId: Int64);
begin
  FMessageId := AMessageId;
  FWatches.Update(AMessageId);
  FVSTWatchValues.Invalidate;
  FVSTWatchHistory.Invalidate;
end;

procedure TfrmWatchesView.UpdateWatchHistory;
begin
  if Assigned(SelectedWatch) then
  begin
    FTVPWatchHistory.BeginUpdate;
    TFactories.InitializeTVP(
      FTVPWatchHistory,
      FVSTWatchHistory,
      SelectedWatch.List as IObjectList,
      FWatchHistoryColumnDefinitions
    );
    ConnectWatchHistoryCDEvents;
    FTVPWatchHistory.EndUpdate;
    if SelectedWatch.Count > 0 then
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
  Logger.Track(Self, 'GotoFirst');
  if FVSTWatchValues.Focused then
    FTVPWatchValues.View.MoveCurrentToFirst
  else if FVSTWatchHistory.Focused then
    FTVPWatchHistory.View.MoveCurrentToFirst;
end;

procedure TfrmWatchesView.GotoLast;
begin
  Logger.Track(Self, 'GotoLast');
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
