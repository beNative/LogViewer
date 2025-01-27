{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.CallStack.View;

{ Callstack viewer. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,

  LogViewer.DisplayValues.Settings, LogViewer.CallStack.Settings;

type
  TfrmCallStackView = class(TForm)
  private
    FTVPCallStack          : TTreeViewPresenter;
    FVSTCallStack          : TVirtualStringTree;
    FCallStack             : IObjectList;
    FDisplayValuesSettings : TDisplayValuesSettings;
    FSettings              : TCallStackSettings;

    procedure FCallStackChanged(
      Sender     : TObject;
      const Item : TObject;
      Action     : TCollectionChangedAction
    );
    function FCDLevelCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    function FCDTitleCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    function FCDDurationCustomDraw(
      Sender                 : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;
    procedure SettingsChanged(Sender: TObject);
    procedure FTVPCallStackDoubleClick(Sender: TObject);

  public
    constructor Create(
      AOwner                 : TComponent;
      AData                  : IObjectList;
      ASettings              : TCallStackSettings;
      ADisplayValuesSettings : TDisplayValuesSettings
    ); reintroduce; virtual;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  Spring,

  DSharp.Windows.ControlTemplates,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,
  DDuce.Logger, DDuce.Utils,

  LogViewer.CallStack.Data, LogViewer.Interfaces;

{$REGION 'construction and destruction'}
constructor TfrmCallStackView.Create(AOwner: TComponent; AData: IObjectList;
  ASettings: TCallStackSettings; ADisplayValuesSettings: TDisplayValuesSettings);
var
  CDS : IColumnDefinitions;
  CD  : TColumnDefinition;
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AData, 'AData');
  Guard.CheckNotNull(ASettings, 'ASettings');
  Guard.CheckNotNull(ADisplayValuesSettings, 'ADisplayValuesSettings');
  FDisplayValuesSettings := ADisplayValuesSettings;
  FSettings              := ASettings;
  FSettings.OnChanged.Add(SettingsChanged);
  FCallStack := AData;
  FCallStack.OnChanged.Add(FCallStackChanged);
  FVSTCallStack := TVirtualStringTreeFactory.CreateList(Self, Self);
  FVSTCallStack.AlignWithMargins := False;
  CDS                  := TFactories.CreateColumnDefinitions;
  CD                   := CDS.Add('');
  CD.ValuePropertyName := 'Level';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnCustomDraw      := FCDLevelCustomDraw;
  CD.Width             := 10;
  CD.AutoSize          := True;
  CD                   := CDS.Add('Name');
  CD.ValuePropertyName := 'Title';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnCustomDraw      := FCDTitleCustomDraw;
  CD.AutoSize          := True;
  CD                   := CDS.Add('Duration');
  CD.ValuePropertyName := 'Duration';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnCustomDraw      := FCDDurationCustomDraw;
  CD.MinWidth          := ScaleSize(15 * FDisplayValuesSettings.TimeStamp.Font.Size);
  CD.Width             := CD.MinWidth;
  CD.Alignment         := taRightJustify;
  CD.AutoSize          := False;
  FVSTCallStack.Header.AutoSizeIndex := 1;
  FTVPCallStack := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTCallStack,
    FCallStack as IObjectList,
    CDS
  );
  FTVPCallStack.ShowHeader    := FSettings.ColumnHeadersVisible;
  FTVPCallStack.OnDoubleClick := FTVPCallStackDoubleClick;
end;

destructor TfrmCallStackView.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FCallStack.OnChanged.RemoveAll(Self);
  FCallStack := nil;
  FDisplayValuesSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmCallStackView.FCallStackChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
begin
  FTVPCallStack.Refresh;
  FTVPCallStack.TreeView.Header.AutoFitColumns;
end;

function TfrmCallStackView.FCDDurationCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.Tracing.AssignTo(TargetCanvas);
    if not Selected then
    begin
      TargetCanvas.Brush.Color := FDisplayValuesSettings.Tracing.BackgroundColor;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
  if DrawMode = dmPaintText then
  begin
    FDisplayValuesSettings.TimeStamp.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

function TfrmCallStackView.FCDLevelCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.Tracing.AssignTo(TargetCanvas);
    if not Selected then
    begin
      TargetCanvas.Brush.Color := FDisplayValuesSettings.Tracing.BackgroundColor;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
  if DrawMode = dmPaintText then
  begin
    FDisplayValuesSettings.Id.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

function TfrmCallStackView.FCDTitleCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  if DrawMode = dmBeforeCellPaint then
  begin
    FDisplayValuesSettings.Tracing.AssignTo(TargetCanvas);
    if not Selected then
    begin
      TargetCanvas.Brush.Color := FDisplayValuesSettings.Tracing.BackgroundColor;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
  if DrawMode = dmPaintText then
  begin
    FDisplayValuesSettings.Tracing.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

procedure TfrmCallStackView.FTVPCallStackDoubleClick(Sender: TObject);
var
  LV : ILogViewer;
begin
  if Supports(Owner, ILogViewer, LV) then
  begin
    if Assigned(FTVPCallStack.SelectedItem) then
    begin
      LV.SelectedLogNode := (FTVPCallStack.SelectedItem as TCallStackData).Node1;
    end;
  end
end;

procedure TfrmCallStackView.SettingsChanged(Sender: TObject);
begin
  FTVPCallStack.ShowHeader := FSettings.ColumnHeadersVisible;
end;
{$ENDREGION}

end.
