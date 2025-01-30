{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.LogLevels.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  Spring.Collections,

  zObjInspector, zObjInspTypes,

  VirtualTrees,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,

  LogViewer.LogLevels.Settings;

type
  TfrmLogLevelSettings = class(TForm)
    splVertical : TSplitter;
    pnlRight    : TPanel;
    pnlLeft     : TPanel;

  private
    FSettings      : TLogLevelSettings;
    FListViewer    : TVirtualStringTree;
    FListPresenter : TTreeViewPresenter;
    FInspector     : TzObjectInspector;

    procedure FListPresenterSelectionChanged(Sender: TObject);

    function FCDLogColorCustomDraw(
      Sender           : TObject;
      ColumnDefinition : TColumnDefinition;
      Item             : TObject;
      TargetCanvas     : TCanvas;
      CellRect         : TRect;
      ImageList        : TCustomImageList;
      DrawMode         : TDrawMode;
      Selected         : Boolean
    ): Boolean;

    procedure FSettingsChanged(Sender: TObject);

    procedure CreateObjects;

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TLogLevelSettings
    ); reintroduce;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

uses
  System.TypInfo,

  DSharp.Windows.ControlTemplates,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.zObjInspector,
  DDuce.Factories.VirtualTrees, DDuce.Logger, DDuce.Logger.Interfaces,

  LogViewer.Resources;

{$REGION 'construction and destruction'}
constructor TfrmLogLevelSettings.Create(AOwner: TComponent;
  ASettings: TLogLevelSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  FSettings.OnChanged.Add(FSettingsChanged);
  CreateObjects;
end;

procedure TfrmLogLevelSettings.CreateObjects;
var
  CDS : IColumnDefinitions;
  CD  : TColumnDefinition;
begin
  FInspector  := TzObjectInspectorFactory.Create(Self, pnlRight);
  FInspector.AlignWithMargins := False;
  FInspector.ObjectVisibility := mvPublished;
  FInspector.SplitterPos      := 60;

  FListViewer := TVirtualStringTreeFactory.CreateList(Self, pnlLeft);
  FListViewer.Header.AutoSizeIndex := 0;
  FListViewer.AlignWithMargins := False;

  CDS            := TFactories.CreateColumnDefinitions;
  FListPresenter := TFactories.CreateTreeViewPresenter(
    Self,
    FListViewer,
    FSettings.LogLevels as IObjectList,
    CDS
  );
  FListPresenter.ShowHeader := True;
  FListViewer.AlignWithMargins := False;
  CD                   := CDS.Add(COLUMNNAME_LOGLEVEL);
  CD.ValuePropertyName := COLUMNNAME_LOGLEVEL;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Caption           := '#';
  CD.Width             := 40;
  CD.MinWidth          := 40;
  CD.AutoSize          := True;
  CD.Alignment         := taCenter;
  //CD.OnCustomDraw      := FCDLogLevelCustomDraw;

  CD                   := CDS.Add(COLUMNNAME_LOGCOLOR);
  CD.ValuePropertyName := COLUMNNAME_LOGCOLOR;
  CD.HintPropertyName  := CD.ValuePropertyName;
  // Only one column in the header can have AutoSize = True. This property
  // determines the value of the Header.AutoSizeIndex property of the treeview.
  // This property will cause the columns to fit to the width of the control,
  // and will reduce the width of the AutoSizeIndex column if needed when the
  // list width is adjusted.
  CD.Width             := 40;
  CD.MinWidth          := 40;
  CD.AutoSize          := True;
  CD.OnCustomDraw      := FCDLogColorCustomDraw;
  CD.Caption           := '';

  CD                   := CDS.Add(COLUMNNAME_LOGCOLORNAME);
  CD.ValuePropertyName := COLUMNNAME_LOGCOLORNAME;
  CD.HintPropertyName  := CD.ValuePropertyName;
  // Only one column in the header can have AutoSize = True. This property
  // determines the value of the Header.AutoSizeIndex property of the treeview.
  // This property will cause the columns to fit to the width of the control,
  // and will reduce the width of the AutoSizeIndex column if needed when the
  // list width is adjusted.
  CD.AutoSize          := True;
  CD.Caption           := SColor;

  CD                   := CDS.Add(COLUMNNAME_LOGALIAS);
  CD.ValuePropertyName := COLUMNNAME_LOGALIAS;
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.Caption           := SLogLevelAlias;
  FListPresenter.OnSelectionChanged := FListPresenterSelectionChanged;
end;

destructor TfrmLogLevelSettings.Destroy;
begin
  FSettings.OnChanged.RemoveAll(Self);
  FSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmLogLevelSettings.FCDLogColorCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
begin
  Result := False;
  if DrawMode = dmAfterCellPaint then
  begin
    TargetCanvas.Brush.Color := TLogLevelProperties(Item).Color;
    TargetCanvas.FillRect(CellRect);
    Result := True;
  end;
end;

procedure TfrmLogLevelSettings.FListPresenterSelectionChanged(Sender: TObject);
begin
  Logger.SendObject('Sender', FListPresenter.SelectedItem);
  if Assigned(FListPresenter.SelectedItem) then
  begin
    FInspector.Component := FListPresenter.SelectedItem;
    FListViewer.SetFocus;
  end;
end;

procedure TfrmLogLevelSettings.FSettingsChanged(Sender: TObject);
begin
  UpdateActions;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmLogLevelSettings.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(FListViewer) then
  begin
    FListViewer.Invalidate;
  end;
end;
{$ENDREGION}

end.
