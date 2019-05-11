{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.DisplayValues.Settings.View;

{ Settings view for display values configuration. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  zObjInspector,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,

  DDuce.Settings.TextFormat,

  LogViewer.DisplayValues.Settings;

type
  TfrmDisplayValuesSettings = class(TForm)
    pnlRight    : TPanel;
    pnlLeft     : TPanel;
    splVertical : TSplitter;

  private
    FSettings      : TDisplayValuesSettings;
    FInspector     : TzObjectInspector;
    FListViewer    : TVirtualStringTree;
    FList          : IList<TTextFormatSettings>;
    FListPresenter : TTreeViewPresenter;

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

    procedure FListPresenterSelectionChanged(Sender: TObject);
    procedure FFormatSettingsChanged(Sender: TObject);

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TDisplayValuesSettings
    ); reintroduce;

  end;

implementation

{$R *.dfm}

uses
  DSharp.Windows.ControlTemplates,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.zObjInspector,
  DDuce.Factories.VirtualTrees;

{$REGION 'construction and destruction'}
constructor TfrmDisplayValuesSettings.Create(AOwner: TComponent;
  ASettings: TDisplayValuesSettings);
var
  CDS : IColumnDefinitions;
  CD  : TColumnDefinition;
begin
  inherited Create(AOwner);
  FSettings   := ASettings;
  FSettings.OnChanged.Add(FFormatSettingsChanged);
  FInspector  := TzObjectInspectorFactory.Create(Self, pnlRight);
  FInspector.AlignWithMargins := False;

  FListViewer := TVirtualStringTreeFactory.CreateList(Self, pnlLeft);
  FListViewer.Header.AutoSizeIndex := 0;
  FListViewer.AlignWithMargins := False;

  FList := TCollections.CreateObjectList<TTextFormatSettings>(False);
  FList.Add(FSettings.TimeStamp);
  FList.Add(FSettings.ValueName);
  FList.Add(FSettings.ValueType);
  FList.Add(FSettings.Value);
  FList.Add(FSettings.Id);
  FList.Add(FSettings.Info);
  FList.Add(FSettings.Warning);
  FList.Add(FSettings.Error);
  FList.Add(FSettings.CheckPoint);
  FList.Add(FSettings.Counter);
  FList.Add(FSettings.Tracing);
  FList.Add(FSettings.Enter);
  FList.Add(FSettings.Leave);
  FList.Add(FSettings.Conditional);

  CDS                  := TFactories.CreateColumnDefinitions;
  CD                   := CDS.Add('Name');
  CD.ValuePropertyName := 'Name';
  CD.OnCustomDraw      := FCDNameCustomDraw;
  CD.AutoSize          := True;

  FListPresenter := TFactories.CreateTreeViewPresenter(
    Self,
    FListViewer,
    FList as IObjectList,
    CDS
  );
  FListPresenter.ShowHeader := False;
  FListPresenter.OnSelectionChanged := FListPresenterSelectionChanged;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmDisplayValuesSettings.FListPresenterSelectionChanged(
  Sender: TObject);
begin
  FInspector.Component := FListPresenter.SelectedItem;
end;

function TfrmDisplayValuesSettings.FCDNameCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  FS : TTextFormatSettings;
begin
  FS := Item as TTextFormatSettings;
  if DrawMode = dmBeforeCellPaint then
  begin
    TargetCanvas.Brush.Color := FS.BackgroundColor;
    TargetCanvas.FillRect(CellRect);
  end
  else if DrawMode = dmPaintText then
  begin
    FS.AssignTo(TargetCanvas.Font);
  end;
  Result := True;
end;

procedure TfrmDisplayValuesSettings.FFormatSettingsChanged(Sender: TObject);
begin
  UpdateActions;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDisplayValuesSettings.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(FListViewer) then
    FListViewer.Invalidate;
end;
{$ENDREGION}

end.
