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

unit LogViewer.DisplayValues.Settings.View;

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
    pnlRight : TPanel;
    pnlLeft  : TPanel;

  private
    FSettings      : TDisplayValuesSettings;
    FInspector     : TzObjectInspector;
    FListViewer    : TVirtualStringTree;
    FList          : IList<TTextFormatSettings>;
    FListPresenter : TTreeViewPresenter;

    function FCDNameCustomDraw(
      Sender          : TObject;
      ColumnDefinition: TColumnDefinition;
      Item            : TObject;
      TargetCanvas    : TCanvas;
      CellRect        : TRect;
      ImageList       : TCustomImageList;
      DrawMode        : TDrawMode;
      Selected        : Boolean
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
    procedure BeforeDestruction; override;



  end;

implementation

{$R *.dfm}

uses
  DSharp.Windows.ControlTemplates,

  DDuce.Factories,

  DDuce.Factories.zObjInspector, DDuce.Factories.VirtualTrees;

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
  FListViewer := TVirtualStringTreeFactory.CreateList(Self, pnlLeft);
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
  FList.Add(FSettings.Conditional);

  CDS                  := TFactories.CreateColumnDefinitions;
  CD                   := CDS.Add('Name');
  CD.ValuePropertyName := 'Name';
  CD.OnCustomDraw      := FCDNameCustomDraw;

  FListPresenter := TFactories.CreateTreeViewPresenter(
    Self,
    FListViewer,
    FList as IObjectList,
    CDS
  );
  FListPresenter.ShowHeader := False;
  FListPresenter.OnSelectionChanged := FListPresenterSelectionChanged;



//  CD.ValuePropertyName := 'TimeStamp';
//  CD.HintPropertyName  := CD.ValuePropertyName;
//  //CD.OnCustomDraw      := FCDNameCustomDraw;
//  CD                   := CDS.Add('ValueType');
//  CD.ValuePropertyName := 'ValueType';
//  CD.HintPropertyName  := CD.ValuePropertyName;
//  CD.OnCustomDraw      := FCDTypeCustomDraw;
//  CD                   := CDS.Add('Value');
//  CD.ValuePropertyName := 'Value';
//  CD.HintPropertyName  := CD.ValuePropertyName;
//  CD.AutoSize          := True; // Test
//  CD.OnCustomDraw      := FCDValueCustomDraw;
//  CD                   := CDS.Add('TimeStamp');
//  CD.Width             := 80;
//  CD.ValuePropertyName := 'TimeStamp';
//  CD.HintPropertyName  := CD.ValuePropertyName;
//  CD.OnGetText         := FCDTimeStampGetText;
//  CD.OnCustomDraw      := FCDTimeStampCustomDraw;
end;

procedure TfrmDisplayValuesSettings.BeforeDestruction;
begin
  //FreeAndNil(FListPresenter);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmDisplayValuesSettings.FListPresenterSelectionChanged(
  Sender: TObject);
begin
  FInspector.Component := FListPresenter.SelectedItem;
end;

procedure TfrmDisplayValuesSettings.UpdateActions;
begin
  inherited UpdateActions;
  if Assigned(FListViewer) then
    FListViewer.Invalidate;
end;

function TfrmDisplayValuesSettings.FCDNameCustomDraw(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject; TargetCanvas: TCanvas;
  CellRect: TRect; ImageList: TCustomImageList; DrawMode: TDrawMode;
  Selected: Boolean): Boolean;
var
  FS : TTextFormatSettings;
begin
  FS := Item as TTextFormatSettings;
  if DrawMode = dmPaintText then
  begin
    FS.AssignTo(TargetCanvas.Font);
    TargetCanvas.Brush.Color := FS.BackgroundColor;
  end;
  Result := True;
end;

procedure TfrmDisplayValuesSettings.FFormatSettingsChanged(Sender: TObject);
begin
  UpdateActions;
end;
{$ENDREGION}

end.
