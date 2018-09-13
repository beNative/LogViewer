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

unit LogViewer.CallStack.View;

interface

{ TVirtualStringTree - TTreeViewPresenter combination to display the callstack }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  VirtualTrees,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,

  LogViewer.DisplayValues.Settings;

type
  TfrmCallStackView = class(TForm)
  private
    FTVPCallStack          : TTreeViewPresenter;
    FVSTCallStack          : TVirtualStringTree;
    FCallStack             : IObjectList;
    FDisplayValuesSettings : TDisplayValuesSettings;

    procedure FCallStackChanged(
      Sender     : TObject;
      const Item : TObject;
      Action     : TCollectionChangedAction
    );
    function FCDLevelCustomDraw(
      Sender          : TObject;
      ColumnDefinition: TColumnDefinition;
      Item            : TObject;
      TargetCanvas    : TCanvas;
      CellRect        : TRect;
      ImageList       : TCustomImageList;
      DrawMode        : TDrawMode;
      Selected        : Boolean
    ): Boolean;
    function FCDTitleCustomDraw(
      Sender          : TObject;
      ColumnDefinition: TColumnDefinition;
      Item            : TObject;
      TargetCanvas    : TCanvas;
      CellRect        : TRect;
      ImageList       : TCustomImageList;
      DrawMode        : TDrawMode;
      Selected        : Boolean
    ): Boolean;

  public
    constructor Create(
      AOwner                 : TComponent;
      AData                  : IObjectList;
      ADisplayValuesSettings : TDisplayValuesSettings
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring,

  DSharp.Windows.ControlTemplates,

  DDuce.Factories.TreeViewPresenter, DDuce.Factories.VirtualTrees,

  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TfrmCallStackView.Create(AOwner: TComponent; AData: IObjectList;
  ADisplayValuesSettings: TDisplayValuesSettings);
var
  CDS : IColumnDefinitions;
  CD  : TColumnDefinition;
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AData, 'AData');
  Guard.CheckNotNull(ADisplayValuesSettings, 'ADisplayValuesSettings');
  FDisplayValuesSettings := ADisplayValuesSettings;
  FCallStack := AData;
  FCallStack.OnChanged.Add(FCallStackChanged);
  FVSTCallStack := TVirtualStringTreeFactory.CreateList(Self, Self);
  FVSTCallStack.AlignWithMargins := False;
  CDS                  := TFactories.CreateColumnDefinitions;
  CD                   := CDS.Add('Level');
  CD.ValuePropertyName := 'Level';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnCustomDraw      := FCDLevelCustomDraw;
  CD.AutoSize          := True;
  CD                   := CDS.Add('Title');
  CD.ValuePropertyName := 'Title';
  CD.HintPropertyName  := CD.ValuePropertyName;
  CD.OnCustomDraw      := FCDTitleCustomDraw;
  CD.AutoSize          := True;
  FTVPCallStack := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTCallStack,
    FCallStack as IObjectList,
    CDS
  );
  FTVPCallStack.ShowHeader := False;
end;

procedure TfrmCallStackView.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FCallStack.OnChanged.Remove(FCallStackChanged);
  FCallStack := nil;
  FDisplayValuesSettings := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmCallStackView.FCallStackChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
begin
  FTVPCallStack.Refresh;
  FTVPCallStack.TreeView.Header.AutoFitColumns;
end;

function TfrmCallStackView.FCDLevelCustomDraw(Sender: TObject;
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
{$ENDREGION}

end.
