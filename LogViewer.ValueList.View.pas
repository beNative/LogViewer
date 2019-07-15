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

unit LogViewer.ValueList.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  VirtualTrees, OMultiPanel,

  DDuce.DynamicRecord, DDuce.Components.ValueList,

  LogViewer.DisplayValues.Settings;

type
  TfrmValueListView = class(TForm)
    pnlMain   : TOMultiPanel;
    pnlBottom : TPanel;
    pnlTop    : TPanel;

  private
    FFieldView             : TValueList;
    FPropertyView          : TValueList;
    FData                  : IDynamicRecord;
    FDisplayValuesSettings : TDisplayValuesSettings;

    {$REGION 'property access methods'}
    function GetFields: IDynamicRecord;
    function GetProperties: IDynamicRecord;
    procedure SetFields(const Value: IDynamicRecord);
    procedure SetProperties(const Value: IDynamicRecord);
    function GetData: IDynamicRecord;
    procedure SetData(const Value: IDynamicRecord);
    {$ENDREGION}

    procedure FValueListViewPaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure FValueListViewBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );

  protected
    procedure UpdateData;

  public
    constructor Create(
      AOwner                 : TComponent;
      ADisplayValuesSettings : TDisplayValuesSettings
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Clear;

    property Data: IDynamicRecord
      read GetData write SetData;

    property Fields: IDynamicRecord
      read GetFields write SetFields;

    property Properties: IDynamicRecord
      read GetProperties write SetProperties;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmValueListView.Create(AOwner: TComponent;
  ADisplayValuesSettings: TDisplayValuesSettings);
begin
  inherited Create(AOwner);
  FDisplayValuesSettings := ADisplayValuesSettings;
end;

procedure TfrmValueListView.AfterConstruction;
begin
  inherited AfterConstruction;
  FFieldView                   := TValueList.Create(Self);
  FFieldView.Parent            := pnlTop;
  FFieldView.Align             := alClient;
  FFieldView.ShowHeader        := False;
  FFieldView.Editable          := False;
  FFieldView.BorderStyle       := bsNone;
  FFieldView.ShowGutter        := False;
  FFieldView.OnPaintText       := FValueListViewPaintText;
  FFieldView.OnBeforeCellPaint := FValueListViewBeforeCellPaint;

  FPropertyView                   := TValueList.Create(Self);
  FPropertyView.Parent            := pnlBottom;
  FPropertyView.Align             := alClient;
  FPropertyView.ShowHeader        := False;
  FPropertyView.Editable          := False;
  FPropertyView.BorderStyle       := bsNone;
  FPropertyView.ShowGutter        := False;
  FPropertyView.OnPaintText       := FValueListViewPaintText;
  FPropertyView.OnBeforeCellPaint := FValueListViewBeforeCellPaint;
end;

destructor TfrmValueListView.Destroy;
begin
  FData              := nil;
  FPropertyView.Data := nil;
  FFieldView.Data    := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmValueListView.FValueListViewBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if Column = 1 then
  begin
    FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas);
  end
  else if Column = 2 then
  begin
    FDisplayValuesSettings.Value.AssignTo(TargetCanvas);
  end;
  TargetCanvas.FillRect(CellRect);
end;

procedure TfrmValueListView.FValueListViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Column = 1 then
  begin
    FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas.Font);
  end
  else if Column = 2 then
  begin
    FDisplayValuesSettings.Value.AssignTo(TargetCanvas.Font);
  end;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TfrmValueListView.Clear;
begin
  FFieldView.Clear;
  FPropertyView.Clear;
end;

function TfrmValueListView.GetData: IDynamicRecord;
begin
  Result := FData;
end;

procedure TfrmValueListView.SetData(const Value: IDynamicRecord);
begin
  Clear;
  FData := Value;
  UpdateData;
end;

function TfrmValueListView.GetFields: IDynamicRecord;
begin
  Result := FFieldView.Data;
end;

procedure TfrmValueListView.SetFields(const Value: IDynamicRecord);
begin
  FFieldView.Data := Value;
end;

function TfrmValueListView.GetProperties: IDynamicRecord;
begin
  Result := FPropertyView.Data;
end;

procedure TfrmValueListView.SetProperties(const Value: IDynamicRecord);
begin
  FPropertyView.Data := Value;
end;
procedure TfrmValueListView.UpdateData;
var
  LFieldData    : DynamicRecord;
  LPropertyData : DynamicRecord;
  F             : IDynamicField;
  B             : Boolean;
begin
  if Assigned(Data) then
  begin
    if Data.ContainsField('') then
    begin
      B := False;
      for F in Data do
      begin
        if F.Name = '' then
          B := True
        else
        begin
          if B then
            LPropertyData[F.Name] := F.Value
          else
            LFieldData[F.Name] := F.Value;
        end;
      end;
      FFieldView.Data := LFieldData;
      FPropertyView.Data := LPropertyData;
      pnlMain.PanelCollection[1].Visible := True;
    end
    else
    begin
      FFieldView.Data := FData;
      pnlMain.PanelCollection[1].Visible := False;
    end;
  end;
end;
{$ENDREGION}

end.
