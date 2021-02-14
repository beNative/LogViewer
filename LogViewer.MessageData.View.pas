{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.MessageData.View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  OMultiPanel, VirtualTrees,

  DDuce.Components.ValueList, DDuce.DynamicRecord,

  LogViewer.DisplayValues.Settings, LogViewer.MessageList.LogNode;

type
  TfrmMessageDataView = class(TForm)
    pnlMain  : TOMultiPanel;
    pnlLeft  : TPanel;

  private
    FLogNode               : TLogNode;
    FLogNodeDataView       : TValueList;
    FData                  : DynamicRecord;
    FDisplayValuesSettings : TDisplayValuesSettings;

    procedure ApplyDisplaySettings(
      const AName   : string;
      const ACanvas : TCanvas
    );
    procedure AssignData;

    {$REGION 'property access methods'}
    function GetLogNode: TLogNode;
    procedure SetLogNode(const Value: TLogNode);
    {$ENDREGION}

    procedure FLogNodeDataViewPaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure FLogNodeDataViewBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );
    function GetData: IDynamicRecord;

  public
    constructor Create(
      AOwner                 : TComponent;
      ADisplayValuesSettings : TDisplayValuesSettings
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure Clear;
    procedure Refresh;

    property LogNode: TLogNode
      read GetLogNode write SetLogNode;

    property Data: IDynamicRecord
      read GetData;
  end;

implementation

{$R *.dfm}

uses
  System.Rtti,

  Spring,

  DDuce.Settings.TextFormat, DDuce.Logger.Interfaces;

{$REGION 'construction and destruction'}
constructor TfrmMessageDataView.Create(AOwner: TComponent;
  ADisplayValuesSettings: TDisplayValuesSettings);
begin
  inherited Create(AOwner);
  FDisplayValuesSettings := ADisplayValuesSettings;
end;

procedure TfrmMessageDataView.AfterConstruction;
begin
  inherited AfterConstruction;
  FLogNodeDataView                   := TValueList.Create(Self);
  FLogNodeDataView.Parent            := pnlLeft;
  FLogNodeDataView.Align             := alClient;
  FLogNodeDataView.ShowHeader        := False;
  FLogNodeDataView.Editable          := False;
  FLogNodeDataView.BorderStyle       := bsNone;
  //FLogNodeDataView.ShowGutter        := False;
  FLogNodeDataView.OnPaintText       := FLogNodeDataViewPaintText;
  FLogNodeDataView.OnBeforeCellPaint := FLogNodeDataViewBeforeCellPaint;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMessageDataView.GetData: IDynamicRecord;
begin
  Result := FData;
end;

function TfrmMessageDataView.GetLogNode: TLogNode;
begin
  Result := FLogNode;
end;

procedure TfrmMessageDataView.Refresh;
begin
  FLogNodeDataView.Data := FData;
  FLogNodeDataView.Refresh;
end;

procedure TfrmMessageDataView.SetLogNode(const Value: TLogNode);
begin
  if Value <> FLogNode then
  begin
    FLogNode := Value;
    if Assigned(FLogNode) then
    begin
      AssignData;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMessageDataView.FLogNodeDataViewBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  N  : TValueListNode;
begin
  if Column = 2 then
  begin
    N := Sender.GetNodeData<TValueListNode>(Node);
    ApplyDisplaySettings(N.Data.Name, TargetCanvas);
  end;
  TargetCanvas.FillRect(CellRect);
end;

procedure TfrmMessageDataView.FLogNodeDataViewPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  N  : TValueListNode;
begin
  if Column = 2 then
  begin
    N := Sender.GetNodeData<TValueListNode>(Node);
    ApplyDisplaySettings(N.Data.Name, TargetCanvas);
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMessageDataView.ApplyDisplaySettings(const AName: string;
  const ACanvas: TCanvas);
var
  TFS : TTextFormatSettings;
begin
  TFS := nil;
  if AName = 'Id' then
    TFS := FDisplayValuesSettings.Id
  else if AName = 'ValueName' then
    TFS := FDisplayValuesSettings.ValueName
  else if AName = 'Value' then
    TFS := FDisplayValuesSettings.Value
  else if AName = 'ValueType' then
    TFS := FDisplayValuesSettings.ValueType
  else if AName = 'TimeStamp' then
    TFS := FDisplayValuesSettings.TimeStamp
  else if AName = 'Text' then
  begin
    case LogNode.MessageType of
      lmtInfo:
        TFS := FDisplayValuesSettings.Info;
      lmtError:
        TFS := FDisplayValuesSettings.Error;
      lmtWarning:
        TFS := FDisplayValuesSettings.Warning;
      lmtEnterMethod:
        TFS := FDisplayValuesSettings.Enter;
      lmtLeaveMethod:
        TFS := FDisplayValuesSettings.Leave;
      lmtConditional:
        TFS := FDisplayValuesSettings.Conditional;
      lmtCheckpoint:
        TFS := FDisplayValuesSettings.CheckPoint;
      lmtAction:
        TFS := FDisplayValuesSettings.Action;
    end;
  end;
  if Assigned(TFS) then
    TFS.AssignTo(ACanvas);
end;

procedure TfrmMessageDataView.AssignData;
begin
  FData.Clear;
  FData['Id']       := FLogNode.Id;
  FData['LogLevel'] := FLogNode.LogLevel;
  // we need to use LogMessageTypeNameOf as no RTTI is generated for this
  // type.
  FData['MessageType'] := LogMessageTypeNameOf(FLogNode.MessageType);
  FData['TimeStamp']   := FLogNode.TimeStamp;
  if not FLogNode.Text.IsEmpty then
  begin
    FData['Text']     := FLogNode.Text;
    FData['TextSize'] := FLogNode.TextSize;
  end;
  if not FLogNode.ValueName.IsEmpty then
    FData['ValueName'] := FLogNode.ValueName;
  if not FLogNode.ValueType.IsEmpty then
    FData['ValueType'] := FLogNode.ValueType;
  if not FLogNode.Highlighter.IsEmpty then
    FData['Highlighter'] := FLogNode.Highlighter;
  if Assigned(FLogNode.MessageData) then
    FData['DataSize'] := FLogNode.MessageData.Size;
  if not FLogNode.Value.IsEmpty then
    FData['Value'] := FLogNode.Value;
  FLogNodeDataView.Data := FData;
  FLogNodeDataView.Refresh;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmMessageDataView.Clear;
begin
  FLogNodeDataView.Clear;
end;
{$ENDREGION}

end.
