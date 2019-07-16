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
    pnlRight : TPanel;
    pnlLeft  : TPanel;

  private
    FLogNode               : TLogNode;
    FLogNodeDataView       : TValueList;
    FData                  : DynamicRecord;
    FDisplayValuesSettings : TDisplayValuesSettings;

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

  public
    constructor Create(
      AOwner                 : TComponent;
      ADisplayValuesSettings : TDisplayValuesSettings
    ); reintroduce; virtual;
    procedure AfterConstruction; override;


    procedure Clear;

    property LogNode: TLogNode
      read GetLogNode write SetLogNode;
  end;

implementation

{$R *.dfm}

uses
  Spring,

  DDuce.Logger.Interfaces;

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
  FLogNodeDataView.ShowGutter        := False;
  FLogNodeDataView.OnPaintText       := FLogNodeDataViewPaintText;
  FLogNodeDataView.OnBeforeCellPaint := FLogNodeDataViewBeforeCellPaint;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMessageDataView.GetLogNode: TLogNode;
begin
  Result := FLogNode;
end;

procedure TfrmMessageDataView.SetLogNode(const Value: TLogNode);
begin
  if Value <> FLogNode then
  begin
    FLogNode := Value;
    if Assigned(FLogNode) then
    begin
      FData.From(FLogNode);
      // we need to use LogMessageTypeNameOf as no RTTI is generated for this
      // type.
      FData['MessageType']  := LogMessageTypeNameOf(FLogNode.MessageType);
      FLogNodeDataView.Data := FData;
      FLogNodeDataView.Refresh;
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

  if Column = 1 then
  begin
    //FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas);
  end
  else if Column = 2 then
  begin
    N := Sender.GetNodeData<TValueListNode>(Node);
    if N.Data.Name = 'Id' then
      FDisplayValuesSettings.Id.AssignTo(TargetCanvas)
    else if N.Data.Name = 'ValueName' then
      FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas)
    else if N.Data.Name = 'Value' then
      FDisplayValuesSettings.Value.AssignTo(TargetCanvas)
    else if N.Data.Name = 'ValueType' then
      FDisplayValuesSettings.ValueType.AssignTo(TargetCanvas)
    else if N.Data.Name = 'TimeStamp' then
      FDisplayValuesSettings.TimeStamp.AssignTo(TargetCanvas)
    else if N.Data.Name = 'Text' then
    begin
      case LogNode.MessageType of
        lmtInfo:
          FDisplayValuesSettings.Info.AssignTo(TargetCanvas);
        lmtError:
          FDisplayValuesSettings.Error.AssignTo(TargetCanvas);
        lmtWarning:
          FDisplayValuesSettings.Warning.AssignTo(TargetCanvas);
        lmtEnterMethod:
          FDisplayValuesSettings.Enter.AssignTo(TargetCanvas);
        lmtLeaveMethod:
          FDisplayValuesSettings.Leave.AssignTo(TargetCanvas);
        lmtConditional:
          FDisplayValuesSettings.Conditional.AssignTo(TargetCanvas);
        lmtCheckpoint:
          FDisplayValuesSettings.CheckPoint.AssignTo(TargetCanvas);
//        lmtStrings: ;
//        lmtCallStack: ;
//        lmtComponent: ;
//        lmtException: ;
//        lmtBitmap: ;
//        lmtHeapInfo: ;
//        lmtMemory: ;
//        lmtCustomData: ;
//        lmtObject: ;
//        lmtInterface: ;
//        lmtPersistent: ;
//        lmtReserved: ;
//        lmtWatch: ;
//        lmtCounter: ;
//        lmtColor: ;
//        lmtAlphaColor: ;
//        lmtScreenShot: ;
//        lmtText: ;
//        lmtDataSet: ;
//        lmtAction: ;
//        lmtClear: ;
//        lmtNone: ;
//        lmtExtreme: ;
      end;
    end;
  end;
  TargetCanvas.FillRect(CellRect);
end;

procedure TfrmMessageDataView.FLogNodeDataViewPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  N  : TValueListNode;
begin
  if Column = 1 then
  begin
    //FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas.Font);
  end
  else if Column = 2 then
  begin
    N := Sender.GetNodeData<TValueListNode>(Node);
    if N.Data.Name = 'Id' then
      FDisplayValuesSettings.Id.AssignTo(TargetCanvas)
    else if N.Data.Name = 'ValueName' then
      FDisplayValuesSettings.ValueName.AssignTo(TargetCanvas)
    else if N.Data.Name = 'Value' then
      FDisplayValuesSettings.Value.AssignTo(TargetCanvas)
    else if N.Data.Name = 'ValueType' then
      FDisplayValuesSettings.ValueType.AssignTo(TargetCanvas)
    else if N.Data.Name = 'TimeStamp' then
      FDisplayValuesSettings.TimeStamp.AssignTo(TargetCanvas)
    else if N.Data.Name = 'Text' then
    begin
      case LogNode.MessageType of
        lmtInfo:
          FDisplayValuesSettings.Info.AssignTo(TargetCanvas);
        lmtError:
          FDisplayValuesSettings.Error.AssignTo(TargetCanvas);
        lmtWarning:
          FDisplayValuesSettings.Warning.AssignTo(TargetCanvas);
        lmtEnterMethod:
          FDisplayValuesSettings.Enter.AssignTo(TargetCanvas);
        lmtLeaveMethod:
          FDisplayValuesSettings.Leave.AssignTo(TargetCanvas);
        lmtConditional:
          FDisplayValuesSettings.Conditional.AssignTo(TargetCanvas);
        lmtCheckpoint:
          FDisplayValuesSettings.CheckPoint.AssignTo(TargetCanvas);
      end;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmMessageDataView.Clear;
begin
  FLogNodeDataView.Clear;
end;
{$ENDREGION}

end.
