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

  OMultiPanel,

  DDuce.Components.ValueList, DDuce.DynamicRecord,

  LogViewer.MessageList.LogNode;

type
  TfrmMessageDataView = class(TForm)
    pnlMain  : TOMultiPanel;
    pnlRight : TPanel;
    pnlLeft  : TPanel;

  private
    FLogNode         : TLogNode;
    FLogNodeDataView : TValueList;
    FData            : DynamicRecord;

    function GetLogNode: TLogNode;
    procedure SetLogNode(const Value: TLogNode);

  public
    procedure AfterConstruction; override;

    property LogNode: TLogNode
      read GetLogNode write SetLogNode;


  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmMessageDataView.AfterConstruction;
begin
  inherited AfterConstruction;
  FLogNodeDataView             := TValueList.Create(Self);
  FLogNodeDataView.Parent      := pnlLeft;
  FLogNodeDataView.Align       := alClient;
  FLogNodeDataView.ShowHeader  := False;
  FLogNodeDataView.Editable    := False;
  FLogNodeDataView.BorderStyle := bsNone;
  FLogNodeDataView.ShowGutter  := False;
end;
{$ENDREGION}

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
      FLogNodeDataView.Data := FData;
      FLogNodeDataView.Refresh
    end;
  end;
end;

end.
