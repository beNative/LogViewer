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

unit LogViewer.DataSet.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Comp.DataSet,
  FireDAC.Stan.StorageBin,

  DDuce.Components.DBGridView;

type
  TfrmDataSetView = class(TForm)
    dscMain : TDataSource;

  private
    FDataSet    : TFDMemTable;
    FDBGridView : TDBGridView;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(AStream: TStream);
  end;

implementation

{$R *.dfm}

uses
  Spring,

  DDuce.Factories.GridView;

{$REGION 'construction and destruction'}
procedure TfrmDataSetView.AfterConstruction;
begin
  inherited AfterConstruction;
  FDataSet    := TFDMemTable.Create(Self);
  FDBGridView := TGridViewFactory.CreateDBGridView(Self, Self, dscMain);
  FDBGridView.AlignWithMargins := False;
  FDBGridView.GridLines        := False;
  FDBGridView.BorderStyle      := bsNone;
end;

destructor TfrmDataSetView.Destroy;
begin
  FreeAndNil(FDataSet);
  FreeAndNil(FDBGridView);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmDataSetView.Clear;
begin
  FDataSet.Active := False;
end;

procedure TfrmDataSetView.LoadFromStream(AStream: TStream);
begin
  if Assigned(AStream) then
  begin
    AStream.Position := 0;
    FDataSet.LoadFromStream(AStream);
    dscMain.DataSet := FDataSet;
    FDBGridView.AutoSizeCols;
  end
  else
    Clear;
end;
{$ENDREGION}

end.
