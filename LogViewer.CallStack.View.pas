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
  DSharp.Core.DataTemplates,

  LogViewer.CallStack.Data;

type
  TfrmCallStackView = class(TForm)
  private
    FTVPCallStack : TTreeViewPresenter;
    FVSTCallStack : TVirtualStringTree;
    FCallStack    : IObjectList;

    procedure FCallStackChanged(
      Sender     : TObject;
      const Item : TObject;
      Action     : TCollectionChangedAction
    );

  public
    constructor Create(
      AOwner : TComponent;
      AData  : IObjectList
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Factories, DDuce.Factories.VirtualTrees;

{$REGION 'construction and destruction'}
constructor TfrmCallStackView.Create(AOwner: TComponent; AData: IObjectList);
begin
  inherited Create(AOwner);
  FCallStack := AData;
  FCallStack.OnChanged.Add(FCallStackChanged);
  FVSTCallStack := TVirtualStringTreeFactory.CreateGrid(Self, Self);
  FTVPCallStack := TFactories.CreateTreeViewPresenter(
    Self,
    FVSTCallStack,
    FCallStack as IObjectList
  );
  FTVPCallStack.ShowHeader := False;
end;

procedure TfrmCallStackView.BeforeDestruction;
begin
  FCallStack.OnChanged.Remove(FCallStackChanged);
  FCallStack := nil;
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
{$ENDREGION}

end.
