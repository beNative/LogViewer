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

unit LogViewer.Settings.Dialog;

{ Application settings dialog. }

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.ActnList, Vcl.StdCtrls, Vcl.ImgList,

  VirtualTrees,

  SynEditHighlighter, SynEditCodeFolding, SynHighlighterJScript, SynEdit,

  DDuce.Editor.Interfaces, DDuce.Components.VirtualTrees.Node,

  LogViewer.Settings, LogViewer.Settings.Dialog.Data,
  LogViewer.Comport.Settings.View, LogViewer.WinIPC.Settings.View,
  LogViewer.Watches.Settings.View, LogViewer.WinODS.Settings.View,
  LogViewer.ZeroMQ.Settings.View, LogViewer.DisplayValues.Settings.View;

type
  TConfigNode = TVTNode<TConfigData>;

type
  TfrmLogViewerSettings = class(TForm)
    {$REGION 'designer controls'}
    aclMain                 : TActionList;
    actApply                : TAction;
    actCancel               : TAction;
    actClose                : TAction;
    btnCancel               : TButton;
    btnClose                : TButton;
    btnClose1               : TButton;
    imlMain                 : TImageList;
    pgcMain                 : TPageControl;
    pnlBottom               : TPanel;
    pnlConfigTree           : TPanel;
    splVertical             : TSplitter;
    tsAdvanced              : TTabSheet;
    tsCallstack             : TTabSheet;
    tsComport               : TTabSheet;
    tsDisplayValuesSettings : TTabSheet;
    tsWatches               : TTabSheet;
    tsWinIPC                : TTabSheet;
    tsWinODS                : TTabSheet;
    tsZeroMQ                : TTabSheet;
    seSettings              : TSynEdit;
    synJScript              : TSynJScriptSyn;
    {$ENDREGION}

    procedure actCloseExecute(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);

  private
    FConfigTree                : TVirtualStringTree;
    FSettings                  : TLogViewerSettings;
    FComportSettingsForm       : TfrmComPortSettings;
    FWatchSettingsForm         : TfrmWatchSettings;
    FWinIPCSettingsForm        : TfrmWinIPCSettings;
    FWinODSSettingsForm        : TfrmWinODSSettings;
    FZeroMQSettingsForm        : TfrmZeroMQSettings;
    FDisplayValuesSettingsForm : TfrmDisplayValuesSettings;

    procedure FConfigTreeGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FConfigTreeFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FConfigTreeFreeNode(
      Sender: TBaseVirtualTree;
      Node: PVirtualNode
    );

  protected
    procedure BuildTree;

    function AddNode(
      AParentNode : TConfigNode;
      const AText : string;
      ATabSheet   : TTabSheet
    ): TConfigNode;

    procedure CreateSettingsForms;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      AOwner    : TComponent;
      ASettings : TLogViewerSettings
    ); reintroduce;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Editor.Factories, DDuce.Factories.VirtualTrees,

  LogViewer.Resources;

{$REGION 'construction and destruction'}
constructor TfrmLogViewerSettings.Create(AOwner: TComponent;
  ASettings: TLogViewerSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmLogViewerSettings.AfterConstruction;
var
  I : Integer;
begin
  inherited AfterConstruction;
  CreateSettingsForms;
  FConfigTree := TVirtualStringTreeFactory.CreateTreeList(Self, pnlConfigTree);
  FConfigTree.OnGetText      := FConfigTreeGetText;
  FConfigTree.OnFreeNode     := FConfigTreeFreeNode;
  FConfigTree.OnFocusChanged := FConfigTreeFocusChanged;
  FConfigTree.Header.Options := FConfigTree.Header.Options - [hoVisible];
  FConfigTree.TreeOptions.PaintOptions :=
    FConfigTree.TreeOptions.PaintOptions + [toShowTreeLines];
  FConfigTree.Margins.Right := 0;

  FConfigTree.NodeDataSize := SizeOf(TConfigNode);
  BuildTree;
  for I := 0 to pgcMain.PageCount - 1 do
  begin
    pgcMain.Pages[I].TabVisible := False;
  end;
  pgcMain.ActivePage := tsDisplayValuesSettings;
  seSettings.Lines.LoadFromFile(FSettings.FileName);
end;

procedure TfrmLogViewerSettings.BeforeDestruction;
begin
  FConfigTree.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLogViewerSettings.FConfigTreeFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  CN: TConfigNode;
begin
  CN := Sender.GetNodeData<TConfigNode>(Node);
  if Assigned(CN.Data.TabSheet) then
    pgcMain.ActivePage := CN.Data.TabSheet;
end;

procedure TfrmLogViewerSettings.FConfigTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  CN : TConfigNode;
begin
  CN := Sender.GetNodeData<TConfigNode>(Node);
  FreeAndNil(CN);
end;

procedure TfrmLogViewerSettings.FConfigTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  CN : TConfigNode;
begin
  CN := Sender.GetNodeData<TConfigNode>(Node);
  CellText := CN.Text;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLogViewerSettings.actApplyExecute(Sender: TObject);
begin
  FSettings.Save;
  seSettings.Lines.LoadFromFile(FSettings.FileName);
end;

procedure TfrmLogViewerSettings.actCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmLogViewerSettings.actCloseExecute(Sender: TObject);
begin
  FSettings.Save;
  Close;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TfrmLogViewerSettings.AddNode(AParentNode: TConfigNode; const AText:
  string;  ATabSheet: TTabSheet): TConfigNode;
begin
  if Assigned(AParentNode) then
  begin
    Result := AParentNode.Add(TConfigData.Create(AText, ATabSheet));
  end
  else
  begin
    Result := TConfigNode.Create(
      FConfigTree,
      TConfigData.Create(AText, ATabSheet)
    );
  end;
  Result.Text := AText;
end;

procedure TfrmLogViewerSettings.BuildTree;
var
  LNode : TConfigNode;
begin
  LNode := AddNode(nil, SViewSettings, nil);
  AddNode(LNode, SDisplaySettings, tsDisplayValuesSettings);
  AddNode(LNode, SWatches, tsWatches);
  AddNode(LNode, SCallStack, tsCallstack);

  LNode := AddNode(nil, SChannelSettings, nil);
  AddNode(LNode, SWinIPC, tsWinIPC);
  AddNode(LNode, SWinODS, tsWinODS);
  AddNode(LNode, SComPort, tsComport);
  AddNode(LNode, SZeroMQ, tsZeroMQ);

  AddNode(nil, SGeneralSettings, nil);
  AddNode(nil, SAdvanced, tsAdvanced);
  FConfigTree.FullExpand;
end;

procedure TfrmLogViewerSettings.CreateSettingsForms;
begin
  FComportSettingsForm := TfrmComPortSettings.Create(Self, FSettings.ComPortSettings);
  FComportSettingsForm.Parent      := tsComport;
  FComportSettingsForm.Align       := alClient;
  FComportSettingsForm.BorderStyle := bsNone;
  FComportSettingsForm.Visible     := True;

  FWatchSettingsForm := TfrmWatchSettings.Create(Self, FSettings.WatchSettings);
  FWatchSettingsForm.Parent      := tsWatches;
  FWatchSettingsForm.Align       := alClient;
  FWatchSettingsForm.BorderStyle := bsNone;
  FWatchSettingsForm.Visible     := True;

  FWinIPCSettingsForm := TfrmWinIPCSettings.Create(Self, FSettings.WinIPCSettings);
  FWinIPCSettingsForm.Parent      := tsWinIPC;
  FWinIPCSettingsForm.Align       := alClient;
  FWinIPCSettingsForm.BorderStyle := bsNone;
  FWinIPCSettingsForm.Visible     := True;

  FWinODSSettingsForm := TfrmWinODSSettings.Create(Self, FSettings.WinODSSettings);
  FWinODSSettingsForm.Parent      := tsWinODS;
  FWinODSSettingsForm.Align       := alClient;
  FWinODSSettingsForm.BorderStyle := bsNone;
  FWinODSSettingsForm.Visible     := True;

  FZeroMQSettingsForm := TfrmZeroMQSettings.Create(Self, FSettings.ZeroMQSettings);
  FZeroMQSettingsForm.Parent      := tsZeroMQ;
  FZeroMQSettingsForm.Align       := alClient;
  FZeroMQSettingsForm.BorderStyle := bsNone;
  FZeroMQSettingsForm.Visible     := True;

  FDisplayValuesSettingsForm := TfrmDisplayValuesSettings.Create(
    Self, FSettings.DisplayValuesSettings
  );
  FDisplayValuesSettingsForm.Parent      := tsDisplayValuesSettings;
  FDisplayValuesSettingsForm.Align       := alClient;
  FDisplayValuesSettingsForm.BorderStyle := bsNone;
  FDisplayValuesSettingsForm.Visible     := True;
end;
{$ENDREGION}

end.
