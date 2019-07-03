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
  LogViewer.MessageList.Settings.View,
  LogViewer.Receivers.ComPort.Settings.View,
  LogViewer.Receivers.WinODS.Settings.View,
  LogViewer.Receivers.WinIPC.Settings.View, LogViewer.Watches.Settings.View,
  LogViewer.Receivers.ZeroMQ.Settings.View,
  LogViewer.DisplayValues.Settings.View, kpagecontrol, kcontrols;

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
    pgcMain                 : TKPageControl;
    pnlBottom               : TPanel;
    pnlConfigTree           : TPanel;
    seSettings              : TSynEdit;
    shpLine                 : TShape;
    splVertical             : TSplitter;
    synJScript              : TSynJScriptSyn;
    tsAdvanced              : TKTabSheet;
    tsCallStack             : TKTabSheet;
    tsComPort               : TKTabSheet;
    tsDisplayValueSettings  : TKTabSheet;
    tsViewSettings          : TKTabSheet;
    tsWatches               : TKTabSheet;
    tsWinIPC                : TKTabSheet;
    tsWinODS                : TKTabSheet;
    tsZeroMQ                : TKTabSheet;
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
    FViewSettingsForm          : TfrmViewSettings;

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
      ATabSheet   : TKTabSheet
    ): TConfigNode;

    procedure CreateSettingsForms;

  public
    procedure AfterConstruction; override;
    constructor Create(
      AOwner    : TComponent;
      ASettings : TLogViewerSettings
    ); reintroduce;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

uses
  DDuce.Utils, DDuce.Editor.Factories, DDuce.Factories.VirtualTrees,

  LogViewer.Resources;

{$REGION 'construction and destruction'}
constructor TfrmLogViewerSettings.Create(AOwner: TComponent;
  ASettings: TLogViewerSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmLogViewerSettings.AfterConstruction;
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
  pgcMain.ActivePage := tsViewSettings;
  seSettings.Lines.LoadFromFile(FSettings.FileName);
end;

destructor TfrmLogViewerSettings.Destroy;
begin
  FConfigTree.Free;
  inherited Destroy;
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
  string;  ATabSheet: TKTabSheet): TConfigNode;
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
  LNode := AddNode(nil, SViewSettings, tsViewSettings);
  AddNode(LNode, SDisplaySettings, tsDisplayValueSettings);
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
  FComportSettingsForm :=
    TfrmComPortSettings.Create(Self, FSettings.ComPortSettings);
  AssignFormParent(FComportSettingsForm, tsComport);

  FWatchSettingsForm := TfrmWatchSettings.Create(Self, FSettings.WatchSettings);
  AssignFormParent(FWatchSettingsForm, tsWatches);

  FWinIPCSettingsForm :=
    TfrmWinIPCSettings.Create(Self, FSettings.WinIPCSettings);
  AssignFormParent(FWinIPCSettingsForm, tsWinIPC);

  FWinODSSettingsForm :=
    TfrmWinODSSettings.Create(Self, FSettings.WinODSSettings);
  AssignFormParent(FWinODSSettingsForm, tsWinODS);

  FZeroMQSettingsForm :=
    TfrmZeroMQSettings.Create(Self, FSettings.ZeroMQSettings);
  AssignFormParent(FZeroMQSettingsForm, tsZeroMQ);

  FDisplayValuesSettingsForm :=
    TfrmDisplayValuesSettings.Create(Self, FSettings.DisplayValuesSettings);
  AssignFormParent(FDisplayValuesSettingsForm, tsDisplayValueSettings);

  FViewSettingsForm :=
    TfrmViewSettings.Create(Self, FSettings.MessageListSettings);
  AssignFormParent(FViewSettingsForm, tsViewSettings);
end;
{$ENDREGION}

end.
