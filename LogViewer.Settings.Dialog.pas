{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

  VirtualTrees, kpagecontrol, kcontrols,

  SynEditHighlighter, SynHighlighterJScript, SynEdit, SynEditCodeFolding,

  DDuce.Components.VirtualTrees.Node,

  LogViewer.Settings, LogViewer.Settings.Dialog.Data,
  LogViewer.MessageList.Settings.View,
  LogViewer.Receivers.ComPort.Settings.View,
  LogViewer.Receivers.Winods.Settings.View,
  LogViewer.Receivers.Winipc.Settings.View, LogViewer.Watches.Settings.View,
  LogViewer.CallStack.Settings.View, LogViewer.Receivers.Zmq.Settings.View,
  LogViewer.DisplayValues.Settings.View, LogViewer.LogLevels.Settings.View;

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
    chkEmitLogMessages      : TCheckBox;
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
    tsGeneralSettings       : TKTabSheet;
    tsLogLevels             : TKTabSheet;
    tsViewSettings          : TKTabSheet;
    tsWatches               : TKTabSheet;
    tsWinIPC                : TKTabSheet;
    tsWinODS                : TKTabSheet;
    tsZeroMQ                : TKTabSheet;
    chkDebugMode: TCheckBox;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actCloseExecute(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    {$ENDREGION}

    procedure chkEmitLogMessagesClick(Sender: TObject);
    procedure chkDebugModeClick(Sender: TObject);

  private
    FConfigTree                : TVirtualStringTree;
    FSettings                  : TLogViewerSettings;
    FComportSettingsForm       : TfrmComPortSettings;
    FWatchSettingsForm         : TfrmWatchSettings;
    FCallStackSettingsForm     : TfrmCallStackSettings;
    FWinIPCSettingsForm        : TfrmWinipcSettings;
    FWinODSSettingsForm        : TfrmWinodsSettings;
    FZmqSettingsForm           : TfrmZmqSettings;
    FDisplayValuesSettingsForm : TfrmDisplayValuesSettings;
    FViewSettingsForm          : TfrmViewSettings;
    FLogLevelSettingsForm      : TfrmLogLevelSettings;

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
  VirtualTrees.Types, VirtualTrees.Header,

  DDuce.Utils, DDuce.Factories.VirtualTrees,

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
  chkEmitLogMessages.Checked := FSettings.EmitLogMessages;
end;

destructor TfrmLogViewerSettings.Destroy;
begin
  FConfigTree.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLogViewerSettings.chkDebugModeClick(Sender: TObject);
begin
  FSettings.DebugMode := (Sender as TCheckBox).Checked;
end;

procedure TfrmLogViewerSettings.chkEmitLogMessagesClick(Sender: TObject);
begin
  FSettings.EmitLogMessages := (Sender as TCheckBox).Checked;
end;

procedure TfrmLogViewerSettings.FConfigTreeFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  CN : TConfigNode;
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

{ Sets up the configuration tree nodes and the associated tab sheets. }

procedure TfrmLogViewerSettings.BuildTree;
var
  LNode : TConfigNode;
begin
  LNode := AddNode(nil, SViewSettings, tsViewSettings);
  AddNode(LNode, SDisplaySettings, tsDisplayValueSettings);
  AddNode(LNode, SLogLevels, tsLogLevels);
  AddNode(LNode, SWatches, tsWatches);
  AddNode(LNode, SCallStack, tsCallstack);

  LNode := AddNode(nil, SChannelSettings, nil);
  AddNode(LNode, SWinIPC, tsWinIPC);
  AddNode(LNode, SWinODS, tsWinODS);
  AddNode(LNode, SComPort, tsComport);
  AddNode(LNode, SZeroMQ, tsZeroMQ);

  AddNode(nil, SGeneralSettings, tsGeneralSettings);
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

  FCallStackSettingsForm :=
    TfrmCallStackSettings.Create(Self, FSettings.CallStackSettings);
  AssignFormParent(FCallStackSettingsForm, tsCallStack);

  FWinIPCSettingsForm :=
    TfrmWinipcSettings.Create(Self, FSettings.WinipcSettings);
  AssignFormParent(FWinIPCSettingsForm, tsWinIPC);

  FWinODSSettingsForm :=
    TfrmWinodsSettings.Create(Self, FSettings.WinodsSettings);
  AssignFormParent(FWinODSSettingsForm, tsWinODS);

  FZmqSettingsForm :=
    TfrmZmqSettings.Create(Self, FSettings.ZmqSettings);
  AssignFormParent(FZmqSettingsForm, tsZeroMQ);

  FDisplayValuesSettingsForm :=
    TfrmDisplayValuesSettings.Create(Self, FSettings.DisplayValuesSettings);
  AssignFormParent(FDisplayValuesSettingsForm, tsDisplayValueSettings);

  FViewSettingsForm :=
    TfrmViewSettings.Create(Self, FSettings.MessageListSettings);
  AssignFormParent(FViewSettingsForm, tsViewSettings);

  FLogLevelSettingsForm :=
    TfrmLogLevelSettings.Create(Self, FSettings.LogLevelSettings);
  AssignFormParent(FLogLevelSettingsForm , tsLogLevels);
end;
{$ENDREGION}

end.
