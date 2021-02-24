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

unit LogViewer.Manager;

{ Manages all application modules. }

interface

uses
  System.SysUtils, System.Classes, System.Actions, System.ImageList,
  System.Generics.Collections,
  Vcl.ExtCtrls, Vcl.ImgList, Vcl.Controls, Vcl.ActnList, Vcl.Menus,

  Spring, Spring.Collections,

  DDuce.Logger.Interfaces, DDuce.Editor.Interfaces,

  LogViewer.Interfaces, LogViewer.Settings, LogViewer.Events,
  LogViewer.Commands,

  LogViewer.MessageFilter.View;

type
  TdmManager = class(TDataModule, ILogViewerActions,
                                  ILogViewerMenus,
                                  ILogViewerCommands,
                                  ILogViewerEvents,
                                  ILogViewerManager
  )
    {$REGION 'designer controls'}
    aclMain                   : TActionList;
    actAbout                  : TAction;
    actAction                 : TAction;
    actAutoScrollMessages     : TAction;
    actBitmap                 : TAction;
    actCallStack              : TAction;
    actCheckPoint             : TAction;
    actClearMessages          : TAction;
    actCloseMessageView       : TAction;
    actCloseOtherMessageViews : TAction;
    actCollapseAll            : TAction;
    actComponent              : TAction;
    actConditional            : TAction;
    actCounter                : TAction;
    actCustomData             : TAction;
    actDashboard              : TAction;
    actDataSet                : TAction;
    actError                  : TAction;
    actException              : TAction;
    actExpandAll              : TAction;
    actFilterMessages         : TAction;
    actGotoFirst              : TAction;
    actGotoLast               : TAction;
    actHeapInfo               : TAction;
    actInfo                   : TAction;
    actInterface              : TAction;
    actMemory                 : TAction;
    actMessageTypesMenu       : TAction;
    actMethodTraces           : TAction;
    actObject                 : TAction;
    actPersistent             : TAction;
    actSaveBitmapAs           : TAction;
    actScreenshot             : TAction;
    actSetFocusToFilter       : TAction;
    actSettings               : TAction;
    actShowFilterView         : TAction;
    actStart                  : TAction;
    actStop                   : TAction;
    actStrings                : TAction;
    actText                   : TAction;
    actToggleAlwaysOnTop      : TAction;
    actToggleFullscreen       : TAction;
    actValue                  : TAction;
    actWarning                : TAction;
    imlMain                   : TImageList;
    imlMessageTypes           : TImageList;
    ppmLogTreeViewer          : TPopupMenu;
    ppmMessageTypes           : TPopupMenu;
    actCloseTerminatedProcesses: TAction;
    actToggleLeftPanelVisible: TAction;
    actToggleRightPanelVisible: TAction;
    ppmSubscriber: TPopupMenu;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAutoScrollMessagesExecute(Sender: TObject);
    procedure actBitmapExecute(Sender: TObject);
    procedure actCallStackExecute(Sender: TObject);
    procedure actCheckPointExecute(Sender: TObject);
    procedure actClearMessagesExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actConditionalExecute(Sender: TObject);
    procedure actCustomDataExecute(Sender: TObject);
    procedure actErrorExecute(Sender: TObject);
    procedure actExceptionExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actFilterMessagesExecute(Sender: TObject);
    procedure actGotoFirstExecute(Sender: TObject);
    procedure actGotoLastExecute(Sender: TObject);
    procedure actHeapInfoExecute(Sender: TObject);
    procedure actInfoExecute(Sender: TObject);
    procedure actMemoryExecute(Sender: TObject);
    procedure actMethodTracesExecute(Sender: TObject);
    procedure actComponentExecute(Sender: TObject);
    procedure actSetFocusToFilterExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStringsExecute(Sender: TObject);
    procedure actToggleAlwaysOnTopExecute(Sender: TObject);
    procedure actToggleFullscreenExecute(Sender: TObject);
    procedure actValueExecute(Sender: TObject);
    procedure actWarningExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actObjectExecute(Sender: TObject);
    procedure actPersistentExecute(Sender: TObject);
    procedure actInterfaceExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure aclMainExecute(Action: TBasicAction; var Handled: Boolean);
    procedure actActionExecute(Sender: TObject);
    procedure actDataSetExecute(Sender: TObject);
    procedure actCounterExecute(Sender: TObject);
    procedure actTextExecute(Sender: TObject);
    procedure actScreenshotExecute(Sender: TObject);
    procedure actShowFilterViewExecute(Sender: TObject);
    procedure actCloseMessageViewExecute(Sender: TObject);
    procedure actCloseOtherMessageViewsExecute(Sender: TObject);
    procedure actDashboardExecute(Sender: TObject);
    procedure actCloseTerminatedProcessesExecute(Sender: TObject);
    procedure actToggleRightPanelVisibleExecute(Sender: TObject);
    procedure actToggleLeftPanelVisibleExecute(Sender: TObject);
    {$ENDREGION}

  private
    FSettings       : TLogViewerSettings;
    FEvents         : TLogViewerEvents;
    FCommands       : TLogViewerCommands;
    FActiveView     : Weak<ILogViewer>;
    FViewList       : IList<ILogViewer>;
    FReceivers      : IList<IChannelReceiver>;
    FEditorManager  : IEditorManager;
    FEditorSettings : IEditorSettings;
    FFilterView     : TfrmMessageFilter;

    function AddMenuItem(
      AParent : TMenuItem;
      AAction : TBasicAction = nil
    ): TMenuItem; overload;
    function AddMenuItem(
      AParent : TMenuItem;
      AMenu   : TMenu
    ): TMenuItem; overload;

    procedure BuildLogTreeViewerPopupMenu;
    procedure BuildMessageTypesPopupMenu;
    procedure BuildSubscriberPopupMenu;

    procedure InitializeEditorActions;

    procedure FSettingsChanged(Sender: TObject);

  protected
    {$REGION 'property access methods'}
    function GetEditorManager: IEditorManager;
    function GetCommands: ILogViewerCommands;
    function GetEvents: ILogViewerEvents;
    function GetSettings: TLogViewerSettings;
    function GetViews: IList<ILogViewer>;
    function GetReceivers: IList<IChannelReceiver>;
    function GetMessageTypesPopupMenu: TPopupMenu;
    function GetLogTreeViewerPopupMenu: TPopupMenu;
    function GetSubscriberPopupMenu: TPopupMenu;
    {$ENDREGION}

    procedure FeatureNotImplemented;
    function AsComponent: TComponent;
    function DeleteView(AView: ILogViewer): Boolean;

    procedure FReceiverSubscriberListChanged(
      Sender     : TObject;
      const Item : ISubscriber;
      Action     : TCollectionChangedAction
    );

    procedure UpdateVisibleMessageTypes(
      const AMessageType : TLogMessageType;
      const ASender      : TObject;
      const AToggle      : Boolean = True
    );

    {$REGION 'ILogViewerActions'}
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;

    procedure UpdateActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;
    {$ENDREGION}

    {$REGION 'ILogViewerMenus'}
    function GetMenus: ILogViewerMenus;

    property Menus: ILogViewerMenus
      read GetMenus;

    property LogTreeViewerPopupMenu: TPopupMenu
      read GetLogTreeViewerPopupMenu;

    property MessageTypesPopupMenu: TPopupMenu
      read GetMessageTypesPopupMenu;

    property SubscriberPopupMenu: TPopupMenu
      read GetSubscriberPopupMenu;
    {$ENDREGION}

    {$REGION 'ILogViewerManager'}
    function GetActions: ILogViewerActions;
    function GetActiveView: ILogViewer;
    procedure SetActiveView(const Value: ILogViewer);

    procedure AddView(ALogViewer: ILogViewer);
    procedure AddReceiver(AReceiver: IChannelReceiver);

    property ActiveView: ILogViewer
      read GetActiveView write SetActiveView;

    property Actions: ILogViewerActions
      read GetActions;
    {$ENDREGION}

    {$REGION 'ILogViewerCommands'}
    property Commands: ILogViewerCommands
      read GetCommands implements ILogViewerCommands;
    {$ENDREGION}

    {$REGION 'ILogViewerEvents'}
    property Events: ILogViewerEvents
      read GetEvents implements ILogViewerEvents;
    {$ENDREGION}

    property Settings: TLogViewerSettings
      read GetSettings;

    property Views: IList<ILogViewer>
      read GetViews;

    property Receivers: IList<IChannelReceiver>
      read GetReceivers;

    property EditorManager: IEditorManager
      read GetEditorManager;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TLogViewerSettings
    ); reintroduce; virtual;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Forms, Vcl.Dialogs,

  DDuce.Editor.Factories, DDuce.AboutDialog, DDuce.Utils.Winapi,
  DDuce.Logger,

  LogViewer.Factories, LogViewer.Resources, LogViewer.Settings.Dialog,
  LogViewer.MessageList.Settings;

{$REGION 'construction and destruction'}
constructor TdmManager.Create(AOwner: TComponent; ASettings: TLogViewerSettings);
begin
  Logger.Track(Self, 'Create');
  Guard.CheckNotNull(ASettings, 'ASettings');
  inherited Create(AOwner);
  FSettings := ASettings;
  FSettings.OnChanged.Add(FSettingsChanged);
end;

procedure TdmManager.AfterConstruction;
begin
  inherited AfterConstruction;
  VCLComObject := nil;
  FEvents         := TLogViewerEvents.Create(Self);
  FCommands       := TLogViewerCommands.Create(Self);
  FReceivers      := TCollections.CreateInterfaceList<IChannelReceiver>;
  FViewList       := TCollections.CreateInterfaceList<ILogViewer>;
  FEditorSettings := TEditorFactories.CreateSettings(Self, 'settings.xml');
  FEditorManager  := TEditorFactories.CreateManager(Self, FEditorSettings);
  InitializeEditorActions;
  BuildMessageTypesPopupMenu;
  BuildLogTreeViewerPopupMenu;
  BuildSubscriberPopupMenu;
  FFilterView := TfrmMessageFilter.Create(
    Self,
    Settings.MessageListSettings,
    imlMessageTypes
  );
  UpdateActions;
end;

destructor TdmManager.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FActiveView     := nil;
  FreeAndNil(FFilterView);
  FSettings.OnChanged.RemoveAll(Self);
  FViewList.OnChanged.RemoveAll(Self);
  FViewList.Clear;
  FSettings.Save;
  FViewList       := nil;
  FReceivers      := nil;
  FSettings       := nil;
  FEditorSettings := nil;
  FEditorManager  := nil;

  FreeAndNil(FEvents);   // needs to happen just before inherited call
  FreeAndNil(FCommands);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TdmManager.aclMainExecute(Action: TBasicAction; var Handled: Boolean);
begin
  Logger.Action(Action);
end;

procedure TdmManager.actAboutExecute(Sender: TObject);
begin
  ShowAboutDialog;
end;

procedure TdmManager.actActionExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtAction, Sender);
end;

procedure TdmManager.actAutoScrollMessagesExecute(Sender: TObject);
begin
  FSettings.MessageListSettings.AutoScrollMessages :=
    not FSettings.MessageListSettings.AutoScrollMessages;
  UpdateActions;
end;

procedure TdmManager.actBitmapExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtBitmap, Sender);
end;

procedure TdmManager.actCallStackExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtCallStack, Sender);
end;

procedure TdmManager.actCheckPointExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtCheckpoint, Sender);
end;

procedure TdmManager.actClearMessagesExecute(Sender: TObject);
begin
  Commands.ClearMessages;
end;

procedure TdmManager.actCloseMessageViewExecute(Sender: TObject);
begin
  DeleteView(FActiveView);
end;

procedure TdmManager.actCloseOtherMessageViewsExecute(Sender: TObject);
begin
  while Views.Count > 1 do
  begin
    if Views.First <> FActiveView then
      DeleteView(Views.First)
    else
    begin
      DeleteView(Views.Last);
    end;
  end;
end;

procedure TdmManager.actCloseTerminatedProcessesExecute(Sender: TObject);
var
  I : Integer;
  V : ILogViewer;
begin
  I := 0;
  while I < Views.Count do
  begin
    V := Views[I];
    if Supports(V.Subscriber, IWinipc) or Supports(V.Subscriber, IWinods) then
    begin
      if not CheckProcessExists(V.Subscriber.SourceId) then
        DeleteView(V)
      else
      begin
        Inc(I);
      end;
    end
    else
    begin
      Inc(I);
    end;
  end;
end;

procedure TdmManager.actCollapseAllExecute(Sender: TObject);
begin
  Commands.CollapseAll;
end;

procedure TdmManager.actConditionalExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtConditional, Sender);
end;

procedure TdmManager.actCounterExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtCounter, Sender);
end;

procedure TdmManager.actCustomDataExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtCustomData, Sender);
end;

procedure TdmManager.actDashboardExecute(Sender: TObject);
begin
  Events.DoShowDashboard;
end;

procedure TdmManager.actDataSetExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtDataSet, Sender);
end;

procedure TdmManager.actErrorExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtError, Sender);
end;

procedure TdmManager.actExceptionExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtException, Sender);
end;

procedure TdmManager.actExpandAllExecute(Sender: TObject);
begin
  Commands.ExpandAll;
end;

procedure TdmManager.actFilterMessagesExecute(Sender: TObject);
begin
  Commands.UpdateView;
end;

procedure TdmManager.actGotoFirstExecute(Sender: TObject);
begin
  Commands.GotoFirst;
end;

procedure TdmManager.actGotoLastExecute(Sender: TObject);
begin
  Commands.GotoLast;
end;

procedure TdmManager.actHeapInfoExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtHeapInfo, Sender);
end;

procedure TdmManager.actMemoryExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtMemory, Sender);
end;

procedure TdmManager.actMethodTracesExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtEnterMethod, Sender);
  UpdateVisibleMessageTypes(lmtLeaveMethod, Sender, False);
end;

procedure TdmManager.actComponentExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtComponent, Sender);
end;

procedure TdmManager.actObjectExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtObject, Sender);
end;

procedure TdmManager.actPersistentExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtPersistent, Sender);
end;

procedure TdmManager.actScreenshotExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtScreenShot, Sender);
end;

procedure TdmManager.actSetFocusToFilterExecute(Sender: TObject);
begin
  Commands.SetFocusToFilter;
end;

procedure TdmManager.actSettingsExecute(Sender: TObject);
var
  F : TfrmLogViewerSettings;
begin
  F := TfrmLogViewerSettings.Create(Self, FSettings);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TdmManager.actShowFilterViewExecute(Sender: TObject);
begin
  FFilterView.Show;
end;

procedure TdmManager.actStartExecute(Sender: TObject);
begin
  Commands.Start;
  UpdateActions;
end;

procedure TdmManager.actStopExecute(Sender: TObject);
begin
  Commands.Stop;
  UpdateActions;
end;

procedure TdmManager.actStringsExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtStrings, Sender);
end;

procedure TdmManager.actTextExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtText, Sender);
end;

procedure TdmManager.actToggleAlwaysOnTopExecute(Sender: TObject);
var
  A : TAction;
begin
  A := Sender as TAction;
  if A.Checked then
    Settings.FormSettings.FormStyle := fsStayOnTop
  else
    Settings.FormSettings.FormStyle := fsNormal;
end;

procedure TdmManager.actToggleFullscreenExecute(Sender: TObject);
var
  A : TAction;
begin
  A := Sender as TAction;
  if A.Checked then
    Settings.FormSettings.WindowState := wsMaximized
  else
    Settings.FormSettings.WindowState := wsNormal;
end;

procedure TdmManager.actToggleLeftPanelVisibleExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
    ActiveView.LeftPanelVisible := actToggleLeftPanelVisible.Checked;
end;

procedure TdmManager.actToggleRightPanelVisibleExecute(Sender: TObject);
begin
  if Assigned(ActiveView) then
    ActiveView.RightPanelVisible := actToggleRightPanelVisible.Checked;
end;

procedure TdmManager.actInfoExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtInfo, Sender);
end;

procedure TdmManager.actInterfaceExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtInterface, Sender);
end;

procedure TdmManager.actWarningExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtWarning, Sender);
end;

procedure TdmManager.actValueExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtValue, Sender);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmManager.GetActionList: TActionList;
begin
  Result := aclMain;
end;

function TdmManager.GetActions: ILogViewerActions;
begin
  Result := Self as ILogViewerActions;
end;

function TdmManager.GetActiveView: ILogViewer;
begin
  Result := FActiveView;
end;

function TdmManager.GetCommands: ILogViewerCommands;
begin
  Result := FCommands;
end;

function TdmManager.GetEditorManager: IEditorManager;
begin
  Result := FEditorManager;
end;

function TdmManager.GetEvents: ILogViewerEvents;
begin
  Result := FEvents;
end;

procedure TdmManager.SetActiveView(const Value: ILogViewer);
begin
  Logger.Track(Self, 'SetActiveView');
  if Assigned(Value) then
  begin
    FActiveView := Value;
    if Assigned(FActiveView.Target) then
    begin
      Events.DoActiveViewChange(FActiveView);
      Logger.Watch('FActiveView', FActiveView.Target.Form.Caption);
    end;
    UpdateActions;
  end
  else if not Assigned(Value) then
  begin
    FActiveView := nil;
    Logger.Warn('FActiveView set to nil');
  end;
end;

function TdmManager.GetItem(AName: string): TCustomAction;
var
  A  : TCustomAction;
  CA : TContainedAction;
begin
  Result := nil;
  for CA in aclMain do
  begin
    if CA.Name = AName then
    begin
      A := CA as TCustomAction;
      Result := A;
      Break;
    end;
  end;
end;

function TdmManager.GetLogTreeViewerPopupMenu: TPopupMenu;
begin
  Result := ppmLogTreeViewer;
end;

function TdmManager.GetMenus: ILogViewerMenus;
begin
  Result := Self as ILogViewerMenus;
end;

function TdmManager.GetMessageTypesPopupMenu: TPopupMenu;
begin
  Result := ppmMessageTypes;
end;

function TdmManager.GetReceivers: IList<IChannelReceiver>;
begin
  Result := FReceivers as IList<IChannelReceiver>;
end;

function TdmManager.GetSettings: TLogViewerSettings;
begin
  Result := FSettings;
end;

function TdmManager.GetSubscriberPopupMenu: TPopupMenu;
begin
  Result := ppmSubscriber;
end;

function TdmManager.GetViews: IList<ILogViewer>;
begin
  Result := FViewList;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmManager.FReceiverSubscriberListChanged(Sender: TObject;
  const Item: ISubscriber; Action: TCollectionChangedAction);
begin
  if Action = caAdded then
  begin
    AddView(TLogViewerFactories.CreateLogViewer(Self, Item));
  end;
end;

procedure TdmManager.FSettingsChanged(Sender: TObject);
begin
//  UpdateActions;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TdmManager.AddMenuItem(AParent: TMenuItem;
  AAction: TBasicAction): TMenuItem;
var
  MI: TMenuItem;
begin
  if not Assigned(AAction) then
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Caption := ('-');
    AParent.Add(MI);
    Result := nil;
  end
  else
  begin
    MI := TMenuItem.Create(AParent.Owner);
    MI.Action := AAction;
    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
    begin
      MI.RadioItem := True;
    end;
    AParent.Add(MI);
    Result := MI;
  end;
end;

function TdmManager.AddMenuItem(AParent: TMenuItem; AMenu: TMenu): TMenuItem;
var
  MI  : TMenuItem;
  M   : TMenuItem;
  SM  : TMenuItem;
  SMI : TMenuItem;
  I   : Integer;
begin
  MI := TMenuItem.Create(AMenu);
  MI.Action := AMenu.Items.Action;
  AParent.Add(MI);
  for M in AMenu.Items do
  begin
    SMI := AddMenuItem(MI, M.Action);
    // add submenu(s)
    if M.Count > 0 then
    begin
      for I := 0 to M.Count - 1 do
      begin
        SM := M.Items[I];
        AddMenuItem(SMI, SM.Action);
      end;
    end;
  end;
  MI.Enabled := True;
  Result := MI;
end;

procedure TdmManager.BuildLogTreeViewerPopupMenu;
var
  MI: TMenuItem;
begin
  MI := LogTreeViewerPopupMenu.Items;
  MI.Clear;
  AddMenuItem(MI, actAutoScrollMessages);
  AddMenuItem(MI, actClearMessages);
  AddMenuItem(MI);
  AddMenuItem(MI, actCollapseAll);
  AddMenuItem(MI, actExpandAll);
  AddMenuItem(MI);
  AddMenuItem(MI, actGotoFirst);
  AddMenuItem(MI, actGotoLast);
  AddMenuItem(MI);
  AddMenuItem(MI, actStart);
  AddMenuItem(MI, actStop);
  AddMenuItem(MI);
  AddMenuItem(MI, MessageTypesPopupMenu);
  AddMenuItem(MI);
  AddMenuItem(MI, actToggleLeftPanelVisible);
  AddMenuItem(MI, actToggleRightPanelVisible);
  AddMenuItem(MI);
  AddMenuItem(MI, actCloseMessageView);
  AddMenuItem(MI, actCloseOtherMessageViews);
  AddMenuItem(MI, actCloseTerminatedProcesses);
end;

procedure TdmManager.BuildMessageTypesPopupMenu;
var
  MI : TMenuItem;
begin
  actMessageTypesMenu.DisableIfNoHandler := False;
  MI := MessageTypesPopupMenu.Items;
  MI.Action := actMessageTypesMenu;

  AddMenuItem(MI, actInfo);
  AddMenuItem(MI, actWarning);
  AddMenuItem(MI, actError);
  AddMenuItem(MI, actException);
  AddMenuItem(MI, actConditional);
  AddMenuItem(MI);
  AddMenuItem(MI, actValue);
  AddMenuItem(MI, actBitmap);
  AddMenuItem(MI, actStrings);
  AddMenuItem(MI, actDataSet);
  AddMenuItem(MI, actObject);
  AddMenuItem(MI, actInterface);
  AddMenuItem(MI, actPersistent);
  AddMenuItem(MI, actComponent);
  AddMenuItem(MI, actText);
  AddMenuItem(MI, actCustomData);
  AddMenuItem(MI);
  AddMenuItem(MI, actMethodTraces);
  AddMenuItem(MI, actAction);
  AddMenuItem(MI);
  AddMenuItem(MI, actHeapInfo);
  AddMenuItem(MI, actCallStack);
  AddMenuItem(MI, actMemory);
  AddMenuItem(MI);
  AddMenuItem(MI, actCheckPoint);
end;

procedure TdmManager.BuildSubscriberPopupMenu;
var
  MI : TMenuItem;
begin
  MI := SubscriberPopupMenu.Items;
  AddMenuItem(MI, actStart);
  AddMenuItem(MI, actStop);
  AddMenuItem(MI);
  AddMenuItem(MI, actCloseMessageView);
  AddMenuItem(MI, actCloseOtherMessageViews);
  AddMenuItem(MI, actCloseTerminatedProcesses);
end;

procedure TdmManager.FeatureNotImplemented;
begin
  ShowMessage('This feature is not implemented yet.');
end;

procedure TdmManager.InitializeEditorActions;
begin
  FEditorManager.Actions['actSettings'].Visible          := False;
  FEditorManager.Actions['actOpen'].Visible              := False;
  FEditorManager.Actions['actSave'].Visible              := False;
  FEditorManager.Actions['actSmartSelect'].Visible       := False;
  FEditorManager.Actions['actClose'].Visible             := False;
  FEditorManager.Actions['actCloseOthers'].Visible       := False;
  FEditorManager.Actions['actSaveAll'].Visible           := False;
  FEditorManager.Actions['actSingleInstance'].Visible    := False;
  FEditorManager.Actions['actToggleMaximized'].Visible   := False;
  FEditorManager.Actions['actStayOnTop'].Visible         := False;
  FEditorManager.Actions['actReload'].Visible            := False;
  FEditorManager.Actions['actNew'].Visible               := False;
  FEditorManager.Actions['actMonitorChanges'].Visible    := False;
  FEditorManager.Actions['actCreateDesktopLink'].Visible := False;
  FEditorManager.Actions['actShowCharacterMap'].Visible  := False;

  FEditorManager.Actions['actSettings'].Enabled          := False;
  FEditorManager.Actions['actOpen'].Enabled              := False;
  FEditorManager.Actions['actSave'].Enabled              := False;
  FEditorManager.Actions['actSmartSelect'].Enabled       := False;
  FEditorManager.Actions['actClose'].Enabled             := False;
  FEditorManager.Actions['actCloseOthers'].Enabled       := False;
  FEditorManager.Actions['actSaveAll'].Enabled           := False;
  FEditorManager.Actions['actSingleInstance'].Enabled    := False;
  FEditorManager.Actions['actToggleMaximized'].Enabled   := False;
  FEditorManager.Actions['actStayOnTop'].Enabled         := False;
  FEditorManager.Actions['actReload'].Enabled            := False;
  FEditorManager.Actions['actNew'].Enabled               := False;
  FEditorManager.Actions['actMonitorChanges'].Enabled    := False;
  FEditorManager.Actions['actCreateDesktopLink'].Enabled := False;
  FEditorManager.Actions['actShowCharacterMap'].Enabled  := False;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TdmManager.AsComponent: TComponent;
begin
  Result := Self;
end;

procedure TdmManager.AddReceiver(AReceiver: IChannelReceiver);
begin
  Logger.Track(Self, 'AddReceiver');
  Guard.CheckNotNull(AReceiver, 'AReceiver');
  FReceivers.Add(AReceiver);
  Events.DoAddReceiver(AReceiver);
  AReceiver.SubscriberList.OnValueChanged.UseFreeNotification := False;
  AReceiver.SubscriberList.OnValueChanged.Add(FReceiverSubscriberListChanged);
end;

procedure TdmManager.AddView(ALogViewer: ILogViewer);
begin
  Logger.Track(Self, 'AddView');
  Guard.CheckNotNull(ALogViewer, 'ALogViewer');
  FViewList.Add(ALogViewer);
  if not FReceivers.Contains(ALogViewer.Subscriber.Receiver) then
  begin
    FReceivers.Add(ALogViewer.Subscriber.Receiver);
  end;
  Events.DoAddLogViewer(ALogViewer);
  ActiveView := ALogViewer;
  ActiveView.UpdateView;
  UpdateActions;
end;

function TdmManager.DeleteView(AView: ILogViewer): Boolean;
var
  I : Integer;
  S : ISubscriber;
begin
  Logger.Track(Self, 'DeleteView');
  if Assigned(AView) then
  begin
    Events.DoDeleteLogViewer(AView);
    I := FViewList.IndexOf(AView);
    if ActiveView = AView then
      FActiveView := nil;
    S := AView.Subscriber;
    S.Receiver.SubscriberList.Remove(S.SourceId);
    FViewList[I].Form.Close; // instance still exists after closing
    FViewList.Delete(I); // automatically frees the instance
    Result := True;
  end
  else
    Result := False;
end;

{ Gets called from the active messages view. }

procedure TdmManager.UpdateActions;
var
  B   : Boolean;
  MLS : TMessageListSettings;
begin
  MLS := Settings.MessageListSettings;

  B := Assigned(ActiveView) and Assigned(ActiveView.Subscriber);
  // workaround toolbar issue which refuses to reflect checked state when button
  // is first Disabled, then Enabled and Checked
  actAutoScrollMessages.Visible      := B;
  actToggleRightPanelVisible.Visible := B;
  actToggleLeftPanelVisible.Visible  := B;
  actToggleLeftPanelVisible.Checked  := B and ActiveView.LeftPanelVisible;
  actToggleRightPanelVisible.Checked := B and ActiveView.RightPanelVisible;

  actMessageTypesMenu.Enabled := B;
  actShowFilterView.Enabled   := B;
  actStart.Enabled            := B and not ActiveView.Subscriber.Enabled;
  actStop.Enabled             := B and not actStart.Enabled;
  actBitmap.Enabled           := B;
  actAction.Enabled           := B;
  actText.Enabled             := B;
  actDataSet.Enabled          := B;
  actCallStack.Enabled        := B;
  actCheckPoint.Enabled       := B;
  actConditional.Enabled      := B;
  actInfo.Enabled             := B;
  actWarning.Enabled          := B;
  actValue.Enabled            := B;
  actError.Enabled            := B;
  actMethodTraces.Enabled     := B;
  actException.Enabled        := B;
  actComponent.Enabled        := B;
  actHeapInfo.Enabled         := B;
  actCustomData.Enabled       := B;
  actStrings.Enabled          := B;
  actObject.Enabled           := B;
  actPersistent.Enabled       := B;
  actInterface.Enabled        := B;
  actMemory.Enabled           := B;
  actCollapseAll.Enabled      := B;
  actExpandAll.Enabled        := B;
  actClearMessages.Enabled    := B;
  actFilterMessages.Enabled := B and
    not Settings.MessageListSettings.AutoFilterMessages;
  actToggleAlwaysOnTop.Checked := Settings.FormSettings.FormStyle = fsStayOnTop;
  actToggleFullscreen.Checked := Settings.FormSettings.WindowState = wsMaximized;
  actAutoScrollMessages.Checked := MLS.AutoScrollMessages;

  actBitmap.Checked       := lmtBitmap in MLS.VisibleMessageTypes;
  actCallStack.Checked    := lmtCallStack in MLS.VisibleMessageTypes;
  actCheckPoint.Checked   := lmtCheckpoint in MLS.VisibleMessageTypes;
  actConditional.Checked  := lmtConditional in MLS.VisibleMessageTypes;
  actInfo.Checked         := lmtInfo in MLS.VisibleMessageTypes;
  actWarning.Checked      := lmtWarning in MLS.VisibleMessageTypes;
  actValue.Checked        := lmtValue in MLS.VisibleMessageTypes;
  actError.Checked        := lmtError in MLS.VisibleMessageTypes;
  actMethodTraces.Checked := lmtEnterMethod in MLS.VisibleMessageTypes;
  actException.Checked    := lmtException in MLS.VisibleMessageTypes;
  actComponent.Checked    := lmtComponent in MLS.VisibleMessageTypes;
  actObject.Checked       := lmtObject in MLS.VisibleMessageTypes;
  actPersistent.Checked   := lmtPersistent in MLS.VisibleMessageTypes;
  actInterface.Checked    := lmtInterface in MLS.VisibleMessageTypes;
  actHeapInfo.Checked     := lmtHeapInfo in MLS.VisibleMessageTypes;
  actCustomData.Checked   := lmtCustomData in MLS.VisibleMessageTypes;
  actStrings.Checked      := lmtStrings in MLS.VisibleMessageTypes;
  actMemory.Checked       := lmtMemory in MLS.VisibleMessageTypes;
  actText.Checked         := lmtText in MLS.VisibleMessageTypes;
  actDataSet.Checked      := lmtDataSet in MLS.VisibleMessageTypes;
  actAction.Checked       := lmtAction in MLS.VisibleMessageTypes;

  if B then
    ActiveView.UpdateView;

  if Assigned(FViewList) then
    Logger.Watch('ViewCount', FViewList.Count);
  if Assigned(FReceivers) then
    Logger.Watch('ReceiverCount', FReceivers.Count);
end;

procedure TdmManager.UpdateVisibleMessageTypes(
  const AMessageType: TLogMessageType; const ASender: TObject;
  const AToggle: Boolean);
var
  A   : TAction;
  MLS : TMessageListSettings;
begin
  MLS := Settings.MessageListSettings;
  if Assigned(FActiveView.Target) then
  begin
    A := ASender as TAction;
    Logger.Send('A.Name', A.Name);
    if AToggle then
      A.Checked := not A.Checked;
    if A.Checked then
      MLS.VisibleMessageTypes := MLS.VisibleMessageTypes + [AMessageType]
    else
      MLS.VisibleMessageTypes := MLS.VisibleMessageTypes - [AMessageType];
    UpdateActions;
    FActiveView.Target.UpdateView;
  end;
end;
{$ENDREGION}

end.
