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

unit LogViewer.Manager;

interface

{ Handles all application modules. }

uses
  System.SysUtils, System.Classes, System.Actions, System.ImageList,
  Vcl.ExtCtrls, Vcl.ImgList, Vcl.Controls, Vcl.ActnList, Vcl.Menus,

  Spring, Spring.Collections,

  DDuce.Logger.Interfaces,

  LogViewer.Interfaces, LogViewer.Settings, LogViewer.Events,
  LogViewer.Commands;

type
  TdmManager = class(TDataModule, ILogViewerActions,
                                  ILogViewerMenus,
                                  ILogViewerCommands,
                                  ILogViewerEvents,
                                  ILogViewerManager
  )
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actAutoScrollMessages : TAction;
    actBitmap             : TAction;
    actCallStack          : TAction;
    actCheckPoint         : TAction;
    actClearMessages      : TAction;
    actCollapseAll        : TAction;
    actConditional        : TAction;
    actCustomData         : TAction;
    actError              : TAction;
    actException          : TAction;
    actExpandAll          : TAction;
    actFilterMessages     : TAction;
    actGotoFirst          : TAction;
    actGotoLast           : TAction;
    actHeapInfo           : TAction;
    actInfo               : TAction;
    actMemory             : TAction;
    actMethodTraces       : TAction;
    actComponent: TAction;
    actOpen               : TAction;
    actSave               : TAction;
    actSelectAll          : TAction;
    actSelectNone         : TAction;
    actSetFocusToFilter   : TAction;
    actSettings           : TAction;
    actStop               : TAction;
    actStrings            : TAction;
    actToggleAlwaysOnTop  : TAction;
    actToggleFullscreen   : TAction;
    actValue              : TAction;
    actWarning            : TAction;
    imlMain               : TImageList;
    imlMessageTypes       : TImageList;
    tmrPoll               : TTimer;
    ppmLogTreeViewer: TPopupMenu;
    ppmMessageTypes: TPopupMenu;
    actMessageTypesMenu: TAction;
    actStart: TAction;
    actObject: TAction;
    actPersistent: TAction;
    actInterface: TAction;
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
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectNoneExecute(Sender: TObject);
    procedure actSetFocusToFilterExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStringsExecute(Sender: TObject);
    procedure actToggleAlwaysOnTopExecute(Sender: TObject);
    procedure actToggleFullscreenExecute(Sender: TObject);
    procedure actValueExecute(Sender: TObject);
    procedure actWarningExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    {$ENDREGION}

  private
    FSettings   : TLogViewerSettings;
    FEvents     : TLogViewerEvents;
    FCommands   : TLogViewerCommands;
    FActiveView : ILogViewer;
    FViewList   : IList<ILogViewer>;
    FReceivers  : IList<IChannelReceiver>;
    FLogQueues  : IList<ILogQueue>;

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

  protected
    function GetMessageTypesPopupMenu: TPopupMenu;
    function GetLogTreeViewerPopupMenu: TPopupMenu;
    function GetCommands: ILogViewerCommands;
    function GetEvents: ILogViewerEvents;
    function GetSettings: TLogViewerSettings;
    function GetViews: IList<ILogViewer>;
    function GetReceivers: IList<IChannelReceiver>;

    procedure ActiveViewChanged;

    function AsComponent: TComponent;

    procedure ReceiverNewLogQueue(
      Sender    : TObject;
      ALogQueue : ILogQueue
    );

    procedure UpdateVisibleMessageTypes(
      const AMessageType : TLogMessageType;
      const Sender       : TObject;
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

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TLogViewerSettings
    ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  Vcl.Forms,

  LogViewer.Factories, LogViewer.Resources,
  LogViewer.Settings.Dialog, LogViewer.MessageList.Settings;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TdmManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FEvents    := TLogViewerEvents.Create(Self);
  FCommands  := TLogViewerCommands.Create(Self);
  FReceivers := TCollections.CreateInterfaceList<IChannelReceiver>;
  FLogQueues := TCollections.CreateInterfaceList<ILogQueue>;
  FViewList  := TCollections.CreateInterfaceList<ILogViewer>;
  BuildMessageTypesPopupMenu;
  BuildLogTreeViewerPopupMenu;
end;

procedure TdmManager.BeforeDestruction;
begin
  FreeAndNil(FCommands);
  FreeAndNil(FEvents);
  FSettings := nil;
  inherited BeforeDestruction;
end;

constructor TdmManager.Create(AOwner: TComponent; ASettings: TLogViewerSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TdmManager.actAutoScrollMessagesExecute(Sender: TObject);
begin
  FSettings.MessageListSettings.AutoScrollMessages :=
    not FSettings.MessageListSettings.AutoScrollMessages;
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

procedure TdmManager.actCollapseAllExecute(Sender: TObject);
begin
  Commands.CollapseAll;
end;

procedure TdmManager.actConditionalExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtConditional, Sender);
end;

procedure TdmManager.actCustomDataExecute(Sender: TObject);
begin
//
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

procedure TdmManager.actOpenExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actSaveExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actSelectAllExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actSelectNoneExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actSetFocusToFilterExecute(Sender: TObject);
begin
//
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

procedure TdmManager.actStartExecute(Sender: TObject);
begin
  Commands.Start;
  actStop.Enabled := True;
  actStart.Enabled := False;
end;

procedure TdmManager.actStopExecute(Sender: TObject);
begin
  Commands.Stop;
  actStop.Enabled := False;
  actStart.Enabled := True;
end;

procedure TdmManager.actStringsExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtStrings, Sender);
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

procedure TdmManager.actInfoExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtInfo, Sender);
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

function TdmManager.GetEvents: ILogViewerEvents;
begin
  Result := FEvents;
end;

procedure TdmManager.SetActiveView(const Value: ILogViewer);
begin
  if Assigned(Value) and (Value <> FActiveView) then
  begin
    FActiveView := Value;
//    Events.DoActiveViewChange;
    ActiveViewChanged;
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
  Result := ppmLogTreeViewer
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

function TdmManager.GetViews: IList<ILogViewer>;
begin
  Result := FViewList;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmManager.ReceiverNewLogQueue(Sender: TObject; ALogQueue: ILogQueue);
begin
  FLogQueues.Add(ALogQueue);
  AddView(TLogViewerFactories.CreateLogViewer(Self, ALogQueue));
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
end;

procedure TdmManager.BuildMessageTypesPopupMenu;
var
  MI: TMenuItem;
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
  AddMenuItem(MI, actCustomData);
  AddMenuItem(MI);
  AddMenuItem(MI, actMethodTraces);
  AddMenuItem(MI);
  AddMenuItem(MI, actHeapInfo);
  AddMenuItem(MI, actCallStack);
  AddMenuItem(MI, actMemory);
  AddMenuItem(MI);
  AddMenuItem(MI, actCheckPoint);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TdmManager.AsComponent: TComponent;
begin
  Result := Self;
end;

procedure TdmManager.ActiveViewChanged;
begin
  UpdateActions;
end;

procedure TdmManager.AddReceiver(AReceiver: IChannelReceiver);
begin
  Guard.CheckNotNull(AReceiver, 'AReceiver');
  FReceivers.Add(AReceiver);
  Events.DoAddReceiver(AReceiver);
  AReceiver.OnNewLogQueue.Add(ReceiverNewLogQueue);
end;

procedure TdmManager.AddView(ALogViewer: ILogViewer);
begin
  Guard.CheckNotNull(ALogViewer, 'ALogViewer');
  FViewList.Add(ALogViewer);
  if not FReceivers.Contains(ALogViewer.LogQueue.Receiver) then
  begin
    FReceivers.Add(ALogViewer.LogQueue.Receiver);
  end;
  Events.DoAddLogViewer(ALogViewer);
  FActiveView := ALogViewer;
end;

{ Gets called from the active messages view. }

procedure TdmManager.UpdateActions;
var
  B   : Boolean;
  MLS : TMessageListSettings;
begin
  MLS := Settings.MessageListSettings;
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
  actAutoScrollMessages.Checked
    := FSettings.MessageListSettings.AutoScrollMessages;
  B := Assigned(ActiveView);
  actStart.Enabled              := B and not ActiveView.LogQueue.Enabled;
  actStop.Enabled               := B and not actStart.Enabled;
  actBitmap.Enabled             := B;
  actCallStack.Enabled          := B;
  actCheckPoint.Enabled         := B;
  actConditional.Enabled        := B;
  actInfo.Enabled               := B;
  actWarning.Enabled            := B;
  actValue.Enabled              := B;
  actError.Enabled              := B;
  actMethodTraces.Enabled       := B;
  actException.Enabled          := B;
  actComponent.Enabled          := B;
  actHeapInfo.Enabled           := B;
  actCustomData.Enabled         := B;
  actStrings.Enabled            := B;
  actObject.Enabled             := B;
  actPersistent.Enabled         := B;
  actInterface.Enabled          := B;
  actMemory.Enabled             := B;
  actCollapseAll.Enabled        := B;
  actExpandAll.Enabled          := B;
  actAutoScrollMessages.Enabled := B;
  actClearMessages.Enabled      := B;
  actSelectAll.Enabled          := MLS.VisibleMessageTypes <> ALL_MESSAGES;
  actSelectNone.Enabled         := MLS.VisibleMessageTypes <> [];
  actFilterMessages.Enabled :=
    not Settings.MessageListSettings.AutoFilterMessages;
  actToggleAlwaysOnTop.Checked := Settings.FormSettings.FormStyle = fsStayOnTop;
  actToggleFullscreen.Checked := Settings.FormSettings.WindowState = wsMaximized;
end;

procedure TdmManager.UpdateVisibleMessageTypes(
  const AMessageType: TLogMessageType; const Sender: TObject;
  const AToggle: Boolean);
var
  A   : TAction;
  MLS : TMessageListSettings;
begin
  MLS := Settings.MessageListSettings;
  if Assigned(FActiveView) then
  begin
    A := Sender as TAction;
    if AToggle then
      A.Checked := not A.Checked;
    if A.Checked then
      MLS.VisibleMessageTypes := MLS.VisibleMessageTypes + [AMessageType]
    else
      MLS.VisibleMessageTypes := MLS.VisibleMessageTypes - [AMessageType];
    FActiveView.UpdateView;
  end;
end;
{$ENDREGION}

end.
