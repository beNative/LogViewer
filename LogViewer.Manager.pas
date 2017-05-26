{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Handles all application events. }

uses
  System.SysUtils, System.Classes, System.Actions,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.ActnList,

  Spring, Spring.Collections,

  DDuce.Logger.Interfaces,

  LogViewer.Interfaces, LogViewer.Settings;

{
  TODO:
  - handle list of registered channels (of wich each is associated with a
    view?) => IList<IChannelReceiver>
}

type
  TdmManager = class(TDataModule, ILogViewerActions,
                                  ILogViewerMenus,
                                  ILogViewerCommands,
                                  ILogViewerEvents,
                                  ILogViewerManager
  )
    {$REGION 'designer controls'}
    aclMain              : TActionList;
    actBitmap            : TAction;
    actCallStack         : TAction;
    actCheckPoint        : TAction;
    actClearMessages     : TAction;
    actCollapseAll       : TAction;
    actConditional       : TAction;
    actCustomData        : TAction;
    actError             : TAction;
    actException         : TAction;
    actExpandAll         : TAction;
    actFilterMessages    : TAction;
    actHeapInfo          : TAction;
    actInfo              : TAction;
    actMemory            : TAction;
    actMethodTraces      : TAction;
    actObject            : TAction;
    actOpen              : TAction;
    actSave              : TAction;
    actSelectAll         : TAction;
    actSelectNone        : TAction;
    actSetFocusToFilter  : TAction;
    actStop              : TAction;
    actStrings           : TAction;
    actToggleAlwaysOnTop : TAction;
    actToggleFullscreen  : TAction;
    actValue             : TAction;
    actWarning           : TAction;
    imlMain              : TImageList;
    imlMessageTypes      : TImageList;
    tmrPoll              : TTimer;
    actGotoFirst: TAction;
    actGotoLast: TAction;
    actSettings: TAction;
    actAutoScrollMessages: TAction;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actClearMessagesExecute(Sender: TObject);
    procedure actToggleAlwaysOnTopExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectNoneExecute(Sender: TObject);
    procedure actInfoExecute(Sender: TObject);
    procedure actValueExecute(Sender: TObject);
    procedure actWarningExecute(Sender: TObject);
    procedure actConditionalExecute(Sender: TObject);
    procedure actErrorExecute(Sender: TObject);
    procedure actCheckPointExecute(Sender: TObject);
    procedure actStringsExecute(Sender: TObject);
    procedure actCallStackExecute(Sender: TObject);
    procedure actObjectExecute(Sender: TObject);
    procedure actExceptionExecute(Sender: TObject);
    procedure actBitmapExecute(Sender: TObject);
    procedure actHeapInfoExecute(Sender: TObject);
    procedure actMemoryExecute(Sender: TObject);
    procedure actCustomDataExecute(Sender: TObject);
    procedure actMethodTracesExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actFilterMessagesExecute(Sender: TObject);
    procedure actSetFocusToFilterExecute(Sender: TObject);
    procedure actToggleFullscreenExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actGotoFirstExecute(Sender: TObject);
    procedure actGotoLastExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actAutoScrollMessagesExecute(Sender: TObject);
    {$ENDREGION}

  private
    FSettings            : TLogViewerSettings;
    FEvents              : ILogViewerEvents;
    FCommands            : ILogViewerCommands;
    FActiveView          : ILogViewerMessagesView;
    FViewList            : IList<ILogViewerMessagesView>;
    FReceivers           : IList<IChannelReceiver>;

  protected
    function GetCommands: ILogViewerCommands;
    function GetEvents: ILogViewerEvents;
    function GetSettings: TLogViewerSettings;
    function GetViews: IList<ILogViewerMessagesView>;

    procedure ActiveViewChanged;

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
    {$ENDREGION}

    {$REGION 'ILogViewerManager'}
    function GetActions: ILogViewerActions;
    function GetActiveView: ILogViewerMessagesView;
    procedure SetActiveView(const Value: ILogViewerMessagesView);

    procedure AddView(AView: ILogViewerMessagesView);

    property ActiveView: ILogViewerMessagesView
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

    property Views: IList<ILogViewerMessagesView>
      read GetViews;

  public
    constructor Create(AOwner: TComponent; ASettings: TLogViewerSettings);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  Vcl.Forms,

  LogViewer.Events, LogViewer.Commands, LogViewer.Resources,
  LogViewer.Settings.Dialog, LogViewer.MessageList.Settings;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TdmManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FEvents   := TLogViewerEvents.Create(Self);
  FCommands := TLogViewerCommands.Create(Self);
  FReceivers := TCollections.CreateInterfaceList<IChannelReceiver>;
  FViewList  := TCollections.CreateInterfaceList<ILogViewerMessagesView>;
end;

procedure TdmManager.BeforeDestruction;
begin
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
//
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

procedure TdmManager.actObjectExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtObject, Sender);
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
  F := TfrmLogViewerSettings.Create(Self);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TdmManager.actStopExecute(Sender: TObject);
begin
  if actStop.Checked then
    Commands.Stop
  else
    Commands.Start;
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

function TdmManager.GetActiveView: ILogViewerMessagesView;
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

procedure TdmManager.SetActiveView(const Value: ILogViewerMessagesView);
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

function TdmManager.GetMenus: ILogViewerMenus;
begin
  Result := Self as ILogViewerMenus;
end;

function TdmManager.GetSettings: TLogViewerSettings;
begin
  Result := FSettings;
end;

function TdmManager.GetViews: IList<ILogViewerMessagesView>;
begin
  Result := FViewList;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmManager.ActiveViewChanged;
begin
  UpdateActions;
end;

procedure TdmManager.AddView(AView: ILogViewerMessagesView);
begin
  FViewList.Add(AView);
  if not FReceivers.Contains(AView.Receiver) then
  begin
    FReceivers.Add(AView.Receiver);
  end;
  FActiveView := AView;
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
  actObject.Checked       := lmtObject in MLS.VisibleMessageTypes;
  actHeapInfo.Checked     := lmtHeapInfo in MLS.VisibleMessageTypes;
  actCustomData.Checked   := lmtCustomData in MLS.VisibleMessageTypes;
  actStrings.Checked      := lmtStrings in MLS.VisibleMessageTypes;
  actMemory.Checked       := lmtMemory in MLS.VisibleMessageTypes;
  actAutoScrollMessages.Checked
    := FSettings.MessageListSettings.AutoScrollMessages;
  B                       := not actStop.Checked;
  actBitmap.Enabled       := B;
  actCallStack.Enabled    := B;
  actCheckPoint.Enabled   := B;
  actConditional.Enabled  := B;
  actInfo.Enabled         := B;
  actWarning.Enabled      := B;
  actValue.Enabled        := B;
  actError.Enabled        := B;
  actMethodTraces.Enabled := B;
  actException.Enabled    := B;
  actObject.Enabled       := B;
  actHeapInfo.Enabled     := B;
  actCustomData.Enabled   := B;
  actStrings.Enabled      := B;
  actMemory.Enabled       := B;
  actSelectAll.Enabled    := MLS.VisibleMessageTypes <> ALL_MESSAGES;
  actSelectNone.Enabled   := MLS.VisibleMessageTypes <> [];
  actToggleAlwaysOnTop.Checked := Settings.FormSettings.FormStyle = fsStayOnTop;
  actToggleFullscreen.Checked := Settings.FormSettings.WindowState = wsMaximized;

  //actFilterMessages.Enabled := not chkAutoFilter.Checked and (TitleFilter <> '');
  //sbrMain.SimpleText := Format('%d messages received.', [FMessageCount]);
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
