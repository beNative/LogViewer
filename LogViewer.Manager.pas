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

  Spring.Collections, Spring,

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
    procedure actZeroMQChannelExecute(Sender: TObject);
    procedure actWinIPCChannelExecute(Sender: TObject);
    procedure actSetFocusToFilterExecute(Sender: TObject);
    procedure actToggleFullscreenExecute(Sender: TObject);
    procedure actODSChannelExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    {$ENDREGION}

  private
    FSettings            : TLogViewerSettings;
    FEvents              : ILogViewerEvents;
    FCommands            : ILogViewerCommands;
    FActiveView          : ILogViewerMessagesView;
    FViewList            : IList<ILogViewerMessagesView>;
    FVisibleMessageTypes : TLogMessageTypes;

  protected
    function GetCommands: ILogViewerCommands;
    function GetEvents: ILogViewerEvents;
    function GetVisibleMessageTypes: TLogMessageTypes;
    function GetSettings: TLogViewerSettings;

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

    property VisibleMessageTypes: TLogMessageTypes
      read GetVisibleMessageTypes;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  Vcl.Forms,

  LogViewer.Events, LogViewer.Commands, LogViewer.Resources;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TdmManager.AddView(AView: ILogViewerMessagesView);
begin
  FViewList.Add(AView);
end;

procedure TdmManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TLogViewerSettings.Create;
  FEvents   := TLogViewerEvents.Create(Self);
  FCommands := TLogViewerCommands.Create(Self);
  FVisibleMessageTypes := ALL_MESSAGES;
end;

procedure TdmManager.BeforeDestruction;
begin
  FSettings.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
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
  UpdateVisibleMessageTypes(lmtLeaveMethod, Sender);
end;

procedure TdmManager.actObjectExecute(Sender: TObject);
begin
  UpdateVisibleMessageTypes(lmtObject, Sender);
end;

procedure TdmManager.actODSChannelExecute(Sender: TObject);
begin
//
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

procedure TdmManager.actWinIPCChannelExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actZeroMQChannelExecute(Sender: TObject);
begin
//
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
    //Events.DoActiveViewChange;
    //ActiveViewChanged;
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

function TdmManager.GetVisibleMessageTypes: TLogMessageTypes;
begin
  Result := FVisibleMessageTypes;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Gets called from the active messages view. }

procedure TdmManager.UpdateActions;
var
  B: Boolean;
begin
  actBitmap.Checked        := lmtBitmap in FVisibleMessageTypes;
  actCallStack.Checked     := lmtCallStack in FVisibleMessageTypes;
  actCheckPoint.Checked    := lmtCheckpoint in FVisibleMessageTypes;
  actConditional.Checked   := lmtConditional in FVisibleMessageTypes;
  actInfo.Checked          := lmtInfo in FVisibleMessageTypes;
  actWarning.Checked       := lmtWarning in FVisibleMessageTypes;
  actValue.Checked         := lmtValue in FVisibleMessageTypes;
  actError.Checked         := lmtError in FVisibleMessageTypes;
  actMethodTraces.Checked  := lmtEnterMethod in FVisibleMessageTypes;
  actException.Checked     := lmtException in FVisibleMessageTypes;
  actObject.Checked        := lmtObject in FVisibleMessageTypes;
  actHeapInfo.Checked      := lmtHeapInfo in FVisibleMessageTypes;
  actCustomData.Checked    := lmtCustomData in FVisibleMessageTypes;
  actStrings.Checked       := lmtStrings in FVisibleMessageTypes;
  actMemory.Checked        := lmtMemory in FVisibleMessageTypes;
  B                        := not actStop.Checked;
  actBitmap.Enabled        := B;
  actCallStack.Enabled     := B;
  actCheckPoint.Enabled    := B;
  actConditional.Enabled   := B;
  actInfo.Enabled          := B;
  actWarning.Enabled       := B;
  actValue.Enabled         := B;
  actError.Enabled         := B;
  actMethodTraces.Enabled  := B;
  actException.Enabled     := B;
  actObject.Enabled        := B;
  actHeapInfo.Enabled      := B;
  actCustomData.Enabled    := B;
  actStrings.Enabled       := B;
  actMemory.Enabled        := B;
  actSelectAll.Enabled  := not (FVisibleMessageTypes = ALL_MESSAGES);
  actSelectNone.Enabled := not (FVisibleMessageTypes = []);
  //actToggleAlwaysOnTop.Checked := FormStyle = fsStayOnTop;

  //actFilterMessages.Enabled := not chkAutoFilter.Checked and (TitleFilter <> '');
  //sbrMain.SimpleText := Format('%d messages received.', [FMessageCount]);
end;

procedure TdmManager.UpdateVisibleMessageTypes(
  const AMessageType: TLogMessageType; const Sender: TObject;
  const AToggle: Boolean);
var
  A : TAction;
begin
  A := Sender as TAction;
  if AToggle then
    A.Checked := not A.Checked;
  if A.Checked then
    Include(FVisibleMessageTypes, AMessageType)
  else
    Exclude(FVisibleMessageTypes, AMessageType);

  if Assigned(FActiveView) then
    FActiveView.UpdateView;
end;

{$ENDREGION}

end.
