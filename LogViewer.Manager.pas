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

  LogViewer.Interfaces, LogViewer.Settings;


{
  TODO:
  - handle list of registered channels (of wich each is associated with a
    view?) => IList<IChannelReceiver>
}

type
  TdmManager = class(TDataModule, ILogViewerActions,
                                  ILogViewerMenus,
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
    actODSChannel        : TAction;
    actOpen              : TAction;
    actSave              : TAction;
    actSelectAll         : TAction;
    actSelectNone        : TAction;
    actSerialPortChannel : TAction;
    actSetFocusToFilter  : TAction;
    actStop              : TAction;
    actStrings           : TAction;
    actToggleAlwaysOnTop : TAction;
    actToggleFullscreen  : TAction;
    actValue             : TAction;
    actWarning           : TAction;
    actWinIPCChannel     : TAction;
    actZeroMQChannel     : TAction;
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
    FSettings   : TLogViewerSettings;
    FActiveView : ILogViewerMessagesView;

  protected
    function GetActiveView: ILogViewerMessagesView;
    procedure SetActiveView(const Value: ILogViewerMessagesView);

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
    // TODO
    {$ENDREGION}

    {$REGION 'ILogViewerManager'}
    function GetMenus: ILogViewerMenus;
    function GetActions: ILogViewerActions;
    function GetSettings: TLogViewerSettings;

    property Menus: ILogViewerMenus
      read GetMenus;

    property Actions: ILogViewerActions
      read GetActions;

    property Settings: TLogViewerSettings
      read GetSettings;
    {$ENDREGION}

    { Set/get the reference to the active view. }
    property ActiveView: ILogViewerMessagesView
      read GetActiveView write SetActiveView;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  Vcl.Forms;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TdmManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings := TLogViewerSettings.Create;

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
//
end;

procedure TdmManager.actCallStackExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actCheckPointExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actClearMessagesExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actCollapseAllExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actConditionalExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actCustomDataExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actErrorExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actExceptionExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actExpandAllExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actFilterMessagesExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actHeapInfoExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actMemoryExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actMethodTracesExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actObjectExecute(Sender: TObject);
begin
//
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
//
end;

procedure TdmManager.actStringsExecute(Sender: TObject);
begin
//
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
//
end;

procedure TdmManager.actWarningExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actValueExecute(Sender: TObject);
begin
//
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
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmManager.UpdateActions;
begin

end;
{$ENDREGION}

end.
