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

uses
  System.SysUtils, System.Classes, System.Actions,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.ActnList,

  LogViewer.Interfaces, LogViewer.Settings;

type
  TdmManager = class(TDataModule, ILogViewerActions, ILogViewerManager)
    {$REGION 'designer controls'}
    aclMain              : TActionList;
    actClearMessages     : TAction;
    actToggleAlwaysOnTop : TAction;
    actOpen              : TAction;
    actSave              : TAction;
    actSelectAll         : TAction;
    actSelectNone        : TAction;
    actToggleInfo        : TAction;
    actToggleWarning     : TAction;
    actValue             : TAction;
    actError             : TAction;
    actConditional       : TAction;
    actCheckPoint        : TAction;
    actStrings           : TAction;
    actCallStack         : TAction;
    actObject            : TAction;
    actException         : TAction;
    actBitmap            : TAction;
    actHeapInfo          : TAction;
    actMemory            : TAction;
    actCustomData        : TAction;
    actMethodTraces      : TAction;
    actStop              : TAction;
    actFilterMessages    : TAction;
    actZeroMQChannel     : TAction;
    actWinIPCChannel     : TAction;
    actSetFocusToFilter  : TAction;
    actToggleFullscreen  : TAction;
    actODSChannel        : TAction;
    actCollapseAll       : TAction;
    actExpandAll         : TAction;
    imlMessageTypes      : TImageList;
    tmrPoll              : TTimer;
    imlMain              : TImageList;
    {$ENDREGION}

    procedure actClearMessagesExecute(Sender: TObject);
    procedure actToggleAlwaysOnTopExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectNoneExecute(Sender: TObject);
    procedure actToggleInfoExecute(Sender: TObject);
    procedure actValueExecute(Sender: TObject);
    procedure actToggleWarningExecute(Sender: TObject);
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

  private
    FSettings : TLogViewerSettings;

  protected

    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;

    procedure UpdateActions;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ActionList: TActionList
      read GetActionList;

  public

  end;

implementation

{$R *.dfm}

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
begin
//
end;

procedure TdmManager.actToggleFullscreenExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actToggleInfoExecute(Sender: TObject);
begin
//
end;

procedure TdmManager.actToggleWarningExecute(Sender: TObject);
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

function TdmManager.GetItem(AName: string): TCustomAction;
//var
//  A: TCustomAction;
begin
//  A := aclMain.ActionByName(AName) as TCustomAction;
//  if Assigned(A) then
//    Result := A
//  else
//    Logger.SendWarning(Format('Action with name (%s) not found!', [AName]));
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmManager.UpdateActions;
begin

end;
{$ENDREGION}



end.
