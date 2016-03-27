{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.ActnList;

type
  TdmManager = class(TDataModule)
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
  private

  public

  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
