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

unit LogViewer.WinODS.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  LogViewer.WinODS.Settings;

type
  TfrmWinODSSettings = class(TForm)
    {$REGION 'designer controls'}
    lblProcess   : TLabel;
    lblProcessId : TLabel;
    edtProcess   : TButtonedEdit;
    edtProcessId : TButtonedEdit;
    {$ENDREGION}

  private
    FSettings : TWinODSSettings;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TWinODSSettings
    ); reintroduce;

    procedure BeforeDestruction; override;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmWinODSSettings.Create(AOwner: TComponent;
  ASettings: TWinODSSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmWinODSSettings.BeforeDestruction;
begin
  FSettings := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
