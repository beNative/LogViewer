{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Receivers.Winods.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  LogViewer.Receivers.Winods.Settings;

type
  TfrmWinodsSettings = class(TForm)
    {$REGION 'designer controls'}
    lblProcess   : TLabel;
    lblProcessId : TLabel;
    edtProcess   : TButtonedEdit;
    edtProcessId : TButtonedEdit;
    {$ENDREGION}

  private
    FSettings : TWinodsSettings;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TWinodsSettings
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmWinodsSettings.Create(AOwner: TComponent;
  ASettings: TWinodsSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

destructor TfrmWinodsSettings.Destroy;
begin
  FSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

end.
