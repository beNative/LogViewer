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

unit LogViewer.ComPort.Settings.View;

{ ComPort configuration view. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  LogViewer.ComPort.Settings;

type
  TfrmComPortSettings = class(TForm)
    cbxPort     : TComboBox;
    cbxBaudRate : TComboBox;
    cbxParity   : TComboBox;
    cbxDataBits : TComboBox;
    cbxStopBits : TComboBox;
    lblPort     : TLabel;
    lblBaudRate : TLabel;
    lblDataBits : TLabel;
    lblParity   : TLabel;
    lblStopBits : TLabel;

  private
    FSettings: TComPortSettings;

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TComPortSettings
    ); reintroduce;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmComPortSettings.Create(AOwner: TComponent;
  ASettings: TComPortSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmComPortSettings.BeforeDestruction;
begin
  FSettings := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmComPortSettings.UpdateActions;
begin
  inherited UpdateActions;
  cbxPort.Text     := FSettings.Port;
  cbxBaudRate.Text := FSettings.BaudRate.ToString;
  cbxDataBits.Text := FSettings.DataBits.ToString;
  cbxStopBits.Text := FSettings.StopBits.ToString;
  cbxParity.Text   := FSettings.Parity;
end;
{$ENDREGION}

end.
