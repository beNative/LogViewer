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

unit LogViewer.ZeroMQ.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  LogViewer.ZeroMQ.Settings;

type
  TfrmZeroMQSettings = class(TForm)
    edtAddress : TEdit;
    edtPort    : TEdit;
    lblAddress : TLabel;
    lblPort    : TLabel;
    mmoSubscriptions: TMemo;

  private
    FSettings : TZeroMQSettings;

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TZeroMQSettings
    ); reintroduce; virtual;

  end;

implementation

{$R *.dfm}

uses
  Spring;

{$REGION 'construction and destruction'}
constructor TfrmZeroMQSettings.Create(AOwner: TComponent;
  ASettings: TZeroMQSettings);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(ASettings, 'ASettings');
  FSettings := ASettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmZeroMQSettings.UpdateActions;
begin
  inherited UpdateActions;
  edtAddress.Text := FSettings.Address;
  edtPort.Text    := FSettings.Port.ToString;
end;
{$ENDREGION}


end.
