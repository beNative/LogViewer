{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Receivers.Winipc.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  LogViewer.Receivers.Winipc.Settings;

type
  TfrmWinipcSettings = class(TForm)
    lblWindowHandleName  : TLabel;
    edtWindowHandleName  : TEdit;
    edtPollingInterval   : TLabeledEdit;
    lblPollingIntervalMs : TLabel;

    procedure edtPollingIntervalChange(Sender: TObject);

  private
    FSettings : TWinipcSettings;

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TWinipcSettings
    ); reintroduce;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmWinipcSettings.Create(AOwner: TComponent;
  ASettings: TWinipcSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  edtPollingInterval.Text := FSettings.PollingInterval.ToString;
end;

destructor TfrmWinipcSettings.Destroy;
begin
  FSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmWinipcSettings.edtPollingIntervalChange(Sender: TObject);
begin
  FSettings.PollingInterval := StrToIntDef(edtPollingInterval.Text, 100);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmWinipcSettings.UpdateActions;
begin
  inherited UpdateActions;
end;
{$ENDREGION}

end.
