{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.MessageList.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  LogViewer.MessageList.Settings;

type
  TfrmViewSettings = class(TForm)
    chkSmartTimeStamps        : TCheckBox;
    chkAutoScrollMessages     : TCheckBox;
    chkAutoFilterMessages     : TCheckBox;
    chkDynamicAutoSizeColumns : TCheckBox;
    chkHideColumnHeaders      : TCheckBox;

    procedure chkSmartTimeStampsClick(Sender: TObject);
    procedure chkAutoScrollMessagesClick(Sender: TObject);
    procedure chkAutoFilterMessagesClick(Sender: TObject);
    procedure chkDynamicAutoSizeColumnsClick(Sender: TObject);
    procedure chkHideColumnHeadersClick(Sender: TObject);

  private
    FSettings : TMessageListSettings;

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TMessageListSettings
    ); reintroduce; virtual;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmViewSettings.Create(AOwner: TComponent;
  ASettings: TMessageListSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmViewSettings.chkAutoFilterMessagesClick(Sender: TObject);
begin
  FSettings.AutoFilterMessages := (Sender as TCheckBox).Checked;
end;

procedure TfrmViewSettings.chkAutoScrollMessagesClick(Sender: TObject);
begin
  FSettings.AutoScrollMessages := (Sender as TCheckBox).Checked;
end;

procedure TfrmViewSettings.chkDynamicAutoSizeColumnsClick(Sender: TObject);
begin
  FSettings.DynamicAutoSizeColumns := (Sender as TCheckBox).Checked;
end;

procedure TfrmViewSettings.chkHideColumnHeadersClick(Sender: TObject);
begin
  FSettings.ColumnHeadersVisible := not (Sender as TCheckBox).Checked;
end;

procedure TfrmViewSettings.chkSmartTimeStampsClick(Sender: TObject);
begin
  FSettings.SmartTimeStamps := (Sender as TCheckBox).Checked;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmViewSettings.UpdateActions;
begin
  inherited UpdateActions;
  chkSmartTimeStamps.Checked        := FSettings.SmartTimeStamps;
  chkAutoScrollMessages.Checked     := FSettings.AutoScrollMessages;
  chkAutoFilterMessages.Checked     := FSettings.AutoFilterMessages;
  chkDynamicAutoSizeColumns.Checked := FSettings.DynamicAutoSizeColumns;
  chkHideColumnHeaders.Checked      := not FSettings.ColumnHeadersVisible;
end;
{$ENDREGION}

end.
