{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.Watches.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  LogViewer.Watches.Settings;

type
  TfrmWatchSettings = class(TForm)
    chkOnlyTrackChanges        : TCheckBox;
    chkSyncWithSelectedMessage : TCheckBox;
    chkShowWatchHistory        : TCheckBox;
    chkHideColumnHeaders       : TCheckBox;

    {$REGION 'event handlers'}
    procedure chkOnlyTrackChangesClick(Sender: TObject);
    procedure chkShowWatchHistoryClick(Sender: TObject);
    procedure chkSyncWithSelectedMessageClick(Sender: TObject);
    procedure chkHideColumnHeadersClick(Sender: TObject);
    {$ENDREGION}

  private
    FSettings : TWatchSettings;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TWatchSettings
    ); reintroduce;

    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmWatchSettings.Create(AOwner: TComponent;
  ASettings: TWatchSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  chkOnlyTrackChanges.Checked        := FSettings.OnlyTrackChanges;
  chkSyncWithSelectedMessage.Checked := FSettings.SyncWithSelection;
  chkShowWatchHistory.Checked        := FSettings.WatchHistoryVisible;
  chkHideColumnHeaders.Checked       := not FSettings.ColumnHeadersVisible;
end;

destructor TfrmWatchSettings.Destroy;
begin
  FSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmWatchSettings.chkHideColumnHeadersClick(Sender: TObject);
begin
  FSettings.ColumnHeadersVisible := not (Sender as TCheckBox).Checked;
end;

procedure TfrmWatchSettings.chkOnlyTrackChangesClick(Sender: TObject);
begin
  FSettings.OnlyTrackChanges := (Sender as TCheckBox).Checked;
end;

procedure TfrmWatchSettings.chkShowWatchHistoryClick(Sender: TObject);
begin
  FSettings.WatchHistoryVisible := (Sender as TCheckBox).Checked;
end;

procedure TfrmWatchSettings.chkSyncWithSelectedMessageClick(Sender: TObject);
begin
  FSettings.SyncWithSelection := (Sender as TCheckBox).Checked;
end;
{$ENDREGION}

end.
