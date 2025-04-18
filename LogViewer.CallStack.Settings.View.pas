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

unit LogViewer.CallStack.Settings.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  LogViewer.CallStack.Settings;

type
  TfrmCallStackSettings = class(TForm)
    chkHideColumnHeaders : TCheckBox;

    procedure chkHideColumnHeadersClick(Sender: TObject);

  private
    FSettings : TCallStackSettings;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TCallStackSettings
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmCallStackSettings.Create(AOwner: TComponent;
  ASettings: TCallStackSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  chkHideColumnHeaders.Checked := not FSettings.ColumnHeadersVisible;
end;

destructor TfrmCallStackSettings.Destroy;
begin
  FSettings := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmCallStackSettings.chkHideColumnHeadersClick(Sender: TObject);
begin
  FSettings.ColumnHeadersVisible := not (Sender as TCheckBox).Checked;
end;
{$ENDREGION}

end.
