{
  Copyright (C) 2013-2024 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.RawData.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Clipbrd, Vcl.Menus,
  Vcl.ActnList,

  kcontrols, khexeditor;

type
  TfrmRawDataView = class(TForm)
    heMain  : TKHexEditor;
    ppmMain : TPopupMenu;
    mniCopy : TMenuItem;
    aclMain : TActionList;
    actCopy : TAction;

    procedure actCopyExecute(Sender: TObject);

  public
    procedure Clear;
    procedure LoadFromStream(AStream: TStream);
  end;

implementation

{$R *.dfm}

uses
  Spring;

{$REGION 'action handlers'}
procedure TfrmRawDataView.actCopyExecute(Sender: TObject);
var
  SS : TStringStream;
begin
  SS := TStringStream.Create;
  try
    heMain.SaveToStream(SS);
    Clipboard.AsText := SS.DataString;
  finally
    SS.Free;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmRawDataView.Clear;
begin
  heMain.Clear;
end;

procedure TfrmRawDataView.LoadFromStream(AStream: TStream);
begin
  if Assigned(AStream) then
  begin
    Guard.CheckNotNull(AStream, 'AStream');
    AStream.Position := 0;
    heMain.LoadFromStream(AStream);
  end
  else
  begin
    Clear;
  end;
end;
{$ENDREGION}

end.
