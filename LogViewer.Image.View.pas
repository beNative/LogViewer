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

unit LogViewer.Image.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfrmImageView = class(TForm)
    edtPixelFormat : TLabeledEdit;
    edtHandleType  : TLabeledEdit;
    edtHeight      : TLabeledEdit;
    edtWidth       : TLabeledEdit;
    sbxMain        : TScrollBox;
    imgBitmap      : TImage;

  private
  public
    procedure Clear;

  end;

implementation

{$R *.dfm}

{$REGION 'public methods'}
procedure TfrmImageView.Clear;
begin
  imgBitmap.Picture   := nil;
  edtWidth.Text       := '';
  edtHeight.Text      := '';
  edtPixelFormat.Text := '';
  edtHandleType.Text  := '';
end;
{$ENDREGION}

end.
