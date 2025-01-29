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

unit LogViewer.Image.View;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ActnList, Vcl.Menus, Vcl.Mask;

type
  TfrmImageView = class(TForm)
    {$REGION 'designer controls'}
    aclMain        : TActionList;
    actCopy        : TAction;
    edtHandleType  : TLabeledEdit;
    edtHeight      : TLabeledEdit;
    edtPixelFormat : TLabeledEdit;
    edtWidth       : TLabeledEdit;
    imgBitmap      : TImage;
    mniCopy        : TMenuItem;
    ppmMain        : TPopupMenu;
    sbxMain        : TScrollBox;
    {$ENDREGION}

    procedure actCopyExecute(Sender: TObject);

  private
    function GetBitmap: TBitmap;

  public
    procedure Clear;
    procedure LoadFromStream(AStream: TStream);

    property Bitmap : TBitmap
      read GetBitmap;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Clipbrd,

  Spring,

  DDuce.Reflect;

{$REGION 'property access methods'}
function TfrmImageView.GetBitmap: TBitmap;
begin
  Result := imgBitmap.Picture.Bitmap;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmImageView.actCopyExecute(Sender: TObject);
begin
  Clipboard.Assign(imgBitmap.Picture);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmImageView.Clear;
begin
  imgBitmap.Picture   := nil;
  edtWidth.Text       := '';
  edtHeight.Text      := '';
  edtPixelFormat.Text := '';
  edtHandleType.Text  := '';
end;

procedure TfrmImageView.LoadFromStream(AStream: TStream);
begin
  Guard.CheckNotNull(AStream, 'AStream');
  AStream.Position := 0;
  imgBitmap.Picture.LoadFromStream(AStream);
  with imgBitmap.Picture do
  begin
    edtWidth.Text       := imgBitmap.Width.ToString;
    edtHeight.Text      := imgBitmap.Height.ToString;
    //edtPixelFormat.Text := Bitmap.PixelFormat.ToString;
    //edtHandleType.Text  := Bitmap.HandleType.ToString;
  end;
end;
{$ENDREGION}

end.
