object frmZeroMQSettings: TfrmZeroMQSettings
  Left = 0
  Top = 0
  ClientHeight = 307
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    313
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAddress: TLabel
    Left = 8
    Top = 11
    Width = 43
    Height = 13
    Caption = 'Address:'
    FocusControl = edtAddress
  end
  object lblPort: TLabel
    Left = 8
    Top = 37
    Width = 24
    Height = 13
    Caption = 'Port:'
    FocusControl = edtPort
  end
  object edtAddress: TEdit
    Left = 57
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtPort: TEdit
    Left = 57
    Top = 34
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object mmoSubscriptions: TMemo
    Left = 8
    Top = 72
    Width = 297
    Height = 227
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    ExplicitWidth = 292
  end
end
