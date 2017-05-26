object frmComPortSettings: TfrmComPortSettings
  Left = 0
  Top = 0
  ClientHeight = 175
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblPort: TLabel
    Left = 72
    Top = 19
    Width = 30
    Height = 13
    Caption = 'lblPort'
    FocusControl = cbxPort
  end
  object lblBaudRate: TLabel
    Left = 52
    Top = 46
    Width = 57
    Height = 13
    Caption = 'lblBaudRate'
    FocusControl = cbxBaudRate
  end
  object lblDataBits: TLabel
    Left = 52
    Top = 100
    Width = 50
    Height = 13
    Caption = 'lblDataBits'
    FocusControl = cbxDataBits
  end
  object lblParity: TLabel
    Left = 56
    Top = 73
    Width = 38
    Height = 13
    Caption = 'lblParity'
    FocusControl = cbxParity
  end
  object lblStopBits: TLabel
    Left = 53
    Top = 127
    Width = 49
    Height = 13
    Caption = 'lblStopBits'
  end
  object cbxPort: TComboBox
    Left = 128
    Top = 16
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object cbxBaudRate: TComboBox
    Left = 128
    Top = 43
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object cbxParity: TComboBox
    Left = 128
    Top = 70
    Width = 145
    Height = 21
    TabOrder = 2
  end
  object cbxDataBits: TComboBox
    Left = 128
    Top = 97
    Width = 145
    Height = 21
    TabOrder = 3
  end
  object cbxStopBits: TComboBox
    Left = 128
    Top = 124
    Width = 145
    Height = 21
    TabOrder = 4
  end
end
