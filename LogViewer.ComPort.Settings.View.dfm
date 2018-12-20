object frmComPortSettings: TfrmComPortSettings
  Left = 0
  Top = 0
  ClientHeight = 149
  ClientWidth = 258
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
    Left = 16
    Top = 11
    Width = 24
    Height = 13
    Caption = 'Port:'
    FocusControl = cbxPort
  end
  object lblBaudRate: TLabel
    Left = 16
    Top = 38
    Width = 48
    Height = 13
    Caption = 'Baudrate:'
    FocusControl = cbxBaudRate
  end
  object lblDataBits: TLabel
    Left = 16
    Top = 92
    Width = 47
    Height = 13
    Caption = 'Data bits:'
    FocusControl = cbxDataBits
  end
  object lblParity: TLabel
    Left = 16
    Top = 65
    Width = 32
    Height = 13
    Caption = 'Parity:'
    FocusControl = cbxParity
  end
  object lblStopBits: TLabel
    Left = 16
    Top = 119
    Width = 46
    Height = 13
    Caption = 'Stop bits:'
  end
  object cbxPort: TComboBox
    Left = 88
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object cbxBaudRate: TComboBox
    Left = 88
    Top = 35
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object cbxParity: TComboBox
    Left = 88
    Top = 62
    Width = 145
    Height = 21
    TabOrder = 2
  end
  object cbxDataBits: TComboBox
    Left = 88
    Top = 89
    Width = 145
    Height = 21
    TabOrder = 3
  end
  object cbxStopBits: TComboBox
    Left = 88
    Top = 116
    Width = 145
    Height = 21
    TabOrder = 4
  end
end
