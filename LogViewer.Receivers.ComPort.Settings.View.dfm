object frmComPortSettings: TfrmComPortSettings
  Left = 228
  Top = 228
  ClientHeight = 69
  ClientWidth = 270
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object lblPort: TLabel
    Left = 16
    Top = 11
    Width = 24
    Height = 13
    Margins.Bottom = 2
    Caption = 'Port:'
    FocusControl = cbxPort
  end
  object lblBaudRate: TLabel
    Left = 16
    Top = 38
    Width = 49
    Height = 13
    Margins.Bottom = 2
    Caption = 'Baudrate:'
    FocusControl = cbxBaudRate
  end
  object cbxPort: TComboBox
    Left = 88
    Top = 8
    Width = 145
    Height = 31
    TabOrder = 0
  end
  object cbxBaudRate: TComboBox
    Left = 88
    Top = 35
    Width = 145
    Height = 31
    TabOrder = 1
  end
end
