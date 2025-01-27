object frmViewSettings: TfrmViewSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 230
  ClientWidth = 330
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 23
  object chkSmartTimeStamps: TCheckBox
    Left = 24
    Top = 12
    Width = 218
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Use smart timestamps'
    TabOrder = 0
    OnClick = chkSmartTimeStampsClick
  end
  object chkAutoScrollMessages: TCheckBox
    Left = 24
    Top = 47
    Width = 254
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Auto scroll messages'
    TabOrder = 1
    OnClick = chkAutoScrollMessagesClick
  end
  object chkAutoFilterMessages: TCheckBox
    Left = 24
    Top = 81
    Width = 218
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Auto filter messages'
    TabOrder = 2
    OnClick = chkAutoFilterMessagesClick
  end
  object chkDynamicAutoSizeColumns: TCheckBox
    Left = 24
    Top = 116
    Width = 218
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Dynamic autosize columns'
    TabOrder = 3
    OnClick = chkDynamicAutoSizeColumnsClick
  end
  object chkHideColumnHeaders: TCheckBox
    Left = 24
    Top = 150
    Width = 218
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Hide column headers'
    TabOrder = 4
    OnClick = chkHideColumnHeadersClick
  end
end
