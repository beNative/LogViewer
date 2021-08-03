object frmViewSettings: TfrmViewSettings
  Left = 0
  Top = 0
  ClientHeight = 153
  ClientWidth = 214
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object chkSmartTimeStamps: TCheckBox
    Left = 16
    Top = 8
    Width = 145
    Height = 17
    Caption = 'Use smart timestamps'
    TabOrder = 0
    OnClick = chkSmartTimeStampsClick
  end
  object chkAutoScrollMessages: TCheckBox
    Left = 16
    Top = 31
    Width = 169
    Height = 17
    Caption = 'Auto scroll messages'
    TabOrder = 1
    OnClick = chkAutoScrollMessagesClick
  end
  object chkAutoFilterMessages: TCheckBox
    Left = 16
    Top = 54
    Width = 145
    Height = 17
    Caption = 'Auto filter messages'
    TabOrder = 2
    OnClick = chkAutoFilterMessagesClick
  end
  object chkDynamicAutoSizeColumns: TCheckBox
    Left = 16
    Top = 77
    Width = 145
    Height = 17
    Caption = 'Dynamic autosize columns'
    TabOrder = 3
    OnClick = chkDynamicAutoSizeColumnsClick
  end
  object chkHideColumnHeaders: TCheckBox
    Left = 16
    Top = 100
    Width = 145
    Height = 17
    Caption = 'Hide column headers'
    TabOrder = 4
    OnClick = chkHideColumnHeadersClick
  end
end
