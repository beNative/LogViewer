object frmViewSettings: TfrmViewSettings
  Left = 0
  Top = 0
  ClientHeight = 123
  ClientWidth = 499
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
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
end
