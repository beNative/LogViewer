object frmWinODSSettings: TfrmWinODSSettings
  Left = 0
  Top = 0
  ClientHeight = 61
  ClientWidth = 213
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblProcess: TLabel
    Left = 16
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Process:'
    FocusControl = edtProcess
  end
  object lblProcessId: TLabel
    Left = 16
    Top = 32
    Width = 54
    Height = 13
    Caption = 'Process Id:'
    FocusControl = edtProcessId
  end
  object edtProcess: TButtonedEdit
    Left = 76
    Top = 5
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtProcessId: TButtonedEdit
    Left = 76
    Top = 29
    Width = 121
    Height = 21
    TabOrder = 1
  end
end
