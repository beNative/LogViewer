object frmWinodsSettings: TfrmWinodsSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 92
  ClientWidth = 329
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  PixelsPerInch = 144
  TextHeight = 23
  object lblProcess: TLabel
    Left = 24
    Top = 12
    Width = 61
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Caption = 'Process:'
    FocusControl = edtProcess
  end
  object lblProcessId: TLabel
    Left = 24
    Top = 48
    Width = 81
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Caption = 'Process Id:'
    FocusControl = edtProcessId
  end
  object edtProcess: TButtonedEdit
    Left = 114
    Top = 8
    Width = 182
    Height = 31
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 0
  end
  object edtProcessId: TButtonedEdit
    Left = 114
    Top = 44
    Width = 182
    Height = 31
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 1
  end
end
