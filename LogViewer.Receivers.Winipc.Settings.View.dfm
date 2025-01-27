object frmWinipcSettings: TfrmWinipcSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 126
  ClientWidth = 495
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
  object lblWindowHandleName: TLabel
    Left = 12
    Top = 12
    Width = 171
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Caption = 'Window handle name:'
    FocusControl = edtWindowHandleName
  end
  object lblPollingIntervalMs: TLabel
    Left = 275
    Top = 54
    Width = 22
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Caption = 'ms'
  end
  object edtWindowHandleName: TEdit
    Left = 197
    Top = 9
    Width = 273
    Height = 31
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 0
  end
  object edtPollingInterval: TLabeledEdit
    Left = 197
    Top = 50
    Width = 85
    Height = 31
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    EditLabel.Width = 116
    EditLabel.Height = 31
    EditLabel.Margins.Left = 5
    EditLabel.Margins.Top = 5
    EditLabel.Margins.Right = 5
    EditLabel.Margins.Bottom = 5
    EditLabel.Caption = 'Polling &interval:'
    LabelPosition = lpLeft
    NumbersOnly = True
    TabOrder = 1
    Text = ''
    OnChange = edtPollingIntervalChange
  end
end
