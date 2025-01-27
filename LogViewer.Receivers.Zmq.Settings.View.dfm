object frmZmqSettings: TfrmZmqSettings
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 303
  ClientWidth = 426
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
  object lblPollingTimeoutMs: TLabel
    Left = 227
    Top = 17
    Width = 22
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Caption = 'ms'
  end
  object lblPollingIntervalMs: TLabel
    Left = 227
    Top = 57
    Width = 22
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Caption = 'ms'
  end
  object edtPollingTimeout: TLabeledEdit
    Left = 132
    Top = 12
    Width = 86
    Height = 31
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    EditLabel.Width = 120
    EditLabel.Height = 31
    EditLabel.Margins.Left = 5
    EditLabel.Margins.Top = 5
    EditLabel.Margins.Right = 5
    EditLabel.Margins.Bottom = 5
    EditLabel.Caption = 'Polling &timeout:'
    LabelPosition = lpLeft
    NumbersOnly = True
    TabOrder = 0
    Text = ''
    OnChange = edtPollingTimeoutChange
  end
  object edtPollingInterval: TLabeledEdit
    Left = 132
    Top = 53
    Width = 86
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
