object frmZmqSettings: TfrmZmqSettings
  Left = 0
  Top = 0
  ClientHeight = 202
  ClientWidth = 280
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  TextHeight = 13
  object lblPollingTimeoutMs: TLabel
    Left = 151
    Top = 11
    Width = 14
    Height = 13
    Caption = 'ms'
  end
  object lblPollingIntervalMs: TLabel
    Left = 151
    Top = 38
    Width = 14
    Height = 13
    Caption = 'ms'
  end
  object edtPollingTimeout: TLabeledEdit
    Left = 88
    Top = 8
    Width = 57
    Height = 21
    Alignment = taRightJustify
    EditLabel.Width = 82
    EditLabel.Height = 21
    EditLabel.Caption = 'Polling &timeout:'
    LabelPosition = lpLeft
    NumbersOnly = True
    TabOrder = 0
    Text = ''
    OnChange = edtPollingTimeoutChange
  end
  object edtPollingInterval: TLabeledEdit
    Left = 88
    Top = 35
    Width = 57
    Height = 21
    Alignment = taRightJustify
    EditLabel.Width = 80
    EditLabel.Height = 21
    EditLabel.Caption = 'Polling &interval:'
    LabelPosition = lpLeft
    NumbersOnly = True
    TabOrder = 1
    Text = ''
    OnChange = edtPollingIntervalChange
  end
end
