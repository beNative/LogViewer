object frmZeroMQSettings: TfrmZeroMQSettings
  Left = 0
  Top = 0
  ClientHeight = 202
  ClientWidth = 278
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblPollingTimeoutMs: TLabel
    Left = 151
    Top = 11
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object lblPollingIntervalMs: TLabel
    Left = 151
    Top = 38
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object edtPollingTimeout: TLabeledEdit
    Left = 88
    Top = 8
    Width = 57
    Height = 21
    Alignment = taRightJustify
    EditLabel.Width = 73
    EditLabel.Height = 13
    EditLabel.Caption = 'Polling &timeout:'
    LabelPosition = lpLeft
    NumbersOnly = True
    TabOrder = 0
    OnChange = edtPollingTimeoutChange
  end
  object edtPollingInterval: TLabeledEdit
    Left = 88
    Top = 35
    Width = 57
    Height = 21
    Alignment = taRightJustify
    EditLabel.Width = 73
    EditLabel.Height = 13
    EditLabel.Caption = 'Polling &interval:'
    LabelPosition = lpLeft
    NumbersOnly = True
    TabOrder = 1
    OnChange = edtPollingIntervalChange
  end
end
