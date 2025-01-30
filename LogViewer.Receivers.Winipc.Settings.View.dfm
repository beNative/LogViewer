object frmWinipcSettings: TfrmWinipcSettings
  Left = 0
  Top = 0
  ClientHeight = 84
  ClientWidth = 326
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  TextHeight = 13
  object lblWindowHandleName: TLabel
    Left = 8
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Window handle name:'
    FocusControl = edtWindowHandleName
  end
  object lblPollingIntervalMs: TLabel
    Left = 183
    Top = 36
    Width = 14
    Height = 13
    Caption = 'ms'
  end
  object edtWindowHandleName: TEdit
    Left = 131
    Top = 6
    Width = 182
    Height = 21
    TabOrder = 0
  end
  object edtPollingInterval: TLabeledEdit
    Left = 131
    Top = 33
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
