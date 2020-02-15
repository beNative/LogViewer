object frmWinipcSettings: TfrmWinipcSettings
  Left = 0
  Top = 0
  ClientHeight = 84
  ClientWidth = 324
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblWindowHandleName: TLabel
    Left = 8
    Top = 8
    Width = 106
    Height = 13
    Caption = 'Window handle name:'
    FocusControl = edtWindowHandleName
  end
  object lblPollingIntervalMs: TLabel
    Left = 183
    Top = 36
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object edtWindowHandleName: TEdit
    Left = 120
    Top = 6
    Width = 193
    Height = 21
    TabOrder = 0
  end
  object edtPollingInterval: TLabeledEdit
    Left = 120
    Top = 33
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
