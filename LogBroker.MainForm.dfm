object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 30
  ClientWidth = 272
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ScreenSnap = True
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblIPC: TLabel
    Left = 4
    Top = 8
    Width = 19
    Height = 13
    Caption = 'IPC:'
  end
  object shpStat: TShape
    Left = 243
    Top = 7
    Width = 21
    Height = 16
    Brush.Color = clBackground
  end
  object edtIPCPort: TLabeledEdit
    Left = 115
    Top = 4
    Width = 73
    Height = 21
    Alignment = taCenter
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'Port:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    LabelPosition = lpLeft
    NumbersOnly = True
    ParentFont = False
    TabOrder = 0
    Text = '5555'
  end
  object chkIPCAutoPort: TCheckBox
    Left = 194
    Top = 7
    Width = 41
    Height = 17
    Hint = 'Auto assign TCP port for IPC messages'
    Caption = 'Auto'
    TabOrder = 1
    OnClick = chkIPCAutoPortClick
  end
  object tsIPC: TToggleSwitch
    Left = 27
    Top = 4
    Width = 54
    Height = 20
    TabOrder = 2
  end
end
