object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 30
  ClientWidth = 306
  Color = clWindow
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ScreenSnap = True
  Visible = True
  TextHeight = 13
  object lblIPC: TLabel
    Left = 4
    Top = 8
    Width = 19
    Height = 13
    Caption = 'IPC:'
  end
  object shpStat: TShape
    Left = 276
    Top = 6
    Width = 21
    Height = 19
    Brush.Color = clBackground
  end
  object edtIPCPort: TLabeledEdit
    Left = 140
    Top = 5
    Width = 68
    Height = 21
    Alignment = taCenter
    EditLabel.Width = 24
    EditLabel.Height = 21
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
    Left = 213
    Top = 7
    Width = 58
    Height = 17
    Hint = 'Auto assign TCP port for IPC messages'
    Caption = 'Auto'
    TabOrder = 1
    OnClick = chkIPCAutoPortClick
  end
  object tsIPC: TToggleSwitch
    Left = 27
    Top = 5
    Width = 81
    Height = 20
    TabOrder = 2
  end
end
