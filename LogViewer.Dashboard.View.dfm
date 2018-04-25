object frmDashboard: TfrmDashboard
  Left = 0
  Top = 0
  ClientHeight = 557
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object chkWinODSEnabled: TCheckBox
    Left = 28
    Top = 39
    Width = 106
    Height = 17
    Caption = 'WinODS enabled'
    TabOrder = 0
  end
  object chkWinIPCEnabled: TCheckBox
    Left = 28
    Top = 8
    Width = 97
    Height = 17
    Caption = 'WinIPC enabled'
    TabOrder = 1
  end
  object chkZeroMQEnabled: TCheckBox
    Left = 28
    Top = 70
    Width = 154
    Height = 17
    Caption = 'ZeroMQ subscriber enabled'
    TabOrder = 2
  end
  object pnlLogChannels: TPanel
    Left = 0
    Top = 128
    Width = 460
    Height = 429
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
  end
  object btnAddWinIPCLogViewer: TButton
    Left = 188
    Top = 4
    Width = 75
    Height = 25
    Action = actAddWinIPCLogViewer
    TabOrder = 4
  end
  object btnAddWinODSLogViewer: TButton
    Left = 188
    Top = 35
    Width = 75
    Height = 25
    Action = actAddWinODSLogViewer
    TabOrder = 5
  end
  object btnAddZeroMQLogViewer: TButton
    Left = 188
    Top = 66
    Width = 75
    Height = 25
    Action = actAddZeroMQLogViewer
    TabOrder = 6
  end
  object chkComPortEnabled: TCheckBox
    Left = 28
    Top = 101
    Width = 106
    Height = 17
    Caption = 'COM port enabled'
    TabOrder = 7
  end
  object btnAddComPortLogViewer: TButton
    Left = 188
    Top = 97
    Width = 75
    Height = 25
    Action = actAddComPortLogViewer
    TabOrder = 8
  end
  object aclMain: TActionList
    Left = 352
    Top = 40
    object actAddWinIPCLogViewer: TAction
      Caption = 'Add viewer'
      OnExecute = actAddWinIPCLogViewerExecute
    end
    object actAddWinODSLogViewer: TAction
      Caption = 'Add viewer'
      OnExecute = actAddWinODSLogViewerExecute
    end
    object actAddZeroMQLogViewer: TAction
      Caption = 'Add viewer'
      OnExecute = actAddZeroMQLogViewerExecute
    end
    object actAddComPortLogViewer: TAction
      Caption = 'Add viewer'
      OnExecute = actAddComPortLogViewerExecute
    end
  end
end
