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
    Left = 38
    Top = 34
    Width = 106
    Height = 17
    Caption = 'WinODS enabled'
    TabOrder = 1
    OnClick = chkWinODSEnabledClick
  end
  object chkWinIPCEnabled: TCheckBox
    Left = 38
    Top = 11
    Width = 97
    Height = 17
    Caption = 'WinIPC enabled'
    TabOrder = 0
    OnClick = chkWinIPCEnabledClick
  end
  object chkZeroMQEnabled: TCheckBox
    Left = 37
    Top = 59
    Width = 154
    Height = 17
    Caption = 'ZeroMQ subscriber enabled'
    TabOrder = 2
    OnClick = chkZeroMQEnabledClick
  end
  object pnlLogChannels: TPanel
    Left = 0
    Top = 128
    Width = 460
    Height = 429
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 4
  end
  object chkComPortEnabled: TCheckBox
    Left = 38
    Top = 83
    Width = 106
    Height = 17
    Caption = 'COM port enabled'
    TabOrder = 3
    OnClick = chkComPortEnabledClick
  end
  object bgMain: TButtonGroup
    Left = 8
    Top = 8
    Width = 26
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    ButtonOptions = [gboFullSize, gboShowCaptions]
    Enabled = False
    Images = dmManager.imlMain
    Items = <
      item
        ImageIndex = 28
      end
      item
        ImageIndex = 28
      end
      item
        ImageIndex = 6
      end
      item
        ImageIndex = 24
      end>
    TabOrder = 5
  end
  object aclMain: TActionList
    Left = 352
    Top = 40
  end
end
