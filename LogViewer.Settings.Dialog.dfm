object frmLogViewerSettings: TfrmLogViewerSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 373
  ClientWidth = 799
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlConfigTree: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 332
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pgcMain: TPageControl
    Left = 201
    Top = 0
    Width = 598
    Height = 332
    ActivePage = tsComport
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    ExplicitWidth = 583
    object tsWatches: TTabSheet
      Caption = 'Watches'
      ExplicitWidth = 856
    end
    object tsCallstack: TTabSheet
      Caption = 'Callstack'
      ImageIndex = 1
      ExplicitWidth = 575
    end
    object tsWinIPC: TTabSheet
      Caption = 'WinIPC'
      ImageIndex = 2
      ExplicitWidth = 575
    end
    object tsWinODS: TTabSheet
      Caption = 'OutputDebugString API'
      ImageIndex = 3
      ExplicitWidth = 575
    end
    object tsComport: TTabSheet
      Caption = 'Serial port'
      ImageIndex = 4
      ExplicitWidth = 575
    end
    object tsZeroMQ: TTabSheet
      Caption = 'ZeroMQ'
      ImageIndex = 5
      ExplicitWidth = 575
    end
    object tsDisplayValuesSettings: TTabSheet
      Caption = 'DisplayValuesSettings'
      ImageIndex = 6
      ExplicitWidth = 575
    end
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 7
      ExplicitWidth = 856
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 332
    Width = 799
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 784
    object btnClose: TButton
      Left = 660
      Top = 6
      Width = 120
      Height = 25
      Action = actClose
      TabOrder = 0
    end
  end
  object aclMain: TActionList
    Left = 328
    Top = 192
    object actClose: TAction
      Caption = '&Close'
      OnExecute = actCloseExecute
    end
  end
end
