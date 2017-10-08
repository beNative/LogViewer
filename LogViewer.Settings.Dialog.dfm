object frmLogViewerSettings: TfrmLogViewerSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 373
  ClientWidth = 665
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
    object tvConfig: TTreeView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 195
      Height = 326
      Align = alClient
      AutoExpand = True
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnChange = tvConfigChange
      Items.NodeData = {
        03020000003C0000000000000000000000FFFFFFFFFFFFFFFF01000000000000
        0002000000010F5600690065007700650072002000730065007400740069006E
        00670073002C0000000000000000000000FFFFFFFFFFFFFFFF01000000000000
        0000000000010757006100740063006800650073003000000000000000000000
        00FFFFFFFFFFFFFFFF0000000000000000000000000109430061006C006C0073
        007400610063006B0040000000000000000000000000000000FFFFFFFF000000
        00000000000400000001114300680061006E006E0065006C0020007200650063
        006500690076006500720073002A0000000100000000000000FFFFFFFFFFFFFF
        FF0000000000000000000000000106570069006E004900500043004800000000
        00000000000000FFFFFFFFFFFFFFFF00000000000000000000000001154F0075
        0074007000750074004400650062007500670053007400720069006E00670020
        00410050004900340000000100000000000000FFFFFFFFFFFFFFFF0000000000
        00000000000000010B530065007200690061006C00200070006F00720074002A
        0000000200000000000000FFFFFFFFFFFFFFFF00000000000000000000000001
        065A00650072006F004D005100}
      ExplicitWidth = 198
    end
  end
  object pgcMain: TPageControl
    Left = 201
    Top = 0
    Width = 464
    Height = 332
    ActivePage = tsComport
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    ExplicitLeft = 249
    ExplicitWidth = 416
    object tsWatches: TTabSheet
      Caption = 'Watches'
      ExplicitWidth = 408
    end
    object tsCallstack: TTabSheet
      Caption = 'Callstack'
      ImageIndex = 1
      ExplicitWidth = 408
    end
    object tsWinIPC: TTabSheet
      Caption = 'WinIPC'
      ImageIndex = 2
      ExplicitWidth = 408
    end
    object tsWinODS: TTabSheet
      Caption = 'OutputDebugString API'
      ImageIndex = 3
      ExplicitWidth = 408
    end
    object tsComport: TTabSheet
      Caption = 'Serial port'
      ImageIndex = 4
      ExplicitWidth = 408
    end
    object tsZeroMQ: TTabSheet
      Caption = 'ZeroMQ'
      ImageIndex = 5
      ExplicitWidth = 473
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 332
    Width = 665
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnClose: TButton
      Left = 541
      Top = 6
      Width = 120
      Height = 25
      Action = actClose
      TabOrder = 0
    end
    object btn2: TButton
      Left = 415
      Top = 6
      Width = 120
      Height = 25
      Caption = 'btn2'
      TabOrder = 1
    end
  end
  object aclMain: TActionList
    Left = 328
    Top = 192
    object actClose: TAction
      Caption = 'Close'
      OnExecute = actCloseExecute
    end
  end
end
