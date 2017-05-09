object frmAddMessageView: TfrmAddMessageView
  Left = 0
  Top = 0
  ClientHeight = 353
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgcReceiverSettings: TPageControl
    Left = 0
    Top = 40
    Width = 504
    Height = 313
    ActivePage = tsSerial
    Align = alBottom
    TabOrder = 0
    object tsWinIPC: TTabSheet
      Caption = 'WinIPC'
      ExplicitLeft = 8
      object pnlWinIPC: TPanel
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 160
        ExplicitTop = 120
        ExplicitWidth = 185
        ExplicitHeight = 41
      end
    end
    object tsZeroMQ: TTabSheet
      Caption = 'ZeroMQ'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object pnlZeroMQ: TPanel
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 168
        ExplicitTop = 128
        ExplicitWidth = 185
        ExplicitHeight = 41
      end
    end
    object tsSerial: TTabSheet
      Caption = 'Serial'
      ImageIndex = 2
      ExplicitWidth = 281
      ExplicitHeight = 165
      object pnlSerial: TPanel
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 176
        ExplicitTop = 136
        ExplicitWidth = 185
        ExplicitHeight = 41
      end
    end
  end
end
