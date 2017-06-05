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
    ActivePage = tsWinIPC
    Align = alBottom
    TabOrder = 0
    object tsWinIPC: TTabSheet
      Caption = 'WinIPC'
      object pnlWinIPC: TPanel
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object tsZeroMQ: TTabSheet
      Caption = 'ZeroMQ'
      ImageIndex = 1
      object pnlZeroMQ: TPanel
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        TabOrder = 0
      end
    end
    object tsSerial: TTabSheet
      Caption = 'Serial'
      ImageIndex = 2
      object pnlSerial: TPanel
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object tsODS: TTabSheet
      Caption = 'OutputDebugString API'
      ImageIndex = 3
    end
  end
end
