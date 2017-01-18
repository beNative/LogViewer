object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Logviewer'
  ClientHeight = 939
  ClientWidth = 1434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1428
    Height = 933
    ActivePage = tsIPC
    Align = alClient
    TabOrder = 0
    object tsIPC: TTabSheet
      Caption = 'IPC'
    end
    object tsODS: TTabSheet
      Caption = 'ODS'
      ImageIndex = 1
    end
    object tsZeroMQ: TTabSheet
      Caption = 'ZeroMQ'
      ImageIndex = 2
    end
  end
end
