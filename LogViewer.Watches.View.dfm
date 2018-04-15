object frmWatchesView: TfrmWatchesView
  Left = 0
  Top = 0
  ClientHeight = 581
  ClientWidth = 402
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splHorizontal: TSplitter
    Left = 0
    Top = 318
    Width = 402
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    Color = clBtnFace
    ParentColor = False
    ExplicitTop = 320
  end
  object pnlWatches: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 318
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 314
  end
  object pnlWatchHistory: TPanel
    Left = 0
    Top = 324
    Width = 402
    Height = 257
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
end
