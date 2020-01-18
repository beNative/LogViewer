object frmWatchesView: TfrmWatchesView
  Left = 0
  Top = 0
  ClientHeight = 739
  ClientWidth = 444
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TOMultiPanel
    Left = 0
    Top = 0
    Width = 444
    Height = 739
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = pnlWatches
        Position = 0.500000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = pnlWatchHistory
        Position = 1.000000000000000000
        Visible = False
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    SplitterColor = clScrollBar
    SplitterHoverColor = clScrollBar
    Align = alClient
    BevelEdges = []
    TabOrder = 0
    object pnlWatches: TPanel
      Left = 0
      Top = 0
      Width = 444
      Height = 370
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlWatchHistory: TPanel
      Left = 0
      Top = 373
      Width = 444
      Height = 366
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
    end
  end
end
