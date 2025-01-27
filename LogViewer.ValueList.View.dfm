object frmValueListView: TfrmValueListView
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 854
  ClientWidth = 606
  Color = clWhite
  DoubleBuffered = True
  ParentFont = True
  PixelsPerInch = 144
  TextHeight = 25
  object pnlMain: TOMultiPanel
    Left = 0
    Top = 0
    Width = 606
    Height = 854
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = pnlTop
        Position = 0.500000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = pnlBottom
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    SplitterColor = clScrollBar
    SplitterHoverColor = clScrollBar
    Align = alClient
    AutoSize = True
    BevelEdges = []
    TabOrder = 0
    object pnlBottom: TPanel
      Left = 0
      Top = 430
      Width = 606
      Height = 424
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 606
      Height = 427
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
end
