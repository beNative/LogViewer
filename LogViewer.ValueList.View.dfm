object frmValueList: TfrmValueList
  Left = 0
  Top = 0
  ClientHeight = 569
  ClientWidth = 398
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TOMultiPanel
    Left = 0
    Top = 0
    Width = 398
    Height = 569
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
    SplitterSize = 6
    Align = alClient
    AutoSize = True
    BevelEdges = []
    TabOrder = 0
    ExplicitLeft = 112
    ExplicitTop = 288
    ExplicitWidth = 185
    ExplicitHeight = 41
    object pnlBottom: TPanel
      Left = 0
      Top = 290
      Width = 398
      Height = 279
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 398
      Height = 284
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
end
