object frmMessageDataView: TfrmMessageDataView
  Left = 0
  Top = 0
  ClientHeight = 287
  ClientWidth = 601
  Color = clBtnFace
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
    Width = 601
    Height = 287
    PanelCollection = <
      item
        Control = pnlLeft
        Position = 0.500000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = pnlRight
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    DesignSize = (
      601
      287)
    object pnlRight: TPanel
      Left = 303
      Top = 0
      Width = 298
      Height = 287
      Anchors = []
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 300
      Height = 287
      Anchors = []
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
end
