object frmDisplayValuesSettings: TfrmDisplayValuesSettings
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 378
  ClientWidth = 489
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object splVertical: TSplitter
    Left = 145
    Top = 0
    Width = 4
    Height = 378
    Color = clScrollBar
    ParentColor = False
    ResizeStyle = rsUpdate
  end
  object pnlRight: TPanel
    Left = 149
    Top = 0
    Width = 340
    Height = 378
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 147
    ExplicitWidth = 342
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 378
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
end
