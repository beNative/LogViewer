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
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 145
    Top = 0
    Width = 2
    Height = 378
    Color = clScrollBar
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitHeight = 339
  end
  object pnlRight: TPanel
    Left = 147
    Top = 0
    Width = 342
    Height = 378
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
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
