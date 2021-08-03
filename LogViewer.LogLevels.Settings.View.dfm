object frmLogLevelSettings: TfrmLogLevelSettings
  Left = 0
  Top = 0
  ClientHeight = 411
  ClientWidth = 516
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
    Left = 281
    Top = 0
    Width = 4
    Height = 411
    Color = clScrollBar
    ParentColor = False
    ResizeStyle = rsLine
  end
  object pnlRight: TPanel
    Left = 285
    Top = 0
    Width = 231
    Height = 411
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 411
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
end
