object frmRawDataView: TfrmRawDataView
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 450
  ClientWidth = 567
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object heMain: TKHexEditor
    Left = 0
    Top = 0
    Width = 567
    Height = 450
    Align = alClient
    BorderStyle = bsNone
    Colors.HorzLines = clScrollBar
    Colors.Separators = clScrollBar
    Colors.VertLines = clScrollBar
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpFixed
    Font.Style = []
    LineHeightPercent = 100
    Options = [eoDisableCaret, eoGroupUndo, eoScrollWindow, eoShowFormatting]
    ReadOnly = True
    ScrollSpeed = 200
    TabOrder = 0
  end
end
