object frmRawDataView: TfrmRawDataView
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 450
  ClientWidth = 567
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
    PopupMenu = ppmMain
    ReadOnly = True
    ScrollSpeed = 200
    TabOrder = 0
  end
  object ppmMain: TPopupMenu
    Left = 432
    Top = 179
    object mniCopy: TMenuItem
      Action = actCopy
      ShortCut = 16451
    end
  end
  object aclMain: TActionList
    Left = 512
    Top = 176
    object actCopy: TAction
      Caption = 'Copy'
      Hint = 'Copy image to clipboard'
      OnExecute = actCopyExecute
    end
  end
end
