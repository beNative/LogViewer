object frmWatchesView: TfrmWatchesView
  Left = 0
  Top = 0
  ClientHeight = 378
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgcWatches: TPageControl
    Left = 0
    Top = 0
    Width = 401
    Height = 378
    ActivePage = tsLatest
    Align = alClient
    TabOrder = 0
    OnChanging = pgcWatchesChanging
    object tsLatest: TTabSheet
      Caption = 'Latest'
    end
    object tsSelected: TTabSheet
      Caption = 'Selected'
    end
    object tsHistory: TTabSheet
      Caption = 'History'
      object cbxWatchHistory: TComboBox
        Left = 0
        Top = 0
        Width = 393
        Height = 21
        Align = alTop
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbxWatchHistoryChange
      end
    end
  end
end
