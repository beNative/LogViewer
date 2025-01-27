object frmWatchSettings: TfrmWatchSettings
  Left = 190
  Top = 190
  ClientHeight = 209
  ClientWidth = 366
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object chkOnlyTrackChanges: TCheckBox
    Left = 16
    Top = 8
    Width = 153
    Height = 17
    Caption = 'Only track changes.'
    TabOrder = 0
    OnClick = chkOnlyTrackChangesClick
  end
  object chkSyncWithSelectedMessage: TCheckBox
    Left = 16
    Top = 31
    Width = 281
    Height = 17
    Caption = 'Synchronize watch value with selected log message.'
    TabOrder = 1
    OnClick = chkSyncWithSelectedMessageClick
  end
  object chkShowWatchHistory: TCheckBox
    Left = 16
    Top = 54
    Width = 129
    Height = 17
    Caption = 'Show watch history.'
    TabOrder = 2
    OnClick = chkShowWatchHistoryClick
  end
  object chkHideColumnHeaders: TCheckBox
    Left = 16
    Top = 77
    Width = 281
    Height = 17
    Caption = 'Hide column headers'
    TabOrder = 3
    OnClick = chkHideColumnHeadersClick
  end
end
