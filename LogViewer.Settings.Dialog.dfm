object frmLogViewerSettings: TfrmLogViewerSettings
  Left = 0
  Top = 0
  Caption = 'c'
  ClientHeight = 373
  ClientWidth = 665
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlConfigTree: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 373
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object tvConfig: TTreeView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 243
      Height = 367
      Align = alClient
      AutoExpand = True
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      Items.NodeData = {
        03020000003C0000000000000000000000FFFFFFFFFFFFFFFF01000000000000
        0002000000010F5600690065007700650072002000730065007400740069006E
        00670073002C0000000000000000000000FFFFFFFFFFFFFFFF01000000000000
        0000000000010757006100740063006800650073003000000000000000000000
        00FFFFFFFFFFFFFFFF0000000000000000000000000109430061006C006C0073
        007400610063006B0040000000000000000000000000000000FFFFFFFF000000
        00000000000400000001114300680061006E006E0065006C0020007200650063
        006500690076006500720073002A0000000100000000000000FFFFFFFFFFFFFF
        FF0000000000000000000000000106570069006E004900500043004800000000
        00000000000000FFFFFFFFFFFFFFFF00000000000000000000000001154F0075
        0074007000750074004400650062007500670053007400720069006E00670020
        00410050004900340000000100000000000000FFFFFFFFFFFFFFFF0000000000
        00000000000000010B530065007200690061006C00200070006F00720074002A
        0000000200000000000000FFFFFFFFFFFFFFFF00000000000000000000000001
        065A00650072006F004D005100}
      ExplicitLeft = 4
      ExplicitTop = 2
    end
  end
  object pgcMain: TPageControl
    Left = 249
    Top = 0
    Width = 416
    Height = 373
    ActivePage = ts3
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object ts1: TTabSheet
      TabVisible = False
      ExplicitLeft = 6
      ExplicitTop = 7
      object ctgrypnlgrp1: TCategoryPanelGroup
        Left = 0
        Top = 0
        Height = 363
        VertScrollBar.Tracking = True
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = []
        TabOrder = 0
      end
    end
    object ts2: TTabSheet
      Caption = 'ts2'
      ImageIndex = 1
      TabVisible = False
      ExplicitTop = 27
      ExplicitHeight = 342
    end
    object ts3: TTabSheet
      Caption = 'ts3'
      ImageIndex = 2
      TabVisible = False
      ExplicitTop = 27
      ExplicitHeight = 342
    end
  end
end
