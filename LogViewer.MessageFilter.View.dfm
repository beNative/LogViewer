object frmMessageFilter: TfrmMessageFilter
  Left = 76
  Top = 76
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Message filter'
  ClientHeight = 627
  ClientWidth = 233
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  PopupMode = pmAuto
  ScreenSnap = True
  TextHeight = 13
  object pnlMessageFilter: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 627
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object pgcMain: TKPageControl
      Left = 0
      Top = 0
      Width = 233
      Height = 627
      ActivePageIndex = 0
      Align = alClient
      ParentBackground = False
      TabOrder = 0
      object tsClientSide: TKTabSheet
        Caption = 'Client side'
        ExplicitTop = 16
        ExplicitHeight = 611
      end
      object tsSourceSide: TKTabSheet
        Caption = 'Source side'
        ExplicitTop = 16
        ExplicitHeight = 611
      end
    end
  end
end
