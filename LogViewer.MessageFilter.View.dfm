object frmMessageFilter: TfrmMessageFilter
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Message filter'
  ClientHeight = 627
  ClientWidth = 215
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PopupMode = pmAuto
  ScreenSnap = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMessageFilter: TPanel
    Left = 0
    Top = 0
    Width = 215
    Height = 627
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object pgcMain: TKPageControl
      Left = 0
      Top = 0
      Width = 215
      Height = 627
      ActivePageIndex = 0
      Align = alClient
      ParentBackground = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object tsClientSide: TKTabSheet
        Caption = 'Client side'
      end
      object tsSourceSide: TKTabSheet
        Caption = 'Source side'
      end
    end
  end
end
