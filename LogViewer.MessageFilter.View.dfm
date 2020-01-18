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
  ParentFont = True
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
      TabOrder = 0
      object tsClientSide: TKTabSheet
        Caption = 'Client side'
        ExplicitTop = 0
        ExplicitWidth = 167
        ExplicitHeight = 0
      end
      object tsSourceSide: TKTabSheet
        Caption = 'Source side'
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
  end
end
