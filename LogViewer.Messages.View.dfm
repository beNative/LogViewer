object frmMessagesView: TfrmMessagesView
  Left = 0
  Top = 0
  ClientHeight = 806
  ClientWidth = 1392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 954
    Top = 0
    Width = 8
    Height = 806
    Align = alRight
    ExplicitLeft = 1005
    ExplicitTop = -8
    ExplicitHeight = 814
  end
  object splLeftVertical: TSplitter
    Left = 353
    Top = 0
    Width = 9
    Height = 806
    ExplicitLeft = 286
    ExplicitTop = -8
    ExplicitHeight = 814
  end
  object pnlRight: TPanel
    Left = 962
    Top = 0
    Width = 430
    Height = 806
    Align = alRight
    BevelOuter = bvNone
    Caption = 'pnlRight'
    TabOrder = 0
    object pgcMessageDetails: TPageControl
      Left = 0
      Top = 31
      Width = 430
      Height = 775
      ActivePage = tsTextViewer
      Align = alClient
      TabOrder = 0
      object tsTextViewer: TTabSheet
        Caption = 'Text'
      end
      object tsImageViewer: TTabSheet
        Caption = 'Image'
        object imgViewer: TImage
          Left = 0
          Top = 0
          Width = 422
          Height = 747
          Align = alClient
          ExplicitHeight = 561
        end
      end
      object tsInspector: TTabSheet
        Caption = 'Inspector'
      end
      object tsHexEditor: TTabSheet
        Caption = 'Hex edtor'
      end
    end
    object pnlMessageContent: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 424
      Height = 25
      Align = alTop
      BevelKind = bkFlat
      BevelOuter = bvNone
      Caption = 'Message content'
      Color = clAppWorkSpace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
    end
  end
  object pnlMessages: TPanel
    Left = 362
    Top = 0
    Width = 592
    Height = 806
    Align = alClient
    BevelOuter = bvNone
    BevelWidth = 2
    ParentBackground = False
    TabOrder = 1
    object pnlFilter: TPanel
      Left = 0
      Top = 0
      Width = 592
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        592
        30)
      object btnExpandAll: TSpeedButton
        Left = 3
        Top = 6
        Width = 23
        Height = 22
        Flat = True
      end
      object btnCollapseAll: TSpeedButton
        Left = 24
        Top = 6
        Width = 23
        Height = 22
        Flat = True
      end
      object edtMessageFilter: TLabeledEdit
        Left = 126
        Top = 8
        Width = 261
        Height = 21
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Message filter:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        LabelPosition = lpLeft
        ParentFont = False
        TabOrder = 0
        TextHint = 'filter log messages'
      end
      object btnFilterMessages: TButton
        Left = 480
        Top = 6
        Width = 102
        Height = 25
        Anchors = [akTop, akRight]
        Default = True
        TabOrder = 1
      end
      object chkAutoFilter: TCheckBox
        Left = 402
        Top = 10
        Width = 71
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Autofilter'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 806
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object pnlCallStackWatch: TPanel
      Left = 0
      Top = 0
      Width = 353
      Height = 806
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object splLeftHorizontal: TSplitter
        Left = 0
        Top = 378
        Width = 353
        Height = 8
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 392
        ExplicitWidth = 286
      end
      object pnlLeftBottom: TPanel
        Left = 0
        Top = 386
        Width = 353
        Height = 420
        Align = alBottom
        BevelOuter = bvNone
        Caption = 'pnlLeftBottom'
        ShowCaption = False
        TabOrder = 1
        object pnlWatches: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 347
          Height = 25
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 'Watches'
          Color = clAppWorkSpace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
        end
      end
      object pnlCallStack: TPanel
        Left = 0
        Top = 0
        Width = 353
        Height = 378
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnlCallStackTitle: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 347
          Height = 25
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 'Callstack'
          Color = clAppWorkSpace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
        end
      end
    end
  end
end
