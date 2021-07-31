object frmLogViewerSettings: TfrmLogViewerSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 415
  ClientWidth = 668
  Color = clWhite
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 184
    Top = 0
    Width = 2
    Height = 384
    Color = clScrollBar
    ParentColor = False
    ResizeStyle = rsLine
    ExplicitLeft = 207
    ExplicitHeight = 373
  end
  object pnlConfigTree: TPanel
    Left = 0
    Top = 0
    Width = 184
    Height = 384
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 384
    Width = 668
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      668
      31)
    object shpLine: TShape
      Left = 0
      Top = 0
      Width = 668
      Height = 1
      Align = alTop
      Pen.Color = clScrollBar
    end
    object btnClose: TButton
      Left = 515
      Top = 4
      Width = 150
      Height = 25
      Action = actClose
      Anchors = [akRight, akBottom]
      Images = imlMain
      TabOrder = 2
    end
    object btnClose1: TButton
      Left = 203
      Top = 4
      Width = 150
      Height = 25
      Action = actApply
      Anchors = [akRight, akBottom]
      Images = imlMain
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 359
      Top = 4
      Width = 150
      Height = 25
      Action = actCancel
      Anchors = [akRight, akBottom]
      Images = imlMain
      TabOrder = 1
    end
  end
  object pgcMain: TKPageControl
    Left = 186
    Top = 0
    Width = 482
    Height = 384
    ActivePageIndex = 10
    Align = alClient
    ParentBackground = False
    TabHeight = 0
    TabOrder = 2
    object tsWatches: TKTabSheet
      Caption = 'Watches'
    end
    object tsCallStack: TKTabSheet
      Caption = 'Callstack'
    end
    object tsComPort: TKTabSheet
      Caption = 'ComPort'
    end
    object tsZeroMQ: TKTabSheet
      Caption = 'ZeroMQ'
    end
    object tsWinODS: TKTabSheet
      Caption = 'WinODS'
    end
    object tsWinIPC: TKTabSheet
      Caption = 'WinIPC'
    end
    object tsDisplayValueSettings: TKTabSheet
      Caption = 'DisplayValueSettings'
    end
    object tsAdvanced: TKTabSheet
      Caption = 'Advanced'
      object seSettings: TSynEdit
        Left = 0
        Top = 0
        Width = 482
        Height = 384
        Align = alClient
        ActiveLineColor = clYellow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        TabOrder = 0
        CodeFolding.GutterShapeSize = 11
        CodeFolding.CollapsedLineColor = clGrayText
        CodeFolding.FolderBarLinesColor = clGrayText
        CodeFolding.IndentGuidesColor = clGray
        CodeFolding.IndentGuides = True
        CodeFolding.ShowCollapsedLine = False
        CodeFolding.ShowHintMark = True
        UseCodeFolding = False
        BorderStyle = bsNone
        Gutter.AutoSize = True
        Gutter.Color = cl3DLight
        Gutter.Font.Charset = ANSI_CHARSET
        Gutter.Font.Color = clSilver
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Consolas'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 0
        Gutter.RightOffset = 0
        Gutter.ShowLineNumbers = True
        Gutter.ShowModification = True
        Gutter.Width = 15
        Gutter.Gradient = True
        Gutter.GradientStartColor = clWhite
        Gutter.GradientEndColor = clWhite
        Highlighter = synJScript
        Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ReadOnly = True
        RightEdgeColor = cl3DLight
        TabWidth = 2
        WordWrap = True
        FontSmoothing = fsmClearType
      end
    end
    object tsViewSettings: TKTabSheet
      Caption = 'ViewSettings'
    end
    object tsLogLevels: TKTabSheet
      Caption = 'KTabSheet1'
    end
    object tsGeneralSettings: TKTabSheet
      Caption = 'KTabSheet1'
      object chkEmitLogMessages: TCheckBox
        Left = 17
        Top = 16
        Width = 128
        Height = 17
        Hint = 
          'Enables a remote logviewer instance to subscribe to '#13#10'log messag' +
          'es sent by the application. '#13#10'When enabled you can connect a log' +
          'viewer by subscribing to this '#13#10'ZeroMQ endpoint: tcp://localhost' +
          ':42134.'#13#10
        Caption = 'Emit log messages'
        TabOrder = 0
        OnClick = chkEmitLogMessagesClick
      end
      object chkDebugMode: TCheckBox
        Left = 17
        Top = 39
        Width = 80
        Height = 17
        Hint = 
          'In Debug mode, the application is able to capture log messages f' +
          'rom another LogViewer instance '#13#10'which is configured to emit log' +
          ' messages.'#13#10'In this special mode the option to emit log messages' +
          ' is disabled to avoid crosstalk with other instances.'
        Caption = 'Debug mode'
        TabOrder = 1
        OnClick = chkDebugModeClick
      end
    end
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 568
    Top = 80
    object actClose: TAction
      Caption = '&Close'
      ImageIndex = 0
      OnExecute = actCloseExecute
    end
    object actApply: TAction
      Caption = '&Apply'
      ImageIndex = 1
      OnExecute = actApplyExecute
    end
    object actCancel: TAction
      Caption = '&Cancel'
      ImageIndex = 2
      OnExecute = actCancelExecute
    end
  end
  object imlMain: TImageList
    ColorDepth = cd32Bit
    Left = 568
    Top = 32
    Bitmap = {
      494C010103000800040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000001001B09532ED308512ED100000010000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000C000012730000
      31BA00003ACC00003ACC00003ACC00003ACC00003ACC00003ACC00003ACC0000
      3ACC000031BA000012730000000C000000000000000000000000000000000000
      000000000000000503370C693BEE2A9368FC1F8456F704261590000000000000
      0000000000000000000000000000000000000000000000000000000000040101
      4BAC01014BAC0000000400000000000000000000000000000000000000040101
      4BAC01014BAC0000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000167306066DDD0F0F
      C2F91010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010
      D9FF0E0EC2F905056DDD00001673000000000000000000000000000000000000
      0000010F085C0D7040F53EA57DFF4DB492FF4CB08DFF0D7141F60001001D0000
      0000000000000000000000000000000000000000000000000004030355B61111
      A1F51111A1F5030355B600000004000000000000000000000004030355B61111
      A1F51111A1F5030355B600000004000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000042BA1414C1F91010
      D1FF1010D1FF1010B6FF1010D1FF1010D1FF1010D1FF1010D1FF1010B6FF1010
      D1FF1010D1FF0E0EBEF9000042BA000000000000000000000000000000020424
      148D157446F552B391FF51B795FF3FAF8BFF53B897FF35956CFA06351EA90000
      0000000000000000000000000000000000000000000005054DAC1616A4F53A3A
      C8FF3A3AC8FF1616A5F5050556B60000000400000004050556B61616A5F53B3B
      C8FF3A3AC8FF1616A5F505054DAC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000054CC1B1BCEFF1010
      C8FF1010B2FFDCDCDCFF1010B2FF1010C8FF1010C8FF1010B2FFEEEEEEFF1010
      B2FF1010C8FF1010C8FF000054CC00000000000000000000000C084125BB2181
      54F563BF9FFF52BA97FF58BC9BFF69C3A5FF4AB792FF66C0A2FF127444F60003
      022E000000000000000000000000000000000000000006064EAC1A1AA7F54444
      CDFF4040CCFF4545CDFF1A1AA7F5070757B6070757B61A1AA7F54545CDFF4141
      CCFF4444CDFF1A1AA7F506064EAC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000057CC2121C8FF1010
      BEFFD1D1D1FFD6D6D6FFDCDCDCFF1010ADFF1010ADFFEAEAEAFFEEEEEEFFEEEE
      EEFF1010BEFF1111BEFF000057CC0000000000000000073D23B5359268F872C7
      ABFF56BD9BFF6AC5A7FF5EB593FE55AF8DFC62C2A2FF60C1A1FF4DA884FD0846
      28C1000000000000000000000000000000000000000000000004090958B61F1F
      ABF54F4FD1FF4949D0FF4F4FD1FF2121A9F32121A9F34F4FD1FF4949D0FF4F4F
      D1FF1F1FABF5090958B600000004000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000005ACC3333C7FF1111
      B4FF1010B4FFD1D1D1FFD6D6D6FFDCDCDCFFE2E2E2FFE6E6E6FFEAEAEAFF1010
      B4FF1010B4FF1313B6FF00005ACC00000000000000000B5932D960B695FF72CA
      ADFF7DCEB3FF4DA581FB0D673BEB117444F77BCAAFFF55BF9CFF7ECDB3FF1C7A
      4DF6000704420000000000000000000000000000000000000000000000040A0A
      59B62525ADF55858D6FF5151D4FF5A5AD7FF5A5AD7FF5252D4FF5858D6FF2525
      ADF50A0A59B60000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000005DCC4545CEFF2525
      B5FF1313ABFF1010AAFFD1D1D1FFD6D6D6FFDCDCDCFFE2E2E2FF1010AAFF1010
      AAFF1010AAFF1717B0FF00005DCC000000000000000000040230137444F677C5
      A8FF378E67F50A4D2DCB00000016052D1A9C479E79F978CDB2FF6AC8A9FF6ABD
      9DFF0A5732D70000000100000000000000000000000000000000000000000000
      00040C0C5BB62B2BAEF36464DBFF6262DBFF6262DBFF6464DBFF2B2BAEF30C0C
      5BB6000000040000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000005FCC4949D2FF3232
      BBFF2D2DB8FF12129FFFCECECEFFD1D1D1FFD6D6D6FFDCDCDCFF10109EFF1010
      A1FF1010A1FF1C1CACFF00005FCC000000000000000000000000010D07570D66
      3BE8052B18970000000400000000000000160F7042F58CD2B9FF61C7A6FF92D8
      C1FF2C8559F5010E075800000000000000000000000000000000000000000000
      00040E0E5DB63030B2F36D6DDFFF6969DFFF6969DFFF6D6DDFFF3131B2F30E0E
      5DB6000000040000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000062CC4F4FD8FF3636
      BFFF2222ABFFFFFFFFFFF7F7F7FFE8E8E8FFDEDEDEFFDBDBDBFFDDDDDDFF1010
      9BFF1515A0FF2A2AB5FF000062CC000000000000000000000000000000000000
      000000000000000000000000000000000000031F1182459972F790DAC1FF71CF
      B0FF86CDB3FF0C653AE800000006000000000000000000000000000000040F0F
      5DB63333B5F57575E3FF6969E1FF7676E4FF7676E4FF6868E1FF7575E3FF3333
      B5F50F0F5DB60000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000064CC5959E2FF4141
      CAFFFFFFFFFFFFFFFFFFFFFFFFFF4141CAFF4141CAFFFFFFFFFFFFFFFFFFFFFF
      FFFF4141CAFF4D4DD6FF000064CC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000B0E6B3EEF93D5BDFF70D0
      B1FF9CDFC9FF3E936BF602170C7000000000000000000000000411115FB63737
      B9F67D7DE6FF7070E4FF7E7EE6FF3A3AB7F43A3AB7F47E7EE6FF7171E4FF7D7D
      E6FF3737B8F511115FB600000004000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000068CC6161EAFF4E4E
      D7FF4E4ED7FFFFFFFFFF4E4ED7FF4E4ED7FF4E4ED7FF4E4ED7FFFFFFFFFF4E4E
      D7FF4E4ED7FF5959E2FF000068CC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002130A673A8F68F59FE1
      CBFF74D4B4FF97D8C1FF0E6E3FF20000000E00000000111155AC3D3DBBF68686
      E7FF7777E5FF8888E7FF3E3EBBF613135FB613135FB63E3EBBF68888E7FF7878
      E5FF8787E7FF3D3DBBF6121255AC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000057BA5B5BE0F95A5A
      E3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5A
      E3FF5A5AE3FF5656DCF9000057BA000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000030C5F37E18BD0
      B6FF79D5B7FF99DFC8FF4EA07CF80423148900000000131356AC4242BDF68F8F
      E9FF9090E9FF4343BDF6151561B60000000400000004151561B64343BDF69090
      E9FF8F8FE9FF4242BDF6131356AC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000022731F1F95DD5E5E
      E3F96C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6B6B
      F4FF5D5DE2F91F1F94DD00002273000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010A064E2C85
      5BF4A3E2CDFF91DCC3FF98D7C0FF0D653AE60000000000000004161662B64646
      BEF64747BEF6161662B600000004000000000000000000000004171762B64848
      BFF64848BEF6171762B600000004000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000C000022730000
      5ABA00006DCC00006DCC00006DCC00006DCC00006DCC00006DCC00006DCC0000
      6DCC00005ABA000022730000000C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000B50
      2ECE77C2A3FE71BF9FFE19784AF6031E117E0000000000000000000000041616
      58AC161658AC0000000400000000000000000000000000000000000000041616
      58AC161658AC0000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000004
      02310D653AE8094829C30003022E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object synJScript: TSynJScriptSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMedGray
    CommentAttri.Style = []
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clRed
    NumberAttri.Style = [fsBold]
    StringAttri.Foreground = clGreen
    StringAttri.Style = [fsBold]
    SymbolAttri.Foreground = clMaroon
    SymbolAttri.Style = [fsBold]
    Left = 495
    Top = 36
  end
end
