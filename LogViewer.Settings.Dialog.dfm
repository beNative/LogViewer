object frmLogViewerSettings: TfrmLogViewerSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 415
  ClientWidth = 670
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  PopupMode = pmAuto
  Position = poMainFormCenter
  ShowHint = True
  TextHeight = 13
  object splVertical: TSplitter
    Left = 184
    Top = 0
    Width = 4
    Height = 384
    Color = clScrollBar
    ParentColor = False
    ResizeStyle = rsLine
  end
  object pnlConfigTree: TPanel
    Left = 0
    Top = 0
    Width = 184
    Height = 384
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 367
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 384
    Width = 670
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 367
    ExplicitWidth = 664
    DesignSize = (
      670
      31)
    object shpLine: TShape
      Left = 0
      Top = 0
      Width = 670
      Height = 1
      Align = alTop
      Pen.Color = clScrollBar
      ExplicitWidth = 674
    end
    object btnClose: TButton
      Left = 515
      Top = 4
      Width = 150
      Height = 25
      Action = actClose
      Anchors = [akRight, akBottom]
      ImageName = 'Item1'
      Images = imlMain
      TabOrder = 2
      ExplicitLeft = 509
    end
    object btnClose1: TButton
      Left = 203
      Top = 4
      Width = 150
      Height = 25
      Action = actApply
      Anchors = [akRight, akBottom]
      ImageName = 'Item2'
      Images = imlMain
      TabOrder = 0
      ExplicitLeft = 197
    end
    object btnCancel: TButton
      Left = 359
      Top = 4
      Width = 150
      Height = 25
      Action = actCancel
      Anchors = [akRight, akBottom]
      ImageName = 'Item3'
      Images = imlMain
      TabOrder = 1
      ExplicitLeft = 353
    end
  end
  object pgcMain: TKPageControl
    Left = 188
    Top = 0
    Width = 482
    Height = 384
    ActivePageIndex = 10
    Align = alClient
    ParentBackground = False
    TabHeight = 0
    TabOrder = 2
    ExplicitLeft = 187
    ExplicitWidth = 477
    ExplicitHeight = 367
    object tsWatches: TKTabSheet
      Caption = 'Watches'
      ExplicitWidth = 483
    end
    object tsCallStack: TKTabSheet
      Caption = 'Callstack'
      ExplicitWidth = 483
    end
    object tsComPort: TKTabSheet
      Caption = 'ComPort'
      ExplicitWidth = 483
    end
    object tsZeroMQ: TKTabSheet
      Caption = 'ZeroMQ'
      ExplicitWidth = 483
    end
    object tsWinODS: TKTabSheet
      Caption = 'WinODS'
      ExplicitWidth = 483
    end
    object tsWinIPC: TKTabSheet
      Caption = 'WinIPC'
      ExplicitWidth = 483
    end
    object tsDisplayValueSettings: TKTabSheet
      Caption = 'DisplayValueSettings'
      ExplicitWidth = 483
    end
    object tsAdvanced: TKTabSheet
      Caption = 'Advanced'
      ExplicitWidth = 483
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
        BookMarkOptions.LeftMargin = 0
        BookMarkOptions.XOffset = 0
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
        ExplicitWidth = 483
      end
    end
    object tsViewSettings: TKTabSheet
      Caption = 'ViewSettings'
      ExplicitWidth = 483
    end
    object tsLogLevels: TKTabSheet
      Caption = 'KTabSheet1'
      ExplicitWidth = 483
    end
    object tsGeneralSettings: TKTabSheet
      Caption = 'KTabSheet1'
      ExplicitWidth = 477
      ExplicitHeight = 367
      object pnlGeneralSettings: TPanel
        Left = 0
        Top = 0
        Width = 482
        Height = 384
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlGeneralSettings'
        ParentBackground = False
        ShowCaption = False
        TabOrder = 0
        ExplicitLeft = 56
        ExplicitTop = 136
        ExplicitWidth = 249
        ExplicitHeight = 129
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
          Width = 128
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
  object imcMain: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002324944415478DAA593C16BD37014C75FD3345B6D414C697730C268A1
              0E0F95997A50D48D892DD84311F4E84947A520038BB03F41900A22140BF5E4D1
              83ECB043EBA844054513D1C37414567A589B469A946217B396105F7E2CA5169C
              C21E3C028FDFF7F3FBFEC8F7B92CCB82C394CB06F4FB7DFFAB6231D32B97932E
              4D630F12582CAB1D4D26CB573299A2DFEFEF13C0CB7C3EE75D5FCF9E894438FF
              F434F357B17D99610C3ED7EB3B462A55B896CBE509E0592251B9CA710BE6DE1E
              03FF5116450D2A8A22DCAA541204508AC7C5742CC6ABAA4A0ECCADADC1563AFD
              87687CE6F3F9A05CAB49B745314E004F11908A46F976BB0D67ABD591E8D3D212
              F94ECEF0ED20B45AD21D075040C0658EE36559864EA703D7B7B7C1300CE8F57A
              A0EB3A98A60914454195E7C1E3F14030188486D72B651DC013042C8642BCA228
              600E8720379B707A6303DC6E3711300C036FE27170EFBBF0A183D6CC8C74D701
              3C46C02596E5E54603744D030B6F3C290823B1DD1F62B1D13398A92968709CB4
              E2001E21E03C00AFD6EBF6BF821313629AA6497F8C4609804267DF6767A57B0E
              E0A1ED40D378BDDB85C098F82BDAB6EB62AD4600767D0987317E2E10C361E9BE
              0378808064BBCD0F7677E1071E3A258AF00DC5A17DCBF6EC02BA7B876267F63A
              12915647000CD2E2E6E6C2115D27416A621F9F08CFF8EC274D0FDECECF0BAB4E
              909E6394D552297B4E55B963C3E18169ECA2F87D20B013585E2EDC74A26C2FD3
              0B5CA62D5CA65FFF58262F2ED31C2ED38DF1653A4CFD0600AD20F061AD159000
              00000049454E44AE426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000028D4944415478DA63FCFFFF3F03B1E0E2E5F3868DABC2B68B7028DECE
              0BE8C9D6D1D6BBC4488A01094DB6E75C9D6C0CFEFCFDC5B0F9E0861FF672491D
              441BB063CF66BFF5575B97A507E5707FFBF58EE1EF6F7686C9CBFB9E1365C0DF
              BF7F99231BF5EE250627CA31B2BE6310E45260D87D62CF4FF64FAA13893260F1
              9AD9E917DEADEA8BF488E67AF9F92A032F8B3243F7E296CF4BCBAEC91034E0EB
              D7AFDC51ED5A4FF2A34A05BEFCBECB20C1AFC3B07AEFEAEFEA5C5E5529917913
              081AD03FB7B1E90DEBD952774B678EF75F1F3230FF136598B77EEECB15B5D7E4
              D8D8D87EC10DD8B06D55F8E213D513F5C43CD75767F4E5B2B0B0FC79F5EA9558
              F24483FBB911C55C6FBF5E659016346458B47DC9376FB5A2E440AFF015207D60
              032E5DB96050B3CAF758B44734E7C9CBA7BE7D78F9F75A47C66AEF29CBEBDB98
              449EC61A6B6AB37DFFFD89E1CB570686BD874EDC58527F468B9191F13FDC80C8
              06A39BB6E6FA6A1222420C6C2CDC0C976E5FF973EAD2850F7F197FF024FB2571
              BCFD7A8B419C4F9361F1B6E5DFB31D67F8D8593BED8379116C405E47D05626A1
              D7CE367A66EC1FBE3D61E0E1106178FBF1EBFF3F7F7EFE1312E062FEFFFF1FC3
              D397EFFF3DB8F5FDC8CCEA9DF6C8610436E0CB972F3C659322D77F65BB6FE568
              62C1F5E3F77B06162636062E764186B75F1E320870CA00437EEBF7B6F06D16A0
              E48B610008FCFBF78FA9736665F7A9172B331C4D8DB95858FE337CFBF18E8187
              5384E1D6A3E7BF993F2AADEF2E591C8E1E4B18D1B866F3D298F9278A671A6BAB
              7389F2F333FCFEF39F61EFA953DF67649E519795917D4CD000103873EEA479E3
              DAD0EDF232027C9FBFFDFAADC6E933B33AABA7005B3AC199909E3E7B2A5D362B
              70071303E3BFC9053BED0504043E60530700AE2E3C13DD21D146000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002644944415478DAA593CB4B626118C6BF73B440C74BC6388C7ACA4C09
              840A5C04B6104FD0A6169258E3051D1725EDFA63DA755B348A974971388BDA04
              792848685154208465E3A80DE3D0683AA70BCE71FC3E31956CD5597DBCBFF77D
              0ECF7BC1AAD52A78CB8735042A950AF7DBD25210BE6796976D5C2EB7D29AF81A
              4702082E2C5044A140020C0319B13836B3BE6E6A2421EEF1507DC522097F87F8
              DA1AE248E0EBE262B82F9B9D564F4EF2A0C0E5EEEE7DA6B73766DED8304181E8
              FC3C45DCDE9288D7F221FFA1506C7F5A59994502218F274CA4D3D36A92E4B1C5
              22C0251270B9B7779F914A635080C8E749F5C4048F2D14002E16D79952B96D5D
              5D9D7DB61071BB29792E476A0C061E5B2A015C2402C9C3C3BF5040333EFE0EC5
              0402903C38607232196DD9DC6C5A68F8DC72B92859366BD4E8F57CB65C061CA1
              10B17F777700AFBD93F13873A350D0735EAFA9AD89AD9DDE723AA98FD9AC41A5
              D3096A813AE07241EAE4A4FC532EDF9FF3F94CAD137A21107238A80FE9B44135
              3222002C5B07380E526767E55FFDFDFB56BFBFB3002C0ED86C943499342AB55A
              3EFBF808383C5EDDC2C303C0BBBBC1F74482C96B34B43D1834BDD803BFD54ABD
              4F248CC4D010BFFAF404B05A41E6EAAA8CA6303828788E5D5C30BFB55ADA110A
              359BF8C5E108F71C1D4D110303FC6ACD37D6D50532A914F3677898860292F373
              23A152D519870332D7D74C616C6CE7B3DF5F1FE3A6DD1E16C5E35372B99C0F0B
              6E7239A6303A4A3B2311B4483E8B85EA393D35CA140A3E5C24C88B7AFD8E3B10
              68EE81D76CA684C7C7465850D2E9685734DAB6CAAFF1B6267AED76742CAE40A0
              E33175E2D85BCFF93FCFBA6CF0FFE069AC0000000049454E44AE426082}
          end>
      end>
    Left = 342
    Top = 136
  end
  object imlMain: TVirtualImageList
    AutoFill = True
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Item1'
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Item2'
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Item3'
        Name = 'Item3'
      end>
    ImageCollection = imcMain
    Left = 502
    Top = 152
  end
end
