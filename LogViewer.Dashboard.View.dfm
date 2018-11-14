object frmDashboard: TfrmDashboard
  Left = 0
  Top = 0
  ClientHeight = 421
  ClientWidth = 917
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlLogChannels: TPanel
    Left = 0
    Top = 0
    Width = 917
    Height = 421
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object splVertical: TSplitter
      Left = 305
      Top = 0
      Width = 7
      Height = 421
      ExplicitLeft = 437
    end
    object pnlRight: TPanel
      AlignWithMargins = True
      Left = 312
      Top = 3
      Width = 602
      Height = 418
      Margins.Left = 0
      Margins.Bottom = 0
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 305
      Height = 421
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object pgcMain: TPageControl
        Left = 0
        Top = 0
        Width = 305
        Height = 421
        ActivePage = tsWinIPC
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        MultiLine = True
        ParentFont = False
        Style = tsFlatButtons
        TabOrder = 0
        object tsWinIPC: TTabSheet
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Caption = 'WinIPC'
          ImageIndex = 28
          object lblWinIPCDescription: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 28
            Width = 291
            Height = 362
            Align = alClient
            Caption = 
              'Messages are exchanged between the source application and LogVie' +
              'wer using WM_COPY Windows messages. These messages have a specif' +
              'ic signature and payload that is recognized by LogViewer. '#13#10#13#10'Th' +
              'e messages are sent from the originating process using the SendM' +
              'essage method from the Windows API. The SendMessage function cal' +
              'ls the window procedure for the specified window and does not re' +
              'turn until the window procedure has processed the message. This ' +
              'means that each message the application logs is always in sync w' +
              'ith LogViewer. As a result this method is slower than receivers ' +
              'based on queueuing.'#13#10#13#10'A new node is automatically created when ' +
              'a process is detected that sends compatible log messages.'#13#10
            WordWrap = True
            ExplicitWidth = 287
            ExplicitHeight = 221
          end
          object pnlWinIPCTitle: TPanel
            Left = 0
            Top = 0
            Width = 297
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = 'Windows IPC'
            Font.Charset = ANSI_CHARSET
            Font.Color = clGray
            Font.Height = -16
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
        object tsWinODS: TTabSheet
          Caption = 'WinODS'
          ImageIndex = 28
          object lblWinODSDescription: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 28
            Width = 291
            Height = 362
            Align = alClient
            Caption = 
              'This receiver captures messages from any local applicatiion that' +
              ' uses the OutputDebugString routine from the native Windows API ' +
              'to emit logging information. The native version of OutputDebugSt' +
              'ring uses ASCII text messages.'#13#10#13#10'The performance impact by usin' +
              'g OutputDebugString in your application is generally quite low w' +
              'hen no one is listening to its output. However, having a listene' +
              'r running in the background like LogViewer can make it more than' +
              ' 10 times slower, and even much more in a mutithreaded environme' +
              'nt. The reason is that any listener needs to hook the report eve' +
              'nt, and the handling of the event is done within the scope of th' +
              'e OutputDebugString call. Moreover, if several threads call Outp' +
              'utDebugString concurrently, they will be synchronized. '#13#10#13#10'A new' +
              ' node is automatically created when a new process is detected th' +
              'at sends these messages.'#13#10
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitHeight = 247
          end
          object pnlWinODSTitle: TPanel
            Left = 0
            Top = 0
            Width = 297
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = 'Windows API OuputDebugString '
            Font.Charset = ANSI_CHARSET
            Font.Color = clGray
            Font.Height = -16
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
        object tsZeroMQ: TTabSheet
          Caption = 'ZeroMQ'
          ImageIndex = 6
          DesignSize = (
            297
            390)
          object btnSubscribeToList: TButton
            Left = -1
            Top = 316
            Width = 299
            Height = 25
            Action = actSubscribeToList
            Anchors = [akLeft, akRight, akBottom]
            Default = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object pnlZeroMQTitle: TPanel
            Left = 0
            Top = 0
            Width = 297
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = 'ZeroMQ'
            Font.Charset = ANSI_CHARSET
            Font.Color = clGray
            Font.Height = -16
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 1
          end
          object lstZeroMQ: TValueListEditor
            Left = 1
            Top = 31
            Width = 296
            Height = 279
            Anchors = [akLeft, akTop, akRight, akBottom]
            DisplayOptions = [doColumnTitles, doAutoColResize]
            DrawingStyle = gdsClassic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
            Options = [goVertLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
            ParentFont = False
            TabOrder = 2
            TitleCaptions.Strings = (
              'Name'
              'Endpoint')
            ColWidths = (
              145
              145)
            RowHeights = (
              18
              18)
          end
          object pnlZeroMQButtons: TGridPanel
            Left = 0
            Top = 342
            Width = 297
            Height = 48
            Anchors = [akLeft, akRight, akBottom]
            BevelOuter = bvNone
            ColumnCollection = <
              item
                Value = 50.000000000000000000
              end
              item
                Value = 50.000000000000000000
              end>
            ControlCollection = <
              item
                Column = 0
                Control = btnAddZMQNodeLocalHost
                Row = 0
              end
              item
                Column = 1
                Control = btnAddZMQNodeForLogViewer
                Row = 0
              end>
            RowCollection = <
              item
                Value = 100.000000000000000000
              end>
            TabOrder = 3
            object btnAddZMQNodeLocalHost: TButton
              AlignWithMargins = True
              Left = 0
              Top = 0
              Width = 148
              Height = 48
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Action = actSubscribeToLocalHost
              Align = alClient
              TabOrder = 0
              WordWrap = True
            end
            object btnAddZMQNodeForLogViewer: TButton
              AlignWithMargins = True
              Left = 150
              Top = 0
              Width = 147
              Height = 48
              Margins.Left = 2
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              Action = actAddSubscribeToLogViewer
              Align = alClient
              TabOrder = 1
              WordWrap = True
            end
          end
        end
        object tsCOMPort: TTabSheet
          Caption = 'COMPort'
          ImageIndex = 24
          object pnlCOMPortTitle: TPanel
            Left = 0
            Top = 0
            Width = 297
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = 'COM port'
            Font.Charset = ANSI_CHARSET
            Font.Color = clGray
            Font.Height = -16
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
      end
    end
  end
  object aclMain: TActionList
    Left = 328
    Top = 160
    object actSubscribeToLocalHost: TAction
      Caption = 'Subscribe to default local publisher (tcp://localhost:5555)'
      OnExecute = actSubscribeToLocalHostExecute
    end
    object actInspectTreeview: TAction
      Caption = 'Inspect treeview'
      OnExecute = actInspectTreeviewExecute
    end
    object actAddSubscribeToLogViewer: TAction
      Caption = 'Subscribe to LogViewer instance (tcp://localhost:42134)'
      OnExecute = actAddSubscribeToLogViewerExecute
    end
    object actSubscribeToList: TAction
      Caption = 'Subscribe to list of endpoints'
      OnExecute = actSubscribeToListExecute
    end
    object actCloseSubscriber: TAction
      Caption = 'Close subscriber'
      OnExecute = actCloseSubscriberExecute
    end
  end
  object imlMain: TImageList
    ColorDepth = cd32Bit
    Left = 328
    Top = 112
    Bitmap = {
      494C010103000500740010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C0D0C4F777C7AF01D1E1D770000000000000005151515660808
      08400000000000000000000000000000000000000000000000008E92EBFF0000
      0000000000008789ECFF494CF4FF2B2FF8FF2A2FF8FF4648F5FF8286EDFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000330000003300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004B4E4DBEA2A5A4FF7E8280F50707073D474A49BA858886F57C81
      7FF600000000000000000000000000000000000000004F51F3FF0000FFFF3F3F
      F6FF1112FCFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0B0C
      FDFF7D82EDFF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000009A9896FF666463FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000134D4F4EC04749
      49B90E0E0E54737977EDB3B4B4FFA8A9A9FD818684F78D908FF5B7B8B8FF7E83
      81F7000000000000000000000000000000008C8CEBFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF5558F2FF00000000000000000000000000000000000000000000
      0000000000000000000000000022989694FF62605EFF00000022000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008080840818684F6ABAD
      ACFF868A88F29FA2A1FCCACACAFFD7D7D7FFD7D7D7FFC8C8C8FFBDBDBDFF8085
      83F7222322813C3F3DAA1F20207B00000002000000003F3FF6FF0000FFFF0000
      FFFF0000FFFF0405FEFF8385ECFFB5B9E5FFB9BBE5FF8B8DEBFF292CF9FF0000
      FFFF0000FFFF0000FFFF7879EEFF000000000000000000000000000000000000
      00000000000000000000474747B9949495FF959696FF474747B9000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000046B6F6DE3B6B8
      B7FFC9CACAFFE4E4E4FFF5F5F5FFE1E3E2FFE1E2E2FFF5F5F5FFE2E3E3FFB4B6
      B6FFA0A3A2FDA9ABAAFF848987FD00000014000000000F10FCFF0000FFFF0000
      FFFF0000FFFF0000FFFF5355F3FF00000000000000000000000000000000595B
      F2FF0000FFFF0000FFFF0708FDFFC8C9E3FF0000000000000000000000000000
      00000000000000000000939393FFE3E2DFFFCCCAC7FF949494FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000001010119434545B5ACAF
      AEFFDFDFDFFFD8D9D9FE848987F45D6060D35D605FD3848987F4D7D8D8FEDBDC
      DCFFC8C8C8FF939695F6464947B8000000018487ECFF0000FFFF0000FFFF0505
      FEFF0000FFFF0000FFFF0000FFFF5454F3FF0000000000000000000000000000
      00003031F7FF0000FFFF0000FFFF7173EEFF0000000000000000000000000000
      00000000000000000021929292FFDCDBDAFFDCDBDAFF929292FF000000210000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000454745B5808583F78F9291F3CECF
      CFFFE5E5E5FF838886F30C0C0C4D00000000000000000C0C0C4D838886F3E2E2
      E2FFCBCBCBFF828785FC00000010000000004648F4FF0000FFFF0000FFFF8286
      EDFF5858F2FF0000FFFF0000FFFF0000FFFF5356F3FF00000000000000000000
      00009696EAFF0000FFFF0000FFFF3539F7FF0000000000000000000000000000
      000000000021434343B6909191FF8D8E8EFF8D8E8EFF909191FF434343B60000
      0021000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B807EF4B7BAB9FFD7D9D9FFDADB
      DBFFCCCECEFF696C6BDF00000000000000000000000000000000696C6ADFC9CA
      CAFFD4D5D5FF8F9392F5686C6ADF0B0B0B4A292DF8FF0000FFFF0000FFFFB5B7
      E5FF000000005959F2FF0000FFFF0000FFFF0000FFFF5355F3FF000000000000
      0000C6C9E3FF0000FFFF0000FFFF1A1AFBFF0000000000000000000000230000
      0033434343B6BDBDBCFFD9D8D5FFD7D6D4FFD7D6D4FFD9D8D5FFBDBDBCFF4343
      43B6000000330000002300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000006060637555857C98A8E8DF4DCDD
      DDFFC8CAC9FF6D706FE3000000000000000000000000000000006D6F6FE3C4C6
      C5FFD7D8D8FFD6D7D7FFB9BCBBFF7F8482F8292DF8FF0000FFFF0000FFFFB5B7
      E5FF00000000000000006363F0FF0000FFFF0000FFFF0000FFFF5455F3FF0000
      0000C6C9E3FF0000FFFF0000FFFF1A1AFBFF00000000000000004F4F4FBF9A9A
      9AFF919191FFD0CDCCFFC5C4C2FFC4C3C1FFC4C3C1FFC5C4C2FFD0CDCCFF9191
      91FF9A9A9AFF4F4F4FBF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000015828785FCDDE0
      DFFFE1E2E2FF838785F21112125D00000001000000011112125D838685F2DCDE
      DEFFD2D4D3FF888B8AF2818684FA3335349C4649F4FF0000FFFF0000FFFF8488
      ECFF0000000000000000000000006363F0FF0000FFFF0000FFFF0000FFFF5053
      F3FF9293EAFF0000FFFF0000FFFF3539F7FF0000000000000000999999FFD3D3
      D3FF8F8F8FFFCDCCCBFFB4B3B1FFB5B4B2FFB5B4B2FFB4B3B1FFCDCCCBFF8F8F
      8FFFD3D3D3FF999999FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000004575A5ACCACAFAEF9E6E8
      E8FFE6E8E8FFCCCFCEFF858987F26F7472E66F7472E6858887F2CACCCBFFE1E3
      E3FFB5B9B8FF3C3E3EAB00000007000000008487ECFF0000FFFF0000FFFF2223
      FAFF000000000000000000000000000000006363F0FF0000FFFF0000FFFF0000
      FFFF0708FDFF0000FFFF0000FFFF7173EEFF0000000000000000999999FFD2D2
      D2FF8E8F8FFFD2CFCEFFA5A2A0FFA6A3A1FFA6A3A1FFA5A2A0FFD2CFCEFF8E8F
      8FFFD2D2D2FF999999FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000001010118838886FCB7BAB9FE9CA0
      9EF5C9CCCCFFEAECECFFE9EBEBFFD4D6D6FFD3D6D5FFE7E9E9FFE7E9E9FFE6E8
      E8FFCFD2D1FF737977EB0000000700000000000000000F0FFCFF0000FFFF0000
      FFFF4B50F4FF000000000000000000000000000000006363F0FF0000FFFF0000
      FFFF0000FFFF0000FFFF0708FDFF0000000000000000000000339C9C9CFFD6D5
      D5FF929191FFD9D7D5FFD8D5D4FFD8D5D4FFD8D5D4FFD8D5D4FFDAD7D5FF9291
      91FFD6D5D5FF9C9C9CFF00000033000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000001141515652728288A1516
      1668818684F7E8EAEAFFECEEEEFFE9EBEBFFECEEEEFFECEEEEFF9DA1A0F58489
      87F1B4B7B6FC858A88F6080808410000000000000000898BEBFF0000FFFF0000
      FFFF0000FFFF2223FAFF8688ECFFB5B9E5FFB9BBE5FF8A8CEBFF0B0CFDFF0000
      FFFF0000FFFF0000FFFF3135F7FF0000000000000000898887FF898684FF8984
      81FF888480FF86807BFF837B77FF827975FF847A75FF867C77FF88817BFF8984
      80FF898481FF898684FF898887FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000818684F9E4E6E6FF929795F4828785FAD3D5D5FFD7DADAFF727674E80707
      073C3435359D3A3B3BA50000000F000000000000000000000000676AF0FF0000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF8588ECFF0000000000000000000000003C98
      DFFF000000003991D7FF95E9FFFF93EBFFFF6BDBFFFF45CDFFFF3C93D7FF0000
      00003C98DFFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007A7F7DF2888C8AF43B3D3DA8080808407D8280F4AFB3B2FF4A4D4BBA0000
      0000000000000000000000000000000000000000000000000000000000008A8D
      EBFF1112FCFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0B0C
      FDFF4348F5FF0000FFFF4E4EF3FF000000000000000000000000000000004497
      D6FF000000004091D0FFC8F7FFFFDFFFFFFFCBF6FFFFBDF2FFFF4292D0FF0000
      00004497D6FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000050605350F0F0F570000000300000000090909456C716FE4080808400000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008788ECFF494BF4FF2B2FF8FF2A2FF8FF4648F5FF8284EDFF0000
      0000000000009598EAFF00000000000000000000000000000000000000000000
      000000000000214562AF4190CEFF3E8ECCFF3F8ECCFF4290CEFF214562AF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF000000D81F000000000000800700000000
      00000003000000000000800100000000000081E000000000000000F000000000
      0000007000000000000008300000000000000C100000000000000E0000000000
      00000F0000000000000087810000000000008001000000000000C00000000000
      0000E001000000000000F81B0000000000000000000000000000000000000000
      000000000000}
  end
  object ppmMain: TPopupMenu
    Left = 328
    Top = 224
    object mniCloseSsubscriber: TMenuItem
      Action = actCloseSubscriber
    end
  end
end
