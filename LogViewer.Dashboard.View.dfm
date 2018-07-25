object frmDashboard: TfrmDashboard
  Left = 0
  Top = 0
  ClientHeight = 557
  ClientWidth = 829
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
    Top = 32
    Width = 829
    Height = 525
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object splVertical: TSplitter
      Left = 505
      Top = 0
      Width = 7
      Height = 525
      ExplicitLeft = 320
      ExplicitHeight = 429
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 505
      Height = 525
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlRight: TPanel
      Left = 512
      Top = 0
      Width = 317
      Height = 525
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pgcMain: TPageControl
        Left = 0
        Top = 0
        Width = 317
        Height = 525
        ActivePage = tsZeroMQ
        Align = alClient
        Images = dmManager.imlMain
        TabOrder = 0
        object tsWinIPC: TTabSheet
          Caption = 'WinIPC'
          ImageIndex = 28
        end
        object tsWinODS: TTabSheet
          Caption = 'WinODS'
          ImageIndex = 28
        end
        object tsZeroMQ: TTabSheet
          Caption = 'ZeroMQ'
          ImageIndex = 6
          object edtAddress: TLabeledEdit
            Left = 72
            Top = 16
            Width = 121
            Height = 21
            EditLabel.Width = 43
            EditLabel.Height = 13
            EditLabel.Caption = 'Address:'
            LabelPosition = lpLeft
            TabOrder = 0
            Text = '192.168.0.226'
          end
          object edtPort: TLabeledEdit
            Left = 72
            Top = 43
            Width = 121
            Height = 21
            EditLabel.Width = 24
            EditLabel.Height = 13
            EditLabel.Caption = 'Port:'
            LabelPosition = lpLeft
            LabelSpacing = 20
            TabOrder = 1
            Text = '5556'
          end
          object btnAddZeroMQNode: TButton
            Left = 74
            Top = 70
            Width = 119
            Height = 25
            Action = actAddZeroMQNode
            TabOrder = 2
          end
        end
        object tsCOMPort: TTabSheet
          Caption = 'COMPort'
          ImageIndex = 24
        end
      end
    end
  end
  object aclMain: TActionList
    Left = 168
    Top = 152
    object actAddZeroMQNode: TAction
      Caption = 'Add ZeroMQ node'
      OnExecute = actAddZeroMQNodeExecute
    end
  end
end
