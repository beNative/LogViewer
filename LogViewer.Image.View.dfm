object frmImageView: TfrmImageView
  Left = 0
  Top = 0
  ClientHeight = 909
  ClientWidth = 682
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edtPixelFormat: TLabeledEdit
    Left = 303
    Top = 6
    Width = 73
    Height = 21
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'Pixel format:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object edtHandleType: TLabeledEdit
    Left = 448
    Top = 8
    Width = 73
    Height = 21
    EditLabel.Width = 62
    EditLabel.Height = 13
    EditLabel.Caption = 'Handle type:'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object edtHeight: TLabeledEdit
    Left = 160
    Top = 6
    Width = 73
    Height = 21
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'Height:'
    LabelPosition = lpLeft
    TabOrder = 2
  end
  object edtWidth: TLabeledEdit
    Left = 40
    Top = 6
    Width = 73
    Height = 21
    EditLabel.Width = 32
    EditLabel.Height = 13
    EditLabel.Caption = 'Width:'
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object sbxMain: TScrollBox
    Left = 0
    Top = 35
    Width = 682
    Height = 874
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelEdges = []
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clWhite
    ParentColor = False
    TabOrder = 4
    ExplicitWidth = 1043
    object imgBitmap: TImage
      Left = 3
      Top = 3
      Width = 673
      Height = 585
      AutoSize = True
      Center = True
      IncrementalDisplay = True
      Proportional = True
    end
  end
end
