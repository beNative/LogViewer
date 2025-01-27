object frmImageView: TfrmImageView
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 1364
  ClientWidth = 1032
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  PixelsPerInch = 144
  TextHeight = 23
  object edtPixelFormat: TLabeledEdit
    Left = 455
    Top = 5
    Width = 109
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    BevelOuter = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    EditLabel.Width = 95
    EditLabel.Height = 25
    EditLabel.Margins.Left = 5
    EditLabel.Margins.Top = 5
    EditLabel.Margins.Right = 5
    EditLabel.Margins.Bottom = 5
    EditLabel.Caption = 'Pixel format:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    LabelPosition = lpLeft
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    Text = ''
  end
  object edtHandleType: TLabeledEdit
    Left = 672
    Top = 5
    Width = 110
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    BevelOuter = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    EditLabel.Width = 96
    EditLabel.Height = 25
    EditLabel.Margins.Left = 5
    EditLabel.Margins.Top = 5
    EditLabel.Margins.Right = 5
    EditLabel.Margins.Bottom = 5
    EditLabel.Caption = 'Handle type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    LabelPosition = lpLeft
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Text = ''
  end
  object edtHeight: TLabeledEdit
    Left = 240
    Top = 5
    Width = 110
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    BevelOuter = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    EditLabel.Width = 55
    EditLabel.Height = 25
    EditLabel.Margins.Left = 5
    EditLabel.Margins.Top = 5
    EditLabel.Margins.Right = 5
    EditLabel.Margins.Bottom = 5
    EditLabel.Caption = 'Height:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    LabelPosition = lpLeft
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    Text = ''
  end
  object edtWidth: TLabeledEdit
    Left = 60
    Top = 5
    Width = 110
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    BevelOuter = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    EditLabel.Width = 50
    EditLabel.Height = 25
    EditLabel.Margins.Left = 5
    EditLabel.Margins.Top = 5
    EditLabel.Margins.Right = 5
    EditLabel.Margins.Bottom = 5
    EditLabel.Caption = 'Width:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    LabelPosition = lpLeft
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
    Text = ''
  end
  object sbxMain: TScrollBox
    Left = 0
    Top = 32
    Width = 1032
    Height = 1332
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
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
    object imgBitmap: TImage
      Left = 5
      Top = 5
      Width = 1014
      Height = 877
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = True
      Center = True
      IncrementalDisplay = True
      PopupMenu = ppmMain
      Proportional = True
    end
  end
  object aclMain: TActionList
    Left = 512
    Top = 176
    object actCopy: TAction
      Caption = 'Copy'
      Hint = 'Copy image to clipboard'
      OnExecute = actCopyExecute
    end
  end
  object ppmMain: TPopupMenu
    Left = 432
    Top = 179
    object mniCopy: TMenuItem
      Action = actCopy
      ShortCut = 16451
    end
  end
end
