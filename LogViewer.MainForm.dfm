object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'Logviewer'
  ClientHeight = 599
  ClientWidth = 997
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMainClient: TPanel
    Left = 0
    Top = 30
    Width = 997
    Height = 549
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ctMain: TChromeTabs
      Left = 0
      Top = 0
      Width = 997
      Height = 25
      OnActiveTabChanged = ctMainActiveTabChanged
      OnButtonCloseTabClick = ctMainButtonCloseTabClick
      OnNeedDragImageControl = ctMainNeedDragImageControl
      OnBeforeDrawItem = ctMainBeforeDrawItem
      ActiveTabIndex = -1
      Options.Display.CloseButton.Offsets.Vertical = 6
      Options.Display.CloseButton.Offsets.Horizontal = -8
      Options.Display.CloseButton.Height = 15
      Options.Display.CloseButton.Width = 15
      Options.Display.CloseButton.AutoHide = True
      Options.Display.CloseButton.Visibility = bvAll
      Options.Display.CloseButton.AutoHideWidth = 10
      Options.Display.CloseButton.CrossRadialOffset = 4
      Options.Display.AddButton.Offsets.Vertical = 10
      Options.Display.AddButton.Offsets.Horizontal = 3
      Options.Display.AddButton.Height = 14
      Options.Display.AddButton.Width = 40
      Options.Display.AddButton.ShowPlusSign = True
      Options.Display.AddButton.Visibility = avNone
      Options.Display.AddButton.HorizontalOffsetFloating = -3
      Options.Display.ScrollButtonLeft.Offsets.Vertical = 10
      Options.Display.ScrollButtonLeft.Offsets.Horizontal = 1
      Options.Display.ScrollButtonLeft.Height = 15
      Options.Display.ScrollButtonLeft.Width = 15
      Options.Display.ScrollButtonRight.Offsets.Vertical = 10
      Options.Display.ScrollButtonRight.Offsets.Horizontal = 1
      Options.Display.ScrollButtonRight.Height = 15
      Options.Display.ScrollButtonRight.Width = 15
      Options.Display.TabModifiedGlow.Style = msKnightRider
      Options.Display.TabModifiedGlow.VerticalOffset = -6
      Options.Display.TabModifiedGlow.Height = 30
      Options.Display.TabModifiedGlow.Width = 100
      Options.Display.TabModifiedGlow.AnimationPeriodMS = 100
      Options.Display.TabModifiedGlow.EaseType = ttEaseInOutSine
      Options.Display.TabModifiedGlow.AnimationUpdateMS = 50
      Options.Display.Tabs.SeeThroughTabs = False
      Options.Display.Tabs.TabOverlap = 15
      Options.Display.Tabs.ContentOffsetLeft = 15
      Options.Display.Tabs.ContentOffsetRight = 15
      Options.Display.Tabs.OffsetLeft = 5
      Options.Display.Tabs.OffsetTop = 0
      Options.Display.Tabs.OffsetRight = 5
      Options.Display.Tabs.OffsetBottom = 0
      Options.Display.Tabs.MinWidth = 120
      Options.Display.Tabs.MaxWidth = 600
      Options.Display.Tabs.TabWidthFromContent = True
      Options.Display.Tabs.PinnedWidth = 80
      Options.Display.Tabs.ImageOffsetLeft = 15
      Options.Display.Tabs.TextTrimType = tttNone
      Options.Display.Tabs.Orientation = toTop
      Options.Display.Tabs.BaseLineTabRegionOnly = False
      Options.Display.Tabs.WordWrap = False
      Options.Display.Tabs.TextAlignmentHorizontal = taCenter
      Options.Display.Tabs.TextAlignmentVertical = taVerticalCenter
      Options.Display.Tabs.ShowImages = True
      Options.Display.Tabs.ShowPinnedTabText = True
      Options.Display.TabContainer.TransparentBackground = True
      Options.Display.TabContainer.OverlayButtons = False
      Options.Display.TabContainer.PaddingLeft = 0
      Options.Display.TabContainer.PaddingRight = 0
      Options.Display.TabMouseGlow.Offsets.Vertical = 0
      Options.Display.TabMouseGlow.Offsets.Horizontal = 0
      Options.Display.TabMouseGlow.Height = 200
      Options.Display.TabMouseGlow.Width = 200
      Options.Display.TabMouseGlow.Visible = True
      Options.Display.TabSpinners.Upload.ReverseDirection = True
      Options.Display.TabSpinners.Upload.RenderedAnimationStep = 2
      Options.Display.TabSpinners.Upload.Position.Offsets.Vertical = 0
      Options.Display.TabSpinners.Upload.Position.Offsets.Horizontal = 0
      Options.Display.TabSpinners.Upload.Position.Height = 16
      Options.Display.TabSpinners.Upload.Position.Width = 16
      Options.Display.TabSpinners.Upload.SweepAngle = 135
      Options.Display.TabSpinners.Download.ReverseDirection = False
      Options.Display.TabSpinners.Download.RenderedAnimationStep = 5
      Options.Display.TabSpinners.Download.Position.Offsets.Vertical = 0
      Options.Display.TabSpinners.Download.Position.Offsets.Horizontal = 0
      Options.Display.TabSpinners.Download.Position.Height = 16
      Options.Display.TabSpinners.Download.Position.Width = 16
      Options.Display.TabSpinners.Download.SweepAngle = 135
      Options.Display.TabSpinners.AnimationUpdateMS = 50
      Options.Display.TabSpinners.HideImagesWhenSpinnerVisible = True
      Options.DragDrop.DragType = dtWithinContainer
      Options.DragDrop.DragOutsideImageAlpha = 220
      Options.DragDrop.DragOutsideDistancePixels = 30
      Options.DragDrop.DragStartPixels = 2
      Options.DragDrop.DragControlImageResizeFactor = 0.500000000000000000
      Options.DragDrop.DragCursor = crDefault
      Options.DragDrop.DragDisplay = ddTabAndControl
      Options.DragDrop.DragFormBorderWidth = 2
      Options.DragDrop.DragFormBorderColor = 8421504
      Options.DragDrop.ContrainDraggedTabWithinContainer = True
      Options.Animation.DefaultMovementAnimationTimeMS = 50
      Options.Animation.DefaultStyleAnimationTimeMS = 10
      Options.Animation.AnimationTimerInterval = 5
      Options.Animation.MinimumTabAnimationWidth = 40
      Options.Animation.DefaultMovementEaseType = ttLinearTween
      Options.Animation.DefaultStyleEaseType = ttLinearTween
      Options.Animation.MovementAnimations.TabAdd.UseDefaultEaseType = True
      Options.Animation.MovementAnimations.TabAdd.UseDefaultAnimationTime = True
      Options.Animation.MovementAnimations.TabAdd.EaseType = ttEaseOutExpo
      Options.Animation.MovementAnimations.TabAdd.AnimationTimeMS = 500
      Options.Animation.MovementAnimations.TabDelete.UseDefaultEaseType = True
      Options.Animation.MovementAnimations.TabDelete.UseDefaultAnimationTime = True
      Options.Animation.MovementAnimations.TabDelete.EaseType = ttEaseOutExpo
      Options.Animation.MovementAnimations.TabDelete.AnimationTimeMS = 500
      Options.Animation.MovementAnimations.TabMove.UseDefaultEaseType = False
      Options.Animation.MovementAnimations.TabMove.UseDefaultAnimationTime = False
      Options.Animation.MovementAnimations.TabMove.EaseType = ttEaseOutExpo
      Options.Animation.MovementAnimations.TabMove.AnimationTimeMS = 500
      Options.Behaviour.BackgroundDblClickMaximiseRestoreForm = False
      Options.Behaviour.BackgroundDragMovesForm = False
      Options.Behaviour.TabSmartDeleteResizing = True
      Options.Behaviour.TabSmartDeleteResizeCancelDelay = 700
      Options.Behaviour.UseBuiltInPopupMenu = False
      Options.Behaviour.TabRightClickSelect = True
      Options.Behaviour.ActivateNewTab = True
      Options.Behaviour.DebugMode = False
      Options.Behaviour.IgnoreDoubleClicksWhileAnimatingMovement = True
      Options.Scrolling.Enabled = True
      Options.Scrolling.ScrollButtons = csbRight
      Options.Scrolling.ScrollStep = 20
      Options.Scrolling.ScrollRepeatDelay = 20
      Options.Scrolling.AutoHideButtons = True
      Options.Scrolling.DragScroll = True
      Options.Scrolling.DragScrollOffset = 50
      Options.Scrolling.MouseWheelScroll = True
      Tabs = <>
      LookAndFeel.TabsContainer.StartColor = clWhite
      LookAndFeel.TabsContainer.StopColor = clWhite
      LookAndFeel.TabsContainer.StartAlpha = 255
      LookAndFeel.TabsContainer.StopAlpha = 255
      LookAndFeel.TabsContainer.OutlineColor = cl3DLight
      LookAndFeel.TabsContainer.OutlineAlpha = 0
      LookAndFeel.Tabs.BaseLine.Color = clScrollBar
      LookAndFeel.Tabs.BaseLine.Thickness = 1.000000000000000000
      LookAndFeel.Tabs.BaseLine.Alpha = 255
      LookAndFeel.Tabs.Modified.CentreColor = clBlue
      LookAndFeel.Tabs.Modified.OutsideColor = clBlue
      LookAndFeel.Tabs.Modified.CentreAlpha = 130
      LookAndFeel.Tabs.Modified.OutsideAlpha = 0
      LookAndFeel.Tabs.DefaultFont.Name = 'Segoe UI Semibold'
      LookAndFeel.Tabs.DefaultFont.Color = clBlack
      LookAndFeel.Tabs.DefaultFont.Size = 8
      LookAndFeel.Tabs.DefaultFont.Alpha = 255
      LookAndFeel.Tabs.DefaultFont.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.MouseGlow.CentreColor = clWhite
      LookAndFeel.Tabs.MouseGlow.OutsideColor = clWhite
      LookAndFeel.Tabs.MouseGlow.CentreAlpha = 0
      LookAndFeel.Tabs.MouseGlow.OutsideAlpha = 0
      LookAndFeel.Tabs.Spinners.Upload.Color = 12759975
      LookAndFeel.Tabs.Spinners.Upload.Thickness = 2.500000000000000000
      LookAndFeel.Tabs.Spinners.Upload.Alpha = 255
      LookAndFeel.Tabs.Spinners.Download.Color = 14388040
      LookAndFeel.Tabs.Spinners.Download.Thickness = 2.500000000000000000
      LookAndFeel.Tabs.Spinners.Download.Alpha = 255
      LookAndFeel.Tabs.Active.Font.Name = 'Segoe UI Semibold'
      LookAndFeel.Tabs.Active.Font.Color = clWhite
      LookAndFeel.Tabs.Active.Font.Size = 8
      LookAndFeel.Tabs.Active.Font.Alpha = 255
      LookAndFeel.Tabs.Active.Font.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.Active.Font.UseDefaultFont = False
      LookAndFeel.Tabs.Active.Style.StartColor = clGray
      LookAndFeel.Tabs.Active.Style.StopColor = clGray
      LookAndFeel.Tabs.Active.Style.StartAlpha = 255
      LookAndFeel.Tabs.Active.Style.StopAlpha = 255
      LookAndFeel.Tabs.Active.Style.OutlineColor = clGray
      LookAndFeel.Tabs.Active.Style.OutlineSize = 1.000000000000000000
      LookAndFeel.Tabs.Active.Style.OutlineAlpha = 255
      LookAndFeel.Tabs.NotActive.Font.Name = 'Segoe UI Semibold'
      LookAndFeel.Tabs.NotActive.Font.Color = clBlack
      LookAndFeel.Tabs.NotActive.Font.Size = 8
      LookAndFeel.Tabs.NotActive.Font.Alpha = 255
      LookAndFeel.Tabs.NotActive.Font.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.NotActive.Font.UseDefaultFont = False
      LookAndFeel.Tabs.NotActive.Style.StartColor = clSilver
      LookAndFeel.Tabs.NotActive.Style.StopColor = clSilver
      LookAndFeel.Tabs.NotActive.Style.StartAlpha = 255
      LookAndFeel.Tabs.NotActive.Style.StopAlpha = 255
      LookAndFeel.Tabs.NotActive.Style.OutlineColor = clGray
      LookAndFeel.Tabs.NotActive.Style.OutlineSize = 1.000000000000000000
      LookAndFeel.Tabs.NotActive.Style.OutlineAlpha = 255
      LookAndFeel.Tabs.Hot.Font.Name = 'Segoe UI Semibold'
      LookAndFeel.Tabs.Hot.Font.Color = clBlack
      LookAndFeel.Tabs.Hot.Font.Size = 8
      LookAndFeel.Tabs.Hot.Font.Alpha = 255
      LookAndFeel.Tabs.Hot.Font.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.Hot.Font.UseDefaultFont = False
      LookAndFeel.Tabs.Hot.Style.StartColor = clSilver
      LookAndFeel.Tabs.Hot.Style.StopColor = clSilver
      LookAndFeel.Tabs.Hot.Style.StartAlpha = 255
      LookAndFeel.Tabs.Hot.Style.StopAlpha = 255
      LookAndFeel.Tabs.Hot.Style.OutlineColor = clGray
      LookAndFeel.Tabs.Hot.Style.OutlineSize = 1.000000000000000000
      LookAndFeel.Tabs.Hot.Style.OutlineAlpha = 255
      LookAndFeel.CloseButton.Cross.Normal.Color = clBlack
      LookAndFeel.CloseButton.Cross.Normal.Thickness = 1.500000000000000000
      LookAndFeel.CloseButton.Cross.Normal.Alpha = 255
      LookAndFeel.CloseButton.Cross.Down.Color = clWhite
      LookAndFeel.CloseButton.Cross.Down.Thickness = 2.000000000000000000
      LookAndFeel.CloseButton.Cross.Down.Alpha = 255
      LookAndFeel.CloseButton.Cross.Hot.Color = clWhite
      LookAndFeel.CloseButton.Cross.Hot.Thickness = 2.000000000000000000
      LookAndFeel.CloseButton.Cross.Hot.Alpha = 255
      LookAndFeel.CloseButton.Circle.Normal.StartColor = clGradientActiveCaption
      LookAndFeel.CloseButton.Circle.Normal.StopColor = clNone
      LookAndFeel.CloseButton.Circle.Normal.StartAlpha = 0
      LookAndFeel.CloseButton.Circle.Normal.StopAlpha = 0
      LookAndFeel.CloseButton.Circle.Normal.OutlineColor = clGray
      LookAndFeel.CloseButton.Circle.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.CloseButton.Circle.Normal.OutlineAlpha = 0
      LookAndFeel.CloseButton.Circle.Down.StartColor = 3487169
      LookAndFeel.CloseButton.Circle.Down.StopColor = 3487169
      LookAndFeel.CloseButton.Circle.Down.StartAlpha = 255
      LookAndFeel.CloseButton.Circle.Down.StopAlpha = 255
      LookAndFeel.CloseButton.Circle.Down.OutlineColor = clGray
      LookAndFeel.CloseButton.Circle.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.CloseButton.Circle.Down.OutlineAlpha = 255
      LookAndFeel.CloseButton.Circle.Hot.StartColor = clRed
      LookAndFeel.CloseButton.Circle.Hot.StopColor = clRed
      LookAndFeel.CloseButton.Circle.Hot.StartAlpha = 255
      LookAndFeel.CloseButton.Circle.Hot.StopAlpha = 255
      LookAndFeel.CloseButton.Circle.Hot.OutlineColor = clGray
      LookAndFeel.CloseButton.Circle.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.CloseButton.Circle.Hot.OutlineAlpha = 255
      LookAndFeel.AddButton.Button.Normal.StartColor = clSilver
      LookAndFeel.AddButton.Button.Normal.StopColor = clSilver
      LookAndFeel.AddButton.Button.Normal.StartAlpha = 255
      LookAndFeel.AddButton.Button.Normal.StopAlpha = 255
      LookAndFeel.AddButton.Button.Normal.OutlineColor = clGray
      LookAndFeel.AddButton.Button.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.Button.Normal.OutlineAlpha = 255
      LookAndFeel.AddButton.Button.Down.StartColor = clSilver
      LookAndFeel.AddButton.Button.Down.StopColor = clSilver
      LookAndFeel.AddButton.Button.Down.StartAlpha = 255
      LookAndFeel.AddButton.Button.Down.StopAlpha = 255
      LookAndFeel.AddButton.Button.Down.OutlineColor = clGray
      LookAndFeel.AddButton.Button.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.Button.Down.OutlineAlpha = 255
      LookAndFeel.AddButton.Button.Hot.StartColor = clBlack
      LookAndFeel.AddButton.Button.Hot.StopColor = clBlack
      LookAndFeel.AddButton.Button.Hot.StartAlpha = 255
      LookAndFeel.AddButton.Button.Hot.StopAlpha = 255
      LookAndFeel.AddButton.Button.Hot.OutlineColor = clGray
      LookAndFeel.AddButton.Button.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.Button.Hot.OutlineAlpha = 255
      LookAndFeel.AddButton.PlusSign.Normal.StartColor = clWhite
      LookAndFeel.AddButton.PlusSign.Normal.StopColor = clWhite
      LookAndFeel.AddButton.PlusSign.Normal.StartAlpha = 255
      LookAndFeel.AddButton.PlusSign.Normal.StopAlpha = 255
      LookAndFeel.AddButton.PlusSign.Normal.OutlineColor = clGray
      LookAndFeel.AddButton.PlusSign.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.PlusSign.Normal.OutlineAlpha = 255
      LookAndFeel.AddButton.PlusSign.Down.StartColor = clWhite
      LookAndFeel.AddButton.PlusSign.Down.StopColor = clWhite
      LookAndFeel.AddButton.PlusSign.Down.StartAlpha = 255
      LookAndFeel.AddButton.PlusSign.Down.StopAlpha = 255
      LookAndFeel.AddButton.PlusSign.Down.OutlineColor = clGray
      LookAndFeel.AddButton.PlusSign.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.PlusSign.Down.OutlineAlpha = 255
      LookAndFeel.AddButton.PlusSign.Hot.StartColor = clWhite
      LookAndFeel.AddButton.PlusSign.Hot.StopColor = clWhite
      LookAndFeel.AddButton.PlusSign.Hot.StartAlpha = 255
      LookAndFeel.AddButton.PlusSign.Hot.StopAlpha = 255
      LookAndFeel.AddButton.PlusSign.Hot.OutlineColor = clGray
      LookAndFeel.AddButton.PlusSign.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.PlusSign.Hot.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Normal.StartColor = clSilver
      LookAndFeel.ScrollButtons.Button.Normal.StopColor = clSilver
      LookAndFeel.ScrollButtons.Button.Normal.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Normal.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Normal.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Button.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Normal.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Down.StartColor = clSilver
      LookAndFeel.ScrollButtons.Button.Down.StopColor = clSilver
      LookAndFeel.ScrollButtons.Button.Down.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Down.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Down.OutlineColor = clBlack
      LookAndFeel.ScrollButtons.Button.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Down.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Hot.StartColor = clBlack
      LookAndFeel.ScrollButtons.Button.Hot.StopColor = clBlack
      LookAndFeel.ScrollButtons.Button.Hot.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Hot.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Hot.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Button.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Hot.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Disabled.StartColor = clSilver
      LookAndFeel.ScrollButtons.Button.Disabled.StopColor = clSilver
      LookAndFeel.ScrollButtons.Button.Disabled.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Disabled.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Disabled.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Button.Disabled.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Disabled.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Normal.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Normal.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Normal.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Normal.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Normal.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Normal.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Down.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Down.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Down.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Down.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Down.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Down.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Hot.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Hot.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Hot.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Hot.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Hot.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Hot.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Disabled.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Disabled.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Disabled.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Disabled.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineAlpha = 255
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semibold'
      Font.Style = [fsBold]
      PopupMenu = ppmTabs
      TabOrder = 0
    end
  end
  object pnlStatusBar: TPanel
    Left = 0
    Top = 579
    Width = 997
    Height = 20
    Align = alBottom
    BevelEdges = []
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object shpLine: TShape
      Left = 0
      Top = 0
      Width = 997
      Height = 1
      Align = alTop
      Pen.Color = clScrollBar
    end
    object pnlSourceName: TPanel
      AlignWithMargins = True
      Left = 89
      Top = 2
      Width = 304
      Height = 17
      Hint = 'Source'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlDelta: TPanel
      AlignWithMargins = True
      Left = 776
      Top = 2
      Width = 110
      Height = 17
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alRight
      BevelEdges = [beLeft]
      BevelKind = bkFlat
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
    end
    object pbrCPU: TKPercentProgressBar
      AlignWithMargins = True
      Left = 946
      Top = 2
      Width = 50
      Height = 17
      Hint = 'CPU load'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alRight
      BarColor = clGreen
      Frame = False
      Min = 0
      Max = 100
    end
    object pnlMessageCount: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 2
      Width = 86
      Height = 17
      Hint = 'Messages received'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 3
    end
    object pnlMemory: TPanel
      AlignWithMargins = True
      Left = 888
      Top = 2
      Width = 56
      Height = 17
      Hint = 'Memory used'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alRight
      BevelEdges = [beLeft, beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 4
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 997
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 120
    Top = 48
    object actCenterToScreen: TAction
      Caption = 'Center to screen'
      Hint = 'Center form on main screen.'
      ImageIndex = 0
      OnExecute = actCenterToScreenExecute
    end
    object actShowVersion: TAction
      Caption = 'actShowVersion'
      OnExecute = actShowVersionExecute
    end
  end
  object imlMain: TImageList
    Left = 208
    Top = 48
    Bitmap = {
      494C010101000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000180A071AE96341FFE96341FFE96341FF180A071A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000038180F40DD5D3BFF38180F4000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A3F26C0CF55
      33FFCF5533FFCF5533FFCF5533FFDC8067FFCF5533FFCF5533FFCF5533FFCF55
      33FF9A3F26C00000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C14D2BFFEDED
      EDFFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFEDED
      EDFFC14D2BFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001207041A00000000B54725FFEEEE
      EEFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFEEEE
      EEFFB54725FF000000001207041A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AD4422FF2B110940AD4422FFF4F4
      F4FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF4F4
      F4FFAD4422FF2B110940AD4422FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AB4523FFAB4523FFC0745AFFFAFA
      FAFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFFAFA
      FAFFC0745AFFAB4523FFAB4523FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B04C2AFF2C130A40B04C2AFFFFFF
      FFFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFFFF
      FFFFB04C2AFF2C130A40B04C2AFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001208051A00000000BA5735FFBBBB
      BBFFB8B8B8FFB5B5B5FFB1B1B1FFADADADFFA9A9A9FFA5A5A5FFA1A1A1FF9E9E
      9EFFBA5735FF000000001208051A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AE5739DED8BB
      B1FFD9D9D9FFD3D4D4FFCCCDCDFFC7C7C7FFC7C5C5FFCAC5C5FFD2C8C8FFD5AE
      A4FFAE5739DE0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000045241953BB63
      46DED77250FFD77250FFD77250FFE2967DFFD77250FFD77250FFD77250FFBB63
      46DE452419530000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000391F1740E37E5CFF391F174000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000180E0A1AED8765FFED8765FFED8765FF180E0A1A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
  object tskbrMain: TTaskbar
    TaskBarButtons = <
      item
        Action = actCenterToScreen
        Hint = 'Center form on main screen.'
        Icon.Data = {
          0000010001001010200000000000680400001600000028000000100000002000
          0000010020000000000000040000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000180A071AE96341FFE96341FFE963
          41FF180A071A0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000038180F40DD5D3BFF3818
          0F40000000000000000000000000000000000000000000000000000000000000
          0000000000009A3F26C0CF5533FFCF5533FFCF5533FFCF5533FFDC8067FFCF55
          33FFCF5533FFCF5533FFCF5533FF9A3F26C00000000000000000000000000000
          000000000000C14D2BFFEDEDEDFFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
          E9FFE9E9E9FFE9E9E9FFEDEDEDFFC14D2BFF0000000000000000000000001207
          041A00000000B54725FFEEEEEEFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
          E8FFE8E8E8FFE8E8E8FFEEEEEEFFB54725FF000000001207041A00000000AD44
          22FF2B110940AD4422FFF4F4F4FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0
          F0FFF0F0F0FFF0F0F0FFF4F4F4FFAD4422FF2B110940AD4422FF00000000AB45
          23FFAB4523FFC0745AFFFAFAFAFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
          F8FFF8F8F8FFF8F8F8FFFAFAFAFFC0745AFFAB4523FFAB4523FF00000000B04C
          2AFF2C130A40B04C2AFFFFFFFFFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
          FEFFFEFEFEFFFEFEFEFFFFFFFFFFB04C2AFF2C130A40B04C2AFF000000001208
          051A00000000BA5735FFBBBBBBFFB8B8B8FFB5B5B5FFB1B1B1FFADADADFFA9A9
          A9FFA5A5A5FFA1A1A1FF9E9E9EFFBA5735FF000000001208051A000000000000
          000000000000AE5739DED8BBB1FFD9D9D9FFD3D4D4FFCCCDCDFFC7C7C7FFC7C5
          C5FFCAC5C5FFD2C8C8FFD5AEA4FFAE5739DE0000000000000000000000000000
          00000000000045241953BB6346DED77250FFD77250FFD77250FFE2967DFFD772
          50FFD77250FFD77250FFBB6346DE452419530000000000000000000000000000
          00000000000000000000000000000000000000000000391F1740E37E5CFF391F
          1740000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000180E0A1AED8765FFED8765FFED87
          65FF180E0A1A0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000}
      end
      item
        Hint = 
          'Shows the main application form always in front of any other app' +
          'lication.'
        Icon.Data = {
          0000010001001010200000000000680400001600000028000000100000002000
          0000010020000000000000040000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000030303890000003100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000505052D4A4A4A9D0000004C000000030000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000003030341A7A7A7CB02020266000000110000000000009B480000
          98CC000094990000000000000000000000000000000000000000000000000000
          0000000000000000000300000055D6D6D6F70000007B000052690000A0CC5454
          EEFF000094CC0000000000000000000000000000000000000000000000000000
          000000000000000000000000000D00000067C4C4C4FF0000A7CC4A4ADCFF4E4E
          E8FF000096CC0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000059610000B1CC7171EBFF3535CEFF4040
          D8FF00009CC90000000000000000000000000000000000000000000000000000
          000000000000000000000000BB480000B7CC9B9BFEFF5F5FEEFF4848DCFF4646
          D5FF0000A9C40000944800000000000000000000000000000000000000000000
          000000000000000000000000BFCCA8A8FFFF8E8EFFFF8686FCFF7575F2FF5C5C
          ECFF5959E4FF000099CB00009448000000000000000000000000000000000000
          000000000000000000000101E3810000E5AB0000E5AB0000E2AD0000DAB09C9C
          FFFF5C5CECFF5959E4FF0000A4C600009CC9000096CC00009448000000000000
          00000000000000000000000000000000000000000000000000000000E53C0000
          E5AB9C9CFFFF5C5CECFF4242D5FF4B4BE4FF5555EFFF000096CC000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          E53C0000DCAF8080F9FF4E4EE1FF5353DCFF0000A9C400009647000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000E2AD8D8DFFFF9393FCFF0000C7B80000B64300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000E5ABA8A8FFFF0000DAB00000CA3F0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000E53C0000E5AB0000E13D000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000FFFF
          00009FFF000087FF0000C23F0000C03F0000E03F0000F03F0000E01F0000E00F
          0000E0010000FE010000FF010000FF830000FF870000FF8F0000FFFF0000}
      end>
    OverlayIcon.Data = {
      0000010001004040000001002000284200001600000028000000400000008000
      000001002000000000000040000064000000640000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000050000
      000E000000130000001300000013000000130000001300000013000000130000
      0013000000130000001300000013000000130000001300000013000000130000
      0013000000130000001300000013000000130000001300000013000000130000
      0013000000130000001300000013000000130000001300000013000000130000
      0013000000130000001300000013000000130000001300000013000000130000
      001300000013000000130000001300000013000000130000000E00000005FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000E0000
      002B0000003A0000003A0000003A0000003A0000003A0000003A0000003A0000
      003A0000003A0000003A0000003A0000003A0000003A0000003A0000003A0000
      003A0000003A0000003A0000003A0000003A0000003A0000003A0000003A0000
      003A0000003A0000003A0000003A0000003A0000003A0000003A0000003A0000
      003A0000003A0000003A0000003A0000003A0000003A0000003A0000003A0000
      003A0000003A0000003A0000003A0000003A0000003A0000002B0000000EFFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007D7D7DFF7D7D
      7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D
      7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D
      7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D
      7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D
      7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D
      7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF7D7D7DFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080FFFFFF
      FFFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFADBBD7FFF8F8F8FFADBBD7FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFFAFAFAFFF2F2F2FF808080FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484FFFFFF
      FFFFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFADBBD7FFE4E4E4FFADBBD7FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFEBEBEBFFF2F2F2FF848484FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00878787FFFFFF
      FFFFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFADBBD7FFE4E4E4FFADBBD7FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFEBEBEBFFF2F2F2FF878787FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008B8B8BFFFFFF
      FFFFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFADBBD7FFE4E4E4FFADBBD7FFE4E4
      E4FFE4E4E4FFE4E4E4FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5
      E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFEBEBEBFFF2F2F2FF8B8B8BFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008E8E8EFFE2C1
      A3FF8E7F72FF4C4C4CFF84786DFFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FF8E8E8EFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00919191FFFFFF
      FFFF4C4C4CFF4C4C4CFF4C4C4CFFE5E5E5FFADBBD7FFE6E6E6FFADBBD7FFE6E6
      E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FFE6E6E6FFE6E6E6FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE5E5E5FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFEBEBEBFFF2F2F2FF919191FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00949494FFFFFF
      FFFF989898FF4C4C4CFF999999FFE6E6E6FFADBBD7FFE6E6E6FFADBBD7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE7E7E7FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE5E5E5FFE5E5
      E5FFE5E5E5FFE5E5E5FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFEBEBEBFFF2F2F2FF949494FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00969696FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FF969696FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00999999FFFFFF
      FFFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFADBBD7FFE8E8E8FFADBBD7FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE5E5E5FFE5E5E5FFE5E5E5FFE4E4
      E4FFE4E4E4FFE4E4E4FFEBEBEBFFF2F2F2FF999999FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009C9C9CFFFFFF
      FFFFE7E7E7FFE8E8E8FFE8E8E8FFE8E8E8FFADBBD7FFE9E9E9FFADBBD7FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE6E6E6FFE6E6E6FFE6E6E6FFE5E5E5FFE5E5
      E5FFE5E5E5FFE4E4E4FFEBEBEBFFF2F2F2FF9C9C9CFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009E9E9EFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FF9E9E9EFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A1A1A1FFFFFF
      FFFFE9E9E9FFE9E9E9FFE9E9E9FFEAEAEAFFADBBD7FFEAEAEAFFADBBD7FFEAEA
      EAFFEAEAEAFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEB
      EBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEAEAEAFFEAEA
      EAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE8E8E8FFE8E8E8FFE8E8E8FFE7E7E7FFE7E7E7FFE7E7E7FFE6E6
      E6FFE6E6E6FFE6E6E6FFECECECFFF2F2F2FFA1A1A1FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A3A3A3FFFFFF
      FFFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFADBBD7FFEBEBEBFFADBBD7FFEBEB
      EBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFECECECFFECECECFFECEC
      ECFFECECECFFECECECFFECECECFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEB
      EBFFEBEBEBFFEBEBEBFFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFEAEA
      EAFFE9E9E9FFE9E9E9FFE9E9E9FFE8E8E8FFE8E8E8FFE8E8E8FFE7E7E7FFE7E7
      E7FFE7E7E7FFE6E6E6FFECECECFFF2F2F2FFA3A3A3FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A5A5A5FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFA5A5A5FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A7A7A7FFFFFF
      FFFFEBEBEBFFEBEBEBFFEBEBEBFFECECECFFADBBD7FFECECECFFADBBD7FFECEC
      ECFFECECECFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDED
      EDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFECEC
      ECFFECECECFFECECECFFECECECFFECECECFFECECECFFEBEBEBFFEBEBEBFFEBEB
      EBFFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9E9FFE9E9E9FFE8E8
      E8FFE8E8E8FFE8E8E8FFEDEDEDFFF3F3F3FFA7A7A7FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A9A9A9FFFFFF
      FFFFECECECFFECECECFFECECECFFECECECFFADBBD7FFEDEDEDFFADBBD7FFEDED
      EDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEEEEEEFFEEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDED
      EDFFEDEDEDFFEDEDEDFFEDEDEDFFECECECFFECECECFFECECECFFECECECFFECEC
      ECFFEBEBEBFFEBEBEBFFEBEBEBFFEAEAEAFFEAEAEAFFEAEAEAFFE9E9E9FFE9E9
      E9FFE9E9E9FFE8E8E8FFEEEEEEFFF3F3F3FFA9A9A9FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00ABABABFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFABABABFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00ACACACFFFFFF
      FFFFEDEDEDFFEDEDEDFFEDEDEDFFEEEEEEFFADBBD7FFEEEEEEFFADBBD7FFEEEE
      EEFFEEEEEEFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEF
      EFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEDEDEDFFEDEDEDFFEDED
      EDFFEDEDEDFFECECECFFECECECFFECECECFFEBEBEBFFEBEBEBFFEBEBEBFFEAEA
      EAFFEAEAEAFFEAEAEAFFEFEFEFFFF4F4F4FFACACACFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AEAEAEFFFFFF
      FFFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFADBBD7FFEFEFEFFFADBBD7FFEFEF
      EFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFF0F0
      F0FFF0F0F0FFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEF
      EFFFEFEFEFFFEFEFEFFFEFEFEFFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEDED
      EDFFEDEDEDFFEDEDEDFFEDEDEDFFECECECFFECECECFFECECECFFEBEBEBFFEBEB
      EBFFEBEBEBFFEAEAEAFFEFEFEFFFF4F4F4FFAEAEAEFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AFAFAFFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFAFAFAFFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B1B1B1FFFFFF
      FFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFADBBD7FFF0F0F0FFADBBD7FFF0F0
      F0FFF0F0F0FFF0F0F0FFF0F0F0FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1
      F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF0F0F0FFF0F0F0FFF0F0
      F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFEFEFEFFFEFEFEFFFEFEFEFFFEFEF
      EFFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEDEDEDFFEDEDEDFFECECECFFECEC
      ECFFECECECFFEBEBEBFFF0F0F0FFF5F5F5FFB1B1B1FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B2B2B2FFFFFF
      FFFFEFEFEFFFF0F0F0FFF0F0F0FFF0F0F0FFADBBD7FFF0F0F0FFADBBD7FFF1F1
      F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1
      F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1
      F1FFF1F1F1FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFEFEFEFFFEFEF
      EFFFEFEFEFFFEFEFEFFFEEEEEEFFEEEEEEFFEEEEEEFFEDEDEDFFEDEDEDFFEDED
      EDFFECECECFFECECECFFF0F0F0FFF5F5F5FFB2B2B2FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B3B3B3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFB3B3B3FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B4B4B4FFFFFF
      FFFFF0F0F0FFF1F1F1FFF1F1F1FFF1F1F1FFADBBD7FFF1F1F1FFADBBD7FFF2F2
      F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF0F0
      F0FFF0F0F0FFF0F0F0FFEFEFEFFFEFEFEFFFEFEFEFFFEEEEEEFFEEEEEEFFEEEE
      EEFFEDEDEDFFEDEDEDFFF1F1F1FFF6F6F6FFB4B4B4FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B6B6B6FFFFFF
      FFFFF1F1F1FFF1F1F1FFF1F1F1FFF2F2F2FFADBBD7FFF2F2F2FFADBBD7FFF2F2
      F2FFF2F2F2FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3
      F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF2F2
      F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF1F1
      F1FFF1F1F1FFF0F0F0FFF0F0F0FFF0F0F0FFEFEFEFFFEFEFEFFFEFEFEFFFEEEE
      EEFFEEEEEEFFEDEDEDFFF2F2F2FFF6F6F6FFB6B6B6FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B7B7B7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFB7B7B7FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B7B7B7FFFFFF
      FFFFF2F2F2FFF2F2F2FFF2F2F2FFF3F3F3FFADBBD7FFF3F3F3FFADBBD7FFF3F3
      F3FFF3F3F3FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF3F3
      F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2F2FFF2F2
      F2FFF2F2F2FFF1F1F1FFF1F1F1FFF1F1F1FFF0F0F0FFF0F0F0FFF0F0F0FFEFEF
      EFFFEFEFEFFFEEEEEEFFF2F2F2FFF6F6F6FFB7B7B7FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B8B8B8FFFFFF
      FFFF959595FF4C4C4CFF8B8B8BFFF3F3F3FFADBBD7FFF4F4F4FFADBBD7FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF5F5
      F5FFF5F5F5FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF2F2
      F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF1F1F1FFF0F0F0FFF0F0F0FFF0F0
      F0FFEFEFEFFFEFEFEFFFF3F3F3FFF7F7F7FFB8B8B8FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B9B9B9FFE2C1
      A3FF4C4C4CFF4C4C4CFF4C4C4CFFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFB9B9B9FF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BABABAFFFFFF
      FFFF9F9F9FFF4C4C4CFFA0A0A0FFF4F4F4FFADBBD7FFF4F4F4FFADBBD7FFF5F5
      F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5
      F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5
      F5FFF5F5F5FFF5F5F5FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF3F3
      F3FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF1F1
      F1FFF0F0F0FFF0F0F0FFF3F3F3FFF7F7F7FFBABABAFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BBBBBBFFFFFF
      FFFFF4F4F4FFF4F4F4FFF4F4F4FFF5F5F5FFADBBD7FFF5F5F5FFADBBD7FFF5F5
      F5FFF5F5F5FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF5F5
      F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1
      F1FFF1F1F1FFF0F0F0FFF4F4F4FFF7F7F7FFBBBBBBFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BBBBBBFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFBBBBBBFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BCBCBCFFFFFF
      FFFFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFADBBD7FFF6F6F6FFADBBD7FFF6F6
      F6FFF6F6F6FFF6F6F6FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF6F6F6FFF6F6F6FFF6F6
      F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5
      F5FFF4F4F4FFF4F4F4FFF4F4F4FFF3F3F3FFF3F3F3FFF3F3F3FFF2F2F2FFF2F2
      F2FFF1F1F1FFF1F1F1FFF4F4F4FFF8F8F8FFBCBCBCFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BCBCBCFFFFFF
      FFFFF5F5F5FFF5F5F5FFF6F6F6FFF6F6F6FFADBBD7FFF6F6F6FFADBBD7FFF7F7
      F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7
      F7FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6F6FFF5F5F5FFF5F5
      F5FFF5F5F5FFF4F4F4FFF4F4F4FFF4F4F4FFF3F3F3FFF3F3F3FFF3F3F3FFF2F2
      F2FFF2F2F2FFF1F1F1FFF5F5F5FFF8F8F8FFBCBCBCFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BDBDBDFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFBDBDBDFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BDBDBDFFFFFF
      FFFFF6F6F6FFF6F6F6FFF6F6F6FFF7F7F7FFADBBD7FFF7F7F7FFADBBD7FFF7F7
      F7FFF7F7F7FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF7F7
      F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF6F6F6FFF6F6F6FFF6F6F6FFF6F6
      F6FFF5F5F5FFF5F5F5FFF5F5F5FFF4F4F4FFF4F4F4FFF4F4F4FFF3F3F3FFF3F3
      F3FFF3F3F3FFF2F2F2FFF5F5F5FFF8F8F8FFBDBDBDFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BEBEBEFFFFFF
      FFFFF6F6F6FFF6F6F6FFF7F7F7FFF7F7F7FFADBBD7FFF7F7F7FFADBBD7FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF6F6F6FFF6F6
      F6FFF6F6F6FFF5F5F5FFF5F5F5FFF5F5F5FFF4F4F4FFF4F4F4FFF4F4F4FFF3F3
      F3FFF3F3F3FFF2F2F2FFF5F5F5FFF8F8F8FFBEBEBEFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BEBEBEFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFBEBEBEFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BEBEBEFFFFFF
      FFFFF7F7F7FFF7F7F7FFF7F7F7FFF8F8F8FFADBBD7FFF8F8F8FFADBBD7FFF8F8
      F8FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF7F7F7FFF7F7F7FFF7F7
      F7FFF6F6F6FFF6F6F6FFF6F6F6FFF5F5F5FFF5F5F5FFF5F5F5FFF4F4F4FFF4F4
      F4FFF3F3F3FFF3F3F3FFF6F6F6FFF9F9F9FFBEBEBEFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BFBFBFFFFFFF
      FFFFF7F7F7FFF7F7F7FFF8F8F8FFF8F8F8FFADBBD7FFF8F8F8FFADBBD7FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF7F7F7FFF7F7
      F7FFF7F7F7FFF6F6F6FFF6F6F6FFF6F6F6FFF5F5F5FFF5F5F5FFF5F5F5FFF4F4
      F4FFF4F4F4FFF3F3F3FFF6F6F6FFF9F9F9FFBFBFBFFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BFBFBFFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFBFBFBFFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BFBFBFFFFFFF
      FFFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFADBBD7FFF9F9F9FFADBBD7FFF9F9
      F9FFF9F9F9FFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFF7F7F7FFF7F7F7FFF7F7F7FFF6F6F6FFF6F6F6FFF6F6F6FFF5F5F5FFF5F5
      F5FFF4F4F4FFF4F4F4FFF6F6F6FFF9F9F9FFBFBFBFFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BFBFBFFFFFFF
      FFFFF8F8F8FFF8F8F8FFF8F8F8FFF9F9F9FFADBBD7FFF9F9F9FFADBBD7FFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF8F8F8FFF8F8F8FFF8F8
      F8FFF8F8F8FFF7F7F7FFF7F7F7FFF7F7F7FFF6F6F6FFF6F6F6FFF5F5F5FFF5F5
      F5FFF5F5F5FFF4F4F4FFF7F7F7FFF9F9F9FFBFBFBFFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BFBFBFFFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFBFBFBFFF0000003A00000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BFBFBFFFFFFF
      FFFFF8F8F8FFF9F9F9FFF9F9F9FFF9F9F9FFADBBD7FFFAFAFAFFADBBD7FFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF6F6
      F6FFF1F1F1FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF6F6F6FFF6F6F6FFF5F5
      F5FFF5F5F5FFF4F4F4FFF7F7F7FFF9F9F9FFBFBFBFFF0000003800000013FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFADBBD7FFFAFAFAFFADBBD7FFFAFA
      FAFFFAFAFAFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9F9FFF9F9F9FFF9F9F9FFF2F2
      F2FFC7C7C7FFB7B7B7FFBEBEBEFFD0D0D0FFDCDCDCFFE2E2E2FFEAEAEAFFEDED
      EDFFECECECFFEBEBEBFFEFEFEFFFE9E9E9FFC0C0C0FF0000002F0000000EFFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFDEBE
      A0FFA78E79FF7C6A59FF736253FF776556FF7D6B5AFF83705EFF8F7A67FF9B84
      70FFAD9681FFCFBDAEFFE4DED8FFCCCCCCFFBABABAE60000001C00000006FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFF9F9F9FFF9F9F9FFF9F9F9FFFAFAFAFFADBBD7FFFAFAFAFFADBBD7FFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9F9FFF9F9F9FFF9F9
      F9FFBBBBBBFFABABABFFC7C7C7FFB6B6B6FFB6B6B6FFC7C7C7FFDADADAFFE2E2
      E2FFEFEFEFFFE6E6E6FFD1D1D1FFC0C0C0FF7979794C0000000A00000001FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFF9F9F9FFF9F9F9FFFAFAFAFFFAFAFAFFADBBD7FFFAFAFAFFADBBD7FFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9F9FFF9F9
      F9FFC6C6C6FFBCBCBCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FFEFEF
      EFFFE5E5E5FFD5D5D5FFC0C0C0FF989898790000000E0000000200000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFADBBD7FFE2C1A3FFADBBD7FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
      A3FFBBA087FFB8AFA8FFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FFEFEFEFFFE4E4
      E4FFD4D4D4FFC0C0C0FFAAAAAAA100000010000000030000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFF9F9F9FFFAFAFAFFFAFAFAFFFAFAFAFFADBBD7FFFBFBFBFFADBBD7FFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9F9FFF9F9
      F9FFD0D0D0FFBEBEBEFFFCFCFCFFFCFCFCFFF8F8F8FFEFEFEFFFE4E4E4FFD3D3
      D3FFC0C0C0FFAFAFAFAF0000001100000003000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFF989898FF4C4C4CFF8E8E8EFFFAFAFAFFADBBD7FFFBFBFBFFADBBD7FFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9
      F9FFD1D1D1FFDDDDDDFFFCFCFCFFF8F8F8FFEFEFEFFFE4E4E4FFDADADAFFC0C0
      C0FFACACACB1000000140000000400000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFF4C4C4CFF4C4C4CFF4C4C4CFFFAFAFAFFADBBD7FFFBFBFBFFADBBD7FFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9
      F9FFD3D3D3FFE6E6E6FFF8F8F8FFEFEFEFFFE4E4E4FFDDDDDDFFC0C0C0FFB8B8
      B8D853535325000000050000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFA2A2A2FF4C4C4CFFA3A3A3FFFAFAFAFFADBBD7FFFBFBFBFFADBBD7FFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9
      F9FFD8D8D8FFF8F8F8FFEFEFEFFFE4E4E4FFD8D8D8FFC0C0C0FFB8B8B8D85555
      552400000005000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFF9F9F9FFFAFAFAFFFAFAFAFFFAFAFAFFADBBD7FFFBFBFBFFADBBD7FFFBFB
      FBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF9F9
      F9FFE1E1E1FFEFEFEFFFE5E5E5FFD4D4D4FFC0C0C0FFB5B5B5CA5D5D5D210000
      000400000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFF9F9F9FFFAFAFAFFFAFAFAFFFAFAFAFFADBBD7FFFBFBFBFFADBBD7FFFBFB
      FBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF7F7
      F7FFE9E9E9FFE7E7E7FFD6D6D6FFC0C0C0FFAFAFAFAE00000010000000030000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFF9F9F9FFFAFAFAFFFAFAFAFFFAFAFAFFADBBD7FFFBFBFBFFADBBD7FFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFBFBFBFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFF3F3
      F3FFE8E8E8FFD5D5D5FFC0C0C0FFAEAEAE9E0000000E00000003000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFFFFF
      FFFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFADBBD7FFFEFEFEFFADBBD7FFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFAFAFAFFE8E8
      E8FFCDCDCDFFC0C0C0FFA3A3A3710000000A0000000200000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFBEBEBEE19C9C9C3B00000006000000010000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000}
    TabProperties = []
    Left = 32
    Top = 48
  end
  object tmrPoll: TTimer
    Interval = 500
    OnTimer = tmrPollTimer
    Left = 32
    Top = 112
  end
  object ppmTabs: TPopupMenu
    Left = 112
    Top = 104
    object mniCloseOthers: TMenuItem
      Caption = 'Close all others'
    end
  end
end
