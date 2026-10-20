object DisplayWindow: TDisplayWindow
  Left = 1241
  Top = 124
  Width = 344
  Height = 326
  Caption = 'BasinC Display Output'
  Color = clBtnFace
  UseDockManager = True
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 248
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 0
    object DisplayIMG: TFastIMG
      Left = 18
      Top = 18
      Width = 320
      Height = 240
      Transparent = False
      Picture = '(None)'
      AutoSize = False
      SizeMode = smGDI
      DrawStyle = dsDraw
      DIBLeft = 0
      DIBTop = 0
      OnMouseDown = DisplayIMGMouseDown
      OnMouseMove = DisplayIMGMouseMove
      OnMouseUp = DisplayIMGMouseUp
    end
  end
  object DisplayStatusBar1: TStatusBar
    Left = 0
    Top = 248
    Width = 328
    Height = 19
    Panels = <
      item
        Style = psOwnerDraw
        Text = 'Normal'
        Width = 24
      end
      item
        Text = 'Border'
        Width = 304
      end>
    SimplePanel = False
    ShowHint = True
    OnDrawPanel = DisplayStatusBar1DrawPanel
  end
  object SpeedStatusTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = SpeedStatusTimerTimer
    Left = 36
    Top = 4
  end
  object PopupMenu1: TPopupMenu
    OwnerDraw = True
    Left = 4
    Top = 4
    object Token1: TMenuItem
      Caption = 'Insert'
      Default = True
      Enabled = False
      OnClick = PopupMenuClick
    end
    object insPrint1: TMenuItem
      Tag = 1
      Caption = 'Insert PRINT AT here'
      Enabled = False
      OnClick = PopupMenuClick
    end
    object InsPeek1: TMenuItem
      Tag = 6
      Caption = 'Insert PEEK here'
      Enabled = False
      OnClick = PopupMenuClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object insPlot1: TMenuItem
      Tag = 2
      Caption = 'Insert PLOT here'
      Enabled = False
      OnClick = PopupMenuClick
    end
    object insDraw1: TMenuItem
      Tag = 3
      Caption = 'Insert DRAW here from Last PLOT'
      Enabled = False
      OnClick = PopupMenuClick
    end
    object InsCircle1: TMenuItem
      Tag = 4
      Caption = 'Insert Circle here'
      Enabled = False
      OnClick = PopupMenuClick
    end
    object InsRect1: TMenuItem
      Tag = 5
      Caption = 'Insert Rectangle here (Plot top left first)'
      Enabled = False
      OnClick = PopupMenuClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Detokenise1: TMenuItem
      Caption = 'Detokenise'
      Enabled = False
      Visible = False
    end
    object Help2: TMenuItem
      Caption = 'Token Help...'
      Enabled = False
      Visible = False
    end
  end
  object MainMenu1: TMainMenu
    Left = 36
    Top = 4
    object WindowSize1: TMenuItem
      Tag = 50
      Caption = 'Display'
      OnClick = MenuItemClick
      object N100320x2401: TMenuItem
        Tag = 1
        Caption = '100% (320x240)'
        Hint = 'Normal (1:1) Window size'
        ShortCut = 16496
        OnClick = MenuItemClick
      end
      object N200640x4801: TMenuItem
        Tag = 2
        Caption = '200% (640x480)'
        Hint = 'Double (2:1) Window Size'
        ShortCut = 16497
        OnClick = MenuItemClick
      end
      object N3001: TMenuItem
        Tag = 8
        Caption = '300% (960x720)'
        OnClick = MenuItemClick
      end
      object N4001: TMenuItem
        Tag = 9
        Caption = '400% (1280x960)'
        OnClick = MenuItemClick
      end
      object N6001920x14401: TMenuItem
        Tag = 10
        Caption = '600% (1920x1440)'
        OnClick = MenuItemClick
      end
      object Custom1: TMenuItem
        Caption = 'Custom ()'
        Hint = 'The current dimensions'
        OnClick = MenuItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Force11Aspect1: TMenuItem
        Tag = 3
        Caption = 'Force 1:1 Aspect'
        Hint = 'Force BASin to maintain correct width/height ratio'
        OnClick = MenuItemClick
      end
      object SnaptoCodeWindow1: TMenuItem
        Tag = 16
        Caption = 'Snap to Code Window'
        OnClick = MenuItemClick
      end
      object KeepOnTop1: TMenuItem
        Tag = 14
        Caption = 'Keep On Top'
        OnClick = MenuItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object SaveImage1: TMenuItem
        Tag = 12
        Caption = 'Save Image...'
        OnClick = MenuItemClick
      end
      object Print1: TMenuItem
        Caption = 'Print...'
        OnClick = Print1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object DisplayFillBorderCheck: TMenuItem
        Tag = 13
        Caption = 'Fill border'
        Checked = True
        OnClick = MenuItemClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Cut1: TMenuItem
        Tag = 4
        Caption = 'Cut'
        Enabled = False
        Hint = 'Remove the selected text and send it to the clipboard'
        ShortCut = 16472
        OnClick = MenuItemClick
      end
      object Copy1: TMenuItem
        Tag = 5
        Caption = 'Copy'
        Enabled = False
        Hint = 'Copy the selected text to the clipboard'
        ShortCut = 16451
        OnClick = MenuItemClick
      end
      object Paste1: TMenuItem
        Tag = 6
        Caption = 'Paste'
        Enabled = False
        Hint = 'Paste text stored on the clipboard to the edit line'
        ShortCut = 16470
        OnClick = MenuItemClick
      end
      object Delete1: TMenuItem
        Tag = 7
        Caption = 'Delete'
        Enabled = False
        Hint = 'Remove the selected text from the edit line'
        ShortCut = 16430
        OnClick = MenuItemClick
      end
      object CopyImageToClipboard1: TMenuItem
        Tag = 15
        Caption = 'Copy Bitmap To Clipboard'
        OnClick = MenuItemClick
      end
    end
    object Emulation1: TMenuItem
      Caption = 'Emulation'
      object FullSpeed1: TMenuItem
        Caption = 'Toggle Full Speed'
        ShortCut = 112
        OnClick = FullSpeed1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object ViewMicrovitecCub1: TMenuItem
        Tag = 17
        Caption = 'View Microvitec Cub'
        OnClick = MenuItemClick
      end
      object GPUFilterMenu1: TMenuItem
        Caption = 'CRT Filter'
        object GPUFilterOff1: TMenuItem
          Tag = 18
          Caption = 'Off'
          GroupIndex = 1
          RadioItem = True
          OnClick = MenuItemClick
        end
        object GPUFilterLottes1: TMenuItem
          Tag = 19
          Caption = 'Lottes CRT'
          GroupIndex = 1
          RadioItem = True
          OnClick = MenuItemClick
        end
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object DisplayHelp1: TMenuItem
        Caption = 'Display Help'
        OnClick = DisplayHelp1Click
      end
    end
  end
end
