object HexWindow: THexWindow
  Tag = 1
  Left = 59
  Top = 723
  Width = 436
  Height = 265
  BorderIcons = [biSystemMenu]
  Caption = 'HexWindow'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object MPHexEditor1: TMPHexEditor
    Left = 4
    Top = 32
    Width = 419
    Height = 172
    Cursor = crIBeam
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ScrollBars = ssVertical
    TabOrder = 3
    OnKeyDown = MPHexEditor1KeyDown
    Anchors = [akLeft, akTop, akRight, akBottom]
    BytesPerRow = 16
    BytesPerColumn = 1
    Translation = tkAsIs
    OffsetFormat = '1%5!0a:|'
    Colors.Background = clWindow
    Colors.ChangedBackground = clMenu
    Colors.ChangedText = clMaroon
    Colors.CursorFrame = clNavy
    Colors.Offset = clBlack
    Colors.OddColumn = clBlack
    Colors.EvenColumn = clNavy
    Colors.CurrentOffsetBackground = clBtnShadow
    Colors.OffsetBackGround = clBtnFace
    Colors.CurrentOffset = clBtnHighlight
    Colors.Grid = clBtnFace
    FocusFrame = False
    AllowInsertMode = False
    DrawGridLines = False
    HideSelection = True
    GutterWidth = 42
    MaxUndo = 1024
    OnTopLeftChanged = MPHexEditor1TopLeftChanged
    OnChange = MPHexEditor1Change
  end
  object Button1: TButton
    Left = 374
    Top = 208
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 322
    Top = 208
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 4
    Top = 209
    Width = 314
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvLowered
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 4
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
  end
  object Button3: TButton
    Left = 153
    Top = 4
    Width = 32
    Height = 21
    Caption = 'Go'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button3Click
  end
  object ComboBox1: TComboBox
    Left = 4
    Top = 4
    Width = 145
    Height = 22
    ItemHeight = 14
    TabOrder = 0
    OnChange = ComboBox1Change
    OnDropDown = ComboBox1Click
    OnKeyDown = Edit1KeyDown
  end
  object Button4: TButton
    Left = 188
    Top = 4
    Width = 45
    Height = 21
    Caption = 'Find'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 373
    Top = 4
    Width = 45
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Help'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = Button5Click
  end
end
