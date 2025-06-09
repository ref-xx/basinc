object ASMEditorWindow: TASMEditorWindow
  Tag = 1
  Left = 244
  Top = 128
  Width = 490
  Height = 352
  BorderIcons = [biSystemMenu]
  Caption = 'BASIC Editor'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Courier New'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Edit2: TEdit
    Left = 76
    Top = 8
    Width = 121
    Height = 22
    TabOrder = 2
    Text = 'Edit2'
    OnKeyPress = Edit2KeyPress
  end
  object StatusBar2: TStatusBar
    Left = 0
    Top = 265
    Width = 471
    Height = 26
    Align = alNone
    Panels = <
      item
        Width = 50
      end
      item
        Text = 'Ln: Col:'
        Width = 50
      end>
  end
  object TabSet1: TTabSet
    Left = 0
    Top = 244
    Width = 471
    Height = 21
    DitherBackground = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    OnChange = TabSet1Change
  end
  object Panel1: TPanel
    Left = 4
    Top = 4
    Width = 465
    Height = 237
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter1: TSplitter
      Left = 73
      Top = 0
      Height = 237
      OnMoved = Splitter1Moved
    end
    object FastIMG1: TFastIMG
      Left = 80
      Top = 0
      Width = 381
      Height = 237
      Transparent = False
      Picture = '(None)'
      AutoSize = False
      SizeMode = smGDI
      DrawStyle = dsDraw
      DIBLeft = 0
      DIBTop = 0
      ParentShowHint = False
      ShowHint = True
      OnMouseDown = FastIMG1MouseDown
      OnMouseMove = FastIMG1MouseMove
      OnMouseUp = FastIMG1MouseUp
    end
    object ListBox1: TListBox
      Left = 0
      Top = 0
      Width = 73
      Height = 237
      Align = alLeft
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 1
      OnDblClick = ListBox1DblClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 320
    OnTimer = Timer1Timer
    Left = 6
    Top = 6
  end
  object MainMenu1: TMainMenu
    Left = 436
    Top = 208
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Tag = 1
        Caption = 'New'
        OnClick = MenuItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Open1: TMenuItem
        Tag = 2
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = MenuItemClick
      end
      object Save1: TMenuItem
        Tag = 3
        Caption = 'Save'
        ShortCut = 16467
        OnClick = MenuItemClick
      end
      object SaveAs1: TMenuItem
        Tag = 4
        Caption = 'Save As...'
        OnClick = MenuItemClick
      end
      object Close1: TMenuItem
        Tag = 22
        Caption = 'Close'
        OnClick = MenuItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Assemble1: TMenuItem
        Caption = 'Assemble'
        object UsePasmo1: TMenuItem
          Tag = 24
          Caption = 'Use Pasmo to compile'
          OnClick = MenuItemClick
        end
        object ToMemory1: TMenuItem
          Tag = 5
          Caption = 'To Memory'
          ShortCut = 16449
          OnClick = MenuItemClick
        end
        object AsDATAStatements1: TMenuItem
          Tag = 19
          Caption = 'As DATA Statements...'
          OnClick = MenuItemClick
        end
        object ToBinaryFile1: TMenuItem
          Tag = 20
          Caption = 'To Binary File...'
          OnClick = MenuItemClick
        end
        object ToTapeblocks1: TMenuItem
          Tag = 21
          Caption = 'To Tape blocks'
          OnClick = MenuItemClick
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Quit1: TMenuItem
        Tag = 6
        Caption = 'Quit'
        OnClick = MenuItemClick
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Cut1: TMenuItem
        Tag = 7
        Caption = 'Cut'
        ShortCut = 16472
        OnClick = MenuItemClick
      end
      object Copy1: TMenuItem
        Tag = 8
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = MenuItemClick
      end
      object Paste1: TMenuItem
        Tag = 9
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = MenuItemClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Tag = 10
        Caption = 'Find...'
        ShortCut = 16454
        OnClick = MenuItemClick
      end
      object FindNext1: TMenuItem
        Tag = 11
        Caption = 'Find Next'
        ShortCut = 114
        OnClick = MenuItemClick
      end
      object Replace1: TMenuItem
        Tag = 12
        Caption = 'Replace...'
        ShortCut = 16466
        OnClick = MenuItemClick
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object StatusBar1: TMenuItem
        Tag = 14
        Caption = 'Status Bar'
        OnClick = MenuItemClick
      end
      object Labellist1: TMenuItem
        Tag = 16
        Caption = 'Symbol Browser'
        OnClick = MenuItemClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object SyntaxHighlightingOptions1: TMenuItem
        Caption = 'Syntax Highlighting Options...'
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Contents1: TMenuItem
        Tag = 18
        Caption = 'Contents'
        ShortCut = 112
        OnClick = MenuItemClick
      end
    end
  end
end
