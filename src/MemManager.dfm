object MemManagerForm: TMemManagerForm
  Left = 316
  Top = 359
  Width = 728
  Height = 344
  Caption = 'MemManagerForm'
  Color = clBtnFace
  Constraints.MinHeight = 281
  Constraints.MinWidth = 570
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object FastIMG1: TFastIMG
    Left = 10
    Top = 128
    Width = 661
    Height = 100
    Transparent = False
    Picture = '(None)'
    AutoSize = False
    SizeMode = smGDI
    DrawStyle = dsDraw
    DIBLeft = 0
    DIBTop = 0
    OnMouseDown = FastIMG1MouseDown
    OnMouseMove = FastIMG1MouseMove
    OnMouseUp = FastIMG1MouseUp
  end
  object Button1: TButton
    Left = 608
    Top = 230
    Width = 92
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 1
    OnClick = Button1Click
  end
  object txtaddress: TEdit
    Left = 11
    Top = 232
    Width = 149
    Height = 21
    Anchors = [akLeft, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    TabOrder = 2
    Text = '0'
  end
  object MPHexEditor1: TMPHexEditor
    Left = 464
    Top = 0
    Width = 240
    Height = 121
    Cursor = crIBeam
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ScrollBars = ssVertical
    TabOrder = 3
    Anchors = [akLeft, akTop, akRight, akBottom]
    BytesPerRow = 16
    BytesPerColumn = 1
    Translation = tkAsIs
    OffsetFormat = '1%5!0a:|'
    Colors.Background = clWindow
    Colors.ChangedBackground = 11075583
    Colors.ChangedText = clMaroon
    Colors.CursorFrame = clNavy
    Colors.Offset = clBlack
    Colors.OddColumn = clBlue
    Colors.EvenColumn = clNavy
    Colors.CurrentOffsetBackground = clBtnShadow
    Colors.OffsetBackGround = clBtnFace
    Colors.CurrentOffset = clBtnHighlight
    Colors.Grid = clBtnFace
    FocusFrame = False
    AllowInsertMode = False
    DrawGridLines = False
    GutterWidth = 50
    MaxUndo = 1024
  end
  object Button2: TButton
    Left = 507
    Top = 232
    Width = 92
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = 'Button2'
    TabOrder = 4
    OnClick = Button2Click
  end
  object ListView1: TListView
    Left = 10
    Top = 2
    Width = 444
    Height = 119
    Anchors = [akLeft, akTop, akBottom]
    Checkboxes = True
    Columns = <
      item
        MaxWidth = 20
        MinWidth = 20
        Width = 20
      end
      item
        Caption = 'Filename'
        MinWidth = 5
        Width = 123
      end
      item
        Caption = 'Type'
        Width = 62
      end
      item
        Caption = 'Location'
        Width = 74
      end
      item
        Caption = 'Length'
        Width = 74
      end
      item
        Caption = 'Status'
        Width = 80
      end>
    GridLines = True
    RowSelect = True
    PopupMenu = PopupMenu1
    SortType = stText
    TabOrder = 5
    ViewStyle = vsReport
    OnChange = ListView1Change
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 192
    object File1: TMenuItem
      Caption = '&File'
      object ImportBinary1: TMenuItem
        Caption = '&Import binary from file...'
      end
      object Grabcurrentscreen1: TMenuItem
        Caption = '&Grab current screen'
      end
      object Grabfrommemory1: TMenuItem
        Caption = 'Grab from &memory...'
      end
      object DeclareNewBlock1: TMenuItem
        Caption = 'Declare New Block...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Savethismanagement1: TMenuItem
        Caption = '&Save this management...'
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Cut1: TMenuItem
        Caption = '&Cut'
      end
      object Copy1: TMenuItem
        Caption = 'C&opy'
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
      end
      object Delete1: TMenuItem
        Caption = '&Delete'
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object Scale1: TMenuItem
        Caption = '&Toolbar'
      end
    end
    object Tools1: TMenuItem
      Caption = 'Tools'
      object SplitSelected1: TMenuItem
        Caption = 'Split Selected'
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 40
    Top = 192
    object Insertintomemory1: TMenuItem
      Caption = 'Insert into cursor'
      OnClick = Insertintomemory1Click
    end
    object Rewrite1: TMenuItem
      Caption = 'Rewrite to same location'
    end
    object Putto1: TMenuItem
      Caption = 'Send to presets'
      object CHARS1: TMenuItem
        Caption = 'CHARS'
      end
      object SCREEN1: TMenuItem
        Caption = 'SCREEN'
      end
      object UDG1: TMenuItem
        Caption = 'UDG'
      end
      object RAMTOP1: TMenuItem
        Caption = '0 REM'
      end
    end
    object NewBlockfromCurrentState1: TMenuItem
      Caption = 'New Block from Current State'
    end
    object Remove1: TMenuItem
      Caption = 'Delete'
      OnClick = Remove1Click
    end
    object Properties1: TMenuItem
      Caption = 'Properties'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Deselect1: TMenuItem
      Caption = 'Deselect'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ImportFile1: TMenuItem
      Caption = 'Import...'
    end
  end
end
