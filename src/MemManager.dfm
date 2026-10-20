object MemManagerForm: TMemManagerForm
  Left = 967
  Top = 322
  Width = 741
  Height = 348
  Caption = 'BasinC Memory Manager (Beta)'
  Color = clBtnFace
  Constraints.MinHeight = 281
  Constraints.MinWidth = 570
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    725
    289)
  PixelsPerInch = 96
  TextHeight = 13
  object FastIMG1: TFastIMG
    Left = 8
    Top = 168
    Width = 713
    Height = 81
    Transparent = False
    Picture = '(None)'
    AutoSize = False
    SizeMode = smGDI
    DrawStyle = dsDraw
    DIBLeft = 0
    DIBTop = 0
    PopupMenu = PopupMenu1
    OnClick = FastIMG1Click
    OnMouseDown = FastIMG1MouseDown
    OnMouseMove = FastIMG1MouseMove
    OnMouseUp = FastIMG1MouseUp
  end
  object Button1: TButton
    Left = 643
    Top = 255
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = Button1Click
  end
  object txtaddress: TEdit
    Left = 363
    Top = 259
    Width = 66
    Height = 21
    Anchors = [akRight, akBottom]
    Color = clBtnFace
    TabOrder = 2
    Text = '0'
    OnKeyDown = txtaddressKeyDown
  end
  object MPHexEditor1: TMPHexEditor
    Left = 448
    Top = 0
    Width = 265
    Height = 162
    Cursor = crIBeam
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = PopupMenu2
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
    DrawGridLines = True
    GutterWidth = 50
    MaxUndo = 1024
    ShowRuler = True
  end
  object Button2: TButton
    Left = 561
    Top = 257
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Refresh'
    TabOrder = 4
    OnClick = Button2Click
  end
  object ListView1: TListView
    Left = 8
    Top = 2
    Width = 433
    Height = 160
    Anchors = [akLeft, akTop, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Drag'
        MaxWidth = 40
        MinWidth = 20
        Width = 40
      end
      item
        Caption = 'Filename'
        MinWidth = 5
        Width = 100
      end
      item
        Caption = 'Type'
        Width = 60
      end
      item
        Caption = 'Location'
        Width = 60
      end
      item
        Caption = 'Length'
        Width = 60
      end
      item
        Caption = 'Status'
        Width = 145
      end
      item
        Caption = 'Page'
        Width = 20
      end>
    GridLines = True
    HideSelection = False
    RowSelect = True
    PopupMenu = PopupMenu1
    SortType = stText
    TabOrder = 5
    ViewStyle = vsReport
    OnChange = ListView1Change
    OnClick = ListView1Click
    OnDblClick = ListView1DblClick
    OnEdited = ListView1Edited
  end
  object Button3: TButton
    Left = 433
    Top = 257
    Width = 59
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Zoom In'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 497
    Top = 257
    Width = 59
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Zoom Out'
    TabOrder = 7
    OnClick = Button4Click
  end
  object MainMenu1: TMainMenu
    Left = 152
    Top = 256
    object File1: TMenuItem
      Caption = '&Project'
      object OpenManagement1: TMenuItem
        Tag = 45
        Caption = 'Open Management...'
        Hint = 'Open a management'
        OnClick = MenuClick
      end
      object OpenRecent1: TMenuItem
        Caption = 'Open Recent...'
        Visible = False
      end
      object Savethismanagement1: TMenuItem
        Tag = 44
        Caption = '&Save management as...'
        Hint = 'Save blocks and metadata into a management file'
        OnClick = MenuClick
      end
      object ClearManagement1: TMenuItem
        Tag = 49
        Caption = 'Clear Management'
        Hint = 'Removes all blocks from the manager'
        OnClick = MenuClick
      end
      object N1: TMenuItem
        Tag = 43
        Caption = '-'
      end
      object ExportAddressestoListing1: TMenuItem
        Tag = 50
        Caption = 'Export Addresses to Listing...'
        Hint = 
          'Exports Address and Length information of Unchecked items to BAS' +
          'IC Listing as DATA'
        OnClick = MenuClick
      end
      object RewriteAllBlocks1: TMenuItem
        Tag = 93
        Caption = 'Rewrite All Blocks'
        OnClick = ContextMenuClick
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object ToggleROMView1: TMenuItem
        Caption = 'Toggle ROM View'
        Visible = False
        OnClick = ToggleROMView1Click
      end
      object Close1: TMenuItem
        Tag = 99
        Caption = 'Hide Manager'
        Hint = 'Close this window'
        OnClick = MenuClick
      end
      object Refresh1: TMenuItem
        Tag = 51
        AutoHotkeys = maAutomatic
        Caption = 'Refresh'
        ShortCut = 116
        OnClick = MenuClick
      end
    end
    object Blocks1: TMenuItem
      Caption = 'Create Block'
      object New1: TMenuItem
        Tag = 100
        Caption = 'New...'
        Hint = 'Create a blank memory block'
        OnClick = MenuClick
      end
      object Createzx0depackerblock1: TMenuItem
        Tag = 43
        Caption = 'New ZX0 header...'
        Hint = 'Create a zx0 decompresser block without any data attached to it'
        OnClick = MenuClick
      end
      object NewVarTracker1: TMenuItem
        Tag = 52
        Caption = 'New Var Tracker...'
        OnClick = MenuClick
      end
      object Importbinaryfromfile1: TMenuItem
        Tag = 40
        Caption = '&Import from file...'
        Hint = 'Import a binary file '
        OnClick = MenuClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Grabfrommemory2: TMenuItem
        Tag = 42
        Caption = 'Grab at Cursor...'
        Hint = 'Grab data from memory'
        OnClick = MenuClick
      end
      object Grabcurrentscreen2: TMenuItem
        Tag = 41
        Caption = '&Grab Screen'
        Hint = 'Grab SCREEN$ as block'
        OnClick = MenuClick
      end
      object GrabBasic1: TMenuItem
        Tag = 46
        Caption = 'Grab Basic'
        Hint = 'Grab basic including system variables'
        OnClick = MenuClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Duplicate1: TMenuItem
        Tag = 15
        Caption = 'Duplicate Selected'
        Hint = 'Duplicate selected block'
        OnClick = ContextMenuClick
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object Compresszx01: TMenuItem
        Tag = 19
        Caption = 'Pack'
        Hint = 'Compress selected block without ZX0 block'
        OnClick = ContextMenuClick
      end
      object Compressandembeddepacker1: TMenuItem
        Tag = 18
        Caption = 'Pack with depacker...'
        Hint = 
          'Pack ZX0 block with embedded depacker creating a fixed position ' +
          'block'
        OnClick = ContextMenuClick
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Cut1: TMenuItem
        Tag = 10
        Caption = '&Cut'
        OnClick = ContextMenuClick
      end
      object Copy1: TMenuItem
        Tag = 11
        Caption = 'C&opy'
        OnClick = ContextMenuClick
      end
      object Paste1: TMenuItem
        Tag = 12
        Caption = '&Paste'
        OnClick = ContextMenuClick
      end
      object Delete1: TMenuItem
        Tag = 14
        Caption = '&Delete'
        OnClick = ContextMenuClick
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object SplitSelected3: TMenuItem
        Tag = 30
        Caption = 'Split Selected'
        Visible = False
        OnClick = MenuClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Feedback1: TMenuItem
        Caption = 'Beta Feedback...'
        OnClick = Feedback1Click
      end
      object HelponMemoryManagerWindow1: TMenuItem
        Caption = 'Memory Manager Help'
        ShortCut = 112
        OnClick = HelponMemoryManagerWindow1Click
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 192
    Top = 256
    object Insertintomemory1: TMenuItem
      Tag = 90
      Caption = 'Move/Insert into cursor'
      OnClick = ContextMenuClick
    end
    object Rewrite1: TMenuItem
      Tag = 92
      Caption = 'Rewrite/Insert in place'
      OnClick = ContextMenuClick
    end
    object Putto1: TMenuItem
      Caption = 'Insert into presets'
      object NextFreeAddress1: TMenuItem
        Tag = 36
        Caption = 'Auto'
        OnClick = ContextMenuClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object CHARS1: TMenuItem
        Tag = 30
        Caption = 'CHARS'
        OnClick = ContextMenuClick
      end
      object UDG1: TMenuItem
        Tag = 32
        Caption = 'UDG'
        OnClick = ContextMenuClick
      end
      object SCREEN1: TMenuItem
        Tag = 31
        Caption = 'SCREEN'
        OnClick = ContextMenuClick
      end
      object RAMTOP1: TMenuItem
        Tag = 33
        Caption = '0 REM'
        OnClick = ContextMenuClick
      end
      object VARS1: TMenuItem
        Tag = 34
        Caption = 'RAMTOP'
        OnClick = ContextMenuClick
      end
      object N327681: TMenuItem
        Tag = 37
        Caption = '32768'
        OnClick = ContextMenuClick
      end
      object N491921: TMenuItem
        Tag = 35
        Caption = '49152'
        OnClick = ContextMenuClick
      end
    end
    object Lift1: TMenuItem
      Tag = 91
      Caption = 'Lift'
      OnClick = ContextMenuClick
    end
    object oggleTrack1: TMenuItem
      Tag = 17
      Caption = 'Track/Untrack'
      OnClick = ContextMenuClick
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object SetType1: TMenuItem
      Caption = 'Change Type'
      object Data1: TMenuItem
        Tag = 50
        Caption = 'Raw'
        OnClick = ContextMenuClick
      end
      object Screen2: TMenuItem
        Tag = 51
        Caption = 'Screen'
        OnClick = ContextMenuClick
      end
      object Code1: TMenuItem
        Tag = 52
        Caption = 'Code'
        OnClick = ContextMenuClick
      end
      object UDG2: TMenuItem
        Tag = 53
        Caption = 'UDG'
        OnClick = ContextMenuClick
      end
      object Chars2: TMenuItem
        Tag = 54
        Caption = 'Chars'
        OnClick = ContextMenuClick
      end
      object Map1: TMenuItem
        Tag = 55
        Caption = 'Map'
        OnClick = ContextMenuClick
      end
      object Beeps1: TMenuItem
        Tag = 56
        Caption = 'Beeps'
        OnClick = ContextMenuClick
      end
      object AYPSG1: TMenuItem
        Tag = 57
        Caption = 'AY/PSG'
        OnClick = ContextMenuClick
      end
      object zx0Depacker1: TMenuItem
        Tag = 58
        Caption = 'Fixed'
        OnClick = ContextMenuClick
      end
      object Packed1: TMenuItem
        Tag = 59
        Caption = 'Packed'
        OnClick = ContextMenuClick
      end
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Deselect1: TMenuItem
      Tag = 17
      Caption = 'Deselect'
      OnClick = ContextMenuClick
    end
    object NewBlockfromCurrentState1: TMenuItem
      Tag = 15
      Caption = 'Duplicate'
      OnClick = ContextMenuClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Pack1: TMenuItem
      Tag = 19
      Caption = 'Pack'
      OnClick = ContextMenuClick
    end
    object SendtoBinaryGrabber1: TMenuItem
      Tag = 21
      Caption = 'Send to...'
      OnClick = ContextMenuClick
    end
    object ViewasHex1: TMenuItem
      Tag = 20
      Caption = 'Edit'
      Visible = False
      OnClick = ContextMenuClick
    end
    object Edit2: TMenuItem
      Caption = 'Edit in Editor'
      Enabled = False
      Visible = False
      OnClick = ContextMenuClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object CutBlock1: TMenuItem
      Tag = 10
      Caption = 'Cut'
      OnClick = ContextMenuClick
    end
    object CopyBlock1: TMenuItem
      Tag = 11
      Caption = 'Copy'
      OnClick = ContextMenuClick
    end
    object PasteBlock1: TMenuItem
      Tag = 12
      Caption = 'Paste'
      OnClick = ContextMenuClick
    end
    object Remove1: TMenuItem
      Tag = 14
      Caption = 'Delete'
      OnClick = ContextMenuClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Rename1: TMenuItem
      Tag = 13
      Caption = 'Rename'
      OnClick = ContextMenuClick
    end
    object Properties1: TMenuItem
      Tag = 16
      Caption = 'Resize'
      OnClick = ContextMenuClick
    end
    object Lock1: TMenuItem
      Tag = 25
      Caption = 'Lock/Unlock'
      OnClick = ContextMenuClick
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 112
    Top = 256
    object CreateBlockFromSelectedArea1: TMenuItem
      Tag = 80
      Caption = 'Create Block From Selected Area'
      OnClick = ContextMenuClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Jumpto1: TMenuItem
      Tag = 81
      Caption = 'Jump to...'
      OnClick = ContextMenuClick
    end
    object GridToggle1: TMenuItem
      Tag = 82
      Caption = 'Grid Toggle'
      OnClick = ContextMenuClick
    end
    object ApplyEditstoMemory1: TMenuItem
      Tag = 83
      Caption = 'Revert to Memory'
      OnClick = ContextMenuClick
    end
    object ApplyEditstoMemory2: TMenuItem
      Tag = 84
      Caption = 'Apply Edits to Memory'
      OnClick = ContextMenuClick
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object Copy2: TMenuItem
      Tag = 85
      Caption = 'Copy as Text'
      OnClick = ContextMenuClick
    end
    object PasteasText1: TMenuItem
      Tag = 86
      Caption = 'Paste Text'
      OnClick = ContextMenuClick
    end
  end
  object VariableTimer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = VariableTimerTimer
    Left = 232
    Top = 256
  end
end
