object OptionsWindow: TOptionsWindow
  Tag = 1
  Left = 710
  Top = 142
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 477
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    342
    477)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 325
    Height = 432
    ActivePage = TabSheet6
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiLine = True
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'BasinC'
      DesignSize = (
        317
        386)
      object Label1: TLabel
        Left = 4
        Top = 8
        Width = 104
        Height = 13
        Caption = 'Programming Aids'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel1: TThemeBevel
        Left = 4
        Top = 24
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label4: TLabel
        Left = 132
        Top = 260
        Width = 86
        Height = 13
        Caption = 'Graphics Modifier:'
      end
      object Label19: TLabel
        Left = 132
        Top = 220
        Width = 97
        Height = 13
        Caption = 'Brackets Completion'
      end
      object Label25: TLabel
        Left = 132
        Top = 180
        Width = 120
        Height = 13
        Caption = 'Line overwrite protection'
      end
      object Label42: TLabel
        Left = 4
        Top = 288
        Width = 32
        Height = 13
        Caption = 'Other'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ThemeBevel3: TThemeBevel
        Left = 3
        Top = 304
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label43: TLabel
        Left = 132
        Top = 332
        Width = 76
        Height = 13
        Caption = 'Menu Language'
      end
      object CheckBox2: TCheckBox
        Left = 132
        Top = 52
        Width = 173
        Height = 17
        Caption = 'Show Syntax helper on startup'
        TabOrder = 0
      end
      object CheckBox3: TCheckBox
        Left = 132
        Top = 72
        Width = 169
        Height = 17
        Caption = 'Auto Line Number (.)'
        TabOrder = 1
      end
      object ComboBox1: TComboBox
        Left = 132
        Top = 276
        Width = 114
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 5
        Items.Strings = (
          'Alt-Gr (XP/NT Only)'
          'Scroll Lock'
          'Num Lock')
      end
      object CheckBox15: TCheckBox
        Left = 132
        Top = 95
        Width = 173
        Height = 13
        Caption = 'Predictive Auto-Typing'
        TabOrder = 2
      end
      object ComboBox7: TComboBox
        Left = 132
        Top = 236
        Width = 114
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          'None'
          'Auto Typed'
          'Auto Complete')
      end
      object ComboBox9: TComboBox
        Left = 132
        Top = 196
        Width = 114
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 3
        Items.Strings = (
          'None'
          'Protect all'
          'Only for new lines')
      end
      object ComboBox11: TComboBox
        Left = 132
        Top = 352
        Width = 173
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 6
      end
      object CheckBox12: TCheckBox
        Left = 132
        Top = 116
        Width = 117
        Height = 13
        Caption = 'Syntax Highlighting'
        TabOrder = 8
      end
      object Button3: TButton
        Left = 8
        Top = 32
        Width = 118
        Height = 28
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Set Editor Colours'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 7
        OnClick = Button3Click
      end
      object CheckBox30: TCheckBox
        Left = 132
        Top = 160
        Width = 109
        Height = 17
        Caption = 'Indent 5 Spaces'
        TabOrder = 9
      end
      object TrackBar6: TTrackBar
        Left = 0
        Top = 153
        Width = 126
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        LineSize = 4
        Max = 8
        Min = 1
        PageSize = 1
        Position = 1
        TabOrder = 10
        ThumbLength = 12
        TickMarks = tmBoth
        OnChange = TrackBar6Change
      end
      object CheckBox31: TCheckBox
        Left = 132
        Top = 312
        Width = 169
        Height = 17
        Caption = 'Allow Multiple Instances'
        TabOrder = 11
      end
      object chkOpt_OnlineHelp: TCheckBox
        Left = 132
        Top = 136
        Width = 125
        Height = 13
        Caption = 'Use Online Help'
        TabOrder = 12
      end
      object CheckBox34: TCheckBox
        Left = 132
        Top = 32
        Width = 173
        Height = 17
        Caption = 'Show Keywords in REMarks'
        TabOrder = 13
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Fonts'
      ImageIndex = 6
      DesignSize = (
        317
        386)
      object Label28: TLabel
        Left = 4
        Top = 8
        Width = 61
        Height = 13
        Caption = 'Font folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel15: TThemeBevel
        Left = 4
        Top = 24
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object SpeedButton1: TSpeedButton
        Left = 209
        Top = 36
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Glyph.Data = {
          A6020000424DA6020000000000003600000028000000100000000D0000000100
          18000000000070020000130B0000130B00000000000000000000020CCC020CCC
          020CCC0000000000000000000000000000000000000000000000000000000000
          00000000000000000000020CCC020CCC808080A0A0A080808080808080808080
          8080808080808080808080808080808080808080000000000000020CCC020CCC
          B0B0B0CFC8CF00F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8
          FF808080000000000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C000F8FFC0C0C000F8FFB0B0B0404040808080000000020CCCA0A0A0
          EFE8EFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C08080
          80000000808080000000909090EFE8EFC0C0C030E8EFC0C0C030E8EFC0C0C000
          F8FFC0C0C030E8EFC0C0C030C8CF004040808080808080000000A0A0A0DFD8DF
          DFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DF9090902020
          20A0A0A080808000000080808080808080808080808080808080808080808080
          808080808080808080808080808080808020D8DF808080000000020CCCA0A0A0
          DFD8DFB0B0B000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C060D8
          DF90C8CF808080000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C0DFD8DFFFF8FFFFF8FFFFF8FFFFF8FFA0A0A0000000020CCCA0A0A0
          DFD8DFC0C0C000F8FFC0C0C000F8FFC0C0C0CFC8CFA0A0A0A0A0A0A0A0A0A0A0
          A0A0A0A0808080020CCC020CCC020CCCA0A0A0FFF8FFDFD8DFFFF8FFEFE8EFDF
          D8DF909090020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC
          020CCCA0A0A0A0A0A0A0A0A0A0A0A0909090020CCC020CCC020CCC020CCC020C
          CC020CCC020CCC020CCC}
        Layout = blGlyphRight
        OnClick = BtnFontSelectClick
      end
      object Label10: TLabel
        Left = 132
        Top = 124
        Width = 19
        Height = 13
        Caption = 'Size'
      end
      object Label35: TLabel
        Left = 4
        Top = 60
        Width = 59
        Height = 13
        Caption = 'Editor font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel16: TThemeBevel
        Left = 4
        Top = 76
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label36: TLabel
        Left = 4
        Top = 332
        Width = 141
        Height = 13
        Caption = 'Utility/Tool windows font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel17: TThemeBevel
        Left = 4
        Top = 348
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label37: TLabel
        Left = 132
        Top = 84
        Width = 65
        Height = 13
        Caption = 'Font filename'
      end
      object Label52: TLabel
        Left = 148
        Top = 356
        Width = 43
        Height = 13
        Caption = 'Font size'
      end
      object Edit1: TEdit
        Left = 4
        Top = 36
        Width = 205
        Height = 21
        ReadOnly = True
        TabOrder = 0
        Text = 'Edit1'
      end
      object ComboBox4: TComboBox
        Left = 132
        Top = 140
        Width = 94
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        OnChange = ComboBox2Change
        Items.Strings = (
          'Normal'
          'Double '
          'Triple'
          'Quadruple'
          'Quintuple'
          'Sextuple')
      end
      object FastIMG1: TFastIMG
        Left = 36
        Top = 170
        Width = 242
        Height = 150
        Transparent = False
        Picture = '(None)'
        AutoSize = False
        SizeMode = smGDI
        DrawStyle = dsDraw
        DIBLeft = 0
        DIBTop = 0
      end
      object TreeView1: TTreeView
        Left = 132
        Top = 100
        Width = 174
        Height = 21
        Indent = 19
        ParentShowHint = False
        ReadOnly = True
        ShowHint = False
        TabOrder = 1
        ToolTips = False
        Visible = False
        OnClick = ComboBox10Click
        OnCollapsed = TreeView1Expanded
        OnEnter = TreeView1Enter
        OnExpanded = TreeView1Expanded
        OnMouseDown = TreeView1MouseDown
        OnMouseUp = TreeView1MouseUp
      end
      object ComboBox12: TComboBox
        Left = 215
        Top = 356
        Width = 94
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 4
        OnChange = ComboBox2Change
        Items.Strings = (
          'Default'
          '10'
          '12'
          '14'
          '16'
          '18'
          '24'
          '32')
      end
      object Button8: TButton
        Left = 232
        Top = 36
        Width = 75
        Height = 21
        Caption = 'Read Fonts'
        TabOrder = 5
        OnClick = Button8Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Reporting'
      ImageIndex = 4
      DesignSize = (
        317
        386)
      object Bevel2: TThemeBevel
        Left = 4
        Top = 24
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label2: TLabel
        Left = 4
        Top = 8
        Width = 95
        Height = 13
        Caption = 'Error Notification'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object CheckListBox1: TCheckListBox
        Left = 132
        Top = 36
        Width = 178
        Height = 317
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Items.Strings = (
          '0 OK'
          '1 NEXT without FOR'
          '2 Variable not found'
          '3 Subscript wrong'
          '4 Out of memory'
          '5 Out of screen'
          '6 Number too big'
          '7 RETURN without GO SUB'
          '8 End of file'
          '9 STOP statement'
          'A Invalid argument'
          'B Integer out of range'
          'C Nonsense in BASIC'
          'D BREAK - CONT repeats'
          'E Out of DATA'
          'F Invalid filename'
          'G No room for line'
          'H STOP in INPUT'
          'I NEXT without FOR'
          'J Invalid I/O device'
          'K Invalid colour'
          'L BREAK into program'
          'M RAMTOP no good'
          'N Statement lost'
          'O Invalid Stream'
          'P FN without DEF'
          'Q Parameter error'
          'R File loading error'
          'a MERGE error'
          'b Wrong file type'
          'c CODE error'
          'd Too many brackets'
          'e File already exists'
          'f Invalid name'
          'g RAM disk error'
          'h File does not exist'
          'i Invalid device'
          'j Invalid BAUD rate'
          'k Invalid note name'
          'l Number too large'
          'm Note out of range'
          'n Out of range'
          'o Too many tied notes'
          'Unknown Errors')
        TabOrder = 0
      end
      object CheckBox10: TCheckBox
        Left = 132
        Top = 364
        Width = 173
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Move Editor Cursor to Error'
        TabOrder = 1
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Tools/AI'
      ImageIndex = 7
      DesignSize = (
        317
        386)
      object SpeedButton2: TSpeedButton
        Left = 289
        Top = 116
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Glyph.Data = {
          A6020000424DA6020000000000003600000028000000100000000D0000000100
          18000000000070020000130B0000130B00000000000000000000020CCC020CCC
          020CCC0000000000000000000000000000000000000000000000000000000000
          00000000000000000000020CCC020CCC808080A0A0A080808080808080808080
          8080808080808080808080808080808080808080000000000000020CCC020CCC
          B0B0B0CFC8CF00F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8
          FF808080000000000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C000F8FFC0C0C000F8FFB0B0B0404040808080000000020CCCA0A0A0
          EFE8EFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C08080
          80000000808080000000909090EFE8EFC0C0C030E8EFC0C0C030E8EFC0C0C000
          F8FFC0C0C030E8EFC0C0C030C8CF004040808080808080000000A0A0A0DFD8DF
          DFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DF9090902020
          20A0A0A080808000000080808080808080808080808080808080808080808080
          808080808080808080808080808080808020D8DF808080000000020CCCA0A0A0
          DFD8DFB0B0B000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C060D8
          DF90C8CF808080000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C0DFD8DFFFF8FFFFF8FFFFF8FFFFF8FFA0A0A0000000020CCCA0A0A0
          DFD8DFC0C0C000F8FFC0C0C000F8FFC0C0C0CFC8CFA0A0A0A0A0A0A0A0A0A0A0
          A0A0A0A0808080020CCC020CCC020CCCA0A0A0FFF8FFDFD8DFFFF8FFEFE8EFDF
          D8DF909090020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC
          020CCCA0A0A0A0A0A0A0A0A0A0A0A0909090020CCC020CCC020CCC020CCC020C
          CC020CCC020CCC020CCC}
        Layout = blGlyphRight
        OnClick = SpeedButton2Click
      end
      object Label46: TLabel
        Left = 4
        Top = 12
        Width = 80
        Height = 13
        Caption = 'External Tools'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ThemeBevel6: TThemeBevel
        Left = 3
        Top = 28
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label47: TLabel
        Left = 8
        Top = 224
        Width = 88
        Height = 13
        Caption = 'Define AI Provider'
      end
      object Label48: TLabel
        Left = 8
        Top = 40
        Width = 116
        Height = 13
        Caption = 'Pasmo Assembler Folder'
      end
      object SpeedButton4: TSpeedButton
        Left = 288
        Top = 60
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Glyph.Data = {
          A6020000424DA6020000000000003600000028000000100000000D0000000100
          18000000000070020000130B0000130B00000000000000000000020CCC020CCC
          020CCC0000000000000000000000000000000000000000000000000000000000
          00000000000000000000020CCC020CCC808080A0A0A080808080808080808080
          8080808080808080808080808080808080808080000000000000020CCC020CCC
          B0B0B0CFC8CF00F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8
          FF808080000000000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C000F8FFC0C0C000F8FFB0B0B0404040808080000000020CCCA0A0A0
          EFE8EFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C08080
          80000000808080000000909090EFE8EFC0C0C030E8EFC0C0C030E8EFC0C0C000
          F8FFC0C0C030E8EFC0C0C030C8CF004040808080808080000000A0A0A0DFD8DF
          DFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DF9090902020
          20A0A0A080808000000080808080808080808080808080808080808080808080
          808080808080808080808080808080808020D8DF808080000000020CCCA0A0A0
          DFD8DFB0B0B000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C060D8
          DF90C8CF808080000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C0DFD8DFFFF8FFFFF8FFFFF8FFFFF8FFA0A0A0000000020CCCA0A0A0
          DFD8DFC0C0C000F8FFC0C0C000F8FFC0C0C0CFC8CFA0A0A0A0A0A0A0A0A0A0A0
          A0A0A0A0808080020CCC020CCC020CCCA0A0A0FFF8FFDFD8DFFFF8FFEFE8EFDF
          D8DF909090020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC
          020CCCA0A0A0A0A0A0A0A0A0A0A0A0909090020CCC020CCC020CCC020CCC020C
          CC020CCC020CCC020CCC}
        Layout = blGlyphRight
        Visible = False
        OnClick = SpeedButton4Click
      end
      object Label49: TLabel
        Left = 8
        Top = 152
        Width = 96
        Height = 13
        Caption = 'External Text Editor'
      end
      object SpeedButton5: TSpeedButton
        Left = 288
        Top = 172
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Glyph.Data = {
          A6020000424DA6020000000000003600000028000000100000000D0000000100
          18000000000070020000130B0000130B00000000000000000000020CCC020CCC
          020CCC0000000000000000000000000000000000000000000000000000000000
          00000000000000000000020CCC020CCC808080A0A0A080808080808080808080
          8080808080808080808080808080808080808080000000000000020CCC020CCC
          B0B0B0CFC8CF00F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8
          FF808080000000000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C000F8FFC0C0C000F8FFB0B0B0404040808080000000020CCCA0A0A0
          EFE8EFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C08080
          80000000808080000000909090EFE8EFC0C0C030E8EFC0C0C030E8EFC0C0C000
          F8FFC0C0C030E8EFC0C0C030C8CF004040808080808080000000A0A0A0DFD8DF
          DFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DFDFD8DF9090902020
          20A0A0A080808000000080808080808080808080808080808080808080808080
          808080808080808080808080808080808020D8DF808080000000020CCCA0A0A0
          DFD8DFB0B0B000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C000F8FFC0C0C060D8
          DF90C8CF808080000000020CCCA0A0A0DFD8DF00F8FFC0C0C000F8FFC0C0C000
          F8FFC0C0C0DFD8DFFFF8FFFFF8FFFFF8FFFFF8FFA0A0A0000000020CCCA0A0A0
          DFD8DFC0C0C000F8FFC0C0C000F8FFC0C0C0CFC8CFA0A0A0A0A0A0A0A0A0A0A0
          A0A0A0A0808080020CCC020CCC020CCCA0A0A0FFF8FFDFD8DFFFF8FFEFE8EFDF
          D8DF909090020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC020CCC
          020CCCA0A0A0A0A0A0A0A0A0A0A0A0909090020CCC020CCC020CCC020CCC020C
          CC020CCC020CCC020CCC}
        Layout = blGlyphRight
        OnClick = SpeedButton5Click
      end
      object Label54: TLabel
        Left = 4
        Top = 204
        Width = 117
        Height = 13
        Caption = 'Artificial Intelligence'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ThemeBevel7: TThemeBevel
        Left = 3
        Top = 220
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label55: TLabel
        Left = 8
        Top = 96
        Width = 216
        Height = 13
        Caption = 'External Utility/emulator (File > Send SNA...)'
      end
      object Label44: TLabel
        Left = 8
        Top = 336
        Width = 64
        Height = 13
        Caption = 'Select Model:'
      end
      object Edit2: TEdit
        Left = 8
        Top = 116
        Width = 274
        Height = 21
        ReadOnly = True
        TabOrder = 0
        Text = '<not set>'
      end
      object EditAIQueryURL: TEdit
        Left = 24
        Top = 308
        Width = 281
        Height = 21
        Enabled = False
        TabOrder = 1
        Text = 'http://localhost:11434/'
      end
      object Edit4: TEdit
        Left = 8
        Top = 60
        Width = 274
        Height = 21
        Enabled = False
        ReadOnly = True
        TabOrder = 2
        Text = '<pasmo.exe is not found>'
      end
      object Edit5: TEdit
        Left = 8
        Top = 172
        Width = 274
        Height = 21
        Enabled = False
        ReadOnly = True
        TabOrder = 3
        Text = '<Feature is not Available>'
      end
      object RadioAI1: TRadioButton
        Tag = 410
        Left = 8
        Top = 240
        Width = 289
        Height = 17
        Caption = 'Google Gemini (Cloud) Enter your API Key:'
        Checked = True
        TabOrder = 4
        TabStop = True
        OnClick = RadioAI1Click
      end
      object RadioAI2: TRadioButton
        Tag = 411
        Left = 8
        Top = 288
        Width = 297
        Height = 17
        Caption = 'Ollama (Local) Enter Your Server Address:port/'
        TabOrder = 5
        OnClick = RadioAI1Click
      end
      object EditAPIKEY: TEdit
        Left = 24
        Top = 260
        Width = 281
        Height = 21
        MaxLength = 128
        TabOrder = 6
      end
      object cbAIModelSelect: TComboBox
        Left = 24
        Top = 352
        Width = 201
        Height = 21
        ItemHeight = 0
        TabOrder = 7
      end
      object btnAImodelFetch: TButton
        Left = 232
        Top = 352
        Width = 75
        Height = 21
        Caption = 'Get Models'
        TabOrder = 8
        OnClick = btnAImodelFetchClick
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Emulation'
      ImageIndex = 5
      DesignSize = (
        317
        386)
      object Label27: TLabel
        Left = 4
        Top = 8
        Width = 110
        Height = 13
        Caption = 'Z80 Core emulation'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel12: TThemeBevel
        Left = 4
        Top = 24
        Width = 310
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label29: TLabel
        Left = 172
        Top = 44
        Width = 52
        Height = 13
        Caption = 'Cpu Speed'
      end
      object Label30: TLabel
        Left = 271
        Top = 44
        Width = 34
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'xx Mhz'
      end
      object Label31: TLabel
        Left = 4
        Top = 152
        Width = 116
        Height = 13
        Caption = 'ZX Printer emulation'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel13: TThemeBevel
        Left = 4
        Top = 168
        Width = 310
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object ThemeBevel1: TThemeBevel
        Left = 7
        Top = 240
        Width = 310
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label40: TLabel
        Left = 4
        Top = 224
        Width = 90
        Height = 13
        Caption = 'Other Hardware'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label41: TLabel
        Left = 4
        Top = 312
        Width = 62
        Height = 13
        Caption = 'Experience'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ThemeBevel2: TThemeBevel
        Left = 7
        Top = 328
        Width = 310
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label50: TLabel
        Left = 12
        Top = 60
        Width = 79
        Height = 13
        Caption = 'Emulation Speed'
        Visible = False
      end
      object Label51: TLabel
        Left = 110
        Top = 60
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = '50fps'
        Visible = False
      end
      object Label53: TLabel
        Left = 12
        Top = 100
        Width = 76
        Height = 13
        Caption = 'Sound Available'
        Visible = False
      end
      object Label39: TLabel
        Left = 264
        Top = 84
        Width = 37
        Height = 13
        Alignment = taRightJustify
        Caption = 'Label39'
      end
      object CheckBox20: TCheckBox
        Left = 12
        Top = 32
        Width = 173
        Height = 17
        Caption = 'Allow IN for keyboard'
        Enabled = False
        TabOrder = 0
        Visible = False
      end
      object TrackBar4: TTrackBar
        Left = 128
        Top = 60
        Width = 184
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 1118208
        Min = 4
        PageSize = 1
        Frequency = 34944
        Position = 4
        TabOrder = 1
        ThumbLength = 12
        OnChange = TrackBar4Change
      end
      object CheckBox21: TCheckBox
        Left = 132
        Top = 180
        Width = 173
        Height = 17
        Caption = 'Save output between sessions'
        TabOrder = 2
      end
      object CheckBox22: TCheckBox
        Left = 132
        Top = 200
        Width = 173
        Height = 17
        Caption = 'Accelerate printing'
        TabOrder = 3
      end
      object CheckBox24: TCheckBox
        Left = 132
        Top = 248
        Width = 169
        Height = 17
        Caption = 'UlaPlus Ula64 Mode'
        TabOrder = 4
      end
      object Button4: TButton
        Left = 28
        Top = 120
        Width = 75
        Height = 25
        Caption = 'Tree?'
        TabOrder = 5
        Visible = False
        OnClick = Button4Click
      end
      object ButtonReset: TButton
        Left = 132
        Top = 104
        Width = 69
        Height = 25
        Caption = 'Default'
        TabOrder = 6
        OnClick = ButtonResetClick
      end
      object CheckBox25: TCheckBox
        Left = 131
        Top = 360
        Width = 169
        Height = 17
        Caption = 'Fast Resets'
        TabOrder = 7
      end
      object Button5: TButton
        Left = 204
        Top = 104
        Width = 53
        Height = 25
        Caption = 'Double'
        TabOrder = 8
        OnClick = Button5Click
      end
      object CheckBox28: TCheckBox
        Left = 132
        Top = 272
        Width = 113
        Height = 17
        Caption = 'Kempston Mouse'
        TabOrder = 9
      end
      object CheckBox29: TCheckBox
        Left = 131
        Top = 296
        Width = 173
        Height = 17
        Caption = 'SimpleCON Addon'
        TabOrder = 10
      end
      object CheckBox1: TCheckBox
        Left = 132
        Top = 336
        Width = 169
        Height = 17
        Caption = 'Restore previous session'
        TabOrder = 11
      end
      object TrackBar7: TTrackBar
        Left = 5
        Top = 76
        Width = 100
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        PageSize = 1
        Position = 4
        TabOrder = 12
        ThumbLength = 12
        Visible = False
        OnChange = TrackBar7Change
      end
      object Button7: TButton
        Left = 256
        Top = 104
        Width = 53
        Height = 25
        Caption = 'Halve'
        TabOrder = 13
        OnClick = Button7Click
      end
      object CheckBox35: TCheckBox
        Left = 132
        Top = 136
        Width = 145
        Height = 17
        Caption = 'Remember Cpu Speed'
        TabOrder = 14
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Display'
      ImageIndex = 2
      DesignSize = (
        317
        386)
      object Label3: TLabel
        Left = 4
        Top = 8
        Width = 86
        Height = 13
        Caption = 'Scaling Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel3: TThemeBevel
        Left = 4
        Top = 24
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label5: TLabel
        Left = 132
        Top = 108
        Width = 72
        Height = 13
        Caption = 'Scaling Method'
      end
      object Label9: TLabel
        Left = 4
        Top = 236
        Width = 89
        Height = 13
        Caption = 'Embellishments'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel6: TThemeBevel
        Left = 4
        Top = 252
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label20: TLabel
        Left = 4
        Top = 156
        Width = 59
        Height = 13
        Caption = 'Frameskip'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel9: TThemeBevel
        Left = 4
        Top = 172
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label21: TLabel
        Left = 259
        Top = 180
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'xx Frames'
      end
      object Label22: TLabel
        Left = 132
        Top = 180
        Width = 70
        Height = 13
        Caption = 'Update Every:'
      end
      object CheckBox4: TCheckBox
        Left = 132
        Top = 32
        Width = 125
        Height = 17
        Caption = 'Maintain Aspect Ratio'
        TabOrder = 0
        OnClick = CheckBox4Click
      end
      object CheckBox5: TCheckBox
        Left = 132
        Top = 52
        Width = 117
        Height = 17
        Caption = 'Use Integer Scaling'
        TabOrder = 1
        OnClick = CheckBox5Click
      end
      object CheckBox6: TCheckBox
        Left = 132
        Top = 264
        Width = 117
        Height = 17
        Caption = 'Scanlines simulation'
        TabOrder = 6
      end
      object ComboBox2: TComboBox
        Left = 212
        Top = 104
        Width = 98
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 3
        OnChange = ComboBox2Change
        Items.Strings = (
          'None (GDI)'
          'Bilinear'
          'Scale2x'
          'Hq2x'
          'Super 2xSAI'
          'Super Eagle')
      end
      object CheckBox8: TCheckBox
        Left = 132
        Top = 288
        Width = 149
        Height = 17
        Caption = 'Rounded Display Corners'
        TabOrder = 7
      end
      object CheckBox11: TCheckBox
        Left = 132
        Top = 76
        Width = 169
        Height = 17
        Caption = 'Use 8Bit Stretch for GDI Mode'
        TabOrder = 2
        OnClick = CheckBox5Click
      end
      object TrackBar3: TTrackBar
        Left = 126
        Top = 196
        Width = 187
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 50
        Min = 1
        PageSize = 1
        Position = 1
        TabOrder = 4
        ThumbLength = 12
        OnChange = TrackBar3Change
      end
      object CheckBox9: TCheckBox
        Left = 132
        Top = 220
        Width = 169
        Height = 17
        Caption = 'Automatically skip if necessary'
        TabOrder = 5
        OnClick = CheckBox5Click
      end
      object CheckBox36: TCheckBox
        Left = 132
        Top = 324
        Width = 169
        Height = 17
        Caption = 'Experimental GPU Renderer'
        TabOrder = 8
        OnClick = CheckBox5Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Sound'
      ImageIndex = 3
      DesignSize = (
        317
        386)
      object Label11: TLabel
        Left = 4
        Top = 152
        Width = 90
        Height = 13
        Caption = 'Quality Settings'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel7: TThemeBevel
        Left = 4
        Top = 168
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label12: TLabel
        Left = 4
        Top = 256
        Width = 97
        Height = 13
        Caption = 'Buffering Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel8: TThemeBevel
        Left = 4
        Top = 272
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label13: TLabel
        Left = 132
        Top = 184
        Width = 74
        Height = 13
        Caption = 'Frequency (Hz)'
      end
      object Label14: TLabel
        Left = 132
        Top = 212
        Width = 44
        Height = 13
        Caption = 'Bit Depth'
      end
      object Label15: TLabel
        Left = 132
        Top = 284
        Width = 84
        Height = 13
        Caption = 'Sound buffer size'
      end
      object Label16: TLabel
        Left = 259
        Top = 284
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'xx Frames'
      end
      object Label17: TLabel
        Left = 132
        Top = 332
        Width = 71
        Height = 13
        Caption = 'Sound Latency'
      end
      object Label18: TLabel
        Left = 259
        Top = 332
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'xx Frames'
      end
      object Label23: TLabel
        Left = 4
        Top = 8
        Width = 35
        Height = 13
        Caption = 'Sound'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel10: TThemeBevel
        Left = 4
        Top = 24
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label24: TLabel
        Left = 132
        Top = 76
        Width = 69
        Height = 13
        Caption = 'Key Click Type'
      end
      object Label32: TLabel
        Left = 4
        Top = 92
        Width = 42
        Height = 13
        Caption = 'Volume'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel14: TThemeBevel
        Left = 4
        Top = 108
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label33: TLabel
        Left = 132
        Top = 138
        Width = 16
        Height = 13
        Caption = 'Min'
      end
      object Label34: TLabel
        Left = 289
        Top = 138
        Width = 20
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Max'
      end
      object ComboBox5: TComboBox
        Left = 212
        Top = 180
        Width = 98
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 4
        OnChange = ComboBox2Change
        Items.Strings = (
          '11025'
          '22050'
          '44100')
      end
      object ComboBox6: TComboBox
        Left = 212
        Top = 208
        Width = 98
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 5
        OnChange = ComboBox2Change
        Items.Strings = (
          '8 Bit'
          '16 Bit')
      end
      object CheckBox13: TCheckBox
        Left = 132
        Top = 236
        Width = 125
        Height = 17
        Caption = 'Stereo'
        TabOrder = 6
        OnClick = CheckBox4Click
      end
      object TrackBar1: TTrackBar
        Left = 126
        Top = 300
        Width = 188
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 50
        PageSize = 1
        TabOrder = 7
        ThumbLength = 12
        OnChange = TrackBar1Change
      end
      object TrackBar2: TTrackBar
        Left = 126
        Top = 348
        Width = 188
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 50
        PageSize = 1
        TabOrder = 8
        ThumbLength = 12
        OnChange = TrackBar2Change
      end
      object CheckBox14: TCheckBox
        Left = 132
        Top = 32
        Width = 125
        Height = 17
        Caption = 'Beeper/AY Sound'
        TabOrder = 0
      end
      object CheckBox16: TCheckBox
        Left = 132
        Top = 52
        Width = 125
        Height = 17
        Caption = 'Editor Sounds'
        TabOrder = 1
        OnClick = CheckBox16Click
      end
      object ComboBox8: TComboBox
        Left = 212
        Top = 72
        Width = 98
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          '48k'
          '128k')
      end
      object TrackBar5: TTrackBar
        Left = 125
        Top = 116
        Width = 188
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 30000
        Min = 1
        PageSize = 1
        Frequency = 600
        Position = 1
        TabOrder = 3
        ThumbLength = 12
        OnChange = TrackBar1Change
      end
      object CheckBox23: TCheckBox
        Left = 212
        Top = 236
        Width = 97
        Height = 17
        Caption = 'DSound synch'
        TabOrder = 9
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      DesignSize = (
        317
        386)
      object Label6: TLabel
        Left = 4
        Top = 8
        Width = 59
        Height = 13
        Caption = 'Snapshots'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel4: TThemeBevel
        Left = 4
        Top = 24
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label7: TLabel
        Left = 4
        Top = 84
        Width = 51
        Height = 13
        Caption = '.BAS files'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel5: TThemeBevel
        Left = 4
        Top = 100
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label8: TLabel
        Left = 136
        Top = 40
        Width = 86
        Height = 13
        Caption = 'Save .z80 Version'
      end
      object Label26: TLabel
        Left = 4
        Top = 188
        Width = 96
        Height = 13
        Caption = 'Tape Image Files'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel11: TThemeBevel
        Left = 4
        Top = 204
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label38: TLabel
        Left = 104
        Top = 68
        Width = 117
        Height = 13
        Caption = 'Save as Hardware Mode'
      end
      object ThemeBevel5: TThemeBevel
        Left = 3
        Top = 300
        Width = 306
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object Label45: TLabel
        Left = 4
        Top = 284
        Width = 179
        Height = 13
        Caption = 'Auto Backup (/autoback folder)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ComboBox3: TComboBox
        Left = 228
        Top = 36
        Width = 82
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'v1.45'
          'v2.01'
          'v3.5')
      end
      object CheckBox7: TCheckBox
        Left = 132
        Top = 112
        Width = 149
        Height = 17
        Caption = 'Break Multistatement Lines'
        TabOrder = 1
      end
      object CheckBox17: TCheckBox
        Left = 132
        Top = 228
        Width = 173
        Height = 17
        Caption = 'Trap LOAD/MERGE/VERIFY'
        TabOrder = 2
        OnClick = CheckBox4Click
      end
      object CheckBox18: TCheckBox
        Left = 132
        Top = 248
        Width = 173
        Height = 17
        Caption = 'Trap SAVE Operations'
        TabOrder = 3
        OnClick = CheckBox4Click
      end
      object CheckBox19: TCheckBox
        Left = 132
        Top = 212
        Width = 173
        Height = 17
        Caption = 'Rewind when tapes end'
        TabOrder = 4
        OnClick = CheckBox4Click
      end
      object ComboBox10: TComboBox
        Left = 228
        Top = 64
        Width = 82
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 5
        Items.Strings = (
          'Always 48k'
          'Always 128k'
          'Ask')
      end
      object CheckBox26: TCheckBox
        Left = 132
        Top = 152
        Width = 149
        Height = 17
        Caption = 'Save with Autostart'
        TabOrder = 6
      end
      object CheckBox27: TCheckBox
        Left = 132
        Top = 132
        Width = 173
        Height = 17
        Caption = 'Allow BAS files to autostart'
        TabOrder = 7
      end
      object CheckBox32: TCheckBox
        Left = 131
        Top = 312
        Width = 173
        Height = 17
        Caption = 'Backup as .BAS every 9 minutes'
        TabOrder = 8
        OnClick = CheckBox4Click
      end
      object CheckBox33: TCheckBox
        Left = 132
        Top = 172
        Width = 109
        Height = 17
        Caption = 'Auto Show Notes'
        TabOrder = 9
      end
      object TrackBar8: TTrackBar
        Left = 128
        Top = 336
        Width = 185
        Height = 45
        Max = 9
        Position = 1
        TabOrder = 10
        TickMarks = tmTopLeft
        OnChange = TrackBar8Change
      end
      object Button6: TButton
        Left = 8
        Top = 311
        Width = 113
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'View Backup Folder'
        TabOrder = 11
        OnClick = Button6Click
      end
    end
  end
  object Button1: TButton
    Left = 207
    Top = 448
    Width = 61
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 272
    Top = 448
    Width = 61
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object HelpBtn: TButton
    Left = 8
    Top = 448
    Width = 49
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
  object FolderBrowser1: TFolderBrowser
    BrowseFlags = []
    Title = 'Select folder'
    Left = 64
    Top = 446
  end
end
