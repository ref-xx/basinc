object BrushSelectorForm: TBrushSelectorForm
  Left = 544
  Top = 220
  BorderStyle = bsDialog
  Caption = 'Set Caption Here!'
  ClientHeight = 334
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    301
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel5: TPanel
    Left = 4
    Top = 8
    Width = 293
    Height = 281
    Anchors = [akLeft, akTop, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      293
      281)
    object Label11: TLabel
      Left = 4
      Top = 4
      Width = 47
      Height = 13
      Caption = 'Fill Style'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Panel4: TPanel
      Left = 4
      Top = 56
      Width = 287
      Height = 217
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      BorderStyle = bsSingle
      TabOrder = 0
      object ScrollBox3: TScrollBox
        Left = 0
        Top = 0
        Width = 283
        Height = 213
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object FastIMG4: TFastIMG
          Left = 4
          Top = 4
          Width = 277
          Height = 261
          Transparent = False
          Picture = '(None)'
          AutoSize = False
          SizeMode = smGDI
          DrawStyle = dsDraw
          DIBLeft = 0
          DIBTop = 0
          OnClick = FastIMG4Click
          OnDblClick = FastIMG4DblClick
          OnMouseMove = FastIMG4MouseMove
        end
      end
    end
    object Panel6: TPanel
      Left = 4
      Top = 52
      Width = 287
      Height = 221
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        287
        221)
      object Label12: TLabel
        Left = 0
        Top = 140
        Width = 41
        Height = 13
        Caption = 'Rotation'
      end
      object Label13: TLabel
        Left = 253
        Top = 140
        Width = 31
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Label4'
      end
      object Label14: TLabel
        Left = 0
        Top = 180
        Width = 40
        Height = 13
        Caption = 'Repeats'
      end
      object Label15: TLabel
        Left = 253
        Top = 180
        Width = 31
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Label4'
      end
      object Label16: TLabel
        Left = 137
        Top = 96
        Width = 43
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Dithering'
      end
      object FastIMG5: TFastIMG
        Left = 0
        Top = 4
        Width = 128
        Height = 128
        Transparent = False
        Picture = '(None)'
        AutoSize = False
        SizeMode = smGDI
        DrawStyle = dsDraw
        DIBLeft = 0
        DIBTop = 0
      end
      object TrackBar5: TTrackBar
        Left = -4
        Top = 156
        Width = 293
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 359
        PageSize = 1
        Frequency = 10
        TabOrder = 1
        ThumbLength = 10
        OnChange = TrackBar5Change
        OnEnter = TrackBar5Enter
      end
      object TrackBar6: TTrackBar
        Left = -4
        Top = 196
        Width = 293
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 100
        PageSize = 1
        Frequency = 5
        TabOrder = 2
        ThumbLength = 10
        OnChange = TrackBar5Change
        OnEnter = TrackBar5Enter
      end
      object RadioGroup1: TRadioGroup
        Left = 136
        Top = 0
        Width = 149
        Height = 89
        Caption = ' Gradient Style '
        ItemIndex = 0
        Items.Strings = (
          'Linear'
          'Rectangular'
          'Circular'
          'Radial')
        TabOrder = 3
        OnClick = RadioGroup1Click
      end
      object ComboBox3: TComboBox
        Left = 136
        Top = 112
        Width = 149
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = RadioGroup1Click
        Items.Strings = (
          'Random Dot'
          'Diagonal'
          'Clustered'
          'Dispersed'
          '16 Level Ordered'
          '64 Level Ordered'
          'Floyd-Steinberg'
          'Jarvis'
          'Stucki'
          'Burkes'
          'Sierra-3'
          'Sierra-2'
          'Sierra Filter Lite'
          'Atkinson'
          'Stevenson-Arce')
      end
    end
    object ComboBox2: TComboBox
      Left = 4
      Top = 22
      Width = 212
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = ComboBox2Change
      Items.Strings = (
        'Pattern'
        'Gradient')
    end
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 287
    Height = 285
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Pen style'
      DesignSize = (
        279
        257)
      object Label1: TLabel
        Left = 60
        Top = 12
        Width = 52
        Height = 13
        Caption = 'Pen style'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 8
        Top = 200
        Width = 36
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Density'
      end
      object Label10: TLabel
        Left = 237
        Top = 200
        Width = 31
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        Caption = 'Label4'
      end
      object Panel2: TPanel
        Left = 8
        Top = 68
        Width = 260
        Height = 129
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 3
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 0
          Width = 260
          Height = 129
          Align = alClient
          Color = clWindow
          ParentColor = False
          TabOrder = 0
          object FastIMG1: TFastIMG
            Left = 0
            Top = 0
            Width = 256
            Height = 157
            Transparent = False
            Picture = '(None)'
            AutoSize = False
            SizeMode = smGDI
            DrawStyle = dsDraw
            DIBLeft = 0
            DIBTop = 0
            OnClick = FastIMG1Click
            OnDblClick = FastIMG1DblClick
            OnMouseMove = FastIMG1MouseMove
          end
        end
      end
      object FastIMG2: TFastIMG
        Left = 8
        Top = 12
        Width = 42
        Height = 42
        Transparent = False
        Picture = '(None)'
        AutoSize = False
        SizeMode = smGDI
        DrawStyle = dsDraw
        DIBLeft = 0
        DIBTop = 0
      end
      object ComboBox1: TComboBox
        Left = 60
        Top = 34
        Width = 208
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = ComboBox1Change
        Items.Strings = (
          'Round'
          'Square'
          'Graphical')
      end
      object TrackBar4: TTrackBar
        Left = 4
        Top = 216
        Width = 268
        Height = 25
        Anchors = [akLeft, akBottom]
        Max = 100
        Frequency = 2
        TabOrder = 4
        ThumbLength = 10
        OnChange = TrackBar4Change
        OnEnter = TrackBar1Enter
      end
      object Panel1: TPanel
        Left = 8
        Top = 68
        Width = 260
        Height = 129
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          260
          129)
        object Label3: TLabel
          Left = 0
          Top = 28
          Width = 19
          Height = 13
          Caption = 'Size'
        end
        object Label4: TLabel
          Left = 228
          Top = 28
          Width = 31
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Label4'
        end
        object Label7: TLabel
          Left = 0
          Top = 48
          Width = 6
          Height = 13
          Caption = 'X'
        end
        object Label8: TLabel
          Left = 151
          Top = 48
          Width = 6
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Y'
        end
        object Label2: TLabel
          Left = 0
          Top = 0
          Width = 67
          Height = 13
          Caption = 'Pen Options'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Bevel1: TThemeBevel
          Left = 1
          Top = 16
          Width = 263
          Height = 2
          Anchors = [akLeft, akTop, akRight]
        end
        object Label5: TLabel
          Left = 0
          Top = 76
          Width = 41
          Height = 13
          Caption = 'Rotation'
        end
        object Label6: TLabel
          Left = 229
          Top = 76
          Width = 31
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Label4'
        end
        object TrackBar1: TTrackBar
          Left = 8
          Top = 48
          Width = 104
          Height = 25
          Max = 32
          Min = 1
          PageSize = 8
          Frequency = 2
          Position = 1
          TabOrder = 0
          ThumbLength = 10
          OnChange = TrackBar1Change
          OnEnter = TrackBar1Enter
        end
        object TrackBar3: TTrackBar
          Left = 159
          Top = 48
          Width = 104
          Height = 25
          Anchors = [akTop, akRight]
          Max = 32
          Min = 1
          PageSize = 8
          Frequency = 2
          Position = 1
          TabOrder = 1
          ThumbLength = 10
          OnChange = TrackBar1Change
          OnEnter = TrackBar1Enter
        end
        object TrackBar2: TTrackBar
          Left = -4
          Top = 92
          Width = 268
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Max = 359
          PageSize = 1
          Frequency = 10
          TabOrder = 2
          ThumbLength = 10
          OnChange = TrackBar2Change
          OnEnter = TrackBar1Enter
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Pen Fill Style'
      ImageIndex = 1
      DesignSize = (
        279
        257)
      object Panel3: TPanel
        Left = 8
        Top = 12
        Width = 260
        Height = 232
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 0
        object ScrollBox2: TScrollBox
          Left = 0
          Top = 0
          Width = 260
          Height = 232
          Align = alClient
          Color = clWindow
          ParentColor = False
          TabOrder = 0
          object FastIMG3: TFastIMG
            Left = 0
            Top = 0
            Width = 256
            Height = 213
            Transparent = False
            Picture = '(None)'
            AutoSize = False
            SizeMode = smGDI
            DrawStyle = dsDraw
            DIBLeft = 0
            DIBTop = 0
            OnClick = FastIMG3Click
            OnDblClick = FastIMG3DblClick
            OnMouseMove = FastIMG3MouseMove
          end
        end
      end
    end
  end
  object Button1: TButton
    Left = 232
    Top = 301
    Width = 63
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 160
    Top = 301
    Width = 63
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Okay'
    TabOrder = 2
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 305
    Width = 125
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Centre on mouse'
    TabOrder = 4
  end
end
