object UlaColoursWindow: TUlaColoursWindow
  Left = 704
  Top = 455
  BorderStyle = bsDialog
  Caption = 'Ula64 Palette Editor'
  ClientHeight = 345
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    579
    345)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 13
    Top = 4
    Width = 96
    Height = 13
    Caption = 'My Ula64 Palette'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 309
    Top = 4
    Width = 97
    Height = 13
    Caption = 'Available Colours'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TThemeBevel
    Left = 12
    Top = 24
    Width = 259
    Height = 2
  end
  object ThemeBevel1: TThemeBevel
    Left = 308
    Top = 24
    Width = 259
    Height = 2
  end
  object Label3: TLabel
    Left = 293
    Top = 312
    Width = 58
    Height = 13
    Caption = 'Start Line:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PresetPalette: TFastIMG
    Left = 12
    Top = 32
    Width = 557
    Height = 265
    Transparent = False
    Picture = '(None)'
    AutoSize = False
    SizeMode = smGDI
    DrawStyle = dsDraw
    DIBLeft = 0
    DIBTop = 0
    OnMouseDown = PresetPaletteMouseDown
  end
  object Button1: TButton
    Left = 516
    Top = 305
    Width = 49
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 404
    Top = 305
    Width = 107
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Export to Basic'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 8
    Top = 304
    Width = 257
    Height = 33
    BevelInner = bvLowered
    TabOrder = 3
    object Edit2: TEdit
      Left = 8
      Top = 6
      Width = 241
      Height = 21
      TabOrder = 0
      Text = 'Entry: 0 = 122  OUT 48955, 0: OUT 65339, 122'
    end
  end
  object ComboBox1: TComboBox
    Left = 16
    Top = 272
    Width = 161
    Height = 21
    ItemHeight = 13
    TabOrder = 4
    Text = 'Untitled Preset'
    Visible = False
    OnChange = ComboBox1Change
  end
  object Button3: TButton
    Left = 236
    Top = 274
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Delete'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    Visible = False
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 188
    Top = 274
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Add'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Visible = False
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 360
    Top = 308
    Width = 41
    Height = 21
    TabOrder = 7
    Text = '9990'
  end
end
