object PrintForm: TPrintForm
  Tag = 1
  Left = 563
  Top = 76
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Print'
  ClientHeight = 488
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label27: TLabel
    Left = 4
    Top = 8
    Width = 75
    Height = 13
    Caption = 'Print Preview'
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
    Width = 317
    Height = 2
  end
  object Label1: TLabel
    Left = 6
    Top = 352
    Width = 73
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Print Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TThemeBevel
    Left = 6
    Top = 368
    Width = 315
    Height = 2
    Anchors = [akLeft, akBottom]
  end
  object Shape1: TShape
    Left = 100
    Top = 80
    Width = 65
    Height = 65
    Brush.Color = clBlack
  end
  object Label2: TLabel
    Left = 145
    Top = 380
    Width = 59
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Data Source'
  end
  object Label3: TLabel
    Left = 154
    Top = 407
    Width = 55
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Scaling (%)'
  end
  object Label4: TLabel
    Left = 239
    Top = 338
    Width = 55
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Page 88/88'
  end
  object SpeedButton2: TSpeedButton
    Left = 301
    Top = 336
    Width = 17
    Height = 17
    Anchors = [akRight, akBottom]
    Glyph.Data = {
      56010000424D560100000000000036000000280000000A000000090000000100
      18000000000020010000130B0000130B00000000000000000000C5C7C5C5C7C5
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5000000000000C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5000000000000000000C5C7C5C5C7C5C5C7C50000C5C7C5000000
      000000000000000000000000000000000000C5C7C5C5C7C50000C5C7C5000000
      000000000000000000000000000000000000000000C5C7C50000C5C7C5000000
      000000000000000000000000000000000000C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5000000000000000000C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5000000000000C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C50000}
    Layout = blGlyphTop
    OnClick = SpeedButton2Click
  end
  object SpeedButton1: TSpeedButton
    Left = 220
    Top = 336
    Width = 17
    Height = 17
    Anchors = [akRight, akBottom]
    Glyph.Data = {
      56010000424D560100000000000036000000280000000A000000090000000100
      18000000000020010000130B0000130B00000000000000000000C5C7C5C5C7C5
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5000000000000C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5000000000000000000C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      000000000000000000000000000000000000000000C5C7C50000C5C7C5000000
      000000000000000000000000000000000000000000C5C7C50000C5C7C5C5C7C5
      000000000000000000000000000000000000000000C5C7C50000C5C7C5C5C7C5
      C5C7C5000000000000000000C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5000000000000C5C7C5C5C7C5C5C7C5C5C7C50000C5C7C5C5C7C5
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C50000}
    Layout = blGlyphTop
    OnClick = SpeedButton1Click
  end
  object Label5: TLabel
    Left = 2
    Top = 387
    Width = 78
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Break at Column'
  end
  object FastIMG1: TFastIMG
    Left = 4
    Top = 32
    Width = 317
    Height = 301
    Transparent = False
    Picture = '(None)'
    AutoSize = False
    SizeMode = smGDI
    DrawStyle = dsDraw
    DIBLeft = 0
    DIBTop = 0
  end
  object ComboBox1: TComboBox
    Left = 212
    Top = 376
    Width = 106
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
    Items.Strings = (
      'ZX Printer'
      'Current Program'
      'Screen')
  end
  object Button1: TButton
    Left = 160
    Top = 460
    Width = 51
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Print'
    Default = True
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 272
    Top = 460
    Width = 46
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 8
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 213
    Top = 433
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Multiple columns'
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object UpDown1: TUpDown
    Left = 306
    Top = 405
    Width = 12
    Height = 21
    Anchors = [akRight, akBottom]
    Min = 0
    Position = 0
    TabOrder = 4
    Wrap = False
    OnChangingEx = UpDown1ChangingEx
  end
  object MaskEdit1: TMaskEdit
    Left = 213
    Top = 405
    Width = 93
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Text = '100'
    OnChange = MaskEdit1Change
    OnKeyDown = MaskEdit1KeyDown
  end
  object Button3: TButton
    Left = 2
    Top = 460
    Width = 100
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Printer Settings...'
    TabOrder = 6
    OnClick = Button3Click
  end
  object MaskEdit2: TMaskEdit
    Left = 2
    Top = 405
    Width = 93
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Text = '100'
    OnChange = MaskEdit2Change
    OnKeyDown = MaskEdit2KeyDown
  end
  object UpDown2: TUpDown
    Left = 95
    Top = 405
    Width = 12
    Height = 21
    Anchors = [akRight, akBottom]
    Min = 0
    Position = 0
    TabOrder = 2
    Wrap = False
    OnChangingEx = UpDown2ChangingEx
  end
  object CheckBox2: TCheckBox
    Left = 2
    Top = 433
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Display as-is'
    TabOrder = 10
    OnClick = CheckBox2Click
  end
  object Button4: TButton
    Left = 220
    Top = 460
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 11
    OnClick = Button4Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 12
    Top = 40
  end
  object PrintDialog1: TPrintDialog
    Options = [poPageNums, poSelection, poWarning, poDisablePrintToFile]
    Left = 44
    Top = 40
  end
end
