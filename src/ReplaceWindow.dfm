object ReplaceForm: TReplaceForm
  Tag = 1
  Left = 1143
  Top = 560
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Replace...'
  ClientHeight = 222
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 16
    Top = 11
    Width = 63
    Height = 13
    Caption = 'Text to find: '
    Transparent = True
  end
  object Label3: TLabel
    Left = 4
    Top = 39
    Width = 67
    Height = 13
    Caption = 'Replace With:'
    Transparent = True
  end
  object ComboBox1: TComboBox
    Left = 80
    Top = 8
    Width = 194
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
    OnKeyDown = ComboBox1KeyDown
  end
  object ComboBox2: TComboBox
    Left = 80
    Top = 36
    Width = 194
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnKeyDown = ComboBox1KeyDown
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 64
    Width = 113
    Height = 53
    Caption = ' Direction '
    ItemIndex = 0
    Items.Strings = (
      'Forward'
      'Backward')
    TabOrder = 2
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 120
    Width = 266
    Height = 71
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Options '
    TabOrder = 4
    object CheckBox1: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Case Sensitive'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Find Expression'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 48
      Width = 117
      Height = 17
      Caption = 'Replace Expression'
      TabOrder = 2
    end
  end
  object Button2: TButton
    Left = 148
    Top = 194
    Width = 70
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Replace All'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 224
    Top = 194
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = Button1Click
  end
  object RadioGroup1: TRadioGroup
    Left = 128
    Top = 64
    Width = 146
    Height = 53
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Origin '
    ItemIndex = 1
    Items.Strings = (
      'Start of BASIC'
      'Current Program Cursor')
    TabOrder = 3
  end
  object Button3: TButton
    Left = 93
    Top = 194
    Width = 56
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Replace'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 8
    Top = 198
    Width = 53
    Height = 21
    Caption = 'Help'
    TabOrder = 8
    OnClick = Button4Click
  end
end
