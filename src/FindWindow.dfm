object FindForm: TFindForm
  Tag = 1
  Left = 55
  Top = 147
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Find...'
  ClientHeight = 223
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 11
    Width = 63
    Height = 13
    Caption = 'Text to find: '
    Transparent = True
  end
  object Button2: TButton
    Left = 166
    Top = 196
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Find'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 220
    Top = 196
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = Button1Click
  end
  object ComboBox1: TComboBox
    Left = 80
    Top = 8
    Width = 190
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
    OnKeyDown = ComboBox1KeyDown
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 36
    Width = 113
    Height = 61
    Caption = ' Direction '
    ItemIndex = 0
    Items.Strings = (
      'Forward'
      'Backward')
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 104
    Width = 262
    Height = 80
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Options '
    TabOrder = 3
    object CheckBox1: TCheckBox
      Left = 8
      Top = 16
      Width = 149
      Height = 17
      Caption = 'Case Sensitive'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 36
      Width = 133
      Height = 17
      Caption = 'Expression'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 56
      Width = 153
      Height = 17
      Caption = 'Whole word only'
      TabOrder = 2
    end
  end
  object RadioGroup2: TRadioGroup
    Left = 128
    Top = 36
    Width = 142
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Origin '
    Items.Strings = (
      'Start of BASIC'
      'Current Program Cursor')
    TabOrder = 2
  end
  object Button3: TButton
    Left = 12
    Top = 196
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = Button3Click
  end
end
