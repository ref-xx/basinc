object ColoursWindow: TColoursWindow
  Tag = 1
  Left = 881
  Top = 122
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Colours'
  ClientHeight = 274
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 8
    Width = 139
    Height = 13
    Caption = 'Syntax Highlight Options'
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
    Width = 259
    Height = 2
  end
  object Label2: TLabel
    Left = 5
    Top = 148
    Width = 36
    Height = 13
    Caption = 'Colour'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object CheckListBox1: TCheckListBox
    Left = 4
    Top = 32
    Width = 259
    Height = 109
    OnClickCheck = CheckListBox1ClickCheck
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    Items.Strings = (
      'Keywords'
      'Functions'
      'Comments'
      'Symbols'
      'Variables'
      'Undefined Variables'
      'Numbers'
      'Line Numbers'
      'Strings'
      'Editor Foreground'
      'Editor Background')
    ParentFont = False
    TabOrder = 0
    OnClick = CheckListBox1Click
  end
  object FastIMG1: TFastIMG
    Left = 4
    Top = 168
    Width = 259
    Height = 63
    Transparent = False
    Picture = '(None)'
    AutoSize = False
    SizeMode = smGDI
    DrawStyle = dsDraw
    DIBLeft = 0
    DIBTop = 0
    OnMouseDown = FastIMG1MouseDown
  end
  object CheckBox1: TCheckBox
    Left = 194
    Top = 235
    Width = 72
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Boldface'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 193
    Top = 255
    Width = 61
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Italics'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object Button1: TButton
    Left = 6
    Top = 248
    Width = 49
    Height = 21
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
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 60
    Top = 248
    Width = 49
    Height = 21
    Caption = 'Help'
    TabOrder = 5
    OnClick = Button2Click
  end
end
