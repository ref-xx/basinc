object TextForm: TTextForm
  Left = 538
  Top = 267
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Create Text'
  ClientHeight = 184
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 157
    Width = 77
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Create text as: '
  end
  object SpeedButton1: TSpeedButton
    Left = 316
    Top = 28
    Width = 23
    Height = 22
    AllowAllUp = True
    GroupIndex = 1
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777777777777777777777777777777777777777777777777000000007
      7777777700077000777777770007700077777777000770007777777700000007
      7777777700077000777777770007700077777777000770007777777000000007
      7777777777777777777777777777777777777777777777777777}
    OnClick = ComboBox2Change
  end
  object SpeedButton2: TSpeedButton
    Left = 344
    Top = 28
    Width = 23
    Height = 22
    AllowAllUp = True
    GroupIndex = 2
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777777777777777777777777777777777777777777777777000000777
      7777777770087777777777777800777777777777770087777777777777800777
      7777777777700877777777777778007777777777777700877777777777700000
      7777777777777777777777777777777777777777777777777777}
    OnClick = ComboBox2Change
  end
  object SpeedButton3: TSpeedButton
    Left = 372
    Top = 28
    Width = 23
    Height = 22
    AllowAllUp = True
    GroupIndex = 3
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777777777777777777777777777777770000000007777777777777777
      7777777770000077777777770087800777777777007770077777777700777007
      7777777700777007777777770077700777777777007770077777777700777007
      7777777000070000777777777777777777777777777777777777}
    OnClick = ComboBox2Change
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = 'Font Type'
  end
  object Label3: TLabel
    Left = 96
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Font Name'
  end
  object Label4: TLabel
    Left = 252
    Top = 8
    Width = 19
    Height = 13
    Caption = 'Size'
  end
  object Label5: TLabel
    Left = 316
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Attributes'
  end
  object ComboBox1: TComboBox
    Left = 96
    Top = 28
    Width = 149
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object ComboBox2: TComboBox
    Left = 252
    Top = 28
    Width = 57
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBox2Change
    Items.Strings = (
      '6'
      '8'
      '9'
      '10'
      '11'
      '12'
      '14'
      '16'
      '18'
      '20'
      '22'
      '24'
      '26'
      '28'
      '36'
      '48'
      '72')
  end
  object Memo1: TMemo
    Left = 8
    Top = 60
    Width = 301
    Height = 79
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
    OnChange = Memo1Change
  end
  object Button1: TButton
    Left = 332
    Top = 154
    Width = 63
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 268
    Top = 154
    Width = 55
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    TabOrder = 4
    OnClick = Button2Click
  end
  object ComboBox3: TComboBox
    Left = 88
    Top = 154
    Width = 97
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 5
    OnChange = ComboBox2Change
    Items.Strings = (
      'Image'
      'New selection')
  end
  object ComboBox4: TComboBox
    Left = 8
    Top = 28
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    OnChange = ComboBox4Change
    Items.Strings = (
      'Windows'
      'Sinclair')
  end
  object Panel1: TPanel
    Left = 316
    Top = 60
    Width = 79
    Height = 79
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 7
    object FastIMG1: TFastIMG
      Left = 0
      Top = 0
      Width = 79
      Height = 79
      Transparent = False
      Picture = '(None)'
      AutoSize = False
      SizeMode = smGDI
      DrawStyle = dsDraw
      DIBLeft = 0
      DIBTop = 0
      Align = alClient
      OnMouseDown = FastIMG1MouseDown
      OnMouseMove = FastIMG1MouseMove
      OnMouseUp = FastIMG1MouseUp
    end
  end
end
