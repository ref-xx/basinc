object GridSetUpWindow: TGridSetUpWindow
  Left = 613
  Top = 360
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Editing Grid Setup'
  ClientHeight = 166
  ClientWidth = 294
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
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 95
    Height = 13
    Caption = 'Grid Size Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TThemeBevel
    Left = 8
    Top = 24
    Width = 278
    Height = 2
    Anchors = [akLeft, akTop, akRight]
  end
  object Label6: TLabel
    Left = 177
    Top = 40
    Width = 28
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Width'
  end
  object Label2: TLabel
    Left = 173
    Top = 72
    Width = 31
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Height'
  end
  object Label1: TLabel
    Left = 12
    Top = 40
    Width = 19
    Height = 13
    Caption = 'Left'
    OnClick = Button1Click
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 18
    Height = 13
    Caption = 'Top'
  end
  object Label4: TLabel
    Left = 93
    Top = 108
    Width = 41
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Merging '
  end
  object Edit4: TEdit
    Left = 214
    Top = 36
    Width = 71
    Height = 22
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '4'
    OnKeyDown = Edit4KeyDown
  end
  object Edit2: TEdit
    Left = 214
    Top = 68
    Width = 71
    Height = 22
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '4'
    OnKeyDown = Edit4KeyDown
  end
  object Button3: TButton
    Left = 8
    Top = 137
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 7
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 171
    Top = 137
    Width = 52
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 227
    Top = 137
    Width = 58
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 41
    Top = 36
    Width = 73
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '4'
    OnKeyDown = Edit4KeyDown
  end
  object Edit3: TEdit
    Left = 41
    Top = 68
    Width = 73
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '4'
    OnKeyDown = Edit4KeyDown
  end
  object ComboBox1: TComboBox
    Left = 145
    Top = 104
    Width = 141
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Replace current'
      'Add to current'
      'Xor with current'
      'Subtract from current')
  end
end
