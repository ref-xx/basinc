object RenumberForm: TRenumberForm
  Tag = 1
  Left = 1176
  Top = 118
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Renumber'
  ClientHeight = 214
  ClientWidth = 248
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
  DesignSize = (
    248
    214)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 136
    Width = 77
    Height = 13
    Caption = 'Starting at Line:'
  end
  object Label2: TLabel
    Left = 130
    Top = 136
    Width = 107
    Height = 13
    Caption = 'Increment in Steps of:'
  end
  object Label4: TLabel
    Left = 8
    Top = 108
    Width = 72
    Height = 13
    Caption = 'Using Lines...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TThemeBevel
    Left = 8
    Top = 124
    Width = 232
    Height = 2
    Anchors = [akLeft, akTop, akRight]
  end
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 68
    Height = 13
    Caption = 'Renumber...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel2: TThemeBevel
    Left = 8
    Top = 24
    Width = 232
    Height = 2
    Anchors = [akLeft, akTop, akRight]
  end
  object Label6: TLabel
    Left = 104
    Top = 56
    Width = 55
    Height = 13
    Caption = 'From Start:'
    Enabled = False
  end
  object Label7: TLabel
    Left = 180
    Top = 56
    Width = 37
    Height = 13
    Caption = 'To End:'
    Enabled = False
  end
  object Button1: TButton
    Left = 124
    Top = 184
    Width = 52
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 182
    Top = 184
    Width = 58
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 152
    Width = 101
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '10'
  end
  object Edit2: TEdit
    Left = 130
    Top = 152
    Width = 113
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '10'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 32
    Width = 121
    Height = 17
    Caption = 'The Whole Program'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object Edit3: TEdit
    Left = 104
    Top = 72
    Width = 61
    Height = 22
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    Text = '1'
  end
  object Edit4: TEdit
    Left = 180
    Top = 72
    Width = 63
    Height = 22
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Text = '9999'
  end
  object Button3: TButton
    Left = 8
    Top = 184
    Width = 49
    Height = 21
    Caption = 'Help'
    TabOrder = 7
    OnClick = Button3Click
  end
end
