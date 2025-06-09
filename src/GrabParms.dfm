object UDGGrabWindow: TUDGGrabWindow
  Left = 11
  Top = 354
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Sprite/Graphic Editor'
  ClientHeight = 223
  ClientWidth = 273
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    273
    223)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Grab graphics from...'
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
    Width = 261
    Height = 2
  end
  object Label2: TLabel
    Left = 144
    Top = 40
    Width = 39
    Height = 13
    Caption = 'Address'
  end
  object Label3: TLabel
    Left = 92
    Top = 68
    Width = 93
    Height = 13
    Caption = 'Number of graphics'
  end
  object Label4: TLabel
    Left = 8
    Top = 108
    Width = 121
    Height = 13
    Caption = 'Data Format Settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 120
    Top = 140
    Width = 66
    Height = 13
    Caption = 'Width (Bytes)'
  end
  object Label6: TLabel
    Left = 116
    Top = 168
    Width = 69
    Height = 13
    Caption = 'Height (Pixels)'
  end
  object Bevel1: TThemeBevel
    Left = 8
    Top = 124
    Width = 261
    Height = 2
  end
  object Edit1: TEdit
    Left = 192
    Top = 36
    Width = 77
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '65368'
  end
  object Edit2: TEdit
    Left = 192
    Top = 64
    Width = 77
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '21'
  end
  object Button1: TButton
    Left = 153
    Top = 194
    Width = 52
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 211
    Top = 194
    Width = 58
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 194
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Edit3: TEdit
    Left = 192
    Top = 136
    Width = 77
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '65368'
  end
  object Edit4: TEdit
    Left = 192
    Top = 164
    Width = 77
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '21'
  end
end
