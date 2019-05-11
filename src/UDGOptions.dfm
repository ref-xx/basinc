object UDGNew: TUDGNew
  Tag = 1
  Left = 436
  Top = 302
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'New...'
  ClientHeight = 156
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 88
    Top = 96
    Width = 108
    Height = 13
    Caption = 'Graphic Height (Pixels)'
  end
  object Label2: TLabel
    Left = 92
    Top = 68
    Width = 103
    Height = 13
    Caption = 'Graphic width (Bytes)'
  end
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 132
    Height = 13
    Caption = 'Create new Graphic Set'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 8
    Top = 24
    Width = 265
    Height = 2
  end
  object Label6: TLabel
    Left = 120
    Top = 40
    Width = 73
    Height = 13
    Caption = 'No. of graphics'
  end
  object Button1: TButton
    Left = 156
    Top = 127
    Width = 52
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 214
    Top = 127
    Width = 58
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 204
    Top = 92
    Width = 69
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '8'
  end
  object Edit2: TEdit
    Left = 204
    Top = 64
    Width = 69
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '1'
  end
  object Edit4: TEdit
    Left = 204
    Top = 36
    Width = 69
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '21'
  end
  object Button3: TButton
    Left = 8
    Top = 127
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = Button3Click
  end
end
