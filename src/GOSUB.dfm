object GOSUBWindow: TGOSUBWindow
  Left = 720
  Top = 253
  Width = 225
  Height = 234
  BorderIcons = [biSystemMenu]
  Caption = 'GOSUB Stack Viewer'
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
    Left = 4
    Top = 4
    Width = 61
    Height = 13
    Caption = 'Stack Size:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 72
    Top = 4
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object ListBox1: TListBox
    Left = 4
    Top = 20
    Width = 210
    Height = 157
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object Button1: TButton
    Left = 149
    Top = 184
    Width = 63
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 73
    Top = 184
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Show Source'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 182
    Width = 45
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = Button3Click
  end
end
