object MemBlockWindow: TMemBlockWindow
  Tag = 1
  Left = 640
  Top = 427
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Add Memory Block'
  ClientHeight = 94
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 70
    Height = 13
    Caption = 'StartAddress: '
  end
  object Label2: TLabel
    Left = 36
    Top = 40
    Width = 37
    Height = 13
    Caption = 'Length:'
  end
  object Edit1: TEdit
    Left = 80
    Top = 8
    Width = 121
    Height = 22
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyDown = Edit1KeyDown
  end
  object Edit2: TEdit
    Left = 80
    Top = 36
    Width = 121
    Height = 22
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnKeyDown = Edit1KeyDown
  end
  object Button1: TButton
    Left = 96
    Top = 68
    Width = 47
    Height = 21
    Caption = 'Okay'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 148
    Top = 68
    Width = 53
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 68
    Width = 37
    Height = 21
    Caption = 'Help'
    TabOrder = 4
    OnClick = Button3Click
  end
end
