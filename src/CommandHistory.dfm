object CommandWindow: TCommandWindow
  Left = 598
  Top = 260
  Width = 298
  Height = 218
  BorderIcons = [biSystemMenu]
  Caption = 'Command History'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnActivate = ListBox1Click
  OnCreate = FormCreate
  OnDeactivate = ListBox1Click
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 4
    Top = 4
    Width = 266
    Height = 98
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
    OnMouseDown = ListBox1MouseDown
  end
  object Button2: TButton
    Left = 137
    Top = 106
    Width = 79
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Send To Edit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 221
    Top = 106
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 4
    Top = 107
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = Button3Click
  end
end
