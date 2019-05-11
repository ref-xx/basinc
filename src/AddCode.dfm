object AddCodeWindow: TAddCodeWindow
  Tag = 1
  Left = 332
  Top = 138
  Width = 297
  Height = 195
  BorderIcons = [biSystemMenu]
  Caption = 'Add Program Lines'
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 4
    Width = 227
    Height = 13
    Caption = 'Add the following code to the current program?'
  end
  object Memo1: TMemo
    Left = 4
    Top = 24
    Width = 281
    Height = 105
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 171
    Top = 136
    Width = 52
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 227
    Top = 136
    Width = 58
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 7
    Top = 136
    Width = 50
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Help'
    TabOrder = 3
    OnClick = Button3Click
  end
end
