object LogWindow: TLogWindow
  Left = 538
  Top = 250
  Width = 298
  Height = 213
  BorderIcons = [biSystemMenu]
  Caption = 'Log Window'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 4
    Top = 4
    Width = 273
    Height = 125
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object Button1: TButton
    Left = 4
    Top = 134
    Width = 54
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 65
    Top = 134
    Width = 64
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Save As...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 229
    Top = 134
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 184
    Top = 135
    Width = 41
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 4
    OnClick = Button4Click
  end
end
