object EvaluationWindow: TEvaluationWindow
  Left = 596
  Top = 280
  Width = 289
  Height = 204
  BorderIcons = [biSystemMenu]
  Caption = 'Evaluate Expression'
  Color = clBtnFace
  Constraints.MinHeight = 24
  Constraints.MinWidth = 160
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Expression :'
  end
  object Label2: TLabel
    Left = 8
    Top = 52
    Width = 37
    Height = 13
    Caption = 'Result :'
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 24
    Width = 266
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 0
    OnChange = ComboBox1Change
    OnKeyDown = ComboBox1KeyDown
  end
  object Memo1: TMemo
    Left = 8
    Top = 68
    Width = 266
    Height = 47
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 120
    Width = 69
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Evaluate'
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
  object Button2: TButton
    Left = 80
    Top = 120
    Width = 89
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Add as Watch'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 227
    Top = 120
    Width = 47
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 188
    Top = 120
    Width = 37
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = Button4Click
  end
end
