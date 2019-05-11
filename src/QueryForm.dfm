object QueryWindow: TQueryWindow
  Tag = 1
  Left = 732
  Top = 282
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Query Window'
  ClientHeight = 76
  ClientWidth = 208
  Color = clBtnFace
  Constraints.MaxHeight = 114
  Constraints.MinHeight = 24
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 117
    Height = 13
    Caption = 'Your question goes here'
  end
  object Button1: TButton
    Left = 93
    Top = 48
    Width = 49
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Okay'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 146
    Top = 48
    Width = 57
    Height = 21
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 4
    Top = 20
    Width = 199
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 4
    Top = 20
    Width = 199
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyDown = Edit1KeyDown
  end
end
