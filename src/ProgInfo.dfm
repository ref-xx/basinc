object ProgInfoForm: TProgInfoForm
  Left = 510
  Top = 272
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Information'
  ClientHeight = 173
  ClientWidth = 305
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
    Left = 8
    Top = 8
    Width = 120
    Height = 13
    Caption = 'Program Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TThemeBevel
    Left = 8
    Top = 24
    Width = 293
    Height = 2
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 293
    Height = 113
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Label2'
  end
  object Button1: TButton
    Tag = 1
    Left = 184
    Top = 148
    Width = 63
    Height = 21
    Cancel = True
    Caption = 'Okay'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 252
    Top = 148
    Width = 49
    Height = 21
    Caption = 'Help'
    TabOrder = 1
    OnClick = Button2Click
  end
end
