object AssembleForm: TAssembleForm
  Left = 616
  Top = 252
  BorderStyle = bsSingle
  Caption = 'Assemble as DATA Options'
  ClientHeight = 212
  ClientWidth = 278
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
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel3: TThemeBevel
    Left = 8
    Top = 24
    Width = 265
    Height = 2
  end
  object Label4: TLabel
    Left = 84
    Top = 40
    Width = 65
    Height = 13
    Caption = 'Bytes per line'
  end
  object Label6: TLabel
    Left = 8
    Top = 100
    Width = 87
    Height = 13
    Caption = 'Line Numbering'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel4: TThemeBevel
    Left = 8
    Top = 116
    Width = 265
    Height = 2
  end
  object Label7: TLabel
    Left = 112
    Top = 132
    Width = 37
    Height = 13
    Caption = 'Start at'
  end
  object Label8: TLabel
    Left = 124
    Top = 160
    Width = 22
    Height = 13
    Caption = 'Step'
  end
  object Button1: TButton
    Left = 154
    Top = 181
    Width = 52
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 212
    Top = 181
    Width = 58
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 156
    Top = 36
    Width = 117
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '4'
  end
  object CheckBox2: TCheckBox
    Left = 156
    Top = 64
    Width = 121
    Height = 17
    Caption = 'Include POKE code'
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 156
    Top = 128
    Width = 117
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '9000'
  end
  object Edit4: TEdit
    Left = 156
    Top = 156
    Width = 117
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '10'
  end
end
