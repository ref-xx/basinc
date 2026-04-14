object FormCompiler1: TFormCompiler1
  Left = 1240
  Top = 464
  BorderStyle = bsToolWindow
  Caption = 'MCODER3 Compiler'
  ClientHeight = 240
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Compile'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 1
  end
  object Button3: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save As..'
    TabOrder = 2
  end
  object Button4: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 313
    Height = 161
    BevelOuter = bvNone
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Button5: TButton
    Left = 247
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 5
  end
end
