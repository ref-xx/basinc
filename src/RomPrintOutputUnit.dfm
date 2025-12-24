object RomPrintOutputWindow: TRomPrintOutputWindow
  Left = 1406
  Top = 370
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Screen Text Capture'
  ClientHeight = 450
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 281
    Height = 409
    Font.Charset = TURKISH_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 424
    Width = 89
    Height = 21
    Caption = 'Clear Log'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 424
    Width = 89
    Height = 21
    Caption = 'Stop Capture'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 224
    Top = 424
    Width = 65
    Height = 21
    Caption = 'Close'
    TabOrder = 3
    OnClick = Button3Click
  end
end
