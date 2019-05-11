object ErrorDescriptions: TErrorDescriptions
  Tag = 1
  Left = 508
  Top = 288
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Error Help'
  ClientHeight = 182
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
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
    Width = 90
    Height = 13
    Caption = 'Get Description for'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 338
    Height = 116
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Label2'
    WordWrap = True
  end
  object ComboBox1: TComboBox
    Left = 104
    Top = 4
    Width = 249
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
    Items.Strings = (
      '0 OK'
      '1 NEXT without FOR'
      '2 Variable not found'
      '3 Subscript wrong'
      '4 Out of memory'
      '5 Out of screen'
      '6 Number too big'
      '7 RETURN without GO SUB'
      '8 End of file'
      '9 STOP statement'
      'A Invalid argument'
      'B Integer out of range'
      'C Nonsense in BASIC'
      'D BREAK - CONT repeats'
      'E Out of DATA'
      'F Invalid filename'
      'G No room for line'
      'H STOP in INPUT'
      'I NEXT without FOR'
      'J Invalid I/O device'
      'K Invalid colour'
      'L BREAK into program'
      'M RAMTOP no good'
      'N Statement lost'
      'O Invalid Stream'
      'P FN without DEF'
      'Q Parameter error'
      'R File loading error'
      'a MERGE error'
      'b Wrong file type'
      'c CODE error'
      'd Too many brackets'
      'e File already exists'
      'f Invalid name'
      'g RAM disk error'
      'h File does not exist'
      'i Invalid device'
      'j Invalid BAUD rate'
      'k Invalid note name'
      'l Number too large'
      'm Note out of range'
      'n Out of range'
      'o Too many tied notes')
  end
  object Button2: TButton
    Left = 289
    Top = 157
    Width = 61
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 232
    Top = 157
    Width = 53
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 2
    OnClick = Button1Click
  end
end
