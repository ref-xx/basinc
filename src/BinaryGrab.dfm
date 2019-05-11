object BinaryGrabWindow: TBinaryGrabWindow
  Left = 548
  Top = 380
  BorderStyle = bsDialog
  Caption = 'Memory block grabber'
  ClientHeight = 132
  ClientWidth = 285
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
    Width = 150
    Height = 13
    Caption = 'Memory block parameters'
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
    Width = 265
    Height = 2
  end
  object Label6: TLabel
    Left = 152
    Top = 40
    Width = 39
    Height = 13
    Caption = 'Address'
  end
  object Label2: TLabel
    Left = 108
    Top = 72
    Width = 83
    Height = 13
    Caption = 'Block size (bytes)'
  end
  object Edit4: TEdit
    Left = 196
    Top = 36
    Width = 77
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '0'
    OnKeyDown = Edit4KeyDown
  end
  object Edit2: TEdit
    Left = 196
    Top = 68
    Width = 77
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '0'
  end
  object Button3: TButton
    Left = 8
    Top = 101
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 160
    Top = 101
    Width = 52
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 218
    Top = 101
    Width = 58
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button2Click
  end
end
