object BlockProperties: TBlockProperties
  Tag = 1
  Left = 1673
  Top = 348
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Tape Block Properties'
  ClientHeight = 198
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    225
    198)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 30
    Height = 13
    Caption = 'Block'
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
    Width = 213
    Height = 2
  end
  object Notebook1: TNotebook
    Left = 8
    Top = 32
    Width = 213
    Height = 137
    PageIndex = 2
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'Default'
      object Label2: TLabel
        Left = 0
        Top = 4
        Width = 70
        Height = 13
        Caption = 'Program Name'
      end
      object Label3: TLabel
        Left = 108
        Top = 88
        Width = 58
        Height = 13
        Caption = 'Line number'
      end
      object Label6: TLabel
        Left = 0
        Top = 48
        Width = 60
        Height = 13
        Caption = 'Auto-start'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel3: TThemeBevel
        Left = 0
        Top = 64
        Width = 213
        Height = 2
      end
      object Edit1: TEdit
        Left = 0
        Top = 20
        Width = 213
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 108
        Top = 68
        Width = 61
        Height = 17
        BiDiMode = bdLeftToRight
        Caption = 'Enabled'
        ParentBiDiMode = False
        TabOrder = 1
        OnClick = CheckBox1Click
      end
      object Edit2: TEdit
        Left = 108
        Top = 104
        Width = 105
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Code'
      object Label4: TLabel
        Left = 0
        Top = 4
        Width = 54
        Height = 13
        Caption = 'Block Name'
      end
      object Label7: TLabel
        Left = 78
        Top = 80
        Width = 66
        Height = 13
        Caption = 'Start Address'
      end
      object Label8: TLabel
        Left = 50
        Top = 108
        Width = 94
        Height = 13
        Caption = 'Length (Read Only)'
      end
      object Label5: TLabel
        Left = 0
        Top = 48
        Width = 67
        Height = 13
        Caption = 'Parameters'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel2: TThemeBevel
        Left = 0
        Top = 64
        Width = 213
        Height = 2
      end
      object Edit3: TEdit
        Left = 0
        Top = 20
        Width = 213
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object Edit7: TEdit
        Left = 148
        Top = 76
        Width = 65
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object Edit8: TEdit
        Left = 148
        Top = 104
        Width = 65
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 2
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Variable'
      object Label9: TLabel
        Left = 0
        Top = 4
        Width = 54
        Height = 13
        Caption = 'Block Name'
      end
      object Edit4: TEdit
        Left = 0
        Top = 20
        Width = 213
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object Button1: TButton
    Left = 112
    Top = 170
    Width = 47
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 165
    Top = 170
    Width = 53
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 170
    Width = 49
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = Button3Click
  end
end
