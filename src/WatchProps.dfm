object WatchProperties: TWatchProperties
  Tag = 1
  Left = 586
  Top = 185
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Watch Properties'
  ClientHeight = 149
  ClientWidth = 201
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 6
    Top = 28
    Width = 58
    Height = 13
    Caption = 'Watch Type'
  end
  object Button1: TButton
    Left = 96
    Top = 124
    Width = 43
    Height = 21
    Caption = 'Okay'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 144
    Top = 124
    Width = 53
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 136
    Top = 4
    Width = 61
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Enabled'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object ComboBox1: TComboBox
    Left = 76
    Top = 24
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = ComboBox1Change
    Items.Strings = (
      'Expression'
      'Variable'
      'System Variable'
      'Memory Address')
  end
  object Notebook1: TNotebook
    Left = 6
    Top = 48
    Width = 195
    Height = 69
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'Default'
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 52
        Height = 13
        Caption = 'Expression'
      end
      object Edit1: TEdit
        Left = 0
        Top = 16
        Width = 190
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyDown = Edit1KeyDown
      end
      object CheckBox3: TCheckBox
        Left = 81
        Top = 44
        Width = 109
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Break on non-zero  '
        TabOrder = 1
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'SYSVAR'
      object Label3: TLabel
        Left = 0
        Top = 0
        Width = 38
        Height = 13
        Caption = 'Variable'
      end
      object ComboBox2: TComboBox
        Left = 0
        Top = 16
        Width = 190
        Height = 21
        ItemHeight = 0
        TabOrder = 0
        Text = 'ComboBox2'
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'VARIABLE'
      object Label4: TLabel
        Left = 0
        Top = 0
        Width = 76
        Height = 13
        Caption = 'System Variable'
      end
      object ComboBox3: TComboBox
        Left = 0
        Top = 16
        Width = 190
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'ADDRESS'
      object Label5: TLabel
        Left = 0
        Top = 0
        Width = 39
        Height = 13
        Caption = 'Address'
      end
      object Label6: TLabel
        Left = 52
        Top = 48
        Width = 45
        Height = 13
        Caption = 'Data Size'
      end
      object Edit2: TEdit
        Left = 0
        Top = 16
        Width = 190
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object ComboBox4: TComboBox
        Left = 104
        Top = 44
        Width = 86
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          'Byte'
          'Word'
          'DWord')
      end
    end
  end
  object Button3: TButton
    Left = 8
    Top = 124
    Width = 45
    Height = 21
    Caption = 'Help'
    TabOrder = 5
    OnClick = Button3Click
  end
end
