object BPProperties: TBPProperties
  Tag = 1
  Left = 393
  Top = 113
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Breakpoint Properties'
  ClientHeight = 171
  ClientWidth = 276
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
  object Button1: TButton
    Left = 167
    Top = 145
    Width = 47
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 219
    Top = 145
    Width = 53
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object PageControl1: TPageControl
    Left = 4
    Top = 4
    Width = 268
    Height = 133
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Breakpoint'
      object Label1: TLabel
        Left = 83
        Top = 61
        Width = 63
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Line Number:'
      end
      object Label2: TLabel
        Left = 171
        Top = 61
        Width = 54
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Statement:'
      end
      object CheckBox1: TCheckBox
        Left = 191
        Top = 12
        Width = 65
        Height = 17
        Alignment = taLeftJustify
        Anchors = [akTop, akRight]
        Caption = 'Enabled'
        TabOrder = 0
        OnClick = CheckBox1Click
      end
      object CheckBox3: TCheckBox
        Left = 207
        Top = 36
        Width = 49
        Height = 17
        Alignment = taLeftJustify
        Anchors = [akTop, akRight]
        Caption = 'Break '
        TabOrder = 1
      end
      object Edit1: TEdit
        Left = 83
        Top = 77
        Width = 85
        Height = 22
        Anchors = [akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnKeyDown = Edit1KeyDown
      end
      object Edit2: TEdit
        Left = 171
        Top = 77
        Width = 85
        Height = 22
        Anchors = [akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnKeyDown = Edit2KeyDown
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object Label4: TLabel
        Left = 8
        Top = 8
        Width = 49
        Height = 13
        Caption = 'Condition:'
      end
      object Label3: TLabel
        Left = 9
        Top = 57
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Log Text:'
      end
      object Edit3: TEdit
        Left = 8
        Top = 24
        Width = 248
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object Edit4: TEdit
        Left = 8
        Top = 76
        Width = 248
        Height = 22
        Anchors = [akLeft, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object CheckBox2: TCheckBox
        Left = 151
        Top = 57
        Width = 105
        Height = 17
        Alignment = taLeftJustify
        Anchors = [akRight, akBottom]
        Caption = 'Log as expression  '
        TabOrder = 2
      end
    end
  end
  object Button3: TButton
    Left = 4
    Top = 148
    Width = 45
    Height = 21
    Caption = 'Help'
    TabOrder = 3
    OnClick = Button3Click
  end
end
