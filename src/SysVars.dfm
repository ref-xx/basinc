object SysVarsWindow: TSysVarsWindow
  Left = 536
  Top = 262
  Width = 302
  Height = 213
  BorderIcons = [biSystemMenu]
  Caption = 'System Variables'
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
  object Panel1: TPanel
    Left = 4
    Top = 4
    Width = 286
    Height = 139
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clActiveBorder
    TabOrder = 0
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 282
      Height = 135
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Name'
          Width = 75
        end
        item
          Caption = 'Address'
          Width = 85
        end
        item
          Caption = 'Value'
          Width = 120
        end>
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      ParentShowHint = False
      PopupMenu = PopupMenu1
      ShowHint = True
      TabOrder = 0
      ViewStyle = vsReport
      OnInfoTip = ListView1InfoTip
    end
  end
  object Button1: TButton
    Left = 244
    Top = 151
    Width = 46
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 153
    Top = 151
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = Timer1Timer
  end
  object ComboBox1: TComboBox
    Left = 4
    Top = 151
    Width = 145
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBox1Change
    Items.Strings = (
      'Do not Update'
      'Update on Step'
      'Update Every 1 Sec'
      'Update Every 5 Sec'
      'Update Every 10 Secs'
      'Update Every 30 Secs'
      'Update Every Minute')
  end
  object Button3: TButton
    Left = 204
    Top = 150
    Width = 37
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 10
    Top = 26
  end
  object PopupMenu1: TPopupMenu
    Left = 10
    Top = 58
    object EditthisSysVar1: TMenuItem
      Caption = 'Edit this SysVar'
      OnClick = EditthisSysVar1Click
    end
    object WatchthisSysVar1: TMenuItem
      Caption = 'Watch this SysVar'
      OnClick = WatchthisSysVar1Click
    end
    object RefreshtheList1: TMenuItem
      Caption = 'Refresh the List'
      OnClick = RefreshtheList1Click
    end
  end
end
