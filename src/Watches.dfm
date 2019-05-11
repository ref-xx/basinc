object WatchWindow: TWatchWindow
  Left = 1483
  Top = 884
  Width = 383
  Height = 156
  BorderIcons = [biSystemMenu]
  Caption = 'Watch List'
  Color = clBtnFace
  UseDockManager = True
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
  object ListView1: TListView
    Left = 4
    Top = 4
    Width = 350
    Height = 36
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Width = 20
      end
      item
        Caption = 'Expression'
        Width = 250
      end
      item
        Caption = 'Result'
        Width = 100
      end>
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListView1Change
    OnDblClick = ListView1DblClick
    OnKeyDown = ListView1KeyDown
    OnSelectItem = ListView1SelectItem
  end
  object Button2: TButton
    Left = 316
    Top = 45
    Width = 38
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 260
    Top = 45
    Width = 54
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = Refresh1Click
  end
  object Button3: TButton
    Left = 5
    Top = 45
    Width = 72
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Add New...'
    TabOrder = 3
    OnClick = AddWatch1Click
  end
  object Button4: TButton
    Left = 81
    Top = 45
    Width = 48
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Delete'
    TabOrder = 4
    OnClick = Delete1Click
  end
  object Button5: TButton
    Left = 133
    Top = 45
    Width = 64
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Properties'
    TabOrder = 5
    OnClick = Properties1Click
  end
  object Button6: TButton
    Left = 220
    Top = 44
    Width = 37
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = Button6Click
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 8
    Top = 24
    object AddWatch1: TMenuItem
      Caption = 'Add Watch...'
      OnClick = AddWatch1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object EnableAll1: TMenuItem
      Caption = 'Enable All'
      OnClick = EnableAll1Click
    end
    object DisableAll1: TMenuItem
      Caption = 'Disable All'
      OnClick = DisableAll1Click
    end
    object DeleteAll1: TMenuItem
      Caption = 'Delete All'
      OnClick = DeleteAll1Click
    end
    object ENabled1: TMenuItem
      Caption = 'Enabled'
      OnClick = ENabled1Click
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      OnClick = Delete1Click
    end
    object Properties1: TMenuItem
      Caption = 'Properties'
      OnClick = Properties1Click
    end
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh1Click
    end
  end
end
