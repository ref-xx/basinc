object BreakpointsWindow: TBreakpointsWindow
  Left = 238
  Top = 210
  Width = 447
  Height = 276
  BorderIcons = [biSystemMenu]
  Caption = 'Breakpoints'
  Color = clBtnFace
  Constraints.MinHeight = 24
  Constraints.MinWidth = 160
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 4
    Top = 4
    Width = 398
    Height = 96
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Width = 20
      end
      item
        Caption = 'Line'
      end
      item
        Caption = 'Source'
        Width = 150
      end
      item
        Caption = 'Condition'
        Width = 130
      end
      item
        Caption = 'Count'
      end>
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListView1Change
    OnKeyDown = ListView1KeyDown
    OnSelectItem = ListView1SelectItem
  end
  object Button2: TButton
    Left = 366
    Top = 142
    Width = 53
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 5
    Top = 143
    Width = 72
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Add New...'
    TabOrder = 1
    OnClick = AddBreakpoint1Click
  end
  object Button4: TButton
    Left = 81
    Top = 143
    Width = 48
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Delete'
    TabOrder = 2
    OnClick = Delete1Click
  end
  object Button5: TButton
    Left = 133
    Top = 143
    Width = 64
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Properties'
    TabOrder = 3
    OnClick = Properties1Click
  end
  object Button1: TButton
    Left = 311
    Top = 144
    Width = 49
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = Button1Click
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 12
    Top = 28
    object AddBreakpoint1: TMenuItem
      Caption = 'Add Breakpoint...'
      OnClick = AddBreakpoint1Click
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
    object ViewSource1: TMenuItem
      Caption = 'View Source'
      OnClick = ViewSource1Click
    end
    object Properties1: TMenuItem
      Caption = 'Properties'
      OnClick = Properties1Click
    end
  end
end
