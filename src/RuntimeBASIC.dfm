object RunTimeWindow: TRunTimeWindow
  Left = 476
  Top = 357
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Runtime BASIC viewer'
  ClientHeight = 140
  ClientWidth = 353
  Color = clBtnFace
  Constraints.MinHeight = 188
  Constraints.MinWidth = 361
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 4
    Top = 114
    Width = 57
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Run'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 64
    Top = 114
    Width = 45
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Step'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 284
    Top = 114
    Width = 65
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Go to line'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 112
    Top = 114
    Width = 69
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Step Over'
    TabOrder = 2
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 184
    Top = 114
    Width = 57
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Run To'
    TabOrder = 3
    OnClick = Button6Click
  end
  object ListBox1: TCheckListBox
    Left = 4
    Top = 4
    Width = 345
    Height = 106
    Anchors = [akLeft, akTop, akRight, akBottom]
    Flat = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    PopupMenu = PopupMenu1
    Style = lbOwnerDrawFixed
    TabOrder = 5
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
    OnDrawItem = ListBox1DrawItem
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object View1: TMenuItem
      Caption = 'View'
      object ShowBreakpoints1: TMenuItem
        Tag = 1
        Caption = 'Show Breakpoints'
        OnClick = MenuItemClick
      end
      object ShowWatches1: TMenuItem
        Tag = 4
        Caption = 'Show Watches'
        OnClick = MenuItemClick
      end
      object ShowVariables1: TMenuItem
        Tag = 2
        Caption = 'Show Variables'
        OnClick = MenuItemClick
      end
      object ShowSysVars1: TMenuItem
        Tag = 3
        Caption = 'Show SysVars'
        OnClick = MenuItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Tag = 5
        Caption = 'Close'
        OnClick = MenuItemClick
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object ToggleBreakpoint1: TMenuItem
        Tag = 6
        Caption = 'Toggle Breakpoint'
        OnClick = MenuItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object CopySelected1: TMenuItem
        Tag = 8
        Caption = 'Copy Selected'
        OnClick = MenuItemClick
      end
      object SendSelectedtoEdit1: TMenuItem
        Tag = 9
        Caption = 'Send Selected to Edit'
        OnClick = MenuItemClick
      end
    end
    object Run1: TMenuItem
      Caption = 'Run'
      object Run2: TMenuItem
        Caption = 'Run'
        OnClick = Button1Click
      end
      object Goto1: TMenuItem
        Caption = 'Go to'
        OnClick = Button4Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object SingleStepStatement1: TMenuItem
        Caption = 'Single Step Statement'
        OnClick = Button2Click
      end
      object NextLine1: TMenuItem
        Caption = 'Step Over Statement'
        OnClick = Button5Click
      end
      object RunTo1: TMenuItem
        Caption = 'Run To'
        OnClick = Button6Click
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 40
    Top = 8
    object Goto2: TMenuItem
      Caption = 'Go To'
      OnClick = Button4Click
    end
    object RunTo2: TMenuItem
      Caption = 'Run To'
      OnClick = Button6Click
    end
    object ToggleBreakpoint2: TMenuItem
      Tag = 6
      Caption = 'Toggle Breakpoint'
      OnClick = MenuItemClick
    end
    object SendSelectedtoEdit2: TMenuItem
      Tag = 9
      Caption = 'Send Selected to Edit'
      OnClick = MenuItemClick
    end
  end
end
