object FormCompiler1: TFormCompiler1
  Left = 1268
  Top = 134
  Width = 359
  Height = 489
  BorderIcons = []
  BorderStyle = bsSizeToolWin
  FormStyle = fsStayOnTop
  Caption = '48K Compiler'
  Color = clBtnFace
  Constraints.MinHeight = 349
  Constraints.MinWidth = 359
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 343
    Height = 410
    ActivePage = TabSheetAdvanced
    Align = alClient
    TabOrder = 0
    object TabSheetSimple: TTabSheet
      Caption = 'Compiler Controls'
      DesignSize = (
        335
        382)
      object Button1: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Compile'
        Enabled = False
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 88
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Run'
        Enabled = False
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 88
        Top = 348
        Width = 81
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'to Tape'
        Enabled = False
        TabOrder = 2
        OnClick = Button3Click
      end
      object Memo1: TMemo
        Left = 8
        Top = 40
        Width = 313
        Height = 301
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 4
      end
      object Button6: TButton
        Left = 176
        Top = 348
        Width = 147
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Revert and Keep Editing'
        Enabled = False
        TabOrder = 3
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 8
        Top = 348
        Width = 73
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'to Bridge'
        Enabled = False
        TabOrder = 5
        OnClick = Button7Click
      end
    end
    object TabSheetAdvanced: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      DesignSize = (
        335
        382)
      object GroupCompiler: TGroupBox
        Left = 8
        Top = 8
        Width = 319
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use'
        TabOrder = 0
        object RadioMCoder3: TRadioButton
          Left = 8
          Top = 20
          Width = 153
          Height = 17
          Caption = 'Machine Coder Native z80'
          TabOrder = 0
          OnClick = CompilerModeClick
        end
        object RadioZXBasicDirect: TRadioButton
          Left = 193
          Top = 20
          Width = 104
          Height = 17
          Caption = 'ZX Basic-Direct'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = CompilerModeClick
        end
      end
      object GroupDefaultType: TGroupBox
        Left = 168
        Top = 152
        Width = 161
        Height = 41
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Default type'
        TabOrder = 1
        object RadioDefaultAuto: TRadioButton
          Left = 8
          Top = 20
          Width = 49
          Height = 17
          Caption = 'Auto'
          TabOrder = 0
        end
        object RadioDefaultFloat: TRadioButton
          Left = 57
          Top = 20
          Width = 56
          Height = 17
          Caption = 'Float'
          Checked = True
          TabOrder = 1
          TabStop = True
        end
      end
      object GroupVariableTypes: TGroupBox
        Left = 8
        Top = 200
        Width = 319
        Height = 173
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Variables need attention (Not a listing of all vars)'
        TabOrder = 2
        DesignSize = (
          319
          173)
        object ListVariables: TListView
          Left = 8
          Top = 16
          Width = 303
          Height = 117
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Caption = 'Variable'
              Width = 100
            end
            item
              Caption = 'Type'
              Width = 80
            end
            item
              Caption = 'Rename'
              Width = 115
            end>
          HideSelection = False
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
        object ButtonSelectAll: TButton
          Left = 8
          Top = 140
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Select All'
          TabOrder = 1
          OnClick = SelectAllClick
        end
        object ComboVariableType: TComboBox
          Left = 90
          Top = 142
          Width = 128
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akBottom]
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 2
          Text = 'default'
          Items.Strings = (
            'default'
            'ubyte'
            'byte'
            'uint'
            'int'
            'ulong'
            'long'
            'float'
            'fixed')
        end
        object ButtonApplyType: TButton
          Left = 226
          Top = 140
          Width = 77
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Apply'
          TabOrder = 3
          OnClick = ApplyTypeClick
        end
      end
      object GroupBox1: TGroupBox
        Left = 168
        Top = 64
        Width = 161
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable Expression Jump Tables'
        TabOrder = 3
        DesignSize = (
          161
          81)
        object CheckBox3: TCheckBox
          Left = 8
          Top = 48
          Width = 81
          Height = 17
          Caption = 'RESTORE'
          TabOrder = 0
          OnClick = TableOptionClick
        end
        object CheckBox2: TCheckBox
          Left = 8
          Top = 24
          Width = 81
          Height = 17
          Caption = 'GOTO/SUB'
          TabOrder = 1
          OnClick = TableOptionClick
        end
        object Edit2: TEdit
          Left = 88
          Top = 24
          Width = 65
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Text = '9000-9100'
        end
        object Edit3: TEdit
          Left = 88
          Top = 48
          Width = 65
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          Text = '9800-9900'
        end
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 64
        Width = 153
        Height = 129
        Caption = 'Target'
        TabOrder = 4
        object Label1: TLabel
          Left = 8
          Top = 20
          Width = 66
          Height = 13
          Caption = 'Start Address:'
        end
        object Label2: TLabel
          Left = 8
          Top = 50
          Width = 60
          Height = 13
          Caption = 'Optimization:'
        end
        object Edit1: TEdit
          Left = 88
          Top = 16
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '32768'
        end
        object ComboBox1: TComboBox
          Left = 88
          Top = 46
          Width = 57
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = 'Balanced'
          Items.Strings = (
            'Balanced'
            'Speed'
            'Size')
        end
        object CheckBox1: TCheckBox
          Left = 48
          Top = 72
          Width = 81
          Height = 17
          Caption = 'Aggressive'
          TabOrder = 2
        end
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 410
    Width = 343
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button5: TButton
      Left = 8
      Top = 8
      Width = 57
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = Button5Click
    end
    object Button4: TButton
      Left = 72
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 1
    end
  end
end
