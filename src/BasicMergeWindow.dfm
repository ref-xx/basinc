object BasicMergeForm: TBasicMergeForm
  Left = 1477
  Top = 560
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Clean Code'
  ClientHeight = 273
  ClientWidth = 277
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 257
    Height = 225
    Caption = 'Minify Options'
    TabOrder = 0
    object Label1: TLabel
      Left = 56
      Top = 152
      Width = 48
      Height = 13
      Caption = 'Start Line:'
    end
    object Label2: TLabel
      Left = 136
      Top = 152
      Width = 45
      Height = 13
      Caption = 'End Line:'
    end
    object Label3: TLabel
      Left = 16
      Top = 124
      Width = 170
      Height = 13
      Caption = 'Max Chars per Line (Standard=249):'
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 48
      Width = 137
      Height = 17
      Caption = 'Join DATA Statements'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 72
      Width = 225
      Height = 17
      Caption = 'Further process PRINT and DATA'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 96
      Width = 225
      Height = 17
      Caption = 'Do not process lines marked as Breakpoints'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object Edit1: TEdit
      Left = 56
      Top = 168
      Width = 57
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object Edit2: TEdit
      Left = 136
      Top = 168
      Width = 57
      Height = 21
      TabOrder = 4
      Text = '9999'
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 200
      Width = 225
      Height = 17
      Caption = 'I know not all programs can be minified'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 24
      Width = 217
      Height = 17
      Caption = 'Remove All Comments (REM'#39's)'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object Edit3: TEdit
      Left = 200
      Top = 120
      Width = 41
      Height = 21
      TabOrder = 7
      Text = '768'
    end
  end
  object Button1: TButton
    Left = 190
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 110
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Minify!'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 6
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 3
    OnClick = Button3Click
  end
end
