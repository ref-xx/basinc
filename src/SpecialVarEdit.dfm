object SpecialVarsWindow: TSpecialVarsWindow
  Tag = 1
  Left = 552
  Top = 285
  Anchors = []
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'SpecialVarform'
  ClientHeight = 226
  ClientWidth = 247
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 183
    Top = 197
    Width = 61
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button1Click
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 4
    Width = 240
    Height = 186
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'GroupBox1'
    TabOrder = 0
    object Notebook1: TNotebook
      Left = 2
      Top = 15
      Width = 236
      Height = 169
      Align = alClient
      PageIndex = 2
      TabOrder = 0
      object TPage
        Left = 0
        Top = 0
        Caption = 'Default'
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'NumberArray'
        object ListBox1: TListBox
          Left = 4
          Top = 0
          Width = 213
          Height = 149
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          Style = lbOwnerDrawFixed
          TabOrder = 0
          OnDrawItem = ListBox1DrawItem
          OnMouseDown = ListBox1MouseDown
        end
        object Edit1: TEdit
          Left = 12
          Top = 8
          Width = 205
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Visible = False
          OnKeyDown = Edit1KeyDown
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'FORVar'
        object Label2: TLabel
          Left = 8
          Top = 8
          Width = 70
          Height = 13
          Caption = 'Current Value:'
        end
        object Label3: TLabel
          Left = 8
          Top = 52
          Width = 47
          Height = 13
          Caption = 'TO Value:'
        end
        object Label4: TLabel
          Left = 120
          Top = 52
          Width = 57
          Height = 13
          Caption = 'STEP Value:'
        end
        object Label5: TLabel
          Left = 8
          Top = 104
          Width = 57
          Height = 13
          Caption = 'Repeats at:'
        end
        object Label6: TLabel
          Left = 8
          Top = 120
          Width = 23
          Height = 13
          Caption = 'Line:'
        end
        object Label7: TLabel
          Left = 116
          Top = 120
          Width = 50
          Height = 13
          Caption = 'Statement'
        end
        object Edit2: TEdit
          Left = 8
          Top = 24
          Width = 217
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Text = 'Edit2'
          OnKeyDown = Edit2KeyDown
        end
        object Edit3: TEdit
          Left = 8
          Top = 68
          Width = 105
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Text = 'Edit2'
          OnKeyDown = Edit2KeyDown
        end
        object Edit4: TEdit
          Left = 120
          Top = 68
          Width = 105
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          Text = 'Edit2'
          OnKeyDown = Edit2KeyDown
        end
        object Edit5: TEdit
          Left = 8
          Top = 136
          Width = 105
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          Text = 'Edit2'
          OnKeyDown = Edit2KeyDown
        end
        object Edit6: TEdit
          Left = 120
          Top = 136
          Width = 105
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          Text = 'Edit2'
          OnKeyDown = Edit2KeyDown
        end
      end
    end
  end
  object Button2: TButton
    Left = 118
    Top = 197
    Width = 61
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    TabOrder = 1
    OnClick = Button2Click
  end
end
