object BinaryWindow: TBinaryWindow
  Tag = 1
  Left = 635
  Top = 269
  Width = 510
  Height = 335
  BorderIcons = [biSystemMenu]
  Caption = 'BasinC Bridge'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    494
    296)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 119
    Height = 13
    Caption = 'Objects to be routed:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 8
    Top = 154
    Width = 85
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Import options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel4: TThemeBevel
    Left = 8
    Top = 170
    Width = 484
    Height = 2
    Anchors = [akLeft, akRight, akBottom]
  end
  object Button1: TButton
    Left = 433
    Top = 255
    Width = 61
    Height = 21
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 377
    Top = 255
    Width = 53
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Okay'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 255
    Width = 45
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Panel2: TPanel
    Left = 8
    Top = 28
    Width = 485
    Height = 98
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 5
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 485
      Height = 98
      Align = alClient
      Columns = <
        item
          Caption = 'Object Name'
        end
        item
          Caption = 'Route To'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnAdvancedCustomDrawItem = ListView1AdvancedCustomDrawItem
      OnSelectItem = ListView1SelectItem
    end
    object Button4: TButton
      Left = 284
      Top = 24
      Width = 19
      Height = 21
      Caption = '...'
      PopupMenu = PopupMenu1
      TabOrder = 1
      Visible = False
      OnMouseDown = Button4MouseDown
    end
  end
  object Button5: TButton
    Left = 56
    Top = 255
    Width = 45
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Panel1: TPanel
    Left = 8
    Top = 95
    Width = 485
    Height = 86
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 0
    DesignSize = (
      481
      82)
    object Label2: TLabel
      Left = 172
      Top = 7
      Width = 65
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Bytes per line'
    end
    object Label4: TLabel
      Left = 406
      Top = 7
      Width = 22
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Step'
    end
    object Label3: TLabel
      Left = 320
      Top = 7
      Width = 24
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Start'
    end
    object Label5: TLabel
      Left = 376
      Top = 58
      Width = 39
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Address'
    end
    object Edit1: TEdit
      Left = 244
      Top = 4
      Width = 57
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 0
      Text = 'Edit1'
    end
    object CheckBox1: TCheckBox
      Left = 384
      Top = 31
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      Caption = 'Include POKEs'
      TabOrder = 3
      OnClick = CheckBox2Click
    end
    object Edit3: TEdit
      Left = 352
      Top = 4
      Width = 45
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = 'Edit1'
    end
    object Edit2: TEdit
      Left = 436
      Top = 4
      Width = 45
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 2
      Text = 'Edit1'
    end
    object Edit4: TEdit
      Left = 424
      Top = 56
      Width = 57
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 4
      Text = 'Edit1'
    end
  end
  object Panel3: TPanel
    Left = 24
    Top = 127
    Width = 485
    Height = 86
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 6
    DesignSize = (
      481
      82)
    object Label7: TLabel
      Left = 356
      Top = 7
      Width = 59
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Line Number'
    end
    object Label10: TLabel
      Left = 288
      Top = 58
      Width = 39
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Address'
    end
    object Edit5: TEdit
      Left = 424
      Top = 4
      Width = 57
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 0
      Text = 'Edit1'
    end
    object CheckBox2: TCheckBox
      Left = 416
      Top = 33
      Width = 65
      Height = 17
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      Caption = 'Add stub'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object Edit8: TEdit
      Left = 336
      Top = 56
      Width = 57
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 2
      Text = 'Edit1'
    end
    object CheckBox3: TCheckBox
      Left = 404
      Top = 57
      Width = 77
      Height = 17
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      Caption = 'Force Jump'
      TabOrder = 3
    end
  end
  object Panel4: TPanel
    Left = 8
    Top = 119
    Width = 485
    Height = 86
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 8
    DesignSize = (
      481
      82)
    object Label8: TLabel
      Left = 344
      Top = 12
      Width = 39
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Address'
    end
    object Label14: TLabel
      Left = 320
      Top = 60
      Width = 74
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Target Address'
      Enabled = False
    end
    object Edit6: TEdit
      Left = 408
      Top = 8
      Width = 65
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 0
      Text = '0'
    end
    object ChkMemoryCompressZX0: TCheckBox
      Left = 360
      Top = 35
      Width = 113
      Height = 17
      Hint = 'Compress Content using zx0'
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      Caption = 'Compress with zx0'
      TabOrder = 1
      OnClick = ChkMemoryCompressZX0Click
    end
    object ChkMemoryEmbedZX0: TCheckBox
      Left = 224
      Top = 60
      Width = 89
      Height = 17
      Hint = 'This adds 81 byte zx0 depacker as header'
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      Caption = 'Add Depacker'
      Enabled = False
      TabOrder = 2
      OnClick = ChkMemoryEmbedZX0Click
    end
    object EditMemEmbedAdr: TEdit
      Left = 402
      Top = 56
      Width = 73
      Height = 21
      Anchors = [akTop, akRight]
      Enabled = False
      TabOrder = 3
      Text = '16384'
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 143
    Width = 485
    Height = 86
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 7
    object Label9: TLabel
      Left = 60
      Top = 32
      Width = 363
      Height = 13
      Caption = 
        'Tries to extract basic program from a memory image -- no options' +
        ' available.'
    end
  end
  object Panel6: TPanel
    Left = 24
    Top = 56
    Width = 487
    Height = 86
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 9
    DesignSize = (
      483
      82)
    object Label11: TLabel
      Left = 352
      Top = 12
      Width = 39
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Address'
    end
    object Label12: TLabel
      Left = 208
      Top = 12
      Width = 46
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'File Name'
    end
    object Label13: TLabel
      Left = 320
      Top = 60
      Width = 74
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Target Address'
      Enabled = False
    end
    object Edit7: TEdit
      Left = 402
      Top = 8
      Width = 73
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 0
      Text = '0'
    end
    object Edit9: TEdit
      Left = 266
      Top = 8
      Width = 73
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = 'Grabbed'
    end
    object ChkTapeCompressZX0: TCheckBox
      Left = 360
      Top = 35
      Width = 113
      Height = 17
      Hint = 'Compress Content using zx0'
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      Caption = 'Compress with zx0'
      TabOrder = 2
      OnClick = ChkTapeCompressZX0Click
    end
    object ChkTapeEmbedZX0: TCheckBox
      Left = 224
      Top = 60
      Width = 89
      Height = 17
      Hint = 'This adds 81 byte zx0 depacker as header'
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      Caption = 'Add Depacker'
      Enabled = False
      TabOrder = 3
      OnClick = ChkTapeEmbedZX0Click
    end
    object EditTapeEmbedAdr: TEdit
      Left = 402
      Top = 56
      Width = 73
      Height = 21
      Anchors = [akTop, akRight]
      Enabled = False
      TabOrder = 4
      Text = '16384'
    end
  end
  object Panel7: TPanel
    Left = 16
    Top = 176
    Width = 481
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'No Options Available For Man Type'
    TabOrder = 10
    Visible = False
    DesignSize = (
      477
      77)
    object Label15: TLabel
      Left = 332
      Top = 20
      Width = 54
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Block Name'
      Visible = False
    end
    object Edit10: TEdit
      Left = 400
      Top = 16
      Width = 65
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 0
      Text = 'Routed'
      Visible = False
    end
  end
  object Panel8: TPanel
    Left = 5
    Top = 160
    Width = 481
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 11
    Visible = False
    DesignSize = (
      477
      77)
    object Label16: TLabel
      Left = 28
      Top = 12
      Width = 128
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Where to insert new data?'
    end
    object Edit11: TEdit
      Left = 8
      Top = 32
      Width = 65
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 0
      Text = 'UdgData'
      Visible = False
    end
    object Edit12: TEdit
      Left = 8
      Top = 56
      Width = 65
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = '32768'
      Visible = False
    end
    object Edit13: TEdit
      Left = 408
      Top = 52
      Width = 33
      Height = 21
      Anchors = [akTop, akRight]
      Enabled = False
      TabOrder = 2
      Text = '1'
    end
    object RadioButton1: TRadioButton
      Left = 280
      Top = 8
      Width = 137
      Height = 17
      Caption = 'Create New UDG Set'
      Checked = True
      OnClick = UDGTargetClick
      TabOrder = 3
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 280
      Top = 32
      Width = 129
      Height = 17
      Caption = 'Selected UDG'
      OnClick = UDGTargetClick
      TabOrder = 4
    end
    object RadioButton3: TRadioButton
      Left = 280
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Send to Index #'
      OnClick = UDGTargetClick
      TabOrder = 5
    end
    object CheckBox4: TCheckBox
      Left = 32
      Top = 48
      Width = 161
      Height = 17
      Hint = 'Compress Content using zx0'
      Anchors = [akTop, akRight]
      Caption = 'Insert as New Characters'
      Enabled = False
      TabOrder = 6
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 12
    Top = 48
    object DATADecimal1: TMenuItem
      Caption = 'Listing DATA Decimal'
      OnClick = DATADecimal1Click
    end
    object DATAHex1: TMenuItem
      Tag = 1
      Caption = 'Listing DATA Hex'
      OnClick = DATADecimal1Click
    end
    object REMstatement1: TMenuItem
      Tag = 2
      Caption = 'Listing REM statement'
      OnClick = DATADecimal1Click
    end
    object ExtractBASIC1: TMenuItem
      Tag = 3
      Caption = 'Extract BASIC'
      OnClick = DATADecimal1Click
    end
    object SendtoMemory1: TMenuItem
      Tag = 4
      Caption = 'Send to Memory'
      OnClick = DATADecimal1Click
    end
    object SendToTape1: TMenuItem
      Tag = 5
      Caption = 'Send To Tape'
      OnClick = DATADecimal1Click
    end
    object SendToManager1: TMenuItem
      Tag = 6
      Caption = 'Send To Manager'
      OnClick = DATADecimal1Click
    end
    object SendToUDGEditor1: TMenuItem
      Tag = 7
      Caption = 'Send To UDG Editor'
      OnClick = DATADecimal1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object RemoveThisEntry1: TMenuItem
      Caption = 'Delete'
      OnClick = RemoveThisEntry1Click
    end
  end
end
