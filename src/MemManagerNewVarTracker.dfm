object MemManagerVarTracker: TMemManagerVarTracker
  Left = 192
  Top = 125
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Var Tracker'
  ClientHeight = 167
  ClientWidth = 179
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelName: TLabel
    Left = 8
    Top = 12
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label1: TLabel
    Left = 8
    Top = 36
    Width = 38
    Height = 13
    Caption = 'Address'
  end
  object Label2: TLabel
    Left = 8
    Top = 60
    Width = 20
    Height = 13
    Caption = 'Size'
  end
  object Edit1: TEdit
    Left = 48
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Var1'
  end
  object Edit2: TEdit
    Left = 48
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 1
    Text = '32767'
  end
  object Edit3: TEdit
    Left = 48
    Top = 56
    Width = 25
    Height = 21
    TabOrder = 2
    Text = '4'
  end
  object ButtonGetCursor: TButton
    Left = 128
    Top = 32
    Width = 41
    Height = 21
    Caption = 'Get'
    TabOrder = 3
    OnClick = ButtonGetCursorClick
  end
  object TrackBarSize: TTrackBar
    Left = 80
    Top = 56
    Width = 89
    Height = 25
    Max = 8
    Min = 1
    Position = 1
    TabOrder = 4
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = TrackBarSizeChange
  end
  object Button1: TButton
    Left = 96
    Top = 136
    Width = 73
    Height = 21
    Caption = 'Create'
    Default = True
    TabOrder = 5
    OnClick = Button1Click
  end
  object CheckTrack: TCheckBox
    Left = 48
    Top = 88
    Width = 73
    Height = 17
    Caption = 'Tracking'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object Button2: TButton
    Left = 16
    Top = 136
    Width = 73
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = Button2Click
  end
end
