object FormBasinCTips: TFormBasinCTips
  Left = 806
  Top = 312
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'BasinC Tips'
  ClientHeight = 314
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 160
    Height = 298
  end
  object Label1: TLabel
    Left = 176
    Top = 8
    Width = 179
    Height = 24
    Caption = 'BasinC Tip of the Day'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 176
    Top = 40
    Width = 337
    Height = 233
    BevelOuter = bvNone
    TabOrder = 0
    object Memo1: TRichEdit
      Left = 8
      Top = 8
      Width = 321
      Height = 217
      TabStop = False
      BevelOuter = bvNone
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Lines.Strings = (
        'RichEdit')
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object btnNext: TButton
    Left = 352
    Top = 278
    Width = 75
    Height = 25
    Caption = 'Next Tip'
    TabOrder = 2
    OnClick = btnNextClick
  end
  object btnWhatsNew: TButton
    Left = 176
    Top = 278
    Width = 91
    Height = 25
    Caption = 'What'#39's New'
    TabOrder = 3
    OnClick = btnWhatsNewClick
  end
  object btnClose: TButton
    Left = 440
    Top = 278
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object btnPrev: TButton
    Left = 272
    Top = 278
    Width = 75
    Height = 25
    Caption = 'Prev'
    TabOrder = 5
    OnClick = btnPrevClick
  end
  object ChkShowTips: TCheckBox
    Left = 440
    Top = 16
    Width = 73
    Height = 17
    Caption = 'Show Tips'
    TabOrder = 4
    OnClick = ChkShowTipsClick
  end
end
