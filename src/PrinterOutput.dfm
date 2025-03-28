object PrinterForm: TPrinterForm
  Left = 695
  Top = 258
  Width = 293
  Height = 204
  BorderIcons = [biSystemMenu]
  Caption = 'ZX Printer Output'
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
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 8
    Top = 148
    Width = 26
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    OnClick = BitBtn1Click
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000C30E0000C30E00000000000000000000008080008080
      0080800080800080800080800080800080800080800080800080800080800080
      8000808000808000808000808000808000000000000000000000000000000000
      0000000000000000000000000000000000008080008080008080008080000000
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000C6C6
      C600000000808000808000000000000000000000000000000000000000000000
      0000000000000000000000000000000000C6C6C6000000008080000000C6C6C6
      C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600FFFF00FFFF00FFFFC6C6C6C6C6C60000
      00000000000000008080000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C684
      8484848484848484C6C6C6C6C6C6000000C6C6C6000000008080000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00C6C6C6C6C6C6000000000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6
      C6C6C6C6C6C6C6C6C6C6C6000000C6C6C6000000C6C6C6000000008080000000
      000000000000000000000000000000000000000000000000000000C6C6C60000
      00C6C6C6000000000000008080008080000000FFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFF000000C6C6C6000000C6C6C6000000008080008080
      008080000000FFFFFF000000000000000000000000000000FFFFFF0000000000
      00000000000000008080008080008080008080000000FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000008080008080008080008080008080
      008080008080000000FFFFFF000000000000000000000000000000FFFFFF0000
      00008080008080008080008080008080008080008080000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000008080008080008080008080
      0080800080800080800000000000000000000000000000000000000000000000
      0000000000808000808000808000808000808000808000808000808000808000
      8080008080008080008080008080008080008080008080008080}
    Layout = blGlyphBottom
  end
  object BitBtn2: TBitBtn
    Left = 36
    Top = 148
    Width = 25
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    OnClick = BitBtn2Click
    Glyph.Data = {
      36060000424D3606000000000000360000002800000020000000100000000100
      18000000000000060000021F0000021F00000000000000000000E201E5E201E5
      E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201
      E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E2
      01E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5E201E5A57573
      A57573A57573A57573A57573A57573A57573A57573A57573A57573A575739461
      5AE201E5E201E5E201E5E201E58C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C8C
      8C8C8C8C8C8C8C8C8C8C8C8C8C8C777777E201E5E201E5E201E5E201E5A57973
      FFCFC6FFCFC6FFCFC6FFCFC6FFCFC6FFCFC6FFCFC6FFCFC6FFCFC6FFCFC69461
      5AE201E5E201E5E201E5E201E58C8C8CE3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
      E3E3E3E3E3E3E3E3E3E3E3E3E3E3777777E201E5E201E5E201E5E201E5AD7D73
      F7E7D6FFEBD6FFE7CEFFE3C6FFE3C6FFDFBDFFDBB5FFD7B5FFD7ADFFD3A59465
      63E201E5E201E5E201E5E201E5909090E7E7E7EBEBEBE7E7E7E3E3E3E3E3E3DE
      DEDEDADADADADADAD6D6D6D2D2D27C7C7CE201E5E201E5E201E5E201E5B5827B
      F7E7DEFFEFDEFFEBD6FFE7CEFFE3C6FFE3C6FFDFBDFFDBB5FFD7B5FFD7AD9C69
      63E201E5E201E5E201E5E201E5989898EBEBEBEFEFEFEBEBEBE7E7E7E3E3E3E3
      E3E3DEDEDEDADADADADADAD6D6D6808080E201E5E201E5E201E5E201E5B58A7B
      F7EFE7FFEFDEFFEFDEFFEBD6FFE7CEFFE3C6FFE3C6FFDFBDFFDBB5FFD7B59C6D
      63E201E5E201E5E201E5E201E5989898EFEFEFEFEFEFEFEFEFEBEBEBE7E7E7E3
      E3E3E3E3E3DEDEDEDADADADADADA808080E201E5E201E5E201E5E201E5BD8E7B
      F7EFE7FFF3E7FFEFDEFFEFDEFFEBD6FFE7CEFFE3C6FFE3C6FFDFBDFFDBB5A56D
      6BE201E5E201E5E201E5E201E59C9C9CEFEFEFF3F3F3EFEFEFEFEFEFEBEBEBE7
      E7E7E3E3E3E3E3E3DEDEDEDADADA888888E201E5E201E5E201E5E201E5C69684
      FFF3EFFFF7EFFFF3E7FFEFDEFFEFDEFFEBD6FFE7CEFFE3C6FFE3C6FFDFBDA571
      6BE201E5E201E5E201E5E201E5A5A5A5F7F7F7F7F7F7F3F3F3EFEFEFEFEFEFEB
      EBEBE7E7E7E3E3E3E3E3E3DEDEDE888888E201E5E201E5E201E5E201E5CE9A84
      FFF7F7FFFBF7FFF7EFFFF3E7FFEFDEFFEFDEFFEBD6FFE7CEFFE3C6FFE3C6AD75
      6BE201E5E201E5E201E5E201E5A9A9A9FBFBFBFBFBFBF7F7F7F3F3F3EFEFEFEF
      EFEFEBEBEBE7E7E7E3E3E3E3E3E38C8C8CE201E5E201E5E201E5E201E5D6A284
      FFFBFFFFFBF7FFFBF7FFF7EFFFF3E7FFEFDEFFEFDEFFEBD6FFE7CEFFE3C6AD79
      6BE201E5E201E5E201E5E201E5ADADADFDFDFDFBFBFBFBFBFBF7F7F7F3F3F3EF
      EFEFEFEFEFEBEBEBE7E7E7E3E3E38C8C8CE201E5E201E5E201E5E201E5DEAA8C
      FFFBFFFFFBF7FFFBF7FFFBF7FFF7EFFFF3E7FFEFDEFFEFDEFFEBD6FFE7CEAD79
      73E201E5E201E5E201E5E201E5B5B5B5FDFDFDFBFBFBFBFBFBFBFBFBF7F7F7F3
      F3F3EFEFEFEFEFEFEBEBEBE7E7E7909090E201E5E201E5E201E5E201E5DEAE8C
      FFFBF7FFFBF7FFFBF7FFFBF7FFFBF7FFF7EFFFF3E7FFEFDEFFD7CEFFB2B5B57D
      73E201E5E201E5E201E5E201E5B5B5B5FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF7
      F7F7F3F3F3EFEFEFE7E7E7D9D9D9949494E201E5E201E5E201E5E201E5E7B28C
      FFFBF7FFFBF7FFFBF7FFFBF7FFFBF7FFFBF7FFF7EFF7DFCEB57D73B57D73B57D
      73E201E5E201E5E201E5E201E5BABABAFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB
      FBFBF7F7F7E3E3E3949494949494949494E201E5E201E5E201E5E201E5EFBA94
      FFFBF7FFFBF7FFFBF7FFFBF7FFFBF7FFFBF7FFFBF7F7E3D6B57D73EFA652CE9A
      7BE201E5E201E5E201E5E201E5C2C2C2FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB
      FBFBFBFBFBE7E7E7949494A1A1A1A5A5A5E201E5E201E5E201E5E201E5EFBE94
      FFFBF7FFFBF7FFFBFFF7F7F7F7F7F7F7F3F7F7EFEFEFDBD6B57D73D6A284D69E
      84E201E5E201E5E201E5E201E5C2C2C2FBFBFBFBFBFBFDFDFDF7F7F7F7F7F7F5
      F5F5F3F3F3E3E3E3949494ADADADADADADE201E5E201E5E201E5E201E5F7BE94
      DEAA84DEAA84DEAA84DEAA84DEAA84DEAA84DEAA84DEAA84B57D73DEA684E201
      E5E201E5E201E5E201E5E201E5C6C6C6B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1B1
      B1B1B1B1B1B1B1B1949494B1B1B1E201E5E201E5E201E5E201E5}
    Layout = blGlyphBottom
    NumGlyphs = 2
  end
  object Button1: TButton
    Left = 230
    Top = 148
    Width = 48
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 270
    Height = 133
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = clAppWorkSpace
    TabOrder = 0
    object FastIMG1: TFastIMG
      Left = 0
      Top = 0
      Width = 256
      Height = 127
      Transparent = False
      Picture = '(None)'
      AutoSize = False
      SizeMode = smGDI
      DrawStyle = dsDraw
      DIBLeft = 0
      DIBTop = 0
      Color = clWhite
      ParentColor = False
    end
    object ScrollBar1: TScrollBar
      Left = 239
      Top = 0
      Width = 17
      Height = 121
      Enabled = False
      Kind = sbVertical
      Max = 0
      PageSize = 0
      TabOrder = 1
    end
  end
  object BitBtn3: TBitBtn
    Left = 96
    Top = 148
    Width = 49
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    OnClick = BitBtn3Click
    Glyph.Data = {
      36030000424D3603000000000000360000002800000020000000080000000100
      18000000000000030000130B0000130B00000000000000000000C5C7C5C5C7C5
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7
      C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5
      C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5
      000000000000000000000000C5C7C5C5C7C5C5C7C5C5C7C50000000000000000
      00000000C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5
      C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5000000
      C5C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7
      C5C5C7C5000000C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5
      C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5000000
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7
      C5C5C7C5000000C5C7C5C5C7C5000000000000000000000000000000C5C7C5C5
      C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5000000
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7
      C5C5C7C5000000C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5000000C5
      C7C5C5C7C5C5C7C5000000C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5000000
      C5C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7
      C5C5C7C5000000C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5000000C5
      C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5
      000000000000000000000000C5C7C5C5C7C5C5C7C5C5C7C50000000000000000
      00000000C5C7C5C5C7C5C5C7C5000000000000000000000000000000C5C7C5C5
      C7C5000000C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7
      C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5
      C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5}
    Layout = blGlyphBottom
  end
  object BitBtn4: TBitBtn
    Left = 68
    Top = 148
    Width = 25
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    OnClick = BitBtn4Click
    Glyph.Data = {
      4E010000424D4E010000000000003600000028000000090000000A0000000100
      18000000000018010000130B0000130B00000000000000000000C5C7C5C5C7C5
      C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C500C5C7C5C5C7C5C5C7C500
      0000000000000000C5C7C5C5C7C5C5C7C500C5C7C5C5C7C5C5C7C50000000000
      00000000C5C7C5C5C7C5C5C7C500C5C7C5C5C7C5C5C7C5000000000000000000
      C5C7C5C5C7C5C5C7C500C5C7C500000000000000000000000000000000000000
      0000C5C7C500C5C7C5000000000000000000000000000000000000000000C5C7
      C500C5C7C5C5C7C5000000000000000000000000000000C5C7C5C5C7C500C5C7
      C5C5C7C5C5C7C5000000000000000000C5C7C5C5C7C5C5C7C500C5C7C5C5C7C5
      C5C7C5C5C7C5000000C5C7C5C5C7C5C5C7C5C5C7C500C5C7C5C5C7C5C5C7C5C5
      C7C5C5C7C5C5C7C5C5C7C5C5C7C5C5C7C500}
    Layout = blGlyphBottom
  end
  object Button2: TButton
    Left = 186
    Top = 146
    Width = 41
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = Button2Click
  end
  object PopupMenu1: TPopupMenu
    Left = 12
    Top = 12
    object DisplayAreaBW1: TMenuItem
      Caption = 'Display Area (B&&W)'
      OnClick = DisplayAreaBW1Click
    end
    object DisplayAndAttrsColour1: TMenuItem
      Caption = 'Display And Attrs (Colour)'
      OnClick = DisplayAndAttrsColour1Click
    end
  end
end
