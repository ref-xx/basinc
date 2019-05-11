unit ImageImport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math, ShellAPI, AxCtrls,
  FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, FastDrawEx, FastFiles, FastSize, FastFX, GraphicEx,
  Utility;

type
  TBMPImportForm = class(TForm)
    FastIMG1: TFastIMG;
    FastIMG2: TFastIMG;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    Bevel1: TBevel;
    ComboBox1: TComboBox;
    Label4: TLabel;
    TrackBar5: TTrackBar;
    Label5: TLabel;
    ComboBox2: TComboBox;
    Image1: TImage;
    Image2: TImage;
    TrackBar1: TTrackBar;
    Image3: TImage;
    TrackBar2: TTrackBar;
    Button3: TButton;
    Button4: TButton;
    FastIMG3: TFastIMG;
    Label6: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label7: TLabel;
    CheckBox4: TCheckBox;
    Image4: TImage;
    TrackBar3: TTrackBar;
    Label8: TLabel;
    ComboBox3: TComboBox;
    Button2: TButton;
    CheckBox5: TCheckBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FastIMG3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FastIMG2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG2Exit(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CheckBox4Click(Sender: TObject);
    procedure TrackBar5Enter(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    OriginalDIB: TFastDIB;
    ScaledDIB: TFastDIB;
    SpecDIB: TFastDIB;
    PartDIB: TFastDIB;
    SizeDIB: TFastDIB;
    BackClr: Integer;
    SizeAltDIB: TFastDIB;
    GotOriginalDIB, GotScaledDIB: Boolean;
    FinishedDIB: Array[0..255, 0..191] of Byte;
    FinishedAttrs: Array[0..31, 0..23] of Byte;
    SizeRectX, SizeRectY, SizeRectW, SizeRectH, SizeMx, SizeMy, ConvRectX, ConvRectY, ConvRectW, ConvRectH, Mx, My: Integer;
    LastSizeRectX, LastSizeRectY, SizeOffX, SizeOffY, SizeWidth, SizeHeight, SizeMW, SizeMH: Integer;
    SizeMouseDown, SizeMouseMoved, MouseDown, MouseMoved: Boolean;
    Procedure ScaleDIB(UpdateOriginal: Boolean);
    Procedure DrawColours;
    Procedure ApplyModifiers(UpdateOriginal: Boolean);
    Procedure Convert;
    Procedure FileIsDropped(Var Msg: TMessage); Message WM_DropFiles;
    Procedure MakePartDIB;
    Procedure RGBToHSV(Const R,G,B: Extended; Var H,S,V: Extended);
  end;

var
  BMPImportForm: TBMPImportForm;

implementation

{$R *.DFM}

Uses Filing, FastCore, ColoursWind, Paintbox;

procedure TBMPImportForm.Button1Click(Sender: TObject);
Var
  Extension: AnsiString;
  Bitmap: TPicture;
  img: TImage;
  OleGraphic: TOLEGraphic;
  fs: TFileStream;
  bmp: TBitmap;
  w, h: integer;
begin

  If Sender <> nil Then
     Filename := OpenFile(Handle, 'Import image', [FTLoadPics, FTAll], '', False, False);

  If Filename = '' Then Exit;

  If Filename <> #255 Then Begin // a filename of #255 means that OriginalDIB has already been filled.
     If GetFile('') <> 'Ok' Then Exit;
     Extension := Lowercase(ExtractFileExt(Filename));

     If Extension = '.bmp' Then OriginalDIB.LoadFromFile(Filename);
     If (Extension = '.jpg') or (Extension = '.jpeg') Then Begin
        OleGraphic := TOleGraphic.Create;
        fs  := TFileStream.Create(FileName, fmOpenRead or fmSharedenyNone);
        OleGraphic.LoadFromStream(fs);
        img := TImage.Create(self);
        img.Picture.Assign(OleGraphic);
        if Assigned(img) then begin
           bmp := TBitmap.Create;
           bmp.Width := img.picture.Width;
           bmp.Height := img.picture.Height;
           bmp.PixelFormat := pf24bit;
           bmp.Canvas.Draw(0, 0, img.picture.graphic);
           OriginalDIB.SetSize(Bmp.Width, Bmp.Height, 32);
           BitBlt(OriginalDIB.hDc, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
           FreeAndNil(img);
           FreeAndNil(bmp);
        end;
     End;
     If (Extension = '.gif') or (Extension = '.png') Then Begin
        Bitmap := TPicture.Create;
        Bitmap.LoadFromFile(Filename);
        OriginalDIB.SetSize(Bitmap.Graphic.Width, Bitmap.Graphic.Height, 32);
        BitBlt(OriginalDIB.hDc, 0, 0, Bitmap.Graphic.Width, Bitmap.Graphic.Height, Bitmap.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
        Bitmap.Free;
     End;
  End;

  If OriginalDIB.Width <> 0 Then Begin
     GotOriginalDIB := True;
     GotScaledDIB := False;
     TrackBar2.Position := 256;
     Trackbar1.Position := 0;
     Trackbar5.Position := 0;
     TrackBar3.Position := 100;
     ConvRectX := 0;
     ConvRectY := 0;
     ConvRectW := 256;
     ConvRectH := 192;
     SizeRectX := 0;
     SizeRectY := 0;
     SizeRectW := OriginalDIB.Width;
     SizeRectH := OriginalDIB.Height;
     If (OriginalDIB.Width < 256) And (OriginalDIB.Height < 192) Then Begin
        ConvRectX := 128 - (OriginalDIB.Width Div 2);
        ConvRectY := 96 - (OriginalDIB.Height Div 2);
        ConvRectW := OriginalDIB.Width;
        ConvRectH := OriginalDIB.AbsHeight;
     End;
     ScaleDIB(True);
  End;

end;

procedure TBMPImportForm.FormCreate(Sender: TObject);
begin

  OriginalDIB := TFastDIB.Create;
  ScaledDIB := TFastDIB.Create;
  SpecDIB := TFastDIB.Create;
  PartDIB := TFastDIB.Create;
  SizeAltDIB := TFastDIB.Create;
  SizeDIB := TFastDIB.Create;

  ComboBox1.ItemIndex := 0;
  ComboBox2.ItemIndex := 0;
  ComboBox3.ItemIndex := 0;
  BackClr := 0;
  GotOriginalDIB := False;
  GotScaledDIB := False;

  TrackBar2.Position := 256;
  Trackbar1.Position := 0;
  Trackbar5.Position := 0;

end;

procedure TBMPImportForm.FormDestroy(Sender: TObject);
begin

  OriginalDIB.Free;
  SizeAltDIB.Free;
  ScaledDIB.Free;
  SpecDIB.Free;
  PartDIB.Free;
  SizeDIB.Free;

end;

procedure TBMPImportForm.FormShow(Sender: TObject);
begin

  DragAcceptFiles(Handle, True);
  SizeAltDIB.SetSize(256, 192, 32);

  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 32);
  FastIMG2.Bmp.SetSize(FastIMG2.Width, FastIMG2.Height, 32);

  FastIMG1.Bmp.Clear(FRGBn(DisplayPalette[7]));
  FastIMG2.Bmp.Clear(FRGBn(DisplayPalette[7]));

  FastDrawEx.Rectangle32(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width -1, FastIMG1.Bmp.Height -1, tfBlack);
  FastDrawEx.Rectangle32(FastIMG2.Bmp, 0, 0, FastIMG2.Bmp.Width -1, FastIMG2.Bmp.Height -1, tfBlack);

  DrawColours;

  If GotOriginalDIB Then ScaleDIB(True);

  FastIMG1.Repaint;
  FastIMG2.Repaint;

  CheckBox4.Checked := False;

end;

Procedure TBMPImportForm.MakePartDIB;
Var
  Sw, Sh, Sx, Sy: Integer;
Begin

  Sw := SizeRectW;
  Sh := SizeRectH;
  Sx := SizeRectX;
  Sy := SizeRectY;

  If SizeRectW < 0 Then Begin
     Sw := -SizeRectW;
     Sx := SizeRectX + SizeRectW;
  End;

  If SizeRectH < 0 Then Begin
     Sh := -SizeRectH;
     Sy := SizeRectY + SizeRectH;
  End;

  PartDIB.SetSize(SW, SH, 32);
  OriginalDIB.Draw(PartDIB.hDc, -SX, -SY);

End;

Procedure TBMPImportForm.ScaleDIB(UpdateOriginal: Boolean);
Var
  nWidth, nHeight: Integer;
  TempDIB: TFastDIB;
Begin

  If (SizeRectW = 0) or (SizeRectH = 0) Then Exit;

  MakePartDIB;

  ScaledDIB.SetSize(256, 192, 32);
  ScaledDIB.Clear(FRGBn(DisplayPalette[BackClr]));

  If CheckBox1.Checked or (PartDIB.Width > 256) or (PartDIB.Height > 192) Then Begin

     If PartDIB.Width / PartDIB.AbsHeight < 256/192 Then Begin

        nHeight := 192;
        nWidth := Round(nHeight * (PartDIB.Width/PartDIB.AbsHeight));

     End Else Begin

        nWidth := 256;
        nHeight := Round(nWidth * (PartDIB.AbsHeight/PartDIB.Width));

     End;

     SizeDIB.SetSize(nWidth, nHeight, 32);

     Case ComboBox1.ItemIndex of
        0: FastResize(PartDIB, SizeDIB);
        1: Bilinear(PartDIB, SizeDIB);
     End;

     SizeDIB.Draw(ScaledDIB.hDc, (256 - SizeDIB.Width) Div 2, (192 - SizeDIB.AbsHeight) Div 2);
     SizeOffX := (256 - SizeDIB.Width) Div 2;
     SizeOffY := (192 - SizeDIB.AbsHeight) Div 2;
     SizeWidth := SizeDIB.Width;
     SizeHeight := SizeDIB.AbsHeight;

  End Else Begin

     SizeDIB.SetSize(PartDIB.Width, PartDIB.Height, 32);
     PartDIB.Draw(SizeDIB.hDc, 0, 0);
     PartDIB.Draw(ScaledDIB.hDc, 128 - (PartDIB.Width Div 2), 96 - (PartDIB.Height Div 2));
     SizeOffX := 128 - (PartDIB.Width Div 2);
     SizeOffY := 96 - (PartDIB.Height Div 2);
     SizeWidth := PartDIB.Width;
     SizeHeight := PartDIB.AbsHeight;

  End;

  GotScaledDIB := True;
  ApplyModifiers(UpdateOriginal);
  Convert;

End;

procedure TBMPImportForm.ComboBox1Change(Sender: TObject);
begin

  If GotOriginalDIB Then ScaleDIB(True);

end;

Procedure TBMPImportForm.DrawColours;
Var
  Idx: Integer;
Begin

  FastIMG3.Bmp.SetSize(FastIMG3.Width, FastIMG3.Height, 24);
  FastIMG3.Bmp.Clear(TfBtnFace);
  FastDrawEx.Rectangle(FastIMG3.Bmp, 0, 0, FastIMG3.Bmp.Width -1, FastIMG3.Bmp.Height -1, TfBlack);

  For Idx := 0 To 7 Do Begin
     FastDrawEx.Rectangle(FastIMG3.Bmp, (Idx * 32)+2, 2, (Idx * 32)+32, 15, TfBlack);
     FastDrawEx.FillRect(FastIMG3.Bmp, (Idx*32)+3, 3, (Idx*32)+31, 15, FRGBn(DisplayPalette[Idx]));
     FastDrawEx.Rectangle(FastIMG3.Bmp, (Idx * 32)+2, 15, (Idx * 32)+32, 28, TfBlack);
     FastDrawEx.FillRect(FastIMG3.Bmp, (Idx*32)+3, 17, (Idx*32)+31, 28, FRGBn(DisplayPalette[Idx+8]));
  End;

  If BackClr < 8 Then Begin
     ColoursWindow.DrawCheck(FastIMG3.Bmp, (BackClr * 32) + 13, 6);
  End Else Begin
     ColoursWindow.DrawCheck(FastIMG3.Bmp, ((BackClr -8) * 32) + 13, 19);
  End;
  FastIMG3.Repaint;

End;

procedure TBMPImportForm.FastIMG3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  X := X Div 32;
  If X > 7 then X := 7;
  If Y > FastIMG3.Height Div 2 Then Inc(X, 8);
  BackClr := X;
  DrawColours;
  ScaleDIB(True);

end;

procedure TBMPImportForm.CheckBox1Click(Sender: TObject);
begin

  If GotOriginalDIB Then Begin
     ScaleDIB(True);
     ConvRectX := SizeOffX;
     ConvRectY := SizeOffY;
     ConvRectW := SizeWidth;
     ConvRectH := SizeHeight;
     ScaleDIB(True);
  End;

end;

procedure TBMPImportForm.TrackBar5Change(Sender: TObject);
begin

  Label12.Caption := FloatToStr(TrackBar3.Position/100);
  Label11.Caption := IntToStr(TrackBar2.Position -256);
  Label10.Caption := IntToStr(TrackBar1.Position);
  Label9.Caption := IntToStr(TrackBar5.Position);

  If GotScaledDIB Then Begin
     ApplyModifiers(True);
     Convert;
  End;

end;

Procedure TBMPImportForm.ApplyModifiers(UpdateOriginal: Boolean);
Var
  TempDIB: TFastDIB;
  X, Y, SMX, SMY, SMW, SMH, Rd, Gd, Bd: Integer;
  Clr: TFColorA;
Begin

  TempDIB := TFastDIB.Create;
  TempDIB.MakeCopy(ScaledDIB, True);

  Saturation(TempDIB, TrackBar2.Position);
  Contrast(TempDIB, TrackBar1.position, Trackbar1.Position, Trackbar1.Position);
  Lightness(TempDIB, TrackBar5.position, Trackbar5.Position, Trackbar5.Position);
  Gamma(TempDIB, TrackBar3.Position/100, TrackBar3.Position/100, TrackBar3.Position/100);

  If CheckBox5.Checked Then Begin // blow out the saturation and contrast

     For Y := 0 To 191 Do Begin
        For X := 0 To 255 Do Begin
           Clr := TempDIB.Pixels32[Y, X];
           Rd := 0; Gd := 0; Bd := 0;
{           If Clr.r > 128 Then Rd := 1;
           If Clr.g > 128 Then Gd := 1;
           If Clr.b > 128 Then Bd := 1;}

           If Clr.r < 86 Then Rd := 0 Else If Clr.r > 170 Then Rd := 2 Else Rd := 1;
           If Clr.g < 86 Then Gd := 0 Else If Clr.g > 170 Then Gd := 2 Else Gd := 1;
           If Clr.b < 86 Then Bd := 0 Else If Clr.b > 170 Then Bd := 2 Else Bd := 1;

           TempDIB.Pixels32[Y, X] := fRGBA(Rd * 255 Div 2, Gd * 255 Div 2, Bd * 255 Div 2, 0);
        End;
     End;

  End;

  If UpdateOriginal Then Begin
     TempDIB.Draw(FastIMG1.Bmp.hDc, 2, 2);
     TempDIB.Draw(SizeAltDIB.hDc, 0, 0);
     TempDIB.Free;
     FastIMG1.Repaint;
  End;

  If SizeMouseDown Then Begin

     SizeAltDIB.Draw(FastIMG1.Bmp.hDc, 2, 2);

     SMX := SizeMX;
     SMY := SizeMY;
     SMW := SizeMW;
     SMH := SizeMH;

     If SMW < 0 Then Begin
        SMW := - SMW;
        SMX := SMX - SMW;
     End;

     If SMH < 0 Then Begin
        SMH := -SMH;
        SMY := SMY - SMH;
     End;

     For X := SMX to SMX + SMW -1 Do Begin
        If (X >= 0) And (X <= 255) Then Begin
           If (191 - SMY >= 0) and (191 - SMY <= 191) Then
              FastIMG1.Bmp.Pixels32[191 - SMY, X] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
           If (191 - (SMY + SMH -1) >= 0) and (191 - (SMY + SMH -1) <= 191) Then
              FastIMG1.Bmp.Pixels32[191 - (SMY + SMH -1), X] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
        End;
     End;

     For Y := SMY to SMY + SMH -1 Do Begin
        If (191 - Y >= 0) and (191 - Y <= 191) Then Begin
           If (SMX >= 0) and (SMX <= 255) Then
              FastIMG1.Bmp.Pixels32[191 - Y, SMX] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
           If (SMX + SMW -1 >= 0) and (SMX + SMW -1 <= 255) Then
              FastIMG1.Bmp.Pixels32[191 - Y, SMX + SMW -1] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
        End;
     End;

     FastIMG1.Repaint;

  End;

End;

Procedure TBMPImportForm.Convert;
Var
  BestAttrs: Array[0..31, 0..23] of Byte;
  Attr, Clr1, Clr2: Byte;
  X, Y, Xc, Yc, Idx, Rl, Gl, Bl, Rd, Gd, Bd, Cnt, Red1, Green1, Blue1, Bright1, Bright2, nWidth, nHeight: Integer;
  R_Sq, G_Sq, B_Sq, Dist, Dist1, Dist2, Value, H, S, V: Extended;
  ScaleClr, Clr: TFColorA;
  Done, Done2: Boolean;
  TempDIB, TempDIB2, TempDIB3: TFastDIB;
  LCount, DCount, Val, Match, Error_R, Error_G, Error_B: Integer;
  SmallDist, MinLum, MaxLum, r_Mean: Extended;
  Temp_R, Temp_G, Temp_B, TempPixels: Array[0..255, 0..191] of Integer;
  Palette: TFColorTable;
  HueTable: Array[0..7] of Extended;
  MatchArray: Array[0..7] of Integer;
  Brights: Array[0..31, 0..23] of Byte;
Const
  DitherArray16: Array[0..3, 0..3] of Byte =
     ((00, 08, 02, 10),
      (12, 04, 14, 06),
      (03, 11, 01, 09),
      (15, 07, 13, 05));
  DitherArray64: Array[0..7,0..7] of Byte =
     ((16, 34, 26, 42, 18, 36, 28, 44),
      (58, 00, 50, 08, 60, 02, 52, 10),
      (30, 46, 20, 38, 32, 48, 22, 40),
      (54, 12, 62, 04, 56, 14, 64, 06),
      (19, 37, 29, 45, 17, 35, 27, 43),
      (61, 03, 53, 11, 59, 01, 51, 09),
      (33, 49, 23, 41, 31, 47, 21, 39),
      (57, 15, 65, 07, 55, 13, 63, 05));
  DiagonalArray5: Array[0..4, 0..4] of Byte =
    ((4, 2, 1, 3, 5),
     (2, 1, 3, 5, 4),
     (1, 3, 5, 4, 2),
     (3, 5, 4, 2, 1),
     (5, 4, 2, 1, 3));
  DiagonalArray9: Array[0..8, 0..8] of Byte =
    ((8, 6, 4, 2, 1, 3, 5, 7, 9),
     (6, 4, 2, 1, 3, 5, 7, 9, 8),
     (4, 2, 1, 3, 5, 7, 9, 8, 6),
     (2, 1, 3, 5, 7, 9, 8, 6, 4),
     (1, 3, 5, 7, 9, 8, 6, 4, 2),
     (3, 5, 7, 9, 8, 6, 4, 2, 1),
     (5, 7, 9, 8, 6, 4, 2, 1, 3),
     (7, 9, 8, 6, 4, 2, 1, 3, 5),
     (9, 8, 6, 4, 2, 1, 3, 5, 7));
  ClusterArray: Array [0..9] of AnsiString =
     ('000000000',
      '000010000',
      '000011000',
      '010011000',
      '011011000',
      '011011010',
      '011111010',
      '011111110',
      '111111110',
      '111111111');
  DispersedArray: Array [0..9] of AnsiString =
     ('000000000',
      '100000000',
      '100000010',
      '100001010',
      '101001010',
      '101101010',
      '101101110',
      '111101110',
      '111111110',
      '111111111');
  MidToneArray: Array[0..3] of AnsiString =
     ('000',
      '010',
      '101',
      '111');
Begin

  // Resize to match the selected region

  If (ConvRectW = 0) or (ConvRectH = 0) Then Exit;

  If ConvRectW < 0 Then
     ConvRectX := ConvRectX + ConvRectW;

  If ConvRectH < 0 Then
     ConvRectY := ConvRectY + ConvRectH;

  ConvRectW := Abs(ConvRectW);
  ConvRectH := Abs(ConvRectH);

  TempDIB2 := TFastDIB.Create;
  TempDIB2.SetSize(ConvRectW, ConvRectH, 32);
  TempDIB2.Clear(FRGBn(DisplayPalette[BackClr]));

  If (SizeDIB.Width / SizeDIB.Height) < (ConvRectW / ConvRectH) Then Begin

     nHeight := ConvRectH;
     nWidth := Max(1, Round(nHeight * (SizeDIB.Width/SizeDIB.AbsHeight)));

  End Else Begin

     nWidth := ConvRectW;
     nHeight := Max(1, Round(nWidth * (SizeDIB.AbsHeight/SizeDIB.Width)));

  End;

  TempDIB3 := TFastDIB.Create;
  TempDIB3.SetSize(nWidth, nHeight, 32);

  If ComboBox1.ItemIndex = 0 Then
     FastResize(SizeDIB, TempDIB3)
  Else
     Bilinear32(SizeDIB, TempDIB3);

  TempDIB3.Draw(TempDIB2.hDc, (TempDIB2.Width Div 2) - (TempDib3.Width Div 2), (TempDIB2.Height Div 2) - (TempDib3.Height Div 2));
  TempDIB3.Free;

  TempDIB := TFastDIB.Create;
  TempDIB.SetSize(256, 192, 32);

  TempDIB.Clear(FRGBn(DisplayPalette[BackClr]));
  TempDIB2.Draw(TempDIB.hDc, ConvRectX, ConvRectY);
  TempDIB2.Free;

  // Apply modifiers, as they are not permanent

  Saturation(TempDIB, TrackBar2.Position);
  Contrast(TempDIB, TrackBar1.position, Trackbar1.Position, Trackbar1.Position);
  Lightness(TempDIB, TrackBar5.position, Trackbar5.Position, Trackbar5.Position);
  Gamma(TempDIB, TrackBar3.Position/100, TrackBar3.Position/100, TrackBar3.Position/100);

  // For each 8x8 block, find the best two colours from the speccy palette.
  // only colours from either the non-bright or the bright sections should be
  // used - not two from both.

  For X := 0 To 31 Do
     For Y := 0 To 23 Do
        Brights[X, Y] := 0;

  X := 0;
  Y := 0;
  Done := False;

  For Idx := 0 To 7 Do Begin
     RGBToHSV(DisplayPalette[Idx + 8].r, DisplayPalette[Idx + 8].g, DisplayPalette[Idx + 8].b, H, S, V);
     HueTable[Idx] := H;
  End;

  While Not Done Do Begin

     If ComboBox3.ItemIndex = 3 Then Begin // Back + Colour

        For Idx := 0 To 7 Do
           MatchArray[Idx] := 0;

        For Xc := 0 To 7 Do
           For Yc := 0 To 7 Do Begin
              Clr := TempDIB.Pixels32[Y + Yc, X + Xc];
              Rd := 0; Gd := 0; Bd := 0;
              If Clr.r > 128 Then Rd := 1;
              If Clr.g > 128 Then Gd := 1;
              If Clr.b > 128 Then Bd := 1;
              Idx := Bd + (Rd * 2) + (Gd * 4);
              If Idx <> (BackClr And 7) Then
                 Inc(MatchArray[Idx]);
           End;

        Clr1 := BackClr;
        CLr2 := Clr1;
        Rd := 0;
        For Idx := 0 To 7 Do Begin
           If MatchArray[Idx] > Rd Then Begin
              Rd := MatchArray[Idx];
              Clr2 := Idx;
           End;
        End;

     End Else If ComboBox3.ItemIndex = 2 Then Begin // Threshold matching

        For Idx := 0 To 7 Do
           MatchArray[Idx] := 0;

        For Xc := 0 To 7 Do
           For Yc := 0 To 7 Do Begin
              Clr := TempDIB.Pixels32[Y + Yc, X + Xc];
              Rd := 0; Gd := 0; Bd := 0;
              If Clr.r > 128 Then Rd := 1;
              If Clr.g > 128 Then Gd := 1;
              If Clr.b > 128 Then Bd := 1;
              Inc(MatchArray[Bd + (Rd * 2) + (Gd * 4)]);
           End;
        Done2 := False;
        For Idx := 0 To 7 Do
           If MatchArray[Idx] = 64 Then
              Done2 := True;
        If Done2 Then Begin
           For Idx := 0 To 7 Do
              If MatchArray[Idx] <> 0 Then Begin
                 Clr1 := 7 - ((Idx Div 4) * 7);
                 Clr2 := Idx;
              End;
           If Clr2 = 0 Then Clr1 := 0;
           If Clr2 = 7 Then Clr1 := 7;
        End Else Begin
           Rl := 0; Gl := 0;
           For Idx := 0 To 7 Do
              If MatchArray[Idx] > Gl Then Begin
                 Gl := MatchArray[Idx];
                 Clr2 := Idx;
              End;
           MatchArray[Clr2] := 0;
           For Idx := 0 To 7 Do
              If MatchArray[Idx] > Rl Then Begin
                 Rl := MatchArray[Idx];
                 Clr1 := Idx;
              End;
        End;

     End Else If ComboBox3.ItemIndex = 1 Then Begin // HSV conversion

        For Idx := 0 To 7 Do
           MatchArray[Idx] := 0;

        For Xc := 0 To 7 Do
           For Yc := 0 To 7 Do Begin
              Clr := TempDIB.Pixels32[Y + Yc, X + Xc];
              RGBtoHSV(Clr.r, Clr.g, Clr.b, H, S, V);
              // three cases - low saturation (white or black)
              // low luminace - black
              // others - Match by hue.
              If V < 64 Then
                 Match := 0
              Else
                 If S < 0.25 Then // Grey
                    Match := 7
                 Else Begin
                    SmallDist := 99999;
                    For Idx := 6 DownTo 1 Do
                       If Abs(HueTable[Idx] - H) < SmallDist Then Begin
                          SmallDist := Abs(HueTable[Idx] - H);
                          Match := Idx;
                       End;
                    End;
              Inc(MatchArray[Match]);
           End;

        Match := 0;
        Clr1 := 0;
        For Idx := 0 To 7 Do Begin
           If MatchArray[Idx] > Match Then Begin
              Match := MatchArray[Idx];
              Clr1 := Idx;
           End;
        End;

        Match := 0;
        Clr2 := 0;
        For Idx := 0 To 7 Do Begin
           If (MatchArray[Idx] > Match) And (Idx <> Clr1) Then Begin
              Match := MatchArray[Idx];
              Clr2 := Idx;
           End;
        End;

        If Clr1 = Clr2 Then Clr2 := 0;

     End Else If ComboBox3.ItemIndex = 0 Then Begin // RGB conversion

        Rd := 0; Gd := 0; Bd := 0; DCount := 0;
        Rl := 0; Gl := 0; Bl := 0; LCount := 0;

        For Xc := 0 To 7 Do Begin
           For Yc := 0 To 7 Do Begin
              Clr := TempDIB.Pixels32[Y + Yc, X + Xc];
              Val := Round((0.299* Clr.r) +  (0.587 * Clr.g) + (0.114 * Clr.b));
              If Val < 149 Then Begin
                 Inc(Rd, Clr.r);
                 Inc(Gd, Clr.g);
                 Inc(Bd, Clr.b);
                 Inc(DCount);
              End Else Begin
                 Inc(Rl, Clr.r);
                 Inc(Gl, Clr.g);
                 Inc(Bl, Clr.b);
                 Inc(LCount);
              End;
           End;
        End;

        If DCount > 0 Then Begin
           Rd := Round(Rd/DCount);
           Gd := Round(Gd/DCount);
           Bd := Round(Bd/DCount);
        End;

        If LCount > 0 Then Begin
           Rl := Round(Rl/LCount);
           Gl := Round(Gl/LCount);
           Bl := Round(Bl/LCount);
        End;

        If DCount > 0 Then Begin
           Match := -1;
           SmallDist := 9999999;
           For Idx := 0 To 7 Do Begin
              R_Sq := DisplayPalette[Idx].r - Rd;
              G_Sq := DisplayPalette[Idx].g - Gd;
              B_Sq := DisplayPalette[Idx].b - Bd;
              Dist := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));
              If Dist < SmallDist Then Begin
                 SmallDist := Dist;
                 Match := Idx;
              End;
           End;
           Clr1 := Match;
        End Else Begin
           Match := -1;
           SmallDist := 9999999;
           For Idx := 0 To 7 Do Begin
              R_Sq := DisplayPalette[Idx].r - Rl;
              G_Sq := DisplayPalette[Idx].g - Gl;
              B_Sq := DisplayPalette[Idx].b - Bl;
              Dist := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));
              If Dist < SmallDist Then Begin
                 SmallDist := Dist;
                 Match := Idx;
              End;
           End;
           Clr1 := Match;
        End;

        If LCount > 0 Then Begin
           Match := -1;
           SmallDist := 9999999;
           For Idx := 0 To 7 Do Begin
              R_Sq := DisplayPalette[Idx].r - Rl;
              G_Sq := DisplayPalette[Idx].g - Gl;
              B_Sq := DisplayPalette[Idx].b - Bl;
              Dist := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));
              If (Dist < SmallDist) And (Idx <> Clr1) Then Begin
                 SmallDist := Dist;
                 Match := Idx;
              End;
           End;
           Clr2 := Match;
        End Else Begin
           Match := -1;
           SmallDist := 9999999;
           For Idx := 0 To 7 Do Begin
              R_Sq := DisplayPalette[Idx].r - Rd;
              G_Sq := DisplayPalette[Idx].g - Gd;
              B_Sq := DisplayPalette[Idx].b - Bd;
              Dist := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));
              If (Dist < SmallDist) And (Idx <> Clr1) Then Begin
                 SmallDist := Dist;
                 Match := Idx;
              End;
           End;
           Clr2 := Match;
        End;
     End;
{
     // Bright Test

     Value := 0;
     For Xc := 0 To 7 Do
        For Yc := 0 To 7 Do Begin
           Clr := TempDIB.Pixels32[Y + Yc, X + Xc];
           Value := Value + (0.299* Clr.r) +  (0.587 * Clr.g) + (0.114 * Clr.b);
        End;
     Value := Value/64;
     If Value > 200 Then Begin
        Inc(Clr1, 8);
        Inc(Clr2, 8);
     End;
}
     // Intelligent Bright

     Cnt := 0;
     Red1 := 0;
     Green1 := 0;
     Blue1 := 0;
     For Xc := 0 To 7 Do
        For Yc := 0 To 7 Do Begin
           Clr := TempDIB.Pixels32[Y + Yc, X + Xc];
           If Clr.r > 200 Then Begin
              Inc(Cnt);
              Inc(Red1, Clr.r);
           End;
           If Clr.g > 200 Then Begin
              Inc(Cnt);
              Inc(Green1, Clr.g);
           End;
           If Clr.b > 200 Then Begin
              Inc(Cnt);
              Inc(Blue1, Clr.b);
           End;
        End;

     If CheckBox2.Checked And (((Red1+Green1+Blue1)/Cnt) > 200) Then
        Brights[X Div 8, Y Div 8] := 1;

     If Clr2 < Clr1 Then Begin Val := Clr1; Clr1 := Clr2; Clr2 := Val; End;
     If CheckBox3.Checked Then Begin
        BestAttrs[X Div 8, Y Div 8] := 240;
     End Else
        BestAttrs[X Div 8, Y Div 8] := Clr1 + (Clr2 Shl 4);

     Inc(X, 8);
     If X >= 256 Then Begin
        X := 0;
        Inc(Y, 8);
        If Y >= 192 Then
           Done := True;
     End;

  End;

  If CheckBox5.Checked Then Begin // blow out the saturation and contrast

     For Y := 0 To 191 Do Begin
        For X := 0 To 255 Do Begin
           Clr := TempDIB.Pixels32[Y, X];
           Rd := 0; Gd := 0; Bd := 0;
           If Clr.r > 128 Then Rd := 1;
           If Clr.g > 128 Then Gd := 1;
           If Clr.b > 128 Then Bd := 1;
           TempDIB.Pixels32[Y, X] := fRGBA(Rd * 255, Gd * 255, Bd * 255, 0);
        End;
     End;

  End;

  // Now for the actual image conversion

  SpecDIB.SetSize(256, 192, 32);

  Case ComboBox2.ItemIndex of

     0: // No dithering
        Begin
           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));
                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));
                 If Dist1 < Dist2 Then Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                    FinishedDIB[X, Y] := Clr1;
                 End Else Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                    FinishedDIB[X, Y] := Clr2;
                 End;
              End;
           End;
        End;

     1: // Random dot
        Begin
           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 > Dist1 Then Begin
                    If Random(Round(Dist1 + Dist2)) > Dist1 Then Begin
                       SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                       FinishedDIB[X, Y] := Clr1;
                    End Else Begin
                       SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                       FinishedDIB[X, Y] := Clr2;
                    End;
                 End Else
                    If Random(Round(Dist1 + Dist2)) > Dist2 Then Begin
                       SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                       FinishedDIB[X, Y] := Clr2;
                    End Else Begin
                       SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                       FinishedDIB[X, Y] := Clr1;
                    End;

              End;
           End;
        End;

     2: // 5-Level Diagonal Dither
        Begin

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 Val := Round((Dist1 / (Dist1 + Dist2)) * 5);
                 If Val >= DiagonalArray5[X Mod 5, Y Mod 5] Then Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                    FinishedDIB[X, Y] := Clr2;
                 End Else Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                    FinishedDIB[X, Y] := Clr1;
                 End;
              End;
           End;
        End;

     3: // 9-Level Diagonal Dither
        Begin

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 Val := Round((Dist1 / (Dist1 + Dist2)) * 9);
                 If Val >= DiagonalArray9[X Mod 9, Y Mod 9] Then Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                    FinishedDIB[X, Y] := Clr2;
                 End Else Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                    FinishedDIB[X, Y] := Clr1;
                 End;
              End;
           End;
        End;

     4: // Clustered Dot Dither
        Begin

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 Val := Round((Dist1 / (Dist1 + Dist2)) * 10);
                 If Val = 10 Then Val := 9;
                 If ClusterArray[Val][(((Y Mod 3) * 3) + X Mod 3) +1] = '1' Then Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                    FinishedDIB[X, Y] := Clr2;
                 End Else Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                    FinishedDIB[X, Y] := Clr1;
                 End;
              End;
           End;

        End;

     5: // Dispersed Dot Dither
        Begin
           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 Val := Round((Dist1 / (Dist1 + Dist2)) * 10);
                 If Val = 10 Then Val := 9;
                 If DispersedArray[Val][(((Y Mod 3) * 3) + X Mod 3) +1] = '1' Then Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                    FinishedDIB[X, Y] := Clr2;
                 End Else Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                    FinishedDIB[X, Y] := Clr1;
                 End;
              End;
           End;
        End;

     6: // Halftone Dither
        Begin
           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 Val := Round(Abs(Dist1 / (Dist1 + Dist2)) * 255);
                 Case Val of

                    0..119:
                       Begin
                          SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                          FinishedDIB[X, Y] := Clr1;
                       End;

                    120..135:
                       Begin
                          If Y And 1 = 0 Then Begin
                             If X And 1 = 0 Then
                                FinishedDIB[X, Y] := Clr2
                             Else
                                FinishedDIB[X, Y] := Clr1;
                          End Else
                             If X and 1 = 0 Then
                                FinishedDIB[X, Y] := Clr1
                             Else
                                FinishedDIB[X, Y] := Clr2;

                          SpecDIB.Pixels32[Y, X] := DisplayPalette[FinishedDIB[X, Y]];
                       End;

                    136..256:
                       Begin
                          SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                          FinishedDIB[X, Y] := Clr2;
                       End;

                 End;
              End;
           End;
        End;

     7: // 16-Level Ordered Dither
        Begin

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 Val := Round((Dist1 / (Dist1 + Dist2)) * 16);
                 If Val > DitherArray16[X Mod 4, Y Mod 4] Then Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                    FinishedDIB[X, Y] := Clr2;
                 End Else Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                    FinishedDIB[X, Y] := Clr1;
                 End;
              End;
           End;
        End;

     8: // 64-Level Ordered Dither
        Begin

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Clr.r;
                 G_Sq := DisplayPalette[Clr1].g - Clr.g;
                 B_Sq := DisplayPalette[Clr1].b - Clr.b;
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Clr.r;
                 G_Sq := DisplayPalette[Clr2].g - Clr.g;
                 B_Sq := DisplayPalette[Clr2].b - Clr.b;
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 Val := Round((Dist1 / (Dist1 + Dist2)) * 64);
                 If Val >= DitherArray64[X Mod 8, Y Mod 8] Then Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr2];
                    FinishedDIB[X, Y] := Clr2;
                 End Else Begin
                    SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                    FinishedDIB[X, Y] := Clr1;
                 End;
              End;
           End;
        End;

     9: // Modified Floyd-Steinberg
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And ((X Mod 8 <> 7) or (BestAttrs[(X Div 8) +1, Y Div 8] = Attr)) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/16) * 7));
                    Inc(Temp_G[X+1, Y], Round((Error_G/16) * 7));
                    Inc(Temp_B[X+1, Y], Round((Error_B/16) * 7));
                 End;

                 If (X > 0) And ((X Mod 8 <> 0) Or (BestAttrs[(X Div 8) -1, Y Div 8] = Attr)) And (Y < 191) And ((Y Mod 8 <> 7) or (BestAttrs[X Div 8, (Y Div 8) +1] = Attr)) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/16) * 3));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/16) * 3));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/16) * 3));
                 End;

                 If (Y < 191) And ((Y Mod 8 <> 7) or (BestAttrs[X Div 8, (Y Div 8) +1] = Attr)) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/16) * 5));
                    Inc(Temp_G[X, Y+1], Round((Error_G/16) * 5));
                    Inc(Temp_B[X, Y+1], Round((Error_B/16) * 5));
                 End;

                 If (X < 255) And ((X Mod 8 <> 7) or (BestAttrs[(X Div 8) +1, Y Div 8] = Attr)) And (Y < 191) And ((Y Mod 8 <> 7) or (BestAttrs[X Div 8, (Y Div 8) +1] = Attr)) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/16) * 1));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/16) * 1));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/16) * 1));
                 End;

              End;
           End;
        End;

     10: // Jarvis Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/48) * 7));
                    Inc(Temp_G[X+1, Y], Round((Error_G/48) * 7));
                    Inc(Temp_B[X+1, Y], Round((Error_B/48) * 7));
                 End;

                 If (X < 254) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y], Round((Error_R/48) * 5));
                    Inc(Temp_G[X+2, Y], Round((Error_G/48) * 5));
                    Inc(Temp_B[X+2, Y], Round((Error_B/48) * 5));
                 End;

                 If (X > 1) And (Y < 191) And (BestAttrs[(X-2) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+1], Round((Error_R/48) * 3));
                    Inc(Temp_G[X-2, Y+1], Round((Error_G/48) * 3));
                    Inc(Temp_B[X-2, Y+1], Round((Error_B/48) * 3));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/48) * 5));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/48) * 5));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/48) * 5));
                 End;

                 If (Y < 191) And (BestAttrs[X Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/48) * 7));
                    Inc(Temp_G[X, Y+1], Round((Error_G/48) * 7));
                    Inc(Temp_B[X, Y+1], Round((Error_B/48) * 7));
                 End;

                 If (X < 255) And (Y < 191) And (BestAttrs[(X+1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/48) * 5));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/48) * 5));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/48) * 5));
                 End;

                 If (X < 254) And (Y < 191) And (BestAttrs[(X+2) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+1], Round((Error_R/48) * 3));
                    Inc(Temp_G[X+2, Y+1], Round((Error_G/48) * 3));
                    Inc(Temp_B[X+2, Y+1], Round((Error_B/48) * 3));
                 End;

                 If (X > 1) And (Y < 190) And (BestAttrs[(X-2) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+2], Round((Error_R/48) * 1));
                    Inc(Temp_G[X-2, Y+2], Round((Error_G/48) * 1));
                    Inc(Temp_B[X-2, Y+2], Round((Error_B/48) * 1));
                 End;

                 If (X > 0) And (Y < 190) And (BestAttrs[(X-1) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+2], Round((Error_R/48) * 3));
                    Inc(Temp_G[X-1, Y+2], Round((Error_G/48) * 3));
                    Inc(Temp_B[X-1, Y+2], Round((Error_B/48) * 3));
                 End;

                 If (Y < 190) And (BestAttrs[X Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+2], Round((Error_R/48) * 5));
                    Inc(Temp_G[X, Y+2], Round((Error_G/48) * 5));
                    Inc(Temp_B[X, Y+2], Round((Error_B/48) * 5));
                 End;

                 If (X < 255) And (Y < 190) And (BestAttrs[(X+1) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+2], Round((Error_R/48) * 3));
                    Inc(Temp_G[X+1, Y+2], Round((Error_G/48) * 3));
                    Inc(Temp_B[X+1, Y+2], Round((Error_B/48) * 3));
                 End;

                 If (X < 254) And (Y < 190) And (BestAttrs[(X+2) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+2], Round((Error_R/48) * 1));
                    Inc(Temp_G[X+2, Y+2], Round((Error_G/48) * 1));
                    Inc(Temp_B[X+2, Y+2], Round((Error_B/48) * 1));
                 End;

              End;
           End;

        End;

     11: // Stucki Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/42) * 8));
                    Inc(Temp_G[X+1, Y], Round((Error_G/42) * 8));
                    Inc(Temp_B[X+1, Y], Round((Error_B/42) * 8));
                 End;

                 If (X < 254) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y], Round((Error_R/42) * 4));
                    Inc(Temp_G[X+2, Y], Round((Error_G/42) * 4));
                    Inc(Temp_B[X+2, Y], Round((Error_B/42) * 4));
                 End;

                 If (X > 1) And (Y < 191) And (BestAttrs[(X-2) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+1], Round((Error_R/42) * 2));
                    Inc(Temp_G[X-2, Y+1], Round((Error_G/42) * 2));
                    Inc(Temp_B[X-2, Y+1], Round((Error_B/42) * 2));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/42) * 4));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/42) * 4));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/42) * 4));
                 End;

                 If (Y < 191) And (BestAttrs[X Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/42) * 8));
                    Inc(Temp_G[X, Y+1], Round((Error_G/42) * 8));
                    Inc(Temp_B[X, Y+1], Round((Error_B/42) * 8));
                 End;

                 If (X < 255) And (Y < 191) And (BestAttrs[(X+1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/42) * 4));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/42) * 4));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/42) * 4));
                 End;

                 If (X < 254) And (Y < 191) And (BestAttrs[(X+2) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+1], Round((Error_R/42) * 2));
                    Inc(Temp_G[X+2, Y+1], Round((Error_G/42) * 2));
                    Inc(Temp_B[X+2, Y+1], Round((Error_B/42) * 2));
                 End;

                 If (X > 1) And (Y < 190) And (BestAttrs[(X-2) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+2], Round((Error_R/42) * 1));
                    Inc(Temp_G[X-2, Y+2], Round((Error_G/42) * 1));
                    Inc(Temp_B[X-2, Y+2], Round((Error_B/42) * 1));
                 End;

                 If (X > 0) And (Y < 190) And (BestAttrs[(X-1) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+2], Round((Error_R/42) * 2));
                    Inc(Temp_G[X-1, Y+2], Round((Error_G/42) * 2));
                    Inc(Temp_B[X-1, Y+2], Round((Error_B/42) * 2));
                 End;

                 If (Y < 190) And (BestAttrs[X Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+2], Round((Error_R/42) * 4));
                    Inc(Temp_G[X, Y+2], Round((Error_G/42) * 4));
                    Inc(Temp_B[X, Y+2], Round((Error_B/42) * 4));
                 End;

                 If (X < 255) And (Y < 190) And (BestAttrs[(X+1) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+2], Round((Error_R/42) * 2));
                    Inc(Temp_G[X+1, Y+2], Round((Error_G/42) * 2));
                    Inc(Temp_B[X+1, Y+2], Round((Error_B/42) * 2));
                 End;

                 If (X < 254) And (Y < 190) And (BestAttrs[(X+2) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+2], Round((Error_R/42) * 1));
                    Inc(Temp_G[X+2, Y+2], Round((Error_G/42) * 1));
                    Inc(Temp_B[X+2, Y+2], Round((Error_B/42) * 1));
                 End;

              End;
           End;

        End;

     12: // Burkes Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/32) * 8));
                    Inc(Temp_G[X+1, Y], Round((Error_G/32) * 8));
                    Inc(Temp_B[X+1, Y], Round((Error_B/32) * 8));
                 End;

                 If (X < 254) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y], Round((Error_R/32) * 4));
                    Inc(Temp_G[X+2, Y], Round((Error_G/32) * 4));
                    Inc(Temp_B[X+2, Y], Round((Error_B/32) * 4));
                 End;

                 If (X > 1) And (Y < 191) And (BestAttrs[(X-2) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+1], Round((Error_R/32) * 2));
                    Inc(Temp_G[X-2, Y+1], Round((Error_G/32) * 2));
                    Inc(Temp_B[X-2, Y+1], Round((Error_B/32) * 2));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/32) * 4));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/32) * 4));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/32) * 4));
                 End;

                 If (Y < 191) And (BestAttrs[X Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/32) * 8));
                    Inc(Temp_G[X, Y+1], Round((Error_G/32) * 8));
                    Inc(Temp_B[X, Y+1], Round((Error_B/32) * 8));
                 End;

                 If (X < 255) And (Y < 191) And (BestAttrs[(X+1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/32) * 4));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/32) * 4));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/32) * 4));
                 End;

                 If (X < 254) And (Y < 191) And (BestAttrs[(X+2) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+1], Round((Error_R/32) * 2));
                    Inc(Temp_G[X+2, Y+1], Round((Error_G/32) * 2));
                    Inc(Temp_B[X+2, Y+1], Round((Error_B/32) * 2));
                 End;

              End;
           End;

        End;

    13: // Sierra3 Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/32) * 5));
                    Inc(Temp_G[X+1, Y], Round((Error_G/32) * 5));
                    Inc(Temp_B[X+1, Y], Round((Error_B/32) * 5));
                 End;

                 If (X < 254) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y], Round((Error_R/32) * 3));
                    Inc(Temp_G[X+2, Y], Round((Error_G/32) * 3));
                    Inc(Temp_B[X+2, Y], Round((Error_B/32) * 3));
                 End;

                 If (X > 1) And (Y < 191) And (BestAttrs[(X-2) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+1], Round((Error_R/32) * 2));
                    Inc(Temp_G[X-2, Y+1], Round((Error_G/32) * 2));
                    Inc(Temp_B[X-2, Y+1], Round((Error_B/32) * 2));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/32) * 4));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/32) * 4));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/32) * 4));
                 End;

                 If (Y < 191) And (BestAttrs[X Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/32) * 5));
                    Inc(Temp_G[X, Y+1], Round((Error_G/32) * 5));
                    Inc(Temp_B[X, Y+1], Round((Error_B/32) * 5));
                 End;

                 If (X < 255) And (Y < 191) And (BestAttrs[(X+1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/32) * 4));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/32) * 4));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/32) * 4));
                 End;

                 If (X < 254) And (Y < 191) And (BestAttrs[(X+2) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+1], Round((Error_R/32) * 2));
                    Inc(Temp_G[X+2, Y+1], Round((Error_G/32) * 2));
                    Inc(Temp_B[X+2, Y+1], Round((Error_B/32) * 2));
                 End;

                 If (X > 0) And (Y < 190) And (BestAttrs[(X-1) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+2], Round((Error_R/32) * 2));
                    Inc(Temp_G[X-1, Y+2], Round((Error_G/32) * 2));
                    Inc(Temp_B[X-1, Y+2], Round((Error_B/32) * 2));
                 End;

                 If (Y < 190) And (BestAttrs[X Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+2], Round((Error_R/32) * 3));
                    Inc(Temp_G[X, Y+2], Round((Error_G/32) * 3));
                    Inc(Temp_B[X, Y+2], Round((Error_B/32) * 3));
                 End;

                 If (X < 255) And (Y < 190) And (BestAttrs[(X+1) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+2], Round((Error_R/32) * 2));
                    Inc(Temp_G[X+1, Y+2], Round((Error_G/32) * 2));
                    Inc(Temp_B[X+1, Y+2], Round((Error_B/32) * 2));
                 End;

              End;
           End;

        End;

     14: // Sierra2 Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/16) * 4));
                    Inc(Temp_G[X+1, Y], Round((Error_G/16) * 4));
                    Inc(Temp_B[X+1, Y], Round((Error_B/16) * 4));
                 End;

                 If (X < 254) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y], Round((Error_R/16) * 3));
                    Inc(Temp_G[X+2, Y], Round((Error_G/16) * 3));
                    Inc(Temp_B[X+2, Y], Round((Error_B/16) * 3));
                 End;

                 If (X > 1) And (Y < 191) And (BestAttrs[(X-2) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+1], Round((Error_R/16) * 1));
                    Inc(Temp_G[X-2, Y+1], Round((Error_G/16) * 1));
                    Inc(Temp_B[X-2, Y+1], Round((Error_B/16) * 1));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/16) * 2));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/16) * 2));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/16) * 2));
                 End;

                 If (Y < 191) And (BestAttrs[X Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/16) * 3));
                    Inc(Temp_G[X, Y+1], Round((Error_G/16) * 3));
                    Inc(Temp_B[X, Y+1], Round((Error_B/16) * 3));
                 End;

                 If (X < 255) And (Y < 191) And (BestAttrs[(X+1) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/16) * 2));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/16) * 2));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/16) * 2));
                 End;

                 If (X < 254) And (Y < 191) And (BestAttrs[(X+2) Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+1], Round((Error_R/16) * 1));
                    Inc(Temp_G[X+2, Y+1], Round((Error_G/16) * 1));
                    Inc(Temp_B[X+2, Y+1], Round((Error_B/16) * 1));
                 End;

              End;
           End;

        End;

    15: // Sierra Filter Lite Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/4) * 2));
                    Inc(Temp_G[X+1, Y], Round((Error_G/4) * 2));
                    Inc(Temp_B[X+1, Y], Round((Error_B/4) * 2));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/4) * 1));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/4) * 1));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/4) * 1));
                 End;

                 If (Y < 191) And (BestAttrs[X Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/4) * 1));
                    Inc(Temp_G[X, Y+1], Round((Error_G/4) * 1));
                    Inc(Temp_B[X, Y+1], Round((Error_B/4) * 1));
                 End;

              End;
           End;

        End;

    16: // Atkinson Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 255) And (BestAttrs[(X+1) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y], Round((Error_R/8) * 1));
                    Inc(Temp_G[X+1, Y], Round((Error_G/8) * 1));
                    Inc(Temp_B[X+1, Y], Round((Error_B/8) * 1));
                 End;

                 If (X < 254) And (BestAttrs[(X+2) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y], Round((Error_R/8) * 1));
                    Inc(Temp_G[X+2, Y], Round((Error_G/8) * 1));
                    Inc(Temp_B[X+2, Y], Round((Error_B/8) * 1));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/8) * 1));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/8) * 1));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/8) * 1));
                 End;

                 If (Y < 191) And (BestAttrs[X Div 8, (Y+1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+1], Round((Error_R/8) * 1));
                    Inc(Temp_G[X, Y+1], Round((Error_G/8) * 1));
                    Inc(Temp_B[X, Y+1], Round((Error_B/8) * 1));
                 End;

                 If (X < 255) And (Y < 191) And (BestAttrs[(X+1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/8) * 1));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/8) * 1));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/8) * 1));
                 End;

                 If (Y < 190) And (BestAttrs[X Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+2], Round((Error_R/8) * 1));
                    Inc(Temp_G[X, Y+2], Round((Error_G/8) * 1));
                    Inc(Temp_B[X, Y+2], Round((Error_B/8) * 1));
                 End;

              End;
           End;

        End;

    17: // Stevenson-Arce Error Diffusion
        Begin

           For Y := 0 To 191 Do
              For X := 0 To 255 Do Begin
                 Clr := TempDIB.Pixels32[Y, X];
                 Temp_R[X, Y] := Clr.r;
                 Temp_G[X, Y] := Clr.g;
                 Temp_B[X, Y] := Clr.b;
              End;

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin

                 If Temp_R[X, Y] < 0 Then Temp_R[X, Y] := 0;
                 If Temp_G[X, Y] < 0 Then Temp_G[X, Y] := 0;
                 If Temp_B[X, Y] < 0 Then Temp_B[X, Y] := 0;

                 If Temp_R[X, Y] > 255 Then Temp_R[X, Y] := 255;
                 If Temp_G[X, Y] > 255 Then Temp_G[X, Y] := 255;
                 If Temp_B[X, Y] > 255 Then Temp_B[X, Y] := 255;

                 Attr := BestAttrs[X Div 8, Y Div 8];
                 Clr1 := Attr And 15;
                 Clr2 := Attr Shr 4;
                 R_Sq := DisplayPalette[Clr1].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr1].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr1].b - Temp_B[X, Y];
                 Dist1 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 R_Sq := DisplayPalette[Clr2].r - Temp_R[X, Y];
                 G_Sq := DisplayPalette[Clr2].g - Temp_G[X, Y];
                 B_Sq := DisplayPalette[Clr2].b - Temp_B[X, Y];
                 Dist2 := Sqrt((R_Sq * R_Sq) + (G_Sq * G_Sq) + (B_Sq * B_Sq));

                 If Dist2 < Dist1 Then
                    Clr1 := Clr2;

                 Rd := DisplayPalette[Clr1].r;
                 Gd := DisplayPalette[Clr1].g;
                 Bd := DisplayPalette[Clr1].b;

                 Error_R := Temp_R[X, Y] - Rd;
                 Error_G := Temp_G[X, Y] - Gd;
                 Error_B := Temp_B[X, Y] - Bd;

                 SpecDIB.Pixels32[Y, X] := DisplayPalette[Clr1];
                 FinishedDIB[X, Y] := Clr1;

                 If (X < 254) And (BestAttrs[(X+2) Div 8, Y Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y], Round((Error_R/200) * 32));
                    Inc(Temp_G[X+2, Y], Round((Error_G/200) * 32));
                    Inc(Temp_B[X+2, Y], Round((Error_B/200) * 32));
                 End;

                 //

                 If (X > 2) And (Y < 191) And (BestAttrs[(X-3) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-3, Y+1], Round((Error_R/200) * 12));
                    Inc(Temp_G[X-3, Y+1], Round((Error_G/200) * 12));
                    Inc(Temp_B[X-3, Y+1], Round((Error_B/200) * 12));
                 End;

                 If (X > 0) And (Y < 191) And (BestAttrs[(X-1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+1], Round((Error_R/200) * 26));
                    Inc(Temp_G[X-1, Y+1], Round((Error_G/200) * 26));
                    Inc(Temp_B[X-1, Y+1], Round((Error_B/200) * 26));
                 End;

                 If (X < 255) And (Y < 191) And (BestAttrs[(X+1) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+1], Round((Error_R/200) * 30));
                    Inc(Temp_G[X+1, Y+1], Round((Error_G/200) * 30));
                    Inc(Temp_B[X+1, Y+1], Round((Error_B/200) * 30));
                 End;

                 If (X < 253) And (Y < 191) And (BestAttrs[(X+3) Div 8, (Y +1) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+3, Y+1], Round((Error_R/200) * 16));
                    Inc(Temp_G[X+3, Y+1], Round((Error_G/200) * 16));
                    Inc(Temp_B[X+3, Y+1], Round((Error_B/200) * 16));
                 End;

                 //

                 If (Y < 190) And (X > 1) And (BestAttrs[(X -2) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-2, Y+2], Round((Error_R/200) * 12));
                    Inc(Temp_G[X-2, Y+2], Round((Error_G/200) * 12));
                    Inc(Temp_B[X-2, Y+2], Round((Error_B/200) * 12));
                 End;

                 If (Y < 190) And (BestAttrs[X Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X, Y+2], Round((Error_R/200) * 26));
                    Inc(Temp_G[X, Y+2], Round((Error_G/200) * 26));
                    Inc(Temp_B[X, Y+2], Round((Error_B/200) * 26));
                 End;

                 If (Y < 190) And (X < 254) And (BestAttrs[(X +2) Div 8, (Y+2) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+2, Y+2], Round((Error_R/200) * 12));
                    Inc(Temp_G[X+2, Y+2], Round((Error_G/200) * 12));
                    Inc(Temp_B[X+2, Y+2], Round((Error_B/200) * 12));
                 End;

                 //

                 If (X > 2) And (Y < 189) And (BestAttrs[(X-3) Div 8, (Y +3) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-3, Y+3], Round((Error_R/200) * 5));
                    Inc(Temp_G[X-3, Y+3], Round((Error_G/200) * 5));
                    Inc(Temp_B[X-3, Y+3], Round((Error_B/200) * 5));
                 End;

                 If (X > 0) And (Y < 189) And (BestAttrs[(X-1) Div 8, (Y +3) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X-1, Y+3], Round((Error_R/200) * 12));
                    Inc(Temp_G[X-1, Y+3], Round((Error_G/200) * 12));
                    Inc(Temp_B[X-1, Y+3], Round((Error_B/200) * 12));
                 End;

                 If (X < 255) And (Y < 189) And (BestAttrs[(X+1) Div 8, (Y +3) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+1, Y+3], Round((Error_R/200) * 12));
                    Inc(Temp_G[X+1, Y+3], Round((Error_G/200) * 12));
                    Inc(Temp_B[X+1, Y+3], Round((Error_B/200) * 12));
                 End;

                 If (X < 253) And (Y < 189) And (BestAttrs[(X+3) Div 8, (Y +3) Div 8] = Attr) Then Begin
                    Inc(Temp_R[X+3, Y+3], Round((Error_R/200) * 5));
                    Inc(Temp_G[X+3, Y+3], Round((Error_G/200) * 5));
                    Inc(Temp_B[X+3, Y+3], Round((Error_B/200) * 5));
                 End;

              End;
           End;

        End;

  End;

  If MouseDown Then Begin

     For X := ConvRectX to ConvRectX + ConvRectW -1 Do Begin
        If (X >= 0) And (X <= 255) Then Begin
           If (191 - ConvRectY >= 0) and (191 - ConvRectY <= 191) Then
              SpecDIB.Pixels32[191 - ConvRectY, X] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
           If (191 - (ConvRectY + ConvRectH -1) >= 0) and (191 - (ConvRectY + ConvRectH -1) <= 191) Then
              SpecDIB.Pixels32[191 - (ConvRectY + ConvRectH -1), X] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
        End;
     End;

     For Y := ConvRectY to ConvRectY + ConvRectH -1 Do Begin
        If (191 - Y >= 0) and (191 - Y <= 191) Then Begin
           If (ConvRectX >= 0) and (ConvRectX <= 255) Then
              SpecDIB.Pixels32[191 - Y, ConvRectX] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
           If (ConvRectX + ConvRectW -1 >= 0) and (ConvRectX + ConvRectW -1 <= 255) Then
              SpecDIB.Pixels32[191 - Y, ConvRectX + ConvRectW -1] := fRGBA(255 - DisplayPalette[BackClr].r, 255 - DisplayPalette[BackClr].g, 255 - DisplayPalette[BackClr].b, 0);
        End;
     End;

  End;

  If Not CheckBox3.Checked Then Begin
     For Y := 0 To 191 Do
        For X := 0 To 255 Do
           If Brights[X Div 8, Y Div 8] = 1 Then
              SpecDIB.Pixels32[Y, X] := DisplayPalette[FinishedDIB[X, Y] + 8];
  End Else Begin
     For Y := 0 To 191 Do
        For X := 0 To 255 Do
           If Brights[X Div 8, Y Div 8] = 0 Then
              If SpecDIB.Pixels32[Y, X].r = 255 Then
                 SpecDIB.Pixels32[Y, X] := DisplayPalette[FinishedDIB[X, Y] - 8];
  End;

  For X := 0 To 31 Do
     For Y := 0 To 23 Do
        FinishedAttrs[X, Y] := BestAttrs[X, Y] or (128 * Brights[X, Y]);

  TempDIB.Free;
  SpecDIB.Draw(FastIMG2.Bmp.hDc, 2, 2);
  FastIMG2.Repaint;

End;

procedure TBMPImportForm.Button2Click(Sender: TObject);
begin

  TrackBar2.Position := 256;
  Trackbar1.Position := 0;
  Trackbar5.Position := 0;
  TrackBar3.Position := 100;
  If GotOriginalDIB Then
     Convert;

end;

procedure TBMPImportForm.CheckBox2Click(Sender: TObject);
begin

  If GotOriginalDIB Then
     Convert;

end;

procedure TBMPImportForm.ComboBox2Change(Sender: TObject);
begin

  If GotOriginalDIB Then
     Convert;

end;



procedure TBMPImportForm.Button3Click(Sender: TObject);
begin

  Close;

end;

procedure TBMPImportForm.Button4Click(Sender: TObject);
Var
  Idx, X, Y, Attr, ByteVal, BitVal, Ink, Paper, Bright: Integer;
  ScreenArray: AnsiString;
begin

  If CheckBox4.Checked Then Begin

     If ScrPaintForm.CurScreen^.SelActive Then
        ScrPaintForm.ClearSelection;

     SetLength(ScrPaintForm.CurScreen^.SelMask, ConvRectW * ConvRectH);
     SetLength(ScrPaintForm.CurScreen^.SelDetail, ConvRectW * ConvRectH);
     SetLength(ScrPaintForm.CurScreen^.SelAttrDetail, ConvRectW * ConvRectH);

     ScrPaintForm.CurScreen^.SelWidth := ConvRectW;
     ScrPaintForm.CurScreen^.SelHeight := ConvRectH;
     ScrPaintForm.CurScreen^.SelOrigin.X := ConvRectX;
     ScrPaintForm.CurScreen^.SelOrigin.Y := ConvRectY;

     For X := 0 To ConvRectW -1 Do Begin
        For Y := 0 To ConvRectH -1 Do Begin

           ScrPaintForm.CurScreen^.SelMask[X + (Y * ConvRectW)] := 1;
           Attr := FinishedAttrs[(X + ConvRectX) Div 8, 23-((Y + ConvRectY) Div 8)];
           If Not CheckBox2.Checked And ((Attr And 15) > 7) Then Attr := ((Attr And 15) - 8) + (((Attr Shr 4) - 8) Shl 4);
           If FinishedDIB[X + ConvRectX, 191-(Y + ConvRectY)] = Attr And 15 Then ScrPaintForm.CurScreen^.SelDetail[X + (Y * ConvRectW)] := 1;
           Ink := Attr And 15;
           Paper := Attr Shr 4;
           Bright := 0;
           If (Ink > 7) Then Begin
              Dec(Ink, 8);
              Dec(Paper, 8);
              If CheckBox2.Checked Then Bright := 1;
           End;
           ScrPaintForm.CurScreen^.SelAttrDetail[X + (Y * ConvRectW)] := Ink + (Paper Shl 3) + (Bright Shl 6);

        End;
     End;

     ScrPaintForm.CurScreen^.SelActive := True;
     ScrPaintForm.RenderScreen(ScrPaintForm.CurScreenIndex, ScrPaintForm.ShowingAttrs);
     ScrPaintForm.MakeUndo('Image import', True);
     ScrPaintForm.ToolButton1.Enabled := True;
     ScrPaintForm.ToolButton2.Enabled := True;
     ScrPaintForm.ToolButton4.Enabled := True;
     ScrPaintForm.ToolButton5.Enabled := True;
     ScrPaintForm.ToolButton6.Enabled := True;
     ScrPaintForm.ToolButton8.Enabled := True;

     ScrPaintForm.PaintMode := pmSelectRect;

     ScrPaintForm.MouseIsDown := False;
     ScrPaintForm.MenuItemClick(ScrPaintForm.Select1);


  End Else Begin

     // Convert to SCR format and send to the editor.

     SetLength(ScreenArray, 6913);

     For Idx := 0 to 6143 Do Begin
        X := (Idx and 31) * 8;
        Y := ScreenOffsets[Idx];
        Attr := FinishedAttrs[X Div 8, 23-(Y Div 8)];
        If Not CheckBox2.Checked And ((Attr And 15) > 7) Then
           Attr := ((Attr And 15) - 8) + (((Attr Shr 4) - 8) Shl 4);
        ByteVal := 0;
        BitVal := 128;
        While BitVal > 0 Do Begin
           If FinishedDIB[X, 191-Y] = Attr And 15 Then
              ByteVal := ByteVal + BitVal;
           BitVal := BitVal Shr 1;
           Inc(X);
        End;
        ScreenArray[Idx +1] := AnsiChar(ByteVal);
     End;

     For Idx := 6144 To 6911 Do Begin
        X := Idx - 6144;
        Y := X Div 32;
        X := X And 31;
        Attr := FinishedAttrs[X, 23-Y];
        Ink := Attr And 15;
        Paper := Attr Shr 4;
        Bright := 0;
        If (Ink > 7) Then Begin
           Dec(Ink, 8);
           Dec(Paper, 8);
           If CheckBox2.Checked Then Bright := 1;
        End;
        ScreenArray[Idx +1] := AnsiChar(Ink + (Paper Shl 3) + (Bright * 64));
     End;

     ScrPaintForm.MemToScreen(ScreenArray, ScrPaintForm.CurScreenIndex);
     ScrPaintForm.RenderScreen(ScrPaintForm.CurScreenIndex, ScrPaintForm.ShowingAttrs);
     ScrPaintForm.MakeUndo('Image import', True);

  End;

  Close;

end;

Procedure TBMPImportForm.FileIsDropped(Var Msg: TMessage);
Var
  hDrop: THandle;
  fName: Array[0..1024] of Char;
  Name: AnsiString;
begin
  hDrop := Msg.WParam; Name := '';
  DragQueryFile(hDrop,0,fName,254);
  DragFinish(hDrop);
  DragAcceptFiles(Handle, True);

  Name := fName;

  If (Lowercase(ExtractFileExt(Name)) = '.bmp') or
     (Lowercase(ExtractFileExt(Name)) = '.jpg') or
     (Lowercase(ExtractFileExt(Name)) = '.gif') or
     (Lowercase(ExtractFileExt(Name)) = '.png') or
     (Lowercase(ExtractFileExt(Name)) = '.jpeg') then Begin
     Filename := Name;
     Button1Click(nil);
  End;

End;

procedure TBMPImportForm.FastIMG2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  Dec(X, 2);
  Dec(Y, 2);

  If GotScaledDIB Then Begin

     Mx := X;
     My := Y;
     ConvRectW := 1;
     ConvRectH := 1;
     ConvRectX := Mx;
     ConvRectY := My;
     MouseDown := True;
     MouseMoved := False;

  End;

end;

procedure TBMPImportForm.FastIMG2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin

  Dec(X, 2);
  Dec(Y, 2);
  MouseMoved := True;

  If MouseDown Then Begin

     ConvRectX := Mx;
     ConvRectY := My;
     ConvRectW := (X - Mx) +1;
     ConvRectH := (Y - My) +1;

     Label7.Caption := 'Coords: '+IntToStr(ConvRectX)+', '+IntToStr(ConvRectY)+'   Size: '+IntTostr(ConvRectW)+'x'+IntToStr(ConvRectH);

     If GotOriginalDIB Then
        If ConvRectW <> 1 Then
           If ConvRectH <> 1 Then
              ScaleDIB(True);

  End Else Begin

     If GotScaledDIB Then

        Label7.Caption := 'Coords: '+IntToStr(X)+', '+IntToStr(Y)

     Else

        Label7.Caption := '';

  End;

end;

procedure TBMPImportForm.FastIMG2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  If MouseDown Then Begin

     MouseDown := False;
     If GotOriginalDIB Then
        ScaleDIB(True);

  End;

  If Not MouseMoved Then Begin
     ConvRectX := SizeOffX;
     ConvRectY := SizeOffY;
     ConvRectW := SizeWidth;
     ConvRectH := SizeHeight;
     If GotOriginalDIB Then
        ScaleDIB(True);
  End;

end;

procedure TBMPImportForm.FastIMG2Exit(Sender: TObject);
begin

  Label7.Caption := '';

end;

procedure TBMPImportForm.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  Dec(Y, 4);

  If GotOriginalDIB Then Begin

     SizeMx := X;
     SizeMy := Y;
     SizeMW := 1;
     SizeMH := 1;

     SizeRectW := 1;
     SizeRectH := 1;
     LastSizeRectX := SizeRectX;
     LastSizeRectY := SizeRectY;

     SizeRectX := Round((SizeMx - SizeOffX) * (PartDIB.Width / SizeWidth)) + LastSizeRectX;
     SizeRectY := Round((SizeMy - SizeOffY) * (PartDIB.Height / SizeHeight)) + LastSizeRectY;

     SizeMouseDown := True;
     SizeMouseMoved := False;

  End;

end;

procedure TBMPImportForm.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  Sox, Soy, SX, SY, SW, SH, PW, PH: Integer;
begin

  Dec(Y, 4);

  SizeMouseMoved := True;

  If SizeMouseDown Then Begin

     If (X >= SizeOffX) and (X < SizeOffX + SizeWidth) And
        (Y >= SizeOffY) and (Y < SizeOffY + SizeHeight) Then Begin

        SizeMW := (X - SizeMx) +1;
        SizeMH := (Y - SizeMy) +1;
        SizeRectW := Round(SizeMW * (PartDIB.Width / SizeWidth));
        SizeRectH := Round(SizeMH * (PartDIB.Height / SizeHeight));

        Sw := SizeRectW;
        Sh := SizeRectH;
        Sx := SizeRectX;
        Sy := SizeRectY;

        If SizeRectW < 0 Then Begin
           Sw := -SizeRectW;
           Sx := SizeRectX + SizeRectW;
        End;

        If SizeRectH < 0 Then Begin
           Sh := -SizeRectH;
           Sy := SizeRectY + SizeRectH;
        End;

        Label7.Caption := 'Coords: '+IntToStr(SX)+', '+IntToStr(SY)+'   Size: '+IntTostr(SW)+'x'+IntToStr(SH);

        If GotOriginalDIB Then
           If SizeRectW <> 1 Then
              If SizeRectH <> 1 Then Begin
                 Sox := SizeOffX;
                 Soy := SizeOffY;
                 SW := SizeWidth;
                 SH := SizeHeight;
                 PW := PartDIB.Width;
                 PH := PartDIB.Height;
                 ScaleDIB(False);
                 SizeOffX := Sox;
                 SizeOffY := Soy;
                 SizeWidth := SW;
                 SizeHeight := SH;
                 PartDIB.SetSize(PW, PH, 32);
              End;

     End;

  End Else Begin

     If GotScaledDIB Then Begin

        X := Round((X - SizeOffX) * (PartDIB.Width / SizeWidth)) + SizeRectX;
        Y := Round((Y - SizeOffY) * (PartDIB.Height / SizeHeight)) + SizeRectY;
        Label7.Caption := 'Coords: '+IntToStr(X)+', '+IntToStr(Y);

     End Else

        Label7.Caption := '';

  End;

end;

procedure TBMPImportForm.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  If SizeMouseDown Then Begin

     SizeMouseDown := False;

     If Not SizeMouseMoved Then Begin
        SizeRectX := 0;
        SizeRectY := 0;
        SizeRectW := OriginalDIB.Width;
        SizeRectH := OriginalDIB.Height;
        If GotOriginalDIB Then
           ScaleDIB(True);

     End Else Begin

        If SizeRectW < 0 Then Begin
           SizeRectW := -SizeRectW;
           SizeRectX := SizeRectX - SizeRectW;
        End;

        If SizeRectH < 0 Then Begin
           SizeRectH := -SizeRectH;
           SizeRectY := SizeRectY - SizeRectH;
        End;

        If GotOriginalDIB Then
           ScaleDIB(True);

     End;

  End;

end;

procedure TBMPImportForm.CheckBox4Click(Sender: TObject);
begin

  If ScrPaintForm.CurScreen^.SelActive Then Begin

     ConvRectX := ScrPaintForm.CurScreen^.SelOrigin.X;
     ConvRectY := ScrPaintForm.CurScreen^.SelOrigin.Y;
     ConvRectW := ScrPaintForm.CurScreen^.SelWidth;
     ConvRectH := ScrPaintForm.CurScreen^.SelHeight;

     CheckBox2Click(nil);

  End;

end;

Procedure TBMPImportForm.RGBToHSV(Const R,G,B: Extended; Var H,S,V: Extended);
Var
  Delta, Minimum: Extended;
Begin
  Minimum := Min(Min(R, G), B);
  V := Max(Max(R, G), B);

  Delta := V - Minimum;

  If V =  0.0 Then
     S := 0
  Else
     S := Delta / V;

  If S  = 0.0 Then
     H := 0
  Else Begin
     IF R = V Then
        H := 60.0 * (G - B) / Delta
     Else
        If G = V Then
           H := 120.0 + 60.0 * (B - R) / Delta
        Else
           If  B = V Then
              H := 240.0 + 60.0 * (R - G) / Delta;
      If H < 0.0 Then
        H := H + 360.0;
  End;
End;


procedure TBMPImportForm.TrackBar5Enter(Sender: TObject);
begin

  Button4.SetFocus;

end;



end.
