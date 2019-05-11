unit Display;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FastIMG, ExtCtrls, FastDIB, Math, FastSize, Menus, Clipbrd, SyncObjs;

type
  TDisplayWindow = class(TForm)
    Panel1: TPanel;
    DisplayIMG: TFastIMG;
    PopupMenu1: TPopupMenu;
    Token1: TMenuItem;
    Detokenise1: TMenuItem;
    Help2: TMenuItem;
    MainMenu1: TMainMenu;
    WindowSize1: TMenuItem;
    N100320x2401: TMenuItem;
    N200640x4801: TMenuItem;
    N4001: TMenuItem;
    N3001: TMenuItem;
    N6001920x14401: TMenuItem;
    
    Custom1: TMenuItem;
    N1: TMenuItem;
    Force11Aspect1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    Print1: TMenuItem;
    N3: TMenuItem;
    Close1: TMenuItem;
    Help1: TMenuItem;
    DisplayHelp1: TMenuItem;
    Emulation1: TMenuItem;
    FullSpeed1: TMenuItem;
    SaveImage1: TMenuItem;

    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure DisplayIMGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure MenuItemClick(Sender: TObject);
    procedure OnEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Print1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure DisplayHelp1Click(Sender: TObject);
    procedure FullSpeed1Click(Sender: TObject);
    procedure DisplayIMGMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplayIMGMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);


  private
    { Private declarations }
  public
    { Public declarations }
    WantsFront: Boolean;
    WantResize: Boolean;
    Procedure SetClipping;
    Procedure SizeForm(Scale: Integer);
    Procedure SizeDIBS;
    Procedure InitScaleDIBs;
      Procedure SaveImage;

  end;

var
  DisplayWindow: TDisplayWindow;
  BackBuffer8Bit: TFastDIB;
  FrameCounter: Integer;
  Bit16Bmp1, Bit16Bmp2, Bit16Bmp3: TFastDIB;

    DisplayMouseX,    //kemp mouse
    DisplayMouseY,
    DisplayMouseBtn:            dword;



  _LUT16To32:       Array[0..65535] of DWord;
  _RGBToYUV:        Array[0..65535] of DWord;
  _2xSaIColours16:  Array[0..255] of DWord;
  _Hq2xColours16:   Array[0..255] of DWord;

  ResizeSection:       TRTLCriticalSection;
  ResizeSectionBool:   Boolean;

  Function  TryEnterSection: Boolean;
  procedure UpdateBASinDisplay;

{$L 2XSAIMMX.OBJ}
  procedure _2xSaI;
  procedure _SuperEagle;
  Procedure ColourDump16bitNoDirty(Dest: Pointer; Pitch: DWord; LUT: Pointer);
  procedure _2xFillBorder;
  procedure _2xSaISuperEagleLine(srcPointer: Pointer; deltaPtr: Pointer; srcPitch: Integer; width: Integer; dstPointer: Pointer; dstPitch: Integer); cdecl; external;
  procedure _2xSaISuper2xSaILine(srcPointer: Pointer; deltaPtr: Pointer; srcPitch: Integer; width: Integer; dstPointer: Pointer; dstPitch: Integer); cdecl; external;
  procedure _2xSaILine(srcPointer: Pointer; deltaPtr: Pointer; srcPitch: Integer; width: Integer; dstPointer: Pointer; dstPitch: Integer); cdecl; external;
  procedure Init_2xSaIMMX(PixelFormat: DWord); cdecl; external;

{$L HQ2X16.OBJ}
  Procedure _doHq2x;
  procedure _hq2x_16(inBuffer, outBuffer: Pointer; Xres, Yres, pitch: Integer); cdecl; external;
{$L HQ3X16.OBJ}
  Procedure _doHq3x;
  procedure _hq3x_16(inBuffer, outBuffer: Pointer; Xres, Yres, pitch: Integer); cdecl; external;
{$L HQ4X16.OBJ}
  Procedure _doHq4x;
  procedure _hq4x_16(inBuffer, outBuffer: Pointer; Xres, Yres, pitch: Integer); cdecl; external;

implementation

{$R *.DFM}

Uses BASinMain, InputUtils, Filing, Utility, FastCore, ROMUtils, Sound, Printing;

procedure TDisplayWindow.FormShow(Sender: TObject);
begin

  DoubleBuffered := True;
  SysTime := GetTickCount;

  DisplayIMG.Bmp.SetSize(320, 240, 32);
  BackBuffer8Bit := TFastDIB.Create;
  BackBuffer8Bit.SetSize(320, 240, 8);
  ScaleXDIB := TFastDIB.Create;
  AVGEffectDIB := TFastDIB.Create;
  AVGEffectDIB.SetSize(320, 240, 32);

  InitScaleDIBs;

  FrameCounter := 1;

end;

Procedure TDisplayWindow.InitScaleDIBs;
Begin

  Bit16Bmp1.SetSize(320, 240, 16);
  Bit16Bmp2.SetSize(640, 480, 16);

  If Opt_RenderMethod = rmHq2x Then
     Bit16Bmp3.SetSizeEx(640, -480, 16, 5, 6, 5)
  Else
     Bit16Bmp3.SetSizeEx(640, -480, 16, 5, 5, 5);

End;

procedure TDisplayWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
  Token: Byte;
begin

  If (DWord(GetKeyState(VK_LMENU)) <= 1) Then Begin
     Token := KeyToToken(Key, Shift);
	   If Token <> $FF Then
        BufferToken(Token);
  End;

  // Set the keyports so games reading the keyboard get the correct IN values

  BuildKeyPorts(Key, 0);

  //arda 23556
  //OutputDebugString(Pchar(IntToStr(Key) + Chr(Key)));

  If ((Key<65) or (Key>90)) and ((Key<48) or (Key>57)) and (Key<>32) and (Key<>13) Then
  Memory[KSTATE+4]:=255
  else
   Memory[KSTATE+4]:=Key;

  //arda 23556

end;

procedure TDisplayWindow.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Opt_GraphicsMethod = gmAltGr Then
     If Key = VK_CONTROL Then
        If GetKeyState(VK_RMENU) < 2 Then
           If Memory[MODE] = 2 Then
              BufferToken($0F);

	If Key <> VK_SHIFT Then
     CurKeyDown := 0
  Else
     If CurKeyDown = VK_SHIFT then
        CurKeyDown := 0;

  // Set the keyports so games reading the keyboard get the correct IN values

  BuildKeyPorts(Key, 1);
   //OutputDebugString(Pchar('Test-' + IntToStr(Key)));
  //erase KSTATE
   Memory[KSTATE+4] := 255;

end;

procedure TDisplayWindow.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
Var
  NW, NH: Integer;
begin
  Resize := True;
  If Not StartingUp Then Begin
     NW := (Panel1.Width - 8) + (NewWidth - Width);
     NH := (Panel1.Height - 8) + (NewHeight - Height);
     If NW < 256 Then NewWidth := Width;
     If NH < 192 Then NewHeight := Height;
  End;
end;

procedure TDisplayWindow.FormResize(Sender: TObject);
Var
  NW, NH: Integer;
begin

  If AppClosing Then Exit;

  ClearSoundBuffers;

  NW := Max(Panel1.Width -8, 320);
  NH := Max(Panel1.Height -8, 240);

  If Opt_IntegerScaling or (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]) Then Begin
     NW := (NW Div 256) * 320;
     NH := (NH Div 192) * 240;
  End;

  If Opt_MaintainAspect or (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]) Then Begin
     If (NW*0.75) > NH Then
        NW := Round(NH/0.75)
     Else
        NH := Round(NW*0.75);
  End;

  If Opt_ClipCorners Then
     DisplayIMG.SetBounds((Panel1.Width Div 2) - (NW Div 2), (Panel1.Height Div 2) - (NH Div 2), NW, NH)
  Else
     DisplayIMG.SetBounds((Panel1.Width Div 2) - (NW Div 2) -2, (Panel1.Height Div 2) - (NH Div 2) -2, NW, NH);

  SizeDIBS;

  If WorkerThread.DisplaySuspended Then
     DisplayIMG.Repaint;

end;

Procedure TDisplayWindow.SizeDIBS;
Begin

  If TryEnterSection Then Begin

     If DisplayIMG.Bmp <> nil Then
        DisplayIMG.Bmp.SetSize(DisplayIMG.Width, DisplayIMG.Height, 32);

     If Backbuffer8Bit <> nil Then Begin
        BackBuffer8Bit.SetSize(DisplayIMG.Width, DisplayIMG.Height, 8);
        If FastCore.Display <> nil Then Begin
           BackBuffer8Bit.Colors := FastCore.Display.Colors;
           BackBuffer8Bit.UpdateColors;
        End;
     End;

     If ScaleXDIB <> nil Then
        If DisplayIMG.Width <> ScaleXDIB.Width Then Begin
           ScaleXDIB.SetSize(DisplayIMG.Width, -DisplayIMG.Height, 8);
           ScaleXDIB.Colors := @DisplayPalette;
           ScaleXDIB.UpdateColors;
        End;

     WantResize := False;
     BorderUpdate := True;
     DisplayScale := DisplayIMG.Width Div 320;
     SetClipping;
     UpdateBASinDisplay;

     ResizeSectionBool := False;

  End Else
     WantResize := True;

End;

Function TryEnterSection: Boolean;
Begin

  EnterCriticalSection(ResizeSection);
  If Not ResizeSectionBool Then Begin
     Result := True;
     ResizeSectionBool := True;
  End Else
     Result := False;

  LeaveCriticalSection(ResizeSection);

End;

Procedure TDisplayWindow.SetClipping;
Var
  Rgn: HRgn;
  Idx, X, Y: Integer;
  Scale: Extended;
  ClipPoints: Array[0..79] of TPoint;
Const
  CornerPoints: Array[0..19] of TPoint = // This represents the bottom-right corner.
     ((x:0;   y:99),  (x:15;  y:99),  (x:28;  y:99),  (x:40;  y:99),
      (x:51;  y:99),  (x:60;  y:98),  (x:68;  y:97),  (x:75;  y:95),
      (x:81;  y:93),  (x:86;  y:90),  (x:90;  y:86),  (x:93;  y:81),
      (x:95;  y:75),  (x:97;  y:68),  (x:98;  y:60),  (x:99;  y:51),
      (x:99;  y:40),  (x:99;  y:28),  (x:99;  y:15),  (x:99;  y:0));
Begin

  If Opt_ClipCorners Then Begin

     Panel1.BorderStyle := BsNone;

     // Take the smallest side, and make 100pts in the coords about 2/3 of that length.

     If Panel1.Width > Panel1.Height Then
        Scale := (Panel1.Height/2)/100
     Else
        Scale := (Panel1.Width/2)/100;

     // Now do the polygon's points.

     For Idx := 0 To 19 Do Begin

        // Top Left
        X := Round((100 - CornerPoints[Idx].Y) * Scale);
        Y := Panel1.Height - Round((100 - CornerPoints[Idx].X) * Scale);
        ClipPoints[Idx] := Point(X, Y);

        // Top Right

        X := Panel1.Width - Round((100 - CornerPoints[Idx].X) * Scale);
        Y := Panel1.Height - Round((100 - CornerPoints[Idx].Y) * Scale);
        ClipPoints[Idx+20] := Point(X, Y);

        // Bottom Right

        X := Panel1.Width - Round((100 - CornerPoints[Idx].Y) * Scale);
        Y := Round((100 - CornerPoints[Idx].X) * Scale);
        ClipPoints[Idx+40] := Point(X, Y);

        // Bottom Left

        X := Round((100 - CornerPoints[Idx].X) * Scale);
        Y := Round((100 - CornerPoints[Idx].Y) * Scale);
        ClipPoints[Idx+60] := Point(X, Y);

     End;

     Rgn := CreatePolygonRgn(ClipPoints, 80, ALTERNATE);

  End Else Begin

     Panel1.BorderStyle := BsSingle;
     Rgn := CreateRectRgn(0, 0, Panel1.Width, Panel1.Height);

  End;

  SetWindowRgn(Panel1.Handle, Rgn, true);

End;

Procedure UpdateBASinDisplay;
var
  X,Y,Xp,Yp,Ypp,Xpp,T,Z,Z2,Iz2,W1,W2,W3,W4: DWord;
  Y1,Y2: PLine8;
  pc: PFColorA;
  Cla, Clb: TFColorA;
Label
  ExitPoint;
begin

  If (FastCore.Display = Nil) or (BackBuffer8Bit = nil) or (ScaleXDIB = Nil) then Exit;

  If (DisplayWindow.DisplayIMG.BMP.Width <= 2) or
     (DisplayWindow.DisplayIMG.BMP.AbsHeight <= 2) Then Exit;

  If (DisplayWindow.DisplayIMG.Bmp.Width = 320) and (DisplayWindow.DisplayIMG.Bmp.AbsHeight = 240) Then Begin

     BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, 0, 0, 320, 240, FastCore.Display.hDc, 0, 0, SrcCopy);

  End Else

     Case Opt_RenderMethod of

        rmGDI:
           Begin
              If OSIsNT Then
                 ExcludeClipRect(DisplayWindow.DisplayIMG.Bmp.hDc, 0, 0, 1, 1);
              If Opt_8BitStretch Then Begin
                 SetStretchBltMode(DisplayWindow.DisplayIMG.Bmp.hDc, COLORONCOLOR);
                 StretchBlt(BackBuffer8Bit.hdc, 0, 0, DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, FastCore.Display.hDc, 0, 0, 320, 240, SRCCOPY);
       	         BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, 0, 0, DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, BackBuffer8Bit.hdc, 0, 0, SRCCopy);
              End Else
                 SetStretchBltMode(DisplayWindow.DisplayIMG.Bmp.hDc, STRETCH_HALFTONE);
                 StretchBlt(DisplayWindow.DisplayIMG.Bmp.hDc,
                            0,
                            0,
                            DisplayWindow.DisplayIMG.Width,
                            DisplayWindow.DisplayIMG.Height,
                            FastCore.Display.hDc,
                            0,
                            0,
                            320,
                            240,
                            SrcCopy);
           End;

        rmScale2x:
           Begin
              If ScaleXDIB <> nil Then Begin
                 Case DisplayScale of
                    1: FastCore.Display.Draw(ScaleXDIB.hDc, 0, 0);
                    2: Scale2xDIB(FastCore.Display, ScaleXDIB);
                    3: Scale3xDIB(FastCore.Display, ScaleXDIB);
                    4: Scale4xDIB(FastCore.Display, ScaleXDIB);
                 End;
                 BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, (DisplayWindow.DisplayIMG.Bmp.Width Div 2) - (ScaleXDiB.Width Div 2), 0, ScaleXDIB.Width, ScaleXDIB.AbsHeight, ScaleXDIB.hDc, 0, 0, SrcCopy);
              End;
           End;

        rmHq2x:
           Begin
              Case DisplayScale of
                 1: Begin
                       FastCore.Display.Draw(ScaleXDIB.hDc, 0, 0);
                       BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, (DisplayWindow.DisplayIMG.Bmp.Width Div 2) - (ScaleXDiB.Width Div 2), 0, ScaleXDIB.Width, ScaleXDIB.AbsHeight, ScaleXDIB.hDc, 0, 0, SrcCopy);
                    End;
                 2: Begin
                       _doHq2x;
                       BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, (DisplayWindow.DisplayIMG.Bmp.Width Div 2) - (Bit16Bmp3.Width Div 2), 0, Bit16Bmp3.Width, Bit16Bmp3.AbsHeight, Bit16Bmp3.hDc, 0, 0, SrcCopy);
                    End;
                 3: Begin
                       _doHq3x;
                       BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, (DisplayWindow.DisplayIMG.Bmp.Width Div 2) - (Bit16Bmp3.Width Div 2), 0, Bit16Bmp3.Width, Bit16Bmp3.AbsHeight, Bit16Bmp3.hDc, 0, 0, SrcCopy);
                    End;
                 4: Begin
                       _doHq4x;
                       BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, (DisplayWindow.DisplayIMG.Bmp.Width Div 2) - (Bit16Bmp3.Width Div 2), 0, Bit16Bmp3.Width, Bit16Bmp3.AbsHeight, Bit16Bmp3.hDc, 0, 0, SrcCopy);
                    End;
              End;
           End;

        rmSuper2xSAI:
           Begin
              If DisplayScale = 2 Then Begin
                 _2xSaI;
                 BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, (DisplayWindow.DisplayIMG.Bmp.Width Div 2) - (Bit16Bmp3.Width Div 2), 0, Bit16Bmp3.Width, Bit16Bmp3.AbsHeight, Bit16Bmp3.hDc, 0, 0, SrcCopy);
              End Else Begin
                 If OSIsNT Then
                    ExcludeClipRect(DisplayWindow.DisplayIMG.Bmp.hDc, 0, 0, 1, 1);
                 If Opt_8BitStretch Then Begin
                    SetStretchBltMode(DisplayWindow.DisplayIMG.Bmp.hDc, COLORONCOLOR);
                    StretchBlt(BackBuffer8Bit.hdc, 0, 0, DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, FastCore.Display.hDc, 0, 0, 320, 240, SRCCOPY);
       	            BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, 0, 0, DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, BackBuffer8Bit.hdc, 0, 0, SRCCopy);
                 End Else
                    SetStretchBltMode(DisplayWindow.DisplayIMG.Bmp.hDc, STRETCH_HALFTONE);
                    StretchBlt(DisplayWindow.DisplayIMG.Bmp.hDc,
                               0,
                               0,
                               DisplayWindow.DisplayIMG.Width,
                               DisplayWindow.DisplayIMG.Height,
                               FastCore.Display.hDc,
                               0,
                               0,
                               320,
                               240,
                               SrcCopy);
              End;
           End;

        rmSuperEagle:
           Begin
              If DisplayScale = 2 Then Begin
                 _SuperEagle;
                 BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, (DisplayWindow.DisplayIMG.Bmp.Width Div 2) - (Bit16Bmp3.Width Div 2), 0, Bit16Bmp3.Width, Bit16Bmp3.AbsHeight, Bit16Bmp3.hDc, 0, 0, SrcCopy);
              End Else Begin
                 If OSIsNT Then
                    ExcludeClipRect(DisplayWindow.DisplayIMG.Bmp.hDc, 0, 0, 1, 1);
                 If Opt_8BitStretch Then Begin
                    SetStretchBltMode(DisplayWindow.DisplayIMG.Bmp.hDc, COLORONCOLOR);
                    StretchBlt(BackBuffer8Bit.hdc, 0, 0, DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, FastCore.Display.hDc, 0, 0, 320, 240, SRCCOPY);
       	            BitBlt(DisplayWindow.DisplayIMG.Bmp.hDc, 0, 0, DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, BackBuffer8Bit.hdc, 0, 0, SRCCopy);
                 End Else
                    SetStretchBltMode(DisplayWindow.DisplayIMG.Bmp.hDc, STRETCH_HALFTONE);
                    StretchBlt(DisplayWindow.DisplayIMG.Bmp.hDc,
                               0,
                               0,
                               DisplayWindow.DisplayIMG.Width,
                               DisplayWindow.DisplayIMG.Height,
                               FastCore.Display.hDc,
                               0,
                               0,
                               320,
                               240,
                               SrcCopy);
              End;
           End;

        rmBilinear:
           Begin
              Xpp := (319 shl 16) div DisplayWindow.DisplayIMG.Bmp.Width;
              Ypp := (239 shl 16) div DisplayWindow.DisplayIMG.Bmp.AbsHeight;
              Yp  := Ypp * DWord(DisplayWindow.DisplayIMG.Bmp.AbsHeight);
              pc  := Pointer(DisplayWindow.DisplayIMG.bmp.Bits);

              For Y := 0 To DisplayWindow.DisplayIMG.Bmp.AbsHeight -1 Do Begin

                 Xp := Yp shr 16;
                 Y1 := FastCore.Display.Scanlines[Xp];

                 If Xp < DWord(FastCore.Display.AbsHeight) -1 Then Inc(Xp);

                 Y2  := FastCore.Display.Scanlines[Xp];
                 Xp := 0;
                 Z2  := (Yp And $FFFF) +1;
                 Iz2 := ((Not Yp) And $FFFF) +1;

                 For X := 0 To DisplayWindow.DisplayIMG.Bmp.Width -1 Do Begin

                    T := Xp Shr 16;
                    z := Xp And $FFFF;

                    W2 := (Iz2*Z) Shr 16;
                    W1 := Iz2-W2;
                    W4 := (Z2*Z) Shr 16;
                    W3 := Z2-W4;

                    Pc.b := (DisplayPalette[Y1[T]].b   * W1 +
                             DisplayPalette[Y1[T+1]].b * W2 +
                             DisplayPalette[Y2[T]].b   * W3 +
                             DisplayPalette[Y2[T+1]].b * W4) Shr 16;

                    Pc.g := (DisplayPalette[Y1[T]].g   * W1 +
                             DisplayPalette[Y1[T+1]].g * W2 +
                             DisplayPalette[Y2[T]].g   * W3 +
                             DisplayPalette[Y2[T+1]].g * W4) Shr 16;

                    Pc.r := (DisplayPalette[Y1[T]].r   * W1 +
                             DisplayPalette[Y1[T+1]].r * W2 +
                             DisplayPalette[Y2[T]].r   * W3 +
                             DisplayPalette[Y2[T+1]].r * W4) Shr 16;

                    Inc(Xp,Xpp);
                    Inc(Pc);

                 End;

                 Dec(Yp,Ypp);
                 Pc := Ptr(Integer(Pc)+DisplayWindow.DisplayIMG.bmp.Gap);

              End;

           End;

     End;

  If Opt_Scanlines Then Begin

     Pc := Pointer(DisplayWindow.DisplayIMG.Bmp.Bits);
     Xp := DisplayWindow.DisplayIMG.Bmp.Width -1;
     For Y := 0 To (DisplayWindow.DisplayIMG.Bmp.AbsHeight Div 2) -1 Do Begin
        For X := 0 To Xp Do Begin
           If Pc.b > 64 Then Pc.b := Pc.b - 64;
           If Pc.g > 64 Then Pc.g := Pc.g - 64;
           If Pc.r > 64 Then Pc.r := Pc.r - 64;
           Inc(Pc);
        End;
        Inc(Pc, Xp);
        Inc(Pc);
     End;

  End;

  If DisplayWindow.WantsFront Then
     If DisplayWindow.Active And (Screen.ActiveForm = DisplayWindow) Then
        DisplayWindow.WantsFront := False;

  DisplayWindow.DisplayIMG.Repaint;

End;

Procedure TDisplayWindow.SizeForm(Scale: Integer);
Var
  Dw, Dh, Nw, Nh: Integer;
Begin

  If Scale = 0 Then Scale := Round((DisplayIMG.Width/320)*100);

  Dw := Round(320*(Scale/100));
  Dh := Round(240*(Scale/100));
  Nw := Dw + 8  + (GetSystemMetrics(SM_CXSIZEFRAME)*2);
  Nh := Dh + 8  +  GetSystemMetrics(SM_CYMENU) +
                   GetSystemMetrics(SM_CYCAPTION) +
                  (GetSystemMetrics(SM_CYSIZEFRAME)*2);

  DisplayIMG.SetBounds(DisplayIMG.Left, DisplayIMG.Height, Dw, Dh);
  DisplayWindow.SetBounds(Left, Top, Nw, Nh);

End;

procedure TDisplayWindow.DisplayIMGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  Done: Boolean;
  PopUpPoint: TPoint;
  IMGx, IMGy, CharX, CharY: Integer;
  Idx, Index, SearchPos, StartPos, CurPos: Integer;
  
begin

    // If Not MouseHooked Then
     //   If MouseFunction = 1 Then
     //      HookMouse;

     if (ssLeft in Shift) and (ssRight in Shift) then
        DisplayMouseBtn := 3
     else if Button = mbLeft then
        DisplayMouseBtn := 1
     else if Button = mbRight then
        DisplayMouseBtn := 2;




  If Button = mbRight Then Begin

     // Token helper system.
     // Alter X and Y to char positions.

     IMGx := X;
     IMGy := Y;

     X := Round(X / (DisplayIMG.Width/320)) - 32;
     Y := Round(Y / (DisplayIMG.Height/240)) - 24;

     If PtInRect(Rect(0, 192-((GetWord(@Memory[DF_SZ])-1)*8), 256, 192), Point(X, Y)) Then Begin

        X := (X Div 8)+1;
        Y := Y Div 8;

        // There's too much info to be returned, so do this here rather than with a function.

        CharX := 0;
        CharY := 24 - (GetWord(@Memory[DF_SZ]) -1);
        CurPos := GetWord(@Memory[E_LINE]);

        While Memory[CurPos] <> 13 Do Begin
           Case Memory[CurPos] of
              32..163:
                 Inc(CharX, 1);
              164..201:
                 Inc(CharX, Length(AsciiKeywords[Memory[CurPos] - 164]));
              202..255:
                 Begin
                    Inc(CharX, Length(AsciiKeywords[Memory[CurPos] - 164])+1);
                    If Memory[CurPos-1] in [32..201] Then Inc(CharX);
                 End;
           End;

           While CharX > 31 Do Begin
              Dec(CharX, 32);
              Inc(CharY);
           End;

           If CharY > Y Then Break;
           If (CharY = Y) And (CharX >= X) Then Break;

           Inc(CurPos);

        End;

        // We now have a position. Now to search for a reserved word...

        If Memory[CurPos] < 32 Then Exit; // Not interested in these.
        If Memory[CurPos] in [164..201] Then Begin
           // A Token itself, so set up for a DeTokenise.
           Token1.Caption := AsciiKeywords[Memory[CurPos] - 164];
           Detokenise1.Caption := 'DeTokenise';
           EditTokenPos := CurPos;
           EditToken := Memory[CurPos];
           PopUpPoint := DisplayIMG.ClientToScreen(Point(IMGx, IMGy));
           PopUpMenu1.Popup(PopUpPoint.X, PopUpPoint.Y);
           Exit;
        End;

        Idx := 0;
        Done := False;
        While Not Done Do Begin

           If Pos(Uppercase(Chr(Memory[CurPos])), AsciiKeywords[Idx]) > 0 Then Begin
              Index := 1;
              While Index <= Length(AsciiKeywords[Idx]) Do Begin
                 If AsciiKeywords[Idx][Index] = UpperCase(Chr(Memory[CurPos])) Then Begin
                    StartPos := CurPos -(Index -1);
                    SearchPos := StartPos;
                    While SearchPos < StartPos + Length(AsciiKeywords[Idx]) Do Begin
                       If UpperCase(Chr(Memory[SearchPos])) <> AsciiKeywords[Idx][(SearchPos - StartPos)+1] Then
                          Break
                       Else
                          Inc(SearchPos);
                    End;
                    If SearchPos = StartPos + Length(AsciiKeywords[Idx]) Then Begin
                       Done := True; // Idx holds the Keyword index
                       Break;
                    End;
                 End;
                 Inc(Index);
              End;
           End;

           If Not Done Then Begin
              Inc(Idx);
              If Idx = 91 Then Done := True;
           End;

        End;

        If Idx < 91 Then Begin

           // Got a Keyword!

           Token1.Caption := AsciiKeywords[Idx];
           Detokenise1.Caption := 'Tokenise';
           EditTokenPos := CurPos;
           EditToken := Idx + 164;
           PopUpPoint := DisplayIMG.ClientToScreen(Point(IMGx, IMGy));
           PopUpMenu1.Popup(PopUpPoint.X, PopUpPoint.Y);

        End;

     End;

  End;
end;

procedure TDisplayWindow.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
Var
  Key: Word;
begin
  Key := VK_NEXT;
  DisplayWindow.FormKeyDown(Sender, Key, []);
end;

procedure TDisplayWindow.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
Var
  Key: Word;
begin
  Key := VK_PRIOR;
  DisplayWindow.FormKeyDown(Sender, Key, []);
end;

procedure TDisplayWindow.MenuItemClick(Sender: TObject);
begin
  Case (Sender As TComponent).Tag of
     1:
        Begin // 100% Window Size
           DisplayWindow.Show;
           DisplayWindow.SizeForm(100);
        End;
     2:
        Begin // 200% Window Size
           DisplayWindow.Show;
           DisplayWindow.SizeForm(200);

        End;
     3:
        Begin // Force Aspect Ratio
           Opt_MaintainAspect := Not Opt_MaintainAspect;
           DisplayWindow.FormResize(Self);
        End;
     4:
        Begin // Cut
           CutEditLine;
        End;
     5:
        Begin // Copy
           CopyEditLine;
        End;
     6:
        Begin // Paste
           PasteEditLine;
        End;
     7:
        Begin // Delete
           DeleteEditLine;
        End;
     8:
        Begin // Delete
           DisplayWindow.Show;
           DisplayWindow.SizeForm(300);
        End;
     9:
        Begin // Delete
           DisplayWindow.Show;
           DisplayWindow.SizeForm(400);
        End;
     12:
        Begin //Save Image
               SaveImage;
        End;
     10:
        Begin // Delete
           DisplayWindow.Show;
           DisplayWindow.SizeForm(600);
        End;

  End;
end;

procedure TDisplayWindow.SaveImage;
Var
   Result: TFastDIB;
   Filename: String;

  {Attr, Ink, Paper, Bright: Byte;
  TempPixels: Array[0..255, 0..191] of Byte;
  TempAttrs: Array[0..31, 0..23] of Byte;
  Ext, NewFilename: String;
  StartAddr, DataLen: Word;
  ScreenArray: Array of Byte;
  Idx, X, Y, Cx, Cy, Offset: Integer;
  BitVal, ByteVal: Byte;
  Result, Bmpa: TFastDIB;
  Flashing: Boolean;
  Bit: Byte; }
Begin


  Result := TFastDIB.Create;

     Result.SetSize(DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, 32);
     DisplayWindow.DisplayIMG.Bmp.Draw(Result.hDc, 0, 0);

    // get a filename.
              Filename := OpenFile(Handle, 'Save Display as Bitmap Image', [FTBmp], '', True, False);
              If Filename = '' Then Exit;

    Result.SaveToFile(Filename);
    Result.Free;

  {

  For Idx := 1 To 6144 Do Begin

     X := ((Idx -1) and 31) * 8;
     Y := ScreenOffsets[Idx -1];

     ByteVal := Ord(Memory[16383+Idx]);
     BitVal := 128;

     For Bit := 0 To 7 Do Begin
        If ByteVal And BitVal <> 0 Then
           TempPixels[X, Y] := 1
        Else
           TempPixels[X, Y] := 0;
        BitVal := BitVal Shr 1;
        Inc(X);
     End;

  End;

  // And the ATTRs.

  For Idx := 6145 To 6912 Do Begin
     X := Idx - 6145;
     Y := X Div 32;
     X := X And 31;
     TempAttrs[X, Y] := Ord(Memory[16383+Idx]);
  End;



   Bmpa := TFastDIB.Create;
     Bmpa.SetSize(256, 192, 8);
     For Idx := 0 to 15 do Bmpa.Colors[Idx] := DisplayPalette[Idx];
     Bmpa.UpdateColors;




    For Y := 0 To 191 Do Begin
        For X := 0 To 255 Do Begin
           If X Mod 8 = 0 Then Begin
              Attr := TempAttrs[X Div 8, Y Div 8];
              Bright := (Attr And 64) Shr 3;
              Ink := (Attr And 7) + Bright;
              Paper := ((Attr Shr 3) And 7) + Bright;
              If Attr and 128 <> 0 Then Flashing := True;
           End;

              If TempPixels[X, Y] = 1 Then
                 Bmpa.Pixels8[191 - Y, X] := Ink
              Else
                 Bmpa.Pixels8[191 - Y, X] := Paper;
        End;
     End;
            Bmpa.SaveToFile('c:\test.bmp');
        Bmpa.Free;
}

End;

procedure TDisplayWindow.OnEnterMenuLoop(var Message: TMessage);
Begin
  // These manipulate the edit line, so there must be an edit line to manipulate.
  Cut1.Enabled := Registers.EmuRunning and
                  ((Not BASinOutput.Running and ((GetWord(@Memory[WORKSP])-GetWord(@Memory[E_LINE])) > 2)) or
                       (BASinOutput.Running and (Memory[GetWord(@Memory[WORKSP])] <> 13)));
  Copy1.Enabled := Cut1.Enabled;
  Paste1.Enabled := Registers.EmuRunning and ClipBoard.HasFormat(CF_TEXT) and (ClipBoard.AsText <> '');
  Delete1.Enabled := Cut1.Enabled;
  // The Window Size items need to be set accordingly
  N100320x2401.Checked := False;
  N200640x4801.Checked := False;
  Custom1.Checked := False;
  Custom1.Enabled := False;
  Custom1.Visible := False;
  If (DisplayWindow.DisplayIMG.Width = 320) and (DisplayWindow.DisplayIMG.Height = 240) Then
     N100320x2401.Checked := True
  Else If (DisplayWindow.DisplayIMG.Width = 640) and (DisplayWindow.DisplayIMG.Height = 480) Then
     N200640x4801.Checked := True
  Else Begin
     Custom1.Visible := True;
     Custom1.Enabled := True;
     Custom1.Checked := True;
     Custom1.Caption := 'Custom Size ('+IntToStr(DisplayWindow.DisplayIMG.Width)+'x'+IntToStr(DisplayWindow.DisplayIMG.Height)+')';
  End;
  Force11Aspect1.Checked := Opt_MaintainAspect or (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]);
  Force11Aspect1.Enabled := Not (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]);
End;

procedure TDisplayWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // If we're in the maximised State, then we need to un-maximise.
  WindowState := wsNormal;
end;

// Super2xSAI stuff

Procedure ColourDump16bitNoDirty(Dest: Pointer; Pitch: DWord; LUT: Pointer);
Var
  lpDest:  Pointer;
  lPitch:  DWord;
Begin
  lpDest := Dest;
  lPitch := Pitch;

     asm
        pushad

        mov   edx, LUT

        mov   esi, [FastCore.Display]
        mov   esi, [esi+TFastDIB.Bits]
        mov   edi, [lpDest]

        push  240

@loop1:
        push  edi
        push  esi

        push  320/4

@loop2: mov   ebx, [esi]
        add   esi, 4

        movzx eax, bh
        mov   ecx, dword [edx+eax*4]
        movzx eax, bl
        shl   ecx, 16

        shr   ebx, 16
        mov   cx, word [edx+eax*4]
        movzx eax, bh
        mov   [edi], ecx

        mov   ecx, dword [edx+eax*4]
        movzx eax, bl
        shl   ecx, 16

        mov   cx, word [edx+eax*4]
        mov   [edi+4], ecx
        add   edi, 8

        dec   dword [esp]
        jnz   @loop2
        add   esp, 4

        pop   esi
        pop   edi

@SkipScan:
        add   esi, 320
        add   edi, [lPitch]

        dec   dword [esp]
        jnz   @loop1
        add   esp, 4

        popad
     end;
End;

procedure _2xSaI;
var
  srcP, deltaP, dstP: Pointer;
  i: Integer;
begin
  ColourDump16BitNoDirty(Bit16Bmp1.Bits, 640, @_2xSaIColours16[0]);
  srcP := Pointer(DWord(bit16Bmp1.Bits)+(320*2)+2);
  deltaP := Pointer(DWord(bit16Bmp2.Bits)+(320*2)+2);
  dstP := Pointer(DWord(Bit16Bmp3.Bits)+(320*4)+2);
  for i := 0 to 238 do begin
    _2xSaILine(srcP, deltaP, 640, 320, dstP, 1280);
    srcP := Pointer(dword(srcP)+640);
    deltaP := Pointer(dword(deltaP)+640);
    dstP := Pointer(dword(dstP)+640*4);
  end;
  _2xFillBorder;
End;

procedure _SuperEagle; 
var
  srcP, deltaP, dstP: Pointer;
  i: Integer;
begin
  ColourDump16BitNoDirty(Bit16Bmp1.Bits, 640, @_2xSaIColours16[0]);
  srcP := Pointer(DWord(bit16Bmp1.Bits)+(320*2)+2);
  deltaP := Pointer(DWord(bit16Bmp2.Bits)+(320*2)+2);
  dstP := Pointer(DWord(Bit16Bmp3.Bits)+(320*4)+2);
  for i := 0 to 238 do begin
    _2xSaISuperEagleLine(srcP, deltaP, 640, 320, dstP, 1280);
    srcP := Pointer(dword(srcP)+640);
    deltaP := Pointer(dword(deltaP)+640);
    dstP := Pointer(dword(dstP)+640*4);
  end;
  _2xFillBorder;
end;

Procedure _2xFillBorder;
Begin
  asm

     Pushad

     mov esi, [Bit16Bmp3]
     mov edi, [esi+TFastDIB.bits]
     Mov ecx, 320
     Mov eax, BorderDWord
     and eax, 255
     shl eax, 2
     mov ax,  Word [_2xSaiColours16+eax]
     mov ebx, eax
     shl eax, 16
     mov ax,  bx

     Rep Stosd                     // Fill the Top border

     add edi, 238*2560
     mov ecx, 960
     Rep Stosd                     // Fill the bottom border

     popad

  End;
End;

Procedure InitLUTs; 
Var
  i, j, k, r, g, b, Y, u, v: Integer;
Begin

  for i := 0 to 65535 Do
     _LUT16to32[i] := ((i and $F800) Shl 8) + ((i and $07E0) shl 5) + ((i and $001F) shl 3);

  for i := 0 to 31 Do
     for j := 0 to 63 Do
        for k := 0 to 31 Do Begin
           r := i Shl 3;
           g := j shl 2;
           b := k shl 3;
           Y := (r + g + b) shr 2;
           u := 128 + ((r - b) shr 2);
           v := 128 + ((-r + 2*g -b) shr 3);
           _RGBtoYUV[(i shl 11) + (j shl 5) + k] := (Y shl 16) + (u shl 8) + v;
        End;
End;

procedure _doHq2x; 
begin
  if (Bit16Bmp1.width <> 640) or (Bit16Bmp1.Height <> 480) then begin
    Bit16Bmp1.SetSizeEx(320, 240, 16, 5, 6, 5);
    Bit16Bmp3.SetSizeEx(Bit16Bmp1.Width*2, -Bit16Bmp1.Height*2, 16, 5, 6, 5);
  end;
  if Bit16Bmp2.width > 1 then Bit16Bmp2.SetSize(1,1,16);
  ColourDump16BitNoDirty(Bit16Bmp1.Bits, 640, @_Hq2xColours16[0]);
  _hq2x_16(Bit16Bmp1.Bits, Bit16Bmp3.Bits, Bit16Bmp1.Width, Bit16Bmp1.AbsHeight, Bit16Bmp3.width*2);
end;

procedure _doHq3x; 
begin
  if (Bit16Bmp1.width <> 960) or (Bit16Bmp1.Height <> 720) then begin
    Bit16Bmp1.SetSizeEx(320, 240, 16, 5, 6, 5);
    Bit16Bmp3.SetSizeEx(Bit16Bmp1.Width*3, -Bit16Bmp1.Height*3, 16, 5, 6, 5);
  end;
  if Bit16Bmp2.width > 1 then Bit16Bmp2.SetSize(1,1,16);
  ColourDump16BitNoDirty(Bit16Bmp1.Bits, 640, @_Hq2xColours16[0]);
  _hq3x_16(Bit16Bmp1.Bits, Bit16Bmp3.Bits, Bit16Bmp1.Width, Bit16Bmp1.AbsHeight, (Bit16Bmp3.width*2));
end;

procedure _doHq4x;
begin
  if (Bit16Bmp1.width <> 1280) or (Bit16Bmp1.Height <> 960) then begin
    Bit16Bmp1.SetSizeEx(320, 240, 16, 5, 6, 5);
    Bit16Bmp3.SetSizeEx(Bit16Bmp1.Width*4, -Bit16Bmp1.Height*4, 16, 5, 6, 5);
  end;
  if Bit16Bmp2.width > 1 then Bit16Bmp2.SetSize(1,1,16);
  ColourDump16BitNoDirty(Bit16Bmp1.Bits, 640, @_Hq2xColours16[0]);
  _hq4x_16(Bit16Bmp1.Bits, Bit16Bmp3.Bits, Bit16Bmp1.Width, Bit16Bmp1.AbsHeight, (Bit16Bmp3.width*2));
end;


procedure TDisplayWindow.Print1Click(Sender: TObject);
begin

  CentreFormOnForm(PrintForm, nil);
  PrintForm.ComboBox1.ItemIndex := 2;
  ShowWindow(PrintForm, True);

end;

procedure TDisplayWindow.Close1Click(Sender: TObject);
begin

  Close;

end;

procedure TDisplayWindow.FormDeactivate(Sender: TObject);
begin

  CurKeyDown := 0;

end;

procedure TDisplayWindow.DisplayHelp1Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/display.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TDisplayWindow.FullSpeed1Click(Sender: TObject);
begin
//FullSpeed1.Checked :=Not FullSpeed1.Checked ;
if FASTMode = False Then Begin
     FASTMode := True;
     ResetSound;
     End Else Begin
     FASTMode := False;
     BorderUpdate := True;
     End;

end;

procedure TDisplayWindow.DisplayIMGMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 DisplayMouseBtn := 0;
end;

procedure TDisplayWindow.DisplayIMGMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

begin


    if Opt_PCStyleMouse then begin
        x:= (x-(32*displayscale)) div displayscale;
        y:=((192*displayscale)-(y-(24*displayscale)) ) div displayscale;
        if ((x>=0) and (x<=255) and (y>=0) and (y<=191)) then begin

               //help1.Caption := inttostr(x) + ',' +inttostr(y);

              KempMouseX := x and 255;
              KempMouseY := y and 255;
        end;
     end;
end;







Initialization

  InitializeCriticalSection(ResizeSection);
  Init_2xSaIMMX(555);
  InitLUTs;
  Bit16Bmp1 := TFastDIB.Create;
  Bit16Bmp2 := TFastDIB.Create;
  Bit16Bmp3 := TFastDIB.Create;

Finalization

  DeleteCriticalSection(ResizeSection);
  BackBuffer8Bit.Free;
  Bit16Bmp1.Free;
  Bit16Bmp2.Free;
  Bit16Bmp3.Free;

end.


