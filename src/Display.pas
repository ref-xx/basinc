unit Display;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FastIMG, ExtCtrls, FastDIB, Math, FastSize, Menus, Clipbrd, SyncObjs,
  ComCtrls;

type
  TGPUHostPanel = class(TPanel)
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  end;

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
    DisplayStatusBar1: TStatusBar;
    SpeedStatusTimer: TTimer;
    DisplayFillBorderCheck: TMenuItem;
    N4: TMenuItem;
    KeepOnTop1: TMenuItem;
    CopyImageToClipboard1: TMenuItem;
    insPlot1: TMenuItem;
    insPrint1: TMenuItem;
    insDraw1: TMenuItem;
    N5: TMenuItem;
    InsCircle1: TMenuItem;
    InsRect1: TMenuItem;
    InsPeek1: TMenuItem;
    N6: TMenuItem;
    SnaptoCodeWindow1: TMenuItem;
    N7: TMenuItem;
    ViewMicrovitecCub1: TMenuItem;
    GPUFilterMenu1: TMenuItem;
    GPUFilterOff1: TMenuItem;
    GPUFilterLottes1: TMenuItem;

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
    procedure DisplayStatusBar1DrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure SpeedStatusTimerTimer(Sender: TObject);
    procedure DisplayIMGMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplayIMGMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PopupMenuClick(Sender: TObject);


  private
    FGPUHost: TPanel;
    FGPURenderer: Pointer;
    FGPUErrorShown: Boolean;
    FGPUUpdateErrorLogged: Boolean;
    FSpeedIcons: array[Boolean] of TBitmap;
    FSpeedIconsLoaded: Boolean;
    FSpeedStatusReady: Boolean;
    FLastFastMode: Boolean;
    Procedure LoadSpeedIcons;
    Procedure RefreshSpeedStatus;
    Procedure CreateGPUHost;
    Procedure ShutdownGPURenderer;
    Procedure SyncGPUModeUI;
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
    procedure CopyImage;
    Procedure ApplyGPURenderer(Enable: Boolean);
    Procedure SetGPUFilter(FilterID: Integer);
    Function DisplayPointToSpectrum(X, Y: Integer): TPoint;
    Procedure RenderGPUBackground;
    Function UpdateGPUFrame(Pixels: Pointer; BorderRGBA: LongWord): Boolean;
    Destructor Destroy; Override;

  end;

var
  DisplayWindow: TDisplayWindow;
  BackBuffer8Bit: TFastDIB;
  FrameCounter: Integer;
  Bit16Bmp1, Bit16Bmp2, Bit16Bmp3: TFastDIB;

    DisplayMouseX,    //kemp mouse
    DisplayMouseY,
    DisplayMouseBtn:            dword;

    LastAddress,
    MousePosX,
    MousePosY,
    LocateX,
    LocateY,
    LastPlotX,
    LastPlotY:                      Integer;

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

Uses BASinMain, InputUtils, Filing, Utility, FastCore, ROMUtils, Sound, Printing, BasinGpuRenderer, Options, GraphicEx;

procedure TGPUHostPanel.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if (DisplayWindow <> nil) and
    not (csDestroying in DisplayWindow.ComponentState) and
    (DisplayWindow.FGPURenderer <> nil) then Begin
     GPUResize(DisplayWindow.FGPURenderer, ClientWidth, ClientHeight);
     DisplayWindow.RenderGPUBackground;
  End;
end;

Procedure TDisplayWindow.LoadSpeedIcons;
Var
  Fast: Boolean;
  Picture: TPicture;
  FileName: String;
Begin
  If FSpeedIconsLoaded Then Exit;
  FSpeedIconsLoaded := True;
  For Fast := False To True Do Begin
     If Fast Then FileName := 'basinc_fast.png'
     Else FileName := 'basinc_normal.png';
     Picture := TPicture.Create;
     Try
        Try
           Picture.LoadFromFile(BASinDir+'\icons\'+FileName);
           FSpeedIcons[Fast] := TBitmap.Create;
           FSpeedIcons[Fast].Assign(Picture.Graphic);
           FSpeedIcons[Fast].PixelFormat := pf32bit;
        Except
           On E: Exception Do Begin
              FreeAndNil(FSpeedIcons[Fast]);
              OutputDebugString(PChar('BasinC status icon: '+E.Message));
           End;
        End;
     Finally
        Picture.Free;
     End;
  End;
End;

Procedure TDisplayWindow.RefreshSpeedStatus;
Var
  Fast: Boolean;
Begin
  Fast := FASTMode;
  { FASTMode is authoritative; AutoCheck would add another toggle on F1. }
  FullSpeed1.Checked := Fast;
  If FSpeedStatusReady And (FLastFastMode = Fast) Then Exit;
  FSpeedStatusReady := True;
  FLastFastMode := Fast;
  If Fast Then DisplayStatusBar1.Panels[0].Text := 'Fast'
  Else DisplayStatusBar1.Panels[0].Text := 'Normal';
  If FSpeedIcons[Fast] <> nil Then DisplayStatusBar1.Panels[0].Width := 24
  Else DisplayStatusBar1.Panels[0].Width := 56;
  DisplayStatusBar1.Panels[1].Width := Max(0,
    DisplayStatusBar1.ClientWidth - DisplayStatusBar1.Panels[0].Width);
  DisplayStatusBar1.Hint := DisplayStatusBar1.Panels[0].Text+' (F1)';
  DisplayStatusBar1.Invalidate;
End;

procedure TDisplayWindow.SpeedStatusTimerTimer(Sender: TObject);
begin
  RefreshSpeedStatus;
end;

procedure TDisplayWindow.DisplayStatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
type
  PIconPixel = ^TRGBQuad;
var
  Icon: TBitmap;
  Pixel: PIconPixel;
  X, Y, Left, Top, Scale, Alpha: Integer;
  Background: TColor;
  R: TRect;
begin
  If Panel.Index <> 0 Then Exit;
  Background := ColorToRGB(StatusBar.Color);
  StatusBar.Canvas.Brush.Color := Background;
  StatusBar.Canvas.FillRect(Rect);
  Icon := FSpeedIcons[FASTMode];
  If (Icon = nil) Or Icon.Empty Then Begin
     R := Rect;
     DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), Length(Panel.Text),
       R, DT_CENTER Or DT_VCENTER Or DT_SINGLELINE);
     Exit;
  End;
  Scale := Max(1, Min(2, Min((Rect.Right-Rect.Left-4) Div Icon.Width,
    (Rect.Bottom-Rect.Top) Div Icon.Height)));
  Left := Rect.Left + ((Rect.Right-Rect.Left) - Icon.Width*Scale) Div 2;
  Top := Rect.Top + ((Rect.Bottom-Rect.Top) - Icon.Height*Scale) Div 2;
  { The PNGs contain straight alpha; draw integral blocks for their pixel art. }
  For Y := 0 To Icon.Height-1 Do Begin
     Pixel := Icon.ScanLine[Icon.Height-1-Y];
     For X := 0 To Icon.Width-1 Do Begin
        Alpha := Pixel^.rgbReserved;
        If Alpha <> 0 Then Begin
           StatusBar.Canvas.Brush.Color := RGB(
             (Pixel^.rgbRed*Alpha + GetRValue(Background)*(255-Alpha)+127) Div 255,
             (Pixel^.rgbGreen*Alpha + GetGValue(Background)*(255-Alpha)+127) Div 255,
             (Pixel^.rgbBlue*Alpha + GetBValue(Background)*(255-Alpha)+127) Div 255);
           R.Left := Left + X*Scale;
           R.Top := Top + Y*Scale;
           R.Right := R.Left + Scale;
           R.Bottom := R.Top + Scale;
           StatusBar.Canvas.FillRect(R);
        End;
        Inc(Pixel);
     End;
  End;
end;

Function TDisplayWindow.DisplayPointToSpectrum(X, Y: Integer): TPoint;
Begin
  Result := Point(-1, -1);
  If (DisplayIMG.Width <= 0) Or (DisplayIMG.Height <= 0) Then Exit;
  If Opt_GPUDisplay Then Begin
     { Shader origin (263,176) plus bitmap inset (79,113), at native 3x. }
     Result.X := Floor((X * (1440.0 / DisplayIMG.Width) - 342.0) / 3.0);
     Result.Y := Floor((Y * (1200.0 / DisplayIMG.Height) - 289.0) / 3.0);
  End Else Begin
     Result.X := Floor(X * (320.0 / DisplayIMG.Width) - 32.0);
     Result.Y := Floor(Y * (240.0 / DisplayIMG.Height) - 24.0);
  End;
End;

Procedure TDisplayWindow.CreateGPUHost;
Begin
  If FGPUHost <> nil Then Exit;
  FGPUHost := TGPUHostPanel.Create(Self);
  FGPUHost.Parent := Panel1;
  FGPUHost.BevelOuter := bvNone;
  FGPUHost.Caption := '';
  FGPUHost.Color := clBlack;
  FGPUHost.ParentColor := False;
  FGPUHost.Visible := False;
  FGPUHost.OnMouseDown := DisplayIMGMouseDown;
  FGPUHost.OnMouseMove := DisplayIMGMouseMove;
  FGPUHost.OnMouseUp := DisplayIMGMouseUp;
  FGPUHost.SetBounds(DisplayIMG.Left, DisplayIMG.Top, DisplayIMG.Width, DisplayIMG.Height);
End;

Procedure TDisplayWindow.ShutdownGPURenderer;
Begin
  GPUDestroy(FGPURenderer);
  If FGPUHost <> nil Then FGPUHost.Visible := False;
  DisplayIMG.Visible := True;
End;

Procedure TDisplayWindow.SyncGPUModeUI;
Begin
  ViewMicrovitecCub1.Checked := Opt_GPUDisplay;
  GPUFilterOff1.Checked := Opt_GPUFilter = GPU_FILTER_NONE;
  GPUFilterLottes1.Checked := Opt_GPUFilter = GPU_FILTER_LOTTES_CRT;
  If Opt_GPUDisplay Then Begin
     N100320x2401.Caption := '100% (480x400)';
     N200640x4801.Caption := '200% (960x800)';
     N3001.Caption := '300% (1440x1200)';
     N4001.Caption := '400% (1920x1600)';
     N6001920x14401.Caption := '600% (2880x2400)';
  End Else Begin
     N100320x2401.Caption := '100% (320x240)';
     N200640x4801.Caption := '200% (640x480)';
     N3001.Caption := '300% (960x720)';
     N4001.Caption := '400% (1280x960)';
     N6001920x14401.Caption := '600% (1920x1440)';
  End;
  If OptionsWindow <> nil Then
     OptionsWindow.CheckBox36.Checked := Opt_GPUDisplay;
End;

Procedure TDisplayWindow.SetGPUFilter(FilterID: Integer);
Begin
  If (FilterID <> GPU_FILTER_NONE) And (FilterID <> GPU_FILTER_LOTTES_CRT) Then Exit;
  If (FGPURenderer <> nil) And Not GPUSetFilter(FGPURenderer, FilterID) Then Begin
     OutputDebugString(PChar('BasinC GPU filter selection failed: ' + GPUGetLastError));
     Exit;
  End;
  Opt_GPUFilter := FilterID;
  SyncGPUModeUI;
  RenderGPUBackground;
End;
Procedure TDisplayWindow.ApplyGPURenderer(Enable: Boolean);
Var
  ErrorText: String;
  WasGPU: Boolean;
  CurrentScale: Integer;
Begin
  { Restored window bounds already use the saved mode before the DLL exists. }
  WasGPU := Opt_GPUDisplay;
  If WasGPU Then
     CurrentScale := Round(DisplayIMG.Width * 100 / 480)
  Else
     CurrentScale := Round(DisplayIMG.Width * 100 / 320);
  If CurrentScale < 100 Then CurrentScale := 100;
  If Not Enable Then Begin
     Opt_GPUDisplay := False;
     ShutdownGPURenderer;
     SyncGPUModeUI;
     If WasGPU Then SizeForm(CurrentScale);
     DisplayIMG.Repaint;
     Exit;
  End;

  { Do not expose GPU mode to the emulation thread before its renderer exists. }
  Opt_GPUDisplay := False;
  CreateGPUHost;
  FGPUHost.SetBounds(DisplayIMG.Left, DisplayIMG.Top, DisplayIMG.Width, DisplayIMG.Height);

  If FGPURenderer = nil Then Begin
     If Not GPUEnsureLoaded(BASinDir+'\BasinCGPU.dll') Then Begin
        ErrorText := GPUGetLastError;
     End Else If Not GPUCreate(FGPUHost.Handle,
        BASinDir+'\basincLazDX\overlays\Cub452-BACK1440x1200.png',
        BASinDir+'\basincLazDX\overlays\Cub452-MASK1440x1200.png', FGPURenderer) Then Begin
        ErrorText := GPUGetLastError;
     End;

     If FGPURenderer = nil Then Begin
        Opt_GPUDisplay := False;
        ShutdownGPURenderer;
        SyncGPUModeUI;
        If Not FGPUErrorShown Then Begin
           FGPUErrorShown := True;
           MessageBox(Handle, PChar('Experimental GPU Renderer baslatilamadi:'#13+ErrorText),
             'BasinC GPU Renderer', MB_OK or MB_ICONERROR);
        End;
        Exit;
     End;
  End;

  If Not GPUSetFilter(FGPURenderer, Opt_GPUFilter) Then Begin
     ErrorText := GPUGetLastError;
     ShutdownGPURenderer;
     SyncGPUModeUI;
     If WasGPU Then SizeForm(CurrentScale);
     MessageBox(Handle, PChar('GPU filter secilemedi:'#13+ErrorText),
       'BasinC GPU Renderer', MB_OK or MB_ICONERROR);
     Exit;
  End;

  FGPUErrorShown := False;
  FGPUUpdateErrorLogged := False;
  GPUResize(FGPURenderer, FGPUHost.ClientWidth, FGPUHost.ClientHeight);
  Opt_GPUDisplay := True;
  SyncGPUModeUI;
  FGPUHost.Visible := True;
  FGPUHost.BringToFront;
  DisplayIMG.Visible := False;
  If Not WasGPU Then SizeForm(CurrentScale);
  If FastCore.Display <> nil Then
     UpdateBASinDisplay
  Else
     GPURender(FGPURenderer);
End;
Procedure TDisplayWindow.RenderGPUBackground;
Begin
  If Opt_GPUDisplay And (FGPURenderer <> nil) Then
     GPURender(FGPURenderer);
End;

Function TDisplayWindow.UpdateGPUFrame(Pixels: Pointer;
  BorderRGBA: LongWord): Boolean;
Begin
  Result := False;
  If Not Opt_GPUDisplay Or (FGPURenderer = nil) Then Exit;
  Result := GPUUpdateFrame(FGPURenderer, Pixels, BorderRGBA);
  If Result Then Begin
     FGPUUpdateErrorLogged := False;
     GPURender(FGPURenderer);
  End Else If Not FGPUUpdateErrorLogged Then Begin
     FGPUUpdateErrorLogged := True;
     OutputDebugString(PChar('BasinC GPU frame update failed: ' + GPUGetLastError));
  End;
End;
Destructor TDisplayWindow.Destroy;
Begin
  SpeedStatusTimer.Enabled := False;
  ShutdownGPURenderer;
  FSpeedIcons[False].Free;
  FSpeedIcons[True].Free;
  Inherited Destroy;
End;

procedure TDisplayWindow.FormShow(Sender: TObject);
begin

  DoubleBuffered := True;
  SysTime := GetTickCount;
  LoadSpeedIcons;
  RefreshSpeedStatus;
  SpeedStatusTimer.Enabled := True;

  DisplayIMG.Bmp.SetSize(320, 240, 32);
  BackBuffer8Bit := TFastDIB.Create;
  BackBuffer8Bit.SetSize(320, 240, 8);
  ScaleXDIB := TFastDIB.Create;
  AVGEffectDIB := TFastDIB.Create;
  AVGEffectDIB.SetSize(320, 240, 32);

  InitScaleDIBs;
  ApplyGPURenderer(Opt_GPUDisplay);

  FrameCounter := 1;
  DisplayFillBorderCheck.checked:= Opt_DisplayFillBorder;

   if (Opt_DisplayOnTop) Then Begin
       SetWindowPos(Handle, HWND_TOPMOST, 0,0,0,0, SWP_NOMOVE or SWP_NOSIZE);
   End;


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
     If Opt_GPUDisplay Then Begin
        If NW < 480 Then NewWidth := Width;
        If NH < 400 Then NewHeight := Height;
     End Else Begin
        If NW < 256 Then NewWidth := Width;
        If NH < 192 Then NewHeight := Height;
     End;
  End;
end;

procedure TDisplayWindow.FormResize(Sender: TObject);
Var
  NW, NH: Integer;
begin

  If AppClosing Then Exit;

  ClearSoundBuffers;
  DisplayStatusBar1.Panels[1].Width := Max(0,
    DisplayStatusBar1.ClientWidth - DisplayStatusBar1.Panels[0].Width);

  If Opt_GPUDisplay Then Begin
     NW := Max(Panel1.Width - 8, 480);
     NH := Max(Panel1.Height - 8, 400);
     If NW * 5 > NH * 6 Then
        NW := (NH * 6) Div 5
     Else
        NH := (NW * 5) Div 6;
  End Else Begin
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
  End;

  If Opt_ClipCorners Then
     DisplayIMG.SetBounds((Panel1.Width Div 2) - (NW Div 2), (Panel1.Height Div 2) - (NH Div 2), NW, NH)
  Else
     DisplayIMG.SetBounds((Panel1.Width Div 2) - (NW Div 2) -2, (Panel1.Height Div 2) - (NH Div 2) -2, NW, NH);

  If FGPUHost <> nil Then
     FGPUHost.SetBounds(DisplayIMG.Left, DisplayIMG.Top, DisplayIMG.Width, DisplayIMG.Height);

 SizeDIBS;

  If (FGPUHost <> nil) And FGPUHost.Visible Then Begin
     FGPUHost.Invalidate;
     FGPUHost.Update;
  End;

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


     If (FGPURenderer <> nil) And (FGPUHost <> nil) Then
        GPUResize(FGPURenderer, FGPUHost.ClientWidth, FGPUHost.ClientHeight);

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
  Y1,Y2,SourceLine: PLine8;
  pc: PFColorA;
  Cla, Clb: TFColorA;
  GPUFrame: array of Byte;
  SourceIndex, PixelOffset: Integer;
  BorderIndex: Byte;
  BorderColor, PixelColor: TFColorA;
  BorderRGBA: LongWord;
Label
  ExitPoint;
begin

  If Opt_GPUDisplay Then Begin
     If FastCore.Display = Nil Then Begin
        DisplayWindow.RenderGPUBackground;
        Exit;
     End;

     SetLength(GPUFrame, 256 * 192 * 4);
     BorderIndex := FastCore.Display.Pixels8[0, 0];
     If Active64Colours And Opt_64Colours Then
        BorderColor := DisplayPalette64[BorderIndex]
     Else
        BorderColor := DisplayPalette[BorderIndex];
     BorderRGBA := LongWord(BorderColor.r) or
       (LongWord(BorderColor.g) shl 8) or
       (LongWord(BorderColor.b) shl 16) or $FF000000;

     PixelOffset := 0;
     For Y := 0 To 191 Do Begin
        SourceLine := FastCore.Display.Scanlines[Y + 24];
        For X := 0 To 255 Do Begin
           SourceIndex := SourceLine[X + 32];
           If Active64Colours And Opt_64Colours Then
              PixelColor := DisplayPalette64[SourceIndex]
           Else
              PixelColor := DisplayPalette[SourceIndex];
           GPUFrame[PixelOffset] := PixelColor.r;
           GPUFrame[PixelOffset + 1] := PixelColor.g;
           GPUFrame[PixelOffset + 2] := PixelColor.b;
           GPUFrame[PixelOffset + 3] := 255;
           Inc(PixelOffset, 4);
        End;
     End;

     If DisplayWindow.UpdateGPUFrame(@GPUFrame[0], BorderRGBA) Then
        Exit;

     DisplayWindow.RenderGPUBackground;
     Exit;
  End;

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
  temp, Dw, Dh, Nw, Nh: Integer;
Begin

  If Scale = 0 Then Begin
     If Opt_GPUDisplay Then
        Scale := Round((DisplayIMG.Width/480)*100)
     Else
        Scale := Round((DisplayIMG.Width/320)*100);
  End;

  If Opt_GPUDisplay Then Begin
     Dw := Round(480*(Scale/100));
     Dh := Round(400*(Scale/100));
  End Else Begin
     Dw := Round(320*(Scale/100));
     Dh := Round(240*(Scale/100));
  End;
  If Opt_GPUDisplay Then Begin
     Nw := Dw + 8 + (Width - Panel1.Width);
     Nh := Dh + 8 + (Height - Panel1.Height);
  End Else Begin
     Nw := Dw + 8  + (GetSystemMetrics(SM_CXSIZEFRAME)*2);
     Nh := Dh + 8  +  GetSystemMetrics(SM_CYMENU) +
                      GetSystemMetrics(SM_CYCAPTION) +
                     (GetSystemMetrics(SM_CYSIZEFRAME)*2) + DisplayStatusBar1.Height;
  End;

  DisplayIMG.SetBounds(DisplayIMG.Left, DisplayIMG.Top, Dw, Dh);



  DisplayWindow.SetBounds(Left, Top, Nw, Nh);
End;

procedure TDisplayWindow.DisplayIMGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  Done: Boolean;
  PopUpPoint: TPoint;
  IMGx, IMGy, CharX, CharY: Integer;
  SpectrumPoint: TPoint;
  Idx, Index, SearchPos, StartPos, CurPos: Integer;
  
begin
    insPlot1.Enabled:=false;
    insDraw1.Enabled:=false;
    insPrint1.Enabled:=false;
    insCircle1.Enabled:=false;
    insRect1.Enabled:=false;
    InsPeek1.Enabled:=false;

    // If Not MouseHooked Then
     //   If MouseFunction = 1 Then
     //      HookMouse;

     if (ssLeft in Shift) and (ssRight in Shift) then
        DisplayMouseBtn := 3
     else if Button = mbLeft then
        DisplayMouseBtn := 1
     else if Button = mbRight then
        DisplayMouseBtn := 2;


  If Button = mbLeft Then Begin

  End;
  If Button = mbRight Then Begin
           if (MousePosY<>999) Then
           Begin
             insPlot1.Enabled:=true;
             insDraw1.Enabled:=true;
             insPrint1.Enabled:=true;
             insCircle1.Enabled:=true;
             insRect1.Enabled:=true;

           End;
           if (LastAddress>16384) Then  InsPeek1.Enabled:=true;

             PopUpPoint := DisplayIMG.ClientToScreen(Point(X, Y));
             PopUpMenu1.Popup(PopUpPoint.X, PopUpPoint.Y);

  End;

  If Button = mbRight Then Begin

     // Token helper system.
     // Alter X and Y to char positions.

     IMGx := X;
     IMGy := Y;

     SpectrumPoint := DisplayPointToSpectrum(X, Y);
     X := SpectrumPoint.X;
     Y := SpectrumPoint.Y;

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
           Detokenise1.Visible:=true;
           Help2.Visible:=true;
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

  {if (TMenuItem(Sender).Tag = 13) or (TMenuItem(Sender).Tag = 3) then
  Begin
  End Else Begin
        if TMenuItem(Sender).Checked then
        Begin
                Exit;
        End;
  End;
   }

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
        Begin // x300
           DisplayWindow.Show;
           DisplayWindow.SizeForm(300);
        End;
     9:
        Begin // x400
           DisplayWindow.Show;
           DisplayWindow.SizeForm(400);
        End;
     12:
        Begin //Save Image
               SaveImage;
        End;
     10:
        Begin // x600
           DisplayWindow.Show;
           DisplayWindow.SizeForm(600);
        End;
     13:
        Begin //Fill border
          DisplayFillBorderCheck.Checked:= Not DisplayFillBorderCheck.Checked;
          Opt_DisplayFillBorder:=DisplayFillBorderCheck.checked;
          If Opt_DisplayFillBorder Then Begin
              BorderUpdate:= True;
              DisplayWindow.Panel1.Color := clBtnShadow ;
          End Else Begin
              BorderUpdate:= True;
              DisplayWindow.Panel1.Color := 0;
          End;
          SetPortByte(766,getPortByte(766));  //force update border
        End;

     14:
        Begin //Always on top

             KeepOnTop1.Checked:=Not KeepOnTop1.Checked;
             Opt_DisplayOnTop:=KeepOnTop1.Checked;

             if (Opt_DisplayOnTop) Then Begin
                 SetWindowPos(DisplayWindow.Handle, HWND_TOPMOST, 0,0,0,0, SWP_NOMOVE or SWP_NOSIZE);
             End Else Begin
                 SetWindowPos(DisplayWindow.Handle, HWND_NOTOPMOST, 0,0,0,0, SWP_NOMOVE or SWP_NOSIZE);
             End;

        End;

     15:
        Begin //Copy to clipboard
            CopyImage;
        End;

     16:
        Begin //snap to display
           Opt_DisplaySnap:= Not Opt_DisplaySnap;
           BasinOutput.SnaptoEditor1.Checked:=Opt_DisplaySnap;
           SnaptoCodeWindow1.Checked:=Opt_DisplaySnap;
           if (Opt_DisplaySnap) Then Begin
                 DisplayWindow.Top:=BASinOutput.Top;
                 DisplayWindow.Left:=BASinOutput.Left+BASinOutput.Width;
           End;
        End;

     17:
        ApplyGPURenderer(Not Opt_GPUDisplay);
     18:
        SetGPUFilter(GPU_FILTER_NONE);
     19:
        SetGPUFilter(GPU_FILTER_LOTTES_CRT);

     50:
     Begin //update checks
       KeepOnTop1.Checked:=Opt_DisplayOnTop;
     End;
  End;
end;

procedure TDisplayWindow.SaveImage;
Var
   Result: TFastDIB;
   Filename: String;

Begin


  Result := TFastDIB.Create;

     Result.SetSize(DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, 32);
     DisplayWindow.DisplayIMG.Bmp.Draw(Result.hDc, 0, 0);

    // get a filename.
    Filename := OpenFile(Handle, 'Save Display as Bitmap Image', [FTBmp], '', True, False);
    If Filename = '' Then Exit;

    Result.SaveToFile(Filename);
    Result.Free;

End;


procedure TDisplayWindow.CopyImage; //by arda
Var
   Result: TFastDIB;
   Bitmap: TBitmap;
Begin


     Result := TFastDIB.Create;
     Result.SetSize(DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, 32);
     DisplayWindow.DisplayIMG.Bmp.Draw(Result.hDc, 0, 0);

     Bitmap := TBitmap.Create;
      Bitmap.Width := Result.Width;
      Bitmap.Height := Result.Height;
      Bitmap.PixelFormat := pf24bit;
      Result.Draw(Bitmap.Canvas.Handle, 0, 0);
      Clipboard.Assign(Bitmap);
      Bitmap.Free;

    Result.Free;


End;

procedure TDisplayWindow.OnEnterMenuLoop(var Message: TMessage);
Var
  BaseWidth, BaseHeight: Integer;
Begin
  RefreshSpeedStatus;
  If Opt_GPUDisplay Then Begin
     BaseWidth := 480;
     BaseHeight := 400;
  End Else Begin
     BaseWidth := 320;
     BaseHeight := 240;
  End;
  ViewMicrovitecCub1.Checked := Opt_GPUDisplay;
  GPUFilterOff1.Checked := Opt_GPUFilter = GPU_FILTER_NONE;
  GPUFilterLottes1.Checked := Opt_GPUFilter = GPU_FILTER_LOTTES_CRT;
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
  N3001.Checked := False;
  N4001.Checked := False;
  N6001920x14401.Checked := False;
  Custom1.Checked := False;
  Custom1.Enabled := False;
  Custom1.Visible := False;
  If (DisplayIMG.Width = BaseWidth) and (DisplayIMG.Height = BaseHeight) Then
     N100320x2401.Checked := True
  Else If (DisplayIMG.Width = BaseWidth * 2) and (DisplayIMG.Height = BaseHeight * 2) Then
     N200640x4801.Checked := True
  Else If (DisplayIMG.Width = BaseWidth * 3) and (DisplayIMG.Height = BaseHeight * 3) Then
     N3001.Checked := True
  Else If (DisplayIMG.Width = BaseWidth * 4) and (DisplayIMG.Height = BaseHeight * 4) Then
     N4001.Checked := True
  Else If (DisplayIMG.Width = BaseWidth * 6) and (DisplayIMG.Height = BaseHeight * 6) Then
     N6001920x14401.Checked := True
  Else Begin
     Custom1.Visible := True;
     Custom1.Enabled := True;
     Custom1.Checked := True;
     Custom1.Caption := 'Custom Size ('+IntToStr(DisplayWindow.DisplayIMG.Width)+'x'+IntToStr(DisplayWindow.DisplayIMG.Height)+')';
  End;
  Force11Aspect1.Checked := Opt_GPUDisplay or Opt_MaintainAspect or (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]);
  Force11Aspect1.Enabled := Not Opt_GPUDisplay and Not (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]);
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

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/display.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TDisplayWindow.FullSpeed1Click(Sender: TObject);
begin
  If Not FASTMode Then Begin
     FASTMode := True;
     ResetSound;
  End Else Begin
     FASTMode := False;
     BorderUpdate := True;
  End;
  RefreshSpeedStatus;
end;

procedure TDisplayWindow.DisplayIMGMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 DisplayMouseBtn := 0;
end;


procedure TDisplayWindow.DisplayIMGMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  I, PixelAddress, AttributeAddress, AttributeValue: Integer;
  CurX, CurY, PlotY, AttrX, AttrY: Integer;
  SpectrumPoint: TPoint;
  BinaryValue, PlotText: String;
begin
  RefreshSpeedStatus;
  MousePosX := 999;
  MousePosY := 999;
  LocateX := 999;
  LocateY := 999;
  LastAddress := 0;
  SpectrumPoint := DisplayPointToSpectrum(X, Y);
  CurX := SpectrumPoint.X;
  CurY := SpectrumPoint.Y;
  If (CurX >= 0) And (CurX < 256) And (CurY >= 0) And (CurY < 192) Then Begin
     AttrX := CurX Div 8;
     AttrY := CurY Div 8;
     AttributeAddress := 16384 + 6144 + AttrX + AttrY*32;
     AttributeValue := Memory[AttributeAddress];
     PixelAddress := 16384 + (CurY Div 64)*2048 +
       ((CurY Mod 64) Div 8)*32 + (CurY Mod 8)*256 + CurX Div 8;
     LastAddress := PixelAddress;
     BinaryValue := '';
     For I := 7 Downto 0 Do
        If (Memory[PixelAddress] And (1 Shl I)) <> 0 Then
           BinaryValue := BinaryValue + '1'
        Else
           BinaryValue := BinaryValue + '0';
     LocateX := AttrX;
     LocateY := AttrY;
     PlotText := '--,--';
     If CurY < 176 Then Begin
        PlotY := 175 - CurY;
        MousePosX := CurX;
        MousePosY := PlotY;
        PlotText := IntToStr(CurX)+','+IntToStr(PlotY);
     End;
     DisplayStatusBar1.Panels[1].Text :=
       '[AT : '+IntToStr(AttrY)+','+IntToStr(AttrX)+'] [PLOT: '+PlotText+
       '] [CURSOR X,Y: '+IntToStr(CurX)+','+IntToStr(CurY)+
       '] [Address: '+IntToStr(PixelAddress)+' ,'+IntToStr(Memory[PixelAddress])+
       ' B'+BinaryValue+'] [ATTR @'+IntToStr(AttributeAddress)+': '+
       IntToStr(AttributeValue)+' ($'+IntToHex(AttributeValue,2)+')]';
     If Opt_PCStyleMouse Then Begin
        KempMouseX := CurX And 255;
        KempMouseY := (191 - CurY) And 255;
     End;
  End Else
     DisplayStatusBar1.Panels[1].Text := 'Border';
end;

procedure TDisplayWindow.PopupMenuClick(Sender: TObject);
var
  TempStr: String;

  // --- Yardimci Prosedür: Kodu güvenli bir sekilde ekler ---
  procedure InsertCommand(CmdStr: String);
  var
    LineCheck, NewPos: Integer;
    FinalStr: String;
  begin
    // 1. Satir numarasi kontrolü
    LineCheck := BASinOutput.CreateNewLineNumber;
    
    // Eger yeni numara gerekiyorsa (Negatif deger) basina ekle
    if (LineCheck < 0) and (LineCheck <> 99999) then
      FinalStr := IntToStr(Abs(LineCheck)) + ' ' + CmdStr
    else
      FinalStr := CmdStr;

    // 2. Metni hafizaya ekle
    BASinOutput.BASICMem := Copy(BASinOutput.BASICMem, 1, BASinOutput.CursOffset - 1) + 
                            FinalStr + 
                            Copy(BASinOutput.BASICMem, BASinOutput.CursOffset, 999999);

    // 3. Imleci güncelle
    NewPos := BASinOutput.CursOffset + Length(FinalStr);
    BASinOutput.CursOffset := NewPos;
    
    // 4. Editörü bilgilendir ve tazele
    BASinOutput.BASICChanged := True; // Syntax check için gerekli
    BASinOutput.RepaintBASIC(True);
    BASinOutput.UpdateCursorPos(NewPos, False); // Imleci ekranda kaydir
    BASinOutput.MakeCursorVisible; // Görünür oldugundan emin ol
  end;
  // ---------------------------------------------------------

begin
  Case (Sender As TComponent).Tag of
     1:
        Begin // Print AT
            TempStr := 'PRINT AT '+IntToStr(LocateY)+','+IntToStr(LocateX)+';';
            InsertCommand(TempStr);
        End;
     2:
        Begin  // PLOT
            if MousePosY <> 999 Then Begin
              TempStr := 'PLOT '+IntToStr(MousePosX)+','+IntToStr(MousePosY)+':';
              LastPlotX := MousePosX;
              LastPlotY := MousePosY;
              InsertCommand(TempStr);
            End;
        End;
     3:
        Begin  // DRAW
             if MousePosY <> 999 Then Begin
              TempStr := 'DRAW '+IntToStr(MousePosX-LastPlotX)+','+IntToStr(MousePosY-LastPlotY)+':';
              LastPlotX := MousePosX;
              LastPlotY := MousePosY;
              InsertCommand(TempStr);
            End;
        End;
     4:
        Begin  // CIRCLE
             if MousePosY <> 999 Then Begin
              TempStr := 'CIRCLE '+IntToStr(MousePosX)+','+IntToStr(MousePosY)+',15'+':';
              // Circle LastPlot'u etkiler mi? Genelde etkilemez ama Sinclair BASIC mantigina göre degisebilir.
              // Simdilik dokunmuyoruz.
              InsertCommand(TempStr);
            End;
        End;
     5:
        Begin  // Rect (4 tane DRAW komutu ile dikdörtgen çizer)
             if MousePosY <> 999 Then Begin
              // Dikdörtgen çizimi için göreceli koordinatlar
              TempStr := 'DRAW '+IntToStr(MousePosX-LastPlotX)+',0:';
              TempStr := TempStr+'DRAW 0, '+IntToStr(MousePosY-LastPlotY)+':';
              TempStr := TempStr+'DRAW -'+IntToStr(MousePosX-LastPlotX)+',0:';
              TempStr := TempStr+'DRAW 0, -'+IntToStr(MousePosY-LastPlotY)+':';

              LastPlotX := MousePosX;
              LastPlotY := MousePosY;
              InsertCommand(TempStr);
            End;
        End;
     6:
        Begin  // Peek
              // PEEK tek basina bir ise yaramaz (PRINT PEEK... veya LET a=PEEK... olmali)
              // ama orijinal kodunuzda böyle oldugu için koruyoruz.
              TempStr := 'PEEK '+IntToStr(LastAddress);
              InsertCommand(TempStr);
        End;
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


