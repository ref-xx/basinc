unit Paintbox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FastIMG, FastDIB, FastCore, ExtCtrls, Math, Utility, FastDrawEx, Menus, FastDraw,
  ToolWin, ComCtrls, Buttons, ImgList, TransparentPanel, StdCtrls, FastSize, FastFX,
  Tabs, FastFiles, BrushSelector, Clipbrd, Tapes, ThemeBevelUnit;

type

  TSpBtn = Class(TSpeedButton);

  TPaintMode = (pmNone, pmPaint, pmFreehand, pmFreehandConnected,
                pmLine, pmCurve1, pmCurve2, pmFloodFill, pmSpraycan,
                pmRectangle, pmCircle, pmEllipse, pmPolygon,
                pmSelectRect, pmSelectFree, pmMagicWand, pmMove, pmZoom,
                pmTextPlace, pmText, pmColourDropper, pmCloneBrush,
                pmPan, pmPanAttrs, pmRotate, pmRotateAttrs, pmSkew, pmSkewAttrs);

  TAttributeFlags = Set of (pwINK, pwPAPER, pwBRIGHT, pwFLASH);
  TColourMode = (cmAttrs, cmPixels, cmBoth);

  TPattern = (ptPen, ptFill, ptBrush, ptClone);
  TFillType = (ftPattern, ftGradient);
  TPenType = (ptRound, ptSquare, ptGraphical);
  TMergeType = (mtNone, mtAdd, mtXOR, mtSubtract);

  TScrImage = Record
     InkDIB: Array[0..255, 0..191] of Byte;
     Attrs: Array[0..31, 0..23] of Byte;
     SelMask, SelDetail, SelAttrDetail: Array of Byte;
     ZoomLevel: Integer;
     Update: Boolean;
     SelActive: Boolean;
     SelOrigin: TPoint;
     SelWidth, SelHeight: Integer;
     Changed, HasFLASHAttrs: Boolean;
     Undo, Redo: TStringlist;
     Filename: String;
  End;
  pScrImage = ^TScrImage;

  TScrPaintForm = class(TForm)
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    New1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Import1: TMenuItem;
    N3: TMenuItem;
    Close1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Zoomin1: TMenuItem;
    ZoomOut1: TMenuItem;
    N4: TMenuItem;
    ShowAttributes1: TMenuItem;
    Contents1: TMenuItem;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    PopupMenu1: TPopupMenu;
    Freehand1: TMenuItem;
    FreeConnected1: TMenuItem;
    ImageList1: TImageList;
    PopupMenu3: TPopupMenu;
    Lines1: TMenuItem;
    Curves1: TMenuItem;
    PopupMenu4: TPopupMenu;
    Rectangle1: TMenuItem;
    Ellipse1: TMenuItem;
    Circle1: TMenuItem;
    Polygon1: TMenuItem;
    PopupMenu5: TPopupMenu;
    Select1: TMenuItem;
    Selectfreehand1: TMenuItem;
    Magicwand1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    FastIMG1: TFastIMG;
    CoolBar2: TCoolBar;
    Panel3: TPanel;
    FastIMG2: TFastIMG;
    FastIMG5: TFastIMG;
    ToolButton2: TToolButton;
    PopupMenu2: TPopupMenu;
    Attributepaint1: TMenuItem;
    Colourpicker1: TMenuItem;
    FastIMG3: TFastIMG;
    CloneBrush1: TMenuItem;
    ToolButton7: TToolButton;
    Recent1: TMenuItem;
    Item11: TMenuItem;
    Item21: TMenuItem;
    Item31: TMenuItem;
    Item41: TMenuItem;
    Item51: TMenuItem;
    Item61: TMenuItem;
    Item71: TMenuItem;
    Item81: TMenuItem;
    Item91: TMenuItem;
    Item101: TMenuItem;
    N5: TMenuItem;
    Undo1: TMenuItem;
    Clear1: TMenuItem;
    N6: TMenuItem;
    Grid1: TMenuItem;
    ShowBrushShapes1: TMenuItem;
    Image1: TMenuItem;
    Flip1: TMenuItem;
    MIrror1: TMenuItem;
    SnaptoGrid1: TMenuItem;
    N9: TMenuItem;
    Paint1: TMenuItem;
    Pixels1: TMenuItem;
    Attributes1: TMenuItem;
    Seection1: TMenuItem;
    Selectall1: TMenuItem;
    Selectnone1: TMenuItem;
    N10: TMenuItem;
    Load1: TMenuItem;
    Saveas2: TMenuItem;
    N11: TMenuItem;
    ShowMarquee1: TMenuItem;
    N12: TMenuItem;
    Expand1: TMenuItem;
    Contract1: TMenuItem;
    Maketransparent1: TMenuItem;
    Invertpixels1: TMenuItem;
    Invertattributes1: TMenuItem;
    Invert1: TMenuItem;
    RemoveBRIGHT1: TMenuItem;
    RemoveFLASH1: TMenuItem;
    FromCurrentScreen1: TMenuItem;
    Close2: TMenuItem;
    PasteTransparent1: TMenuItem;
    GridSize1: TMenuItem;
    All1: TMenuItem;
    N14: TMenuItem;
    Ink1: TMenuItem;
    Paper1: TMenuItem;
    Flash1: TMenuItem;
    Bright1: TMenuItem;
    FastIMG4: TFastIMG;
    TabControl1: TTabControl;
    Panel4: TPanel;
    Panel5: TPanel;
    N7: TMenuItem;
    N13: TMenuItem;
    Optimisefortape1: TMenuItem;
    ToolButton12: TToolButton;
    PopupMenu6: TPopupMenu;
    PanScroll1: TMenuItem;
    PanScrollwithAttrs1: TMenuItem;
    Rotate1: TMenuItem;
    RotatewithAttrs1: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Bevel1: TThemeBevel;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Bevel2: TThemeBevel;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    Bevel3: TThemeBevel;
    SpeedButton13: TSpeedButton;
    N8: TMenuItem;
    DrawOutline1: TMenuItem;
    Filled1: TMenuItem;
    FastIMG6: TFastIMG;
    N15: TMenuItem;
    Copybitmap1: TMenuItem;
    Pastebitmap1: TMenuItem;
    Defineselection1: TMenuItem;
    Skew1: TMenuItem;
    SkewwithAttrs1: TMenuItem;
    Redo1: TMenuItem;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    Bevel4: TThemeBevel;
    SendtoCurrentScreen1: TMenuItem;
    SendtoTape1: TMenuItem;
    N16: TMenuItem;
    Mousecursor1: TMenuItem;
    Windowspointer1: TMenuItem;
    Crosshair1: TMenuItem;
    None1: TMenuItem;
    PacktoMemory1: TMenuItem;
    N18: TMenuItem;
    PacktoTape1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure SelectTool(Tool: Integer);
    Procedure MenuItemClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure FastIMG2Click(Sender: TObject);
    procedure FastIMG5Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure FastIMG3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1Enter(Sender: TObject);
    procedure FastIMG1Exit(Sender: TObject);
    procedure MainMenuClick(Sender: TObject);
    procedure OnEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FastIMG4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TabControl1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure FastIMG6Enter(Sender: TObject);
    procedure FastIMG6Exit(Sender: TObject);
    procedure FastIMG6MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBox1MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure CoolBar2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
    oldScrollBoxWndMethod, OldPanelWndMethod: TWndMethod;
    procedure NewScrollWndProc(var msg: TMessage);
  public
    { Public declarations }
    Startup: Boolean;
    ElapsedTicks: Integer;
    SprayDensity: Integer;
    AntOffset: Integer;
    NumScreens: Integer;
    CurScreen: pScrImage;
    CurScreenIndex: Integer;
    CurINK, CurPAPER, CurBRIGHT, CurFLASH: Byte;
    Screens: Array of pScrImage;
    ClipScr: TScrImage;
    BackImage: TScrImage;
    InternalDIB: TFastDIB;
    SelectionActive: Boolean;
    PaintMode, LastPaintMode: TPaintMode;
    CloneX, CloneY, CloneOriginX, CloneOriginY, CloneImgX, CloneImgY: Integer;
    ImgMouseX, ImgMouseY, MouseX, MouseY, CentreX, CentreY, SprayX, SprayY: Integer;
    MouseIsDown, MouseInImage, ShowBrushShapes: Boolean;
    MouseButton: TMouseButton;
    ShowingAttrs: Boolean;
    PenArray, FillArray: Array of Byte;
    CurrentPattern: Array of Byte;
    CurrentPatternWidth, CurrentPatternHeight: Integer;
    CurrentFill: Array of Byte;
    CurrentFillWidth, CurrentFillHeight: Integer;
    CurrentBrush: Array of Byte;
    CurrentBrushWidth, CurrentBrushHeight: Integer;
    CurveStartX, CurveStartY, CurveEndX, CurveEndY: Integer;
    CurrentPenType: TPenType;
    CurrentPenDensity, CurrentPenSizeX, CurrentPenSizeY, CurrentPenRotation: Integer;
    EllipseBoxed, CircleBoxed: Boolean;
    PolygonLines: Array of TPoint;
    PaintColorTable: TFColorTable;
    LastSelection: TScrImage;
    FillShapes, OutLineShapes: Boolean;
    NullSelection, CentrePen: Boolean;
    TimerToolButton: TToolButton;
    Closing: Boolean;
    PenHistory: TStringlist;
    ColourMode: TColourMode;
    RecentPics: TStringlist;
    FlashState: Boolean;
    ShowGrid, SnapToGrid: Boolean;
    GridX, GridY: Integer;
    AttrFlags: TAttributeFlags;
    UndoScreen: TScrImage;
    SelectionMarquee: Boolean;
    RotateXAnchor, RotateYAnchor: Integer;
    FillStyle: TFillType;
    GradientDIB: TFastDIB;
    GradientType, NewGradientType: TGradientType;
    GradAnchorX, GradAnchorY, GradRectW, GradRectH, NewGradientWidth, NewGradientHeight: Integer;
    Untitled: Integer;
    CanEnter: Boolean;
    LastMWheelTime: Integer;

    Procedure NewScreen;
    Procedure CopyScreen(var Src, Dst: TScrImage);
    Procedure RemoveScreen(Index: Integer);
    Function  QuerySave(Index: Integer): Boolean;
    Function  SaveImageAs(ImageIndex: Integer): Boolean;
    Procedure MemToScreen(MemImage: String; ScreenIndex: Integer);
    Procedure RenderScreen(Index: Integer; ShowAttrs: Boolean);
    Procedure CreateColorTable;
    Procedure CreateBackImage;
    Procedure RestoreBackImage;
    Procedure PatternFromChar(Destination: TPattern; Character: Char);
    Procedure SetPixel(X, Y, C, Xc, Yc: Integer; FillType: TPattern);
    Procedure RenderBrushShape(Xcc, Ycc: Integer);
    Procedure DrawLine(X1, Y1, X2, Y2, Clr: Integer);
    Procedure DrawCurve(X, Y, Clr: Integer);
    Procedure FloodFill(X, Y, Clr: Integer);
    Procedure DrawRectangle(X1, Y1, X2, Y2, Clr: Integer; Filled: Boolean);
    Procedure DrawEllipse(Cx, Cy, Rx, Ry, Clr: Integer; Boxed, Filled: Boolean);
    Procedure DrawPolygon(Clr: Integer; Filled: Boolean);
    Procedure RemoveSelection(Var MaskScreen: TScrImage);
    Procedure ClearSelection;
    Procedure SetSelectionRect(X1, Y1, X2, Y2: Integer; MergeType: TMergeType);
    Procedure MergeSelections(Var CurrentScreen, LastScreen: TScrImage; MergeType: TMergeType);
    Procedure SetSelectionMagic(X1, Y1: Integer; MergeType: TMergeType);
    Procedure SetSelectionFree(MergeType: TMergeType);
    Procedure MakeSelectionTransparent(Index: Integer);
    Procedure ExpandSelection(Index: Integer);
    Procedure ShrinkSelection(Index: Integer);
    Procedure SetSelectionAttrs;
    Procedure ZoomIn;
    Procedure ZoomOut;
    Procedure RenderToolStyle(DIB: TFastDIB; Pattern: TPattern);
    Procedure MakePen(PenType: TPenType; Var SizeX, SizeY: Integer; Rotation: Integer; Graph: Pointer);
    Procedure RotatePen(Bmp,Dst: TFastDIB; cx,cy: Integer; Angle: Double; Precise: Boolean);
    Function  GetNewAttr(CurAttr, NewINK, NewPAPER, NewBRIGHT, NewFLASH: Byte; Flags: TAttributeFlags): Byte; Overload;
    Function  GetNewAttr(CurAttr, NewAttr: Byte; Flags: TAttributeFlags): Byte; Overload;
    Procedure DrawPalette;
    Procedure HLine(Bmp: TFastDIB; X1, Y, X2: Integer);
    Procedure VLine(Bmp: TFastDIB; X, Y1, Y2: Integer);
    Procedure GHLine(Bmp: TFastDIB; X1, Y, X2: Integer);
    Procedure GVLine(Bmp: TFastDIB; X, Y1, Y2: Integer);
    Procedure MakeRecentList;
    Procedure AddToRecent(Filename: String);
    Procedure BeginText(X, Y, Mx, My: Integer);
    Procedure RenderFlashState;
    Procedure PaintAttribFlags;
    Procedure MakeUndo(Caption: String; ClearRedo: Boolean);
    Procedure MakeRedo(Caption: String);
    Procedure PerformUndo(UndoString: String; Var Screen: pScrImage);
    Function  GetDiff(OldStr, NewStr: String): String;
    Procedure CheckColourMode;
    Procedure SaveSelection(Screen: pScrImage);
    Procedure LoadSelection(Screen: pScrImage);
    Procedure FlipPixels(Screen: pScrImage);
    Procedure FlipATTRs(Screen: pScrImage);
    Procedure RemoveATTR(Screen: pScrImage; Mask: Byte);
    Procedure ImageFlip(Screen: pScrImage);
    Procedure ImageMirror(Screen: pScrImage);
    Procedure OptimiseForTape(Index: Integer);
    Procedure PanImage(Dx, Dy: Integer; Attrs, Wrap: Boolean);
    Procedure RotateImage(Angle, Cx, Cy: Integer; Attrs: Boolean);
    Procedure SkewImage(Cx, Cy, Dx, Dy: Integer; Wrap, Attrs: Boolean);
    Procedure UpdateStatusBar(X, Y: Integer);
    Procedure UpdateToolBar;
    Procedure CreateGradient;
    Procedure MakePreview;
    Function  ScreenToBitmap: TFastDIB;
    Function  SelectionToBitmap: TFastDIB;
    Procedure CopyToClipboard;
    Procedure PasteFromClipboard;
    Function  ScreenToSCR(ImageIndex: Integer): String;
    Procedure CreateScreen(var Dst: TScrImage);
  end;

Const

  AntsArray: Array[0..7, 0..7] of Byte =
     ((0, 0, 0, 0, 1, 1, 1, 1),
      (0, 0, 0, 1, 1, 1, 1, 0),
      (0, 0, 1, 1, 1, 1, 0, 0),
      (0, 1, 1, 1, 1, 0, 0, 0),
      (1, 1, 1, 1, 0, 0, 0, 0),
      (1, 1, 1, 0, 0, 0, 0, 1),
      (1, 1, 0, 0, 0, 0, 1, 1),
      (1, 0, 0, 0, 0, 1, 1, 1));

var
  ScrPaintForm: TScrPaintForm;

Const

  CM_DENYSUBCLASSING = CM_BASE + 2000;
  EDITORNAME = 'BasinC Image Editor - ';

implementation

uses BASinMain, Filing, TextPaint, GridSetup, ImageImport, ScrPreview, Display;

{$R *.DFM}

Procedure TScrPaintForm.CreateScreen(var Dst: TScrImage);
Begin

  SetLength(Dst.SelMask, 0);
  SetLength(Dst.SelDetail, 0);
  SetLength(Dst.SelAttrDetail, 0);
  Dst.Undo := TStringlist.Create;
  Dst.Redo := TStringlist.Create;

End;

procedure TScrPaintForm.CopyScreen(var Src, Dst: TScrImage);
Var
  Idx, Len: Integer;
Begin

  CopyMemory(@Dst.InkDIB, @Src.InkDIB, 256 * 192);
  CopyMemory(@Dst.Attrs, @Src.Attrs, 32 * 24);

  Len := Length(Src.SelMask);
  If Len > 0 Then Begin
     SetLength(Dst.SelMask, Len);
     CopyMemory(@Dst.SelMask[0], @Src.SelMask[0], Len);
  End;

  Len := Length(Src.SelDetail);
  If Len > 0 Then Begin
     SetLength(Dst.SelDetail, Len);
     CopyMemory(@Dst.SelDetail[0], @Src.SelDetail[0], Len);
  End;

  Len := Length(Src.SelAttrDetail);
  If Len > 0 Then Begin
     SetLength(Dst.SelAttrDetail, Len);
     CopyMemory(@Dst.SelAttrDetail[0], @Src.SelAttrDetail[0], Len);
  End;

  Dst.Undo.Clear;
  Dst.Redo.Clear;
  Dst.Undo.AddStrings(Src.Undo);
  Dst.Redo.AddStrings(Src.Redo);
{

  If Dst.Undo <> nil Then Dst.Undo.Free;
  If Src.Undo <> nil Then Begin
     Dst.Undo := TStringlist.Create;
     Dst.Undo.AddStrings(Src.Undo);
  End;

  If Dst.Redo <> nil Then Dst.Redo.Free;
  If Src.Redo <> nil Then Begin
     Dst.Redo := TStringlist.Create;
     Dst.Redo.AddStrings(Src.Redo);
  End;
}
  Dst.ZoomLevel := Src.ZoomLevel;
  Dst.Update := Src.Update;
  Dst.SelActive := Src.SelActive;
  Dst.SelOrigin.x := Src.SelOrigin.x;
  Dst.SelOrigin.y := Src.SelOrigin.y;
  Dst.SelWidth := Src.SelWidth;
  Dst.SelHeight := Src.SelHeight;
  Dst.Changed := Src.Changed;
  Dst.HasFLASHAttrs := Src.HasFLASHAttrs;
  Dst.Filename := Src.Filename;

End;

Procedure TScrPaintForm.CreateColorTable;
Var
  Idx: Integer;
  Clr, ClrA, ClrB: TFColorA;
Begin

  For Idx := 0 To 15 Do Begin

     PaintColorTable[Idx] := DisplayPalette[Idx];

     ClrA := TColorToTFColorA(clHighlight);
     Clr.r := ((DisplayPalette[Idx].r * 3) + ClrA.r) Div 4;
     Clr.g := ((DisplayPalette[Idx].g * 3) + ClrA.g) Div 4;
     Clr.b := ((DisplayPalette[Idx].b * 3) + ClrA.b) Div 4;
     PaintColorTable[Idx+16] := Clr;

     ClrA := TColorToTFColorA(clBlack);
     Clr.r := (Clr.r + ClrA.r) Div 2;
     Clr.g := (Clr.g + ClrA.g) Div 2;
     Clr.b := (Clr.b + ClrA.b) Div 2;
     PaintColorTable[Idx+32] := Clr;

     ClrA := TColorToTFColorA(clWhite);
     Clr.r := (DisplayPalette[Idx].r + (ClrA.r * 3)) Div 4;
     Clr.g := (DisplayPalette[Idx].g + (ClrA.g * 3)) Div 4;
     Clr.b := (DisplayPalette[Idx].b + (ClrA.b * 3)) Div 4;
     PaintColorTable[Idx+48] := Clr;

  End;

  For Idx := 0 To 63 Do Begin

     ClrA := PaintColorTable[Idx];
     ClrB := TColorToTFColorA(clHighlight);

     If (Idx And 7) = 0 Then Begin

        Clr.r := (ClrA.r + (ClrB.r * 3)) Div 4;
        Clr.g := (ClrA.g + (ClrB.g * 3)) Div 4;
        Clr.b := (ClrA.b + (ClrB.b * 3)) Div 4;

     End Else If (Idx And 7) = 1 Then Begin

        ClrB.r := (255 + ClrB.r) Div 2;
        ClrB.g := (255 + ClrB.g) Div 2;
        ClrB.b := (255 + ClrB.b) Div 2;

        Clr.r := (ClrA.r + (ClrB.r)) Div 2;
        Clr.g := (ClrA.g + (ClrB.g)) Div 2;
        Clr.b := (ClrA.b + (ClrB.b)) Div 2;

     End Else Begin

        Clr.r := ((ClrA.r * 3) + (ClrB.r)) Div 4;
        Clr.g := ((ClrA.g * 3) + (ClrB.g)) Div 4;
        Clr.b := ((ClrA.b * 3) + (ClrB.b)) Div 4;

     End;

     PaintColorTable[Idx + 128] := Clr;

  End;

End;

Procedure TScrPaintForm.NewScreen;
Var
  X, Y: Integer;
Begin

  // Create new screen

  SetLength(Screens, NumScreens +1);
  Inc(NumScreens);
  New(Screens[NumScreens -1]);
  CurScreen := Screens[NumScreens -1];
  CurScreenIndex := NumScreens -1;

  With CurScreen^ Do Begin

     // Clear bitmap

     For X := 0 To 255 Do
        For Y := 0 To 191 Do Begin
           InkDIB[X, Y] := 0;
        End;

     SetLength(SelMask, 0);
     SetLength(SelDetail, 0);
     SetLength(SelAttrDetail, 0);

     // White paper, black ink, no BRIGHT, no FLASH.

     For X := 0 To 31 Do
        For Y := 0 To 23 Do
           Attrs[X, Y] := (7*8)+0;

     ZoomLevel := Min(Max(ScrollBox1.ClientWidth Div 256, 1), Max(ScrollBox1.ClientHeight Div 192, 1));
     Update := True;
     SelActive := False;
     SelWidth := 0;
     SelHeight := 0;
     Changed := False;
     HasFLASHAttrs := False;
     Undo := TStringlist.Create;
     Redo := TStringlist.Create;

     Inc(Untitled);
     TabControl1.Tabs.Add('Untitled'+IntToStr(Untitled));
     TabControl1.TabIndex := CurScreenIndex;
     TabControl1Change(nil);
     Caption := EDITORNAME + TabControl1.Tabs[CurScreenIndex] + '  [' + IntToStr(CurScreen^.ZoomLevel) + ':1]';
     Filename := '';

  End;

End;

Procedure TScrPaintForm.RemoveScreen(Index: Integer);
Var
  Idx: Integer;
Begin

  If Screens[Index].Changed Then
     If Not QuerySave(Index) Then Exit;

  // Remove a screen. Shift all subsequent screens down
  // the list

  TabControl1.Tabs.Delete(Index);

  Screens[Index]^.Undo.Free;
  Screens[Index]^.Redo.Free;
  For Idx := Index To NumScreens -2 Do
     Screens[Idx] := Screens[Idx +1];

  // and remove the last one (which is now a duplicate).

  SetLength(Screens, NumScreens -1);
  Dec(NumScreens);

  // Finally, Update the current screen.

  Dispose(CurScreen);
  If CurScreenIndex > Length(Screens) -1 Then Begin
     CurScreenIndex := Length(Screens) -1;
     CurScreen := Screens[CurScreenIndex];
     TabControl1.TabIndex := CurScreenIndex;
  End;
  If CurScreenIndex = Index Then Begin
     CurScreen := Screens[Index];
     TabControl1.TabIndex := Index;
  End;

End;

Function TScrPaintForm.QuerySave(Index: Integer): Boolean;
Var
  MsgVal: Integer;
Begin            
  MsgVal := MessageBox(Handle,
                       PChar('Save changes to image '#13#39+ TrimExtension(ExtractFilename(Screens[Index]^.Filename)) +#39'?'),
                       PChar('Save changes?'),
                       MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL or MB_SETFOREGROUND or MB_TOPMOST);
  If MsgVal = IDYES Then Begin
     Filename := Screens[Index]^.Filename;
     Result := SaveImageAs(Index);
  End Else If MsgVal = IDCANCEL then Begin
     Result := False;
  End Else Begin
     Result := True;
  End;
End;

Function TScrPaintForm.SaveImageAs(ImageIndex: Integer): Boolean;
Var
  Attr, Ink, Paper, Bright: Byte;
  TempPixels: Array[0..255, 0..191] of Byte;
  TempAttrs: Array[0..31, 0..23] of Byte;
  Ext, NewFilename: String;
  StartAddr, DataLen: Word;
  ScreenArray: Array of Byte;
  Idx, X, Y, Cx, Cy, Offset: Integer;
  BitVal, ByteVal: Byte;
  FTypes: TBASICFiles;
  Bmp: TFastDIB;
  Flashing: Boolean;
Begin

  Result := False;
  FTypes := [FTBsc, FTBin, FTBmp, FTGif, FTAll];
  If Filename = '' Then Filename := OpenFile(Handle, 'Save Screen Image as...', FTypes, '', True, False);
  If Filename = '' Then Exit;

  CopyMemory(@TempPixels[0, 0], @Screens[ImageIndex]^.InkDIB, 256 * 192);
  CopyMemory(@TempAttrs[0, 0], @Screens[ImageIndex]^.Attrs, 32 * 24);

  If Screens[ImageIndex]^.SelActive Then
  For X := 0 To Screens[ImageIndex]^.SelWidth -1 Do
     For Y := 0 To Screens[ImageIndex]^.SelHeight -1 Do
        If Screens[ImageIndex]^.SelMask[X + (Y * Screens[ImageIndex]^.SelWidth)] = 1 Then Begin
           Cx := X + Screens[ImageIndex]^.SelOrigin.X;
           Cy := Y + Screens[ImageIndex]^.SelOrigin.Y;
           If (Cx >= 0) and (Cx < 256) and (Cy >= 0) and (Cy < 192) Then
              TempAttrs[Cx Div 8, Cy Div 8] := Screens[ImageIndex]^.SelAttrDetail[X + (Y * Screens[ImageIndex]^.SelWidth)];
        End;

  Ext := Lowercase(ExtractFileExt(Filename));

  If (Ext = '.gif') or (Ext = '.bmp') Then Begin

     Bmp := TFastDIB.Create;
     Bmp.SetSize(256, 192, 8);
     For Idx := 0 to 15 do Bmp.Colors[Idx] := DisplayPalette[Idx];
     Bmp.UpdateColors;

     For Y := 0 To 191 Do Begin
        For X := 0 To 255 Do Begin
           If X Mod 8 = 0 Then Begin
              Attr := TempAttrs[X Div 8, Y Div 8];
              Bright := (Attr And 64) Shr 3;
              Ink := (Attr And 7) + Bright;
              Paper := ((Attr Shr 3) And 7) + Bright;
              If Attr and 128 <> 0 Then Flashing := True;
           End;
           If Screens[ImageIndex]^.SelActive Then Begin
              Offset := X - Screens[ImageIndex]^.SelOrigin.X + (Screens[ImageIndex]^.SelWidth * (Y - Screens[ImageIndex]^.SelOrigin.Y));
              If (Y >= Screens[ImageIndex]^.SelOrigin.Y) and (Y < Screens[ImageIndex]^.SelOrigin.Y + Screens[ImageIndex]^.SelHeight) And
                 (X >= Screens[ImageIndex]^.SelOrigin.X) and (X < Screens[ImageIndex]^.SelOrigin.X + Screens[ImageIndex]^.SelWidth) And
                 (Screens[ImageIndex]^.SelMask[Offset] = 1) Then Begin
                 If Screens[ImageIndex]^.SelDetail[Offset] = 1 Then
                    Bmp.Pixels8[191 - Y, X] := Ink
                 Else
                    Bmp.Pixels8[191 - Y, X] := Paper;
              End Else
                 If TempPixels[X, Y] = 1 Then
                    Bmp.Pixels8[191 - Y, X] := Ink
                 Else
                    Bmp.Pixels8[191 - Y, X] := Paper;
           End Else
              If TempPixels[X, Y] = 1 Then
                 Bmp.Pixels8[191 - Y, X] := Ink
              Else
                 Bmp.Pixels8[191 - Y, X] := Paper;
        End;
     End;

     If Ext = '.bmp' Then Begin

        Bmp.SaveToFile(Filename);
        Bmp.Free;
        Result := True;

     End Else Begin

        If Flashing Then Begin

           SaveGIFFile(Bmp, Filename, True, 32, True);

           For X := 0 To 31 Do
              For Y := 0 To 23 Do
                 If TempAttrs[X, Y] and 128 = 128 Then
                    TempAttrs[X, Y] := (TempAttrs[X, Y] and 192) + ((TempAttrs[X, Y] and 7) Shl 3) + ((TempAttrs[X, Y] and 56) Shr 3);

           For Y := 0 To 191 Do Begin
              For X := 0 To 255 Do Begin
                 If X Mod 8 = 0 Then Begin
                    Attr := TempAttrs[X Div 8, Y Div 8];
                    Bright := (Attr And 64) Shr 3;
                    Ink := (Attr And 7) + Bright;
                    Paper := ((Attr Shr 3) And 7) + Bright;
                 End;
                 If Screens[ImageIndex]^.SelActive Then Begin
                    Offset := X - Screens[ImageIndex]^.SelOrigin.X + (Screens[ImageIndex]^.SelWidth * (Y - Screens[ImageIndex]^.SelOrigin.Y));
                    If (Y >= Screens[ImageIndex]^.SelOrigin.Y) and (Y < Screens[ImageIndex]^.SelOrigin.Y + Screens[ImageIndex]^.SelHeight) And
                       (X >= Screens[ImageIndex]^.SelOrigin.X) and (X < Screens[ImageIndex]^.SelOrigin.X + Screens[ImageIndex]^.SelWidth) And
                       (Screens[ImageIndex]^.SelMask[Offset] = 1) Then Begin
                       If Screens[ImageIndex]^.SelDetail[Offset] = 1 Then
                          Bmp.Pixels8[191 - Y, X] := Ink
                       Else
                          Bmp.Pixels8[191 - Y, X] := Paper;
                    End Else
                       If TempPixels[X, Y] = 1 Then
                          Bmp.Pixels8[191 - Y, X] := Ink
                       Else
                          Bmp.Pixels8[191 - Y, X] := Paper;
                 End Else
                    If TempPixels[X, Y] = 1 Then
                       Bmp.Pixels8[191 - Y, X] := Ink
                    Else
                       Bmp.Pixels8[191 - Y, X] := Paper;
              End;
           End;

           AddGIFFrame(Bmp, Filename, 32, True);
           AddGIFFrame(nil, Filename, 0, False);

        End Else

           SaveGIFFile(Bmp, Filename, False, 0, False);

        Result := True;
        Bmp.Free;

     End;

  End Else Begin

     // Convert to .SCR/.BIN/.BSC

     SetLength(ScreenArray, 6912);

     CopyScreen(Screens[ImageIndex]^, BackImage);

     For X := 0 To Screens[ImageIndex]^.SelWidth -1 Do
        For Y := 0 To Screens[ImageIndex]^.SelHeight -1 Do Begin
           If Screens[ImageIndex]^.SelMask[x + (Screens[ImageIndex]^.SelWidth * y)] = 1 Then
              Screens[ImageIndex]^.InkDIB[X + Screens[ImageIndex]^.SelOrigin.X, Y+Screens[ImageIndex]^.SelOrigin.Y] := Screens[ImageIndex]^.SelDetail[x + (Screens[ImageIndex]^.SelWidth * y)];
        End;

     For X := 0 To Screens[ImageIndex]^.SelWidth -1 Do
        For Y := 0 To Screens[ImageIndex]^.SelHeight -1 Do
           If Screens[ImageIndex]^.SelMask[x + (Screens[ImageIndex]^.SelWidth * y)] = 1 Then
              Screens[ImageIndex]^.Attrs[(X + Screens[ImageIndex]^.SelOrigin.X) Div 8, (Y + Screens[ImageIndex]^.SelOrigin.Y) Div 8] := GetNewAttr(Screens[ImageIndex]^.Attrs[(X + Screens[ImageIndex]^.SelOrigin.X) Div 8, (Y + Screens[ImageIndex]^.SelOrigin.Y) Div 8], Screens[ImageIndex]^.SelAttrDetail[X + (Screens[ImageIndex]^.SelWidth * Y)], AttrFlags);

     For Idx := 0 to 6143 Do Begin
        X := (Idx and 31) * 8;
        Y := ScreenOffsets[Idx];
        ByteVal := 0;
        BitVal := 128;
        While BitVal > 0 Do Begin
           If Screens[ImageIndex]^.InkDIB[X, Y] = 1 Then
              ByteVal := ByteVal + BitVal;
           BitVal := BitVal Shr 1;
           Inc(X);
        End;
        ScreenArray[Idx] := ByteVal;
     End;

     For Idx := 6144 To 6911 Do Begin
        X := Idx - 6144;
        Y := X Div 32;
        X := X And 31;
        ScreenArray[Idx] := Screens[ImageIndex]^.Attrs[X, Y];
     End;

     CopyScreen(BackImage, Screens[ImageIndex]^);

     FileHeader := '                 ';
     NewFileName := ExtractFilename(Filename);
     NewFilename := Copy(NewFilename, 1, Length(NewFilename)-Length(Ext));
     CopyMemory(@FileHeader[2], @NewFilename[1], 10);

     DataLen := 6912;
     StartAddr := 16384;

     If Ext = '.bsc' Then Begin

        PutWord(@FileHeader[$C], DataLen);
        PutWord(@FileHeader[$E], StartAddr);
        SetLength(FileBody, DataLen+17);
        CopyMemory(@FileBody[1], @FileHeader[1], 17);
        CopyMemory(@FileBody[18], @ScreenArray[0], DataLen);
        Result := SaveFile;

     End Else Begin

        SetLength(FileBody, DataLen);
        CopyMemory(@FileBody[1], @ScreenArray[0], DataLen);
        Result := SaveFile;

     End;

  End;

  AddToRecent(Filename);

end;

Procedure TScrPaintForm.MemToScreen(MemImage: String; ScreenIndex: Integer);
Var
  Idx, X, Y: Integer;
  ByteVal, BitVal, Bit: Byte;
  Scr: pScrImage;
Begin

  // Address the correct Screen

  Scr := Screens[ScreenIndex];

  // Now decode the INK bytes

  For Idx := 1 To 6144 Do Begin

     X := ((Idx -1) and 31) * 8;
     Y := ScreenOffsets[Idx -1];

     ByteVal := Ord(MemImage[Idx]);
     BitVal := 128;

     For Bit := 0 To 7 Do Begin
        If ByteVal And BitVal <> 0 Then
           Scr^.InkDIB[X, Y] := 1
        Else
           Scr^.InkDIB[X, Y] := 0;
        BitVal := BitVal Shr 1;
        Inc(X);
     End;

  End;

  // And the ATTRs.

  For Idx := 6145 To 6912 Do Begin
     X := Idx - 6145;
     Y := X Div 32;
     X := X And 31;
     Scr^.Attrs[X, Y] := Ord(MemImage[Idx]);
  End;

  Scr^.Changed := True;

End;

Procedure TScrPaintForm.RenderScreen(Index: Integer; ShowAttrs: Boolean);
Var
  X, Y, Offset, X1, Y1, X2, Y2, x1a, x2a, y1a, y2a, CurZoom, AttrX, AttrY, Cx, Cy: Integer;
  Attr, Ink, Paper, Bright: Byte;
  TempPixels: Array[0..255, 0..191] of Byte;
  TempAttrs: Array[0..31, 0..23] of Byte;
Begin

  CurZoom := FastIMG1.Width Div 256;

  If (CurZoom <> Screens[Index]^.ZoomLevel) Or Screens[Index]^.Update Then Begin

     Screens[Index]^.Update := False;
     LockControl(ScrollBox1, True);

     X := (ScrollBox1.HorzScrollBar.Position + (ScrollBox1.ClientWidth Div 2)) Div CurZoom;
     X := (X * Screens[Index]^.ZoomLevel) - (ScrollBox1.ClientWidth Div 2);

     Y := (ScrollBox1.VertScrollBar.Position + (ScrollBox1.ClientHeight Div 2)) Div CurZoom;
     Y := (Y * Screens[Index]^.ZoomLevel) - (ScrollBox1.ClientHeight Div 2);
     CurZoom := Screens[Index]^.ZoomLevel;

     FastIMG1.SetBounds(0, 0, 256 * CurZoom, 192 * CurZoom);

     FastIMG1.Bmp.SetSize(Min(256 * CurZoom, (2+(ScrollBox1.ClientWidth Div CurZoom)) * CurZoom),
                          Min(192 * CurZoom, (2+(ScrollBox1.ClientHeight Div CurZoom)) * CurZoom), 8);

     FastIMG1.Bmp.Colors := @PaintColorTable;
     FastIMG1.Bmp.updateColors;

     ScrollBox1.DisableAutoRange;
     ScrollBox1.HorzScrollBar.Range := FastIMG1.Width;
     ScrollBox1.VertScrollBar.Range := FastIMG1.Height;
     If ScrollBox1.ClientWidth > FastIMG1.Width Then Begin
        ScrollBox1.HorzScrollBar.Position := 0;
        FastIMG1.Left := (ScrollBox1.ClientWidth Div 2) - (FastIMG1.Width Div 2);
     End Else Begin
        ScrollBox1.HorzScrollBar.Position := 0;
        FastIMG1.Left := 0;
        ScrollBox1.HorzScrollBar.Position := X;
     End;
     If ScrollBox1.ClientHeight > FastIMG1.Height Then Begin
        ScrollBox1.VertScrollBar.Position := 0;
        FastIMG1.Top := (ScrollBox1.ClientHeight Div 2) - (FastIMG1.Height Div 2);
     End Else Begin
        ScrollBox1.VertScrollBar.Position := 0;
        FastIMG1.Top := 0;
        ScrollBox1.VertScrollBar.Position := Y;
     End;
     LockControl(ScrollBox1, False);
  End;

  If ScrollBox1.ClientWidth > FastIMG1.Width Then
     FastIMG1.DIBLeft := 0
  Else
     FastIMG1.DIBLeft := (ScrollBox1.HorzScrollBar.Position Div CurZoom) * CurZoom;

  If ScrollBox1.ClientHeight > FastIMG1.Height Then
     FastIMG1.DIBTop := 0
  Else
     FastIMG1.DIBTop := (ScrollBox1.VertScrollBar.Position Div CurZoom) * CurZoom;

  X1 := FastIMG1.DIBLeft Div CurZoom;
  Y1 := FastIMG1.DIBTop Div CurZoom;
  X2 := X1 + (FastIMG1.Bmp.Width Div CurZoom);
  Y2 := Y1 + (FastIMG1.Bmp.AbsHeight Div CurZoom);
  X1a := 999;
  X2a := 0;
  Y1a := 999;
  Y2a := 0;

  Attr := TempAttrs[0, 0];
  Bright := (Attr And 64) Shr 3;
  Ink := (Attr And 7) + Bright;
  Paper := ((Attr Shr 3) And 7) + Bright;

  CopyMemory(@TempPixels[0, 0], @Screens[Index]^.InkDIB, 256 * 192);
  CopyMemory(@TempAttrs[0, 0], @Screens[Index]^.Attrs, 32 * 24);

  If Screens[Index]^.SelActive Then
     For X := 0 To Screens[Index]^.SelWidth -1 Do
        For Y := 0 To Screens[Index]^.SelHeight -1 Do
           If Screens[Index]^.SelMask[X + (Screens[Index]^.SelWidth * Y)] = 1 Then Begin
              Cx := X + Screens[Index]^.SelOrigin.X;
              Cy := Y + Screens[Index]^.SelOrigin.Y;
              If (Cx >= 0) and (Cx < 256) and (Cy >= 0) and (Cy < 192) Then
                 TempAttrs[Cx Div 8, Cy Div 8] := GetNewAttr(TempAttrs[Cx Div 8, Cy Div 8], Screens[Index]^.SelAttrDetail[X + (Screens[Index]^.SelWidth * Y)], AttrFlags);
           End;

  Screens[Index]^.HasFLASHAttrs := False;

  For Y := Max(0, Y1 -8) To Min(191, Y2) Do Begin
     For X := Max(0, X1 -8) To Min(255, X2) Do Begin
        If X Mod 8 = 0 Then Begin
           If ShowAttrs Then Begin
              Attr := TempAttrs[X Div 8, Y Div 8];
              Bright := (Attr And 64) Shr 3;
              If Attr And 128 > 0 Then Screens[Index]^.HasFLASHAttrs := True;
              If FlashState And ((Attr And 128) = 128) Then Begin
                 Paper := (Attr And 7) + Bright;
                 Ink := ((Attr Shr 3) And 7) + Bright;
              End Else Begin
                 Ink := (Attr And 7) + Bright;
                 Paper := ((Attr Shr 3) And 7) + Bright;
              End;
           End Else Begin
              Ink := CurINK;
              Paper := CurPAPER;
           End;
        End;
        If Screens[Index]^.SelActive Then Begin
           Offset := X - Screens[Index]^.SelOrigin.X + (Screens[Index]^.SelWidth * (Y - Screens[Index]^.SelOrigin.Y));
           If (Y >= Screens[Index]^.SelOrigin.Y) and (Y < Screens[Index]^.SelOrigin.Y + Screens[Index]^.SelHeight) And
              (X >= Screens[Index]^.SelOrigin.X) and (X < Screens[Index]^.SelOrigin.X + Screens[Index]^.SelWidth) And
              (Screens[Index]^.SelMask[Offset] = 1) Then Begin
              If ColourMode in [cmBoth, cmPixels] Then Begin
                 If SelectionMarquee Then Begin
                    If Screens[Index]^.SelDetail[Offset] = 1 Then
                       InternalDIB.Pixels8[191 - Y, X] := Ink + 16
                    Else
                       InternalDIB.Pixels8[191 - Y, X] := Paper + 16;
                 End Else Begin
                    If Screens[Index]^.SelDetail[Offset] = 1 Then
                       InternalDIB.Pixels8[191 - Y, X] := Ink
                    Else
                       InternalDIB.Pixels8[191 - Y, X] := Paper;
                 End;
              End Else Begin
                 If SelectionMarquee Then Begin
                    If TempPixels[X, Y] = 1 Then
                       InternalDIB.Pixels8[191 - Y, X] := Ink + 16
                    Else
                       InternalDIB.Pixels8[191 - Y, X] := Paper + 16;
                 End Else Begin
                    If TempPixels[X, Y] = 1 Then
                       InternalDIB.Pixels8[191 - Y, X] := Ink
                    Else
                       InternalDIB.Pixels8[191 - Y, X] := Paper;
                 End;
              End;
              If X < X1a Then X1a := X;
              If X > X2a Then X2a := X;
              If Y < Y1a Then Y1a := Y;
              If Y > Y2a Then Y2a := Y;
           End Else
              If TempPixels[X, Y] = 1 Then
                 InternalDIB.Pixels8[191 - Y, X] := Ink
              Else
                 InternalDIB.Pixels8[191 - Y, X] := Paper;
        End Else
           If TempPixels[X, Y] = 1 Then
              InternalDIB.Pixels8[191 - Y, X] := Ink
           Else
              InternalDIB.Pixels8[191 - Y, X] := Paper;
     End;
  End;

  InternalDIB.StretchRect(FastIMG1.Bmp.hDc, 0, 0, (X2-X1) * CurZoom, (Y2-Y1) * CurZoom, X1, Y1, (X2-X1), (Y2-Y1));

  If Screens[Index]^.SelActive And SelectionMarquee Then Begin

     Dec(X1a, X1);
     Dec(Y1a, Y1);
     Dec(X2a, X1);
     Dec(Y2a, Y1);

     X1a := Max(0, Min(FastIMG1.Bmp.Width -1, X1a * CurZoom));
     Y1a := Max(0, Min(FastIMG1.Bmp.AbsHeight -1, Y1a * CurZoom));
     X2a := Max(0, Min(FastIMG1.Bmp.Width -1, (X2a +1) * CurZoom));
     Y2a := Max(0, Min(FastIMG1.Bmp.AbsHeight -1, (Y2a +1) * CurZoom));

     For Y := FastIMG1.Bmp.AbsHeight - 1 - Y1a Downto FastIMG1.Bmp.AbsHeight - 1 - Y2a Do
        For X := X1a to X2a Do
           If FastIMG1.Bmp.Pixels8[Y, X] And 16 = 16 Then
              If ((Y > 0) and (FastIMG1.Bmp.Pixels8[Y-1, X] < 16)) or
                 ((X > 0) and (FastIMG1.Bmp.Pixels8[Y, X-1] < 16)) or
                 ((X < FastIMG1.Bmp.Width -1) and (FastIMG1.Bmp.Pixels8[Y, X+1] < 16)) or
                 ((Y < FastIMG1.Bmp.AbsHeight -1) and (FastIMG1.Bmp.Pixels8[Y+1, X] < 16)) Then
                 If AntsArray[(X+AntOffset) mod 8, Y mod 8] = 1 Then
                    FastIMG1.Bmp.Pixels8[Y, X] := (FastIMG1.Bmp.Pixels8[Y, X] And 15) + 48
                 Else
                    FastIMG1.Bmp.Pixels8[Y, X] := (FastIMG1.Bmp.Pixels8[Y, X] And 15) + 32;

  End;

  If ShowBrushShapes Then
     If MouseInImage Then Begin
        RenderBrushShape(ImgMouseX, ImgMouseY);
        If MouseIsDown Then
           If PaintMode = pmCloneBrush Then
              If CentrePen Then
                 RenderBrushShape((CloneImgX - X1 + (CurrentBrushWidth Div 2)) * CurZoom, (CloneImgY - Y1 + (CurrentBrushHeight Div 2)) * CurZoom)
              Else
                 RenderBrushShape((CloneImgX - X1) * CurZoom, (CloneImgY - Y1) * CurZoom);
     End;

  If PaintMode = pmCloneBrush Then Begin
     HLine(FastIMG1.Bmp, (((CloneX - X1) * CurZoom) - 8) + (CurZoom Div 2), ((CloneY - Y1) * CurZoom) + (CurZoom Div 2), (((CloneX - X1) * CurZoom) + 8) + (CurZoom Div 2));
     VLine(FastIMG1.Bmp, ((CloneX - X1) * CurZoom) + (CurZoom Div 2), (((CloneY - Y1) * CurZoom) - 8) + (CurZoom Div 2), (((CloneY - Y1) * CurZoom) + 8) + (CurZoom Div 2));
  End Else
     If PaintMode = pmText Then Begin
        HLine(FastIMG1.Bmp, (((TextForm.TextX - X1) * CurZoom) - 8) + (CurZoom Div 2), ((TextForm.TextY - Y1) * CurZoom) + (CurZoom Div 2), (((TextForm.TextX - X1) * CurZoom) + 8) + (CurZoom Div 2));
        VLine(FastIMG1.Bmp, ((TextForm.TextX - X1) * CurZoom) + (CurZoom Div 2), (((TextForm.TextY - Y1) * CurZoom) - 8) + (CurZoom Div 2), (((TextForm.TextY - Y1) * CurZoom) + 8) + (CurZoom Div 2));
     End;

  ToolButton11.Enabled := Screens[Index]^.ZoomLevel > 1;
  Rotate1.Enabled := Not Screens[Index]^.SelActive;
  Skew1.Enabled := Not Screens[Index]^.SelActive;

  If ShowGrid Then Begin

     Y := (((Y1 Div GridY) * GridY) - Y1) * CurZoom;
     While Y < FastIMG1.Bmp.AbsHeight Do Begin

        GHLine(FastIMG1.Bmp, 0, Y, FastIMG1.Bmp.Width -1);
        Inc(Y, GridY * CurZoom);

     End;

     X := (((X1 Div GridX) * GridX) - X1) * CurZoom;
     While X < FastIMG1.Bmp.Width Do Begin

        GVLine(FastIMG1.Bmp, X, 0, FastIMG1.Bmp.AbsHeight -1);
        Inc(X, GridX * CurZoom);

     End;

  End;

  FastIMG1.Repaint;

End;

procedure TScrPaintForm.FormResize(Sender: TObject);
begin

  If Closing Then Exit;

  If ScrollBox1.ClientWidth > FastIMG1.Width Then
     FastIMG1.Left := (ScrollBox1.ClientWidth Div 2) - (FastIMG1.Width Div 2);

  If ScrollBox1.ClientHeight > FastIMG1.Height Then
     FastIMG1.Top := (ScrollBox1.ClientHeight Div 2) - (FastIMG1.Height Div 2);

  If FastIMG1.Bmp.Width > 0 Then Begin

     FastIMG1.Bmp.SetSize(Min(FastIMG1.Width, (ScrollBox1.ClientWidth Div CurScreen^.ZoomLevel) * CurScreen^.ZoomLevel),
                          Min(FastIMG1.Height, (ScrollBox1.ClientHeight Div CurScreen^.ZoomLevel) * CurScreen^.ZoomLevel), 8);
     FastIMG1.Bmp.Colors := @PaintColorTable;
     FastIMG1.Bmp.updateColors;

     If CurScreen <> nil Then Begin
        CurScreen^.Update := True;
        RenderScreen(CurScreenIndex, ShowingAttrs);
     End;

  End;

  Coolbar2.DoubleBuffered := True;
  Coolbar2.Repaint;
  ToolBar1.Width := ToolButton12.Width + 8;
  Toolbar1.DoubleBuffered := True;
  Toolbar1.Repaint;

  StatusBar1.Panels[1].Width := ClientWidth - 300;

end;

procedure TScrPaintForm.FormShow(Sender: TObject);
Var
  Msg: TMessage; // Dummy message for later on.
begin

  If Length(Screens) = 0 Then NewScreen;

  AntOffset := 0;
  CanEnter := True;

  CreateColorTable;
  InternalDIB.SetSize(256, 192, 8);
  InternalDIB.Colors := @PaintColorTable;
  InternalDIB.UpdateColors;

  RenderScreen(CurScreenIndex, True);

  CurScreen := Screens[CurScreenIndex];
  CopyScreen(CurScreen^, UndoScreen);

  SetLength(PolygonLines, 0);
  PaintMode := pmNone;
  MouseIsDown := False;
  TimerToolButton := ToolButton1;
  ToolButton1Click(ToolButton1);

  FastIMG2.Bmp.SetSize(FastIMG2.Width, FastIMG2.Height, 32);
  FastIMG5.Bmp.SetSize(FastIMG5.Width, FastIMG5.Height, 32);
  RenderToolStyle(FastIMG2.Bmp, ptBrush);
  RenderToolStyle(FastIMG5.Bmp, ptFill);
  AttrFlags := [pwINK, pwPAPER, pwBRIGHT, pwFLASH];
  DrawPalette;
  Filled1.Checked := FillShapes;
  DrawOutline1.Checked := OutlineShapes;

  SpeedButton9.Down := ShowGrid;
  SpeedButton10.Down := SnapToGrid;
  SpeedButton11.Down := ShowingAttrs;
  SpeedButton12.Down := ShowBrushShapes;

  OnEnterMenuLoop(Msg);
  Timer1.Enabled := True;

end;

procedure TScrPaintForm.FormCreate(Sender: TObject);
begin

  Startup := True;
  Untitled := 0;
  Closing := False;
  CentrePen := True;
  SelectionMarquee := True;
  InternalDIB := TFastDIB.Create;
  GradientDIB := TFastDIB.Create;
  FastIMG1.DrawStyle := dsPositioned;
  PenHistory := TStringlist.Create;
  RecentPics := TStringlist.Create;
  NewGradientWidth := 8;
  NewGradientHeight := 8;
  NewGradientType := gtLinear;
  CreateScreen(ClipScr);
  CreateScreen(BackImage);
  CreateScreen(LastSelection);
  CreateScreen(UndoScreen);

  SetLength(CurrentFill, 64);
  CurrentFillWidth := 8;
  CurrentFillHeight := 8;
  FillChar(CurrentFill[0], 64, 1);

  SetLength(CurrentPattern, 64);
  CurrentPatternWidth := 8;
  CurrentPatternHeight := 8;
  FillChar(CurrentPattern[0], 64, 1);

  CurrentPenType := ptRound;
  CurrentPenDensity := 100;
  CurrentPenSizeX := 1;
  CurrentPenSizeY := 1;
  CurrentPenRotation := 0;
  MakePen(CurrentPenType, CurrentPenSizeX, CurrentPenSizeY, CurrentPenRotation, nil);
  SetLength(CurrentBrush, Length(PenArray));
  CopyMemory(@CurrentBrush[0], @PenArray[0], Length(PenArray));
  CurrentBrushWidth := CurrentPenSizeX;
  CurrentBrushHeight := CurrentPenSizeY;

  ShowingAttrs := True;
  FillShapes := True;
  OutlineShapes := True;
  SprayDensity := 5;
  CloneX := 0;
  CloneY := 0;

  CurINK := 0;
  CurPAPER := 7;
  CurBRIGHT := 0;
  CurFLASH := 0;

  OldScrollBoxWndMethod := ScrollBox1.WindowProc;
  ScrollBox1.WindowProc := NewScrollWndProc;

  ColourMode := cmBoth;
  ShowBrushShapes := True;
  FastIMG6.Bmp.SetSize(FastIMG6.Width, FastIMG6.Height, 32);

  GridX := 8; GridY := 8;

end;

procedure TScrPaintForm.NewScrollWndProc(var msg: TMessage);
begin
  If (Msg.Msg = WM_VSCROLL) or (Msg.Msg = WM_HSCROLL) Then
     RenderScreen(CurScreenIndex, ShowingAttrs);
  oldScrollBoxWndMethod(msg);
end;

procedure TScrPaintForm.FormDestroy(Sender: TObject);
begin

  Closing := True;
  InternalDIB.Free;
  GradientDIB.Free;
  PenHistory.Free;
  RecentPics.Free;

end;

procedure TScrPaintForm.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  Offset: Integer;
begin

  If SnapToGrid Then Begin
     X := Round(X/(GridX * CurScreen^.ZoomLevel)) * (GridX * CurScreen^.ZoomLevel);
     Y := Round(Y/(GridY * CurScreen^.ZoomLevel)) * (GridY * CurScreen^.ZoomLevel);
  End;

  MouseIsDown := True;
  MouseButton := Button;
  MouseX := X Div CurScreen^.ZoomLevel;
  MouseY := Y Div CurScreen^.ZoomLevel;
  RotateXAnchor := X;
  RotateYAnchor := Y;
  CentreX := MouseX;
  CentreY := MouseY;
  SprayX := MouseX;
  SprayY := MouseY;

  SetCaptureControl(FastIMG1);

  Offset := (MouseX - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (MouseY - CurScreen^.SelOrigin.y));

  If PaintMode = pmPolygon Then Begin
     If Length(PolygonLines) = 0 Then
        CreateBackImage;
  End Else
     If PaintMode in [pmRectangle, pmCircle, pmEllipse, pmLine, pmCurve1, pmPan, pmPanAttrs, pmRotate, pmRotateAttrs, pmSkew, pmSkewAttrs] Then
        CreateBackImage;

  Case PaintMode of
     pmFloodFill:
        Begin
           If (Not CurScreen^.SelActive) Or (CurScreen^.SelMask[Offset] = 1) Then Begin
              If MouseButton = mbLeft Then
                 FloodFill(MouseX, MouseY, 1)
              Else
                 FloodFill(MouseX, MouseY, 0);
              RenderScreen(CurScreenIndex, ShowingAttrs);
              MakeUndo('Flood Fill', True);
           End;
           CurScreen^.Changed := True;
        End;
     pmPolygon:
        Begin
           If Length(PolygonLines) = 0 Then Begin
              SetLength(PolygonLines, 2);
              PolygonLines[0] := Point(MouseX, MouseY);
           End Else Begin
              If (MouseX = PolygonLines[Length(PolygonLines) -1].X) and (MouseY = PolygonLines[Length(PolygonLines) -1].Y) Then Begin
                 RestoreBackImage;
                 If MouseButton = mbLeft Then
                    DrawPolygon(1, FillShapes)
                 Else
                    DrawPolygon(0, FillShapes);
                 SetLength(PolygonLines, 0);
                 RenderScreen(CurScreenIndex, ShowingAttrs);
                 MakeUndo('Polygon', True);
                 FastIMG1.Repaint;
                 Exit;
              End Else
                 SetLength(PolygonLines, Length(PolygonLines)+1);
           End;
           PolygonLines[Length(PolygonLines) -1] := Point(MouseX, MouseY);
           RestoreBackImage;
           If MouseButton = mbLeft Then
              DrawPolygon(1, FillShapes)
           Else
              DrawPolygon(0, FillShapes);
           RenderScreen(CurScreenIndex, ShowingAttrs);
           CurScreen^.Changed := True;
           FastIMG1.Repaint;
        End;
     pmMagicWand:
        Begin
           NullSelection := False;
           If Button = mbRight Then Begin
              ClearSelection;
              RenderScreen(CurScreenIndex, ShowingAttrs);
              MakeUndo('Null selection', True);
              NullSelection := True;
           End Else Begin
              If (ssShift in Shift) or (ssCtrl in Shift) Then Begin
                 CopyScreen(CurScreen^, LastSelection);
                 ClearSelection;
                 If (ssCtrl in Shift) and (ssShift in Shift) Then
                    SetSelectionMagic(MouseX, MouseY, mtSubtract)
                 Else If ssCtrl in Shift Then
                    SetSelectionMagic(MouseX, MouseY, mtXOR)
                 Else If ssShift in Shift Then
                    SetSelectionMagic(MouseX, MouseY, mtAdd);
                 RenderScreen(CurScreenIndex, ShowingAttrs);
                 FastIMG1.Repaint;
              End Else
                 If CurScreen^.SelActive And
                    (MouseY >= CurScreen^.SelOrigin.Y) and (MouseY < CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) And
                    (MouseX >= CurScreen^.SelOrigin.X) and (MouseX < CurScreen^.SelOrigin.X + CurScreen^.SelWidth) And
                    (CurScreen^.SelMask[Offset] = 1) Then Begin
                    LastPaintMode := pmMagicWand;
                    PaintMode := pmMove;
                 End Else Begin
                    ClearSelection;
                    SetSelectionMagic(MouseX, MouseY, mtNone);
                    RenderScreen(CurScreenIndex, ShowingAttrs);
                    FastIMG1.Repaint;
                 End;
           End;
        End;
     pmSelectRect:
        Begin
           NullSelection := False;
           If Button = mbRight Then Begin
              ClearSelection;
              RenderScreen(CurScreenIndex, ShowingAttrs);
              MakeUndo('Null Selection', True);
              NullSelection := True;
           End Else Begin
              If (ssShift in Shift) or (ssCtrl in Shift) Then Begin
                 CopyScreen(CurScreen^, LastSelection);
                 ClearSelection;
                 If (ssCtrl in Shift) and (ssShift in Shift) Then
                    SetSelectionRect(CentreX, CentreY, MouseX, MouseY, mtSubtract)
                 Else If ssCtrl in Shift Then
                    SetSelectionRect(CentreX, CentreY, MouseX, MouseY, mtXOR)
                 Else if ssShift in Shift Then
                    SetSelectionRect(CentreX, CentreY, MouseX, MouseY, mtAdd);
                 RenderScreen(CurScreenIndex, ShowingAttrs);
                 FastIMG1.Repaint;
              End Else
                 If CurScreen^.SelActive And
                    (MouseY >= CurScreen^.SelOrigin.Y) and (MouseY < CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) And
                    (MouseX >= CurScreen^.SelOrigin.X) and (MouseX < CurScreen^.SelOrigin.X + CurScreen^.SelWidth) And
                    (CurScreen^.SelMask[Offset] = 1) Then Begin
                    LastPaintMode := pmSelectRect;
                    PaintMode := pmMove;
                 End Else Begin
                    ClearSelection;
                    SetSelectionRect(CentreX, CentreY, MouseX, MouseY, mtNone);
                    RenderScreen(CurScreenIndex, ShowingAttrs);
                    FastIMG1.Repaint;
                 End;
              End;
        End;
     pmSelectFree:
        Begin
           NullSelection := False;
           If Button = mbRight Then Begin
              ClearSelection;
              RenderScreen(CurScreenIndex, ShowingAttrs);
              MakeUndo('Null Selection', True);
              NullSelection := True;
           End Else Begin
              If (ssShift in Shift) or (ssCtrl in Shift) Then Begin
                 CopyScreen(CurScreen^, LastSelection);
                 SetLength(PolygonLines, 1);
                 PolygonLines[0] := Point(MouseX, MouseY);
                 ClearSelection;
                 If (ssCtrl in Shift) and (ssShift in Shift) Then
                    SetSelectionFree(mtSubtract)
                 Else If ssCtrl in Shift Then
                    SetSelectionFree(mtXOR)
                 Else if ssShift in Shift Then
                    SetSelectionFree(mtAdd);
                 RenderScreen(CurScreenIndex, ShowingAttrs);
                 FastIMG1.Repaint;
              End Else
                 If CurScreen^.SelActive And
                    (MouseY >= CurScreen^.SelOrigin.Y) and (MouseY < CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) And
                    (MouseX >= CurScreen^.SelOrigin.X) and (MouseX < CurScreen^.SelOrigin.X + CurScreen^.SelWidth) And
                    (CurScreen^.SelMask[Offset] = 1) Then Begin
                    LastPaintMode := pmSelectFree;
                    PaintMode := pmMove;
                 End Else Begin
                    SetLength(PolygonLines, 1);
                    PolygonLines[0] := Point(MouseX, MouseY);
                    ClearSelection;
                    SetSelectionFree(mtNone);
                    RenderScreen(CurScreenIndex, ShowingAttrs);
                    FastIMG1.Repaint;
                 End;
              End;
        End;
     pmCloneBrush:
        Begin
           If Button = mbLeft Then Begin
              CloneOriginX := MouseX;
              CloneOriginY := MouseY;
           End Else Begin
              CloneX := MouseX;
              CloneY := MouseY;
           End;
           FastIMG1MouseMove(Sender, Shift, X, Y);
        End;
     pmTextPlace:
        Begin
           BeginText(MouseX, MouseY, X, Y);
        End;
     pmText:
        Begin
           TextForm.TextX := MouseX;
           TextForm.TextY := MouseY;
           TextForm.CreateText;
        End;
     Else
        FastIMG1MouseMove(Sender, Shift, X, Y);
  End;

end;

procedure TScrPaintForm.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  SetCaptureControl(nil);

  If SnapToGrid Then Begin
     X := Round(X/(GridX * CurScreen^.ZoomLevel)) * (GridX * CurScreen^.ZoomLevel);
     Y := Round(Y/(GridY * CurScreen^.ZoomLevel)) * (GridY * CurScreen^.ZoomLevel);
  End;

  X := X Div CurScreen^.ZoomLevel;
  Y := Y Div CurScreen^.ZoomLevel;
  MouseIsDown := False;

  Case PaintMode of

     pmPan:
        Begin
           MakeUndo('Pan', True);
        End;

     pmPanAttrs:
        Begin
           MakeUndo('Colour Pan', True);
        End;

     pmRotate:
        Begin
           MakeUndo('Rotate', True);
        End;

     pmRotateAttrs:
        Begin
           MakeUndo('Colour Rotation', True);
        End;

     pmSkew:
        Begin
           MakeUndo('Skew', True);
        End;

     pmSkewAttrs:
        Begin
           MakeUndo('Colour Skew', True);
        End;

     pmPaint:
        Begin
           MakeUndo('Attribute painting', True);
        End;

     pmFreehand:
        Begin
           MakeUndo('Freehand drawing', True);
        End;

     pmFreehandConnected:
        Begin
           MakeUndo('Freehand (Connected)', True);
        End;

     pmLine:
        Begin
           MakeUndo('Line drawing', True);
        End;

     pmFloodFill:
        Begin
           MakeUndo('Flood Fill', True);
        End;

     pmSprayCan:
        Begin
           MakeUndo('Spray Can', True);
        End;

     pmRectangle:
        Begin
           MakeUndo('Rectangle', True);
        End;

     pmCircle:
        Begin
           MakeUndo('Circle', True);
        End;

     pmEllipse:
        Begin
           MakeUndo('Ellipse', True);
        End;

     pmMagicWand:
        Begin
           MakeUndo('Magic Wand', True);
        End;

     pmCloneBrush:
        Begin
           MakeUndo('Clone Brush', True);
        End;

     pmCurve1:
        Begin
           CurveStartX := CentreX;
           CurveStartY := CentreY;
           CurveEndX := X;
           CurveEndY := Y;
           PaintMode := pmCurve2;
        End;

     pmCurve2:
        Begin
           PaintMode := pmCurve1;
           MakeUndo('Curves', True);
        End;

     pmMove:
        Begin
           PaintMode := LastPaintMode;
           MakeUndo('Move Selection', True);
        End;

     pmSelectRect:
        Begin
           MakeUndo('Rectangular Selection', True);
        End;

     pmSelectFree:
        Begin
           SetLength(PolygonLines, 0);
           MakeUndo('Freehand Selection', True);
        End;

  End;

  ScrollBox1.SetFocus;

end;

Procedure TScrPaintForm.UpdateStatusBar(X, Y: Integer);
Var
  Coords, Attr, ACoords, Desc, Extra: String;
  Rct: TRect;
  BVal: Byte;
Begin

  If Not Assigned(CurScreen) Then Exit;

  If MouseInImage And (X >= 0) And (Y >= 0) and (X < 256) And (Y < 192) Then Begin
     Coords := IntToStr(X) + ', ' + IntToStr(Y);
     ACoords := IntToStr(X Div 8) + ', ' + IntToStr(Y Div 8);
     StatusBar1.Tag := CurScreen^.Attrs[X Div 8, Y Div 8];
  End Else Begin
     Coords := '--, --';
     ACoords := '--, --';
     StatusBar1.Tag := 0;
  End;

  BVal := StatusBar1.Tag;
  Attr := '['+IntToStr(BVal) + '] (I: ' + IntToStr(BVal and 7) + ' P: ' + IntToStr((BVal Shr 3) And 7) + ' B: ' + IntToStr((BVal Shr 6) And 1) + ' F: ' + IntToStr((BVal Shr 7) And 1) + ')';

  Case PaintMode of

     pmNone:
        Begin
           Desc := 'No Tool';
           Extra := Coords;
        End;
     pmPaint:
        Begin
           Desc := 'Attribute Paint';
           Extra := ACoords + '  ' + Attr;
        End;
     pmFreehand:
        Begin
           Desc := 'Freehand';
           Extra := Coords;
        End;
     pmFreehandConnected:
        Begin
           Desc := 'Freehand line';
           Extra := Coords;
        End;
     pmLine:
        Begin
           Desc := 'Line';
           If MouseIsDown Then
              Extra := IntToStr(CentreX)+', ' + IntToStr(CentreY) + ' -> ' + Coords
           Else
              Extra := Coords;
        End;
     pmCurve1:
        Begin
           Desc := 'Curve';
           If MouseIsDown Then
              Extra := IntToStr(CentreX)+', ' + IntToStr(CentreY) + ' -> ' + Coords
           Else
              Extra := Coords;
        End;
     pmCurve2:
        Begin
           Desc := 'Curve';
           Extra := IntToStr(CurveStartX)+', ' + IntToStr(CurveStartY) + ' -> ' + IntToStr(CurveEndX)+', ' + IntToStr(CurveEndY) + '  ['+Coords+']';
        End;
     pmFloodFill:
        Begin
           Desc := 'Flood Fill';
           Extra := Coords;
        End;
     pmSpraycan:
        Begin
           Desc := 'Spray Can';
           Extra := Coords;
        End;
     pmRectangle:
        Begin
           Desc := 'Rectangle';
           If MouseIsDown Then Begin
              Extra := IntToStr(CentreX)+', ' + IntToStr(CentreY) + ' -> ' + IntToStr(X)+', ' + IntToStr(Y) + '  ('+IntToStr(Abs(CentreX - X))+'x'+IntToStr(Abs(CentreY - Y))+')';
           End Else
              Extra := Coords;
        End;
     pmCircle:
        Begin
           Desc := 'Circle';
           If MouseIsDown Then Begin
              Extra := IntToStr(CentreX)+', ' + IntToStr(CentreY) + ' Radius ' + IntToStr(Round(Sqrt((Abs(X - CentreX) * Abs(X - CentreX))+(Abs(Y - CentreY) * Abs(Y - CentreY)))));
           End Else
              Extra := Coords;
        End;
     pmEllipse:
        Begin
           Desc := 'Ellipse';
           If MouseIsDown Then Begin
              Extra := IntToStr(CentreX)+', ' + IntToStr(CentreY) + ' Radii ' + IntToStr(Abs(CentreX - X)) + 'x' + IntToStr(Abs(CentreY - Y));
           End Else
              Extra := Coords;
        End;
     pmPolygon:
        Begin
           Desc := 'Polygon';
           If MouseIsDown Then Begin
              If Length(PolygonLines) < 3 Then
                 Extra := Coords
              Else
                 Extra := Coords + '  (' + IntToStr(Length(PolygonLines)) + ' Sides)';
           End Else
              Extra := Coords;
        End;
     pmSelectRect:
        Begin
           Desc := 'Select Rectangle';
           If MouseIsDown Then
              Extra := IntToStr(CurScreen^.SelOrigin.X) + ', ' + IntToStr(CurScreen^.SelOrigin.Y) + ' -> ' + IntToStr(CurScreen^.SelOrigin.X + CurScreen^.SelWidth) + ', ' + IntToStr(CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) + '  (' + IntToStr(CurScreen^.SelWidth) + 'x' + IntToStr(CurScreen^.SelHeight) + ')'
           Else
              Extra := Coords;
        End;
     pmSelectFree:
        Begin
           Desc := 'Select Freehand';
           If MouseIsDown Then
              Extra := IntToStr(CurScreen^.SelOrigin.X) + ', ' + IntToStr(CurScreen^.SelOrigin.Y) + ' -> ' + IntToStr(CurScreen^.SelOrigin.X + CurScreen^.SelWidth) + ', ' + IntToStr(CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) + '  (' + IntToStr(CurScreen^.SelWidth) + 'x' + IntToStr(CurScreen^.SelHeight) + ')'
           Else
              Extra := Coords;
        End;
     pmMagicWand:
        Begin
           Desc := 'Magic Wand';
           Extra := Coords;
        End;
     pmMove:
        Begin
           Desc := 'Move Selection';
           Extra := Coords + '  ('+IntToStr(CurScreen^.SelWidth) + 'x' + IntToStr(CurScreen^.SelHeight) + ')';
        End;
     pmTextPlace:
        Begin
           Desc := 'Create Text';
           Extra := Coords;
        End;
     pmText:
        Begin
           Desc := 'Move Text';
           Extra := Coords;
        End;
     pmColourDropper:
        Begin
           Desc := 'Colour Picker';
           Extra := Coords + '  ' + Attr;
        End;
     pmCloneBrush:
        Begin
           Desc := 'Clone Brush';
           If MouseIsDown Then Begin
              Extra := IntToStr(CloneX + (X - CloneOriginX)) + ', ' + IntToStr(CloneY + (Y - CloneOriginY)) + ' -> ' + Coords
           End Else
              Extra := Coords;
        End;
     pmPan:
        Begin
           Desc := 'Pan/Scroll';
           If MouseIsDown Then
              Extra := Coords + '  (Offset: '+IntToStr(X - MouseX)+', '+IntToStr(Y - MouseY)+')'
           Else
              Extra := Coords + '  (LMB - Pan, RMB - Scroll)';
        End;
     pmPanAttrs:
        Begin
           Desc := 'Colour Pan/Scroll';
           If MouseIsDown Then
              Extra := Coords + '  (Offset: '+IntToStr(X - MouseX)+', '+IntToStr(Y - MouseY)+')'
           Else
              Extra := Coords + '  (LMB - Pan, RMB - Scroll)';
        End;
     pmRotate:
        Begin
           Desc := 'Rotate';
           If MouseIsDown Then Begin
              If Not CurScreen^.SelActive Then
                 Extra := IntToStr((RotateXAnchor - ImgMouseX) Mod 360) + '°' + '  Centre: '+IntToStr(CentreX) + ', '+ IntToStr(CentreY)
              Else
                 Extra := IntToStr((RotateXAnchor - ImgMouseX) Mod 360) + '°';
           End Else
              Extra := Coords;
        End;
     pmRotateAttrs:
        Begin
           Desc := 'Colour Rotate';
           If MouseIsDown Then Begin
              If Not CurScreen^.SelActive Then
                 Extra := IntToStr((RotateXAnchor - ImgMouseX) Mod 360) + '°' + '  Centre: '+IntToStr(CentreX) + ', '+ IntToStr(CentreY)
              Else
                 Extra := IntToStr((RotateXAnchor - ImgMouseX) Mod 360) + '°';
           End Else
              Extra := Coords;
        End;
     pmSkew:
        Begin
           Desc := 'Skew';
           If MouseisDown Then
              Extra := '  '+IntToStr(X - MouseX) + ' Pixels X, '+IntToStr(Y - MouseY)+' Pixels Y'
           Else
              Extra := Coords + '  (LMB - Pan, RMB - Scroll)';
        End;

     pmSkewAttrs:
        Begin
           Desc := 'Colour Skew';
           If MouseisDown Then
              Extra := '  '+IntToStr(X - MouseX) + ' Pixels X, '+IntToStr(Y - MouseY)+' Pixels Y'
           Else
              Extra := Coords + '  (LMB - Pan, RMB - Scroll)';
        End;

  End;

  StatusBar1.Panels[0].Text := ' '+Desc;
  StatusBar1.Panels[1].Text := ' '+Extra;
  Rct := Rect(StatusBar1.ClientWidth - 200, 4, StatusBar1.ClientWidth -4, StatusBar1.ClientHeight -2);
  StatusBar1DrawPanel(StatusBar1, StatusBar1.Panels[2], Rct);

End;

procedure TScrPaintForm.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  Xc, Yc, Yp, Xp, Idx, R, Offset: Integer;
  TempColourMode: TColourMode;
  Pixel, Attr, tINK, tPAPER, tBRIGHT, tFLASH: Byte;
begin

  If SnapToGrid Then Begin
     X := Round(X/(GridX * CurScreen^.ZoomLevel)) * (GridX * CurScreen^.ZoomLevel);
     Y := Round(Y/(GridY * CurScreen^.ZoomLevel)) * (GridY * CurScreen^.ZoomLevel);
  End;

  ImgMouseX := X;
  ImgMouseY := Y;
  X := X Div CurScreen^.ZoomLevel;
  Y := Y Div CurScreen^.ZoomLevel;

  If PaintMode in [pmSelectRect, pmSelectFree, pmMagicWand] Then Begin
     If (X >= CurScreen^.SelOrigin.X) and (Y >= CurScreen^.SelOrigin.Y) and
        (X < CurScreen^.SelOrigin.X + CurScreen^.SelWidth) and (Y < CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) and
        (CurScreen^.SelMask[(X - CurScreen^.SelOrigin.X) + ((Y - CurScreen^.SelOrigin.Y) * CurScreen^.SelWidth)] = 1) Then Begin
        Case Opt_MouseImage of
           miNone: FastIMG1.Cursor := crHandPoint;
           miWindows: FastIMG1.Cursor := crHandPoint;
           miCrosshair: FastIMG1.Cursor := crHandPoint;
        End;
     End Else
        Case Opt_MouseImage of
           miNone: FastIMG1.Cursor := crCross;
           miWindows: FastIMG1.Cursor := crArrow;
           miCrosshair: FastIMG1.Cursor := crCross;
        End;
  End Else
     Case Opt_MouseImage of
        miNone: FastIMG1.Cursor := crNone;
        miWindows: FastIMG1.Cursor := crArrow;
        miCrosshair: FastIMG1.Cursor := crCross;
     End;

  MouseInImage := True;
  UpdateStatusBar(X, Y);

  If MouseIsDown Then Begin

     Case PaintMode of

        pmPan:
           Begin
              RestoreBackImage;
              If ssRight in Shift Then
                 PanImage(X - MouseX, Y - MouseY, False, True)
              Else
                 PanImage(X - MouseX, Y - MouseY, False, False);
              X := MouseX;
              Y := MouseY;
              CurScreen^.Changed := True;
           End;

        pmPanAttrs:
           Begin
              RestoreBackImage;
              If ssRight in Shift Then
                 PanImage(X - MouseX, Y - MouseY, True, True)
              Else
                 PanImage(X - MouseX, Y - MouseY, True, False);
              X := MouseX;
              Y := MouseY;
           End;

        pmRotate:
           Begin
              RestoreBackImage;
              RotateImage(RotateXAnchor - ImgMouseX, CentreX, CentreY, False);
              X := MouseX;
              Y := MouseY;
              CurScreen^.Changed := True;
           End;

        pmRotateAttrs:
           Begin
              RestoreBackImage;
              RotateImage(RotateXAnchor - ImgMouseX, CentreX, CentreY, True);
              X := MouseX;
              Y := MouseY;
              CurScreen^.Changed := True;
           End;

        pmSkew:
           Begin
              RestoreBackImage;
              SkewImage(MouseX, MouseY, X - MouseX, Y - MouseY, ssRight in Shift, False);
              X := MouseX;
              Y := MouseY;
              CurScreen^.Changed := True;
           End;

        pmSkewAttrs:
           Begin
              RestoreBackImage;
              SkewImage(MouseX, MouseY, X - MouseX, Y - MouseY, ssRight in Shift, True);
              X := MouseX;
              Y := MouseY;
              CurScreen^.Changed := True;
           End;

        pmCloneBrush:
           Begin
              If ssRight in Shift Then Begin
                 CloneX := X;
                 CloneY := Y;
              End Else Begin
                 tINK := CurINK;
                 tPAPER := CurPAPER;
                 tBRIGHT := CurBRIGHT;
                 tFLASH := CurFLASH;
                 If CentrePen Then Begin
                    Xc := CloneX + (X - CloneOriginX) - (CurrentBrushWidth Div 2);
                    Yc := CloneY + (Y - CloneOriginY) - (CurrentBrushHeight Div 2);
                 End Else Begin
                    Xc := CloneX + (X - CloneOriginX);
                    Yc := CloneY + (Y - CloneOriginY);
                 End;
                 CloneImgX := Xc; CloneImgY := Yc;
                 For Yp := 0 To CurrentBrushHeight -1 Do
                    For Xp := 0 To CurrentBrushWidth -1 Do
                       If CurrentBrush[(Yp * CurrentBrushWidth) + Xp] = 1 Then
                          If (Xc + Xp >= 0) and (Xc + Xp <= 255) and (Yc + Yp >= 0) and (Yc + Yp <= 191) Then Begin
                             Pixel := CurScreen^.InkDIB[Xc + Xp, Yc + Yp];
                             Attr := CurScreen^.Attrs[(Xc + Xp) Div 8, (Yc + Yp) Div 8];
                             If CurScreen^.SelActive Then
                                If ((Xc + Xp) >= CurScreen^.SelOrigin.X) And ((Yc + Yp) >= CurScreen^.SelOrigin.Y) And ((Xc + Xp) < CurScreen^.SelOrigin.X + CurScreen^.SelWidth) And ((Yc + Yp) < CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) Then Begin
                                   Offset := ((Xc + Xp) - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * ((Yc + Yp) - CurScreen^.SelOrigin.Y));
                                   If CurScreen^.SelMask[Offset] = 1 Then Begin
                                   Pixel := CurScreen^.SelDetail[Offset];
                                   Attr := CurScreen^.SelAttrDetail[Offset];
                                End Else
                                   Attr := CurScreen^.Attrs[(Xc + Xp) Div 8, (Yc + Yp) Div 8];
                             End;
                             CurINK := Attr And 7;
                             CurPAPER := (Attr Shr 3) And 7;
                             CurBRIGHT := Attr Shr 6;
                             CurFLASH := Attr Shr 7;
                             If CentrePen Then
                                SetPixel(X - (CurrentBrushWidth Div 2) + Xp, Y - (CurrentBrushHeight Div 2) + Yp, Pixel, 0, 0, ptClone)
                             Else
                                SetPixel(X + Xp, Y + Yp, Pixel, 0, 0, ptClone);
                          End;
                 CurINK := tINK;
                 CurPAPER := tPAPER;
                 CurBRIGHT := tBRIGHT;
                 CurFLASH := tFLASH;
                 CurScreen^.Changed := True;
              End;
           End;

        pmPaint:
           Begin
              TempColourMode := ColourMode;
              ColourMode := cmAttrs;
              If ssRight in Shift Then
                 SetPixel(X, Y, 0, 0, 0, ptClone)
              Else
                 SetPixel(X, Y, 1, 0, 0, ptClone);
              ColourMode := TempColourMode;
              CurScreen^.Changed := True;
           End;

        pmColourDropper:
           Begin
              If CurScreen^.SelActive Then
                 Attr := CurScreen^.SelAttrDetail[(X - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y - CurScreen^.SelOrigin.Y))]
              Else
                 Attr := CurScreen^.Attrs[X Div 8, Y Div 8];
              CurINK := Attr and 7;
              CurPAPER := (Attr Shr 3) and 7;
              CurBRIGHT := (Attr and 64) Shr 6;
              CurFLASH := (Attr and 128) Shr 7;
              DrawPalette;
           End;

        pmFreehand:
           Begin
              If MouseButton = mbLeft Then
                 SetPixel(X, Y, 1, 0, 0, ptPen)
              Else
                 SetPixel(X, Y, 0, 0, 0, ptPen);
              CurScreen^.Changed := True;
           End;

        pmFreehandConnected:
           Begin
              If MouseButton = mbLeft Then
                 DrawLine(MouseX, MouseY, X, Y, 1)
              Else
                 DrawLine(MouseX, MouseY, X, Y, 0);
              CurScreen^.Changed := True;
           End;

        pmLine, pmCurve1:
           Begin
              RestoreBackImage;
              If MouseButton = mbLeft Then
                 DrawLine(CentreX, CentreY, X, Y, 1)
              Else
                 DrawLine(CentreX, CentreY, X, Y, 0);
              CurScreen^.Changed := True;
           End;

        pmCurve2:
           Begin
              RestoreBackImage;
              If MouseButton = mbLeft Then
                 DrawCurve(X, Y, 1)
              Else
                 DrawCurve(X, Y, 0);
              CurScreen^.Changed := True;
           End;

        pmSprayCan:
           Begin
              If Sender <> Timer1 Then Begin
                 SprayX := X;
                 SprayY := Y;
              End;
              For Idx := 0 To SprayDensity Do Begin
                 If CurrentBrushWidth > 1 Then Begin
                    Xc := Random(CurrentBrushWidth);
                    If CentrePen Then
                       x := Round(SprayX-(CurrentBrushWidth/2)+Xc)
                    Else
                       x := SprayX + Xc;
                 End Else Begin
                    Xc := 0;
                    x := SprayX;
                 End;
                 If CurrentBrushHeight > 1 Then Begin
                    Yc := Random(CurrentBrushHeight);
                    If CentrePen Then
                       y := Round(SprayY-(CurrentBrushHeight/2)+Yc)
                    Else
                       y := SprayY + Yc;
                 End Else Begin
                    Yc := 0;
                    y := SprayY;
                 End;
                 If MouseButton = mbLeft Then Begin
                    If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then
                       If CentrePen Then
                          SetPixel(SprayX - (CurrentBrushWidth Div 2), SprayY - (CurrentBrushHeight Div 2), 1, Xc, Yc, ptBrush)
                       Else
                          SetPixel(SprayX, SprayY, 1, Xc, Yc, ptBrush);
                 End Else Begin
                    If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then
                       If CentrePen Then
                          SetPixel(SprayX - (CurrentBrushWidth Div 2), SprayY - (CurrentBrushHeight Div 2), 0, Xc, Yc, ptBrush)
                       Else
                          SetPixel(SprayX, SprayY, 0, Xc, Yc, ptBrush);
                 End;
              End;
              CurScreen^.Changed := True;
           End;

        pmRectangle:
           Begin
              RestoreBackImage;
              If MouseButton = mbLeft Then
                 DrawRectangle(CentreX, CentreY, X, Y, 1, FillShapes)
              Else
                 DrawRectangle(CentreX, CentreY, X, Y, 0, FillShapes);
              CurScreen^.Changed := True;
           End;

        pmCircle:
           Begin
              RestoreBackImage;
              X := Abs(X - CentreX);
              Y := Abs(Y - CentreY);
              R := Round(Sqrt((X * X)+(Y * Y)));
              If MouseButton = mbLeft Then
                 DrawEllipse(CentreX, CentreY, R+CentreX, R+CentreY, 1, CircleBoxed, FillShapes)
              Else
                 DrawEllipse(CentreX, CentreY, R+CentreX, R+CentreY, 0, CircleBoxed, FillShapes);
              CurScreen^.Changed := True;
           End;

        pmEllipse:
           Begin
              RestoreBackImage;
              If MouseButton = mbLeft Then
                 DrawEllipse(CentreX, CentreY, X, Y, 1, EllipseBoxed, FillShapes)
              Else
                 DrawEllipse(CentreX, CentreY, X, Y, 0, EllipseBoxed, FillShapes);
              CurScreen^.Changed := True;
           End;

        pmPolygon:
           Begin
              RestoreBackImage;
              If Length(PolygonLines) > 0 Then Begin
                 PolygonLines[Length(PolygonLines) -1] := Point(X, Y);
                 If MouseButton = mbLeft Then
                    DrawPolygon(1, FillShapes)
                 Else
                    DrawPolygon(0, FillShapes);
              End;
              CurScreen^.Changed := True;
           End;

        pmSelectRect:
           Begin
              If Not NullSelection Then
                 If (ssCtrl in Shift) and (ssShift in Shift) Then Begin
                    ClearSelection;
                    SetSelectionRect(CentreX, CentreY, X, Y, mtSubtract);
                 End Else If ssShift in Shift Then Begin
                    ClearSelection;
                    SetSelectionRect(CentreX, CentreY, X, Y, mtAdd);
                 End Else
                    If ssCtrl in Shift Then Begin
                       ClearSelection;
                       SetSelectionRect(CentreX, CentreY, X, Y, mtXOR);
                    End Else Begin
                       ClearSelection;
                       SetSelectionRect(CentreX, CentreY, X, Y, mtNone);
                    End;
          End;

        pmMove:
           Begin
              CurScreen^.SelOrigin := Point(CurScreen^.SelOrigin.X + X - CentreX, CurScreen^.SelOrigin.Y + Y - CentreY);
              CentreX := X;
              CentreY := Y;
              CurScreen^.Changed := True;
           End;

        pmSelectFree:
           Begin
              If Not NullSelection Then Begin
                 SetLength(PolygonLines, Length(PolygonLines) +1);
                 PolygonLines[Length(PolygonLines) -1] := Point(X, Y);
                 If (ssCtrl in Shift) and (ssShift in Shift) Then Begin
                    ClearSelection;
                    SetSelectionFree(mtSubtract);
                 End Else If ssShift in Shift Then Begin
                    ClearSelection;
                    SetSelectionFree(mtAdd);
                 End Else
                    If ssCtrl in Shift Then Begin
                       ClearSelection;
                       SetSelectionFree(mtXOR);
                    End Else Begin
                       ClearSelection;
                       SetSelectionFree(mtNone);
                    End;
              End;
           End;

        pmText:
           Begin
              TextForm.TextX := X;
              TextForm.TextY := Y;
              TextForm.CreateText;
           End;

     End;

     RenderScreen(CurScreenIndex, ShowingAttrs);

  End Else

     If MouseInImage And ShowBrushShapes Then
        RenderScreen(CurScreenIndex, ShowingAttrs);

  MouseX := X;
  MouseY := Y;

end;

Procedure TScrPaintForm.CreateBackImage;
Begin

  CopyScreen(CurScreen^, BackImage);

End;

Procedure TScrPaintForm.RestoreBackImage;
Begin

  CopyScreen(BackImage, CurScreen^);

End;

Procedure TScrPaintForm.PatternFromChar(Destination: TPattern; Character: Char);
Var
  TempBmp: TFastDIB;
  Got: Boolean;
  X, Y: integer;
Begin

  TempBmp := TFastDIB.Create;
  TempBmp.SetSize(9, 9, 32);

  Got := False;

  Case Character of

     #32..#127:
        Begin // Normal char from CHARS sysvar
           SpecTextToDIB(TempBmp, 0, 2, Character, 2, 0, 1, False, True);
           Got := True;
        End;
     #128..#143:
        Begin // Block graphic
           SpecTextToDIB(TempBmp, 0, 2, Character, 2, 0, 1, False, True);
           Got := True;
        End;
     #144..#164:
        Begin // UDG
           SpecTextToDIB(TempBmp, 0, 2, Character, 2, 0, 1, False, True);
           Got := True;
        End;

  End;

  If Got Then Begin

     Case Destination of

        ptPen:
           Begin
              SetLength(CurrentPattern, 64);
              CurrentPatternWidth := 8;
              CurrentPatternHeight := 8;
              For Y := 0 to 7 Do
                 For X := 0 To 7 Do
                    CurrentPattern[(Y * 8) + X] := TempBmp.Pixels32[7-Y, X].r Shr 7;
           End;
        ptFill:
           Begin
              SetLength(CurrentFill, 64);
              CurrentFillWidth := 8;
              CurrentFillHeight := 8;
              For Y := 0 to 7 Do
                 For X := 0 To 7 Do
                    CurrentFill[(Y * 8) + X] := TempBmp.Pixels32[7-Y, X].r Shr 7;
           End;
        ptBrush:
           Begin
              SetLength(CurrentBrush, 64);
              CurrentBrushWidth := 8;
              CurrentBrushHeight := 8;
              For Y := 0 to 7 Do
                 For X := 0 To 7 Do
                    CurrentBrush[(Y * 8) + X] := TempBmp.Pixels32[7-Y, X].r Shr 7;
           End;

     End;

  End;

  TempBmp.Free;

End;

Procedure TScrPaintForm.HLine(Bmp: TFastDIB; X1, Y, X2: Integer);
Begin

  // Assumes Bpp = 8, X1 < X2, Y is constant.

  If (Y > FastIMG1.Height -1) or (Y < 0) or ((X1 < 0) and (X2 < 0)) or ((X1 > FastIMG1.Width -1) and (X2 > FastIMG1.Width -1)) Then
     Exit;

  X1 := Min(Bmp.Width-1, Max(X1, 0));
  X2 := Min(Bmp.Width-1, Max(X2, 0));
  Y  := Min(Bmp.Height-1, Max((Bmp.Height -1) - Y, 0));

  While X1 <= X2 Do Begin
     Bmp.Pixels8[Y, X1] := ((1 - ((Bmp.Pixels8[Y, X1] and 7) Div 4)) * 7) + (8 - (Bmp.Pixels8[Y, X1] And 8));
     Inc(X1);
  End;

End;

Procedure TScrPaintForm.VLine(Bmp: TFastDIB; X, Y1, Y2: Integer);
Begin

  // Assumes Bpp = 8, Y1 < Y2, X is constant.

  If (X > FastIMG1.Width -1) or (X < 0) or ((Y1 < 0) and (Y2 < 0)) or ((Y1 > FastIMG1.Height -1) and (Y2 > FastIMG1.Height -1)) Then
     Exit;

  X  := Min(Bmp.Width -1, Max(X, 0));
  Y1 := Min(Bmp.Height-1, Max((Bmp.Height -1) - Y1, 0));
  Y2 := Min(Bmp.Height-1, Max((Bmp.Height -1) - Y2, 0));

  While Y2 <= Y1 Do Begin
     Bmp.Pixels8[Y2, X] := ((1 - ((Bmp.Pixels8[Y2, X] and 7) Div 4)) * 7) + (8 - (Bmp.Pixels8[Y2, X] And 8));
     Inc(Y2);
  End;

End;

Procedure TScrPaintForm.GHLine(Bmp: TFastDIB; X1, Y, X2: Integer);
Begin

  // Assumes Bpp = 8, X1 < X2, Y is constant.

  If (Y > FastIMG1.Height -1) or (Y < 0) or ((X1 < 0) and (X2 < 0)) or ((X1 > FastIMG1.Width -1) and (X2 > FastIMG1.Width -1)) Then
     Exit;

  X1 := Min(Bmp.Width-1, Max(X1, 0));
  X2 := Min(Bmp.Width-1, Max(X2, 0));
  Y  := Min(Bmp.Height-1, Max((Bmp.Height -1) - Y, 0));

  While X1 <= X2 Do Begin
     If Bmp.Pixels8[Y, X1] < 128 Then
        Bmp.Pixels8[Y, X1] := Bmp.Pixels8[Y, X1] + 128;
     Inc(X1);
  End;

End;

Procedure TScrPaintForm.GVLine(Bmp: TFastDIB; X, Y1, Y2: Integer);
Begin

  // Assumes Bpp = 8, Y1 < Y2, X is constant.

  If (X > FastIMG1.Width -1) or (X < 0) or ((Y1 < 0) and (Y2 < 0)) or ((Y1 > FastIMG1.Height -1) and (Y2 > FastIMG1.Height -1)) Then
     Exit;

  X  := Min(Bmp.Width -1, Max(X, 0));
  Y1 := Min(Bmp.Height-1, Max((Bmp.Height -1) - Y1, 0));
  Y2 := Min(Bmp.Height-1, Max((Bmp.Height -1) - Y2, 0));

  While Y2 <= Y1 Do Begin
     If Bmp.Pixels8[Y2, X] < 128 Then
        Bmp.Pixels8[Y2, X] := Bmp.Pixels8[Y2, X] + 128;
     Inc(Y2);
  End;

End;

Procedure TScrPaintForm.RenderBrushShape(Xcc, Ycc: Integer);
Var
  BrushArray, pUp, pLeft, pRight, pDown: pByte;
  BrushWidth: Integer;
  BrushHeight: Integer;
  X, Y, Xc, Yc, Xp, Yp, TMod, BMod: Integer;
Const
  AttrBrush: Array [0..63] of Byte =
     (1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1);
Begin

  Xc := Xcc Div CurScreen^.ZoomLevel;
  Yc := Ycc Div CurScreen^.ZoomLevel;

  X := ((Xcc - FastIMG1.DIBLeft) Div CurScreen^.ZoomLevel) * CurScreen^.ZoomLevel;
  Y := ((Ycc - FastIMG1.DIBTop) Div CurScreen^.ZoomLevel) * CurScreen^.ZoomLevel;;

  BrushArray := nil;

  Case PaintMode of

     // Tools which use the current pen shape

     pmFreehand, pmFreehandConnected, pmSpraycan, pmCloneBrush,
     pmLine, pmCurve1, pmRectangle, pmCircle, pmEllipse, pmPolygon:
     Begin

        BrushArray := @CurrentBrush[0];
        BrushWidth := CurrentBrushWidth;
        BrushHeight := CurrentBrushHeight;
        If CentrePen Then Begin
           If CurrentBrushWidth > 1 Then X := X - ((CurrentBrushWidth Div 2) * CurScreen^.ZoomLevel);
           If CurrentBrushHeight > 1 Then Y := Y - ((CurrentBrushHeight Div 2) * CurScreen^.ZoomLevel);
        End;

     End;

     // Tools which affect attributes

     pmColourDropper, pmPaint:
     Begin

        BrushArray := @AttrBrush[0];
        BrushWidth := 8;
        BrushHeight := 8;

        X := (Xc Div 8) * (8 * CurScreen^.ZoomLevel) - FastIMG1.DIBLeft;
        Y := (Yc Div 8) * (8 * CurScreen^.ZoomLevel) - FastIMG1.DIBTop;

     End;

  End;

  If BrushArray <> nil Then Begin

     pUp := Pointer(DWord(BrushArray) - BrushWidth);
     pDown := Pointer(DWord(BrushArray) + BrushWidth);
     pLeft := Pointer(DWord(BrushArray) - 1);
     pRight := Pointer(DWord(BrushArray) + 1);

     For Yc := 0 To BrushHeight -1 Do Begin

        For Xc := 0 To BrushWidth -1 Do Begin

           If Byte(BrushArray^) = 1 Then Begin

              Xp := X + (Xc * CurScreen^.ZoomLevel);
              Yp := Y + (Yc * CurScreen^.ZoomLevel);
              TMod := 0;
              BMod := 0;

              If (Yc = 0)  Or (Byte(pUp^) = 0) Then Begin // Line at top of cell
                 HLine(FastIMG1.Bmp, Xp, Yp, Xp + CurScreen^.ZoomLevel -1);
                 TMod := 1;
              End;

              If (Yc = (BrushHeight -1)) Or (Byte(pDown^) = 0) Then Begin // Line at bottom of cell
                 HLine(FastIMG1.Bmp, Xp, Yp + CurScreen^.ZoomLevel -1, Xp + CurScreen^.ZoomLevel -1);
                 BMod := 1;
              End;

              If (Xc = 0) Or (Byte(pLeft^) = 0) Then // Line at left of cell
                 VLine(FastIMG1.Bmp, Xp, Yp + TMod, Yp + CurScreen^.ZoomLevel -1 -BMod);

              If (Xc = (BrushWidth -1)) or (Byte(pRight^) = 0) Then // Line at right of cell
                 VLine(FastIMG1.Bmp, Xp + CurScreen^.ZoomLevel -1, Yp + TMod,  Yp + CurScreen^.ZoomLevel -1 -BMod);

           End;

           Inc(pUp);
           Inc(pDown);
           Inc(pLeft);
           Inc(pRight);
           Inc(BrushArray);

        End;

     End;

  End;

End;

Procedure TScrPaintForm.SetPixel(X, Y, C, Xc, Yc: Integer; FillType: TPattern);
Var
  Cx, Cy, Cxa, Cya, MinX, MaxX, MinY, MaxY, Offset: Integer;
  TempAttrs, AttrState: Array[0..31, 0..23] of Byte;
Begin

  // Draws the current brush at the specified coordinates, with
  // the current pattern used on all set pixels.

  If Not (ColourMode in [cmBoth, cmPixels, cmAttrs]) Then Exit;

  If CurScreen^.SelActive Then

     If (X < CurScreen^.SelOrigin.X) or (X >= CurScreen^.SelOrigin.X + CurScreen^.SelWidth) or
        (Y < CurScreen^.SelOrigin.Y) or (Y >= CurScreen^.SelOrigin.Y + CurScreen^.SelHeight) or
        (CurScreen^.SelMask[(X - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y - CurScreen^.SelOrigin.Y))] = 0) Then

        Exit

     Else Begin

        FillMemory(@AttrState[0, 0], 32 * 24, 0);

        Case FillType Of

           ptClone:
              Begin
                 MinX := X; MaxX := X; MinY := Y; MaxY := Y;
                 Offset := (X - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y - CurScreen^.SelOrigin.Y));
                 If C = 1 Then Begin
                    If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                       If ColourMode in [cmBoth, cmPixels] Then
                          CurScreen^.SelDetail[Offset] := 1;
                       If ColourMode in [cmBoth, cmAttrs] Then Begin
                          TempAttrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                          AttrState[X Div 8, Y Div 8] := 1;
                       End;
                       CurScreen^.Changed := True;
                    End;
                 End Else
                    If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                       If ColourMode in [cmBoth, cmPixels] Then
                          CurScreen^.SelDetail[Offset] := 0;
                       If ColourMode in [cmBoth, cmAttrs] Then Begin
                          TempAttrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                          AttrState[X Div 8, Y Div 8] := 1;
                       End;
                       CurScreen^.Changed := True;
                    End;
              End;

           ptBrush:
              Begin
                 MinX := X+Xc; MaxX := X+Xc; MinY := Y+Yc; MaxY := Y+Yc;
                 Offset := (X + Xc - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y + Yc - CurScreen^.SelOrigin.Y));
                 If C = 1 Then Begin
                    If (x+xc >= 0) and (x+xc <= 255) and (y+yc >= 0) and (y+yc <= 191) Then
                       If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                          If ColourMode in [cmBoth, cmPixels] Then
                             CurScreen^.SelDetail[Offset] := CurrentPattern[(((Y+yc) Mod CurrentPatternHeight) * CurrentPatternWidth) + ((X+xc) Mod CurrentPatternWidth)];
                          If ColourMode in [cmBoth, cmAttrs] Then Begin
                             TempAttrs[(X +Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                             AttrState[(X +Xc) Div 8, (Y + Yc) Div 8] := 1;
                          End;
                          CurScreen^.Changed := True;
                       End;
                 End Else
                    If (x+xc >= 0) and (x+xc <= 255) and (y+yc >= 0) and (y+yc <= 191) Then
                       If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                          If ColourMode in [cmBoth, cmPixels] Then
                             CurScreen^.SelDetail[Offset] := 0;
                          If ColourMode in [cmBoth, cmAttrs] Then Begin
                             TempAttrs[(X +Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                             AttrState[(X +Xc) Div 8, (Y + Yc) Div 8] := 1;
                          End;
                          CurScreen^.Changed := True;
                       End;
              End;

           ptPen:
              Begin

                 MinX := X; MaxX := X; MinY := Y; MaxY := Y;
                 If CentrePen Then Begin
                    If CurrentBrushWidth > 1 Then Begin
                       x := x-(CurrentBrushWidth Div 2);
                       MinX := X; MaxX := X + CurrentBrushWidth;
                    End;
                    If CurrentBrushHeight > 1 Then Begin
                       y := y-(CurrentBrushHeight Div 2);
                       MinY := Y; MaxY := Y + CurrentBrushHeight;
                    End;
                 End Else Begin
                    MaxX := X + CurrentBrushWidth;
                    MaxY := Y + CurrentBrushHeight;
                 End;
                 Offset := (X + Xc - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y + Yc - CurScreen^.SelOrigin.Y));

                 If C = 1 Then Begin
                    For yc := 0 To CurrentBrushHeight -1 Do Begin
                       For xc := 0 To CurrentBrushWidth -1 Do Begin
                          If Offset > 0 Then Begin
                             If (x+xc >= CurScreen^.Selorigin.X) and (x+xc < CurScreen^.Selorigin.X + CurScreen^.SelWidth) and (y+yc >= CurScreen^.Selorigin.Y) and (y+yc < CurScreen^.Selorigin.Y + CurScreen^.SelHeight) Then
                                If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                                   If ColourMode in [cmBoth, cmPixels] Then
                                      CurScreen^.SelDetail[Offset] := CurrentPattern[(((Y+yc) Mod CurrentPatternHeight) * CurrentPatternWidth) + ((X+xc) Mod CurrentPatternWidth)];
                                   If ColourMode in [cmBoth, cmAttrs] Then Begin
                                      TempAttrs[(X +Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                                      AttrState[(X +Xc) Div 8, (Y + Yc) Div 8] := 1;
                                   End;
                                   CurScreen^.Changed := True;
                                End;
                          End;
                          Inc(Offset);
                       End;
                       Inc(Offset, CurScreen^.SelWidth - CurrentBrushWidth);
                    End;
                 End Else
                    For yc := 0 To CurrentBrushHeight -1 Do Begin
                       For xc := 0 To CurrentBrushWidth -1 Do Begin
                          If Offset > 0 Then Begin
                             If (x+xc >= CurScreen^.Selorigin.X) and (x+xc < CurScreen^.Selorigin.X + CurScreen^.SelWidth) and (y+yc >= CurScreen^.Selorigin.Y) and (y+yc < CurScreen^.Selorigin.Y + CurScreen^.SelHeight) Then
                                If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                                   If ColourMode in [cmBoth, cmPixels] Then
                                      CurScreen^.SelDetail[Offset] := 0;
                                   If ColourMode in [cmBoth, cmAttrs] Then Begin
                                      TempAttrs[(X +Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                                      AttrState[(X +Xc) Div 8, (Y + Yc) Div 8] := 1;
                                   End;
                                   CurScreen^.Changed := True;
                                End;
                          End;
                          Inc(Offset);
                       End;
                       Inc(Offset, CurScreen^.SelWidth - CurrentBrushWidth);
                    End;
              End;

           ptFill:
              Begin

                 MinX := X; MaxX := X; MinY := Y; MaxY := Y;
                 Offset := (X - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y - CurScreen^.SelOrigin.Y));

                 If FillStyle = ftGradient Then Begin

                    If (GradientType <> NewGradientType) or
                       (GradRectW <> NewGradientWidth) or
                       (GradRectH <> NewGradientHeight) Then Begin
                       CreateGradient;
                    End;

                    If ColourMode in [cmBoth, cmPixels] Then Begin
                       If C = 1 Then Begin
                          If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                             CurScreen^.SelDetail[Offset] := 1 - GradientDIB.Pixels8[(GradientDIB.AbsHeight - 1) - (Y - GradAnchorY), X - GradAnchorX];
                             CurScreen^.Changed := True;
                          End;
                       End Else
                          If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                             CurScreen^.SelDetail[Offset] := GradientDIB.Pixels8[(GradientDIB.AbsHeight - 1) - (Y - GradAnchorY), X - GradAnchorX];
                             CurScreen^.Changed := True;
                          End;
                    End;

                    If ColourMode in [cmBoth, cmAttrs] Then Begin
                       TempAttrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                       AttrState[X Div 8, Y Div 8] := 1;
                       CurScreen^.Changed := True;
                    End;

                 End Else Begin

                    If ColourMode in [cmBoth, cmPixels] Then Begin
                       If C = 1 Then Begin
                          If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                             CurScreen^.SelDetail[Offset] := CurrentFill[(((Y-CurScreen^.SelOrigin.Y) Mod CurrentFillHeight) * CurrentFillWidth) + ((X-CurScreen^.SelOrigin.X) Mod CurrentFillWidth)];
                             CurScreen^.Changed := True;
                          End;
                       End Else
                          If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                             CurScreen^.SelDetail[Offset] := 0;
                             CurScreen^.Changed := True;
                          End;
                    End;

                    If ColourMode in [cmBoth, cmAttrs] Then Begin
                       TempAttrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.SelAttrDetail[Offset], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                       AttrState[X Div 8, Y Div 8] := 1;
                       CurScreen^.Changed := True;
                    End;

                 End;

              End;

        End;

     MaxX := Min(MaxX + 7, CurScreen^.SelOrigin.x + CurScreen^.SelWidth -1);
     MaxY := Min(MaxY + 7, CurScreen^.SelOrigin.y + CurScreen^.SelHeight -1);

     For Cx := MinX - CurScreen^.SelOrigin.X To MaxX - CurScreen^.SelOrigin.X + 0 Do
        For Cy := MinY - CurSCreen^.SelOrigin.Y To MaxY - CurScreen^.SelOrigin.Y + 0 Do
           If CurScreen^.SelMask[Cx + (CurScreen^.SelWidth * Cy)] = 1 Then
              If AttrState[(Cx + CurScreen^.SelOrigin.X) Div 8, (Cy + CurScreen^.SelOrigin.Y) Div 8] <> 0 Then
                 CurScreen^.SelAttrDetail[Cx + (CurScreen^.SelWidth * Cy)] := TempAttrs[(Cx + CurScreen^.SelOrigin.X) Div 8, (Cy + CurScreen^.SelOrigin.Y) Div 8];

     Exit;

  End;

  If (X < 0) or (X > 255) or (Y < 0) or (Y > 191) Then Exit;

  Case FillType Of

     ptClone:
        Begin
           If C = 1 Then Begin
              If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                 If ColourMode in [cmBoth, cmPixels] Then
                    CurScreen^.InkDIB[X, Y] := 1;
                 If ColourMode in [cmBoth, cmAttrs] Then
                    CurScreen^.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.Attrs[X Div 8, Y Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                 CurScreen^.Changed := True;
              End;
          End Else
              If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                 If ColourMode in [cmBoth, cmPixels] Then
                    CurScreen^.InkDIB[X, Y] := 0;
                 If ColourMode in [cmBoth, cmAttrs] Then
                    CurScreen^.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.Attrs[X Div 8, Y Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                 CurScreen^.Changed := True;
              End;
        End;

     ptBrush:
        Begin

           If C = 1 Then Begin
              If (x+xc >= 0) and (x+xc <= 255) and (y+yc >= 0) and (y+yc <= 191) Then
                 If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                    If ColourMode in [cmBoth, cmPixels] Then
                       CurScreen^.InkDIB[X+xc, Y+yc] := CurrentPattern[(((Y+yc) Mod CurrentPatternHeight) * CurrentPatternWidth) + ((X+xc) Mod CurrentPatternWidth)];
                    If ColourMode in [cmBoth, cmAttrs] Then
                       CurScreen^.Attrs[(X + Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.Attrs[(X + Xc) Div 8, (Y + Yc) Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                    CurScreen^.Changed := True;
                 End;
           End Else
              If (x+xc >= 0) and (x+xc <= 255) and (y+yc >= 0) and (y+yc <= 191) Then
                 If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                    If ColourMode in [cmBoth, cmPixels] Then
                       CurScreen^.InkDIB[X+xc, Y+yc] := 0;
                    If ColourMode in [cmBoth, cmAttrs] Then
                       CurScreen^.Attrs[(X +Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.Attrs[(X + Xc) Div 8, (Y + Yc) Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                    CurScreen^.Changed := True;
                 End;
        End;

     ptPen:
        Begin

           If CentrePen Then Begin
              If CurrentBrushWidth > 1 Then
                 x := x-(CurrentBrushWidth Div 2);
              If CurrentBrushHeight > 1 Then
                 y := y-(CurrentBrushHeight Div 2);
           End;

           If C = 1 Then Begin
              For yc := 0 To CurrentBrushHeight -1 Do
                 For xc := 0 To CurrentBrushWidth -1 Do
                    If Random(100) < CurrentPenDensity Then
                       If (x+xc >= 0) and (x+xc <= 255) and (y+yc >= 0) and (y+yc <= 191) Then
                          If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                             If ColourMode in [cmBoth, cmPixels] Then
                                CurScreen^.InkDIB[X+xc, Y+yc] := CurrentPattern[(((Y+yc) Mod CurrentPatternHeight) * CurrentPatternWidth) + ((X+xc) Mod CurrentPatternWidth)];
                             If ColourMode in [cmBoth, cmAttrs] Then
                                CurScreen^.Attrs[(X +Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.Attrs[(X + Xc) Div 8, (Y + Yc) Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                             CurScreen^.Changed := True;
                          End;
           End Else
              For yc := 0 To CurrentBrushHeight -1 Do
                 For xc := 0 To CurrentBrushWidth -1 Do
                    If (x+xc >= 0) and (x+xc <= 255) and (y+yc >= 0) and (y+yc <= 191) Then
                       If Random(100) < CurrentPenDensity Then
                          If CurrentBrush[(yc*CurrentBrushWidth)+xc] = 1 Then Begin
                             If ColourMode in [cmBoth, cmPixels] Then
                                CurScreen^.InkDIB[X+xc, Y+yc] := 0;
                             If ColourMode in [cmBoth, cmAttrs] Then
                                CurScreen^.Attrs[(X +Xc) Div 8, (Y + Yc) Div 8] := GetNewAttr(CurScreen^.Attrs[(X + Xc) Div 8, (Y + Yc) Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                             CurScreen^.Changed := True;
                          End;
        End;

     ptFill:
        Begin

           If FillStyle = ftGradient Then Begin

              If (GradientType <> NewGradientType) or
                 (GradRectW <> NewGradientWidth) or
                 (GradRectH <> NewGradientHeight) Then Begin
                 CreateGradient;
              End;

              If C = 1 Then Begin
                 If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                    If ColourMode in [cmBoth, cmPixels] Then
                       CurScreen^.InkDIB[X, Y] := GradientDIB.Pixels8[(GradientDIB.AbsHeight - 1) - (Y - GradAnchorY), X - GradAnchorX];
                    If ColourMode in [cmBoth, cmAttrs] Then
                       CurScreen^.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.Attrs[X Div 8, Y Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                    CurScreen^.Changed := True;
                 End;
              End Else
                 If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                    If ColourMode in [cmBoth, cmPixels] Then
                       CurScreen^.InkDIB[X, Y] := 1 - GradientDIB.Pixels8[(GradientDIB.AbsHeight - 1) - (Y - GradAnchorY), X - GradAnchorX];
                    If ColourMode in [cmBoth, cmAttrs] Then
                       CurScreen^.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.Attrs[X Div 8, Y Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                    CurScreen^.Changed := True;
                 End;

           End Else Begin

              If C = 1 Then Begin
                 If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                    If ColourMode in [cmBoth, cmPixels] Then
                       CurScreen^.InkDIB[X, Y] := CurrentFill[((Y Mod CurrentFillHeight) * CurrentFillWidth) + (X Mod CurrentFillWidth)];
                    If ColourMode in [cmBoth, cmAttrs] Then
                       CurScreen^.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.Attrs[X Div 8, Y Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                    CurScreen^.Changed := True;
                 End;
              End Else
                 If (x >= 0) and (x <= 255) and (y >= 0) and (y <= 191) Then Begin
                    If ColourMode in [cmBoth, cmPixels] Then
                       CurScreen^.InkDIB[X, Y] := 0;
                    If ColourMode in [cmBoth, cmAttrs] Then
                       CurScreen^.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.Attrs[X Div 8, Y Div 8], CurINK, CurPAPER, CurBRIGHT, CurFlash, AttrFlags);
                    CurScreen^.Changed := True;
                 End;

           End;

        End;

  End;

End;

Procedure TScrPaintForm.DrawLine(X1, Y1, X2, Y2, Clr: Integer);
var
  d,ax,ay,sx,sy,dx,dy: Integer;
begin

  dx:=x2-x1; ax:=Abs(dx)shl 1; if dx<0 then sx:=-1 else sx:=1;
  dy:=y2-y1; ay:=Abs(dy)shl 1; if dy<0 then sy:=-1 else sy:=1;
  If (x1 >= 0) and (x1 < 256) and (y1 >= 0) and (y1 < 192) Then SetPixel(x1, y1, clr, 0, 0, ptPen);
  if ax>ay then begin
     d:=ay-(ax shr 1);
     while x1<>x2 do begin
        if d>-1 then begin
           Inc(y1,sy);
           Dec(d,ax);
        end;
        Inc(x1,sx);
        Inc(d,ay);
        If (x1 >= 0) and (x1 < 256) and (y1 >= 0) and (y1 < 192) Then SetPixel(x1, y1, clr, 0, 0, ptPen);
     end;
  end else begin
     d:=ax-(ay shr 1);
     while y1<>y2 do begin
        if d>=0 then begin
           Inc(x1,sx);
           Dec(d,ay);
        end;
        Inc(y1,sy);
        Inc(d,ax);
        If (x1 >= 0) and (x1 < 256) and (y1 >= 0) and (y1 < 192) Then SetPixel(x1, y1, clr, 0, 0, ptPen);
     end;
  end;
end;

Procedure TScrPaintForm.DrawCurve(X, Y, Clr: Integer);
Var
  SPX0, SPY0, SPX1, SPY1, SPX2, SPY2,
  SPCx, SPCy, SPBx, SPBy, SPAx, SPAy,
  LastX, LastY, CurX, CurY, Idx: Integer;
  Time: Extended;
Begin

  SPX0 := CurveStartX;
  SPY0 := CurveStartY;

  SPX1 := X;
  SPY1 := Y;

  SPX2 := CurveEndX;
  SPY2 := CurveEndY;

  SPCx := 3 * (SPX1 - SPX0);
  SPBx := -SPCx;
  SPAx := SPX2 - SPX0 - SPCx - SPBx;

  SPCy := 3 * (SPY1 - SPY0);
  SPBy := -SPCy;
  SPAy := SPY2 - SPY0 - SPCy - SPBy;

  Time := 0;
  LastX := CurveStartX;
  LastY := CurveStartY;

  For Idx := 1 To 50 Do Begin

     CurX := Round((SPAx*Time*Time*Time) + (SPBx*Time*Time) + (SPCx * Time) + SPX0);
     CurY := Round((SPAy*Time*Time*Time) + (SPBy*Time*Time) + (SPCy * Time) + SPY0);

     DrawLine(LastX, LastY, CurX, CurY, Clr);

     Time := Time + (1/(50-1));
     LastX := CurX;
     LastY := CurY;

  End;

End;

Procedure TScrPaintForm.FloodFill(X, Y, Clr: Integer);
Type
  TByteBox = Array[0..257, 0..193] Of Byte;
Var
  PBB: ^TByteBox;
  dir, b: byte;
  x1, y1, MinX, MinY, MaxX, MaxY: integer;
Label
  free, fill, nextpixel, previouspixel;
Begin

  Try

     GetMem(PBB,sizeof(PBB^));

     If Not CurScreen^.SelActive Then Begin

        for y1 := 0 to 191 do
           for x1 := 0 to 255 do
              if CurScreen^.InkDIB[x1,y1] <> CurScreen^.InkDIB[X, Y] then
                 PBB^[x1+1,y1+1] := $40 else PBB^[x1+1,y1+1] := 0;

        y1 := 0; for x1 := 0 to 257 do PBB^[x1,y1] := $40;
        y1 := 193; for x1 := 0 to 257 do PBB^[x1,y1] := $40;
        x1 := 0; for y1 := 0 to 193 do PBB^[x1,y1] := $40;
        x1 := 257; for y1 := 0 to 193 do PBB^[x1,y1] := $40;

     End Else Begin

        If ((X - CurScreen^.SelOrigin.X) < 0) or ((X - CurScreen^.SelOrigin.X) >= CurScreen^.SelWidth) or
           ((Y - CurScreen^.SelOrigin.Y) < 0) or ((Y - CurScreen^.SelOrigin.Y) >= CurScreen^.SelHeight) Then
           Goto free;

        For y1 := 0 To CurScreen^.SelHeight -1 Do
           For x1 := 0 To CurScreen^.SelWidth -1 Do
              if CurScreen^.SelDetail[x1 + (CurScreen^.SelWidth * y1)] <> CurScreen^.SelDetail[(X - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y - CurScreen^.SelOrigin.Y))] Then
                 PBB^[x1+1,y1+1] := $40 else PBB^[x1+1,y1+1] := 0;

        y1 := 0; for x1 := 0 to CurScreen^.SelWidth +1 do PBB^[x1,y1] := $40;
        y1 := CurScreen^.SelHeight +1; for x1 := 0 to CurScreen^.SelWidth +1 do PBB^[x1,y1] := $40;
        x1 := 0; for y1 := 0 to CurScreen^.SelHeight +1 do PBB^[x1,y1] := $40;
        x1 := CurScreen^.SelWidth +1; for y1 := 0 to CurScreen^.SelHeight +1 do PBB^[x1,y1] := $40;

        Dec(X, CurScreen^.SelOrigin.X);
        Dec(Y, CurScreen^.SelOrigin.Y);

     End;

     Inc(X);
     Inc(Y);
     if (PBB^[x,y] and $40) <> 0 then
        goto free;

     PBB[x,y] := $88;
     dir := 0;

  nextpixel:

     case dir of
        0: inc(x);
        1: dec(x);
        2: dec(y);
        3: inc(y);
     end;

     if (PBB^[x,y] and $c0) <> 0 then
        goto previouspixel;

     PBB^[x,y] := $80 or dir;

     if dir <> 1 then
        dir := 0;

     goto nextpixel;

  previouspixel:

     case dir of
        0: dec(x);
        1: inc(x);
        2: inc(y);
        3: dec(y);
     end;

     b := PBB^[x,y];
     inc(dir);
     if (b and $f) = (dir xor 1) then
        inc(dir);
     if dir > 3 then begin
        dir := b and $f;
        if dir >= 8 then
           goto fill
        else
           goto previouspixel;
     end else
        goto nextpixel;

  fill:

     If PaintMode = pmMagicWand Then Begin

        MinX := 9999;
        MaxX := 0;
        MinY := 9999;
        MaxY := 0;

        For y := 1 To 192 Do
           For x := 1 To 256 Do
              if (PBB^[x, y] and $80) <> 0 Then Begin
                 If MinX > x Then MinX := x;
                 If MaxX < x Then MaxX := x;
                 If MinY > Y Then MinY := Y;
                 If MaxY < Y Then MaxY := Y;
              End;

        If FillStyle = ftGradient Then Begin
           NewGradientWidth := (MaxX - MinX)+1;
           NewGradientHeight := (MaxY - MinY)+1;
           GradAnchorX := MinX -1;
           GradAnchorY := MinY -1;
        End;

        CurScreen^.SelActive := True;
        CurScreen^.SelWidth := (MaxX - MinX) +1;
        CurScreen^.SelHeight := (MaxY - MinY) +1;

        SetLength(CurScreen^.SelMask, CurScreen^.SelWidth * CurScreen^.SelHeight);
        SetLength(CurScreen^.SelDetail, CurScreen^.SelWidth * CurScreen^.SelHeight);
        SetLength(CurScreen^.SelAttrDetail, CurScreen^.SelWidth * CurScreen^.SelHeight);

        For y := MinY to MaxY Do
           for x := MinX To MaxX Do
              if (PBB^[x,y] and $80) <> 0 Then Begin
                 CurScreen^.SelMask[(x - MinX) + ((Y  - MinY) * CurScreen^.SelWidth)] := 1;
                 CurScreen^.SelDetail[(x - MinX) + ((Y - MinY) * CurScreen^.SelWidth)] := CurScreen^.InkDIB[x -1, y -1];
              End Else
                 CurScreen^.SelMask[(x - MinX) + ((Y - MinY) * CurScreen^.SelWidth)] := 0;

        CurScreen^.SelOrigin.x := MinX - 1;
        CurScreen^.SelOrigin.y := MinY - 1;

     End Else Begin

        MaxX := 0;
        MaxY := 0;
        MinY := 99999;
        MinX := 99999;
        For X := 1 To 256 Do
           For Y := 1 To 192 Do
              If (PBB^[X, Y] And $80) <> 0 Then Begin
                 If X < MinX Then MinX := X;
                 If X > MaxX Then MaxX := X;
                 If Y < MinY Then MinY := Y;
                 If Y > MaxY Then MaxY := Y;
              End;

        If FillStyle = ftGradient Then Begin
           NewGradientWidth := (MaxX - MinX)+1;
           NewGradientHeight := (MaxY - MinY)+1;
           GradAnchorX := MinX -1 + CurScreen^.SelOrigin.x;
           GradAnchorY := MinY -1 + CurScreen^.SelOrigin.y;
        End;

        If Not CurScreen^.SelActive Then Begin
           for y := 1 to 192 do
              for x := 1 to 256 do
                 if (PBB^[x,y] and $80) <> 0 then
                    SetPixel(x-1, y-1, Clr, 0, 0, ptFill);
        End Else
           for y := 1 to CurScreen^.SelHeight do
              for x := 1 to CurScreen^.SelWidth do
                 if (PBB^[x,y] and $80) <> 0 then
                    SetPixel(x-1+CurScreen^.SelOrigin.X, y-1+CurScreen^.SelOrigin.Y, Clr, 0, 0, ptFill);

     End;

  free:

  finally
     FreeMem(PBB);
  end;

end;

procedure TScrPaintForm.Timer1Timer(Sender: TObject);
Var
  Idx: Integer;
  CPos: TPoint;
  Rct: TRect;
  Spd: TspBtn;
  Render: Boolean;
begin

  If Startup Then Begin
     CurScreen^.ZoomLevel := Min(Max(ScrollBox1.ClientWidth Div 256, 1), Max(ScrollBox1.ClientHeight Div 192, 1));
     RenderScreen(CurScreenIndex, ShowingAttrs);
     Startup := False;
  End;

  If MouseIsDown Then Begin
     Case PaintMode of

        pmSprayCan:
           Begin
              FastIMG1MouseMove(Sender, [], ImgMouseX, ImgMouseY);
           End;

     End;

     If CurrentPenDensity <> 100 Then
        FastIMG1MouseMove(Sender, [], ImgMouseX, ImgMouseY);
  End;

  If CurScreen <> nil Then Begin
     Render := False;
     Inc(ElapsedTicks);
     If ElapsedTicks >= 4 Then Begin
        FlashState := Not FlashState;
        Render := CurScreen^.HasFLASHAttrs;
        If CurFLASH <> 0 Then RenderFlashState;
        Dec(ElapsedTicks, 4);
     End;
     AntOffset := (AntOffset + 1) Mod 8;
     If CurScreen^.SelActive or Render Then Begin
        RenderScreen(CurScreenIndex, ShowingAttrs);
        FastIMG1.Repaint;
     End;
  End;

  For Idx := 0 To ComponentCount -1 Do Begin
     If Components[Idx] is TSpeedButton Then Begin
        Spd := TSpBtn(Components[Idx]);
        If Spd.MouseInControl Then Begin
           GetCursorPos(CPos);
           Rct := Spd.BoundsRect;
           MapWindowPoints(Spd.Parent.Handle, 0, Rct, 2);
           If Not PtInRect(Rct, CPos) then
              Spd.Perform(CM_MOUSELEAVE, 0, 0);
        End;
     End;
  End;

  If TimerToolButton <> nil Then
     If Not TimerToolButton.Down Then
        TimerToolButton.Down := True;

  UpdateToolBar;

  GetCursorPos(CPos);
  CPos := FastIMG1.ScreenToClient(CPos);
  UpdateStatusBar(CPos.X Div CurScreen^.ZoomLevel, CPos.Y Div CurScreen^.ZoomLevel);

end;

Procedure TScrPaintForm.DrawRectangle(X1, Y1, X2, Y2, Clr: Integer; Filled: Boolean);
Var
  X, Y: Integer;
Begin

  // Sanitise the coordinates

  If X1 > X2 Then Begin
     X := X1; X1 := X2; X2 := X;
  End;

  If Y1 > Y2 Then Begin
     Y := Y1; Y1 := Y2; Y2 := Y;
  End;

  If SnapToGrid Then Begin

     If (GridX > 2) And (X2 - X1 > 1) Then Dec(X2, 1);
     If (GridY > 2) And (Y2 - Y1 > 1) Then Dec(Y2, 1);

  End;

  // If Chosen, Fill the the rectangle with the current pattern

  If Filled Then Begin
     If FillStyle = ftGradient Then Begin
        NewGradientWidth := (X2 - X1) +1;
        NewGradientHeight := (Y2 - Y1) +1;
        GradAnchorX := X1;
        GradAnchorY := Y1;
     End;
     For Y := Y1 To Y2 Do
        For X := X1 To X2 Do
           SetPixel(X, Y, Clr, 0, 0, ptFill);
  End;

  // Now draw the outline in the current brush/pattern

  If OutlineShapes Then Begin

     For X := X1 To X2 Do Begin
        SetPixel(X, Y1, Clr, 0, 0, ptPen);
        SetPixel(X, Y2, Clr, 0, 0, ptPen);
     End;

     For Y := Y1 To Y2 Do Begin
        SetPixel(X1, Y, Clr, 0, 0, ptPen);
        SetPixel(X2, Y, Clr, 0, 0, ptPen);
     End;

  End;

End;

Procedure TScrPaintForm.DrawEllipse(Cx, Cy, Rx, Ry, Clr: Integer; Boxed, Filled: Boolean);
var
  Rx2, Ry2, twoRx2, twoRy2, p,x,y,Xn,Yn, px,py: Integer;
  SnapX, SnapY: Boolean;
begin

  If Not Boxed Then Begin
     Rx := Rx - Cx;
     Ry := Ry - Cy;
  End Else Begin
     If Cx > Rx Then Begin X := Cx; Cx := Rx; Rx := X; End;
     If Cy > Ry Then Begin Y := Cy; Cy := Ry; Ry := Y; End;
     X := Rx - Cx;
     Y := Ry - Cy;
     Cx := Round(Cx + (X / 2));
     Cy := Round(Cy + (Y / 2));
     Rx := Round(X / 2);
     Ry := Round(Y / 2);
  End;

  Rx := Abs(Rx);
  Ry := Abs(Ry);
  SnapX := False;
  SnapY := False;

  If SnapToGrid Then Begin

     If (GridX > 2) And (Rx > 1) Then Begin SnapX := True; Dec(Rx, 1); End;
     If (GridY > 2) And (Ry > 1) Then Begin SnapY := True; Dec(Ry, 1); End;

  End;

  If Filled Then Begin

     If FillStyle = ftGradient Then Begin
        NewGradientWidth := (Rx * 2) +1 + (2*Byte(SnapX));
        NewGradientHeight := (Ry * 2) +1 + (2*Byte(SnapY));
        GradAnchorX := (Cx - Rx) - Byte(SnapX);
        GradAnchorY := (Cy - Ry) - Byte(SnapY);
     End;

     Rx2:=Rx*Rx;    Ry2:=Ry*Ry;
     twoRx2:=2*Rx2; twoRy2:=2*Ry2;
     x:=0;          y:=Ry;
     px:=0;
     py:=twoRx2*y;

     p:=Ry2-(Rx2*Ry)+(Rx2 div 4);
     while px<py do begin
        Inc(x);
        Inc(px,twoRy2);
        if p<0 then
           Inc(p,Ry2+px)
        else begin
           Dec(y);
           Dec(py,twoRx2);
           Inc(p,Ry2+px-py);
        end;
        For Xn := Cx-X-Byte(SnapX) To Cx+X Do Begin
           SetPixel(Xn, Cy+Y, Clr, 0, 0, ptFill);
           SetPixel(Xn, Cy-Y-Byte(SnapY), Clr, 0, 0, ptFill);
        End;
     end;
{$R-}
     p:=Round(Ry2*(x+0.5)*(x+0.5)+Rx2*(y-1)*(y-1)-Rx2*Ry2);
{$R+}
     while y>0 do begin
        Dec(y);
        Dec(py,twoRx2);
        if p>0 then
           Inc(p,Rx2-py)
        else begin
           Inc(x);
           Inc(px,twoRy2);
           Inc(p,Rx2-py+px);
        end;
        For Xn := Cx-X-Byte(SnapX) To Cx+X Do Begin
           SetPixel(Xn, Cy+Y, Clr, 0, 0, ptFill);
           SetPixel(Xn, Cy-Y-Byte(SnapY), Clr, 0, 0, ptFill);
        End;
     end;

  End;

  If OutlineShapes Then Begin

     Rx2:=Rx*Rx;    Ry2:=Ry*Ry;
     twoRx2:=2*Rx2; twoRy2:=2*Ry2;
     x:=0;          y:=Ry;
     px:=0;         py:=twoRx2*y;

     SetPixel(Cx+X, Cy+Y, Clr, 0, 0, ptPen);
     SetPixel(Cx-X-Byte(SnapX), Cy+Y, Clr, 0, 0, ptPen);
     SetPixel(Cx+X, Cy-Y-Byte(SnapY), Clr, 0, 0, ptPen);
     SetPixel(Cx-X-Byte(SnapX), Cy-Y-Byte(SnapY), Clr, 0, 0, ptPen);

     p:=Ry2-(Rx2*Ry)+(Rx2 div 4);
     while px<py do begin
        Inc(x);
        Inc(px,twoRy2);
        if p<0 then
           Inc(p,Ry2+px)
        else begin
           Dec(y);
           Dec(py,twoRx2);
           Inc(p,Ry2+px-py);
        end;
        SetPixel(Cx+X, Cy+Y, Clr, 0, 0, ptPen);
        SetPixel(Cx-X-Byte(SnapX), Cy+Y, Clr, 0, 0, ptPen);
        SetPixel(Cx+X, Cy-Y-Byte(SnapY), Clr, 0, 0, ptPen);
        SetPixel(Cx-X-Byte(SnapX), Cy-Y-Byte(SnapY), Clr, 0, 0, ptPen);
     end;

{$R-}
     p:=Round(Ry2*(x+0.5)*(x+0.5)+Rx2*(y-1)*(y-1)-Rx2*Ry2);
{$R+}
     while y>0 do begin
        Dec(y);
        Dec(py,twoRx2);
        if p>0 then
           Inc(p,Rx2-py)
        else begin
           Inc(x);
           Inc(px,twoRy2);
           Inc(p,Rx2-py+px);
        end;
        SetPixel(Cx+X, Cy+Y, Clr, 0, 0, ptPen);
        SetPixel(Cx-X-Byte(SnapX), Cy+Y, Clr, 0, 0, ptPen);
        SetPixel(Cx+X, Cy-Y-Byte(SnapY), Clr, 0, 0, ptPen);
        SetPixel(Cx-X-Byte(SnapY), Cy-Y-Byte(SnapY), Clr, 0, 0, ptPen);
     end;

  End;

end;

Procedure TScrPaintForm.DrawPolygon(Clr: Integer; Filled: Boolean);
Var
  X, Y, XMin, YMin, XMax, YMax: Integer;
  BackDIB: TFastDIB;
Begin

  BackDIB := TFastDIB.Create;
  BackDIB.SetSize(256, 192, 8);

  If Filled Then Begin

     BackDIB.Clearb(1);
     BackDIB.SetPen(PS_NULL, 0, 1);
     BackDIB.SetBrush(BS_SOLID, 0, 1);
     BackDIB.Polygon(PolygonLines);

     XMax := 0;
     YMax := 0;
     XMin := 99999;
     YMin := 99999;
     For Y := 0 To 191 Do
        For X := 0 To 255 Do
           If BackDIB.Pixels8[Y, X] <> 1 Then Begin
              If X < XMin Then XMin := X;
              If X > XMax Then XMax := X;
              If Y < YMin Then YMin := Y;
              If Y > YMax Then YMax := Y;
           End;

     If FillStyle = ftGradient Then Begin
        NewGradientWidth := (XMax - XMin)+1;
        NewGradientHeight := (YMax - YMin)+1;
        GradAnchorX := XMin;
        GradAnchorY := 192-YMax;
     End;

     For X := 0 To 255 Do
        For Y := 0 To 191 Do
           If BackDIB.Pixels8[Y, X] <> 1 Then
              SetPixel(X, 192-Y, 1, 0, 0, ptFill);

  End;

  If OutlineShapes Then Begin

     BackDIB.Clearb(1);
     BackDIB.SetPen(PS_SOLID, 1, 1);
     BackDIB.SetBrush(BS_NULL, 0, 1);
     BackDIB.Polygon(PolygonLines);

     For X := 0 To 255 Do
        For Y := 0 To 191 Do
           If BackDIB.Pixels8[Y, X] <> 1 Then
              SetPixel(X, 192-Y, 1, 0, 0, ptPen);

  End;

  BackDIB.Free;

End;

Procedure TScrPaintForm.RemoveSelection(Var MaskScreen: TScrImage);
Var
  X, Y, Xc, Yc: Integer;
Begin

  If CurScreen^.SelActive Then Begin

     For X := 0 To CurScreen^.SelWidth -1 Do
        For Y := 0 To CurScreen^.SelHeight -1 Do Begin
           Xc := X + CurScreen^.SelOrigin.X;
           Yc := Y + CurScreen^.SelOrigin.Y;
           If CurScreen^.SelMask[x + (CurScreen^.SelWidth * y)] = 1 Then
              If ColourMode in [cmBoth, cmPixels] Then
                 If (Xc >= MaskScreen.SelOrigin.X) And (Yc >= MaskScreen.SelOrigin.Y) and (Xc < MaskScreen.SelOrigin.X + MaskScreen.SelWidth) and (Yc < MaskScreen.SelOrigin.Y + MaskScreen.SelHeight) Then Begin
                    If MaskScreen.SelMask[Xc - MaskScreen.SelOrigin.X + ((Yc - MaskScreen.SelOrigin.Y) * MaskScreen.SelWidth)] = 0 Then
                       If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 255) Then Begin
                          CurScreen^.InkDIB[Xc, Yc] := CurScreen^.SelDetail[x + (CurScreen^.SelWidth * y)];
                          CurScreen^.Attrs[Xc Div 8, Yc Div 8] := GetNewAttr(CurScreen^.Attrs[Xc Div 8, Yc Div 8], CurScreen^.SelAttrDetail[X + (CurScreen^.SelWidth * Y)], AttrFlags);
                       End;
                 End Else
                    If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 255) Then Begin
                       CurScreen^.InkDIB[Xc, Yc] := CurScreen^.SelDetail[x + (CurScreen^.SelWidth * y)];
                       CurScreen^.Attrs[Xc Div 8, Yc Div 8] := GetNewAttr(CurScreen^.Attrs[Xc Div 8, Yc Div 8], CurScreen^.SelAttrDetail[X + (CurScreen^.SelWidth * Y)], AttrFlags);
                    End;
        End;

  End;

  SetLength(CurScreen^.SelMask, 0);
  SetLength(CurScreen^.SelDetail, 0);
  SetLength(CurScreen^.SelAttrDetail, 0);

  CurScreen^.SelOrigin := Point(0, 0);
  CurScreen^.SelWidth := 0;
  CurScreen^.SelHeight := 0;
  CurScreen^.SelActive := False;

End;

Procedure TScrPaintForm.ClearSelection;
Var
  X, Y, Xc, Yc, Attr: Integer;
Begin

  // Merge the current Selection into the current image, if one is
  // active.

  If CurScreen^.SelActive Then Begin

     For X := 0 To CurScreen^.SelWidth -1 Do
        For Y := 0 To CurScreen^.SelHeight -1 Do Begin
           Xc := X + CurScreen^.SelOrigin.X;
           Yc := Y + CurScreen^.SelOrigin.Y;
           If CurScreen^.SelMask[x + (CurScreen^.SelWidth * y)] = 1 Then
              If ColourMode in [cmBoth, cmPixels] Then
                 If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 255) Then
                    CurScreen^.InkDIB[Xc, Yc] := CurScreen^.SelDetail[x + (CurScreen^.SelWidth * y)];
        End;

     For X := 0 To CurScreen^.SelWidth -1 Do
        For Y := 0 To CurScreen^.SelHeight -1 Do Begin
           Xc := X + CurScreen^.SelOrigin.X;
           Yc := Y + CurScreen^.SelOrigin.Y;
           If CurScreen^.SelMask[x + (CurScreen^.SelWidth * y)] = 1 Then
              If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 255) Then
                 CurScreen^.Attrs[Xc Div 8, Yc Div 8] := GetNewAttr(CurScreen^.Attrs[Xc Div 8, Yc Div 8], CurScreen^.SelAttrDetail[X + (CurScreen^.SelWidth * Y)], AttrFlags);
        End;

  End;

  // Now clear the current Selection

  SetLength(CurScreen^.SelMask, 0);
  SetLength(CurScreen^.SelDetail, 0);
  SetLength(CurScreen^.SelAttrDetail, 0);

  CurScreen^.SelOrigin := Point(0, 0);
  CurScreen^.SelWidth := 0;
  CurScreen^.SelHeight := 0;
  CurScreen^.SelActive := False;

End;

Procedure TScrPaintForm.SetSelectionRect(X1, Y1, X2, Y2: Integer; MergeType: TMergeType);
Var
  X, Y, Offset: Integer;
Begin

  // Add a rectangular selection.
  // First, Sanitise the coordinates.

  If X1 > X2 Then Begin X := X1; X1 := X2; X2 := X; End;
  If Y1 > Y2 Then Begin Y := Y1; Y1 := Y2; Y2 := Y; End;

  X1 := Min(Max(0, X1), 256); X2 := Max(Min(256, X2), 0);
  Y1 := Min(Max(0, Y1), 192); Y2 := Max(Min(192, Y2), 0);

  CurScreen^.SelOrigin := Point(X1, Y1);
  CurScreen^.SelWidth := Max(1, X2 - X1);
  CurScreen^.SelHeight := Max(1, Y2 - Y1);

  SetLength(CurScreen^.SelMask, CurScreen^.SelWidth * CurScreen^.SelHeight);
  SetLength(CurScreen^.SelDetail, CurScreen^.SelWidth * CurScreen^.SelHeight);
  SetLength(CurScreen^.SelAttrDetail, CurScreen^.SelWidth * CurScreen^.SelHeight);

  // Now add the mask and detail, and remove the selection from the image.

  For X := X1 To X1 + CurScreen^.SelWidth -1 Do
     For Y := Y1 To Y1 + CurScreen^.SelHeight -1 Do Begin
        Offset := (X - X1) + (CurScreen^.SelWidth * (Y - Y1));
        CurScreen^.SelMask[Offset] := 1;
        CurScreen^.SelDetail[Offset] := CurScreen^.InkDIB[X, Y];
        CurScreen^.InkDIB[X, Y] := 0;
     End;

  CurScreen^.SelActive := True;

  SetSelectionAttrs;
  If MergeType <> mtNone Then
     MergeSelections(CurScreen^, LastSelection, MergeType);

End;

Procedure TScrPaintForm.SetSelectionFree(MergeType: TMergeType);
Var
  X, Y, Cx, Cy, MinX, MaxX, MinY, MaxY, Offset: Integer;
  BackDIB: TFastDIB;
Begin

  BackDIB := TFastDIB.Create;
  BackDIB.SetSize(256, -192, 8);

  BackDIB.Clearb(1);
  BackDIB.SetPen(PS_NULL, 0, 1);
  BackDIB.SetBrush(BS_SOLID, 0, 1);
  BackDIB.Polygon(PolygonLines);

  BackDIB.Pixels8[PolygonLines[0].Y, PolygonLines[0].X] := 0;

  MinX := 99999;
  MaxX := 0;

  MinY := 99999;
  MaxY := 0;

  For Y := 0 To 191 Do
     For X := 0 To 255 Do
        If BackDIB.Pixels8[Y, X] <> 1 Then Begin
           If X < MinX then MinX := X;
           If X > MaxX Then MaxX := X;
           If Y < MinY then MinY := Y;
           If Y > MaxY Then MaxY := Y;
        End;

  CurScreen^.SelOrigin := Point(MinX, MinY);
  CurScreen^.SelWidth := Max(1, (MaxX - MinX) +1);
  CurScreen^.SelHeight := Max(1, (MaxY - MinY) +1);

  SetLength(CurScreen^.SelMask, CurScreen^.SelWidth * CurScreen^.SelHeight);
  SetLength(CurScreen^.SelDetail, CurScreen^.SelWidth * CurScreen^.SelHeight);
  SetLength(CurScreen^.SelAttrDetail, CurScreen^.SelWidth * CurScreen^.SelHeight);

  For Y := MinY To MaxY Do
     For X := MinX To MaxX Do
        If BackDIB.Pixels8[Y, X] <> 1 Then Begin
           Cx := X - MinX; Cy := Y - MinY;
           Offset := Cx + (CurScreen^.SelWidth * Cy);
           CurScreen^.SelMask[Offset] := 1;
           CurScreen^.SelDetail[Offset] := CurScreen^.InkDIB[X, Y];
           CurScreen^.InkDIB[X, Y] := 0;
        End;

  BackDIB.Free;

  CurScreen^.SelActive := True;

  SetSelectionAttrs;
  If MergeType <> mtNone Then
     MergeSelections(CurScreen^, LastSelection, MergeType);

End;

Procedure TScrPaintForm.SetSelectionMagic(X1, Y1: Integer; MergeType: TMergeType);
Begin

  FloodFill(X1, Y1, 1);

  CurScreen^.SelActive := True;

  SetSelectionAttrs;
  If MergeType <> mtNone Then
     MergeSelections(CurScreen^, LastSelection, MergeType);

End;

Procedure TScrPaintForm.SetSelectionAttrs;
Var
  X, Y, Cx, Cy: Integer;
Begin

  For X := Curscreen^.SelOrigin.X To CurScreen^.SelOrigin.X + CurScreen^.SelWidth -1 Do
     For Y := Curscreen^.SelOrigin.Y To CurScreen^.SelOrigin.Y + CurScreen^.SelHeight -1 Do
        If CurScreen^.SelMask[(X - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y - CurScreen^.SelOrigin.Y))] = 1 Then Begin
           Cx := X - CurScreen^.SelOrigin.X;
           Cy := Y - CurScreen^.SelOrigin.Y;
           If (Cx >= 0) And (Cx < 256) and (Cy >= 0) and (Cy < 192) Then
              CurScreen^.SelAttrDetail[Cx + (CurScreen^.SelWidth * Cy)] := CurScreen^.Attrs[X Div 8, Y Div 8];
        End;

  For X := Curscreen^.SelOrigin.X To CurScreen^.SelOrigin.X + CurScreen^.SelWidth -1 Do
     For Y := Curscreen^.SelOrigin.Y To CurScreen^.SelOrigin.Y + CurScreen^.SelHeight -1 Do
        If CurScreen^.SelMask[(X - CurScreen^.SelOrigin.X) + (CurScreen^.SelWidth * (Y - CurScreen^.SelOrigin.Y))] = 1 Then Begin
           Cx := X - CurScreen^.SelOrigin.X;
           Cy := Y - CurScreen^.SelOrigin.Y;
           If (Cx >= 0) And (Cx < 256) and (Cy >= 0) and (Cy < 192) Then
              CurScreen^.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurScreen^.Attrs[X Div 8, Y Div 8], CurINK + (CurPAPER Shl 3) + (CurBRIGHT Shl 6) + (CurFLASH Shl 7), AttrFlags);
        End;

End;

Procedure TScrPaintForm.MergeSelections(Var CurrentScreen, LastScreen: TScrImage; MergeType: TMergeType);
Var
  X, Y, Xc, Yc, Src, Dest: Integer;
  TempScr: TScrImage;
  TempArray: Array of Byte;
  OrgX, OrgY, Wid, Hgt, Offset: Integer;
Begin

  CreateScreen(TempScr);

  OrgX := Min(CurrentScreen.SelOrigin.X, LastScreen.SelOrigin.X);
  OrgY := Min(CurrentScreen.SelOrigin.Y, LastScreen.SelOrigin.Y);
  Wid := Max(CurrentScreen.SelOrigin.X + CurrentScreen.SelWidth, LastScreen.SelOrigin.X + LastScreen.SelWidth) - OrgX;
  Hgt := Max(CurrentScreen.SelOrigin.Y + CurrentScreen.SelHeight, LastScreen.SelOrigin.Y + LastScreen.SelHeight) - OrgY;

  SetLength(TempArray, Wid * Hgt);

  // Merge SelMasks

  Offset := (LastScreen.SelOrigin.X - OrgX) + ((LastScreen.SelOrigin.Y - OrgY) * Wid);
  Src := 0;
  For Y := 0 To LastScreen.SelHeight -1 Do Begin
     Dest := Offset + (Y * Wid);
     For X := 0 To LastScreen.SelWidth -1 Do Begin
        TempArray[Dest] := LastScreen.SelMask[Src];
        Inc(Dest);
        Inc(Src);
     End;
  End;

  Offset := (CurrentScreen.SelOrigin.X - OrgX) + ((CurrentScreen.SelOrigin.Y - OrgY) * Wid);
  Src := 0;

  If MergeType = mtXOR then Begin

     For Y := 0 To CurrentScreen.SelHeight -1 Do Begin
        Dest := Offset + (Y * Wid);
        For X := 0 To CurrentScreen.SelWidth -1 Do Begin
           If CurrentScreen.SelMask[Src] = 1 Then Begin
              TempArray[Dest] := 1 - TempArray[Dest];
              If TempArray[Dest] = 0 Then Begin
                 CurrentScreen.InkDIB[X + CurrentScreen.SelOrigin.X, Y + CurrentScreen.SelOrigin.Y] := CurrentScreen.SelDetail[Src];
                 CurrentScreen.Attrs[(X + CurrentScreen.SelOrigin.X) Div 8, (Y + CurrentScreen.SelOrigin.Y) Div 8] := CurrentScreen.SelAttrDetail[Src];
              End Else
                 CurrentScreen.InkDIB[X + CurrentScreen.SelOrigin.X, Y + CurrentScreen.SelOrigin.Y] := 0;
           End;
           Inc(Dest);
           Inc(Src);
        End;
     End;

  End Else If MergeType = mtAdd Then Begin

     For Y := 0 To CurrentScreen.SelHeight -1 Do Begin
        Dest := Offset + (Y * Wid);
        For X := 0 To CurrentScreen.SelWidth -1 Do Begin
           If CurrentScreen.SelMask[Src] = 1 Then Begin
              TempArray[Dest] := 1;
              CurrentScreen.InkDIB[X + CurrentScreen.SelOrigin.X, Y + CurrentScreen.SelOrigin.Y] := 0;
           End;
           Inc(Dest);
           Inc(Src);
        End;
     End;

  End Else If MergeType = mtSubtract Then Begin

     For Y := 0 To CurrentScreen.SelHeight -1 Do Begin
        Dest := Offset + (Y * Wid);
        For X := 0 To CurrentScreen.SelWidth -1 Do Begin
           If CurrentScreen.SelMask[Src] = 1 Then Begin
              CurrentScreen.InkDIB[X + CurrentScreen.SelOrigin.X, Y + CurrentScreen.SelOrigin.Y] := CurrentScreen.SelDetail[Src];
              CurrentScreen.Attrs[(X + CurrentScreen.SelOrigin.X) Div 8, (Y + CurrentScreen.SelOrigin.Y) Div 8] := CurrentScreen.SelAttrDetail[Src];
              If TempArray[Dest] = 1 Then Begin
                 TempArray[Dest] := 0;
                 CurrentScreen.SelMask[Src] := 0;
              End;
           End;
           Inc(Dest);
           Inc(Src);
        End;
     End;

  End;

  SetLength(TempScr.SelMask, Wid * Hgt);
  CopyMemory(@TempScr.SelMask[0], @TempArray[0], Wid * Hgt);

  // Make new SelDetail - LastScreen SelDetail overrides CurrentScreen SelDetail

  Offset := (CurrentScreen.SelOrigin.X - OrgX) + ((CurrentScreen.SelOrigin.Y - OrgY) * Wid);
  Src := 0;
  For Y := 0 To CurrentScreen.SelHeight -1 Do Begin
     Dest := Offset + (Y * Wid);
     For X := 0 To CurrentScreen.SelWidth -1 Do Begin
        If CurrentScreen.SelMask[Src] = 1 Then
           TempArray[Dest] := CurrentScreen.SelDetail[Src];
        Inc(Dest);
        Inc(Src);
     End;
  End;

  Offset := (LastScreen.SelOrigin.X - OrgX) + ((LastScreen.SelOrigin.Y - OrgY) * Wid);
  Src := 0;
  For Y := 0 To LastScreen.SelHeight -1 Do Begin
     Dest := Offset + (Y * Wid);
     For X := 0 To LastScreen.SelWidth -1 Do Begin
        If LastScreen.SelMask[Src] = 1 Then
           TempArray[Dest] := LastScreen.SelDetail[Src];
        Inc(Dest);
        Inc(Src);
     End;
  End;

  SetLength(TempScr.SelDetail, Wid * Hgt);
  CopyMemory(@TempScr.SelDetail[0], @TempArray[0], Wid * Hgt);

  // And the same goes for the attributes.

  Offset := (CurrentScreen.SelOrigin.X - OrgX) + ((CurrentScreen.SelOrigin.Y - OrgY) * Wid);
  Src := 0;
  For Y := 0 To CurrentScreen.SelHeight -1 Do Begin
     Dest := Offset + (Y * Wid);
     For X := 0 To CurrentScreen.SelWidth -1 Do Begin
        If CurrentScreen.SelMask[Src] = 1 Then
           TempArray[Dest] := CurrentScreen.SelAttrDetail[Src];
        Inc(Dest);
        Inc(Src);
     End;
  End;

  Offset := (LastScreen.SelOrigin.X - OrgX) + ((LastScreen.SelOrigin.Y - OrgY) * Wid);
  Src := 0;
  For Y := 0 To LastScreen.SelHeight -1 Do Begin
     Dest := Offset + (Y * Wid);
     For X := 0 To LastScreen.SelWidth -1 Do Begin
        If LastScreen.SelMask[Src] = 1 Then
           TempArray[Dest] := LastScreen.SelAttrDetail[Src];
        Inc(Dest);
        Inc(Src);
     End;
  End;

  SetLength(TempScr.SelAttrDetail, Wid * Hgt);
  CopyMemory(@TempScr.SelAttrDetail[0], @TempArray[0], Wid * Hgt);

  SetLength(CurrentScreen.SelDetail, Wid * Hgt);
  SetLength(CurrentScreen.SelMask, Wid * Hgt);
  SetLength(CurrentScreen.SelAttrDetail, Wid * Hgt);
  CopyMemory(@CurrentScreen.SelMask[0], @TempScr.SelMask[0], Wid * Hgt);
  CopyMemory(@CurrentScreen.SelDetail[0], @TempScr.SelDetail[0], Wid * Hgt);
  CopyMemory(@CurrentScreen.SelAttrDetail[0], @TempScr.SelAttrDetail[0], Wid * Hgt);

  CurrentScreen.SelWidth := Wid;
  CurrentScreen.SelHeight := Hgt;
  CurrentScreen.SelOrigin := Point(OrgX, OrgY);
  CurrentScreen.SelActive := True;

  For X := 0 To CurrentScreen.SelWidth -1 Do
     For Y := 0 To CurrentScreen.SelHeight -1 Do Begin
        Offset := X + (Y * CurrentScreen.SelWidth);
        If CurrentScreen.SelMask[Offset] = 1 Then Begin
           CurrentScreen.InkDIB[X + CurrentScreen.SelOrigin.X, Y + CurrentScreen.SelOrigin.Y] := 0;
        End;
     End;

  For X := CurrentScreen.SelOrigin.X To CurrentScreen.SelOrigin.X + CurrentScreen.SelWidth -1 Do
     For Y := CurrentScreen.SelOrigin.Y To CurrentScreen.SelOrigin.Y + CurrentScreen.SelHeight -1 Do
        If CurrentScreen.SelMask[(X - CurrentScreen.SelOrigin.X) + (CurrentScreen.SelWidth * (Y - CurrentScreen.SelOrigin.Y))] = 1 Then Begin
           Xc := X - CurrentScreen.SelOrigin.X;
           Yc := Y - CurrentScreen.SelOrigin.Y;
           If (Xc >= 0) And (Xc < 256) and (Yc >= 0) and (Yc < 192) Then
              CurrentScreen.Attrs[X Div 8, Y Div 8] := GetNewAttr(CurrentScreen.Attrs[X Div 8, Y Div 8], CurINK + (CurPAPER Shl 3) + (CurBRIGHT Shl 6) + (CurFLASH Shl 7), AttrFlags);
        End;

End;

Procedure TScrPaintForm.MakeSelectionTransparent(Index: Integer);
Var
  X, Y: Integer;
Begin

  For X := 0 To Screens[Index]^.SelWidth -1 Do Begin
     For Y := 0 To Screens[Index]^.SelHeight -1 Do Begin
        Screens[Index]^.SelMask[X + (Screens[Index]^.SelWidth * Y)] := Screens[Index]^.SelDetail[X + (Screens[Index]^.SelWidth * Y)];
     End;
  End;

End;

Procedure TScrPaintForm.ExpandSelection(Index: Integer);
Var
  X, Y, X1, Y1, Idx, Offset: Integer;
  Backup: TScrImage;
  TempArray: Array of Byte;
Begin

  BackUp.Undo := nil;
  BackUp.Redo := nil;
  CreateScreen(Backup);
  CopyScreen(Screens[Index]^, Backup);

  SetLength(TempArray, (Backup.SelWidth +2) * (Backup.SelHeight +2));
  Idx := Backup.SelWidth +3;
  For Y := 0 To Backup.SelHeight -1 Do Begin
     CopyMemory(@TempArray[Idx], @Backup.SelMask[Y * Backup.SelWidth], Backup.SelWidth);
     Inc(Idx, Backup.SelWidth +2);
  End;

  Inc(Backup.SelWidth, 2);
  Inc(Backup.SelHeight, 2);
  Dec(Backup.SelOrigin.X);
  Dec(Backup.SelOrigin.Y);

  SetLength(Backup.SelMask, Length(TempArray));
  CopyMemory(@Backup.SelMask[0], @TempArray[0], Length(TempArray));

  For X := 1 To Backup.SelWidth -2 Do
     For Y := 1 To Backup.SelHeight -2 Do Begin
        Offset := X + (Y * Backup.SelWidth);
        If Backup.SelMask[Offset] = 1 Then Begin
           TempArray[Offset -1] := 1;
           TempArray[Offset +1] := 1;
           TempArray[Offset + Backup.SelWidth] := 1;
           TempArray[Offset - Backup.SelWidth] := 1;
           TempArray[Offset] := 1;
        End;
     End;

  ClearSelection;

  SetLength(Screens[Index]^.SelMask, Length(TempArray));
  SetLength(Screens[Index]^.SelDetail, Length(TempArray));
  SetLength(Screens[Index]^.SelAttrDetail, Length(TempArray));
  Screens[Index]^.SelOrigin := Point(Backup.SelOrigin.X, Backup.SelOrigin.Y);
  Screens[Index]^.SelWidth := Backup.SelWidth;
  Screens[Index]^.SelHeight := Backup.SelHeight;
  Screens[Index]^.SelActive := True;

  For X := 0 To Backup.SelWidth -1 Do
     For Y := 0 To Backup.SelHeight -1 Do Begin
        Offset := X + (Y * Backup.SelWidth);
        If TempArray[Offset] = 1 Then Begin
           Screens[Index]^.SelMask[Offset] := 1;
           Screens[Index]^.SelDetail[Offset] := Screens[Index]^.InkDIB[X + Screens[Index]^.SelOrigin.x, Y + Screens[Index]^.SelOrigin.Y];
           Screens[Index]^.SelAttrDetail[Offset] := Screens[Index]^.Attrs[(X + Screens[Index]^.SelOrigin.x) Div 8, (Y + Screens[Index]^.SelOrigin.Y) Div 8];
           Screens[Index]^.InkDIB[X + Screens[Index]^.SelOrigin.x, Y + Screens[Index]^.SelOrigin.Y] := 0;
        End;
     End;

  SetSelectionAttrs;

End;

Procedure TScrPaintForm.ShrinkSelection(Index: Integer);
Var
  X, Y, X1, Y1, Offset: Integer;
  Left, Right, Up, Down: Boolean;
  Backup: TScrImage;
Begin

  Backup.Undo := Nil;
  Backup.Redo := Nil;
  CreateScreen(Backup);
  CopyScreen(CurScreen^, Backup);
  ClearSelection;

  SetLength(CurScreen^.SelMask, Length(Backup.SelMask));
  SetLength(CurScreen^.SelDetail, Length(Backup.SelMask));
  SetLength(CurScreen^.SelAttrDetail, Length(Backup.SelMask));

  For X := 0 To Backup.SelWidth -1 Do Begin
     For Y := 0 To Backup.SelHeight -1 Do Begin
        Offset := X + (Y * Backup.SelWidth);
        If Backup.SelMask[Offset] = 1 Then Begin
           Left := False; Right := False; Up := False; Down := False;
           If X > 0 Then Left := Backup.SelMask[Offset -1] = 0 Else Left := True;
           If X < Backup.SelWidth -1 Then Right := Backup.SelMask[Offset +1] = 0 Else Right := True;
           If Y > 0 Then Up := Backup.SelMask[Offset - Backup.SelWidth] = 0 Else Up := True;
           If Y < BackUp.SelHeight -1 Then Down := Backup.SelMask[Offset + Backup.SelWidth] = 0 Else Down := True;
           If Not(Left or Right or Up or Down) Then CurScreen^.SelMask[Offset] := 1;
        End;
     End;
  End;

  CurScreen^.SelOrigin := Point(Backup.SelOrigin.X, Backup.SelOrigin.Y);
  CurScreen^.SelActive := True;
  CurScreen^.SelWidth := Backup.SelWidth;
  CurScreen^.SelHeight := Backup.SelHeight;

  For X := 0 To CurScreen^.SelWidth -1 Do
     For Y := 0 To CurScreen^.SelHeight -1 Do
        If CurScreen^.SelMask[X + (Y * CurScreen^.SelWidth)] = 1 Then Begin
           CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)] := CurScreen^.InkDIB[X + CurScreen^.SelOrigin.X, Y + CurScreen^.SelOrigin.Y];
           CurScreen^.InkDIB[X + CurScreen^.SelOrigin.X, Y + CurScreen^.SelOrigin.Y] := 0;
        End;

  SetSelectionAttrs;

End;

procedure TScrPaintForm.SelectTool(Tool: Integer);
begin

  Case Tool of

     1: // Freehand
        PaintMode := pmFreehand;

     2: // Freehand connected
        PaintMode := pmFreeHandConnected;

     3: // Spray Can
        PaintMode := pmSprayCan;

     4: // Text tool
        PaintMode := pmTextPlace;

     5: // Clone Brush
        PaintMode := pmCloneBrush;

     6: // Lines
        PaintMode := pmLine;

     7: // Curves
        PaintMode := pmCurve1;

     8: // Rectangle
        PaintMode := pmRectangle;

     9: // Ellipses
        PaintMode := pmEllipse;

     10: // Circles
        PaintMode := pmCircle;

     11: // Polygon
        PaintMode := pmPolygon;

     12: // Select
        PaintMode := pmSelectRect;

     13: // Select Freehand
        PaintMode := pmSelectFree;

     14: // Magic Wand
        PaintMode := pmMagicWand;

     15: // Zoom In
        ZoomIn;

     16: // Zoom Out
        ZoomOut;

     17: // Flood Fill
        PaintMode := pmFloodFill;

     18: // Colour picker
        PaintMode := pmColourDropper;

     19: // AttributePaint
        PaintMode := pmPaint;

     20: // Pan
        PaintMode := pmPan;

     21: // Pan with Attrs
        PaintMode := pmPanAttrs;

     22: // Rotate
        PaintMode := pmRotate;

     23: // Rotate with Attrs
        PaintMode := pmRotateAttrs;

     24: // Skew
        PaintMode := pmSkew;

     25: // Skew with Attrs
        PaintMode := pmSkewAttrs;

  End;

  RenderScreen(CurScreenIndex, ShowingAttrs);

end;

Procedure TScrPaintForm.ZoomIn;
Begin

  If CurScreen^.ZoomLevel < 80 Then Begin

     Inc(CurScreen^.ZoomLevel);
     If TextForm.Showing Then
        TextForm.Backup.ZoomLevel := CurScreen^.ZoomLevel;
     RenderScreen(CurScreenIndex, ShowingAttrs);
     Caption := EDITORNAME + TabControl1.Tabs[CurScreenIndex] + '  [' + IntToStr(CurScreen^.ZoomLevel) + ':1]';

  End;

End;

Procedure TScrPaintForm.ZoomOut;
Begin

  If CurScreen^.ZoomLevel > 1 Then Begin
     Dec(CurScreen^.ZoomLevel);
     If TextForm.Showing Then
        TextForm.Backup.ZoomLevel := CurScreen^.ZoomLevel;
     RenderScreen(CurScreenIndex, ShowingAttrs);
     Caption := EDITORNAME + TabControl1.Tabs[CurScreenIndex] + '  [' + IntToStr(CurScreen^.ZoomLevel) + ':1]';
     FormResize(Nil);
  End;

End;

Procedure TScrPaintForm.MenuItemClick(Sender: TObject);
Begin

  Case ((Sender As TMenuItem).GetParentMenu As TMenu).Tag of

     0: Begin
           ToolButton1.ImageIndex := (Sender As TMenuItem).ImageIndex;
           ToolButton1.Tag := (Sender As TMenuItem).Tag;
           ToolButton1Click(ToolButton1);
        End;

     1: Begin
           ToolButton4.ImageIndex := (Sender As TMenuItem).ImageIndex;
           ToolButton4.Tag := (Sender As TMenuItem).Tag;
           ToolButton1Click(ToolButton4);
        End;

     2: Begin
           ToolButton6.ImageIndex := (Sender As TMenuItem).ImageIndex;
           ToolButton6.Tag := (Sender As TMenuItem).Tag;
           ToolButton1Click(ToolButton6);
        End;

     3: Begin
           ToolButton8.ImageIndex := (Sender As TMenuItem).ImageIndex;
           ToolButton8.Tag := (Sender As TMenuItem).Tag;
           ToolButton1Click(ToolButton8);
        End;

     5: Begin
           ToolButton5.ImageIndex := (Sender As TMenuItem).ImageIndex;
           ToolButton5.Tag := (Sender As TMenuItem).Tag;
           ToolButton1Click(ToolButton5);
        End;

     6: Begin
           ToolButton12.ImageIndex := (Sender As TMenuItem).ImageIndex;
           ToolButton12.Tag := (Sender As TMenuItem).Tag;
           ToolButton1Click(ToolButton12);
       End;

  End;

End;

procedure TScrPaintForm.ToolButton1Click(Sender: TObject);
begin

  ToolButton1.Down := False;
  ToolButton2.Down := False;
  ToolButton4.Down := False;
  ToolButton5.Down := False;
  ToolButton6.Down := False;
  ToolButton7.Down := False;
  ToolButton8.Down := False;
  ToolButton12.Down := False;
  SelectTool((Sender As TComponent).Tag);
  TimerToolButton := Sender As TToolButton;
  SendMessage(ToolBar1.Handle,WM_LBUTTONDOWN,0,0);
  SendMessage(ToolBar1.Handle,WM_LBUTTONUP,0,0);

end;

procedure TScrPaintForm.ToolButton10Click(Sender: TObject);
begin

  ZoomIn;

end;

procedure TScrPaintForm.ToolButton11Click(Sender: TObject);
begin

  ZoomOut;

end;

Procedure TScrPaintForm.RenderToolStyle(DIB: TFastDIB; Pattern: TPattern);
Var
  pWidth, pHeight, X, Y, Cx, Cy: Integer;
  pArray: Array of Byte;
  tDIB, tDIB2: TFastDIB;
Begin

  With DIB Do Begin

     Clear(FRGBn(DisplayPalette[7]));
     Rectangle(0, 0, DIB.Width, DIB.Height);

     Case Pattern of

        ptBrush:
           Begin
              SprayDensity := 0;
              SetLength(pArray, Length(CurrentBrush));
              CopyMemory(@pArray[0], @CurrentBrush[0], Length(pArray));
              pWidth := CurrentBrushWidth;
              pHeight := CurrentBrushHeight;
              For X := 0 To pWidth -1 Do
                 For Y := 0 To pHeight -1 Do
                    If pArray[X + (Y * pWidth)] = 1 Then Begin
                       pArray[X + (Y * pWidth)] := CurrentPattern[((Y Mod CurrentPatternHeight) * CurrentPatternWidth) + (X Mod CurrentPatternWidth)];
                       Inc(SprayDensity);
                    End;
              SprayDensity := Max(SprayDensity Div 32, 5);
           End;

        ptFill:
           Begin
              If FillStyle = ftPattern Then Begin
                 SetLength(pArray, Length(CurrentFill));
                 CopyMemory(@pArray[0], @CurrentFill[0], Length(pArray));
                 pWidth := CurrentFillWidth;
                 pHeight := CurrentFillHeight;
              End Else Begin
                 tDIB := TFastDIB.Create;
                 tDIB.SetSize(DIB.Width - 6, DIB.AbsHeight - 6, 8);
                 BrushSelectorForm.MakeGradient(tDIB, NewGradientType, BrushSelectorForm.TrackBar5.Position, BrushSelectorForm.TrackBar6.Position, False);
                 tDIB.Draw(DIB.hDc, 3, 3);
                 tDIB.Free;
                 Exit;
              End;
           End;

     End;

     tDIB := TFastDIB.Create;
     tDIB.SetSize(Max(pWidth, pHeight)+4, Max(pWidth, pHeight)+4, 32);
     tDIB.Clear(FRGBn(DisplayPalette[15]));
     Cx := (tDIB.Width Div 2) - (pWidth Div 2);
     Cy := (tDIB.AbsHeight Div 2) - (pHeight Div 2);

     For X := 0 To pWidth -1 Do
        For Y := 0 To pHeight -1 Do
           If pArray[(Y * pWidth) + X] = 1 Then
              tDIB.Pixels32[(pHeight - 1 - Y) + Cy, X + Cx -1] := DisplayPalette[0];

     If (tDIB.Width > DIB.Width -10) or (tDIB.Height > DIB.Height - 10) Then Begin
        tDIB2 := TFastDIB.Create;
        tDIB2.SetSize(DIB.Width - 3, DIB.Height - 3, 32);
        Bilinear32(tDIB, tDIB2);
        tDIB2.Draw(DIB.hDc, (DIB.Width Div 2) - (tDIB2.Width Div 2), (DIB.Height Div 2) - (tDIB2.Height Div 2));
        tDIB2.Free;
     End Else
        tDIB.Draw(DIB.hDc, (DIB.Width Div 2) - (tDIB.Width Div 2), (DIB.Height Div 2) - (tDIB.Height Div 2));

     tDIB.Free;

  End;

End;

Procedure TScrPaintForm.MakePen(PenType: TPenType; Var SizeX, SizeY: Integer; Rotation: Integer; Graph: Pointer);
Var
  tDIB, fDIB: TFastDIB;
  Size, Cx, Cy, Rx, Ry, Clr, Rx2, Ry2, twoRx2, twoRy2, p,x,y,Xn,Yn, px,py, MinX, MinY, MaxX, MaxY: Integer;
Label
  Rotate;
Begin

  If PenType = ptGraphical Then Begin

     SizeX := Word(Graph^)+1;
     Inc(DWord(Graph), 2);
     SizeY := Word(Graph^)+1;
     Inc(DWord(Graph), 2);
     SetLength(PenArray, SizeX * SizeY);
     CopyMemory(@PenArray[0], Graph, SizeX * SizeY);
     Exit;

  End;

  Size := Max(SizeX *2, SizeY *2);

  tDIB := TFastDIB.Create;
  tDIB.SetSize(Size, Size, 8);
  tDIB.Colors[0].r := 0;
  tDIB.Colors[1].r := 255;
  tDIB.ClearB(0);

  Case PenType of

     ptRound:
        Begin

           If SizeX = 1 Then Begin

              SetLength(PenArray, SizeY);
              FillMemory(@PenArray[0], SizeY, 1);
              For Cx := 0 To SizeX -1 Do
                 For Cy := 0 To SizeY -1 Do
                    tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
              Goto Rotate;

           End Else

              If SizeX = 2 Then Begin

                 SetLength(PenArray, SizeY * 2);
                 FillMemory(@PenArray[0], SizeY * 2, 1);
                 For Cx := 0 To SizeX -1 Do
                    For Cy := 0 To SizeY -1 Do
                       tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
                 Goto Rotate;

              End Else

                 If SizeX = 3 Then Begin

                    SetLength(PenArray, SizeX * SizeY);
                    If SizeY < 3 Then
                       FillMemory(@PenArray[0], SizeX * SizeY, 1)
                    Else Begin
                       FillMemory(@PenArray[0], SizeX * SizeY, 0);
                       For Cy := 0 To SizeY -1 Do Begin
                          PenArray[(Cy * SizeX) + 1] := 1;
                          If (Cy > ((SizeY Div 3) -1)) And (Cy < (SizeY - (SizeY Div 3))) Then Begin
                             PenArray[(Cy * SizeX)] := 1;
                             PenArray[(Cy * SizeX) +2] := 1;
                          End;
                       End;
                    End;
                    For Cx := 0 To SizeX -1 Do
                       For Cy := 0 To SizeY -1 Do
                          tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
                    Goto Rotate;

                 End Else

                    If SizeX = 4 Then Begin

                       SetLength(PenArray, SizeX * SizeY);
                       If SizeY < 3 Then
                          FillMemory(@PenArray[0], SizeX * SizeY, 1)
                       Else
                          If SizeY = 3 Then Begin
                             PenArray[1] := 1; PenArray[2] := 1;
                             PenArray[4] := 1; PenArray[5] := 1; PenArray[6] := 1; PenArray[7] := 1;
                             PenArray[9] := 1; PenArray[10] := 1;
                          End Else Begin
                             FillMemory(@PenArray[0], SizeX * SizeY, 0);
                             For Cy := 0 To SizeY -1 Do Begin
                                PenArray[(Cy * SizeX) + 1] := 1;
                                PenArray[(Cy * SizeX) + 2] := 1;
                                If (Cy > ((SizeY Div 4) -1)) And (Cy < (SizeY - (SizeY Div 4))) Then Begin
                                   PenArray[(Cy * SizeX)] := 1;
                                   PenArray[(Cy * SizeX) +3] := 1;
                                End;
                             End;
                          End;
                       For Cx := 0 To SizeX -1 Do
                          For Cy := 0 To SizeY -1 Do
                             tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
                       Goto Rotate;

                    End;

           If SizeY = 1 Then Begin

              SetLength(PenArray, SizeX);
              FillMemory(@PenArray[0], SizeX, 1);
              For Cx := 0 To SizeX -1 Do
                 For Cy := 0 To SizeY -1 Do
                    tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
              Goto Rotate;

           End Else

              If SizeY = 2 Then Begin

                 SetLength(PenArray, SizeX * 2);
                 FillMemory(@PenArray[0], SizeX * 2, 1);
                 For Cx := 0 To SizeX -1 Do
                    For Cy := 0 To SizeY -1 Do
                       tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
                 Goto Rotate;

              End Else

                 If SizeY = 3 Then Begin

                    SetLength(PenArray, SizeX * SizeY);
                    If SizeX < 3 Then
                       FillMemory(@PenArray[0], SizeX * SizeY, 1)
                    Else Begin
                       FillMemory(@PenArray[0], SizeX * SizeY, 0);
                       For Cx := 0 To SizeX -1 Do Begin
                          PenArray[SizeX + Cx] := 1;
                          If (Cx > ((SizeX Div 3) -1)) And (Cx < (SizeX - (SizeX Div 3))) Then Begin
                             PenArray[Cx] := 1;
                             PenArray[(2 * SizeX) + Cx] := 1;
                          End;
                       End;
                    End;
                    For Cx := 0 To SizeX -1 Do
                       For Cy := 0 To SizeY -1 Do
                          tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
                    Goto Rotate;

                 End Else

                    If SizeY = 4 Then Begin

                       SetLength(PenArray, SizeX * SizeY);
                       If SizeX < 3 Then
                          FillMemory(@PenArray[0], SizeX * SizeY, 1)
                       Else
                          If SizeX = 3 Then Begin
                                               PenArray[1]  := 1;
                             PenArray[3] := 1; PenArray[4]  := 1; PenArray[5] := 1;
                             PenArray[6] := 1; PenArray[7]  := 1; PenArray[9] := 1;
                                               PenArray[10] := 1;
                          End Else Begin
                             FillMemory(@PenArray[0], SizeX * SizeY, 0);
                             For Cx := 0 To SizeX -1 Do Begin
                                PenArray[SizeX + Cx] := 1;
                                PenArray[(2 * SizeX) + Cx] := 1;
                                If (Cx > ((SizeX Div 4) -1)) And (Cx < (SizeX - (SizeX Div 4))) Then Begin
                                   PenArray[Cx] := 1;
                                   PenArray[(3 * SizeX) + Cx] := 1;
                                End;
                             End;
                          End;
                       For Cx := 0 To SizeX -1 Do
                          For Cy := 0 To SizeY -1 Do
                             tDIB.Pixels8[Cy + (SizeY Div 2), Cx + (SizeX Div 2)] := PenArray[(Cy * SizeX)+Cx];
                       Goto Rotate;

                    End;

           Rx := SizeX Div 2;
           Ry := SizeY Div 2;
           Cx := Size Div 2; Cy := Cx;

           Rx2:=Rx*Rx;    Ry2:=Ry*Ry;
           twoRx2:=2*Rx2; twoRy2:=2*Ry2;
           x:=0;          y:=Ry;
           px:=0;         py:=twoRx2*y;

           p:=Ry2-(Rx2*Ry)+(Rx2 div 4);
           while px<py do begin
              Inc(x);
              Inc(px,twoRy2);
              if p<0 then
                 Inc(p,Ry2+px)
              else begin
                 Dec(y);
                 Dec(py,twoRx2);
                 Inc(p,Ry2+px-py);
              end;
              For Xn := Cx-X To Cx+X Do Begin
                 tDIB.Pixels8[Cy+Y, Xn] := 1;
                 tDIB.Pixels8[Cy-Y, Xn] := 1;
              End;
           end;

           p:=Round(Ry2*(x+0.5)*(x+0.5)+Rx2*(y-1)*(y-1)-Rx2*Ry2);
           while y>0 do begin
              Dec(y);
              Dec(py,twoRx2);
              if p>0 then
                 Inc(p,Rx2-py)
              else begin
                 Inc(x);
                 Inc(px,twoRy2);
                 Inc(p,Rx2-py+px);
              end;
              For Xn := Cx-X To Cx+X Do Begin
                 tDIB.Pixels8[Cy+Y, Xn] := 1;
                 tDIB.Pixels8[Cy-Y, Xn] := 1;
              End;
           end;

        End;

     ptSquare:
        Begin

           For Cx := Size Div 2 - (SizeX Div 2) To (Size Div 2 - (SizeX Div 2)) + SizeX -1 Do
              For Cy := Size Div 2 - (SizeY Div 2) To (Size Div 2 - (SizeY Div 2)) + SizeY - 1 Do
                 tDIB.Pixels8[Cy, Cx] := 1;

        End;

  End;

Rotate:

  fDIB := TFastDIB.Create;
  fDIB.SetSize(Size, Size, 8);

  If Rotation <> 0 Then
     RotatePen(tDIB, fDIB, tDIB.Width Div 2, tDIB.AbsHeight Div 2, Rotation, True)
  Else
     tDIB.Draw(fDIB.hDc, 0, 0);

  MinX := 99; MaxX := 0;
  MinY := 99; MaxY := 0;

  For Cy := 0 To Size -1 Do
     For Cx := 0 To Size -1 Do
        If fDIB.Pixels8[Cy, Cx] = 1 Then Begin
           If Cx < MinX Then MinX := Cx;
           If Cx > MaxX Then MaxX := Cx;
           If Cy < MinY Then MinY := Cy;
           If Cy > MaxY Then MaxY := Cy;
        End;

  SetLength(PenArray, ((MaxY - MinY) +1) * ((MaxX - MinX) +1));
  FillMemory(@PenArray[0], Length(PenArray), 0);

  For Cy := MinY To MaxY Do
     For Cx := MinX To MaxX Do
        If fDIB.Pixels8[Cy, Cx] = 1 Then
           PenArray[((Cy - MinY) * ((MaxX - MinX) +1)) + (Cx - MinX)] := 1;

  SizeX := (MaxX - MinX) +1;
  SizeY := (MaxY - MinY) +1;
  tDIB.Free;
  fDIB.Free;

End;

Procedure TScrPaintForm.RotatePen(Bmp, Dst: TFastDIB; cx,cy: Integer; Angle: Double; Precise: Boolean);
var
  x, y, dx, dy, sdx, sdy, xDiff, yDiff, isinTheta, icosTheta: Integer;
  Tmp: PFColor;
  Tmp2: PByte;
  Ignore: Byte;
  sinTheta, cosTheta, Theta: Double;
  TempDIB: TFastDIB;
  CountArray: Array[0..1] of DWord;
begin

  TempDIB := TFastDIB.Create;
  If Precise Then Begin
     TempDIB.SetSize(Bmp.Width * 2, Bmp.AbsHeight * 2, 8);
     Quick2x(Bmp, TempDIB);
     Cx := Cx * 2;
     Cy := Cy * 2;
  End Else
     TempDIB.MakeCopy(Bmp, True);

  CountArray[0] := 0;
  CountArray[1] := 0;

  For Y := 0 To Bmp.AbsHeight -1 Do
     For X := 0 To Bmp.Width -1 Do
        If Bmp.Pixels8[Y, X] = 0 Then
           Inc(CountArray[0])
        Else
           Inc(CountArray[1]);

  Dst.SetSize(TempDIB.Width, TempDIB.AbsHeight, 8);

  Theta:=-Angle*Pi/180;
  sinTheta:=Sin(Theta);
  cosTheta:=Cos(Theta);
  xDiff:=(Dst.Width-TempDIB.Width)div 2;
  yDiff:=(Dst.Height-TempDIB.Height)div 2;
  isinTheta:=Round(sinTheta*$10000);
  icosTheta:=Round(cosTheta*$10000);
  Dst.ClearB(0);

  Tmp2 := Pointer(Dst.Bits);
  for y:=0 to Dst.Height-1 do begin
     sdx:=Round(((cx+(-cx)*cosTheta-(y-cy)*sinTheta)-xDiff)*$10000);
     sdy:=Round(((cy+(-cx)*sinTheta+(y-cy)*cosTheta)-yDiff)*$10000);
     for x:=0 to Dst.Width-1 do begin
        dx:=(sdx shr 16);
        dy:=(sdy shr 16);
        if (dx > -1) and (dx < TempDIB.Width) and (dy > -1) and (dy < TempDIB.Height) then
           If TempDIB.Pixels8[dy, dx] <> Ignore Then
              Tmp2^ := 1 - Ignore;
        Inc(sdx,icosTheta);
        Inc(sdy,isinTheta);
        Inc(Tmp2);
     end;
     Inc(Tmp2,Dst.Gap);
  end;

  TempDIB.SetSize(Bmp.Width, Bmp.AbsHeight, 8);

  If Precise Then Begin
     For X := 0 To Dst.Width -1 Do
        For Y := 0 To Dst.AbsHeight -1 Do
           If Dst.Pixels8[Y, X] <> Ignore Then
              TempDIB.Pixels8[Y Div 2, X Div 2] := 1-Ignore;
     Dst.SetSize(Bmp.Width, Bmp.AbsHeight, 8);
     For Y := 0 To TempDIB.AbsHeight -1 Do
        For X := 0 To TempDIB.Width -1 Do
           Dst.Pixels8[Y, X] := TempDIB.Pixels8[Y, X];
  End;

  TempDIB.Free;

End;

procedure TScrPaintForm.FastIMG2Click(Sender: TObject);
Var
  Mouse: TPoint;
  TempStr: String;
begin

  If PaintMode in [pmText, pmTextPlace] Then Exit;

  Mouse := Point(FastIMG1.DIBLeft + (FastIMG1.Bmp.Width Div 2), FastIMG1.DIBTop + (FastIMG1.Bmp.AbsHeight Div 2));
  Mouse := FastIMG1.ClientToScreen(Mouse);
  CentreForm(BrushSelectorForm, Mouse.X, Mouse.Y);
  BrushSelectorForm.SetType := 0;
  BrushSelectorForm.CheckBox1.Checked := CentrePen;
  ShowWindow(BrushSelectorForm, True);

  If Not BrushSelectorForm.Cancelled Then Begin

     CentrePen := BrushSelectorForm.CheckBox1.Checked;
     SetLength(CurrentBrush, Length(PenArray));
     CopyMemory(@CurrentBrush[0], @PenArray[0], Length(PenArray));
     CurrentBrushWidth := BrushSelectorForm.PenWidth;
     CurrentBrushHeight := BrushSelectorForm.PenHeight;

     TempStr := Copy(BrushSelectorForm.BrushPos[BrushSelectorForm.Img2Selected], 8, 999999);
     SetLength(CurrentPattern, Length(TempStr) -4);
     CopyMemory(@CurrentPattern[0], @TempStr[5], Length(CurrentPattern));
     CurrentPatternWidth := GetWord(@TempStr[1]) +1;
     CurrentPatternHeight := GetWord(@TempStr[3]) +1;

     RenderToolStyle(FastIMG2.Bmp, ptBrush);

     SetLength(TempStr, Length(PenArray));
     CopyMemory(@TempStr[1], @PenArray[0], Length(PenArray));
     TempStr := Chr((CurrentBrushWidth -1) And 255) + Chr((CurrentBrushWidth -1) Shr 8) +
                Chr((CurrentBrushHeight -1) And 255) + Chr((CurrentBrushHeight -1) Shr 8) + TempStr;
     If PenHistory.IndexOf(TempStr) = -1 Then
        PenHistory.Add(TempStr);

     FastIMG2.repaint;

  End;

end;

procedure TScrPaintForm.FastIMG5Click(Sender: TObject);
Var
  Mouse: TPoint;
  TempStr: String;
begin

  If PaintMode in [pmText, pmTextPlace] Then Exit;

  Mouse := Point(FastIMG1.DIBLeft + (FastIMG1.Bmp.Width Div 2), FastIMG1.DIBTop + (FastIMG1.Bmp.AbsHeight Div 2));
  Mouse := FastIMG1.ClientToScreen(Mouse);
  CentreForm(BrushSelectorForm, Mouse.X, Mouse.Y);
  BrushSelectorForm.SetType := 1;
  ShowWindow(BrushSelectorForm, True);

  If Not BrushSelectorForm.Cancelled Then Begin

     If BrushSelectorForm.ComboBox2.ItemIndex = 0 Then Begin

        TempStr := Copy(BrushSelectorForm.BrushPos[BrushSelectorForm.Img3Selected], 8, 999999);
        SetLength(CurrentFill, Length(TempStr) -4);
        CopyMemory(@CurrentFill[0], @TempStr[5], Length(CurrentFill));
        CurrentFillWidth := GetWord(@TempStr[1]) +1;
        CurrentFillHeight := GetWord(@TempStr[3]) +1;
        FillStyle := ftPattern;

     End Else Begin

        NewGradientType := TGradientType(BrushSelectorForm.RadioGroup1.ItemIndex);
        FillStyle := ftGradient;

     End;

     RenderToolStyle(FastIMG5.Bmp, ptFill);
     FastIMG5.repaint;

  End;

end;

Function TScrPaintForm.GetNewAttr(CurAttr, NewINK, NewPAPER, NewBRIGHT, NewFLASH: Byte; Flags: TAttributeFlags): Byte;
Var
  INK, PAPER, FLASH, BRIGHT: Byte;
Begin

  If pwINK in Flags Then
     INK := NewINK
  Else
     INK := CurAttr and 7;

  If pwPAPER in Flags Then
     PAPER := NewPAPER Shl 3
  Else
     PAPER := CurAttr and 56;

  If pwBRIGHT in Flags Then
     BRIGHT := NewBright * 64
  Else
     BRIGHT := CurAttr and 64;

  If pwFLASH in Flags Then
     FLASH := NewFLASH * 128
  Else
     FLASH := CurAttr and 128;

  Result := INK + PAPER + BRIGHT + FLASH;

End;

Function TScrPaintForm.GetNewAttr(CurAttr, NewAttr: Byte; Flags: TAttributeFlags): Byte;
Var
  INK, PAPER, FLASH, BRIGHT: Byte;
Begin

  If pwINK in Flags Then
     INK := NewAttr and 7
  Else
     INK := CurAttr and 7;

  If pwPAPER in Flags Then
     PAPER := NewAttr and 56
  Else
     PAPER := CurAttr and 56;

  If pwBRIGHT in Flags Then
     BRIGHT := NewAttr and 64
  Else
     BRIGHT := CurAttr and 64;

  If pwFLASH in Flags Then
     FLASH := NewAttr and 128
  Else
     FLASH := CurAttr and 128;

  Result := INK + PAPER + BRIGHT + FLASH;

End;

procedure TScrPaintForm.Open1Click(Sender: TObject);
Var
  ScreenString: String;
  Extension: String;
  BinLength, Offset: Integer;
begin

  If Sender <> nil Then
     Filename := OpenFile(Handle, 'Load screen image', [FTBsc, FTBin, FTScr, FTAll], '', False, False);

  If Filename = '' Then Exit;
  If GetFile('.bsc') <> 'Ok' Then Exit;
  Extension := Lowercase(ExtractFileExt(Filename));

  If Extension = '.bsc' Then Begin
     BinLength := Min(GetWord(@FileArray[$0B]), 6912);
     Offset := 17;
  End Else Begin
     BinLength := Min(Length(FileArray), 6912);
     Offset := 0;
  End;

  SetLength(ScreenString, 6912);
  CopyMemory(@ScreenString[1], @FileArray[Offset], 6912);

  MemToScreen(ScreenString, CurScreenIndex);
  RenderScreen(CurScreenIndex, ShowingAttrs);
  TabControl1.Tabs[TabControl1.TabIndex] := ExtractFilename(Filename);
  AddToRecent(Filename);
  MakeUndo('Load Screen', True);

end;

Procedure TScrPaintForm.DrawPalette;
Var
  Idx, Y, X: Integer;
Begin

  // First, the colours at the top of the DIB.

  FastIMG3.Bmp.SetSize(42, 160, 32);
  For Idx := 0 To 7 Do Begin
     FastDrawEx.FillRect32(FastIMG3.Bmp, 0, Idx * 15, 20, ((Idx +1) * 15) -1, DisplayPalette[Idx]);
     FastDrawEx.FillRect32(FastIMG3.Bmp, 21, Idx * 15, 41, ((Idx +1) * 15 -1), DisplayPalette[Idx +8]);
  End;

  Case CurPAPER of
     0..7: FastDrawEx.FillRect32(FastIMG3.Bmp, 0, 120, 41, 149, DisplayPalette[CurPAPER + (8* CurBRIGHT)]);
  End;

  Case CurINK of
     0..7: FastDRawEx.FillRect32(FastIMG3.Bmp, 10, 129, 32, 140, DisplayPalette[CurINK + (8* CurBRIGHT)]);
  End;

  For Idx := 0 To 6 Do Begin
     FastDrawEx.Line32(FastIMG3.Bmp, 0, 40+(Idx * 15), 41, 40+(Idx * 15), FRGBA(0, 0, 0, 0));
  End;

  FastDrawEx.Line32(FastIMG3.Bmp, 21, 40, 21, 159, FRGBA(0,0,0,0));
  FastDrawEx.Rectangle32(FastIMG3.Bmp, 0, 0, 41, 160, TFBlack);
  FastIMG3.Bmp.Pixels32[0, 0] := FRGBA(0, 0, 0, 0);

  PaintAttribFlags;
  RenderFlashState;

End;

Procedure TScrPaintForm.RenderFlashState;
Begin

  If CurFLASH <> 0 Then Begin
     If Not FlashState then
        SpecTextToDIB(FastIMG3.Bmp, 1, 152, 'FLASH', 0, 7, 1, False, False)
     Else
        SpecTextToDIB(FastIMG3.Bmp, 1, 152, 'FLASH', 7, 0, 1, False, False);
  End Else
     SpecTextToDIB(FastIMG3.Bmp, 1, 152, 'FLASH', 0, 7, 1, False, False);

  FastIMG3.Repaint;

End;

procedure TScrPaintForm.FastIMG3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  Clr: Integer;
begin

  If Y < 120 Then Begin

     Clr := Y Div 15;
     If X > 21 Then
        CurBRIGHT := 1
     Else
        CurBRIGHT := 0;

     If Button = mbLeft Then
        CurINK := Clr
     Else
        CurPAPER := Clr;

     DrawPalette;

  End Else If Y > 150 Then Begin

     CurFLASH := 1 - CurFLASH;

     DrawPalette;

  End;

end;

procedure TScrPaintForm.FastIMG1Enter(Sender: TObject);
begin

  MouseInImage := True;
  RenderScreen(CurScreenIndex, ShowingAttrs);

end;

procedure TScrPaintForm.FastIMG1Exit(Sender: TObject);
begin

  MouseInImage := False;
  RenderScreen(CurScreenIndex, ShowingAttrs);

end;

procedure TScrPaintForm.MainMenuClick(Sender: TObject);
Var
  TempStr: String;
begin

  Case (Sender as TComponent).Tag of

     1: // New image
        Begin
           NewScreen;
           RenderScreen(CurScreenIndex, ShowingAttrs);
        End;

     46: // Close Image

        Begin
           If NumScreens > 1 Then Begin
              RemoveScreen(CurScreenIndex);
              RenderScreen(CurScreenIndex, ShowingAttrs);
           End;
        End;

     2: // Open
        Begin
           Open1Click(Sender);
        End;

     3: // Save
        Begin
           Filename := CurScreen^.Filename;
           SaveImageAs(CurScreenIndex);
           CurScreen^.Changed := False;
        End;

     4: // Save As...
        Begin
           Filename := '';
           SaveImageAs(CurScreenIndex);
           CurScreen^.Changed := False;
        End;

     5: // Import
        Begin

           CentreFormOnForm(BMPImportForm, nil);
           ShowWindow(BMPImportForm, True);

        End;

     6: // From Current Screen
        Begin
           SetLength(TempStr, 6912);
           CopyMemory(@TempStr[1], @Memory[16384], 6912);
           MemToScreen(TempStr, CurScreenIndex);
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Screen import', True);
           CurScreen^.Changed := True;
        End;

     7: // Close
        Begin
           Close;
        End;

     8: // Undo
        Begin
           If CurScreen^.Undo.Count > 0 Then Begin
              CopyScreen(CurScreen^, UndoScreen);
              TempStr := CurScreen^.Undo[CurScreen^.Undo.Count -1];
              PerformUndo(TempStr, CurScreen);
              MakeRedo(Copy(TempStr, 1, Pos(#255, TempStr) -1));
              CurScreen^.Undo.Delete(CurScreen^.Undo.Count -1);
              RenderScreen(CurScreenIndex, ShowingAttrs);
              CopyScreen(CurScreen^, UndoScreen);
           End;
        End;

    56: // Redo
        Begin
           If CurScreen^.Redo.Count > 0 Then Begin
              CopyScreen(CurScreen^, UndoScreen);
              TempStr := CurScreen^.Redo[CurScreen^.Redo.Count -1];
              PerformUndo(TempStr, CurScreen);
              MakeUndo(Copy(TempStr, 1, Pos(#255, TempStr) -1), False);
              CurScreen^.Redo.Delete(CurScreen^.Redo.Count -1);
              RenderScreen(CurScreenIndex, ShowingAttrs);
              CopyScreen(CurScreen^, UndoScreen);
           End;
        End;

     9: // Cut
        Begin
           SetLength(ClipScr.SelMask, Length(CurScreen^.SelMask));
           SetLength(ClipScr.SelDetail, Length(CurScreen^.SelDetail));
           SetLength(ClipScr.SelAttrDetail, Length(CurScreen^.SelAttrDetail));
           CopyMemory(@ClipScr.SelMask[0], @CurScreen^.SelMask[0], Length(CurScreen^.SelMask));
           CopyMemory(@ClipScr.SelDetail[0], @CurScreen^.SelDetail[0], Length(CurScreen^.SelDetail));
           CopyMemory(@ClipScr.SelAttrDetail[0], @CurScreen^.SelAttrDetail[0], Length(CurScreen^.SelAttrDetail));
           ClipScr.SelActive := True;
           ClipScr.SelWidth := CurScreen^.SelWidth;
           ClipScr.SelHeight := CurScreen^.SelHeight;
           CurScreen^.SelActive := False;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           CurScreen^.Changed := True;
           MakeUndo('Cut selection', True);
        End;

     10: // Copy
        Begin
           SetLength(ClipScr.SelMask, Length(CurScreen^.SelMask));
           SetLength(ClipScr.SelDetail, Length(CurScreen^.SelDetail));
           SetLength(ClipScr.SelAttrDetail, Length(CurScreen^.SelAttrDetail));
           CopyMemory(@ClipScr.SelMask[0], @CurScreen^.SelMask[0], Length(CurScreen^.SelMask));
           CopyMemory(@ClipScr.SelDetail[0], @CurScreen^.SelDetail[0], Length(CurScreen^.SelDetail));
           CopyMemory(@ClipScr.SelAttrDetail[0], @CurScreen^.SelAttrDetail[0], Length(CurScreen^.SelAttrDetail));
           ClipScr.SelActive := True;
           ClipScr.SelWidth := CurScreen^.SelWidth;
           ClipScr.SelHeight := CurScreen^.SelHeight;
        End;

     11: // Paste
        Begin
           If ClipScr.SelActive Then Begin
              ClearSelection;
              SetLength(CurScreen^.SelMask, Length(ClipScr.SelMask));
              SetLength(CurScreen^.SelDetail, Length(ClipScr.SelDetail));
              SetLength(CurScreen^.SelAttrDetail, Length(ClipScr.SelAttrDetail));
              CopyMemory(@CurScreen^.SelMask[0], @ClipScr.SelMask[0], Length(ClipScr.SelMask));
              CopyMemory(@CurScreen^.SelDetail[0], @ClipScr.SelDetail[0], Length(ClipScr.SelDetail));
              CopyMemory(@CurScreen^.SelAttrDetail[0], @ClipScr.SelAttrDetail[0], Length(ClipScr.SelAttrDetail));
              CurScreen^.SelActive := True;
              CurScreen^.SelWidth := ClipScr.SelWidth;
              CurScreen^.SelHeight := ClipScr.SelHeight;
              CurScreen^.SelOrigin.x := MouseX;
              CurScreen^.SelOrigin.y := MouseY;
              RenderScreen(CurScreenIndex, ShowingAttrs);
              MakeUndo('Paste Selection', True);
           End;
        End;

     47: // Paste Transparent
        Begin
           If ClipScr.SelActive Then Begin
              ClearSelection;
              SetLength(CurScreen^.SelMask, Length(ClipScr.SelMask));
              SetLength(CurScreen^.SelDetail, Length(ClipScr.SelDetail));
              SetLength(CurScreen^.SelAttrDetail, Length(ClipScr.SelAttrDetail));
              CopyMemory(@CurScreen^.SelMask[0], @ClipScr.SelMask[0], Length(ClipScr.SelMask));
              CopyMemory(@CurScreen^.SelDetail[0], @ClipScr.SelDetail[0], Length(ClipScr.SelDetail));
              CopyMemory(@CurScreen^.SelAttrDetail[0], @ClipScr.SelAttrDetail[0], Length(ClipScr.SelAttrDetail));
              CurScreen^.SelActive := True;
              CurScreen^.SelWidth := ClipScr.SelWidth;
              CurScreen^.SelHeight := ClipScr.SelHeight;
              CurScreen^.SelOrigin.x := MouseX;
              CurScreen^.SelOrigin.y := MouseY;
              MakeSelectionTransparent(CurScreenIndex);
              RenderScreen(CurScreenIndex, ShowingAttrs);
              MakeUndo('Paste Transparent', True);
           End;
        End;

     12: // Clear
        Begin
           SetLength(CurScreen^.SelMask, 0);
           SetLength(CurScreen^.SelDetail, 0);
           SetLength(CurScreen^.SelAttrDetail, 0);

           CurScreen^.SelOrigin := Point(0, 0);
           CurScreen^.SelWidth := 0;
           CurScreen^.SelHeight := 0;
           CurScreen^.SelActive := False;
           CurScreen^.SelActive := False;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Clear Selection', True);
        End;

     13: // Paint Pixels on/off
        Begin
           If ColourMode = cmBoth Then
              ColourMode := cmAttrs
           Else
              If ColourMode = cmAttrs Then
                 ColourMode := cmBoth
              Else
                 If ColourMode = cmPixels Then
                    ColourMode := cmAttrs;
        End;

     14: // Paint Attributes on/off
        Begin
           If ColourMode = cmBoth Then
              ColourMode := cmPixels
           Else
              If ColourMode = cmAttrs Then
                 ColourMode := cmPixels
              Else
                 If ColourMode = cmPixels Then
                    ColourMode := cmBoth;
        End;

     15: // Grid
        Begin

           ShowGrid := Not ShowGrid;
           SpeedButton9.Down := ShowGrid;
           RenderScreen(CurScreenIndex, ShowingAttrs);

        End;

     16: // Set grid size
        Begin

           GridSetUpWindow.EditType := 1;
           GridSetUpWindow.EditWidth := GridX;
           GridSetupWindow.EditHeight := GridY;
           CentreFormOnForm(GridSetupWindow, Self);
           ShowWindow(GridSetupWindow, True);
           If Not GridSetUpWindow.Cancelled Then Begin
              GridX := GridSetUpWindow.EditWidth;
              GridY := GridSetUpWindow.EditHeight;
              RenderScreen(CurScreenIndex, ShowingAttrs);
           End;
           MouseIsDown := False;

        End;

     17: // Snap to grid
        Begin

           SnapToGrid := Not SnapToGrid;
           SpeedButton10.Down := SnapToGrid;

        End;

     18: // Zoom in
        Begin
           ToolButton10Click(Nil);
        End;

     19: // Zoom Out
        Begin
           ToolButton11Click(Nil);
        End;

     20: // Optimise for Tape
        Begin

           OptimiseForTape(CurScreenIndex);
           RenderScreen(CurScreenIndex, ShowingAttrs);
           CurScreen^.Changed := True;
           MakeUndo('Optimise for tape', True);

        End;

     21: // Show Attrs
        Begin
           ShowingAttrs := Not ShowingAttrs;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           SpeedButton11.Down := ShowingAttrs;
        End;

     22: // Show Brush Shapes
        Begin
           ShowBrushShapes := Not ShowBrushShapes;
           SpeedButton11.Down := ShowBrushShapes;
           RenderScreen(CurScreenIndex, ShowingAttrs);
        End;

     23: // Image Flip
        Begin
           ImageFlip(CurScreen);
           CurScreen^.Changed := True;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Flip Image', True);
        End;

     24: // Image Mirror
        Begin
           ImageMirror(CurScreen);
           CurScreen^.Changed := True;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Mirror Image', True);
        End;

     26: // Invert pixels
        Begin

           FlipPixels(CurScreen);
           CurScreen^.Changed := True;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Invert pixels', True);

        End;

     27: // Attrs Invert
        Begin

           FlipAttrs(CurScreen);
           CurScreen^.Changed := True;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Invert INK/PAPER', True);

        End;

     28: // Remove BRIGHT attrs
        Begin

           RemoveATTR(CurScreen, 64);
           CurScreen^.Changed := True;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Remove BRIGHT', True);

        End;

     29: // Remove FLASH attrs
        Begin

           RemoveATTR(CurScreen, 128);
           CurScreen^.Changed := True;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Remove FLASH', True);

        End;

     37: // Select all
        Begin
           If CurScreen^.SelActive Then
              ClearSelection;
           SetSelectionRect(0, 0, 256, 192, mtNone);
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Select All', True);
        End;

     38: // Select None
        Begin
           ClearSelection;
           RenderScreen(CurScreenIndex, ShowingAttrs);
           MakeUndo('Select None', True);
        End;

     39: // Selection Load
        Begin
           If CurScreen^.SelActive Then
              ClearSelection;
           LoadSelection(CurScreen);
        End;

     40: // Selection Save
        Begin
           If CurScreen^.SelActive Then
              SaveSelection(CurScreen);
        End;

     41: // Selection Expand
        Begin
           ExpandSelection(CurScreenIndex);
           MakeUndo('Expand Selection', True);
        End;

     42: // Selection Contract
        Begin
           ShrinkSelection(CurScreenIndex);
           MakeUndo('Shrink Selection', True);
        End;

     43: // Selection Make Transparent
        Begin
           MakeSelectionTransparent(CurScreenIndex);
           MakeUndo('Transparent Selection', True);
        End;

     44: // Show Marquee
        Begin
           SelectionMarquee := Not SelectionMarquee;
           RenderScreen(CurScreenIndex, ShowingAttrs);
        End;

     45: // Help Contents
        Begin
           BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/paintbox.html'), HH_DISPLAY_TOPIC, 0);
        End;

     48: // Paint with all attrs
        Begin
           AttrFlags := [pwINK] + [pwPAPER] + [pwBRIGHT] + [pwFLASH];
           DrawPalette;
        End;

     49: // INK
        Begin
           AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwINK)));
           CheckColourMode;
           DrawPalette;
        End;

     50: // PAPER
        Begin
           AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwPAPER)));
           CheckColourMode;
           DrawPalette;
        End;

     51: // BRIGHT
        Begin
           AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwBRIGHT)));
           CheckColourMode;
           DrawPalette;
        End;

     52: // FLASH
        Begin
           AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwFLASH)));
           CheckColourMode;
           DrawPalette;
        End;

     53: // Copy bitmap
        Begin

           CopyToClipboard;

        End;

     54: // Paste Bitmap
        Begin

           PasteFromClipboard;

        End;

     55: // Set custom selection
        Begin

           GridSetUpWindow.EditType := 2;
           If CurScreen^.SelActive Then Begin
              GridSetUpWindow.EditWidth := CurScreen^.SelWidth;
              GridSetupWindow.EditHeight := CurScreen^.SelHeight;
              GridSetupWindow.EditLeft := CurScreen^.SelOrigin.X;
              GridSetupWindow.EditTop := CurScreen^.SelOrigin.Y;
           End Else Begin
              GridSetUpWindow.EditWidth := 0;
              GridSetupWindow.EditHeight := 0;
              GridSetupWindow.EditLeft := 0;
              GridSetupWindow.EditTop := 0;
           End;
           CentreFormOnForm(GridSetupWindow, Self);
           ShowWindow(GridSetupWindow, True);
           If Not GridSetUpWindow.Cancelled Then Begin
              If TMergeType(GridSetupWindow.ComboBox1.ItemIndex) in [mtAdd, mtXOR, mtSubtract] Then
                 CopyScreen(CurScreen^, LastSelection);
              ClearSelection;
              SetSelectionRect(GridSetUpWindow.EditLeft, GridSetupWindow.EditTop, GridSetupWindow.EditWidth + GridSetupWindow.EditLeft, GridSetupWindow.EditHeight + GridSetupWindow.EditTop, TMergeType(GridSetupWindow.ComboBox1.ItemIndex));
              RenderScreen(CurScreenIndex, ShowingAttrs);
              MakeUndo('Set Selection', True);
              FastIMG1.Repaint;
           End;
           MouseIsDown := False;

        End;

     57: // Send to current Screen
        Begin

           TempStr := ScreenToSCR(CurScreenIndex);
           CopyMemory(@Memory[16384], @TempStr[1], 6912);
           If DisplayWindow.Showing Then Begin
              UpdateDisplay;
              UpdateBASinDisplay;
           End;
           NeedDisplayUpdate := False;

        End;

     58: // Send to tape
        Begin

           If Not TapeWindow.Showing Then ShowWindow(TapeWindow, False);
           TempStr := ScreenToSCR(CurScreenIndex);
           FileHeader := #$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80;
           SetLength(FileArray, 6912);
           CopyMemory(@FileArray[0], @TempStr[1], 6912);
           TapeBlockAdd(CODEToTape('Unnamed', 16384));
           TapeWindow.UpdateTapeList;

        End;

     59: // Mouse cursor - windows
        Begin

           Opt_MouseImage := miWindows;

        End;

     60: // Mouse cursor - Crosshair
        Begin

           Opt_MouseImage := miCrosshair;

        End;

     61: // Mouse cursor - None. Enables BrushShapes.
        Begin

           Opt_MouseImage := miNone;
           ShowBrushShapes := True;
           SpeedButton11.Down := ShowBrushShapes;
           RenderScreen(CurScreenIndex, ShowingAttrs);

        End;


     62: // Pack
        Begin

           TempStr := ScreenToSCR(CurScreenIndex);

           // Einar Saukas' ZX0 compressor




           //Push compressed data to memory
           CopyMemory(@Memory[16384], @TempStr[1], 6912);

        End;



     101..110: // Recent Files
        Begin
           Filename := RecentPics[(Sender As TComponent).Tag-101];
           Open1Click(nil);
        End;

  End;

end;

Procedure TScrPaintForm.MakeRecentList;
Var
  F: Integer;
  ItemCaption: String;
  MenuItem: TMenuItem;
Label
  MRUBuild;
Begin

MRUBuild:

  MenuItem := nil;
  Item11.Visible := False;
  Item21.Visible := False;
  Item31.Visible := False;
  Item41.Visible := False;
  Item51.Visible := False;
  Item61.Visible := False;
  Item71.Visible := False;
  Item81.Visible := False;
  Item91.Visible := False;
  Item101.Visible := False;
  If RecentPics.Count > 0 then Begin
     Recent1.Enabled := True;
     For F := 0 To 9 Do Begin
        Case F of
           0: MenuItem := Item11;
           1: MenuItem := Item21;
           2: MenuItem := Item31;
           3: MenuItem := Item41;
           4: MenuItem := Item51;
           5: MenuItem := Item61;
           6: MenuItem := Item71;
           7: MenuItem := Item81;
           8: MenuItem := Item91;
           9: MenuItem := Item101;
        End;
        If F < RecentPics.Count then Begin
           ItemCaption := RecentPics[F];
           If FileExists(ItemCaption) Then Begin
              MenuItem.Caption := '&'+IntToStr(F+1)+' '+ShrinkFileName(ItemCaption, 200);
              MenuItem.Visible := True;
           End Else Begin
              RecentPics.Delete(F);
              Goto MRUBuild;
           End;
        End Else
           MenuItem.Visible := False;
     End;
  End Else
     Recent1.Enabled := False;
End;

Procedure TScrPaintForm.AddToRecent(Filename: String);
Begin
  If Lowercase(ExtractFileExt(Filename)) <> '.scr' Then Exit;
  If RecentPics.IndexOf(Filename) <> -1 Then RecentPics.Delete(RecentPics.IndexOf(Filename));
  RecentPics.Insert(0, FileName);
  While RecentPics.Count > 10 Do RecentPics.Delete(RecentPics.Count -1);
End;

procedure TScrPaintForm.OnEnterMenuLoop(var Message: TMessage);
Begin

  MakeRecentList;
  Save1.Enabled := CurScreen^.Changed;
  Close2.Enabled := NumScreens > 1;
  Cut1.Enabled := CurScreen^.SelActive;
  Copy1.Enabled := CurScreen^.SelActive;
  Paste1.Enabled := ClipScr.SelActive;
  PasteTransparent1.Enabled := ClipScr.SelActive;
  Clear1.Enabled := CurScreen^.SelActive;
  SelectNone1.Enabled := CurScreen^.SelActive;
  Load1.Enabled := CurScreen^.SelActive;
  SaveAs2.Enabled := CurScreen^.SelActive;
  Expand1.Enabled := CurScreen^.SelActive;
  Contract1.Enabled := CurScreen^.SelActive;
  MakeTransparent1.Enabled := CurScreen^.SelActive;

  Pixels1.Checked := ColourMode in [cmBoth, cmPixels];
  ShowAttributes1.Checked := ShowingAttrs;
  ShowBrushShapes1.Checked := ShowBrushShapes;
  SnapToGrid1.Checked := SnapToGrid;
  Grid1.Checked := ShowGrid;

  Ink1.Checked := pwINK in AttrFlags;
  Paper1.Checked := pwPAPER in AttrFlags;
  Bright1.Checked := pwBRIGHT in AttrFlags;
  Flash1.Checked := pwFLASH in AttrFlags;

  WindowsPointer1.Checked := Opt_MouseImage = miWindows;
  Crosshair1.Checked := Opt_MouseImage = miCrosshair;
  None1.Checked := Opt_MouseImage = miNone;

  If CurScreen^.Undo.Count > 0 Then Begin
     Undo1.Enabled := True;
     SpeedButton14.Enabled := True;
     Undo1.Caption := 'Undo '+Copy(CurScreen^.Undo[CurScreen^.Undo.Count -1], 1, Pos(#255, CurScreen^.Undo[CurScreen^.Undo.Count -1]) -1);
  End Else Begin
     Undo1.Caption := 'Undo';
     Undo1.Enabled := False;
     SpeedButton14.Enabled := False;
  End;

  If CurScreen^.Redo.Count > 0 Then Begin
     Redo1.Enabled := True;
     SpeedButton15.Enabled := True;
     Redo1.Caption := 'Redo '+Copy(CurScreen^.Redo[CurScreen^.Redo.Count -1], 1, Pos(#255, CurScreen^.Redo[CurScreen^.Redo.Count -1]) -1);
  End Else Begin
     Redo1.Caption := 'Redo';
     Redo1.Enabled := False;
     SpeedButton15.Enabled := False;
  End;

  ShowMarquee1.Checked := SelectionMarquee;
  PasteBitmap1.Enabled := Clipboard.HasFormat(CF_BITMAP);

End;

Procedure TScrPaintForm.BeginText(X, Y, Mx, My: Integer);
Begin

  Inc(My, 8);

  TextForm.TextX := X;
  TextForm.TextY := Y;

  ToolButton1.Enabled := False;
  ToolButton2.Enabled := False;
  ToolButton4.Enabled := False;
  ToolButton5.Enabled := False;
  ToolButton6.Enabled := False;
  ToolButton8.Enabled := False;

  PaintMode := pmText;

  TextForm.SetBounds(FastIMG1.ClientToScreen(Point(Mx, My)).X, FastIMG1.ClientToScreen(Point(Mx, My)).Y, TextForm.Width, TextForm.Height);
  ShowWindow(TextForm, False);

End;

procedure TScrPaintForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  Timer1.Enabled := False;
  If TextForm.Showing or TextForm.Visible Then
     TextForm.Close;

end;

Procedure TScrPaintForm.PaintAttribFlags;
Var
  Idx: Integer;
  Highlight: Boolean;
Begin

  FastIMG4.Bmp.SetSize(FastIMG4.Width, FastIMG4.Height, 32);
  FastIMG4.Bmp.Clear(FRGBn(DisplayPalette[15]));
  FastDrawEx.Rectangle32(FastIMG4.bmp, 0, 0, 41, 11, TFBLack);
  FastIMG4.Bmp.Pixels32[0, 0] := FRGBA(0, 0, 0, 0);

  For Idx := 0 To 3 Do Begin
     Case Idx of
        0: Highlight := pwINK in AttrFlags;
        1: Highlight := pwPAPER in AttrFlags;
        2: Highlight := pwBRIGHT in AttrFlags;
        3: Highlight := pwFLASH in AttrFlags;
     End;
     If Highlight Then
        SpecTextToDIB(FastIMG4.Bmp, (Idx * 10)+1, 3, 'IPBF'[Idx + 1], 0, -1, 1, False, False)
     Else
        SpecTextToDIB(FastIMG4.Bmp, (Idx * 10)+1, 3, 'IPBF'[Idx + 1], 7, -1, 0, False, False);
  End;

  FastIMG4.Repaint;

End;

procedure TScrPaintForm.FastIMG4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  If X < 11 Then
     AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwINK)))
  Else
     If X < 21 Then
        AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwPAPER)))
     Else
        If X < 31 Then
           AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwBRIGHT)))
        Else
           AttrFlags := TAttributeFlags(Byte(AttrFlags) Xor (1 Shl Byte(pwFLASH)));

  PaintAttribFlags;
  CheckColourMode;

end;

procedure TScrPaintForm.TabControl1Change(Sender: TObject);
begin

  CurScreen := Screens[TabControl1.TabIndex];
  CurScreenIndex := TabControl1.TabIndex;
  CopyScreen(CurScreen^, UndoScreen);

  If Not (fsShowing in FormState) Then
     RenderScreen(CurScreenIndex, ShowingAttrs);

  MakePreview;

  Caption := EDITORNAME + TabControl1.Tabs[CurScreenIndex] + '  [' + IntToStr(CurScreen^.ZoomLevel) + ':1]';

end;

Procedure TScrPaintForm.MakeUndo(Caption: String; ClearRedo: Boolean);
Var
  TempStr, NewStr, OldStr, UndoStr: String;
  ImageChanged: Boolean;
Begin

  UndoStr := '';
  ImageChanged := False;

  SetLength(NewStr, 256 * 192);
  SetLength(OldStr, 256 * 192);
  CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.InkDIB[0, 0], 49152);
  CopyMemory(@OldStr[1], @UndoScreen.InkDIB[0, 0], 49152);
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #1 + TempStr;
     ImageChanged := True;
  End;

  SetLength(NewStr, Length(Screens[CurScreenIndex]^.SelDetail));
  SetLength(OldStr, Length(UndoScreen.SelDetail));
  If NewStr <> '' Then CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.SelDetail[0], Length(Screens[CurScreenIndex]^.SelDetail));
  If OldStr <> '' Then CopyMemory(@OldStr[1], @UndoScreen.SelDetail[0], Length(UndoScreen.SelDetail));
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #2 + TempStr;
     ImageChanged := True;
  End;

  SetLength(NewStr, Length(Screens[CurScreenIndex]^.SelAttrDetail));
  SetLength(OldStr, Length(UndoScreen.SelAttrDetail));
  If NewStr <> '' Then CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.SelAttrDetail[0], Length(Screens[CurScreenIndex]^.SelAttrDetail));
  If OldStr <> '' Then CopyMemory(@OldStr[1], @UndoScreen.SelAttrDetail[0], Length(UndoScreen.SelAttrDetail));
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #3 + TempStr;
     ImageChanged := True;
  End;

  SetLength(NewStr, Length(Screens[CurScreenIndex]^.SelMask));
  SetLength(OldStr, Length(UndoScreen.SelMask));
  If NewStr <> '' Then CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.SelMask[0], Length(Screens[CurScreenIndex]^.SelMask));
  If OldStr <> '' Then CopyMemory(@OldStr[1], @UndoScreen.SelMask[0], Length(UndoScreen.SelMask));
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #4 + TempStr;
     ImageChanged := True;
  End;

  SetLength(NewStr, 768);
  SetLength(OldStr, 768);

  CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.Attrs[0, 0], 768);
  CopyMemory(@OldStr[1], @UndoScreen.Attrs[0, 0], 768);
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #5 + TempStr;
     ImageChanged := True;
  End;

  If Screens[CurScreenIndex]^.SelActive <> UndoScreen.SelActive Then UndoStr := UndoStr + #6 + Chr(Byte(UndoScreen.SelActive));
  If Screens[CurScreenIndex]^.SelOrigin.X <> UndoScreen.SelOrigin.X Then Begin ImageChanged := True; UndoStr := UndoStr + #7 + Chr(UndoScreen.SelOrigin.X And 255) + Chr(UndoScreen.SelOrigin.X Shr 8); End;
  If Screens[CurScreenIndex]^.SelOrigin.Y <> UndoScreen.SelOrigin.Y Then Begin IMageChanged := True; UndoStr := UndoStr + #8 + Chr(UndoScreen.SelOrigin.Y And 255) + Chr(UndoScreen.SelOrigin.Y Shr 8); End;
  If Screens[CurScreenIndex]^.SelWidth <> UndoScreen.SelWidth Then UndoStr := UndoStr + #9 + Chr(UndoScreen.SelWidth And 255) + Chr(UndoScreen.SelWidth Shr 8);
  If Screens[CurScreenIndex]^.SelHeight <> UndoScreen.SelHeight Then UndoStr := UndoStr + #10 + Chr(UndoScreen.SelHeight And 255) + Chr(UndoScreen.SelHeight Shr 8);

  If UndoStr <> '' Then Begin
     Screens[CurScreenIndex]^.Undo.Add(Caption + #255 + UndoStr);
     CopyScreen(CurScreen^, UndoScreen);
     If ClearRedo Then Begin
        CurScreen^.Redo.Clear;
     End;
  End;

  If ImageChanged Then Begin
     MakePreview;
  End;

End;

Procedure TScrPaintForm.MakeRedo(Caption: String);
Var
  TempStr, NewStr, OldStr, UndoStr: String;
  ImageChanged: Boolean;
Begin

  UndoStr := '';
  ImageChanged := False;

  SetLength(NewStr, 256 * 192);
  SetLength(OldStr, 256 * 192);
  CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.InkDIB[0, 0], 49152);
  CopyMemory(@OldStr[1], @UndoScreen.InkDIB[0, 0], 49152);
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #1 + TempStr;
     ImageChanged := True;
  End;

  SetLength(NewStr, Length(Screens[CurScreenIndex]^.SelDetail));
  SetLength(OldStr, Length(UndoScreen.SelDetail));
  If NewStr <> '' Then CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.SelDetail[0], Length(Screens[CurScreenIndex]^.SelDetail));
  If OldStr <> '' Then CopyMemory(@OldStr[1], @UndoScreen.SelDetail[0], Length(UndoScreen.SelDetail));
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #2 + TempStr;
     ImageChanged := True;
  End;

  SetLength(NewStr, Length(Screens[CurScreenIndex]^.SelAttrDetail));
  SetLength(OldStr, Length(UndoScreen.SelAttrDetail));
  If NewStr <> '' Then CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.SelAttrDetail[0], Length(Screens[CurScreenIndex]^.SelAttrDetail));
  If OldStr <> '' Then CopyMemory(@OldStr[1], @UndoScreen.SelAttrDetail[0], Length(UndoScreen.SelAttrDetail));
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #3 + TempStr;
     ImageChanged := True;
  End;

  SetLength(NewStr, Length(Screens[CurScreenIndex]^.SelMask));
  SetLength(OldStr, Length(UndoScreen.SelMask));
  If NewStr <> '' Then CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.SelMask[0], Length(Screens[CurScreenIndex]^.SelMask));
  If OldStr <> '' Then CopyMemory(@OldStr[1], @UndoScreen.SelMask[0], Length(UndoScreen.SelMask));
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then UndoStr := UndoStr + #4 + TempStr;

  SetLength(NewStr, 768);
  SetLength(OldStr, 768);

  CopyMemory(@NewStr[1], @Screens[CurScreenIndex]^.Attrs[0, 0], 768);
  CopyMemory(@OldStr[1], @UndoScreen.Attrs[0, 0], 768);
  TempStr := GetDiff(OldStr, NewStr);
  If TempStr <> '' Then Begin
     UndoStr := UndoStr + #5 + TempStr;
     ImageChanged := True;
  End;

  If Screens[CurScreenIndex]^.SelActive <> UndoScreen.SelActive Then UndoStr := UndoStr + #6 + Chr(Byte(UndoScreen.SelActive));
  If Screens[CurScreenIndex]^.SelOrigin.X <> UndoScreen.SelOrigin.X Then Begin ImageChanged := True; UndoStr := UndoStr + #7 + Chr(UndoScreen.SelOrigin.X And 255) + Chr(UndoScreen.SelOrigin.X Shr 8); End;
  If Screens[CurScreenIndex]^.SelOrigin.Y <> UndoScreen.SelOrigin.Y Then Begin IMageChanged := True; UndoStr := UndoStr + #8 + Chr(UndoScreen.SelOrigin.Y And 255) + Chr(UndoScreen.SelOrigin.Y Shr 8); End;
  If Screens[CurScreenIndex]^.SelWidth <> UndoScreen.SelWidth Then UndoStr := UndoStr + #9 + Chr(UndoScreen.SelWidth And 255) + Chr(UndoScreen.SelWidth Shr 8);
  If Screens[CurScreenIndex]^.SelHeight <> UndoScreen.SelHeight Then UndoStr := UndoStr + #10 + Chr(UndoScreen.SelHeight And 255) + Chr(UndoScreen.SelHeight Shr 8);

  If UndoStr <> '' Then Begin
     Screens[CurScreenIndex]^.Redo.Add(Caption + #255 + UndoStr);
     CopyScreen(CurScreen^, UndoScreen);
  End;

End;

Procedure TScrPaintForm.PerformUndo(UndoString: String; Var Screen: pScrImage);
Var
  Idx, Idx2: Integer;
  Reps, Offset, Len: Word;
  Ptr: pByte;
Begin

  Idx := Pos(#255, UndoString) +1;

  While Idx < Length(UndoString) Do Begin

     Case UndoString[Idx] of
        #1..#5:
           Begin
              Idx2 := Idx +1;
              Reps := GetWord(@UndoString[Idx2]);
              Inc(Idx2, 2);
              If Reps <> 0 Then Begin
                 While Reps > 0 Do Begin
                    Offset := GetWord(@UndoString[Idx2]);
                    Inc(Idx2, 2);
                    Len := GetWord(@UndoString[Idx2]);
                    Inc(Idx2, 2);
                    If Len > 0 Then Begin
                       Case UndoString[Idx] of
                          #1: Ptr := @Screen^.InkDIB[0, 0];
                          #2: Ptr := @Screen^.SelDetail[0];
                          #3: Ptr := @Screen^.SelAttrDetail[0];
                          #4: Ptr := @Screen^.SelMask[0];
                          #5: Ptr := @Screen^.Attrs[0];
                       End;
                       Inc(Ptr, Offset -1);
                    End;
                    While Len > 0 Do Begin
                       Ptr^ := Ord(UndoString[Idx2]);
                       Inc(Ptr);
                       Dec(Len);
                       Inc(Idx2);
                    End;
                    Dec(Reps);
                 End;
              End Else Begin
                 Len := GetDWord(@UndoString[Idx2]);
                 Inc(Idx2, 4);
                 If Len > 0 Then Begin
                    Case UndoString[Idx] of
                       #1: Ptr := @Screen^.InkDIB[0, 0];
                       #2: Begin
                             SetLength(Screen^.SelDetail, Len);
                             Ptr := @Screen^.SelDetail[0];
                          End;
                       #3: Begin
                             SetLength(Screen^.SelAttrDetail, Len);
                             Ptr := @Screen^.SelAttrDetail[0];
                          End;
                       #4: Begin
                             SetLength(Screen^.SelMask, Len);
                             Ptr := @Screen^.SelMask[0];
                          End;
                       #5: Ptr := @Screen^.Attrs[0];
                    End;
                    CopyMemory(Ptr, @UndoString[Idx2], Len);
                 End;
                 Inc(Idx2, Len);
              End;
              Idx := Idx2;
           End;
        #6:
           Begin
              Inc(Idx);
              Screen^.SelActive := Boolean(UndoString[Idx]);
              Inc(Idx);
           End;
        #7:
           Begin
              Inc(Idx);
              Screen^.SelOrigin.X := GetWord(@UndoString[Idx]);
              Inc(Idx, 2);
           End;
        #8:
           Begin
              Inc(Idx);
              Screen^.SelOrigin.Y := GetWord(@UndoString[Idx]);
              Inc(Idx, 2);
           End;
        #9:
           Begin
              Inc(Idx);
              Screen^.SelWidth := GetWord(@UndoString[Idx]);
              Inc(Idx, 2);
           End;
        #10:
           Begin
              Inc(Idx);
              Screen^.SelHeight := GetWord(@UndoString[Idx]);
              Inc(Idx, 2);
           End;
     End;

  End;

  MakePreview;

End;

Function TScrPaintForm.GetDiff(OldStr, NewStr: String): String;
Var
  Idx, StrLen, StartPos, CutLen, NumDiffs: Integer;
Begin

  NumDiffs := 0;
  StrLen := Length(OldStr);
  Result := '  ';
  Idx := 1;

  If Length(NewStr) <> StrLen Then Begin

     Result := #0#0+'    '+OldStr;
     PutDWord(@Result[3], Length(OldStr));

  End Else Begin

     // Search for a changed byte
     While (Idx < StrLen +1) And (OldStr[Idx] = NewStr[Idx]) Do Inc(Idx);
     If Idx <> StrLen +1 Then Begin
        // Add the position of the first changed byte
        Result := Result + Chr(Idx And 255) + Chr(Idx Shr 8);
        StartPos := Idx;
        Idx := StrLen;
        While (Idx > StartPos) And (OldStr[Idx] = NewStr[Idx]) Do Dec(Idx);
        CutLen := (Idx - StartPos) +1;
        Result := Result + Chr(CutLen and 255) + Chr(CutLen Shr 8) + Copy(OldStr, StartPos, CutLen);
        Inc(NumDiffs);
     End;

     If NumDiffs = 0 Then
        Result := ''
     Else Begin
        Result[1] := Chr(NumDiffs And 255);
        Result[2] := Chr(NumDiffs Shr 8);
     End;

  End;

End;

Procedure TScrPaintForm.CheckColourMode;
Begin

  If AttrFlags = [] Then
     ColourMode := cmPixels
  Else
     If ColourMode = cmPixels Then
        ColourMode := cmBoth;

End;

Procedure TScrPaintForm.SaveSelection(Screen: pScrImage);
Var
  X, Y, DataLen: Integer;
  TempStr: String;
  FTypes: TBASICFiles;
Begin

  FTypes := [FTSelection];
  If Filename = '' Then Filename := OpenFile(Handle, 'Save Selection as...', FTypes, '', True, False);
  If Filename = '' Then Exit;

  TempStr := Chr(Screen^.SelOrigin.X and 255) + Chr(Screen^.SelOrigin.X Shr 8) +
             Chr(Screen^.SelOrigin.Y and 255) + Chr(Screen^.SelOrigin.Y Shr 8) +
             Chr(Screen^.SelWidth and 255) + Chr(Screen^.SelWidth Shr 8) +
             Chr(Screen^.SelHeight and 255) + Chr(Screen^.SelHeight Shr 8);

  For X := 0 To Screen^.SelWidth -1 Do
     For Y := 0 To Screen^.SelHeight -1 Do
        TempStr := TempStr + Chr(Screen^.SelMask[X + (Screen^.SelWidth * Y)]) + Chr(Screen^.SelDetail[X + (Screen^.SelWidth * Y)]) + Chr(Screen^.SelAttrDetail[X + (Screen^.SelWidth * Y)]);

  DataLen := Length(TempStr);
  SetLength(FileBody, DataLen);
  CopyMemory(@FileBody[1], @TempStr[1], DataLen);
  SaveFile;

End;

Procedure TScrPaintForm.LoadSelection(Screen: pScrImage);
Var
  X, Y, Idx: Integer;
Begin

  Filename := OpenFile(Handle, 'Load Selection', [FTSelection], '', False, False);
  If Filename = '' Then Exit;
  If GetFile('.bsl') <> 'Ok' Then Exit;

  Screen^.SelOrigin.X := GetWord(@FileArray[0]);
  Screen^.SelOrigin.Y := GetWord(@FileArray[2]);
  Screen^.SelWidth := GetWord(@FileArray[4]);
  Screen^.SelHeight := GetWord(@FileArray[6]);

  SetLength(Screen^.SelMask, Screen^.SelWidth * Screen^.SelHeight);
  SetLength(Screen^.SelDetail, Screen^.SelWidth * Screen^.SelHeight);
  SetLength(Screen^.SelAttrDetail, Screen^.SelWidth * Screen^.SelHeight);

  Idx := 8;
  For X := 0 To Screen^.SelWidth -1 Do
     For Y := 0 To Screen^.SelHeight -1 Do Begin
        Screen^.SelMask[X + (Screen^.SelWidth * Y)] := FileArray[Idx];
        Screen^.SelDetail[X + (Screen^.SelWidth * Y)] := FileArray[Idx +1];
        Screen^.SelAttrDetail[X + (Screen^.SelWidth * Y)] := FileArray[Idx +2];
        Inc(Idx, 3);
     End;

  Screen^.SelActive := True;
  MakeUndo('Load Selection', True);
  RenderScreen(CurScreenIndex, ShowingAttrs);

End;

Procedure TScrPaintForm.FlipPixels(Screen: pScrImage);
Var
  X, Y: Integer;
Begin

  If Screen^.SelActive Then Begin

     For X := 0 To Screen^.SelWidth -1 Do
        For Y := 0 To Screen^.SelHeight -1 Do
           If Screen^.SelMask[X + (Y * Screen^.SelWidth)] = 1 Then
              Screen^.SelDetail[X + (Y * Screen^.SelWidth)] := 1 - Screen^.SelDetail[X + (Y * Screen^.SelWidth)];

  End Else Begin

     For X := 0 To 255 Do
        For Y := 0 to 191 Do
           Screen^.InkDIB[X, Y] := 1 - Screen^.InkDIB[X, Y];

  End;

End;

Procedure TScrPaintForm.FlipAttrs(Screen: pScrImage);
Var
  X, Y: Integer;
Begin

  If Screen^.SelActive Then Begin

     For X := 0 To Screen^.SelWidth -1 Do
        For Y := 0 To Screen^.SelHeight -1 Do
           If Screen^.SelMask[X + (Y * Screen^.SelWidth)] = 1 Then
              Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] := (Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] and 192) +
                                                                  ((Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] and 7) Shl 3) +
                                                                  ((Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] and 56) Shr 3);

  End Else Begin

     For X := 0 To 31 Do
        For Y := 0 to 23 Do
           Screen^.Attrs[X, Y] := (Screen^.Attrs[X, Y] And 192) + ((Screen^.Attrs[X, Y] And 7) Shl 3) + ((Screen^.Attrs[X, Y] And 56) Shr 3);

  End;

End;

Procedure TScrPaintForm.RemoveAttr(Screen: pScrImage; Mask: Byte);
Var
  X, Y: Integer;
Begin

  If Screen^.SelActive Then Begin

     For X := 0 To Screen^.SelWidth -1 Do
        For Y := 0 To Screen^.SelHeight -1 Do
           If Screen^.SelMask[X + (Y * Screen^.SelWidth)] = 1 Then
              Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] := Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] - (Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] and Mask);

  End Else Begin

     For X := 0 To 31 Do
        For Y := 0 to 23 Do
           Screen^.Attrs[X, Y] := Screen^.Attrs[X, Y] - (Screen^.Attrs[X, Y] And Mask);

  End;

End;

Procedure TScrPaintForm.ImageFlip(Screen: pScrImage);
Var
  Buffer: Array[0..255, 0..191] of Byte;
  UnBuffer: Array Of Byte;
  X, Y: Integer;
Begin

  If Screen^.SelActive Then Begin

     SetLength(UnBuffer, Screen^.SelWidth * Screen^.SelHeight);

     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do UnBuffer[X + (((Screen^.SelHeight -1) - Y) * Screen^.SelWidth)] := Screen^.SelMask[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do Screen^.SelMask[X + (Y * Screen^.SelWidth)] := UnBuffer[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do UnBuffer[X + (((Screen^.SelHeight -1) - Y) * Screen^.SelWidth)] := Screen^.SelDetail[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do Screen^.SelDetail[X + (Y * Screen^.SelWidth)] := UnBuffer[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do UnBuffer[X + (((Screen^.SelHeight -1) - Y) * Screen^.SelWidth)] := Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] := UnBuffer[X + (Y * Screen^.SelWidth)];

  End Else Begin

     For X := 0 To 255 Do For Y := 0 To 191 Do Buffer[X, 191-Y] := Screen^.InkDIB[X, Y];
     For X := 0 To 255 Do For Y := 0 To 191 Do Screen^.InkDIB[X, Y] := Buffer[X, Y];
     For X := 0 To 31 Do For Y := 0 To 23 Do Buffer[X, 23-Y] := SCreen^.Attrs[X, Y];
     For X := 0 To 31 Do For Y := 0 To 23 Do Screen^.Attrs[X, Y] := Buffer[X, Y];

  End;

End;

Procedure TScrPaintForm.ImageMirror(Screen: pScrImage);
Var
  Buffer: Array[0..255, 0..191] of Byte;
  UnBuffer: Array of Byte;
  X, Y: Integer;
Begin

  If Screen^.SelActive Then Begin

     SetLength(UnBuffer, Screen^.SelWidth * Screen^.SelHeight);

     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do UnBuffer[(Screen^.SelWidth -1) - X + (Y * Screen^.SelWidth)] := Screen^.SelMask[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do Screen^.SelMask[X + (Y * Screen^.SelWidth)] := UnBuffer[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do UnBuffer[(Screen^.SelWidth -1) - X + (Y * Screen^.SelWidth)] := Screen^.SelDetail[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do Screen^.SelDetail[X + (Y * Screen^.SelWidth)] := UnBuffer[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do UnBuffer[(Screen^.SelWidth -1) - X + (Y * Screen^.SelWidth)] := Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)];
     For X := 0 To Screen^.SelWidth -1 Do For Y := 0 To Screen.SelHeight -1 Do Screen^.SelAttrDetail[X + (Y * Screen^.SelWidth)] := UnBuffer[X + (Y * Screen^.SelWidth)];

  End Else Begin

     For X := 0 To 255 Do For Y := 0 To 191 Do Buffer[255 - X, Y] := Screen^.InkDIB[X, Y];
     For X := 0 To 255 Do For Y := 0 To 191 Do Screen^.InkDIB[X, Y] := Buffer[X, Y];
     For X := 0 To 31 Do For Y := 0 To 23 Do Buffer[31 - X, Y] := SCreen^.Attrs[X, Y];
     For X := 0 To 31 Do For Y := 0 To 23 Do Screen^.Attrs[X, Y] := Buffer[X, Y];

  End;

End;

procedure TScrPaintForm.CheckBox1Click(Sender: TObject);
begin

  If Sender = Filled1 Then Begin
     Filled1.Checked := Not Filled1.Checked;
     FillShapes := Filled1.Checked;
     If not FillShapes Then
        If Not OutlineShapes Then
           CheckBox1Click(DrawOutline1);
  End;

  If Sender = DrawOutline1 Then Begin
     DrawOutline1.Checked := Not DrawOutline1.Checked;
     OutlineShapes := DrawOutline1.Checked;
     If Not OutlineShapes Then
        If Not FillShapes then
           CheckBox1Click(Filled1);
  End;

end;

Procedure TScrPaintForm.OptimiseForTape(Index: Integer);
Var
  X, Y, Idx, Count, CountInverted, Addr: Integer;
  ByteVal, BitVal: Byte;
  ScreenArray: String;
  Bytes, BytesInverted: Array[0..8] of Byte;
Begin

  SetLength(ScreenArray, 6912);

  For Idx := 0 to 6143 Do Begin
     X := (Idx and 31) * 8;
     Y := ScreenOffsets[Idx];
     ByteVal := 0;
     BitVal := 128;
     While BitVal > 0 Do Begin
        If Screens[Index]^.InkDIB[X, Y] = 1 Then
           ByteVal := ByteVal + BitVal;
        BitVal := BitVal Shr 1;
        Inc(X);
     End;
     ScreenArray[Idx +1] := Chr(ByteVal);
  End;

  For Idx := 6144 To 6911 Do Begin
     X := Idx - 6144;
     Y := X Div 32;
     X := X And 31;
     ScreenArray[Idx +1] := Chr(Screens[Index]^.Attrs[X, Y]);
  End;

  For Y := 0 To 23 Do
     For X := 0 To 31 Do Begin
        Addr := ScreenAddresses[Y * 8] + X;
        For Idx := 0 To 7 Do Begin
           Bytes[Idx] := Byte(ScreenArray[1 + Addr + (Idx * 256)]);
           BytesInverted[Idx] := Bytes[Idx] Xor 255;
        End;
        Bytes[8] := Byte(ScreenArray[6145 + (Y * 32) + X]);
        BytesInverted[8] := ((Bytes[8] And 7) Shl 3) + ((Bytes[8] And 56) shr 3) + (Bytes[8] And 192);
        Count := 0;
        CountInverted := 0;
        For Idx := 0 To 7 Do Begin
           Inc(Count, SetCount[Bytes[Idx]]);
           Inc(CountInverted, SetCount[BytesInverted[Idx]]);
        End;
        If CountInverted < Count Then Begin
           For Idx := 0 To 7 Do
              ScreenArray[Addr + 1 + (Idx * 256)] := Chr(BytesInverted[Idx]);
           ScreenArray[6145 + (Y * 32) + X] := Chr(BytesInverted[8]);
        End;
     End;

  MemToScreen(ScreenArray, Index);

End;

Procedure TScrPaintForm.PanImage(Dx, Dy: Integer; Attrs, Wrap: Boolean);
Var
  TempScr: Array[0..255, 0..191] of Byte;
  TempAttrs: Array[0..31, 0..23] of Byte;
  TempAttrDetail, TempDetail: Array of Byte;
  X, Y, Px, Py, pDx, pDy, Idx: Integer;
  Invalid: Boolean;
Begin

  FillMemory(@TempScr[0, 0], 256*192, 0);
  FillMemory(@TempAttrs[0, 0], 768, 0);

  If CurScreen^.SelActive Then Begin

     SetLength(TempAttrDetail, Length(CurScreen^.SelAttrDetail));
     SetLength(TempDetail, Length(CurScreen^.SelDetail));
     For Idx := 0 To Length(CurScreen^.SelAttrDetail)-1 Do
        TempAttrDetail[Idx] := GetNewAttr(CurScreen^.SelAttrDetail[Idx], CurINK + (CurPAPER Shl 3) + (CurBRIGHT Shl 6) + (CurFLASH Shl 7), AttrFlags);

     For X := 0 To CurScreen^.SelWidth -1 Do
        For Y := 0 To CurScreen^.SelHeight -1 Do Begin
           Px := X + Dx;
           Py := Y + Dy;
           Invalid := False;
           If Wrap Then Begin
              While Px < 0 Do Px := CurScreen^.SelWidth + Px;
              While Px > CurScreen^.SelWidth -1 Do Px := Px - CurScreen^.SelWidth;
              While Py < 0 Do Py := CurScreen^.SelHeight + Py;
              While Py > CurScreen^.SelHeight -1 Do Py := Py - CurScreen^.SelHeight;
              Invalid := False;
           End Else Begin
              Invalid := (Px < 0) or (Px > CurScreen^.SelWidth -1) or (Py < 0) or (Py > CurScreen^.SelHeight -1);
           End;
           If Not Invalid Then Begin
              If Attrs Then
                 TempAttrDetail[Px + (Py * CurScreen^.SelWidth)] := CurScreen^.SelAttrDetail[X + (Y * CurScreen^.SelWidth)];
              TempDetail[Px + (Py * CurScreen^.SelWidth)] := CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)];
           End;
        End;

     If Attrs Then
        CopyMemory(@CurScreen^.SelAttrDetail[0], @TempAttrDetail[0], Length(CurScreen^.SelAttrDetail));
     CopyMemory(@CurScreen^.SelDetail[0], @TempDetail[0], Length(CurScreen^.SelDetail));

  End Else Begin

     If Attrs Then Begin

        pDx := Dx Div 8;
        pDy := Dy Div 8;

        For X := 0 To 31 Do
           For Y := 0 To 23 Do Begin
              Px := X + pDx;
              Py := Y + pDy;
              Invalid := False;
              If Wrap Then Begin
                 While Px < 0 Do Px := 32 + Px;
                 While Px > 31 Do Px := Px - 32;
                 While Py < 0 Do Py := 24 + Py;
                 While Py > 23 Do Py := Py - 24;
                 Invalid := False;
              End Else Begin
                 Invalid := (Px < 0) or (Px > 31) or (Py < 0) or (Py > 23);
              End;
              If Not Invalid Then
                 TempAttrs[Px, Py] := CurScreen^.Attrs[X, Y];
           End;

        CopyMemory(@CurScreen^.Attrs[0, 0], @TempAttrs[0, 0], 768);

     End;

     For X := 0 To 255 Do
        For Y := 0 To 191 Do Begin
           Px := X + Dx;
           Py := Y + Dy;
           Invalid := False;
           If Wrap Then Begin
              While Px < 0 Do Px := 256 + Px;
              While Px > 255 Do Px := Px - 256;
              While Py < 0 Do Py := 192 + Py;
              While Py > 191 Do Py := Py - 192;
              Invalid := False;
           End Else Begin
              Invalid := (Px < 0) or (Px > 255) or (Py < 0) or (Py > 191);
           End;
           If Not Invalid Then
              TempScr[Px, Py] := CurScreen^.InkDIB[X, Y];
        End;

     CopyMemory(@CurScreen^.InkDIB[0, 0], @TempScr[0, 0], 256 * 192);

  End;

End;

Procedure TScrPaintForm.RotateImage(Angle, Cx, Cy: Integer; Attrs: Boolean);
Var
  DIB, MaskDIB, DetailDIB, AttrDIB, Dest: TFastDIB;
  X, Y, X1, Y1, MinX, MinY, MaxX, MaxY, NewWidth, NewHeight, nSize, OffX, OffY: Integer;
  Attr: Byte;
Begin

  If CurScreen^.SelActive Then Begin

     nSize := Max(CurScreen^.SelWidth, CurScreen^.SelHeight) * 2;

     MaskDIB := TFastDIB.Create;
     MaskDIB.SetSize(nSize, nSize, 8);
     MaskDIB.ClearB(0);

     OffX := (nSize - CurScreen^.SelWidth) Div 2;
     OffY := (nSize - CurScreen^.SelHeight) Div 2;

     DetailDIB := TFastDIB.Create;
     DetailDIB.MakeCopy(MaskDIB, True);

     AttrDIB := TFastDIB.Create;
     AttrDIB.MakeCopy(MaskDIB, True);

     Dest := TFastDIB.Create;
     Dest.MakeCopy(MaskDIB, True);

     For Y := 0 To CurScreen^.SelHeight -1 Do
        For X := 0 To CurScreen^.SelWidth -1 Do Begin
           MaskDIB.Pixels8[Y + OffY, X + OffX] := CurScreen^.SelMask[X + (Y * CurScreen^.SelWidth)];
           DetailDIB.Pixels8[Y + OffY, X + OffX] := CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)];
           AttrDIB.Pixels8[Y + OffY, X + OffX] := CurScreen^.SelAttrDetail[X + (Y * CurScreen^.SelWidth)];
        End;

     nSize := nSize Shr 1;

     FastFX.Transform(MaskDIB, Dest, nSize, nSize, Round(Sin(Angle*Pi/180)*65536), Round(Cos(Angle*Pi/180)*65536), False);
     MinX := 9999; MaxX := 0;
     MinY := 9999; MaxY := 0;
     For Y := 0 To Dest.Height -1 Do
        For X := 0 To Dest.Width -1 Do
           If Dest.Pixels8[Y, X] = 1 Then Begin
              If Y < MinY Then MinY := Y;
              If Y > MaxY Then MaxY := Y;
              If X < MinX Then MinX := X;
              If X > MaxX Then MaxX := X;
           End;

     If (MinX <> 9999) and (MaxX <> 0) And (MinY <> 9999) And (MaxY <> 0) Then Begin

        NewWidth := (MaxX - MinX) +1;
        NewHeight := (MaxY - MinY) +1;
        SetLength(CurScreen^.SelMask, NewWidth * NewHeight);
        SetLength(CurScreen^.SelDetail, NewWidth * NewHeight);
        SetLength(CurScreen^.SelAttrDetail, NewWidth * NewHeight);

        X1 := 0;
        For Y := MinY To MaxY Do
           For X := MinX To MaxX Do Begin
              CurScreen^.SelMask[X1] := Dest.Pixels8[Y, X];
              Inc(X1);
           End;

        Dest.ClearB(0);
        FastFX.Transform(DetailDIB, Dest, nSize, nSize, Round(Sin(Angle*Pi/180)*65536), Round(Cos(Angle*Pi/180)*65536), False);
        X1 := 0;
        For Y := MinY To MaxY Do
           For X := MinX To MaxX Do Begin
              If CurScreen^.SelMask[X1] = 1 Then
                 CurScreen^.SelDetail[X1] := Dest.Pixels8[Y, X];
              Inc(X1);
           End;

        Dest.ClearB(0);
        FastFX.Transform(AttrDIB, Dest, nSize, nSize, Round(Sin(Angle*Pi/180)*65536), Round(Cos(Angle*Pi/180)*65536), False);
        X1 := 0;
        For Y := MinY To MaxY Do
           For X := MinX To MaxX Do Begin
              If CurScreen^.SelMask[X1] = 1 Then
                 CurScreen^.SelAttrDetail[X1] := Dest.Pixels8[Y, X];
              Inc(X1);
           End;

        Dec(CurScreen^.SelOrigin.X, (NewWidth - CurScreen^.SelWidth) Div 2);
        Dec(CurScreen^.SelOrigin.Y, (NewHeight - CurScreen^.SelHeight) Div 2);

        CurScreen^.SelWidth := NewWidth;
        CurScreen^.SelHeight := NewHeight;

     End;

     MaskDIB.Free;
     DetailDIB.Free;
     AttrDIB.Free;
     Dest.Free;

  End Else Begin

     // Rotate the pixels slice of the image

     DIB := TFastDIB.Create;
     DIB.SetSize(256, 192, 8);
     DIB.ClearB(0);
     Dest := TFastDIB.Create;
     Dest.SetSize(256, 192, 8);
     Dest.ClearB(0);
     For Y := 0 To 191 Do
        For X := 0 To 254 Do
           DIB.Pixels8[Y, X] := CurScreen^.InkDIB[X, Y];

     FastFX.Transform(DIB, Dest, Cx, Cy, Round(Sin(Angle*Pi/180)*65536), Round(Cos(Angle*Pi/180)*65536), False);

     For Y := 0 To 191 Do
        For X := 0 To 254 Do
           CurScreen^.InkDIB[X, Y] := Dest.Pixels8[Y, X];

     If Attrs Then Begin

        For X := 0 To 31 Do
           For Y := 0 To 23 Do Begin
              Attr := CurScreen^.Attrs[X, Y];
                 For Y1 := Y * 8 To (Y * 8) + 7 Do
                    For X1 := X * 8 To (X * 8) + 7 Do
                       DIB.Pixels8[Y1, X1] := Attr;
           End;

        Dest.ClearB(GetNewAttr(0, CurINK, CurPAPER, CURBRIGHT, CURFLASH, [pwINK, pwPAPER, pwBRIGHT, pwFLASH]));
        FastFX.Transform(DIB, Dest, Cx, Cy, Round(Sin(Angle*Pi/180)*65536), Round(Cos(Angle*Pi/180)*65536), False);

        For X := 0 To 31 Do
           For Y := 0 To 23 Do
              CurScreen^.Attrs[X, Y] := Dest.Pixels8[Y * 8, X * 8];

     End;

     DIB.Free;
     Dest.Free;

  End;

End;

procedure TScrPaintForm.StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
Var
  BVal: Byte;
  TempDIB: TFastDIB;
  X, Y, Bright: Integer;
  Attr: String;
begin

  If MouseInImage And ((MouseX >= 0) and (MouseX < 256) And (MouseY >= 0) And (MouseY < 192)) Then Begin

     BVal := CurScreen^.Attrs[MouseX Div 8, MouseY Div 8];
     Attr := '['+IntToStr(BVal) + '] (I: ' + IntToStr(BVal and 7) + ' P: ' + IntToStr((BVal Shr 3) And 7) + ' B: ' + IntToStr((BVal Shr 6) And 1) + ' F: ' + IntToStr((BVal Shr 7) And 1) + ')';
     Bright := (BVal Shr 6) And 1;
     TempDIB := TFastDIB.Create;
     TempDIB.SetSize((Rect.Bottom - Rect.Top) - 4, (Rect.Bottom - Rect.Top) - 4, 32);
     TempDIB.Clear(FRGBn(DisplayPalette[((BVal Shr 3) And 7) + (Bright * 8)]));
     For Y := 0 To TempDIB.AbsHeight -1 Do
        For X := Y To TempDIB.Width -1 Do
           TempDIB.Pixels32[Y, X] := DisplayPalette[(BVal And 7) + (Bright * 8)];
     StatusBar1.Canvas.Brush.Style := bsClear;
     TempDIB.Draw(StatusBar1.Canvas.Handle, Rect.Left + 6, 6);
     TempDIB.Free;

     StatusBar1.Panels[2].Text := '          ' + Attr;

  End Else Begin

     Attr := '[] (I: - P: - B: - F: -)';
     StatusBar1.Panels[2].Text := '          ' + Attr;

     TempDIB := TFastDIB.Create;
     TempDIB.SetSize((Rect.Bottom - Rect.Top) - 4, (Rect.Bottom - Rect.Top) - 4, 32);
     TempDIB.Clear(FRGBn(DisplayPalette[7]));
     For Y := 0 To TempDIB.AbsHeight -1 Do
        For X := 0 To TempDIB.Width -1 Do
           If Odd(Y) Then Begin
              If Odd(X) Then
                 TempDIB.Pixels32[Y, X] := DisplayPalette[15]
              Else
                 TempDIB.Pixels32[Y, X] := DisplayPalette[0];
           End Else
              If Odd(X) Then
                 TempDIB.Pixels32[Y, X] := DisplayPalette[0]
              Else
                 TempDIB.Pixels32[Y, X] := DisplayPalette[15];

     StatusBar1.Canvas.Brush.Style := bsClear;
     TempDIB.Draw(StatusBar1.Canvas.Handle, Rect.Left + 6, 6);
     TempDIB.Free;

  End;

end;

Procedure TScrPaintForm.UpdateToolBar;
Begin

  If CurScreen <> Nil Then Begin

     SpeedButton4.Enabled := CurScreen^.SelActive;
     SpeedButton5.Enabled := CurScreen^.SelActive;
     SpeedButton8.Enabled := CurScreen^.SelActive;
     SpeedButton6.Enabled := ClipScr.SelActive;

  End;

End;

Procedure TScrPaintForm.CreateGradient;
Var
  Y, X: Integer;
Begin

  GradientDIB.SetSize(NewGradientWidth, NewGradientHeight, 8);
  BrushSelectorForm.MakeGradient(GradientDIB, NewGradientType, BrushSelectorForm.TrackBar5.Position, BrushSelectorForm.TrackBar6.Position, True);
  For Y := 0 To GradientDIB.AbsHeight -1 Do
     For X := 0 To GradientDIB.Width -1 Do
        If GradientDIB.Pixels8[Y, X] = 255 Then
           GradientDIB.Pixels8[Y, X] := 1;

  GradRectW := NewGradientWidth;
  GradRectH := NewGradientHeight;
  GradientType := NewGradientType;

End;

Procedure TScrPaintForm.MakePreview;
Var
  Bright, X, Y, Xc, Yc, Offset, Attr, Len: Integer;
  Ink, Paper: TFColorA;
  TempScr: TScrImage;
Begin

  CreateScreen(TempScr);

  CopyMemory(@TempScr.InkDIB, @CurScreen^.InkDIB, 256 * 192);
  CopyMemory(@TempScr.Attrs, @CurScreen^.Attrs, 32 * 24);


  TempScr.SelActive := CurScreen^.SelActive;
  TempScr.SelOrigin.x := CurScreen^.SelOrigin.x;
  TempScr.SelOrigin.y := CurScreen^.SelOrigin.y;
  TempScr.SelWidth := CurScreen^.SelWidth;
  TempScr.SelHeight := CurScreen^.SelHeight;

  Len := Length(CurScreen^.SelMask);
  If Len > 0 Then Begin
     SetLength(TempScr.SelMask, Len);
     CopyMemory(@TempScr.SelMask[0], @CurScreen^.SelMask[0], Len);
  End;

  Len := Length(CurScreen^.SelDetail);
  If Len > 0 Then Begin
     SetLength(TempScr.SelDetail, Len);
     CopyMemory(@TempScr.SelDetail[0], @CurScreen^.SelDetail[0], Len);
  End;

  Len := Length(CurScreen^.SelAttrDetail);
  If Len > 0 Then Begin
     SetLength(TempScr.SelAttrDetail, Len);
     CopyMemory(@TempScr.SelAttrDetail[0], @CurScreen^.SelAttrDetail[0], Len);
  End;

  If TempScr.SelActive Then Begin

     For X := 0 To TempScr.SelWidth -1 Do
        For Y := 0 To TempScr.SelHeight -1 Do Begin
           Xc := X + TempScr.SelOrigin.X;
           Yc := Y + TempScr.SelOrigin.Y;
           If TempScr.SelMask[x + (TempScr.SelWidth * y)] = 1 Then
              If ColourMode in [cmBoth, cmPixels] Then
                 If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 191) Then
                    TempScr.InkDIB[Xc, Yc] := TempScr.SelDetail[x + (TempScr.SelWidth * y)];
        End;

     For X := 0 To TempScr.SelWidth -1 Do
        For Y := 0 To TempScr.SelHeight -1 Do Begin
           Xc := X + TempScr.SelOrigin.X;
           Yc := Y + TempScr.SelOrigin.Y;
           If TempScr.SelMask[x + (TempScr.SelWidth * y)] = 1 Then
              If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 191) Then
                 TempScr.Attrs[Xc Div 8, Yc Div 8] := GetNewAttr(TempScr.Attrs[Xc Div 8, Yc Div 8], TempScr.SelAttrDetail[X + (TempScr.SelWidth * Y)], AttrFlags);
        End;

  End;

  For Y := 0 To 191 Do Begin
     For X := 0 To 255 Do Begin
        If X Mod 8 = 0 Then Begin
           Attr := TempScr.Attrs[X Div 8, Y Div 8];
           Bright := (Attr And 64) Shr 3;
           Ink := DisplayPalette[(Attr And 7) + Bright];
           Paper := DisplayPalette[((Attr Shr 3) And 7) + Bright];
        End;
        If TempScr.InkDIB[X, Y] = 1 Then
           ScrPreviewForm.Bmp.Pixels32[191 - Y, X] := Ink
        Else
           ScrPreviewForm.Bmp.Pixels32[191 - Y, X] := Paper;
     End;
  End;

  Bilinear32(ScrPreviewForm.Bmp, FastIMG6.Bmp);
  FastIMG6.Repaint;

End;

procedure TScrPaintForm.FastIMG6Enter(Sender: TObject);
Var
  Pt: TPoint;
begin

  If CanEnter Then Begin
     Pt := Point(FastIMG6.Left, FastIMG6.Top);
     CentreForm(ScrPreviewForm, Panel3.ClientToScreen(Pt).X, Panel3.ClientToScreen(Pt).Y);
     ScrPreviewForm.Show;
  End;

end;

procedure TScrPaintForm.FastIMG6Exit(Sender: TObject);
begin

  CanEnter := True;

end;

procedure TScrPaintForm.FastIMG6MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  FastIMG6Enter(Sender);

end;

Function TScrPaintForm.ScreenToBitmap: TFastDIB;
Var
  Bright, X, Y, Xc, Yc, Offset, Attr, Len: Integer;
  Ink, Paper: TFColorA;
  TempScr: TScrImage;
Begin

  Result := TFastDIB.Create;
  Result.SetSize(256, 192, 32);

  CreateScreen(TempScr);
  CopyMemory(@TempScr.InkDIB, @CurScreen^.InkDIB, 256 * 192);
  CopyMemory(@TempScr.Attrs, @CurScreen^.Attrs, 32 * 24);

  Len := Length(CurScreen^.SelMask);
  If Len > 0 Then Begin
     SetLength(TempScr.SelMask, Len);
     CopyMemory(@TempScr.SelMask[0], @CurScreen^.SelMask[0], Len);
  End;

  Len := Length(CurScreen^.SelDetail);
  If Len > 0 Then Begin
     SetLength(TempScr.SelDetail, Len);
     CopyMemory(@TempScr.SelDetail[0], @CurScreen^.SelDetail[0], Len);
  End;

  Len := Length(CurScreen^.SelAttrDetail);
  If Len > 0 Then Begin
     SetLength(TempScr.SelAttrDetail, Len);
     CopyMemory(@TempScr.SelAttrDetail[0], @CurScreen^.SelAttrDetail[0], Len);
  End;

  TempScr.SelActive := CurScreen^.SelActive;
  TempScr.SelOrigin.x := CurScreen^.SelOrigin.x;
  TempScr.SelOrigin.y := CurScreen^.SelOrigin.y;
  TempScr.SelWidth := CurScreen^.SelWidth;
  TempScr.SelHeight := CurScreen^.SelHeight;

  If TempScr.SelActive Then Begin

     For X := 0 To TempScr.SelWidth -1 Do
        For Y := 0 To TempScr.SelHeight -1 Do Begin
           Xc := X + TempScr.SelOrigin.X;
           Yc := Y + TempScr.SelOrigin.Y;
           If TempScr.SelMask[x + (TempScr.SelWidth * y)] = 1 Then
              If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 255) Then
                 TempScr.InkDIB[Xc, Yc] := TempScr.SelDetail[x + (TempScr.SelWidth * y)];
        End;

     For X := 0 To TempScr.SelWidth -1 Do
        For Y := 0 To TempScr.SelHeight -1 Do Begin
           Xc := X + TempScr.SelOrigin.X;
           Yc := Y + TempScr.SelOrigin.Y;
           If TempScr.SelMask[x + (TempScr.SelWidth * y)] = 1 Then
              If (Xc >= 0) and (Xc <= 255) And (Yc >= 0) And (Yc <= 255) Then
                 TempScr.Attrs[Xc Div 8, Yc Div 8] := TempScr.SelAttrDetail[X + (TempScr.SelWidth * Y)];
        End;

  End;

  For Y := 0 To 191 Do Begin
     For X := 0 To 255 Do Begin
        If X Mod 8 = 0 Then Begin
           Attr := TempScr.Attrs[X Div 8, Y Div 8];
           Bright := (Attr And 64) Shr 3;
           Ink := DisplayPalette[(Attr And 7) + Bright];
           Paper := DisplayPalette[((Attr Shr 3) And 7) + Bright];
        End;
        If TempScr.InkDIB[X, Y] = 1 Then
           Result.Pixels32[191 - Y, X] := Ink
        Else
           Result.Pixels32[191 - Y, X] := Paper;
     End;
  End;

End;

Function TScrPaintForm.SelectionToBitmap: TFastDIB;
Var
  Bright, X, Y, Attr: Integer;
  Ink, Paper: TFColorA;
Begin

  If CurScreen^.SelActive Then Begin

     Result := TFastDIB.Create;
     Result.SetSize(CurScreen^.SelWidth, CurScreen^.SelHeight, 32);

     For Y := 0 To CurScreen^.SelHeight -1 Do Begin
        For X := 0 To CurScreen^.SelWidth -1 Do Begin
           Attr := CurScreen^.SelAttrDetail[X + (Y * CurScreen^.SelWidth)];
           Bright := (Attr And 64) Shr 3;
           Ink := DisplayPalette[(Attr And 7) + Bright];
           Paper := DisplayPalette[((Attr Shr 3) And 7) + Bright];
           If CurScreen^.SelMask[X + (Y * CurScreen^.SelWidth)] = 1 Then
              If CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)] = 1 Then
                 Result.Pixels32[(CurScreen^.SelHeight -1) - Y, X] := Ink
              Else
                 Result.Pixels32[(CurScreen^.SelHeight -1) - Y, X] := Paper;
        End;
     End;

  End;

End;

Procedure TScrPaintForm.CopyToClipboard;
Var
  TempDIB: TFastDIB;
  BMP: TBitmap;
Begin

  If Not CurScreen^.SelActive Then
     TempDIB := ScreenToBitmap
  Else
     TempDIB := SelectionToBitmap;

  BMP := TBitmap.Create;
  BMP.Width := TempDIB.Width;
  BMP.Height := TempDIB.AbsHeight;
  BMP.PixelFormat := pf24bit;

  TempDIB.Draw(BMP.Canvas.Handle, 0, 0);
  TempDIB.Free;

  Clipboard.Assign(BMP);

End;

Procedure TScrPaintForm.PasteFromClipboard;
Var
  BMP: TBitmap;
Begin

  // Uses the bitmap import dialog to get images from the clipboard.

  If Clipboard.HasFormat(CF_BITMAP) Then Begin

     BMP := TBitmap.Create;
     BMP.Assign(Clipboard);

     BMPImportForm.OriginalDIB.SetSize(BMP.Width, BMP.Height, 32);
     BitBlt(BMPImportForm.OriginalDIB.hDc, 0, 0, BMP.Width, BMP.Height, BMP.Canvas.Handle, 0, 0, SRCCOPY);
     BMP.Free;
     Filename := #255;
     BMPImportForm.Button1Click(nil);
     CentreFormOnForm(BMPImportForm, nil);
     ShowWindow(BMPImportForm, True);

  End;

End;

Procedure TScrPaintForm.SkewImage(Cx, Cy, Dx, Dy: Integer; Wrap, Attrs: Boolean);
Var
  NewPixels, NewAttrs: Array[0..255, 0..191] of Byte;
  SelMaskArray, SelDetailArray, SelAttrArray: Array of Byte;
  tDx, tDy, Idx, X, Y, NewX, NewY, Xc, Yc, Nw, Nh, MinX, MinY, MaxX, MaxY, Offset: Integer;
Begin

  If CurScreen^.SelActive Then Begin

     tDx := Dx;
     tDy := Dy;

     For Idx := 0 To 1 Do Begin

        If Idx = 0 Then
           Dy := 0
        Else Begin
           Dx := 0;
           Dy := tDy;
        End;

        Nw := CurScreen^.SelWidth + (Abs(Dx) * 2);
        Nh := CurScreen^.SelHeight + (Abs(Dy) * 2);
        SetLength(SelMaskArray, Nw * Nh);
        SetLength(SelDetailArray, Nw * Nh);
        SetLength(SelAttrArray, Nw * Nh);
        FillMemory(@SelMaskArray[0], Nw * Nh, 0);
        FillMemory(@SelAttrArray[0], Nw * Nh, 0);
        FillMemory(@SelDetailArray[0], Nw * Nh, 0);

        For Y := 0 To Nh -1 Do
           For X := 0 To Nw -1 Do Begin
              NewX := X - Trunc((Dx / CurScreen^.SelHeight) * Y) - Abs(Dx);
              NewY := Y - Trunc((Dy / CurScreen^.SelWidth) * X) - Abs(Dy);
              If (NewX >= 0) and (NewX < CurScreen^.SelWidth) and (NewY >= 0) And (NewY < CurScreen^.SelHeight) Then Begin
                 SelMaskArray[X + (Y * Nw)] := CurScreen^.SelMask[NewX + (NewY * CurScreen^.SelWidth)];
                 SelDetailArray[X + (Y * Nw)] := CurScreen^.SelDetail[NewX + (NewY * CurScreen^.SelWidth)];
                 SelAttrArray[X + (Y * Nw)] := CurScreen^.SelAttrDetail[NewX + (NewY * CurScreen^.SelWidth)];
              End;
           End;

        MinX := 99999;
        MinY := 99999;
        MaxX := 0;
        MaxY := 0;

        For X := 0 To Nw -1 Do
           For Y := 0 To Nh -1 Do
              If SelMaskArray[X + (Y * Nw)] = 1 Then Begin
                 If X > MaxX Then MaxX := X;
                 If X < MinX Then MinX := X;
                 If Y > MaxY Then MaxY := Y;
                 If Y < MinY Then MinY := Y;
              End;

        SetLength(CurScreen^.SelMask, ((MaxX - MinX) +1) * ((MaxY - MinY) +1));
        SetLength(CurScreen^.SelDetail, ((MaxX - MinX) +1) * ((MaxY - MinY) +1));
        SetLength(CurScreen^.SelAttrDetail, ((MaxX - MinX) +1) * ((MaxY - MinY) +1));

        Offset := 0;
        For Y := MinY To MaxY Do
           For X := MinX to MaxX Do Begin
              CurScreen^.SelMask[Offset] := SelMaskArray[X + (Y * Nw)];
              CurScreen^.SelDetail[Offset] := SelDetailArray[X + (Y * Nw)];
              CurScreen^.SelAttrDetail[Offset] := SelAttrArray[X + (Y * Nw)];
              Inc(Offset);
           End;

        CurScreen^.SelWidth := (MaxX - MinX) +1;
        CurScreen^.SelHeight := (MaxY - MinY) +1;

     End;

     Inc(CurScreen^.SelOrigin.x, -Abs(tDx Div 2));
     Inc(CurScreen^.SelOrigin.y, -Abs(tDy Div 2));

  End Else Begin

     FillMemory(@NewPixels[0, 0], 256*192, 0);
     FillMemory(@NewAttrs[0, 0], 256 * 192, GetNewAttr(0, CurInk, CurPaper, CurBright, CurFlash, [pwINK, pwPAPER, pwBRIGHT, pwFLASH]));

     For X := 0 To 255 Do
        For Y := 0 To 191 Do Begin

           NewX := X + Round((Dx / 192) * (Y - Cy));
           NewY := Y + Round((Dy / 256) * (X - Cx));

           If Wrap Then Begin
              While NewX > 255 Do Dec(NewX, 256);
              While NewX < 0 Do Inc(NewX, 256);
              While NewY > 191 Do Dec(NewY, 192);
              While NewY < 0 Do Inc(NewY, 192);
           End;

           If (NewX >= 0) And (NewX < 256) And (NewY >= 0) And (NewY < 192) Then Begin
              NewPixels[NewX, NewY] := CurScreen^.InkDIB[X, Y];
              For Xc := 0 To 7 Do
                 For Yc := 0 To 7 Do
                    NewAttrs[((NewX Div 8) * 8) + Xc, ((NewY Div 8) * 8) + Yc] := CurScreen^.Attrs[X Div 8, Y Div 8];
           End;

        End;

     For X := 1 To 254 Do
        For Y := 1 To 190 Do
           If CurScreen^.InkDIB[X, Y] = 1 Then
              If (CurScreen^.InkDIB[X, Y -1] = 1) And (CurScreen^.InkDIB[X -1, Y] = 1) And
                 (CurScreen^.InkDIB[X +1, Y] = 1) And (CurScreen^.InkDIB[X, Y+1] =1) Then Begin
                 NewX := X + Round((Dx / 192) * (Y - Cy));
                 NewY := Y + Round((Dy / 256) * (X - Cx));
                 If (NewX >= 0) And (NewX < 256) And (NewY >= 0) And (NewY < 192) Then Begin
                    If (NewX > 0) And (NewPixels[NewX -1, NewY] = 0) Then NewPixels[NewX -1, NewY] := 1;
                    If (NewX < 255) And (NewPixels[NewX +1, NewY] = 0) Then NewPixels[NewX +1, NewY] := 1;
                    If (NewY > 0) And (NewPixels[NewX, NewY -1] = 0) Then NewPixels[NewX, NewY -1] := 1;
                    If (NewY < 191) And (NewPixels[NewX, NewY +1] = 0) Then NewPixels[NewX, NewY +1] := 1;
                 End;
              End;

     CopyMemory(@CurScreen^.InkDIB[0, 0], @NewPixels[0, 0], 256 * 192);
     If Attrs Then
        For X := 0 To 31 Do
           For Y := 0 To 23 Do
              CurScreen^.Attrs[X, Y] := NewAttrs[X * 8, Y * 8];

  End;

End;

Function TScrPaintForm.ScreenToSCR(ImageIndex: Integer): String;
Var
  X, Y, Idx: Integer;
  ByteVal, BitVal: Byte;
Begin

  SetLength(Result, 6912);
  CopyScreen(Screens[ImageIndex]^, BackImage);

  If Screens[ImageIndex]^.SelActive Then Begin
     For X := 0 To Screens[ImageIndex]^.SelWidth -1 Do
        For Y := 0 To Screens[ImageIndex]^.SelHeight -1 Do Begin
           If Screens[ImageIndex]^.SelMask[x + (Screens[ImageIndex]^.SelWidth * y)] = 1 Then
              Screens[ImageIndex]^.InkDIB[X + Screens[ImageIndex]^.SelOrigin.X, Y+Screens[ImageIndex]^.SelOrigin.Y] := Screens[ImageIndex]^.SelDetail[x + (Screens[ImageIndex]^.SelWidth * y)];
        End;

     For X := 0 To Screens[ImageIndex]^.SelWidth -1 Do
        For Y := 0 To Screens[ImageIndex]^.SelHeight -1 Do
           If Screens[ImageIndex]^.SelMask[x + (Screens[ImageIndex]^.SelWidth * y)] = 1 Then
              Screens[ImageIndex]^.Attrs[(X + Screens[ImageIndex]^.SelOrigin.X) Div 8, (Y + Screens[ImageIndex]^.SelOrigin.Y) Div 8] := GetNewAttr(Screens[ImageIndex]^.Attrs[(X + Screens[ImageIndex]^.SelOrigin.X) Div 8, (Y + Screens[ImageIndex]^.SelOrigin.Y) Div 8], Screens[ImageIndex]^.SelAttrDetail[X + (Screens[ImageIndex]^.SelWidth * Y)], AttrFlags);
  End;

  For Idx := 0 to 6143 Do Begin
     X := (Idx and 31) * 8;
     Y := ScreenOffsets[Idx];
     ByteVal := 0;
     BitVal := 128;
     While BitVal > 0 Do Begin
        If Screens[ImageIndex]^.InkDIB[X, Y] = 1 Then
           ByteVal := ByteVal + BitVal;
        BitVal := BitVal Shr 1;
        Inc(X);
     End;
     Result[Idx +1] := Chr(ByteVal);
  End;

  For Idx := 6144 To 6911 Do Begin
     X := Idx - 6144;
     Y := X Div 32;
     X := X And 31;
     Result[Idx +1] := Chr(Screens[ImageIndex]^.Attrs[X, Y]);
  End;

  CopyScreen(BackImage, Screens[ImageIndex]^);

End;

procedure TScrPaintForm.ScrollBox1MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
Var
  Time: Integer;
begin

  Time := GetCurrentTime;
  If Time - LastMWheelTime > 50 Then
     ZoomOut;

  LastMWheelTime := Time;

end;

procedure TScrPaintForm.ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
Var
  Time: Integer;
begin

  Time := GetCurrentTime;
  If Time - LastMWheelTime > 30 Then
     ZoomIn;

  LastMWheelTime := Time;

end;

procedure TScrPaintForm.CoolBar2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  Var
  Msg: TMessage; // Dummy message for later on.
begin
  OnEnterMenuLoop(Msg);
end;

end.

{// Old skew method - inaccurate when skewing by large amounts.

     For X := 0 To CurScreen^.SelWidth -1 Do
        For Y := 0 To CurScreen^.SelHeight -1 Do Begin

           If Dx > 0 Then
              NewX := X + Trunc((Dx / CurScreen^.SelHeight) * Y) + Abs(Dx)
           Else
              NewX := X - Trunc((Abs(Dx) / CurScreen^.SelHeight) * Y) + Abs(Dx);

           If Dy > 0 Then
              NewY := Y + Trunc((Dy / CurScreen^.SelWidth) * X) + Abs(Dy)
           Else
              NewY := Y - Trunc((Abs(Dy) / CurScreen^.SelWidth) * X) + Abs(Dy);

           If (NewX >= 0) And (NewX < Nw) And (NewY >= 0) And (NewY < Nh) Then Begin
              SelMaskArray[NewX + (NewY * Nw)] := CurScreen^.SelMask[X + (Y * CurScreen^.SelWidth)];
              SelDetailArray[NewX + (NewY * Nw)] := CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)];
              For Xc := (NewX Div 8) * 8 To ((NewX Div 8) * 8) + 7 Do
                 For Yc := (NewY Div 8) * 8 To ((NewY Div 8) * 8) + 7 Do
                    If (Xc < Nw) and (Yc < Nh) Then
                       SelAttrArray[Xc + (Yc * Nw)] := CurScreen^.SelAttrDetail[X + (Y * CurScreen^.SelWidth)];
           End;

        End;


     For X := 1 To CurScreen^.SelWidth -2 Do
        For Y := 1 To CurScreen^.SelHeight-2 Do
           If CurScreen^.SelMask[X + (Y * CurScreen^.SelWidth)] = 1 Then
              If (CurScreen^.SelMask[X + ((Y -1) * CurScreen^.SelWidth)] = 1) And
                 (CurScreen^.SelMask[X -1 + (Y * CurScreen^.SelWidth)] = 1) And
                 (CurScreen^.SelMask[X +1 + (Y * CurScreen^.SelWidth)] = 1) And
                 (CurScreen^.SelMask[X + ((Y+1) * CurScreen^.SelWidth)] =1) Then Begin
                 If Dx > 0 Then
                    NewX := X + Trunc((Dx / CurScreen^.SelHeight) * Y) + Abs(Dx)
                 Else
                    NewX := X - Trunc((Abs(Dx) / CurScreen^.SelHeight) * Y) + Abs(Dx);
                 If Dy > 0 Then
                    NewY := Y + Trunc((Dy / CurScreen^.SelWidth) * X) + Abs(Dy)
                 Else
                    NewY := Y - Trunc((Abs(Dy) / CurScreen^.SelWidth) * X) + Abs(Dy);
                 If (NewX >= 0) And (NewX < Nw) And (NewY >= 0) And (NewY < Nh) Then Begin

                    If (NewX > 0) And (SelMaskArray[NewX -1 + (NewY * Nw)] = 0) Then Begin
                       SelMaskArray[NewX -1 + (NewY * Nw)] := 1;
                       SelDetailArray[NewX -1 + (NewY * Nw)] := CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)];
                    End;
                    If (NewX < nW) And (SelMaskArray[NewX +1 + (NewY * Nw)] = 0) Then Begin
                       SelMaskArray[NewX +1 + (NewY * Nw)] := 1;
                       SelDetailArray[NewX +1 + (NewY * Nw)] := CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)];
                    End;
                    If (NewY > 0) And (SelMaskArray[NewX + ((NewY -1) * Nw)] = 0) Then Begin
                       SelMaskArray[NewX + ((NewY -1) * Nw)] := 1;
                       SelDetailArray[NewX + ((NewY -1) * Nw)] := CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)];
                    End;
                    If (NewY < nH) And (SelMaskArray[NewX + ((NewY +1) * Nw)] = 0) Then Begin
                       SelMaskArray[NewX + ((NewY +1) * Nw)] := 1;
                       SelDetailArray[NewX + ((NewY +1) * Nw)] := CurScreen^.SelDetail[X + (Y * CurScreen^.SelWidth)];
                    End;
                 End;
              End;

     MinX := 99999;
     MinY := 99999;
     MaxX := 0;
     MaxY := 0;

     For X := 0 To Nw -1 Do
        For Y := 0 To Nh -1 Do
           If SelMaskArray[X + (Y * Nw)] = 1 Then Begin
              If X > MaxX Then MaxX := X;
              If X < MinX Then MinX := X;
              If Y > MaxY Then MaxY := Y;
              If Y < MinY Then MinY := Y;
           End;

     SetLength(CurScreen^.SelMask, ((MaxX - MinX) +1) * ((MaxY - MinY) +1));
     SetLength(CurScreen^.SelDetail, ((MaxX - MinX) +1) * ((MaxY - MinY) +1));
     SetLength(CurScreen^.SelAttrDetail, ((MaxX - MinX) +1) * ((MaxY - MinY) +1));

     Offset := 0;
     For Y := MinY To MaxY Do
        For X := MinX to MaxX Do Begin
           CurScreen^.SelMask[Offset] := SelMaskArray[X + (Y * Nw)];
           CurScreen^.SelDetail[Offset] := SelDetailArray[X + (Y * Nw)];
           CurScreen^.SelAttrDetail[Offset] := SelAttrArray[X + (Y * Nw)];
           Inc(Offset);
        End;

     Inc(CurScreen^.SelOrigin.x, -Abs(Dx Div 2));
     Inc(CurScreen^.SelOrigin.y, -Abs(Dy Div 2));

     CurScreen^.SelWidth := (MaxX - MinX) +1;
     CurScreen^.SelHeight := (MaxY - MinY) +1;
}

