unit UDGEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, FastIMG, ExtCtrls, StdCtrls, FastDIB, FastDraw, Buttons, ClipBrd,
  ComCtrls, Math, FastDrawEx;

type

  TUDGPaintMode = (BitSet, BitUnSet);
  TGridArray = Array of Byte;

  TUDGWindow = class(TForm)
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    FastIMG5: TFastIMG;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    Panel3: TPanel;
    FastIMG1: TFastIMG;
    FastIMG3: TFastIMG;
    ScrollBox1: TScrollBox;
    FastIMG2: TFastIMG;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N4: TMenuItem;
    Open1: TMenuItem;
    Grab1: TMenuItem;
    FromUDGs1: TMenuItem;
    FromCurrentCHARS1: TMenuItem;
    FromMemory1: TMenuItem;
    N5: TMenuItem;
    Send1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Animator1: TMenuItem;
    N8: TMenuItem;
    Exit1: TMenuItem;
    Edit3: TMenuItem;
    Undo1: TMenuItem;
    N3: TMenuItem;
    Grid1: TMenuItem;
    Setup1: TMenuItem;
    Fill1: TMenuItem;
    N2: TMenuItem;
    Clear2: TMenuItem;
    Invert2: TMenuItem;
    Flip2: TMenuItem;
    Horizontal2: TMenuItem;
    Vertical2: TMenuItem;
    Rotate2: TMenuItem;
    N90Left1: TMenuItem;
    N90Right2: TMenuItem;
    Shift2: TMenuItem;
    Left2: TMenuItem;
    Right2: TMenuItem;
    Up2: TMenuItem;
    Down2: TMenuItem;
    Characters1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Insertnew1: TMenuItem;
    Delete1: TMenuItem;
    N6: TMenuItem;
    Clear1: TMenuItem;
    Invert1: TMenuItem;
    Flip1: TMenuItem;
    Horizontal1: TMenuItem;
    Vertical1: TMenuItem;
    Rotate1: TMenuItem;
    N901: TMenuItem;
    N90Right1: TMenuItem;
    Shift1: TMenuItem;
    Left1: TMenuItem;
    Right1: TMenuItem;
    Up1: TMenuItem;
    Down1: TMenuItem;
    Help1: TMenuItem;
    UDGEditorHelp1: TMenuItem;
    FastIMG4: TFastIMG;
    SpeedButton21: TSpeedButton;
    FastIMG6: TFastIMG;
    N7: TMenuItem;
    ExportCharacter1: TMenuItem;
    SetupCount1: TMenuItem;
    N9: TMenuItem;
    Save1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1Exit(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FromMemory1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FromUDGs1Click(Sender: TObject);
    procedure FromCurrentCHARS1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure CopyASMdb1Click(Sender: TObject);
    procedure FastIMG2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Insertnew1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure UDGEditorHelp1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton18Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure SpeedButton20Click(Sender: TObject);
    procedure Fill1Click(Sender: TObject);
    procedure Setup1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Send1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Copysetasasm1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FastIMG5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG5MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: AnsiChar);
    procedure FastIMG5MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton21Click(Sender: TObject);
    procedure FastIMG6MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG6MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ExportCharacter1Click(Sender: TObject);
    procedure SetupCount1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    UdgFileName: AnsiString;  //last open/saved filename //arda--requested by DamienG >1.793
    changed: Boolean;         //keep track of change (not really, only tracks mouse downs)
    PaletteLineWidth: Integer;
    Address:  Word;
    CurChar:  DWord;         // CurChar is the current character being edited
    NumChars: Integer;        // Number of characters to be edited
    CurChars: AnsiString;        // Curchars holds the current chunk of memory being edited.
    AssignedChars: AnsiString;   // Assigned characters - which UDG is assigned to which section of the editing grid
    OnionChars: AnsiString;      // As for AssignedChars, but for the onion-skin display
    OnionDown,
    OnionShowing: Boolean;
    EditWidth,
    EditHeight,
    DataWidth,
    DataHeight,
    DragChar,
    DragOp,
    DragX,
    DragY,
    CellW,
    CellH,
    CurCellO,
    CurCellC,
    CurCharX,
    CurCharY,
    CurCellX,
    CurCellY: Integer;
    FirstRun: Boolean;
    PaintMode: TUDGPaintMode;
    UndoBuffer: AnsiString;
    ClipChar: Array of Byte;
    MouseDown,
    GotUndo,
    GotClip:  Boolean;
    CurObj:   Integer;
    AnimList: TStringlist;
    AnimMouseDown: Boolean;
    AnimViewOffset, AnimSelected,
    AnimScrollWidth, AnimScrollPos,
    AnimMouseX, AnimMouseY: Integer;
    Function  GetAssignedChars: AnsiString;
    Procedure SetUpGrid(Ew, Eh, Dw, Dh: Integer);
    Procedure RepaintChars;
    Procedure SaveAs(Filenamez: AnsiString);
    Procedure DrawChar(DIB: TFastDIB; X, Y, Index: Integer; Selected: Boolean);
    Procedure UpdateChar(Index: Integer; Selected: Boolean);
    Procedure GetBIGChar(ForceDraw: Boolean);
    Procedure FlipBit;
    Procedure SendToMemory;
    Function  BitState: TUDGPaintMode;
    procedure OnEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
    Procedure StoreUndo;
    Procedure RestoreUndo;
    Function  CreateASMChar(Index: Integer): AnsiString;
    procedure GrabFromMemory;
    Procedure UpdateStatusBar;
    Procedure UpdateDragImage;
    Procedure RotateRightChar(Index: Integer);
    Procedure RotateLeftChar(Index: Integer);
    Procedure MirrorLRChar(Index: Integer);
    Procedure MirrorVTChar(Index: Integer);
    Procedure ShiftLeftChar(Index: Integer);
    Procedure ShiftRightChar(Index: Integer);
    Procedure ShiftUpChar(Index: Integer);
    Procedure ShiftDownChar(Index: Integer);
    Procedure ClearChar(Index: Integer);
    Procedure InvertChar(Index: Integer);
    Procedure ExchangeChars(Src, Dst: Integer);
    Procedure ArrangeChars(SrcChars, DstChars: AnsiString);
    Procedure SetPaletteSize;
    Procedure RenderCurrentAnim(NewSet: Boolean);
    Procedure AnimScrollInView(Index: Integer);
  end;

var
  UDGWindow: TUDGWindow;

implementation

{$R *.DFM}

Uses GridSetup, FastCore, ROMUtils, Filing, InputUtils, Utility, UDGOptions, AnimPreview,
     AddCode, Evaluate, BasinMain, TokenWindow, GrabParms, BinaryForm, Binaries;

procedure TUDGWindow.GrabFromMemory;
Var
  F: Integer;
begin
  StoreUndo;
  While (Address -1 + (NumChars * DataWidth * DataHeight) > 65535) or (NumChars > 255) Do Dec(NumChars);
  // Now Grab to the CurChars String.
  CurChars := '';
  For F := 0 To (NumChars * DataWidth * DataHeight) -1 Do
     CurChars := CurChars + AnsiChar(Memory[F+Address]);
  CurChar := 1;
  AssignedChars := #1#0;
  SetUpGrid(EditWidth, EditHeight, DataWidth, DataHeight);
  SetPaletteSize;
  FormResize(Nil);
  UpdateStatusBar;
End;

procedure TUDGWindow.FormCreate(Sender: TObject);
begin
  changed:=false;
  UdgFileName:='';
  AnimList := TStringlist.Create;

  Panel3.SetBounds(Panel3.Left, Panel3.Top, Panel3.Width, SpeedButton1.Top - Panel3.Top - 4);
  ScrollBox1.SetBounds(ScrollBox1.Left, ScrollBox1.Top, ScrollBox1.Width, SpeedButton1.Top - ScrollBox1.Top - 4);


  Address := 65368;
  CurChar := 1;
  CurObj := 1;

  If ProgramIs128k Then
     NumChars := 19
  Else
     NumChars := 21;

  While Length(CurChars) < 168 Do CurChars := CurChars + #0;

  // Default Data setup is for UDGs.

  SetUpGrid(1, 1, 1, 8);
  SetPaletteSize;

  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 32);
  FastIMG2.Bmp.SetSize(ScrollBox1.Width-100,FastIMG2.Height, 32);
  FastIMG4.Bmp.SetSize(FastIMG4.Width, FastIMG4.Height, 32);

  FirstRun := True;
  GotUndo := False;
  PaintMode := BitSet;

  RenderCurrentAnim(True);

end;

Procedure TUDGWindow.SetUpGrid(Ew, Eh, Dw, Dh: Integer);
Var
  Idx: Integer;
Begin

  EditWidth := Ew;
  EditHeight := Eh;
  DataWidth := Dw;
  DataHeight := Dh;

  SetLength(AssignedChars, 0);
  SetLength(OnionChars, 0);

  SetLength(AssignedChars, EditWidth * EditHeight * 2);
  SetLength(OnionChars, EditWidth * EditHeight * 2);
  SetLength(ClipChar, DataWidth * DataHeight);

  For Idx := 0 To (EditWidth * EditHeight) -1 Do Begin
     PutWord(@AssignedChars[(Idx * 2) +1], (Idx Mod NumChars) +1);
     PutWord(@OnionChars[(Idx * 2) +1], 0);
  End;

  If (EditWidth = 1) and (EditHeight = 1) Then Begin

     SpeedButton11.Visible := False;
     SpeedButton12.Visible := False;
     SpeedButton13.Visible := False;
     SpeedButton14.Visible := False;
     SpeedButton15.Visible := False;
     SpeedButton16.Visible := False;
     SpeedButton17.Visible := False;
     SpeedButton18.Visible := False;
     SpeedButton19.Visible := False;
     SpeedButton20.Visible := False;

  End Else Begin

     SpeedButton11.Visible := True;
     SpeedButton12.Visible := True;
     SpeedButton13.Visible := True;
     SpeedButton14.Visible := True;
     SpeedButton15.Visible := True;
     SpeedButton16.Visible := True;
     SpeedButton17.Visible := True;
     SpeedButton18.Visible := True;
     SpeedButton19.Visible := True;
     SpeedButton20.Visible := True;

  End;

  If DataWidth * 8 = DataHeight Then Begin

     SpeedButton1.Enabled := True;
     SpeedButton2.Enabled := True;
     SpeedButton15.Enabled := True;
     SpeedButton16.Enabled := True;

  End Else Begin

     SpeedButton1.Enabled := False;
     SpeedButton2.Enabled := False;
     SpeedButton15.Enabled := False;
     SpeedButton16.Enabled := False;

  End;

End;

Procedure TUDGWindow.RepaintChars;
Var
  F, X, Y, Y1, cW, cH, Idx: DWord;
  FirstLine, Selected: Boolean;
Begin

  // Cells are (Width * 8) + 7 by (Height + 7)

  cW := (DataWidth * 8) + 7;
  cH := DataHeight + 7;

  FastIMG2.Bmp.Clear(FastDraw.TColorToTFColor(ClBtnFace));//ClBtnFace));

  X := 2; Y := 3;
  PaletteLineWidth := 0;
  FirstLine := True;
  For F := 1 To NumChars Do Begin

     FastDraw.Line(FastIMG2.Bmp, X-2, Y-3, X+(cW -2), Y-3, TFSpecBlack);
     FastDraw.Line(FastIMG2.Bmp, X-2, Y-3, X-2, Y+(cH - 4), TFSpecBlack);
     FastDraw.Line(FastIMG2.Bmp, X+(cW -2), Y-3, X+(cw -2), Y+(cH - 4), TFSpecBlack);
     FastDraw.Line(FastIMG2.Bmp, X-2, Y+(cH - 3), X+ (cW - 2), Y+(cH - 3), TFSpecBlack);
     FastDraw.FillRect(FastIMG2.Bmp, X-1, DWord(FastIMG2.Bmp.Height) - (Y-1), X+(cW - 3), DWord(FastIMG2.Bmp.Height) - (Y+(cH - 3)), TfSpecWhite);

     If F = 207 Then
        CurCharX := 1;

     If CurChar = F Then Selected := True Else Selected := False;
     For Idx := 0 To (Length(AssignedChars) Div 2) -1 Do Begin
        If GetWord(@AssignedChars[(Idx *2) +1]) = F Then
           Selected := True;
     End;

     If Selected Then Begin
        CurCharX := X+2;
        CurCharY := Y+2;
        For Y1 := Y-1 To Y+(cH -5) Do
           FastDraw.Line(FastIMG2.Bmp, X, Y1, X+(cW - 4), Y1, TFSpecWhiteB);
        DrawChar(FastIMG2.Bmp, X+2, Y+2, F, True);
     End Else Begin
        DrawChar(FastIMG2.Bmp, X+2, Y+2, F, False);
     End;

     Inc(X, cW);
     If FirstLine Then Inc(PaletteLineWidth);

     If X > DWord(FastIMG2.Bmp.Width) -1 Then Begin
        X := 2;
        FirstLine := False;
        Inc(Y, cH);
     End;


  End;

  FastIMG2.Repaint;
  RenderCurrentAnim(False);

End;

Procedure TUDGWindow.UpdateChar(Index: Integer; Selected: Boolean);
Var
  X, Y, cW, cH, nR: Integer;
Begin

  cW := (DataWidth * 8) + 7;
  cH := DataHeight + 7;

  Dec(Index);
  nR := Index Div ((FastIMG2.Bmp.Width -1) Div cW);

  Y := 3 + (nR * cH);
  X := 2 + ((Index - (Nr * ((FastIMG2.Bmp.Width -1) Div cW))) * cW);

  DrawChar(FastIMG2.Bmp, X+2, Y+2, Index +1, Selected);

End;

Procedure TUDGWindow.DrawChar(DIB: TFastDIB; X, Y, Index: Integer; Selected: Boolean);
Var
  F, G, H, ByteVal: Byte;
  X1: Integer;
Begin

  Index := ((Index -1)*(DataWidth * DataHeight))+1; // Convert to an offset into the array
  Y := DIB.Height - Y;

  For F := 0 To DataHeight -1 Do Begin
     X1 := X;

     For G := 0 To DataWidth -1 Do Begin
        ByteVal := Ord(CurChars[Index]);
        If Selected Then Begin
           For H := 7 DownTo 0 Do Begin
              If ByteVal and (1 Shl H) > 0 Then
                 DIB.Pixels32[Y-F, X1+(7-H)] := DisplayPalette[0]
              Else
                 DIB.Pixels32[Y-F, X1+(7-H)] := DisplayPalette[15];
           End;
        End Else Begin
           For H := 7 DownTo 0 Do Begin
              If ByteVal and (1 Shl H) > 0 Then
                 DIB.Pixels32[Y-F, X1+(7-H)] := DisplayPalette[0]
              Else
                 DIB.Pixels32[Y-F, X1+(7-H)] := DisplayPalette[7];
           End;
        End;

        Inc(Index);
        Inc(X1, 8);

     End;
  End;

End;

Procedure TUDGWindow.GetBIGChar(ForceDraw: Boolean);
Var
  F, G, H, ByteVal, OnionByte, CellType: Byte;
  X, Y, X1: DWord;
  Colour, TFHalf, TFUDGBlack, OnionColour, PColour: TFColor;
  MousePos: TPoint;
  Index, CellIdx, OnionIdx, OnionIndex, CharOffset, DragCell,
  OffX, OffX2, OffY, Idx: Integer;
  ValidCell, ValidOnion: Boolean;
Begin

  TFUDGBlack.r := 64;
  TFUDGBlack.g := 64;
  TFUDGBlack.b := 64;

  TFHalf.r := (TFSpecWhite.r Div 6) * 4;
  TFHalf.g := (TFSpecWhite.g Div 6) * 4;
  TFHalf.b := (TFSpecWhite.b Div 6) * 4;

  // Prepare the grid

  FastIMG1.Bmp.Clear(TfShadow);
  FastIMG3.Bmp.Clear(TfShadow);
  FastIMG6.Bmp.Clear(TfShadow);
  For X := 0 to FastIMG3.Bmp.Width -1 Do Begin
     FastIMG3.Bmp.Pixels32[0, X] := DisplayPalette[0];
     FastIMG3.Bmp.Pixels32[FastIMG3.Bmp.AbsHeight -1, X] := DisplayPalette[0];
     FastIMG6.Bmp.Pixels32[0, X] := DisplayPalette[0];
     FastIMG6.Bmp.Pixels32[FastIMG6.Bmp.AbsHeight -1, X] := DisplayPalette[0];
  End;

  For Y := 0 to FastIMG3.Bmp.AbsHeight -1 Do Begin
     FastIMG3.Bmp.Pixels32[Y, 0] := DisplayPalette[0];
     FastIMG3.Bmp.Pixels32[Y, FastIMG3.Bmp.Width -1] := DisplayPalette[0];
     FastIMG6.Bmp.Pixels32[Y, 0] := DisplayPalette[0];
     FastIMG6.Bmp.Pixels32[Y, FastIMG6.Bmp.Width -1] := DisplayPalette[0];
  End;

  OffX := 0;
  OffY := FastIMG1.Bmp.AbsHeight - (CellH * DataHeight) -1;
  If OffY < 0 Then OffY := 0;

  DragCell := -1;
  If DragOp <> 0 Then Begin

     Windows.GetCursorPos(MousePos);
     MousePos := FastIMG1.ScreenToClient(MousePos);

     If PtInRect(FastIMG1.ClientRect, MousePos) Then Begin

        // Dragged into the editor area - which AnsiChar are we over then?

        CharOffset := Min((MousePos.X Div (CellW * (DataWidth * 8))), EditWidth -1) + (Min(((MousePos.Y) Div (CellH * DataHeight)), EditHeight -1) * EditWidth);
        If CharOffset <= Length(AssignedChars) Div 2 Then
           DragCell := GetWord(@AssignedChars[(CharOffset * 2)+1]);

     End Else Begin

        Windows.GetCursorPos(MousePos);
        MousePos := FastIMG3.ScreenToClient(MousePos);

        If PtInRect(FastIMG3.ClientRect, MousePos) Then Begin

           // Dragged into the preview area - which AnsiChar are we over then?

           CharOffset := Min((MousePos.X Div (DataWidth * 8)), EditWidth -1) + (Min((MousePos.Y Div DataHeight), EditHeight -1) * EditWidth);
           If CharOffset <= Length(AssignedChars) Div 2 Then
              DragCell := GetWord(@AssignedChars[(CharOffset * 2)+1]);

        End Else Begin

           Windows.GetCursorPos(MousePos);
           MousePos := FastIMG6.ScreenToClient(MousePos);

           If PtInRect(FastIMG6.ClientRect, MousePos) Then Begin

              // Dragged into the onionskin area - which AnsiChar are we over then?

              CharOffset := Min((MousePos.X Div (DataWidth * 8)), EditWidth -1) + (Min((MousePos.Y Div DataHeight), EditHeight -1) * EditWidth);
              If CharOffset <= Length(OnionChars) Div 2 Then
                 DragCell := -GetWord(@OnionChars[(CharOffset * 2)+1]);

           End;

        End;

     End;

  End;

  For Idx := 1 To EditWidth * EditHeight Do Begin

     ValidCell := False;
     ValidOnion := False;

     If Idx <= Length(AssignedChars) Div 2 Then Begin

        If GetWord(@AssignedChars[((Idx -1) * 2)+1]) <> 0 Then Begin
           CellIdx := GetWord(@AssignedChars[((Idx - 1) * 2) +1]);
           Index := ((CellIdx -1) * DataWidth * DataHeight)+1; // Convert to an offset into the array
           ValidCell := True;
        End;

        If GetWord(@OnionChars[((Idx -1) * 2) +1]) <> 0 Then Begin
           OnionIdx := GetWord(@OnionChars[((Idx -1) * 2) +1]);
           OnionIndex := ((OnionIdx -1) * DataWidth * DataHeight)+1;
           ValidOnion := True;
        End;

     End;

     For F := 0 To DataHeight -1 Do Begin

        OffX2 := OffX;

        For G := 0 To DataWidth -1 Do Begin

           If ValidCell Then
              ByteVal := Ord(CurChars[Index])
           Else
              ByteVal := 0;

           If ValidOnion Then
              OnionByte := Ord(CurChars[OnionIndex])
           Else
              OnionByte := 0;

           For H := 7 DownTo 0 Do Begin

              CellType := 0;
              If (Not OnionShowing) And (ByteVal and (1 Shl H) > 0) Then Inc(CellType, 1);
              If OnionByte and (1 Shl H) > 0 Then Inc(CellType, 2);
              If (DragCell = CellIdx) or ((H = CurCellX) and (F = CurCellY) and (CellIdx = CurCellC) and (G = CurCellO)) Then Inc(CellType, 4);

              Case CellType of
                 0: Begin Colour := TFSpecWhite; PColour := TFSpecWhite; End;
                 1: Begin Colour := TFUDGBlack; PColour := TFUDGBlack; End;
                 2: Begin If ValidOnion Then Colour := TFHalf Else Colour := TFUDGBlack; PColour := TFSpecWhite; End;
                 3: Begin Colour := TFSpecBlack; PColour := TFUDGBlack; End;

                 4: Begin Colour := TFSpecWhiteB; PColour := TFSpecWhiteB; End;
                 5: Begin Colour := TFSpecBlue; PColour := TFSpecBlue; End;
                 6: Begin Colour := TFSpecWhiteB; PColour := TFSpecWhiteB; End;
                 7: Begin Colour := TFSpecBlue; PColour := TFSpecBlue; End;
              End;

              If -CellIdx = DragChar Then Begin
                 If OnionByte and (1 Shl H) > 0 Then
                    OnionColour := TFBlack
                 Else
                    OnionColour := TFSpecWhiteB;
              End Else
                 If OnionByte and (1 Shl H) > 0 Then
                    OnionColour := TFShadow
                 Else
                    OnionColour := TFBtnFace;

              X := OffX2 + (7-H)*CellW;
              Y := OffY + ((DataHeight -1) -F)*CellH;

              // Fill the cell

              FastDraw.FillRect(FastIMG1.Bmp, X+1, Y, X+CellW-1, Y+CellH-1, Colour);
              If Not OnionShowing Then
                 FastIMG3.Bmp.Pixels32[(Y Div CellH)+1, (X Div CellW)+1] := TFColorToTFColorA(PColour);
              FastIMG6.Bmp.Pixels32[(Y Div CellH)+1, (X Div CellW)+1] := TFColorToTFColorA(OnionColour);

              // Draw the grid lines
              For X1 := X to X + CellW -1 Do
                 FastIMG1.Bmp.Pixels32[Y, X1] := DisplayPalette[0];
              For X1 := Y to Y + CellH -1 Do Begin
                 If X = 0 Then
                    FastIMG1.Bmp.Pixels32[X1, X] := DisplayPalette[0];
                 FastIMG1.Bmp.Pixels32[X1, X+CellW] := DisplayPalette[0];
              End;
              If Y + CellH = FastIMG1.Bmp.Height Then
                 For X1 := X to X + CellW -1 Do FastIMG1.Bmp.Pixels32[Y+CellH -1, X1] := DisplayPalette[0]
              Else
                 For X1 := X to X + CellW -1 Do FastIMG1.Bmp.Pixels32[Y+CellH, X1] := DisplayPalette[0];

              // Use a red line to separate the chars in a multichar grid
              If (G = DataWidth -1) and (H = 0) Then
                 If X+CellW+1 < FastIMG1.Bmp.Width Then
                    For X1 := Y to Y + CellH -1 Do
                       FastIMG1.Bmp.Pixels32[X1, X+CellW] := DisplayPalette[2];

              If F = 0 Then
                 If Y+CellH < FastIMG1.Bmp.AbsHeight -1 Then
                    For X1 := X to X + CellW -1 Do
                       FastIMG1.Bmp.Pixels32[Y+CellH, X1] := DisplayPalette[2];
           End;

           Inc(Index);
           Inc(OnionIndex);
           Inc(OffX2, 8 * CellW);

        End;

     End;

     Inc(OffX, CellW * DataWidth * 8);
     If OffX >= EditWidth * DataWidth * 8 * CellW Then Begin
        OffX := 0;
        Dec(OffY, CellH * DataHeight);
     End;

  End;

  FastIMG1.Repaint;
  If Not OnionShowing Then
     FastIMG3.Repaint;
  FastIMG6.Repaint;

End;

procedure TUDGWindow.FormShow(Sender: TObject);
begin
  If FirstRun and (ProgStateFlag <> PS_Reset) Then Begin
     FormResize(nil);
     GrabFromMemory;
     FirstRun := False;
     GotUndo := False;
  End;
  CurCellX := -1;
  CurCellY := -1;
  CurCellX := -1;
  OnionDown := False;
  OnionShowing := False;
  RepaintChars;
  GetBIGChar(True);
end;

procedure TUDGWindow.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  OldCx, OldCy, OldCc, OldCo, CharOffset: Integer;
begin

  OldCx := CurCellX;
  OldCy := CurCellY;
  OldCc := CurCellC;
  OldCo := CurCellO;

  If PtInRect(FastIMG1.ClientRect, Point(X, Y)) Then Begin

     CharOffset := Min((X Div (CellW * 8 * DataWidth)), EditWidth -1) + (Min((Y Div (CellH * DataHeight)), EditHeight -1) * EditWidth);

     If CharOffset <= Length(AssignedChars) Div 2 Then Begin
        CurCellC := GetWord(@AssignedChars[(CharOffset * 2) +1]);
        If CurCellC <> 0 Then Begin
           If X = FastIMG1.Width -1 Then
              CurCellX := 0
           Else Begin
              CurCellX := ((X - ((X Div (CellW * 8 * DataWidth)) * (CellW * 8 * DataWidth))) Div CellW);
              CurCellO := CurCellX Div 8;
              CurCellX := 7 - (CurCellX mod 8);
           End;
           If Y = FastIMG1.Height -1 Then
              CurCellY := DataHeight -1
           Else
              CurCellY := (Y - ((Y Div (CellH * DataHeight)) * (CellH * DataHeight))) Div CellH;
        End Else Begin
           CurCellC := -1;
           CurCellX := -1;
           CurCellY := -1;
           CurCellO := -1;
        End;
     End Else Begin
        CurCellC := -1;
        CurCellX := -1;
        CurCellY := -1;
        CurCellO := -1;
     End;

     If MouseDown Then
        If (OldCx <> CurCellX) or (OldCy <> CurCellY) or (OldCc <> CurCellC) or (OldCo <> CurCellO) Then
           If BitState <> PaintMode Then
              FlipBit;

  End Else Begin

     CurCellC := -1;
     CurCellX := -1;
     CurCellY := -1;
     CurCellO := -1;

  End;

  GetBigChar(True);

end;

Function TUDGWindow.BitState: TUDGPaintMode;
Begin
  If Ord(CurChars[((CurCellC -1) * DataWidth * DataHeight) + (CurCellY * DataWidth) + CurCellO + 1]) and (1 Shl CurCellX) <> 0 Then
     Result := BitSet
  Else
     Result := BitUnSet;
End;

Procedure TUDGWindow.FlipBit;
Var
  Offset: DWord;
  ByteVal: Byte;
Begin

  If CurCellC > 0 Then Begin

     Offset := ((CurCellC -1) * DataWidth * DataHeight) + (CurCellY * DataWidth) + CurCellO + 1;

     ByteVal := Ord(CurChars[Offset]);
     ByteVal := ByteVal xor (1 Shl CurCellX);
     CurChars[Offset] := AnsiChar(ByteVal);
     GetBigChar(True);

     UpdateChar(CurCellC, CurCellC = CurChar);

     FastIMG2.Repaint;

  End;

End;

procedure TUDGWindow.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StoreUndo;
  If ssLeft in Shift Then Begin
     FlipBit;
     PaintMode := BitState;
     MouseDown := True;
     GetBIGChar(True);
  End;
  if (changed=false) Then Begin
        UDGWindow.Caption:= UDGWindow.Caption+'*';
        changed:=true;
  End;
end;

procedure TUDGWindow.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
  RenderCurrentAnim(False);
end;

procedure TUDGWindow.FastIMG1Exit(Sender: TObject);
begin
  CurCellC := -1;
  CurCellX := -1;
  CurCellY := -1;
  CurCellO := -1;
  GetBIGChar(True);
end;

procedure TUDGWindow.Button3Click(Sender: TObject);
begin

  UDGGrabWindow.WindowType := 1;
  UDGGrabWindow.Label1.Caption := 'Send graphics To...';

  UDGGrabWindow.Edit1.Text := IntToStr(Address);
  UDGGrabWindow.Edit2.Text := IntToStr(NumChars);

  CentreFormOnForm(UDGGrabWindow, Self);
  ShowWindow(UDGGrabWindow, True);

  If Not UDGGrabWindow.Cancelled Then Begin

     Address := UDGGrabWindow.Val1;
     NumChars := UDGGrabWindow.Val2;
     SendToMemory;

  End;

end;

Procedure TUDGWindow.SendToMemory;
Var
  F: DWord;
Begin
  // Check for valid address.
  While (Address -1 + (NumChars* DataWidth * DataHeight) > 65535) or (NumChars > 255) Do Dec(NumChars);
  // Now POKE to memory.
  For F := Address To Address+(NumChars * DataWidth * DataHeight) -1 Do
     If F > 16383 Then Memory[F] := Ord(CurChars[(F-Address)+1]);
  // And cause the listing to repaint with the new chars
  BASinOutput.RepaintBASIC(True);
  If TokenForm.Visible Then
     TokenForm.PageControl1Change(nil);
End;

procedure TUDGWindow.FromMemory1Click(Sender: TObject);
begin

  UDGGrabWindow.WindowType := 0;
  UDGGrabWindow.Label1.Caption := 'Grab graphics From...';

  UDGGrabWindow.Edit1.Text := IntToStr(Address);
  UDGGrabWindow.Edit2.Text := IntToStr(NumChars);
  UDGGrabWindow.Edit3.Text := IntToStr(DataWidth);
  UDGGrabWindow.Edit4.Text := IntToStr(DataHeight);

  CentreFormOnForm(UDGGrabWindow, Self);
  ShowWindow(UDGGrabWindow, True);

  If Not UDGGrabWindow.Cancelled Then Begin

     Address := UDGGrabWindow.Val1;
     NumChars := UDGGrabWindow.Val2;
     DataWidth := UDGGrabWindow.Val3;
     DataHeight := UDGGrabWindow.Val4;
     GrabFromMemory;

  End;

  MouseDown := False;

end;

procedure TUDGWindow.Exit1Click(Sender: TObject);
begin
  MouseDown := False;
  Close;
end;

procedure TUDGWindow.FromUDGs1Click(Sender: TObject);
begin
  If ProgramIs128k Then
     NumChars := 19
  Else
     NumChars := 21;
  Address := GetWord(@Memory[UDG]);
  DataWidth := 1;
  DataHeight := 8;
  While (Address -1 + (NumChars * DataWidth * DataHeight) > 65535) or (NumChars > 255) Do Dec(NumChars);
  GrabFromMemory;
  MouseDown := False;
end;

procedure TUDGWindow.FromCurrentCHARS1Click(Sender: TObject);
begin
  NumChars := 96;
  Address := GetWord(@Memory[CHARS])+256;
  DataWidth := 1;
  DataHeight := 8;
  While (Address -1 + (NumChars*8) > 65535) or (NumChars > 96) Do Dec(NumChars);
  GrabFromMemory;
  MouseDown := False;
end;

Procedure TUDGWindow.SaveAs(Filenamez: AnsiString);
Var
 Ext, NewFilename: AnsiString;
  StartAddr, DataLen: Word;
  FTypes: TBASICFiles;
Begin

  MouseDown := False;
  Filename := Filenamez;

  While NumChars > 255 Do Dec(NumChars);

  // Save the DATA as a CODE (.bsc) or Binary block.
  If Filename = '' Then Begin
     FTypes := [FTBsc, FTBin, FTCh8, FTAll];
     If (DataWidth = 1) and (DataHeight = 8) Then
        FTYpes := FTypes + [FTSpecCHR];
     Filename := OpenFile(Handle, 'Save CODE Block', FTypes, '', True, False);
     If Filename = '' Then Begin
        Exit;
     End;
  End;

  // An empty fileheader
  FileHeader := '                 ';

  Ext := Lowercase(ExtractFileExt(Filename));

  NewFileName := ExtractFilename(Filename);
  NewFilename := Copy(NewFilename, 1, Length(NewFilename)-Length(Ext));
  CopyMemory(@FileHeader[2], @NewFilename[1], 10);

  DataLen := NumChars*DataWidth*DataHeight;
  StartAddr := Address;

  If Ext = '.bsc' Then Begin

     PutWord(@FileHeader[$C], DataLen);
     PutWord(@FileHeader[$E], StartAddr);
     SetLength(FileBody, DataLen+17);
     CopyMemory(@FileBody[1], @FileHeader[1], 17);
     CopyMemory(@FileBody[18], @CurChars[1], DataLen);

  End Else Begin

     SetLength(FileBody, DataLen);
     CopyMemory(@FileBody[1], @CurChars[1], DataLen);

  End;
  UdgFileName:=Filename;
  UDGWindow.Caption:='UDG Character Editor - '+ ExtractFilename(UdgFileName);
  changed:=false;
  SaveFile;

end;
procedure TUDGWindow.SaveAs1Click(Sender: TObject);


Begin
  SaveAs('');
end;

procedure TUDGWindow.Open1Click(Sender: TObject);
Var
  Extension: AnsiString;
  CodeLength, LPos, Offset: DWord;
begin

  // Open a file requester for CODE blocks.
  // Ignore any .bsc's starting address, and send straight to
  // the CurChars[] Array, up to 768 bytes, or 96 chars.
  // Basically lifted and modified from Filing.LoadCode().

  MouseDown := False;

  Filename := OpenFile(Handle, 'Load Code Block', [FTBsc, FTBin, FTCh8, FTSpecCHR, FTAll], '', False, False);
  If Filename = '' Then Exit;
  If GetFile('.bsc') <> 'Ok' Then Exit;
  Extension := Lowercase(ExtractFileExt(Filename));

  If Extension = '.bsc' Then Begin
     CodeLength := GetWord(@FileArray[$0B]);
     Offset := 17;
  End Else Begin
     CodeLength := Length(FileArray);
     Offset := 0;
  End;

  // Get the desired data format

  UDGGrabWindow.WindowType := 2;
  UDGGrabWindow.Edit3.Text := IntToStr(DataWidth);
  UDGGrabWindow.Edit4.Text := IntToStr(DataHeight);
  UDGGrabWindow.FileSpec := Filename;

  CentreFormOnForm(UDGGrabWindow, Self);
  ShowWindow(UDGGrabWindow, True);

  If Not UDGGrabWindow.Cancelled Then Begin

     DataWidth := UDGGrabWindow.Val3;
     DataHeight := UDGGrabWindow.Val4;

     // Adjust CodeLength so that:
     // Only multiples of 8 are loaded (round to nearest AnsiChar),
     // and only a maximum of 768 bytes (96 chars) are loaded.

     CodeLength := DataWidth * DataHeight * (CodeLength Div (DataWidth * DataHeight));

     // Now Create NumChars and The CurChars Array.

     StoreUndo;

     NumChars := CodeLength Div (DataWidth * DataHeight);
     SetLength(ClipChar, DataWidth * DataHeight);
     CurChar := 0;
     AssignedChars := #1#0;
     CurChars := '';
     For LPos := Offset to CodeLength +Offset -1 Do
        CurChars := CurChars + AnsiChar(FileArray[LPos]);

     FormResize(nil);
     SetPaletteSize;
     SetLength(FileArray, 0);
     RepaintChars;
     GetBigChar(True);
     UpdateStatusBar;
     UdgFileName:=FileName;
     UDGWindow.Caption:='UDG Character Editor - '+ ExtractFilename(UdgFileName);
     changed:=false;

  End;

end;

procedure TUDGWindow.Copy1Click(Sender: TObject);
Var
  Offset, Y: Dword;
begin
  Offset := ((CurChar -1)*DataWidth*DataHeight)+1;
  For Y := 0 To (DataWidth * DataHeight) -1 Do
     ClipChar[Y] := Ord(CurChars[Offset+Y]);
  GotClip := True;
  MouseDown := False;
end;

procedure TUDGWindow.Paste1Click(Sender: TObject);
Var
  Offset, Y: Dword;
begin
  If GotClip Then Begin
     StoreUndo;
     Offset := ((CurChar -1)*DataWidth*DataHeight)+1;
     For Y := 0 To (DataWidth * DataHeight) -1 Do
        CurChars[Offset+Y] := AnsiChar(ClipChar[Y]);
     RepaintChars;
     GetBIGChar(True);
  End;
  MouseDown := False;
end;

procedure TUDGWindow.OnEnterMenuLoop(var Message: TMessage);
Begin
  Paste1.Enabled := GotClip;
  Undo1.Enabled := GotUndo;
  InsertNew1.Enabled := NumChars < 255;
  Delete1.Enabled := NumChars > 1;
  Fill1.Enabled := SpeedButton11.Visible;
  Invert2.Enabled := SpeedButton11.Visible;
  Flip2.Enabled := SpeedButton11.Visible;
  Rotate2.Enabled := SpeedButton11.Visible and SpeedButton15.Enabled;
  Shift2.Enabled := SpeedButton11.Visible;
  Rotate1.Enabled := SpeedButton1.Enabled;
  MouseDown := False;
End;

Procedure TUDGWindow.StoreUndo;
begin
  UndoBuffer := AnsiChar(CurChar)+CurChars;
  GotUndo := True;
End;

Procedure TUDGWindow.RestoreUndo;
begin
  CurChars := Copy(UndoBuffer, 2, 999999);
  NumChars := Length(CurChars) Div (DataWidth * DataHeight);
  CurChar := Ord(UndoBuffer[1]);
  GotUndo := False;
  RepaintChars;
  GetBIGChar(True);
End;

procedure TUDGWindow.Undo1Click(Sender: TObject);
begin
  RestoreUndo;
  MouseDown := False;
end;

Function TUDGWindow.CreateASMChar(Index: Integer): AnsiString;
Var
  X, Y, Z, Offset: DWord;
  CurByte: AnsiString;
  ByteVal: Byte;
Begin
  Result := '                  db    %';
  Offset := (Index*DataWidth*DataHeight)+1;
  For Y := 0 To DataHeight -1 Do Begin
     For X := 0 To DataWidth -1 Do Begin
        CurByte := '';
        ByteVal := Ord(CurChars[Offset+(Y * DataWidth)+X]);
        For Z := 0 To 7 Do
           If ByteVal and (1 shl Z) = 0 Then
              CurByte := '0'+CurByte
           Else
              CurByte := '1'+CurByte;
        If X < DataWidth -1 Then
           Result := Result + CurByte + ', %'
        Else
           Result := Result + CurByte;
     End;
     If Y < DataHeight -1 Then
        Result := Result + #13#10 + '                  db    %'
     Else
        Result := Result + #13#10;
  End;
End;

procedure TUDGWindow.CopyASMdb1Click(Sender: TObject);
begin
  MouseDown := False;
  ClipBoard.SetTextBuf(PChar(CreateASMChar(CurChar -1)));
end;

// Dragging and Selecting

procedure TUDGWindow.FastIMG2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  cW, cH: Integer;
begin

  If Button = mbLeft Then
     DragOp := 1
  Else
     If Button = mbRight Then
        DragOp := 2
     Else
        DragOp := 0;

  If DragOp <> 0 Then Begin

     cW := (DataWidth * 8) + 7;
     cH := DataHeight + 7;
     DragChar := (X Div cW) + (((Y Div cH) * (PaletteLineWidth)));
     If (DragChar >= NumChars) or ((X Div Cw) >= PaletteLineWidth) Then
        DragOp := 0
     Else Begin
        DragX := X;
        DragY := Y;
        SetCaptureControl(FastIMG2);
     End;

  End;

end;

procedure TUDGWindow.FastIMG2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  TempByte: Byte;
  NewChar: DWord;
  Src, Dst, cW, cH, Idx, DragCell, CharOffset: Integer;
  MousePos, NewPos, NewPos2: TPoint;
begin

  // This code will be called from FastIMG2, even if you've right-click-dragged.
  // Therefore, it would be a good idea to convert to the form's client coord system and then decide
  // upon which FastIMG we've been dropped (big grid, preview grid, palette, animation, onionskin).

  Windows.GetCursorPos(MousePos);

  NewPos := FastIMG2.ScreenToClient(MousePos);
  NewPos2 := Panel1.ScreenToClient(MousePos);

  If PtInRect(ScrollBox1.BoundsRect, NewPos2) And PtInRect(FastIMG2.ClientRect, NewPos) Then Begin

     // Dropped on the palette. Two checks are needed because the FastIMG could be bigger than the scrollbox
     // and therefore, graphics dropped on the anim-strip might trigger FastIMG2 instead.

     cW := (DataWidth * 8) + 7;
     cH := DataHeight + 7;

     NewChar := (X Div cW) + (((Y Div cH) * (FastIMG2.Bmp.Width Div cW)));

     If (NewChar < NumChars) And ((X Div Cw) < PaletteLineWidth) Then Begin

        CurChar := NewChar;
        If CurChar <> DragChar Then Begin

           StoreUndo;

           Src := 1+ (DragChar * DataHeight * DataWidth);
           Dst := 1+ (CurChar * DataHeight * DataWidth);

           Case DragOp of
              1: Begin // Copy over

                    For Idx := Src To Src + (DataWidth * DataHeight) -1 Do
                       CurChars[(Idx - Src) + Dst] := CurChars[Idx];

                 End;
              2: Begin // Exchange

                    For Idx := 0 To (DataWidth * DataHeight) -1 Do Begin
                       TempByte := Ord(CurChars[Src+Idx]);
                       CurChars[Src+Idx] := CurChars[Dst+Idx];
                       CurChars[Dst+Idx] := AnsiChar(TempByte);
                    End;

                 End;
           End;

        End;

        If (EditWidth = 1) and (EditHeight = 1) Then Begin
           PutWord(@AssignedChars[1], CurChar +1);
        End;

        Inc(CurChar);
        GetBIGChar(True);

     End;

  End Else Begin

     NewPos := FastIMG1.ScreenToClient(MousePos);
     If PtInRect(FastIMG1.ClientRect, NewPos) Then Begin

        // Dropped on the Big editing grid.
        // Left - drag, assign this AnsiChar to this grid position
        // Right - drag, copy this AnsiChar's contents to this grid position

        CharOffset := Min((NewPos.X Div (CellW * (DataWidth * 8))), EditWidth -1) + (Min(((NewPos.Y) Div (CellH * DataHeight)), EditHeight -1) * EditWidth);

        If CharOffset <= Length(AssignedChars) Div 2 Then Begin

           DragCell := GetWord(@AssignedChars[(CharOffset * 2) +1]);

           If DragOp = 1 Then Begin

              PutWord(@AssignedChars[(CharOffset * 2) +1], DragChar +1);
              GetBIGChar(True);

           End Else Begin

              StoreUndo;

              Src := 1+ (DragChar * DataHeight * DataWidth);
              Dst := DragCell * DataWidth * DataHeight;

              For Idx := Src To Src + (DataWidth * DataHeight) -1 Do
                 CurChars[(Idx - Src) + Dst] := CurChars[Idx];

              GetBIGChar(True);

           End;

        End;

     End Else Begin

        NewPos := FastIMG3.ScreenToClient(MousePos);
        If PtInRect(FastIMG3.ClientRect, NewPos) Then Begin

           // Dropped on the small preview grid - same operation as for the Big Grid.

           CharOffset := Min((NewPos.X Div (DataWidth * 8)), EditWidth -1) + (Min((NewPos.Y Div DataHeight), EditHeight -1) * EditWidth);

           If CharOffset <= Length(AssignedChars) Div 2 Then Begin

              DragCell := GetWord(@AssignedChars[(CharOffset * 2) +1]);

              If DragOp = 1 Then Begin

                 PutWord(@AssignedChars[(CharOffset * 2)+1], DragChar +1);
                 DragOp := 0;
                 GetBIGChar(True);

              End Else Begin

                 StoreUndo;

                 Src := 1+ (DragChar * DataWidth * DataHeight);
                 Dst := DragCell * DataWidth * DataHeight;

                 For Idx := Src To Src + (DataWidth * DataHeight) -1 Do
                    CurChars[(Idx - Src) + Dst] := CurChars[Idx];

                 GetBIGChar(True);

              End;

           End;

        End Else Begin

           NewPos := FastIMG6.ScreenToClient(MousePos);
           If PtInRect(FastIMG6.ClientRect, NewPos) Then Begin

              // Dropped on the small onionskin grid

              CharOffset := Min((NewPos.X Div (DataWidth * 8)), EditWidth -1) + (Min((NewPos.Y Div DataHeight), EditHeight -1) * EditWidth);

              If CharOffset <= Length(OnionChars) Then Begin

                 DragCell := GetWord(@OnionChars[(CharOffset * 2) +1]);
                 PutWord(@OnionChars[(CharOffset * 2) +1], DragChar +1);
                 GetBIGChar(True);

              End;

           End Else Begin

              NewPos := FastIMG5.ScreenToClient(MousePos);
              If PtInRect(FastIMG5.ClientRect, NewPos) Then Begin

                 // Dropped on the Animation strip. Parameters for insertion should already
                 // have been set by the mousemove procedure whilst dragging.

                 If AnimSelected <> 0 Then Begin

                    If AnimSelected < 0 Then Begin

                       // Insert operation.

                       AnimSelected := -AnimSelected;
                       If AnimSelected > AnimList.Count Then AnimSelected := AnimList.Count;
                       If AnimList.Count = 0 Then
                          AnimList.Add(#0#0+AnsiChar((DragChar +1) And 255) + AnsiChar((DragChar +1) Shr 8))
                       Else
                          AnimList.Insert(AnimSelected -1, #0#0+AnsiChar((DragChar +1) And 255) + AnsiChar((DragChar +1) Shr 8));
                       AnimScrollInView(AnimSelected -1);
                       AnimSelected := 0;

                    End Else Begin

                       If AnimSelected > AnimList.Count Then AnimSelected := AnimList.Count;
                       If AnimList.Count = 0 Then Begin

                          AnimList.Add(#0#0+AnsiChar((DragChar +1) And 255) + AnsiChar((DragChar +1) Shr 8));
                          AnimScrollInView(AnimList.Count -1);
                          AnimSelected := 0;

                       End Else Begin

                          If AnimSelected = AnimList.Count Then Begin

                             // Replace the last frame, and add a new one.

                             AnimList[AnimSelected -1] := #0#0+AnsiChar((DragChar +1) And 255) + AnsiChar((DragChar +1) Shr 8);
                             AnimList.Add(#0#0#0);
                             AnimScrollInView(AnimList.Count -1);
                             AnimSelected := 0;

                          End Else Begin

                             // Replace this frame
                             AnimList[AnimSelected -1] := #0#0+AnsiChar((DragChar +1) And 255) + AnsiChar((DragChar +1) Shr 8);
                             AnimScrollInView(AnimSelected -1);
                             AnimSelected := 0;

                          End;

                       End;

                    End;

                 End;

              End;

           End;

        End;

     End;

  End;

  AnimSelected := 0;
  RenderCurrentAnim(False);
  FastIMG4.Visible := False;
  ReleaseCapture;
  DragOp := 0;
  RepaintChars;

end;

procedure TUDGWindow.FastIMG2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  MousePos, NewPos: TPoint;
  PointingAt: Integer;
begin

  If DragOp <> 0 Then Begin

     If (DragX <> X) or (DragY <> Y) Then Begin

        DragX := X;
        DragY := Y;

        If Not FastIMG4.Visible Then Begin
           FastIMG4.Width := (DataWidth * 8)+4;
           FastIMG4.Height := DataHeight + 4;
           If (FastIMG4.Bmp.Width <> FastIMG4.Width) or (FastIMG4.Bmp.AbsHeight <> FastIMG4.Height) Then
              FastIMG4.Bmp.SetSize(FastIMG4.Width, FastIMG4.Height, 32);
           DrawChar(FastIMG4.Bmp, 2, 3, DragChar +1, True);
           UpdateDragImage;
           FastIMG4.Visible := True;
           FastIMG4.Repaint;
        End Else
           UpdateDragImage;

     End;

     // In the animation filmstrip?

     Windows.GetCursorPos(MousePos);
     NewPos := FastIMG5.ScreenToClient(MousePos);
     If PtInRect(FastIMG5.ClientRect, NewPos) Then Begin

        // Which frame are we pounting at?

        X := NewPos.X + AnimViewOffset;
        PointingAt := X Div ((DataWidth * 8) + 8);
        X :=  X - (PointingAt * ((DataWidth * 8) + 8));

        // Are we pointing at the blank space between frames? If so, this is an insert operation

        If PointingAt = AnimList.Count -1 Then
           AnimSelected := PointingAt +1
        Else
           If X < 3 Then
              AnimSelected := -(PointingAt +1)
           Else
              If X > (DataWidth * 8) +5 Then
                 AnimSelected := -(PointingAt +2)
              Else
                 AnimSelected := PointingAt +1;

        RenderCurrentAnim(False);

     End;

  End;

end;

Procedure TUDGWindow.UpdateDragImage;
Var
  X, Y: Integer;
  MousePos: TPoint;
Begin

  If DragOp <> 0 Then Begin

     Windows.GetCursorPos(MousePos);
     MousePos := ScreenToClient(MousePos);

     X := MousePos.X - (((DataWidth * 8) +4) Div 2);
     Y := MousePos.Y - ((DataHeight + 4) Div 2);

     If Y >= StatusBar1.Top - (DataHeight + 4) Then Y := StatusBar1.Top - (DataHeight + 5);
     If Y < 0 Then Y := 0;
     If X <= 0 Then X := 1;
     If X >= ClientWidth - ((DataWidth * 8) + 4) Then X := ClientWidth - ((DataWidth * 8) + 5);
     FastIMG4.SetBounds(X, Y, (DataWidth * 8) +4, DataHeight + 4);
     GetBIGChar(True);

  End;

End;

Procedure TUDGWindow.UpdateStatusBar;
Begin

  StatusBar1.Panels[0].Text := ' Editing '+IntToStr(NumChars) + ' graphic(s) from address '+IntToStr(Address) + '-'+IntToStr(FastIMG2.Bmp.Width );

  StatusBar1.Repaint;

End;

procedure TUDGWindow.Insertnew1Click(Sender: TObject);
Var
  NewChar: AnsiString;
  Y, NewAnimChar: Integer;
begin

  StoreUndo;
  NewChar := '';
  For Y := 0 To (DataWidth * DataHeight) -1 Do
     NewChar := NewChar + AnsiChar(0);
  CurChars := Copy(CurChars, 1, (CurChar -1) * DataWidth * DataHeight)+NewChar+Copy(CurChars, ((CurChar -1) * DataWidth * DataHeight)+1, 999999);
  Inc(NumChars);
  SetPaletteSize;
  UpdateStatusBar;
  GetBIGChar(True);
  RepaintChars;
  MouseDown := False;
  If AnimList.Count > 0 Then
     For Y := 0 To AnimList.Count -1 Do
        If GetWord(@AnimList[Y][3]) >= CurChar Then Begin
           NewAnimChar := GetWord(@AnimList[Y][3]) +1;
           AnimList[Y] := Copy(AnimList[Y], 1, 2)+AnsiChar(NewAnimChar And 255) + AnsiChar(NewAnimChar Shr 8);
        End;

end;

procedure TUDGWindow.Delete1Click(Sender: TObject);
Var
  Y, NewAnimChar: Integer;
begin

  StoreUndo;
  CurChars := Copy(CurChars, 1, (CurChar -1) * DataWidth * DataHeight)+Copy(CurChars, (CurChar * DataWidth * DataHeight)+1, 999999);
  Dec(NumChars);
  If CurChar > NumChars Then CurChar := NumChars;
  UpdateStatusBar;
  SetPaletteSize;
  GetBIGChar(True);
  RepaintChars;
  MouseDown := False;
  If AnimList.Count > 0 Then
     For Y := 0 To AnimList.Count -1 Do
        If GetWord(@AnimList[Y][3]) >= CurChar Then Begin
           NewAnimChar := GetWord(@AnimList[Y][3]) -1;
           AnimList[Y] := Copy(AnimList[Y], 1, 2)+AnsiChar(NewAnimChar And 255) + AnsiChar(NewAnimChar Shr 8);
        End;

end;

procedure TUDGWindow.UDGEditorHelp1Click(Sender: TObject);
begin
  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_udg_editor.html'), HH_DISPLAY_TOPIC, 0);
  MouseDown := False;
end;

procedure TUDGWindow.FormResize(Sender: TObject);
Var
  Ew, Eh, PreW, PreH, SpaceW, SpaceH, DataW, DataH: Integer;
begin

  Panel2.SetBounds(0, ClientHeight - StatusBar1.Height - Panel2.Height, ClientWidth, Panel2.Height);
  FastIMG5.SetBounds(FastIMG5.Left, FastIMG5.Top, Panel2.Width - SpeedButton21.Width - 16, FastIMG5.Height);
  FastIMG5.Bmp.SetSize(FastIMG5.Width, FastIMG5.Height, 32);
  FastIMG5.Bmp.Clear(TfBtnFace);
  Panel1.SetBounds(0, 0, ClientWidth, Panel2.Top);

  PreW := 2 + (EditWidth * DataWidth * 8);
  PreH := 2 + (EditHeight * DataHeight);

  SpaceW := Panel3.ClientWidth - 24 - PreW;
  SpaceH := Panel3.ClientHeight - 16;

  DataW := EditWidth * DataWidth * 8;
  DataH := EditHeight * DataHeight;

  Ew := SpaceW;
  Eh := SpaceH;

  If DataW/DataH < SpaceW/SpaceH Then
     Ew := MulDiv(Eh, DataW, DataH)
  Else
     Eh := MulDiv(Ew, DataH, DataW);

  Ew := (Ew Div DataW) * DataW;
  Eh := (Eh Div DataH) * DataH;

  FastIMG1.SetBounds(8, 8, Ew +1, Eh +1);
  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 32);

  CellW := FastIMG1.Bmp.Width Div (EditWidth * DataWidth * 8);
  CellH := FastIMG1.Bmp.AbsHeight Div (EditHeight * DataHeight);

  FastIMG3.SetBounds(Panel3.ClientWidth - 8 - PreW, FastIMG1.Top, PreW, PreH);
  FastIMG3.Bmp.SetSize(FastIMG3.Width, FastIMG3.Height, 32);

  FastIMG6.SetBounds(Panel3.ClientWidth - 8 - PreW, FastIMG3.Top + FastIMG3.Height + 8, PreW, PreH);
  FastIMG6.Bmp.SetSize(FastIMG6.Width, FastIMG6.Height, 32);

  GetBIGChar(True);
  If AnimList <> Nil Then
     RenderCurrentAnim(False);
  
end;

Procedure TUDGWindow.SetPaletteSize;
Var
  cW, cH, nW, nH, Idx: Integer;
Begin

  // Set up the size of the scrollbox which contains the palette

  cW := (DataWidth * 8) + 7;
  cH := DataHeight + 7;

  ScrollBox1.VertScrollBar.Visible := False;

  nW := (ScrollBox1.ClientWidth - 8) Div cW;
  nH := (NumChars Div nW) + 1;

  If (nH * cH) +1 > ScrollBox1.ClientHeight Then Begin
     ScrollBox1.VertScrollBar.Visible := True;
     nW := (ScrollBox1.ClientWidth - 8 - GetSystemMetrics(SM_CXVSCROLL)) Div cW;
     nH := (NumChars Div nW) + 1;
  End;

  For Idx := 1 To Length(OnionChars) Do
     OnionChars[Idx] := #0;

  FastIMG2.Bmp.SetSize((nW * cW) +1, (nH * cH) + 1, 32);
  FastIMG2.SetBounds(Max((ScrollBox1.ClientWidth Div 2) - (FastIMG2.Bmp.Width Div 2), 0), 8, Max(FastIMG2.Bmp.Width, ScrollBox1.ClientWidth), Max(FastIMG2.Bmp.AbsHeight, ScrollBox1.Height));

  RenderCurrentAnim(True);
  RepaintChars;


End;

Procedure TUDGWindow.RotateRightChar(Index: Integer);
Var
  Idx, Offset, X, Y, Bit, RowIdx, RowBit: Integer;
  NewChar: AnsiString;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new rotated AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // For a 90 degree right rotation, we need to take each column, left to right,
  // and use those as the new rows. Easier to convert from X/Y to Offset and Bit.

  RowIdx := 1;

  For X := 0 To (DataWidth * 8) -1 Do Begin

     RowBit := 128;
     Offset := Index + ((DataHeight -1) * DataWidth) + (X Div 8);
     Bit := 1 Shl (7 - (X Mod 8));

     For Y := DataHeight -1 DownTo 0 Do Begin

        If Byte(CurChars[Offset]) And Bit <> 0 Then
           NewChar[RowIdx] := AnsiChar(Byte(NewChar[RowIdx]) or RowBit);

        RowBit := RowBit Shr 1;
        If RowBit = 0 Then Begin
           Inc(RowIdx);
           RowBit := 128;
        End;

        Dec(Offset, DataWidth);

     End;

  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.RotateLeftChar(Index: Integer);
Var
  Idx, Offset, X, Y, Bit, RowIdx, RowBit: Integer;
  NewChar: AnsiString;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new rotated AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // For a 90 degree Left rotation, we need to take each column, right to left,
  // and use those as the new rows. Easier to convert from X/Y to Offset and Bit.

  RowIdx := 1;

  For X := (DataWidth * 8) -1 DownTo 0 Do Begin

     RowBit := 128;
     Offset := Index + (X Div 8);
     Bit := 1 Shl (7 - (X Mod 8));

     For Y := DataHeight -1 DownTo 0 Do Begin

        If Byte(CurChars[Offset]) And Bit <> 0 Then
           NewChar[RowIdx] := AnsiChar(Byte(NewChar[RowIdx]) or RowBit);

        RowBit := RowBit Shr 1;
        If RowBit = 0 Then Begin
           Inc(RowIdx);
           RowBit := 128;
        End;

        Inc(Offset, DataWidth);

     End;

  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.MirrorLRChar(Index: Integer);
Var
  ByteVal, FlippedByte: Byte;
  Idx, X, Y, G, H, Offset: Integer;
  NewChar: AnsiString;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new mirrored AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // Mirror bitwise first, then store mirrored in the data line

  For Y := 0 To DataHeight -1 Do Begin
     For X := 0 To DataWidth -1 Do Begin

        ByteVal := Byte(CurChars[Index + (Y * DataWidth) + X]);
        FlippedByte := 0;
        G := 1;
        For H := 7 DownTo 0 Do Begin
           If ByteVal and (1 Shl H) <> 0 Then FlippedByte := FlippedByte Or G;
           G := G Shl 1;
        End;
        NewChar[(Y * DataWidth) + ((DataWidth -1) -X) +1] := AnsiChar(FlippedByte);

     End;
  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.MirrorVTChar(Index: Integer);
Var
  Y, X, Offset, Idx: Integer;
  NewChar: AnsiString;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new mirrored AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // Mirror simply by copying rows from top to bottom.

  For Y := 0 To DataHeight -1 Do Begin

     Offset := Index + (Y * DataWidth);

     For X := 0 To DataWidth -1 Do Begin

        NewChar[(((DataHeight - 1) - Y) * DataWidth) +X +1] := CurChars[Offset + X];

     End;

  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.ShiftLeftChar(Index: Integer);
Var
  NewChar: AnsiString;
  Offset, Idx, X, Y: Integer;
  ByteVal, SavedBit1, SavedBit2: Byte;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new shifted AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // Row by row, shift each byte from right to left and save the high bit from each.

  For Y := 0 To DataHeight -1 Do Begin

     Offset := Index + (Y * DataWidth);
     SavedBit1 := 0;

     For X := DataWidth -1 DownTo 0 Do Begin

        ByteVal := Byte(CurChars[OffSet + X]);
        SavedBit2 := (ByteVal and 128) Shr 7;
        ByteVal := (ByteVal Shl 1) or SavedBit1;
        SavedBit1 := SavedBit2;
        NewChar[(Y * DataWidth) +X +1] := AnsiChar(ByteVal);

        If X = 0 Then
           NewChar[(Y * DataWidth) + DataWidth] := AnsiChar(Byte(NewChar[(Y* DataWidth) + DataWidth]) or SavedBit1);

     End;

  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.ShiftRightChar(Index: Integer);
Var
  NewChar: AnsiString;
  Offset, Idx, X, Y: Integer;
  ByteVal, SavedBit1, SavedBit2: Byte;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new shifted AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // Row by row, shift each byte from left to right and save the high bit from each.

  For Y := 0 To DataHeight -1 Do Begin

     Offset := Index + (Y * DataWidth);
     SavedBit1 := 0;

     For X := 0 To DataWidth -1 Do Begin

        ByteVal := Byte(CurChars[OffSet + X]);
        SavedBit2 := (ByteVal and 1) Shl 7;
        ByteVal := (ByteVal Shr 1) or SavedBit1;
        SavedBit1 := SavedBit2;
        NewChar[(Y * DataWidth) +X +1] := AnsiChar(ByteVal);

        If X = DataWidth -1 Then
           NewChar[(Y * DataWidth) + 1] := AnsiChar(Byte(NewChar[(Y* DataWidth) + 1]) or SavedBit1);

     End;

  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.ShiftUpChar(Index: Integer);
Var
  NewChar: AnsiString;
  Offset, Idx, X, Y: Integer;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new shifted AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // Possibly the simplest shift operation - just move all the rows up one,
  // except the very top row, which goes on the bottom.

  For Y := DataHeight -1 DownTo 0 Do Begin

     OffSet := Index + (Y * DataWidth);

     For X := 0 To DataWidth -1 Do Begin

        If Y <> 0 Then
           NewChar[((Y -1) * DataWidth) + X + 1] := CurChars[Offset + X]
        Else
           NewChar[((DataHeight -1) * DataWidth) + X + 1] := CurChars[Offset + X];

     End;

  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.ShiftDownChar(Index: Integer);
Var
  NewChar: AnsiString;
  Offset, Idx, X, Y: Integer;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Create a temporary storage space for the new shifted AnsiChar

  SetLength(NewChar, DataWidth * DataHeight);
  For Idx := 1 To Length(NewChar) Do
     NewChar[Idx] := #0;

  // Again, possibly the simplest shift operation - just move all the rows down one,
  // except the very bottom row, which goes on the top.

  For Y := 0 To DataHeight -1 Do Begin

     OffSet := Index + (Y * DataWidth);

     For X := 0 To DataWidth -1 Do Begin

        If Y <> DataHeight -1 Then
           NewChar[((Y +1) * DataWidth) + X + 1] := CurChars[Offset + X]
        Else
           NewChar[X + 1] := CurChars[Offset + X];

     End;

  End;

  For Offset := 1 To Length(NewChar) Do
     CurChars[Index + Offset -1] := NewChar[Offset];

End;

Procedure TUDGWindow.ClearChar(Index: Integer);
Var
  X, Y: Integer;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // Just set all the bytes for this AnsiChar to 0.

  For Y := 0 To DataHeight -1 Do
     For X := 0 To DataWidth -1 Do
        CurChars[Index + (Y * DataWidth) + X] := AnsiChar(0);

End;

Procedure TUDGWindow.InvertChar(Index: Integer);
Var
  X, Y: Integer;
Begin

  // Convert to an index in the array

  Index := ((Index -1) * DataWidth * DataHeight)+1;

  // XOR all bytes in the AnsiChar with 255

  For Y := 0 To DataHeight -1 Do
     For X := 0 To DataWidth -1 Do
        CurChars[Index + (Y * DataWidth) + X] := AnsiChar(Byte(CurChars[Index + (Y * DataWidth) + X]) xor 255);

End;

procedure TUDGWindow.SpeedButton6Click(Sender: TObject);
begin

  StoreUndo;
  ClearChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton5Click(Sender: TObject);
begin

  StoreUndo;
  InvertChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton3Click(Sender: TObject);
begin

  StoreUndo;
  MirrorLRChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton4Click(Sender: TObject);
begin

  StoreUndo;
  MirrorVTChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton1Click(Sender: TObject);
begin

  StoreUndo;
  RotateLeftChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton2Click(Sender: TObject);
begin

  StoreUndo;
  RotateRightChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton7Click(Sender: TObject);
begin

  StoreUndo;
  ShiftLeftChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton8Click(Sender: TObject);
begin

  StoreUndo;
  ShiftRightChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton9Click(Sender: TObject);
begin

  StoreUndo;
  ShiftUpChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton10Click(Sender: TObject);
begin

  StoreUndo;
  ShiftDownChar(CurChar);
  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton11Click(Sender: TObject);
Var
  Idx: Integer;
begin

  // Clear the whole editing grid

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     ClearChar(GetWord(@AssignedChars[((Idx -1) * 2)+1]));

  GetBIGChar(True);
  RepaintChars;

end;

Function TUDGWindow.GetAssignedChars: AnsiString;
Var
  cChar: Word;
  Idx, Idx2: Integer;
  Found: Boolean;
Begin

  Result := '';
  For Idx := 0 To (Length(AssignedChars) Div 2) -1 Do Begin
     cChar := GetWord(@AssignedChars[(Idx * 2) +1]);
     Found := False;
     For Idx2 := 0 To Length(Result) -1 Do
        If GetWord(@Result[(Idx2 * 2) +1]) = cChar Then
           Found := True;
     If Not Found Then
        Result := Result + AnsiChar(cChar And 255) + AnsiChar(cChar Shr 8);
  End;

End;

Procedure TUDGWindow.SpeedButton12Click(Sender: TObject);
Var
  Idx: Integer;
  CharsDone: AnsiString;
begin

  // Invert the whole editing grid

  StoreUndo;
  CharsDone := GetAssignedChars;
  For Idx := 0 To (Length(CharsDone) Div 2) -1 Do
     InvertChar(GetWord(@CharsDone[(Idx * 2) +1]));

  GetBIGChar(True);
  RepaintChars;

end;

Procedure TUDGWindow.ArrangeChars(SrcChars, DstChars: AnsiString);
Var
  Y, Idx, Src, Dst: Integer;
  NewData: AnsiString;
Begin

  NewData := CurChars;

  For Idx := 1 To EditWidth * EditHeight Do Begin
     Src := GetWord(@SrcChars[(Idx * 2)-1]);
     Dst := GetWord(@DstChars[(Idx * 2)-1]);
     Src := 1+ ((Src -1) * DataHeight * DataWidth);
     Dst := 1+ ((Dst -1) * DataHeight * DataWidth);
     For Y := 0 To (DataWidth * DataHeight) -1 Do
        NewData[Src+Y] := CurChars[Dst+Y];
  End;

  CurChars := NewData;

End;

procedure TUDGWindow.SpeedButton13Click(Sender: TObject);
Var
  Idx, Y, X: Integer;
  NewChars: AnsiString;
begin

  // Flip left/right for each AnsiChar in the grid

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     MirrorLRChar(GetWord(@AssignedChars[(Idx * 2) -1]));

  // Mirror the grid itself

  NewChars := AssignedChars;

  For Y := 0 To EditHeight -1 Do
     For X := 1 To EditWidth Do
        PutWord(@NewChars[(((Y * EditWidth) + (EditWidth + 1 - X)) * 2) -1], GetWord(@AssignedChars[(((Y * EditWidth) + X) * 2) -1]));

  ArrangeChars(AssignedChars, NewChars);

  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton14Click(Sender: TObject);
Var
  Idx, Y, X: Integer;
  NewChars: AnsiString;
begin

  // Flip vertically for each AnsiChar in the grid

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     MirrorVTChar(GetWord(@AssignedChars[(Idx * 2) -1]));

  // Mirror the grid itself

  NewChars := AssignedChars;

  For X := 1 To EditWidth Do
     For Y := 0 To EditHeight -1 Do
        PutWord(@NewChars[((((EditHeight - 1 - Y) * EditWidth) + X) * 2) -1], GetWord(@AssignedChars[(((Y * EditWidth) + X) * 2) -1]));

  ArrangeChars(AssignedChars, NewChars);

  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton15Click(Sender: TObject);
Var
  Idx, Y, X: Integer;
  NewChars: AnsiString;
begin

  // Rotate each AnsiChar in the grid

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     RotateLeftChar(GetWord(@AssignedChars[(Idx * 2)-1]));

  // Rotate the grid itself

  NewChars := AssignedChars;

  For X := EditWidth DownTo 1 Do
     For Y := 0 To EditHeight -1 Do
        PutWord(@NewChars[((Y + 1 + ((EditWidth - X) * EditHeight)) * 2) -1], GetWord(@AssignedChars[(((Y * EditWidth) + X) * 2) -1]));

  ArrangeChars(AssignedChars, NewChars);

  Idx := EditWidth;
  EditWidth := EditHeight;
  EditHeight := Idx;
  FormResize(nil);

  RepaintChars;

end;

procedure TUDGWindow.SpeedButton16Click(Sender: TObject);
Var
  Idx, Y, X: Integer;
  NewChars: AnsiString;
begin

  // Rotate each AnsiChar in the grid

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     RotateRightChar(GetWord(@AssignedChars[(Idx * 2)-1]));

  // Rotate the grid itself

  NewChars := AssignedChars;

  For X := 1 To EditWidth Do
     For Y := EditHeight -1 DownTo 0 Do
        PutWord(@NewChars[(((EditHeight - Y) + ((X -1) * EditHeight)) * 2) -1], GetWord(@AssignedChars[(((Y * EditWidth) + X) * 2)-1]));

  ArrangeChars(AssignedChars, NewChars);

  Idx := EditWidth;
  EditWidth := EditHeight;
  EditHeight := Idx;
  FormResize(Nil);

  RepaintChars;

end;

procedure TUDGWindow.SpeedButton17Click(Sender: TObject);
Var
  Idx, Gx, Gy, X, Y, Index, SavedBit, SavedBit2: Integer;
begin

  // Shift the grid left -
  // Overflow runs into adjacent chars...

  // First, shift each AnsiChar left one pixel

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     ShiftLeftChar(GetWord(@AssignedChars[(Idx * 2)-1]));

  For Gy := 0 To EditHeight -1 Do

     For Y := 0 To DataHeight -1 Do Begin

        Index := GetWord(@AssignedChars[(((Gy * EditWidth) + 1) * 2) -1]);
        Index := ((Index -1) * DataWidth * DataHeight) +1;

        SavedBit := Byte(CurChars[Index + (Y * DataWidth) + DataWidth -1]) And 1;

        For Gx := EditWidth DownTo 1 Do Begin

           Index := GetWord(@AssignedChars[(((Gy * EditWidth) + Gx) * 2) -1]);
           Index := ((Index -1) * DataWidth * DataHeight) +1;

           X := Index + DataWidth -1;
           SavedBit2 := Byte(CurChars[X + (Y * DataWidth)]) and 1;
           CurChars[X + (Y * DataWidth)] := AnsiChar((Byte(CurChars[X + (Y * DataWidth)]) And 254) + SavedBit);
           SavedBit := SavedBit2;

        End;

     End;

  GetBIGChar(True);
  RepaintChars;

End;

procedure TUDGWindow.SpeedButton18Click(Sender: TObject);
Var
  Idx, Gx, Gy, X, Y, Index, SavedBit, SavedBit2: Integer;
begin

  // Shift the grid right -
  // Overflow runs into adjacent chars...

  // First, shift each AnsiChar right one pixel

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     ShiftRightChar(GetWord(@AssignedChars[(Idx * 2)-1]));

  For Gy := 0 To EditHeight -1 Do

     For Y := 0 To DataHeight -1 Do Begin

        Index := GetWord(@AssignedChars[(((Gy * EditWidth) + EditWidth) * 2) -1]);
        Index := ((Index -1) * DataWidth * DataHeight) +1;

        SavedBit := Byte(CurChars[Index + (Y * DataWidth)]) And 128;

        For Gx := 1 To EditWidth Do Begin

           Index := GetWord(@AssignedChars[(((Gy * EditWidth) + Gx) * 2) -1]);
           Index := ((Index -1) * DataWidth * DataHeight) +1;

           X := Index;
           SavedBit2 := Byte(CurChars[X + (Y * DataWidth)]) and 128;
           CurChars[X + (Y * DataWidth)] := AnsiChar((Byte(CurChars[X + (Y * DataWidth)]) And 127) + SavedBit);
           SavedBit := SavedBit2;

        End;

     End;

  GetBIGChar(True);
  RepaintChars;

End;

procedure TUDGWindow.SpeedButton19Click(Sender: TObject);
Var
  Idx, Gx, Gy, X, Index, SavedByte, SavedByte2: Integer;
begin

  // Shift the grid up -
  // Overflow runs into adjacent chars...

  // First, shift each AnsiChar right one pixel

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     ShiftUpChar(GetWord(@AssignedChars[(Idx * 2)-1]));

  For Gx := 1 To EditWidth Do

     For X := 1 To DataWidth Do Begin

        Index := GetWord(@AssignedChars[(Gx * 2)-1]);
        Index := ((Index -1) * DataWidth * DataHeight) + X;

        SavedByte := Byte(CurChars[Index + (DataWidth * (DataHeight -1))]);

        For Gy := EditHeight -1 DownTo 0 Do Begin

           Index := GetWord(@AssignedChars[(((Gy * EditWidth) + Gx) * 2) -1]);
           Index := ((Index -1) * DataWidth * DataHeight) + X;

           SavedByte2 := Byte(CurChars[Index + (DataWidth * (DataHeight -1))]);
           CurChars[Index + (DataWidth * (DataHeight -1))] := AnsiChar(SavedByte);
           SavedByte := SavedByte2;

        End;

     End;

  GetBIGChar(True);
  RepaintChars;

end;

procedure TUDGWindow.SpeedButton20Click(Sender: TObject);
Var
  Idx, Gx, Gy, X, Index, SavedByte, SavedByte2: Integer;
begin

  // Shift the grid down -
  // Overflow runs into adjacent chars...

  // First, shift each AnsiChar right one pixel

  StoreUndo;
  For Idx := 1 To EditWidth * EditHeight Do
     ShiftDownChar(GetWord(@AssignedChars[(Idx * 2) -1]));

  For Gx := 1 To EditWidth Do

     For X := 1 To DataWidth Do Begin

        Index := GetWord(@AssignedChars[((Gx + ((EditHeight -1 )* EditWidth)) * 2) -1]);
        Index := ((Index -1) * DataWidth * DataHeight) + X;

        SavedByte := Byte(CurChars[Index]);

        For Gy := 0 To EditHeight -1 Do Begin

           Index := GetWord(@AssignedChars[(((Gy * EditWidth) + Gx) * 2) -1]);
           Index := ((Index -1) * DataWidth * DataHeight) + X;

           SavedByte2 := Byte(CurChars[Index]);
           CurChars[Index] := AnsiChar(SavedByte);
           SavedByte := SavedByte2;

        End;

     End;

  GetBIGChar(True);
  RepaintChars;

end;

Procedure TUDGWindow.ExchangeChars(Src, Dst: Integer);
Var
  Idx: Integer;
  TempByte: Byte;
Begin

  Src := 1+ ((Src -1) * DataHeight * DataWidth);
  Dst := 1+ ((Dst -1) * DataHeight * DataWidth);

  For Idx := 0 To (DataWidth * DataHeight) -1 Do Begin
     TempByte := Ord(CurChars[Src+Idx]);
     CurChars[Src+Idx] := CurChars[Dst+Idx];
     CurChars[Dst+Idx] := AnsiChar(TempByte);
  End;

End;

procedure TUDGWindow.Fill1Click(Sender: TObject);
Var
  Idx: Integer;
begin

  For Idx := 1 To EditWidth * EditHeight Do
     PutWord(@AssignedChars[(Idx * 2)-1], CurChar);

  RepaintChars;
  GetBIGChar(True);
  MouseDown := False;

end;

procedure TUDGWindow.Setup1Click(Sender: TObject);
begin

  GridSetUpWindow.EditType := 0;
  GridSetUpWindow.NumChars := NumChars;
  CentreFormOnForm(GridSetupWindow, Self);
  ShowWindow(GridSetupWindow, True);
  If Not GridSetUpWindow.Cancelled Then Begin

     SetUpGrid(GridSetUpWindow.EditWidth, GridSetUpWindow.EditHeight, DataWidth, DataHeight);

     FormResize(nil);
     RepaintChars;
  End;
  MouseDown := False;

end;

procedure TUDGWindow.Cut1Click(Sender: TObject);
begin

  Copy1Click(nil);
  Delete1Click(nil);
  MouseDown := False;

end;

procedure TUDGWindow.Send1Click(Sender: TObject);
Var
  Binary: AnsiString;
  F: Integer;
begin

  // Send the current data as a binary to the Binary Files import tool

  Binary := '';
  For F := 1 To NumChars * DataWidth * DataHeight Do
     Binary := Binary + CurChars[F];

  BinaryWindow.AddBinaryEx('GraphicData'+IntToStr(CurObj), Binary, btMemory, Address);
  CentreFormOnForm(BinaryWindow, Self);
  BinaryWindow.ShowModal;
  Inc(CurObj);
  MouseDown := False;
  BinaryWindow.Caption := 'Export Graphics to Memory';

end;

procedure TUDGWindow.New1Click(Sender: TObject);
Var
  Idx: Integer;
begin

  CentreFormOnForm(UDGNew, Self);
  ShowWindow(UDGNew, True);

  If UDGNew.NumGFX > 0 Then Begin

     // Create a new palette of empty graphics

     StoreUndo;

     CurChars := '';
     NumChars := UDGNew.NumGFX;
     DataWidth := UDGNew.WidthGFX;
     DataHeight := UDGNew.HeightGFX;
     SetLength(ClipChar, DataWidth * DataHeight);
     For Idx := 1 to NumChars * DataWidth * DataHeight Do
        CurChars := CurChars + AnsiChar(0);

     SetUpGrid(EditWidth, EditHeight, DataWidth, DataHeight);
     CurChar := 1;
     FormResize(nil);
     SetPaletteSize;
     RepaintChars;
     GetBigChar(True);
     UpdateStatusBar;
     UdgFileName:='';
     UDGWindow.Caption:='UDG Character Editor';
     changed:=false;

  End;
  MouseDown := False;

end;

procedure TUDGWindow.Copysetasasm1Click(Sender: TObject);
Var
  Text, CurChar: AnsiString;
  Loop: Integer;
begin
  Text := '';
  For Loop := 0 To NumChars -1 Do Begin
     CurChar := CreateASMChar(Loop);
     Text := Text + CurChar;
     If Loop < NumChars -1 Then Text := Text + #13#10;
  End;
  ClipBoard.SetTextBuf(PChar(Text));
  MouseDown := False;
end;

procedure TUDGWindow.FormDestroy(Sender: TObject);
Var
  CurHeight, NewHeight: Integer;
begin

  AnimList.Free;
  AnimList := Nil;

  CurHeight := Panel2.Height;
  NewHeight := 8 + 6 + 6 + 4 + 4 + 4 + 4 + 4;
  Panel2.Height := NewHeight;
  SizeForm(Self, Left, Top, Width, Height + (NewHeight - CurHeight));

end;

Procedure TUDGWindow.RenderCurrentAnim(NewSet: Boolean);
Var
  CurHeight, NewHeight, Idx, Idx2, Time, X, Y, TempVal: Integer;
  ByteVal, BitValue: Byte;
  TempStr, Frame, BlankFrame: AnsiString;
  TempDIB: TFastDIB;
  ClrWhite, ClrBlack, ClrDark, ClrBtnFace, ClrShadow: TFColorA;
Begin

  // Set the animwindow size

  CurHeight := Panel2.Height;
  NewHeight := DataHeight + 32;
  If NewSet or (NewHeight <> CurHeight) Or (AnimList.Count = 0) Then Begin

     Panel2.Height := NewHeight;
     SizeForm(Self, Left, Top, Width, Height + (NewHeight - CurHeight));
     SpeedButton21.Top := ((DataHeight + 20) Div 2) - (SpeedButton21.Height Div 2);
     FastIMG5.SetBounds(SpeedButton21.Left + SpeedButton21.Width + 8, 0, Panel2.ClientWidth - (SpeedButton21.Left + SpeedButton21.Width + 8), Panel2.ClientHeight);
     FastIMG5.Bmp.SetSize(FastIMG5.Width, FastIMG5.Height, 32);
     FastIMG5.Bmp.Clear(TfBtnFace);

     // The anim height has changed, so clear the current anim.

     AnimList.Clear;
     AnimList.Add(#0#0#0#0);
     AnimSelected := 0;
     AnimViewOffset := 0;

  End;

  SpeedButton21.Enabled := AnimList.Count > 1;

  // Now render the frames

  TempDIB := TFastDIB.Create;
  TempVal := ((DataWidth * 8) + 8) * AnimList.Count;
  TempDIB.SetSize(Max(TempVal, FastIMG5.Width), DataHeight + 20, 32);

  // Checks for visibility

  If TempVal < FastIMG5.Width Then
     AnimViewOffset := 0
  Else
     If AnimViewOffset + FastIMG5.Width > TempVal Then
        AnimViewOffset := TempVal - FastIMG5.Width;
  // Render the filmstrips

  BlankFrame := '';
  For Idx := 1 To DataHeight * DataWidth Do
     BlankFrame := BlankFrame + #0;

  ClrWhite := TFColorToTFColorA(TfWhite);
  ClrBlack := TFColorToTFColorA(TfBlack);
  ClrDark := DisplayPalette[7];
  ClrBtnFace := TFColorToTFColorA(TfBtnFace);
  ClrShadow := TFColorToTFColorA(TfShadow);
  ClrShadow.r := (ClrShadow.r + ClrBtnFace.r) Shr 1;
  ClrShadow.g := (ClrShadow.g + ClrBtnFace.g) Shr 1;
  ClrShadow.b := (ClrShadow.b + ClrBtnFace.b) Shr 1;

  TempDIB.Clear(TFColorAToTFColor(ClrDark));
  FastDrawEx.FillRect32(TempDIB, TempVal, 0, TempDIB.Width -1, TempDIB.Height -1, ClrBtnFace);
  FastDrawEx.FillRect32(TempDIB, 0, 0, TempVal -1, 5, TFColorToTFColorA(TfBlack));
  FastDrawEx.FillRect32(TempDIB, 0, TempDIB.Height - 6, TempVal -1, TempDIB.Height -1, TFColorToTFColorA(TfBlack));
  FastDrawEx.FillRect32(TempDIB, TempVal, 0, TempDIB.Width -1, 5, ClrShadow);
  FastDrawEx.FillRect32(TempDIB, TempVal, TempDIB.Height - 6, TempDIB.Width -1, TempDIB.Height -1, ClrShadow);

  Idx := 2;
  While Idx < TempDIB.Width Do Begin
     If Idx >= 0 Then Begin
        If (Idx-2) Mod 6 < 4 Then Begin
           If Idx < TempVal Then Begin
              TempDIB.Pixels32[2, Idx] := ClrWhite;
              TempDIB.Pixels32[3, Idx] := ClrWhite;
              TempDIB.Pixels32[TempDIB.AbsHeight - 3, Idx] := ClrWhite;
              TempDIB.Pixels32[TempDIB.AbsHeight - 4, Idx] := ClrWhite;
           End Else Begin
              TempDIB.Pixels32[2, Idx] := ClrBtnFace;
              TempDIB.Pixels32[3, Idx] := ClrBtnFace;
              TempDIB.Pixels32[TempDIB.AbsHeight - 3, Idx] := ClrBtnFace;
              TempDIB.Pixels32[TempDIB.AbsHeight - 4, Idx] := ClrBtnFace;
           End;
        End;
        If (Idx Mod ((DataWidth * 8) + 8) = 0) or (Idx Mod ((DataWidth * 8) + 8) = (DataWidth * 8) + 7) Then Begin
           For Idx2 := 6 To TempDIB.AbsHeight - 6 Do
              If Idx >= TempVal Then
                 TempDIB.Pixels32[Idx2, Idx] := ClrShadow
              Else
                 TempDIB.Pixels32[Idx2, Idx] := ClrBlack;
        End;
     End;
     Inc(Idx);
  End;

  If AnimList.Count > 0 Then
     For Idx := 0 To AnimList.Count -1 Do Begin

        Time := GetWord(@AnimList[Idx][1]);

        If GetWord(@AnimList[Idx][3]) = 0 Then Begin

           // Empty frame - should be the last one.

           Frame := BlankFrame;

        End Else Begin

           // Get the frame from the list

           Frame := '';
           TempStr := AnimList[Idx];
           For Idx2 := (GetWord(@TempStr[3]) -1) * DataWidth * DataHeight to ((GetWord(@TempStr[3])) * DataWidth * DataHeight) -1 Do
              Frame := Frame + Curchars[Idx2 +1];

        End;

        X := (Idx * ((DataWidth * 8) + 8)) + 4;
        Y := 9 + DataHeight;

        If AnimSelected <> 0 Then
           If AnimSelected -1 = Idx Then
              FastDrawEx.FillRect32(TempDIB, X - 3, Y - DataHeight - 3, X + (DataWidth * 8) + 2, Y + 4, ClrWhite)
           Else
              If AnimSelected < 0 Then
                 If Idx = (-AnimSelected)-1 Then
                    For Idx2 := 6 To TempDIB.AbsHeight - 6 Do Begin
                       TempDIB.Pixels32[Idx2, X -4] := DisplayPalette[9];
                       If X -5 > 0 Then
                          TempDIB.Pixels32[Idx2, X -5] := DIsplayPalette[9];
                    End;

        For Idx2 := 1 To Length(Frame) Do Begin
           ByteVal := Ord(Frame[Idx2]);
           BitValue := 128;
           While BitValue > 0 Do Begin
              If ByteVal And BitValue <> 0 Then Begin
                 TempDIB.Pixels32[Y, X] := ClrBlack;
                 If AnimSelected <> 0 Then
                    If AnimSelected -1 = Idx Then
                       TempDIB.Pixels32[Y, X] := DisplayPalette[9];
              End;
              BitValue := BitValue Shr 1;
              Inc(X);
           End;
           If Idx2 Mod DataWidth = 0 Then Begin
              Dec(Y);
              X := (Idx * ((DataWidth * 8) + 8)) + 4;
           End;
        End;

     End;

  TempDIB.Draw(FastIMG5.Bmp.hDc, -AnimViewOffset, 0);
  TempDIB.Free;

  // Now for the scrollbar if the display size is smaller than the anim size

  AnimScrollWidth := Min(FastIMG5.Width, Round((FastIMG5.Width / TempVal) * FastIMG5.Width));
  AnimScrollPos := Round((FastIMG5.Width / TempVal) * AnimViewOffset);

  FastDrawEx.FillRect32(FastIMG5.Bmp, 0, FastIMG5.Bmp.AbsHeight -9, FastIMG5.Width -1, FastIMG5.Bmp.AbsHeight -4, ClrShadow);
  If TempVal > FastIMG5.Width Then
     FastDrawEx.FillRect32(FastIMG5.Bmp, AnimScrollPos, FastIMG5.Bmp.AbsHeight -9, AnimScrollPos + AnimScrollWidth -1, FastIMG5.Bmp.AbsHeight -4, DisplayPalette[0]);

  FastIMG5.Repaint;

End;

Procedure TUDGWindow.AnimScrollInView(Index: Integer);
Var
  FrameWidth, FrameOffset, StripWidth: Integer;
Begin

  // Sets the AnimViewOffset so that the selected anim frame (Index) is
  // Visible if it's not currently. Note that this does not cause a re-paint
  // of the animation!

  FrameWidth := (DataWidth * 8) + 8;
  FrameOffset := Index * FrameWidth;
  StripWidth := FastIMG5.Width Div FrameWidth;

  If FrameOffset < AnimViewOffset Then
     AnimViewOffset := FrameOffset
  Else
     If FrameOffset > AnimViewOffset + StripWidth Then
        AnimViewOffset := Max(0, FrameOffset - StripWidth - FrameWidth);

End;

procedure TUDGWindow.FastIMG5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  If (Y >= 39) And (Y <= 44) Then Begin

     // Clicked the scrollbar region.

     If X < AnimScrollPos Then Begin

        // Clicked to the left - set AnimViewOffset to a point one page below

        AnimViewOffset := Max(0, AnimViewOffset - FastIMG5.Width);
        RenderCurrentAnim(False);

     End Else
        If X > AnimScrollPos + AnimScrollWidth Then Begin

           // Clicked to the right

           AnimViewOffset := Min(AnimViewOffset + FastIMG5.Width, (AnimList.Count * ((DataWidth * 8) + 8)) - FastIMG5.Width);
           RenderCurrentAnim(False);

        End Else Begin

           // Clicked the scrollbar - do nothing, but store the coords for a dragging op later

           AnimMouseDown := True;

        End;

  End;

  AnimMouseX := X;
  AnimMouseY := Y;

end;

procedure TUDGWindow.FastIMG5MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  Dx: Integer;
begin

  If AnimMouseDown Then Begin

     Dx := X - AnimMouseX;
     AnimViewOffset := Round(AnimViewOffset + (Dx * (AnimList.Count * (((DataWidth * 8) + 8))) / FastIMG5.Width));
     AnimViewOffset := Min(Max(AnimViewOffset, 0), (AnimList.Count * ((DataWidth * 8) + 8)) - FastIMG5.Width);
     AnimMouseX := X;
     RenderCurrentAnim(False);

  End;

end;

procedure TUDGWindow.FormKeyPress(Sender: TObject; var Key: AnsiChar);
begin

  If Lowercase(Key) = 'a' Then Begin
     AnimList[AnimList.Count -1] := #0#0+AnsiChar(CurChar And 255)+AnsiChar(CurChar Shr 8);
     AnimList.Add(#0#0#0#0);
     AnimScrollInView(AnimList.Count -1);
     AnimSelected := 0;
     RenderCurrentAnim(False);
  End Else
     If Lowercase(Key) = 'o' Then Begin
        OnionChars := AssignedChars;
        GetBIGChar(True);
     End;

end;

procedure TUDGWindow.FastIMG5MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  Idx: Integer;
begin

  If AnimMouseDown Then
     AnimMouseDown := False
  Else Begin
     If (Y > 0) And (Y < 28) Then Begin
        Idx := (X + AnimViewOffset) Div ((DataWidth * 8) + 8);
        If (Abs(X - AnimMouseX) < 4) and (Abs(Y - AnimMouseY) < 4) Then Begin
           If Idx < AnimList.Count -1 Then Begin
              AnimList.Delete(Idx);
              AnimScrollInView(Idx);
              RenderCurrentAnim(False);
           End;
        End;

     End;
  End;

end;

procedure TUDGWindow.SpeedButton21Click(Sender: TObject);
begin

  // Launch the animation preview window

  CentreFormOnForm(AnimPreviewWindow, Self);
  ShowWindow(AnimPreviewWindow, True);

end;

procedure TUDGWindow.FastIMG6MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  Idx: Integer;
begin

  If Button = mbRight Then Begin
     For Idx := 1 To Length(OnionChars) Do
        OnionChars[Idx] := #0;
     GetBIGChar(True);
  End Else
     If Button = mbLeft Then Begin
        OnionDown := True;
        OnionShowing := True;
        GetBIGChar(True);
     End;

end;

procedure TUDGWindow.FastIMG6MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  OnionDown := False;
  OnionShowing := False;
  GetBIGChar(True);

end;

procedure TUDGWindow.ExportCharacter1Click(Sender: TObject);
Var
  Binary: AnsiString;
  F: Integer;
begin

  // Send the current character as a binary to the Binary Files import tool

  Binary := '';
  For F := CurChar To CurChar + (DataWidth * DataHeight) -1 Do
     Binary := Binary + CurChars[F];

  BinaryWindow.AddBinaryEx('GraphicData'+IntToStr(CurObj), Binary, btMemory, Address);
  CentreFormOnForm(BinaryWindow, Self);
  BinaryWindow.ShowModal;
  Inc(CurObj);
  MouseDown := False;

end;

procedure TUDGWindow.SetupCount1Click(Sender: TObject);
begin
GridSetUpWindow.EditType := 3;
  GridSetUpWindow.NumChars := NumChars;
  GridSetUpWindow.Address := Address;

  CentreFormOnForm(GridSetupWindow, Self);
  ShowWindow(GridSetupWindow, True);
  If Not GridSetUpWindow.Cancelled Then Begin

        Address := GridSetUpWindow.Address;
        NumChars := GridSetUpWindow.NumChars;
        While (Address -1 + (NumChars*8) > 65535) or (NumChars > 96) Do Dec(NumChars);
        GrabFromMemory;

     FormResize(nil);
     RepaintChars;
  End;
  MouseDown := False;
end;

procedure TUDGWindow.Save1Click(Sender: TObject);
begin
     SaveAs(UdgFileName);
end;

End.


