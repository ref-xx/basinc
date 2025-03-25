unit TokenWindow; // Quite possibly the neatest utility I've ever written :-)  [dunny]  //but it's hard to fix things here [arda]

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, FastSize, FastIMG, FastDIB, FastDraw, StdCtrls, Math, ExtCtrls;

type
  TBoxPos = Record Pt: TPoint; Width: Integer; End;

  TTokenForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    FastIMG1: TFastIMG;
    FastIMG2: TFastIMG;
    FastIMG3: TFastIMG;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    FastIMG4: TFastIMG;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Button1: TButton;
    Label4: TLabel;
    FastIMG6: TFastIMG;
    Label5: TLabel;
    FastIMG5: TFastIMG;
    Label6: TLabel;
    FastIMG7: TFastIMG;
    Label7: TLabel;
    FastIMG8: TFastIMG;
    Label8: TStaticText;
    CheckBox1: TCheckBox;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG4MouseMove(Sender: TObject; Shift:  TShiftState;  X, Y:  Integer);
    procedure FastIMG1MouseMove(Sender: TObject; Shift:  TShiftState;  X, Y:  Integer);
    procedure FastIMG2MouseMove(Sender: TObject; Shift:  TShiftState;  X, Y:  Integer);
    procedure FastIMG3MouseMove(Sender: TObject; Shift:  TShiftState;  X, Y:  Integer);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FastIMG8MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG8Exit(Sender: TObject);
    procedure FastIMG8DblClick(Sender: TObject);
    procedure FastIMG6MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG6Exit(Sender: TObject);
    procedure FastIMG6DblClick(Sender: TObject);
    procedure FastIMG7MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG7Exit(Sender: TObject);
    procedure FastIMG7DblClick(Sender: TObject);
    procedure FastIMG5MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG5DblClick(Sender: TObject);
    procedure FastIMG5Exit(Sender: TObject);
    procedure FastIMG1Exit(Sender: TObject);
    procedure FastIMG2Exit(Sender: TObject);
    procedure FastIMG3Exit(Sender: TObject);
    procedure UpdateLabel8(Token: Byte);
    procedure PageControl1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);

  private
    { Private declarations }
  public
    { Public declarations }
    IgnoreNextClick: Boolean;
    MouseDownTimer: DWord;
    KeywordsPos: Array[0..95] of TBoxPos;
    AttrsPos: Array[0..5] of TBoxPos;
    ColoursPos: Array[0..15] of TBoxPos;
    EDITPos: Array[0..7] of TBoxPos;
    Procedure DrawTokens;
    Procedure DrawAlphas;
    Procedure DrawGFXChars;
    Procedure DrawUDGs;
    Procedure DrawEDITChars;
    Procedure DrawATTRChars;
    Procedure DrawColourChars;
    Procedure DrawKeywords;
    Procedure BuildBigChar(Sender: TObject; Character: Byte; X, Y: Integer);
    Function  TranslateToTokens(Character: Byte): String;
  end;

var
  TokenForm: TTokenForm;

Const

  AttributeTokens: Array[0..9] of String =
     ('Flash Off', 'Flash On', 'Bright Off', 'Bright On', 'True Video', 'Inv Video','Flash 8','Bright 8','Inverse 8','Over 8');

  ColourTokens: Array[0..7] of String =
     ('Black', 'Blue', 'Red', 'Magenta', 'Green', 'Cyan', 'Yellow', 'White');

  EditingTokens: Array[0..7] of String =
     ('CAPS', 'EDIT', #166+' Left', #165+' Right', #168+' Down', #167+' Up', 'DELETE', 'ENTER');


implementation

{$R *.DFM}

Uses BASinMain, FastCore, Utility, InputUtils, ROMUtils;

Procedure TTokenForm.DrawTokens;
Begin
  DrawAlphas;
  DrawGFXChars;
  DrawUDGs;
  DrawEDITChars;
  DrawATTRChars;
  DrawColourChars;
  DrawKeywords;
End;

Procedure TTokenForm.DrawAlphas;
Var
  X, Y, Y1, F: Integer;
Begin

  // Alpha-Numerics

  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 32);
  FastIMG1.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG1.Bmp, [Point(0, 0), Point(FastIMG1.Width -1, 0), Point(FastIMG1.Width-1, FastIMG1.Height -1), Point(0, FastIMG1.Height -1)], TfBlack);
  X := 2; Y := 3;
  For F := 32 To 127 Do Begin
     FastDraw.Line(FastIMG1.Bmp, X-2, Y-3, X+14, Y-3, TFSpecBlack);
     FastDraw.Line(FastIMG1.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
     If FastIMG1.Tag = F-32 Then Begin
        For Y1 := Y-1 To Y+10 Do
           FastDraw.Line(FastIMG1.Bmp, X, Y1, X+11, Y1, TFSpecBlack);
        SpecTextToDIB(FastIMG1.Bmp, X+2, Y+2, Chr(F), 7, 0, 1, False, CheckBox1.Checked);
     End Else Begin
        SpecTextToDIB(FastIMG1.Bmp, X+2, Y+2, Chr(F), 0, 7, 0, False, CheckBox1.Checked);
     End;
     Inc(X, 15);
     If X > FastIMG1.Bmp.Width -1 Then Begin
        X := 2;
        Inc(Y, 15);
     End;
  End;
  FastIMG1.Repaint;

End;

Procedure TTokenForm.DrawGFXChars;
Var
  X, Y, Y1, F: Integer;
Begin

  // Preset Gfx chars

  FastIMG2.Bmp.SetSize(FastIMG2.Width, FastIMG2.Height, 32);
  FastIMG2.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG2.Bmp, [Point(0, 0), Point(FastIMG2.Width -1, 0), Point(FastIMG2.Width-1, FastIMG2.Height -1), Point(0, FastIMG2.Height -1)], TfBlack);
  X := 2; Y := 3;
  For F := 128 To 143 Do Begin
     FastDraw.Line(FastIMG2.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
     If FastIMG2.Tag = F-128 Then Begin
        For Y1 := Y-1 To Y+10 Do
           FastDraw.Line(FastIMG2.Bmp, X, Y1, X+11, Y1, TFSpecBlack);
        SpecTextToDIB(FastIMG2.Bmp, X+2, Y+2, Chr(F), 7, 0, 1, False, False);
     End Else Begin
        SpecTextToDIB(FastIMG2.Bmp, X+2, Y+2, Chr(F), 0, 7, 0, False, False);
     End;
     Inc(X, 15);
  End;
  FastIMG2.Repaint;

End;

Procedure TTokenForm.DrawUDGs;
Var
  X, Y, Y1, F, UDGLimit: Integer;
Begin

  // User Defined Graphics (UDGs)

  If ProgramIs128k Then Begin
     FastIMG3.Width := (15 * 19) +1;
     UDGLimit := 162;
  End Else Begin
     FastIMG3.Width := (15 * 21) +1;
     UDGLimit := 164;
  End;

  FastIMG3.Bmp.SetSize(FastIMG3.Width, FastIMG3.Height, 32);
  FastIMG3.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG3.Bmp, [Point(0, 0), Point(FastIMG3.Width -1, 0), Point(FastIMG3.Width-1, FastIMG3.Height -1), Point(0, FastIMG3.Height -1)], TfBlack);
  X := 2; Y := 3;
  For F := 144 To UDGLimit Do Begin
     FastDraw.Line(FastIMG3.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
     If FastIMG3.Tag = F-144 Then Begin
        For Y1 := Y-1 To Y+10 Do
           FastDraw.Line(FastIMG3.Bmp, X, Y1, X+11, Y1, TFSpecBlack);
        SpecTextToDIB(FastIMG3.Bmp, X+2, Y+2, Chr(F), 7, 0, 1, False, False);
     End Else Begin
        SpecTextToDIB(FastIMG3.Bmp, X+2, Y+2, Chr(F), 0, 7, 0, False, False);
     End;
     Inc(X, 15);
  End;
  FastIMG3.Repaint;

End;

Procedure TTokenForm.DrawEDITChars;
Var
  X, Y, Y1, F: Integer;
Begin

  // Editing Controls

  FastIMG5.Bmp.SetSize(FastIMG5.Width, FastIMG5.Height, 32);
  FastIMG5.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG5.Bmp, [Point(0, 0), Point(FastIMG5.Width -1, 0), Point(FastIMG5.Width-1, FastIMG5.Height -1), Point(0, FastIMG5.Height -1)], TfBlack);
  X := 2; Y := 3;
  For F := 0 to 7 Do Begin
     FastDraw.Line(FastIMG5.Bmp, X-2, Y-3, X+72, Y-3, TFSpecBlack);
     FastDraw.Line(FastIMG5.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
     If FastIMG5.Tag = F Then Begin
        For Y1 := Y-1 To Y+10 Do
           FastDraw.Line(FastIMG5.Bmp, X, Y1, X+70, Y1, TFSpecBlack);
        SpecTextToDIB(FastIMG5.Bmp, X+2, Y+2, EditingTokens[F], 7, 0, 1, False, False);
     End Else Begin
        SpecTextToDIB(FastIMG5.Bmp, X+2, Y+2, EditingTokens[F], 0, 7, 0, False, False);
     End;
     EDITPos[F].Pt.X := X-1;
     EDITPos[F].Pt.Y := Y-2;
     EDITPos[F].Width := 70;
     Inc(X, 74);
     If X > FastIMG5.Bmp.Width -1 Then Begin
        X := 2;
        Inc(Y, 15);
     End;
  End;
  If FastIMG5.Tag = -2 Then FastIMG5.Tag := -1;
  FastIMG5.Repaint;

End;

Procedure TTokenForm.DrawATTRChars;
Var
  BW,X, Y, Y1, F: Integer;

Begin

  // Attribute controls
  BW:=86;
  FastIMG6.Bmp.SetSize(FastIMG6.Width, FastIMG6.Height, 32);
  FastIMG6.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG6.Bmp, [Point(0, 0), Point(FastIMG6.Width -1, 0), Point(FastIMG6.Width-1, FastIMG6.Height -1), Point(0, FastIMG6.Height -1)], TfBlack);
  X := 2; Y := 3;
  For F := 0 to 5 Do Begin
     FastDraw.Line(FastIMG6.Bmp, X-2, Y-3, X+BW, Y-3, TFSpecBlack);
     FastDraw.Line(FastIMG6.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
     If FastIMG6.Tag = F Then Begin
        For Y1 := Y-1 To Y+10 Do
           FastDraw.Line(FastIMG6.Bmp, X, Y1, X+BW-2, Y1, TFSpecBlack);
        SpecTextToDIB(FastIMG6.Bmp, X+2, Y+2, AttributeTokens[F], 7, 0, 1, False, False);
     End Else Begin
        SpecTextToDIB(FastIMG6.Bmp, X+2, Y+2, AttributeTokens[F], 0, 7, 0, False, False);
     End;
     AttrsPos[F].Pt.X := X-1;
     AttrsPos[F].Pt.Y := Y-2;
     AttrsPos[F].Width := BW-2;
     Inc(X, BW+2);
     If (X+BW) > FastIMG6.Bmp.Width -1 Then Begin
        X := 2;
        Inc(Y, 15);
     End;
  End;
  If FastIMG6.Tag = -2 Then FastIMG6.Tag := -1;
  FastIMG6.Repaint;

End;

Procedure TTokenForm.DrawColourChars;
Var
  X, X1, Y, Y1, F, L: Integer;
Begin

  // Colour Codes

  FastIMG7.Bmp.SetSize(FastIMG7.Width, FastIMG7.Height, 32);
  FastIMG7.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG7.Bmp, [Point(0, 0), Point(FastIMG7.Width -1, 0), Point(FastIMG7.Width-1, FastIMG7.Height -1), Point(0, FastIMG7.Height -1)], TfBlack);
  X := 2; Y := 3;
  For F := 0 to 7 Do Begin
     L := (Length(ColourTokens[F])*8)+8;
     For X1 := X-2 To X+L-3 Do
        For Y1 := Y-3 To Y+11 Do
           FastIMG7.Bmp.Pixels32[FastIMG7.Bmp.Height - 1 - Y1, X1] := DisplayPalette[F+8];
     FastDraw.Line(FastIMG7.Bmp, X-2, Y-3, X+L-2, Y-3, TFSpecBlack);
     FastDraw.Line(FastIMG7.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
     If FastIMG7.Tag = F Then Begin
        For Y1 := Y-1 To Y+10 Do
           FastDraw.Line(FastIMG7.Bmp, X, Y1, X+L-4, Y1, TFSpecBlack);
        SpecTextToDIB(FastIMG7.Bmp, X+2, Y+2, ColourTokens[F], F, 16, 1, False, False);
     End Else
        SpecTextToDIB(FastIMG7.Bmp, X+2, Y+2, ColourTokens[F], 16, F, 1, False, False);
     ColoursPos[F].Pt.X := X-1;
     ColoursPos[F].Pt.Y := Y-2;
     ColoursPos[F].Width := L;
     Inc(X, L);
  End;
  X := 2; Inc(Y, 15);
  For F := 0 to 7 Do Begin
     L := (Length(ColourTokens[F])*8)+8;
     FastDraw.Line(FastIMG7.Bmp, X-2, Y-3, X+L-2, Y-3, TFSpecBlack);
     FastDraw.Line(FastIMG7.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
     If FastIMG7.Tag = F+8 Then Begin
        For X1 := X-1 To X+L-3 Do
           For Y1 := Y-2 To Y+11 Do
              FastIMG7.Bmp.Pixels32[FastIMG7.Bmp.Height - 1 - Y1, X1] := DisplayPalette[F];
        SpecTextToDIB(FastIMG7.Bmp, X+2, Y+2, ColourTokens[F], 16, F, 1, False, False);
     End Else
        SpecTextToDIB(FastIMG7.Bmp, X+2, Y+2, ColourTokens[F], F+8, 7, 0, False, False);
     ColoursPos[F+8].Pt.X := X-1;
     ColoursPos[F+8].Pt.Y := Y-2;
     ColoursPos[F+8].Width := L;
     Inc(X, L);
  End;

  If FastIMG7.Tag = -2 Then FastIMG7.Tag := -1;
  FastIMG7.Repaint;

End;

Procedure TTokenForm.DrawKeywords;
Var
  X, Y, Y1, F, L, G, P, TestX, LenMod, LMod, H: Integer;
Begin

  // Keywords - a bit more complex.

  FastIMG8.Bmp.SetSize(FastIMG8.Width, FastIMG8.Height, 32);
  FastIMG8.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG8.Bmp, [Point(0, 0), Point(FastIMG8.Width -1, 0), Point(FastIMG8.Width-1, FastIMG8.Height -1), Point(0, FastIMG8.Height -1)], TfBlack);
  X := 2; Y := 3;
  If ProgramIs128k Then Begin
     F := 0;
     P := 8;
  End Else Begin
     F := 2;
     P := 10;
  End;
  While F < 93 Do Begin
     G := 0; TestX := 2;
     While (TestX < FastIMG8.BMP.Width) and (f+G < 94) Do Begin
        Inc(TestX, (Length(AsciiKeywords[F+G])*8)+P);
        Inc(G);
     End;
     Dec(G);
     Dec(TestX,(Length(AsciiKeywords[F+G])*8)+P);
     LenMod := (FastIMG8.Bmp.Width - TestX) Div G;
     For H := 0 To G-1 Do Begin
        If H = G-1 Then
           L := FastIMG8.Bmp.Width - X
        Else
           L := (Length(AsciiKeywords[F])*8)+P+LenMod;
        LMod := (L-((Length(AsciiKeywords[F])*8)+P)) Div 2;
        FastDraw.Line(FastIMG8.Bmp, X-2, Y-3, X+L-2, Y-3, TFSpecBlack);
        FastDraw.Line(FastIMG8.Bmp, X-2, Y-3, X-2, Y+11, TFSpecBlack);
        If FastIMG8.Tag = F Then Begin
           For Y1 := Y-1 To Y+10 Do
              FastDraw.Line(FastIMG8.Bmp, X, Y1, X+L-4, Y1, TFSpecBlack);
           SpecTextToDIB(FastIMG8.Bmp, 3+X+LMod, Y+2, AsciiKeywords[F], 7, 0, 1, False, False);
        End Else Begin
           SpecTextToDIB(FastIMG8.Bmp, 3+X+LMod, Y+2, AsciiKeywords[F], 0, 7, 0, False, False);
        End;
        KeywordsPos[F].Pt.X := X-1;
        KeywordsPos[F].Pt.Y := Y-2;
        KeywordsPos[F].Width := L-2;
        Inc(X, L);
        Inc(F);
     End;
     X := 2;
     Inc(Y, 15);
  End;
  If FastIMG8.Tag = -2 Then FastIMG8.Tag := -1;
  FastIMG8.Repaint;

End;

procedure TTokenForm.FormShow(Sender: TObject);
begin
  FastIMG8.Tag := -2; // -2 means no position info yet stored.
  FastIMG7.Tag := -2;
  FastIMG6.Tag := -2;
  FastIMG5.Tag := -2;
  FastIMG3.Tag := -2;
  FastIMG2.Tag := -2;
  FastIMG1.Tag := -2;
  PageControl1Change(Nil);
  IgnoreNextClick := False;
end;

procedure TTokenForm.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  cx, cy: Integer;
  ch: Byte;
begin
  If IgnoreNextClick Then Begin
     IgnoreNextClick := False;
     Exit;
  End;
  cx := X Div 15;
  cy := Y Div 15;
  If cx > 23 Then cx := 23;
  ch := cx+(cy*24)+32;
  If ch in [32..127] Then Begin
     BuildBigChar(FastIMG1, ch, FastIMG1.Left+(cx*15)+7, FastIMG1.Top+(cy*15)+7);
     MouseDownTimer := GetTickCount;
  End;
end;

procedure TTokenForm.FastIMG2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  ch: Byte;
begin
  If IgnoreNextClick Then Begin
     IgnoreNextClick := False;
     Exit;
  End;
  ch := Byte(X Div 15)+128;
  If ch in [128..143] Then Begin
     BuildBigChar(FastIMG2, ch, FastIMG2.Left+((X Div 15)*15)+7, FastIMG2.Top+7);
     MouseDownTimer := GetTickCount;
  End;
end;

procedure TTokenForm.FastIMG3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  ch: Byte;
begin
  If IgnoreNextClick Then Begin
     IgnoreNextClick := False;
     Exit;
  End;
  ch := Byte(X Div 15)+144;
  If ch in [144..164] Then Begin
     BuildBigChar(FastIMG3, ch, FastIMG3.Left+((X Div 15)*15)+7, FastIMG3.Top+7);
     MouseDownTimer := GetTickCount;
  End;
end;

Procedure TTokenForm.BuildBigChar(Sender: TObject; Character: Byte; X, Y: Integer);
Var
  TempDIB: TFastDIB;
  AX, AY, XX, YY: DWord;
Begin

  If FastIMG4.Visible And (Character = FastIMG4.Tag) Then Exit;

  FastIMG4.Visible := False;
  FastIMG4.Bmp.SetSize(32, 32, 32);
  FastIMG4.Bmp.Clear(TFSpecWhite);
  FastDraw.PolyLine(FastIMG4.Bmp, [Point(0, 0), Point(FastIMG4.Width -1, 0), Point(FastIMG4.Width-1, FastIMG4.Height -1), Point(0, FastIMG4.Height -1)], TfBlack);
  FastIMG4.Left := Min(Max(X - 16, 0), (Sender As TFastIMG).Left + (Sender As TFastIMG).Width - 16);
  FastIMG4.Top := Min(Max(Y - 16, 0), (Sender As TFastIMG).Top + (Sender As TFastIMG).Height);

  TempDIB := TFastDIB.Create;
  TempDIB.SetSize(9, 9, 32);
  If Character in [32..127] then
     SpecTextToDIB(TempDIB, 0, 2, Chr(Character), 0, 7, 0, False, CheckBox1.Checked)
  Else
     SpecTextToDIB(TempDIB, 0, 2, Chr(Character), 0, 7, 0, False, False);

  For AX := 0 to 7 Do
     For AY := 0 To 7 Do
        If TempDIB.Pixels32[AY, AX].r = 0 Then Begin
           XX := AX*3;
           YY := AY*3;
           Inc(XX, 4);
           Inc(YY, 4);
           FastIMG4.Bmp.Pixels32[YY, XX] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY, XX+1] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY, XX+2] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY+1, XX] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY+1, XX+1] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY+1, XX+2] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY+2, XX] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY+2, XX+1] := DisplayPalette[0];
           FastIMG4.Bmp.Pixels32[YY+2, XX+2] := DisplayPalette[0];
        End;

  TempDIB.Free;

  If Character in [128..143] Then
     FastDraw.PolyLine(FastIMG4.Bmp, [Point(4, 4), Point(FastIMG4.Width -5, 4), Point(FastIMG4.Width-5, FastIMG4.Height -5), Point(4, FastIMG4.Height -5)], TfBlack);

  FastIMG4.Tag := Character;
  FastIMG4.Visible := True;
  FastIMG4.BringToFront;

  UpdateLabel8(Character);

End;

procedure TTokenForm.FastIMG4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  CurTimer: DWord;
begin
  If Button <> mbLeft Then Exit;
  CurTimer := GetTickCount - MouseDownTimer;
  If GetDoubleClickTime >= CurTimer Then Begin
     // Send the char
     IgnoreNextClick := True;
     If (FastIMG4.Tag <> 0) and (X > 8) and (X < 24) and (Y > 8) and (Y < 24) then Begin
        If BASinOutput.Running and Registers.EmuRunning Then
           BufferToken(FastIMG4.Tag)
        Else Begin
           BASinOutput.PerformTokenIn(Chr(FastIMG4.Tag));
           BASinOutput.RepaintBASIC(True);
           BASinOutput.SetFocus;
        End;
        FastIMG4.Tag := 0;
     End;
  End;
  FastIMG4.Visible := False;
end;

procedure TTokenForm.FastIMG4MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  If ssLeft in Shift Then Begin
     // convert x and y to the tab sheet's client area
     Inc(X, FastIMG4.Left);
     Inc(Y, FastIMG4.Top);
     // Which FastIMG are we in now?
     If PtInRect(Rect(FastIMG1.Left, FastIMG1.Top, FastIMG1.Left+FastIMG1.Width-1, FastIMG1.Top+FastIMG1.Height-1), Point(X, Y)) Then Begin
        FastIMG1MouseDown(Self, mbLeft, [], X-FastIMG1.Left, Y-FastIMG1.Top);
     End Else If PtInRect(Rect(FastIMG2.Left, FastIMG2.Top, FastIMG2.Left+FastIMG2.Width-1, FastIMG2.Top+FastIMG2.Height-1), Point(X, Y)) Then Begin
        FastIMG2MouseDown(Self, mbLeft, [], X-FastIMG2.Left, Y-FastIMG2.Top);
     End Else If PtInRect(Rect(FastIMG3.Left, FastIMG3.Top, FastIMG3.Left+FastIMG3.Width-1, FastIMG3.Top+FastIMG3.Height-1), Point(X, Y)) Then Begin
        FastIMG3MouseDown(Self, mbLeft, [], X-FastIMG3.Left, Y-FastIMG3.Top);
     End;
  End;
end;

procedure TTokenForm.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  If FastIMG4.Visible and (FastIMG4.Tag in [32..127]) and (ssLeft in Shift) Then
     FastIMG1MouseDown(Self, mbLeft, [], X, Y);
  FastIMG1.Tag := (X Div 15)+(24 * (Y Div 15));
  UpdateLabel8(FastIMG1.Tag+32);
  DrawAlphas;
end;

procedure TTokenForm.FastIMG2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  If FastIMG4.Visible and (FastIMG4.Tag in [128..143]) and (ssLeft in Shift) Then
     FastIMG2MouseDown(Self, mbLeft, [], X, Y);
  FastIMG2.Tag := (X Div 15);
  UpdateLabel8(FastIMG2.Tag+128);
  DrawGFXChars;
end;

procedure TTokenForm.FastIMG3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  If FastIMG4.Visible and (FastIMG4.Tag in [144..164]) and (ssLeft in Shift) Then
     FastIMG3MouseDown(Self, mbLeft, [], X, Y);
  FastIMG3.Tag := (X Div 15);
  UpdateLabel8(FastIMG3.Tag+144);
  DrawUDGs;
end;

procedure TTokenForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  PageControl1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - Button1.Height -20);
  Button1.SetBounds(ClientWidth - Button1.Width - 8, ClientHeight - Button1.Height - 8, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - Button2.WIdth - 4, Button1.Top, Button2.Width, Button1.Height);
end;

procedure TTokenForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TTokenForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If FastIMG4.Visible Then FastIMG4.Visible := False;
end;

procedure TTokenForm.FormActivate(Sender: TObject);
begin
  If FastIMG4.Visible Then FastIMG4.Visible := False;
end;

procedure TTokenForm.FastIMG8MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  F: DWord;
begin
  If FastIMG8.Tag > -2 Then
     For F := 0 To 92 Do
        If (X >= KeywordsPos[F].Pt.X) Then
           If (Y >= KeywordsPos[F].Pt.Y) Then
              If (Y < KeywordsPos[F].Pt.Y + 12) Then
                 If (X < KeywordsPos[F].Pt.X + KeywordsPos[F].Width) Then Begin
                    FastIMG8.Tag := F;
                    DrawKeywords;
                    UpdateLabel8(F+163);
                    Exit;
                 End;
end;

procedure TTokenForm.FastIMG8Exit(Sender: TObject);
begin
  Label8.Caption := '';
  FastIMG8.Tag := -1;
  DrawTokens;
end;

procedure TTokenForm.FastIMG8DblClick(Sender: TObject);
begin
  If FastIMG8.Tag > -1 Then
     If Not (BASinOutput.Running or Registers.EmuRunning) Then Begin


        BASinOutput.PerformTokenIn(TranslateToTokens(FastIMG8.Tag + 163));  // arda - changed 164 -> 163

        BASinOutput.RepaintBASIC(True);
        BASinOutput.SetFocus;
     End Else
        BufferToken(FastIMG8.Tag+163);
end;

procedure TTokenForm.FastIMG6MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  F: DWord;
begin
  If FastIMG6.Tag > -2 Then
     For F := 0 To 5 Do
        If (X >= AttrsPos[F].Pt.X) Then
           If (Y >= AttrsPos[F].Pt.Y) Then
              If (Y < AttrsPos[F].Pt.Y + 12) Then
                 If (X < AttrsPos[F].Pt.X + AttrsPos[F].Width) Then Begin
                    FastIMG6.Tag := F;
                    DrawAttrChars;
                    UpdateLabel8(F);
                    Exit;
                 End;
end;

procedure TTokenForm.FastIMG6Exit(Sender: TObject);
begin
  Label8.Caption := '';
  FastIMG6.Tag:= -1;
  DrawAttrChars;
end;

procedure TTokenForm.FastIMG6DblClick(Sender: TObject);
begin
  If FastIMG6.Tag > -1 Then
     If Not (BASinOutput.Running or Registers.EmuRunning) Then Begin
        BASinOutput.PerformTokenIn(TranslateToTokens(FastIMG6.Tag));
        BASinOutput.RepaintBASIC(True);
        BASinOutput.SetFocus;
     End Else
        BufferToken(FastIMG6.Tag);
end;

procedure TTokenForm.FastIMG7MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  F: DWord;
begin
  If FastIMG7.Tag > -2 Then
     For F := 0 To 15 Do
        If (X >= ColoursPos[F].Pt.X) Then
           If (Y >= ColoursPos[F].Pt.Y) Then
              If (Y < ColoursPos[F].Pt.Y + 12) Then
                 If (X < ColoursPos[F].Pt.X + ColoursPos[F].Width) Then Begin
                    FastIMG7.Tag := F;
                    DrawColourChars;
                    UpdateLabel8(F+16);
                 End;
end;

procedure TTokenForm.FastIMG7Exit(Sender: TObject);
begin
  FastIMG7.Tag:= -1;
  DrawColourChars;
end;

procedure TTokenForm.FastIMG7DblClick(Sender: TObject);
begin
  If FastIMG7.Tag > -1 Then
     If Not (BASinOutput.Running or Registers.EmuRunning) Then Begin
        BASinOutput.PerformTokenIn(TranslateToTokens(FastIMG7.Tag + 16));
        BASinOutput.RepaintBASIC(True);
        BASinOutput.SetFocus;
     End Else
        BufferToken(FastIMG7.Tag+16);
end;

procedure TTokenForm.FastIMG5MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  F: DWord;

begin
  If FastIMG5.Tag > -2 Then
     For F := 0 To 7 Do
        If (X >= EDITPos[F].Pt.X) Then
           If (Y >= EDITPos[F].Pt.Y) Then
              If (Y < EDITPos[F].Pt.Y + 12) Then
                 If (X < EDITPos[F].Pt.X + EDITPos[F].Width) Then Begin

                    FastIMG5.Tag := F;
                    DrawEDITChars;
                    UpdateLabel8(F+6);
                 End;
end;

procedure TTokenForm.FastIMG5Exit(Sender: TObject);
begin
  Label8.Caption := '';
  FastIMG5.Tag:= -1;
  DrawEDITChars;
end;

procedure TTokenForm.FastIMG5DblClick(Sender: TObject);
begin
  If FastIMG5.Tag > -1 Then
     If Not (BASinOutput.Running or Registers.EmuRunning) Then Begin
        BASinOutput.PerformTokenIn(TranslateToTokens(FastIMG5.Tag + 6));
        BASinOutput.RepaintBASIC(True);
        BASinOutput.SetFocus;
     End Else
        BufferToken(FastIMG5.Tag+6);
end;

procedure TTokenForm.FastIMG1Exit(Sender: TObject);
begin
  Label8.Caption := '';
  FastIMG1.Tag := -1;
  DrawAlphas;
end;

procedure TTokenForm.FastIMG2Exit(Sender: TObject);
begin
  Label8.Caption := '';
  FastIMG2.Tag := -1;
  DrawGFXChars;
end;

procedure TTokenForm.FastIMG3Exit(Sender: TObject);
begin
  Label8.Caption := '';
  FastIMG3.Tag := -1;
  DrawUDGs;
end;

Procedure TTokenForm.UpdateLabel8(Token: Byte);
Var
  NewCaption: String;
Begin

  NewCaption := '';

  If ProgramIs128k Then Begin
     Case Token of
        0..5:       NewCaption := AttributeTokens[Token];
        6..13:      NewCaption := EditingTokens[Token -6];
        16..23:     NewCaption := ColourTokens[Token -16] + ' Paper';
        24..31:     NewCaption := ColourTokens[Token -24] + ' Ink';
        32..127:    NewCaption := Chr(Token);
        128..143:   NewCaption := 'Chequerboard Char';
        144..162:   NewCaption := 'User Defined Graphic '#39+Chr(Token-79)+#39;
        163..255:   NewCaption := 'Keyword '+AsciiKeywords[Token-163];
     End;
  End Else Begin
     Case Token of
        0..5:       NewCaption := AttributeTokens[Token];
        6..13:      NewCaption := EditingTokens[Token -6];
        16..23:     NewCaption := ColourTokens[Token -16] + ' Paper';
        24..31:     NewCaption := ColourTokens[Token -24] + ' Ink';
        32..127:    NewCaption := Chr(Token);
        128..143:   NewCaption := 'Chequerboard Char';
        144..164:   NewCaption := 'User Defined Graphic '#39+Chr(Token-79)+#39;
        165..255:   NewCaption := 'Keyword '+AsciiKeywords[Token-163];
     End;
  End;

  If NewCaption = ' ' Then NewCaption := '<Space>';
  If NewCaption = '`' Then NewCaption := '£';
  If NewCaption = #38 Then NewCaption := '&&';
  If NewCaption = #127 Then NewCaption := '<Copyright>';

  If Copy(NewCaption, 1, 1) > Chr(164) Then
     NewCaption := 'Cursor'+Copy(NewCaption, 2, 9999);

  NewCaption := NewCaption + ' = CHR$('+IntToStr(Token)+')';

  If Label8.Caption <> NewCaption Then
     Label8.Caption := NewCaption;
End;

procedure TTokenForm.PageControl1Change(Sender: TObject);
begin
  DrawTokens;
end;

procedure TTokenForm.CheckBox1Click(Sender: TObject);
Var
  Character: Byte;
begin
  DrawAlphas;
  If FastIMG4.Visible and (FastIMG4.Tag in [32..127]) then Begin
     Character := FastIMG4.Tag;
     FastIMG4.Tag := 0;
     BuildBigChar(FastIMG1, character, FastIMG4.Left+16, FastIMG4.Top+16);
  End;
end;

Function TTokenForm.TranslateToTokens(Character: Byte): String;
Begin

  If ProgramIs128k Then Begin
     Case Character of
        0..1:     Result := #18 + Chr(Character);
        2..3:     Result := #19 + Chr(Character -2);
        4..5:     Result := #20 + Chr(Character -4);
        6..15:    Result := Chr(Character);
        16..23:   Result := #17 + Chr(Character -16);
        24..31:   Result := #16 + Chr(Character -24);
        32..162:  Result := Chr(Character);
        163..255: If BASinOutput.CursStringStart = 0 Then
                     Result := AsciiKeywords[Character -165]
                  Else
                     Result := Chr(Character);
     End;
  End Else Begin
     Case Character of
        0..1:     Result := #18 + Chr(Character);
        2..3:     Result := #19 + Chr(Character -2);
        4..5:     Result := #20 + Chr(Character -4);
        6..15:    Result := Chr(Character);
        16..23:   Result := #17 + Chr(Character -16);
        24..31:   Result := #16 + Chr(Character -24);
        32..164:  Result := Chr(Character);
        165..255: If BASinOutput.CursStringStart = 0 Then
                     Result := AsciiKeywords[Character -163]
                  Else
                     Result := Chr(Character);
     End;
  End;
  if GetASyncKeyState(VK_SHIFT) and 1 = 1 Then Result := Chr(Character);


End;

procedure TTokenForm.Button2Click(Sender: TObject);
begin
  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_token_table.html'), HH_DISPLAY_TOPIC, 0);
end;



procedure TTokenForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
        If (Key = 84) and (Shift = [ssCtrl]) then begin
             Close;
        End;
end;

procedure TTokenForm.Button1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
        If (Key = 84) and (Shift = [ssCtrl]) then begin
             Close;
        End;
end;

procedure TTokenForm.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
 if (GetKeyState(VK_Control) < 0) and (Msg.CharCode = Ord('T')) Then Close;
end;

end.
