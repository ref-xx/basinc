unit BASICEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FastIMG, ExtCtrls, FastDIB, Utility, Math, FastDraw, ClipBrd,
  Menus, ComCtrls, z80Assembler, ROMUtils, FindWindow, Tabs;

type

  TAsmFile = Record
    CursorChar: Char;
    CursorPoint: TPoint;
    AsmFilename, BASICMem: String;
    Changed, ValidFile, CodeError, CursorInString,
    CursorVisible, CursorState: Boolean;
    ScrollPosV, ScrollPosH: Integer;
    ViewOffset, ViewLine, ViewColumn,
    CursPrevLineStart, CursLineStart,
    CursOffset, CursorColumn, CursorLine,
    CursPageEnd, EditorSelAnchor, EditorSelLength: Integer;
    Labels: TStringlist;
  End;

  PAsmFile = ^TAsmFile;

  TBASICEditorWindow = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Timer1: TTimer;
    FastIMG1: TFastIMG;
    StatusBar2: TStatusBar;
    New1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N2: TMenuItem;
    Assemble1: TMenuItem;
    N3: TMenuItem;
    Quit1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N4: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    Replace1: TMenuItem;
    View1: TMenuItem;
    StatusBar1: TMenuItem;
    Labellist1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    ToMemory1: TMenuItem;
    AsDATAStatements1: TMenuItem;
    ToBinaryFile1: TMenuItem;
    ToTapeblocks1: TMenuItem;
    TabSet1: TTabSet;
    Close1: TMenuItem;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    Edit2: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure PerformKeyDown(Key: Word; Shift: TShiftState; Repaint: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBox1VScroll(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType);
    procedure ScrollBox1HScroll(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType);
    Procedure MenuItemClick(Sender: TObject);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure Splitter1Moved(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure OnEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
  private
    { Private declarations }
    procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;
  public
    { Public declarations }
    ScrollBox1: TNewScrollBox;
    ViewOffset,
    ViewLine,
    ViewColumn,
    CursPrevLineStart,
    CursLineStart,
    CursOffset: Integer;
    BASICMem: String;
    CodeError,
    CursorInString,
    CursorVisible,
    CursorState: Boolean;
    CursorPoint: TPoint;
    CursorChar: Char;
    CursorColumn,
    CursorLine,
    CursPageEnd: Integer;
    Changed, ValidFile, NonActivePaint: Boolean;
    AsmFilename, CurAsmName, CurAsmFilename, OldAsm: String;
    EditorSelAnchor, EditorSelLength: Integer;
    LabelList: String;
    Procedure SetScrollBoxBounds;
    Procedure RepaintBASIC(DoPaint: Boolean);
    Procedure DrawChar(DIB: TFastDIB; CharPtr: PByte; X, Y: Integer; Ink, Paper, Bright: Byte);
    Function  ValidLabel(newLabel: String): Boolean;
    Procedure BuildLabelList;
    Procedure RepaintCursor;
    Procedure MakeCursorVisible;
    Procedure NewFile(Filename: String);
    Function  CheckforSave(Index: Integer): Boolean;
    Function  SaveAsm(index: Integer; SaveAs: Boolean): Boolean;
    Procedure NewFromText(Filename, Assembly: String);
    Procedure SaveAs;
    Procedure CloseFile;
    Procedure ClipCut;
    Procedure ClipCopy;
    Procedure ClipPaste;
    Function  OpenAsmFile(var Filename: String): String;
    Procedure CursorToLine(LineNum: Integer; Filename: String);
    Procedure Open(Filename: String);
    Procedure Assemble;
    Procedure AssembleAsDATA;
    Procedure AssembleBinary;
    Procedure AssembleToTape;
    Function  AsStrings(Assembly: String): TStringlist;
    Function  GetCharPos(X, Y: Integer): DWord;
    Procedure UpdateCursorPos(Offset: Integer; Shifted: Boolean);
    Function  SelStart: DWord;
    Function  SelEnd: DWord;
    Procedure AtStartofPass(Sender: TObject);
    Procedure AtEndofPass(Sender: TObject);
    Function  FindNextForward(Term: String; Pos: Integer; MatchCase: Boolean): TFindResult;
    Function  FindNextBackward(Term: String; Pos: Integer; MatchCase: Boolean): TFindResult;
    Function  ClearFiles: Boolean;
    Procedure WorkSpaceToFile(Index: Integer);
    Procedure FileToWorkSpace(Index: Integer);
    Procedure UpdateCaption;
    Function  HasContent(Index: Integer): Boolean;
    Procedure AddSourceFiles(var Z80Assembler: TZ80Assembler; Exclude: Integer);
  end;

var
  BASICEditorWindow: TBASICEditorWindow;
  AsmMemory: Array[0..65535] of Byte;
  DATALines: TStringlist;
  ExpectedAddr: Word;
  NumPasses, NumBytes: Integer;
  AsmFiles: Array of PAsmFile;
  FileCount: Integer;
  AllLabels: TStringlist;

  procedure AfterLineProc(sender: TObject; Addr: Word; var Len: Word; page: TZ80MemBlock; srcFile, srcLine: Integer);

Const

  TFSpecDark:  TFColor  = (b:187; g:189; r:187);
  TFSpecDarkA: TFColorA = (b:187; g:189; r:187);

implementation

{$R *.DFM}

Uses FastCore, InputUtils, Filing, Sound, AsmForm, AddCode, Tapes,
  ReplaceWindow, CPUDisplay;

procedure TBASICEditorWindow.CMDialogKey(var msg: TCMDialogKey);
begin
  if msg.Charcode <> VK_TAB then inherited;
end;

Procedure TBASICEditorWindow.RepaintBASIC(DoPaint: Boolean);
Var
  Ink, Paper, Bright, pInk, pPaper, pBright, Inverse: Byte;
  CurChar: Char;
  TempStr, tLabels, NewLabel, LineStr: String;
  XPos, YPos, TempVal, ViewX: Integer;
  CurPos, NumLines, TempPrevLine, TempPrevLine2,
  CurLine, LineLen, MaxLineLen, Offset, ESelStart, ESelEnd: DWord;
  InString, NewLine: Boolean;
Begin

  MaxLineLen := 0;
  NumLines := 0;
  LineLen := 0;
  CurPos := 1;

  XPos := - ViewColumn * 8;
  YPos := - ViewLine * 8;

  CursorVisible := False;
  InString := False;
  NewLine := True;
  TempPrevLine2 := 1;
  TempPrevLine := 1;

  pInk := 0;
  pPaper := 7;
  pBright := 0;
  Inverse := 0;

  CursorLine := 0;
  CursorColumn := 1;

  ESelStart := SelStart;
  ESelEnd := SelEnd;

  ViewOffset := -1;
  ViewX := -ViewColumn * 8;

  DisplayPalette[16] := TFSpecDarkA;

  If DoPaint Then Begin
     FastIMG1.Bmp.SetSize(((FastIMG1.Width+8) Div 8)*8, ((FastIMG1.Height+16) Div 8)*8, 32);
     FastIMG1.Bmp.Clear(TFSpecWhite);
  End;

  tLabels := '';

  While CurPos <= Length(BASICMem) Do Begin

     If (YPos >= 0) and (ViewOffset = -1) Then ViewOffset := CurPos;
     If YPos < FastIMG1.Bmp.AbsHeight Then CursPageEnd := TempPrevLine;

     // Draw the characters one by one - using an optimised version
     // of SpecTextToDIB(), from Utility.pas.

     CurChar := BASICMem[CurPos];
     If NewLine Then Begin
        XPos := ViewX;
        NewLine := False;
        If CurPos <= CursOffset Then Begin
           CursorColumn := 1;
           Inc(CursorLine);
        End;
     End;

     // Test for and store the params of the cursor, if we're at the right position.

     If CurPos = CursOffset Then Begin
        CursorVisible := (YPos >= 0) and (YPos < FastIMG1.Bmp.AbsHeight - 8) and (XPos >= 0) and (XPos < FastIMG1.Bmp.Width -8);
        CursorChar := BASICMem[CurPos];
        CursorPoint := Point(XPos, YPos);
        CursPrevLineStart := TempPrevLine2;
        CursLineStart := TempPrevLine;
        CursorInString := InString;
        If Not NonActivePaint Then Begin
           Bright := 1;
           If CodeError Then Begin
              If CursorState Then Begin
                 Ink := 2;
                 Paper := 7;
              End Else Begin
                 Ink := 7;
                 Paper := 2;
              End;
           End Else Begin
              If CursorState Then Begin
                 Ink := 1;
                 Paper := 7;
              End Else Begin
                 Ink := 7;
                 Paper := 1;
              End;
           End;
        End;
        If CursorChar < #32 Then CursorChar := ' ';
     End Else Begin
        If XPos > ViewX -1 Then Begin
           If Inverse = 0 Then Begin
              Ink := pInk;
              Paper := pPaper;
           End Else Begin
              Ink := pPaper;
              Paper := pInk;
           End;
           Bright := pBright;
        End Else Begin
           Ink := 0;
           Paper := 16;
           Bright := 0;
        End;
        If (CurPos >= ESelStart) and (CurPos < ESelEnd) and (ESelStart <> ESelEnd) Then Begin
           Ink := pPaper;
           Paper := pInk;
        End;
     End;

     Case CurChar of
        #13:
           Begin // Carriage Return
              While BASICMem[CurPos] < #32 Do
                 Inc(CurPos);
              Dec(CurPos);
              If DoPaint Then
                 If (YPos > 0) and (YPos < FastIMG1.Bmp.AbsHeight) Then
                    DrawChar(FastIMG1.Bmp, @RealChars[1], XPos, YPos, Ink, Paper, Bright);
              XPos := ViewX;
              Inc(YPos, 8);
              Inc(NumLines);
              If LineLen > MaxLineLen Then MaxLineLen := LineLen;
              LineLen := 0;
              InString := False;
              NewLine := True;
              If NumLines = ViewLine Then YPos := 0;
              TempPrevLine2 := TempPrevLine;
              TempPrevLine := CurPos+1;
           End;
        #32..#143:
           Begin // Alpha-numeric/symbols/Chequerboard chars
              If CurChar = ':' Then Begin
                 If Not InString Then Begin
                    NewLabel := Copy(BASICMem, TempPrevLine, (CurPos - TempPrevLine) +1);
                    If ValidLabel(NewLabel) Then
                       tLabels := tLabels + NewLabel+','+IntToStr(NumLines)+',';
                 End;
              End;
              If CurChar = '"' Then InString := Not InString;
              If DoPaint Then
                 If (YPos > 0) and (YPos < FastIMG1.Bmp.AbsHeight) Then
                    DrawChar(FastIMG1.Bmp, @RealChars[(Byte(CurChar)-32)*8], XPos, YPos, Ink, Paper, Bright);
              Inc(XPos, 8);
           End;
        #144..#164:
           Begin // UDGs
              If DoPaint Then
                 If (YPos > 0) and (YPos < FastIMG1.Bmp.AbsHeight) Then
                    DrawChar(FastIMG1.Bmp, @Memory[((Byte(CurChar)-144)*8)+GetWord(@Memory[UDG])-1], XPos, YPos, Ink, Paper, Bright);
              Inc(XPos, 8);
           End;
     End;

     Inc(CurPos);
     If CurPos <= CursOffset Then Inc(CursorColumn);
     Inc(LineLen);

  End;

  If DoPaint Then Begin

     If CursorVisible and (CursorChar = #13) and (Screen.ActiveForm = Self) Then RepaintCursor;

     If ScrollBox1 <> nil Then Begin

        ScrollBox1.VertScrollBar.Range := (1+NumLines) * 8;
        ScrollBox1.VertScrollBar.Increment := 8;
        ScrollBox1.VertScrollBar.Visible := True;

        ScrollBox1.HorzScrollBar.Range := (1+MaxLineLen) * 8;
        ScrollBox1.HorzScrollBar.Increment := 8;
        ScrollBox1.HorzScrollBar.Visible := True;

     End;

     If tLabels <> LabelList Then Begin
        LabelList := tLabels;
        BuildLabelList;
     End;

     FastIMG1.BringToFront;
     FastIMG1.Repaint;

  End;

  StatusBar2.Panels[1].Text := ' Ln: '+IntToStr(Max(CursorLine, 1)) + ' Col: ' + IntToStr(CursorColumn);

End;

Function TBASICEditorWindow.ValidLabel(newLabel: String): Boolean;
Var
  Len, Idx: Integer;
Begin

  Result := False;

  Len := Length(NewLabel);
  If Len = 0 Then Exit;
  If newLabel[Len] <> ':' Then Exit;
  If Not (newLabel[1] in ['A'..'Z', 'a'..'z', '_', '.']) Then Exit;
  Idx := 1;
  While Idx < Length(newLabel) Do Begin
     If Not (newLabel[Idx] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) Then
        Exit;
     Inc(Idx);
  End;
  Result := True;

End;

Procedure TBASICEditorWindow.RepaintCursor;
Var
  Ink, Paper: Byte;
Begin
  If CursorVisible and (Screen.ActiveForm = Self) Then Begin
     If CodeError Then Begin
        If CursorState Then Begin
           Ink := 2;
           Paper := 7;
        End Else Begin
           Ink := 7;
           Paper := 2;
        End;
     End Else Begin
        If CursorState Then Begin
           Ink := 1;
           Paper := 7;
        End Else Begin
           Ink := 7;
           Paper := 1;
        End;
     End;
     Case CursorChar of
        #32..#143:
           DrawChar(FastIMG1.Bmp, @RealChars[(Byte(CursorChar)-32)*8], CursorPoint.X, CursorPoint.Y, Ink, Paper, 1);
        #144..#164:
           DrawChar(FastIMG1.Bmp, @Memory[((Byte(CursorChar)-144)*8)+GetWord(@Memory[UDG])-1], CursorPoint.X, CursorPoint.Y, Ink, Paper, 1);
     End;
     FastIMG1.Repaint;
  End;
End;

Procedure TBASICEditorWindow.DrawChar(DIB: TFastDIB; CharPtr: PByte; X, Y: Integer; Ink, Paper, Bright: Byte);
Var
  Bit: Byte;
  Line: DWord;
  PixelPtr, InkPtr, PaperPtr: PfColorA;

Begin

  If (X < 0) or (X > DIB.Width -8) or (Y < 0) or (Y > DIB.AbsHeight -8) Then Exit;

  Y := (DIB.AbsHeight -1) - Y;
  PixelPtr := Pointer(DIB.Bits);
  Inc(PixelPtr, (Y * DIB.Width) + X);

  If Bright = 1 Then Begin
     Inc(Ink, 8);
     Inc(Paper, 8);
  End;

  InkPtr := @DisplayPalette[Ink];
  PaperPtr := @DisplayPalette[Paper];

  Inc(CharPtr);

  For Line := 0 To 7 Do Begin

     Bit := 128;
     While Bit > 0 Do Begin

        If Byte(CharPtr^) And Bit = 0 Then
           PixelPtr^ := PaperPtr^
        Else
           PixelPtr^ := InkPtr^;

        Bit := Bit Shr 1;
        Inc(PixelPtr);

     End;

     Dec(PixelPtr, DIB.Width+8);
     Inc(CharPtr);

  End;

End;

procedure TBASICEditorWindow.FormShow(Sender: TObject);
begin
  If Scrollbox1 = nil Then Begin
     ScrollBox1 := TNewScrollBox.Create(Self);
     SetScrollBoxBounds;
     ScrollBox1.Parent := Self;
     ScrollBox1.Anchors := [akTop, akLeft, akbottom, akRight];
     ScrollBox1.AutoScroll := False;
     ScrollBox1.OnVerticalScroll := ScrollBox1VScroll;
     ScrollBox1.OnHorizontalScroll := ScrollBox1HScroll;
  End;
  ListBox1.Color := TfColorToTColor(TFSpecDark);
  RepaintBASIC(True);
  FormResize(Self);
  If Visible Then Edit2.SetFocus;
end;

Procedure TBASICEditorWindow.SetScrollBoxBounds;
Var
  X, Y, W, H: Integer;
Begin

  If Opt_AsmLabelList then Begin
     X := ListBox1.Width + Splitter1.Width;
     W := ClientWidth - ListBox1.Width - Splitter1.Width;
  End Else Begin
     X := 0;
     W := ClientWidth;
  End;

  If Opt_AsmStatusBar Then Begin
     Y := 0;
     H := ClientHeight - StatusBar2.Height - TabSet1.Height;
  End Else Begin
     Y := 0;
     H := ClientHeight - TabSet1.Height;
  End;

  ScrollBox1.SetBounds(X, Y, W, H);

End;

procedure TBASICEditorWindow.FormResize(Sender: TObject);
begin
  If ScrollBox1 <> nil Then Begin
     FastIMG1.SetBounds(ScrollBox1.Left+2, ScrollBox1.Top+2, ScrollBox1.ClientWidth, ScrollBox1.ClientHeight);
     StatusBar2.Panels[0].Width := StatusBar2.ClientWidth - Canvas.TextWidth('Ln: 000 Col: 000');
     RepaintBASIC(True);
  End;
end;

procedure TBASICEditorWindow.ScrollBox1VScroll(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType);
begin
  If Visible Then Edit2.SetFocus;
  If EventType = vsThumbTrack Then
     ViewLine := ScrollBox1.ScrollInfo.nTrackPos Div 8
  Else
     ViewLine := ScrollBox1.ScrollInfo.nPos Div 8;
  RepaintBASIC(True);
end;

procedure TBASICEditorWindow.ScrollBox1HScroll(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType);
begin
  If Visible Then Edit2.SetFocus;
  If EventType = hsThumbTrack Then
     ViewColumn := ScrollBox1.ScrollInfo.nTrackPos Div 8
  Else
     ViewColumn := ScrollBox1.ScrollInfo.nPos Div 8;
  RepaintBASIC(True);
end;

procedure TBASICEditorWindow.Timer1Timer(Sender: TObject);
begin
  CursorState := Not CursorState;
  If Screen.ActiveForm = Self Then Begin
     RepaintCursor;
     NonActivePaint := False;
  End Else
     If Not NonActivePaint Then Begin
        NonActivePaint := True;
        RepaintBASIC(True);
        FastIMG1.Repaint;
     End;
end;

procedure TBASICEditorWindow.FormCreate(Sender: TObject);
begin

  FileCount := 0;
  ScrollBox1 := nil;
  NewFile('');

end;

procedure TBASICEditorWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  PerformKeyDown(Key, Shift, True);
End;

Procedure TBASICEditorWindow.PerformKeyDown(Key: Word; Shift: TShiftState; Repaint: Boolean);
Var
  InString, IsTab: Boolean;
  CurPos, XStart, Idx, Count, NumSpaces: Integer;
  InsertChar: Char;
  KeyString: String;
begin
  If Not Edit2.Focused Then Exit;
  CodeError := False;
  Case Key of
     VK_LEFT:
        Begin
           If CursOffset > 1 Then Begin
              UpdateCursorPos(CursOffset -1, ssShift in Shift);
              MakeSound(1);
           End;
        End;
     VK_RIGHT:
        Begin
           If CursOffset < Length(BASICMem) Then Begin
              UpdateCursorPos(CursOffset +1, ssShift in Shift);
              MakeSound(1);
           End;
        End;
     VK_UP:
        Begin
           If (ViewLine = 0) And (CursorPoint.y = 0) Then Exit;
           XStart := -ViewColumn * 8;
           CurPos := CursPrevLineStart;
           While (XStart < CursorPoint.X) and (CurPos < (CursLineStart -1)) Do Begin
              Inc(CurPos);
              If BASICMem[CurPos] >= ' ' Then
                 Inc(XStart, 8);
           End;
           UpdateCursorPos(CurPos, ssShift in Shift);
           MakeSound(1);
        End;
     VK_DOWN:
        Begin
           InString := False;
           CurPos := CursLineStart;
           If CurPos < Length(BASICMem) Then Repeat
              If BASICMem[CurPos] = '"' Then InString := Not InString;
              Inc(CurPos);
           Until (BASICMem[CurPos] = #13) or (CurPos >= Length(BASICMem));
           MakeSound(1);
           If CurPos >= Length(BASICMem) Then Exit;
           While BASICMem[CurPos] < ' ' Do
              Inc(CurPos);
           XStart := -ViewColumn * 8;
           CursOffset := CurPos;
           InString := False;
           CurPos := CursOffset;
           While (BASICMem[CurPos] <> #13) and (XStart < CursorPoint.X) Do Begin
              Inc(CurPos);
              If BASICMem[CurPos] >= ' ' Then Begin
                 Inc(XStart, 8);
                 If BASICMem[CurPos] = '"' Then InString := Not InString;
              End;
           End;
           UpdateCursorPos(CurPos, ssShift in Shift);
        End;
     VK_HOME:
        Begin
           UpdateCursorPos(CursLineStart, ssShift in Shift);
           MakeSound(1);
        End;
     VK_END:
        Begin
           InString := False;
           CurPos := CursLineStart;
           Repeat
              Inc(CurPos);
              If BASICMem[CurPos] = '"' Then InString := Not InString;
           Until (BASICMem[CurPos] = #13);
           UpdateCursorPos(CurPos, ssShift in Shift);
           MakeSound(1);
        End;
     VK_PRIOR:
        Begin
           ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - (FastIMG1.Height - (16*8));
           ViewLine := ScrollBox1.VertScrollBar.Position Div (8*8);
           UpdateCursorPos(ViewOffset, ssShift in Shift);
           MakeSound(1);
        End;
     VK_NEXT:
        Begin
           ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + (FastIMG1.Height - (16*8));
           ViewLine := ScrollBox1.VertScrollBar.Position Div (8*8);
           UpdateCursorPos(CursPageEnd, ssShift in Shift);
           MakeSound(1);
        End;
     VK_BACK:
        Begin
           MakeSound(1);
           If SelStart <> SelEnd Then Begin
              BASICMem := Copy(BASICMem, 1, SelStart -1) + Copy(BASICMem, SelEnd, 99999);
              Changed := True;
           End Else If CursOffset > 1 Then Begin
              If BASICMem[CursOffset -1] = ' ' Then
                 If Not CursorInString Then Begin
                    // Backspace through tabs - Find the next tab point, and convert to an offset.
                    NumSpaces := CursorColumn -1;
                    If NumSpaces Mod 3 = 0 Then
                       Dec(NumSpaces, 3)
                    Else
                       NumSpaces := ((NumSpaces Div 3)*3);
                    Inc(NumSpaces);
                    NumSpaces := CursorColumn - NumSpaces; // Now holds the offset.
                    Count := CursOffset - NumSpaces;
                    IsTab := True;
                    While Count < CursOffset Do Begin
                       If BASICMem[Count] <> ' ' Then
                          IsTab := False;
                       Inc(Count);
                    End;
                    If IsTab Then Begin
                       BASICMem := Copy(BASICMem, 1, CursOffset - (NumSpaces +1)) + Copy(BASICMem, CursOffset, 99999);
                       Changed := True;
                       UpdateCursorPos(CursOffset - NumSpaces, False);
                       RepaintBASIC(True);
                       MakeCursorVisible;
                       UpdateCaption;
                       Exit;
                    End;
                 End;
              BASICMem := Copy(BASICMem, 1, CursOffset -2)+Copy(BASICMem, CursOffset, 99999);
              UpdateCursorPos(CursOffset -1, false);
              Changed := True;
           End;
        End;
     VK_TAB:
        Begin
           NumSpaces := (4 + (((CursorColumn -1) Div 3) * 3)) - CursorColumn;
           Count := NumSpaces;
           While Count > 0 Do Begin
              BASICMem := Copy(BASICMem, 1, CursOffset -1)+' '+Copy(BASICMem, CursOffset, 99999);
              Changed := True;
              Dec(Count);
           End;
           UpdateCursorPos(CursOffset + NumSpaces, false);
           MakeSound(1);
        End;
     VK_DELETE:
        Begin
           If SelStart <> SelEnd Then Begin
              BASICMem := Copy(BASICMem, 1, SelStart -1) + Copy(BASICMem, SelEnd, 99999);
              Changed := True;
           End Else
              If CursOffset < Length(BASICMem) Then Begin
                 BASICMem := Copy(BASICMem, 1, CursOffset -1)+Copy(BASICMem, CursOffset+1, 99999);
                 Changed := True;
              End;
           MakeSound(1);
        End;
     VK_RETURN:
        Begin
           If CursorColumn <> 1 Then Begin
              BASICMem := Copy(BASICMem, 1, CursOffset -1)+#13+'   '+Copy(BASICMem, CursOffset, 99999);
              UpdateCursorPos(CursOffset +4, false);
           End Else Begin
              BASICMem := Copy(BASICMem, 1, CursOffset -1)+#13+Copy(BASICMem, CursOffset, 99999);
              UpdateCursorPos(CursOffset +1, false);
           End;
           Changed := True;
           MakeSound(2);
        End;
  Else
     Begin
        KeyString := GetCharFromVKey(Key);
        If KeyString <> '' Then Begin
           InsertChar := KeyString[1];
           If InsertChar <> '' Then Begin
              If SelStart <> SelEnd Then Begin
                 BASICMem := Copy(BASICMem, 1, SelStart -1) + Copy(BASICMem, SelEnd, 99999);
              End;
              BASICMem := Copy(BASICMem, 1, CursOffset -1)+InsertChar+Copy(BASICMem, CursOffset, 99999);
              Inc(CursOffset);
              Changed := True;
              If InsertChar = ':' Then Begin
                 If Not CursorInString Then Begin
                    Idx := CursOffset;
                    While BASICMem[Idx] = #13 Do
                       Dec(Idx);
                    While (Idx > 0) and (BASICMem[Idx] <> #13) Do
                       Dec(Idx);
                    Inc(Idx);
                    While BASICMem[Idx] <= ' ' Do Begin
                       BASICMem := Copy(BASICMem, 1, Idx -1)+Copy(BASICMem, Idx+1, 99999);
                       Dec(CursOffset);
                    End;
                 End;
              End;
              UpdateCursorPos(CursOffset, false);
              MakeSound(1);
           End;
        End;
     End;
  End;

  RepaintBASIC(Repaint);
  If Repaint Then MakeCursorVisible;
  If Changed Then UpdateCaption;

end;

Procedure TBASICEditorWindow.MakeCursorVisible;
Var
  OldPoint: TPoint;
Begin
  OldPoint := Point(ViewLine, ViewColumn);
  If CursorPoint.X < 0 Then
     ViewColumn := Max(ViewColumn - ((-CursorPoint.X+64) Div 8), 0);
  If CursorPoint.X > FastIMG1.Bmp.Width -64 Then
     Inc(ViewColumn, (CursorPoint.X - (FastIMG1.Bmp.Width -64)) Div 8);
  If CursorPoint.Y < 0 Then
     ViewLine := Max(ViewLine - ((-CursorPoint.Y) Div 8), 0);
  If CursorPoint.Y > FastIMG1.Bmp.Height -32 Then
     Inc(ViewLine, (CursorPoint.Y - (FastIMG1.Bmp.Height -24)) Div 8);
  If (ViewLine <> OldPoint.X) or (ViewColumn <> OldPoint.Y) Then Begin
     ScrollBox1.VertScrollBar.Position := ViewLine * 8;
     ScrollBox1.HorzScrollBar.Position := ViewColumn * 8;
     RepaintBASIC(True);
  End;
End;

procedure TBASICEditorWindow.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Visible Then Edit2.SetFocus;
  If (Button = mbLeft) and (ViewOffset > 0) Then Begin
     UpdateCursorPos(GetCharPos(X, Y), ssShift in Shift);
     RepaintBASIC(True);
     MakeCursorVisible;
  End;
End;

Function TBASICEditorWindow.GetCharPos(X, Y: Integer): DWord;
Var
  InString: Boolean;
  CurPos, XStart, YPos: Integer;
Begin
  YPos := 0;
  InString := False;
  CurPos := ViewOffset;
  While (CurPos < Length(BASICMem)) and (YPos+8 < Y) Do Begin
     Inc(CurPos);
     If BASICMem[CurPos] = '"' Then InString := Not InString;
     If BASICMem[CurPos] = #13 Then Inc(YPos, 8);
  End;
  Inc(CurPos);
  If CurPos >= Length(BASICMem) Then Begin
     CurPos := Length(BASICMem);
     Result := CurPos;
  End Else Begin
     Result := CurPos;
     XStart := -ViewColumn * 8;
     InString := False;
     CurPos := Result;
     While (BASICMem[CurPos] <> #13) and (XStart < X-8) Do Begin
        Inc(CurPos);
        If BASICMem[CurPos] >= ' ' Then Begin
           Inc(XStart, 8);
           If BASICMem[CurPos] = '"' Then InString := Not InString;
        End;
     End;
     Result := CurPos;
  End;
end;

procedure TBASICEditorWindow.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  If Not Edit2.Focused Then Exit;
  ViewLine := Min((ScrollBox1.VertScrollBar.Range Div 8) - (FastIMG1.Height Div 8), ViewLine +3);
  ScrollBox1.VertScrollBar.Position := ViewLine * 8;
  RepaintBASIC(True);
  RepaintCursor;
end;

procedure TBASICEditorWindow.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  If Not Edit2.Focused Then Exit;
  If ViewLine > 3 Then
     ViewLine := ViewLine -3
  Else
     ViewLine := 0;
  ScrollBox1.VertScrollBar.Position := ViewLine * 8;
  RepaintBASIC(True);
  RepaintCursor;
end;

Procedure TBASICEditorWindow.UpdateCursorPos(Offset: Integer; Shifted: Boolean);
Begin
  CursOffset := Offset;
  If Shifted Then Begin
     If EditorSelLength < 0 Then
        EditorSelLength := -(EditorSelAnchor - Integer(CursOffset))
     Else
        EditorSelLength := Integer(CursOffset) - EditorSelAnchor;
  End Else Begin
     EditorSelAnchor := CursOffset;
     EditorSelLength := 0;
  End;
End;

Function TBASICEditorWindow.SelStart: DWord;
Begin
  If EditorSelLength < 0 Then
     Result := (EditorSelAnchor + EditorSelLength)
  Else
     Result := EditorSelAnchor;
End;

Function TBASICEditorWindow.SelEnd: DWord;
Begin
  If EditorSelLength < 0 Then
     Result := EditorSelAnchor
  Else
     Result := EditorSelAnchor + EditorSelLength;
End;

Procedure TBASICEditorWindow.NewFile(Filename: String);
Var
  Idx: Integer;
Begin

  SetLength(AsmFiles, FileCount +1);
  Inc(FileCount);
  New(AsmFiles[FileCount -1]);

  AsmFiles[FileCount -1]^.BASICMem := '   '#13;
  AsmFiles[FileCount -1]^.CursOffset := 4;
  AsmFiles[FileCount -1]^.CursorState := False;
  AsmFiles[FileCount -1]^.CursorChar := ' ';
  AsmFiles[FileCount -1]^.CodeError := False;
  AsmFiles[FileCount -1]^.Changed := False;
  AsmFiles[FileCount -1]^.ValidFile := False;
  AsmFiles[FileCount -1]^.CursorInString := False;
  AsmFiles[FileCount -1]^.CursorVisible := True;
  AsmFiles[FileCount -1]^.ViewLine := 0;
  AsmFiles[FileCount -1]^.ViewColumn := 0;
  AsmFiles[FileCount -1]^.EditorSelAnchor := 1;
  AsmFiles[FileCount -1]^.EditorSelLength := 0;
  AsmFiles[FileCount -1]^.Labels := TStringlist.Create;

  RepaintBASIC(True);

  If Filename = '' then Begin
     Idx := 1;
     While True Do Begin
        If Lowercase(AsmFiles[Idx-1]^.AsmFilename) = 'untitled'+IntToStr(Idx)+'.asm' Then
           Inc(Idx)
        Else
           Break;
     End;
     AsmFiles[FileCount -1]^.AsmFilename := 'Untitled'+IntToStr(Idx)+'.asm';
  End Else Begin
     AsmFiles[FileCount -1]^.AsmFilename := Filename;
     AsmFiles[FileCount -1]^.ValidFile := True;
  End;

  TabSet1.Tabs.Add(TrimExtension(ExtractFilename(AsmFiles[FileCount -1]^.AsmFilename)));
  TabSet1.TabIndex := Length(AsmFiles) -1;

  Caption := 'z80 Assembler - '+AsmFiles[FileCount -1]^.AsmFilename;
  If AsmFiles[FileCount -1]^.Changed Then Caption := Caption + '*';
  If Visible Then Edit2.SetFocus;

End;

Procedure TBASICEditorWindow.Open(Filename: String);
Var
  NewAsm: String;
Begin

  NewAsm := OpenAsmFile(Filename);

  If NewAsm <> '' Then Begin

     NewFile(Filename);
     BASICMem := NewAsm;
     CursOffset := 4;
     RepaintBASIC(True);

     CurAsmName := TrimExtension(ExtractFilename(Filename));
     CurAsmFilename := Filename;

     Caption := 'z80 Assembler - '+AsmFilename;
     FormResize(nil);
     If Visible Then Edit2.SetFocus;

  End;

End;

Procedure TBASICEditorWindow.NewFromText(Filename, Assembly: String);
Begin
  NewFile(Filename);
  BASICMem := Assembly;
  CursOffset := 4;
  RepaintBASIC(True);

  CurAsmName := TrimExtension(ExtractFilename(Filename));
  CurAsmFilename := Filename;

  Caption := 'z80 Assembler - '+AsmFilename+'*';
  FormResize(nil);
  If Visible Then Edit2.SetFocus;
End;

Function TBASICEditorWindow.OpenAsmFile(var Filename: String): String;
Var
  Assembly: TStringlist;
  Idx: Integer;
Begin

  If Filename = '' Then Begin
     Filename := OpenFile(Handle, 'Load Assembly Source', [FTAssembly], '', False);
     If Filename = '' Then
        Exit;
  End;

  Assembly := TStringlist.Create;
  Assembly.LoadFromFile(Filename);

  Result := Assembly.Text;
  Assembly.Free;

End;

Function TBASICEditorWindow.CheckForSave(Index: Integer): Boolean;
Var
  MsgVal: Integer;
Begin
  Result := True;
  If AsmFiles[Index]^.Changed Then Begin
     MsgVal := MessageBox(Handle,
                          PChar('Save changes to assembly text '#13#39+ TrimExtension(ExtractFilename(AsmFiles[Index]^.AsmFilename)) +#39'?'),
                          PChar('Save changes?'),
                          MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL or MB_SETFOREGROUND or MB_TOPMOST);
     If MsgVal = IDYES Then Begin
        Result := SaveAsm(Index, False);
     End Else If MsgVal = IDCANCEL then Begin
        Result := False;
        Exit;
     End Else Begin
        Result := True;
        Exit;
     End;
  End;
  If Not AsmFiles[Index]^.ValidFile Then
     If HasContent(Index) Then Begin
        MsgVal := MessageBox(Handle,
                             PChar('Your assembly text '#13#39+ TrimExtension(ExtractFilename(AsmFiles[Index]^.AsmFilename)) +#39' has not been saved.'#13'Save now?'),
                             PChar('Save Assembly text'),
                             MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL or MB_SETFOREGROUND or MB_TOPMOST);
        If MsgVal = IDYES Then Begin
           Result := SaveAsm(Index, False);
        End Else If MsgVal = IDCANCEL then Begin
           Result := False
        End Else
           Result := True;
     End;
End;

Function TBASICEditorWindow.SaveAsm(Index: Integer; SaveAs: Boolean): Boolean;
Var
  Assembly: TStringlist;
  Filename: String;
Begin

  Result := True;

  If TabSet1.TabIndex = Index Then
     WorkSpaceToFile(Index);

  If (Not AsmFiles[Index]^.ValidFile) or SaveAs Then Begin
     Filename := OpenFile(Handle, 'Save Assembly Text', [FTAssembly], '', True);
     If Filename = '' Then Begin
        DoError($1A, 'Save cancelled');
        Result := False;
        Exit;
     End;
  End;

  If FileExists(Filename) Then DeleteFile(Filename);
  If FileExists(Filename) Then
     MessageBox(Handle, pChar('Could not overwrite your file '#39+ExtractFilename(Filename)+#39+'.'), pChar('Save Error'), MB_ICONWARNING or MB_OK);

  Assembly := AsStrings(AsmFiles[Index]^.BASICMem);
  Assembly.SaveToFile(Filename);
  Assembly.Free;

  AsmFiles[Index]^.AsmFilename := Filename;
  AsmFiles[Index]^.ValidFile := True;
  AsmFiles[Index]^.Changed := False;
  TabSet1.Tabs[Index] := TrimExtension(ExtractFilename(AsmFiles[FileCount -1]^.AsmFilename));
  Caption := 'z80 Assembler - '+AsmFiles[Index]^.AsmFilename;
  If Visible Then Edit2.SetFocus;
End;

Procedure TBASICEditorWindow.SaveAs;
Begin

  SaveAsm(TabSet1.TabIndex, True);

End;

Procedure TBASICEditorWindow.ClipCut;
Var
  TempStr: String;
Begin

  If SelStart <> SelEnd Then Begin
     TempStr := Copy(BASICMem, SelStart, SelEnd - SelStart);
     BASICMem := Copy(BASICMem, 1, SelStart -1)+Copy(BASICMem, SelEnd, 99999);
     UpdateCursorPos(SelStart, False);
     EditorSelAnchor := CursOffset;
     EditorSelLength := 0;
     ClipBoard.SetTextBuf(pChar(TempStr));
     RepaintBASIC(True);
     MakeSound(1);
     Changed := True;
     UpdateCaption;
  End;

End;

Procedure TBASICEditorWindow.ClipCopy;
Var
  TempStr: String;
Begin

  If SelStart <> SelEnd Then Begin
     TempStr := Copy(BASICMem, SelStart, SelEnd - SelStart);
     ClipBoard.SetTextBuf(pChar(TempStr));
     EditorSelAnchor := CursOffset;
     EditorSelLength := 0;
     RepaintBASIC(True);
     MakeSound(1);
     If Visible Then Edit2.SetFocus;
  End;

End;

Procedure TBASICEditorWindow.ClipPaste;
Begin

  If ClipBoard.HasFormat(CF_TEXT) Then Begin

     If SelStart <> SelEnd Then Begin
        BASICMem := Copy(BASICMem, 1, SelStart -1)+Copy(BASICMem, SelEnd, 99999);
        CursOffset := SelStart;
     End;

     BASICMem := Copy(BASICMem, 1, CursOffset -1) + ClipBoard.AsText + Copy(BASICMem, CursOffset, 9999);
     UpdateCursorPos(CursOffset + Length(ClipBoard.AsText), False);
     RepaintBASIC(True);
     Changed := True;
     UpdateCaption;
     MakeSound(1);
     If Visible Then Edit2.SetFocus;

  End;

End;

Procedure TBASICEditorWindow.MenuItemClick(Sender: TObject);
Begin

  Case (Sender As TMenuItem).Tag of

      1: NewFile('');
      2: Open('');
      3: SaveAsm(TabSet1.TabIndex, False);
      4: SaveAs;
      5: Assemble;
      6: Close;
      7: ClipCut;
      8: ClipCopy;
      9: ClipPaste;

     10:
        Begin
           CentreFormOnForm(FindForm, Self);
           FindForm.FindType := ftASM;
           ShowWindow(FindForm, True);
        End;
     11:
        Begin // Find Next
           FindForm.FindType := ftASM;
           FindForm.DoFind(nil);
        End;
     12:
        Begin
           ReplaceForm.ReplaceType := rtAsm;
           CentreFormOnForm(ReplaceForm, Self);
           ShowWindow(ReplaceForm, True);
        End;
     14: Begin
           Opt_AsmStatusBar := Not StatusBar2.Visible;
           If StatusBar2.Visible Then
              StatusBar2.Visible := False
           Else Begin
              TabSet1.Visible := False;
              StatusBar2.Visible := True;
              TabSet1.Visible := True;
           End;
           Opt_AsmStatusBar := StatusBar2.Visible;
           SetScrollBoxBounds;
           FormResize(Self);
         End;
     16: Begin
           If ListBox1.Visible Then
              ListBox1.Visible := False
           Else
              ListBox1.Visible := True;
           Opt_AsmLabelList := ListBox1.Visible;
           SetScrollBoxBounds;
           FormResize(nil);
         End;

     19: AssembleAsDATA;
     20: AssembleBinary;
     21: AssembleToTape;
     22: CloseFile;

  End;

End;

Procedure TBASICEditorWindow.Assemble;
Var
  Z80Assembler: TZ80Assembler;
  Assembly: TStringlist;
  Line, ErrorType, Idx, Idx2, Idx3, Idx4: Integer;
  Filename, Text: String;
  pAsmSymb: pAsmSymbol;
  OnDisk: Boolean;
  Flags: DWord;
Begin

  WorkSpaceToFile(TabSet1.TabIndex);
  Assembly := AsStrings(BASICMem);
  If Assembly.Count = 0 Then Exit;
  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@Memory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);
  Z80Assembler.Assemble(Assembly, '', -1, '');
  If Z80Assembler.NumErrors > 0 Then Begin
     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, Filename);
     Exit;
  End;

  CPUWindow.ClearLabels(16384, 65535);
  AllLabels.Clear;

  If Z80Assembler.NumAsmSymbols > 0 Then Begin

     // First pass - clear the labels from files that have been assembled this time

     For Idx := 0 To 31 Do Begin
        For Idx2 := 0 To Z80Assembler.AsmSymbols[Idx].Count -1 Do Begin
           idx3 := integer(z80Assembler.AsmSymbols[Idx].Objects[Idx2]);
           pAsmSymb := PAsmSymbol(@z80assembler.AsmSymbolsExt[idx3]);
           flags := pAsmSymb.SrcFileNum;
           Filename := Z80Assembler.SourceFiles[flags];
           If Filename = '' Then Filename := AsmFiles[TabSet1.TabIndex]^.AsmFilename;
           For Idx4 := 0 To Length(AsmFiles)-1 Do
              If Uppercase(AsmFiles[Idx4]^.AsmFilename) = Uppercase(Filename) Then Begin
                 AsmFiles[Idx4]^.Labels.Clear;
                 Break;
              End;
        End;
     End;

     // Second pass - add the labels to each open file's label list.

     Idx4 := 0;
     For Idx := 0 To 31 Do Begin
        For Idx2 := 0 To Z80Assembler.AsmSymbols[Idx].Count -1 Do Begin
           idx3 := integer(z80Assembler.AsmSymbols[Idx].Objects[Idx2]);
           pAsmSymb := PAsmSymbol(@z80assembler.AsmSymbolsExt[idx3]);
           flags := pAsmSymb.SetData;
           If (flags and (SYM_SET or SYM_MACRO)) = 0 Then Begin
              OnDisk := True;
              Filename := Z80Assembler.SourceFiles[pAsmSymb.SrcFileNum];
              If Filename = '' Then Filename := AsmFiles[TabSet1.TabIndex]^.AsmFilename;
              For Idx4 := 0 To Length(AsmFiles)-1 Do
                 If Uppercase(AsmFiles[Idx4]^.AsmFilename) = Uppercase(Filename) Then Begin
                    OnDisk := False;
                    Break;
                 End;
              If OnDisk Then Begin
                 AllLabels.Add(IntToStr(pAsmSymb.StartAddress)+','+z80Assembler.AsmSymbols[Idx][Idx2]);
              End Else Begin
                 AsmFiles[Idx4]^.Labels.Add(IntToStr(pAsmSymb.StartAddress)+','+z80Assembler.AsmSymbols[Idx][Idx2]);
              End;
           End;
        End;
     End;

     // Now build the main list

     For Idx := 0 To Length(AsmFiles) -1 Do Begin
        AllLabels.AddStrings(AsmFiles[Idx]^.Labels);
     End;

  End;

  CPUWindow.AddAssemblerSymbols;
  Z80Assembler.Free;
  Assembly.Free;

  MessageBox(Handle, pChar('Assembly succeeded - built '+IntToStr(NumBytes)+' bytes.'), pChar('Assembly Completed'), MB_OK or MB_ICONINFORMATION);

End;

Procedure TBASICEditorWindow.AssembleBinary;
Var
  Z80Assembler: TZ80Assembler;
  Assembly: TStringlist;
  AsmMemory: Array[0..65535] of Byte;
  Line, ErrorType: Integer;
  Filename, Text: String;
  FStream: TFileStream;
Begin

  WorkSpaceToFile(TabSet1.TabIndex);
  Assembly := AsStrings(BASICMem);
  If Assembly.Count = 0 Then Exit;
  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);
  Z80Assembler.Assemble(Assembly, '', -1, '');
  If Z80Assembler.NumErrors > 0 Then Begin
     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, Filename);
  End Else Begin

     Filename := OpenFile(Handle, 'Save Binary File', [FTAssembly], '', True);
     If Filename = '' Then Exit;

     If FileExists(Filename) Then DeleteFile(Filename);
     If FileExists(Filename) Then
        MessageBox(Handle, pChar('Could not overwrite your file '#39+ExtractFilename(Filename)+#39+'.'), pChar('Save Error'), MB_ICONWARNING or MB_OK);

     FStream := TFilestream.create(Filename, fmCreate or fmShareDenyNone);
     FStream.Write(AsmMemory[Z80Assembler.DefaultPage.AltLo], Z80Assembler.DefaultPage.AltHi - Z80Assembler.DefaultPage.AltLo);
     FStream.Free;

  End;
  Z80Assembler.Free;
  Assembly.Free;

End;

Procedure TBASICEditorWindow.AssembleToTape;
Var
  Z80Assembler: TZ80Assembler;
  Assembly: TStringlist;
  Line, ErrorType: Integer;
  Filename, Text: String;
  AsmMemory: Array[0..65535] Of Byte;
Begin

  WorkSpaceToFile(TabSet1.TabIndex);
  Assembly := AsStrings(BASICMem);
  If Assembly.Count = 0 Then Exit;
  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);
  Z80Assembler.Assemble(Assembly, '', -1, '');
  If Z80Assembler.NumErrors > 0 Then Begin
     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, Filename);
  End Else Begin

     SetLength(FileArray, Z80Assembler.DefaultPage.AltHi - Z80Assembler.DefaultPage.AltLo);
     CopyMemory(@FileArray[0], @AsmMemory[Z80Assembler.DefaultPage.AltLo], Length(FileArray));
     TapeBlockAdd(CODEToTape('Assembly', Z80Assembler.DefaultPage.AltLo));
     TapeWindow.UpdateTapeList;
     ShowWindow(TapeWindow, False);

  End;
  Z80Assembler.Free;
  Assembly.Free;

End;

Procedure TBASICEditorWindow.AssembleAsDATA;
Var
  Z80Assembler: TZ80Assembler;
  Assembly, NewCode: TStringlist;
  Line, ErrorType, CurLineNum, Idx, Idx2: Integer;
  Filename, Text: String;
Label
  GetValues;
Begin

  WorkSpaceToFile(TabSet1.TabIndex);

  Assembly := AsStrings(BASICMem);
  If Assembly.Count = 0 Then Exit;

  NumPasses := 0;
  NumBytes := 0;
  ExpectedAddr := 0;

  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);

  Z80Assembler.Assemble(Assembly, '', -1, '');

  If Z80Assembler.NumErrors > 0 Then Begin

     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, Filename);
     Z80Assembler.Free;
     Assembly.Free;

  End Else Begin

  GetValues:

     CentreFormOnForm(AssembleForm, Self);
     ShowWindow(AssembleForm, True);
     If AssembleForm.Cancelled Then exit;

     If AssembleForm.StartAt + ((NumBytes Div AssembleForm.BytesPerLine) * AssembleForm.Step) > 9999 Then Begin
        MessageBox(Handle, pChar('Your Line numbering parameters will'#13'result in a line number greater than'#13'9999.'#13#13'Please review your settings.'), pChar('Could not generate BASIC'), MB_OK or MB_ICONWARNING);
        Goto GetValues;
     End;

     CurLineNum := AssembleForm.StartAt;
     NewCode := TStringlist.Create;
     If AssembleForm.CheckBox2.Checked Then Begin
        If DATALines.Count > 1 Then
           NewCode.Add(IntToStr(CurLineNum) + ' RESTORE '+IntToStr(CurLineNum + AssembleForm.Step)+': FOR A=1 TO '+IntToStr(DATALines.Count)+': READ B,C: FOR D=B TO B+C-1: READ C: POKE D,C: NEXT D: NEXT A')
        Else
           NewCode.Add(IntToStr(CurLineNum) + ' RESTORE '+IntToStr(CurLineNum + AssembleForm.Step)+': READ A,B: FOR C=A TO A+B-1: READ B: POKE C,B: NEXT C');
        Inc(CurLineNum, AssembleForm.Step);
     End;

     For Idx := 0 To DATALines.Count -1 Do Begin

        Text := IntToStr(CurLineNum) + ' DATA '+Copy(DATALines[Idx], 1, Pos(',', DATALines[Idx]));
        DATALines[Idx] := Copy(DATALines[Idx], Pos(',', DATALines[Idx])+1, 99999);
        Text := Text + IntToStr(Length(DATALines[Idx]))+',';

        NumBytes := 0;
        For Idx2 := 1 To Length(DATALines[Idx]) Do Begin
           Text := Text + IntToStr(Ord(DATALines[Idx][Idx2]))+',';
           Inc(NumBytes);
           If NumBytes > AssembleForm.BytesPerLine Then Begin
              If Text[Length(Text)] = ',' Then
                 Text := Copy(Text, 1, Length(Text) -1);
              NewCode.Add(Text);
              Inc(CurLineNum, AssembleForm.Step);
              If Idx2 <> Length(DATALines[Idx]) Then Begin
                 Text := IntToStr(CurLineNum)+' DATA ';
                 NumBytes := 0;
              End;
           End;
        End;
        If Text <> '' Then Begin
           If Text[Length(Text)] = ',' Then
              Text := Copy(Text, 1, Length(Text) -1);
           NewCode.Add(Text);
           Inc(CurLineNum, AssembleForm.Step);
        End;

     End;

     AddCodeWindow.ClearCode;
     AddCodeWindow.AddCode(NewCode);
     NewCode.Clear;
     NewCode.Free;

     CentreFormOnForm(AddCodeWindow, Self);
     ShowWindow(AddCodeWindow, True);

     Z80Assembler.Free;
     Assembly.Free;

  End;

End;

Procedure TBASICEditorWindow.AddSourceFiles(var Z80Assembler: TZ80Assembler; Exclude: Integer);
Var
  Idx: Integer;
  StrArray: TStringlist;
Begin

  For Idx := 0 To FileCount -1 Do Begin
     If Idx <> Exclude Then Begin
        StrArray := AsStrings(AsmFiles[Idx]^.BASICMem);
        Z80Assembler.AddSource(StrArray, ASmFiles[Idx]^.AsmFilename);
     End;
  End;

End;

Procedure TBASICEditorWindow.AtStartofPass(Sender: TObject);
Begin
  Inc(NumPasses);
  DATAlines.Clear;
  NumBytes := 0;
End;

Procedure TBASICEditorWindow.AtEndOfPass(Sender: TObject);
Begin
  //
End;

procedure AfterLineProc(sender: TObject; Addr: Word; var Len: Word; page: TZ80MemBlock; srcFile, srcLine: Integer);
Var
  AssembledBytes: String;
  Idx: Integer;
Begin
  If Addr <> ExpectedAddr Then Begin
     // Must have been an ORG.
     DATALines.Add(IntToStr(Addr)+',');
  End;
  ExpectedAddr := Addr + Len;
  AssembledBytes := '';
  For Idx := Addr To Addr + Len -1 Do
     AssembledBytes := AssembledBytes + Chr(AsmMemory[Idx]);

  Inc(NumBytes, Len);

  DATALines[DATALines.Count -1] := DATALines[DATALines.Count -1] + AssembledBytes;

End;

Procedure TBASICEditorWindow.CursorToLine(LineNum: Integer; Filename: String);
Var
  Line, Coff, Idx: Integer;
Begin

  If Filename <> '' Then Begin

     For Idx := 0 To FileCount Do
        If Idx < FileCount then
           If Lowercase(AsmFiles[Idx]^.AsmFilename) = Lowercase(Filename) Then Begin
              TabSet1.TabIndex := Idx;
              Break;
           End;

     If Idx = FileCount Then Begin
        Open(Filename);
     End;

  End;

  Line := 1;
  Coff := 1;
  While Coff < Length(BASICMem) Do Begin
     If BASICMem[Coff] = #13 Then Begin
        Inc(Line);
        If Line >= LineNum Then Begin
           CursOffset := Coff+1;
           While BASICMem[CursOffset] <= ' ' Do
              Inc(CursOffset);
           UpdateCursorPos(CursOffset, False);
           RepaintBASIC(False);
           MakeCursorVisible;
           RepaintBASIC(True);
           If Visible Then Edit2.SetFocus;
           Exit;
        End;
     End;
     Inc(Coff);
  End;
End;

Function TBASICEditorWindow.AsStrings(Assembly: String): TStringlist;
Var
  Str, Line: String;
  Idx: Integer;
Begin

  Result := TStringlist.create;
  Str := Assembly;
  Idx := 1;

  While Str <> '' Do Begin
     Line := Copy(Str, 1, Pos(#13, Str) -1);
     Str := Copy(Str, Pos(#13, Str) +1, 9999);
     If Line <> '' Then
        Result.Add(Line);
  End;

End;

Function TBASICEditorWindow.FindNextForward(Term: String; Pos: Integer; MatchCase: Boolean): TFindResult;
Var
  Idx: Integer;
  Done: Boolean;
Begin
  Idx := Pos -1;
  Done := False;
  If Idx < 0 Then Idx := 0;
  If Not MatchCase Then Term := Uppercase(Term);
  While Not Done Do Begin
     Inc(Idx);
     If Idx = Length(BASICMem)+1 Then Break;
     If Not MatchCase Then Begin
        If UpperCase(BASICMem[Idx]) = Term[1] Then
           If UpperCase(Copy(BASICMem, Idx, Length(Term))) = Term Then
              Done := True;
     End Else Begin
        If BASICMem[Idx] = Term[1] Then
           If Copy(BASICMem, Idx, Length(Term)) = Term Then
              Done := True;
     End;
  End;
  If Done Then
     Result.Position := Idx
  Else
     Result.Position := 0;
  If Visible Then Edit2.SetFocus;
End;

Function TBASICEditorWindow.FindNextBackward(Term: String; Pos: Integer; MatchCase: Boolean): TFindResult;
Var
  Idx: Integer;
  Done: Boolean;
Begin
  Idx := Pos +1;
  Done := False;
  If Idx > Length(BASICMem) Then Idx := Length(BASICMem);
  If Not MatchCase Then Term := Uppercase(Term);
  While Not Done Do Begin
     Dec(Idx);
     If Idx = 0 Then Break;
     If Not MatchCase Then Begin
        If UpperCase(BASICMem[Idx]) = Term[1] Then
           If UpperCase(Copy(BASICMem, Idx, Length(Term))) = Term Then
              Done := True;
     End Else Begin
        If BASICMem[Idx] = Term[1] Then
           If Copy(BASICMem, Idx, Length(Term)) = Term Then
              Done := True;
     End;
  End;
  If Done Then
     Result.Position := Idx
  Else
     Result.Position := 0;
  If Visible Then Edit2.SetFocus;
End;

procedure TBASICEditorWindow.TabSet1Change(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin

  If FileCount = 0 Then Exit;
  If TabSet1.TabIndex > -1 Then Begin
     WorkSpaceToFile(TabSet1.TabIndex);
  End;
  FileToWorkSpace(NewTab);
  AllowChange := True;
  FormResize(Nil);
  If Visible Then Edit2.SetFocus;

end;

Procedure TBASICEditorWindow.FileToWorkSpace(Index: Integer);
Var
  AsmFile: PAsmFile;
Begin
  AsmFile := AsmFiles[Index];
  CursorChar := AsmFile^.CursorChar;
  CursorPoint := AsmFile^.CursorPoint;
  AsmFilename := AsmFile^.AsmFilename;
  BASICMem := AsmFile^.BASICMem;
  CodeError := AsmFile^.CodeError;
  CursorInString := AsmFile^.CursorInString;
  CursorVisible := AsmFile^.CursorVisible;
  CursorState := AsmFile^.CursorState;
  ViewOffset := AsmFile^.ViewOffset;
  ValidFile := AsmFile^.ValidFile;
  Changed := AsmFile^.Changed;
  ViewLine := AsmFile^.ViewLine;
  ViewColumn := AsmFile^.ViewColumn;
  CursPrevLineStart := AsmFile^.CursPrevLineStart;
  CursLineStart := AsmFile^.CursLineStart;
  CursOffset := AsmFile^.CursOffset;
  CursorColumn := AsmFile^.CursorColumn;
  CursorLine := AsmFile^.CursorLine;
  CursPageEnd := AsmFile^.CursPageEnd;
  EditorSelAnchor := AsmFile^.EditorSelAnchor;
  EditorSelLength := AsmFile^.EditorSelLength;
  Caption := 'z80 Assembler - '+AsmFile^.AsmFilename;
  If Changed Then Caption := Caption + '*';
  RepaintBASIC(True);
  If ScrollBox1 <> nil Then Begin
     ScrollBox1.VertScrollBar.Position := AsmFile^.ScrollPosV;
     ScrollBox1.HorzScrollBar.Position := AsmFile^.ScrollPosH;
  End;
End;

Procedure TBASICEditorWindow.WorkSpaceToFile(Index: Integer);
Var
  AsmFile: PAsmFile;
Begin
  AsmFile := AsmFiles[Index];
  If ScrollBox1 <> Nil Then Begin
     AsmFile^.ScrollPosV := ScrollBox1.VertScrollBar.Position;
     AsmFile^.ScrollPosH := ScrollBox1.HorzScrollBar.Position;
  End Else Begin
     AsmFile^.ScrollPosV := 0;
     AsmFile^.ScrollPosH := 0;
  End;
  AsmFile^.CursorChar := CursorChar;
  AsmFile^.CursorPoint := CursorPoint;
  AsmFile^.AsmFilename := AsmFilename;
  AsmFile^.BASICMem := BASICMem;
  AsmFile^.Changed := Changed;
  AsmFile^.ValidFile := ValidFile;
  AsmFile^.CodeError := CodeError;
  AsmFile^.CursorInString := CursorInString;
  AsmFile^.CursorVisible := CursorVisible;
  AsmFile^.CursorState := CursorState;
  AsmFile^.ViewOffset := ViewOffset;
  AsmFile^.ViewLine := ViewLine;
  AsmFile^.ViewColumn := ViewColumn;
  AsmFile^.CursPrevLineStart := CursPrevLineStart;
  AsmFile^.CursLineStart := CursLineStart;
  AsmFile^.CursOffset := CursOffset;
  AsmFile^.CursorColumn := CursorColumn;
  AsmFile^.CursorLine := CursorLine;
  AsmFile^.CursPageEnd := CursPageEnd;
  AsmFile^.EditorSelAnchor := EditorSelAnchor;
  AsmFile^.EditorSelLength := EditorSelLength;
End;

Procedure TBASICEditorWindow.CloseFile;
Var
  Idx, Index: Integer;
Begin

  Index := TabSet1.TabIndex;
  WorkSpaceToFile(Index);

  CheckForSave(Index);
  AsmFiles[Index].Labels.Free;
  Dispose(AsmFiles[Index]);

  If Index < FileCount -1 Then Begin
     For Idx := Index To FileCount -2 Do
        AsmFiles[Idx] := AsmFiles[Idx +1];
     SetLength(AsmFiles, FileCount -1);
     Dec(FileCount);
     FileToWorkSpace(Index);
     TabSet1.Tabs.Delete(Index);
  End Else Begin
     Dec(Index);
     SetLength(AsmFiles, FileCount -1);
     Dec(FileCount);
     FileToWorkSpace(Index);
     TabSet1.Tabs.Delete(Index +1);
  End;
  If Visible Then Edit2.SetFocus;

End;

Function TBASICEditorWindow.ClearFiles: Boolean;
Var
  Idx: Integer;
Begin

  Result := False;

  For Idx := 0 To FileCount -1 Do
     If Not CheckForSave(Idx) then
        Exit;

  Result := True;

  For Idx := 0 To FileCount -1 Do Begin
     AsmFiles[Idx].Labels.Free;
     Dispose(AsmFiles[Idx]);
  End;

End;

Procedure TBASICEditorWindow.UpdateCaption;
Begin
  If Changed Then
     If Copy(Caption, Length(Caption), 1) <> '*' Then
        Caption := Caption + '*';
End;

Function TBASICEditorWindow.HasContent(Index: Integer): Boolean;
Var
  Idx: Integer;
Begin

  Result := False;

  If Index = TabSet1.TabIndex Then
     WorkSpaceToFile(Index);

  Idx := 1;
  While Idx < Length(AsmFiles[Index]^.BASICMem) Do Begin
     If AsmFiles[Index]^.BASICMem[Idx] > ' ' Then Begin
        Result := True;
        Exit;
     End;
     Inc(idx);
  End;

End;

procedure TBASICEditorWindow.Splitter1Moved(Sender: TObject);
begin
  SetScrollBoxBounds;
  FormResize(nil);
end;

Procedure TBASICEditorWindow.BuildLabelList;
Var
  nLabel, TempStr: String;
  nLineNum, Ps: Integer;
Begin

  ListBox1.Items.BeginUpdate;
  ListBox1.Tag := ListBox1.ItemIndex;
  ListBox1.Items.Clear;

  TempStr := LabelList;

  While TempStr <> '' Do Begin

     Ps := Pos(',', TempStr);
     nLabel := Copy(TempStr, 1, Ps -2);
     TempStr := Copy(TempStr, Ps +1, 99999);

     Ps := Pos(',', TempStr);
     nLineNum := StrToInt(Copy(TempStr, 1, Ps -1));
     TempStr := Copy(TempStr, Ps +1, 99999);

     ListBox1.Items.Add(nLabel);
     ListBox1.Items.Objects[ListBox1.Items.Count -1] := TObject(nLineNum);

  End;

  ListBox1.ItemIndex := ListBox1.Tag;
  ListBox1.Items.EndUpdate;

End;

procedure TBASICEditorWindow.ListBox1DblClick(Sender: TObject);
begin
  CursorToLine(Integer(ListBox1.Items.Objects[ListBox1.ItemIndex]) +1, '');
end;

procedure TBASICEditorWindow.OnEnterMenuLoop(var Message: TMessage);
Begin

  Cut1.Enabled := SelStart <> SelEnd;
  Copy1.Enabled := Cut1.Enabled;
  Paste1.Enabled := (ClipBoard.HasFormat(CF_TEXT)) and (ClipBoard.AsText <> '');

  Find1.Enabled := HasContent(TabSet1.TabIndex);
  FindNext1.Enabled := Find1.Enabled;
  Replace1.Enabled := Find1.Enabled;

  StatusBar1.Checked := StatusBar2.Visible;
  LabelList1.Checked := ListBox1.Visible;

End;


Initialization

  DataLines := TStringlist.Create;
  AllLabels := TStringlist.Create;

Finalization

  DataLines.Free;
  AllLabels.Free;

end.

// Opcode Helper
// Syntax highlighting

