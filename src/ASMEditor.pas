unit ASMEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FastIMG, ExtCtrls, FastDIB, Utility, Math, FastDraw, ClipBrd,
  Menus, ComCtrls, z80Assembler, ROMUtils, FindWindow, Tabs;

type

  tWordType = (tTLabel, tTEquate, tTMacro, tTStruct, tTDefine);

  TAsmFile = Record
    CursorChar: Char;
    CursorPoint: TPoint;
    AsmStrs: TStringlist;
    AsmFilename: String;
    Changed, ValidFile, CodeError, CursorInString,
    CursorVisible, CursorState: Boolean;
    ScrollPosV, ScrollPosH, ViewLine, ViewColumn,
    CursorColumn, CursorLine, MaxLineLen: Integer;
    EditorSelStart, EditorSelEnd: TPoint;
    Labels, AssembledLabels: TStringlist;
    Types: TStringlist;
  End;

  PAsmFile = ^TAsmFile;

  TAsmLine = Class
     LabelText: pString;
     LineLen: Integer;
  End;

  pAsmLine = ^TAsmLine;

  TASMEditorWindow = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Timer1: TTimer;
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
    Edit2: TEdit;
    Panel1: TPanel;
    FastIMG1: TFastIMG;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    N5: TMenuItem;
    SyntaxHighlightingOptions1: TMenuItem;
    UsePasmo1: TMenuItem;
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
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;
  public
    { Public declarations }
    ScrollBox1: TNewScrollBox;
    NonActivePaint: Boolean;
    LabelList: String;
    CurFile: PAsmFile;
    MouseDown: Boolean;
    LineSelStart, LineSelEnd: Integer;
    LastInstruction: DWord;
    Procedure SetScrollBoxBounds;
    Procedure RepaintASM(DoPaint: Boolean);
    Procedure NewWord(DIB: TFastDIB; CurWord: String; XPos, YPos, Ink, Paper, Bright: Integer; LineBreak, REMLine, InString: Boolean; CurPos: Integer);
    Function  IsDirective(Text: String): Boolean;
    Function  IsRegister(Text: String): Boolean;
    Function  IsOpcode(Text: String): Boolean;
    Function  IsType(Text: String): Boolean;
    Function  IsNumber(Text: String): Boolean;
    Procedure DrawChar(DIB: TFastDIB; CharPtr: PByte; X, Y: Integer; Ink, Paper, Bright: Byte);
    Function  ValidType(Text: String): String;
    Procedure BuildLabelList;
    Procedure RepaintCursor;
    Procedure MakeCursorVisible;
    Procedure NewFile(Filename: String);
    Function  CheckforSave(Index: Integer): Boolean;
    Function  SaveAsm(index: Integer; SaveAs: Boolean): Boolean;
    Procedure NewFromText(Filename: String; var Assembly: TStringlist);
    Procedure SaveAs;
    Procedure CloseFile;
    Procedure ClipCut;
    Procedure ClipCopy;
    Procedure ClipPaste;
    Function  OpenAsmFile(var Filename: String): TStringlist;
    Procedure CursorToLine(LineNum, Column: Integer; Filename: String);
    Procedure Open(Filename: String);
    //Begin 1.81 Arda
    Function  LoadBinToMemory(const FileName: string; var MemArray: array of Byte; OrgAdr: Word): Boolean;
    Procedure SaveToTempFile(Index: Integer; out FileName: string);
    Function  RunPasmo(const SrcFile, OutFile: string; out Output: string): Boolean;
    Function  FileSizeByName(const FileName: string): Integer;
    Procedure SimpleComp;
    Function  GetOrg:Integer;
    Procedure AssembleWithPasmo;
    Procedure Assemble;
    Procedure AssembleWithPasmoAsDATA;
    procedure AssembleBinaryWithPasmo;
    Procedure AssembleAsDATA;
    Procedure AssembleBinary;
    procedure AssembleTapeWithPasmo;
    Procedure AssembleToTape;
    procedure FillDATALinesSimple(const Mem: array of Byte; StartAddr, Size: Integer);
    Procedure ConvertBinToData(const MemPtr: PByte; Size: Integer);
    //End 1.81 Arda
    Function  GetCharPos(X, Y: Integer): TPoint;
    Procedure UpdateCursorPos(var Offset: TPoint; Shifted: Boolean);
    Function  SelStart: TPoint;
    Function  SelEnd: TPoint;
    Procedure AtStartofPass(Sender: TObject);
    Procedure AtEndofPass(Sender: TObject);
    Function  FindNextForward(Term: String; Pos: TPoint; MatchCase: Boolean): TFindResult;
    Function  FindNextBackward(Term: String; Pos: TPoint; MatchCase: Boolean): TFindResult;
    Function  ClearFiles: Boolean;
    Procedure UpdateCaption; //fixed 1.81 Arda
    Function  HasContent(Index: Integer): Boolean;
    Procedure AddSourceFiles(var Z80Assembler: TZ80Assembler; Exclude: Integer);
    Procedure BuildAsmRecords(AsmFile: TAsmFile);
    Procedure ChangeLine(LineNum: Integer);
    Procedure DeleteLine(LineNum: Integer);
    Procedure InsertLine(LineNum: Integer; Text: String);
    Procedure GetMaxLineLen;
    Function  RemoveSelection: Boolean;
    Function  SelectionAsText: String;
  end;

var
  ASMEditorWindow: TASMEditorWindow;
  AsmMemory: Array[0..65535] of Byte;
  DATALines: TStringlist;
  ExpectedAddr: Word;
  NumPasses, NumBytes: Integer;
  AsmFiles: Array of PAsmFile;
  FileCount: Integer;
  AllLabels: TStringlist;
  AsmFindPos, AsmReplacePos: TPoint;

  procedure AfterLineProc(sender: TObject; Addr: Word; var Len: Word; page: TZ80MemBlock; srcFile, srcLine: Integer);
  Function  SortByLineNum(List: TStringlist; Index1, Index2: Integer): Integer;

Const

  TFSpecDark:  TFColor  = (b:187; g:189; r:187);
  TFSpecDarkA: TFColorA = (b:187; g:189; r:187);

  RegisterWords: array [0..44] of TAsmWord = (
     (S: '(BC)'; I: 138), (S: '(C)'; I: 157),
     (S: '(DE)'; I: 139), (S: '(E)'; I: 158),
     (S: '(HL)'; I: 140), (S: '(SP)'; I: 141),
     (S: 'A'; I: 120),    (S: 'A'#39; I: 128),
     (S: 'AF'; I: 112),   (S: 'AF'#39; I: 116),
     (S: 'B'; I: 122),    (S: 'B'#39; I: 130),
     (S: 'BC'; I: 113),   (S: 'BC'#39; I: 117),
     (S: 'C'; I: 123),    (S: 'C'#39; I: 131),
     (S: 'D'; I: 124),    (S: 'D'#39; I: 132),
     (S: 'DE'; I: 114),   (S: 'DE'#39; I: 118),
     (S: 'E'; I: 125),    (S: 'E'#39; I: 133),
     (S: 'F'; I: 121),    (S: 'F'#39; I: 129),
     (S: 'H'; I: 126),    (S: 'H'#39; I: 134),
     (S: 'HL'; I: 115),   (S: 'HL'#39; I: 119),
     (S: 'I'; I: 136),    (S: 'IM'; I: 43),
     (S: 'IX'; I: 108),   (S: 'IXH'; I: 153),
     (S: 'IXL'; I: 154),  (S: 'IY'; I: 109),
     (S: 'IYH'; I: 155),  (S: 'IYL'; I: 156),
     (S: 'L'; I: 127),    (S: 'L'#39; I: 135),
     (S: 'PC'; I: 111),   (S: 'R'; I: 137),
     (S: 'SP'; I: 110),   (S: 'XH'; I: 159),
     (S: 'XL'; I: 160),   (S: 'YH'; I: 161),
     (S: 'YL'; I: 162));

  OpcodesWords: array [0..78] of TAsmWord = (

     (S: 'ADC'; I: 172),  (S: 'ADD'; I: 1),
     (S: 'AND'; I: 3),    (S: 'BIT'; I: 4),
     (S: 'C'; I: 108),
     (S: 'CALL'; I: 6),   (S: 'CCF'; I: 7),
     (S: 'CODE'; I: 8),   (S: 'CP'; I: 9),
     (S: 'CPD'; I: 10),   (S: 'CPDR'; I: 11),
     (S: 'CPI'; I: 12),   (S: 'CPIR'; I: 13),
     (S: 'CPL'; I: 14),   (S: 'DAA'; I: 15),
     (S: 'DEC'; I: 19),   (S: 'DI'; I: 24),
     (S: 'DJNZ'; I: 26),  (S: 'EI'; I: 29),
     (S: 'EX'; I: 35),    (S: 'EXX'; I: 36),
     (S: 'HALT'; I: 41),  (S: 'IM'; I: 43),
     (S: 'IN'; I: 44),    (S: 'INC'; I: 45),
     (S: 'IND'; I: 47),   (S: 'INDR'; I: 48),
     (S: 'INI'; I: 49),   (S: 'INIR'; I: 50),
     (S: 'JP'; I: 51),    (S: 'JR'; I: 52),
     (S: 'LD'; I: 53),    (S: 'LDD'; I: 54),
     (S: 'LDDR'; I: 55),  (S: 'LDI'; I: 56),
     (S: 'LDIR'; I: 57),  (S: 'M'; I: 109),
     (S: 'NEG'; I: 59),
     (S: 'NC'; I: 110),   (S: 'NZ'; I: 111),
     (S: 'NOP'; I: 62),   (S: 'OR'; I: 64),
     (S: 'OTDR'; I: 66),  (S: 'OTIR'; I: 67),
     (S: 'OUT'; I: 68),   (S: 'OUTD'; I: 69),
     (S: 'OUTI'; I: 70),
     (S: 'P'; I: 112),    (S: 'PE'; I: 113),
     (S: 'PO'; I: 114),   (S: 'POP'; I: 72),
     (S: 'PUSH'; I: 73),
     (S: 'RES'; I: 74),   (S: 'RET'; I: 75),
     (S: 'RETI'; I: 76),  (S: 'RETN'; I: 77),
     (S: 'RL'; I: 79),    (S: 'RLA'; I: 80),
     (S: 'RLC'; I: 81),   (S: 'RLCA'; I: 82),
     (S: 'RLD'; I: 83),   (S: 'RR'; I: 85),
     (S: 'RRA'; I: 86),   (S: 'RRC'; I: 87),
     (S: 'RRCA'; I: 88),  (S: 'RRD'; I: 89),
     (S: 'RST'; I: 90),   (S: 'SBC'; I: 91),
     (S: 'SCF'; I: 92),   (S: 'SET'; I: 93),
     (S: 'SHL'; I: 94),   (S: 'SHR'; I: 95),
     (S: 'SLA'; I: 98),   (S: 'SLL'; I: 152),
     (S: 'SRA'; I: 100),  (S: 'SRL'; I: 101),
     (S: 'SUB'; I: 104),  (S: 'XOR'; I: 107),
     (S: 'Z'; I: 115));

implementation

{$R *.DFM}

Uses FastCore, InputUtils, Filing, Sound, AsmForm, AddCode, Tapes,
  ReplaceWindow, CPUDisplay;

procedure TASMEditorWindow.CMDialogKey(var msg: TCMDialogKey);
begin
  if msg.Charcode <> VK_TAB then inherited;
end;

Procedure TASMEditorWindow.RepaintASM(DoPaint: Boolean);
Var
  PageLen, CharOffsetX, NumLines, XPos, YPos, CurPos, LineIdx, sStartL, sStartC, sEndL, sEndC: Integer;
  Ink, Paper, Bright, pInk, pPaper, pBright, CurWordPos, CurWordOrg: Integer;
  InSelection, inComment, inString, LineBreak, AllSpaces: Boolean;
  AsmText, CurWord: String;
  TempPt: TPoint;
  CurChar: Char;
Label
  Painted;
Begin

  If TabSet1.TabIndex = -1 Then Exit;
  If FileCount = 0 Then Exit;

  If CurFile^.AsmStrs <> nil Then
     NumLines := CurFile^.AsmStrs.Count
  Else
     NumLines := 0;

  DisplayPalette[16] := TFSpecDarkA;

  If DoPaint Then Begin
     FastIMG1.Bmp.SetSize(((FastIMG1.Width+8) Div 8)*8, ((FastIMG1.Height+16) Div 8)*8, 32);
     FastIMG1.Bmp.Clear(TFColorAToTFColor(DisplayPalette[Opt_AsmBackground]));
  End;

  If NumLines = 0 Then Begin
     CurFile^.CursorPoint := Point(0, 0);
     CurFile^.CursorVisible := True;
     CurFile^.CursorChar := ' ';
     Goto Painted;
  End;

  PageLen := (FastIMG1.Bmp.AbsHeight +16) Div 8;
  CharOffsetX := CurFile^.ViewColumn;
  CurFile^.CursorVisible := False;
  YPos := 0;

  TempPt := SelStart;
  sStartL := TempPt.Y -1;
  sStartC := TempPt.X;

  TempPt := SelEnd;
  sEndL := TempPt.Y-1;
  sEndC := TempPt.X;
  AllSpaces := False;

  For LineIdx := CurFile^.ViewLine To Min(NumLines -1, CurFile^.ViewLine + PageLen) Do Begin

     XPos := -(CharOffsetX * 8);
     LineBreak := False;
     InString := False;
     InComment := False;
     CurPos := 1;
     AsmText := CurFile^.AsmStrs[LineIdx]+#13;

     CurWord := '';
     CurWordOrg := XPos;
     CurWordPos := 1;

     LineSelStart := 0;
     LineSelEnd := 0;

     If (LineIdx >= sStartL) and (LineIdx <= sEndL) Then Begin

        If LineIdx = sStartL Then
           LineSelStart := sStartC
        Else
           LineSelStart := 1;

        If LineIdx = sEndL Then
           LineSelEnd := sEndC
        Else
           LineSelEnd := Length(AsmText);

     End;

     While CurPos <= Length(AsmText) Do Begin

        CurChar := AsmText[CurPos];

        If (LineIdx = CurFile^.CursorLine -1) and (CurPos = CurFile^.CursorColumn) Then Begin

           CurFile^.CursorVisible := (XPos >= 0) and (XPos < FastIMG1.Bmp.Width -8);
           If CurChar = #13 Then
              CurFile^.CursorChar := ' '
           Else
              CurFile^.CursorChar := CurChar;
           CurFile^.CursorPoint := Point(XPos, YPos);
           CurFile^.CursorInString := InString;

        End;

        Ink := 0;
        Paper := 7;
        Bright := 0;

        Case CurChar of
           #32:
              Begin
                 If DoPaint Then Begin
                    If CurWord <> '' Then Begin
                       If CurWord[Length(CurWord)] <> ' ' Then Begin
                          NewWord(FastIMG1.Bmp, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, InComment, InString, CurWordPos);
                          CurWordOrg := XPos;
                          CurWordPos := CurPos;
                          CurWord := '';
                       End;
                    End;
                    CurWord := CurWord + ' ';
                    AllSpaces := True;
                    Inc(XPos, 8);
                 End;
              End;
           #33..#164:
              Begin // Alpha-numeric/symbols/Chequerboard chars
                 If DoPaint Then Begin

                    If CurChar in ['"', #39] Then Begin

                       If CurWord <> '' Then Begin
                          If Not InComment Then Begin
                             If Not InString Then Begin
                                If Not AllSpaces Then
                                   NewWord(FastIMG1.Bmp, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, InComment, InString, CurWordPos);
                                CurWordOrg := XPos;
                                CurWord := CurChar;
                                AllSpaces := False;
                             End Else Begin
                                If Not AllSpaces Then
                                   NewWord(FastIMG1.Bmp, CurWord + CurChar, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, InComment, InString, CurWordPos);
                                CurWord := '';
                                AllSpaces := False;
                             End;
                          End Else
                             CurWord := CurWord + CurChar;
                       End Else Begin
                          CurWord := CurChar;
                          CurWordOrg := XPos;
                       End;
                       If (CurChar = #39) and Not InComment Then
                          InString := Not InString
                       Else
                          If CurChar <> #39 Then
                             InString := Not InString;
                    End Else Begin

                       If Not InComment and (CurChar = ';') Then Begin

                          If CurWord <> '' Then Begin
                             If Not AllSpaces Then
                                NewWord(FastIMG1.Bmp, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, False, InString, CurWordPos);
                             CurWord := '';
                             AllSpaces := False;
                          End;
                          inComment := True;
                          inString := False;
                          CurWord := ';';
                          CurWordOrg := XPos;

                       End Else Begin

                          If (CurChar in ['0'..'9', 'A'..'Z', 'a'..'z', '$', '%', '_', '.', '&']) or InComment or InString Then Begin
                             If CurWord = '' Then Begin
                                CurWordOrg := XPos;
                                CurWordPos := CurPos;
                             End Else
                                If AllSpaces Then Begin
                                   CurWordOrg := XPos;
                                   AllSpaces := False;
                                   CurWord := '';
                                End Else
                                   If Not (CurChar in ['h', 'H', 'b', 'B', 'o', 'O', 'x', 'X']) and (CurChar in ['A'..'Z', 'a'..'z']) and ((CurWord[1] in ['0'..'9'])) Then Begin
                                      If Not AllSpaces Then
                                         NewWord(FastIMG1.Bmp, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, InComment, InString, CurWordPos);
                                      CurWordOrg := XPos;
                                      AllSpaces := False;
                                      CurWord := '';
                                   End;
                             CurWord := CurWord + CurChar;
                          End Else Begin
                             If CurWord <> '' Then Begin
                                If Not AllSpaces Then
                                   NewWord(FastIMG1.Bmp, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, InComment, InString, CurWordPos);
                                AllSpaces := False;
                             End;
                             If Not (CurChar in ['"', #39]) Then Begin
                                If Not AllSpaces Then
                                   NewWord(FastIMG1.Bmp, CurChar, XPos, YPos, Ink, Paper, Bright, LineBreak, InComment, InString, CurPos);
                                AllSpaces := False;
                             End;
                             CurWord := '';
                          End;

                       End;
                    End;
                 End;

                 Inc(XPos, 8);

              End;

        End;

        Inc(CurPos);

     End;

     If CurWord <> '' Then
        If Not AllSpaces Then
           NewWord(FastIMG1.Bmp, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, InComment, InString, CurWordPos);
     Inc(YPos, 8);

  End;

Painted:

  If DoPaint Then Begin

     If CurFile^.CursorVisible and (Screen.ActiveForm = Self) Then RepaintCursor;

     If ScrollBox1 <> nil Then Begin

        If NumLines >= (FastIMG1.Bmp.AbsHeight Div 8) -1 Then
           ScrollBox1.VertScrollBar.Range := (1+NumLines) * 8
        Else
           ScrollBox1.VertScrollBar.Range := 0;

        ScrollBox1.VertScrollBar.Increment := 8;
        ScrollBox1.VertScrollBar.Visible := True;

        ScrollBox1.HorzScrollBar.Range := (1+CurFile^.MaxLineLen) * 8;
        ScrollBox1.HorzScrollBar.Increment := 8;
        ScrollBox1.HorzScrollBar.Visible := True;

     End;

     FastIMG1.BringToFront;
     FastIMG1.Repaint;

  End;

  StatusBar2.Panels[1].Text := ' Ln: '+IntToStr(Max(CurFile^.CursorLine, 1)) + ' Col: ' + IntToStr(CurFile^.CursorColumn);

End;

Procedure TASMEditorWindow.NewWord(DIB: TFastDIB; CurWord: String; XPos, YPos, Ink, Paper, Bright: Integer; LineBreak, REMLine, InString: Boolean; CurPos: Integer);
Var
  NewText, TempStr: String;
  SelStart: Integer;
  Bold, Italic: Boolean;
Label
  Render;
Begin
  If CurWord = '' Then Exit;
  NewText := UpperCase(CurWord);

  Bold := False;
  Italic := False;

  If Opt_SyntaxHighlight and not LineBreak Then Begin
     Paper := -1;
     If Opt_AsmHLComments Then Begin
        If REMLine or (NewText[1] = ';') Then Begin // A Comment?
           Ink := Opt_AsmCommentsColour;
           Bold := Opt_AsmCommentsBold;
           Italic := Opt_AsmCommentsItalic;
           Goto Render;
        End;
     End;
     If Opt_ASMHLStrings Then Begin
        If InString or (NewText[1] in ['"', #39]) Then Begin // A string literal?
           Ink := Opt_AsmStringColour;
           Bold := Opt_AsmStringBold;
           Italic := Opt_AsmStringItalic;
           InString := True;
           Goto Render;
        End;
     End;
     If Opt_ASMHLSymbols Then Begin
        If (Length(NewText) = 1) and (NewText[1] in ['!', '"', '£', '$', '%', '^', '&', '*', '(', ')',
                                                     '_', '-', '+', '=', '{', '}', '[', ']', '#', '~', #39,
                                                     '@', ':', '<', '>', ',', '.', '?', '/', '|', '\', '¬', '`']) Then Begin // A Symbol character?
           Ink := Opt_AsmSymbolColour;
           Bold := Opt_AsmSymbolBold;
           Italic := Opt_AsmSymbolItalic;
           Goto Render;
        End;
     End;
     If Opt_ASMHLNumbers Then Begin
        If IsNumber(NewText) Then Begin // A number (with $ or % prefix)?
           Ink := Opt_AsmNumberColour;
           Bold := Opt_AsmNumberBold;
           Italic := Opt_AsmNumberItalic;
           Goto Render;
        End;
     End;
     If Opt_ASMHLOpcodes Then Begin
        If IsOpCode(NewText) Then Begin // A Z80 Opcode?
           Ink := Opt_AsmOpcodeColour;
           Bold := Opt_AsmOpcodeBold;
           Italic := Opt_AsmOpcodeItalic;
           Goto Render;
        End;
     End;
     If Opt_ASMHLRegs Then Begin
        If IsRegister(NewText) Then Begin // A Z80 Register?
           Ink := Opt_AsmRegsColour;
           Bold := Opt_AsmRegsBold;
           Italic := Opt_AsmRegsItalic;
           Goto Render;
        End;
     End;
     TempStr := ValidType(NewText);
     If TempStr <> '' Then Begin
        Case TempStr[1] of
           Chr(Integer(tTLabel)):
              If Opt_AsmHLLabels Then Begin
                 If IsType(TempStr) Then Begin
                    Ink := Opt_AsmLabelColour;
                    Bold := Opt_AsmLabelBold;
                    Italic := Opt_AsmLabelItalic;
                    Goto Render;
                 End;
              End;
           Chr(Integer(tTEquate)):
              If Opt_AsmHLEquates Then Begin
                 If IsType(TempStr) Then Begin
                    Ink := Opt_AsmEquateColour;
                    Bold := Opt_AsmEquateBold;
                    Italic := Opt_AsmEquateItalic;
                    Goto Render;
                 End;
              End;
           Chr(Integer(tTMacro)):
              If Opt_AsmHLMacros Then Begin
                 If IsType(TempStr) Then Begin
                    Ink := Opt_AsmMacroColour;
                    Bold := Opt_AsmMacroBold;
                    Italic := Opt_AsmLabelItalic;
                    Goto Render;
                 End;
              End;
           Chr(Integer(tTStruct)):
              If Opt_AsmHLStructs Then Begin
                 If IsType(TempStr) Then Begin
                    Ink := Opt_AsmStructColour;
                    Bold := Opt_AsmStructBold;
                    Italic := Opt_AsmStructItalic;
                    Goto Render;
                 End;
              End;
           Chr(Integer(tTDefine)):
              If Opt_AsmHLDefines Then Begin
                 If IsType(TempStr) Then Begin
                    Ink := Opt_AsmDefineColour;
                    Bold := Opt_AsmDefineBold;
                    Italic := Opt_AsmDefineItalic;
                    Goto Render;
                 End;
              End;
        End;
     End;
     If Opt_ASMHlReserved Then Begin
        If IsDirective(NewText) Then Begin // An assembler directive (ORG etc)?
           Ink := Opt_AsmReservedColour;
           Bold := Opt_AsmReservedBold;
           Italic := Opt_AsmReservedItalic;
           Goto Render;
        End;
     End;
     If Opt_AsmHLErrors Then Begin
        Ink := Opt_AsmErrorColour;
        Bold := Opt_AsmErrorBold;
        Italic := Opt_AsmErrorItalic;
     End;

  End;

Render:

  If (CurPos > LineSelEnd) or (CurPos + Length(NewText) < LineSelStart) or (LineSelStart = LineSelEnd) Then Begin

     If Bold Then Begin
        SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
        SpecTextToDIB(DIB, XPos +1, YPos +1, CurWord, Ink, -1, Bright, Italic, InString);
     End Else
        SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);

  End Else Begin

     If LineSelStart < CurPos Then
        SelStart := CurPos
     Else
        SelStart := LineSelStart;

     If CurPos < SelStart Then Begin // the selection covers part of the word
        TempStr := Copy(CurWord, 1, SelStart - CurPos);
        If Bold Then Begin
           SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
           SpecTextToDIB(DIB, XPos +1, YPos +1, CurWord, Ink, -1, Bright, Italic, InString);
        End Else
           SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
        Inc(XPos, Length(TempStr) * 8);
     End;

     // Draw the selected part

     TempStr := Copy(CurWord, (SelStart - CurPos)+1, (LineSelEnd - SelStart)+1);
     SpecTextToDIB(DIB, XPos, YPos +1, TempStr, Opt_AsmBackground, Opt_AsmForeground, 0, False, InString);
     Inc(XPos, Length(TempStr) * 8);

     // And any non-selected text after the selection

     If CurPos + Length(NewText) >= LineSelEnd Then Begin
        TempStr := Copy(CurWord, LineSelEnd +1 - CurPos +1, 99999);
        If Bold Then Begin
           SpecTextToDIB(DIB, XPos, YPos +1, TempStr, Ink, Paper, Bright, Italic, InString);
           SpecTextToDIB(DIB, XPos +1, YPos +1, TempStr, Ink, -1, Bright, Italic, InString);
        End Else
           SpecTextToDIB(DIB, XPos, YPos +1, TempStr, Ink, Paper, Bright, Italic, InString);
     End;

  End;

End;

Function TASMEditorWindow.IsDirective(Text: String): Boolean;
Var
  Idx: Integer;
Begin
  Result := False;
  For Idx := 0 To High(AsmWords) -1 Do
     If AsmWords[Idx].S = Text Then Begin
        Result := True;
        Break;
     End;
End;

Function TASMEditorWindow.IsRegister(Text: String): Boolean;
Var
  Idx: Integer;
Begin
  Result := False;
  For Idx := 0 To High(RegisterWords) -1 Do
     If RegisterWords[Idx].S = Text Then Begin
        Result := True;
        Break;
     End;
End;

Function TASMEditorWindow.IsOpcode(Text: String): Boolean;
Var
  Idx: Integer;
Begin
  Result := False;
  For Idx := 0 To High(OpcodesWords) -1 Do
     If OpcodesWords[Idx].S = Text Then Begin
        If OpcodesWords[Idx].I in [6, 51, 52, 75] Then
           LastInstruction := 1
        Else
           If OpcodesWords[Idx].I in [108..115] Then Begin
              If LastInstruction = 1 Then Begin
                 Result := True;
                 LastInstruction := 0;
                 Break;
              End Else Begin
                 Result := False;
                 Break;
              End;
           End Else
              LastInstruction := 0;
        Result := True;
        Break;
     End;
End;

Function TASMEditorWindow.IsType(Text: String): Boolean;
Begin
  Result := CurFile^.Labels.IndexOf(Text) <> -1;
End;

Function TASMEditorWindow.IsNumber(Text: String): Boolean;
Var
  i, Idx: Integer;
Begin
  Result := False;
  i := Length(Text);
  If i = 1 Then
     If not (Text[1] in ['0'..'9']) Then
        Exit;
  If Text[1] in ['$', '&'] Then Begin // $, & Prefix Hex number?
     For Idx := 2 To i Do
        If not (Text[Idx] in ['0'..'9', 'A'..'F']) Then
           Exit;
     Result := True;
  End Else
     If Copy(Text, 1, 2) = '0X' Then Begin // 0x Prefix Hex number?
        If i = 2 Then
           Exit;
        For Idx := 3 To i Do
           If not (Text[Idx] in ['0'..'9', 'A'..'F']) Then
              Exit;
        Result := True;
     End Else
        If Text[i] = 'H' Then Begin // h Suffix Hex number?
           For Idx := 1 To i -1 Do
              If not (Text[Idx] in ['0'..'9', 'A'..'F']) Then
                 Exit;
           Result := True;
        End Else
           If Text[1] = '%' Then Begin // % prefix Binary?
              For Idx := 2 To i Do
                 If not (Text[Idx] in ['0', '1']) Then
                    Exit;
              Result := True;
           End Else
              If Text[i] = 'B' Then Begin // b Suffix Binary?
                 For Idx := 1 To i -1 Do
                    If not (Text[Idx] in ['0', '1']) Then
                       Exit;
                 Result := True;
              End Else
                 If Text[i] = 'O' Then Begin // o Prefix Octal?
                    For Idx := 1 To i -1 Do
                       If not (Text[Idx] in ['0'..'7']) Then
                          Exit;
                    Result := True;
                 End Else Begin // regular number?
                    For Idx := 1 To i Do
                       If not (Text[Idx] in ['0'..'9']) Then
                          Exit;
                    Result := True;
                 End;
End;


Function TASMEditorWindow.ValidType(Text: String): String;
Var
  Len, Idx, BaseIdx: Integer;
  tType: tWordType;
Begin

  Result := '';

  Len := Length(Text);
  Text := Uppercase(Text);
  If Len = 0 Then Exit;
  If Not (Text[1] in ['A'..'Z', '_', '.']) Then Exit;
  Result := Text;

  If Copy(Result, 1, 6) = 'DEFINE' Then Begin
     tType := tTDefine;
     Idx := 7;
     While (Idx <= Len) and (Result[Idx] <= ' ') Do
        Inc(Idx);
     BaseIdx := Idx;
  End Else Begin
     tTYpe := tTLabel;
     Idx := 1;
     BaseIdx := 1;
  End;

  While Idx <= Len Do Begin
     If Text[Idx] <= ' ' Then Begin
        Result := Copy(Text, BaseIdx, Idx - BaseIdx);
        Break;
     End;
     If Not (Text[Idx] in ['A'..'Z', '0'..'9', '_', '.', ':']) Then Begin
        Result := '';
        Exit;
     End Else
        If Text[Idx] = ':' Then Begin
           Result := Copy(Text, 1, Idx -1);
           Break;
        End;
     Inc(Idx);
  End;

  If Result <> '' Then Begin

     If Idx <= Length(Text) then
        While (Idx <= Len) and (Text[Idx] <= ' ') Do
           Inc(Idx);

     If (Copy(Text, Idx, 3) = 'EQU') or (Copy(Text, Idx, 1) = '=') Then
        tType := tTEquate
     Else
        If Copy(Text, Idx, 5) = 'STRUCT' Then
           tType := tTStruct
        Else
           If Copy(Text, Idx, 5) = 'MACRO' Then
              tType := tTMacro;

     For Idx := 0 To High(AsmWords) -1 Do
        If AsmWords[Idx].S = Result Then Begin
           Result := '';
           Exit;
        End;

     Result := Chr(Integer(tType))+Result;

  End;

End;

Procedure TASMEditorWindow.RepaintCursor;
Var
  Ink, Paper: Byte;
Begin
  If CurFile^.CursorVisible and (Screen.ActiveForm = Self) Then Begin
     If CurFile^.CodeError Then Begin
        If CurFile^.CursorState Then Begin
           Ink := 2;
           Paper := 7;
        End Else Begin
           Ink := 7;
           Paper := 2;
        End;
     End Else Begin
        If CurFile^.CursorState Then Begin
           Ink := 1;
           Paper := 7;
        End Else Begin
           Ink := 7;
           Paper := 1;
        End;
     End;
     Case CurFile^.CursorChar of
        #32..#143:
           DrawChar(FastIMG1.Bmp, @EditorChars[(Byte(CurFile^.CursorChar)-32)*8], CurFile^.CursorPoint.X, CurFile^.CursorPoint.Y, Ink, Paper, 1);
        #144..#164:
           DrawChar(FastIMG1.Bmp, @Memory[((Byte(CurFile^.CursorChar)-144)*8)+GetWord(@Memory[UDG])-1], CurFile^.CursorPoint.X, CurFile^.CursorPoint.Y, Ink, Paper, 1);
     End;
     FastIMG1.Repaint;
  End;
End;

Procedure TASMEditorWindow.DrawChar(DIB: TFastDIB; CharPtr: PByte; X, Y: Integer; Ink, Paper, Bright: Byte);
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

procedure TASMEditorWindow.FormShow(Sender: TObject);
begin
  If Scrollbox1 = nil Then Begin
     ScrollBox1 := TNewScrollBox.Create(Self);
     SetScrollBoxBounds;
     ScrollBox1.Parent := Panel1;
     ScrollBox1.Anchors := [akTop, akLeft, akbottom, akRight];
     ScrollBox1.AutoScroll := False;
     ScrollBox1.OnVerticalScroll := ScrollBox1VScroll;
     ScrollBox1.OnHorizontalScroll := ScrollBox1HScroll;
  End;
  UsePasmo1.Enabled:= Opt_AsmPasmoAvailable;
  UsePasmo1.Checked:= Opt_AsmPasmoAvailable;

  ListBox1.Color := TfColorToTColor(TFSpecDark);
  RepaintASM(True);
  FormResize(Self);
  timer1.Enabled:=true;
  If Visible Then Edit2.SetFocus;
end;

Procedure TASMEditorWindow.SetScrollBoxBounds;
Var
  X, Y, W, H: Integer;
Begin

  If CurFile <> nil Then Begin

     If Opt_AsmLabelList then Begin
        X := ListBox1.Width + Splitter1.Width;
        W := Panel1.ClientWidth - ListBox1.Width - Splitter1.Width;
     End Else Begin
        X := 0;
        W := Panel1.ClientWidth;
     End;

  End Else Begin

     X := 0;
     W := Panel1.ClientWidth;

  End;

  Y := 0;
  H := Panel1.ClientHeight;

  If ScrollBox1 <> nil Then
     ScrollBox1.SetBounds(X, Y, W, H);

End;

procedure TASMEditorWindow.FormResize(Sender: TObject);
begin
  If ScrollBox1 <> nil Then Begin

     If Opt_AsmStatusBar Then
        Panel1.SetBounds(0, 0, ClientWidth, ClientHeight - TabSet1.Height - StatusBar2.Height - 2)
     Else
        Panel1.SetBounds(0, 0, CLientWidth, ClientHeight - TabSet1.Height - 2);

     FastIMG1.SetBounds(ScrollBox1.Left+2, ScrollBox1.Top+2, ScrollBox1.ClientWidth, ScrollBox1.ClientHeight);
     StatusBar2.Panels[0].Width := StatusBar2.ClientWidth - Canvas.TextWidth('Ln: 000 Col: 000');

     StatusBar2.SetBounds(0, ClientHeight - StatusBar2.Height, ClientWidth, StatusBar2.Height);
     If Opt_AsmStatusBar Then
        TabSet1.SetBounds(0, StatusBar2.Top - TabSet1.Height, ClientWidth, TabSet1.Height)
     Else
        TabSet1.SetBounds(0, ClientHeight - TabSet1.Height, ClientWidth, TabSet1.Height);
     listBox1.SetBounds(0, 0, ListBox1.Width, ClientHeight - StatusBar2.Height - TabSet1.Height);
     Splitter1.SetBounds(ListBox1.Width, 0, Splitter1.Width, ListBox1.Width);

     RepaintASM(True);

  End;
end;

procedure TASMEditorWindow.ScrollBox1VScroll(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType);
begin
  If Visible Then Edit2.SetFocus;
  If EventType = vsThumbTrack Then
     CurFile^.ViewLine := ScrollBox1.ScrollInfo.nTrackPos Div 8
  Else
     CurFile^.ViewLine := ScrollBox1.ScrollInfo.nPos Div 8;
  RepaintASM(True);
end;

procedure TASMEditorWindow.ScrollBox1HScroll(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType);
begin
  If Visible Then Edit2.SetFocus;
  If EventType = hsThumbTrack Then
     CurFile^.ViewColumn := ScrollBox1.ScrollInfo.nTrackPos Div 8
  Else
     CurFile^.ViewColumn := ScrollBox1.ScrollInfo.nPos Div 8;
  RepaintASM(True);
end;

procedure TASMEditorWindow.Timer1Timer(Sender: TObject);
begin
  If CurFile <> nil Then Begin
     CurFile^.CursorState := Not CurFile^.CursorState;
     If Screen.ActiveForm = Self Then Begin
        RepaintCursor;
        NonActivePaint := False;
     End Else
        If Not NonActivePaint Then Begin
           NonActivePaint := True;
           RepaintASM(True);
           FastIMG1.Repaint;
        End;
  End;
end;

procedure TASMEditorWindow.FormCreate(Sender: TObject);
begin

  FileCount := 0;
  ScrollBox1 := nil;
  NewFile('');

end;

procedure TASMEditorWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  PerformKeyDown(Key, Shift, True);
End;

Procedure TASMEditorWindow.PerformKeyDown(Key: Word; Shift: TShiftState; Repaint: Boolean);
Var
  Count, NumSpaces: Integer;

  Pt: TPoint;
  Col, Line: pInteger;
  InsertChar: Char;
  LineStr, KeyString: String;
begin
  If Not Edit2.Focused Then Exit;

  CurFile^.CodeError := False;

  Col := @CurFile^.CursorColumn;
  Line := @CurFile^.CursorLine;

  Case Key of
     VK_LEFT:
        Begin
           If not (ssCtrl in Shift) Then Begin
              Dec(Col^);
              If Col^ = 0 Then
                 If Line^ > 1 Then Begin
                    Dec(Line^);
                    Col^ := Length(CurFile^.AsmStrs[Line^ -1]) +1;
                 End Else Begin
                    Col^ := 1;
                    Line^ := 1;
                 End;
              MakeSound(1);
           End Else Begin
              If Col^ > 1 Then Begin
                 Dec(Col^);
                 While (Col^ > 0) and not (CurFile^.AsmStrs[Line^ -1][Col^] in ['_', 'A'..'Z', 'a'..'z', '0'..'9']) Do Dec(Col^);
                 While (Col^ > 0) and (CurFile^.AsmStrs[Line^ -1][Col^] in ['_', 'A'..'Z', 'a'..'z', '0'..'9']) Do Dec(Col^);
                 If Col^ = 0 Then Begin
                    If Line^ > 1 Then Begin
                       Dec(Line^);
                       Col^ := Length(CurFile^.AsmStrs[Line^ -1]) +1;
                       PerformKeyDown(VK_LEFT, Shift, Repaint);
                    End Else
                       Col^ := 1;
                 End Else Begin
                    Inc(Col^);
                    MakeSound(1);
                 End;
              End Else Begin
                 If Line^ > 1 Then Begin
                    Dec(Line^);
                    Col^ := Length(CurFile^.AsmStrs[Line^ -1]) +1;
                    PerformKeyDown(VK_LEFT, Shift, Repaint);
                 End Else
                    Col^ := 1;
              End;
           End;
        End;
     VK_RIGHT:
        Begin
           If Not (ssCtrl in Shift) Then Begin
              Inc(Col^);
              If Col^ > Length(CurFile^.AsmStrs[Line^ -1]) +1 Then
                 If Line^ < CurFile^.AsmStrs.Count Then Begin
                    Col^ := 1;
                    Inc(Line^);
                 End Else
                    Dec(Col^);
              MakeSound(1);
           End Else Begin
              While (Col^ < Length(CurFile^.AsmStrs[Line^ -1])+1) and (CurFile^.AsmStrs[Line^ -1][Col^] in ['_', 'A'..'Z', 'a'..'z', '0'..'9']) Do Inc(Col^);
              While (Col^ < Length(CurFile^.AsmStrs[Line^ -1])+1) and not (CurFile^.AsmStrs[Line^ -1][Col^] in ['_', 'A'..'Z', 'a'..'z', '0'..'9']) Do Inc(Col^);
              If Col^ = Length(CurFile^.AsmStrs[Line^ -1])+1 Then Begin
                 If Line^ < CurFile^.AsmStrs.Count Then Begin
                    Inc(Line^);
                    Col^ := 1;
                    PerformKeyDown(VK_RIGHT, Shift, Repaint);
                 End Else
                    Col^ := 1;
              End Else
                 MakeSound(1);
           End;
        End;
     VK_UP:
        Begin
           If ssCtrl in Shift Then
              If CurFile^.ViewLine > 0 Then
                 Dec(CurFile^.ViewLine);
           If Line^ > 1 Then Begin
              Dec(Line^);     
              Col^ := Min(Col^, Length(CurFile^.AsmStrs[Line^ -1]) +1);
           End;
           MakeSound(1);
        End;
     VK_DOWN:
        Begin
           If ssCtrl in Shift Then
              If CurFile^.ViewLine < CurFile^.AsmStrs.Count - (FastIMG1.Bmp.AbsHeight Div 8) Then
                 Inc(CurFile^.ViewLine);
           If Line^ < CurFile^.AsmStrs.Count Then Begin
              Inc(Line^);
              Col^ := Min(Col^, Length(CurFile^.AsmStrs[Line^ -1]) +1);
           End;
           MakeSound(1);
        End;
     VK_HOME:
        Begin
           If ssCtrl in Shift Then
              Line^ := 1;
           Col^ := 1;
           MakeSound(1);
        End;
     VK_END:
        Begin
           If ssCtrl in Shift Then
              Line^ := CurFile^.AsmStrs.Count;
           Col^ := Length(CurFile^.AsmStrs[Line^ -1]) +1;
           MakeSound(1);
        End;
     VK_PRIOR:
        Begin
           Line^ := Max(1, Line^ - (FastIMG1.Bmp.AbsHeight Div 8));
           Col^ := Min(Col^, Length(CurFile^.AsmStrs[Line^ -1]) +1);
           MakeSound(1);
        End;
     VK_NEXT:
        Begin
           Line^ := Min(CurFile^.AsmStrs.Count, Line^ + (FastIMG1.Bmp.AbsHeight Div 8));
           Col^ := Min(Col^, Length(CurFile^.AsmStrs[Line^ -1]) +1);
           MakeSound(1);
        End;
     VK_BACK:
        Begin
           If Not RemoveSelection Then
              If Col^ > 1 Then Begin
                 LineStr := CurFile^.AsmStrs[Line^ -1];
                 LineStr := Copy(LineStr, 1, Col^ -2) + Copy(LineStr, Col^, 9999);
                 CurFile^.AsmStrs[Line^ -1] := LineStr;
                 ChangeLine(Line^ -1);
                 Dec(Col^);
              End Else Begin
                 If Line^ > 1 Then Begin
                    LineStr := CurFile^.AsmStrs[Line^ -2];
                    Col^ := Length(LineStr) +1;
                    LineStr := LineStr + CurFile^.AsmStrs[Line^ -1];
                    CurFile^.AsmStrs[Line^ -2] := LineStr;
                    DeleteLine(Line^ -1);
                    ChangeLine(Line^ -2);
                    Dec(Line^);
                 End;
              End;
           MakeSound(1);
           CurFile^.Changed := True;
        End;
     VK_TAB:
        Begin
           RemoveSelection;
           NumSpaces := ((Opt_TabSize +1) + (((Col^ -1) Div Opt_TabSize) * Opt_TabSize)) - Col^;
           LineStr := CurFile^.AsmStrs[Line^ -1];
           Count := NumSpaces;
           While Count > 0 Do Begin
              LineStr := Copy(LineStr, 1, Col^ -1)+' '+Copy(LineStr, Col^, 99999);
              Dec(Count);
           End;
           CurFile^.AsmStrs[Line^ -1] := LineStr;
           Inc(Col^, NumSpaces);
           ChangeLine(Line^ -1);
           MakeSound(1);
        End;
     VK_DELETE:
        Begin
           If Not RemoveSelection Then Begin
              LineStr := CurFile^.AsmStrs[Line^ -1];
              If Col^ = Length(LineStr) +1 Then Begin
                 If Line^ < CurFile^.AsmStrs.Count Then Begin
                    LineStr := LineStr + CurFile^.AsmStrs[Line^];
                    DeleteLine(Line^);
                    CurFile^.Changed := True;
                 End;
              End Else Begin
                 LineStr := Copy(LineStr, 1, Col^ -1) + Copy(LineStr, Col^ +1, 99999);
                 CurFile^.Changed := True;
              End;
              CurFile^.AsmStrs[Line^ -1] := LineStr;
              ChangeLine(Line^ -1);
           End;
           MakeSound(1);
        End;
     VK_SHIFT:
        Begin
           Exit;
        End;
     VK_RETURN:
        Begin
           RemoveSelection;
           If Col^ = Length(CurFile^.AsmStrs[Line^ -1]) +1 Then Begin
              InsertLine(Line^, '');
              Col^ := 1;
              Inc(Line^);
           End Else Begin
              LineStr := CurFile^.AsmStrs[Line^ -1];
              InsertLine(Line^, Copy(LineStr, Col^, 99999));
              CurFile^.AsmStrs[Line^ -1] := Copy(LineStr, 1, Col^ -1);
              ChangeLine(Line^ -1);
              Col^ := 1;
              Inc(Line^);
           End;
           CurFile^.Changed := True;
           MakeSound(1);
        End;
  Else
     Begin
        KeyString := GetCharFromVKey(Key);
        If KeyString <> '' Then Begin
           InsertChar := KeyString[1];
           If InsertChar <> '' Then Begin
              RemoveSelection;
              LineStr := CurFile^.AsmStrs[Line^ -1];
              LineStr := Copy(LineStr, 1, Col^ -1) + InsertChar + Copy(LineStr, Col^, 99999);
              Inc(Col^);
              CurFile^.Changed := True;
              CurFile^.AsmStrs[Line^ -1] := LineStr;
              ChangeLine(Line^ -1);
              Shift := Shift - [ssShift];
              MakeSound(1);
           End;
        End;
     End;
  End;

  Pt := Point(Col^, Line^);
  UpdateCursorPos(Pt, ssShift in Shift);
  Line^ := Pt.Y;
  Col^ := Pt.X;
  RepaintASM(Repaint);
  If Repaint Then MakeCursorVisible;
  If CurFile^.Changed Then UpdateCaption;

end;

Procedure TASMEditorWindow.GetMaxLineLen;
Var
  Idx, Len: Integer;
Begin

  For Idx := 0 To CurFile^.AsmStrs.Count -1 Do Begin

     Len := Length(CurFile^.AsmStrs[Idx]);
     If Len > CurFile^.MaxLineLen Then
        CurFile^.MaxLineLen := Len;                                                                   

  End;

End;

Procedure TASMEditorWindow.ChangeLine(LineNum: Integer);
Var
  Lab: String;
  Idx: Integer;
Begin
  // Check to see if a label now exists, and update the list if necessary.
  Lab := ValidType(CurFile^.AsmStrs[LineNum]);
  If Lab = '' Then Begin
     If TAsmLine(CurFile^.AsmStrs.Objects[LineNum]).LabelText <> Nil Then Begin
        // Remove the label from the list
        TAsmLine(CurFile^.AsmStrs.Objects[LineNum]).LabelText := nil;
        For Idx := 0 To CurFile^.Labels.Count -1 Do
           If Integer(CurFile^.Labels.Objects[Idx]) = LineNum +1 Then Begin
              CurFile^.Labels.Delete(Idx);
              break;
           End;
        BuildLabelList;
     End;
  End Else Begin
     // Add the new label to the list
     If TAsmLine(CurFile^.AsmStrs.Objects[LineNum]).LabelText = Nil Then Begin
        CurFile^.Labels.Add(Lab);
        CurFile^.Labels.Objects[CurFile^.Labels.Count -1] := TObject(Integer(LineNum +1));
        TAsmLine(CurFile^.AsmStrs.Objects[LineNum]).LabelText := Pointer(CurFile^.Labels[CurFile^.Labels.Count -1]);
        BuildLabelList;
     End Else Begin
        For Idx := 0 To CurFile^.Labels.Count -1 Do
           If Integer(CurFile^.Labels.Objects[Idx]) = LineNum +1 Then Begin
              CurFile^.Labels[Idx] := Lab;
              break;
           End;
        BuildLabelList;
     End;

  End;

  // And a similar operation for equates

  GetMaxLineLen;

End;

Procedure TASMEditorWindow.DeleteLine(LineNum: Integer);
Var
  Idx: Integer;
Begin

  // Remove a line, and it's label if necessary, and update the line numbers of other labels accordingly
  // LineNum is zero-based

  // Remove the line's label entry and the line itself
  TAsmLine(CurFile^.AsmStrs.Objects[LineNum]).Free;
  CurFile^.AsmStrs.Delete(LineNum);

  // Now run through the label list, and delete the label

  Idx := 0;
  While Idx < CurFile^.Labels.Count Do Begin
     If Integer(CurFile^.Labels.Objects[Idx]) = LineNum +1 Then Begin
        CurFile^.Labels.Delete(Idx);
        Break;
     End Else
        Inc(Idx);
  End;

  // And now update the labels that are *after* this line
  Idx := 0;
  While Idx < CurFile^.Labels.Count Do Begin
     If Integer(CurFile^.Labels.Objects[Idx]) > LineNum Then
        CurFile^.Labels.Objects[Idx] := TObject(Integer(CurFile^.Labels.Objects[Idx])-1);
     Inc(Idx);
  End;

  GetMaxLineLen;

End;

Procedure TASMEditorWindow.InsertLine(LineNum: Integer; Text: String);
Var
  Idx: Integer;
Begin
  // Insert a new line, check for a label, and update the other labels to shift their line numbers.
  // LineNum is zero-based
  Idx := 0;
  While Idx < CurFile^.Labels.Count Do Begin
     If Integer(CurFile^.Labels.Objects[Idx]) > LineNum Then
        CurFile^.Labels.Objects[Idx] := TObject(Integer(CurFile^.Labels.Objects[Idx])+1);
     Inc(Idx);
  End;
  CurFile^.AsmStrs.Insert(LineNum, Text);
  CurFile^.AsmStrs.Objects[LineNum] := TAsmLine.Create;
  TAsmLine(CurFile^.AsmStrs.Objects[LineNum]).LineLen := Length(Text);
  ChangeLine(LineNum);
  GetMaxLineLen;
End;

Procedure TASMEditorWindow.MakeCursorVisible;
Var
  Col, Line, vCol, vLine: pInteger;
  vSize, vWidth, vMargin: Integer;
Begin
  Col := @CurFile^.CursorColumn;
  Line := @CurFile^.CursorLine;
  vLine := @CurFile^.ViewLine;
  vCol := @CurFile^.ViewColumn;
  vSize := FastIMG1.Bmp.AbsHeight Div 8;
  vWidth := FastIMG1.Bmp.Width Div 8;
  vMargin := 4;

  If Line^ < vLine^ + vMargin Then
     vLine^ := Line^ - vMargin;

  If Line^ > vLine^ + vSize - vMargin Then
     vLine^ := Line^ - vSize + vMargin;

  vLine^ := Max(Min(CurFile.AsmStrs.Count - vSize +2, vLine^), 0);

  If Col^ < vCol^ + vMargin Then
     vCol^ := Col^ - vMargin;

  If Col^ > vCol^ + vWidth - vMargin Then
     vCol^ := Col^ - vWidth + vMargin;

  vCol^ := Max(Min(CurFile.MaxLineLen - vWidth -1, vCol^), 0);

  ScrollBox1.VertScrollBar.Position := vLine^ * 8;
  ScrollBox1.HorzScrollBar.Position := vCol^ * 8;

  RepaintASM(True);

End;

procedure TASMEditorWindow.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  cPoint: TPoint;
begin
  If Visible Then Edit2.SetFocus;
  If Button = mbLeft Then Begin
     cPoint := GetCharPos(X, Y);
      if (cPoint.X >= 0) and (cPoint.Y >= 0) and (cPoint.Y < CurFile^.AsmStrs.Count) then
      Begin
       UpdateCursorPos(cPoint, ssShift in Shift);
       CurFile^.CursorLine := cPoint.Y;
       CurFile^.CursorColumn := cPoint.X;
       RepaintASM(True);
       MakeCursorVisible;
       MouseDown := True;
     End;
  End;
End;

procedure TASMEditorWindow.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
end;

procedure TASMEditorWindow.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  cPoint: TPoint;
begin
  If MouseDown Then Begin
     cPoint := GetCharPos(X, Y);
     UpdateCursorPos(cPoint, True);
     CurFile^.CursorLine := cPoint.Y;
     CurFile^.CursorColumn := cPoint.X;
     RepaintASM(True);
     MakeCursorVisible;
  End;
end;


Function TASMEditorWindow.GetCharPos(X, Y: Integer): TPoint;
Var
  Line, Column: Integer;
Begin

  Line := 0;
  While Line * 8 < Y Do
     Inc(Line);
  Result.Y := CurFile.ViewLine + Line;

  Column := 0;
  While Column * 8 < X Do
     Inc(Column);
  If CurFile.ViewColumn = 0 Then
     Result.X := CurFile.ViewColumn + Column
  Else
     Result.X := CurFile.ViewColumn + Column -1;

end;

procedure TASMEditorWindow.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  If Not Edit2.Focused Then Exit;
  CurFile^.ViewLine := Max(0, Min((ScrollBox1.VertScrollBar.Range Div 8) - (FastIMG1.Height Div 8), CurFile^.ViewLine +3));
  ScrollBox1.VertScrollBar.Position := CurFile^.ViewLine * 8;
  RepaintASM(True);
  RepaintCursor;
end;

procedure TASMEditorWindow.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  If Not Edit2.Focused Then Exit;
  If CurFile^.ViewLine > 3 Then
     CurFile^.ViewLine := CurFile^.ViewLine -3
  Else
     CurFile^.ViewLine := 0;
  ScrollBox1.VertScrollBar.Position := CurFile^.ViewLine * 8;
  RepaintASM(True);
  RepaintCursor;
end;

Procedure TASMEditorWindow.UpdateCursorPos(var Offset: TPoint; Shifted: Boolean);
Begin
  if Offset.Y > CurFile^.AsmStrs.Count then
    Offset.Y := CurFile^.AsmStrs.Count;

  if Offset.Y < 1 then
    Offset.Y := 1;

  if Offset.X > Length(CurFile^.AsmStrs[Offset.Y - 1]) + 1 then
    Offset.X := Length(CurFile^.AsmStrs[Offset.Y - 1]) + 1;

  if Shifted then
    CurFile^.EditorSelEnd := Point(Offset.X, Offset.Y)
  else
  begin
    CurFile^.EditorSelStart := Point(Offset.X, Offset.Y);
    CurFile^.EditorSelEnd := Point(Offset.X, Offset.Y);
  end;
End;


Function TASMEditorWindow.SelStart: TPoint;
Begin
  If CurFile^.EditorSelStart.Y < CurFile^.EditorSelEnd.Y Then Begin
     Result.X := CurFile^.EditorSelStart.X;
     Result.Y := CurFile^.EditorSelStart.Y;
  End Else Begin
     If CurFile^.EditorSelStart.Y = CurFile^.EditorSelEnd.Y Then Begin
        If CurFile.EditorSelStart.X < CurFile^.EditorSelEnd.X Then Begin
           Result.X := CurFile^.EditorSelStart.X;
           Result.Y := CurFile^.EditorSelStart.Y;
        End Else Begin
           Result.X := CurFile^.EditorSelEnd.X;
           Result.Y := CurFile^.EditorSelEnd.Y;
        End;
     End Else Begin
        Result.X := CurFile^.EditorSelEnd.X;
        Result.Y := CurFile^.EditorSelEnd.Y;
     End;
  End;
End;

Function TASMEditorWindow.SelEnd: TPoint;
Begin
  If CurFile^.EditorSelStart.Y < CurFile^.EditorSelEnd.Y Then Begin
     Result.X := CurFile^.EditorSelEnd.X;
     Result.Y := CurFile^.EditorSelEnd.Y;
  End Else Begin
     If CurFile^.EditorSelStart.Y = CurFile^.EditorSelEnd.Y Then Begin
        If CurFile.EditorSelStart.X < CurFile^.EditorSelEnd.X Then Begin
           Result.X := CurFile^.EditorSelEnd.X;
           Result.Y := CurFile^.EditorSelEnd.Y;
        End Else Begin
           Result.X := CurFile^.EditorSelStart.X;
           Result.Y := CurFile^.EditorSelStart.Y;
        End;
     End Else Begin
        Result.X := CurFile^.EditorSelStart.X;
        Result.Y := CurFile^.EditorSelStart.Y;
     End;
  End;
End;

Function TASMEditorWindow.RemoveSelection: Boolean;
Var
  sStart, sEnd: TPoint;
  TempStr: String;
Begin

  Result := False;

  // Get the start and end points

  sStart := SelStart;
  sEnd := SelEnd;

  // Is there a selection worthy of note?

  If sStart.x = sEnd.x Then
     If sStart.y = sEnd.y Then
        Exit;

  // Is the start point the cursor position?

  If (sStart.Y = CurFile^.CursorLine) and (sStart.X = CurFile.CursorColumn) Then Begin
     // Advance the selection one character into the file
     If sStart.X < Length(CurFile^.AsmStrs[sStart.Y -1]) Then
        Inc(sStart.X)
     Else Begin
        sStart.X := 1;
        Inc(sStart.Y);
     End;
  End Else Begin
     // cut the selection short by one character.
     If sEnd.X > 1 Then
        Dec(sEnd.X)
     Else Begin
        Dec(sEnd.Y);
        sEnd.X := Length(CurFile^.AsmStrs[sEnd.Y -1]);
     End;
     CurFile^.CursorLine := sStart.Y;
     CurFile^.CursorColumn := sStart.X;
  End;

  // Now, remove the selection.

  If sStart.Y = sEnd.Y then Begin
     // a one line selection.
     TempStr := CurFile^.AsmStrs[sStart.y -1];
     TempStr := Copy(TempStr, 1, sStart.X -1) + Copy(TempStr, sEnd.X +1, 99999);
     CurFile^.AsmStrs[sStart.Y -1] := TempStr;
     ChangeLine(sStart.Y -1);
  End Else Begin
     // More than one line. Cut the first line to shape. Add the remainder of the last line to it, and call change.
     // Delete all other lines up to and including the end line.
     TempStr := Copy(CurFile^.AsmStrs[sStart.Y -1], 1, sStart.X -1) + Copy(CurFile^.AsmStrs[sEnd.Y -1], sEnd.X +1, 99999);
     While sEnd.Y <> sStart.Y -1 do Begin
        DeleteLine(sStart.Y -1);
        Dec(sEnd.Y);
     End;
     If TempStr <> '' Then Begin
        CurFile^.AsmStrs[sStart.Y -1] := TempStr;
        ChangeLine(sStart.Y -1);
     End;
     BuildLabelList;
  End;

  CurFile^.Changed := True;
  Result := True;

End;

Function TASMEditorWindow.SelectionAsText: String;
Var
  sStart, sEnd: TPoint;
  Idx: Integer;
Begin

  Result := '';

  sStart := SelStart;
  sEnd := SelEnd;

  If (sStart.x <> sEnd.x) or (sStart.y <> sEnd.y) Then Begin
     If (sStart.Y = CurFile^.CursorLine) and (sStart.X = CurFile.CursorColumn) Then Begin
        // Advance the selection one character into the file
        If sStart.X < Length(CurFile^.AsmStrs[sStart.Y -1]) Then
           Inc(sStart.X)
        Else Begin
           sStart.X := 1;
           Inc(sStart.Y);
        End;
     End Else Begin
        // cut the selection short by one character.
        If sEnd.X > 1 Then
           Dec(sEnd.X)
        Else Begin
           Dec(sEnd.Y);
           sEnd.X := Length(CurFile^.AsmStrs[sEnd.Y -1]);
        End;
     End;
     If sStart.y = sEnd.y Then
        Result := Copy(CurFile^.AsmStrs[sStart.y -1], sStart.x, (sEnd.x - sStart.x)+1)
     Else Begin
        Result := Copy(CurFile^.AsmStrs[sStart.y -1], sStart.x, 99999) + #13#10;
        Idx := sStart.y;
        While Idx <> sEnd.y -1 Do Begin
           Result := Result + CurFile^.AsmStrs[Idx] + #13#10;
           Inc(Idx);
        End;
        Result := Result + Copy(CurFile^.AsmStrs[sEnd.y -1], 1, sEnd.x);
     End;

  End;

End;

Procedure TASMEditorWindow.NewFile(Filename: String);
Var
  Idx: Integer;
Begin

  SetLength(AsmFiles, FileCount +1);
  Inc(FileCount);
  New(AsmFiles[FileCount -1]);

  AsmFiles[FileCount -1]^.AsmStrs := TStringlist.Create;
  AsmFiles[FileCount -1]^.AsmStrs.Add('');
  AsmFiles[FileCount -1]^.AsmStrs.Objects[0] := TAsmLine.Create;
  TAsmLine(AsmFiles[FileCount -1]^.AsmStrs.Objects[0]).LabelText := nil;

  AsmFiles[FileCount -1]^.CursorState := False;
  AsmFiles[FileCount -1]^.CursorChar := ' ';
  AsmFiles[FileCount -1]^.CodeError := False;
  AsmFiles[FileCount -1]^.Changed := False;
  AsmFiles[FileCount -1]^.ValidFile := False;
  AsmFiles[FileCount -1]^.CursorInString := False;
  AsmFiles[FileCount -1]^.CursorVisible := True;
  AsmFiles[FileCount -1]^.ViewLine := 0;
  AsmFiles[FileCount -1]^.ViewColumn := 0;
  AsmFiles[FileCount -1]^.CursorLine := 1;
  AsmFiles[FileCount -1]^.CursorColumn := 1;
  AsmFiles[FileCount -1]^.EditorSelStart := Point(0, 0);
  AsmFiles[FileCount -1]^.EditorSelEnd := Point(0, 0);
  AsmFiles[FileCount -1]^.Labels := TStringlist.Create;
  AsmFiles[FileCount -1]^.AssembledLabels := TStringlist.Create;
  AsmFiles[FileCount -1]^.Types := TStringlist.Create;
  AsmFiles[FileCount -1]^.MaxLineLen := 0;

  RepaintASM(True);

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

  FastIMG1.Visible := True;
  ListBox1.Visible := True;
  SetScrollBoxBounds;
  FormResize(nil);

  If Visible Then Edit2.SetFocus;

End;

Procedure TASMEditorWindow.BuildAsmRecords(AsmFile: TAsmFile);
Var
  Idx, Len: Integer;
  Lab: String;
Begin

  For Idx := 0 To AsmFile.AsmStrs.Count -1 Do Begin

     AsmFile.AsmStrs.Objects[Idx] := TAsmLine.Create;

     // Determine if this line declares a valid Label

     TAsmLine(AsmFile.AsmStrs.Objects[Idx]).LabelText := nil;
     Lab := ValidType(AsmFile.AsmStrs[Idx]);
     If Lab <> '' Then Begin
        AsmFile.Labels.Add(Lab);
        AsmFile.Labels.Objects[AsmFile.Labels.Count -1] := TObject(Integer(Idx +1));
        TAsmLine(AsmFile.AsmStrs.Objects[Idx]).LabelText := Pointer(AsmFile.Labels[AsmFile.Labels.Count -1]);
     End;

     Len := Length(AsmFile.AsmStrs[Idx]);
     TAsmLine(AsmFile.AsmStrs.Objects[Idx]).LineLen := Len;
     If Len > AsmFile.MaxLineLen Then
        AsmFile.MaxLineLen := Len;

  End;

  BuildLabelList;
  GetMaxLineLen;

End;

Procedure TASMEditorWindow.Open(Filename: String);
Var
  NewAsm: TStringlist;
  Done: Boolean;
  Idx: Integer;
Begin

  NewAsm := OpenAsmFile(Filename);

  If NewAsm <> nil Then Begin

     Done := False;
     For Idx := 0 To FileCount -1 Do Begin
        If Not HasContent(Idx) Then Begin
           CurFile := AsmFiles[Idx];
           CurFile^.CursorState := False;
           CurFile^.CursorChar := ' ';
           CurFile^.CodeError := False;
           CurFile^.Changed := False;
           CurFile^.ValidFile := False;
           CurFile^.CursorInString := False;
           CurFile^.CursorVisible := True;
           CurFile^.ViewLine := 0;
           CurFile^.ViewColumn := 0;
           CurFile^.CursorLine := 1;
           CurFile^.CursorColumn := 1;
           CurFile^.EditorSelStart := Point(0, 0);
           CurFile^.EditorSelEnd := Point(0, 0);
           CurFile^.MaxLineLen := 0;
           CurFile^.AsmFilename := Filename;
           CurFile^.ValidFile := True;
           TabSet1.Tabs[Idx] := TrimExtension(ExtractFilename(AsmFiles[Idx]^.AsmFilename));
           TabSet1.TabIndex := Idx;
           Caption := 'z80 Assembler - '+AsmFiles[Idx]^.AsmFilename;
           If AsmFiles[Idx]^.Changed Then Caption := Caption + '*';
           Done := True;
           Break;
        End;
     End;
     If Not Done Then
       NewFile(Filename);

     CurFile^.AsmStrs.Clear;
     CurFile^.AsmStrs.AddStrings(NewAsm);

     BuildAsmRecords(CurFile^);
     RepaintASM(True);

     Caption := 'z80 Assembler - '+CurFile^.AsmFilename;
     FormResize(nil);
     If Visible Then Edit2.SetFocus;

  End;

End;

Procedure TASMEditorWindow.NewFromText(Filename: String; var Assembly: TStringlist);
Begin

  NewFile(Filename);
  CurFile^.AsmStrs.Clear;
  CurFile^.AsmStrs.AddStrings(Assembly);

  BuildAsmRecords(CurFile^);

  RepaintASM(True);
  Caption := 'z80 Assembler - '+CurFile^.AsmFilename+'*';
  FormResize(nil);
  If Visible Then Edit2.SetFocus;

End;

Function TASMEditorWindow.OpenAsmFile(var Filename: String): TStringlist;
Var
  Idx, Ps, Count, NumSpaces: Integer;
Begin

  If Filename = '' Then Begin
     Filename := OpenFile(Handle, 'Load Assembly Source', [FTAssembly], '', False, False);
     If Filename = '' Then
        Exit;
  End;

  Result := TStringlist.Create;
  Result.LoadFromFile(Filename);

  For Idx := 0 To Result.Count -1 Do Begin

     While Pos(#09, Result[Idx]) <> 0 Do Begin

        Ps := Pos(#09, Result[Idx]);
        Result[Idx] := Copy(Result[Idx], 1, Ps -1)+Copy(Result[Idx], Ps +1, 99999);
        NumSpaces := ((Opt_TabSize+1) + (((Ps -1) Div Opt_TabSize) * Opt_TabSize)) - Ps;
        Count := NumSpaces;
        While Count > 0 Do Begin
           Result[Idx] := Copy(Result[Idx], 1, Ps -1)+' '+Copy(Result[Idx], Ps, 99999);
           Dec(Count);
        End;

     End;

  End;

  If Result.Count = 0 Then Begin
     Result.Free;
     Result := nil;
  End;

End;

Function TASMEditorWindow.CheckForSave(Index: Integer): Boolean;
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

Function TASMEditorWindow.SaveAsm(Index: Integer; SaveAs: Boolean): Boolean;
Var
  Assembly: TStringlist;
  Filename: String;
Begin

  Result := True;

  If (Not AsmFiles[Index]^.ValidFile) or SaveAs Then Begin
     Filename := OpenFile(Handle, 'Save Assembly Text', [FTAssembly], '', True, False);
     If Filename = '' Then Begin
        Result := False;
        Exit;
     End;
  End Else
     Filename := AsmFiles[Index]^.AsmFilename;

  If FileExists(Filename) Then DeleteFile(Filename);
  If FileExists(Filename) Then
     MessageBox(Handle, pChar('Could not overwrite your file '#39+ExtractFilename(Filename)+#39+'.'), pChar('Save Error'), MB_ICONWARNING or MB_OK);

  Assembly := AsmFiles[Index]^.AsmStrs;
  Assembly.SaveToFile(Filename);

  AsmFiles[Index]^.AsmFilename := Filename;
  AsmFiles[Index]^.ValidFile := True;
  AsmFiles[Index]^.Changed := False;
  TabSet1.Tabs[Index] := TrimExtension(ExtractFilename(AsmFiles[FileCount -1]^.AsmFilename));
  Caption := 'z80 Assembler - '+AsmFiles[Index]^.AsmFilename;
  If Visible Then Edit2.SetFocus;
End;

Procedure TASMEditorWindow.SaveAs;
Begin

  SaveAsm(TabSet1.TabIndex, True);

End;

Procedure TASMEditorWindow.ClipCut;
Var
  sStart, sEnd: TPoint;
Begin

  sStart := SelStart;
  sEnd := SelEnd;

  If (sStart.x <> sEnd.x) or (sStart.y <> sEnd.y) Then
     ClipBoard.SetTextBuf(pChar(SelectionAsText));

  RemoveSelection;
  RepaintASM(True);
  If Visible Then Edit2.SetFocus;

End;

Procedure TASMEditorWindow.ClipCopy;
Var
  sStart, sEnd: TPoint;
Begin

  sStart := SelStart;
  sEnd := SelEnd;

  If (sStart.x <> sEnd.x) or (sStart.y <> sEnd.y) Then
     ClipBoard.SetTextBuf(pChar(SelectionAsText));

  RepaintASM(True);
  If Visible Then Edit2.SetFocus;

End;

Procedure TASMEditorWindow.ClipPaste;
Var
  List: TStringlist;
  Idx: Integer;
Begin

  RemoveSelection;

  If ClipBoard.HasFormat(CF_TEXT) Then Begin

     // Convert the text string into a stringlist
     List := TStringlist.Create;
     List.SetText(pChar(Clipboard.AsText));

     For Idx := List.Count -1 DownTo 0 Do
        InsertLine(CurFile^.CursorLine -1, List[Idx]);

     List.Free;
     BuildLabelList;
     RepaintASM(True);

     If Visible Then Edit2.SetFocus;

  End;

End;

Procedure TASMEditorWindow.MenuItemClick(Sender: TObject);
Begin

  Case (Sender As TMenuItem).Tag of

      1: NewFile('');
      2: Open('');
      3: SaveAsm(TabSet1.TabIndex, False);
      4: SaveAs;
      5: if (UsePasmo1.Checked) Then AssembleWithPasmo else Assemble; //1.81 Arda
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
           AsmFindPos := Point(CurFile^.CursorColumn, CurFile^.CursorLine);
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

     19: if (UsePasmo1.Checked) Then AssembleWithPasmoAsDATA else AssembleAsDATA;  //1.81 Arda
     20: if (UsePasmo1.Checked) Then AssembleBinaryWithPasmo else AssembleBinary;  //1.81 Arda
     21: if (UsePasmo1.Checked) Then AssembleTapeWithPasmo else AssembleToTape;  //TODO
     22: CloseFile;
     24: Begin
         //use pasmo
         UsePasmo1.Checked:= Not UsePasmo1.Checked;
     End;

  End;

End;

procedure TASMEditorWindow.SimpleComp;
Var
  SrcFile, OutFile, Linex, ErrorText,  Output: AnsiString;
  Lines: TStringList;
  OrgAdr,NumBytes: Integer;

Begin
  OrgAdr:=32768;
  SaveToTempFile(TabSet1.TabIndex, SrcFile);
  //OutFile := ChangeFileExt(SrcFile, '.bin');
  {if RunPasmo(SrcFile, OutFile, Output) then
  begin
      NumBytes := FileSizeByName(OutFile);
        if (LoadBinToMemory(OutFile,OrgAdr)) Then Begin
          MessageBox(Handle, PChar('Pasmo Assembly succeeded - built ' +
          IntToStr(NumBytes) + ' bytes at' + IntToStr(OrgAdr)), 'Assembly Completed', MB_OK or MB_ICONINFORMATION);
        End Else Begin
          MessageBox(Handle, PChar('Output is too big: change the Org address'), 'Overflow!', MB_OK or MB_ICONINFORMATION);
        End;
  End;}
End;

Function  TASMEditorWindow.GetOrg:Integer;
Var
  Z80Assembler: TZ80Assembler;
  Line, ErrorType, Idx, Idx2, Idx3, Idx4, OrgAdr,i: Integer;
  Filename, Text: String;
  pAsmSymb: pAsmSymbol;
  OnDisk: Boolean;
  Flags: DWord;
  AsmMemory: Array[0..65535] of Byte;
  SrcFile, OutFile, Output, Linex, ErrorText: string;
  Lines: TStringList;
  NumBytes: Integer;
Begin

  If CurFile^.AsmStrs.Count = 0 Then Exit;
  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);
  Z80Assembler.Assemble(CurFile^.AsmStrs, '', -1, '');
  If Z80Assembler.OrgAddr < 0 Then Begin

     result:=-1;
     Exit;
  End;
  OrgAdr:= Z80Assembler.OrgAddr;
  Z80Assembler.Free;
  result:=OrgAdr;
End;

Procedure TASMEditorWindow.AssembleWithPasmo;
Var
  Line, ErrorType, Idx, Idx2, Idx3, Idx4, OrgAdr,i: Integer;
  Filename, Text: String;
  pAsmSymb: pAsmSymbol;
  OnDisk: Boolean;
  Flags: DWord;
  AsmMemory: Array[0..65535] of Byte;
  SrcFile, OutFile, Output, Linex, ErrorText: string;
  Lines: TStringList;
  NumBytes: Integer;
Begin
  OrgAdr:=GetOrg;
  If OrgAdr < 0 Then Begin
     MessageBox(Handle, pChar('Please specify an ORG <adress> as start address.'), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     Exit;
  End;
   //Now ready to send to pasmo   -------------------

  SaveToTempFile(TabSet1.TabIndex, SrcFile);
  OutFile := ChangeFileExt(SrcFile, '.bin');

  if RunPasmo(SrcFile, OutFile, Output) then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := Output;
      ErrorText := '';
      for i := 0 to Lines.Count - 1 do
          if Pos('ERROR', Lines[i]) > 0 then
             ErrorText := ErrorText + Lines[i] + #13#10;

      if ErrorText <> '' then
      begin
        MessageBox(Handle, PChar(ErrorText), 'Compilation Errors...', MB_OK or MB_ICONERROR);
      end
      else if Pos('Pass 2 finished', Output) > 0 then
      begin
        NumBytes := FileSizeByName(OutFile);
        if (LoadBinToMemory(OutFile,memory, OrgAdr)) Then Begin
          MessageBox(Handle, PChar('Pasmo Assembly succeeded - built ' +
          IntToStr(NumBytes) + ' bytes at' + IntToStr(OrgAdr)), 'Assembly Completed', MB_OK or MB_ICONINFORMATION);
        End Else Begin
          MessageBox(Handle, PChar('Output is too big: change the Org address'), 'Overflow!', MB_OK or MB_ICONINFORMATION);
        End;
      end;

   finally
      Lines.Free;
    end;
  end
  else
    ShowMessage('Pasmo could not run.');

End;

function TASMEditorWindow.LoadBinToMemory(const FileName: string; var MemArray: array of Byte; OrgAdr: Word): Boolean;
var
  FS: TFileStream;
  Buffer: array of Byte;
  TotalSize: Integer;
begin
  Result := False;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    TotalSize := FS.Size;
    if OrgAdr + TotalSize > Length(MemArray) then Exit;

    SetLength(Buffer, TotalSize);
    FS.ReadBuffer(Buffer[0], TotalSize);
    Move(Buffer[0], MemArray[OrgAdr], TotalSize);
    Result := True;
  finally
    FS.Free;
  end;
end;




Procedure TASMEditorWindow.SaveToTempFile(Index: Integer; out FileName: string);
var
  TempPath: array[0..MAX_PATH] of Char;
  TempFile: array[0..MAX_PATH] of Char;
begin
  FileName := '';
  if (Index < 0) or (Index >= Length(AsmFiles)) then Exit;
  if not Assigned(AsmFiles[Index]) or not Assigned(AsmFiles[Index]^.AsmStrs) then Exit;

  GetTempPath(MAX_PATH, TempPath);
  GetTempFileName(TempPath, 'ASM', 0, TempFile);
  FileName := TempFile;

  AsmFiles[Index]^.AsmStrs.SaveToFile(FileName);
end;


function  TASMEditorWindow.RunPasmo(const SrcFile, OutFile: string; out Output: AnsiString): Boolean;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutRead, StdOutWrite: THandle;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: DWORD;

  LineStr: AnsiString;

  Cmd: string;
const
  INVALID_HANDLE = THandle(-1);
begin
  StdOutWrite := INVALID_HANDLE;
  StdOutRead  := INVALID_HANDLE;

  Result := False;
  Output := '';

  // Pipe security
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  // Pipe create
  if not CreatePipe(StdOutRead, StdOutWrite, @SA, 0) then Exit;
  try
    ZeroMemory(@SI, SizeOf(SI));
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;
    SI.hStdOutput := StdOutWrite;
    SI.hStdError := StdOutWrite;

    Cmd := ExtractFilePath(Application.ExeName) + 'pasmo.exe -v ' +
           SrcFile + ' ' + OutFile;

    if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, nil, SI, PI) then
    begin
      CloseHandle(StdOutWrite);
      StdOutWrite := INVALID_HANDLE;
while ReadFile(StdOutRead, Buffer, SizeOf(Buffer), BytesRead, nil) do
begin
  if BytesRead = 0 then Break;
  SetString(LineStr, PAnsiChar(@Buffer), BytesRead);
  Output := Output + string(LineStr);
end;

      WaitForSingleObject(PI.hProcess, INFINITE);
      CloseHandle(PI.hProcess);
      CloseHandle(PI.hThread);
      Result := True;
    end;
  finally
    if StdOutRead  <> INVALID_HANDLE then CloseHandle(StdOutRead);
    if StdOutWrite <> INVALID_HANDLE then CloseHandle(StdOutWrite);
  end;
end;



function TASMEditorWindow.FileSizeByName(const FileName: string): Integer;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := FS.Size;
  finally
    FS.Free;
  end;
end;




Procedure TASMEditorWindow.Assemble;
Var
  Z80Assembler: TZ80Assembler;
  Line, ErrorType, Idx, Idx2, Idx3, Idx4: Integer;
  Filename, Text: String;
  pAsmSymb: pAsmSymbol;
  OnDisk: Boolean;
  Flags: DWord;
  AsmMemory: Array[0..65535] of Byte;
Begin
  Move(Memory[0], AsmMemory[0], SizeOf(Memory));

  If CurFile^.AsmStrs.Count = 0 Then Exit;
  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);
  Z80Assembler.Assemble(CurFile^.AsmStrs, '', -1, '');
  If Z80Assembler.NumErrors > 0 Then Begin
     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, 1, Filename);
     Exit;
  End;
  Move(AsmMemory[0], Memory[0], SizeOf(Memory));

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
           If Filename = '' Then Filename := CurFile^.AsmFilename;
           For Idx4 := 0 To Length(AsmFiles)-1 Do
              If Uppercase(AsmFiles[Idx4]^.AsmFilename) = Uppercase(Filename) Then Begin
                 AsmFiles[Idx4]^.AssembledLabels.Clear;
                 Break;
              End;
        End;
     End;

     // Second pass - add the labels to each open file's label list.

     For Idx := 0 To 31 Do Begin
        For Idx2 := 0 To Z80Assembler.AsmSymbols[Idx].Count -1 Do Begin
           idx3 := integer(z80Assembler.AsmSymbols[Idx].Objects[Idx2]);
           pAsmSymb := PAsmSymbol(@z80assembler.AsmSymbolsExt[idx3]);
           flags := pAsmSymb.SetData;
           If (flags and (SYM_SET or SYM_MACRO)) = 0 Then Begin
              OnDisk := True;
              Filename := Z80Assembler.SourceFiles[pAsmSymb.SrcFileNum];
              If Filename = '' Then Filename := CurFile^.AsmFilename;
              For Idx4 := 0 To Length(AsmFiles)-1 Do
                 If Uppercase(AsmFiles[Idx4]^.AsmFilename) = Uppercase(Filename) Then Begin
                    OnDisk := False;
                    Break;
                 End;
              If OnDisk Then Begin
                 AllLabels.Add(IntToStr(pAsmSymb.StartAddress)+','+z80Assembler.AsmSymbols[Idx][Idx2]);
              End Else Begin
                 AsmFiles[Idx4]^.AssembledLabels.Add(IntToStr(pAsmSymb.StartAddress)+','+z80Assembler.AsmSymbols[Idx][Idx2]);
              End;
           End;
        End;
     End;

     // Now build the main list

     For Idx := 0 To Length(AsmFiles) -1 Do Begin
        AllLabels.AddStrings(AsmFiles[Idx]^.AssembledLabels);
     End;

  End;

  CPUWindow.AddAssemblerSymbols;
  Z80Assembler.Free;

  MessageBox(Handle, pChar('Assembly succeeded - built '+IntToStr(NumBytes)+' bytes.'), pChar('Assembly Completed'), MB_OK or MB_ICONINFORMATION);

End;

Procedure TASMEditorWindow.AssembleBinary;
Var
  Z80Assembler: TZ80Assembler;
  AsmMemory: Array[0..65535] of Byte;
  Line, ErrorType: Integer;
  Filename, Text: String;
  FStream: TFileStream;
Begin

  If CurFile^.AsmStrs.Count = 0 Then Exit;
  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);
  Z80Assembler.Assemble(CurFile^.AsmStrs, '', -1, '');
  If Z80Assembler.NumErrors > 0 Then Begin
     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, 1, Filename);
  End Else Begin

     Filename := OpenFile(Handle, 'Save Binary File', [FTAssembly], '', True, False);
     If Filename = '' Then Exit;

     If FileExists(Filename) Then DeleteFile(Filename);
     If FileExists(Filename) Then
        MessageBox(Handle, pChar('Could not overwrite your file '#39+ExtractFilename(Filename)+#39+'.'), pChar('Save Error'), MB_ICONWARNING or MB_OK);

     FStream := TFilestream.create(Filename, fmCreate or fmShareDenyNone);
     FStream.Write(AsmMemory[Z80Assembler.DefaultPage.AltLo], Z80Assembler.DefaultPage.AltHi - Z80Assembler.DefaultPage.AltLo);
     FStream.Free;

  End;
  Z80Assembler.Free;

End;

procedure TASMEditorWindow.AssembleTapeWithPasmo;
var
  SrcFile, OutFile, Output: AnsiString;
  OrgAdr: Word;

  FileSize: Integer;
begin
  OrgAdr := GetOrg;
  if OrgAdr < 0 then
  begin
    MessageBox(Handle, PChar('Please specify an ORG <adress> as start address.'), PChar('Assembly Error'), MB_OK or MB_ICONWARNING);
    Exit;
  end;

  SaveToTempFile(TabSet1.TabIndex, SrcFile);
  OutFile := ChangeFileExt(SrcFile, '.bin');

  if RunPasmo(SrcFile, OutFile, Output) then
  begin
    if Pos('ERROR', UpperCase(Output)) > 0 then
    begin
      MessageBox(Handle, PChar(Output), 'Pasmo Error', MB_OK or MB_ICONERROR);
      Exit;
    end
    else if Pos('PASS 2 FINISHED', UpperCase(Output)) > 0 then
    begin
      if not FileExists(OutFile) then
      begin
        MessageBox(Handle, 'Pasmo output file not found.', 'Error', MB_OK or MB_ICONERROR);
        Exit;
      end;

      // Dosya boyunu al
      FileSize := FileSizeByName(OutFile);
      if FileSize <= 0 then
      begin
        MessageBox(Handle, 'Output file is empty.', 'Error', MB_OK or MB_ICONERROR);
        Exit;
      end;

      // Belleðe yükle
      SetLength(FileArray,FileSize);
      if not LoadBinToMemory(OutFile, FileArray, 0) then
      begin
        MessageBox(Handle, 'Could not load binary into memory.', 'Error', MB_OK or MB_ICONERROR);
        Exit;
      end;

      // TAPE blok oluþtur
      TapeBlockAdd(CODEToTape('PasmoOut', OrgAdr));
      TapeWindow.UpdateTapeList;
      ShowWindow(TapeWindow, False);
    end;
  end
  else
    MessageBox(Handle, 'Pasmo execution failed.', 'Error', MB_OK or MB_ICONERROR);
end;



Procedure TASMEditorWindow.AssembleToTape;
Var
  Z80Assembler: TZ80Assembler;
  Line, ErrorType: Integer;
  Filename, Text: String;
  AsmMemory: Array[0..65535] Of Byte;
Begin

  If CurFile^.AsmStrs.Count = 0 Then Exit;
  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);
  Z80Assembler.Assemble(CurFile^.AsmStrs, '', -1, '');
  If Z80Assembler.NumErrors > 0 Then Begin
     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, 1, Filename);
  End Else Begin

     SetLength(FileArray, Z80Assembler.DefaultPage.AltHi - Z80Assembler.DefaultPage.AltLo);
     CopyMemory(@FileArray[0], @AsmMemory[Z80Assembler.DefaultPage.AltLo], Length(FileArray));
     TapeBlockAdd(CODEToTape('Assembly', Z80Assembler.DefaultPage.AltLo));
     TapeWindow.UpdateTapeList;
     ShowWindow(TapeWindow, False);

  End;
  Z80Assembler.Free;

End;

Procedure TASMEditorWindow.AssembleAsDATA;
Var
  Z80Assembler: TZ80Assembler;
  NewCode: TStringlist;
  Line, ErrorType, CurLineNum, Idx, Idx2: Integer;
  Filename, Text: String;
Label
  GetValues;
Begin

  If CurFile^.AsmStrs.Count = 0 Then Exit;

  NumPasses := 0;
  NumBytes := 0;
  ExpectedAddr := 0;

  Z80Assembler := TZ80Assembler.Create;
  Z80Assembler.atStartOfPass := AtStartOfPass;
  Z80Assembler.atEndOfPass := AtEndOfPass;
  Z80Assembler.AfterLine := AfterLineProc;
  Z80Assembler.SetMem48(@AsmMemory[16384]);
  AddSourceFiles(Z80Assembler, TabSet1.TabIndex);

  Z80Assembler.Assemble(CurFile^.AsmStrs, '', -1, '');

  If Z80Assembler.NumErrors > 0 Then Begin

     Z80Assembler.GetError(0, Line, Filename, ErrorType, Text);
     MessageBox(Handle, pChar('Assembly failed at line '+IntToStr(Line)+' with error:'#13#13+Text), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     CursorToLine(Line, 1, Filename);
     Z80Assembler.Free;

  End Else Begin

  GetValues:
      {
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
           If NumBytes >= AssembleForm.BytesPerLine Then Begin
              If Text[Length(Text)] = ',' Then
                 Text := Copy(Text, 1, Length(Text) -1);
              NewCode.Add(Text);
              Inc(CurLineNum, AssembleForm.Step);
              If Idx2 < Length(DATALines[Idx]) Then Begin
                 Text := IntToStr(CurLineNum)+' DATA ';
                 NumBytes := 0;
              End Else
                 Text := '';
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
      }

     ConvertBinToData(@AsmMemory[16384], NumBytes);
     Z80Assembler.Free;

  End;

End;

procedure TASMEditorWindow.AssembleWithPasmoAsDATA;
var
  SrcFile, OutFile, Output: AnsiString;
  OrgAdr: Word;
  NumBytes: Integer;
begin

  OrgAdr:=GetOrg;
  If OrgAdr < 0 Then Begin
     MessageBox(Handle, pChar('Please specify an ORG <adress> as start address.'), pChar('Assembly Error'), MB_OK or MB_ICONWARNING);
     Exit;
  End;

  SaveToTempFile(TabSet1.TabIndex, SrcFile);
  OutFile := ChangeFileExt(SrcFile, '.bin');

  if RunPasmo(SrcFile, OutFile, Output) then
  begin
    if Pos('ERROR', UpperCase(Output)) > 0 then
      MessageBox(Handle, PChar(Output), 'Pasmo Error', MB_OK or MB_ICONERROR)
    else if Pos('PASS 2 FINISHED', UpperCase(Output)) > 0 then
    begin
      NumBytes := FileSizeByName(OutFile);
      if LoadBinToMemory(OutFile, AsmMemory, OrgAdr) then
      Begin
        FillDATALinesSimple(AsmMemory, OrgAdr, NumBytes);
        ConvertBinToData(@AsmMemory[OrgAdr], NumBytes);
      End else
        MessageBox(Handle, 'Output too big for memory.', 'Overflow', MB_OK or MB_ICONWARNING);
    end;
  end;
end;


procedure TASMEditorWindow.AssembleBinaryWithPasmo;
var
  SrcFile, OutFile, Output: AnsiString;
  OrgAdr: Word;
  NumBytes: Integer;
begin
  OrgAdr := GetOrg;
  if OrgAdr < 0 then
  begin
    MessageBox(Handle, PChar('Please specify an ORG <adress> as start address.'), PChar('Assembly Error'), MB_OK or MB_ICONWARNING);
    Exit;
  end;

  SaveToTempFile(TabSet1.TabIndex, SrcFile);
  OutFile := ChangeFileExt(SrcFile, '.bin');

  if RunPasmo(SrcFile, OutFile, Output) then
  begin
    if Pos('ERROR', UpperCase(Output)) > 0 then
    begin
      MessageBox(Handle, PChar(Output), 'Pasmo Error', MB_OK or MB_ICONERROR);
      Exit;
    end
    else if Pos('PASS 2 FINISHED', UpperCase(Output)) > 0 then
    begin
      NumBytes := FileSizeByName(OutFile);
      Filename := OpenFile(Handle, 'Save Binary File', [FTAssembly], '', True, False);
      if Filename = '' then Exit;

      if FileExists(Filename) then DeleteFile(Filename);
      if FileExists(Filename) then
      begin
        MessageBox(Handle, PChar('Could not overwrite your file '#39 + ExtractFilename(Filename) + #39 + '.'), PChar('Save Error'), MB_ICONWARNING or MB_OK);
        Exit;
      end;

      // Eksik olan kopyalama iþlemi:
      if not CopyFile(PChar(OutFile), PChar(Filename), False) then
        MessageBox(Handle, 'Could not save binary output.', 'Save Error', MB_OK or MB_ICONERROR)
      else
        MessageBox(Handle, 'Binary saved successfully.', 'Success', MB_OK or MB_ICONINFORMATION);
    end;
  end
  else
    MessageBox(Handle, 'Pasmo execution failed.', 'Error', MB_OK or MB_ICONERROR);
end;



procedure TASMEditorWindow.FillDATALinesSimple(const Mem: array of Byte; StartAddr, Size: Integer);
var
  S: string;
  i: Integer;
begin
  DATALines.Clear;
  S := IntToStr(StartAddr) + ',';
  for i := 0 to Size - 1 do
    S := S + Chr(Mem[StartAddr + i]);
  DATALines.Add(S);
end;


procedure TASMEditorWindow.ConvertBinToData(const MemPtr: PByte; Size: Integer);
var
  CurLineNum, Idx, Idx2, NumBytes: Integer;
  Text: string;
  NewCode: TStringList;
Label
  GetValues;
begin
GetValues:
  CentreFormOnForm(AssembleForm, Self);
  ShowWindow(AssembleForm, True);
  if AssembleForm.Cancelled then Exit;

  if AssembleForm.StartAt + ((Size div AssembleForm.BytesPerLine) * AssembleForm.Step) > 9999 then
  begin
    MessageBox(Handle, 'Your Line numbering parameters will result in a line number greater than 9999.'#13#13'Please review your settings.', 'Could not generate BASIC', MB_OK or MB_ICONWARNING);
    goto GetValues;
  end;

  CurLineNum := AssembleForm.StartAt;
  NewCode := TStringList.Create;
  try
    if AssembleForm.CheckBox2.Checked then
    begin
      if DATALines.Count > 1 then
        NewCode.Add(IntToStr(CurLineNum) + ' RESTORE ' + IntToStr(CurLineNum + AssembleForm.Step) +
          ': FOR A=1 TO ' + IntToStr(DATALines.Count) +
          ': READ B,C: FOR D=B TO B+C-1: READ C: POKE D,C: NEXT D: NEXT A')
      else
        NewCode.Add(IntToStr(CurLineNum) + ' RESTORE ' + IntToStr(CurLineNum + AssembleForm.Step) +
          ': READ A,B: FOR C=A TO A+B-1: READ B: POKE C,B: NEXT C');
      Inc(CurLineNum, AssembleForm.Step);
    end;

    for Idx := 0 to DATALines.Count - 1 do
    begin
      Text := IntToStr(CurLineNum) + ' DATA ' + Copy(DATALines[Idx], 1, Pos(',', DATALines[Idx]));
      DATALines[Idx] := Copy(DATALines[Idx], Pos(',', DATALines[Idx]) + 1, MaxInt);
      Text := Text + IntToStr(Length(DATALines[Idx])) + ',';

      NumBytes := 0;
      for Idx2 := 1 to Length(DATALines[Idx]) do
      begin
        Text := Text + IntToStr(Ord(DATALines[Idx][Idx2])) + ',';
        Inc(NumBytes);
        if NumBytes >= AssembleForm.BytesPerLine then
        begin
          if Text[Length(Text)] = ',' then
            Text := Copy(Text, 1, Length(Text) - 1);
          NewCode.Add(Text);
          Inc(CurLineNum, AssembleForm.Step);
          if Idx2 < Length(DATALines[Idx]) then
            Text := IntToStr(CurLineNum) + ' DATA '
          else
            Text := '';
          NumBytes := 0;
        end;
      end;

      if Text <> '' then
      begin
        if Text[Length(Text)] = ',' then
          Text := Copy(Text, 1, Length(Text) - 1);
        NewCode.Add(Text);
        Inc(CurLineNum, AssembleForm.Step);
      end;
    end;

    AddCodeWindow.ClearCode;
    AddCodeWindow.AddCode(NewCode);
    CentreFormOnForm(AddCodeWindow, Self);
    ShowWindow(AddCodeWindow, True);
  finally
    NewCode.Free;
  end;
end;



Procedure TASMEditorWindow.AddSourceFiles(var Z80Assembler: TZ80Assembler; Exclude: Integer);
Var
  Idx: Integer;
Begin

  For Idx := 0 To FileCount -1 Do Begin
     If Idx <> Exclude Then Begin
        Z80Assembler.AddSource(AsmFiles[Idx]^.AsmStrs, ASmFiles[Idx]^.AsmFilename);
     End;
  End;

End;

Procedure TASMEditorWindow.AtStartofPass(Sender: TObject);
Begin
  Inc(NumPasses);
  DATAlines.Clear;
  NumBytes := 0;
End;

Procedure TASMEditorWindow.AtEndOfPass(Sender: TObject);
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

  If AssembledBytes <> '' Then
     If DATALines.Count <> 0 Then
        DATALines[DATALines.Count -1] := DATALines[DATALines.Count -1] + AssembledBytes
     Else
        DATALines.Add(AssembledBytes);

End;

Procedure TASMEditorWindow.CursorToLine(LineNum, Column: Integer; Filename: String);
Var
  Idx: Integer;
  cPoint: TPoint;
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

  cPoint.x := Column;
  cPoint.y := LineNum;
  UpdateCursorPos(cPoint, False);
  CurFile^.CursorLine := cPoint.y;
  CurFile^.CursorColumn := cPoint.x;
  If (cPoint.y < CurFile^.ViewLine) or (cPoint.y > CurFile^.ViewLine + (FastIMG1.Bmp.AbsHeight Div 8)) Then
     CurFile^.ViewLine := Max(0, cPoint.y - ((FastIMG1.Bmp.AbsHeight Div 8) Shr 1));

  RepaintASM(True);

End;

Function TASMEditorWindow.FindNextForward(Term: String; Pos: TPoint; MatchCase: Boolean): TFindResult;
Var
  Done: Boolean;
Begin
  If Not MatchCase Then Term := Uppercase(Term);
  Done := False;
  While Not Done Do Begin
     If CurFile^.AsmStrs[Pos.y -1] <> '' Then
        If Not MatchCase Then Begin
           If UpperCase(CurFile^.AsmStrs[Pos.y -1][Pos.x]) = Term[1] Then
              If Uppercase(Copy(CurFile^.AsmStrs[Pos.y -1], Pos.x, Length(Term))) = Term Then
                 Done := True;
        End Else
           If CurFile^.AsmStrs[Pos.y -1][Pos.x] = Term[1] Then
              If Copy(CurFile^.AsmStrs[Pos.y -1], Pos.x, Length(Term)) = Term Then
                 Done := True;
     Inc(Pos.x);
     If Pos.x >= Length(CurFile^.AsmStrs[Pos.y -1])+1 Then Begin
        Pos.x := 1;
        Inc(Pos.y);
        If Pos.y = CurFile^.AsmStrs.Count +1 Then
           Break;
     End;
  End;

  If Done Then Begin
     Result.LineNum := Pos.y;
     Result.Position := Pos.x -1;
     If Visible Then Edit2.SetFocus;
  End Else Begin
     Result.LineNum := 0;
     Result.Position := 0;
  End;

End;

Function TASMEditorWindow.FindNextBackward(Term: String; Pos: TPoint; MatchCase: Boolean): TFindResult;
Var
  Done: Boolean;
Begin

  Dec(Pos.x);
  If Pos.x = 0 Then
     If Pos.y > 1 Then Begin
        Dec(Pos.y);
        Pos.x := Length(CurFile^.AsmStrs[Pos.y -1]);
     End Else Begin
        Result.Position := 0;
        Exit;
     End;

  If Not MatchCase Then Term := Uppercase(Term);
  Done := False;
  While Not Done Do Begin
     If CurFile^.AsmStrs[Pos.y -1] <> '' Then
        If Not MatchCase Then Begin
           If UpperCase(CurFile^.AsmStrs[Pos.y -1][Pos.x]) = Term[1] Then
              If Uppercase(Copy(CurFile^.AsmStrs[Pos.y -1], Pos.x, Length(Term))) = Term Then
                 Done := True;
        End Else
           If CurFile^.AsmStrs[Pos.y -1][Pos.x] = Term[1] Then
              If Copy(CurFile^.AsmStrs[Pos.y -1], Pos.x, Length(Term)) = Term Then
                 Done := True;
     Dec(Pos.x);
     If Pos.x <= 0 Then Begin
        Dec(Pos.y);
        If Pos.y = 0 Then
           Break;
        Pos.x := Length(CurFile^.AsmStrs[Pos.y -1]);
     End;
  End;

  If Done Then Begin
     Result.LineNum := Pos.y;
     Result.Position := Pos.x +1;
  End Else
     Result.Position := 0;

  If Visible Then Edit2.SetFocus;

End;

procedure TASMEditorWindow.TabSet1Change(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin

  If FileCount = 0 Then Exit;
  AllowChange := True;
  CurFile := AsmFiles[NewTab];
  RepaintASM(True);
  BuildLabelList;
  FormResize(Nil);
  If Visible Then Edit2.SetFocus;

end;

Procedure TASMEditorWindow.CloseFile;
Var
  Idx, Index: Integer;
Begin

  Index := TabSet1.TabIndex;
  CheckForSave(Index);

  For Idx := 0 To AsmFiles[Index]^.AsmStrs.Count -1 Do
     AsmFiles[Index]^.AsmStrs.Objects[Idx].Free;

  AsmFiles[Index]^.Labels.Free;
  AsmFiles[Index]^.AssembledLabels.Free;
  AsmFiles[Index]^.types.Free;
  AsmFiles[Index]^.AsmStrs.Free;
  AsmFiles[Index]^.AsmStrs := Nil;
  Dispose(AsmFiles[Index]);

  If Index < FileCount -1 Then Begin
     For Idx := Index To FileCount -2 Do
        AsmFiles[Idx] := AsmFiles[Idx +1];
     SetLength(AsmFiles, FileCount -1);
     Dec(FileCount);
     TabSet1.Tabs.Delete(Index);
  End Else Begin
     Dec(Index);
     SetLength(AsmFiles, FileCount -1);
     Dec(FileCount);
     TabSet1.Tabs.Delete(Index +1);
  End;

  If TabSet1.Tabs.Count = 0 Then Begin
     CurFile := nil;
     FastIMG1.Visible := False;
     ListBox1.Visible := False;
     Caption := 'z80 Assembler - No file in memory';
     SetScrollBoxBounds;
  End Else
     CurFile := AsmFiles[TabSet1.TabIndex];

  BuildLabelList;
  RepaintASM(True);
  FormResize(Nil);
  If Visible Then Edit2.SetFocus;

End;

Function TASMEditorWindow.ClearFiles: Boolean;
Var
  Idx, Idx2: Integer;
Begin

  Result := False;

  For Idx := 0 To FileCount -1 Do
     If Not CheckForSave(Idx) then
        Exit;

  Result := True;

  For Idx := 0 To FileCount -1 Do Begin
     For Idx2 := 0 To AsmFiles[Idx]^.AsmStrs.Count -1 Do
        AsmFiles[Idx]^.AsmStrs.Objects[Idx2].Free;
     AsmFiles[Idx]^.Labels.Free;
     AsmFiles[Idx]^.AssembledLabels.Free;
     AsmFiles[Idx]^.Types.Free;
     AsmFiles[Idx]^.AsmStrs.Free;
     AsmFiles[Idx]^.AsmStrs := Nil;
     Dispose(AsmFiles[Idx]);
  End;

  FileCount := 0;

End;

Procedure TASMEditorWindow.UpdateCaption;
Begin
  If CurFile^.Changed Then
     If Copy(Caption, Length(Caption), 1) <> '*' Then
        Caption := Caption + '*';
End;

Function TASMEditorWindow.HasContent(Index: Integer): Boolean;
Var
  Idx, Idx2: Integer;
Begin

  Result := False;

  Idx := 0;
  While Idx < AsmFiles[Index]^.AsmStrs.Count Do Begin
     Idx2 := 1;
     While Idx2 < Length(AsmFiles[Index]^.AsmStrs[Idx]) Do Begin
        If AsmFiles[Index]^.AsmStrs[Idx][Idx2] > ' ' Then Begin
           Result := True;
           Exit;
        End;
        Inc(Idx2);
     End;
     Inc(Idx);
  End;

End;

procedure TASMEditorWindow.Splitter1Moved(Sender: TObject);
begin
  ListBox1.Width := Splitter1.Left - Splitter1.Width;
  SetScrollBoxBounds;
  FormResize(nil);
end;

Procedure TASMEditorWindow.BuildLabelList;
Var
  Idx: Integer;
Begin

  ListBox1.Items.BeginUpdate;
  ListBox1.Tag := ListBox1.ItemIndex;
  ListBox1.Items.Clear;

  If CurFile <> Nil Then Begin

     CurFile^.Labels.CustomSort(SortByLineNum);

     For Idx := 0 To CurFile^.Labels.Count -1 Do Begin

        If CurFile^.Labels[Idx][1] = Chr(Integer(tTLabel)) Then Begin
           ListBox1.Items.Add(Copy(CurFile^.Labels[Idx], 2, 99999));
           ListBox1.Items.Objects[ListBox1.Items.Count -1] := CurFile^.Labels.Objects[Idx];
        End;

     End;

     ListBox1.ItemIndex := ListBox1.Tag;

  End;

  ListBox1.Items.EndUpdate;

End;

Function SortByLineNum(List: TStringlist; Index1, Index2: Integer): Integer;
Begin

  If Integer(List.Objects[Index1]) < Integer(List.Objects[Index2]) Then
     Result := -1
  Else
     If Integer(List.Objects[Index1]) > Integer(List.Objects[Index2]) Then
        Result := 1
     Else
        Result := 0;

End;

procedure TASMEditorWindow.ListBox1DblClick(Sender: TObject);
begin
  CursorToLine(Integer(ListBox1.Items.Objects[ListBox1.ItemIndex]), 1, '');
  Edit2.SetFocus;
end;

procedure TASMEditorWindow.OnEnterMenuLoop(var Message: TMessage);
Begin

  If CurFile <> nil Then Begin

     Cut1.Enabled := Not ((CurFile^.EditorSelStart.Y = CurFile^.EditorSelEnd.Y) and (CurFile^.EditorSelStart.X = CurFile^.EditorSelEnd.X));

     Copy1.Enabled := Cut1.Enabled;
     Paste1.Enabled := (ClipBoard.HasFormat(CF_TEXT)) and (ClipBoard.AsText <> '');

     Find1.Enabled := HasContent(TabSet1.TabIndex);
     FindNext1.Enabled := Find1.Enabled;
     Replace1.Enabled := Find1.Enabled;

     Close1.Enabled := True;
     Save1.Enabled := CurFile^.Changed;
     SaveAs1.Enabled := True;
     Assemble1.Enabled := HasContent(TabSet1.TabIndex);

     LabelList1.Enabled := True;

  End Else Begin

     Cut1.Enabled := False;
     Copy1.Enabled := False;
     Paste1.Enabled := False;
     Find1.Enabled := False;
     FindNext1.Enabled := False;
     Replace1.Enabled := False;
     Close1.Enabled := False;

     Save1.Enabled := False;
     SaveAs1.Enabled := False;
     Assemble1.Enabled := False;

     LabelList1.Enabled := False;

  End;

  StatusBar1.Checked := StatusBar2.Visible;
  LabelList1.Checked := ListBox1.Visible;

End;

procedure TASMEditorWindow.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;

procedure TASMEditorWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 timer1.Enabled:=false;
end;

Initialization

  DataLines := TStringlist.Create;
  AllLabels := TStringlist.Create;

Finalization

  DataLines.Free;
  AllLabels.Free;

end.


// editor stuff to complete:

  // Syntax highlighting options
  // Click a label/Equate -> jump to it
  // Source markers
  // MRU List


