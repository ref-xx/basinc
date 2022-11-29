unit BASIC;

interface

Uses Classes, SysUtils, Windows;

Procedure BinaryToBASIC(Binary: String; var List: TStringlist; Is128k: Boolean);
Function  GetWord(WordPtr: Pointer): Word;
Function  DeTokeniseLine(Line: String; DetokeniseStrings, ProgramIs128k: Boolean): String;

Const

  SpaceKeywords: Array[0..93] of String = (  'SPECTRUM ', 'PLAY ', 'RND', 'INKEY$', 'PI', 'FN ', 'POINT ', 'SCREEN$ ', 'ATTR ',
                                             'AT ', 'TAB ', 'VAL$ ', 'CODE ', 'VAL ', 'LEN ', 'SIN ', 'COS ',
					     'TAN ', 'ASN ', 'ACS ', 'ATN ', 'LN ', 'EXP ', 'INT ', 'SQR ',
					     'SGN ', 'ABS ', 'PEEK ', 'IN ', 'USR ', 'STR$ ','CHR$ ', 'NOT ',
					     'BIN ', 'OR ', 'AND ', '<=', '>=', '<>', 'LINE ', 'THEN ',
					     'TO ', 'STEP ', 'DEF FN ', 'CAT ', 'FORMAT ', 'MOVE ', 'ERASE ',
					     'OPEN #', ' CLOSE #', 'MERGE ', 'VERIFY ', 'BEEP ', 'CIRCLE ',
					     'INK ', 'PAPER ', 'FLASH ', 'BRIGHT ', 'INVERSE ', 'OVER ',
					     'OUT ', 'LPRINT ', 'LLIST ', 'STOP ', 'READ ', 'DATA ', 'RESTORE ',
					     'NEW ', 'BORDER ', 'CONTINUE ', 'DIM ', 'REM ', 'FOR ', 'GO TO ',
					     'GO SUB ', 'INPUT ', 'LOAD ', 'LIST ', 'LET ', 'PAUSE ', 'NEXT ',
                                             'POKE ', 'PRINT ', 'PLOT ', 'RUN ', 'SAVE ', 'RANDOMIZE ', 'IF ',
                                             'CLS ', 'DRAW ', 'CLEAR ', 'RETURN ', 'COPY ', 'DUMMY');

implementation

Procedure BinaryToBASIC(Binary: String; var List: TStringlist; Is128k: Boolean);
Var
  TempWord, CurAddress, Count: DWord;
  BASICLine, BASICStr, TempStr: String;
Begin

  List.Clear;
  CurAddress := 1;

  Repeat

     If Ord(Binary[CurAddress]) >= 128 Then Break;

     BASICLine := '';
     TempWord := GetWord(@Binary[CurAddress]);
     TempStr := IntToStr((TempWord Shr 8) + ((TempWord and 255) Shl 8));

     If StrToInt(TempStr) > 16383 Then Exit;

     BASICLine := BASICLine + TempStr +' ';

     Inc(CurAddress, 2);
     TempWord := GetWord(@Binary[CurAddress]);

     Inc(CurAddress, 2);
     TempStr := '';
     For Count := 1 To TempWord -1 Do
        TempStr := TempStr + Binary[CurAddress+Count -1];

     Inc(CurAddress, TempWord);

     BASICStr := '';
     BASICStr := DetokeniseLine(TempStr, False, Is128k);
     List.Add(BASICLine + BASICStr);

  Until CurAddress >= DWord(Length(Binary));

End;

Function GetWord(WordPtr: Pointer): Word;
Asm
  mov ax, [eax]
End;

Function DeTokeniseLine(Line: String; DetokeniseStrings, ProgramIs128k: Boolean): String;
Var
	Token,
  LastToken,
	TempLine: String;
	CurChar: Byte;
	Index: Integer;
  InString, REMCommand: Boolean;
Begin
	TempLine := Line;
	While Copy(TempLine, 1, 1) = ' ' Do TempLine := Copy(TempLine, 2, 999999);
	Index := 1;
  LastToken := '';
  InString := False;
  REMCommand := False;
	While Index < Length(TempLine)+1 Do Begin
		CurChar := Ord(TempLine[Index]);
     If CurChar = 34 Then InString := Not InString;
     If Not InString and (CurChar = 14) and Not REMCommand Then Begin
        Inc(Index, 5);
     End Else	If (((Not InString) or DetokeniseStrings) or (REMCommand)) and (CurChar > $A2) Then Begin
        If Not REMCommand Then Begin
			   Token := SpaceKeyWords[CurChar - $A3];
			   If Token[1] in ['A'..'Z'] Then Begin
				   If (CurChar > $C4) or ((CurChar in [$A3, $A4]) and ProgramIs128k) Then
					   If Result <> '' Then
                    If Copy(LastToken, Length(LastToken), 1) <> ' ' Then
                       Token := ' '+Token;
			   End;
        End Else
           Token := Chr(CurChar);
			Result := Result + Token;
        LastToken := Token;
        If CurChar = $EA Then
           REMCommand := True;
		End Else Begin
			Result := Result + TempLine[Index];
        LastToken := '';
		End;
		Inc(Index);
	End;
	While Copy(Result, Length(Result), 1) = ' ' Do
		Result := Copy(Result, 1, Length(Result) -1);
End;



end.
