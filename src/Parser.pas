// ********************************* BASIC Parser *********************************
// *        This unit contains the routines to parse a text line of BASIC.        *
// * ParseInputLine will determine what type of line you entered, and then parse  *
// *   accordingly. The return is a structure containing any errors and syntax    *
// * strings. Syntax strings are interspersed with spectrum colour control chars, *
// *                       and a $10+$02 denotes an error.                        *
// ********************************************************************************

unit Parser;

Interface

Uses SysUtils, Classes, Graphics, Windows, Math;

Type

  TParseError =  Record Error, Syntax, Line: AnsiString; ErrorCode, Position, PositionEnd, Statement, Linenum: Integer; NowTyping: Set Of 0..255; End;
  TParseItem =   Record ItemType, Position: Integer; End;
  TParseStack =  Class
     Items:      Array Of TParseItem;
     NumItems:   Integer;
     NilParseItem: TParseItem;
     Constructor Create;
     Procedure   Push(Item: TParseItem); Overload;
     Procedure   Push(Item, Position: Integer); Overload;
     Function    Pop: TParseItem;
     Function    PopTop: TParseItem;
     Function    GetItemType(Index: Integer): Integer;
     Function    GetItem(Index: Integer): TParseItem;
     Procedure   SetItem(Index: Integer; Item: TParseItem);
     Procedure   Delete(Index: Integer);
     Procedure   DeleteItems(Index, Count: Integer);
  End;

  TThingSet = Set Of 0..255;

  Procedure InitParser;

  Function  BeautifyBASIC(BASIC: AnsiString): AnsiString;
  Function  SplitStatements(BASIC: AnsiString): TStringlist;
  Function  BuildParseStack(Line: AnsiString): TParseStack;
  Function  IsReserved(Token: AnsiString): Integer;
  Function  ContainsReserved(Token: AnsiString): Integer;
  Function  IndexIsReserved(Token: AnsiString): Integer;
  Function  IsKeyword(Token: AnsiString): Integer;
  Function  RemoveSpaces(Word: AnsiString): AnsiString;
  Function  WhatIs(TokenType: Integer): AnsiString;

  Function  ParseExprLine(Line: AnsiString): TParseError;
  Function  ParseInputLine(Line: AnsiString): TParseError;
  Function  GetLineType(Text: AnsiString): Integer;
  Function  ParseCodeLine(Line: AnsiString; Storable: Boolean): TParseError;
  Function  GetExprType(Var Start: Integer; PS: TParseStack; Pri, Expected: Integer; ModGlb: Boolean): Integer;
  Function  ParseNumSubs(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseStrSubs(Var Start: Integer; PS: TParseStack; VarType: Integer): Integer;
  Function  ParseKeyword(Keyword: Integer; Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseGenericType(Var Start: Integer; Key: AnsiString; PS: TParseStack; Pri: Integer; ExpectList: Array of Integer; SyntaxList: Array Of AnsiString; Return: Integer): Integer;
  Function  ParseCAT(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseERASE(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseFN(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseGFXFuncs(Var Start: Integer; Key: AnsiString; Params: Array Of AnsiString; PS: TParseStack; NumTerms: Integer): Integer;
  Function  ParseCOPY(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseDATAREAD(Var Start: Integer; PS: TParseStack; Read: Boolean): Integer;
  Function  ParseDIM(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseLIST(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseFORMAT(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseLET(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParsePRINTINPUT(Var Start: Integer; PS: TParseStack; INPUT: Boolean): Integer;
  Function  ParseDEFFN(Var Start: Integer; PS: TParseStack): Integer;
  Function  ParseLOADSAVE(Var Start: Integer; PS: TParseStack; SAVE: Boolean): Integer;
  Function  ParseMERGE(Var Start: Integer; PS: TParseStack): Integer;

  Function  HighlightReserved(Text: AnsiString; HighlightSymbols: Boolean): AnsiString;
  Procedure ColorTextOut(Canvas: TCanvas; Text: AnsiString; X, Y: Integer);
  Function  ParseStackToStringlist(Stack: TParseStack): TStringlist;
  Function  ParseStringList(List: TStringlist): TParseError;
  Function  StripColours(Text: AnsiString): AnsiString;
  Function  GetContents(Thing: TThingSet): AnsiString;

Const

  Types: Array[0..32] of AnsiString =
  ('SIUnknown', 'SINumLiteral', 'SIStrLiteral', 'SINumVar', 'SINumVarSL', 'SIStrVar', 'SIStrVarSL', 'SIEol',
   'SISymbol', 'SITextItem', 'SILessThan', 'SIMoreThan', 'SILessThanEq', 'SIMoreThanEq', 'SINotEqual',
   'SIBinNumber', 'SINumExpr', 'SIStrExpr', 'SIFloat', 'SIPrintItem', 'SIColourItem', 'SIPrintSep',
   'SIStatement', 'SIUnterminatedStr', 'SIComment', 'SINumVarSubs', 'SIStrVarSubs', 'SIOptional', 'SIAnyVar',
   'SIAnyExpr', 'SIStrVarSliced', 'SIAnyVarNS', 'SIAnyVarSL');

  Keywords: Array[1..90] of AnsiString =
  // Keywords - can begin statements
  ('BEEP-1001', 'BORDER-1002', 'CAT-1003', 'CIRCLE-1004', 'CLEAR-1005', 'CLOSE-1006', 'CLS-1007',  'CONTINUE-1008', 'COPY-1009', 'DATA-1010',
   'DEF FN-1011', 'DIM-1012', 'DRAW-1013', 'ERASE-1014', 'FOR-1015', 'FORMAT-1016', 'GO SUB-1017', 'GO TO-1018', 'IF-1019', 'INPUT-1020',
   'LET-1021', 'LIST-1022', 'LLIST-1023', 'LOAD-1024', 'LPRINT-1025', 'MERGE-1026', 'MOVE-1027', 'NEW-1028', 'NEXT-1029', 'OPEN-1030',
   'OUT-1031', 'PAUSE-1032', 'PLAY-1033', 'PLOT-1034', 'POKE-1035', 'PRINT-1036', 'RANDOMIZE-1037', 'READ-1038', 'REM-1039', 'RESTORE-1040',
   'RETURN-1041', 'RUN-1042', 'SAVE-1043', 'SPECTRUM-1044', 'STOP-1045', 'VERIFY-1046',

  // Meta-Keywords - can begin statements and be used inside statements
   'BRIGHT-1047', 'FLASH-1048', 'INK-1049', 'INVERSE-1050', 'OVER-1051', 'PAPER-1052',

  // Non-function, non-statement keywords
   'TO-1053', 'STEP-1054', 'LINE-1055', 'THEN-1056', 'AT-1057', 'TAB-1058',

  // Functions
   'ABS-1059', 'ACS-1060', 'AND-1061',
   'ASN-1062', 'ATN-1063', 'ATTR-1064', 'BIN-1065', 'CHR$-1066', 'CODE-1067', 'COS-1068', 'EXP-1069', 'FN-1070', 'IN-1071',
   'INKEY$-1072', 'INT-1073', 'LEN-1074', 'LN-1075', 'NOT-1076', 'OR-1077', 'PEEK-1078', 'PI-1079', 'POINT-1080', 'RND-1081',
   'SCREEN$-1082', 'SGN-1083', 'SIN-1084', 'SQR-1085', 'STR$-1086', 'TAN-1087', 'USR-1088', 'VAL-1089', 'VAL$-1090');

   SIUnknown = 0; SINumLiteral = 1; SIStrLiteral = 2; SINumVar = 3; SINumVarSL = 4; SIStrVar = 5;
   SIStrVarSL = 6; SIEol = 7; SISymbol = 8; SITextItem = 9; SILessThan = 10; SIMoreThan = 11;
   SILessThanEq = 12; SIMoreThanEq = 13; SINotEqual = 14; SIBinNumber = 15; SINumExpr = 16; SIStrExpr = 17;
   SIFloat = 18; SIPrintItem = 19; SIColourItem = 20; SIPrintSep = 21; SIStatement = 22; SIUnterminated = 23;
   SIComment = 24; SINumVarSubs = 25; SIStrVarSubs = 26; SIOptional = 27; SIAnyVar = 28; SIAnyExpr = 29;
   SIStrVarSliced = 30; SIAnyVarNS = 31; SIAdd = 43; SIMinus = 45; SIEquals = 61; SIOpenBrace = 40;
   SICloseBrace = 41; SIPower = 94; SIMul = 42; SIDiv = 47; SIComma = 44; SISemiColon = 59; SIColon = 58;
   SIApos = 39; SIPoint = 46; SIAnyVarSL = 32; SIColourOrNum = 33;

   AlphaNumerics:   AnsiString  = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
   Alphas:          AnsiString  = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
   Numerics:        AnsiString  = '0123456789';
   Symbols:         AnsiString  = '@$!#£%&_{}[]~\|?.()+-/*^"'#39':;,<>=';
   MathSymbols:     AnsiString  = '(-+';
   KeyWordCutOff:   Integer = 1053;
   FunctionCutoff:  Integer = 1059;

Var

  KeywordsHash: Array[0..25] of Integer;
  KeyWordNames: Array[1..91] of AnsiString;
  KeyWordsIDXTable: Array[1..91] of Integer;
  KeyWordsIDXNames: Array[1..91] of AnsiString;
  SyntaxTemplate, ErrorMsg: AnsiString;
  NumReserved, SI_TO, SI_AND, SI_OR, GlobalType, GlobalPos, GlobalReturn: Integer;
  ParserInitDone: Boolean;
  AllExprs, NumericTypes, VarTypes, StringTypes, NumVars, StrVars,
  PrintSeps, DefVarTypes, MathTypes, RelOps, ColourWords, Specials, NonStarters, GlobalExpect: Set Of 0..255;

Implementation

Constructor TParseStack.Create; Begin SetLength(Items, 0); NumItems := 0; NilParseItem.ItemType := -1; End;
Procedure   TParseStack.Push(Item: TParseItem); Begin If Item.ItemType = -1 Then Exit; SetLength(Items, NumItems +1); Items[NumItems] := Item; Inc(NumItems); End;
Procedure   TParseStack.Push(Item, Position: Integer); Var PI: TParseItem; Begin PI.ItemType := Item; PI.Position := Position; Push(PI); End;
Function    TParseStack.Pop: TParseItem; Begin If NumItems = 0 Then Result := NilParseItem Else Begin Dec(NumItems); Result := Items[NumItems]; Items := Copy(Items, 0, NumItems); End; End;
Function    TParseStack.PopTop: TParseItem; Begin If NumItems = 0 Then Result := NilParseItem Else Begin Dec(NumItems); Result := Items[0]; Items := Copy(Items, 1, NumItems); SetLength(Items, NumItems); End; End;
Function    TParseStack.GetItem(Index: Integer): TParseItem; Begin If (Index > NumItems -1) or (Index < 0) Then Result := NilParseItem Else Result := Items[Index]; End;
Function    TParseStack.GetItemType(Index: Integer): Integer; Begin If (Index > NumItems -1) or (Index < 0) Then Result := SIEol Else Result := Items[Index].ItemType; End;
Procedure   TParseStack.SetItem(Index: Integer; Item: TParseItem); Begin Items[Index] := Item; End;
Procedure   TParseStack.Delete(Index: Integer); Var F: Integer; Begin For F := Index To NumItems -2 Do Items[F] := Items[F+1]; Dec(NumItems); SetLength(Items, NumItems); End;
Procedure   TParseStack.DeleteItems(Index, Count: Integer); Var F: Integer; Begin For F := Index To NumItems -(Count +1) Do Items[F] := Items[F+Count]; Dec(NumItems, Count); SetLength(Items, NumItems); End;

Function CompareStrToSubStr(Const S1, S2: AnsiString; SubStart2, SubLen2: DWord): Boolean;
Var
  Idx1, StrLen1, StrLen2: DWord;
Begin
  Idx1 := 1;
  Result := False;
  StrLen1 := Length(S1);
  StrLen2 := Min(SubLen2, Length(S2));
  While Idx1 <= SubLen2 Do Begin
     If SubStart2 > StrLen2 Then Exit;
     If Idx1 > StrLen1 Then Exit;
     If S1[Idx1] <> S2[SubStart2] Then Exit;
     Inc(Idx1);
     Inc(SubStart2);
  End;
  Result := True;
End;

// ************************** Parse InputLine *****************************
// * This is the main road into the parser. Send a line of code here, and *
// *   Its type is checked (storable, direct or expression) and parsed.   *
// *        The resulting TParseError returns all you need to know.       *
// ************************************************************************

Function ParseInputLine(Line: AnsiString): TParseError;
Var
  TempType, F, Idx: Integer;
  DefiniteExpression: Boolean;
  NewLine: AnsiString;
Begin

  Idx := 0;
  NewLine := '';
  While Idx < Length(Line) Do Begin
     Inc(Idx);
     If Line[Idx] in [#32..#164] Then NewLine := NewLine + Line[Idx];
  End;
  Line := NewLine;

  Globaltype := SIUnknown;
  GlobalPos := 0;
  GlobalReturn := SIUnknown;
  ErrorMsg := '';
  Result.LineNum := 0; Result.Statement := 0;
  Result.Error := ''; Result.Line := Line;
  Result.Position := 0; Result.ErrorCode := 0;
  Result.Syntax := AnsiChar(16)+AnsiChar(1)+'LineNumber'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'Statement'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'Expression';
  While Copy(Line, 1, 1) = ' ' Do Line := Copy(Line, 2, 999999);
  TempType := GetLineType(Line);
  Case TempType Of
     0: Begin
           If Line <> '' then Begin
              Result.Error := 'Indeterminate line type';
              Result.ErrorCode := 99;
           End;
        End;
     1: Begin
           DefiniteExpression := False;
           Result := ParseExprLine(Line);
           For Idx := 1 To Length(Line) Do Begin
              If (Pos(Line[Idx], Symbols) <> 0) or (pos(Line[Idx], Numerics) <> 0) or (Line[idx] = '$') Then Begin
                 DefiniteExpression := True;
                 Break;
              End;
           End;
           If Not DefiniteExpression Then Begin
              Result := ParseCodeLine(Line, False);
              If Result.ErrorCode = -1 Then Begin
                 Result.Error := 'Expecting Keyword or Expression';
                 Result.ErrorCode := 98;
              End;
           End;
        End;
     2: Result := ParseCodeLine(Line, False);
     3: Result := ParseCodeLine(Line, True);
     4: Result.Syntax := 'LineNumber ['+AnsiChar(16)+AnsiChar(1)+'Statement'+AnsiChar(16)+AnsiChar(1)+']';
  End;
  If Temptype <> 3 Then Result.LineNum := 0;
  If Result.ErrorCode < 0 Then Begin
     For F := 1 to Length(Result.Syntax) Do Begin
        If Result.Syntax[F] = AnsiChar(1) Then Result.Syntax[F] := AnsiChar(2);
     End;
  End;
  If Result.ErrorCode = 0 Then Result.Error := 'Ok, ';
  Result.NowTyping := GlobalExpect;
End;

// *************************** Get Line Type ******************************
// * Determines what a line of text actually *is* - 0: Unknown, 1: Expr,  *
// *          2: Direct Command, 3: Storable Line, 4: Either 1 or 3       *
// ************************************************************************

Function GetLineType(Text: AnsiString): Integer;
Var
  TempStr: AnsiString;
  TempInt, F: Integer;
  SciFlag: Boolean;
Begin
  Result := 0;
  SciFlag := False;
  If Text = '' Then Exit;
  While Copy(Text, 1, 1) = ' ' Do Text := Copy(Text, 2, 999999);
  If Text = '' Then Exit;
  If Pos(Text[1], Alphas) > 0 Then Begin
     F := 1; TempStr := '';
     While Pos(Text[F], Alphas) > 0 Do Begin
        TempStr := TempStr + Text[F];
        Inc(F);
     End;
     If (Text[F] = ' ') and (UpperCase(TempStr) = 'GO') or (UpperCase(TempStr) = 'DEF') Then Begin
        TempStr := TempStr + ' ';
        Inc(F);
        While Pos(Text[F], Alphas) > 0 Do Begin
           TempStr := TempStr + Text[F];
           Inc(F);
        End;
     End;
     If UpperCase(TempStr) = 'GOSUB' Then TempStr := 'Go Sub';
     If UpperCase(TempStr) = 'GOTO' Then TempStr := 'Go To';
     If UpperCase(TempStr) = 'DEFFN' Then TempStr := 'Def Fn';
     If Text[F] = '$' Then
        TempStr := TempStr + '$';
     TempInt := IsReserved(UpperCase(TempStr));
     If (TempInt <> 0) and (TempInt < FunctionCutOff-1000) Then Begin
        Result := 2;
     End Else Begin
        If TempInt = 0 Then Begin
           If (Uppercase(TempStr) = 'GO') or (Uppercase(TempStr) = 'DEF') Then
              Result := 0
           Else Begin
              Text := Text + #0;
              While (F < Length(Text)) and (Text[F] = ' ') Do
                 Inc(F);
              If Ord(Text[F]) in MathTypes Then
                 Result := 1
              Else Begin
                 If Text[F] = '$' Then
                    If Pos(Text[1], Alphas) <> 0 Then Begin
                       Result := 1;
                       Exit;
                    End Else Begin
                       Result := 0;
                       Exit;
                    End;

                 F := 1;
                 While F < Length(Text) Do Begin
                    If Pos(Text[F], AlphaNumerics+'( ') = 0 Then Begin
                       Result := 0;
                       Exit;
                    End;
                    If Text[F] = '(' Then Break;
                    Inc(F);
                 End;
                 Result := 1;
              End;
           End;
        End Else Begin
           Result := 1;
        End;
     End;
  End Else Begin
     If (Pos(Text[1], Symbols) > 0) and (Text[1] <> ':') Then Begin
        Result := 1;
     End Else If Pos(Text[1], Numerics) > 0 Then Begin
        F := 1; TempStr := '';
        While (Pos(Text[F], Numerics) > 0) or
              ((UpperCase(Text[F]) = 'E') and Not SciFlag) Do Begin
           If UpperCase(Text[F]) = 'E' Then SciFlag := True;
           TempStr := TempStr + Text[F];
           Inc(F);
        End;
        While Text[F] = ' ' Do Inc(F);
        If (Pos(Text[F], Symbols) > 0) and (Text[F] <> ':') Then Begin
           Result := 1;
        End Else Begin
           If (Text[F] = ':') or (Pos(Text[F], Alphas) <> 0) Then Begin
              If (Uppercase(Copy(Text, F, 2)) = 'OR') or (Uppercase(Copy(Text, F, 3)) = 'AND') Then
                 Result := 1
              Else
                 Result := 3;
           End Else If F >= Length(Text) Then Result := 4;
        End;
     End Else
        If Text[1] = ':' Then Result := 2;
  End;
  If SciFlag Then Result := 1;
End;

// ************************** Parse ExprLine ******************************
// *     Once a line has been identified as a direct expression, this     *
// *           routine is used to check the syntax of the code.           *
// ************************************************************************

Function ParseExprLine(Line: AnsiString): TParseError;
Var
  Text: AnsiString;
  Temptype, Start: Integer;
  PS: TParseStack;
  PI: TParseItem;
  Lines: TStringlist;
Begin
  GlobalType := SIUnknown;
  GlobalReturn := 0;
  SyntaxTemplate := '';
  Start := 1;
  Text := BeautifyBASIC(Line);
  PS := BuildParseStack(Text);
  Temptype := GetExprType(Start, PS, 0, 0, True);
  SyntaxTemplate := SyntaxTemplate + AnsiChar(16)+AnsiChar(4)+' ['+WhatIs(GlobalReturn)+']';
  If TempType <= 0 Then Begin
     If (SyntaxTemplate = '') or (Pos(AnsiChar(1), SyntaxTemplate) = 0) Then
        If GlobalType in NumericTypes Then
           SyntaxTemplate := AnsiChar(16)+AnsiChar(1)+'Numeric Expression'
        Else If GlobalType in StringTypes Then
           SyntaxTemplate := AnsiChar(16)+AnsiChar(1)+'String Expression'
        Else
           SyntaxTemplate := AnsiChar(16)+AnsiChar(1)+'Expression';
     PI := PS.GetItem(Start);
     If PI.ItemType = SIStrVar Then ErrorMsg := 'String variables must be single letters.';
     Result.Error := ErrorMsg; Result.Syntax := SyntaxTemplate;
     Result.Line := Text; If TempType <> 0 Then Result.ErrorCode := TempType Else Result.ErrorCode := -1;
     Result.Position := PI.Position;
     If GlobalPos <> 0 Then Result.PositionEnd := PS.GetItem(GlobalPos).Position Else Result.PositionEnd := PS.GetItem(Start+1).Position;
     Result.Linenum := 0;
     Result.Statement := 1;
     PS.Free;
     Exit;
  End Else Begin
     If GlobalType in NumericTypes Then
        SyntaxTemplate := AnsiChar(16)+AnsiChar(1)+'Numeric Expression'
     Else
        SyntaxTemplate := AnsiChar(16)+AnsiChar(1)+'String Expression';
     If (Temptype in Numerictypes) or (TempType in StringTypes) Then Begin
        Result.ErrorCode := 0;
        ErrorMsg := '';
     End Else Begin
        Result.ErrorCode := -1;
     End;
     If Result.ErrorCode = 0 Then Begin
        Lines := SplitStatements(Text);
        If Lines.Count > 1 Then Begin
           Result.Error := 'Multiple Expressions not allowed.'; Result.Syntax := SyntaxTemplate;
           Result.Line := Text; Result.ErrorCode := -1; Result.Position := 0; Result.Linenum := 0;
           Result.PositionEnd := 0; Result.Statement := 1;
           Lines.Free;
           PS.Free;
           Exit;
        End;
     End;
     If Start <> PS.NumItems -1 Then Begin
        Result.ErrorCode := -1;
        Result.Error := 'Expected End of Line.';
        Result.Position := PS.GetItem(Start).Position;
        Result.PositionEnd := PS.GetItem(PS.NumItems-1).Position;
        Result.Syntax := SyntaxTemplate;
        Result.Line := Text;
        Result.LineNum := 0;
        Result.Statement := 1;
     End Else Begin
        Result.Error := ErrorMsg; Result.Syntax := SyntaxTemplate; Result.Line := Text; Result.Position := 0;
        Result.Linenum := 0; Result.Statement := 1; Result.PositionEnd := 0;
     End;
     PS.Free;
  End;
End;

// ************************** Parse CodeLine ******************************
// * Once a line has been identified as either a direct or storable line, *
// * This routine is used to check the line number and the syntax of the  *
// *                      individual statements.                          *
// ************************************************************************

Function ParseCodeLine(Line: AnsiString; Storable: Boolean): TParseError;
Var
  Lines: TStringlist;
  PS: TParseStack;
  PI: TParseItem;
  Start, F, Temptype: Integer;
  Text, LineNum: AnsiString;
begin
  With Result Do Begin
     Error := ''; Syntax := '';
     Line := ''; Position := 0;
     Statement := 0; ErrorCode := 0;
     LineNum := 0; PositionEnd := 0;
  End;
  While Copy(Line, 1, 1)=' ' Do Line := Copy(Line, 2, 99999);
  If Storable Then Begin
     F := 1;
     While Pos(Copy(Line, F, 1), Numerics) > 0 Do Begin
        LineNum := LineNum + Copy(Line, F, 1);
        Inc(F);
     End;
     If (StrToIntDef(LineNum, -1) < 0) Or (StrToIntDef(LineNum, -1) > 9999) Then Begin
        Result.Error := 'Invalid Line Number';
        Result.Statement := 1;
        Result.Syntax := AnsiChar(16)+AnsiChar(2)+'LineNumber'+AnsiChar(16)+AnsiChar(0)+' [ : Statement]';
        Result.ErrorCode := -1;
        Result.Line := Line;
        Result.Position := 1;
        Result.PositionEnd := Length(LineNum)+1;
        Exit;
     End Else
        Result.LineNum := StrToInt(LineNum);
  End;
  Text := BeautifyBASIC(Copy(Line, Length(LineNum)+1, 999999));
  If Text <> '' Then Begin
     Lines := SplitStatements(Text);
     If Lines.Count > 0 Then Begin
        For F := 0 To Lines.Count -1 Do Begin
           syntaxtemplate := '';
           GlobalReturn := 0;
           ErrorMsg := '';
           Start := 1;
           PS := BuildParseStack(Copy(Lines[F], Pos(AnsiChar(255), Lines[F])+1, 999999));
           TempType := PS.GetItemType(Start);
           If TempType <> SIEol Then Begin
              If TempType > 1000 Then Begin
                 Dec(TempType, 1000);
                 Inc(Start);
                 If TempType = IsReserved('REM') Then Begin
                    If (F < Lines.Count-1) or (PS.NumItems > 3) Then
                       Result.Syntax := 'REM Comments'
                    Else
                       Result.Syntax := 'REM ['+AnsiChar(16)+AnsiChar(1)+'Comments'+AnsiChar(16)+AnsiChar(0)+']';
                    Result.Error := '';
                    Result.Position := 0;
                    Result.PositionEnd := 0;
                    Result.ErrorCode := 0;
                    Result.Statement := F+1;
                    Result.Line := Text;
                    PS.Free;
                    Exit;
                 End;
                 If TempType+1000 < KeyWordCutOff Then Begin
                    TempType := ParseKeyword(TempType+1000, Start, PS);
                    SyntaxTemplate := SyntaxTemplate + AnsiChar(16)+AnsiChar(4)+' ['+WhatIs(GlobalReturn)+']';
                 End Else Begin
                    Result.Error := 'Keyword '+WhatIs(Temptype+1000)+' Cannot start a line.';
                    Result.Position := PS.GetItem(1).Position;
                    If LineNum <> '' Then
                       Result.Position := Length(LineNum+' ')+StrToInt(Copy(Lines[F], 1, Pos(AnsiChar(255), Lines[F])-1))+PS.GetItem(1).Position-1
                    Else
                       Result.Position := StrToInt(Copy(Lines[F], 1, Pos(AnsiChar(255), Lines[F])-1))+PS.GetItem(1).Position-1;
                    Result.PositionEnd := (PS.GetItem(2).Position - PS.GetItem(1).Position) + Result.Position;
                    Result.ErrorCode := -1;
                    Result.Statement := F+1;
                    If Storable then Result.Line := LineNum+' '+Text Else Result.Line := Text;
                    If Storable Then Begin
                       If F = 0 Then
                          Result.Syntax := 'LineNumber'+AnsiChar(16)+AnsiChar(2)+' Statement'+AnsiChar(16)+AnsiChar(0)+' [ : Statement]'
                       Else
                          Result.Syntax := 'LineNumber Statement : '+AnsiChar(16)+AnsiChar(2)+'Statement';
                    End Else Begin
                       If F = 0 Then
                          Result.Syntax := AnsiChar(16)+AnsiChar(2)+'Statement'+AnsiChar(16)+AnsiChar(0)+' [ : Statement]'
                       Else
                          Result.Syntax := 'Statement : '+AnsiChar(16)+AnsiChar(2)+'Statement';
                    End;
                    PS.Free;
                    Exit;
                 End;
              End;
           End;
           PI := PS.GetItem(Start);
           If PI.ItemType = SIStrVar Then
              ErrorMsg := 'String variables must be single letters.';
           Result.Error := ErrorMsg;
           Result.ErrorCode := TempType;
           If Storable then Result.Line := LineNum+' '+Text Else Result.Line := Text;
           Result.Statement := F+1;
           If TempType <> SIStatement Then Begin
              If LineNum <> '' Then
                 Result.Position := Length(LineNum+' ')+StrToInt(Copy(Lines[F], 1, Pos(AnsiChar(255), Lines[F])-1))+PI.Position-1
              Else
                 Result.Position := StrToInt(Copy(Lines[F], 1, Pos(AnsiChar(255), Lines[F])-1))+PI.Position-1;
              If GlobalPos <> 0 Then Result.PositionEnd := (PS.GetItem(GlobalPos).Position - PI.Position)+Result.Position Else Result.PositionEnd := (PS.GetItem(Start+1).Position - PS.GetItem(Start).Position) + Result.Position;
              If TempType <= 0 Then Begin
                 If TempType <> 0 Then Result.ErrorCode := TempType Else Result.ErrorCode := -1;
                 If Copy(Lines[F], Length(Lines[F]), 1) = AnsiChar(255) Then Result.Error := 'Expecting a Statement';
                 Result.Syntax := SyntaxTemplate;
                 If F < Lines.Count -1 Then Result.ErrorCode := -1;
                 PS.Free;
                 Lines.Free;
                 Exit;
              End Else Begin
                 If (PS.GetItemType(Start) <> SIEol) or ((F < Lines.Count-1) and (PS.GetItemType(Start) <> SIEol)) Then Result.ErrorCode := -1;
                 If Storable Then Begin
                    If F = 0 Then
                       Result.Syntax := 'LineNumber'+AnsiChar(16)+AnsiChar(1)+' Statement'+AnsiChar(16)+AnsiChar(0)+' [ : Statement]'
                    Else
                       Result.Syntax := 'LineNumber Statement : '+AnsiChar(16)+AnsiChar(1)+'Statement';
                 End Else Begin
                    If F = 0 Then
                       Result.Syntax := AnsiChar(16)+AnsiChar(1)+'Statement'+AnsiChar(16)+AnsiChar(0)+' [ : Statement]'
                    Else
                       Result.Syntax := 'Statement : '+AnsiChar(16)+AnsiChar(1)+'Statement';
                 End;
                 If TempType = SIEol Then Result.ErrorCode := 0 Else Result.ErrorCode := -1;
                 Result.Error := 'Expecting Keyword.';
              End;
              TempType := PS.GetItemType(Start);
              PS.Free;
              If (F = Lines.Count -1) or (TempType <> SIEol) Then Begin
                 Lines.Free;
                 Exit;
              End;
           End Else Begin
              Result.Syntax := SyntaxTemplate;
              Result.Position := 0;
           End;
        End;
     End;
  End;
  PS.Free;
  Lines.Free;
  Result.Error := 'Ok';
end;

// ***************************** What Is **********************************
// * This is used for error reporting, to determine what type a token is, *
// *         in english. ie, SINumExpr is "Numeric Expression".           *
// ************************************************************************

Function WhatIs(TokenType: Integer): AnsiString;
Var
  Idx: Integer;
Const
  WhatIsTypes: Array[0..47] of AnsiString =
  ('Unknown Type', 'Literal Number', 'Literal String', 'Numeric Variable', 'Single Letter Numeric Variable',
   'String Variable', 'Single Letter String Variable', 'End Of Line', 'Symbol', 'Item Of Text',
   'Less Than Operator', 'More Than Operator', 'Less Than Or Equal to Operator', 'More Than or Equal to Operator',
   'Does Not Equal Operator', 'Binary Number', 'Numeric Expresion', 'String Expression', 'Fractional Number',
   'Print Item', 'Colour Item', 'Print Separator', 'Statement', 'Unterminated String', 'Comment',
   'Subscripted/Sliced Numeric Variable', 'Subscripted/Sliced String Variable', '', 'Any Variable',
   'Any Expression', 'Sliced String Variable', 'Any Non-Subscripted/Sliced Variable ', 'Space', '!', '"',
   '#', '$', '%', '&', AnsiChar(39), 'OpenBracket', 'CloseBracket', '*', '+', 'Comma', '-', '.', '/');
Begin
  If TokenType < 47 Then Begin
     Result := WhatIsTypes[TokenType];
  End Else Begin
     If TokenType < 1000 Then Begin
        Result := AnsiChar(TokenType);
     End Else Begin
        Idx := 1;
        Repeat
           Result := Copy(Keywords[Idx], Pos('-', Keywords[Idx]) +1, 999);
           If Result = IntToStr(TokenType) Then Begin
              Result := Copy(Keywords[Idx], 1, Pos('-', Keywords[Idx]) -1);
              Break;
           End;
           Inc(Idx);
        Until Idx = NumReserved;
     End;
  End;
End;

// *************************** Init Parser ********************************
// * Initialises all the structures and "Short Cut" variables for the     *
// * Parser system. Must be called before accessing any parser functions. *
// ************************************************************************

Procedure InitParser;
Var
  TempList: TStringlist;
  F, PS: Integer;
  Found: Boolean;
Begin
  NumReserved := High(Keywords);
  TempList := TStringlist.Create;
  For F := 1 To High(Keywords) Do TempList.Add(Keywords[F]);
  For F := 0 To TempList.Count-1 Do
     KeyWordNames[F +1] := Copy(TempList[F], 1, Pos('-', TempList[F])-1);
  TempList.Sort;
  For F := 1 To TempList.Count Do Begin
     Keywords[F] := TempList[F-1];
     KeywordsIDXTable[F] := StrToInt(Copy(Keywords[F], Pos('-', Keywords[F])+1, 999));
     KeyWordsIDXNames[F] := Copy(Keywords[F], 1, Pos('-', Keywords[F])-1);
  End;

  For F := 0 To 25 Do Begin
     Ps := 1;
     Found := False;
     Repeat
        If Ord(Keywords[Ps, 1])-65 = F Then Found := True Else Inc(Ps);
     Until Found or (Ps = TempList.Count);
     KeywordsHash[F] := Ps;
  End;
  TempList.Free;
  Specials := [ISReserved('AT'), ISReserved('TAB'), ISReserved('BRIGHT'), ISReserved('OVER'), ISReserved('INVERSE'), ISReserved('FLASH'), ISReserved('INK'), ISReserved('PAPER')];
  NonStarters := [ISReserved('TO'), IsReserved('STEP'), IsReserved('LINE'), IsReserved('THEN')];
  SI_TO := 1000 +IsReserved('TO');
  SI_OR := 1000 +IsReserved('OR');
  SI_AND := 1000 +IsReserved('AND');
  ColourWords := [ISReserved('INK'), ISReserved('PAPER'), ISReserved('FLASH'), ISReserved('BRIGHT'), ISReserved('INVERSE'), ISReserved('OVER'), ISReserved('AT'), ISReserved('TAB')];
  AllExprs := [1, 3, 4, 15, 16, 18, 25, 2, 6, 17, 26, 23..28, 30];
  NumericTypes := [1, 3, 4, 15, 16, 18, 25];
  StringTypes := [2, 6, 17, 26, 30];
  VarTypes := [3, 4, 6, 23..28, 30];
  PrintSeps := [39, 44, 59];
  DefVarTypes := [3, 4, 6];
  RelOps := [10..14, 61];
  NumVars := [3, 4, 25];
  StrVars := [6, 26, 30];
  MathTypes := [SIAdd, SIMinus, SIMul, SIDiv, SIPower, SIMoreThan, SILessThan, SIMoreThanEq, SILessThanEq, SIEquals, SINotEqual];
  ErrorMsg := 'Ok.';
  SyntaxTemplate := '';
  ParserInitDone := True;
End;

// ************************* Beautify Basic *******************************
// * Takes a String of sinclair basic and prettifies it - pads out the    *
// * keywords with spaces, and removes leading and trailing spaces.       *
// * It is here that the GOSUB, GOTO and DEFFN are processed with spaces  *
// * or without - both are legal.                                         *
// ************************************************************************

Function BeautifyBASIC(BASIC: AnsiString): AnsiString;
Var
  G, Reserved, StartAt: Integer;
  InString, REM_Found, GotKeyword: Boolean;
  Work, Word, OldWord: AnsiString;
Begin
  REM_Found := False;
  GotKeyword := False;
  If BASIC = '' Then Exit;
  Work := BASIC+' '; G := 0; InString := False; Word := '';
  While G < Length(Work) Do Begin
     Inc(G);
     If Work[G] = '"' then InString := Not InString;
     If Not InString Then Begin
        If (GotKeyword and (Pos(UpperCase(Work[G]), Alphas+'$0123456789') <> 0)) or (Pos(UpperCase(Work[G]), Alphas+'$') <> 0) or REM_Found Then Begin
           Word := Word + Work[G];
           If Length(Word) = 1 Then StartAt := G;
        End Else If Word <> '' Then Begin
           If Pos(UpperCase(Word)+'/', 'GO/DEF/') = 0 Then Begin
              OldWord := Word;
              If UpperCase(Copy(Word, 1, 5)) = 'GOSUB' Then Begin Word := 'GO SUB'+Copy(Word, 6, 999999); Inc(G); End;
              If UpperCase(Copy(Word, 1, 4)) = 'GOTO' Then Begin Word := 'GO TO'+Copy(Word, 5, 999999); Inc(G); End;
              If UpperCase(Copy(Word, 1, 5)) = 'DEFFN' Then Begin Word := 'DEF FN'+Copy(Word, 6, 999999); Inc(G); End;
              Reserved := ContainsReserved(Uppercase(Word));
              If Reserved <> 0 Then Begin
                 GotKeyword := True;
                 Delete(Work, StartAt, Length(OldWord));
                 Insert(Uppercase(Word), Work, StartAt);
                 If Work[StartAt+Length(Word)] <> ' ' Then Insert(' ', Work, StartAt+Length(Word));
                 If (StartAt > 1) and (Reserved < 56) Then
                    If Work[StartAt-1] <> ' ' Then
                       Insert(' ', Work, StartAt);
              End;
              If UpperCase(Word) = 'REM' Then REM_Found := True;
              Word := '';
           End Else
              Word := Word + ' ';
        End;
     End;
  End;
  While Copy(Work, Length(Work), 1) = ' ' Do Work := Copy(Work, 1, Length(Work) -1);
  While Copy(Work, 1, 1) = ' ' Do Work := Copy(Work, 2, 999999);
  Result := Work;
End;

// ************************* Split Statements *****************************
// * Splits a line of basic up into statements in a stringlist. Makes     *
// * parsing easier, you just have to process each line in the stringlist *
// ************************************************************************

Function SplitStatements(BASIC: AnsiString): TStringlist;
Var
  F, Start: Integer;
  InString: Boolean;
  Work: AnsiString;
Begin
  InString := False;
  Work := '';
  Result := TStringlist.Create;
  BASIC := BASIC + ':';
  Start := 1;
  For F := 1 To Length(BASIC) Do Begin
     If BASIC[F] = '"' Then InString := Not InString;
     If (F = Length(BASIC)) And Instring Then InString := False;
     If (BASIC[F] <> ':') or ((BASIC[F] = ':') and InString) Then Begin
        Work := Work + BASIC[F];
     End Else Begin
        Result.Add(IntToStr(Start)+AnsiChar(255)+Work);
        Start := F +1;
        Work := '';
     End;
  End;
  F := 0;
  While F < Result.Count Do Begin
     Work := Result[F];
     While Copy(Work, 1, 1) = ' ' Do Work := Copy(Work, 2, 999999);
     Inc(F);
  End;
End;

// ************************ Build Parse Stack *****************************
// * The real meat of the pre-parsing process - this tokenises everything *
// * in the AnsiString out to a stack of tokens. See the comments for the     *
// * types. Handle floats, Sci-Notation and quote-in-quote systems itself *
// ************************************************************************

Function BuildParseStack(Line: AnsiString): TParseStack;
Var
  SI, PreSI, NegSI: TParseItem;
  WordType: Integer;
  S, Word: AnsiString;
  S2: AnsiChar; IsBinary, SciFlag: Boolean;
  F, StartPos, ReservedIndex: Integer;
Begin
  NegSI.ItemType := -1; NegSI.Position := -1;
  Result := TParseStack.Create;
  WordType := SIUnknown; Word := ''; F := 0;
  Line := Line + ' ';
  Result.Push(0, 0);
  While F < Length(Line) Do Begin
     Inc(F); S := Uppercase(Line[F]); S2 := Line[F];
     Case WordType Of
        SIComment:   Begin
                          Result.Push(SIComment, F);
                          F := Length(Line);
                     End;
        SIUnknown:   Begin
                          If S <> ' ' Then Begin
                             If Pos(S, Alphas)   <> 0 Then WordType := SITextItem;
                             If Pos(S, Numerics) <> 0 Then WordType := SINumLiteral;
                             If Pos(S, Symbols)  <> 0 Then WordType := SISymbol;
                             If S = '"' Then WordType := SIStrLiteral;
                             Word := ''; StartPos := F; Dec(F);
                             IsBinary := True; SciFlag := False;
                             If Wordtype = SIUnknown Then WordType := SISymbol;
                          End;
                       End;
        SITextItem:    Begin
                          If Pos(S, Alphas)   <> 0 Then Word := Word + S2;
                          If Pos(S, Numerics) <> 0 Then Begin
                             ReservedIndex := IsReserved(Uppercase(Word));
                             If ReservedIndex <> 0 Then Begin
                                Result.Push(ReservedIndex+1000, StartPos);
                                WordType := SIUnknown;
                                Dec(F);
                             End Else
                                Word := Word + S2;
                          End;
                          If (Pos(S, Symbols) <> 0) or (S = ' ') Then Begin
                             If (S = ' ') and ((UpperCase(Word) = 'DEF') or (UpperCase(Word) = 'GO')) Then
                                Word := Word + S2
                             Else Begin
                                If S = '$' Then Begin Word := Word + '$'; Inc(F); End;
                                ReservedIndex := IsReserved(Uppercase(Word));
                                If ReservedIndex <> 0 Then Begin
                                   Result.Push(ReservedIndex+1000, StartPos);
                                   If UpperCase(Word) = 'REM' Then WordType := SIComment;
                                End Else Begin
                                   If Result.NumItems > 0 Then SI := Result.Pop Else SI.ItemType := -1;
                                   If (SI.ItemType = SINumVar) or (SI.ItemType = SINumVarSL) Then Begin
                                      If S = '$' Then
                                         Result.Push(SIStrVar, SI.Position)
                                      Else
                                         Result.Push(SINumVar, SI.Position);
                                   End Else Begin
                                      Result.Push(SI);
                                      If S = '$' Then Begin
                                         If Length(Word) = 2 Then Result.Push(SIStrVarSL, StartPos)
                                         Else Result.Push(SIStrVar, StartPos);
                                      End Else Begin
                                         If Length(Word) = 1 Then Result.Push(SINumVarSL, StartPos)
                                         Else Result.Push(SINumVar, StartPos);
                                      End;
                                   End;
                                End;
                                If WordType <> SIComment Then WordType := SIUnknown;
                                Dec(F);
                             End;
                          End;
                       End;
        SINumLiteral:  Begin
                          If (Pos(S, Numerics) <> 0) or
                             ((Not SciFlag) and (S = 'E') and ((Pos(Copy(Line, F+1,1), Numerics) <> 0) or ((Pos(Copy(Line, F+1, 1), '-+') <> 0) and (Pos(Copy(line, F+2, 1), Numerics) <> 0)))) or
                             (SciFlag and ((S = '+') or (S = '-')) and (Uppercase(Line[F-1])='E')) Then Begin
                             Word := Word + S2;
                             If (S2 <> '0') and (S2 <> '1') Then IsBinary := False;
                             If (S = 'E') or (S = '+') or (S = '-') Then Begin SciFlag := True; S := '2'; End;
                          End;
                          If (Pos(S, Symbols) <> 0) or (Pos(S, Alphas) <> 0) or (S = ' ') Then Begin
                             If IsBinary Then Begin
                                If Result.GetItemType(Result.NumItems -1) = IsReserved('BIN') +1000 Then
                                   Result.Push(SIBinNumber, StartPos)
                                Else IsBinary := False;
                             End;
                             If Not IsBinary Then Begin
                                If Result.NumItems > 0 Then SI := Result.Pop Else SI := NegSI;
                                If Result.NumItems > 0 Then PreSI := Result.Pop Else PreSI := NegSI;
                                If SI.ItemType = Ord('.') Then Begin
                                   If PreSI.ItemType = SINumLiteral Then Begin
                                      Result.Push(SIFloat, PreSI.Position);
                                   End Else begin
                                      If PreSI.ItemType <> -1 Then Result.Push(PreSI);
                                      Result.Push(SIFloat, SI.Position);
                                   End;
                                End Else Begin
                                   If PreSI.ItemType <> -1 Then Result.Push(PreSI);
                                   If SI.ItemType <> -1 Then Result.Push(SI);
                                   Result.Push(SINumLiteral, StartPos);
                                End;
                             End;
                             WordType := SIUnknown;
                             Dec(F);
                          End;
                       End;
        SISymbol:      Begin
                          If Line[F] = '=' Then Begin
                             If Result.NumItems > 0 Then SI := Result.Pop Else SI.Itemtype := -1;
                             If SI.ItemType = SIMoreThan Then Result.Push(SIMoreThanEq, SI.Position) Else
                                If SI.Itemtype = SILessThan Then Result.Push(SILessThanEq, SI.Position) Else Begin
                                   If SI.ItemType <> -1 Then Result.Push(SI);
                                   Result.Push(Ord('='), StartPos);
                                End;
                          End Else If Line[F] = '>' Then Begin
                             If Result.NumItems > 0 Then SI := Result.Pop Else SI.Itemtype := -1;
                             If SI.Itemtype = SILessThan Then Result.Push(SINotEqual, SI.Position) Else Begin
                                If SI.ItemType <> -1 Then Result.Push(SI);
                                Result.Push(SIMoreThan, StartPos);
                             End;
                          End Else If Line[F] = '<' Then Begin
                             Result.Push(SILessThan, StartPos);
                          End Else Begin
                             Result.Push(Ord(Line[F]), StartPos);
                          End;
                          WordType := SIUnknown;
                       End;
        SIStrLiteral:  Begin
                          Word := Word + S2;
                          If (S = '"') and (Length(Word) > 1) then Begin
                             If Result.NumItems > 0 Then SI := Result.Pop Else SI.ItemType := SIStrLiteral;
                             If SI.ItemType = SIStrLiteral Then
                                Result.Push(SIStrLiteral, StartPos)
                             Else Begin
                                Result.Push(SI);
                                Result.Push(SIStrLiteral, StartPos)
                             End;
                             WordType := SIUnknown;
                          End;
                       End;
     End;
  End;
  If Word <> '' Then
     If WordType in [SINumliteral, SITextItem, SINumLiteral, SIComment] Then
        Result.Push(WordType, StartPos);
  If WordType = SIStrLiteral Then Result.Push(SIUnterminated, StartPos);
  Result.Push(SIEol, Length(Line));
End;

// **************************** Is Reserved *******************************
// * Checks a AnsiString against the internal database of keywords, and   *
// * returns -1 if not a keyword, or the index of that keyword.           *
// ************************************************************************

Function IsReserved(Token: AnsiString): Integer;
Begin
  Result := 0;
  If Token[1] >= 'A' Then Begin
     Result := KeywordsHash[Ord(Token[1])-65];
     While (Result <= NumReserved) and (Token <> Copy(Keywords[Result], 1, Pos('-', Keywords[Result])-1)) Do Inc(Result);
     If Result >= NumReserved+1 Then Begin
        Result := 0;
     End Else
        Result := StrToInt(Copy(Keywords[Result], Pos('-', Keywords[Result])+1, 999))-1000;
  End;
End;

Function IndexIsReserved(Token: AnsiString): Integer;
Begin
  Result := 0;
  If Token[1] >= 'A' Then Begin
     Result := KeywordsHash[Ord(Token[1])-65];
     While (Result <= NumReserved) and (Token <> KeywordsIDXNames[Result]) Do Inc(Result);
     If Result >= NumReserved+1 Then
        Result := 0;
  End;
End;

Function ContainsReserved(Token: AnsiString): Integer;
Var
  Keyword: AnsiString;
Begin
  Result := 0;
  If Token[1] >= 'A' Then Begin
     Result := KeywordsHash[Ord(Token[1])-65];
     KeyWord := Copy(Keywords[Result], 1, Pos('-', Keywords[Result])-1);
     While (Result <= NumReserved) and (Copy(Token, 1, Length(Keyword)) <> Keyword) Do Begin
        Inc(Result);
        KeyWord := Copy(Keywords[Result], 1, Pos('-', Keywords[Result])-1);
     End;
     If Result >= NumReserved+1 Then
        Result := 0;
  End;
End;

Function IsKeyword(Token: AnsiString): Integer;
Begin
  Result := 0;
  If Token[1] >= 'A' Then Begin
     Result := 1;
     While (Result <= NumReserved) and (Token <> KeywordNames[Result]) Do Inc(Result);
     If Result >= NumReserved+1 Then Result := 0;
  End;
End;

// *************************** Remove Spaces ******************************
// * Simply strips all space chars (32) out of the AnsiString, and returns    *
// * That AnsiString.                                                         *
// ************************************************************************

Function RemoveSpaces(Word: AnsiString): AnsiString;
Var
F: Integer;
Begin
  Result := '';
  For F := 1 to Length(Word) Do If Word[F] <> ' ' Then Result := Result + Word[F];
End;

// *************************** Get Expr Type ******************************
// *     This function is used by the ParseKeyword() function to check    *
// *    Expressions in the keyword syntax. Returns either the expression  *
// *     type, or an error constant (<0). If an expression is not found   *
// *              as in 1+() it returns 0 (SIUnknown).                    *
// ************************************************************************

Function GetExprType(Var Start: Integer; PS: TParseStack; Pri, Expected: Integer; ModGlb: Boolean): Integer;
Var
  CurItem, PrevType, NextType, NewPri, PrevTerm, Expect, TempStart, BeginStart: Integer;
  Finished, NoInc: Boolean;
Begin
  Result := SIUnknown;
  PrevType := SIUnknown;
  Expect := SIUnknown;
  Finished := False;
  NoInc := False;
  BeginStart := Start;
  TempStart := 0;
  GlobalPos := 0;
  PrevTerm := 0;
  CurItem := PS.GetItemType(Start);
  While CurItem in [SIAdd, SIMinus] Do Begin
     If CurItem = SIMinus Then Begin
        Result := SINumExpr;
        PrevTerm := SINumExpr;
        Expect := SINumExpr;
        If Expected = SIUnknown Then Expected := SINumExpr;
        If Expected = SIStrExpr Then Begin
           Result := -1;
           ErrorMsg := 'Expecting a String Expression.';
           Exit;
        End;
     End;
     PrevType := CurItem;
     Inc(Start);
     CurItem := PS.GetItemType(Start);
  End;
  While Not Finished Do Begin
     CurItem := PS.GetItemType(Start);
     If Not ((CurItem in NumericTypes) or (CurItem in StringTypes) or (CurItem in MathTypes) or (CurItem in [SIOpenBrace, SICloseBrace]) or (CurItem > 1000) or (CurItem = SIEol)) Then Begin
        If CurItem = SIUnterminated Then Begin
           Result := -2;
           ErrorMsg := 'Unterminated String.';
           Exit;
        End;
        Finished := True;
     End;
     If CurItem = SIEol Then Finished := True;
     If CurItem = SIOpenBrace Then Begin
        TempStart := Start;
        Inc(Start);
        If (PrevType in VarTypes) or (PrevType in StringTypes) Then Begin
           If PrevType in StringTypes Then Begin
              If PrevType In StrVars Then NextType := SIStrVar Else NextType := SIStrLiteral;
              If PrevType = SIStrVarSubs Then NextType := PrevType;
           End Else NextType := PrevType;
           If NextType = SINumVarSL Then
              CurItem := ParseNumSubs(Start, PS)
           Else
              If NextType in [SIStrVar, SiStrLiteral] Then
                 CurItem := ParseStrSubs(Start, PS, NextType)
              Else Begin
                 ErrorMsg := 'Only single letter variables can be dimensioned.';
                 Result := -1;
                 If GlobalPos = 0 Then GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End;
           If CurItem <= 0 Then Begin
              Result := CurItem;
              Exit;
           End Else Begin
              PrevType := CurItem;
              CurItem := PS.GetItemType(Start);
           End;
        End Else Begin
           CurItem := GetExprType(Start, PS, 0, Expected, False);
           If CurItem >= 0 Then Begin
              If CurItem = SIUnknown Then Begin
                 ErrorMsg := 'Expecting an Expression.';
                 Result := -1;
                 If GlobalPos = 0 Then GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End Else If PS.GetItemtype(Start) <> SICloseBrace Then Begin
                 Result := -2;
                 ErrorMsg := 'Brackets Not Balanced.';
                 If GlobalPos = 0 Then GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End;
           End Else Begin
              Result := CurItem;
              Exit;
           End;
        End;
     End;
     If CurItem = SICloseBrace Then Begin
        Finished := True;
     End;
     If (CurItem in MathTypes) or (CurItem = SI_AND) or (CurItem = SI_OR) then Begin
        If Not(PrevType in NumericTypes) and Not(PrevType in StringTypes) and Not(CurItem in [SIAdd, SIMinus]) Then
           Finished := True
        Else Begin
           Case CurItem Of
              SIPower:      Begin NewPri := 10; Expect := SINumExpr; End;
              SIMul, SIDiv: Begin NewPri := 8;  Expect := SINumExpr; End;
              SIAdd:        Begin NewPri := 6;  If PrevTerm in NumericTypes Then Expect := SINumExpr Else Expect := SIStrExpr; End;
              SIMinus:      Begin NewPri := 6;  Expect := SINumExpr; End;
              SIEquals, SINotEqual, SIMoreThan, SILessThan, SIMoreThanEq, SILessThanEq: Begin NewPri := 5; If PrevTerm in NumericTypes Then Expect := SINumExpr Else Expect := SIStrExpr; End;
           Else
              If CurItem = SI_AND then Begin NewPri := 3; Expect := SINumExpr; End
              Else If CurItem = SI_OR Then Begin NewPri := 2; Expect := SINumExpr; End;
           End;
           If NewPri > Pri then Begin
              Inc(Start);
              TempStart := Start;
              NoInc := True;
              PrevType := CurItem;
              CurItem := GetExprType(Start, PS, NewPri, Expect, False);
              If CurItem < 0 Then Begin
                 Result := CurItem;
                 Exit;
              End;
           End Else Begin
              Exit;
           End;
        End;
     End;
     If CurItem > 1000 Then Begin
        If Not ((PrevType in MathTypes) or (PrevType = SI_AND) or (PrevType = SI_OR) or (PrevType = SIUnknown)) Then Exit;
        If Not (CurItem-1000 in Specials) and (CurItem < KeyWordCutOff) Then Begin
           If (PrevType in MathTypes) or (PrevType = SI_AND) or (PrevType = SI_OR) Then Begin
              Result := -1;
              ErrorMsg := 'Keyword '+WhatIs(CurItem)+' must begin a line.';
           End;
           Exit;
        End;
        TempStart := Start;
        Inc(Start);
        NoInc := True;
        CurItem := ParseKeyword(CurItem, Start, PS);
        If CurItem <= 0 Then Begin
           Result := CurItem;
           Exit;
        End Else Begin
           If (CurItem in [SIPrintItem, SIColourItem]) and (TempStart <> BeginStart) Then Begin
              Result := -2;
              If CurItem = SIPrintItem Then ErrorMsg := 'Print Items are not allowed here.' Else ErrorMsg := 'Colour Items are not allowed here.';
              If TempStart <> 0 Then Begin
                 GlobalPos := Start;
                 Start := TempStart;
              End;
              Exit;
           End;
           If Not ((CurItem in StringTypes) or (CurItem in NumericTypes)) Then Result := CurItem;
        End;
     End;
     If (CurItem in Stringtypes) or (CurItem in NumericTypes) Then Begin
        If (PrevType in MathTypes) or (PrevType = SI_AND) or (PrevType = SI_OR) Then Begin
           Case PrevType of
              SIAdd:
                    Begin
                       If ((CurItem in StringTypes) and (PrevTerm in NumericTypes)) or
                          ((PrevTerm in StringTypes) and (CurItem in NumericTypes)) Then Begin
                          Result := -2;
                          If TempStart <> 0 Then Begin
                             GlobalPos := Start;
                             Start := TempStart;
                          End;
                          ErrorMsg := 'Cannot Add a String to a Numeric.';
                          Exit;
                       End Else Begin
                          If CurItem in StringTypes Then
                             Result := SIStrExpr
                          Else
                             Result := SINumExpr;
                             If CurItem in VarTypes Then
                                PrevType := CurItem
                             Else
                                PrevType := Result;
                       End;
                    End;
              SIMinus, SIMul, SIDiv, SIPower:
                    Begin
                       If (CurItem in StringTypes) or (PrevTerm in StringTypes) Then Begin
                          Result := -2;
                          If TempStart <> 0 Then Begin
                             GlobalPos := Start;
                             Start := TempStart;
                          End;
                          ErrorMsg := 'Cannot use a '+WhatIs(PrevType)+' with Strings.';
                          Exit;
                       End Else Begin
                          Result := SINumExpr;
                          If CurItem in VarTypes Then
                             PrevType := CurItem
                          Else
                             PrevType := Result;
                       End;
                    End;
              SIEquals, SINotEqual, SIMoreThan, SILessThan, SIMoreThanEq, SILessThanEq:
                    Begin
                       If ((CurItem in StringTypes) and (PrevTerm in NumericTypes)) or
                          ((PrevTerm in StringTypes) and (CurItem in NumericTypes)) Then Begin
                          Result := -2;
                          If TempStart <> 0 Then Begin
                             GlobalPos := Start;
                             Start := TempStart;
                          End;
                          ErrorMsg := 'Cannot Compare a String to a Numeric.';
                          Exit;
                       End Else Begin
                          Result := SINumExpr;
                          PrevType := Result;
                       End;
                    End;
           Else
              If PrevType = SI_AND Then Begin
                 If Not(CurItem in NumericTypes) Then Begin
                    Result := -2;
                    If TempStart <> 0 Then Begin
                       GlobalPos := Start;
                       Start := TempStart;
                    End;
                    ErrorMsg := 'AND Requires a Numeric 2nd Parameter.';
                    Exit;
                 End Else Begin
                    If PrevTerm in NumericTypes Then Begin
                       PrevType := SINumExpr;
                       Result := SINumExpr;
                    End Else If PrevTerm in StringTypes Then Begin
                       PrevType := SIStrExpr;
                       Result := SIStrExpr;
                    End;
                 End;
              End Else If PrevType = SI_OR Then Begin
                 If (Not(CurItem in NumericTypes)) or (Not(PrevTerm in NumericTypes)) Then Begin
                    Result := -2;
                    If TempStart <> 0 Then Begin
                       GlobalPos := Start;
                       Start := TempStart;
                    End;
                    ErrorMsg := 'OR Requires two Numeric parameters.';
                    Exit;
                 End Else Begin
                    Result := SINumExpr;
                    PrevType := SINumExpr;
                 End;
              End;
           End;
           TempStart := 0;
           PrevTerm := PrevType;
        End Else Begin
           If Result = SIUnknown Then Begin
              If CurItem in NumericTypes Then Result := SINumExpr;
              If CurItem in StringTypes Then Result := SIStrExpr;
              PrevTerm := Result;
              PrevType := CurItem;
           End Else Begin
              If (PrevType in MathTypes) or (PrevType = SI_AND) or (PrevType = SI_OR) Then Result := -1;
              Exit;
           End;
        End;
        If Not NoInc Then Inc(Start) Else NoInc := False;
     End;
     If ModGLB Then GlobalType := Result;
  End;
  If ErrorMsg = '' Then Begin
     If Expected <> SIUnknown Then ErrorMsg := 'Expecting a '+WhatIs(Expected)+'.' Else ErrorMsg := 'Expecting an Expression.';
  End;
  If (PrevType in MathTypes) or (PrevType = SI_OR) or (Prevtype = SI_AND) Then Begin
     Result := -2;
     If ErrorMsg = '' Then Begin
        If Expect <> 0 Then
           ErrorMsg := 'Expecting a '+WhatIs(Expect)+'.'
        Else
           ErrorMsg := 'Expecting an Expression.';
     End;
  End;
End;

// *********************** Parse Num/Str Subs *****************************
// * This checks a variable followed by a Bracket, to see if it is a      *
// * valid subscript or slicer. Returns -Pos if error or the variable     *
// * type if the syntax is correct. Start is left at the next item in the *
// * stack.                                                               *
// ************************************************************************

Function ParseNumSubs(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart, Expect: Integer;
  Finished: Boolean;
  SyntaxBackup: AnsiString;
Begin
  SyntaxBackup := SyntaxTemplate;
  SyntaxTemplate := 'NumVar('+AnsiChar(16)+AnsiChar(1)+'Subscripts'+AnsiChar(16)+AnsiChar(0)+')';
  Expect := SINumExpr;
  Finished := False;
  While Not Finished Do Begin
     Case Expect Of
        SINumExpr:
           Begin
              SyntaxTemplate := 'NumVar('+AnsiChar(16)+AnsiChar(1)+'Subscript '+AnsiChar(16)+AnsiChar(0)+'[, Subscript] )';
              TempStart := Start;
              TempType := GetExprType(Start, PS, 0, SINumExpr, False);
              If TempType <= 0 Then Begin
                 Result := TempType;
                 Exit;
              End;
              If Not(TempType in NumericTypes) Then Begin
                 Result := -1; //error - Expecting NumExpr.
                 ErrorMsg := 'Subscript Parameters must be Numeric.';
                 GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End Else Begin
                 Expect := SIComma;
              End;
           End;
        SIComma:
           Begin
              SyntaxTemplate := 'NumVar(Subscript ['+AnsiChar(16)+AnsiChar(1)+','+AnsiChar(16)+AnsiChar(0)+' Subscript] '+AnsiChar(16)+AnsiChar(1)+')';
              TempType := PS.GetItemType(Start);
              If TempType = SIComma Then Begin
                 Expect := SINumExpr;
                 Inc(Start);
              End Else If TempType = SICloseBrace Then Begin
                 Finished := True;
                 SyntaxTemplate := SyntaxBackup;
                 Inc(Start);
              End Else Begin
                 If Temptype <> SIEol Then Result := -1 Else Result := SIUnknown;
                 //error - Expecting , or )
                 ErrorMsg := 'Expecting <comma> or <Close Bracket>.';
                 Exit;
              End;
           End;
     End;
  End;
  Result := SINumVarSubs;
  SyntaxTemplate := SyntaxBackup;
End;

Function ParseStrSubs(Var Start: Integer; PS: TParseStack; VarType: Integer): Integer;
Var
  TempType, TempStart, Expect: Integer;
  Finished, GotStart, GotTO, GotSubs, CanClose: Boolean;
  SyntaxBackup: AnsiString;
Begin
  SyntaxBackup := SyntaxTemplate;
  CanClose := True;
  Expect := SINumExpr;
  Finished := False;
  GotStart := False;
  gotSubs := False;
  GotTO := False;
  While Not Finished Do Begin
     Case Expect Of
        SINumExpr:
           Begin
              If VarType <> SIStrLiteral Then Begin
                 If VarType = SIStrVarSubs Then
                    SyntaxTemplate := 'String ('+AnsiChar(16)+AnsiChar(1)+'Start'+AnsiChar(16)+AnsiChar(0)+' [ '+AnsiChar(16)+AnsiChar(1)+'TO'+AnsiChar(16)+AnsiChar(0)+' End] )'
                 Else
                    SyntaxTemplate := 'StrVar ('+AnsiChar(16)+AnsiChar(1)+'Subscript'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'Start'+AnsiChar(16)+AnsiChar(0)+' [, '+AnsiChar(16)+AnsiChar(1)+'TO'+AnsiChar(16)+AnsiChar(0)+' End] )';
              End Else
                 SyntaxTemplate := 'StrLiteral ('+AnsiChar(16)+AnsiChar(1)+'Start'+AnsiChar(16)+AnsiChar(0)+' [ '+AnsiChar(16)+AnsiChar(1)+'TO'+AnsiChar(16)+AnsiChar(0)+' End] )';
              TempStart := Start;
              If PS.GetItemType(Start) <> SI_TO Then Begin
                 If CanClose and (PS.GetItemType(Start) = SICloseBrace) Then Begin
                    Inc(Start);
                    Result := SIStrVarSubs;
                    SyntaxTemplate := SyntaxBackup;
                    Exit;
                 End Else
                    CanClose := False;
                 TempType := GetExprType(Start, PS, 0, SINumExpr, False);
                 If TempType <= 0 Then Begin
                    Result := TempType;
                    Exit;
                 End;
                 If Not(TempType in NumericTypes) Then Begin
                    Result := -1; //error - Expecting NumExpr.
                    ErrorMsg := 'Expecting a Numeric expression, TO or ")"';
                    GlobalPos := Start;
                    Start := TempStart;
                    Exit;
                 End Else Begin
                    Expect := SIComma;
                    GotStart := True;
                 End;
              End Else Begin
                 Inc(Start);
                 Finished := True;
                 GotTO := True;
              End;
           End;
        SIComma:
           Begin
              If VarType <> SIStrLiteral Then Begin
                 If VarType = SIStrVarSubs Then
                    SyntaxTemplate := 'String (Start ['+AnsiChar(16)+AnsiChar(1)+' TO '+AnsiChar(16)+AnsiChar(0)+'End]'+AnsiChar(16)+AnsiChar(1)+' )'
                 Else
                    SyntaxTemplate := 'StrVar (Subscript/Start ['+AnsiChar(16)+AnsiChar(1)+', TO '+AnsiChar(16)+AnsiChar(0)+'End]'+AnsiChar(16)+AnsiChar(1)+' )';
              End Else
                 SyntaxTemplate := 'StrLiteral (Start ['+AnsiChar(16)+AnsiChar(1)+' TO '+AnsiChar(16)+AnsiChar(0)+'End]'+AnsiChar(16)+AnsiChar(1)+' )';
              TempType := PS.GetItemType(Start);
              If TempType = SIComma Then Begin
                 If VarType = SIStrVarSubs Then Begin
                    Result := -1;
                    ErrorMsg := 'Cannot Subscript a Sliced String.';
                    Exit;
                 End Else If VarType = SIStrLiteral then Begin
                    Result := -1;
                    ErrorMsg := 'Cannot Subscript a String Literal.';
                    Exit;
                 End Else Begin
                    Expect := SINumExpr;
                    GotStart := False;
                    GotSubs := True;
                    Inc(Start);
                 End;
              End Else If TempType = SICloseBrace Then Begin
                 Finished := True;
                 SyntaxTemplate := SyntaxBackup;
                 Inc(Start);
              End Else If TempType = SI_TO Then Begin
                 Finished := True;
                 Inc(Start);
                 GotTO := True;
              End Else Begin
                 If Temptype <> SIEol Then Result := -1 Else Result := SIUnknown;
                 //error - Expecting , or )
                 ErrorMsg := 'Expecting <comma> or <Close Bracket>.';
                 Exit;
              End;
           End;
     End;
  End;
  If GotTo Then Begin
     //process the TO [End] portion.
     If GotStart Then Begin
        If VarType = SIStrLiteral Then
           SyntaxTemplate := 'StrLiteral (Start TO ['+AnsiChar(16)+AnsiChar(1)+'End'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+')'
        Else If GotSubs then
           SyntaxTemplate := 'StrVar (Subscripts, Start TO ['+AnsiChar(16)+AnsiChar(1)+'End'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+')'
        Else If VarType <> SIStrVarSubs Then
           SyntaxTemplate := 'StrVar (Start TO ['+AnsiChar(16)+AnsiChar(1)+'End'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+')'
        Else
           SyntaxTemplate := 'String (Start TO ['+AnsiChar(16)+AnsiChar(1)+'End'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+')';
        //END is optional here.
        TempType := PS.GetItemType(Start);
        If TempType <> SICloseBrace Then Begin
           If TempType = SIEol Then Begin
              Result := 0;
              ErrorMsg := 'Expecting a Numeric expression or <Close Bracket>.';
              Exit;
           End;
           TempStart := Start;
           TempType := GetExprType(Start, PS, 0, SINumExpr, False);
           If TempType <= 0 Then Begin
              Result := TempType;
              Exit;
           End Else Begin
              If Not (TempType in NumericTypes) Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := 0;
                 ErrorMsg := 'Expecting a Numeric type.';
                 GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End Else Begin
                 //now expecting a close bracket.
                 If VarType = SIStrLiteral Then
                    SyntaxTemplate := 'StrLiteral (Start TO End '+AnsiChar(16)+AnsiChar(1)+')'
                 Else If GotSubs then
                    SyntaxTemplate := 'StrVar (Subscripts, Start TO End '+AnsiChar(16)+AnsiChar(1)+')'
                 Else If VarType <> SIStrVarSubs Then
                    SyntaxTemplate := 'StrVar (Start TO End '+AnsiChar(16)+AnsiChar(1)+')'
                 Else
                    SyntaxTemplate := 'String (Start TO End '+AnsiChar(16)+AnsiChar(1)+')';
                 Temptype := PS.GetItemType(Start);
                 If TempType <> SICloseBrace Then Begin
                    If TempType <> SIEol Then Result := -1 Else Result := 0;
                    ErrorMsg := 'Expecting a <Close Bracket>.';
                    Exit;
                 End Else Begin
                    Inc(Start);
                 End;
              End;
           End;
        End Else Begin
           Inc(Start);
        End;
     End Else Begin
        If VarType = SIStrLiteral Then
           SyntaxTemplate := 'StrLiteral ( TO '+AnsiChar(16)+AnsiChar(1)+' End '+AnsiChar(16)+AnsiChar(0)+')'
        Else If GotSubs then
           SyntaxTemplate := 'StrVar (Subscripts, TO '+AnsiChar(16)+AnsiChar(1)+' End '+AnsiChar(16)+AnsiChar(0)+')'
        Else If VarType <> SIStrVarSubs Then
           SyntaxTemplate := 'StrVar ( TO '+AnsiChar(16)+AnsiChar(1)+' End '+AnsiChar(16)+AnsiChar(0)+')'
        Else
           SyntaxTemplate := 'String ( TO '+AnsiChar(16)+AnsiChar(1)+' End '+AnsiChar(16)+AnsiChar(0)+')';
        //END is mandatory here.
        TempStart := Start;
        TempType := GetExprType(Start, PS, 0, SINumExpr, False);
        If TempType <= 0 Then Begin
           Result := TempType;
           Exit;
        End Else Begin
           If Not (TempType in NumericTypes) Then Begin
              If TempType <> SIEol Then Result := -1 Else Result := 0;
              ErrorMsg := 'Expecting a Numeric type.';
              GlobalPos := Start;
              Start := TempStart;
              Exit;
           End Else Begin
              //now expecting a close bracket.
              If VarType = SIStrLiteral Then
                 SyntaxTemplate := 'StrLiteral ( TO End '+AnsiChar(16)+AnsiChar(1)+')'
              Else If GotSubs then
                 SyntaxTemplate := 'StrVar (Subscripts, TO End '+AnsiChar(16)+AnsiChar(1)+')'
              Else If VarType <> SIStrVarSubs Then
                 SyntaxTemplate := 'StrVar ( TO End '+AnsiChar(16)+AnsiChar(1)+')'
              Else
                 SyntaxTemplate := 'String ( TO End '+AnsiChar(16)+AnsiChar(1)+')';
              Temptype := PS.GetItemType(Start);
              If TempType <> SICloseBrace Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := 0;
                 ErrorMsg := 'Expecting a <Close Bracket>.';
                 Exit;
              End Else Begin
                 Inc(Start);
              End;
           End;
        End;
     End;
  End;
  Result := SIStrVarSubs;
  SyntaxTemplate := SyntaxBackup;
End;

// ************************** Parse Keyword ******************************
// * This is used as a jumpgate to the relevant routine for each keyword *
// ***********************************************************************

Function ParseKeyword(Keyword: Integer; Var Start: Integer; PS: TParseStack): Integer;
Var
  ReturnBak, F: Integer;
  SyntaxBackup: AnsiString;
  Statement, InOptional: Boolean;
Begin
  InOptional := False;
  If Start = 2 Then Statement := True Else Statement := False;
  ReturnBak := GlobalReturn;
  SyntaxBackup := SyntaxTemplate;
  Case Keyword Of
{BEEP}      1001: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'BEEP ', PS, 0, [SINumExpr, SIComma, SINumExpr, SIEol], ['Duration', ', ', 'Pitch'], SIStatement); End;
{BORDER}    1002: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'BORDER ', PS, 0, [SINumExpr, SIEol], ['Colour (0..7)'], SIStatement); End;
{CAT}       1003: Begin GlobalReturn := SIStatement;  Result := ParseCAT(Start, PS); End;
{CIRCLE}    1004: Begin GlobalReturn := SIStatement;  Result := ParseGFXFuncs(Start, 'CIRCLE ', ['X', 'Y', 'Radius'], PS, 3); End;
{CLEAR}     1005: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'CLEAR ', PS, 0, [SIOptional, SINumExpr, SIEol], ['', 'Address'], SIStatement); End;
{CLOSE}     1006: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'CLOSE ', PS, 0, [Ord('#'), SINumExpr, SIEol], ['#', 'Channel'], SIStatement); End;
{CLS}       1007: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'CLS ', PS, 0, [SIEol], [AnsiChar(16)+AnsiChar(0)+'[No Parameters]'], SIStatement); End;
{CONTINUE}  1008: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'CONTINUE ', PS, 0, [SIEol], [AnsiChar(16)+AnsiChar(0)+'[No Parameters]'], SIStatement); End;
{COPY}      1009: Begin GlobalReturn := SIStatement;  Result := ParseCopy(Start, PS); End;
{DATA}      1010: Begin GlobalReturn := SIStatement;  Result := ParseDATAREAD(Start, PS, False); End;
{DEF FN}    1011: Begin GlobalReturn := SIStatement;  Result := ParseDEFFN(Start, PS); End;
{DIM}       1012: Begin GlobalReturn := SIStatement;  Result := ParseDIM(Start, PS); End;
{DRAW}      1013: Begin GlobalReturn := SIStatement;  Result := ParseGFXFuncs(Start, 'DRAW ', ['X', 'Y', 'Angle'], PS, 4); End;
{ERASE}     1014: Begin GlobalReturn := SIStatement;  Result := ParseErase(Start, Ps); End;
{FOR}       1015: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'FOR ', PS, 0, [SINumVarSL, SIEquals, SINumExpr, SI_TO, SINumExpr, SIOptional, IsReserved('STEP')+1000, SINumExpr, SIEol], ['Var', ' = ', 'Start', ' TO ', 'End', '', ' STEP ', 'Increment'],  SIStatement); End;
{FORMAT}    1016: Begin GlobalReturn := SIStatement;  Result := ParseFormat(Start, PS); End;
{GO SUB}    1017: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'GO SUB ', PS, 0, [SINumExpr, SIEol], ['Line Number'], SIStatement); End;
{GO TO}     1018: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'GO TO ', PS, 0, [SINumExpr, SIEol], ['Line Number'], SIStatement); End;
{IF}        1019: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'IF ', PS, 0, [SINumExpr, IsReserved('THEN')+1000, SIOptional, SIStatement, SIEol], ['Expression', ' THEN ', '', 'Statement'], SIStatement); End;
{INPUT}     1020: Begin GlobalReturn := SIStatement;  Result := ParsePRINTINPUT(Start, PS, True); End;
{LET}       1021: Begin GlobalReturn := SIStatement;  Result := ParseLET(Start, PS); End;
{LIST}      1022: Begin GlobalReturn := SIStatement;  Result := ParseList(Start, PS); End;
{LLIST}     1023: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'LLIST ', PS, 0, [SIOptional, SINumExpr, SIEol], ['', 'Line Number'], SIStatement); End;
{LOAD}      1024: Begin GlobalReturn := SIStatement;  Result := ParseLOADSAVE(Start, PS, False); End;
{LPRINT}    1025: Begin GlobalReturn := SIStatement;  Result := ParsePRINTINPUT(Start, PS, False); End;
{MERGE}     1026: Begin GlobalReturn := SIStatement;  Result := ParseMERGE(Start, Ps); End;
{MOVE}      1027: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'MOVE ', PS, 0, [SIStrExpr, SI_TO, SIStrExpr, SIEol], ['Filename', ' TO ', 'Filename'], SIStatement); End;
{NEW}       1028: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'NEW ', PS, 0, [SIEol], [AnsiChar(16)+AnsiChar(0)+'[No Parameters]'], SIStatement); End;
{NEXT}      1029: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'NEXT ', PS, 0, [SINumVarSL, SIEol], ['For-Variable'], SIStatement); End;
{OPEN}      1030: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'OPEN ', PS, 0, [Ord('#'), SINumExpr, SIComma, SIStrExpr, SIEol], ['#', 'Channel', ', ', 'Stream'], SIStatement); End;
{OUT}       1031: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'OUT ', PS, 0, [SINumExpr, SIComma, SINumExpr, SIEol], ['Port', ', ', 'Byte'], SIStatement); End;
{PAUSE}     1032: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'PAUSE ', PS, 0, [SINumExpr, SIEol], ['Duration (Frames)'], SIStatement); End;
{PLAY}      1033: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'PLAY ', PS, 0, [SIStrExpr, SIOptional, SIComma, SIStrExpr, SIOptional, SIComma, SIStrExpr, SIOptional, SIComma, SIStrExpr, SIOptional, SIComma, SIStrExpr, SIOptional, SIComma, SIStrExpr, SIOptional, SIComma, SIStrExpr, SIOptional, SIComma, SIStrExpr, SIOptional, SIComma, SIEol], ['Channel1', '', ', ', 'Channel2', '', ', ', 'Channel3', '', ', ', 'Midi1', '', ', ', 'Midi2', '', ', ', 'Midi3', '', ', ', 'Midi4', '', ', ', 'Midi5'], SIStatement); End;
{PLOT}      1034: Begin GlobalReturn := SIStatement;  Result := ParseGFXFuncs(Start, 'PLOT ', ['X', 'Y'], PS, 2); End;
{POKE}      1035: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'POKE ', PS, 0, [SINumExpr, SIComma, SINumExpr, SIEol], ['Address', ', ', 'Byte'], SIStatement); End;
{PRINT}     1036: Begin GlobalReturn := SIStatement;  Result := ParsePRINTINPUT(Start, PS, False); End;
{RANDOMIZE} 1037: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'RANDOMIZE ', PS, 0, [SIOptional, SINumExpr, SIEol], ['', 'Seed'], SIStatement); End;
{READ}      1038: Begin GlobalReturn := SIStatement;  Result := ParseDATAREAD(Start, PS, True); End;
{REM}       1039: Begin GlobalReturn := SIStatement;  Result := SIStatement; End;
{RESTORE}   1040: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'RESTORE ', PS, 0, [SIOptional, SINumExpr, SIEol], ['', 'Line Number'], SIStatement); End;
{RETURN}    1041: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'RETURN ', PS, 0, [SIEol], [AnsiChar(16)+AnsiChar(0)+'[No Parameters]'], SIStatement); End;
{RUN}       1042: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'RUN ', PS, 0, [SIOptional, SINumExpr, SIEol], ['', 'Line Number'], SIStatement); End;
{SAVE}      1043: Begin GlobalReturn := SIStatement;  Result := ParseLOADSAVE(Start, PS, True); End;
{SPECTRUM}  1044: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'SPECTRUM ', PS, 0, [SIEol], [AnsiChar(16)+AnsiChar(0)+'[No Parameters]'], SIStatement); End;
{STOP}      1045: Begin GlobalReturn := SIStatement;  Result := ParseGenericType(Start, 'STOP ', PS, 0, [SIEol], [AnsiChar(16)+AnsiChar(0)+'[No Parameters]'], SIStatement); End;
{VERIFY}    1046: Begin GlobalReturn := SIStatement;  Result := ParseLOADSAVE(Start, PS, False); End;

{BRIGHT}    1047: Begin GlobalReturn := SIColourItem; Result := ParseGenericType(Start, 'BRIGHT ', PS, 0, [SINumExpr], ['State (0/1)'], SIColourItem); End;
{FLASH}     1048: Begin GlobalReturn := SIColourItem; Result := ParseGenericType(Start, 'FLASH ', PS, 0, [SINumExpr], ['State (0/1)'], SIColourItem); End;
{INK}       1049: Begin GlobalReturn := SIColourItem; Result := ParseGenericType(Start, 'INK ', PS, 0, [SINumExpr], ['Colour (0..7)'], SIColourItem); End;
{INVERSE}   1050: Begin GlobalReturn := SIColourItem; Result := ParseGenericType(Start, 'INVERSE ', PS, 0, [SINumExpr], ['State (0/1)'], SIColourItem); End;
{OVER}      1051: Begin GlobalReturn := SIColourItem; Result := ParseGenericType(Start, 'OVER ', PS, 0, [SINumExpr], ['State (0/1)'], SIColourItem); End;
{PAPER}     1052: Begin GlobalReturn := SIColourItem; Result := ParseGenericType(Start, 'PAPER ', PS, 0, [SINumExpr], ['Colour (0..7)'], SIColourItem); End;

{AT}        1057: Begin GlobalReturn := SIPrintItem;  Result := ParseGenericType(Start, 'AT ', PS, 0, [SINumExpr, SIComma, SINumExpr], ['Y', ', ', 'X'], SIPrintItem); End;
{TAB}       1058: Begin GlobalReturn := SIPrintItem;  Result := ParseGenericType(Start, 'TAB ', PS, 0, [SINumExpr], ['Tab-Stop'], SIPrintItem); End;

{ABS}       1059: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'ABS ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{ACS}       1060: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'ACS ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{ASN}       1062: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'ASN ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{ATN}       1063: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'ATN ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{ATTR}      1064: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'ATTR ', PS, 0, [SIOpenBrace, SINumExpr, SIComma, SINumExpr, SICloseBrace], ['( ', 'Y', ', ', 'X ', ')'], SINumExpr); End;
{BIN}       1065: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'BIN ', PS, 11, [SIOptional, SIBinNumber], ['', 'Binary Number'], SINumExpr); End;
{CHR$}      1066: Begin GlobalReturn := SIStrExpr;    Result := ParseGenericType(Start, 'CHR$ ', PS, 11, [SINumExpr], ['Value'], SIStrExpr); End;
{CODE}      1067: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'CODE ', PS, 11, [SIStrExpr], ['String'], SINumExpr); End;
{COS}       1068: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'COS ', PS, 11, [SINumExpr], ['Radians'], SINumExpr); End;
{EXP}       1069: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'EXP ', PS, 11, [SINumExpr], ['Power'], SINumExpr); End;
{FN}        1070: Begin GlobalReturn := SIAnyExpr;    Result := ParseFN(Start, PS); End;
{IN}        1071: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'IN ', PS, 11, [SINumExpr], ['Port'], SINumExpr); End;
{INKEY$}    1072: Begin GlobalReturn := SIStrExpr;    Result := SIStrExpr; End;
{INT}       1073: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'INT ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{LEN}       1074: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'LEN ', PS, 11, [SIStrExpr], ['String'], SINumExpr); End;
{LN}        1075: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'LN ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{NOT}       1076: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'NOT ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{PEEK}      1078: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'PEEK ', PS, 11, [SINumExpr], ['Address'], SINumExpr); End;
{PI}        1079: Begin GlobalReturn := SINumExpr;    Result := SINumExpr; End;
{POINT}     1080: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'POINT ', PS, 0, [SIOpenBrace, SINumExpr, SIComma, SINumExpr, SICloseBrace], ['( ', 'X', ', ', 'Y ', ')'], SINumExpr); End;
{RND}       1081: Begin GlobalReturn := SINumExpr;    Result := SINumExpr; End;
{SCREEN$}   1082: Begin GlobalReturn := SIStrExpr;    Result := ParseGenericType(Start, 'SCREEN$ ', PS, 0, [SIOpenBrace, SINumExpr, SIComma, SINumExpr, SICloseBrace], ['( ', 'X', ', ', 'Y ', ')'], SIStrExpr); End;
{SGN}       1083: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'SGN ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{SIN}       1084: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'SIN ', PS, 11, [SINumExpr], ['Radians'], SINumExpr); End;
{SQR}       1085: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'SQR ', PS, 11, [SINumExpr], ['Value'], SINumExpr); End;
{STR$}      1086: Begin GlobalReturn := SIStrExpr;    Result := ParseGenericType(Start, 'STR$ ', PS, 11, [SINumExpr], ['Value'], SIStrExpr); End;
{TAN}       1087: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'TAN ', PS, 11, [SINumExpr], ['Radians'], SINumExpr); End;
{USR}       1088: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'USR ', PS, 11, [SIAnyExpr], ['Address/UDG-String'], SINumExpr); End;
{VAL}       1089: Begin GlobalReturn := SINumExpr;    Result := ParseGenericType(Start, 'VAL ', PS, 11, [SIStrExpr], ['String'], SINumExpr); End;
{VAL$}      1090: Begin GlobalReturn := SIStrExpr;    Result := ParseGenericType(Start, 'VAL$ ', PS, 11, [SIStrExpr], ['String'], SIStrExpr); End;
  End;
  If (GlobalReturn = SIAnyExpr) And ((Result = SINumExpr) or (Result = SIStrExpr)) Then GlobalReturn := Result;
  If (GlobalReturn = SIColourItem) and Statement and (ReturnBak <> 0) Then GlobalReturn := SIStatement;
  If Statement and (Result = SIColourItem) Then Result := SIStatement;
  If ((Result = SIStatement) or (Statement and (Result = SIColourItem))) and (PS.GetItemType(Start) <> SIEol) Then Begin
     Result := -1;
  End;
  If result > 0 Then Begin
     For F := 1 to Length(SyntaxTemplate) Do Begin
        If SyntaxTemplate[F] = '[' Then InOptional := True;
        If SyntaxTemplate[F] = ']' Then InOptional := False;
        If SyntaxTemplate[F] = AnsiChar(2) Then SyntaxTemplate[F] := AnsiChar(0);
        If (SyntaxTemplate[F] = AnsiChar(1)) and (Not InOptional) Then SyntaxTemplate[F] := AnsiChar(0);
     End;
     If SyntaxBackUp <> '' Then If Pos(AnsiChar(16)+AnsiChar(1), SyntaxTemplate) < 1 Then SyntaxTemplate := SyntaxBackup;
     If ReturnBak <> 0 Then GlobalReturn := ReturnBak;
  End;
End;

Function ParseGenericType(Var Start: Integer; Key: AnsiString; PS: TParseStack; Pri: Integer; ExpectList: Array of Integer; SyntaxList: Array Of AnsiString; Return: Integer): Integer;
Var
  F, Optional, TempType, TempStart, Expect, ExpectPos: Integer;
  Error, LateOption: Boolean;
  S: AnsiString;
Begin
  If GlobalType = SIUnknown Then GlobalType := Return;
  Error := False;
  ExpectPos := -1;
  SyntaxTemplate := Key;
  While (ExpectPos < High(ExpectList)) and Not Error Do Begin
     Inc(ExpectPos);
     Optional := -10;
     LateOption := False;
     Expect := ExpectList[ExpectPos];
     If Expect = SIOptional Then Begin
        Inc(ExpectPos);
        Expect := ExpectList[ExpectPos];
        Optional := ExpectPos-1;
     End;
     SyntaxTemplate := Key;
     For F := 0 To ExpectPos-1 Do If F <= High(SyntaxList) Then SyntaxTemplate := SyntaxTemplate + SyntaxList[F];
     If (Expect = SIEol) and (High(ExpectList)>0) Then Begin
        Dec(ExpectPos); If ExpectList[ExpectPos] = SIOptional Then Dec(ExpectPos);
        SyntaxTemplate := Copy(SyntaxTemplate, 1, Length(SyntaxTemplate)-Length(SyntaxList[Expectpos]));
     End;
     If ExpectPos <= High(SyntaxList) Then Begin
        If (Optional = ExpectPos-1) and (PS.GetItemType(Start) = SIEol) Then Begin
           If Copy(SyntaxTemplate, Length(SyntaxTemplate)-2, 2) = ' '+AnsiChar(16) Then
              SyntaxTemplate := SyntaxTemplate+'['
           Else
              If SyntaxTemplate[Length(SyntaxTemplate)] <> ' ' Then
                 SyntaxTemplate := SyntaxTemplate + ' ['
              Else
                 SyntaxTemplate := SyntaxTemplate + '[';
           LateOption := True;
        End;
        SyntaxTemplate := SyntaxTemplate + AnsiChar(16)+AnsiChar(1)+SyntaxList[ExpectPos];
     End;
     If ExpectPos < High(ExpectList) Then Begin
        SyntaxTemplate := SyntaxTemplate + AnsiChar(16)+AnsiChar(0);
        For F := ExpectPos+1 To High(ExpectList) Do If F <= High(SyntaxList) Then Begin
           If ExpectList[F] = SIOptional Then Begin
              If Not LateOption Then
                 If Copy(SyntaxTemplate, Length(SyntaxTemplate)-2, 2) = ' '+AnsiChar(16) Then
                    SyntaxTemplate := SyntaxTemplate+'[' Else
                       If SyntaxTemplate[Length(SyntaxTemplate)] <> ' ' Then SyntaxTemplate := SyntaxTemplate + ' [' Else
                          SyntaxTemplate := SyntaxTemplate + '[';
              LateOption := True;
           End;
           SyntaxTemplate := SyntaxTemplate + SyntaxList[F];
        End;
     End;
     If LateOption Then SyntaxTemplate := SyntaxTemplate + ']';
     If (Expect = SIEol) and (High(ExpectList)>0) Then Inc(ExpectPos);
     //End Syntax bit, start to parse.
     If (ExpectPos-Optional) = 1 Then Begin
        If PS.GetItemType(Start) = SIEol Then Begin
           GlobalExpect := [Expect];
           Result := Return;
           Exit;
        End;
     End;
     Case Expect Of
        SINumExpr:
           Begin
              TempStart := Start;
              TempType := GetExprType(Start, PS, Pri, SINumExpr, False);
              If TempType <= 0 Then Begin
                 Result := TempType;
                 Exit;
              End Else If Not(TempType in NumericTypes) Then Begin
                 Result := -1;
                 GlobalPos := Start;
                 Start := TempStart;
                 ErrorMsg := 'Expecting a Numeric expression.';
                 Exit;
              End;
           End;
        SIStrExpr:
           Begin
              TempStart := Start;
              TempType := GetExprType(Start, PS, Pri, SIStrExpr, False);
              If TempType <= 0 Then Begin
                 Result := TempType;
                 Exit;
              End Else If Not(TempType in StringTypes) Then Begin
                 GlobalPos := Start;
                 Start := TempStart;
                 Result := -1;
                 ErrorMsg := 'Expecting a String expression.';
                 Exit;
              End;
           End;
        SINumVar:
           Begin
              TempType := PS.GetItemType(Start);
              If Not ((TempType in VarTypes) and (Temptype In NumericTypes)) Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := 0;
                 ErrorMsg := 'Expecting a Numeric Variable.';
                 Exit;
              End;
              Inc(Start);
           End;
        SIStrVar:
           Begin
              TempType := PS.GetItemType(Start);
              If Not ((TempType in VarTypes) and (Temptype In StringTypes)) Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := 0;
                 ErrorMsg := 'Expecting a String Variable.';
                 Exit;
              End;
              Inc(Start);
           End;
        SIAnyVar:
           Begin
              TempType := PS.GetItemType(Start);
              If Not (TempType in VarTypes) Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := 0;
                 ErrorMsg := 'Expecting a Variable type.';
                 Exit;
              End;
              Inc(Start);
           End;
        SIAnyExpr:
           Begin
              TempStart := Start;
              TempType := GetExprType(Start, PS, Pri, SIUnknown, False);
              If TempType <= 0 Then Begin
                 Result := TempType;
                 Exit;
              End Else If Not (TempType in AllExprs) Then Begin
                 GlobalPos := Start;
                 Start := TempStart;
                 Result := -1;
                 ErrorMsg := 'Expecting an expression.';
                 Exit;
              End;
           End;
        SIAnyVarNS:
           Begin
              Temptype := PS.GetItemType(Start);
              If TempType In [SINumVarSubs, SIStrVarSubs] Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := 0;
                 ErrorMsg := 'Expecting an un-subscripted variable.';
                 Exit;
              End;
              If Not (TempType In NumVars) and Not (TempType In StrVars) Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := 0;
                 ErrorMsg := 'Expecting an un-subscripted variable.';
                 Exit;
              End;
              Inc(Start);
           End;
        SIStatement:
           Begin
              Dec(Start);
              TempType := SIColon;
              While TempType = SIColon Do Begin
                 Inc(Start);
                 TempType := PS.GetItemType(Start);
              End;
              If TempType > 1000 Then Begin
                 Dec(TempType, 1000);
                 Inc(Start);
                 If TempType < KeyWordCutOff Then
                    TempType := ParseKeyword(TempType+1000, Start, PS)
                 Else Begin
                    Result := -1;
                    ErrorMsg := 'Expecting Keyword.';
                    Exit;
                 End;
              End Else Begin
                 If TempType <> SIEol Then Begin
                    Result := -1;
                    ErrorMsg := 'Expecting Keyword.';
                 End Else Begin
                    Result := Return;
                 End;
                 Exit;
              End;
              If TempType <= 0 Then Begin
                 Result := TempType;
                 Exit;
              End Else If Not (TempType in [SIStatement, SIColourItem, SIColon]) Then Begin
                 If TempType <> SIUnknown Then Result := -1 Else Result := SIUnknown;
                 ErrorMsg := 'Expecting a Valid Statement.';
                 Exit;
              End Else Begin
                 Result := Return;
                 Exit;
              End;
           End;
     Else
        Begin
           TempType := PS.GetItemType(Start);
           If TempType <> Expect Then Begin
              If TempType <> SIEol Then Result := -1 Else Result := 0;
              S := WhatIs(Expect);
              If Pos(S, Symbols) <> 0 Then S := ' '+AnsiChar(39)+S+AnsiChar(39)+' ';
              ErrorMsg := 'Expecting a '+S;
              Exit;
           End;
           Inc(Start);
        End;
     End;
     GlobalExpect := [Expect];
  End;
  Result := Return;
End;

//rest of the special functions go here.

Function ParseFN(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart, Expect, NumParams: Integer;
  Finished: Boolean;
Begin
  //First, expect a num/str SL Var.
  SyntaxTemplate := 'FN '+AnsiChar(16)+AnsiChar(1)+'Var'+AnsiChar(16)+AnsiChar(0)+' ( Params )';
  TempType := PS.GetItemType(Start);
  If TempType in [4, 6] Then Begin
     GlobalExpect := [SINumVarSL, SIStrVar];
     If TempType = 4 Then Result := SINumExpr Else Result := SIStrExpr;
     If GlobalType = SIUnknown Then GlobalType := Result;
     //Next, an Open Bracket.
     SyntaxTemplate := 'FN Var '+AnsiChar(16)+AnsiChar(1)+'('+AnsiChar(16)+AnsiChar(0)+' Params )';
     Inc(Start);
     Temptype := PS.GetItemType(Start);
     If TempType = SIOpenBrace Then Begin
        GlobalExpect := [SIOpenBrace];
        //Next, expecting either a Close Bracket or any Expr.
        SyntaxTemplate := 'FN Var ( ['+AnsiChar(16)+AnsiChar(1)+'Params'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+')';
        NumParams := 0;
        Inc(Start);
        If PS.GetItemType(Start) = SICloseBrace Then Begin
           GlobalExpect := [SICloseBrace];
           Inc(Start);
           SyntaxTemplate := 'FN Var ('+AnsiChar(16)+AnsiChar(1)+')';
           Exit; //End Of FN a()
        End Else Begin
           //expecting one or more params of type anyexpr.
           Expect := SIAnyExpr;
           Finished := False;
           While Not Finished Do Begin
              Case Expect Of
                 SIAnyExpr:
                    Begin
                       GlobalExpect := [SIAnyExpr];
                       If NumParams > 0 Then
                          SyntaxTemplate := 'FN Var ( Param, '+AnsiChar(16)+AnsiChar(1)+'Param'+AnsiChar(16)+AnsiChar(0)+' )'
                       Else
                          SyntaxTemplate := 'FN Var ( ['+AnsiChar(16)+AnsiChar(1)+'Params'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+')';
                       TempStart := Start;
                       TempType := GetExprType(Start, PS, 0, SIUnknown, False);
                       If TempType <= 0 Then Begin
                          Result := TempType;
                          Exit;
                       End;
                       If (TempType <> SINumExpr) and (TempType <> SIStrExpr) Then Begin
                          Result := -1; //error - Expecting AnyExpr.
                          ErrorMsg := 'Expecting an expression.';
                          GlobalPos := Start;
                          Start := TempStart;
                          Exit;
                       End Else Begin
                          Expect := SIComma;
                          Inc(NumParams);
                       End;
                    End;
                 SIComma:
                    Begin
                       GlobalExpect := [SIComma, SICloseBrace];
                       SyntaxTemplate := 'FN Var ( Param ['+AnsiChar(16)+AnsiChar(1)+','+AnsiChar(16)+AnsiChar(0)+' Param]'+AnsiChar(16)+AnsiChar(1)+' )';
                       TempType := PS.GetItemType(Start);
                       If TempType = SIComma Then Begin
                          Expect := SIAnyExpr;
                          Inc(Start);
                       End Else If TempType = SICloseBrace Then Begin
                          GlobalExpect := [SICloseBrace];
                          Finished := True;
                          SyntaxTemplate := 'FN Var ( Params'+AnsiChar(16)+AnsiChar(1)+' )';
                          Inc(Start);
                       End Else Begin
                          If Temptype <> SIEol Then Result := -1 Else Result := SIUnknown;
                          //error - Expecting , or )
                          ErrorMsg := 'Expecting <comma> or <Close Bracket>.';
                          Exit;
                       End;
                    End;
              End;
           End;
        End;
     End Else Begin
        If Temptype <> SIEol Then Result := -1 Else Result := SIUnknown;
        ErrorMsg := 'Expecting <Open Bracket>.';
     End;
  End Else Begin
     If Temptype <> SIEol Then Result := -1 Else Result := SIUnknown;
     ErrorMsg := 'Expecting Single Letter Numeric or String Variable.';
  End;
End;

Function ParseCAT(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart: Integer;
  Channel, StringThere: Boolean;
Begin
  Channel := False; StringThere := False;
  SyntaxTemplate := 'CAT ['+AnsiChar(16)+AnsiChar(1)+'!/#/Filespec'+AnsiChar(16)+AnsiChar(0)+']';
  Temptype := PS.GetItemType(Start);
  If TempType = SIEol Then Begin Result := SIStatement; Exit; End;
  GlobalExpect := [Ord('!'), Ord('#'), SIStrExpr];
  If TempType = Ord('!') then Begin
     SyntaxTemplate := 'CAT !';
     Inc(Start);
     TempType := PS.GetItemType(Start);
     GlobalExpect := [Ord('!')];
     If TempType <> SIEol Then Begin
        Result := -1;
        ErrorMsg := 'Not Expecting a '+Whatis(Temptype)+' here.';
        Exit;
     End Else Begin
        Result := SIStatement;
        Exit;
     End;
  End;
  If TempType = Ord('#') Then Begin
     Inc(Start);
     SyntaxTemplate := 'CAT #'+AnsiChar(16)+AnsiChar(1)+'Channel '+AnsiChar(16)+AnsiChar(0)+'[, FileSpec]';
     TempStart := Start;
     GlobalExpect := [SINumExpr];
     Temptype := GetExprType(Start, PS, 0, SINumExpr, False);
     If TempType <= 0 Then Begin
        Result := TempType; //error in expression - propagate out
        Exit;
     End Else Begin
        If Not (TempType in NumericTypes) Then Begin
           Result := -1; //error - expecting numeric
           ErrorMsg := 'Expecting a Numeric Expression.';
           GlobalPos := Start;
           Start := TempStart;
           Exit;
        End;
     End;
     Channel := True;
     TempType := PS.GetItemType(Start);
     SyntaxTemplate := 'CAT #Channel ['+AnsiChar(16)+AnsiChar(1)+', FileSpec'+AnsiChar(16)+AnsiChar(0)+']';
     If TempType = SIComma Then Begin
        SyntaxTemplate := 'CAT #Channel, '+AnsiChar(16)+AnsiChar(1)+'FileSpec'+AnsiChar(16)+AnsiChar(0);
        Inc(Start);
        TempStart := Start;
        GlobalExpect := [SIStrExpr];
        Temptype := GetExprType(Start, PS, 0, SIStrExpr, False);
        If TempType <= 0 Then Begin
           Result := TempType;
           Exit;
        End Else Begin
           If Not (TempType in StringTypes) Then Begin
              Result := -1; //error - expecting AnsiString type
              ErrorMsg := 'Expecting String Expression.';
              GlobalPos := Start;
              Start := TempStart;
              Exit;
           End;
        End;
        StringThere := True;
        SyntaxTemplate := 'CAT #Channel, FileSpec';
     End Else If TempType <> SIEol Then Begin
        SyntaxTemplate := 'CAT #Channel';
     End Else Begin
        Result := SIStatement;
        Exit;
     End;
  End Else Begin
     SyntaxTemplate := 'CAT ['+AnsiChar(16)+AnsiChar(1)+'FileSpec'+AnsiChar(16)+AnsiChar(0)+']';
     GlobalExpect := [SIStrExpr];
     If TempType = SIEol Then Begin
        Result := SIStatement;
        Exit;
     End;
     TempStart := Start;
     GlobalExpect := [SIStrExpr];
     Temptype := GetExprType(Start, PS, 0, SIStrExpr, False);
     If TempType <= 0 Then Begin
        Result := TempType;
        Exit;
     End Else Begin
        If Not (TempType in StringTypes) Then Begin
           Result := -1;
           GlobalPos := Start;
           Start := TempStart;
           ErrorMsg := 'Expecting String Expression.';
           Exit;
        End;
     End;
     StringThere := True;
     SyntaxTemplate := 'CAT FileSpec';
  End;
  If Channel and Not StringThere Then SyntaxTemplate := 'CAT #Channel ';
  If Channel and StringThere Then SyntaxTemplate := 'CAT #Channel, FileSpec ';
  If Not Channel and Not StringThere Then SyntaxTemplate := 'CAT ';
  If Not Channel and StringThere Then SyntaxTemplate := 'CAT FileSpec ';
  TempType := PS.GetItemType(Start);
  If TempType = SIEol Then Begin
     Result := SIStatement;
     Exit;
  End Else Begin
     Result := -1;
     ErrorMsg := 'Not expecting any further parameters.';
     Exit;
  End;
End;

Function ParseERASE(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart: Integer;
  RAMDiskOp: Boolean;
Begin
  RAMDiskOp := False;
  SyntaxTemplate := 'ERASE ['+AnsiChar(16)+AnsiChar(1)+'!'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+'Filename';
  Temptype := PS.GetItemType(Start);
  GlobalExpect := [Ord('!'), SIStrExpr];
  If TempType = Ord('!') then Begin
     SyntaxTemplate := 'ERASE ! '+AnsiChar(16)+AnsiChar(1)+'Filename';
     RAMDiskOp := True;
     Inc(Start);
  End;
  Temptype := GetExprType(Start, PS, 0, SIStrExpr, False);
  If TempType <= 0 Then Begin
     Result := TempType; //error in expression - propagate out
     If RAMDiskOp Then
        ErrorMsg := 'Expecting a String Expression.'
     Else
        ErrorMsg := 'Expecting a "!" or String Expression.';
     Exit;
  End Else Begin
     If Not (TempType in StringTypes) Then Begin
        Result := -1; //error - expecting AnsiString
        If RAMDiskOp Then
           ErrorMsg := 'Expecting a String Expression.'
        Else
           ErrorMsg := 'Expecting a "!" or String Expression.';
        GlobalPos := Start;
        Start := TempStart;
        Exit;
     End;
  End;
  TempType := PS.GetItemType(Start);
  If TempType = SIEol Then Begin
     Result := SIStatement;
     Exit;
  End Else Begin
     Result := -1;
     ErrorMsg := 'Not Expecting a '+WhatIs(TempType)+' here.';
     Exit;
  End;
  Result := SIStatement;
End;

Function ParseMERGE(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart: Integer;
  RAMDiskOp: Boolean;
Begin
  RAMDiskOp := False;
  SyntaxTemplate := 'MERGE ['+AnsiChar(16)+AnsiChar(1)+'!'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+'Filename';
  Temptype := PS.GetItemType(Start);
  GlobalExpect := [Ord('!'), SIStrExpr];
  If TempType = Ord('!') then Begin
     SyntaxTemplate := 'ERASE ! '+AnsiChar(16)+AnsiChar(1)+'Filename';
     RAMDiskOp := True;
     Inc(Start);
  End;
  Temptype := GetExprType(Start, PS, 0, SIStrExpr, False);
  If TempType <= 0 Then Begin
     Result := TempType; //error in expression - propagate out
     If RAMDiskOp Then
        ErrorMsg := 'Expecting a String Expression.'
     Else
        ErrorMsg := 'Expecting a "!" or String Expression.';
     Exit;
  End Else Begin
     If Not (TempType in StringTypes) Then Begin
        Result := -1; //error - expecting AnsiString
        If RAMDiskOp Then
           ErrorMsg := 'Expecting a String Expression.'
        Else
           ErrorMsg := 'Expecting a "!" or String Expression.';
        GlobalPos := Start;
        Start := TempStart;
        Exit;
     End;
  End;
  TempType := PS.GetItemType(Start);
  If TempType = SIEol Then Begin
     Result := SIStatement;
     Exit;
  End Else Begin
     Result := -1;
     ErrorMsg := 'Not Expecting a '+WhatIs(TempType)+' here.';
     Exit;
  End;
  Result := SIStatement;
End;

Function ParseGFXFuncs(Var Start: Integer; Key: AnsiString; Params: Array Of AnsiString; PS: TParseStack; NumTerms: Integer): Integer;
Var
  TempType, F, TempStart: Integer;
  OptionalThird, GotColourItems: Boolean;
Begin
  OptionalThird := False;
  //First, check for colour items.
  GotColourItems := False;
  Repeat
     SyntaxTemplate := Key+'['+AnsiChar(16)+AnsiChar(1)+'ColourItems'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1);
     If NumTerms = 4 Then Begin OptionalThird := True; TempStart := 3; End Else TempStart := NumTerms;
     For F := 1 to TempStart Do Begin
        If F = 2 Then SyntaxTemplate := SyntaxTemplate + AnsiChar(16)+AnsiChar(0);
        If F <> TempStart Then
           SyntaxTemplate := SyntaxTemplate + Params[F-1]+AnsiChar(16)+AnsiChar(0)+', '
        Else Begin
           If OptionalThird Then Begin
              SyntaxTemplate := Copy(SyntaxTemplate, 1, Length(SyntaxTemplate)-2);
              SyntaxTemplate := SyntaxTemplate + ' [, '+Params[F-1]+']'
           End Else
              SyntaxTemplate := SyntaxTemplate + Params[F-1];
        End;
     End;
     TempStart := Start;
     TempType := GetExprType(Start, PS, 0, SIUnknown, False);
     If TempType <= 0 Then Begin
        GlobalExpect := [SIColourItem, SINumExpr];
        Result := TempType;
        Exit;
     End;
     If TempType = SIColourItem Then Begin
        SyntaxTemplate := Key+'ColourItem'+AnsiChar(16)+AnsiChar(1)+' Separator'+AnsiChar(16)+AnsiChar(0)+'...';
        TempType := PS.GetItemType(Start);
        If Not (TempType in PrintSeps) Then Begin
           GlobalExpect := [SIPrintSep];
           If TempType <> SIEol Then Result := -1 Else Result := 0;
           ErrorMsg := 'Expecting a Separator here.';
           Exit;
        End Else Begin
           TempType := SIColourItem;
           GotColourItems := True;
           Inc(Start);
        End;
     End Else Begin
        Start := TempStart +1;
     End;
  Until TempType <> SIColourItem;
  //now get up to 3 numexprs, seperated by commas.
  GlobalExpect := [SINumExpr];
  If GotColourItems Then Key := Key + 'ColourItem ';
  Dec(Start);
  Case NumTerms Of
     2: TempType := ParseGenericType(Start, Key, PS, 0, [SINumExpr, SIComma, SINumExpr, SIEol], [Params[0], ', ', Params[1]], SIStatement);
     3: TempType := ParseGenericType(Start, Key, PS, 0, [SINumExpr, SIComma, SINumExpr, SIComma, SINumExpr, SIEol], [Params[0], ', ', Params[1], ',', Params[2]], SIStatement);
     4: TempType := ParseGenericType(Start, Key, PS, 0, [SINumExpr, SIComma, SINumExpr, SIOptional, SIComma, SINumExpr, SIEol], [Params[0], ', ', Params[1], '', ', ', Params[2]], SIStatement);
  End;
  If TempType <= 0 Then Begin
     Result := TempType;
     Exit;
  End Else If TempType <> SIStatement Then Begin
     Result := -1;
     Exit;
  End;
  Result := SIStatement;
End;

Function ParseDataRead(Var Start: Integer; PS: TParseStack; Read: Boolean): Integer;
Var
  TempType, TempStart, LastType: Integer;
  Finished: Boolean;
  Key: AnsiString;
Begin
  Finished := False;
  If Read Then Key := 'READ ' Else Key := 'DATA ';
  Repeat
     If Read Then
        SyntaxTemplate := Key + AnsiChar(16)+AnsiChar(1)+' Var'+AnsiChar(16)+AnsiChar(0)+' [,Var]'
     Else
        SyntaxTemplate := Key + AnsiChar(16)+AnsiChar(1)+' Expression'+AnsiChar(16)+AnsiChar(0)+' [, Expression]';
     TempStart := Start;
     If Not Read Then Begin
        TempType := GetExprType(Start, PS, 0, SIUnknown, False);
        GlobalExpect := [SIAnyExpr];
     End Else Begin
        TempType := PS.GetItemType(Start);
        GlobalExpect := [SIAnyVar];
     End;
     If TempType <= 0 Then Begin
        Result := Temptype;
        Exit;
     End Else Begin
        If Read Then Begin
           If Not (Temptype in Vartypes) Then Begin
              If TempType<> SIEol Then Result := -1 Else Result := 0;
              //error - must be a variable type in READ
              ErrorMsg := 'READ expects variable types.';
              Start := TempStart;
              Exit;
           End;
        End Else Begin
           If Not (TempType in AllExprs) Then Begin
              Result := -1; //error - must be an Expression type in DATA
              ErrorMsg := 'Expecting any expression.';
              GlobalPos := Start;
              Start := TempStart;
              Exit;
           End;
        End;
     End;
     If Read Then Inc(Start);
     If Read Then
        SyntaxTemplate := Key + 'Var ['+AnsiChar(16)+AnsiChar(1)+','+AnsiChar(16)+AnsiChar(0)+' Var]'
     Else
        SyntaxTemplate := Key + 'Expression ['+AnsiChar(16)+AnsiChar(1)+','+AnsiChar(16)+AnsiChar(0)+' Expression]';
     LastType := TempType;
     Temptype := PS.GetItemtype(Start);
     If Temptype = SIEol Then Finished := True Else Begin
        If Read Then
           If TempType = SIOpenBrace Then Begin
              Inc(Start);
              If LastType in [SINumVar, SINumVarSL] Then Begin
                 TempType := ParseNumSubs(Start, PS);
              End Else Begin
                 TempType := ParseStrSubs(Start, PS, LastType);
              End;
              If TempType < 0 Then Begin
                 Result := TempType;
                 Exit;
              End;
              TempType := PS.GetItemType(Start);
           End;
        If TempType = SIEol Then
           Finished := True
        Else
           If Temptype <> SIComma Then Begin
              GlobalExpect := [SIComma];
              Result := -1; //error - Must be comma.
              ErrorMsg := 'Expecting a <comma>';
              Exit;
           End;
        Inc(Start);
     End;
  Until Finished;
  Result := SIStatement;
End;

Function ParseDIM(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempStart, Temptype: Integer;
Begin
  SyntaxTemplate := 'DIM '+AnsiChar(16)+AnsiChar(1)+'Var'+AnsiChar(16)+AnsiChar(0)+' ( Dimensions )';
  TempType := PS.GetItemType(Start);
  If Not (TempType in [4..6]) Then Begin
     If TempType <> SIEol Then Result := -1 Else Result := 0;
     //error - expecting Variable
     GlobalExpect := [SIAnyVarNS];
     ErrorMsg := 'Expecting un-subscripted, single letter Variable type.';
     Exit;
  End Else Begin
     Inc(Start);
     SyntaxTemplate := 'DIM Var'+AnsiChar(16)+AnsiChar(1)+' ('+AnsiChar(16)+AnsiChar(0)+' Dimension )';
     TempType := PS.GetItemType(Start);
     If TempType <> SIOpenBrace Then Begin
        If TempType <> SIEol Then Result := -1 Else Result := 0; //error - expecting Open Bracket
        GLobalExpect := [SIOpenBrace];
        ErrorMsg := 'Expecting <Open Bracket>';
        Exit;
     End Else Begin
        Inc(Start);
        Repeat
           //check next term for numvar
           SyntaxTemplate := 'DIM Var ( '+AnsiChar(16)+AnsiChar(1)+'Dimension'+AnsiChar(16)+AnsiChar(0)+' [, Dimension] )';
           TempStart := Start;
           TempType := GetExprType(Start, PS, 0, SINumVar, False);
           If Temptype <= 0 Then Begin
              Result := TempType;
              GlobalExpect := [SINumExpr];
              Exit;
           End Else Begin
              If Not(TempType in NumericTypes) Then Begin
                 GlobalExpect := [SINumExpr];
                 Result := -1; //error - expecting numeric type
                 ErrorMsg := 'Expecting a Numeric Expression.';
                 GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End Else Begin
                 SyntaxTemplate := 'DIM Var ( Dimension ['+AnsiChar(16)+AnsiChar(1)+','+AnsiChar(16)+AnsiChar(0)+' Dimension] '+AnsiChar(16)+AnsiChar(1)+')';
                 TempType := PS.GetItemType(Start);
                 GLobalExpect := [SIComma, SICloseBrace];
                 If TempType <> SIComma Then Begin
                    If TempType <> SICloseBrace Then Begin
                       If TempType <> SIEol Then Result := -1 Else Result := 0;
                       //error - expecting Comma or CloseBrace
                       ErrorMsg := 'Expecting <Comma> or <Open Bracket>';
                       Exit;
                    End Else Begin
                       Inc(Start);
                       GlobalExpect := [SICloseBrace];
                       If PS.GetItemType(Start) = SIEol Then Begin
                          SyntaxTemplate := 'DIM Var ( Dimensions'+AnsiChar(16)+AnsiChar(1)+' )';
                          Result := SIStatement;
                          Exit;
                       End;
                    End;
                 End Else Begin
                    Inc(Start);
                 End;
              End;
           End;
        Until 1 = 0;
     End;
  End;
End;

Function ParseCopy(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempStart, TempType: Integer;
Begin
  SyntaxTemplate := 'COPY ['+AnsiChar(16)+AnsiChar(1)+'EXP'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'Filename'+AnsiChar(16)+AnsiChar(0)+']';
  TempType := PS.GetItemType(Start);
  GlobalExpect := [IsReserved('EXP')+1000, SIStrExpr];
  If TempType = IsReserved('EXP')+1000 Then Begin
     Inc(Start);
     SyntaxTemplate := 'COPY EXP ['+AnsiChar(16)+AnsiChar(1)+'INVERSE'+AnsiChar(16)+AnsiChar(0)+']';
     TempType := PS.GetItemType(Start);
     GlobalExpect := GlobalExpect - [SIStrExpr];
     If TempType <> IsReserved('INVERSE')+1000 Then Begin
        If TempType <> SIEol Then Result := -1 Else Result := SIStatement;
        If Result = -1 Then ErrorMsg := 'Expecting INVERSE.' Else ErrorMsg := '';
        Exit;
     End Else Begin
        SyntaxTemplate := 'COPY EXP '+AnsiChar(16)+AnsiChar(1)+'INVERSE';
        Inc(Start);
        If PS.GetItemType(Start) <> SIEol Then Begin
           Result := -1;
           ErrorMsg := 'Not expecting a '+WhatIs(PS.GetItemType(Start))+' here.';
           Exit;
        End Else Begin
           Result := SIStatement;
           Exit;
        End;
     End;
  End Else If TempType = SIEol Then Begin
     Result := SIStatement;
     Exit;
  End Else Begin
     TempStart := Start;
     TempType := GetExprType(Start, PS, 0, SIStrExpr, False);
     If TempType <= 0 Then Begin
        Result := TempType;
        Exit;
     End Else If Not (TempType In StringTypes) Then Begin
        Result := -1; //error - expecting AnsiString type
        ErrorMsg := 'Expecting EXP or String Expression.';
        GlobalPos := Start;
        Start := TempStart;
        Exit;
     End Else Begin
        TempType := PS.GetItemType(Start);
        SyntaxTemplate := 'COPY Filename '+AnsiChar(16)+AnsiChar(1)+'TO '+AnsiChar(16)+AnsiChar(0)+'Filename/SCREEN$/LPRINT';
        If TempType <> SI_TO Then Begin
           If TempType <> SIEol Then Result := -1 Else Result := 0;
           ErrorMsg := 'Expecting TO.';
           Exit;
        End Else Begin
           Inc(Start);
           TempType := PS.GetItemType(Start);
           SyntaxTemplate := 'COPY Filename TO '+AnsiChar(16)+AnsiChar(1)+'Filename/SCREEN$/LPRINT';
           If TempType = SIEol Then Begin
              Result := SIUnknown;
              ErrorMsg := 'Expecting SCREEN$, LPRINT or String';
              Exit;
           End Else If (TempType = IsReserved('SCREEN$')+1000) or (TempType = IsReserved('LPRINT')+1000) Then Begin
              If TempType = IsReserved('SCREEN$')+1000 Then Begin
                 SyntaxTemplate := 'COPY Filename TO '+AnsiChar(16)+AnsiChar(1)+'SCREEN$';
              End Else Begin
                 SyntaxTemplate := 'COPY Filename TO '+AnsiChar(16)+AnsiChar(1)+'LPRINT';
              End;
              Inc(Start);
              If PS.GetItemType(Start) <> SIEol Then Begin
                 Result := -1;
                 ErrorMsg := 'Not expecting a '+WhatIs(PS.GetItemType(Start))+' here.';
                 Exit;
              End Else Begin
                 Result := SIStatement;
                 Exit;
              End;
           End Else Begin
              TempStart := Start;
              TempType := GetExprType(Start, PS, 0, SIStrExpr, False);
              If TempType <= 0 Then Begin
                 Result := TempType;
                 Exit;
              End Else Begin
                 If Not (TempType In StringTypes) Then Begin
                    Result := -1; //error - expecting AnsiString type
                    ErrorMsg := 'Expecting SCREEN$, LPRINT or String.';
                    GlobalPos := Start;
                    Start := TempStart;
                    Exit;
                 End Else Begin
                    SyntaxTemplate := 'COPY Filename TO '+AnsiChar(16)+AnsiChar(1)+'Filename';
                    Result := SIStatement;
                 End;
              End;
           End;
        End;
     End;
  End;
End;

Function ParseLIST(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart: Integer;
  Channel: Boolean;
Begin
  Channel := False;
  SyntaxTemplate := 'LIST ['+AnsiChar(16)+AnsiChar(1)+'#'+AnsiChar(16)+AnsiChar(0)+'Channel,]['+AnsiChar(16)+AnsiChar(1)+'Line No'+AnsiChar(16)+AnsiChar(0)+']';
  If PS.GetItemType(Start) = Ord('#') Then Begin
     SyntaxTemplate := 'LIST #'+AnsiChar(16)+AnsiChar(1)+'Channel '+AnsiChar(16)+AnsiChar(0)+'[, Line No]';
     Inc(Start);
     TempStart := Start;
     TempType := GetExprType(Start, PS, 0, SINumExpr, False);
     If TempType <= 0 Then Begin
        Result := TempType;
        Exit;
     End Else Begin
        If Not (Temptype in Numerictypes) Then Begin
           Result := -1; //expecting numexpr.
           ErrorMsg := 'Expecting a Numeric Expression.';
           GlobalPos := Start;
           Start := TempStart;
           Exit;
        End Else Begin
           Temptype := PS.GetItemType(Start);
           If TempType = SIEol Then Begin
              SyntaxTemplate := 'LIST #Channel ['+AnsiChar(16)+AnsiChar(1)+', '+AnsiChar(16)+AnsiChar(0)+' Line No]';
              Result := SIStatement;
              Exit;
           End Else Begin
              SyntaxTemplate := 'LIST #Channel'+AnsiChar(16)+AnsiChar(1)+', '+AnsiChar(16)+AnsiChar(0)+' Line No';
              If Temptype <> SIComma Then Begin
                 If TempType <> SIEol Then Result := -1 Else Result := SIUnknown;
                 ErrorMsg := 'Expecting a <Comma>';
                 Exit;
              End Else Begin Inc(Start); Channel := True; End;
           End;
        End;
     End;
  End Else Begin
     If PS.GetItemType(Start) = SIEol Then Begin
        Result := SIStatement;
        Exit;
     End;
  End;
  If Channel Then SyntaxTemplate := 'LIST #Channel, '+AnsiChar(16)+AnsiChar(1)+'Line No' Else SyntaxTemplate := 'LIST '+AnsiChar(16)+AnsiChar(1)+'Line No';
  TempStart := Start;
  TempType := GetExprType(Start, PS, 0, SINumExpr, False);
  If TempType <= 0 Then Begin
     Result := TempType;
     Exit;
  End Else Begin
     If Not (Temptype in Numerictypes) Then Begin
        Result := -1; //expecting numexpr.
        ErrorMsg := 'Expecting a Numeric Expression.';
        GlobalPos := Start;
        Start := TempStart;
        Exit;
     End Else Begin
        If PS.GetItemtype(Start) <> SIEol Then Begin
           Result := -1; //error - expecting EOL.
           ErrorMsg := 'Not expecting a '+WhatIs(PS.GetItemType(Start))+' here.';
           Exit;
        End;
     End;
  End;
  Result := SIStatement;
End;

Function ParseFormat(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempStart, TempType: Integer;
Begin
  TempType := PS.GetItemType(Start);
  SyntaxTemplate := 'FORMAT '+AnsiChar(16)+AnsiChar(1)+'LINE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'LPRINT'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'Drive';
  If TempType = SIEol Then Begin
     Result := SIUnknown;
     ErrorMsg := 'Expecting LINE, LPRINT or String.';
     Exit;
  End Else If TempType = IsReserved('LINE')+1000 Then Begin
     Inc(Start);
     SyntaxTemplate := 'FORMAT LINE '+AnsiChar(16)+AnsiChar(1)+'Baud Rate';
     TempStart := Start;
     Temptype := GetExprType(Start, PS, 0, SINumExpr, False);
     If TempType <= 0 Then Begin
        Result := TempType;
        Exit;
     End Else Begin
        If Not (Temptype In NumericTypes) Then Begin
           Result := -1; //error - expecting Numexpr
           ErrorMsg := 'Expecting a Numeric Expression.';
           GlobalPos := Start;
           Start := TempStart;
           Exit;
        End;
     End;
  End Else If Temptype = IsReserved('LPRINT')+1000 Then Begin
     Inc(Start);
     Temptype := ParseGenericType(Start, 'FORMAT LPRINT ', PS, 0, [SIStrExpr, SIOptional, SISemiColon, SIStrExpr], ['Fmt-String', '', '; ', ' Fmt-String'], SIStatement);
     If TempType <= 0 Then Begin
        Result := Temptype;
        Exit;
     End;
  End Else Begin
     SyntaxTemplate := 'FORMAT '+AnsiChar(16)+AnsiChar(1)+'LINE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'LPRINT'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'Drive';
     TempStart := Start;
     TempType := GetExprType(Start, PS, 0, SIStrExpr, False);
     If Temptype <= 0 Then Begin
        Result := Temptype;
        Exit;
     End Else If Not (Temptype in StringTypes) Then Begin
        Result := -1; //line, lprint or strexpr
        ErrorMsg := 'Expecting LINE, LPRINT or a String Expression.';
        GlobalPos := Start;
        Start := TempStart;
        Exit;
     End;
  End;
  If PS.GetItemType(Start) <> SIEol Then Begin
     Result := -1;
     ErrorMsg := 'Not Expecting a '+WhatIs(PS.GetItemType(Start))+' here.';
     Exit;
  End;
  Result := SIStatement;
End;

Function ParseLET(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart, VarType: Integer;
Begin
  TempType := PS.GetItemType(Start);
  SyntaxTemplate := 'LET '+AnsiChar(16)+AnsiChar(1)+'Var'+AnsiChar(16)+AnsiChar(0)+' = Expression';
  If Not(TempType in VarTypes) Then Begin
     If TempType = SIEol Then Result := SIUnknown Else Result := -1; //error - variable type required
     ErrorMsg := 'Expecting a Variable Type.';
     Exit;
  End Else Begin
     Inc(Start);
     If Temptype in NumericTypes Then VarType := SINumVar Else VarType := SIStrVar;
     While PS.GetItemType(Start) = SIOpenBrace Do Begin
        Inc(Start);
        If VarType = SINumVar Then TempType := ParseNumSubs(Start, PS) Else Temptype := ParseStrSubs(Start, PS, VarType);
        If TempType <= 0 Then Begin
           Result := TempType;
           Exit;
        End;
     End;
     If VarType = SINumVar Then
        SyntaxTemplate := 'LET Var'+AnsiChar(16)+AnsiChar(1)+' = '+AnsiChar(16)+AnsiChar(0)+'Numeric'
     Else
        SyntaxTemplate := 'LET Var$'+AnsiChar(16)+AnsiChar(1)+' = '+AnsiChar(16)+AnsiChar(0)+'String';
     If PS.GetItemType(Start) <> SIEquals Then Begin
        If PS.GetItemType(Start) = SIEol Then Result := SIUnknown Else Result := -1; //error - expecting '='
        ErrorMsg := 'Expecting an = symbol.';
        Exit;
     End Else Begin
        If VarType = SINumVar Then
           SyntaxTemplate := 'LET Var = '+AnsiChar(16)+AnsiChar(1)+'Numeric'
        Else
           SyntaxTemplate := 'LET Var$ = '+AnsiChar(16)+AnsiChar(1)+'String';
        Inc(Start);
        TempStart := Start;
        If VarType = SINumVar Then
           Temptype := GetExprType(Start, PS, 0, SINumExpr, False)
        Else
           Temptype := GetExprType(Start, PS, 0, SIStrExpr, False);
        If Temptype <= 0 Then Begin
           Result := TempType;
           Exit;
        End Else Begin
           If (VarType = SINumVar) and Not (Temptype In NumericTypes) Then Begin
              Result := -1; //error - Expecting Numeric Type
              ErrorMsg := 'Expecting a Numeric Type.';
              GlobalPos := Start;
              Start := TempStart;
              Exit;
           End Else Begin
              If (VarType = SIStrVar) and Not (Temptype in Stringtypes) Then Begin
                 Result := -1; //error - Expecting AnsiString Type
                 ErrorMsg := 'Expecting a string Type';
                 GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End;
           End;
        End;
     End;
  End;
  Result := SIStatement;
End;

Function ParsePRINTINPUT(Var Start: Integer; PS: TParseStack; INPUT: Boolean): Integer;
Var
  TempType, TempStart, VarType, Expect: Integer;
  GotParam, InBrace: Boolean;
  Key, Item: AnsiString;
Begin
  GotParam := False;
  InBrace := False;
  If INPUT Then Begin Key := 'INPUT '; Item := 'Input '; End else Begin Key := 'PRINT '; Item := 'Print '; End;
  Expect := SIAnyExpr;
  Repeat
     If Expect = SIAnyExpr Then Begin
        If (Input and GotParam) or Not Input Then
           SyntaxTemplate := Key+'['+AnsiChar(16)+AnsiChar(1)+Item+'Item/Separator'+AnsiChar(16)+AnsiChar(0)+']'
        Else
           SyntaxTemplate := Key+AnsiChar(16)+AnsiChar(1)+Item+'Item/Separator';
        If INPUT and (PS.GetItemType(Start) = SIOpenBrace) Then Begin // A bracket here is a PRINT system for the prompt.
           SyntaxTemplate := Key+'('+AnsiChar(16)+AnsiChar(1)+'Prompt Sequence'+AnsiChar(16)+AnsiChar(0)+')';
           If Not InBrace Then Begin
              InBrace := True;
              Inc(Start);
           End;
        End;
        If PS.GetItemType(Start) = Ord('#') Then Begin
           If InBrace Then Begin
              Result := -1;
              ErrorMsg := 'Cannot open a channel in a prompt sequence.';
              SyntaxTemplate := Key +'('+AnsiChar(16)+AnsiChar(2)+'PRINT Items'+AnsiChar(16)+AnsiChar(0)+') [Seperator]';
              GlobalPos := Start;
              Exit;
           End;
           SyntaxTemplate := Key+'#'+AnsiChar(16)+AnsiChar(1)+'Channel'+AnsiChar(16)+AnsiChar(0)+' [Separator]';
           Inc(Start);
           TempStart := Start;
           TempType := GetExprType(Start, PS, 0, SINumExpr, False);
           If Temptype <= 0 Then Begin
              Result := TempType;
              Exit;
           End Else Begin
              If Not (TempType in NumericTypes) Then Begin
                 Result := -1; //error - expecting numeric type
                 ErrorMsg := 'Expecting a Numeric Expression.';
                 SyntaxTemplate := Key+'#'+AnsiChar(16)+AnsiChar(2)+'Channel'+AnsiChar(16)+AnsiChar(0)+' [Separator]';
                 GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End Else Begin
                 Expect := SIPrintSep;
                 GotParam := True;
              End;
           End;
        End Else Begin
           TempType := PS.GetItemType(Start);
           If TempType = IsReserved('LINE')+1000 Then Begin
              If Not INPUT Then Begin
                 Result := -1;
                 ErrorMsg := 'Keyword LINE not allowed in PRINT statements.';
                 Exit;
              End Else If InBrace Then Begin
                 Result := -1;
                 ErrorMsg := 'Cannot open a channel in a prompt sequence.';
                 SyntaxTemplate := Key +'('+AnsiChar(16)+AnsiChar(2)+'PRINT Items'+AnsiChar(16)+AnsiChar(0)+') [Seperator]';
                 GlobalPos := Start;
                 Exit;
              End Else Begin
                 Inc(Start);
                 SyntaxTemplate := Key+'LINE '+AnsiChar(16)+AnsiChar(1)+'Variable'+AnsiChar(16)+AnsiChar(0)+' [Separator]';
                 TempType := PS.GetItemType(Start);
                 If (Not (TempType in VarTypes)) or (TempType In NumericTypes) Then Begin
                    If TempType = SIEol Then Result := 0 Else Result := -1; //error - expecting Variable type
                    ErrorMsg := 'Expecting a String Variable.';
                    Exit;
                 End Else Begin
                    Inc(Start);
                    If PS.GetItemType(Start) = SIOpenBrace Then Begin
                       Inc(Start);
                       If Temptype in NumericTypes Then TempType := ParseNumSubs(Start, PS) Else TempType := ParseStrSubs(Start, PS, VarType);
                       If TempType <= 0 Then Begin
                          Result := TempType; //error in subscript/slicer
                          Exit;
                       End;
                    End;
                    Expect := SIPrintSep;
                    GotParam := True;
                 End;
              End;
           End Else If (TempType in VarTypes) and INPUT and Not InBrace Then Begin
              Inc(Start);
              If PS.GetItemType(Start) = SIOpenBrace Then Begin
                 Inc(Start);
                 If Temptype in NumericTypes Then TempType := ParseNumSubs(Start, PS) Else TempType := ParseStrSubs(Start, PS, VarType);
                 If TempType < 0 Then Begin
                    Result := TempType; //error in subscript/slicer
                    Exit;
                 End;
              End;
              Expect := SIPrintSep;
              GotParam := True;
           End Else If InBrace and (TempType = SICloseBrace) Then Begin
              InBrace := False;
              Inc(Start);
              Expect := SIPrintSep;
           End Else If (TempType <> SIEol) and (Not (TempType in PrintSeps)) Then Begin
              TempStart := Start;
              TempType := GetExprType(Start, PS, 0, SIUnknown, False);
              If TempType <= 0 Then Begin
                 Result := TempType;
                 Exit;
              End Else If Not ((TempType in AllExprs) or (TempType in [SIPrintItem, SIColourItem])) Then Begin
                 Result := -1; //error - illegal expression
                 ErrorMsg := 'Expecting an Expression, Colour Item or Print Item.';
                 GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End;
              Expect := SIPrintSep;
              GotParam := True;
           End Else Begin
              If TempType = SIEol Then Begin
                 If InBrace and INPUT then Begin
                    Result := -1; //error - Prompt sequence
                    ErrorMsg := 'INPUT Prompt sequence not closed.';
                    Exit;
                 End;
                 If (Not GotParam) and INPUT Then Begin
                    Result := SIUnknown; //error - INPUT requires at least one parameter
                    ErrorMsg := 'INPUT Requires at least one parameter.';
                    Exit;
                 End;
                 Result := SIStatement;
                 Exit;
              End;
              Inc(Start);
              GotParam := True;
           End;
        End;
     End Else Begin
        SyntaxTemplate := Key+Item+'Item ['+AnsiChar(16)+AnsiChar(1)+'Separator'+AnsiChar(16)+AnsiChar(0)+']';
        TempType := PS.GetItemType(Start);
        If InBrace And (TempType = SICloseBrace) Then Begin
           Expect := SIAnyExpr;
           GotParam := True;
           InBrace := False;
           Inc(Start);
        End Else If Not (TempType in PrintSeps) Then Begin
           If TempType = SIEol Then Begin
              If InBrace and INPUT then Begin
                 Result := -1; //error - Prompt sequence
                 ErrorMsg := 'INPUT Prompt sequence not closed.';
                 Exit;
              End;
              If (Not GotParam) and INPUT Then Begin
                 Result := -1; //error - INPUT requires at least one parameter
                 ErrorMsg := 'INPUT Requires at least one parameter.';
                 Exit;
              End;
              Result := SIStatement;
              Exit;
           End Else Begin
              Result := -1; //error - expecting print Separator
              ErrorMsg := 'Expecting a Print Separator';
              Exit;
           End;
        End Else Begin
           Expect := SIAnyExpr;
           GotParam := True;
           Inc(Start);
        End;
     End;
  Until 1=0;
End;

Function ParseDEFFN(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, Expect, NumParams, TempStart: Integer;
  Finished: Boolean;
  ExprType, VarType: AnsiString;
Begin
  //First, expect a num/str SL Var.
  SyntaxTemplate := 'DEF FN '+AnsiChar(16)+AnsiChar(1)+'Var'+AnsiChar(16)+AnsiChar(0)+' ( Params ) = Expression';
  TempType := PS.GetItemType(Start);
  If TempType in [4, 6] Then Begin
     If TempType = 4 Then Begin
        Result := SINumExpr;
        ExprType := 'Numeric';
        VarType := 'Var'
     End Else Begin
        Result := SIStrExpr;
        ExprType := 'String';
        VarType := 'Var$';
     End;
     SyntaxTemplate := 'DEF FN '+VarType+' '+AnsiChar(16)+AnsiChar(1)+' ( '+AnsiChar(16)+AnsiChar(0)+'Params ) = '+ExprType;
     //Next, an Open Bracket.
     Inc(Start);
     If PS.GetItemType(Start) = SIOpenBrace Then Begin
        //Next, expecting either a Close Bracket or any variable.
        SyntaxTemplate := 'DEF FN '+VarType+' ( ['+AnsiChar(16)+AnsiChar(1)+'Params'+AnsiChar(16)+AnsiChar(0)+']'+AnsiChar(16)+AnsiChar(1)+' ) '+AnsiChar(16)+AnsiChar(0)+'= '+ExprType;
        Inc(Start);
        NumParams := 0;
        If PS.GetItemType(Start) = SICloseBrace Then Begin
           Inc(Start);
           //End Of DEF FN a()
        End Else Begin
           //expecting one or more params of type anyVar.
           Expect := SIAnyVar;
           Finished := False;
           While Not Finished Do Begin
              Case Expect Of
                 SIAnyVar:
                    Begin
                       If NumParams > 0 Then
                          SyntaxTemplate := 'DEF FN '+VarType+' ( Var, '+AnsiChar(16)+AnsiChar(1)+'Var'+AnsiChar(16)+AnsiChar(0)+' ) = '+ExprType
                       Else
                          SyntaxTemplate := 'DEF FN '+VarType+' ( ['+AnsiChar(16)+AnsiChar(1)+'Params'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+' ) '+AnsiChar(16)+AnsiChar(0)+'= '+ExprType;
                       TempType := PS.GetItemType(Start);
                       If Not (TempType in [SINumVar, SIStrVar, SINumVarSL, SIStrVarSL]) Then Begin
                          If TempType <> SIEol Then Result := -1 Else Result := SIUnknown;
                          //error - Expecting Any Var.
                          ErrorMsg := 'Expecting an un-subscripted Variable.';
                          Exit;
                       End Else Begin
                          Expect := SIComma;
                          Inc(Start);
                          Inc(NumParams);
                       End;
                    End;
                 SIComma:
                    Begin
                       SyntaxTemplate := 'DEF FN '+VarType+' ( Var ['+AnsiChar(16)+AnsiChar(1)+','+AnsiChar(16)+AnsiChar(0)+' Var]'+AnsiChar(16)+AnsiChar(1)+' ) '+AnsiChar(16)+AnsiChar(0)+'= '+ExprType;
                       TempType := PS.GetItemType(Start);
                       If TempType = SIComma Then Begin
                          Expect := SIAnyVar;
                          Inc(Start);
                       End Else If TempType = SICloseBrace Then Begin
                          Finished := True;
                          Inc(Start);
                       End Else Begin
                          If TempType <> SIEol Then Result := -1 Else Result := SIUnknown;
                          //error - Expecting , or )
                          ErrorMsg := 'Expecting a <Comma> or <Close Bracket>';
                          Exit;
                       End;
                    End;
              End;
           End;
        End;
     End Else Begin
        If PS.GetItemType(Start) <> SIEol Then Result := -1 Else Result := SIUnknown;
        //error - expecting '('
        ErrorMsg := 'Expecting <Open Bracket>';
        Exit;
     End;
  End Else Begin
     If TempType <> SIEol Then Result := -1 Else Result := SIUnknown;
     ErrorMsg := 'Expecting a Single-Letter Variable.';
     Exit;
  End;
  If NumParams > 0 Then
     SyntaxTemplate := 'DEF FN '+VarType+' ( Params ) '+AnsiChar(16)+AnsiChar(1)+'= '+AnsiChar(16)+AnsiChar(0)+ExprType
  Else
     SyntaxTemplate := 'DEF FN '+VarType+' ()'+AnsiChar(16)+AnsiChar(1)+' = '+AnsiChar(16)+AnsiChar(0)+ExprType;
  If PS.GetItemType(Start) <> SIEquals Then Begin
     If PS.GetItemType(Start) <> SIEol Then Result := -1 Else Result := SIUnknown;
     //error - expecting =
     ErrorMsg := 'Expecting an '+AnsiChar(39)+'='+AnsiChar(39)+' symbol.';
     Exit;
  End Else Begin
     If NumParams > 0 Then
        SyntaxTemplate := 'DEF FN '+VarType+' ( Params ) = '+AnsiChar(16)+AnsiChar(1)+ExprType
     Else
        SyntaxTemplate := 'DEF FN '+VarType+' () = '+AnsiChar(16)+AnsiChar(1)+ExprType;
     Inc(Start);
     TempStart := Start;
     TempType := GetExprtype(Start, PS, 0, Result, False);
     If TempType <= 0 Then Begin
        Result := TempType;
        Exit;
     End Else Begin
        If ((Result = SINumExpr) and Not (TempType In Numerictypes)) or
           ((Result = SIStrExpr) and Not (TempType In Stringtypes)) Then Begin
           Result := -1; //error - types do not match
           If Result = SINumExpr Then
              ErrorMsg := 'Expecting a Numeric Expression.'
           Else
              ErrorMsg := 'Expecting a String Expression.';
           GlobalPos := Start;
           Start := TempStart;
           Exit;
        End;
        Result := SIStatement;
     End;
  End;
End;

Function ParseLOADSAVE(Var Start: Integer; PS: TParseStack; SAVE: Boolean): Integer;
Var
  TempStart, TempType: Integer;
  RAMDiskOp: Boolean;
  Key: AnsiString;
Begin
  RAMDiskOp := False;
  If SAVE Then Key := 'SAVE ' Else Key := 'LOAD ';
  Result := SIStatement;
  TempStart := Start;
  SyntaxTemplate := Key+'['+AnsiChar(16)+AnsiChar(1)+'!'+AnsiChar(16)+AnsiChar(0)+'] '+AnsiChar(16)+AnsiChar(1)+'Filename '+AnsiChar(16)+AnsiChar(0);
  If SAVE Then
     SyntaxTemplate := SyntaxTemplate + '[LINE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'SCREEN$]'
  Else
     SyntaxTemplate := SyntaxTemplate + '[DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'SCREEN$]';
  // First an optional "!" mark.

  Temptype := PS.GetItemType(Start);
  GlobalExpect := [Ord('!'), SIStrExpr];
  If TempType = Ord('!') then Begin
     SyntaxTemplate := Key+'! '+AnsiChar(16)+AnsiChar(1)+'Filename '+AnsiChar(16)+AnsiChar(0);
     If SAVE Then
        SyntaxTemplate := SyntaxTemplate + '[LINE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'SCREEN$]'
     Else
        SyntaxTemplate := SyntaxTemplate + '[DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(0)+'SCREEN$]';
     RAMDiskOp := True;
     Inc(Start);
  End;

  TempType := GetExprType(Start, PS, 0, SIStrExpr, False);
  If TempType <= 0 Then Begin
     Result := Temptype;
     If RAMDiskOp Then
        ErrorMsg := 'Expecting a String Expression.'
     Else
        ErrorMsg := 'Expecting a "!" or AnsiString Expression.';
     Exit;
  End Else If Not(TempType in StringTypes) Then Begin
     Result := -1; //error - Expecting AnsiString type
     If RAMDiskOp Then
        ErrorMsg := 'Expecting a AnsiString Expression.'
     Else
        ErrorMsg := 'Expecting a "!" or AnsiString Expression.';
     GlobalPos := Start;
     Start := TempStart;
     Exit;
  End;

  //expecting line, data, code, screen$ or EOL
  If RAMDiskOp Then
     SyntaxTemplate := Key+'! Filename ['+AnsiChar(16)+AnsiChar(1)
  Else
     SyntaxTemplate := Key+'Filename ['+AnsiChar(16)+AnsiChar(1);

  If SAVE Then
     SyntaxTemplate := SyntaxTemplate + 'LINE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'SCREEN$'+AnsiChar(16)+AnsiChar(0)+']'
  Else
     SyntaxTemplate := SyntaxTemplate + 'DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'SCREEN$'+AnsiChar(16)+AnsiChar(0)+']';
  TempType := PS.GetItemtype(Start);
  If TempType = SIEol Then Begin
     Result := SIStatement;
     Exit;
  End Else Begin
     SyntaxTemplate := Key+'Filename '+AnsiChar(16)+AnsiChar(1);
     If SAVE Then
        SyntaxTemplate := SyntaxTemplate + 'LINE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'SCREEN$'
     Else
     SyntaxTemplate := SyntaxTemplate + 'DATA'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'CODE'+AnsiChar(16)+AnsiChar(0)+'/'+AnsiChar(16)+AnsiChar(1)+'SCREEN$';

     If TempType = IsReserved('DATA')+1000 Then Begin
        Inc(Start);
        SyntaxTemplate := Key+'Filename DATA '+AnsiChar(16)+AnsiChar(1)+'Var'+AnsiChar(16)+AnsiChar(0)+'()';
        If PS.GetItemType(Start) In VarTypes Then Begin
           Inc(Start);
           SyntaxTemplate := Key+'Filename DATA Var'+AnsiChar(16)+AnsiChar(1)+'('+AnsiChar(16)+AnsiChar(0)+')';
           If PS.GetItemType(Start) = SIOpenBrace Then Begin
              Inc(Start);
              SyntaxTemplate := Key+'Filename DATA Var('+AnsiChar(16)+AnsiChar(1)+')';
              If PS.GetItemType(Start) = SICloseBrace Then Begin
                 Inc(Start);
              End Else Begin
                 If PS.GetItemType(Start) = SIEol Then Result := SIUnknown Else Result := -1;
                 //Error - Expecting )
                 ErrorMsg := 'Expecting <Close Bracket>';
              End;
           End Else Begin
              If PS.GetItemType(Start) = SIEol Then Result := SIUnknown Else Result := -1;
              //error - Expecting (
              ErrorMsg := 'Expecting <Open Bracket>';
           End;
        End Else Begin
           If PS.GetItemType(Start) = SIEol Then Result := SIUnknown Else Result := -1;
           //error - Expecting Variable
           ErrorMsg := 'Expecting a Variable type.';
        End;
        If Result <= 0 Then Exit;
     End Else If Temptype = IsReserved('LINE')+1000 Then Begin
        If Not SAVE Then Begin
           Result := -1;
           ErrorMsg := 'Can'+AnsiChar(39)+'t LOAD with LINE command.';
           Exit;
        End;
        SyntaxTemplate := Key+'Filename LINE '+AnsiChar(16)+AnsiChar(1)+'Number';
        Inc(Start);
        TempStart := Start;
        Temptype := GetExprType(Start, PS, 0, SINumExpr, False);
        If TempType <= 0 Then Begin
           Result := Temptype;
           Exit;
        End Else If Not (Temptype in NumericTypes) Then Begin
           Result := -1; //error - Numeric type expected
           ErrorMsg := 'Expecting a Numeric Expression.';
           GlobalPos := Start;
           Start := TempStart;
           Exit;
        End;
     End Else If Temptype = IsReserved('SCREEN$')+1000 Then Begin
        Inc(Start);
        If Start<PS.NumItems -1 Then SyntaxTemplate := Key+'Filename '+AnsiChar(16)+AnsiChar(2)+'SCREEN$' Else SyntaxTemplate := Key+'Filename SCREEN$';
     End Else If TempType = IsReserved('CODE')+1000 Then Begin
        If Save Then SyntaxTemplate := Key+'Filename CODE '+AnsiChar(16)+AnsiChar(1)+'Address'+AnsiChar(16)+AnsiChar(0)+', Bytes'
        Else SyntaxTemplate := Key+'Filename CODE ['+AnsiChar(16)+AnsiChar(1)+'Address'+AnsiChar(16)+AnsiChar(0)+'] [, Bytes]';
        Inc(Start);
        If (PS.GetItemType(Start) = SIEol) Then begin
           If Not SAVE Then Begin
              Result := SIStatement;
              Exit;
           End Else Begin
              Result := 0; //error - expecting numexpr;
              ErrorMsg := 'Expecting a Numeric Expression';
              Exit;
           End;
        End Else Begin
           TempStart := Start;
           TempType := GetExprType(Start, PS, 0, SINumExpr, False);
           If TempType <= 0 Then Begin
              Result := TempType;
              Exit;
           End Else Begin
              If Not (Temptype In NumericTypes) Then Begin
                 Result := -1; //error - expecting numeric type
                 ErrorMsg := 'Expecting a Numeric Expression';
                 GlobalPos := Start;
                 Start := TempStart;
                 Exit;
              End Else Begin
                 TempType := PS.GetItemType(Start);
                 If Save Then SyntaxTemplate := Key+'Filename CODE Address'+AnsiChar(16)+AnsiChar(1)+','+AnsiChar(16)+AnsiChar(0)+' Bytes'
                 Else SyntaxTemplate := Key+'Filename CODE Address ['+AnsiChar(16)+AnsiChar(1)+', '+AnsiChar(16)+AnsiChar(0)+'Bytes]';
                 If (TempType = SIEol) Then Begin
                    If Not Save Then Begin
                       Result := SIStatement;
                       Exit;
                    End Else Begin
                       Result := SIUnknown;
                       //error - expecting ','
                       ErrorMsg := 'Expecting a <Comma>';
                       Exit;
                    End;
                 End Else Begin
                    If TempType <> SIComma Then Begin
                       Result := -1; //error - expecting comma
                       ErrorMsg := 'Expecting a <Comma>';
                       Exit;
                    End Else Begin
                       Inc(Start);
                       SyntaxTemplate := Key+'Filename CODE Address, '+AnsiChar(16)+AnsiChar(1)+'Bytes';
                       TempStart := Start;
                       TempType := GetExprType(Start, PS, 0, SINumExpr, False);
                       If TempType <= 0 Then Begin
                          Result := Temptype;
                          Exit;
                       End Else Begin
                          If Not(Temptype in NumericTypes) Then Begin
                             Result := -1; //error - expecting Numeric type
                             GlobalPos := Start;
                             Start := TempStart;
                             ErrorMsg := 'Expecting a Numeric Expression';
                             Exit;
                          End;
                       End;
                    End;
                 End;
              End;
           End;
        End;
     End Else Begin
        If PS.GetItemType(Start) <> SIEol Then Result := -1 Else Result := SIUnknown;
        //error - expecting DATA, LINE, SCREEN$ or CODE
        If SAVE Then
           ErrorMsg := 'Expecting DATA, LINE, SCREEN$ or CODE.'
        Else
           ErrorMsg := 'Expecting DATA, SCREEN$ or CODE.';
        Exit;
     End;
  End;
  If PS.GetItemType(Start) <> SIEol Then Begin
     Result := -1; //error - Expecting EOL
     ErrorMsg := 'Not expecting a '+WhatIs(PS.GetItemType(Start))+' here.';
  End Else Begin
     Result := SIStatement;
  End;
End;

// ***************** BONUS ROUTINES *******************
// * These routines are a little extra as an offshoot *
// *   of the functions and procedures in this unit.  *
// *                                                  *
// * HighlightReserved - Inserts bold chars into a    *
// *                     line, bolding the reserved   *
// *                     words in it.                 *
// *                                                  *
// * ColorTextOut - This will draw a line of text     *
// *                containing spectrum style format  *
// *                characters to a canvas.           *
// *                                                  *
// * ParseStringlist - will parse a complete program  *
// *                   for errors, if it is stored    *
// *                   as text in a stringlist.       *
// *                                                  *
// * ParseStackToStringList - Converts a TParseStack  *
// *                          into a list of human    *
// *                          readable terms.         *
// ****************************************************

Function HighlightReserved(Text: AnsiString; HighlightSymbols: Boolean): AnsiString;
Var
  F: Integer;
  NewText: AnsiString;
  InString, REM_Found: Boolean;
Begin
  REM_Found := False;
  InString := False;
  Result := '';
  NewText := '';
  If Length(Text) = 0 Then Exit;
  While Text <> '' Do Begin
     While (Pos(Copy(Text, 1, 1), Alphas+'$') = 0) and (Length(Text) > 0) Do Begin
        If Copy(Text, 1, 1) = '"' Then If InString Then InString := False Else InString := True;
        Result := Result + Copy(Text, 1, 1);
        Text := Copy(Text, 2, 999999);
     End;
     NewText := '';
     While (Pos(Copy(Text, 1, 1), Alphas+'$') > 0) and (Length(Text) > 0) Do Begin
        NewText := NewText + Copy(Text, 1, 1);
        Text := Copy(Text, 2, 999999);
     End;
     If NewText <> '' Then Begin
        If NewText = UpperCase(NewText) Then Begin
           If Not InString and (IsReserved(NewText) <> 0) and Not REM_Found Then Begin
              If NewText = 'REM' Then REM_Found := true;
              Result := Result+AnsiChar(25)+AnsiChar(1)+NewText+AnsiChar(25)+AnsiChar(0);
           End Else Begin
              Result := Result+NewText;
           End;
        End Else Begin
           Result := Result+NewText;
        End;
     End;
  End;
  If HighlightSymbols Then Begin
     NewText := '';
     For F := 1 to length(Result) Do Begin
        Text := Copy(Result, F, 1);
        If (Text = ',') or (Text = ';') or (Text = '#') or (Text = ':') Then Begin
           NewText := NewText+AnsiChar(25)+AnsiChar(1)+Text+AnsiChar(25)+AnsiChar(0);
        End Else Begin
           NewText := NewText + Text;
        End;
     End;
     Result := NewText;
  End;
End;

Procedure ColorTextOut(Canvas: TCanvas; Text: AnsiString; X, Y: Integer);
Var
  F: Integer;
Const
  Colors: Array[0..7] of TColor = (CLBlack, CLNavy, CLMaroon, CLPurple, CLGreen, CLTeal, CLOlive, CLSilver);
Begin
  Canvas.Brush.Style := BsClear;
  For F := 1 to Length(Text) Do Begin
     If Ord(Text[F]) < 31 Then Begin
        If Text[F] = AnsiChar(16) Then Canvas.Font.Color := Colors[Ord(Text[F+1])];
        If Text[F] = AnsiChar(25) Then If FsBold in Canvas.Font.Style Then Canvas.Font.Style := Canvas.Font.Style - [FsBold] Else Canvas.Font.Style := Canvas.Font.Style + [FsBold];
        If Text[F] = AnsiChar(17) Then Canvas.Font.Color := Colors[Ord(Text[F+1])];
     End Else Begin
        If (Text[F] = '{') or (Text[F] = '}') Then Begin
           If Text[F] = '{' Then Begin Canvas.Font.Style := Canvas.Font.Style + [FsItalic]; End;
           If Text[F] = '}' Then Begin Canvas.Font.Style := Canvas.Font.Style - [FsItalic]; End;
        End Else Begin
           Canvas.TextOut(X, Y, Text[F]);
           If FsItalic in Canvas.Font.Style Then Begin
              Canvas.Font.Style := Canvas.Font.Style - [FsItalic];
              Inc(X, Canvas.TextWidth(Text[F]));
              Canvas.Font.Style := Canvas.Font.Style + [FsItalic];
           End Else Inc(X, Canvas.TextWidth(Text[F]));
           If FsBold in Canvas.Font.Style Then Dec(X);
        End;
     End;
  End;
  Canvas.Font.Style := Canvas.Font.Style - [FsBold];
  Canvas.Font.Color := ClBlack;
End;

Function ParseStringlist(List: TStringlist): TParseError;
Var
  F: Integer;
Begin
  With Result Do Begin Error := ''; Syntax := ''; Line := ''; Position := 0; Statement := 0; ErrorCode := 0; LineNum := 0; End;
  For F := 0 To List.Count -1 Do Begin
     Result := ParseCodeLine(List[F], True);
     If Result.ErrorCode <= 0 Then Begin
        if Result.LineNum < 0 Then Result.LineNum := F;
        Exit;
     End;
  End;
  Result.ErrorCode := SIStatement;
End;

Function ParseStackToStringlist(Stack: TParseStack): TStringlist;
Var
  F: Integer;
  S: TParseItem;
Begin
  Result := TStringlist.Create;
  For F := 0 To Stack.NumItems -1 Do Begin
     S := Stack.GetItem(F);
     If S.ItemType < 0 Then Result.Add('ERR:'+IntToStr(-S.ItemType)) Else
        If S.ItemType < 32 Then Result.Add(Types[S.Itemtype]) else
           If S.ItemType < 1000 Then Result.Add(AnsiChar(S.ItemType)) Else
              Result.Add('['+Copy(Keywords[S.Itemtype -1000], 1, Pos('-', Keywords[S.Itemtype -1000]) -1)+']');
     Result[Result.Count -1] := IntToStr(S.Position)+' - '+Result[Result.Count -1];
  End;
End;

Function StripColours(Text: AnsiString): AnsiString;
Var
  F: Integer;
Begin
  Result := '';
  F := 1;
  While F <= Length(Text) Do Begin
     If Text[F] >= ' ' Then Result := Result + Text[F];
     Inc(F);
  End;
End;

Function GetContents(Thing: TThingSet): AnsiString;
Begin
  If SIUnknown in Thing Then Result := Result + WhatIs(0) +' ';
	If SINumLiteral in Thing Then Result := Result + WhatIs(1) +' ';
	If SIStrLiteral in Thing Then Result := Result + WhatIs(2) +' ';
	If SINumVar in Thing Then Result := Result + WhatIs(3) +' ';
	If SINumVarSL in Thing Then Result := Result + WhatIs(4) +' ';
	If SIStrVar in Thing Then Result := Result + WhatIs(5) +' ';

  If SIStrVarSL in Thing Then Result := Result + WhatIs(6) +' ';
	If SIEol in Thing Then Result := Result + WhatIs(7) +' ';
	If SISymbol in Thing Then Result := Result + WhatIs(8) +' ';
	If SITextItem in Thing Then Result := Result + WhatIs(9) +' ';
	If SILessThan in Thing Then Result := Result + WhatIs(10) +' ';
	If SIMoreThan in Thing Then Result := Result + WhatIs(11) +' ';

  If SILessThanEq in Thing Then Result := Result + WhatIs(12) +' ';
	If SIMoreThanEq in Thing Then Result := Result + WhatIs(13) +' ';
	If SINotEqual in Thing Then Result := Result + WhatIs(14) +' ';
	If SIBinNumber in Thing Then Result := Result + WhatIs(15) +' ';
	If SINumExpr in Thing Then Result := Result + WhatIs(16) +' ';
	If SIStrExpr in Thing Then Result := Result + WhatIs(17) +' ';

  If SIFloat in Thing Then Result := Result + WhatIs(18) +' ';
	If SIPrintItem in Thing Then Result := Result + WhatIs(19) +' ';
	If SIColourItem in Thing Then Result := Result + WhatIs(20) +' ';
	If SIPrintSep in Thing Then Result := Result + WhatIs(21) +' ';
	If SIStatement in Thing Then Result := Result + WhatIs(22) +' ';
	If SIUnterminated in Thing Then Result := Result + WhatIs(23) +' ';

  If SIComment in Thing Then Result := Result + WhatIs(24) +' ';
	If SINumVarSubs in Thing Then Result := Result + WhatIs(25) +' ';
	If SIStrVarSubs in Thing Then Result := Result + WhatIs(26) +' ';
	If SIOptional in Thing Then Result := Result + WhatIs(27) +' ';
	If SIAnyVar in Thing Then Result := Result + WhatIs(28) +' ';
	If SIAnyExpr in Thing Then Result := Result + WhatIs(29) +' ';

  If SIStrVarSliced in Thing Then Result := Result + WhatIs(30) +' ';
	If SIAnyVarNS in Thing Then Result := Result + WhatIs(31) +' ';
	If SIAdd in Thing Then Result := Result + WhatIs(43) +' ';
	If SIMinus in Thing Then Result := Result + WhatIs(45) +' ';
	If SIEquals in Thing Then Result := Result + WhatIs(61) +' ';
	If SIOpenBrace in Thing Then Result := Result + WhatIs(40) +' ';

  If SICloseBrace in Thing Then Result := Result + WhatIs(41) +' ';
	If SIPower in Thing Then Result := Result + WhatIs(94) +' ';
	If SIMul in Thing Then Result := Result + WhatIs(42) +' ';
	If SIDiv in Thing Then Result := Result + WhatIs(47) +' ';
	If SIComma in Thing Then Result := Result + WhatIs(44) +' ';
	If SISemiColon in Thing Then Result := Result + WhatIs(59) +' ';
	If SIColon in Thing Then Result := Result + WhatIs(58) +' ';

  If SIApos in Thing Then Result := Result + WhatIs(39) +' ';
	If SIPoint in Thing Then Result := Result + WhatIs(46) +' ';
	If SIAnyVarSL in Thing Then Result := Result + WhatIs(32) +' ';
	If SIColourOrNum in Thing Then Result := Result + WhatIs(33) +' ';
End;

end.




// This version of CAT accepts EXP (should +3 BASIC ever be supported...)

{
Function ParseCAT(Var Start: Integer; PS: TParseStack): Integer;
Var
  TempType, TempStart, rEXP: Integer;
  Channel, StringThere: Boolean;
Begin
  Channel := False; StringThere := False;
  rEXP := 1000 + IsReserved('EXP');
  SyntaxTemplate := 'CAT ['+AnsiChar(16)+AnsiChar(1)+'!/#/Filespec/EXP'+AnsiChar(16)+AnsiChar(0)+']';
  Temptype := PS.GetItemType(Start);
  If TempType = SIEol Then Begin Result := SIStatement; Exit; End;
  GlobalExpect := [Ord('!'), Ord('#'), SIStrExpr, rEXP];
  If TempType = Ord('!') then Begin
     SyntaxTemplate := 'CAT !';
     Inc(Start);
     TempType := PS.GetItemType(Start);
     GlobalExpect := [Ord('!')];
     If TempType <> SIEol Then Begin
        Result := -1;
        ErrorMsg := 'Not Expecting a '+Whatis(Temptype)+' here.';
        Exit;
     End Else Begin
        Result := SIStatement;
        Exit;
     End;
  End;
  If TempType = Ord('#') Then Begin
     Inc(Start);
     SyntaxTemplate := 'CAT #'+AnsiChar(16)+AnsiChar(1)+'Channel '+AnsiChar(16)+AnsiChar(0)+'[, FileSpec/EXP]';
     TempStart := Start;
     GlobalExpect := [SINumExpr];
     Temptype := GetExprType(Start, PS, 0, SINumExpr, False);
     If TempType <= 0 Then Begin
        Result := TempType; //error in expression - propagate out
        Exit;
     End Else Begin
        If Not (TempType in NumericTypes) Then Begin
           Result := -1; //error - expecting numeric
           ErrorMsg := 'Expecting a Numeric Expression.';
           GlobalPos := Start;
           Start := TempStart;
           Exit;
        End;
     End;
     Channel := True;
     TempType := PS.GetItemType(Start);
     SyntaxTemplate := 'CAT #Channel ['+AnsiChar(16)+AnsiChar(1)+', FileSpec/EXP'+AnsiChar(16)+AnsiChar(0)+']';
     If TempType = SIComma Then Begin
        SyntaxTemplate := 'CAT #Channel, '+AnsiChar(16)+AnsiChar(1)+'FileSpec'+AnsiChar(16)+AnsiChar(0)+' [EXP]';
        Inc(Start);
        TempStart := Start;
        GlobalExpect := [SIStrExpr];
        Temptype := GetExprType(Start, PS, 0, SIStrExpr, False);
        If TempType <= 0 Then Begin
           Result := TempType;
           Exit;
        End Else Begin
           If Not (TempType in StringTypes) Then Begin
              Result := -1; //error - expecting AnsiString type
              ErrorMsg := 'Expecting AnsiString Expression.';
              GlobalPos := Start;
              Start := TempStart;
              Exit;
           End;
        End;
        StringThere := True;
        SyntaxTemplate := 'CAT #Channel, FileSpec ['+AnsiChar(16)+AnsiChar(1)+'EXP'+AnsiChar(16)+AnsiChar(0)+']';
     End Else If TempType <> SIEol Then Begin
        SyntaxTemplate := 'CAT #Channel ['+AnsiChar(16)+AnsiChar(1)+'EXP'+AnsiChar(16)+AnsiChar(0)+']';
     End Else Begin
        Result := SIStatement;
        Exit;
     End;
  End Else Begin
     SyntaxTemplate := 'CAT ['+AnsiChar(16)+AnsiChar(1)+'FileSpec/EXP'+AnsiChar(16)+AnsiChar(0)+']';
     GlobalExpect := [rEXP, SIStrExpr];
     If TempType <> rEXP Then Begin
        If TempType = SIEol Then Begin
           Result := SIStatement;
           Exit;
        End;
        TempStart := Start;
        GlobalExpect := [SIStrExpr];
        Temptype := GetExprType(Start, PS, 0, SIStrExpr, False);
        If TempType <= 0 Then Begin
           Result := TempType;
           Exit;
        End Else Begin
           If Not (TempType in StringTypes) Then Begin
              Result := -1;
              GlobalPos := Start;
              Start := TempStart;
              ErrorMsg := 'Expecting AnsiString Expression.';
              Exit;
           End;
        End;
        StringThere := True;
        SyntaxTemplate := 'CAT FileSpec ['+AnsiChar(16)+AnsiChar(1)+'EXP'+AnsiChar(16)+AnsiChar(0)+']';
     End Else Begin
        SyntaxTemplate := 'CAT EXP';
        Inc(Start);
        TempType := PS.GetItemType(Start);
        GlobalExpect := [rEXP];
        If TempType <> SIEol Then Begin
           Result := -1;
           ErrorMsg := 'Not Expecting a '+Whatis(Temptype)+' here.';
           Exit;
        End Else Begin
           Result := SIStatement;
           Exit;
        End;
     End;
  End;
  If Channel and Not StringThere Then SyntaxTemplate := 'CAT #Channel ';
  If Channel and StringThere Then SyntaxTemplate := 'CAT #Channel, FileSpec ';
  If Not Channel and Not StringThere Then SyntaxTemplate := 'CAT ';
  If Not Channel and StringThere Then SyntaxTemplate := 'CAT FileSpec ';
  TempType := PS.GetItemType(Start);
  If TempType = SIEol Then Begin
     SyntaxTemplate := SyntaxTemplate + '['+AnsiChar(16)+AnsiChar(1)+'EXP'+AnsiChar(16)+AnsiChar(0)+']';
     Result := SIStatement;
     Exit;
  End Else If TempType = rEXP Then Begin
     SyntaxTemplate := SyntaxTemplate + AnsiChar(16)+AnsiChar(1)+'EXP';
     Inc(Start);
     TempType := PS.GetItemType(Start);
     If TempType <> SIEol Then Begin
        Result := -1;
        ErrorMsg := 'Not Expecting a '+WhatIs(TempType)+' here.';
        Exit;
     End;
     Result := SIStatement;
  End Else Begin
     SyntaxTemplate := SyntaxTemplate + AnsiChar(16)+AnsiChar(1)+'EXP';
     Result := -1;
     ErrorMsg := 'Expecting optional EXP.';
     Exit;
  End;
End;
}
