unit Compiler;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls,
  ExtCtrls, StdCtrls, FileCtrl, ClipBrd, Math;

  Procedure Compile;
  Function  CompileLine(LineLabel, BASIC: String): String;
  Procedure AddAsm(AsmText: String);
  Procedure AddLibs;
  Procedure AllocVars;
  Procedure AddStringConstants;
  Procedure FormatComments;

  // Generic Functions

  Function  GetExpression(BASIC: String; var Index: Integer): String;
  Function  GetPriority(Operator: String): Integer;
  Function  GetStringConstant(BASIC: String; var Index: Integer): String;
  Function  GetVarName(BASIC: String; var Index: Integer): String;
  Function  GetNumber(BASIC: String; var Index: Integer): String;
  Function  Get5Bytes(Value: Extended): String;
  Function  Byte5ToFloat(Exponent: Byte; Mantissa: DWord): Extended;
  Function  BuildExpressionAsm(ItemStack: TStringlist): String;

  // BASIC Keywords

  Function  CompileFOR(BASIC: String; Var Index: Integer): String;
  Function  CompileNEXT(BASIC: String; var Index: Integer): String;
  Function  CompilePOKE(BASIC: String; Var Index: Integer): String;
  Function  CompileBORDER(BASIC: String; Var Index: Integer): String;
  Function  CompileGOTO(BASIC: String; var Index: Integer): String;
  Function  CompileLET(BASIC: String; var Index: Integer): String;
  Function  CompilePAUSE(BASIC: String; Var Index: Integer): String;
  Function  CompileDIM(BASIC: String; var Index: Integer): String;

  // BASIC Functions

  Function  CompilePEEK: String;
  Function  CompileRND: String;
  Function  CompileOR:  String;
  Function  CompileNOT: String;
  Function  CompilePI:  String;
  Function  CompileINT: String;
  Function  CompileCHRStr: String;

Var

  CompileLabel: String;
  InputStream: String;
  Variables: TStringlist;
  Lines: TStringlist;
  Strings: TStringlist;
  LineNumbers: TStringlist;
  AsmOutput: TStringlist;

  Flags: Array[0..999] of Boolean;

Const

  // Flags which tell the AddLibs() function which library functions
  // are used in the assembly.

  Use_GetStackedNumber       = 0;
  Use_GetTwoStackedNumbers   = 1;
  Use_ReclaimStackItem       = 2;
  Use_StackShortInt          = 3;
  Use_NegateStackItem        = 4;
  Use_RNDReplacement         = 5;
  Use_UpdateNumVar           = 6;
  Use_UpdateStrVar           = 7;
  Use_StackNumericVar        = 8;
  Use_UpdateJumpTable        = 9;
  Use_CleanWorkSpace         = 10;
  Use_StackStringVar         = 11;
  Use_UpdateFORVar           = 12;
  Use_DoNext                 = 13;
  Use_PerformDIM             = 14;

implementation

Uses BASinMain, ROMUtils, FastCore, BASSupport, Utility, AsmEditor;

Procedure Compile;
Var
  Idx, Index: Integer;
  LineNum, StatementNum: Word;
  LineLen: Word;
  CurLine, NewStr, AsmText: String;
  InString: Boolean;
Begin

  // Is there a program to compile?

  If GetWord(@Memory[PROG]) = GetWord(@Memory[VARS]) Then Begin

     MessageBox(BASinOutput.Handle, pChar('There is currently no BASIC program'#13'in memory.'), pChar('Compiler Error'), MB_OK or MB_ICONINFORMATION);
     Exit;

  End;

  // Initialise the internal storage and get the current program.

  For Idx := 0 To 999 Do
     Flags[Idx] := False;

  // These are various storage areas which hold variable names, lines of BASIC and other
  // such resources.

  Variables.Clear;
  Lines.Clear;
  LineNumbers.Clear;
  Strings.Clear;
  AsmOutput.Clear;

  InputStream := GetMemoryString(GetWord(@Memory[PROG]), GetWord(@Memory[VARS]) - GetWord(@Memory[PROG]), Memory);

  // Now split the program up into lines

  Idx := 1;

  Repeat
     CurLine := '';
     LineNum := Ord(InputStream[Idx+1])+(Ord(InputStream[Idx]) Shl 8);
     Inc(Idx, 2);
     LineLen := GetWord(@InputStream[Idx]);
     Inc(Idx, 2);
     CurLine := StripWhiteSpace(Strip5Bytes(Copy(InputStream, Idx, LineLen)));
     Inc(Idx, LineLen);
     Index := 1;
     InString := False;
     While Index <= Length(CurLine) Do Begin
        If CurLine[Index] = '"' Then InString := Not InString;
        If (Not InString) and (CurLine[Index] in ['A'..'Z']) Then
           CurLine[Index] := Chr(Ord(CurLine[Index])+32);
        Inc(Index);
     End;

     // Got the line, now split up into statements if necessary.

     Index := 1;
     StatementNum := 0;
     InString := False;
     While Index <= Length(CurLine) Do Begin
        If CurLine[Index] = '"' Then InString := Not InString;
        If Not InString and (CurLine[Index] = ':') Then Begin
           Lines.Add(Copy(CurLine, 1, Index -1));
           If StatementNum = 0 Then
              LineNumbers.Add(IntToStr(LineNum))
           Else
              LineNumbers.Add(IntToStr(LineNum)+'_'+IntToStr(StatementNum));
           Inc(StatementNum);
           CurLine := Copy(CurLine, Index+1, 9999);
           Index := 0;
        End;
        Inc(Index);
     End;

     // Store the lines now - statements take the form <line number>_<statement number>

     If Length(CurLine) > 0 Then Begin
        Lines.Add(CurLine);
        If StatementNum = 0 Then
           LineNumbers.Add(IntToStr(LineNum))
        Else Begin
           LineNumbers.Add(IntToStr(LineNum)+'_'+IntToStr(StatementNum));
        End;
     End;
  Until Idx >= Length(InputStream);

  // Now search for LET, FOR and DIM to gather up Variable names.
  // Also get all "" pairs to define strings

  For LineNum := 0 To Lines.Count -1 Do Begin

     Idx := 1;
     CurLine := Lines[LineNum];

     Repeat
        If CurLine[Idx] = '"' Then Begin // Hardcoded String Constant
           Inc(Idx);
           Strings.Add(GetDelimitedString(CurLine, Idx, '"'));
        End;
        If (Idx = 1) or ((Idx > 1) and (CurLine[Idx-1] = ':')) Then Begin
           If CurLine[Idx] = #241 Then Begin // LET
              Inc(Idx);
              NewStr := GetVarName(CurLine, Idx);
              If Variables.IndexOf('*'+NewStr) = -1 Then
                 If Variables.IndexOf('F'+NewStr) = -1 Then // Is this var already declared as a FOR var?
                    Variables.Add('*'+NewStr);
           End Else
                 If CurLine[Idx] = #235 Then Begin // FOR
                    Inc(Idx);
                    NewStr := GetVarName(CurLine, Idx);
                    If Variables.IndexOf('F'+NewStr) = -1 Then Begin
                       If Variables.IndexOf('*'+NewStr) <> -1 Then // If this var was already declared as a numeric, delete it.
                          Variables.Delete(Variables.IndexOf('*'+NewStr));
                       Variables.Add('F'+NewStr);
                    End;
                 End Else
                    If CurLine[Idx] = #233 Then Begin // DIM
                       Inc(Idx);
                       NewStr := GetVarName(CurLine, Idx);
                       If Variables.IndexOf('A'+NewStr) = -1 Then
                          Variables.Add('A'+NewStr);
                    End;
        End;
        Inc(Idx);
     Until Idx >= Length(CurLine);

  End;

  // First Phase is over, now start to compile, line by line.

  For Idx := 0 To Lines.Count -1 Do Begin

     NewStr := CompileLine(LineNumbers[Idx], Lines[Idx]);
     AddAsm(NewStr);

  End;

  // And add the final RET that will return us to BASIC

  AsmOutput.Add('   RET   ; END OF PROGRAM');
  AsmOutput.Add('');

  // And add any library routines (such as variable handlers and other bits'n'bobs)

  AddLibs;
  AllocVars;
  AddStringConstants;

  // Prettify the comments. Automatically commented code. How cool is that?

  FormatComments;

  // Now send the results to our assembler window.

  AsmText := '';
  For Idx := 0 To AsmOutput.Count -1 Do
     AsmText := AsmText + AsmOutput[Idx] + #13;

  AsmEditorWindow.NewFromText('CompiledBASIC.asm', AsmOutput);
  ShowWindow(AsmEditorWindow, False);

End;

Procedure AddAsm(AsmText: String);
Begin

  // This should be called instead of just simply adding to the
  // ASM stringlist - it handles multiple lines (seperated by #13's) properly.

  If Copy(AsmText, Length(AsmText), 1) <> #13 Then
     AsmText := AsmText + #13;

  While AsmText <> '' Do Begin
     AsmOutput.Add(Copy(AsmText, 1, Pos(#13, AsmText)-1));
     AsmText := Copy(AsmText, Pos(#13, AsmText)+1, 99999);
  End;

End;

Procedure FormatComments;
Var
  Idx, CommentPos: Integer;
  S: String;
Begin

  // Lines up the comments to column 40, which gives just enough room for
  // the asm to rest to the left of them. For no other reason other than it
  // looks lovely.

  For Idx := 0 To AsmOutput.Count -1 Do Begin
     S := AsmOutput[Idx];
     CommentPos := Pos(';', S);
     If CommentPos > 0 Then Begin
        While CommentPos < 40 Do Begin
           S := Copy(S, 1, CommentPos-1)+' '+Copy(S, CommentPos, 9999);
           CommentPos := Pos(';', S);
        End;
        AsmOutput[Idx] := S;
     End;
  End;
End;


Function CompileLine(LineLabel, BASIC: String): String;
Var
  Index: Integer;
Begin

  // Compiles a single line of BASIC code. Because Statements are treated as#
  // Seperate lines of code, we only have to handle the initial keyword.

  CompileLabel := LineLabel;
  Result := 'Line'+LineLabel+':    ; '+FormatEscapes(DetokeniseLine(BASIC, False))+#13;

  Index := 1;
  Repeat
     // Get the keyword
     Case BASIC[Index] Of
        #231: // BORDER
           Begin
              Result := Result + CompileBORDER(BASIC, Index) +#13;
           End;
        #233: // DIM
           Begin
              Result := Result + CompileDIM(BASIC, Index) +#13;
           End;
        #235: // FOR
           Begin
              Result := Result + CompileFOR(BASIC, Index) +#13;
           End;
        #236: // GO TO
           Begin
              Result := Result + CompileGOTO(BASIC, Index) +#13;
           End;
        #241: // LET
           Begin
              Result := Result + CompileLET(BASIC, Index) +#13;
           End;
        #242: // PAUSE
           Begin
              Result := Result + CompilePAUSE(BASIC, Index) +#13;
           End;
        #243: // NEXT
           Begin
              Result := Result + CompileNEXT(BASIC, Index) +#13;
           End;
        #244: // POKE
           Begin
              Result := Result + CompilePOKE(BASIC, Index) +#13;
           End;
     End;
     Inc(Index);
  Until Index > Length(BASIC);

  If Flags[Use_CleanWorkSpace] Then
     Result := Result + #13 + '   CALL CleanWorkspace'#13;

End;

Function GetExpression(BASIC: String; Var Index: Integer): String;
Var
  OperatorStack: TStringlist;
  OperandStack: TStringlist;
  OperatorPri, Idx: Integer;
  Item: String;
  Done, PriDone, ExpectOperand: Boolean;
Begin

  // One of the main routines of the compiler - this will take an input stream
  // and begin compiling an expression. For example, a BORDER command takes a numeric
  // expression - this routine will get that expression, no matter how complex.

  // It does this by maintaining two stacks which create a modified RPN sequence of
  // forth-like "commands", each of which can be compiled seperately. This results in
  // a slightly sub-optimal stack-based assembly, but it works very well indeed.

  OperatorStack := TStringlist.Create;
  OperandStack := TStringlist.Create;

  Done := False;
  ExpectOperand := True;

  While Not Done Do Begin
     Item := '';
     Case BASIC[Index] Of
        '"':
           Begin // Found a string constant, so identify it's index into the LUT (See later).
              Item := '"'+GetStringConstant(BASIC, Index);
           End;
        '0'..'9', '.':
           Begin // Stack a number
              Item := '!'+GetNumber(BASIC, Index);
           End;
        'a'..'z':
           Begin // alphabetics not in strings must be variables.
              Item := GetVarName(BASIC, Index);
           End;
     Else
        Begin
           Item := BASIC[Index];
           If ExpectOperand And (Item[1] in ['-', '+']) Then Begin
              // Handle unary +/- (expecting an Operand, not an Operator).
              Item := '!'+GetNumber(BASIC, Index);
              If Item = '!-' Then
                 Item := '-';
           End Else
              Inc(Index);
        End;
     End;

     If Item[1] in ['!', '"', 'a'..'z'] Then Begin
        // Simple terms, just send them to the Operand Stack.
        OperandStack.Add(Item);
        // And we are no longer expecting an operand.
        ExpectOperand := False;
     End Else Begin
        If Item = '(' Then Begin
           If ExpectOperand Then
              // Begin the Open Bracket sequence.
              OperatorStack.Add(Item)
           Else Begin
              // A bracket when we are expecting an operator *must*
              // Be either a string slicer or an array parameter.

           End;
        End Else Begin
           If Item = ')' Then Begin
              // If we have no matching brackets on the Operator Stack, then this is
              // likely to be a terminating bracket from somewhere else - leave it alone and exit.
              If OperatorStack.Count = 0 Then
                 Done := True
              Else Begin
                 // Otherwise, this is a closing bracket. Remove all Operators stacked, onto the Operand Stack
                 // Since the initial open bracket was added. The Operand Stack can be seen as the final
                 // "result" stack in this sense.
                 While OperatorStack[OperatorStack.Count -1] <> '(' Do Begin
                    OperandStack.Add(OperatorStack[OperatorStack.Count -1]);
                    OperatorStack.Delete(OperatorStack.Count -1);
                 End;
                 // And remove the OpenBracket, it's no longer needed.
                 OperatorStack.Delete(OperatorStack.Count -1);
              End;
           End Else Begin
              // Any other items we come across must be functions or operators.
              // if they have priority, then they get added to the "result" or Operator Stack
              // Else, they get sorted into the OperandStack - by shifting out all operands that
              // have a higher priority.
              OperatorPri := GetPriority(Item);
              If (OperatorStack.Count = 0) and (OperatorPri > -1) Then Begin
                 OperatorStack.Add(Item);
                 ExpectOperand := True;
              End Else Begin
                 PriDone := OperatorPri = -1;
                 While Not PriDone Do Begin
                    If OperatorStack.Count = 0 Then begin
                       PriDone := True;
                    End Else Begin
                       If OperatorPri < GetPriority(OperatorStack[OperatorStack.Count -1]) Then Begin
                          OperandStack.Add(OperatorStack[OperatorStack.Count -1]);
                          OperatorStack.Delete(OperatorStack.Count -1);
                       End Else Begin
                          PriDone := True;
                       End;
                    End;
                 End;
                 If OperatorPri > -1 Then Begin
                    OperatorStack.Add(Item);
                    ExpectOperand := True;
                 End Else Begin
                    Dec(Index);
                    Done := True;
                 End;
              End;
           End;
        End;
     End;

     If Index > Length(BASIC) Then Done := True;

  End;

  // After all that rather complex juggling, we have an Operator Stack that stores
  // All remaining, prioritised, items. We need to tack those onto the end of the Operand Stack.

  For Idx := OperatorStack.Count -1 DownTo 0 Do
     OperandStack.Add(OperatorStack[Idx]);

  // And now we "compile" this resulting Stack of commands and items
  // into z80 Asm, one by one.

  Result := BuildExpressionAsm(OperandStack);

  OperandStack.Free;
  OperatorStack.Free;

End;

Function GetPriority(Operator: String): Integer;
Begin

  // Returns the priority of the current "operator" in relation
  // to other operators. Used by the GetExpression function.

  Case Operator[1] of
     #165:                      Result := 11; // RND
     #167:                      Result := 11; // PI
     #186:                      Result := 11; // INT
     #190:                      Result := 11; // PEEK
     #194:                      Result := 11; // CHR$
     '^':                       Result := 10; // Power
     '*', '/':                  Result := 8;  // MultOps
     '+', '-':                  Result := 6;  // AddOps
     '=', '>', '<', #199..#201: Result := 5;  // RelOps
     #195:                      Result := 4;  // NOT
     #198:                      Result := 3;  // AND
     #197:                      Result := 2;  // OR
  Else
     Result := -1;
  End;

End;

Function GetStringConstant(BASIC: String; var Index: Integer): String;
Begin

  // Gets the string constant in the input stream, and then
  // searches the pre-built array of strings to get its' index,
  // which is returned.

  Inc(Index);
  Result := GetDelimitedString(BASIC, Index, '"');
  If Strings.IndexOf(Result) > -1 Then
     Result := IntToStr(Strings.IndexOf(Result));
  Inc(Index);

End;

Function GetVarName(BASIC: String; var Index: Integer): String;
Begin

  // Versatile, in that it not only identifies the variable
  // it also performs a quick check for arrays - but not slicers.

  Result := '';
  While BASIC[Index] in ['a'..'z', '0'..'9', '$'] Do Begin
     If BASIC[Index] <> '$' Then
        Result := Result + BASIC[Index]
     Else
        Result := Result + '_str';
     Inc(Index);
     If Index > Length(BASIC) Then Break;
  End;
  If BASIC[Index] = '(' Then Begin
     If Pos('_str', Result) = 0 Then
        Result := Result + '_array'
  End;
End;

Function GetNumber(BASIC: String; var Index: Integer): String;
Var
  Number, Fraction, Exponent: Extended;
  GotNumber, EXPSign, Sign: Boolean;
Begin

  // Extracts a number as a 5 Byte float from the input stream.
  // Handles all spectrum numbers, including the e+/- form.
  // If there's not a valid number, it bails out with a "-" result.

  GotNumber := False;
  Number := 0;
  Sign := True;

  // Handle unary +/-

  While BASIC[Index] in ['-', '+'] Do Begin
     If BASIC[Index] = '-' Then
        Sign := Not Sign;
     Inc(Index);
  End;

  // Get the whole number (integer) part

  While BASIC[Index] In ['0'..'9'] Do Begin
     Number := (Number * 10)+(Ord(BASIC[Index])-48);
     GotNumber := True;
     Inc(Index);
  End;

  // Check for a fraction

  If BASIC[Index] = '.' Then Begin
     Fraction := 1;
     Inc(Index);
     While BASIC[Index] In ['0'..'9'] Do Begin
        GotNumber := True;
        Fraction := Fraction / 10;
        Number := Number + (Fraction*(Ord(BASIC[Index])-48));
        Inc(Index);
     End;
  End;

  // Test for "e", ie 1.03e-4

  If (BASIC[Index] = 'e') or (BASIC[Index] = 'E') Then Begin
     Inc(Index);
     Exponent := 0;
     EXPSign := True;
     If BASIC[Index] = '+' Then
        Inc(Index)
     Else If BASIC[Index] = '-' Then Begin
        Inc(Index);
        EXPSign := False;
     End;
     While BASIC[Index] in ['0'..'9'] Do Begin
        Exponent := (Exponent *10)+(Ord(BASIC[Index])-48);
        GotNumber := True;
        Inc(Index);
     End;
     If EXPSign Then
        Number := Number * Power(10, Exponent)
     Else
        Number := Number / Power(10, Exponent);
  End;

  // Apply the unary minus, if necessary

  If Not Sign Then number := -Number;

  // And return the number in 5 bytes, or a "-" for an error.
  // The error is not likely, as all code should have been parsed by the
  // ROM already.

  If Not GotNumber Then Begin
     If Not Sign Then Result := '-';
  End Else Begin
     Result := Get5Bytes(Number);
  End;

End;

Function BuildExpressionAsm(ItemStack: TStringlist): String;
Var
  Byte5Number, S, DummyStack: String;
  Idx, NumStacked: Integer;
Begin

  // It is here that the expression stacks generated by GetExpression() are
  // compiled into z80 asm text.

  // Each item gets compiled - any numbers are stacked, variables are read from
  // storage in VARS and the results stacked, etc
  // A "dummy stack" is kept for negation purposes and other reasons.

  Result := '';
  DummyStack := '';
  NumStacked := 0;
  For Idx := 0 To ItemStack.Count -1 Do Begin
     S := ItemStack[Idx];
     Case S[1] Of
        '"':
           Begin
              // Strings are treated as vars - they're stored at the
              // end of the code as DEFBs.
              Result := Result + '   LD   HL,String'+S[2]+#13;
              S := Strings[StrToInt(S[2])];
              If Length(S) > 25 Then
                 S := Copy(S, 1, 25)+'...';
              Result := Result + '   CALL StackStringVar   ; Stack String Const ("'+S+'")'+#13#13;
              Flags[Use_StackStringVar] := True;
              DummyStack := '$'+DummyStack;
              Inc(NumStacked);
           End;
        'a'..'z':
           Begin
              // Begin by setting up a pointer to the Variable
              // which can be obtained by reading from the Vars Jump LUT at the end of the code.
              Result := Result + '   LD   HL,(Var_'+S+')   ; Address the var in VARS area'+#13;
              // Now just call the relevant stacking routine.
              If Pos('_str', S) = 0 Then Begin
                 Result := Result + '   CALL StackNumericVar'+#13#13;
                 Flags[Use_StackNumericVar] := True;
                 DummyStack := '#'+DummyStack;
              End Else Begin
                 Result := Result + '   CALL StackStringVar'+#13#13;
                 Flags[Use_StackStringVar] := True;
                 DummyStack := '$'+DummyStack;
              End;
              Inc(NumStacked);
           End;
        '!':
           Begin
              // Numbers are stacked as is, but a neat little optimisation is used for
              // stacking Short integers.
              Byte5Number := Copy(S, 2, 99999);
              If (GetByte(@Byte5Number[1]) > 0) or (Ord(Byte5Number[4]) > 0) Then Begin
                 Result := Result + '   LD   A,'+IntToStr(GetByte(@Byte5Number[1]))+#13;
                 Result := Result + '   LD   DE,'+IntToStr(GetWord(@Byte5Number[2]))+#13;
                 Result := Result + '   LD   BC,'+IntToStr(GetWord(@Byte5Number[4]))+#13;
                 Result := Result + '   CALL $2AB6   ; [ROM] STACK-STORE ('+FloatToStrEx(Byte5ToFloat(GetByte(@Byte5Number[1]), GetDWord(@Byte5Number[2])))+')'+#13#13;
              End Else Begin
                 Result := Result + '   LD   DE,'+IntToStr(GetWord(@Byte5Number[2]))+#13;
                 Result := Result + '   CALL StackShortInt ; Stack Signed Word ('+FloatToStrEx(Byte5ToFloat(GetByte(@Byte5Number[1]), GetDWord(@Byte5Number[2])))+')'+#13#13;
                 Flags[Use_StackShortInt] := True;
              End;
              DummyStack := '#'+DummyStack;
              Inc(NumStacked);
           End;
        '+':
           Begin
              // If this addition is called and nothing is on the dummy stack,
              // then it's a unary positive, and can be ignored.
              If NumStacked > 1 Then Begin
                 // Numbers are added by setting up HL and DE to point to the
                 // last two numbers on the stack.
                 // Strings are concatenated by the ROM itself.
                 If Copy(DummyStack, 1, 2) = '##' Then Begin
                    Flags[Use_GetTwoStackedNumbers] := True;
                    Result := Result + '   CALL GetTwoStackedNumbers'#13;
                    Result := Result + '   CALL $3014        ; [ROM] Call the Addition routine.'#13;
                    Result := Result + '   CALL ReclaimStackItem'#13#13;
                    Flags[Use_ReclaimStackItem] := True;
                    DummyStack := Copy(DummyStack, 2, 9999);
                    Dec(NumStacked);
                 End Else Begin
                    Result := Result + '   CALL $359C        ; [ROM] Perform the Addition.'#13#13;
                    DummyStack := Copy(DummyStack, 2, 9999);
                    Dec(NumStacked);
                 End;
              End;
           End;
        '-':
           Begin
              // Again, as with the "+", we need to test for a unary minus,
              // which cannot be ignored, and needs to negate the last number stacked.
              If NumStacked > 1 Then Begin
                 Flags[Use_GetTwoStackedNumbers] := True;
                 Result := Result + '   CALL GetTwoStackedNumbers'#13;
                 Result := Result + '   CALL $300F        ; [ROM] Call the Subtraction routine.'#13;
                 Result := Result + '   CALL ReclaimStackItem'#13#13;
                 Flags[Use_ReclaimStackItem] := True;
                 DummyStack := Copy(DummyStack, 2, 9999);
                 Dec(NumStacked);
              End Else Begin
                 Result := Result + '   CALL NegateStackItem'+#13#13;
                 Flags[Use_NegateStackItem] := True;
              End;
           End;

        // Multiply and Divide both use a similar structure - they both get pointers to
        // the top two numbers on the calculator stack, and they both call a ROM routine to
        // get the job done.

        '*':
           Begin
              If NumStacked > 1 Then Begin
                 Flags[Use_GetTwoStackedNumbers] := True;
                 Result := Result + '   CALL GetTwoStackedNumbers'#13;
                 Result := Result + '   CALL $30CA        ; [ROM] Call the Multiplication routine.'#13;
                 Result := Result + '   CALL ReclaimStackItem'#13#13;
                 Flags[Use_ReclaimStackItem] := True;
                 DummyStack := Copy(DummyStack, 2, 9999);
                 Dec(NumStacked);
              End;
           End;
        '/':
           Begin
              If NumStacked > 1 Then Begin
                 Flags[Use_GetTwoStackedNumbers] := True;
                 Result := Result + '   CALL GetTwoStackedNumbers'#13;
                 Result := Result + '   CALL $31AF        ; [ROM] Call the Division routine.'#13;
                 Result := Result + '   CALL ReclaimStackItem'#13#13;
                 Flags[Use_ReclaimStackItem] := True;
                 DummyStack := Copy(DummyStack, 2, 9999);
                 Dec(NumStacked);
              End;
           End;

        // Exponentiation, like the multiply and divide routines, use the top two numbers, but the ROM
        // routine called disposes of the superfluous top stack item itself, so we don't need to call
        // ReclaimStackItem.

        '^':
           Begin
              If NumStacked > 1 Then Begin
                 Flags[Use_GetTwoStackedNumbers] := True;
                 Result := Result + '   CALL GetTwoStackedNumbers'#13;
                 Result := Result + '   CALL $3851        ; [ROM] Call the Exponentiation routine.'#13#13;
                 DummyStack := Copy(DummyStack, 2, 9999);
                 Dec(NumStacked);
              End;
           End;

        // From now on, we're compiling functions. These usually use a stacked item or two,
        // and return a newly stacked item. Each has its' own compile function.
        // Don't forget that any function that modifies the type of item at the top of the stack,
        // or removes/adds a stacked item needs to update the dummy stack accordingly.

        #165:
           Begin // RND - adds a new stacked number
              Result := Result + CompileRND;
              DummyStack := '#'+DummyStack;
              Inc(NumStacked);
           End;
        #167:
           Begin // PI - adds a new stacked number
              Result := Result + CompilePI;
              DummyStack := '#'+DummyStack;
              Inc(NumStacked);
           End;
        #186:
           Begin // INT
              Result := Result + CompileINT;
           End;
        #190:
           Begin // PEEK
              Result := Result + CompilePEEK;
           End;
        #194:
           Begin // CHR$ - changes the top stacked item from numeric to char
              Result := Result + CompileCHRStr;
              DummyStack := '$'+Copy(DummyStack, 2, 9999);
           End;
        #195:
           Begin // NOT
              Result := Result + CompileNot;
           End;
        #197:
           Begin // OR - removes a number from the stack
              If NumStacked > 1 Then Begin
                 DummyStack := Copy(DummyStack, 2, 9999);
                 Result := Result + CompileOR;
                 Dec(NumStacked);
              End;
           End;
     End;
  End;
End;

Function Get5Bytes(Value: Extended): String;
Var
  Sign: Byte;
  Mantissa: Integer;
  Exp: Integer;
  NewVal: Integer;
Begin

  // Converts any floating point number to the spectrum's 5 byte
  // representation.

  Result := '';
  If ((Value >= -65535) and (Value < 65536)) and
     (Value = Round(Value)) Then Begin
     Result := #0;
     If Value >= 0 Then
        Result := Result + #0
     Else Begin
        Result := Result + #$FF;
        Value := Value + 65536;
     End;
     NewVal := Round(Value);
     Result := Result + Chr(Byte(NewVal and $FF)) + Chr(Byte(NewVal Shr 8)) + #0;
  End Else Begin
     If Value < 0 Then Begin
        Sign := $80;
        Value := -Value;
     End Else
        Sign := 0;
     Exp := Floor(log2(Value));
     If (Exp < -129) or (Exp > 126) Then Exit;
     Mantissa := Floor((Value/Power(2.0, Exp) -1.0) * 2147483648 +0.5);
     Result :=          Chr(Byte(  Round(Exp)+$81));
     Result := Result + Chr(Byte(((Mantissa Shr 24) and $7F) or Sign));
     Result := Result + Chr(Byte(( Mantissa Shr 16) and $FF));
     Result := Result + Chr(Byte(( Mantissa Shr 8)  and $FF));
     Result := Result + Chr(Byte(  Mantissa and $FF));
  End;
End;

Function Byte5ToFloat(Exponent: Byte; Mantissa: DWord): Extended;
Var
  ResStr: String;
Begin

  // Converts a spectrum 5 byte floating point number to it's human
  // readable form.

  If Exponent = 0 Then Begin
     Result := (Mantissa Shr 8) and 65535;
     If (Mantissa And 255) = $FF Then Result := Result - 65536;
  End Else Begin
     Mantissa := (Mantissa Shr 24)+(((Mantissa Shr 16) And $FF) Shl 8)+(((Mantissa Shr 8) and $FF) Shl 16)+((Mantissa And $FF) Shl 24);
     Result := Power(2.0, Exponent - $81) * (((Mantissa And $7FFFFFFF)/2147483648)+1.0);
     If (Mantissa Shr 24) and $80 = $80 Then Result := -Result;
     ResStr := FloatToStrEx(Result);
     If Length(ResStr) > 9 Then Begin
        Result := Result * (Power(10, Length(ResStr)-9));
        Result := Round(Result) / (Power(10, Length(ResStr)-9));
     End;
  End;
End;

// BASIC Keywords

Function CompileFOR(BASIC: String; Var Index: Integer): String;
Var
  StartCode, LimitCode, StepCode, VarName, NextLine: String;
  NextLineIdx: Integer;
Begin

  // FOR <Var> = <Start> TO <End> [STEP <Increment>]

  Inc(Index);
  VarName := GetVarName(BASIC, Index);
  Inc(Index);
  StartCode := GetExpression(BASIC, Index);
  Inc(Index);
  LimitCode := GetExpression(BASIC, Index);

  // Test for STEP

  If BASIC[Index] = #205 Then Begin
     Inc(Index);
     StepCode := GetExpression(BASIC, Index);
  End Else Begin
     StepCode :=            '   LD   DE,256     ; STEP not present - assume 1.'+#13;
     StepCode := StepCode + '   CALL StackShortInt'+#13#13;
  End;

  // Find the location of the next line or statement - it's here that we will jump to
  // when the NEXT is encountered.

  NextLineIdx := LineNumbers.IndexOf(CompileLabel)+1;

  // Only a FOR statement that actually does something needs to be compiled.

  If NextLineIdx < LineNumbers.Count  Then Begin
     NextLine := LineNumbers[NextLineIdx];
     Result := StepCode + LimitCode + StartCode;
     Result := Result + '   LD   DE,(Var_'+VarName+')   ; Address the var in VARS area'+#13;
     Result := Result + '   LD   BC,Line'+NextLine+'        ; Store the address of the next line'+#13;
     Result := Result + '   CALL UpdateFORVar'+#13#13;
     Flags[Use_UpdateFORVar] := True;
  End Else Begin
     Result := '   ; [Uncompiled FOR Statement]'+#13#13;
  End;

End;

Function CompileNEXT(BASIC: String; var Index: Integer): String;
Var
  VarName: String;
Begin

  // NEXT <Var>

  Inc(Index);
  VarName := GetVarName(BASIC, Index);
  Result :=          '   LD   HL,(Var_'+VarName+')   ; Address the var in VARS area'+#13;
  Result := Result + '   CALL DoNext                 ; And sort out the loop where necessary.'+#13#13;
  Flags[Use_DoNext] := True;

End;

Function CompilePOKE(BASIC: String; var Index: Integer): String;
Begin

  // POKE <Address>,<Byte Value>

  Inc(Index);
  Result := GetExpression(BASIC, Index);
  Inc(Index);
  Result := Result + GetExpression(BASIC, Index) + '   CALL $1E80   ; [ROM] POKE Command'+#13#13;

End;

Function CompileBORDER(BASIC: String; Var Index: Integer): String;
Begin

  // BORDER <0..7>

  Inc(Index);
//  Result := GetExpression(BASIC, Index) + '   CALL $2294   ; [ROM] BORDER Command'+#13#13;

  Result := GetExpression(BASIC, Index) + '   CALL $2DA2   ; [ROM] Unstack to BC'+#13
                                        + '   OUT  ($FE),A ; Set BORDER colour'+#13#13;


End;

Function CompilePAUSE(BASIC: String; Var Index: Integer): String;
Begin

  // PAUSE <Duration>

  Inc(Index);
  Result := GetExpression(BASIC, Index) + '   CALL $1F3A   ; [ROM] PAUSE Command'+#13#13;

End;

Function CompileGOTO(BASIC: String; var Index: Integer): String;
Var
  S: String;
Begin

  // GOTO <Line Number>
  // Doesn't handle computed line numbers yet, ie GO TO a*2

  Inc(Index);
  S := GetNumber(BASIC, Index);
  S := IntToStr((GetByte(@S[2]) Shl 8)+GetByte(@S[3]));
  Result := '   JP Line'+S+#13;

End;

Function CompileLET(BASIC: String; var Index: Integer): String;
Var
  VarName: String;
Begin

  // LET <Var> = <Expression>

  Inc(Index);
  VarName := GetVarName(BASIC, Index);
  Inc(Index);
  Result := GetExpression(BASIC, Index);
  Result := Result + '   LD   DE,(Var_'+VarName+')   ; Address the var in VARS area'+#13;
  If Pos('_str', VarName) <> 0 Then Begin
     Result := Result + '   LD   BC,Var_'+VarName+#13;
     Result := Result + '   PUSH BC'+#13;
     Result := Result + '   CALL UpdateStrVar'+#13#13;
     Flags[Use_UpdateStrVar] := True;
  End Else Begin
     Result := Result + '   CALL UpdateNumVar'+#13#13;
     Flags[Use_UpdateNumVar] := True;
  End;
End;

Function CompileDIM(BASIC: String; var Index: Integer): String;
Begin

  // DIM <Var>(<Elements<,Elements>>)

  Inc(Index);
  Result := GetExpression(BASIC, Index);
  While BASIC[Index] = ',' Do Begin
     Inc(Index);
     Result := Result + GetExpression(BASIC, Index);
  End;
  Result := Result + '   CALL PerformDIM'+#13#13;
  Flags[Use_PerformDIM] := True;
  Inc(Index);
  
End;

// BASIC Functions

Function CompileRND: String;
Begin

  // The ROM RND function can't be used, as it's not just a simple RET on exit.
  // So we have to create our own RND routine. It's a copy of the ROM routine
  // so we retain compatibility.

  Flags[Use_RNDReplacement] := True;
  Result := '   CALL RNDReplacement   ; Call the new RND Function.'#13#13;

End;

Function CompileOR: String;
Begin
  Flags[Use_GetTwoStackedNumbers] := True;
  Result :=          '   CALL GetTwoStackedNumbers'#13;
  Result := Result + '   CALL $351B        ; [ROM] Call the OR Function.'#13;
  Result := Result + '   CALL ReclaimStackItem'#13#13;
  Flags[Use_ReclaimStackItem] := True;
End;

Function CompileNOT: String;
Begin
  Result :=          '   LD   HL,($5C65) ; Address STKEND'#13;
  Result := Result + '   LD   BC,$FFFB   ; Prepare to jump back 5 Bytes'#13;
  Result := Result + '   ADD  HL,BC      ; HL now points to the last value on the stack'#13;
  Result := Result + '   CALL $3501      ; [ROM] Call the NOT Function.'#13#13;
End;

Function CompilePI: String;
Var
  Byte5NUmber: String;
  Idx: Integer;
Begin
  Idx := 1;
  Byte5Number := GetNumber('3.141592654', Idx);
  Result :=          '   LD   A,'+IntToStr(GetByte(@Byte5Number[1]))+'   ; Prepare PI for stacking'+#13;
  Result := Result + '   LD   DE,'+IntToStr(GetWord(@Byte5Number[2]))+#13;
  Result := Result + '   LD   BC,'+IntToStr(GetWord(@Byte5Number[4]))+#13;
  Result := Result + '   CALL $2AB6      ; [ROM] STACK-STORE (PI)'+#13#13;
End;

Function CompileINT: String;
Begin
  Flags[Use_GetStackedNumber] := True;
  Result :=          '   CALL GetStackedNumber'#13;
  Result := Result + '   CALL $36AF      ; [ROM] Call the INT routine.'#13#13;
End;

Function CompileCHRStr: String;
Begin
  Result :=          '   CALL $35C9      ; [ROM] Call the CHR$ routine.'#13#13;
End;

Function CompilePEEK: String;
Begin
  Flags[Use_GetStackedNumber] := True;
  Result :=          '   CALL $1E99      ; [ROM] puts address of next stacked integer in BC.'+#13;
  Result := Result + '   LD   A,(BC)     ; load contents into A register.'+#13;
  Result := Result + '   LD   D,A'+#13;
  Result := Result + '   LD   E,0'+#13;
  Result := Result + '   CALL StackShortInt'+#13#13;
  Flags[Use_StackShortInt] := True;
End;

// Additional code and library routines

Procedure AddLibs;
Begin
  If Flags[Use_PerformDIM] Then Begin

     // Redimensions an array. On entry, 

  End;

  If Flags[Use_DoNext] Then Begin

     // Performs a NEXT - we can't use the ROM version because
     // of the way we handle line numbers.

     AsmOutput.Add('DoNext:');
     AsmOutput.Add('   LD      ($5C68),HL      ; Set MEM to point to the three 5-byte values');
     AsmOutput.Add('                           ; value, limit, step.');
     AsmOutput.Add('   RST  28H                ;; FP-CALC      add step and re-store');
     AsmOutput.Add('   DEFB $E0                ;; get-mem-0    v.');
     AsmOutput.Add('   DEFB $E2                ;; get-mem-2    v,s.');
     AsmOutput.Add('   DEFB $0F                ;; addition     v+s.');
     AsmOutput.Add('   DEFB $C0                ;; st-mem-0     v+s.');
     AsmOutput.Add('   DEFB $02                ;; delete       .');
     AsmOutput.Add('   DEFB $38                ;; end-calc');
     AsmOutput.Add('');
     AsmOutput.Add('   CALL $1DDA              ; [ROM] routine NEXT-LOOP tests against limit.');
     AsmOutput.Add('   RET  C                  ; return if no more iterations possible.');
     AsmOutput.Add('');
     AsmOutput.Add('   LD   HL,($5C68)         ; find start of variable contents from MEM.');
     AsmOutput.Add('   LD   DE,$F              ; add 3*5 to');
     AsmOutput.Add('   ADD  HL,DE              ; address the looping address');
     AsmOutput.Add('   LD   E,(HL)');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   LD   D,(HL)');
     AsmOutput.Add('   EX   DE,HL');
     AsmOutput.Add('   JP   (HL)               ; and go there');
     AsmOutput.Add('');

  End;

  If Flags[Use_UpdateFORVar] Then Begin

     // Set up a FOR variable in VARS space

     AsmOutput.Add('UpdateFORVar:');
     AsmOutput.Add('   PUSH BC');
     AsmOutput.Add('   CALL UpdateNumVar    ; UnStack the STEP');
     AsmOutput.Add('   CALL UpdateNumVar    ; and the LIMIT');
     AsmOutput.Add('   CALL UpdateNumVar    ; and the START, or CURRENT Value');
     AsmOutput.Add('   POP  BC');
     AsmOutput.Add('   EX   DE,HL');
     AsmOutput.Add('   LD   (HL),C        ; so now just store the address');
     AsmOutput.Add('   INC  HL             ; of the line to jump to when we loop');
     AsmOutput.Add('   LD   (HL),B');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');
     Flags[Use_UpdateNumVar] := True;

  End;

  If Flags[Use_StackStringVar] Then Begin

     // Places a *copy* of a string in the spare space -
     // and a pointer to that copy on the calculator stack.

     AsmOutput.Add('StackStringVar:');
     AsmOutput.Add('   LD   C,(HL)');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   LD   B,(HL)');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   PUSH HL                             ; Store the start of the string');
     AsmOutput.Add('   PUSH BC');
     AsmOutput.Add('');
     AsmOutput.Add('   RST  30h                            ; Reserve BC Spaces at [WORKSP]');
     AsmOutput.Add('   POP  BC');
     AsmOutput.Add('   CALL $2AB6                          ; [ROM] STACK-STORE (String)');
     AsmOutput.Add('');
     AsmOutput.Add('   POP  HL                             ; Restore the address of the source string');
     AsmOutput.Add('   LDIR                                ; and copy the source to [WORKSP]');
     AsmOutput.Add('');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_StackNumericVar] Then Begin

     // Sends a 5 Byte representation of a number to the calculator stack.

     AsmOutput.Add('StackNumericVar:');
     AsmOutput.Add('   LD   A,(HL)');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   LD   E,(HL)');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   LD   D,(HL)');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   LD   C,(HL)');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   LD   B,(HL)');
     AsmOutput.Add('   CALL $2AB6   ; [ROM] STACK-STORE (Variable)');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_UpdateNumVar] Then Begin

     // Adds a 5 Byte number pointed to by DE to the Stack.

     AsmOutput.Add('UpdateNumVar:');
     AsmOutput.Add('   LD   HL,($5C65)   ; Address STKEND');
     AsmOutput.Add('   LD   BC,$FFFB     ; Prepare to step back 5 bytes');
     AsmOutput.Add('   ADD  HL,BC        ; Address the last item on the stack');
     AsmOutput.Add('   LDI               ; Copy 5 bytes');
     AsmOutput.Add('   LDI');
     AsmOutput.Add('   LDI');
     AsmOutput.Add('   LDI');
     AsmOutput.Add('   LDI');
     AsmOutput.Add('   CALL ReclaimStackItem');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');
     Flags[Use_ReclaimStackItem] := True;

  End;

  If Flags[Use_UpdateStrVar] Then Begin

     // Updates a string variable's contents in the VARS Space.
     // It mostly handles strings that get bigger or smaller, and updates pointers to suit.

     // On Entry:
     //    The return address is in BC
     //    The Address of that var's Jump Table entry is in IX
     //    The Address of the var's data (in the VARS area) is in DE

     AsmOutput.Add('UpdateStrVar:');
     AsmOutput.Add('   POP  BC                             ; Save the return address');
     AsmOutput.Add('   POP  IX                             ; Save the address of the Jump table entry that will be affected');
     AsmOutput.Add('   PUSH BC                             ; Restack the return address');
     AsmOutput.Add('   PUSH DE                             ; Store the VARS Address for later');
     AsmOutput.Add('');
     AsmOutput.Add('   CALL $2BF1                          ; [ROM] Get the string parameters from the calculator stack');
     AsmOutput.Add('');
     AsmOutput.Add('   POP  HL                             ; And get the address of the VARS space back.');
     AsmOutput.Add('   PUSH BC                             ; Store the length of the stacked var');
     AsmOutput.Add('   LD   E,(HL)                         ; Grab the current Var Length into DE, and...');
     AsmOutput.Add('   LD   (HL),C                         ; ...Update the variable length marker');
     AsmOutput.Add('   INC  HL');
     AsmOutput.Add('   LD   D,(HL)');
     AsmOutput.Add('   LD   (HL),B');
     AsmOutput.Add('   INC  HL                             ; HL now points at the start of the string');
     AsmOutput.Add('   PUSH HL                             ; and we'#39'll need it later, so store it.');
     AsmOutput.Add('');
     AsmOutput.Add('   PUSH BC');
     AsmOutput.Add('   LD   B,D                            ; Prepare to reclaim the old string - length to BC');
     AsmOutput.Add('   LD   C,E');
     AsmOutput.Add('   CALL $19E8                          ; [ROM] routine RECLAIM-2 to reclaim the old string.');
     AsmOutput.Add('   POP  BC');
     AsmOutput.Add('   INC  IX');
     AsmOutput.Add('   INC  IX                             ; This variables jump table entry does not need updating,');
     AsmOutput.Add('                                       ; But all subsequent vars do.');
     AsmOutput.Add('   EX   DE, HL                         ; Now Test - is the new string smaller than the old?');
     AsmOutput.Add('   AND  A');
     AsmOutput.Add('   SBC  HL, BC');
     AsmOutput.Add('   EX   DE, HL');
     AsmOutput.Add('   JR   NC, UpdateVarStringLarger      ; If it isnt then just carry on');
     AsmOutput.Add('');
     AsmOutput.Add('   PUSH BC                             ; Store BC before it gets modified');
     AsmOutput.Add('   CALL UpdateJumpTableDec             ; Otherwise, jump into the next routine early to invert BC');
     AsmOutput.Add('   POP  BC                             ; and Restore BC for the actual MAKE-ROOM process');
     AsmOutput.Add('   JP   UpdateStrVarCont');
     AsmOutput.Add('');
     AsmOutput.Add('UpdateVarStringLarger:');
     AsmOutput.Add('   CALL UpdateJumpTableAdd');
     AsmOutput.Add('');
     AsmOutput.Add('UpdateStrVarCont:');
     AsmOutput.Add('   POP  HL');
     AsmOutput.Add('');
     AsmOutput.Add('   CALL $1655                          ; [ROM] routine MAKE-ROOM creates BC spaces for new string');
     AsmOutput.Add('   LD   DE,($5C61)                     ; Get WORKSP into DE - the start of the string');
     AsmOutput.Add('');
     AsmOutput.Add('   INC  HL                             ; advance to first new location.');
     AsmOutput.Add('   EX   DE,HL');
     AsmOutput.Add('   POP  BC                             ; Set up for LDIR - BC is length, DE is destination');
     AsmOutput.Add('   LDIR                                ; and HL is the source string to copy from');
     AsmOutput.Add('');
     AsmOutput.Add('   CALL CleanWorkspace                 ; and... tidy up.');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

     Flags[Use_UpdateJumpTable] := True;
     Flags[Use_CleanWorkspace] := True;

  End;

  If Flags[Use_UpdateJumpTable] Then Begin

     // When a variable shrinks or grows in size (usually strings) they cause variables
     // in the VARS area to move around. This routine updates the LUT for the addresses
     // of these vars.

     // Entering the Routine *here* causes all vars to be moved back by BC bytes. This
     // is done by just "inverting" BC, so the addition later on becomes a subtraction.

     AsmOutput.Add('UpdateJumpTableDec:        ; Adjust BC to move Backwards in memory');
     AsmOutput.Add('   LD   A,B');
     AsmOutput.Add('   CPL');
     AsmOutput.Add('   LD   B,A');
     AsmOutput.Add('   LD   A,C');
     AsmOutput.Add('   CPL');
     AsmOutput.Add('   LD   C,A');
     AsmOutput.Add('   INC  BC');

     // Entering here will leave BC as an addition.

     AsmOutput.Add('UpdateJumpTableAdd:');
     AsmOutput.Add('   LD   A,$FF');
     AsmOutput.Add('   LD   L,(IX+0)           ; Get the location from the table');
     AsmOutput.Add('   LD   H,(IX+1)');
     AsmOutput.Add('   CP   H                  ; Test with A (Is it $FF?)');
     AsmOutput.Add('   RET  Z                  ; Bail out if so');
     AsmOutput.Add('   ADD  HL,BC              ; Otherwise, add the offset, and');
     AsmOutput.Add('   LD   (IX+0),L           ; Update the table.');
     AsmOutput.Add('   LD   (IX+1),H');
     AsmOutput.Add('   INC  IX');
     AsmOutput.Add('   INC  IX');
     AsmOutput.Add('   JP   UpdateJumpTableAdd ; Go back and do the next one');
     AsmOutput.Add('');

  End;

  If Flags[Use_GetStackedNumber] Then Begin

     // Most Arithmetic routines that operate on one
     // operand will use this routine to get pointers to the top
     // item on the calculator stack.

     AsmOutput.Add('GetStackedNumber:');
     AsmOutput.Add('   LD   HL,($5C65)   ; HL = STKEND');
     AsmOutput.Add('   LD   BC,$FFFB     ; Numbers are (-)5 Bytes');
     AsmOutput.Add('   ADD  HL,BC        ; (HL) = Last Number');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_GetTwoStackedNumbers] Then Begin

     // Again, any Arithmetic routines that use two operands will likely
     // request pointers to the top two calculator stack entries.

     AsmOutput.Add('GetTwoStackedNumbers:');
     AsmOutput.Add('   LD   HL,($5C65)   ; HL = STKEND');
     AsmOutput.Add('   LD   BC,$FFFB     ; Numbers are (-)5 Bytes');
     AsmOutput.Add('   ADD  HL,BC        ; (HL) = Last Number');
     AsmOutput.Add('   LD   D,H');
     AsmOutput.Add('   LD   E,L          ; (DE) = Last Number');
     AsmOutput.Add('   ADD  HL,BC        ; (HL) = First Number');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_ReclaimStackItem] Then Begin

     // Doesn't actually do any reclaiming as such - it just
     // updates the System Variable STKEND to allow the top item to
     // "fall off" the stack.

     AsmOutput.Add('ReclaimStackItem:');
     AsmOutput.Add('   LD   HL,($5C65)   ; Address STKEND');
     AsmOutput.Add('   LD   BC,$FFFB     ; Prepare to reclaim 5 Bytes');
     AsmOutput.Add('   ADD  HL,BC        ; Move (HL) Back 5 bytes to reclaim old value');
     AsmOutput.Add('   LD   ($5C65),HL   ; Update STKEND to reflect the change');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_StackShortint] Then Begin

     // Adds a small number to the calculator stack.
     // Small numbers only occupy two of the 5 Byte representation, the other 3 bytes
     // are zeros, so we can shortcut a little here.

     AsmOutput.Add('StackShortInt:');
     AsmOutput.Add('   XOR  A');
     AsmOutput.Add('   LD   B,A');
     AsmOutput.Add('   LD   C,A');
     AsmOutput.Add('   CALL $2AB6        ; [ROM] STACK-STORE');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_NegateStackItem] Then Begin

     // A short call to make the top stack item the negative of itself.

     AsmOutput.Add('NegateStackItem:');
     AsmOutput.Add('   LD   HL,($5C65)   ; Address STKEND');
     AsmOutput.Add('   LD   BC,$FFFB     ; Prepare to step back 5 bytes');
     AsmOutput.Add('   LD   D,H');
     AsmOutput.Add('   LD   E,L          ; Set DE to STKEND');
     AsmOutput.Add('   ADD  HL,BC        ; Set HL to last Stacked Number');
     AsmOutput.Add('   CALL $346E        ; [ROM] Call Negate');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_RNDReplacement] Then Begin

     // As mentioned above, the ROM routine for RND is incompatible to
     // BASin's compiler calling - it doesn't just RET back to where it came from.
     // Therefore, here's a copy of the routine that *does* return.

     AsmOutput.Add('RNDReplacement:');
     AsmOutput.Add('   LD   BC,($5C76)   ; fetch system variable SEED');
     AsmOutput.Add('   CALL $2D2B        ; routine STACK-BC places on calculator stack');
     AsmOutput.Add('   RST  28H          ;; FP-CALC            ;s.');
     AsmOutput.Add('   DEFB $A1          ;; stk-one            ;s,1.');
     AsmOutput.Add('   DEFB $0F          ;; addition           ;s+1.');
     AsmOutput.Add('   DEFB $34          ;; stk-data           ;');
     AsmOutput.Add('   DEFB $37          ;; Exponent: $87,');
     AsmOutput.Add('                     ;; Bytes: 1');
     AsmOutput.Add('   DEFB $16          ;; (+00,+00,+00)      ;s+1,75.');
     AsmOutput.Add('   DEFB $04          ;; multiply           ;(s+1)*75 = v');
     AsmOutput.Add('   DEFB $34          ;; stk-data           ;v.');
     AsmOutput.Add('   DEFB $80          ;; Bytes: 3');
     AsmOutput.Add('   DEFB $41          ;; Exponent $91');
     AsmOutput.Add('   DEFB $00,$00,$80  ;; (+00)              ;v,65537.');
     AsmOutput.Add('   DEFB $32          ;; n-mod-m            ;remainder, result.');
     AsmOutput.Add('   DEFB $02          ;; delete             ;remainder.');
     AsmOutput.Add('   DEFB $A1          ;; stk-one            ;remainder, 1.');
     AsmOutput.Add('   DEFB $03          ;; subtract           ;remainder - 1. = rnd');
     AsmOutput.Add('   DEFB $31          ;; duplicate          ;rnd,rnd.');
     AsmOutput.Add('   DEFB $38          ;; end-calc');
     AsmOutput.Add('');
     AsmOutput.Add('   CALL $2DA2        ; routine FP-TO-BC');
     AsmOutput.Add('   LD   ($5C76),BC   ; store in SEED for next starting point.');
     AsmOutput.Add('   LD   A,(HL)       ; fetch exponent');
     AsmOutput.Add('   AND  A            ; is it zero ?');
     AsmOutput.Add('   RET  Z            ; exit if so.');
     AsmOutput.Add('');
     AsmOutput.Add('   SUB  $10          ; reduce exponent by 2^16');
     AsmOutput.Add('   LD   (HL),A       ; place back');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

  If Flags[Use_CleanWorkspace] Then Begin

     // Particularly with strings, the Workspace region of memory is used a lot.
     // This routine reclaims that area after use. It's good housekeeping more than
     // anything.

     AsmOutput.Add('CleanWorkspace:');
     AsmOutput.Add('   LD   HL,($5C63)   ; Fetch STKBOT');
     AsmOutput.Add('   LD   DE,($5C61)   ; Fetch start of Workspace');
     AsmOutput.Add('   CALL $19E5        ; [ROM] Reclaim the Workspace');
     AsmOutput.Add('   RET');
     AsmOutput.Add('');

  End;

End;

Procedure AllocVars;
Var
  AllocSpace, Idx, Count: Integer;
  VarName, DEFBs: String;
  TempVars: TStringlist;
Begin

  // A real biggie for the compiler. Variables are handled a little differently to the ROM,
  // mostly because when we compile, we know what variables we're going to use.

  // We use a jump table (LUT) which stores addresses of Vars in the VARS space, which we use to
  // quickly address variables rather than using the ROM method of stepping through searching
  // for the correct variable.

  // We retain ROM compatibility - you can compile LET A=2 and then run it with a USR command -
  // and after that you can go to BASIC and type PRINT A and get a meaningful result. There is one
  // problem with this - your variables can overrun your compiled code if you're not careful, as
  // the code exists higher up in memory than the VARS space, as opposed to BASIC code which lives
  // below them.

  If Variables.Count = 0 Then Exit;

  AsmOutput.Insert(0, '');
  AsmOutput.Insert(0, '   CALL AllocateVars');
  AsmOutput.Insert(0, '');
  AsmOutput.Insert(0, 'NumVars EQU '+IntToStr(Variables.Count));

  // Ensure that Arrays and Strings are allocated *after* the numeric vars

  TempVars := TStringlist.Create;

  Idx := 0;
  While Idx < Variables.Count Do Begin
     If Pos('_str', Variables[Idx]) <> 0 Then Begin
        VarName := Variables[Idx];
        Variables.Delete(Idx);
        TempVars.Add(VarName);
     End Else Begin
        Inc(Idx);
     End;
  End;

  Variables.AddStrings(TempVars);
  TempVars.Free;

  // And begin.

  AsmOutput.Add('AllocateVars:');

  // For each variable, figure out how much space each needs,
  // and insert that before the first char of the variable's name,
  // as a byte to be read later.

  // Allocspace is a running total of all the space needed.

  AllocSpace := 0;
  For Idx := 0 To Variables.Count -1 Do Begin
     VarName := Lowercase(Variables[Idx]);
     If Varname[1] = 'f' Then Begin
        Inc(AllocSpace, 19);
        Variables[Idx] := Chr(18)+VarName;
     End Else Begin
        If Varname[1] = 'a' Then Begin
           If Pos('_str', VarName) <> 0 Then Begin
              Inc(AllocSpace, 7);
              Variables[Idx] := Chr(6)+VarName;
           End Else Begin
              Inc(AllocSpace, 11);
              Variables[Idx] := Chr(10)+VarName;
           End;
        End Else Begin
           If Pos('_str', VarName) <> 0 Then Begin
              Inc(AllocSpace, 3);
              Variables[Idx] := Chr(2)+VarName;
           End Else Begin
              Inc(AllocSpace, Length(VarName)+5);
              Variables[Idx] := Chr(5)+VarName;
           End;
        End;
     End;
  End;

  // When running the compiled code for the first time, the current variables in
  // VARS space are cleared out, equivalent to the RUN command's behaviour of executing a CLEAR.

  AsmOutput.Add('   LD   BC,($5CB2)');
  AsmOutput.Add('   LD   DE,($5C4B)');
  AsmOutput.Add('   LD   HL,($5C59)');
  AsmOutput.Add('   DEC  HL');
  AsmOutput.Add('   CALL $19E5 ; Clear any current variables');
  AsmOutput.Add('');

  // Using the Allocspace var, we create some room in the VARS area for our
  // variables.

  AsmOutput.Add('   LD   IX,'+'Var_'+Copy(Variables[0], 3, 9999)+' ; set IX up to point at the jump table');
  AsmOutput.Add('   LD   BC,'+IntToStr(AllocSpace)+'    ; Allocate space for Variables');
  AsmOutput.Add('   LD   HL,($5C59)                     ; fetch E_LINE to HL.');
  AsmOutput.Add('   DEC  HL                             ; point to location before, the variables');
  AsmOutput.Add('                                       ; end-marker.');
  AsmOutput.Add('   CALL $1655                          ; routine MAKE-ROOM creates BC spaces');
  AsmOutput.Add('                                       ; for name and numeric value.');
  AsmOutput.Add('   INC  HL                             ; advance to first new location.');
  AsmOutput.Add('');
  AsmOutput.Add('   EX   DE,HL                          ; set DE=Start of space in VARS area');
  AsmOutput.Add('   LD   HL,VarNames                    ; set HL to the first var in the table');
  AsmOutput.Add('   LD   A,NumVars');

  // Now, for each variable, we need to copy the info out of the
  // table we have created (which we do later on) into the VARS area.

  AsmOutput.Add('VarLoop:');
  AsmOutput.Add('   LD   C,(HL)');
  AsmOutput.Add('   INC  HL');
  AsmOutput.Add('   LD   B,(HL)                         ; BC now holds the length of the vars data');
  AsmOutput.Add('   INC  HL');
  AsmOutput.Add('   PUSH BC                             ; preserve length of vars data');
  AsmOutput.Add('   LD   C,(HL)');
  AsmOutput.Add('   INC  HL');
  AsmOutput.Add('   LD   B,(HL)                         ; BC now holds the length of the var name');
  AsmOutput.Add('   INC  HL                             ; HL now points to the var name');
  AsmOutput.Add('   LDIR                                ; Copy var name to VARS area,');
  AsmOutput.Add('                                       ; advancing DE to point to the vars value and,');
  AsmOutput.Add('                                       ; advancing HL to point to the Filler Bytes (0s) in "VarNames" table');
  AsmOutput.Add('   LD   (IX+0),E                       ; Store pointer to vars value in jump table');
  AsmOutput.Add('   LD   (IX+1),D');
  AsmOutput.Add('   INC   IX');
  AsmOutput.Add('   INC   IX');
  AsmOutput.Add('   POP   BC                            ; BC = length of vars data (from "VarNames")');
  AsmOutput.Add('   LDIR                                ; Copy the filler bytes.');
  AsmOutput.Add('                                       ; DE now points to the next var,');
  AsmOutput.Add('                                       ; HL points to the next var in "VarNames" table');
  AsmOutput.Add('   DEC  A');
  AsmOutput.Add('   JR   NZ,VarLoop');
  AsmOutput.Add('   RET');
  AsmOutput.Add('');

  // Et Voila, we have VARS already allocated and ready to be used - either by us, or from BASIC :-)

  AsmOutput.Add('VarNames:                              ; Here follows the Variable Names Table');
  AsmOutput.Add('');

  // The Variables are declared here for speed - an intial "state" and a LUT follow.
  // The State takes the form:

  // DEFB <Size of Contents, in LSB Word form> <Length of Name, in LSB Word form>
  // DEFB <Any filler bytes needed>

  For Idx := 0 To Variables.Count -1 Do Begin

     VarName := Copy(Variables[Idx], 2, 9999);

     // Create the "header" which contains the info about the var.

     If VarName[1] = 'A' Then Begin
        If Pos('_str', VarName) <> 0 Then Begin // String Array
           VarName := Copy(VarName, 1, Pos('_', VarName)-1);
           AsmOutput.Add('   DEFB 6,0,1,0');
           DEFBs := '   DEFB '+IntToStr((Ord(VarName[1])-$60)+192);
        End Else Begin // Numeric Array
           VarName := Copy(VarName, 1, Pos('_', VarName)-1);
           AsmOutput.Add('   DEFB 10,0,1,0');
           DEFBs := '   DEFB '+IntToStr((Ord(VarName[1])-$60)+128);
        End;
     End Else Begin
        VarName := Copy(VarName, 2, 9999);
        If Pos('_str', VarName) <> 0 Then Begin // String
           VarName := Copy(VarName, 1, Pos('_', VarName)-1);
           AsmOutput.Add('   DEFB 2,0,1,0');
           DEFBs := '   DEFB '+IntToStr((Ord(VarName[1])-$60)+64);
        End Else Begin
           AsmOutput.Add('   DEFB '+IntToStr(Ord(Variables[Idx][1]))+',0,'+IntToStr(Length(VarName))+',0');
           If Length(VarName) > 1 Then Begin // Simple Numeric
              DEFBs := '   DEFB '+IntToStr((Ord(VarName[1])-$60)+160);
              If Length(VarName) > 2 Then
                 DEFBs := DEFBs + ',"'+Copy(VarName, 2, Length(VarName)-2)+'"';
              DEFBs := DEFBs+',"'+VarName[Length(VarName)]+'"+$80';
           End Else Begin // Complex Numeric
              If Variables[Idx][1] = '*' Then
                 DEFBs := '   DEFB '+IntToStr((Ord(VarName[1])-$60)+224)
              Else
                 DEFBs := '   DEFB '+IntToStr((Ord(VarName[1])-$60)+96);
           End;
        End;
     End;

     // Add the "initial state" - the contents of that variable.

     AsmOutput.Add(DEFBs + '     ; '+Copy(Variables[Idx], 3, 9999));
     DEFBs := '   DEFB ';
     For Count := 1 To Ord(Variables[Idx][1]) Do
        DEFBs := DEFBs + '0,';
     DEFBs := Copy(DEFBs, 1, Length(DEFBs)-1)+' ; Initial filler bytes';
     AsmOutput.Add(DEFBs);

  End;

  AsmOutput.Add('');

  // Now create the LUT or Jump Table. This is initially zeros, but gets updated during the routine
  // Above that allocates the Vars in the VARS area.

  AsmOutput.Add('; Here follows the Variables jump table');
  For Idx := 0 To Variables.Count -1 Do Begin
     VarName := Copy(Variables[Idx], 3, 9999);
     If VarName[Length(VarName)] = '$' Then
        VarName := Copy(VarName, 1, Length(VarName)-1)+'_str';
     AsmOutput.Add('Var_'+VarName+':');
     AsmOutput.Add('   DEFB 0,0');
  End;

  // Adds a marker of two bytes ($FFFF) which tells the UpdateJumpTable routine
  // that there are no more variable addresses to update.

  AsmOutput.Add('EndOfVars:');
  AsmOutput.Add('   DEFB 255,255');
  AsmOutput.Add('');

End;

Procedure AddStringConstants;
Var
  Idx: Integer;
Begin

  // Creates a small table of strings. Any string constants (text inserted between "" marks)
  // are declared here and addressed by a pointer in the compiled code.

  // The format is DEFB <Length Hi>,<Length Lo>,<String>

  If Strings.Count > 0 Then Begin
     AsmOutput.Add('         ; String Constants Follow');
     AsmOutput.Add('');
     For Idx := 0 to Strings.Count -1 Do Begin
        AsmOutput.Add('String'+IntToStr(Idx)+':');
        AsmOutput.Add('   DEFB '+IntToStr(Length(Strings[Idx]))+',0,'+'"'+Strings[Idx]+'"');
     End;
  End;

End;

Initialization

Strings := TStringlist.Create;
Variables := TStringlist.Create;
Lines := TStringlist.Create;
LineNumbers := TStringlist.Create;
AsmOutput := TStringlist.Create;

Finalization

Strings.Free;
Variables.Free;
Lines.Free;
LineNumbers.Free;
AsmOutput.Free;

end.
