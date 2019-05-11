unit Evaluate;

interface

Uses Windows, Classes, Sysutils, Math, FastCore;

Type

  TExpression = Record
     SyntaxChecked: Boolean;
     Expression,
     Tokenised,
     ResultStr: AnsiString;
     ResultNum: Extended;
     ResultType,
     ResultError: Byte;
  End;

  Procedure EvaluateExpr(Var Expression: TExpression);
  Function  EvaluateNum(Expression: AnsiString; Default: Integer): Extended;
  Function  EvaluateStr(Expression: AnsiString; Default: AnsiString): AnsiString;

Var

  Evaluating:    Boolean; // This is used to prevent the main window from popping to the front
                       // when an expression is evaluated.
  EvalAbort:     Boolean; // If it gets stuck, then we can exit the evaluation.

  EvalMemory:    Array[0..65535] of Byte;
  EvalSysVars:   Array[0..438] of Byte;
  EvalRegisters: TZ80Registers;

implementation

Uses BASinMain, ROMUtils, BASSupport, Utility;

Procedure EvaluateExpr(Var Expression: TExpression);
Var
  F, Idx: Integer;
  Finished, InString: Boolean;
  StrStart, StrLen, Timer: DWord;
  NewSEED: Word;
  Expr, TempStr: AnsiString;
Begin

  If In128k Then Begin
     Expression.ResultType := 2;
     Expression.ResultNum := 1;
     Expression.ResultError := 0;
     Expression.ResultStr := '  Evaluations unavailable in 128k mode.';
     Exit;
  End;

  // This works by using the following system:
  // 2 Passes! This procedure calls itself. First, the expression is evaluated for
  // syntax. If all is well, then the expression itself is processed. This means that
  // any evaluation is done twice - once to check syntax, one to evaluate. However:
  // Watches (which need to evaluate quickly whilst a program is running) get a speed boost
  // by only evaluating once, for every evaluation called after the initial setup. If a watch
  // expression passes the syntax once, it won't fail again unless it's modified.

  Evaluating := True;
  EvalAbort := False;

  If Not Expression.SyntaxChecked Then Begin

     // 1 - Copy the current memory into the evaluation space
     // It's likely that we do this on the first pass, not the second.

     CopyMemory(@EvalMemory[16384], @Memory[16384], 49152);
     CopyMemory(@EvalRegisters.PC, @Registers.PC, SizeOf(TZ80Registers));

     If Expression.Tokenised = '' Then Begin

        // Process for escape characters:

        Expr := '';
        F := 1;
        InString := False;
        While F < Length(Expression.Expression)+1 Do Begin
           If Expression.Expression[F] = '\' Then Begin
              ProcessEscapeChars(Expression.Expression, F, Expr)
           End Else
              If Not InString and (Expression.Expression[F] = '$') Then Begin
                 Idx := F +1;
                 TempStr := '$';
                 While (Idx <= Length(Expression.Expression)) and (Uppercase(Expression.Expression)[Idx] in ['0'..'9', 'A'..'F']) Do Begin
                    TempStr := TempStr + Expression.Expression[Idx];
                    Inc(Idx);
                 End;
                 If TempStr <> '$' Then Begin
                    TempStr := IntToStr(StrToInt(TempStr));
                    Expression.Expression := Copy(Expression.Expression, 1, F -1) + TempStr + Copy(Expression.Expression, Idx, 9999999);
                    Dec(F);
                 End Else
                    Expr := Expr + Expression.Expression[F];
              End Else Begin
                 If Expression.Expression[F] = '"' Then InString := Not InString;
                 Expr := Expr + Expression.Expression[F];
              End;
           Inc(F);
        End;

        Expression.Tokenised := TokeniseLine(Expr, False);

     End;

     // 2 - Tokenise the expression, and poke it to the edit line.

     PutEditLine(Expression.Tokenised, EvalMemory);
     EvalMemory[FLAGS] := EvalMemory[FLAGS] and 127

  End Else Begin

     If EvalMemory[0] <> 0 Then Begin

        CopyMemory(@EvalMemory[16384], @Memory[16384], 49152);
        CopyMemory(@EvalRegisters.PC, @Registers.PC, SizeOf(TZ80Registers));
        PutEditLine(Expression.Tokenised, EvalMemory);

     End;

     Expression.ResultType := 0;
     EvalMemory[FLAGS] := EvalMemory[FLAGS] or 128;

  End;

  // Also ensure that we're not in INPUT.
  EvalMemory[FLAGX] := EvalMemory[FLAGX] And 223;

  // 3 - Set up the sysvars for an RST 20h to point to the expression
  // CH_ADD is the sysvar that the routine will read characters from.
  // Set PC to SCANNING, and set up a RETurn address of 0, which we trap.

  PutWord(@EvalMemory[CH_ADD], Getword(@EvalMemory[E_LINE]));
  EvalRegisters.PC := $24FB;
  PutWord(@EvalMemory[EvalRegisters.SP], $FFFF);

  // 4 - run an emulation loop until the SCANNING stops and a result (or an error) occurs.
  // Loop until either PC = 8 or PC = $FFFF.

  Finished := False;
  Timer := 0;
  Timer := GetTickCount;    //

  While Not Finished and (GetTickCount - Timer < Opt_EvalTimeOut) Do Begin

     asm

        pushad

        lea  edi, EvalMemory
        lea  esi, EvalRegisters

        cmp  EvalRegisters.haltemu, true
        je   @Halted

     @Start:

        xor  ebx, ebx
		   mov  bx,  Word [esi+TZ80Registers.PC]
		   xor  ecx, ecx
		   mov  cl,  Byte [edi+ebx]
        mov  edx, DWord [edi+ebx+1]
        xor  eax, eax
		   call DWord [Ops+ecx*4]

        //add  Timer, edx
        jmp  @Finish

     @Halted:

        inc  Word [esi+TZ80Registers.PC]
        jmp  @Start

     @Finish:

        popad

     end;

     If EvalAbort Then Begin
        Finished := True;
        Expression.SyntaxChecked := True;
     End;

     If GetTickCount - Timer >= Opt_EvalTimeOut Then Begin
        Expression.ResultType := 2;
        Expression.ResultNum := 1;
        Expression.ResultError := 0;
        Expression.ResultStr := '  The evaluation timed out.';
        Finished := True;
     End Else If (EvalRegisters.PC = $11CB) or (EvalRegisters.PC = $1219) or (EvalRegisters.PC = $12A9) Then Begin
        Expression.ResultType := 2;
        Expression.ResultNum := 1;
        Expression.ResultError := 0;
        Expression.ResultStr := '  Illegal Evaluation.';
        Finished := True;
     End Else If EvalRegisters.PC = $55 Then Begin

        // Error - the statement returned with an error.
        Expression.ResultType := 2;

        // pop the offending character into the numeric return.
        Expression.ResultNum := GetWord(@EvalMemory[CH_ADD])-GetWord(@EvalMemory[E_LINE]);
        Expression.ResultError := EvalRegisters.L;
        Expression.ResultStr := ErrorAddresses[EvalRegisters.L+1].Desc;
        Finished := True;

     End Else If EvalRegisters.PC = $FFFF Then Begin

        If Not Expression.SyntaxChecked Then Begin

           // Having now parsed the expression, we grab it again and
           // process it *again* to get the result.
           Expression.SyntaxChecked := True;
           Expression.ResultType := 0;
           EvalMemory[0] := 0;
           Expression.Tokenised := Insert5Bytes(Expression.Tokenised);
           EvaluateExpr(Expression);
           EvalMemory[0] := 243;
           Evaluating := False;
           // Error?
           If Expression.ResultType = 2 Then Begin
              Finished := True;
           End Else Begin
              //No Error, so we get the result.
              If EvalMemory[FLAGS] and 64 = 64 Then Begin
                 // Numeric Result
                 Expression.ResultType := 1;
                 StrStart := GetWord(@EvalMemory[STKEND])-5;
                 Expression.ResultNum := Byte5ToFloat(EvalMemory[StrStart], GetDWord(@EvalMemory[StrStart+1]));
                 Finished := True;
              End Else Begin
                 // AnsiString Result
                 Expression.ResultType := 0;
                 StrLen := (EvalMemory[GetWord(@EvalMemory[STKEND])-1] shl 8)+EvalMemory[GetWord(@EvalMemory[STKEND])-2];
                 StrStart := (EvalMemory[GetWord(@EvalMemory[STKEND])-3] shl 8)+EvalMemory[GetWord(@EvalMemory[STKEND])-4];
                 Expression.ResultStr := InsertEscapes(GetMemoryString(StrStart, StrLen, EvalMemory));
                 Expression.ResultNum := 0;
                 Finished := True;
              End;
           End;
        End Else Begin
           // Error?
           If Expression.ResultType <> 2 Then Begin
              //No Error, so we get the result.
              If EvalMemory[FLAGS] and 64 = 64 Then Begin
                 // Numeric Result
                 Expression.ResultType := 1;
                 StrStart := GetWord(@EvalMemory[STKEND])-5;
                 Expression.ResultNum := Byte5ToFloat(EvalMemory[StrStart], GetDWord(@EvalMemory[StrStart+1]));
                 Finished := True;
              End Else Begin
                 // AnsiString Result
                 Expression.ResultType := 0;
                 StrLen := (EvalMemory[GetWord(@EvalMemory[STKEND])-1] shl 8)+EvalMemory[GetWord(@EvalMemory[STKEND])-2];
                 StrStart := (EvalMemory[GetWord(@EvalMemory[STKEND])-3] shl 8)+EvalMemory[GetWord(@EvalMemory[STKEND])-4];
                 Expression.ResultStr := InsertEscapes(GetMemoryString(StrStart, StrLen, EvalMemory));
                 Expression.ResultNum := 0;
                 Finished := True;
              End;
           End Else
              Finished := True;
        End;
     End;
  End;

  // 5 - restore the emulation. We need to update the sysvar for random numbers,
  // as otherwise we'd get the same number sequence in the evaluator every time
  // the RND routine is called.

  If EvalMemory[0] <> 0 Then Begin
     NewSEED := GetWord(@EvalMemory[SEED]);
     PutWord(@Memory[SEED], NewSEED);
  End;

  Evaluating := False;

End;

Function EvaluateNum(Expression: AnsiString; Default: Integer): Extended;
Var
  Expr: TExpression;
Begin
  Expr.SyntaxChecked := False;
  Expr.Expression := Expression;
  EvaluateExpr(Expr);
  If Expr.ResultType = 1 Then
     Result := Expr.ResultNum
  Else
     Result := Default;
End;

Function EvaluateStr(Expression: AnsiString; Default: AnsiString): AnsiString;
Var
  Expr: TExpression;
Begin
  Expr.SyntaxChecked := False;
  Expr.Expression := Expression;
  EvaluateExpr(Expr);
  If Expr.ResultType = 0 Then
     Result := Expr.ResultStr
  Else
     Result := Default;
End;

end.
