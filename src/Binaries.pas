unit Binaries;

interface

Uses

  Windows, Classes, SysUtils, BASSupport, FastCore, ROMUtils, Tapes, Filing, Utility;

Type

  TBinaryMethod = (bmDecimal, bmHex, bmREM, bmASM, bmBASIC, bmMemory);

  Procedure BinaryToDATADec(Binary: String; var List: TStringlist; IncludePOKEs: Boolean; Address, StartLine, LineStep, BytesPerLine: Word);
  Procedure BinaryToDATAHex(Binary: String; var List: TStringlist; IncludePOKEs: Boolean; Address, StartLine, LineStep, BytesPerLine: Word);
  Procedure BinaryToREM(Binary: String; var List: TStringlist; CopyStub, ForceJump: Boolean; Address, LineNumber: Word);
  Procedure BinaryToAsmText(Binary: String; var List: TStringlist; BytesPerLine: Integer);
  Procedure BinaryToBASIC(Binary: String; var List: TStringlist);
  Procedure BinaryToMemory(Binary: String; Address: Word);
  Procedure BinaryToTape(BlockName:String; Binary: String; Address: Word);

implementation

Procedure BinaryToDATADec(Binary: String; var List: TStringlist; IncludePOKEs: Boolean; Address, StartLine, LineStep, BytesPerLine: Word);
Var
  CurLineNum, Count, Num, Offset: Integer;
  TempStr: String;
Begin

  // Produces DATA statements from a string. Fills the supplied TStringlist with the
  // encoded BASIC lines.

  List.Clear;

  CurLineNum := StartLine;
  Count := BytesPerLine;
  Num := Length(Binary);
  OffSet := 1;

  If IncludePOKEs Then Begin
     List.Add(IntToStr(CurLineNum)+' RESTORE '+IntToStr(CurLineNum+1)+': FOR F='+IntToStr(Address)+' TO '+IntToStr(Address+Num-1)+': READ A: POKE F,A: NEXT F');
     Inc(CurLineNum, LineStep);
  End;

  While Num > 0 Do Begin
     If Count = BytesPerLine Then Begin
        TempStr := IntToStr(CurLineNum) + ' DATA ';
        Inc(CurLineNum, LineStep);
     End;
     TempStr := TempStr + IntToStr(Ord(Binary[Offset]))+',';
     Inc(Offset);
     Dec(Count);
     If (Count = 0) or (Length(TempStr)> 240) Then Begin
        Count := BytesPerLine;
        List.Add(Copy(TempStr, 1, Length(TempStr)-1));
        TempStr := '';
     End;
     Dec(Num);
  End;

  If TempStr <> '' Then
     List.Add(Copy(TempStr, 1, Length(TempStr)-1));

End;

Procedure BinaryToDATAHex(Binary: String; var List: TStringlist; IncludePOKEs: Boolean; Address, StartLine, LineStep, BytesPerLine: Word);
Var
  CurLineNum, Count, Num, Offset: Integer;
  TempStr: String;
Begin

  // Produces DATA statements containing strings which themselves contain
  // 2-byte hex equivalents of the data. Slow to poke, but very compact.
  // Fills the supplied TStringlist with the encoded BASIC lines.

  List.Clear;

  CurLineNum := StartLine;
  Count := BytesPerLine;
  Num := Length(Binary);
  OffSet := 1;

  If IncludePOKEs Then
     Inc(CurLineNum, LineStep);

  While Num > 0 Do Begin

     If Count = BytesPerLine Then Begin
        TempStr := IntToStr(CurLineNum) + ' DATA "';
        Inc(CurLineNum, LineStep);
     End;

     TempStr := TempStr + IntToHex(Ord(Binary[Offset]), 2);

     Inc(Offset);

     Dec(Count);
     If (Count = 0) or (Length(TempStr)> 240) Then Begin
        Count := BytesPerLine;
        List.Add(TempStr+'"');
        TempStr := '';
     End;

     Dec(Num);

  End;

  If TempStr <> '' Then
     List.Add(TempStr+'"');

  If IncludePOKES Then
     List.Insert(0, IntToStr(StartLine)+' RESTORE '+IntToStr(StartLine+LineStep)+': FOR f=0 TO '+IntToStr(List.Count -1)+': READ a$: FOR g=1 TO LEN a$ STEP 2: POKE '+IntToStr(Address -1)+'+(g/2)+(f*'+IntToStr(BytesPerLine)+'),(16*(CODE(a$(g))-48-(7 AND a$(g)>"9")))+CODE(a$(g+1))-48-(7 AND a$(g+1)>"9"): NEXT g: NEXT f');

End;

Procedure BinaryToREM(Binary: String; var List: TStringlist; CopyStub, ForceJump: Boolean; Address, LineNumber: Word);
Var
  TempStr, Stub: String;
  BinLen: Integer;
Begin

  // Converts a binary string to a REM statement.
  // If specified, a small asm stub is attached which copies the data to a specified address. Optionally,
  // execution can be set to continue at this code.

  List.Clear;
  BinLen := Length(Binary);
  TempStr := IntToStr(LineNumber) + ' ';

  If CopyStub or ForceJump Then Begin

     // The code should be executed - either because the data needs to be copied out
     // (for screen data, UDGs or somesuch) or because the data itself is executable z80 code.

     TempStr := TempStr + 'RANDOMIZE USR (PEEK 23637+256*PEEK 23638-';

     If CopyStub Then Stub := #33 + #16 + #0 +                                           // Padding to 16 bytes - HL is 16 at this point
                              #237 + #74 +                                               // ADC HL,BC - HL now points at source data
                              #17 + Chr(Address And 255) + Chr(Address Shr 8) +          // LD DE,Address
                              #1  + Chr(BinLen And 255) + Chr(BinLen Shr 8) +            // LD BC,Size
                              #237 + #176;                                               // LDIR

     If ForceJump Then Stub := Stub + #195 + Chr(Address And 255) + Chr(Address Shr 8);  // JP Address

     Binary := Stub + Binary;

  End;

  TempStr := TempStr + IntToStr(Length(Binary)) + ': RETURN: REM ' + InsertEscapes(Binary);
  List.Add(TempStr);

End;

Procedure BinaryToAsmText(Binary: String; var List: TStringlist; BytesPerLine: Integer);
Var
  TempStr, CurByte: String;
  Idx, Z, ByteCount: Integer;
  ByteVal: Byte;
Begin

  List.Clear;

  Idx := 1;
  ByteCount := 0;
  TempStr := '                  db    %';

  While Idx < Length(Binary) Do begin

     CurByte := '';
     ByteVal := Ord(Binary[Idx]);

     For Z := 0 To 7 Do
        If ByteVal and (1 shl Z) = 0 Then
           CurByte := '0'+CurByte
        Else
           CurByte := '1'+CurByte;

     If ByteCount < BytesPerLine -1 Then Begin
        TempStr := TempStr + CurByte + ', %';
        Inc(ByteCount);
     End Else Begin
        TempStr := TempStr + CurByte;
        List.Add(TempStr);
        TempStr := '                  db    %';
        ByteCount := 0;
     End;

     Inc(Idx);

  End;

  If ByteCount < BytesPerLine -1 Then Begin
     TempStr := TempStr + CurByte;
     List.Add(TempStr);
  End;

End;

Procedure BinaryToBASIC(Binary: String; var List: TStringlist);
Var
  TempWord, CurAddress, Count: DWord;
  BASICLine, BASICStr, TempStr: String;
Begin

  // Extract BASIC from Binary File.

  List.Clear;
  CurAddress := 1;

  Repeat

     // If we encounter a line that starts $80xx then it's the end of the prog.
     If Ord(Binary[CurAddress]) >= 128 Then Break;

     // Line Numbers are stored big-endian
     BASICLine := '';
     TempWord := GetWord(@Binary[CurAddress]);
     TempStr := IntToStr((TempWord Shr 8) + ((TempWord and 255) Shl 8));

     If StrToInt(TempStr) > 16383 Then Exit;

     BASICLine := BASICLine + TempStr +' ';

     // Now Address the Length
     Inc(CurAddress, 2);
     // Get the line length (includes terminal #13)
     TempWord := GetWord(@Binary[CurAddress]);

     // Sanity Checks

     If Integer(TempWord + CurAddress) > Length(Binary) Then Exit;
     If TempWord = 0 Then Exit;

     Inc(CurAddress, 2);
     TempStr := '';
     For Count := 1 To TempWord -1 Do
        TempStr := TempStr + Binary[CurAddress+Count -1];

     // and now address the next line for later.
     Inc(CurAddress, TempWord);

     // Now Detokenise the line we just got.
     BASICStr := '';
     BASICStr := DetokeniseLine(TempStr, False);

     // Process for #14 5 byte number codes, and then Escape characters.
     BASICStr := InsertEscapes(BASICStr);
     List.Add(BASICLine + BASICStr);

  Until CurAddress >= DWord(Length(Binary));

End;

Procedure BinaryToMemory(Binary: String; Address: Word);
Var
  Idx: Integer;
Begin

  Idx := 1;

  While Idx <= Length(Binary) Do Begin

     If Address >= 16384 Then
        Memory[Idx + Address -1] := Ord(Binary[Idx]);

     Inc(Idx);

  End;

End;

Procedure BinaryToTape(BlockName:String; Binary: String; Address: Word);
Begin

  // Sends a binary object to the current tape file, as a CODE block.

  SetLength(FileArray, Length(Binary));
  CopyMemory(@FileArray[0], @Binary[1], Length(Binary));
  TapeBlockAdd(CODEToTape(BlockName, Address));
  TapeWindow.UpdateTapeList;
  ShowWindow(TapeWindow, False);

End;

end.


