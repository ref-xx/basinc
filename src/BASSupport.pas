unit BASSupport;

interface

Uses Windows, Math, SysUtils, Classes;

  Procedure DecodeBAS;
  Function  ProcessBASLine(CurLine: String): String;
  Procedure ParseBASLabels;
  Procedure ProcessDEFFN(var CurBASICLine: String);
  Function  FindDEFFN(Line: String): Integer;
  Procedure GetNextBASLine(var CurPos: Integer; var ResultLine: String);
  Function  GetVarDimensions(Var CurLine: String; Var LinePos, NumElements: Integer; ElementSize: Byte): String;
  Function  GetElementsNum(Var CurLine: String; Var LinePos, NumElements: Integer): String;
  Function  GetElementsStr(Var CurLine: String; Var LinePos: Integer): String;
  Function  GetString(var CurLine: String; Var LinePos: Integer): String;
  Function  GetSign(CurLine: String; var LinePos: Integer; var CurBASICLine: String): Boolean;
  Function  GetNumber(CurLine: String; var LinePos: Integer; var CurBASICLine: String; IsBinary: Boolean): Extended;
  Function  FloatTo5Byte(Value: Extended): String;
  Function  Byte5ToFloat(Exponent: Byte; Mantissa: DWord): Extended;
  Procedure ProcessEscapeChars(Var Line: String; var LPos: Integer; var BASICLine: String);
  Function  InsertEscapes(Line: String): String;
  Function  GetColourCode(Var Line: String; Var LinePos: Integer): String;
  Function  GetToken(Keyword: String): Byte;
  Function  FormatEscapes(CurLine: String): String;
  Function  Strip5Bytes(Text: String): String;
  Function  Insert5Bytes(Text: String): String;
  Function  StripWhiteSpace(Text: String): String;
  Function  GetDelimitedString(Text: String; Var Index: Integer; Delimiter: Char): String;

  Procedure SaveBAS(SaveEdit, DoSave: Boolean);

Const

  CRCtable: ARRAY[0..255] OF DWORD =
 ($00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
  $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
  $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
  $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
  $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
  $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
  $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
  $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
  $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
  $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
  $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
  $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
  $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
  $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
  $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
  $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
  $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
  $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
  $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
  $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
  $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
  $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
  $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
  $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
  $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
  $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
  $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
  $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
  $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
  $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
  $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
  $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

Var

  BASLineReadNum: Integer;
  GetNumError:    boolean;

implementation

Uses FastCore, ROMUtils, Filing, Utility, LogWind;

Function GetCRC32FromString(Str: String): String;
Var
  i, CRCValue:  DWORD;
  q: ^BYTE;
Begin
  CRCValue := $FFFFFFFF;
  q := @Str[1];
  For i := 0 TO Length(Str) -1 Do Begin
    CRCvalue := (CRCvalue Shr 8) Xor
    CRCTable[q^ Xor (CRCvalue And $000000FF)];
    Inc(q)
  End;
  Result := UpperCase(IntToHex(CRCValue, 8));
End;

Function CheckCRC: String;
Var
  Str: String;
  CurPos: Integer;
  CurLine, CRC32String, NewCRCString: String;
Begin

  Result := '';
  CRC32String := '';
  CurLine := '';
  CurPos := 0;
  Str := '';

  While CurLine <> '$$END$$' Do Begin

     GetNextBASLine(CurPos, CurLine);
     If Lowercase(Copy(CurLine, 1, 5)) = 'check' Then
        CRC32String := Uppercase(Copy(CurLine, 7, Length(CurLine)))
     Else
        Str := Str + CurLine + #13#10;

  End;

  NewCRCString := GetCRC32FromString(Str);
  If CRC32String <> '' Then Begin
     If NewCRCString <> CRC32String Then
        Log('Warning - BAS file may be corrupt');
  End Else
     Result := NewCRCString;

End;

Procedure DecodeBAS;
Var
  TempValue: Extended;
  AutoLine: Word;
  InString, Done: Boolean;
  LinePos, CurPos, LineNum, Idx, Idx2, NumElements: Integer;
  TempStr, CurBASICLine, VarSpace, CurLine, VarString: String;
Label
  AbortLine;
Begin

  // This rather lengthy procedure Decodes a .bas file
  // currently held in FileArray.
  // Avoids the use of Delphi's TStringlist.
  // It supports all of zmakebas's escape codes and labeling systems.

  Log('Loading BAS file - '+Filename);

  CheckCRC;

  BASLineReadNum := 0;

  ParseBASLabels;

  CurBASICLine := '';
  VarSpace := '';
  FileBody := '';
  CurPos := 0;
  Done := False;
  AutoLine := $8000;

  Repeat

     AbortLine:

     Inc(BASLineReadNum);
     GetNextBASLine(CurPos, CurLine);
     CurLine := CurLine + ' ';
     If CurLine = '$$END$$ ' Then Begin
        Done := True;
     End Else Begin
        // Process a line of BASIC
        If Curline <> ' ' Then Begin
           // Comments begin with a '#' in .bas files.
           If CurLine[1] <> '#' Then Begin
              // Process the special commands first, so they don't get
              // assigned a linenumber.
              // Currently, these are "Auto", "Check" and "Var".
              If Lowercase(Copy(CurLine, 1, 5)) = 'check' Then
                 // Do Nothing
              Else If LowerCase(Copy(CurLine, 1, 4)) = 'auto' Then Begin
                 // Found a line which specifies the AutoStart line number.
                 // Grab the number for later poking into the header. First,
                 // skip non-numerals.
                 LinePos := 5;
                 TempStr := '';
                 While (not (CurLine[LinePos] in ['0'..'9'])) and (linePos < Length(CurLine)+1) Do
                    Inc(LinePos);
                 // Grab the number now.
                 If LinePos < Length(CurLine)+1 Then Begin
                    While CurLine[LinePos] in ['0'..'9'] Do Begin
                       TempStr := TempStr + CurLine[LinePos];
                       Inc(LinePos);
                    End;
                    If Opt_LoadAutoStart Then
                       If Integer(GetAsyncKeyState(VK_SHIFT)) >= 0 Then
                          AutoLine := StrToIntDef(TempStr, $8000);
                 End;
              End Else If Lowercase(Copy(CurLine, 1, 3)) = 'var' Then Begin
                 // Variables declared at runtime and saved within the program
                 // This allows for example, The user to type:
                 // 10 PRINT A
                 // LET A=20
                 // SAVE "PROG" LINE 10
                 // and the program will autorun with A set to 20.

                 // Step 1 - remove all whitespace *not* inside quotes.
                 CurLine := Copy(CurLine, 4, 999999);
                 InString := False;
                 TempStr := '';
                 LinePos := 1;
                 While LinePos < Length(CurLine)+1 Do Begin
                    If CurLine[LinePos] = '"' Then InString := Not InString;
                    If (InString and (CurLine[LinePos] <= ' ')) or
                       (CurLine[LinePos] > ' ') Then TempStr := TempStr + CurLine[LinePos];
                    Inc(LinePos);
                 End;
                 CurLine := TempStr;
                 // Step 2 - Get the variable name, terminated by ":"
                 LinePos := 1;
                 TempStr := '';
                 While Not Done and (LinePos < Length(CurLine)+1) Do Begin
                    If CurLine[LinePos] = ':' Then
                       Done := True
                    Else
                       TempStr := TempStr + CurLine[LinePos];
                    Inc(LinePos);
                 End;
                 If Not Done Then Begin
                    // Var is invalid - not terminated by a ":"
                    Log('Loading BAS file - Invalid Variable');
                    Goto AbortLine;
                 End;

                 Done := False;
                 CurLine := Copy(CurLine, LinePos, 999999);
                 // if the user specified a "$" after the name, then
                 // trim it now. We only determine type on the next field.
                 If TempStr[Length(TempStr)] = '$' Then
                    TempStr := Copy(TempStr, 1, Length(TempStr)-1);
                 VarString := LowerCase(TempStr);
                 // Step 4 - Get the type of variable. Num, Str, NumArray, StrArray, NumFOR.
                 // Terminated by "=" or "(" for dimensions.
                 LinePos := 1;
                 TempStr := '';
                 While Not Done and (LinePos < Length(CurLine)+1) Do Begin
                    If (CurLine[LinePos] = '=') or (CurLine[LinePos] = '(') Then
                       Done := True
                    Else
                       TempStr := TempStr + CurLine[LinePos];
                    Inc(LinePos);
                 End;
                 If Not Done Then Begin
                    // Var is invalid - argument is not qualified with "=" or "("
                    Log('Loading BAS file - Invalid Variable');
                    Goto AbortLine;
                 End;

                 Done := False;
                 CurLine := Copy(CurLine, LinePos, 999999);
                 TempStr := lowercase(TempStr);
                 LinePos := 1;

                 // Step 5 - Determine subtype, and get the actual var.
                 // This now branches into seperate routines for each type.
                 If TempStr = 'num' Then Begin
                    // Numeric Var.
                    TempValue := GetNumber(Curline, LinePos, TempStr, False);

                    If GetNumError Then Begin
                       Log('Loading BAS file - Invalid Numvar Value');
                       Goto AbortLine;
                    End;

                    TempStr := FloatTo5Byte(TempValue);
                    If Length(VarString) = 1 Then Begin
                       // Simple Numeric
                       VarString := Chr(96+(Ord(VarString[1])-96))+TempStr;
                    End Else Begin
                       // Complex Numeric
                       LinePos := 2;
                       VarString[1] := Chr(160+(Ord(VarString[1])-96));
                       For LinePos := 2 To Length(VarString) Do
                          VarString[LinePos] := Chr(Ord(VarString[LinePos]) and $7F);
                       VarString[Length(VarString)] := Chr(Ord(VarString[Length(VarString)])+$80);
                       VarString := VarString + TempStr;
                    End;
                 End Else If TempStr = 'str' Then Begin
                    // Simple String
                    TempStr := Copy(CurLine, 1, Length(CurLine) -1);
                    CurLine := '';
                    If TempStr = '' Then Begin
                       Log('Loading BAS file - Invalid String Var Argument');
                       Goto AbortLine;
                    End;
                    VarString := Chr(64+(Ord(VarString[1])-96))+'  '+Copy(TempStr, 2, 999999);
                    PutWord(@VarString[2], Word(Length(TempStr)-1));
                 End Else If TempStr = 'numarray' Then Begin
                    // A Numeric Array. We're currently pointing at the char after "("
                    // So go and get the array dimensions.
                    TempStr := GetVarDimensions(CurLine, LinePos, NumElements, 5);
                    If TempStr = '' Then Begin
                       Log('Loading BAS file - Invalid NumArray Parameters');
                       Goto AbortLine;
                    End;
                    VarString := Chr(128+(Ord(VarString[1])-96))+TempStr;
                    LinePos := 1;
                    TempStr := GetElementsNum(CurLine, LinePos, NumElements);
                    If TempStr = '' Then Begin
                       Log('Loading BAS file - Invalid NumArray Element');
                       Goto AbortLine;
                    End;
                    VarString := VarString + TempStr;
                 End Else If TempStr = 'strarray' Then Begin
                    // A string array - process as a numeric, but elements are
                    // one byte in size, as opposed to 5.
                    TempStr := GetVarDimensions(CurLine, LinePos, NumElements, 1);
                    If TempStr = '' Then Begin
                       Log('Loading BAS file - Invalid StrArray Parameters');
                       Goto AbortLine;
                    End;
                    VarString := Chr(192+(Ord(VarString[1])-96))+TempStr;
                    LinePos := 1;
                    TempStr := GetElementsStr(CurLine, LinePos);
                    If TempStr = '' Then Begin
                       Log('Loading BAS file - Invalid StrArray Element');
                       Goto AbortLine;
                    End;
                    VarString := VarString + TempStr;
                 End Else If TempStr = 'numfor' Then Begin
                    // a Looping FOR control variable. Unlikely to be of use to the
                    // .bas writer, but needs to be saved even so.
                    // First, set the name:
                    VarString := Chr(224+(Ord(VarString[1])-96));
                    // Get the current value
                    LinePos := 1;
                    TempValue := GetNumber(CurLine, LinePos, TempStr, False);
                    If GetNumError Then Begin
                       Log('Loading BAS file - Invalid FOR Var Value');
                       Goto AbortLine;
                    End;
                    TempStr := FloatTo5Byte(TempValue);
                    VarString := VarString + TempStr;
                    // Get Limit
                    Inc(LinePos);
                    TempValue := GetNumber(CurLine, LinePos, TempStr, False);
                    If GetNumError Then Begin
                       Log('Loading BAS file - Invalid FOR Var Limit');
                       Goto AbortLine;
                    End;
                    TempStr := FloatTo5Byte(TempValue);
                    VarString := VarString + TempStr;
                    // Get Step
                    Inc(LinePos);
                    TempValue := GetNumber(CurLine, LinePos, TempStr, False);
                    If GetNumError Then Begin
                       Log('Loading BAS file - Invalid FOR Var STEP');
                       Goto AbortLine;
                    End;
                    TempStr := FloatTo5Byte(TempValue);
                    VarString := VarString + TempStr;
                    // Get Line - a WORD value
                    Inc(LinePos);
                    TempValue := GetNumber(CurLine, LinePos, TempStr, False);
                    If GetNumError Then Begin
                       Log('Loading BAS file - Invalid FOR Var line number');
                       Goto AbortLine;
                    End;
                    VarString := VarString + '  ';
                    PutWord(@VarString[Length(VarString)-1], Word(Round(TempValue)));
                    // And finally, the statement.
                    Inc(LinePos);
                    TempValue := GetNumber(CurLine, LinePos, TempStr, False);
                    If GetNumError Then Begin
                       Log('Loading BAS file - Invalid FOR Var Statement');
                       Goto AbortLine;
                    End;
                    VarString := VarString + Chr(Byte(Round(TempValue)));
                 End Else Begin
                    // Var is invalid - unknown type
                    Log('Loading BAS file - Unknown Variable "'+TempStr+'"');
                    Goto AbortLine;
                 End;
                 // Having got the Variable in VarString, now add to the VarSpace.
                 VarSpace := VarSpace + VarString;
                 Dec(LinePos);
              End Else Begin
                 // Process the line, as it must (hopefully) be BASIC text.
                 // Note that lines are inserted in line order.
                 CurBASICLine := ProcessBASLine(Copy(CurLine, 1, Length(CurLine)-1));
                 If CurBASICLine = '' Then Goto AbortLine;
                 // Find the line number
                 LineNum := (Ord(CurBASICLine[1]) Shl 8) + Ord(CurBASICLine[2]);
                 // Got the line number, find its place in the text. The current file
                 // is in tokenised Speccy format, so it's a case of grabbing words. Which are backwards :(
                 Idx := 1;
                 While True Do Begin

                    If Idx >= Length(FileBody) Then
                       Break
                    Else
                       If (Ord(FileBody[Idx]) Shl 8) + Ord(FileBody[Idx +1]) >= LineNum Then
                          Break
                       Else
                          Inc(Idx, Ord(FileBody[Idx +2])+(Ord(FileBody[Idx + 3]) Shl 8) + 4);

                 End;

                 If Idx >= Length(FileBody) Then
                    FileBody := Copy(FileBody, 1, Idx -1) + CurBASICLine + Copy(FileBody, Idx, 999999)
                 Else
                    If (Ord(FileBody[Idx]) Shl 8) + Ord(FileBody[Idx +1]) <> LineNum Then
                       FileBody := Copy(FileBody, 1, Idx -1) + CurBASICLine + Copy(FileBody, Idx, 999999)
                    Else Begin
                       Idx2 := Idx;
                       While (Idx2 < Length(FileBody)) and (FileBody[Idx2] <> #13) Do
                          Inc(Idx2);
                       FileBody := Copy(FileBody, 1, Idx -1) + CurBASICLine + Copy(FileBody, Idx2 +1, 999999);
                    End;

                 CurBASICLine := '';

              End;
           End;
        End;
     End;

  Until Done;

  // Now the file has been processed, and so we can create a header.
  // The blank header was already created above.

  // The AutoStart
  PutWord(@FileHeader[$D], AutoLine);

  // Now Just add the length of the BASIC (Without the Variables)
  PutWord(@FileHeader[$F], Word(Length(FileBody)));
  FileBody := FileBody + VarSpace;

  // Total Block Length
  PutWord(@FileHeader[$B], Word(Length(FileBody)));

  Log('Loading BAS file - Success');

End;

Function ProcessBASLine(CurLine: String): String;
Var
  CurChar, LastChar: Char;
  TempValue: Extended;
  TempWord: Word;
  InString, REMCommand: Boolean;
  LinePos, LineNum, LineLen: Integer;
  TempStr, CurBASICLine: String;
Begin

  CurBASICLine := '';
  Result := '';
  REMCommand := False;
  InString := False;
  // Lines in a .bas must start with a number.
  // so get the line number now.
  LinePos := 1;
  TempStr := '';
  While CurLine[LinePos] in ['0'..'9'] Do Begin
     TempStr := TempStr + CurLine[LinePos];
     Inc(LinePos);
  End;
  // If it's an empty string, then no numbers were found, so
  // Set PC to R Tape Loading Error.
  LineNum := StrToIntDef(TempStr, -1);
  If (LineNum < 0) or (LineNum > 999999) Then Begin
     Log('Loading BAS file - Invalid Line Number');
     DoError($1A, 'Invalid line number');
     Exit;
  End Else Begin
     // Store the line number in the FileBody.
     // with two pad bytes for the line length when it's been calculated.
     TempWord := Word(LineNum);
     CurBASICLine := CurBASICLine + Chr(TempWord shr 8) + Chr(TempWord and 255)+#0#0;
  End;
  // Having got the line number, go get the rest of the line.
  // it should be ready for tokenising, so...
  CurLine := Copy(CurLine, LinePos, 999999);
  CurLine := TokeniseLine(CurLine, False);
  LinePos := 1;
  LineLen := Length(CurLine)+1;
  CurChar := #0;
  // and it should now be filled with lovely tokens for the keywords.
  // now to do the numbers into speccy #14 + 5 Byte form, and process specials
  // such as the zmakebas's \ escape char.
  While LinePos < LineLen Do Begin
     LastChar := CurChar;
     CurChar := CurLine[LinePos];
     If Not (Instring or REMCommand) and (CurChar = '$') and not (Curline[LinePos-1] in ['a'..'z', 'A'..'Z']) Then Begin
        // A Hex number, of the $xx variety.
        // go get the number, if one exists.
        TempValue := GetNumber(CurLine, LinePos, CurBASICLine, False);
        If GetNumError Then Begin
           Log('Loading BAS file - Invalid number in line '+IntToStr(LineNum));
           Exit;
        End;
        TempStr := FloatTo5Byte(TempValue);
        CurBASICLine := CurBASICLine + #$E + TempStr;
        Dec(LinePos);
     End Else If Not (Instring or REMCommand) and (CurChar in ['0'..'9', '.', '%']) and Not (LastChar in ['0'..'9', 'a'..'z', 'A'..'Z']) then Begin
        // A number. Needs to be dumped as-is, then converted
        // to Spectrum 5-Byte floating point.
        // We don't need to know the sign of the number,
        // the ROM will do that at Runtime.
        GetSign(CurLine, LinePos, CurBASICLine);
        // go get the number, if one exists.
        TempValue := GetNumber(CurLine, LinePos, CurBASICLine, False);
        If GetNumError Then Begin
           Log('Loading BAS file - Invalid number in line '+IntToStr(LineNum));
           Exit;
        End;
        // Now just update the number with the correct sign,
        // and process to 5 bytes float/smallint format.
        TempStr := FloatTo5Byte(TempValue);
        CurBASICLine := CurBASICLine + #$E + TempStr;
        Dec(LinePos);
     End Else If CurChar = '"' Then Begin
        // This is necessary - the backslash escape code is
        // only legal inside strings.
        InString := Not InString;
        CurBASICLine := CurBASICLine + '"';
     End Else If CurChar = '\' Then Begin
        // This is an escape character, for UDGs and such.
        ProcessEscapeChars(CurLine, LinePos, CurBASICLine);
     End Else If CurChar = Chr(196) Then Begin
        // BIN is a pain, it stores the 5byte fp after the 101010...
        // Number.
        CurBASICLine := CurBASICLine + CurChar;
        Inc(LinePos);
        While (CurLine[LinePos] = ' ') and (LinePos < LineLen) Do Begin
           CurBASICLine := CurBASICLine + CurLine[LinePos];
           Inc(LinePos);
        End;
        If LinePos >= LineLen Then Begin
           Log('Loading BAS file - BIN without Argument at line '+IntToStr(LineNum));
           Exit;
        End;
        // Possible whitespace skipped, on with the number
        TempValue := GetNumber(CurLine, LinePos, CurBASICLine, True);
        If GetNumError Then Begin
           Log('Loading BAS file - Invalid number in line '+IntToStr(LineNum));
           Exit;
        End;
        If Not REMCommand Then
           CurBASICLine := CurBASICLine + #$E + FloatTo5Byte(TempValue);
        Dec(LinePos);
     End Else Begin
        // A keyword token, or probably an Alpha character,
        // as chars < Space are stripped earlier on by the call
        // to GetNextBASLine.
        If CurChar = #234 Then
           REMCommand := True;
        If (CurChar <> ' ') or InString or REMCommand Then Begin
           If CurChar = '£' Then CurChar := #96;
           CurBASICLine := CurBASICLine + CurChar;
        End;
     End;
     Inc(LinePos);
  End;

  // *Finally* we have a tokenised line. The last job is to test for DEF FN, and
  // insert the dummy bytes after each parameter.
  ProcessDEFFN(CurBASICLine);

  // Got a line, now add the $13 terminator, and add to the filebody.
  // Also update the length of the text in the 3/4th bytes.
  PutWord(@CurBASICLine[3], Word(Length(CurBASICLine)-3));
  CurBASICLine := CurBASICLine + #13;
  Result := CurBASICLine;

End;

Procedure ProcessDEFFN(var CurBASICLine: String);
Var
  LinePos: Integer;
  TempStr: String;
  TempWord: Word;
  DEFFNDone, DEFFNHasParams: Boolean;
Begin
  LinePos := FindDEFFN(CurBASICLine);
  If LinePos <> 0 Then Begin
     Inc(LinePos);
     DEFFNDone := False;
     DEFFNHasParams := False;
     While Not DEFFNDone Do Begin
        // Find either the ")" or the next "," - whichever comes first.
        // If we find an Alpha or a "," then we have parameters.
        If CurBASICLine[LinePos] in ['a'..'z', 'A'..'Z'] Then
           DEFFNHasParams := True;
        If CurBASICLine[LinePos] = ')' Then Begin
           DEFFNDone := True;
           If DEFFNHasParams Then Begin
              If CurBASICLine[LinePos-1] in ['a'..'z', 'A'..'Z', '$'] Then
                 CurBASICLine := Copy(CurBASICLine, 1, LinePos-1)+#$0E#0#0#0#0#0+Copy(CurBASICLine, LinePos, 999999);
           End;
        End;
        If CurBASICLine[LinePos] = ',' Then Begin
           DEFFNHasParams := True;
           If CurBASICLine[LinePos-1] in ['a'..'z', 'A'..'Z', '$'] Then
              CurBASICLine := Copy(CurBASICLine, 1, LinePos-1)+#$0E#0#0#0#0#0+Copy(CurBASICLine, LinePos, 999999);
           Inc(LinePos, 6);
        End;
        Inc(LinePos);
        If LinePos >= Length(CurBASICLine) Then DEFFNDone := True;
        // Test to see if there's any more DEF FNs on the line
        If DEFFNDone Then Begin
           TempStr := Copy(CurBASICLine, LinePos+1, 999999);
           TempWord := FindDEFFN(TempStr);
           If TempWord <> 0 Then Begin
              Inc(LinePos, TempWord+1);
              DEFFNDone := False;
           End;
        End;
     End;
  End;
End;

Function FindDEFFN(Line: String): Integer;
Var
  Found: Boolean;
Begin
  Result := 0;
  Found := False;
  If Length(Line) >= 1 Then Begin
     Result := 1; //r16 fix by arda //Result := 5; R15 fix by dunny
     While Not Found and (Result < Length(Line)) Do Begin
        If Line[Result] = #$CE Then Found := True;
        Inc(Result);
     End;
  End;
  If Not Found Then
     Result := 0;
End;

Function GetString(var CurLine: String; Var LinePos: Integer): String;
Var
  TempStr: String;
  LPos: Integer;
Begin
  // Gets, and removes, a string delimited by quote marks from
  // Curline.
  TempStr := '';
  Result := '';
  // step past the initial quote mark, if available.
  If CurLine[LinePos] <> '"' Then Begin
     Log('Invalid String - No opening quotes');
     Exit;
  End;
  Inc(LinePos);
  While CurLine[LinePos] <> '"' Do Begin
     TempStr := TempStr + CurLine[LinePos];
     Inc(LinePos);
  End;
  // And step past the closing '"' mark.
  If CurLine[LinePos] <> '"' Then Begin
     Log('Invalid String - No closing quotes');
     Exit;
  End;
  Inc(LinePos);
  CurLine := Copy(CurLine, LinePos, 999999);
  // Now process the string for escape codes
  LPos := 1;
  Result := '"';
  While LPos < Length(TempStr)+1 Do Begin
     If TempStr[LPos] = '\' Then
        ProcessEscapeChars(TempStr, LPos, Result)
     Else
        Result := Result + TempStr[LPos];
     Inc(LPos);
  End;
End;

Function GetVarDimensions(Var CurLine: String; Var LinePos, NumElements: Integer; ElementSize: Byte): String;
Var
  Done: Boolean;
  Count: Integer;
  TempStr: String;
  TempValue: Extended;
  TempWord: Word;
Begin
  // Grabs the dimensions of an array (in "()"s) from a string.
  // And returns the Variable as it is constructed in memory.
  // Does not get the elements though - that is a job for the calling procedure,
  // Which will also have to fill in the variable name.
  Result := '   ';
  Done := False;
  Count := 0;
  NumElements := 1;
  // Add in the first "(" as it's been trimmed.
  CurLine := '('+CurLine;
  // now repeat, getting numbers until EOL reached.
  While Not Done Do begin
     If LinePos >= Length(CurLine) Then Begin
        Result := '';
        Exit;
     End;
     If CurLine[LinePos] = ')' Then
        Done := True
     Else Begin
        Inc(LinePos);
        TempValue := GetNumber(CurLine, LinePos, TempStr, False);
        Result := Result + '  ';
        PutWord(@Result[Length(Result)-1], Word(Round(TempValue)));
        NumElements := NumElements * Round(TempValue);
        Inc(Count);
     End;
  End;
  If Count = 0 Then Begin
     Result := '';
     Exit;
  End;
  Result[3] := Chr(Count);
  TempWord := (Count *2)+(NumElements*ElementSize)+1;
  PutWord(@Result[1], TempWord);
  // Now discard the array dimensions part, ready for the calling proc
  // to process the elements.
  CurLine := Copy(CurLine, LinePos+1, 999999);
  Inc(LinePos);
End;

Function GetElementsNum(Var CurLine: String; Var LinePos, NumElements: Integer): String;
Var
  TempStr, NumStr: String;
  Count: Integer;
Begin
  Result := '';
  Count := NumElements;
  // repeat, getting numbers until count reached.
  While Count <> 0 Do begin
     Inc(LinePos);
     NumStr := FloatTo5Byte(GetNumber(CurLine, LinePos, TempStr, False));
     If NumStr = '' Then Begin
        Result := '';
        Exit;
     End;
     Result := Result + NumStr;
     CurLine := Copy(CurLine, LinePos, 999999);
     LinePos := 1;
     Dec(Count);
  End;
End;

Function GetElementsStr(Var CurLine: String; Var LinePos: Integer): String;
Var
  TempStr: String;
Begin
  TempStr := '';
  // repeat, getting Quote delimited strings, until EOL reached.
  While CurLine <> '' Do begin
     If (CurLine[LinePos] <> ',') And (CurLine[LinePos+1] <> '"') Then Begin
        Result := '';
        Exit;
     End;
     // Step over the , and the "
     Inc(LinePos, 2);
     While (LinePos < Length(CurLine)) And (CurLine[LinePos] <> '"') Do Begin
        TempStr := TempStr + CurLine[LinePos];
        Inc(LinePos);
     End;
     CurLine := Copy(CurLine, LinePos+1, 999999);
     LinePos := 1;
  End;
  // Now process the string for escape codes
  Result := '';
  While LinePos < Length(TempStr)+1 Do Begin
     If TempStr[LinePos] = '\' Then
        ProcessEscapeChars(TempStr, LinePos, Result)
     Else
        Result := Result + TempStr[LinePos];
     Inc(LinePos);
  End;
  linePos := 1;
End;

Function GetSign(CurLine: String; var LinePos: Integer; var CurBASICLine: String): Boolean;
Begin
  // Grabs the sign of a number from text, +/- chars.
  // You can add the chars found to the BASIC text by specifying
  // a valid string as CurBASICLine. Specifying nil will get the sign
  // without copying chars to the BASIC.
  Result := False;
  While ((CurLine[LinePos] = '+') or (Curline[LinePos] = '-')) and
        (LinePos < Length(CurLine)+1) Do Begin
     // Any number of +,- can precede a number.
     If CurLine[LinePos] = '-' Then Result := Not Result;
     If @CurBASICLine <> nil Then
        CurBASICLine := CurBASICLine + CurLine[LinePos];
     Inc(LinePos);
  End;
End;

Function GetNumber(CurLine: String; var LinePos: Integer; var CurBASICLine: String; IsBinary: Boolean): Extended;
Var
  ExpSign: Boolean;
  Divider: Extended;
  Exponent: Integer;
  CurChar: Char;
  TempDWord: DWord;
  IsNegative: Boolean;
  TempStr: String;
Begin
  // Like GetSign, but returns a number rather than a sign.
  // The calling proc must ensure that the char pointed at by linepos, in
  // curline, is a valid digit. Any sign should be processed before this
  // function is called.
  // Handles both Binary (BIN 10101/%10101) and Hex (0x0000/$0000) formats.

  Result := 0.0;
  If Length(CurLine) < LinePos Then Exit;

  // First, test for +/- modifiers
  IsNegative := False;
  While CurLine[LinePos] in ['-', '+'] Do Begin
     If CurLine[LinePos] = '-' Then IsNegative := Not IsNegative;
     Inc(LinePos);
  End;

  // Next, test for Hex.
  GetNumError := False;
  If ((CurLine[LinePos+1] in ['x', 'X']) and
      (Curline[LinePos] = '0') and Not
      (CurLine[LinePos-1] in ['a'..'z', 'A'..'Z', '0'..'9'])) or
      ((CurLine[LinePos] = '$') and (CurLine[LinePos+1] in ['0'..'9'])) Then Begin
     // This is a hex number.
     CurBASICLine := CurBASICLine + '0x';
     TempDWord := 0;
     Inc(LinePos);
     If CurLine[LinePos] in ['x', 'X'] Then Inc(linePos);
     While CurLine[LinePos] in ['0'..'9', 'A'..'F', 'a'..'f'] Do Begin
        CurChar := CurLine[LinePos];
        If CurChar in ['a'..'f'] Then CurChar := Chr(Ord(CurChar)-32);
        If CurChar in ['A'..'F'] Then
           TempDWord := (TempDWord Shl 4) + Ord(CurChar)-55
        Else
           TempDWord := (TempDWord shl 4) + Ord(CurChar)-48;
        CurBASICLine := CurBASICLine + CurLine[LinePos];
        Inc(LinePos);
     End;
     Result := TempDWord;
  End Else If IsBinary or (CurLine[LinePos] = '%') Then Begin
     // It's not hex, the calling proc expects binary, which may not exist
     TempDWord := 0;
     If CurLine[LinePos] = '%' Then Inc(LinePos);
     If Not IsBinary Then TempStr := '%';
     While CurLine[LinePos] in ['0', '1'] Do Begin
        TempStr := TempStr + CurLine[LinePos];
        TempDWord := (TempDWord Shl 1) + (Ord(CurLine[LinePos])-48);
        Inc(LinePos);
     End;
      If (TempStr <> '%') and (TempStr <> '') Then
        CurBASICLine := CurBASICLine + TempStr;
     Result := TempDWord;
  End Else Begin
     // Not expecting Binary, not found any hex, so...
     // Must be a number.
     If Not (CurLine[LinePos] in ['0'..'9', '.', 'e', 'E']) Then Begin
        GetNumError := True;
        Exit;
     End;
     Result := 0;
     While (CurLine[LinePos] in ['0'..'9']) And (LinePos < Length(CurLine)+1) Do Begin
        Result := (Result *10)+(Ord(CurLine[LinePos])-48);
        CurBASICLine := CurBASICLine + CurLine[LinePos];
        Inc(LinePos);
     End;
     // Now test for 'E' and '.'
     If CurLine[LinePos] = '.' Then Begin
        // the point *must* be followed by numbers.
        CurBASICLine := CurBASICLine + '.';
        Inc(LinePos);
        Divider := 1;
        While CurLine[LinePos] in ['0'..'9'] Do Begin
           CurBASICLine := CurBASICLine + CurLine[LinePos];
           Divider := Divider /10;
           Result := Result + (Divider*(Ord(CurLine[LinePos])-48));
           Inc(LinePos);
        End;
     End;
     If (Curline[Linepos] = 'e') or (Curline[LinePos] = 'E') Then Begin
        CurBASICLine := CurBASICLine + CurLine[LinePos];
        Inc(LinePos);
        // 'E' must be followed by an optional single +/- sign,
        // and then an integer.
        Exponent := 0;
        EXPSign := True;
        If CurLine[LinePos] = '+' Then Begin
           CurBASICLine := CurBASICLine + '+';
           Inc(LinePos);
        End Else If CurLine[linePos] = '-' Then Begin
           CurBASICLine := CurBASICLine + '-';
           Inc(LinePos);
           EXPSign := False;
        End;
        While CurLine[LinePos] in ['0'..'9'] Do Begin
           Exponent := (Exponent *10)+(Ord(CurLine[LinePos])-48);
           CurBASICLine := CurBASICLine + CurLine[LinePos];
           Inc(LinePos);
        End;
        // We have an exponent, now calculate the new value
        If EXPSign Then
           Result := Result * Power(10, Exponent)
        Else
           Result := Result / Power(10, Exponent);
     End;
     If IsNegative Then Result := -Result;
  End;
End;

Function FloatTo5Byte(Value: Extended): String;
Var
  Sign: Byte;
  Mantissa: DWord;
  Exp: Integer;
  NewVal: Integer;
Begin

  Result := '';

  // If it's a small integer, then store as such, else
  // it's the 5 byte float.

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

     // 5 byte floating point.

     // Determine the sign
     If Value < 0 Then Begin
        Sign := $80;
        Value := -Value;
     End Else
        Sign := 0;

     // Find the exponent.
     Exp := Floor(log2(Value));
     If (Exp < -129) or (Exp > 126) Then Exit;

     // And the mantissa. Multiply by a big number for later shifting
     Mantissa := Round(((Value/Power(2.0, Exp) -1.0) * 2147483648) + 0.5);

     // Now store the bytes. Shift the by now huge mantissa
     // and grab each byte as it falls off the end.
     // First the Exponent.
     Result :=          Chr(Byte(  Exp + $81));
     // Next the Mantissa - note that we ignore the high bit of the first byte.
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
  // Converts from Spectrum 5Byte format to a floating point format.
  // This is primarily for the Variables converter, so that they can
  // be saved in Human-Readable form.
  Set8087CW($133f);
  If Exponent = 0 Then Begin
     // the SmallINT form.
     Result := (Mantissa Shr 8) and 65535;
     If (Mantissa And 255) = $FF Then Result := Result - 65536;
  End Else Begin
     // the more complex floating point form
     // The mantissa as it arrives in DWord form is backwards -
     // Sort that out now :)
     Mantissa := (Mantissa Shr 24)+(((Mantissa Shr 16) And $FF) Shl 8)+(((Mantissa Shr 8) and $FF) Shl 16)+((Mantissa And $FF) Shl 24);
     Result := Power(2.0, Exponent - $81) * (((Mantissa And $7FFFFFFF)/2147483648)+1.0);
     If (Mantissa Shr 24) and $80 = $80 Then Result := -Result;
     // Now, correct for the higher accuracy that Delphi uses.
     ResStr := FloatToStrEx(Result);
     If Length(ResStr) > 9 Then Begin
        Result := Result * (Power(10, Length(ResStr)-9));
        Result := Round(Result) / (Power(10, Length(ResStr)-9));
     End;
  End;
End;

Procedure GetNextBASLine(var CurPos: Integer; var ResultLine: String);
Var
  CurChar: String;
  ArrayLen, Idx: Integer;
Label
  Start;
Begin

  // Grabs the next line of ASCII .bas from FileArray.

  ResultLine := '';

Start:

  CurChar := '';
  ArrayLen := Length(FileArray);
  If CurPos >= ArrayLen Then Begin
     // We've reached the end of the file.
     ResultLine := '$$END$$';
     Exit;
  End Else Repeat
     CurChar := Chr(FileArray[CurPos]);
     Inc(CurPos);
     If CurChar >= ' ' Then ResultLine := ResultLine + CurChar;
     // Tabs are ignored.
  Until ((CurChar < ' ') and (ResultLine <> '') and (CurChar > #9)) or (CurPos = ArrayLen);
  If ResultLine <> '' Then Begin
     // Remove trailing Whitespace
     While (Copy(ResultLine, Length(ResultLine), 1) <= ' ') and (ResultLine <> '') Do
        ResultLine := Copy(ResultLine, 1, Length(ResultLine) -1);
     If Copy(ResultLine, Length(ResultLine), 1) = '\' Then Begin
        // The \ escape char at the end of a line means
        // that the next line is a continuation of this one.
        // However, this is only for subsequent lines that do not start with a numeric - they start a new program line.
        ResultLine := Copy(ResultLine, 1, Length(ResultLine) -1);
        While Chr(FileArray[CurPos]) <= ' ' Do Inc(CurPos);
        If Not (Chr(FileArray[CurPos]) in ['0'..'9']) Then Goto Start;
     End;
  End;
  If ResultLine <> '' Then Begin
     // Remove Whitespace, leading.
     While Copy(ResultLine, 1, 1) <= ' ' Do ResultLine := Copy(ResultLine, 2, 999999);
     If BASLineReadNum > 0 Then Begin
        // And filter/replace bad OCR chars
        Idx := 1;
        While Idx <= Length(ResultLine) Do Begin
           If ResultLine[Idx] > #127 Then
              Log(ExtractFilename(Filename)+': Invalid Char at line '+IntToStr(BASLineReadNum));
           Case ResultLine[Idx] Of
              #151: ResultLine[Idx] := '-';
              #169: ResultLine[Idx] := #127;
              #171: ResultLine[Idx] := '<';
              #178: ResultLine[Idx] := '2';
              #179: ResultLine[Idx] := '3';
              #187: ResultLine[Idx] := '>';
           End;
           Inc(Idx);
        End;
     End;
  End;

End;

Procedure ParseBASLabels;
Var
  UsingLabels, Done: Boolean;
  CurLine, Labels, NewBAS, TempStr: String;
  StepVal, StartVal, CurPos, NumLines, LPos, NPos, QPos: Integer;
Begin

  // FileArray contains a freshly loaded .bas file,
  // but we need to determine if it contains labels or not.
  // this involves two passes: one to grab the relevant lines,
  // whilst picking up label names, and another to resolve the labels,
  // and insert line numbers.

  // Note - just like zmakebas, the @ symbol denotes a label, even inside quotes.
  // BASin will not save with labels, so these will not be replaced, as no lines
  // will start with "@". Beware that any .bas files handwritten to use labels that
  // use the "@" in strings will get them replaced, so you'll have to use "\@" to get
  // the "@" symbol.

  UsingLabels := False;
  Labels := '';
  NumLines := 0;
  CurPos := 0;

  While CurLine <> '$$END$$' Do Begin
     GetNextBASLine(CurPos, CurLine);
     If CurLine <> '' Then Begin
        If CurLine[1] <> '#' Then Begin
           // non - #comment lines are the ones we're interested in
           // Strip leading spaces, and check for labels.
           While CurLine[1] <= ' ' Do CurLine := Copy(CurLine, 2, 999999);
           If CurLine[1] = '@' Then Begin
              UsingLabels := True;
              // Grab the label, store the line number for later reference.
              TempStr := '';
              LPos := 2;
              While (CurLine[LPos] in ['0'..'9', 'a'..'z', 'A'..'Z']) and
                    (LPos <= Length(CurLine)) Do Begin
                 TempStr := TempStr + CurLine[LPos];
                 Inc(LPos);
              End;
              // remove the label from the line, +1 to account for the ":"
              // at the end of the label.
              CurLine := Copy(CurLine, LPos+1, 999999);
              // The label references this line, so store this line number.
              Labels := Labels+'|'+TempStr+'\'+IntTostr(NumLines);
           End;
           If Curline <> '' Then Begin
              NewBAS := NewBAS + CurLine + #10#13;
              If (LowerCase(Copy(CurLine, 1, 4)) <> 'auto') and
                 (LowerCase(Copy(CurLine, 1, 3)) <> 'var') Then Inc(NumLines);
           End;
        End;
     End;
  End;

  If not UsingLabels Then exit;

  Labels := Labels + '|';

  // Labels have been found, and their positions stored.
  // Copy the new .bas file into the FileArray.

  SetLength(FileArray, Length(NewBAS));
  CopyMemory(@FileArray[0], @NewBAS[1], Length(NewBAS));
  NewBAS := '';

  // now determine the step value of the renumber.

  StepVal := 10;
  StartVal := 10;
  While StepVal*(Numlines+1) > 999999 Do StepVal := StepVal Div 2;

  // And now start to renumber. In here, also test for the @label,
  // and replace with the correct value, which can now be calculated.

  CurLine := '';
  CurPos := 0;
  NumLines := 0;
  Done := False;

  Repeat
     GetNextBASLine(CurPos, CurLine);
     If CurLine <> '$$END$$' Then Begin
        // the line is guranteed to be a valid speccy line, as all
        // labels and comments have been removed.
        If (LowerCase(Copy(CurLine, 1, 4)) <> 'auto') and
           (LowerCase(Copy(CurLine, 1, 3)) <> 'var') Then Begin
           CurLine := IntToStr((NumLines*StepVal)+StartVal)+' '+CurLine;
           Inc(NumLines);
        End;
        LPos := 1;
        While LPos < Length(CurLine)+1 Do Begin
           If CurLine[LPos] = '@' Then Begin
              // Found an "@" char - if it's not an escape sequence
              // (the preceding char was not a "\") then replace it with
              // the correct line number.
              If CurLine[LPos-1] <> '\' Then Begin
                 NPos := LPos;
                 Inc(LPos);
                 TempStr := '|';
                 While (CurLine[LPos] in ['0'..'9', 'a'..'z', 'A'..'Z']) and
                       (LPos <= Length(CurLine)) Do Begin
                    TempStr := TempStr + CurLine[LPos];
                    Inc(LPos);
                 End;
                 TempStr := TempStr+'\';
                 // Got the label, so now grab the line number it came from,
                 // and update CurLine accordingly.
                 QPos := Pos(TempStr, Labels);
                 If QPos = 0 Then Begin
                    Log('Loading BAS file - Invalid Label (@'+Copy(TempStr, 2, Length(TempStr)-2)+')');
                 End Else Begin
                    TempStr := Copy(Labels, QPos, 999999);
                    TempStr := Copy(TempStr, Pos('\', TempStr)+1, 999999);
                    QPos := StrToInt(Copy(TempStr, 1, Pos('|', TempStr)-1));
                    // QPos is now the line number it came from.
                    // so multiply out...
                    QPos := (QPos * StepVal) + StartVal;
                    // now insert into the line.
                    CurLine := Copy(CurLine, 1, NPos-1)+IntToStr(QPos)+Copy(CurLine, LPos, 999999);
                    Inc(LPos, Length(IntToStr(QPos))-1);
                 End;
              End;
           End;
           Inc(LPos);
        End;
        NewBAS := NewBAS + CurLine + #10#13;
     End Else
        Done := True;
  Until Done;

  // Copy the new .bas file into the FileArray.

  SetLength(FileArray, Length(NewBAS));
  CopyMemory(@FileArray[0], @NewBAS[1], Length(NewBAS));

End;

Procedure ProcessEscapeChars(Var Line: String; var LPos: Integer; var BASICLine: String);
Var
  CurChar: Char;
  TempStr: String;
  LineLen: Integer;
  TempValue: Extended;
  ColourBase, MaxColour: Byte;
Begin
  LineLen := Length(Line)+1;
  Inc(LPos);
  CurChar := Line[LPos];
  If CurChar in ['A'..'Z'] Then CurChar := Chr(Ord(CurChar)+32);
  If CurChar in ['a'..'u'] Then Begin
     // normal UDGS
     CurChar := Chr((ord(CurChar)-97)+144);
     BASICLine := BASICLine + CurChar;
  End Else If CurChar = '*' Then Begin
     // Copyright Symbol
     CurChar := Chr(127);
     BASICLine := BASICLine + CurChar;
  End Else If CurChar = '`' Then Begin
     // Pound Sign
     CurChar := #96;
     BASICLine := BASICLine + CurChar;
  End Else If CurChar in [' ', '.', #39, ':'] Then Begin
     // Escape codes for gfxchars
     TempStr := CurChar;
     Inc(LPos);
     TempStr := TempStr + Line[LPos];
     If TempStr = '  '   Then CurChar := Chr(128);
     If TempStr = ' '#39 Then CurChar := Chr(129);
     If TempStr = #39' ' Then CurChar := Chr(130);
     If TempStr = #39#39 Then CurChar := Chr(131);
     If TempStr = ' .'   THen CurChar := Chr(132);
     If TempStr = ' :'   Then CurChar := Chr(133);
     If TempStr = #39'.' Then CurChar := Chr(134);
     If TempStr = #39':' Then CurChar := Chr(135);
     If TempStr = '. '   Then CurChar := Chr(136);
     If TempStr = '.'#39 Then CurChar := Chr(137);
     If TempStr = ': '   Then CurChar := Chr(138);
     If TempStr = ':'#39 Then CurChar := Chr(139);
     If TempStr = '..'   Then CurChar := Chr(140);
     If TempStr = '.:'   Then CurChar := Chr(141);
     If TempStr = ':.'   Then CurChar := Chr(142);
     If TempStr = '::'   Then CurChar := Chr(143);
     BASICLine := BASICLine + CurChar;
  End Else If CurChar = '#' Then Begin
     // An embedded ASCII code
     Inc(LPos);
     TempValue := 0;
     TempStr := Copy(Line, LPos, 3);
     While TempStr <> '' Do Begin
        If TempStr[1] in ['0'..'9'] Then
           TempValue := (TempValue * 10)+Ord(TempStr[1])-48;
        TempStr := Copy(TempStr, 2, 999);
     End;
     BASICLine := BASICLine + Chr(Round(TempValue) and 255);
     Inc(LPos, 2);
  End Else If CurChar = '{' Then Begin
     // Embedded Colour controls
     // Uses {} to surround a group of colours, using the form
     // i for ink, p for paper, f for flash, b for bright.
     Inc(LPos);
     While (Line[LPos] <> '}') and (LPos < LineLen) do Begin
        CurChar := Line[LPos];
        Inc(LPos);
        // Only apply if Colourbase <> $FF after processing
        // which allows INVERSE VIDEO and TRUE VIDEO to be
        // Processed correctly.
        ColourBase := $FF;
        MaxColour := 0;
        If CurChar in ['A'..'Z'] Then CurChar := Chr(Ord(CurChar)-32);
        If CurChar = 'i' Then Begin
           // set INK (Chr$ 24+arg)
           ColourBase := 16;
           MaxColour := 9;
        End Else If CurChar = 'p' Then Begin
           // Set PAPER (Chr$ 16+arg)
           ColourBase := 17;
           MaxColour := 9;
        End Else If CurChar = 'f' Then Begin
           // Set FLASH (Chr$ 0+arg)
           ColourBase := 18;
           MaxColour := 1;
        End Else If CurChar = 'b' Then Begin
           // Set BRIGHT (Chr$ 2+arg)
           ColourBase := 19;
           MaxColour := 1;
        End Else If CurChar = 'o' Then Begin
           // Set BRIGHT (Chr$ 2+arg)
           ColourBase := 21;
           MaxColour := 1;
        End Else If CurChar = 't' Then Begin
           // Set TAB (Chr$ 2+arg)
           ColourBase := 23;
           MaxColour := 255;
        End Else If CurChar = 'a' Then Begin
           ColourBase := 22;
           MaxColour := 254;
        End Else If CurChar = 'v' Then Begin
           // Set VIDEO Mode - "vn" is normal, "vi" is inverse.
           If Line[LPos] in ['n', 'N'] Then Begin
              // Normal mode
              BASICLine := BASICLine + Chr(20) + Chr(0);
           End Else If Line[LPos] in ['i', 'I'] Then Begin
              // Inverse Mode
              BASICLine := BASICLine + Chr(20) + Chr(1);
           End Else Begin
              // Invalid Video Mode
              DoError($1A, 'BAS: Invalid Video Mode');
              Exit;
           End;
        End Else Begin
           // Invalid Colour code
           Log('Invalid Escape code - invalid colour');
           DoError($1A, 'BAS: Invalid colour');
           Exit;
        End;
        // We got here with a valid Colour code prefix
        // If ColourBase <> $FF, then we need a colour value.
        If ColourBase <> $FF Then Begin
           If MaxColour = $FF Then Begin
              // This is for the "t" TAB Char.
              TempValue := GetNumber(Line, LPos, TempStr, False);
              BASICLine := BASICLine + Chr(ColourBase) + Chr(Round(TempValue) and 255);
              Dec(Lpos);
           End Else If MaxColour = $FE Then Begin
              // The AT Character - 2 numbers, "," seperated.
              TempValue := GetNumber(Line, LPos, TempStr, False);
              BASICLine := BASICLine + Chr(ColourBase) + Chr(Round(TempValue) and 255);
              If Line[LPos] <> ',' Then Begin
                 Log('Invalid Escape code - invalid AT Control');
                 DoError($1A, 'BAS: Invalid AT control');
                 Exit;
              End Else Inc(LPos);
              TempValue := GetNumber(Line, LPos, TempStr, False);
              BASICLine := BASICLine + Chr(ColourBase) + Chr(Round(TempValue) and 255);
              Dec(Lpos);
           End Else If Line[LPos] in ['0'..'9'] Then Begin
              If Ord(Line[LPos]) -48 <= MaxColour Then Begin
                 BASICLine := BASICLine +
                                 Chr(ColourBase) +
                                 Chr(Ord(Line[LPos]) -48);
              End Else Begin
                 Log('Invalid Escape code - invalid colour (Out of Range)');
                 DoError($1A, 'BAS: Colour out of range');
                 Exit;
              End;
           End Else Begin
              Log('Invalid Escape code - invalid colour (Out of Range)');
              DoError($1A, 'BAS: Colour out of range');
              Exit;
           End;
        End;
        Inc(LPos);
     End;
  End Else Begin
     // Not an escape character as such, but we have to treat
     // it as one, so output the next char *only*.
     BASICLine := BASICLine + CurChar;
  End;
End;

Function InsertEscapes(Line: String): String;
Var
  LinePos: Integer;
  InColours: Boolean;
  TempStr, ColourString: String;
Const
  BlockChars: Array[0..15] of String =
     ('  ', ' '#39, #39' ', #39#39, ' .', ' :', #39'.',
      #39':', '. ', '.'#39, ': ', ':'#39, '..', '.:', ':.', '::');
Begin
  InColours := False;
  Result := '';
  LinePos := 1;
  While (LinePos <= Length(Line)+1) and (Line <> '') Do Begin
     If Not (Line[LinePos] in [#16..#23]) And InColours Then Begin
        If InColours and (TempStr <> '\{') Then Begin
           Result := Result + TempStr + '}';
           InColours := False;
        End;
     End;
     Case Ord(Line[LinePos]) Of
        16..23:
           Begin
              // Colour codes.
              // These might *not* actually be colour controls, so let's make sure they are,
              // or at least that when they get expanded, they return to their original form :-)
              If Not InColours Then TempStr := '\{';
              InColours := True;
              ColourString := GetColourCode(Line, LinePos);
              If ColourString[2] <> '#' Then Begin
                 // A valid colour code - add it to the list
                 TempStr := TempStr + ColourString;
              End Else Begin
                 // Invalid - Close the {} group, if not empty, and continue.
                 // Add the colour code, but not the argument, let the next iteration catch that.
                 InColours := False;
                 If TempStr <> '\{' Then Result := Result + TempStr + '}';
                 Result := Result + ColourString;
              End;
           End;
        128..143:
           Begin
              // Block Graphics chars
              Result := Result + '\' + BlockChars[Ord(Line[LinePos])-128];
              Inc(LinePos);
           End;
        144..164:
           Begin
              // UDG Characters
              Result := Result + '\' + Chr(97+Ord(Line[LinePos])-144);
              Inc(LinePos);
           End;
        96:
           Begin
              // £ (Pound) Symbol
              Result := Result + '\`';
              Inc(LinePos);
           End;
        127:
           Begin
              // (C) Copyright Symbol
              Result := Result + '\*';
              Inc(LinePos);
           End;
        ord('\'):
           Begin
              // a backslash.
              Result := Result + '\\';
              Inc(LinePos);
           End;
        ord('@'):
           Begin
              // an @ symbol - not strictly necessary, but here for
              // complete zmakebas compatibility.
              Result := Result + '\@';
              Inc(LinePos);
           End;
     Else
        Begin
           If ((Line[LinePos] >= #32) and (Line[LinePos] <= #164)) or ((TempStr <> '') and (LinePos = Length(Line)+1)) Then Begin
              // A normal Ascii Char
              If InColours and (TempStr <> '\{') Then Begin
                 Result := Result + TempStr + '}';
                 InColours := False;
              End;
              If LinePos < Length(Line)+1 Then
                 Result := Result + Line[LinePos];
              TempStr := '';
           End Else If LinePos < Length(Line)+1 Then Begin
              // The Detokeniser(tm) will have left any tokens that may be present in
              // Strings alone. Although you can't edit them inside BASin, you can include
              // them by editing the .bas in a text editor.
              Result := Result + '\#';
              TempStr := IntToStr(Ord(Line[LinePos]));
              While Length(TempStr) < 3 Do TempStr := '0'+TempStr;
              Result := Result + TempStr;
              TempStr := '';
           End Else
              TempStr := '';
           Inc(LinePos);
        End;
     End;
  End;
End;

Function GetColourCode(Var Line: String; Var LinePos: Integer): String;
Var
  TempStr: String;
Const
  Colours: String = 'ipfbvoat';
Label
  InvalidColour;
Begin
  Result := Colours[Ord(Line[LinePos])-15];
  If Line[LinePos] <> #20 Then Begin
     // Not a Video mode, is it the AT control?
     If Line[LinePos] = #22 Then Begin
        // AT takes two bytes.
        TempStr := IntToStr(Ord(Line[LinePos+1]));
        Result := Result + TempStr+',';
        TempStr := IntToStr(Ord(Line[LinePos+2]));
        Result := Result + TempStr;
        Inc(LinePos, 3);
     End Else Begin
        // All others take one byte parameters.
        If Line[LinePos+1] in [#0..#9] Then Begin
           Result := Result + Chr(Ord(Line[LinePos+1])+48);
           Inc(LinePos, 2);
        End Else
           Goto InvalidColour;
     End;
  End Else Begin
     // Inverse or true video
     If Line[LinePos+1] = #0 Then
        Result := Result + 'n'
     Else if Line[LinePos+1] = #1 Then
        Result := Result + 'i'
     Else If Line[LinePos+1] in [#0..#9] Then
        Result := Result + Chr(Ord(Line[LinePos+1])+48)
     Else
        Goto InvalidColour;
     Inc(LinePos, 2);
  End;
  Exit;

InvalidColour:

  Result := '\#';
  TempStr := IntToStr(Ord(Line[LinePos]));
  While Length(TempStr) < 3 Do TempStr := '0'+TempStr;
  Result := Result + TempStr;
  Inc(LinePos);

End;

Procedure SaveBAS(SaveEdit, DoSave: Boolean);
Var
  TempByte: Byte;
  LinePos: Integer;
  TempValue: Extended;
  TempDWord, SaveDWord: DWord;
  Done, FirstLine, InString: Boolean;
  BASICStr, TempStr, VarString: String;
  AutoStart, TempWord, VarAddress, CurAddress, Idx: Word;
  AVars: Array[1..6] of String;
Begin

  // Saves a program from the BASIC area.
  // IX points at the address to start grabbing data from,
  // Fileheader contains info about autostart.

  FileBody := '';
  CurAddress := GetWord(@Memory[PROG]);

  // If we need to save the edit line, then do so now.
  // This precludes the AutoStart.


  If SaveEdit Then Begin

     FileBody := FileBody + '# The current Edit line'+#13#10#13#10;
     FileBody := FileBody + GetEditLine+#13#10#13#10;

  End Else Begin

     // First, write the AutoStart.
     AutoStart := GetWord(@FileHeader[$0E]);
     If AutoStart < $8000 Then
        FileBody := 'Auto '+IntToStr(AutoStart)+#13#10;

  End;

  // Now grab the VARS

  AVars[1] := '';
  AVars[2] := '';
  AVars[3] := '';
  AVars[4] := '';
  AVars[5] := '';
  AVars[6] := '';

  VarAddress := GetWord(@Memory[VARS]);
  Done := Memory[GetWord(@Memory[VARS])] = $80;
  While Not Done Do Begin
     If Memory[VarAddress] = 128 Then Begin
        Done := True;
     End Else Begin
        TempStr := 'Var ';
        Case (Memory[VarAddress] and 224) of
           64:
              Begin
                 // String
                 TempStr := TempStr + Chr((Memory[VarAddress] and 31)+96) + '$: Str = "';
                 TempWord := GetWord(@Memory[VarAddress+1]);
                 If VarAddress + TempWord < GetWord(@Memory[E_LINE]) Then Begin
                    TempStr := TempStr + InsertEscapes(GetMemoryString(VarAddress+3, TempWord, Memory))+'"';
                    Inc(VarAddress, TempWord+3);
                    AVars[5] := AVars[5] + TempStr + #13#10;
                 End Else
                    Done := True;
              End;
           96:
              Begin
                 // Simple Numeric
                 TempStr := TempStr + Chr((Memory[VarAddress] and 31)+96) + ': Num = ';
                 TempByte := Memory[VarAddress+1];
                 TempDWord := GetDWord(@Memory[VarAddress+2]);
                 TempValue := Byte5ToFloat(TempByte, TempDWord);
                 TempStr := TempStr + FloatToStrEx(TempValue);
                 Inc(VarAddress, 6);
                 AVars[1] := AVars[1] + TempStr + #13#10;
              End;
           160:
              Begin
                 // Complex Numeric
                 TempStr := TempStr + Chr((Memory[VarAddress] and 31)+96);
                 Inc(VarAddress);
                 While Memory[VarAddress] and 128 = 0 Do Begin
                    TempStr := TempStr + LowerCase(Chr(Memory[VarAddress]));
                    Inc(VarAddress);
                 End;
                 TempStr := TempStr + LowerCase(Chr(Memory[VarAddress] and 127));
                 TempByte := Memory[VarAddress+1];
                 TempDWord := GetDWord(@Memory[VarAddress+2]);
                 TempValue := Byte5ToFloat(TempByte, TempDWord);
                 TempStr := TempStr + ': Num = '+FloatToStrEx(TempValue);
                 Inc(VarAddress, 6);
                 AVars[2] := AVars[2] + TempStr + #13#10;
              End;
           128:
              Begin
                 // Numeric Array
                 TempStr := TempStr + Chr((Memory[VarAddress] and 31)+96) + ': NumArray(';
                 TempByte := Memory[VarAddress+3];
                 SaveDWord := 1;
                 Inc(VarAddress, 4);
                 While TempByte > 0 Do Begin
                    TempWord := GetWord(@Memory[VarAddress]);
                    If TempByte > 1 Then
                       TempStr := TempStr + IntToStr(TempWord) + ', '
                    Else
                       TempStr := TempStr + IntToStr(TempWord) + ') = ';
                    SaveDWord := SaveDWord * TempWord;
                    Dec(TempByte);
                    Inc(VarAddress, 2);
                 End;
                 While SaveDWord > 0 Do Begin
                    TempByte := Memory[VarAddress];
                    TempDWord := GetDWord(@Memory[VarAddress+1]);
                    TempValue := Byte5ToFloat(TempByte, TempDWord);
                    TempStr := TempStr + FloatToStrEx(TempValue);
                    If SaveDWord > 1 Then TempStr := TempStr + ', ';
                    Inc(VarAddress, 5);
                    Dec(SaveDWord);
                 End;
                 AVars[3] := AVars[3] + TempStr + #13#10;
              End;
           192:
              Begin
                 // String Array
                 TempStr := TempStr + Chr((Memory[VarAddress] and 31)+96) + '$: StrArray(';
                 TempByte := Memory[VarAddress+3];
                 SaveDWord := 1;
                 Inc(VarAddress, 4);
                 While TempByte > 0 Do Begin
                    TempWord := GetWord(@Memory[VarAddress]);
                    SaveDWord := SaveDWord * TempWord;
                    If TempByte > 1 Then
                       TempStr := TempStr + IntToStr(TempWord) + ', '
                    Else
                       TempStr := TempStr + IntToStr(TempWord) + ') = "';
                    Dec(TempByte);
                    Inc(VarAddress, 2);
                    If VarAddress >= GetWord(@Memory[E_LINE]) Then Begin
                       Done := True;
                       AVars[6] := '';
                    End;
                 End;
                 While SaveDWord > 0 Do Begin
                    TempStr := TempStr + Chr(Memory[VarAddress]);
                    If SaveDWord = 1 Then TempStr := TempStr + '"';
                    Inc(VarAddress);
                    Dec(SaveDWord);
                 End;
                 TempStr := InsertEscapes(TempStr);
                 AVars[6] := AVars[6] + TempStr + #13#10;
              End;
           224:
              Begin
                 // FOR Variable
                 TempStr := TempStr + Chr((Memory[VarAddress] and 31)+96) + ': NumFOR = ';
                 TempStr := TempStr + FloatToStrEx(Byte5ToFloat(Memory[VarAddress+1], GetDWord(@Memory[VarAddress+2])))+', ';
                 Inc(VarAddress, 6);
                 TempStr := TempStr + FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])))+', ';
                 Inc(VarAddress, 5);
                 TempStr := TempStr + FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])))+', ';
                 Inc(VarAddress, 5);
                 TempStr := TempStr + IntToStr(GetWord(@Memory[VarAddress]))+', ';
                 TempStr := TempStr + IntToStr(Memory[VarAddress+2]);
                 Inc(VarAddress, 3);
                 AVars[4] := AVars[4] + TempStr + #13#10;
              End;
        Else
           Done := True;
        End;
     End;
  End;

  // Now Insert the variables into the .bas

  VarString := '';

  For TempByte := 1 To 6 Do Begin
     VarString := VarString + AVars[TempByte];
  End;

  If VarString <> '' Then Begin
     FileBody := FileBody + #13#10 + '# Run-time Variables'+#13#10#13#10;
     FileBody := FileBody + VarString + #13#10 + '# End Run-time Variables'+#13#10#13#10;
  End;

  // And finally, start grabbing the BASIC area as text.

  If CurAddress < GetWord(@Memory[VARS]) Then Repeat

     // Line Numbers are stored big-endian
     TempWord := GetWord(@Memory[CurAddress]);
     TempStr := IntToStr((TempWord Shr 8) + ((TempWord and 255) Shl 8));
     While Length(TempStr) < 4 Do TempStr := ' '+TempStr;
     FileBody := FileBody + TempStr +' ';

     // Now Address the Length
     Inc(CurAddress, 2);
     TempWord := GetWord(@Memory[CurAddress]) -1; // ignore terminal #13
     Inc(CurAddress, 2);
     TempStr := '';

     Idx := CurAddress;
     TempStr := '';
     While (TempWord > 0) and (Idx < GetWord(@Memory[VARS])) Do Begin
        TempStr := TempStr + Chr(Memory[Idx]);
        Inc(Idx);
        Dec(TempWord);
     End;
     TempWord := (Idx - CurAddress) +1;

     // and now address the next line for later.
     Inc(CurAddress, TempWord);

     // Now Detokenise the line we just got.
     BASICStr := '';
     BASICStr := DetokeniseLine(TempStr, False);

     // Process for #14 5 byte number codes, and then Escape characters.
     BASICStr := InsertEscapes(BASICStr);

     // Now we have a line, let's prettify it a bit.
     // By breaking up multistatement lines.

     If opt_SavePretty Then Begin

        FirstLine := True;
        InString := False;
        LinePos := 1;
        While LinePos < Length(BASICStr)+1 Do Begin
           If BASICStr[LinePos] = '"' Then InString := Not InString;
           If Not InString and (BASICStr[LinePos] = ':') Then Begin
              // Not a string, and a colon found. Split.
              // If it's the first line, it doesn't need aligning past
              // the line number.
              If Not Firstline Then Begin
                 FileBody := FileBody + '    ' + Copy(BASICStr, 1, LinePos) + '\' + #13#10;
                 BASICStr := Copy(BASICStr, LinePos+1, 999999);
                 LinePos := 1;
              End Else Begin
                 FileBody := FileBody + Copy(BASICStr, 1, LinePos) + '\' + #13#10;
                 BASICStr := Copy(BASICStr, LinePos+1, 999999);
                 FirstLine := False;
                 LinePos := 1;
              End;
           End;
           Inc(LinePos);
        End;
        If Not Firstline Then
           FileBody := FileBody + '    ' + Copy(BASICStr, 1, LinePos) + #13#10
        Else
           FileBody := FileBody + Copy(BASICStr, 1, LinePos) + #13#10;

  End Else Begin

     FileBody := FileBody + BASICStr + #13#10;

  End;

  Until CurAddress >= GetWord(@Memory[VARS]);

  If DoSave Then Begin

     // When saving, add the CRC32 checksum to the end of the file.

     SetLength(FileArray, Length(FileBody));
     CopyMemory(@FileArray[0], @FileBody[1], Length(FileBody));
     TempStr := CheckCRC;
     FileBody := 'Check '+TempStr+#13#10+FileBody;

     // Call SaveFile with the real filename. This will save FileBody.
     If Lowercase(ExtractFileExt(Filename)) <> '.bas' Then Filename := Filename + '.bas';
     If SaveFile then
        if (trim(Copy(ExtractFilename(Filename),1,8))<>'autoback') Then
            SetProjectName(Filename);
        //SetProjectName(ExtractFilename(Filename));

  End;

End;

Function GetToken(Keyword: String): Byte;
Begin
  Result := 0;
  While (Result < 90) and (AsciiKeywords[Result] <> Keyword) Do Inc(Result);
End;

Function FormatEscapes(CurLine: String): String;
Var
  F: Integer;
Begin
  Result := '';
  F := 1;
  While F < Length(CurLine)+1 Do Begin
     If CurLine[F] = '\' Then Begin
        ProcessEscapeChars(CurLine, F, Result)
     End Else
        If (CurLine[F] > Chr(31)) or (CurLine[F] = #13) Then
           Result := Result + CurLine[F];
     Inc(F);
  End;
End;

Function Insert5Bytes(Text: String): String;
Var
  LinePos, LineLen: Integer;
  REMCommand, InString: Boolean;
  CurChar: Char;
  TempValue: Extended;
  TempStr: String;
Begin
  LinePos := 1;
  LineLen := Length(Text)+1;
  REMCommand := False;
  InString := False;
  Result := '';
  Text := Text + #$FF;
  While LinePos < LineLen Do Begin
     If Text[LinePos] in [#65..#90, #97..#122] Then
        While Text[LinePos] in [#32, #36, #48..#57, #65..#90, #97..#122] Do Begin     //let a91=100 ardafix! typo 48..56 :)
           Result := Result + Text[LinePos];
           Inc(LinePos);
           If LinePos >= LineLen Then
              Exit;
        End;
     CurChar := Text[LinePos];
     If Not (Instring or REMCommand) and (CurChar in ['0'..'9', '.']) then Begin
        GetSign(Text, LinePos, Result);
        TempValue := GetNumber(Text, LinePos, Result, False);
        TempStr := FloatTo5Byte(TempValue);
        Result := Result + #$E + TempStr;
        Dec(LinePos);
     End Else If CurChar = #$E Then Begin
        Result := Result + Copy(Text, LinePos, 6);
        Inc(LinePos, 5);
     End Else If CurChar = '"' Then Begin
        InString := Not InString;
        Result := Result + '"';
     End Else If CurChar = Chr(196) Then Begin
        Result := Result + CurChar;
        Inc(LinePos);
        While (Text[LinePos] = ' ') and (LinePos < LineLen) Do Begin
           Result := Result + Text[LinePos];
           Inc(LinePos);
        End;
        TempValue := GetNumber(Text, LinePos, Result, True);
        If not instring then
           Result := Result + #$E + FloatTo5Byte(TempValue);
        Dec(LinePos);
     End Else Begin
        If CurChar = #234 Then REMCommand := True;
        If (CurChar <> ' ') or InString or REMCommand Then
           Result := Result + CurChar;
     End;
     Inc(LinePos);
  End;

End;

Function Strip5Bytes(Text: String): String;
Var
  InString: Boolean;
  LineLen, LinePos: Word;
  CurChar: Char;
Begin
  Result := '';
  InString := False;
  LineLen := Length(Text);
  LinePos := 1;
  While LinePos <= LineLen Do Begin
     CurChar := Text[LinePos];
     If CurChar = '"' Then InString := Not InString;
     If (CurChar = #14) and not InString Then Inc(LinePos, 6);
     If LinePos <= LineLen Then Result := Result + Text[LinePos];
     Inc(LinePos);
  End;
End;

Function StripWhiteSpace(Text: String): String;
Var
  Idx: Integer;
  InString: Boolean;
Begin
  Idx := 1;
  Result := '';
  InString := False;
  While Idx < Length(Text) Do Begin
     If Text[Idx] = '"' Then
        InString := Not InString;
     If Ord(Text[Idx]) > 32 Then
        Result := Result + Text[Idx]
     Else
        If InString Then
           Result := Result + Text[Idx];
     Inc(Idx);
  End;
End;

Function GetDelimitedString(Text: String; Var Index: Integer; Delimiter: Char): String;
Begin
  Result := '';
  While Index <= Length(Text) Do Begin
     If Text[Index] <> Delimiter Then Begin
        Result := Result + Text[Index];
        Inc(Index);
     End Else
        Exit;
  End;
End;

end.
