unit Profiling;

interface

Uses

  Classes, SysUtils, Windows;

Var

  ProfileArray: Array[0..2550000] of Int64;
  ProfilingEnabled: Boolean;
  ProfileBASICList: TStringlist;
  ProfilingPoint: Integer;
  ProfileTs: Int64;
  ProfileInitDone: Boolean;

  Procedure NewProfile;
  Procedure UpdateProfileTs(TStates: DWord);
  Procedure AddProfileEntry;
  Procedure StopProfile;

implementation

  Uses FastCore, ROMUtils, BASinMain, BASSupport;

  Procedure NewProfile;
  Var
     Str, BASIC: String;
     REMCommand, InString: Boolean;
     Idx, Idx2, LineNum, StatementNum, LastIDX: Integer;
  Begin

     ProfileTs := 0;

     ProfileBASICList.Clear;
     BASIC := '0 ' + InsertEscapes(DetokeniseLine(GetEditLine, False)) + #13 + BASinOutput.BASICMem;

     Idx := 1;
     LastIdx := 1;
     InString := False;
     REMCommand := False;

     LineNum := 0;
     StatementNum := 0;

     If BASIC <> '' Then Repeat

        If BASIC[Idx] = '"' Then
           InString := Not InString;

        If Not InString Then
           If BASIC[Idx] in ['R', 'r'] Then
              If (UpperCase(Copy(BASIC, Idx, 4)) = 'REM ') Then
                 REMCommand := True;

        If ((BASIC[Idx] = ':') and Not InString and Not REMCommand) or
           (BASIC[Idx] = #13) or
           (Not Instring and Not REMCommand and (BASIC[Idx -1] in ['n', 'N']) and (Uppercase(Copy(BASIC, Idx -4, 5)) = 'THEN ')) Then Begin

           If LastIDX <> Idx Then Begin
              Str := Copy(BASIC, LastIdx, Idx - LastIdx);
              Idx2 := 1;
              While Idx2 < Length(Str)+1 Do Begin
                 If Str[Idx2] in [#0..#31] Then Begin
                    Delete(Str, Idx2, 1);
                    Dec(Idx2);
                 End Else
                    If Str[Idx2] in [#128..#143] Then
                       Str[Idx2] := '#'
                    Else
                       If Str[Idx2] in [#144..#162] Then
                          Str[Idx2] := Chr(Ord(Str[Idx2])-79)
                       Else
                          If Str[Idx2] in [#165..#255] Then
                             Str := Copy(Str, 1, Idx2 -1)+ AsciiKeywords[Ord(Str[Idx2])-163] + Copy(Str, Idx2+1, 999999);
                 Inc(Idx2);
              End;

              If Str[1] in ['0'..'9'] Then Begin
                 Idx2 := 1;
                 LineNum := 0;
                 While Str[Idx2] in ['0'..'9'] Do Begin
                    LineNum := (LineNum * 10) + Ord(Str[Idx2])-48;
                    Inc(Idx2);
                 End;
                 StatementNum := 1;
                 Str := Copy(Str, Idx2, 999999);
              End Else
                 Inc(StatementNum);

              While Copy(Str, 1, 1) = ' ' Do Str := Copy(Str, 2, 999999);

              ProfileBASICList.Add(IntToStr(LineNum)+':'+IntToStr(StatementNum)+#255+Str);
              ProfileArray[(LineNum*255) + StatementNum -1] := 0;
              InString := False;
              REMCommand := False;

           End;
           LastIdx := Idx +1;

        End;

        Inc(Idx);

     Until Idx > Length(BASIC);

     ProfilingPoint := -1;
     ProfileInitDone := True;

  End;

  Procedure UpdateProfileTs(TStates: DWord);
  Begin

     Inc(ProfileTs, TStates);

  End;

  Procedure AddProfileEntry;
  Begin

     If Not ProfileInitDone Then Exit;

     Case Registers.PC of

        $1303: // Error stop - Complete the last profile entry and exit
           Begin

              If ProfilingPoint <> -1 Then
                 Inc(ProfileArray[ProfilingPoint], ProfileTs);
                 
              ProfileInitDone := False;

           End;
        $1B56: // Get-Param - all statements *start* here, as this is where the initial token is picked up.
           Begin

              If ProfilingPoint <> -1 Then
                 Inc(ProfileArray[ProfilingPoint], ProfileTs);

              ProfileTs := 0;

              If GetWord(@Memory[PPC]) = 65534 Then
                 ProfilingPoint := Memory[SUBPPC] -1
              Else
                 ProfilingPoint := (GetWord(@Memory[PPC]) *255) + Memory[SUBPPC] -1;

           End;
     End;

  End;

  Procedure StopProfile;
  Begin

     ProfilingEnabled := False;
     ProfileBASICList.Free;

  End;

Initialization

  ProfileBASICList := TStringlist.Create;
  ProfileInitDone := False;

end.

