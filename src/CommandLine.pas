unit CommandLine;

interface

function IsDebugCommandLine: Boolean;
function IsFastCommandLine: Boolean;
procedure StartDebugCommandProcessing;

implementation

uses
  Windows, SysUtils, Classes, Forms, ExtCtrls, BasinMain, ROMUtils, FastCore,
  Compiler, RomPrintOutputUnit, Utility, CPUDisplay, Parser, BASSupport;

type
  TRunForOutputMode = (romTextCapture, romMemoryDump);
  TBasicVariableKind = (bvkNumeric, bvkString);

  TDebugCommandProcessor = class
  private
    FCommands: TStringList;
    FInputFilename: String;
    FOutputFilename: String;
    FParseError: String;
    FCommandIndex: Integer;
    FStartedAt: DWord;
    FRunStartedAt: DWord;
    FRunDuration: DWord;
    FRunFrameTarget: Int64;
    FRunPCTarget: Integer;
    FOriginalPCBreakpoint: Byte;
    FLoadStopSerial: DWord;
    FRunStopSerial: DWord;
    FHasRunSeconds: Boolean;
    FHasRunFrames: Boolean;
    FHasRunPC: Boolean;
    FHasUntilVar: Boolean;
    FUntilVarName: String;
    FUntilVarText: String;
    FUntilVarNumber: Extended;
    FDumpRegisters: Boolean;
    FDumpVarNames: TStringList;
    FRunOutputFilename: String;
    FRunOutputMode: TRunForOutputMode;
    FMemoryStart: Integer;
    FMemoryEnd: Integer;
    FRunStarted: Boolean;
    FRunningForDuration: Boolean;
    FTimer: TTimer;
    procedure Finish(ExitStatus: Integer);
    procedure ParseArguments(DebugIndex: Integer);
    function ParseRunForOptions: Boolean;
    function ParseMemoryAddress(const Text: String; var Address: Integer): Boolean;
    function IsValidVariableName(const Name: String): Boolean;
    function ParseUntilVariable(const Expression: String): Boolean;
    function ParseDumpVariables(const Names: String): Boolean;
    function GetBasicScalar(const Name: String; var Kind: TBasicVariableKind;
      var NumberValue: Extended; var StringValue: String): Boolean;
    function UntilVariableReached: Boolean;
    function ResolveOutputFilename(const DefaultExtension: String): String;
    procedure RunNextCommand;
    procedure RunCheckOnly;
    procedure StartRunFor;
    procedure MarkRunStarted;
    procedure FinishRunFor;
    procedure SaveMemoryDump;
    procedure SaveRegisterDump;
    procedure SaveVariableDump;
    procedure TimerTick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
  end;

var
  DebugCommandLine: Boolean = False;
  DebugProcessor: TDebugCommandProcessor = nil;

function FindDebugSwitch: Integer;
var
  Idx: Integer;
begin
  Result := 0;
  For Idx := 1 To ParamCount Do
     If LowerCase(ParamStr(Idx)) = '--debug' Then Begin
        Result := Idx;
        Exit;
     End;
end;

function FindFastSwitch: Integer;
var
  Idx: Integer;
begin
  Result := 0;
  For Idx := 1 To ParamCount Do
     If LowerCase(ParamStr(Idx)) = '--fast' Then Begin
        Result := Idx;
        Exit;
     End;
end;

function IsDebugCommandLine: Boolean;
begin
  Result := DebugCommandLine;
end;

function IsFastCommandLine: Boolean;
begin
  Result := CommandLineFastMode;
end;

constructor TDebugCommandProcessor.Create;
var
  DebugIndex: Integer;
begin
  inherited Create;
  FCommands := TStringList.Create;
  FDumpVarNames := TStringList.Create;
  FInputFilename := ExpandFileName(ParamStr(1));
  DebugIndex := FindDebugSwitch;
  ParseArguments(DebugIndex);
end;

destructor TDebugCommandProcessor.Destroy;
begin
  FTimer.Free;
  FDumpVarNames.Free;
  FCommands.Free;
  inherited Destroy;
end;

procedure TDebugCommandProcessor.Finish(ExitStatus: Integer);
begin
  If Assigned(FTimer) Then
     FTimer.Enabled := False;
  Flush(Output);
  ExitProcess(ExitStatus);
end;

procedure TDebugCommandProcessor.ParseArguments(DebugIndex: Integer);
var
  Argument: String;
  Idx: Integer;
begin
  Idx := DebugIndex + 1;
  While Idx <= ParamCount Do Begin
     Argument := ParamStr(Idx);

     If LowerCase(Argument) = '--fast' Then Begin
        // Global switch; already applied during unit initialization.
     End Else If (LowerCase(Argument) = '-outputfile') or
        (LowerCase(Argument) = '--outputfile') Then Begin
        If FOutputFilename <> '' Then Begin
           FParseError := 'outputfile specified more than once';
           Exit;
        End;

        Inc(Idx);
        If Idx > ParamCount Then Begin
           FParseError := 'outputfile requires a filename';
           Exit;
        End;

        Argument := ParamStr(Idx);
        If Argument = '' Then Begin
           FParseError := 'outputfile requires a filename';
           Exit;
        End;
        If Argument[1] = '-' Then Begin
           FParseError := 'outputfile requires a filename';
           Exit;
        End;

        FOutputFilename := ExpandFileName(Argument);
     End Else
        FCommands.Add(Argument);

     Inc(Idx);
  End;
end;

function TDebugCommandProcessor.ResolveOutputFilename(
  const DefaultExtension: String): String;
begin
  If FOutputFilename <> '' Then
     Result := FOutputFilename
  Else
     Result := ChangeFileExt(FInputFilename, DefaultExtension);
end;

function TDebugCommandProcessor.ParseMemoryAddress(
  const Text: String; var Address: Integer): Boolean;
var
  ValueText: String;
begin
  ValueText := Trim(Text);
  If LowerCase(Copy(ValueText, 1, 2)) = '0x' Then
     ValueText := '$' + Copy(ValueText, 3, Length(ValueText) - 2);

  Address := StrToIntDef(ValueText, -1);
  Result := (Address >= 0) and (Address <= 65535);
end;

function TDebugCommandProcessor.IsValidVariableName(
  const Name: String): Boolean;
var
  Idx, LastChar: Integer;
begin
  Result := False;
  If Name = '' Then Exit;
  If Not (Name[1] in ['a'..'z', 'A'..'Z']) Then Exit;

  LastChar := Length(Name);
  If Name[LastChar] = '$' Then Begin
     If LastChar <> 2 Then Exit;
     Dec(LastChar);
  End;

  For Idx := 2 To LastChar Do
     If Not (Name[Idx] in ['a'..'z', 'A'..'Z', '0'..'9']) Then
        Exit;
  Result := True;
end;

function TDebugCommandProcessor.ParseUntilVariable(
  const Expression: String): Boolean;
var
  EqualPos: Integer;
  NumberText, EncodedNumber: String;
begin
  Result := False;
  EqualPos := Pos('=', Expression);
  If EqualPos <= 1 Then Begin
     WriteLn('debug: --untilvar requires name=value');
     Exit;
  End;

  FUntilVarName := LowerCase(Trim(Copy(Expression, 1, EqualPos - 1)));
  FUntilVarText := Trim(Copy(Expression, EqualPos + 1,
                             Length(Expression) - EqualPos));
  If Not IsValidVariableName(FUntilVarName) Then Begin
     WriteLn('debug: invalid BASIC variable name: ', FUntilVarName);
     Exit;
  End;
  If FUntilVarText = '' Then Begin
     WriteLn('debug: --untilvar requires a value');
     Exit;
  End;

  If FUntilVarName[Length(FUntilVarName)] = '$' Then Begin
     If (Length(FUntilVarText) >= 2) and
        (FUntilVarText[1] = '"') and
        (FUntilVarText[Length(FUntilVarText)] = '"') Then
        FUntilVarText := Copy(FUntilVarText, 2, Length(FUntilVarText) - 2);
  End Else Begin
     NumberText := StringReplace(FUntilVarText, '.', DecimalSeparator,
                                 [rfReplaceAll]);
     Try
        FUntilVarNumber := StrToFloat(NumberText);
     Except
        WriteLn('debug: --untilvar numeric value is invalid: ', FUntilVarText);
        Exit;
     End;
     EncodedNumber := FloatTo5Byte(FUntilVarNumber);
     FUntilVarNumber := Byte5ToFloat(Byte(EncodedNumber[1]),
                                    GetDWord(@EncodedNumber[2]));
  End;
  Result := True;
end;

function TDebugCommandProcessor.ParseDumpVariables(
  const Names: String): Boolean;
var
  CommaPos, Idx: Integer;
  Name, Remaining: String;
begin
  Result := False;
  Remaining := Names;
  If Trim(Remaining) = '' Then Begin
     WriteLn('debug: --dumpvar requires a comma-separated variable list');
     Exit;
  End;

  While Remaining <> '' Do Begin
     CommaPos := Pos(',', Remaining);
     If CommaPos = 0 Then Begin
        Name := Trim(Remaining);
        Remaining := '';
     End Else Begin
        Name := Trim(Copy(Remaining, 1, CommaPos - 1));
        Delete(Remaining, 1, CommaPos);
     End;
     Name := LowerCase(Name);
     If Not IsValidVariableName(Name) Then Begin
        WriteLn('debug: invalid BASIC variable name in --dumpvar: ', Name);
        Exit;
     End;
     For Idx := 0 To FDumpVarNames.Count - 1 Do
        If SameText(FDumpVarNames[Idx], Name) Then Begin
           WriteLn('debug: duplicate variable in --dumpvar: ', Name);
           Exit;
        End;
     FDumpVarNames.Add(Name);
  End;
  Result := True;
end;

function TDebugCommandProcessor.GetBasicScalar(const Name: String;
  var Kind: TBasicVariableKind; var NumberValue: Extended;
  var StringValue: String): Boolean;
var
  VarAddress, NameAddress, DataLength: Integer;
  VariableType: Byte;
  CurrentName: String;
begin
  Result := False;
  VarAddress := GetWord(@Memory[VARS]);

  While (VarAddress >= 0) and (VarAddress <= 65535) and
        (Memory[VarAddress] <> 128) Do Begin
     VariableType := Memory[VarAddress] and 224;
     Case VariableType of
        64:
           Begin
              CurrentName := AnsiChar((Memory[VarAddress] and 31) + 96) + '$';
              If VarAddress > 65532 Then Exit;
              DataLength := GetWord(@Memory[VarAddress + 1]);
              If SameText(CurrentName, Name) Then Begin
                 If VarAddress + 3 + DataLength > 65536 Then Exit;
                 Kind := bvkString;
                 StringValue := GetMemoryString(VarAddress + 3, DataLength,
                                                Memory);
                 Result := True;
                 Exit;
              End;
              Inc(VarAddress, DataLength + 3);
           End;
        96:
           Begin
              CurrentName := AnsiChar((Memory[VarAddress] and 31) + 96);
              If VarAddress > 65530 Then Exit;
              If SameText(CurrentName, Name) Then Begin
                 Kind := bvkNumeric;
                 NumberValue := Byte5ToFloat(Memory[VarAddress + 1],
                                  GetDWord(@Memory[VarAddress + 2]));
                 Result := True;
                 Exit;
              End;
              Inc(VarAddress, 6);
           End;
        160:
           Begin
              CurrentName := AnsiChar((Memory[VarAddress] and 31) + 96);
              NameAddress := VarAddress + 1;
              While (NameAddress <= 65535) and
                    (Memory[NameAddress] and 128 = 0) Do Begin
                 CurrentName := CurrentName +
                                LowerCase(AnsiChar(Memory[NameAddress]));
                 Inc(NameAddress);
              End;
              If NameAddress > 65530 Then Exit;
              CurrentName := CurrentName +
                             LowerCase(AnsiChar(Memory[NameAddress] and 127));
              If SameText(CurrentName, Name) Then Begin
                 Kind := bvkNumeric;
                 NumberValue := Byte5ToFloat(Memory[NameAddress + 1],
                                  GetDWord(@Memory[NameAddress + 2]));
                 Result := True;
                 Exit;
              End;
              VarAddress := NameAddress + 6;
           End;
        128, 192:
           Begin
              If VarAddress > 65532 Then Exit;
              DataLength := GetWord(@Memory[VarAddress + 1]);
              Inc(VarAddress, DataLength + 3);
           End;
        224:
           Begin
              CurrentName := AnsiChar((Memory[VarAddress] and 31) + 96);
              If VarAddress > 65516 Then Exit;
              If SameText(CurrentName, Name) Then Begin
                 Kind := bvkNumeric;
                 NumberValue := Byte5ToFloat(Memory[VarAddress + 1],
                                  GetDWord(@Memory[VarAddress + 2]));
                 Result := True;
                 Exit;
              End;
              Inc(VarAddress, 19);
           End;
     Else
        Inc(VarAddress);
     End;
  End;
end;

function TDebugCommandProcessor.UntilVariableReached: Boolean;
var
  Kind: TBasicVariableKind;
  NumberValue: Extended;
  StringValue: String;
begin
  Result := False;
  If Not FHasUntilVar Then Exit;
  If Not GetBasicScalar(FUntilVarName, Kind, NumberValue, StringValue) Then
     Exit;

  If FUntilVarName[Length(FUntilVarName)] = '$' Then
     Result := (Kind = bvkString) and (StringValue = FUntilVarText)
  Else
     Result := (Kind = bvkNumeric) and (NumberValue = FUntilVarNumber);
end;

function TDebugCommandProcessor.ParseRunForOptions: Boolean;
var
  Argument: String;
  Frames: Int64;
  Seconds: Integer;
  OutputModeSpecified: Boolean;
begin
  Result := False;
  FHasRunSeconds := False;
  FHasRunFrames := False;
  FHasRunPC := False;
  FHasUntilVar := False;
  FDumpRegisters := False;
  FDumpVarNames.Clear;
  FRunDuration := 0;
  FRunFrameTarget := 0;
  FRunPCTarget := 0;
  FRunOutputMode := romTextCapture;
  OutputModeSpecified := False;

  // Backward compatibility: "runfor 7" means "--seconds 7".
  If FCommandIndex < FCommands.Count Then Begin
     Seconds := StrToIntDef(FCommands[FCommandIndex], 0);
     If Seconds > 0 Then Begin
        FHasRunSeconds := True;
        FRunDuration := DWord(Seconds) * 1000;
        Inc(FCommandIndex);
     End;
  End;

  While FCommandIndex < FCommands.Count Do Begin
     Argument := LowerCase(FCommands[FCommandIndex]);

     If (Argument = '--seconds') or (Argument = 'seconds') Then Begin
        If FHasRunSeconds Then Begin
           WriteLn('debug: --seconds specified more than once');
           Exit;
        End;
        Inc(FCommandIndex);
        If FCommandIndex >= FCommands.Count Then Begin
           WriteLn('debug: --seconds requires a positive integer');
           Exit;
        End;
        Seconds := StrToIntDef(FCommands[FCommandIndex], 0);
        If (Seconds <= 0) or (DWord(Seconds) > High(DWord) div 1000) Then Begin
           WriteLn('debug: --seconds requires a valid positive integer');
           Exit;
        End;
        FHasRunSeconds := True;
        FRunDuration := DWord(Seconds) * 1000;
        Inc(FCommandIndex);
     End Else If (Argument = '--frames') or (Argument = 'frames') Then Begin
        If FHasRunFrames Then Begin
           WriteLn('debug: --frames specified more than once');
           Exit;
        End;
        Inc(FCommandIndex);
        If FCommandIndex >= FCommands.Count Then Begin
           WriteLn('debug: --frames requires a positive integer');
           Exit;
        End;
        Frames := StrToInt64Def(FCommands[FCommandIndex], 0);
        If Frames <= 0 Then Begin
           WriteLn('debug: --frames requires a positive integer');
           Exit;
        End;
         FHasRunFrames := True;
         FRunFrameTarget := Frames;
         Inc(FCommandIndex);
     End Else If (Argument = '--pc') or (Argument = 'pc') Then Begin
        If FHasRunPC Then Begin
           WriteLn('debug: --pc specified more than once');
           Exit;
        End;
        Inc(FCommandIndex);
        If FCommandIndex >= FCommands.Count Then Begin
           WriteLn('debug: --pc requires an address');
           Exit;
        End;
        If Not ParseMemoryAddress(FCommands[FCommandIndex], FRunPCTarget) Then Begin
           WriteLn('debug: --pc address must be between 0 and 65535');
           Exit;
        End;
        FHasRunPC := True;
        Inc(FCommandIndex);
     End Else If (Argument = '--untilvar') or
                 (Argument = 'untilvar') Then Begin
        If FHasUntilVar Then Begin
           WriteLn('debug: --untilvar specified more than once');
           Exit;
        End;
        Inc(FCommandIndex);
        If FCommandIndex >= FCommands.Count Then Begin
           WriteLn('debug: --untilvar requires name=value');
           Exit;
        End;
        If Not ParseUntilVariable(FCommands[FCommandIndex]) Then
           Exit;
        FHasUntilVar := True;
        Inc(FCommandIndex);
     End Else If (Argument = '--dumpregisters') or
                 (Argument = 'dumpregisters') Then Begin
        If FDumpRegisters Then Begin
           WriteLn('debug: --dumpregisters specified more than once');
           Exit;
        End;
        FDumpRegisters := True;
        Inc(FCommandIndex);
     End Else If (Argument = '--dumpvar') or
                 (Argument = 'dumpvar') Then Begin
        If FDumpVarNames.Count > 0 Then Begin
           WriteLn('debug: --dumpvar specified more than once');
           Exit;
        End;
        Inc(FCommandIndex);
        If FCommandIndex >= FCommands.Count Then Begin
           WriteLn('debug: --dumpvar requires a comma-separated variable list');
           Exit;
        End;
        If Not ParseDumpVariables(FCommands[FCommandIndex]) Then
           Exit;
        Inc(FCommandIndex);
     End Else If (Argument = '--textcapture') or
                 (Argument = 'textcapture') Then Begin
        If OutputModeSpecified Then Begin
           WriteLn('debug: runfor output mode specified more than once');
           Exit;
        End;
        OutputModeSpecified := True;
        FRunOutputMode := romTextCapture;
        Inc(FCommandIndex);
     End Else If (Argument = '--dumpmemory') or
                 (Argument = 'dumpmemory') Then Begin
        If OutputModeSpecified Then Begin
           WriteLn('debug: runfor output mode specified more than once');
           Exit;
        End;
        OutputModeSpecified := True;
        FRunOutputMode := romMemoryDump;
        Inc(FCommandIndex);

        If FCommandIndex + 1 >= FCommands.Count Then Begin
           WriteLn('debug: --dumpmemory requires start and end addresses');
           Exit;
        End;
        If Not ParseMemoryAddress(FCommands[FCommandIndex], FMemoryStart) or
           Not ParseMemoryAddress(FCommands[FCommandIndex + 1], FMemoryEnd) Then Begin
           WriteLn('debug: memory addresses must be between 0 and 65535');
           Exit;
        End;
        If FMemoryStart > FMemoryEnd Then Begin
           WriteLn('debug: dumpmemory start address must not exceed end address');
           Exit;
        End;
        Inc(FCommandIndex, 2);
     End Else If (Argument = 'compile') or (Argument = 'runfor') Then Begin
        Break;
     End Else Begin
        WriteLn('debug: unknown runfor option: ', FCommands[FCommandIndex]);
        Exit;
     End;
  End;

  If Not FHasRunSeconds and Not FHasRunFrames and Not FHasRunPC and
     Not FHasUntilVar Then Begin
     WriteLn('debug: runfor requires --seconds, --frames, --pc or --untilvar');
     Exit;
  End;
  If FDumpRegisters and Not FHasRunPC Then Begin
     WriteLn('debug: --dumpregisters requires --pc');
     Exit;
  End;

  Result := True;
end;

procedure TDebugCommandProcessor.RunCheckOnly;
var
  CheckResult: TParseError;
  CurLine: String;
  OutputFilename: String;
  Report: TStringList;
  LineStart, LineEnd: Integer;
  HasSyntaxError: Boolean;
begin
  If LowerCase(ExtractFileExt(FInputFilename)) <> '.bas' Then Begin
     WriteLn('debug: --checkonly requires a .bas input file');
     Finish(3);
     Exit;
  End;

  HasSyntaxError := False;
  LineStart := 1;
  While LineStart <= Length(BASinOutput.BASICMem) Do Begin
     LineEnd := LineStart;
     While (LineEnd <= Length(BASinOutput.BASICMem)) and
           (BASinOutput.BASICMem[LineEnd] <> #13) Do
        Inc(LineEnd);
     CurLine := Copy(BASinOutput.BASICMem, LineStart, LineEnd - LineStart);
     If Trim(CurLine) <> '' Then Begin
        CheckResult := ParseInputLine(CurLine);
        If (CheckResult.ErrorCode < 0) or
           (CheckResult.ErrorCode = 99) Then Begin
           HasSyntaxError := True;
           Break;
        End;
     End;
     LineStart := LineEnd + 1;
  End;

  OutputFilename := ResolveOutputFilename('.log');
  Report := TStringList.Create;
  Try
     If HasSyntaxError Then
        Report.Add('ERROR ' + IntToStr(CheckResult.LineNum) + ':' +
                   IntToStr(CheckResult.Statement))
     Else
        Report.Add('OK');
     Report.SaveToFile(OutputFilename);
  Except
     On E: Exception Do Begin
        Report.Free;
        WriteLn('debug: could not write ', OutputFilename, ': ', E.Message);
        Finish(2);
        Exit;
     End;
  End;
  Report.Free;

  WriteLn('debug: wrote ', OutputFilename);
  If HasSyntaxError Then
     Finish(1)
  Else
     Finish(0);
end;

procedure TDebugCommandProcessor.RunNextCommand;
var
  Command: String;
  OutputFilename: String;
begin
  ControlEmulation(False);

  If FCommandIndex >= FCommands.Count Then Begin
     Finish(0);
     Exit;
  End;

  Command := LowerCase(FCommands[FCommandIndex]);
  Inc(FCommandIndex);

  If (Command = '--checkonly') or (Command = 'checkonly') Then Begin
     If FCommandIndex < FCommands.Count Then Begin
        WriteLn('debug: --checkonly does not accept additional commands');
        Finish(3);
        Exit;
     End;
     RunCheckOnly;
  End Else If Command = 'compile' Then Begin
     OutputFilename := ResolveOutputFilename('.asm');
     WriteLn('debug: compiling ', FInputFilename);
     Try
        If Not CompileToFile(OutputFilename) Then Begin
           WriteLn('debug: compile failed');
           Finish(2);
           Exit;
        End;
     Except
        On E: Exception Do Begin
           WriteLn('debug: compile failed: ', E.Message);
           Finish(2);
           Exit;
        End;
     End;
     WriteLn('debug: wrote ', OutputFilename);
     RunNextCommand;
  End Else If Command = 'runfor' Then Begin
     If Not ParseRunForOptions Then Begin
        Finish(1);
        Exit;
     End;
     StartRunFor;
  End Else If Copy(Command, 1, 2) = '--' Then Begin
     WriteLn('debug: ', Command, ' is not valid outside its command');
     Finish(1);
  End Else Begin
     WriteLn('debug: unknown command: ', FCommands[FCommandIndex - 1]);
     Finish(3);
  End;
end;

procedure TDebugCommandProcessor.StartRunFor;
var
  ProgramAlreadyRunning: Boolean;
begin
  ProgramAlreadyRunning := BASinOutput.Running;
  ControlEmulation(False);

  If FRunOutputMode = romMemoryDump Then
     FRunOutputFilename := ResolveOutputFilename('.bin')
  Else
     FRunOutputFilename := ResolveOutputFilename('.log');

  Try
     If FRunOutputMode = romTextCapture Then Begin
        RomPrintOutputWindow.InitSpectrumScreen;
        RomPrintOutputWindow.SetTrap;
     End;

     FRunStarted := False;
     RunFrameCount := 0;
     If FHasRunFrames Then
        RunFrameLimit := FRunFrameTarget
     Else
        RunFrameLimit := 0;
     If FHasRunPC Then Begin
        FOriginalPCBreakpoint := CPUBreakPoints[FRunPCTarget];
        CPUBreakPoints[FRunPCTarget] := FOriginalPCBreakpoint Or 1;
        SuppressBreakpointWindow := True;
     End;
     StepOperation := False;
     ProgStateFlag := PS_Unknown;
     CommandLineUntilVariableMatched := False;
     CommandLineUntilVariableCheck := nil;
     CommandLineRunStartPending := False;
     CommandLineRunStartedNotify := MarkRunStarted;
     FRunningForDuration := True;
     If FRunOutputMode = romMemoryDump Then
        WriteLn('debug: running ', FInputFilename,
                ', then dumping memory ', FMemoryStart, '..', FMemoryEnd)
     Else
        WriteLn('debug: running ', FInputFilename, ' with text capture');
     If ProgramAlreadyRunning Then Begin
        MarkRunStarted;
        ControlEmulation(True)
     End Else
        BASinOutput.RunOrResume(nil);
  Except
     On E: Exception Do Begin
        FRunningForDuration := False;
        CommandLineRunStartPending := False;
        CommandLineRunStartedNotify := nil;
        CommandLineUntilVariableCheck := nil;
        If FHasRunPC Then Begin
           CPUBreakPoints[FRunPCTarget] := FOriginalPCBreakpoint;
           SuppressBreakpointWindow := False;
        End;
        If FRunOutputMode = romTextCapture Then
           RomPrintOutputWindow.RemoveTrap;
        WriteLn('debug: could not start runfor: ', E.Message);
        Finish(2);
     End;
  End;
end;

procedure TDebugCommandProcessor.MarkRunStarted;
begin
  If FRunStarted Then Exit;
  FRunStarted := True;
  FRunStartedAt := GetTickCount;
  FRunStopSerial := ProgramStopSerial;
  CommandLineRunStartPending := False;
  CommandLineRunStartedNotify := nil;
  If FRunOutputMode = romTextCapture Then
     RomPrintOutputWindow.InitSpectrumScreen;
  If FHasUntilVar Then
     CommandLineUntilVariableCheck := UntilVariableReached;
end;

procedure TDebugCommandProcessor.SaveMemoryDump;
var
  OutputStream: TFileStream;
  ByteCount: Integer;
begin
  OutputStream := TFileStream.Create(FRunOutputFilename, fmCreate);
  Try
     ByteCount := FMemoryEnd - FMemoryStart + 1;
     OutputStream.WriteBuffer(Memory[FMemoryStart], ByteCount);
  Finally
     OutputStream.Free;
  End;
end;

procedure TDebugCommandProcessor.SaveRegisterDump;
var
  RegisterLog: TStringList;
  RegisterFilename: String;
  FlagNames: String;
begin
  RegisterFilename := FRunOutputFilename + '.reglog';
  FlagNames := '';
  If Registers.F and $80 <> 0 Then FlagNames := FlagNames + 'S';
  If Registers.F and $40 <> 0 Then FlagNames := FlagNames + 'Z';
  If Registers.F and $20 <> 0 Then FlagNames := FlagNames + '5';
  If Registers.F and $10 <> 0 Then FlagNames := FlagNames + 'H';
  If Registers.F and $08 <> 0 Then FlagNames := FlagNames + '3';
  If Registers.F and $04 <> 0 Then FlagNames := FlagNames + 'P/V';
  If Registers.F and $02 <> 0 Then FlagNames := FlagNames + 'N';
  If Registers.F and $01 <> 0 Then FlagNames := FlagNames + 'C';
  If FlagNames = '' Then FlagNames := '-';

  RegisterLog := TStringList.Create;
  Try
     RegisterLog.Add('PC=$' + IntToHex(Registers.PC, 4));
     RegisterLog.Add('SP=$' + IntToHex(Registers.SP, 4));
     RegisterLog.Add('IX=$' + IntToHex(Registers.IX, 4));
     RegisterLog.Add('IY=$' + IntToHex(Registers.IY, 4));
     RegisterLog.Add('AF=$' + IntToHex(Registers.A, 2) + IntToHex(Registers.F, 2));
     RegisterLog.Add('BC=$' + IntToHex(Registers.B, 2) + IntToHex(Registers.C, 2));
     RegisterLog.Add('DE=$' + IntToHex(Registers.D, 2) + IntToHex(Registers.E, 2));
     RegisterLog.Add('HL=$' + IntToHex(Registers.H, 2) + IntToHex(Registers.L, 2));
     RegisterLog.Add('AF''=$' + IntToHex(Registers.An, 2) + IntToHex(Registers.Fn, 2));
     RegisterLog.Add('BC''=$' + IntToHex(Registers.Bn, 2) + IntToHex(Registers.Cn, 2));
     RegisterLog.Add('DE''=$' + IntToHex(Registers.Dn, 2) + IntToHex(Registers.En, 2));
     RegisterLog.Add('HL''=$' + IntToHex(Registers.Hn, 2) + IntToHex(Registers.Ln, 2));
     RegisterLog.Add('I=$' + IntToHex(Registers.I, 2));
     RegisterLog.Add('R=$' + IntToHex(Registers.R, 2));
     RegisterLog.Add('IM=' + IntToStr(Registers.IntMode));
     RegisterLog.Add('IFF1=' + BoolToStr(Registers.IntsEnabled, True));
     RegisterLog.Add('FLAGS=' + FlagNames);
     RegisterLog.Add('HALT=' + BoolToStr(Registers.HaltEmu, True));
     RegisterLog.Add('CAN_INTERRUPT=' + BoolToStr(Registers.CanInterrupt, True));
     RegisterLog.Add('EMU_RUNNING=' + BoolToStr(Registers.EmuRunning, True));
     RegisterLog.Add('TOTAL_TS=' + IntToStr(Registers.TotalTS));
     RegisterLog.Add('LAST_TOTAL_TS=' + IntToStr(Registers.LastTotalTS));
     RegisterLog.Add('TIME=' + IntToStr(Registers.Time));
     RegisterLog.Add('RUN_FRAME_COUNT=' + IntToStr(RunFrameCount));
     RegisterLog.Add('SPN=$' + IntToHex(Registers.SPn, 4));
     RegisterLog.Add('TARGET_ADDR=$' + IntToHex(Registers.TargetAddr, 4));
     RegisterLog.Add('WORD1=$' + IntToHex(Registers.Word1, 4));
     RegisterLog.Add('WORD2=$' + IntToHex(Registers.Word2, 4));
     RegisterLog.Add('WORD3=$' + IntToHex(Registers.Word3, 4));
     RegisterLog.Add('LAST_FE=$' + IntToHex(Registers.LastFE, 2));
     RegisterLog.Add('FLASH_STATE=' + IntToStr(Registers.FlashState));
     RegisterLog.SaveToFile(RegisterFilename);
  Finally
     RegisterLog.Free;
  End;
  WriteLn('debug: wrote ', RegisterFilename);
end;

procedure TDebugCommandProcessor.SaveVariableDump;
var
  Idx: Integer;
  Kind: TBasicVariableKind;
  NumberValue: Extended;
  StringValue, VariableFilename, ValueText: String;
  VariableLog: TStringList;
begin
  VariableFilename := FRunOutputFilename + '.varlog';
  VariableLog := TStringList.Create;
  Try
     For Idx := 0 To FDumpVarNames.Count - 1 Do Begin
        If GetBasicScalar(FDumpVarNames[Idx], Kind, NumberValue,
                          StringValue) Then Begin
           If Kind = bvkString Then
              ValueText := '"' + InsertEscapes(StringValue) + '"'
           Else
              ValueText := FloatToStrEx(NumberValue);
        End Else
           ValueText := 'Variable not found';
        VariableLog.Add(FDumpVarNames[Idx] + '=' + ValueText);
     End;
     VariableLog.SaveToFile(VariableFilename);
  Finally
     VariableLog.Free;
  End;
  WriteLn('debug: wrote ', VariableFilename);
end;

procedure TDebugCommandProcessor.FinishRunFor;
var
  StoppedAtPC: Boolean;
begin
  StoppedAtPC := FHasRunPC and StepOperation and
                 (Registers.PC = FRunPCTarget);
  FRunningForDuration := False;
  CommandLineRunStartPending := False;
  CommandLineRunStartedNotify := nil;
  CommandLineUntilVariableCheck := nil;
  RunFrameLimit := 0;
  If FHasRunPC Then Begin
     CPUBreakPoints[FRunPCTarget] := FOriginalPCBreakpoint;
     SuppressBreakpointWindow := False;
  End;
  ControlEmulation(False);
  BASinOutput.Running := False;
  If FRunOutputMode = romTextCapture Then
     RomPrintOutputWindow.RemoveTrap;

  Try
     If FRunOutputMode = romMemoryDump Then
        SaveMemoryDump
     Else
        RomPrintOutputWindow.Memo1.Lines.SaveToFile(FRunOutputFilename);
     If FDumpRegisters and StoppedAtPC Then
        SaveRegisterDump;
     If FDumpVarNames.Count > 0 Then
        SaveVariableDump;
  Except
     On E: Exception Do Begin
        WriteLn('debug: could not write ', FRunOutputFilename, ': ', E.Message);
        Finish(2);
        Exit;
     End;
  End;

  WriteLn('debug: wrote ', FRunOutputFilename);
  RunNextCommand;
end;

procedure TDebugCommandProcessor.TimerTick(Sender: TObject);
begin
  If FRunningForDuration Then Begin
     If FRunStarted and
        ((FHasRunSeconds and
          (GetTickCount - FRunStartedAt >= FRunDuration)) or
         (FHasRunFrames and
          (RunFrameCount >= FRunFrameTarget)) or
         (FHasRunPC and StepOperation and
          (Registers.PC = FRunPCTarget)) or
         CommandLineUntilVariableMatched or
         (ProgramStopSerial <> FRunStopSerial)) Then
        FinishRunFor;
     If Not FRunStarted and (GetTickCount - FStartedAt > 30000) Then Begin
        WriteLn('debug: timed out while starting RUN');
        Finish(4);
     End;
  End Else If SameText(ExpandFileName(CurProjectFilename), FInputFilename) and
             (GetWord(@Memory[PROG]) <> GetWord(@Memory[VARS])) and
             (ProgramStopSerial <> FLoadStopSerial) and
             Not BASinOutput.Running and Not Registers.EmuRunning and
             Editing and Not Trapped Then Begin
     RunNextCommand;
  End Else If GetTickCount - FStartedAt > 30000 Then Begin
     WriteLn('debug: timed out while loading ', FInputFilename);
     Finish(4);
  End;
end;

procedure TDebugCommandProcessor.Start;
begin
  If (ParamCount < 3) or (FindDebugSwitch <= 1) Then Begin
     WriteLn('usage: BasinC.exe <bas file> --debug runfor ' +
             '[--seconds n] [--frames n] [--pc address] [--untilvar name=value] ' +
             '[--dumpregisters] [--dumpvar name[,name...]] ' +
             '[--textcapture | --dumpmemory start end] ' +
             '[--outputfile filename] [--fast]'#13#10 +
             '   or: BasinC.exe <bas file> --debug --checkonly ' +
             '[--outputfile report.log]');
     Finish(3);
     Exit;
  End;

  If Not FileExists(FInputFilename) Then Begin
     WriteLn('debug: input file not found: ', FInputFilename);
     Finish(2);
     Exit;
  End;

  If FParseError <> '' Then Begin
     WriteLn('debug: ', FParseError);
     Finish(3);
     Exit;
  End;

  If FCommands.Count = 0 Then Begin
     WriteLn('debug: no commands specified');
     Finish(3);
     Exit;
  End;

  FStartedAt := GetTickCount;
  FLoadStopSerial := ProgramStopSerial;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerTick;
  FTimer.Enabled := True;
end;

procedure StartDebugCommandProcessing;
begin
  If Not DebugCommandLine Then Exit;
  DebugProcessor := TDebugCommandProcessor.Create;
  DebugProcessor.Start;
end;

initialization
  DebugCommandLine := FindDebugSwitch > 0;
  CommandLineFastMode := FindFastSwitch > 0;
  If CommandLineFastMode Then
     FASTMode := True;

finalization
  DebugProcessor.Free;

end.
