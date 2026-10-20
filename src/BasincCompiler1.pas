unit BasincCompiler1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TMCoderStatus = (msCompileStarted, msPart1Loaded, msPart2Loaded, msCompileFailed, msCompileComplete);

const
  WM_MCODER_STATUS = WM_USER + 101;
  WM_MCODER_CHAR = WM_USER + 102;

type
  TFormCompiler1 = class(TForm)
    PageControl1: TPageControl;
    BottomPanel: TPanel;
    TabSheetSimple: TTabSheet;
    TabSheetAdvanced: TTabSheet;
    GroupCompiler: TGroupBox;
    RadioMCoder3: TRadioButton;
    RadioZXBasicDirect: TRadioButton;
    GroupDefaultType: TGroupBox;
    RadioDefaultAuto: TRadioButton;
    RadioDefaultFloat: TRadioButton;
    GroupVariableTypes: TGroupBox;
    ListVariables: TListView;
    ButtonSelectAll: TButton;
    ComboVariableType: TComboBox;
    ButtonApplyType: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Button5: TButton;
    Button6: TButton;
    GroupBox1: TGroupBox;
    Button7: TButton;
    GroupBox2: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CompilerModeClick(Sender: TObject);
     procedure TableOptionClick(Sender: TObject);
        procedure SelectAllClick(Sender: TObject);
    procedure ApplyTypeClick(Sender: TObject);
  private
    FAllowClose: Boolean;
    FCompiledProgramBody: String;
    FCompiledProgramLen: Word;
    FCompiledDataLen: Word;
    FPreviewTapeIndex: Integer;
    FPreviewTapeBlock: String;
    FCompilerScreenLine: Integer;
    FCompilerCaptureBuffer: String;
    FCompilerStage: Integer;
    FZXCompileThread: TThread;
    FZXCompiledBinary: String;
    FZXCompiledOrigin: Word;
    FZXSourceFilename: String;
    FZXJumpTableRange: String;
    FZXDynamicRestoreRange: String;
    procedure MCoderStatus(var Message: TMessage); message WM_MCODER_STATUS;
    procedure MCoderChar(var Message: TMessage); message WM_MCODER_CHAR;
    procedure AddStatus(const Text: String);
    procedure ResetCompilerScreenRows;
    procedure UpdateButtons;
    function HasBASICProgram: Boolean;
    function HasMCoderTape: Boolean;
    function GetZXBasicCompilerFilename: String;
     function Is128kCompileBlocked: Boolean;
     procedure RefreshZXTableControls;
     
          function ValidateZXTableRange(const SwitchName, RangeText: String; var Normalized: String): Boolean;
     function ValidateZXBasicTableOptions: Boolean;
    procedure CloseFromButton;
    procedure RestoreZXEditorState;
    procedure ShowTapeWindow;
    procedure CaptureCompiledProgram;
    procedure InjectCompiledProgram;
    function BuildCompiledProgramTapeBlock: String;
    procedure AddCompiledProgramToTape;
    procedure ResetCompileState;
    procedure RefreshAdvancedControls;
    procedure RefreshNumericVariables;
    procedure LoadCompilerMetadataTypes(Types: TStringList);
    function BuildCompilerMetadata: String;
    
    procedure CompileZXBasicDirect;
    procedure ZXCompileFinished(const Output: String; ExitCode: DWORD);
    procedure RunZXBasicDirect;
    procedure SendZXBinaryToTape;
    procedure SendZXBinaryToBridge;
    function SaveTemporaryBAS(const TargetFilename: String): Boolean;
    function BuildZXBasicCommandLine(const SourceFilename, OutputFilename: String): String;
    function BuildZXVariableOptions: String;
     function ApplyZXVariableRenames(const Source: String): String;
    function GetZXProjectName: String;
    function GetZXOrigin(var Origin: Word): Boolean;
    { Private declarations }
  public
    CompileStateSaved: Boolean;
    CompileSuccessful: Boolean;
    CompileRunning: Boolean;
    PreviewTapeLoading: Boolean;
    procedure CleanupPreviewTapeBlock;
    { Public declarations }
  end;

  TZXBasicCompileThread = class(TThread)
  private
    FOwner: TFormCompiler1;
    FCommand: String;
    FWorkingDirectory: String;
    FOutput: String;
    FExitCode: DWORD;
    FDisplayedLength: Integer;
    procedure SyncOutput;
    procedure SyncComplete;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFormCompiler1; const Command, WorkingDirectory: String);
    property Output: String read FOutput;
    property ExitCode: DWORD read FExitCode;
  end;

var
  FormCompiler1: TFormCompiler1;

procedure PostMCoderCompilerStatus(Status: TMCoderStatus);
procedure PostMCoderCompilerFailure(ErrorNumber: Byte; Line: Word; Statement: Byte);
procedure PostMCoderCompilerChar(CharCode: Byte; Row, Col: Word);

implementation

uses
  ROMUtils, FastCore, Filing, BasinMain, Utility, Tapes, External, BASSupport,
  Binaries, BinaryForm, Parser;

{$R *.dfm}

constructor TZXBasicCompileThread.Create(AOwner: TFormCompiler1; const Command, WorkingDirectory: String);
begin
   inherited Create(True);
   FreeOnTerminate := True;
   FOwner := AOwner;
   FCommand := Command;
   FWorkingDirectory := WorkingDirectory;
   FOutput := '';
   FExitCode := $FFFFFFFF;
   FDisplayedLength := 0;
end;

procedure TZXBasicCompileThread.SyncOutput;
var
   NewText: String;
begin
   if not Assigned(FOwner) then Exit;
   if FDisplayedLength < Length(FOutput) then begin
      NewText := Copy(FOutput, FDisplayedLength + 1, Length(FOutput) - FDisplayedLength);
      FOwner.Memo1.SelStart := Length(FOwner.Memo1.Text);
      FOwner.Memo1.SelText := NewText;
      FOwner.Memo1.SelStart := Length(FOwner.Memo1.Text);
      FDisplayedLength := Length(FOutput);
   end;
end;

procedure TZXBasicCompileThread.SyncComplete;
begin
   if Assigned(FOwner) then
      FOwner.ZXCompileFinished(FOutput, FExitCode);
end;

procedure TZXBasicCompileThread.Execute;
var
   SecurityAttributes: TSecurityAttributes;
   StartupInfo: TStartupInfo;
   ProcessInfo: TProcessInformation;
   StdOutRead, StdOutWrite: THandle;
   Buffer: Array[0..2047] of Char;
   BytesRead: DWORD;
   Text: String;
   CommandLine: String;
begin
   StdOutRead := 0;
   StdOutWrite := 0;
   FExitCode := $FFFFFFFF;
   FillChar(SecurityAttributes, SizeOf(SecurityAttributes), 0);
   SecurityAttributes.nLength := SizeOf(SecurityAttributes);
   SecurityAttributes.bInheritHandle := True;

   if not CreatePipe(StdOutRead, StdOutWrite, @SecurityAttributes, 0) then begin
      FOutput := 'Could not create compiler output pipe: ' + SysErrorMessage(GetLastError) + #13#10;
      Synchronize(SyncOutput);
      Synchronize(SyncComplete);
      Exit;
   end;

   try
      SetHandleInformation(StdOutRead, HANDLE_FLAG_INHERIT, 0);
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      StartupInfo.cb := SizeOf(StartupInfo);
      StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := SW_HIDE;
      StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      StartupInfo.hStdOutput := StdOutWrite;
      StartupInfo.hStdError := StdOutWrite;
      FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);
      CommandLine := FCommand;

      if not CreateProcess(nil, PChar(CommandLine), nil, nil, True, 0, nil,
                           PChar(FWorkingDirectory), StartupInfo, ProcessInfo) then begin
         FOutput := 'Could not start zxbc-direct.exe: ' + SysErrorMessage(GetLastError) + #13#10;
         Synchronize(SyncOutput);
         Synchronize(SyncComplete);
         Exit;
      end;

      CloseHandle(StdOutWrite);
      StdOutWrite := 0;
      while ReadFile(StdOutRead, Buffer, SizeOf(Buffer), BytesRead, nil) do begin
         if BytesRead = 0 then Break;
         SetString(Text, PChar(@Buffer[0]), BytesRead);
         FOutput := FOutput + Text;
         Synchronize(SyncOutput);
      end;

      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, FExitCode);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      Synchronize(SyncOutput);
      Synchronize(SyncComplete);
   finally
      if StdOutRead <> 0 then CloseHandle(StdOutRead);
      if StdOutWrite <> 0 then CloseHandle(StdOutWrite);
   end;
end;

procedure PostMCoderCompilerStatus(Status: TMCoderStatus);
begin
   If Assigned(FormCompiler1) and FormCompiler1.HandleAllocated Then
      PostMessage(FormCompiler1.Handle, WM_MCODER_STATUS, Ord(Status), 0);
end;

procedure PostMCoderCompilerFailure(ErrorNumber: Byte; Line: Word; Statement: Byte);
Var
   ErrorData: Integer;
begin
   ErrorData := Integer(ErrorNumber) or (Integer(Statement) Shl 8) or
                (Integer(Line) Shl 16);
   If Assigned(FormCompiler1) and FormCompiler1.HandleAllocated Then
      PostMessage(FormCompiler1.Handle, WM_MCODER_STATUS,
                  Ord(msCompileFailed), ErrorData);
end;

procedure PostMCoderCompilerChar(CharCode: Byte; Row, Col: Word);
begin
   If Assigned(FormCompiler1) and FormCompiler1.HandleAllocated Then
      SendMessage(FormCompiler1.Handle, WM_MCODER_CHAR, CharCode, (Row Shl 16) or Col);
end;


procedure TFormCompiler1.LoadCompilerMetadataTypes(Types: TStringList);
var
   Lines: TStringList;
   Line, Payload, TypeName, VarNames, VarName: String;
   LineIndex, SpacePos, CommaPos: Integer;
begin
   Types.Clear;
   Types.CaseSensitive := False;
   Lines := TStringList.Create;
   try
      Lines.Text := GetCompilerMetadata;
      for LineIndex := 0 to Lines.Count - 1 do begin
         Line := Trim(Lines[LineIndex]);
         if LowerCase(Copy(Line, 1, 9)) = '#compiler' then begin
            Payload := Trim(Copy(Line, 10, Length(Line)));
            SpacePos := Pos(' ', Payload);
            if SpacePos > 0 then begin
               TypeName := LowerCase(Trim(Copy(Payload, 1, SpacePos - 1)));
               VarNames := Trim(Copy(Payload, SpacePos + 1, Length(Payload)));
               while VarNames <> '' do begin
                  CommaPos := Pos(',', VarNames);
                  if CommaPos > 0 then begin
                     VarName := Trim(Copy(VarNames, 1, CommaPos - 1));
                     VarNames := Trim(Copy(VarNames, CommaPos + 1, Length(VarNames)));
                  end else begin
                     VarName := Trim(VarNames);
                     VarNames := '';
                  end;
                  if VarName <> '' then
                     Types.Values[LowerCase(VarName)] := TypeName;
               end;
            end;
         end;
      end;
   finally
      Lines.Free;
   end;
end;

procedure TFormCompiler1.RefreshNumericVariables;
var
   Names, Types, Lines, Kinds, Rows, RenameMap, CollisionKinds: TStringList;
   Stack: TParseStack;
   Source, TypeName, VariableKind, LineText, VarName, RowName, RowKind, RowKey: String;
   DisplayKind: String;
   I, RuntimeIndex, Position, CollisionCount, SeparatorPos: Integer;
   Item: TListItem;
   Key, BaseName, KindList, CollisionKind, RenameName, Candidate, Prefix, Suffix: String;
   procedure RegisterVariable(const VariableName, KindName: String);
   var
      ExistingKinds, RowIdentifier, CanonicalName: String;
   begin
      if VariableName = '' then Exit;
      RowIdentifier := LowerCase(VariableName) + #1 + LowerCase(KindName);
      if Rows.IndexOf(RowIdentifier) < 0 then
         Rows.Add(VariableName + #1 + LowerCase(KindName));
      if Names.IndexOf(VariableName) < 0 then
         Names.Add(VariableName);
      CanonicalName := LowerCase(VariableName);
       if (CanonicalName <> '') and (CanonicalName[Length(CanonicalName)] = '$') then
          Delete(CanonicalName, Length(CanonicalName), 1);
       ExistingKinds := Kinds.Values[CanonicalName];
      if Pos(',' + LowerCase(KindName) + ',', ',' + LowerCase(ExistingKinds) + ',') = 0 then begin
         if ExistingKinds <> '' then
            ExistingKinds := ExistingKinds + ',';
         Kinds.Values[CanonicalName] := ExistingKinds + LowerCase(KindName);
      end;
   end;
function HasStringArrayRow(const AName: String): Boolean;
   var
      J, Sep: Integer;
   begin
      Result := False;
      for J := 0 to Rows.Count - 1 do begin
         Sep := Pos(#1, Rows[J]);
         if (Sep > 0) and
            SameText(Copy(Rows[J], 1, Sep - 1), AName) and
            SameText(Copy(Rows[J], Sep + 1, Length(Rows[J])), 'strarray') then begin
            Result := True;
            Exit;
         end;
      end;
   end;
   function ExistingStringRename(const AName: String): String;
   var
      J, Sep: Integer;
      OtherName, OtherKind: String;
   begin
      Result := '';
      for J := 0 to Rows.Count - 1 do begin
         Sep := Pos(#1, Rows[J]);
         if Sep <= 0 then
            Continue;
         OtherName := Copy(Rows[J], 1, Sep - 1);
         OtherKind := Copy(Rows[J], Sep + 1, Length(Rows[J]));
         if SameText(OtherName, AName) and
            ((OtherKind = 'string') or (OtherKind = 'strarray')) then begin
            Result := RenameMap.Values[Rows[J]];
            if Result <> '' then
               Exit;
         end;
      end;
   end;
   function CanonicalRoot(const AName: String): String;
   begin
      Result := LowerCase(AName);
      if (Result <> '') and (Result[Length(Result)] = '$') then
         Delete(Result, Length(Result), 1);
   end;
   function AliasAlreadyUsed(const AName: String): Boolean;
   var
      J: Integer;
      AliasRoot: String;
   begin
      AliasRoot := CanonicalRoot(AName);
      Result := False;
      for J := 0 to Names.Count - 1 do
         if CanonicalRoot(Names[J]) = AliasRoot then begin
            Result := True;
            Exit;
         end;
      for J := 0 to RenameMap.Count - 1 do
         if CanonicalRoot(Copy(RenameMap[J], Pos('=', RenameMap[J]) + 1, Length(RenameMap[J]))) = AliasRoot then begin
            Result := True;
            Exit;
         end;
   end;
    begin
    Names := TStringList.Create;
    Kinds := TStringList.Create;
    Rows := TStringList.Create;
    RenameMap := TStringList.Create;
    CollisionKinds := TStringList.Create;
   Types := TStringList.Create;
   try
      Names.CaseSensitive := False;
       Kinds.CaseSensitive := False;
       Rows.CaseSensitive := False;
       RenameMap.CaseSensitive := False;
       CollisionKinds.CaseSensitive := False;
      Types.CaseSensitive := False;
      LoadCompilerMetadataTypes(Types);
      Source := '';
      if Assigned(BASinOutput) then
         Source := BASinOutput.BASICMem;

             
        begin
            Lines := TStringList.Create;
           try
              Lines.Text := Source;
              InitParser;
              for I := 0 to Lines.Count - 1 do begin
                 LineText := BeautifyBASIC(Lines[I]);
                 Stack := BuildParseStack(LineText);
                 try
                    for RuntimeIndex := 0 to Stack.NumItems - 1 do
                       if Stack.GetItemType(RuntimeIndex) in [SINumVar, SINumVarSL, SIStrVar, SIStrVarSL] then begin
                          Position := Stack.GetItem(RuntimeIndex).Position;
                          while (Position <= Length(LineText)) and
                                (LineText[Position] in ['A'..'Z', 'a'..'z', '0'..'9', '$']) do
                             Inc(Position);
                          VarName := Copy(LineText, Stack.GetItem(RuntimeIndex).Position,
                                         Position - Stack.GetItem(RuntimeIndex).Position);
                          if VarName <> '' then begin

                              VariableKind := 'numeric';
                              if Stack.GetItemType(RuntimeIndex) in [SIStrVar, SIStrVarSL] then
                                 VariableKind := 'string'
                              else begin
                                 while (Position <= Length(LineText)) and (LineText[Position] = ' ') do
                                    Inc(Position);
                                 if (Position <= Length(LineText)) and (LineText[Position] = '(') and
                                    not ((RuntimeIndex > 0) and
                                         (Stack.GetItemType(RuntimeIndex - 1) = 1070)) then
                                    VariableKind := 'numarray';
                              end;
                              if VariableKind = 'string' then begin
                               while (Position <= Length(LineText)) and (LineText[Position] = ' ') do
                                  Inc(Position);
                               if (Position <= Length(LineText)) and (LineText[Position] = '(') and
                                  (RuntimeIndex > 0) and
                                  (Stack.GetItemType(RuntimeIndex - 1) = 1012) then
                                  VariableKind := 'strarray';
                            end;
                               RegisterVariable(VarName, VariableKind);
                           end;
                       end;
                 finally
                    Stack.Free;
                 end;
              end;
           finally
              Lines.Free;
           end;
        end;

         for I := 0 to Names.Count - 1 do begin
          Key := LowerCase(Names[I]);
          if (Key <> '') and (Key[Length(Key)] = '$') then
             Delete(Key, Length(Key), 1);
          KindList := Kinds.Values[Key];
          if Pos('numeric', KindList) > 0 then begin
             KindList := Kinds.Values[Key];
           CollisionKind := CollisionKinds.Values[Key];
             if Pos(',numeric,', ',' + CollisionKind + ',') = 0 then
                CollisionKinds.Values[Key] := CollisionKind + 'numeric,';
          end;
          if Pos('string', KindList) > 0 then begin
             KindList := Kinds.Values[Key];
           CollisionKind := CollisionKinds.Values[Key];
             if Pos(',string,', ',' + CollisionKind + ',') = 0 then
                CollisionKinds.Values[Key] := CollisionKind + 'string,';
          end;
          if Pos('numarray', KindList) > 0 then begin
             KindList := Kinds.Values[Key];
           CollisionKind := CollisionKinds.Values[Key];
             if Pos(',numarray,', ',' + CollisionKind + ',') = 0 then
                CollisionKinds.Values[Key] := CollisionKind + 'numarray,';
          end;
          if Pos('strarray', KindList) > 0 then begin
             KindList := Kinds.Values[Key];
           CollisionKind := CollisionKinds.Values[Key];
             if Pos(',strarray,', ',' + CollisionKind + ',') = 0 then
                CollisionKinds.Values[Key] := CollisionKind + 'strarray,';
          end;
       end;

       for I := 0 to Rows.Count - 1 do begin
          SeparatorPos := Pos(#1, Rows[I]);
          RowName := Copy(Rows[I], 1, SeparatorPos - 1);
          RowKind := Copy(Rows[I], SeparatorPos + 1, Length(Rows[I]));
          Key := LowerCase(RowName);
          if (Key <> '') and (Key[Length(Key)] = '$') then
             Delete(Key, Length(Key), 1);
          KindList := Kinds.Values[Key];
           CollisionKind := CollisionKinds.Values[Key];
          CollisionCount := 0;
          if Pos('numeric', CollisionKind) > 0 then Inc(CollisionCount);
          { String and strarray are the same logical kind: DIM a$(...) is
             a fixed-length string, not a second variable alongside a$. }
           if (Pos('string', CollisionKind) > 0) or
              (Pos('strarray', CollisionKind) > 0) then
              Inc(CollisionCount);
          if Pos('numarray', CollisionKind) > 0 then Inc(CollisionCount);
          
          RenameName := '';
          if CollisionCount > 1 then begin
              if (RowKind = 'string') or (RowKind = 'strarray') then
                 RenameName := ExistingStringRename(RowName);
             if RowKind = 'string' then begin
                 RenameName := ExistingStringRename(RowName);
                 if RenameName = '' then begin
                { If the same root also has DIM a$(...), every string
                   reference must use the string-array alias. }
                 if Pos('strarray', KindList) > 0 then
                    Prefix := 'sary_'
                 else
                    Prefix := 'str_';
                BaseName := LowerCase(RowName);
                Suffix := '$';
                if (BaseName <> '') and (BaseName[Length(BaseName)] = '$') then
                   Delete(BaseName, Length(BaseName), 1);
                Candidate := Prefix + BaseName + Suffix;
                while AliasAlreadyUsed(Candidate) do
                   Candidate := Prefix + Candidate;
                RenameName := Candidate;
             end;
              end else if (RowKind = 'strarray') and (RenameName = '') then begin
                Prefix := 'sary_';
                BaseName := LowerCase(RowName);
                Suffix := '$';
                if (BaseName <> '') and (BaseName[Length(BaseName)] = '$') then
                   Delete(BaseName, Length(BaseName), 1);
                Candidate := Prefix + BaseName + Suffix;
                while AliasAlreadyUsed(Candidate) do
                   Candidate := Prefix + Candidate;
                RenameName := Candidate;
             end else if RowKind = 'numarray' then begin
                Prefix := 'nary_';
                BaseName := LowerCase(RowName);
                Candidate := Prefix + BaseName;
                while AliasAlreadyUsed(Candidate) do
                   Candidate := Prefix + Candidate;
                RenameName := Candidate;
             end;
          end;
          RenameMap.Values[Rows[I]] := RenameName;
       end;

       ListVariables.Items.BeginUpdate;
      try
         ListVariables.Items.Clear; { rows follow }
         for I := 0 to Rows.Count - 1 do begin
            
            SeparatorPos := Pos(#1, Rows[I]);
             RowName := Copy(Rows[I], 1, SeparatorPos - 1);
             RowKind := Copy(Rows[I], SeparatorPos + 1, Length(Rows[I]));
             RowKey := Rows[I];
             { ListVariables is populated below, after applying the visibility rules. }
             
             RenameName := RenameMap.Values[RowKey];

             { String variables are only useful in this list when a rename
               is required.  DIM a$(...) and a$ are one logical string, so
               prefer the single strarray row when both were discovered. }
             if (RowKind = 'string') and HasStringArrayRow(RowName) then
                Continue;
             if (RowKind = 'string') or (RowKind = 'strarray') then begin
                if RenameName = '' then
                   Continue;
                DisplayKind := 'string';
                TypeName := 'string';
             end else begin
                if (RowKind <> 'numeric') and (RowKind <> 'numarray') then
                   Continue;
                DisplayKind := RowKind;
                TypeName := LowerCase(Types.Values[LowerCase(RowName)]);
                if (TypeName = '') and (RenameName <> '') then
                   TypeName := LowerCase(Types.Values[LowerCase(RenameName)]);
                
                   
                if TypeName = '' then
                   TypeName := 'default';
             end;

             Item := ListVariables.Items.Add;
             Item.Caption := RowName;
             
                
            
            
            Item.SubItems.Add(TypeName);
             Item.SubItems.Add(RenameName);
             Item.SubItems.Add(DisplayKind);
         end;
      finally
         ListVariables.Items.EndUpdate;
      end;
   finally
      Types.Free;
      CollisionKinds.Free;
       RenameMap.Free;
       Rows.Free;
       Kinds.Free;
       Names.Free;
   end;
end;

function TFormCompiler1.BuildCompilerMetadata: String;
var
   TypeIndex, ItemIndex: Integer;
   TypeName, VariableNames: String;
begin
   Result := '';
   for TypeIndex := 0 to ComboVariableType.Items.Count - 1 do begin
      TypeName := LowerCase(Trim(ComboVariableType.Items[TypeIndex]));
      if TypeName = 'default' then
         Continue;
      VariableNames := '';
      for ItemIndex := 0 to ListVariables.Items.Count - 1 do
         if (ListVariables.Items[ItemIndex].SubItems.Count > 2) and
             (SameText(Trim(ListVariables.Items[ItemIndex].SubItems[2]), 'numeric') or
              SameText(Trim(ListVariables.Items[ItemIndex].SubItems[2]), 'numarray')) and
            (LowerCase(Trim(ListVariables.Items[ItemIndex].SubItems[0])) = TypeName) then begin
            if VariableNames <> '' then
               VariableNames := VariableNames + ',';
            if (ListVariables.Items[ItemIndex].SubItems.Count > 1) and
                (Trim(ListVariables.Items[ItemIndex].SubItems[1]) <> '') then
                VariableNames := VariableNames + ListVariables.Items[ItemIndex].SubItems[1]
             else
                VariableNames := VariableNames + ListVariables.Items[ItemIndex].Caption;
         end;
      if VariableNames <> '' then
         Result := Result + '#Compiler ' + TypeName + ' ' + VariableNames + #13#10;
   end;
end;

procedure TFormCompiler1.RefreshZXTableControls;
var
   DirectMode: Boolean;
begin
   DirectMode := RadioZXBasicDirect.Checked;
   GroupBox1.Enabled := DirectMode;
   CheckBox2.Enabled := DirectMode;
   CheckBox3.Enabled := DirectMode;
   Edit2.Enabled := DirectMode and CheckBox2.Checked;
   Edit3.Enabled := DirectMode and CheckBox3.Checked;
end;

procedure TFormCompiler1.TableOptionClick(Sender: TObject);
begin
   RefreshZXTableControls;
   UpdateButtons;
end;

procedure TFormCompiler1.RefreshAdvancedControls;
begin
   GroupDefaultType.Enabled := RadioZXBasicDirect.Checked;
   GroupVariableTypes.Enabled := RadioZXBasicDirect.Checked;
   RefreshZXTableControls;
   RefreshNumericVariables;
end;

function TFormCompiler1.GetZXProjectName: String;
var
   Source: String;
   I: Integer;
   C: Char;
begin
   Source := Trim(CurProjectName);
   if Source = '' then
      Source := ChangeFileExt(ExtractFileName(Filename), '');
   Source := TrimExtension(Source);
   Result := '';
   for I := 1 to Length(Source) do begin
      C := Source[I];
      if C in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] then
         Result := Result + C
      else if (Ord(C) >= 32) and (Ord(C) <= 126) then
         Result := Result + '_';
   end;
   Result := Trim(Result);
   if Length(Result) > 10 then
      Result := Copy(Result, 1, 10);
   if Result = '' then
      Result := 'BASIC';
end;

function TFormCompiler1.GetZXOrigin(var Origin: Word): Boolean;
var
   Value: Integer;
begin
   Value := StrToIntDef(Trim(Edit1.Text), -1);
   Result := (Value >= 0) and (Value <= 65535);
   if Result then
      Origin := Word(Value);
end;

function TFormCompiler1.BuildZXVariableOptions: String;
var
   TypeIndex, ItemIndex: Integer;
   TypeName, SwitchName, VariableNames: String;
begin
   Result := '';
   for TypeIndex := 0 to ComboVariableType.Items.Count - 1 do begin
      TypeName := LowerCase(Trim(ComboVariableType.Items[TypeIndex]));
      SwitchName := '';
      if TypeName = 'ubyte' then SwitchName := '--varubyte'
      else if TypeName = 'byte' then SwitchName := '--varbyte'
      else if TypeName = 'uint' then SwitchName := '--varuinteger'
      else if TypeName = 'int' then SwitchName := '--varinteger'
      else if TypeName = 'ulong' then SwitchName := '--varulong'
      else if TypeName = 'long' then SwitchName := '--varlong'
      else if TypeName = 'float' then SwitchName := '--varfloat'
      else if TypeName = 'fixed' then SwitchName := '--varfixed';
      if SwitchName = '' then Continue;

      VariableNames := '';
      for ItemIndex := 0 to ListVariables.Items.Count - 1 do
         if (ListVariables.Items[ItemIndex].SubItems.Count > 2) and
             (SameText(Trim(ListVariables.Items[ItemIndex].SubItems[2]), 'numeric') or
              SameText(Trim(ListVariables.Items[ItemIndex].SubItems[2]), 'numarray')) and
            (LowerCase(Trim(ListVariables.Items[ItemIndex].SubItems[0])) = TypeName) then begin
            if VariableNames <> '' then
               VariableNames := VariableNames + ',';
            if (ListVariables.Items[ItemIndex].SubItems.Count > 1) and
                (Trim(ListVariables.Items[ItemIndex].SubItems[1]) <> '') then
                VariableNames := VariableNames + ListVariables.Items[ItemIndex].SubItems[1]
             else
                VariableNames := VariableNames + ListVariables.Items[ItemIndex].Caption;
         end;
      if VariableNames <> '' then
         Result := Result + ' ' + SwitchName + ' ' + VariableNames;
   end;
end;

function TFormCompiler1.ApplyZXVariableRenames(const Source: String): String;
var
   StringRenames, StringArrayRenames, ArrayRenames: TStringList;
   I, StartPos, LookAhead: Integer;
   Token, Replacement, PreviousToken: String;
   InString, InRemark: Boolean;
   Item: TListItem;
   KindName, RenameName: String;
begin
   StringRenames := TStringList.Create;
   StringArrayRenames := TStringList.Create;
   ArrayRenames := TStringList.Create;
   try
      StringRenames.CaseSensitive := False;
      StringArrayRenames.CaseSensitive := False;
      ArrayRenames.CaseSensitive := False;
      for I := 0 to ListVariables.Items.Count - 1 do begin
         Item := ListVariables.Items[I];
         RenameName := '';
         KindName := '';
         if Item.SubItems.Count > 1 then
            RenameName := Trim(Item.SubItems[1]);
         if Item.SubItems.Count > 2 then
            KindName := LowerCase(Trim(Item.SubItems[2]));
         if (RenameName <> '') and (KindName = 'numarray') then
            ArrayRenames.Values[Item.Caption] := RenameName
         else if (RenameName <> '') and (KindName = 'strarray') then begin
            StringArrayRenames.Values[Item.Caption] := RenameName;
             { A string DIM and an ordinary string share one alias. }
             StringRenames.Values[Item.Caption] := RenameName;
          end
         else if (RenameName <> '') and (KindName = 'string') then
            StringRenames.Values[Item.Caption] := RenameName;
      end;

      Result := '';
      I := 1;
      InString := False;
      PreviousToken := '';
      InRemark := False;
            PreviousToken := '';
      while I <= Length(Source) do begin
         if Source[I] in [#13, #10] then begin
            Result := Result + Source[I];
            InString := False;
            InRemark := False;
            PreviousToken := '';
            Inc(I);
         end else if InString then begin
            Result := Result + Source[I];
            if Source[I] = '"' then
               InString := False;
            Inc(I);
         end else if InRemark then begin
            Result := Result + Source[I];
            Inc(I);
         end else if Source[I] = '"' then begin
            InString := True;
            Result := Result + Source[I];
            Inc(I);
         end else if Source[I] in ['A'..'Z', 'a'..'z'] then begin
            StartPos := I;
            while (I <= Length(Source)) and
                  (Source[I] in ['A'..'Z', 'a'..'z', '0'..'9']) do
               Inc(I);
            Token := Copy(Source, StartPos, I - StartPos);
            Replacement := '';
            if (I <= Length(Source)) and (Source[I] = '$') then begin
               Inc(I);
               Token := Token + '$';
               LookAhead := I;
               while (LookAhead <= Length(Source)) and (Source[LookAhead] = ' ') do
                  Inc(LookAhead);
               if (LookAhead <= Length(Source)) and (Source[LookAhead] = '(') then
                  Replacement := StringArrayRenames.Values[Token];
               if Replacement = '' then
                  Replacement := StringRenames.Values[Token];
               if (Replacement <> '') and
                  (StringArrayRenames.Values[Token] <> '') and
                  (LookAhead <= Length(Source)) and (Source[LookAhead] = '(') then begin
                  Result := Result + Replacement + Copy(Source, I, LookAhead - I);
                  I := LookAhead;
                  Continue;
               end;
            end else begin
               LookAhead := I;
               while (LookAhead <= Length(Source)) and (Source[LookAhead] = ' ') do
                  Inc(LookAhead);
               if (LookAhead <= Length(Source)) and (Source[LookAhead] = '(') and
                  not SameText(PreviousToken, 'FN') then begin
                  Replacement := ArrayRenames.Values[Token];
                  if Replacement <> '' then begin
                     Result := Result + Replacement + Copy(Source, I, LookAhead - I);
                     I := LookAhead;
                     Continue;
                  end;
               end;
            end;
            if Replacement <> '' then
               Result := Result + Replacement
            else
               Result := Result + Token;
            if SameText(Token, 'REM') then
               InRemark := True;
            PreviousToken := Token;
         end else begin
            Result := Result + Source[I];
            Inc(I);
         end;
      end;
   finally
      ArrayRenames.Free;
      StringArrayRenames.Free;
      StringRenames.Free;
   end;
end;

function TFormCompiler1.ValidateZXTableRange(const SwitchName, RangeText: String; var Normalized: String): Boolean;
var
   Compact, Part, StartText, EndText: String;
   I, PartStart, CommaPos, DashPos, StartValue, EndValue: Integer;
   function IsDigits(const Value: String): Boolean;
   var
      J: Integer;
   begin
      Result := Value <> '';
      for J := 1 to Length(Value) do
         if not (Value[J] in ['0'..'9']) then begin
            Result := False;
            Exit;
         end;
   end;
   function InvalidRange: Boolean;
   begin
      MessageBox(Handle,
         PChar('Invalid ' + SwitchName + ' range.'#13#10 +
               'Expected format: start-end,start-end'#13#10 +
               'Example: 10-20,50-90,5000-6000'),
         'ZX Basic-Direct', MB_OK or MB_ICONWARNING);
      Result := False;
   end;
begin
   Normalized := '';
   Compact := '';
   for I := 1 to Length(RangeText) do
      if not (RangeText[I] in [' ', #9, #10, #13]) then
         Compact := Compact + RangeText[I];
    if Compact = '' then begin
       Result := True;
       Exit;
    end;

    PartStart := 1;
   while PartStart <= Length(Compact) do begin
      CommaPos := PartStart;
      while (CommaPos <= Length(Compact)) and (Compact[CommaPos] <> ',') do
         Inc(CommaPos);
      Part := Copy(Compact, PartStart, CommaPos - PartStart);
      DashPos := Pos('-', Part);
      if (DashPos <= 1) or (DashPos >= Length(Part)) then begin
         Result := InvalidRange;
         Exit;
      end;
      StartText := Copy(Part, 1, DashPos - 1);
      EndText := Copy(Part, DashPos + 1, Length(Part));
      if not IsDigits(StartText) or not IsDigits(EndText) or
         not TryStrToInt(StartText, StartValue) or
         not TryStrToInt(EndText, EndValue) or
         (StartValue > EndValue) then begin
         Result := InvalidRange;
         Exit;
      end;
      if Normalized <> '' then
         Normalized := Normalized + ',';
      Normalized := Normalized + StartText + '-' + EndText;
      if CommaPos > Length(Compact) then
         Break;
      PartStart := CommaPos + 1;
   end;

   if (Length(Compact) > 0) and (Compact[Length(Compact)] = ',') then begin
      Result := InvalidRange;
      Exit;
   end;
   Result := True;
end;

function TFormCompiler1.ValidateZXBasicTableOptions: Boolean;
begin
   FZXJumpTableRange := '';
   FZXDynamicRestoreRange := '';
   Result := True;
   if CheckBox2.Checked and
      not ValidateZXTableRange('--enablejumptables', Edit2.Text, FZXJumpTableRange) then
      Result := False;
   if Result and CheckBox3.Checked and
      not ValidateZXTableRange('--enabledynamicrestore', Edit3.Text, FZXDynamicRestoreRange) then
      Result := False;
end;

function TFormCompiler1.BuildZXBasicCommandLine(const SourceFilename, OutputFilename: String): String;
var
   Strategy, CompilerFilename: String;
begin
   CompilerFilename := GetZXBasicCompilerFilename;
   Result := '"' + CompilerFilename + '" "' +
             SourceFilename + '" -f bin --basinc -Z';
   if CheckBox1.Checked then
      Result := Result + ' -O3';

   Strategy := LowerCase(Trim(ComboBox1.Text));
   if Strategy = 'speed' then
      Strategy := 'speed'
   else if Strategy = 'size' then
      Strategy := 'size'
   else
      Strategy := 'auto';
   Result := Result + ' --opt-strategy ' + Strategy;

   if RadioDefaultFloat.Checked then
      Result := Result + ' --default-float';
   Result := Result + ' --org ' + Trim(Edit1.Text);
   if CheckBox2.Checked then begin
      Result := Result + ' --enablejumptables';
      if FZXJumpTableRange <> '' then
         Result := Result + ' ' + FZXJumpTableRange;
   end;
   if CheckBox3.Checked then begin
      Result := Result + ' --enabledynamicrestore';
      if FZXDynamicRestoreRange <> '' then
         Result := Result + ' ' + FZXDynamicRestoreRange;
   end;
   Result := Result + BuildZXVariableOptions +
             ' -o "' + OutputFilename + '"';
end;

function TFormCompiler1.SaveTemporaryBAS(const TargetFilename: String): Boolean;
var
   OriginalFilename, OriginalBASIC, OriginalMetadata, CompilerSource, OriginalFileBody: String;
   SavedMemory: array[0..65535] of Byte;
   OriginalBracketLevel: Integer;
begin
   Result := False;
   OriginalFilename := Filename;
   OriginalBracketLevel := BASinOutput.BracketLevel;
    OriginalBASIC := BASinOutput.BASICMem;
    OriginalMetadata := GetCompilerMetadata;
   try
      CopyMemory(@SavedMemory[0], @Memory[0], SizeOf(SavedMemory));

       OriginalFileBody := FileBody;
       CompilerSource := ApplyZXVariableRenames(OriginalBASIC);
       BASinOutput.BASICMem := CompilerSource;
       SetCompilerMetadata(BuildCompilerMetadata);
       if not BASinOutput.TokeniseEditText(False) then
         Exit;
      SaveBAS(False, False);
      Filename := TargetFilename;
      Result := SaveFile;
       finally
       CopyMemory(@Memory[0], @SavedMemory[0], SizeOf(SavedMemory));
       BASinOutput.BracketLevel := OriginalBracketLevel;
       FileBody := OriginalFileBody;
      BASinOutput.BASICMem := OriginalBASIC;
       SetCompilerMetadata(OriginalMetadata);
       Filename := OriginalFilename;
   end;
end;

procedure TFormCompiler1.CompileZXBasicDirect;
var
   WorkDirectory, CompilerFilename, OutputFilename, CommandLine: String;
   Origin: Word;
begin
   if Assigned(FZXCompileThread) then Exit;
   if not HasBASICProgram then begin
      Memo1.Text := 'There is currently no BASIC program in memory.';
      UpdateButtons;
      Exit;
   end;
   if not GetZXOrigin(Origin) then begin
      Memo1.Text := 'Invalid start address.';
      UpdateButtons;
      Exit;
   end;

   if not ValidateZXBasicTableOptions then begin
       UpdateButtons;
       Exit;
    end;

    WorkDirectory := IncludeTrailingPathDelimiter(BASinDir);
   CompilerFilename := GetZXBasicCompilerFilename;
   if CompilerFilename = '' then begin
      RadioZXBasicDirect.Enabled := False;
       RadioMCoder3.Checked := True;
       AddStatus('ZX Basic-Direct: exe not found.');
       
       RefreshAdvancedControls;
      
         
      UpdateButtons;
      Exit;
   end;

   FZXSourceFilename := WorkDirectory + GetZXProjectName + '-Cmp.bas';
   OutputFilename := WorkDirectory + 'compiled_temp.bin';
   DeleteFile(WorkDirectory + 'compiled_temp.tap');
   DeleteFile(OutputFilename);

   ControlEmulation(False);
   SaveEmulationState(TempState);
    RefreshNumericVariables;
   if not SaveTemporaryBAS(FZXSourceFilename) then begin
      LoadEmulationState(TempState, True);
      CompileStateSaved := False;
      Memo1.Text := 'Could not create the temporary BAS file.';
      UpdateButtons;
      Exit;
   end;

   FZXCompiledBinary := '';
   FZXCompiledOrigin := Origin;
   CompileStateSaved := True;
   CompileSuccessful := False;
   CompileRunning := True;
   Memo1.Clear;
   AddStatus('Compiling with ZX Basic-Direct...');
   CommandLine := BuildZXBasicCommandLine(FZXSourceFilename, OutputFilename);
   FZXCompileThread := TZXBasicCompileThread.Create(Self, CommandLine, WorkDirectory);
   FZXCompileThread.Resume;
   UpdateButtons;
end;

procedure TFormCompiler1.ZXCompileFinished(const Output: String; ExitCode: DWORD);
var
   OutputFilename: String;
   Stream: TFileStream;
   DataSize: Int64;
begin
   FZXCompileThread := nil;
   CompileRunning := False;
   OutputFilename := IncludeTrailingPathDelimiter(BASinDir) + 'compiled_temp.bin';
   if ExitCode <> 0 then begin
      AddStatus('ZX Basic-Direct compilation failed. Exit code: ' + IntToStr(ExitCode));
      if CompileStateSaved or (TempState.Registers.PC <> 0) then
         LoadEmulationState(TempState, True);
      CompileSuccessful := False;
      CompileStateSaved := False;
      UpdateButtons;
      Exit;
   end;
   if not FileExists(OutputFilename) then begin
      AddStatus('Compilation finished, but compiled_temp.bin was not created.');
      if CompileStateSaved or (TempState.Registers.PC <> 0) then
         LoadEmulationState(TempState, True);
      CompileSuccessful := False;
      CompileStateSaved := False;
      UpdateButtons;
      Exit;
   end;

   Stream := TFileStream.Create(OutputFilename, fmOpenRead or fmShareDenyWrite);
   try
      DataSize := Stream.Size;
      if (DataSize <= 0) or (DataSize > 65536 - Integer(FZXCompiledOrigin)) then begin
         AddStatus('The compiled binary does not fit in emulated memory.');
         LoadEmulationState(TempState, True);
         CompileSuccessful := False;
         CompileStateSaved := False;
         UpdateButtons;
         Exit;
      end;
      SetLength(FZXCompiledBinary, Integer(DataSize));
      Stream.ReadBuffer(FZXCompiledBinary[1], Integer(DataSize));
   finally
      Stream.Free;
   end;

   CopyMemory(@Memory[FZXCompiledOrigin], @FZXCompiledBinary[1], Length(FZXCompiledBinary));
   CompileStateSaved := True;
   CompileSuccessful := True;
   AddStatus('Compiled binary loaded at ' + IntToStr(FZXCompiledOrigin) +
             ' (' + IntToStr(Length(FZXCompiledBinary)) + ' bytes).');
   UpdateButtons;
end;

procedure TFormCompiler1.RunZXBasicDirect;
var
   OriginalFilename: String;
begin
   if FZXCompiledBinary = '' then Exit;
   ControlEmulation(False);
   OriginalFilename := Filename;
   try
      Filename := IncludeTrailingPathDelimiter(BASinDir) + '48.Rom';
      LoadROM(Memory);
   finally
       Filename := OriginalFilename;
   end;
   Trapped := False;
   CopyMemory(@Memory[FZXCompiledOrigin], @FZXCompiledBinary[1], Length(FZXCompiledBinary));
   BASinOutput.Running := False;
   FASTMode := False;
   FullSpeed := False;
   Registers.PC := FZXCompiledOrigin;
   AddStatus('Starting compiled program from address ' + IntToStr(FZXCompiledOrigin) + '.');
   ControlEmulation(True);
end;

procedure TFormCompiler1.SendZXBinaryToTape;
begin
   if FZXCompiledBinary = '' then Exit;
   if Length(FZXCompiledBinary) > 65535 then begin
      AddStatus('The binary is too large for a TAP CODE block.');
      Exit;
   end;
   BinaryToTape(GetZXProjectName, FZXCompiledBinary, FZXCompiledOrigin, 1, False);
   ShowTapeWindow;
   AddStatus('Compiled binary sent to tape.');
end;

procedure TFormCompiler1.SendZXBinaryToBridge;
begin
   if (FZXCompiledBinary = '') or not Assigned(BinaryWindow) then Exit;
   BinaryWindow.ClearBinaries;
   BinaryWindow.AddBinaryEx(GetZXProjectName + '.bin', FZXCompiledBinary, btManager, FZXCompiledOrigin);
   CentreFormOnForm(BinaryWindow, Self);
   AddStatus('Compiled binary opened in the Binary Window.');
   if BinaryWindow.Visible then
      BinaryWindow.Hide;
   BinaryWindow.Enabled := True;
   Windows.EnableWindow(BinaryWindow.Handle, True);
   BinaryWindow.ShowModal;
   BringToFront;
end;

procedure TFormCompiler1.CompilerModeClick(Sender: TObject);
begin
   GroupDefaultType.Enabled := RadioZXBasicDirect.Checked;
   GroupVariableTypes.Enabled := RadioZXBasicDirect.Checked;
   TableOptionClick(Sender);
end;

procedure TFormCompiler1.SelectAllClick(Sender: TObject);
var
   I: Integer;
begin
   for I := 0 to ListVariables.Items.Count - 1 do
      ListVariables.Items[I].Selected := True;
end;

procedure TFormCompiler1.ApplyTypeClick(Sender: TObject);
var
   I: Integer;
   Metadata: String;
begin
   if ComboVariableType.ItemIndex < 0 then
      Exit;
   for I := 0 to ListVariables.Items.Count - 1 do
      if ListVariables.Items[I].Selected and
          (ListVariables.Items[I].SubItems.Count > 2) and
          (SameText(Trim(ListVariables.Items[I].SubItems[2]), 'numeric') or
           SameText(Trim(ListVariables.Items[I].SubItems[2]), 'numarray')) then
         ListVariables.Items[I].SubItems[0] := LowerCase(ComboVariableType.Items[ComboVariableType.ItemIndex]);
   Metadata := BuildCompilerMetadata;
   if Metadata <> GetCompilerMetadata then
      SetCompilerMetadata(Metadata);
end;

procedure TFormCompiler1.AddStatus(const Text: String);
begin
   Memo1.Lines.Add(Text);
   Memo1.SelStart := Length(Memo1.Text);
end;

procedure TFormCompiler1.ResetCompilerScreenRows;
begin
   FCompilerScreenLine := -1;
   FCompilerCaptureBuffer := '';
   FCompilerStage := 0;
end;

procedure TFormCompiler1.MCoderChar(var Message: TMessage);
Var
   CharCode: Byte;
   Row, Col: Word;
   LineText: String;
   StageText, StatusText: String;
begin
   CharCode := Byte(Message.WParam and $FF);
   Row := Word((Message.LParam Shr 16) and $FFFF);
   Col := Word(Message.LParam and $FFFF);

   If (Row > 23) or (Col > 31) or not (CharCode in [32..126]) Then Exit;

   If FCompilerStage < 6 Then Begin
      FCompilerCaptureBuffer := FCompilerCaptureBuffer + Chr(CharCode);
      If Length(FCompilerCaptureBuffer) > 32 Then
         Delete(FCompilerCaptureBuffer, 1, Length(FCompilerCaptureBuffer) - 32);

      Case FCompilerStage of
         0: Begin StageText := 'REM MICRO';   StatusText := 'Cleaning up...'; End;
         1: Begin StageText := 'DEFFN DATA';  StatusText := 'Parsing defined functions and data'; End;
         2: Begin StageText := 'COMPACTE';    StatusText := 'Shrinking strings'; End;
         3: Begin StageText := 'VARIABLES';   StatusText := 'Checking variables'; End;
         4: Begin StageText := 'ROUTINES';    StatusText := 'Preparing to compile'; End;
         5: Begin StageText := 'PLAY';        StatusText := 'Compiling now...'; End;
      End;

      If Pos(StageText, FCompilerCaptureBuffer) > 0 Then Begin
         AddStatus(StatusText);
         Inc(FCompilerStage);
         FCompilerCaptureBuffer := '';
      End;

      Exit;
   End;

   // After PLAY, MCODER updates the current BASIC line number in-place.
   If not (CharCode in [Ord('0')..Ord('9')]) Then Exit;

   If FCompilerScreenLine < 0 Then Begin
      Memo1.Lines.Add(StringOfChar(' ', 32));
      FCompilerScreenLine := Memo1.Lines.Count - 1;
   End;

   If FCompilerScreenLine >= Memo1.Lines.Count Then Exit;

   LineText := Memo1.Lines[FCompilerScreenLine];
   While Length(LineText) < 32 Do
      LineText := LineText + ' ';
   LineText[Col + 1] := Chr(CharCode);
   Memo1.Lines[FCompilerScreenLine] := LineText;
   Memo1.SelStart := Length(Memo1.Text);
end;

procedure TFormCompiler1.MCoderStatus(var Message: TMessage);
Var
   ErrorNumber, ErrorStatement: Byte;
   ErrorIndex: Integer;
   ErrorLine: Word;
   ErrorText: String;
begin
   Case TMCoderStatus(Message.WParam) of
      msCompileStarted:
         Begin
            CompileRunning := True;
            AddStatus('Compiling...');
            UpdateButtons;
         End;
      msPart1Loaded:
         AddStatus('Loaded compiler part 1.');
      msPart2Loaded:
         AddStatus('Loaded compiler part 2.');
      msCompileFailed:
         Begin
            ErrorNumber := Byte(Cardinal(Message.LParam) and $FF);
            ErrorStatement := Byte((Cardinal(Message.LParam) Shr 8) and $FF);
            ErrorLine := Word((Cardinal(Message.LParam) Shr 16) and $FFFF);

            If ErrorNumber = $FF Then
               ErrorIndex := 0
            Else
               ErrorIndex := ErrorNumber + 1;

            ErrorText := '';
            If (ErrorIndex > 0) and (ErrorIndex <= High(ErrorAddresses)) Then
               ErrorText := ErrorAddresses[ErrorIndex].Desc + ', ' +
                            IntToStr(ErrorLine) + ':' +
                            IntToStr(ErrorStatement);

            ControlEmulation(False);
            If CompileStateSaved Then
               LoadEmulationState(TempState, True);
            If (ErrorLine > 0) and (ErrorLine < 10000) Then
               PostMessage(BASinOutput.Handle, WM_UPDATEPROGCURSOR,
                           ErrorLine, ErrorStatement);
            CleanupPreviewTapeBlock;
            FASTMode := False;
            FullSpeed := False;
            CompileRunning := False;
            CompileSuccessful := False;
            CompileStateSaved := False;
            If ErrorText <> '' Then
               AddStatus('Compile failed: ' + ErrorText)
            Else
               AddStatus('Compile failed.');
            If Copy(ErrorText, 1, 1) = '3' Then
               AddStatus('STRINGS & ARRAYS must have constant dimensions. ie. DIM b (i,j) is illegal and would provoke an error 3. DIM a (10,20,30) is legal. Neither can you re-dimension an existing array.');
            UpdateButtons;
         End;
      msCompileComplete:
         Begin
            FASTMode := False;
            FullSpeed := False;
            CompileRunning := False;
            CaptureCompiledProgram;
            CompileSuccessful := FCompiledProgramBody <> '';
            If CompileSuccessful Then
               AddStatus('Compile complete, Run, Save or Revert to Basic')
            Else
               AddStatus('Compile complete, but compiled program could not be captured.');
            UpdateButtons;
         End;
   End;
end;

function TFormCompiler1.HasBASICProgram: Boolean;
begin
   Result := GetWord(@Memory[PROG]) <> GetWord(@Memory[VARS]);
end;

function TFormCompiler1.HasMCoderTape: Boolean;
begin
   // The MCODER3 TAP dependency is temporarily optional.
   Result := True;
end;

function TFormCompiler1.GetZXBasicCompilerFilename: String;
var
   RootDirectory, Candidate: String;
begin
   RootDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
   Candidate := RootDirectory + 'external\zxbcdirector\zxbc-director.exe';
   if FileExists(Candidate) then begin
      Result := Candidate;
      Exit;
   end;
   Candidate := RootDirectory + 'external\zxbc-direct.exe';
   if FileExists(Candidate) then
      Result := Candidate
   else
      Result := '';
end;

function TFormCompiler1.Is128kCompileBlocked: Boolean;
begin
   CheckFor128kCommands;
   Result := In128k or ProgramIs128k;
end;

procedure TFormCompiler1.UpdateButtons;
begin
   if RadioZXBasicDirect.Checked then
      Button1.Enabled := HasBASICProgram and (GetZXBasicCompilerFilename <> '') and
                          not CompileRunning and not CompileSuccessful and
                         not Registers.EmuRunning
   else
      Button1.Enabled := HasMCoderTape and HasBASICProgram and not Is128kCompileBlocked and
                         not CompileRunning and not CompileSuccessful and
                         not Registers.EmuRunning;
   if RadioZXBasicDirect.Checked then begin
      Button2.Enabled := CompileSuccessful and (FZXCompiledBinary <> '');
      Button3.Enabled := CompileSuccessful and (FZXCompiledBinary <> '');
      Button7.Enabled := CompileSuccessful and (FZXCompiledBinary <> '');
   end else begin
      Button2.Enabled := CompileSuccessful and (FCompiledProgramBody <> '');
      Button3.Enabled := CompileSuccessful and (FCompiledProgramBody <> '');
      Button7.Enabled := False;
   end;
   Button5.Enabled := not CompileRunning;
   Button6.Enabled := CompileStateSaved and not CompileRunning;
end;

procedure TFormCompiler1.ResetCompileState;
begin
   CompileStateSaved := False;
   CompileSuccessful := False;
   CompileRunning := False;
   PreviewTapeLoading := False;
   FAllowClose := False;
   FCompiledProgramBody := '';
   FCompiledProgramLen := 0;
   FCompiledDataLen := 0;
   FPreviewTapeIndex := -1;
   FPreviewTapeBlock := '';
   FZXCompiledBinary := '';
   FZXCompiledOrigin := 0;
   FZXSourceFilename := '';
   ResetCompilerScreenRows;
end;

procedure TFormCompiler1.FormShow(Sender: TObject);
var
   ZXCompilerAvailable: Boolean;
begin
     ResetCompileState;
     ZXCompilerAvailable := GetZXBasicCompilerFilename <> '';
      RadioMCoder3.Enabled := True;
      RadioZXBasicDirect.Enabled := ZXCompilerAvailable;
      if not ZXCompilerAvailable and RadioZXBasicDirect.Checked then
         RadioMCoder3.Checked := True;
      
         
      Memo1.Text:= 'Ready to compile.';
     If RadioMCoder3.Checked and not HasMCoderTape Then Begin
        Memo1.Text:= 'To use the compiler, the TAP file from ZXDB or Infoseek 8421 entry must be placed in the same folder as basinc.exe. See. spectrumcomputing.co.uk/entry/8421 *TAP* version.';
     End Else If Not HasBASICProgram Then Begin
        Memo1.Text:= 'There is currently no BASIC program in memory.';
     End Else If RadioMCoder3.Checked and Is128kCompileBlocked Then Begin
         Memo1.Text:= 'This compiler can only compile 48K BASIC programs. 128K mode or 128K commands were detected.';
      End;
      



      


      
         


      if not ZXCompilerAvailable then
         Memo1.Lines.Add('ZX Basic-Direct unavailable.');
      RefreshAdvancedControls;
      UpdateButtons;
end;

procedure TFormCompiler1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := FAllowClose or AppClosing;
end;

procedure TFormCompiler1.Button1Click(Sender: TObject);
begin
   If RadioZXBasicDirect.Checked Then Begin
      CompileZXBasicDirect;
      Exit;
   End;
   If RadioMCoder3.Checked and not HasMCoderTape Then Begin
      Memo1.Text:= 'To use the compiler, the TAP file from ZXDB Infoseek entry 8421 must be placed in the same folder as basinc.exe.';
      UpdateButtons;
      Exit;
   End;

   If Is128kCompileBlocked Then Begin
      Memo1.Text:= 'This compiler can only compile 48K BASIC programs. 128K mode or 128K commands were detected.';
      UpdateButtons;
      Exit;
   End;

   If Not HasBASICProgram Then Begin
      Memo1.Text:= 'There is currently no BASIC program in memory.';
      UpdateButtons;
      Exit;
   End;

   // Load MCODER3 loader and part 1, jump to $4BDB and run.
   ControlEmulation(False);
   SaveEmulationState(TempState);
   CompileStateSaved := True;
   CompileSuccessful := False;
   CompileRunning := True;
   ResetCompilerScreenRows;
   FASTMode := True;
   UpdateButtons;
   Filename := BASinDir+'\48.Rom';
   LoadROM(Memory); //disable all traps.
   Romutils.MCoderResetPrintCapture;
   PutByte(@Memory[23624], 0);  //set border color to 0
   PutWord(@Memory[$0BC5], $00ED); //trap printed characters while MCODER is running
   CopyMemory(@Memory[23536], @MCODER3_PART0_LOADER[0], SizeOf(MCODER3_PART0_LOADER));

   //Trap Mcoder entry point
   Romutils.Old07f5:= GetWord(@Memory[$07f5]);
   PutWord(@Memory[$07f5], $00ED);
   Registers.SP:=$5BF2;
   Registers.PC:=$5c00;
   PostMCoderCompilerStatus(msCompileStarted);
   ControlEmulation(True);

end;

procedure TFormCompiler1.Button2Click(Sender: TObject);
Var
   Block: String;
   SourceState: TEmulationState;
begin
   If RadioZXBasicDirect.Checked Then Begin
      RunZXBasicDirect;
      Exit;
   End;
   If (Not CompileSuccessful) or (FCompiledProgramBody = '') Then Exit;

   ControlEmulation(False);
   FASTMode := False;
   FullSpeed := False;

   Block := BuildCompiledProgramTapeBlock;
   If Block = '' Then Exit;

   CleanupPreviewTapeBlock;
   CopyMemory(@SourceState, @TempState, SizeOf(TEmulationState));
   TapeBlockAdd(Block);
   FPreviewTapeIndex := TapeBlocks.Count - 1;
   FPreviewTapeBlock := Block;
   TapePosition := FPreviewTapeIndex;
   TapeWindow.UpdateTapeList;
   Opt_TapeTrapLOAD := True;
   PreviewTapeLoading := True;
   LOADQuoteQuote('');
   FASTMode := False;
   FullSpeed := False;
   CopyMemory(@TempState, @SourceState, SizeOf(TEmulationState));

   AddStatus('Running compiled preview from clean LOAD state.');
   UpdateButtons;
end;

procedure TFormCompiler1.Button3Click(Sender: TObject);
begin
   If RadioZXBasicDirect.Checked Then Begin
      SendZXBinaryToTape;
      Exit;
   End;
   If (Not CompileSuccessful) or (FCompiledProgramBody = '') Then Exit;
   ShowTapeWindow;
   AddCompiledProgramToTape;
   AddStatus('Compiled program sent to tape.');
end;

procedure TFormCompiler1.Button7Click(Sender: TObject);
begin
   If RadioZXBasicDirect.Checked Then
      SendZXBinaryToBridge;
end;

procedure TFormCompiler1.Button5Click(Sender: TObject);
begin
   CloseFromButton;
end;

procedure TFormCompiler1.RestoreZXEditorState;
begin
   ControlEmulation(False);
   if CompileStateSaved then
      LoadEmulationState(TempState, True);
   ControlEmulation(False);
   BASinOutput.Running := False;
   FASTMode := False;
   FullSpeed := False;
   Trapped := False;
   ProgStateFlag := PS_UNKNOWN;
   PostMessage(BASinOutput.Handle, WM_UPDATEPROGBUTTONS, 0, 0);
   PostMessage(BASinOutput.Handle, WM_UPDATEPROGRAM, 0, 0);
   PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
   PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
   CompileStateSaved := False;
   CompileRunning := False;
   CompileSuccessful := False;
   FZXCompiledBinary := '';
end;

procedure TFormCompiler1.CloseFromButton;
begin
   if CompileRunning then Exit;
   If CompileStateSaved and (CompileRunning or CompileSuccessful) Then Begin
      If MessageDlg('The BASIC program currently in memory has been replaced by compiled code.'#13#10+
                    'Closing this window without reverting will discard the editable BASIC version.'#13#10#13#10+
                    'Continue closing?',
                    mtWarning, [mbOK, mbCancel], 0) <> mrOK Then
         Exit;
   End;

   { Close deliberately leaves the compiled runtime state in place.  Use
     Revert and Keep Editing when the original editor state is required. }
   FAllowClose := True;
   Close;
end;

procedure TFormCompiler1.ShowTapeWindow;
begin
   TapeWindow.Left := Left + Width + 8;
   If TapeWindow.Left + TapeWindow.Width > Screen.DesktopWidth Then
      TapeWindow.Left := Left - TapeWindow.Width - 8;
   If TapeWindow.Left < 0 Then
      TapeWindow.Left := 0;
   TapeWindow.Top := Top;
   If TapeWindow.Top + TapeWindow.Height > Screen.DesktopHeight Then
      TapeWindow.Top := Screen.DesktopHeight - TapeWindow.Height;
   If TapeWindow.Top < 0 Then
      TapeWindow.Top := 0;

   If Not TapeWindow.Visible Then
      Utility.ShowWindow(TapeWindow, False)
   Else
      TapeWindow.BringToFront;
end;

procedure TFormCompiler1.AddCompiledProgramToTape;
Var
   Block: String;
begin
   Block := BuildCompiledProgramTapeBlock;
   If Block = '' Then Exit;

   TapeBlockAdd(Block);
   TapeWindow.UpdateTapeList;
end;

procedure TFormCompiler1.CleanupPreviewTapeBlock;
begin
   If (FPreviewTapeIndex >= 0) and (FPreviewTapeIndex < TapeBlocks.Count) and
      (TapeBlocks[FPreviewTapeIndex] = FPreviewTapeBlock) Then Begin
      TapeBlocks.Delete(FPreviewTapeIndex);
      If TapePosition > FPreviewTapeIndex Then
         Dec(TapePosition)
      Else If TapePosition >= TapeBlocks.Count Then
         TapePosition := TapeBlocks.Count - 1;
      If TapePosition < 0 Then
         TapePosition := 0;
      TapeWindow.UpdateTapeList;
   End;

   FPreviewTapeIndex := -1;
   FPreviewTapeBlock := '';
   PreviewTapeLoading := False;
end;

procedure TFormCompiler1.CaptureCompiledProgram;
Var
   ProgramStart: Word;
begin
   ProgramStart := GetWord(@Memory[PROG]);

   FCompiledProgramBody := '';
   FCompiledProgramLen := 0;
   FCompiledDataLen := 0;

   If (GetWord(@Memory[E_LINE]) <= ProgramStart) or
      (GetWord(@Memory[VARS]) < ProgramStart) or
      (GetWord(@Memory[VARS]) > GetWord(@Memory[E_LINE])) Then
      Exit;

   // E_LINE points one byte past the variables terminator. The ROM loader
   // supplies that $80 byte itself when loading a BASIC program.
   FCompiledDataLen := GetWord(@Memory[E_LINE]) - ProgramStart - 1;
   FCompiledProgramLen := GetWord(@Memory[VARS]) - ProgramStart;
   FCompiledProgramBody := GetMemoryString(ProgramStart, FCompiledDataLen, Memory);
end;

procedure TFormCompiler1.InjectCompiledProgram;
Var
   ProgramStart, EndAddress: Word;
   F: Integer;
begin
   ProgramStart := GetWord(@Memory[PROG]);
   If FCompiledProgramBody = '' Then Exit;

   For F := 1 To Length(FCompiledProgramBody) Do
      Memory[ProgramStart + F - 1] := Ord(FCompiledProgramBody[F]);

   EndAddress := ProgramStart + FCompiledDataLen;
   Memory[EndAddress] := $80;

   PutWord(@Memory[VARS], ProgramStart + FCompiledProgramLen);
   PutWord(@Memory[E_LINE], EndAddress + 1);
   PutWord(@Memory[WORKSP], EndAddress + 1);
   PutWord(@Memory[STKBOT], EndAddress + 1);
   PutWord(@Memory[STKEND], EndAddress + 1);

   PostMessage(BASinOutput.Handle, WM_UPDATEPROGRAM, 1, 0);
   PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
end;

function TFormCompiler1.BuildCompiledProgramTapeBlock: String;
Var
   F: Integer;
   FileName, Header: String;
   CheckSum: Byte;
begin
   Result := '';
   If (FCompiledProgramBody = '') or (FCompiledDataLen = 0) Then Exit;

   FileName := Copy(CurProjectName, 1, 10);
   While Length(FileName) < 10 Do FileName := FileName + ' ';

   Header := #19+#0+#0+#0+FileName+
             Chr(FCompiledDataLen And 255)+Chr(FCompiledDataLen Shr 8)+
             #1#0+
             Chr(FCompiledProgramLen And 255)+Chr(FCompiledProgramLen Shr 8);

   CheckSum := 0;
   For F := 4 To Length(Header) Do
      CheckSum := CheckSum Xor Ord(Header[F]);
   Header := Header + Chr(CheckSum);

   Result := Header;
   Result := Result + Chr((FCompiledDataLen + 2) And 255)+Chr((FCompiledDataLen + 2) Shr 8)+#255+FCompiledProgramBody;

   CheckSum := 255;
   For F := 1 To Length(FCompiledProgramBody) Do
      CheckSum := CheckSum Xor Ord(FCompiledProgramBody[F]);
   Result := Result + Chr(CheckSum);
end;

procedure TFormCompiler1.Button6Click(Sender: TObject);
begin
   If RadioZXBasicDirect.Checked Then Begin
      RestoreZXEditorState;
      CloseFromButton;
      Exit;
   End;
   If CompileStateSaved Then
      LoadEmulationState(TempState, True);
   CleanupPreviewTapeBlock;
   CompileRunning := False;
   CompileSuccessful := False;
   CloseFromButton;
end;

end.
