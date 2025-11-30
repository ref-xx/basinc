program BasinC;

uses
  Forms,
  Windows,
  Messages,
  SysUtils,
  dialogs,
  BasinMain in 'BasinMain.pas' {BASinOutput},
  Display in 'Display.pas' {DisplayWindow},
  GridSetup in 'GridSetup.pas' {GridSetUpWindow},
  Binaries in 'Binaries.pas',
  BinaryGrab in 'BinaryGrab.pas' {BinaryGrabWindow},
  Paintbox in 'Paintbox.pas' {ScrPaintForm},
  BrushSelector in 'BrushSelector.pas' {BrushSelectorForm},
  TextPaint in 'TextPaint.pas' {TextForm},
  ImageImport in 'ImageImport.pas' {BMPImportForm},
  ScrPreview in 'ScrPreview.pas' {ScrPreviewForm},
  MemManager in 'MemManager.pas' {MemManagerForm},
  Languages in 'Languages.pas',
  ConsoleOutput in 'ConsoleOutput.pas' {ConsoleOutForm},
  UlaColours in 'UlaColours.pas' {UlaColoursWindow},
  basinet in 'basinet.pas' {BasinetWindow},
  Utility in 'utility.pas',
  notes in 'notes.pas' {NotesWindow},
  BlockUnit in 'BlockUnit.pas',
  CompressorUnit in 'CompressorUnit.pas',
  VarsWindow in 'VarsWindow.pas' {VariablesWindow},
  Options in 'Options.pas' {OptionsWindow},
  Tapes in 'Tapes.pas' {TapeWindow},
  ASMEditor in 'ASMEditor.pas' {ASMEditorWindow},
  HexEdit in 'HexEdit.pas' {HexWindow},
  zx0packer in 'zx0packer.pas',
  RomPrintOutputUnit in 'RomPrintOutputUnit.pas' {RomPrintOutputWindow};

{$R *.RES}
//{$R WindowsXP.RES}
{$Z4}

var
 Mutex : THandle;
 PreviousHandle : THandle;
 res: integer;
 copyDataStruct : TCopyDataStruct;
 stringToSend : string;
 //i: integer;
begin

  //  For i := 0 to ParamCount do
  //  ShowMessage('Parameter '+IntToStr(i)+' = '+paramstr(i));

  BASinDir := ExtractFilePath(Application.Exename);
  While Copy(BASinDir, Length(BASinDir), 1) = '\' Do
     BASinDir := Copy(BASinDir, 1, Length(BASinDir)-1);

   MyMsg := RegisterWindowMessage('Basinc_Mutex');
  Mutex := CreateMutex(nil, True, 'Basinc_Mutex');
  if ( (Mutex = 0) OR (GetLastError = ERROR_ALREADY_EXISTS)) AND (FileExists(BasinDir +'\allowinstance.opt')=false) then
  begin

    //this is a second instance, send a message to first instance and quit silently.
    PreviousHandle := FindWindow('TBasinOutput', pchar(ReleaseName));
    if PreviousHandle <> 0 then
    begin
     SetForegroundWindow(PreviousHandle);
     if (ParamStr(1)<>'') then begin
         stringToSend := ParamStr(1);
         copyDataStruct.dwData := 0; //use it to identify the message contents
         copyDataStruct.cbData := 1 + Length(stringToSend);
         copyDataStruct.lpData := PChar(stringToSend);

         res := SendMessage(PreviousHandle, WM_COPYDATA, Integer(PreviousHandle), Integer(@copyDataStruct));
         if res=0 Then
         Begin
         //do nothing
         End;
     end;

    end;

end
else
begin

  Application.Initialize;
  Application.Title := 'BasinC';
  Application.CreateForm(TBASinOutput, BASinOutput);
  Application.CreateForm(TDisplayWindow, DisplayWindow);
  Application.CreateForm(TBinaryGrabWindow, BinaryGrabWindow);
  Application.CreateForm(TScrPaintForm, ScrPaintForm);
  Application.CreateForm(TBrushSelectorForm, BrushSelectorForm);
  Application.CreateForm(TTextForm, TextForm);
  Application.CreateForm(TBMPImportForm, BMPImportForm);
  Application.CreateForm(TScrPreviewForm, ScrPreviewForm);
  Application.CreateForm(TNotesWindow, NotesWindow);
  Application.CreateForm(TVariablesWindow, VariablesWindow);
  Application.CreateForm(TOptionsWindow, OptionsWindow);
  Application.CreateForm(TTapeWindow, TapeWindow);
  Application.CreateForm(TASMEditorWindow, ASMEditorWindow);
  Application.CreateForm(THexWindow, HexWindow);
  Application.CreateForm(TRomPrintOutputWindow, RomPrintOutputWindow);
  Application.Run;
  if Mutex <> 0 then CloseHandle(Mutex);

end;                       





end.





//  EnumWindows( @EnumWindowsProc, NULL);

