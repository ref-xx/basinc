unit BasinMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, FastIMG, ExtCtrls, FastDIB, Utility, StdCtrls,
  FileCtrl, ClipBrd, Math, FastSize, FastFiles, FastDraw, DirectSound,
  ToolWin, Buttons, ImgList, CommCtrl, ROMUtils, Tabs, MMSystem, ShellAPI,
  ThemeMgr, TransparentPanel, AnimPreview, Parser, ConsoleOutput,Languages,
  GraphicEx;

type

  TSourceMarker = Record MarkedLine, MarkedStatement: Integer; Assigned: Boolean; End;

  TBASinOutput = class(TForm)

    StatusBar1:                    TStatusBar;
    Panel1:                        TPanel;
    Panel2: TCoolBar;
    Timer2:                        TTimer;
    Timer1:                        TTimer;

    FastIMG1:                      TFastIMG;
    Image2:                        TImage;
    Image3:                        TImage;
    Image4:                        TImage;
    Image5:                        TImage;

    MainMenu1:                     TMainMenu;
     File1:                        TMenuItem;
        New1:                      TMenuItem;
        N5:                        TMenuItem;
        Load1:                     TMenuItem;
        ReLOAD1:                   TMenuItem;
           PreviousSession1:       TMenuItem;
           N12:                    TMenuItem;
           Item11:                 TMenuItem;
           Item21:                 TMenuItem;
           Item31:                 TMenuItem;
           Item41:                 TMenuItem;
           Item51:                 TMenuItem;
           Item61:                 TMenuItem;
           Item71:                 TMenuItem;
           Item81:                 TMenuItem;
        ImportBASIC1:              TMenuItem;
        ImportfromTapeImage1:      TMenuItem;
        N1:                        TMenuItem;
        Save1:                     TMenuItem;
        SaveBASICas1:              TMenuItem;
        N2:                        TMenuItem;
        Print1:                    TMenuItem;
        N9:                        TMenuItem;
        Exit1:                     TMenuItem;
     Edit1:                        TMenuItem;
        Undo1:                     TMenuItem;
        Redo1:                     TMenuItem;
        N6:                        TMenuItem;
        Cut1:                      TMenuItem;
        Copy1:                     TMenuItem;
        Paste1:                    TMenuItem;
        Delete1:                   TMenuItem;
        N14:                       TMenuItem;
        CopyListing1:              TMenuItem;
     Search1:                      TMenuItem;
        Find1:                     TMenuItem;
        Replace1:                  TMenuItem;
        FindNext1:                 TMenuItem;
        ReplaceNext1:              TMenuItem;
        N10:                       TMenuItem;
        GotoLineNumber1:           TMenuItem;
        GotoError1:                TMenuItem;
     View1:                        TMenuItem;
        ProgramInformation1:       TMenuItem;
        LastError1:                TMenuItem;
        CharacterRuler1:           TMenuItem;
        DebugWindows1:             TMenuItem;
           SyntaxHelper1:          TMenuItem;
           CommandHistory1:        TMenuItem;
           N15:                    TMenuItem;
           Variables1:             TMenuItem;
           SystemVariables1:       TMenuItem;
           Watches1:               TMenuItem;
           GOSUBStack1:            TMenuItem;
           Breakpoints1:           TMenuItem;
           LogWindow1:             TMenuItem;
           MemoryViewer1:          TMenuItem;
           MemoryMap1:             TMenuItem;
     WindowSize1:                  TMenuItem;
        DisplayWindow1:            TMenuItem;
        N100320x2401:              TMenuItem;
        N200640x4801:              TMenuItem;
        Custom1:                   TMenuItem;
        N13:                       TMenuItem;
        Force11Aspect1:            TMenuItem;
     Run1:                         TMenuItem;
        Run2:                      TMenuItem;
        Continue1:                 TMenuItem;
        GOTO1:                     TMenuItem;
        N11:                       TMenuItem;
        ForceBREAK1:               TMenuItem;
        N7:                        TMenuItem;
        TraceExecution1:           TMenuItem;
        SingleStepStatement1:      TMenuItem;
        StepToNext1:               TMenuItem;
        RunTo1:                    TMenuItem;
        N8:                        TMenuItem;
        ExpressionEvaluator1:      TMenuItem;
        AddBreakpoint1:            TMenuItem;
        AddWatch1:                 TMenuItem;
     Tools1:                       TMenuItem;
        BASinOptions1:             TMenuItem;
        N4:                        TMenuItem;
        TokenTable1:               TMenuItem;
        BEEPComposer1:             TMenuItem;
        UDGEditor1:                TMenuItem;
        ScreenPaintbox1:           TMenuItem;
        Renumber1:                 TMenuItem;
        TapeCreator1:              TMenuItem;
        Compiler1:                 TMenuItem;
     Help1:                        TMenuItem;
        Contents1:                 TMenuItem;
        CommandHelp1:              TMenuItem;
        ErrorHelp1:                TMenuItem;
        SinclairBASICManual1:      TMenuItem;
        N3:                        TMenuItem;
        About1:                    TMenuItem;

    PopupMenu1:                    TPopupMenu;
     Token1:                       TMenuItem;
     Help2:                        TMenuItem;
     EditVariable1:                TMenuItem;
     Cut2:                         TMenuItem;
     Copy2:                        TMenuItem;
     Paste2:                       TMenuItem;
     N16:                          TMenuItem;
     N17:                          TMenuItem;
     Debug1:                       TMenuItem;
     ToggleBreakpoint1:            TMenuItem;
     RunToCursor1:                 TMenuItem;
     GoToCursor1:                  TMenuItem;
     WatchVariable1:               TMenuItem;
     FindLine1:                    TMenuItem;
     Tokenise1:                    TMenuItem;

    ThemeManager1: TThemeManager;

    ZXPrinterOutput1: TMenuItem;
    oggleBreakpoint1: TMenuItem;
    ProfileResults1: TMenuItem;
    EnableProfiling1: TMenuItem;
    Assembler1: TMenuItem;
    CPUWindow1: TMenuItem;
    Wordwrapstring1: TMenuItem;
    Splitat32chars1: TMenuItem;
    Insertspaces1: TMenuItem;
    SourceMarkers1: TMenuItem;
    SetMarker1: TMenuItem;
    GetMarker1: TMenuItem;
    Marker01: TMenuItem;
    Marker11: TMenuItem;
    Marker21: TMenuItem;
    Marker31: TMenuItem;
    Marker41: TMenuItem;
    Marker51: TMenuItem;
    Marker02: TMenuItem;
    Marker12: TMenuItem;
    Marker22: TMenuItem;
    Marker32: TMenuItem;
    Marker42: TMenuItem;
    Marker52: TMenuItem;
    N18: TMenuItem;
    Clearall1: TMenuItem;
    Marker61: TMenuItem;
    Marker71: TMenuItem;
    Marker81: TMenuItem;
    Marker91: TMenuItem;
    Marker62: TMenuItem;
    Marker72: TMenuItem;
    Marker82: TMenuItem;
    Marker92: TMenuItem;
    Bevel5: TThemeBevel;
    StatusBar2: TMenuItem;
    ToolBar1: TMenuItem;
    StringOperation1: TMenuItem;
    Tokeniseall1: TMenuItem;
    Detokeniseall1: TMenuItem;
    CoolBar1: TCoolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Bevel1: TThemeBevel;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Bevel2: TThemeBevel;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Bevel3: TThemeBevel;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    Bevel4: TThemeBevel;
    SpeedButton12: TSpeedButton;
    Bevel6: TThemeBevel;
    Bevel7: TThemeBevel;
    Label1: TLabel;
    Memorygrabber1: TMenuItem;
    RulerIMG: TFastIMG;
    FullSpeed1: TMenuItem;
    N19: TMenuItem;
    MemoryEditor1: TMenuItem;
    LoadBasinState1: TMenuItem;
    SaveWorkspace1: TMenuItem;
    simplecon1: TMenuItem;
    UlaPlusPalette1: TMenuItem;
    ExportTap1: TMenuItem;
    BasinCNetworkDrive1: TMenuItem;
    SendtoExternalUtiility1: TMenuItem;
    CodeControlIcons1: TMenuItem;
    Timer3: TTimer;
    LabelDebug: TLabel;
    ProjectNotes1: TMenuItem;
    AddNote1: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    AddSnippet1: TMenuItem;

    procedure FormClose            (Sender: TObject; var Action: TCloseAction);
    procedure IdleProc             (Sender: TObject; var Done: Boolean);
    procedure FormCreate           (Sender: TObject);
    procedure FormShow             (Sender: TObject);
    procedure FormResize           (Sender: TObject);
    procedure MenuItemClick        (Sender: TObject);
    procedure OnHint               (Sender: TObject);
    procedure OnShowHint           (var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure FormActivate         (Sender: TObject);
    procedure Timer2Timer          (Sender: TObject);
    procedure Timer1Timer          (Sender: TObject);
    procedure FastIMG1DblClick     (Sender: TObject);
    procedure SpeedButton4Click    (Sender: TObject);
    procedure StatusBar1DrawPanel  (StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure FormMouseWheelDown   (Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp     (Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure Token1DrawItem       (Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure FormKeyDown          (Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp            (Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FastIMG1MouseMove    (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp      (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseDown    (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBox1VScroll    (Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType);
    procedure ScrollBox1HScroll    (Sender: TObject; Pos: SmallInt; EventType: THScrollEventType);
    Function  AppKeyDownHook       (var Msg: TMessage): Boolean;

    procedure OnUpdateProg         (var Message: TMessage); message WM_UPDATEPROGRAM;
    procedure OnUpdateProgCursor   (var Message: TMessage); message WM_UPDATEPROGCURSOR;
    procedure OnUpdateProgButtons  (var Message: TMessage); message WM_UPDATEPROGBUTTONS;
    procedure OnUpdateVars         (var Message: TMessage); message WM_UPDATEVARS;
    procedure OnEnterMenuLoop      (var Message: TMessage); message WM_ENTERMENULOOP;
    procedure OnExitMenuLoop       (var Message: TMessage); message WM_EXITMENULOOP;
    procedure OnUpdateCursor       (var Message: TMessage); message WM_UPDATECURSOR;
    procedure CMDialogKey          (var msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure UpdateMenu;
    procedure SetCaption;
    Procedure GetOSVersion;
    procedure UpdateParseText;
    Procedure SetToolBarButtons;
    procedure AddToMRUList         (Filename: String);
    Function  GetCurrentEditorLine:String;
    Function  TestLine             (LineAdd: String; ErrorCheck: Boolean; var IsDirect: Boolean): Boolean;
    procedure FormDeactivate       (Sender: TObject);
    Procedure UpdateCursorPos      (Offset: Integer; Shift: Boolean);
    Function  AddLine              (Line: String): Boolean;
    function  GetShortName         (sLongName: string): string;

    Function  TokeniseEditText     (SyntaxCheck: Boolean): Boolean;
    Procedure TokeniseString;
    Procedure DeTokeniseString;
    Procedure SetRuler;
    Procedure DrawRuler;
    Procedure AcceptPrediction;
    Procedure ClearPrediction;
    Procedure SetSyntaxHelper;
    procedure PopupMenu1Popup      (Sender: TObject);
    procedure Help2MeasureItem     (Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure Tokenise1Click       (Sender: TObject);
    Procedure SendToEditor         (Token: Byte);
    Procedure WordWrap             (Start, Len, Mode: Integer);
    Procedure ShowWindows;
    procedure simplecon1Click(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);


  private
    { Private declarations }
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA; //arda

  public
    { Public declarations }

    ScrollBox1:                    TNewScrollBox;

    ColourLabel1,
    ColourLabel2:                  TColourLabel;

    MousePos:                      TPoint;
    MouseDown:                     Boolean;
    NonActivePaint:                Boolean;

    AutoBack,
    TokenisePoint,
    TokeniseLen,
    BracketLevel,
    ViewOffset,
    ViewLine,
    ViewColumn,
    StringLen,
    StringStart,
    CursProgLineStart,
    CursProgLineEnd,
    CursPrevLineStart,
    CursLineStart,
    CursPageStart,
    CursPageEnd:                   Integer;

    CursStringStart,
    CursStringEnd,
    CursStringOffset,
    CursOffset:                    DWord;

    ShowingPrediction,
    InsertMode,
    InMenu:                        Boolean;
    PageMove,
    BASICChanged:                  Boolean;
    SystTemp,
    LastLineBuffer,
    BASICMem:                      String;

    WantNewLine,
    CodeError,
    CursorInString,
    CursorVisible,
    SplitStatements,
    CursorState:                   Boolean;

    CursorPoint:                   TPoint;
    CursListLine,
    CursLineNum,
    CursStatementNum:              Word;
    CursorChar:                    Char;

    EditorGFXMode,
    AbortStatement,
    OverWriteCursor,
    SingleStep:                    Boolean;
    NextLine:                      Boolean;
    Running:                       Boolean;
    RunningAck:                    Boolean;
    LineSel,
    NXTLIN,
    GOTOStatement,
    GOTOLine,
    HLLine,
    HLStatement,
    RunStatement,
    RunLine:                       DWord;


    UndoItem:                      String;
    UndoList,
    RedoList,
    BASICList:                     TStringlist;

    Abort:                         Boolean;


    SourceMarkers:                 Array[0..10] of TSourceMarker;

    Procedure GetBASIC;
    Procedure RepaintBASIC         (DoPaint: Boolean);
    Function  RepaintCursor:       Boolean;
    Procedure MakeCursorVisible;
    Procedure PerformTokenIn       (Tokens: String);
    Procedure PerformKeyDown       (Key: Word; Shift: TShiftState; Repaint: Boolean);
    function  GetCharPos           (X, Y: Integer): DWord;
    Procedure PopUp;
    procedure SetGoTo              (Sender: TObject);
    procedure RunTo                (Sender: TObject);
    procedure StepOver             (Sender: TObject);
    procedure RunOrResume          (Sender: TObject);
    Procedure RunProgram           (Line: Word);
    Function  GetFirstLineNum:     Integer;
    procedure DoSingleStep         (Sender: TObject);
    Procedure UpdateRuntimeButtons;
    Procedure FindAndActivateLine  (LineNumber, Statement: Integer);
    Procedure ClearDebugPoints;
    Function  FindNextForward      (Term: AnsiString; Pos: Integer; MatchCase, WholeWord: Boolean): TFindResult;
    Function  FindNextBackward     (Term: AnsiString; Pos: Integer; MatchCase, WholeWord: Boolean): TFindResult;
    Function  GetEditorState: String;
    Procedure AdjustCursorPoint;
    Procedure AddUndo;
    Procedure AddRedo;
    Procedure PerformUndo;
    Procedure PerformRedo;
    Function  GetUndoNumber        (var Item: String): Integer;
    Function  LineExists           (LineNumber: Integer): Integer;
    Function  GetSourcePos         (var LineNumber, Statement: Integer): Integer;
    Function  GetSourceLine        (LineNum, Statement: Integer): String;
    Procedure SetDark;
    Procedure DoMessages;
    Procedure SetSourceMarker      (Index: Integer);
    Procedure GetSourceMarker      (Index: Integer);
    Procedure ClearSourceMarkers;

    Procedure FileIsDropped(Var Msg: TMessage); Message WM_DropFiles;

  end;

var
  MyMsg: Cardinal;
  BASinOutput: TBASinOutput;
  MRUList: TStringlist;
  StartingUp: Boolean;
  ScaleXDIB, AVGEffectDIB, CharDIB: TFastDIB;
  AppClosing: Boolean = False;
  ForceScaleUpdate: Boolean = True;
  SysTime: DWord;
  TBNormalImages: TImageList;
  OSIsXP: Boolean;
  IdIsName: Boolean;
  OSIsNT: Boolean;
  SkipThisFrame: Boolean;
  TFSpecDark: TFColor;
  TFSpecDarkA: TFColorA;
  LastWord: String;
  LatestWord: String;
  WordOffset: Integer;    //used for indenting
  WordStack: String;      //used for indent stack
  EditorSelAnchor,
  EditorSelLength: Integer;
  ParseResult: String;
  ParseError: TParseError;

const

  HH_DISPLAY_TOPIC        = $0000;
  HH_DISPLAY_TOC          = $0001;
  HH_CLOSE_ALL            = $0012;

  Function  HtmlHelp(hwndCaller: HWND; pszFile: PChar; uCommand: UINT; dwData: DWORD): HWND; stdcall; external 'HHCTRL.OCX' name 'HtmlHelpA';

  Procedure SmallTextOut(Bmp: TFastDIB; Text: String; X, Y: Integer; Clr: TFColorA);
  Function  GetPredictiveText(CurWord: String; Context: Integer): String;
  Procedure DrawChar(DIB: TFastDIB; CharPtr: PByte; X, Y: Integer; Ink, Paper, Bright: Byte);
  Procedure NewWord(DIB: TFastDIB; CurWord: String; XPos, YPos, Ink, Paper, Bright: Integer; LineBreak, REMLine, InString, DoPaint: Boolean; CurPos: Integer);
  Function  EditorSelStart: DWord;
  Function  EditorSelEnd: DWord;
  Function  IsNumber(Text: String): Boolean;
  Procedure ControlEmulation(Start: Boolean);
  Procedure SetGOTOPoint(Line, Statement: DWord);
  Function  CompareStrToSubStr(Const S1, S2: String; SubStart2, SubLen2: DWord): Boolean;

implementation

{$R *.DFM}
{$Z4}

Uses FastCore, InputUtils, Filing, BASSupport,  EvaluateBox, QueryForm,
     Evaluate, Breakpoints, LogWind, Watches, VarsWindow, SysVars, HexEdit, About, CommandHistory,
     FindWindow, ReplaceWindow, ErrorWindow, TokenWindow, Tapes, MemMap, UDGEdit, RenumWindow,
     BinaryForm, Compiler, Options, Display, GOSUB, Sound, BEEP, ErrorDescs, AddCode, SpecialVarEdit,
     BreakpointProperties, WatchProps, MessageBox, MemBlockAdd, ColoursWind, UDGOptions,
     PrinterOutput, Printing, Profiling, ProfilingForm, ProgInfo, CPUDisplay, AsmEditor,
     BlockProps, AsmForm, GrabParms, GridSetup, BinaryGrab, PaintBox, Binaries,
     MemManager, UlaColours, basinet, notes; // basinet

Procedure ControlEmulation(Start: Boolean);
Begin
  If Start Then Begin
     If Not Registers.EmuRunning Then
        DisplayWindow.WantsFront := True;
     Registers.EmuRunning := True;
     SkipThisFrame := False;
     If SecondaryBuffer <> nil Then Begin
        ResetSound;
        SecondaryBuffer.Play(0, 0, DSBPLAY_LOOPING);
     End;
     If WorkerThread <> nil Then
        ResumeWorkerThread;
     QueryPerformanceCounter(LastSynchPos);
     LastSynchError := 0;
     With CPUWindow Do Begin
        Button5.Caption := 'Stop';
        Button4.Enabled := False;
        Button2.Enabled := False;
        Button3.Enabled := False;
        Button6.Enabled := False;
     End;
  End Else Begin
     If SecondaryBuffer <> nil Then Begin
        SecondaryBuffer.Stop;
        ResetSound;
     End;
     Registers.EmuRunning := False;
     DisplayWindow.WantsFront := False;
     If WorkerThread <> nil Then
        SuspendWorkerThread;
     VariablesWindow.Button1.Enabled := False;
     If SpecialVarsWindow.Visible Then
        SpecialVarsWindow.Close;
     With CPUWindow Do Begin
        Button5.Caption := 'Run';
        Button4.Enabled := True;
        Button2.Enabled := True;
        Button3.Enabled := True;
        Button6.Enabled := True;
     End;
  End;
End;

function TBASinOutput.AppKeyDownHook(var Msg: TMessage): Boolean;
begin
  Result := Msg.Msg = Cm_AppKeyDown
end;

Function CompareStrToSubStr(Const S1, S2: String; SubStart2, SubLen2: DWord): Boolean;
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

Procedure TBASinOutput.IdleProc(Sender: TObject; var Done: Boolean);
Begin

  // The main loop - where all the emulation happens.
  // Has two modes. First is regular spectrum mode and the other (fastmode = true)
  // is running at full tilt for effects such as smooth listing and editing.

  // Only emulate if a ROM trap is not in effect, and the emulation is running.

  If Not Trapped and Registers.EmuRunning Then Begin
     If FastMode Then Begin
        // Full tilt mode - maximum CPU usage, use sparingly.
        ExecuteEmulationLoop_FullSpeed;


     End Else Begin
        // Regular speccy mode - uses very little CPU.
        ExecuteEmulationloop_SpectrumSpeed;
        If Not SoundAvailable Then Begin
           // No Sound, so we have to
           // time a frame - 20 ms is one frame at 50hz
           While GetTickCount - SysTime < 19 Do Begin
              // Just sleep for the remainder of the frame.
              Sleep(1);
           End;
           // Set the end of this frame as the start of the next.
           SysTime := GetTickCount;
        End;
     End;
     If StepOperation Then Begin
        CPUWindow.CPURunning := Registers.EmuRunning;
        Registers.EmuRunning := False;
        If CPUWindow.Showing Then
           CPUWindow.FormShow(nil);
        ShowWindow(CPUWindow, False);
        Exit;
     End;
     // Interrupt handling happens now, and frame update if the autoskip isn't taking care of it.
     ExecuteEndOfFrame;
     If Opt_AutoFrameSkip Then Begin
        If FastMode or FullSpeed Then
           Sleep(1);
     End Else Begin
        Dec(FrameCounter);
        If FrameCounter <= 0 Then Begin
           If DisplayWindow.Showing Then Begin
              UpdateDisplay;
              UpdateBASinDisplay;
           End;
           NeedDisplayUpdate := False;
           FrameCounter := opt_FrameSkip;
        End Else
           NeedDisplayUpdate := False;
     End;
     // Update any info that may have changed
     If MemMapWindow.Visible Then MemMapWindow.DrawMemoryMap;
     Done := False;
  End Else
     // If we're trapped, then this proc should not be called again, to let system
     // messages be processed. If we're emulating, then we need to keep going to maintain
     // smoothness.
     Done := True;
End;

procedure TBASinOutput.FormShow(Sender: TObject);
begin

  DumpDoneQuit:=false;


  NeedDisplayUpdate := True;
  InitWorkerThread;

  Evaluating := False;
  Filename := BASinDir + '\48.rom';
  If Not LoadROM(EvalMemory) Then Begin
     Timer1.Enabled := False;
     Timer2.Enabled := False;
     AppClosing := True;
     BASinOutput.Left := Screen.Width + 1000;
     PostMessage(BASinOutput.Handle, WM_QUIT, 0, 0);
     Abort := True;
     Exit;
  End;

  If Not FileExists(BASinDir+'\basinC.bin') Then Begin
     Timer1.Enabled := False;
     Timer2.Enabled := False;
     AppClosing := True;
     Windows.MessageBox(BASinOutput.Handle, 'A file named '#39'basinC.bin'#39' could not be opened.'#13'BasinC cannot run without this file,'#13'and will now close.', PChar('Missing BASin File'), MB_OK or MB_ICONWARNING);
     BASinOutput.Left := Screen.Width + 1000;
     PostMessage(BASinOutput.Handle, WM_QUIT, 0, 0);
     Abort := True;
     Exit;
  End;

  CharDIB := TFastDIB.Create;
  CharDIB.SetSize(8, 8, 32);

  SetToolBarButtons;

  WantNewLine := False;
  OnHint(Nil);
  DoubleBuffered := True;
  Application.OnActivate := FormActivate;
  UpdateRunTimeButtons;

  Application.CreateForm(TUDGGrabWindow, UDGGrabWindow);
  Application.CreateForm(TEvaluationWindow, EvaluationWindow);
  Application.CreateForm(TQueryWindow, QueryWindow);
  Application.CreateForm(TBreakpointsWindow, BreakpointsWindow);
  Application.CreateForm(TBPProperties, BPProperties);
  Application.CreateForm(TLogWindow, LogWindow);
  Application.CreateForm(TWatchWindow, WatchWindow);
  Application.CreateForm(TWatchProperties, WatchProperties);
  Application.CreateForm(TVariablesWindow, VariablesWindow);
  Application.CreateForm(TSpecialVarsWindow, SpecialVarsWindow);
  Application.CreateForm(TSysVarsWindow, SysVarsWindow);
  Application.CreateForm(THexWindow, HexWindow);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TCommandWindow, CommandWindow);
  Application.CreateForm(TFindForm, FindForm);
  Application.CreateForm(TReplaceForm, ReplaceForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.CreateForm(TErrorForm, ErrorForm);
  Application.CreateForm(TTokenForm, TokenForm);
  Application.CreateForm(TTapeWindow, TapeWindow);
  Application.CreateForm(TMemBlockWindow, MemBlockWindow);
  Application.CreateForm(TMemMapWindow, MemMapWindow);
  Application.CreateForm(TUDGWindow, UDGWindow);
  Application.CreateForm(TUDGNew, UDGNew);
  Application.CreateForm(TRenumberForm, RenumberForm);
  Application.CreateForm(TBinaryWindow, BinaryWindow);
  Application.CreateForm(TOptionsWindow, OptionsWindow);
  Application.CreateForm(TAddCodeWindow, AddCodeWindow);
  Application.CreateForm(TColoursWindow, ColoursWindow);
  Application.CreateForm(TGOSUBWindow, GOSUBWindow);
  Application.CreateForm(TBEEPWindow, BEEPWindow);
  Application.CreateForm(TErrorDescriptions, ErrorDescriptions);
  Application.CreateForm(TBlockProperties, BlockProperties);
  Application.CreateForm(TPrinterForm, PrinterForm);
  Application.CreateForm(TPrintForm, PrintForm);
  Application.CreateForm(TProfileForm, ProfileForm);
  Application.CreateForm(TProgInfoForm, ProgInfoForm);
  Application.CreateForm(TCPUWindow, CPUWindow);
  Application.CreateForm(TAsmEditorWindow, AsmEditorWindow);
  Application.CreateForm(TAssembleForm, AssembleForm);
  Application.CreateForm(TGridSetUpWindow, GridSetUpWindow);
  Application.CreateForm(TMemManagerForm, MemManagerForm);
  Application.CreateForm(TConsoleOutForm, ConsoleOutForm);
  Application.CreateForm(TUlaColoursWindow, UlaColoursWindow);
  Application.CreateForm(TBasinetWindow, BasinetWindow);
  //DebugLog('Create Windows - AnimPreviewWindow');
  Application.CreateForm(TAnimPreviewWindow, AnimPreviewWindow);

  SysTime := GetTickCount;

  Application.OnIdle := IdleProc;
  Application.OnHint := OnHint;
  Application.OnShowHint := OnShowHint;

  INITParser;
  If INITEmulation Then Begin;

     LoadOptions;

     INITSound;

     Opt_ShowingSyntax := Not Opt_ShowingSyntax;
     MenuItemClick(SyntaxHelper1);
     SetRuler;

     If Opt_AutoLoadSession Then Begin
        RestoreEmulationState(BASinDir+'\Session.bin');
        Memory[FLAGX] := Memory[FLAGX] and 223;
        Running := False;
        BufferKey(0, 13);
        BufferKey(1, 13);
        ControlEmulation(False);
     End Else
        ControlEmulation(True);

     SetCaption;
     EditorGFXMode := False;
     PageMove := False;
     InMenu := False;
     RepaintBASIC(True);

     BracketLevel := 0;
     StartingUp := False;
     InsertMode := True;

     DisplayWindow.Tag := 3;

     //arda commandline >1.69

     if (ParamStr(1)<>'') then Begin
        LoadQuoteQuote(ParamStr(1));

     End;



     SetLanguage(Opt_Language);  //arda

     NotesWindow.Memo1.Lines.Clear;
     //end arda

     ShowWindows;

  End;

end;

Procedure TBASinOutput.ShowWindows;
Var
  Idx: Integer;
Begin

  For Idx := 1 to Application.ComponentCount -1 Do Begin
     If Application.Components[Idx] is TForm then Begin
        If (Application.Components[Idx] As TForm).Tag and 2 = 2 Then Begin
           (Application.Components[Idx] As TForm).Tag := (Application.Components[Idx] As TForm).Tag - 2;
           ShowWindow(Application.Components[Idx] As TForm, False);
        End;
     End;
  End;

End;

Procedure TBASinOutput.SetToolBarButtons;
Begin

  SpeedButton4.Glyph.Assign(Image2.Picture.Bitmap); // Run
  SpeedButton6.Glyph.Assign(Image3.Picture.Bitmap); // Break

end;

Procedure TBASinOutput.GetOSVersion;
var
  verInfo : TOSVERSIONINFO;
begin
  verInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
	OSIsNT := False;
  if GetVersionEx(verInfo) then
		OSIsNT := verInfo.dwPlatformId = VER_PLATFORM_WIN32_NT;
    OSIsXP := (verInfo.dwMajorVersion > 5) or ((verInfo.dwMajorVersion = 5) and (verInfo.dwMinorVersion > 0));
End;



procedure TBASinOutput.FormResize(Sender: TObject);
begin

  If AppClosing Then Exit;

  ClearSoundBuffers;

  StatusBar1.Panels[0].Width := 25;
  Panel2.Visible := Opt_ShowingSyntax;

  StatusBar1.Visible := Opt_ShowStatusBar;

  ScrollBox1.Align := AlNone;
  RulerIMG.Align := AlNone;
  Panel2.Align := AlNone;
  Bevel5.Align := AlNone;

  Bevel5.Align := AlBottom;
  Panel2.Align := AlBottom;
  RulerIMG.Align := AlBottom;
  ScrollBox1.Align := AlClient;

  StatusBar1.Panels[2].Width := Canvas.TextWidth('INS')+50;
  StatusBar1.Panels[1].Width := StatusBar1.Width - StatusBar1.Panels[0].Width - StatusBar1.Panels[2].Width;

  ColourLabel1.SetBounds(4, 3, Panel2.Width - 8, 16);
  ColourLabel2.SetBounds(4, 19, Panel2.Width - 8, 16);

  FastIMG1.Setbounds(ScrollBox1.Left +2, ScrollBox1.Top +2, ScrollBox1.ClientWidth, ScrollBox1.ClientHeight);
  If ScrollBox1.ClientHeight > ScrollBox1.VertScrollBar.Range Then ViewLine := 0;
  Label1.SetBounds(Bevel3.Left + Bevel3.Width + 8, (CoolBar1.Height Div 2) - (Label1.Height Div 2), CoolBar1.ClientWidth - Bevel3.Left - Bevel3.Width - 16, Label1.Height);
  RepaintBASIC(True);

  UpdateParseText;

end;

procedure TBASinOutput.MenuItemClick(Sender: TObject);
Var
  Token: Byte;
  Idx: Integer;
  TempKey: Word;
  Expr: TExpression;
  HelpKeyword, LastText, TempStr, BASIC: String;
  Result, RunningEmu, Done, InString: Boolean;
  NewLines: TStringlist;
  FStream: TFilestream;
  Parameters: String;
  lpos: integer; //arda's temp integer
begin
  If (opt_GraphicsMethod = gmAltGr) and Not (Sender Is TSpeedButton) Then Begin
     // A quirk of Delphi (or maybe Windows) is that if you press Alt-Gr and
     // a letter, any CTRL-ALT-? Menu shortcuts fire.
     // So test for Alt-Gr and Bail out with a graphics char if that's the case.
     If DWord(GetASyncKeyState(VK_RMENU)) <> 0 Then Begin
        Token := 0;
        Case (Sender As TComponent).Tag Of
           12: Token := Ord('C');
           22: Token := Ord('F');
           30: Token := Ord('S');
           31: Token := Ord('P');
           33: Token := Ord('G');
           55: Token := Ord('L');
           56: Token := Ord('M');
           61: Token := Ord('N');
           86: Token := Ord('R');
        End;
        If Not Registers.EmuRunning Then Begin
           If Token <> 0 Then
              PerformKeyDown(Token, [], True);
        End Else Begin
           If Token <> 0 Then Begin
              CurKeyDown := Token;
              BufferToken(Token + $4F);
           End;
        End;
        Exit;
     End;
  End;

  Case (Sender As TComponent).Tag Of
     1:
        Begin // File-New
           If CheckForSave Then Begin
              If Not Registers.EmuRunning Then ControlEmulation(True);
              SaveEmulationState(UndoState);
              NotesWindow.Memo1.Lines.Clear; //arda. I can't remember if this is ok here.
               //FASTMode := True; //arda

              Reset;

           End;
        End;
     2:
        Begin // LOAD "" (Open...)
           If Not Registers.EmuRunning Then Begin
              TapeTrapLoad := False;
              LOADQuoteQuote('');


           End;
        End;
     3..10:
        Begin // MRU Items. LOAD "" them.
           LOADQuoteQuote(MRUList[(Sender As TComponent).Tag-3]);
        End;
     11:
        Begin // Assembler
           ShowWindow(AsmEditorWindow, False);
        End;
     12:
        Begin // CPU Window
           CPUWindow.CPURunning := Registers.EmuRunning;
           ShowWindow(CPUWindow, False);
        End;
     13:
        Begin // Save As...
           If ProgStateFlag <> PS_RESET Then SaveCurrentProgram('');
        End;
     14:
        Begin // Print...
           PrintForm.ComboBox1.ItemIndex := 1;
           CentreFormOnForm(PrintForm, nil);
           ShowWindow(PrintForm, True);
        End;
     15:
        Begin // File-Exit
           Close;
        End;
     16:
        Begin // Undo
           If UndoList.Count > 0 Then PerformUndo;
        End;
     17:
        Begin // Cut
           TempStr := Copy(BASICMem, EditorSelStart, (EditorSelEnd - EditorSelStart));
           AddUndo;
           BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
           UpdateCursorPos(EditorSelStart, False);
           ShowingPrediction := False;
           BASICChanged := True;
           UpdateParseText;
           RepaintBASIC(False);
           MakeCursorVisible;
           ClipBoard.SetTextBuf(PChar(TempStr));
        End;
     18:
        Begin // Copy
           TempStr := InsertEscapes(Copy(BASICMem, EditorSelStart, (EditorSelEnd - EditorSelStart)));
           ClipBoard.SetTextBuf(PChar(TempStr));
        End;
     19:
        Begin // Paste
           If Running or Registers.EmuRunning or Not Focused Then Exit;
           If ClipBoard.HasFormat(CF_TEXT) Then Begin
              TempStr := FormatEscapes(ClipBoard.AsText);
              AddUndo;
              If EditorSelStart <> EditorSelEnd Then Begin // Paste over selection
                 BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
              End;
              ShowingPrediction := False;
              If Pos(#13, TempStr) <> 0 Then Begin // Multiline paste?
                 AddCodeWindow.ClearCode;
                 NewLines := TStringlist.Create;
                 Repeat
                    BASIC := Copy(TempStr, 1, Pos(#13, TempStr)-1);
                    TempStr :=  Copy(TempStr, Pos(#13, TempStr)+1, 999999);
                    If BASIC <> '' Then
                       NewLines.Add(InsertEscapes(BASIC));
                 Until Pos(#13, TempStr) = 0;
                 If TempStr <> '' Then
                    NewLines.Add(TempStr);
                 AddCodeWindow.AddCode(NewLines);
                 NewLines.Free;
                 CentreFormOnForm(AddCodeWindow, Self);
                 ShowWindow(AddCodeWindow, True);
              End Else Begin
                 BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+FormatEscapes(TempStr)+Copy(BASICMem, EditorSelStart, 999999);
                 UpdateCursorPos(EditorSelStart + DWord(Length(TempStr)), False);
              End;
              BASICChanged := True;
              UpdateParseText;
              RepaintBASIC(False);
              MakeCursorVisible;
           End;
        End;
     20:
        Begin // Delete
           AddUndo;
           BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
           UpdateCursorPos(EditorSelStart, False);
           ShowingPrediction := False;
           BASICChanged := True;
           UpdateParseText;
           RepaintBASIC(False);
           MakeCursorVisible;
        End;
     21:
        Begin // Find...
           CentreFormOnForm(FindForm, Self);
           FindForm.FindType := ftBASIC;
           ShowWindow(FindForm, True);
        End;
     22:
        Begin // Find Next
           FindForm.FindType := ftBASIC;
           FindForm.DoFind(nil);
        End;
     23:
        Begin // Replace...
           ReplaceForm.ReplaceType := rtBASIC;
           CentreFormOnForm(ReplaceForm, Self);
           ShowWindow(ReplaceForm, True);
        End;
     24:
        Begin // EDIT Line..
           Done := False;
           RunningEmu := Registers.EmuRunning;
           ControlEmulation(False);
           CentreFormOnForm(QueryWindow, BASinOutput);
           While Not Done Do Begin
              QueryWindow.GetQuery('Go To Line...', 'Line No. to Edit:', 'Okay', 'Cancel', [LastText]);
              If QueryWindow.ResultText <> '' Then Begin
                 Expr.Expression := QueryWindow.ResultText;
                 Expr.SyntaxChecked := False;
                 EvaluateExpr(Expr);
                 LastText := '';
                 If QueryWindow.Edit1.Text <> '' Then
                    LastText := QueryWindow.Edit1.Text
                 Else If QueryWindow.ComboBox1.Text <> '' Then
                    LastText := QueryWindow.ComboBox1.Text;
                 Case Expr.ResultType of
                    0: Begin
                          Windows.MessageBox(Handle, pChar('Line Number must be numeric'), pChar('Line Number error'), MB_OK or MB_ICONWARNING);
                       End;
                    1: Begin
                          Done := True;
                          RunningEmu := True;
                          FindAndActivateLine(Round(Expr.ResultNum), 1);
                       End;
                    2: Begin
                          Windows.MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 999999)), pChar('Line Number error'), MB_OK or MB_ICONWARNING);
                       End;
                 End;
              End Else
                 Done := True;
           End;
           ControlEmulation(RunningEmu);
        End;
     25:
        Begin // Goto Error
           FindAndActivateLine(LastErrorLine, LastErrorStatement);
        End;
     26:
        Begin // Syntax Helper
           Opt_ShowingSyntax := Not Opt_ShowingSyntax;
           SetSyntaxHelper;
        End;
     27:
        Begin // Display Window
           If DisplayWindow.Visible Then
              DisplayWindow.Hide
           Else
              ShowWindow(DisplayWindow, False);
        End;
     28:
        Begin // Command History;
           ShowWindow(CommandWindow, False);
        End;
     29:
        Begin // Variables Window
           ShowWindow(VariablesWindow, False);
        End;
     30:
        Begin // System Variables Window
           ShowWindow(SysVarsWindow, False);
        End;
     31:
        Begin // Breakpoints Window
           ShowWindow(BreakPointsWindow, False);
        End;
     32:
        Begin // Watches Window
           ShowWindow(WatchWindow, False);
        End;
     33:
        Begin // GOSUB Stack Window
           ShowWindow(GOSUBWindow, False);
        End;
     34:
        Begin // RUN
           RunOrResume(Nil);
        End;
     35:
        Begin // Wordwrap by splitting
           WordWrap(StringStart, StringLen, 0);
        End;
     36:
        Begin // GO TO...
           GOTOLine := CursLineNum;
           GOTOStatement := CursStatementNum;
           If Not Running Then
              SetGOTOPoint(GOTOLine, GOTOStatement);
           If Not Registers.EmuRunning Then
              RunOrResume(Nil);
        End;
     37:
        Begin // Single Step
           DoSingleStep(Nil);
        End;
     38:
        Begin // Next Line
           StepOver(Nil);
        End;
     39:
        Begin // Run To
           RunLine := CursLineNum;
           RunStatement := CursStatementNum;
           RunProgram(65535);
        End;
     40:
        Begin // Program Information
           CentreFormOnForm(ProgInfoForm, nil);
           ShowWindow(ProgInfoForm, True);
        End;
     41:
        Begin // Evaluation Calculator
           CentreFormOnForm(EvaluationWindow, Self);
           ShowWindow(EvaluationWindow, False);
        End;
     42:
        Begin // Toggle Breakpoint
           BreakpointsWindow.ToggleBreakpoint(CursLineNum, CursStatementNum);
        End;
     43:
        Begin // Add Watch
           WatchWindow.AddWatch1Click(nil);
        End;
     44:
        Begin // Options
           CentreForm(OptionsWindow, Screen.Width Div 2, Screen.Height Div 2);
           ShowWindow(OptionsWindow, True);
           SetSyntaxHelper;
        End;
     45:
        Begin // Token Table
           ShowWindow(TokenForm, False);
        End;
     46:
        Begin // Wordwrap by inserting spaces
           WordWrap(StringStart, StringLen, 1);
        End;
     47:
        Begin // UDG Editor
           ShowWindow(UDGWindow, False);
        End;
     48:
        Begin // Memory grabber
           CentreFormOnForm(BinaryGrabWindow, Self);
           ShowWindow(BinaryGrabWindow, True);
           If Not BinaryGrabWindow.Cancelled Then Begin
              BinaryWindow.ClearBinaries;
              TempStr := '';
              For Idx := BinaryGrabWindow.BlockAddress To BinaryGrabWindow.BlockAddress + BinaryGrabWindow.BlockSize -1 Do
                 TempStr := TempStr + Chr(Memory[Idx]);
              BinaryWindow.AddBinary('Memory Block ('+IntToStr(BinaryGrabWindow.BlockAddress)+', '+IntToStr(BinaryGrabWindow.BlockSize)+')', TempStr);
              CentreFormOnForm(BinaryWindow, Self);
              ShowWindow(BinaryWindow, True);
           End;
        End;
     49:
        Begin // Renumber
           CentreFormOnForm(RenumberForm, Self);
           ShowWindow(RenumberForm, True);
        End;
     50:
        Begin // BASin Help
           HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/index.html'), HH_DISPLAY_TOPIC, 0);
        End;
     51, 77:
        Begin // Command Help
           If Sender <> Help2 Then
              PopUpMenu1PopUp(nil);
           If Help2.Visible Then Begin
              HelpKeyword := AsciiKeywords[Token1.ImageIndex];
              If HelpKeyword = 'VAL$' Then
                 HelpKeyword := 'VAL_';
              If Copy(HelpKeyword, Length(HelpKeyword), 1) = '$' Then
                 HelpKeyword := Copy(HelpKeyword, 1, Length(HelpKeyword) -1);
              While Pos(' ', HelpKeyword) <> 0 Do
                 HelpKeyword := Copy(HelpKeyword, 1, Pos(' ', HelpKeyword) -1) + Copy(HelpKeyword, Pos(' ', HelpKeyword)+1, 999999);
              HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/kwd_'+lowercase(HelpKeyword)+'.html'), HH_DISPLAY_TOPIC, 0);
           End;
        End;
     52:
        Begin // BASIC Manual (Help)
           HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/manual_contents.html'), HH_DISPLAY_TOPIC, 0);
        End;
     53:
        Begin // About Box
           CentreForm(AboutBox, Screen.Width Div 2, Screen.Height Div 2);
           ShowWindow(AboutBox, True);
        End;
     54:
        Begin // Show/Hide the Statusbar
           Opt_ShowStatusBar := Not Opt_ShowStatusBar;
           SetRuler;
        End;
     55:
        Begin // Log Window
           CentreForm(LogWindow, Screen.Width Div 2, Screen.Height Div 2);
           ShowWindow(LogWindow, False);
        End;
     56:
        Begin // Memory Viewer
           HexWindow.GetMemory(0, 65536, 'Edit Memory');
           ShowWindow(HexWindow, False);
        End;
     57:
        Begin // Force BREAK (Undo)
           LoadEmulationState(BREAKState, True);
           If Registers.PC = $805 Then Begin
              PutWord(@Memory[NEWPPC],$FFFF);
              Memory[NSPPC] := $FF;
           End;
           Running := False;
           NeedParseUpdate := True;
           UpdateRuntimeButtons;
           If SysVarsWindow.Visible Then
              SysVarsWindow.UpdateSysVars(0);
           If WatchWindow.Visible Then
              UpdateWatches;
           If VariablesWindow.Visible Then
              VariablesWindow.BuildVarsList;
           If HexWindow.Visible Then
              HexWindow.GetMemory(0, 65536, HexWindow.Caption);
           PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
           TempKey := VK_END;
           DisplayWindow.FormKeyDown(Self, TempKey, []);
           Registers.EmuRunning:=False;

        End;
     58:
        Begin // Replace Again
           ReplaceForm.DoReplace(nil);
        End;
     59:
        Begin // View Last Error
           ErrorForm.ShowError(False);
        End;
     60:
        Begin // Reload the autosaved session.bin file.
           RestoreEmulationState(BASinDir+'\Session.bin');
        End;
     61:
        Begin // Memory Map Window
           ShowWindow(MemMapWindow, False);
        End;
     62:
        Begin // Tape Builder Window
           ShowWindow(TapeWindow, False);
        End;
     63:
        Begin // Save...

            if (trim(Copy(ExtractFilename(CurProjectFileName),1,8))='autoback') Then Begin
            SaveCurrentProgram('');
            End Else Begin

                If Not ProjectSaved Then
                        SaveCurrentProgram('')
                Else
                        SaveCurrentProgram(CurProjectFileName);
            End;
        End;
     64:
        Begin // 100% Window Size
           DisplayWindow.Show;
           DisplayWindow.SizeForm(100);
        End;
     65:
        Begin // 200% Window Size
           DisplayWindow.Show;
           DisplayWindow.SizeForm(200);
        End;
     66:
        Begin // Force Aspect Ratio
           Opt_MaintainAspect := Not Opt_MaintainAspect;
           DisplayWindow.FormResize(Self);
        End;
     67:
        Begin // Copy entire listing to clipboard
           Idx := 1;
           BASIC := '';
           TempStr := '';
           InString := False;
           While Idx <= Length(BASICMem) Do Begin
              If BASICMem[Idx] = '"' Then InString := Not InString;
              If Not InString and (BASICMem[Idx] = #13) Then Begin
                 TempStr := InsertEscapes(TempStr);
                 BASIC := BASIC + TempStr  + #13#10;
                 TempStr := '';
              End Else Begin
                 TempStr := TempStr + BASICMem[Idx];
              End;
              Inc(Idx);
           End;
           ClipBoard.SetTextBuf(pChar(BASIC));
        End;
     68:
        Begin // Import BINARY file
           Filename := OpenFile(Self.Handle, 'Open BINARY File', [FTAll], '', False, True);
           If Filename <> '' Then Begin
              NewLines := TStringlist.Create;
              If Pos(#0, Filename) > 0 Then Begin
                 TempStr := Copy(Filename, 1, Pos(#0, Filename)-1);
                 Filename := Copy(Filename, Pos(#0, Filename) +1, 999999);
                 While Filename <> '' Do Begin
                    NewLines.Add(TempStr + '\' + Copy(Filename, 1, Pos(#0, Filename)-1));
                    Filename := Copy(Filename, Pos(#0, Filename) +1, 999999);
                 End;
              End Else
                 NewLines.Add(Filename);
              BinaryWindow.ClearBinaries;
              For Idx := 0 To NewLines.Count -1 Do Begin
                 Filename := NewLines[Idx];
                 If OpenFileStream(FStream, fmOpenRead or fmShareDenyNone, Filename) Then Begin
                    SetLength(TempStr, FStream.Size);
                    FStream.Read(TempStr[1], FStream.Size);
                    FStream.Free;
                    BinaryWindow.AddBinary(Filename, TempStr);
                 End;
              End;
              If BinaryWindow.BinaryFiles.Count > 0 Then Begin
                 CentreFormOnForm(BinaryWindow, Self);
                 ShowWindow(BinaryWindow, True);
              End;
           End;
        End;
     69:
        Begin // Compiler
           Compile;
        End;
     70:
        Begin // Continue
           DoContinue;
        End;
     71:
        Begin // Redo
           If RedoList.Count > 0 Then
           Begin

           PerformRedo;
           End;
        End;
     72:
        Begin // Trace program execution
           Opt_FollowProgram := Not Opt_FollowProgram;
        End;
     73:
        Begin // Character Ruler
           Opt_CharacterRuler := Not Opt_CharacterRuler;
           SetRuler;
        End;
     74:
        Begin // Add Breakpoint
           BreakpointsWindow.AddBreakPoint1Click(nil);
        End;
     75:
        Begin // BEEP Composer
           CentreFormOnForm(BeepWindow, nil);
           ShowWindow(BEEPWindow, False);
        End;
     76:
        Begin // Error Help
           CentreFormOnForm(ErrorDescriptions, nil);
           ShowWindow(ErrorDescriptions, False);
        End;
     78:
        Begin // Edit Variable
           VariablesWindow.Button1Click(nil);
        End;
     112:
        Begin // Add note
        //NotesWindow.Memo1.Text:= NotesWindow.Memo1.Text + '/r/n' +Token1.Caption;
          lpos:=Pos(IntToStr(CursLineNum) + ':' + IntToStr(CursStatementNum) +' ' + stringreplace(Token1.Caption,'&','',[rfReplaceAll, rfIgnoreCase]), NotesWindow.Memo1.Text);
          if (lpos > 0) Then Begin
                NotesWindow.Memo1.SelStart:= lpos-1;
                NotesWindow.Memo1.SelLength := length(IntToStr(CursLineNum) + ':' + IntToStr(CursStatementNum) +' ' + stringreplace(Token1.Caption,'&','',[rfReplaceAll, rfIgnoreCase]));

          End Else Begin
                NotesWindow.Memo1.Lines.Add(IntToStr(CursLineNum) + ':' + IntToStr(CursStatementNum) +' ' + stringreplace(Token1.Caption,'&','',[rfReplaceAll, rfIgnoreCase])+ ' ');
                NotesWindow.Memo1.SelStart:= Length(NotesWindow.Memo1.Text)-2;
                NotesWindow.Memo1.SelLength:=0;
                End;

          ShowWindow(NotesWindow, False);
        End;
     113:
        Begin  // Add Snippet

          BasinetWindow.ClearSnippet;

          TempStr := InsertEscapes(Copy(BASICMem, EditorSelStart, (EditorSelEnd - EditorSelStart)));
          BasinetWindow.Memo_RawCode.Lines.Clear;
          BasinetWindow.Memo_RawCode.Lines.Add(TempStr);

          TempStr :=  Copy(BASICMem, EditorSelStart, (EditorSelEnd - EditorSelStart));


         NewLines := TStringlist.Create;
	 Repeat
		BASIC := Copy(TempStr, 1, Pos(#13, TempStr)-1);
		TempStr :=  Copy(TempStr, Pos(#13, TempStr)+1, 999999);
		If BASIC <> '' Then
		   NewLines.Add(InsertEscapes(BASIC));
	 Until Pos(#13, TempStr) = 0;

	 If TempStr <> '' Then
		NewLines.Add(TempStr);
                
	 BasinetWindow.AddSnippet(NewLines);
         BasinetWindow.AddingSnippet;
	 NewLines.Free;

	 ShowWindow(BasinetWindow, False);

        End;
     79:
        Begin // Find Line
           If FindLine1.Caption = '&Find Line' Then
              FindAndActivateLine(FindLine1.ImageIndex, 1)
           Else
              FindAndActivateLine(FindLine1.ImageIndex, EditVariable1.ImageIndex);
        End;
     80:
        Begin // Toggle breakpoint
           BreakpointsWindow.ToggleBreakpoint(ToggleBreakpoint1.ImageIndex, RunToCursor1.ImageIndex);
        End;
     81:
        Begin // Run To Line
           RunLine := ToggleBreakPoint1.ImageIndex;
           RunStatement := RunToCursor1.ImageIndex;
           RunProgram(65535);
        End;
     82:
        Begin // GO TO Line
           GOTOLine := ToggleBreakPoint1.ImageIndex;
           GOTOStatement := RunToCursor1.ImageIndex;
           If Not Running Then
              SetGOTOPoint(GOTOLine, GOTOStatement);
           If Not Registers.EmuRunning Then
              RunOrResume(Nil);
        End;
     83:
        Begin // Watch Variable
           VariablesWindow.Button2Click(nil);
        End;
     84:
        Begin // Attach Tape File
           TapeWindow.FromFile1Click(Self);
        End;
     85:
        Begin // ZX Printer window
           ShowWindow(PrinterForm, False);
        End;
     86:
        Begin // Profile window
           ShowWindow(ProfileForm, False);
        End;
     87:
        Begin // Enable Profiling
           ProfilingEnabled := Not ProfilingEnabled;
        End;
     88:
        Begin // Clear Source Markers
           ClearSourceMarkers;
        End;
     89:
        Begin // Show or hide the Toolbar
           Opt_ShowToolbar := Not Opt_ShowToolBar;
           SetRuler;
        End;
     90..94:
        Begin // Set source marker
           SetSourceMarker((Sender as TMenuItem).Tag - 90);
        End;
     100..104:
        Begin // Jump to source marker
           GetSourceMarker((Sender as TMenuItem).Tag - 100);
        End;
     105:
        Begin // Tokenise a whole string
           TokeniseString;
        End;
     106:
        Begin // Tokenise a whole string
           DeTokeniseString;
        End;
     107:
        Begin // Screen Paintbox
           ScrPaintForm.Show;
//           ShowWindow(ScrPaintForm, False);
        End;

     108:
        Begin // Memory Manager
           MemManagerForm.Show;

        End;

     109:
        Begin // BasCloud
           BasinetWindow.ClearSnippet;
           ShowWindow(BasinetWindow, False)

        End;

     110:
        Begin // Ula Colours window
          CentreFormOnForm(UlaColoursWindow, Self);
          ShowWindow(UlaColoursWindow, False);

        End;

     111:
        Begin // My Memos - Project Notes
          ShowWindow(NotesWindow, False);

        End;

        //112 add note
        //113 add snipped --added upper part of the list due to priority
     119:
        Begin //Export Tap
        TapeWindow.ExportTape;

        {***


        ***}
        End;

      120:
        Begin //Export to external
         if fileexists(Opt_ExternalExec) Then
         Begin

           Filename := BASinDir + '\BasincEx.sna';
           SaveProgram;

           Parameters := GetShortName(BASinDir + '\BasincEx.sna');
           ShellExecute(handle,'open',PChar(Opt_ExternalExec), PChar(Parameters),'',SW_SHOWNORMAL);
         End Else Begin
            //show an error
            Result := MessageDlg('You haven`t set any external utiliy in the options window or it doesn`t exist.'+#13+#13+'Please set one in Options > Files > External Utility section.', mtWarning, [mbOK], 0) = mrOK;

         End;
        End;

        121:
        Begin //show-hide icons
        Opt_Controlicons := Not Opt_Controlicons;
        SetRuler;

        End;
  End;

end;

function GetShortName(sLongName: string): string;
var
  sShortName    : string;
  nShortNameLen : integer;
begin
  SetLength(sShortName, MAX_PATH);
  nShortNameLen := GetShortPathName(
    PChar(sLongName), PChar(sShortName), MAX_PATH - 1
  );
  if (0 = nShortNameLen) then
  begin
    // handle errors...
  end;
  SetLength(sShortName, nShortNameLen);
  Result := sShortName;
end;

Procedure TBASinOutput.SetSyntaxHelper;
Begin
  SyntaxHelper1.Checked := Opt_ShowingSyntax;
  Toolbar1.Checked := Opt_ShowToolBar;
  StatusBar2.Checked := Opt_ShowStatusBar;
  StatusBar1.BringToFront;
  NeedParseUpdate := True;
  Panel2.Visible := Opt_ShowingSyntax;
  If Opt_ShowingSyntax Then Begin
     Bevel5.Visible := Opt_ShowStatusBar;
     RulerIMG.Height := 15;
     If Not Opt_CharacterRuler Then
        ScrollBox1.Height := Panel2.Top - ScrollBox1.Top
     Else
        ScrollBox1.Height := RulerIMG.Top - ScrollBox1.Top;
  End Else Begin
     RulerIMG.Height := 19;
     If Not Opt_CharacterRuler Then Begin
        ScrollBox1.Height := StatusBar1.Top - ScrollBox1.Top;
        Bevel5.Visible := False;
     End Else Begin
        ScrollBox1.Height := RulerIMG.Top - ScrollBox1.Top;
        Bevel5.Visible := Opt_ShowStatusBar;
     End;
  End;
  UpdateParseText;
  FormResize(nil);
End;

procedure TBASinOutput.FormClose(Sender: TObject; var Action: TCloseAction);
Var
  State: TEmulationState;
begin
  If Not CheckForSave Then Begin
     Action := caNone;
     Exit;
  End Else Begin
     If Not AsmEditorWindow.ClearFiles Then Begin
        Action := caNone;
        Exit;
     End Else Begin
        AppClosing := True;
        PrintForm.Close;
        If Not Abort Then Begin
           SaveOptions;
           SaveEmulationState(State);
           StoreEmulationState(BASinDIR+'\Session.bin', State);
        End;
        CloseEmulation;
        DisplayWindow.Close;
        TapeWindow.ListView1.Items.Clear;
        TapeBlocks.Free;
        Action := caFree;
        ScaleXDib.Free;
        AVGEffectDIB.Free;
        MRUList.Free;
        ScrollBox1.Free;
        TBNormalImages.Free;
        BASICList.Free;
        UndoList.Free;
        RedoList.Free;
        CharDIB.Free;
        MS_48kClick.Free;
        MS_128kClick.Free;
        MS_Ok.Free;
        MS_Error.Free;
        StopProfile;
     End;
  End;
end;

procedure TBASinOutput.UpdateParseText;
Begin
  If Not Opt_ShowingSyntax then exit;
  ParseResult := '';
  If Editing and (Memory[FLAGX] and 32 = 0) and Not Running Then Begin
     If ProgStateFlag = PS_Reset Then Begin
        ParseResult := 'Reset in progress - '#16#1'Please Wait'#16#0;
        ParseError.Error := '';
     End Else If ProgStateFlag = PS_Stopped Then Begin
        ParseResult := 'Program Stopped. Any key to EDIT.';
        ProgStateFlag := PS_Unknown;
     End Else Begin
        If Not Registers.EmuRunning Then Begin
           ParseResult := GetCurrentEditorLine+' ';
           ParseError := ParseInputLine(ParseResult);
           If CodeError Then
              CursorType := '?'
           Else
              If EditorGFXMode Then
                 CursorType := 'G'
              Else
                 If ParseError.ErrorCode = 99 Then // Indeterminate type
                    CursorType := '!'
                 Else
                    If (ParseError.Error = 'Expecting Keyword.') or
                       (ParseError.Syntax = 'LineNumber ['#$10#1'Statement'#$10#1']') or
                       (Pos(#$10#1'Statement', ParseError.Syntax) <> 0) Then
                          CursorType := 'K'
                    Else
                       If DWord(GetKeyState(VK_CAPITAL) and 1) = 1 Then
                          CursorType := 'C'
                       Else
                          CursorType := 'L';
           StatusBar1.Repaint;
        End Else Begin
           ParseResult := GetCurrentStatement(GetEditLine);
           ParseResult := DetokeniseLine(ParseResult, False);
           If Memory[FLAGX] and 32 = 32 Then
              ParseError := ParseExprLine(ParseResult)
           Else
              ParseError := ParseInputLine(ParseResult);
        End;
        ParseResult := ParseError.Syntax;
     End;
  End Else Begin
     ParseResult := 'Program Running - '#16#4'Escape'#16#0' to BREAK';
     ParseError.ErrorCode := 0;
     ParseError.Error := '';
  End;
  If Opt_ShowingSyntax Then Begin
     ColourLabel1.Str := ParseResult;
     If ParseError.Error <> '' Then Begin
        If ParseError.ErrorCode = -1 Then Begin
           If ParseError.Error[Length(ParseError.Error)] = '.' Then
              Delete(ParseError.Error, Length(ParseError.Error), 1);
           ColourLabel2.Str := HighlightReserved(ParseError.Error+' in Statement '+IntToStr(ParseError.Statement), False);
        End Else Begin
           If ParseError.Error[Length(ParseError.Error)-1] = ',' Then
              Delete(ParseError.Error, Length(ParseError.Error) -1, 2);
           ColourLabel2.Str := HighlightReserved(ParseError.Error, False);
        End;
     End;
     ColourLabel1.Repaint;
     ColourLabel2.Repaint;
  End;
  NeedParseUpdate := False;
End;

Procedure TBASinOutput.OnShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
Var
  Idx, StartPos, EndPos, SavePos: Integer;
  TempStr, VarName: String;
  HitRect: TRect;
Begin
  CanShow := True;
  If HintInfo.HintControl = FastIMG1 Then Begin
     CanShow := False;
     If VariablesWindow.ListView1.Items.Count > 0 Then Begin
        StartPos := GetCharPos(MousePos.X, MousePos.Y);
        SavePos := StartPos;
        If BASICMem[StartPos] in ['$', '0'..'9', 'A'..'Z', 'a'..'z'] Then Begin
           While (StartPos > 1) and (BASICMem[StartPos] in ['$', '0'..'9', 'A'..'Z', 'a'..'z']) Do
              Dec(StartPos);
           Inc(StartPos);
           EndPos := StartPos;
           While (EndPos < Length(BASICMem)) and (BASICMem[EndPos] in ['$', '0'..'9', 'A'..'Z', 'a'..'z']) Do
              Inc(EndPos);
           VarName := Uppercase(Copy(BASICMem, StartPos, EndPos - StartPos));
           HitRect.Left := (MousePos.X Div (8*Opt_FontScale))*(8*Opt_FontScale)-((SavePos-StartPos)*Opt_FontScale);
           HitRect.Top := (MousePos.Y Div (8*Opt_FontScale))*(8*Opt_FontScale);
           HitRect.Right := HitRect.Left;
           HitRect.Bottom := HitRect.Top;
           Idx := 0;
           While Idx < VariablesWindow.ListView1.Items.Count Do Begin
              TempStr := Uppercase(VariablesWindow.ListView1.Items[Idx].Caption);
              If TempStr = VarName Then Break;
              Inc(Idx);
           End;
           If Idx < VariablesWindow.ListView1.Items.Count Then Begin
              HintStr := VariablesWindow.ListView1.Items[Idx].SubItems[0]+' '+VarName+'='+VariablesWindow.ListView1.Items[Idx].SubItems[1];
              If Length(HintStr) > 75 Then Begin
                 Idx := 75;
                 While Idx < Length(HintStr) Do Begin
                    HintStr := Copy(HintStr, 1, Idx)+#13+Copy(HintStr, Idx+1, 999999);
                    Inc(Idx, 75);
                 End;
              End;
              HintInfo.CursorRect := HitRect;
              CanShow := True;
           End;
        End;
     End;
  End;
End;

Procedure TBASinOutput.OnHint(Sender: TObject);
Var
  Atk, Acnt, Aen, Bytes, BytesUsed: Integer;
  Status: String;
label
loop10, loop20, loop60;
Begin
  If (Screen.ActiveForm = Self) or (Sender = Timer2) Then
     If Application.Hint <> '' Then
        StatusBar1.Panels[1].Text := ' '+Application.Hint
     Else Begin
        Bytes := GetWord(@Memory[RAMTOP])-GetWord(@Memory[STKEND]);
        BytesUsed := GetWord(@Memory[E_LINE])-GetWord(@Memory[PROG])-1;


        // Following code is just an experiment by arda
        Aen := GetWord(@Memory[PROG]);
        Acnt:=0;

loop10:
          Aen := Aen+4;
          Acnt:=Acnt+4;
loop20:
          if Aen>GetWord(@Memory[STKEND]) Then Begin
            Acnt:=Acnt-4;
            Goto loop60;
          End;

          Atk := GetByte(@Memory[Aen]);

          if Atk=226 Then Begin
                Acnt:=Acnt-4;
                Goto loop60;
          End;
          if Atk=14 Then Begin
              Aen:=Aen+6;
              Goto loop20;
          End;
          if Atk=13 Then Begin
              Aen:=Aen+1;
              Acnt:=Acnt+1;
              Goto loop10;
          End;
          Aen := Aen+1;
          Acnt := Acnt+1;
          Goto loop20;


loop60:
       //end of experiment



        If Bytes < 1024 Then
           Status := ' '+IntToStr(Abs(Bytes))+' Bytes available to BASIC ('
        Else
           Status := ' '+IntToStr(Bytes Div 1024)+' Kb available to BASIC (';

        //If BytesUsed < 1024 Then  //commented by arda
           Status := Status + IntToStr(Abs(BytesUsed)) + ' Total Bytes Used';
        //Else
           //Status := Status + IntToStr(BytesUsed Div 1024) + ' Kb Used)';

           Status := Status + ', '+ IntToStr(Abs(Acnt)) + ' Bytes in Listing';
        Status := Status +')';
        StatusBar1.Panels[1].Text := Status;
     End;
  StatusBar1.Panels[0].Width := 25;
End;

Procedure TBASinOutput.OnUpdateProg(var Message: TMessage);
Begin
  GetBASIC;
  If Message.WParam = 1 Then
     GenerateBASICChecksum(BASICChecksum);
  RepaintBASIC(True);
  FormResize(nil);
  MakeCursorVisible;
  UpdateRuntimeButtons;
  If BreakpointsWindow.Visible Then BreakpointsWindow.BuildBreakpointsList;
End;

Procedure TBASinOutput.OnUpdateProgCursor(var Message: TMessage);
Begin
  If Message.wParam <> 0 Then
     FindAndActivateLine(Message.wParam, Message.lParam and 255);
End;

Procedure TBASinOutput.OnUpdateProgButtons(var Message: TMessage);
Begin
  UpdateRuntimeButtons;
End;

Procedure TBASinOutput.OnUpdateVars(var Message: TMessage);
Begin
  If VariablesWindow.Visible Then
     VariablesWindow.BuildVarsList;
End;


procedure TBASinOutput.FormCreate(Sender: TObject);
Var
  Idx: Integer;
begin

 //arda new menu manual redrawing directive begin

   //Screen.MenuFont.Name := 'Arial Black';
   //MainMenu1.OwnerDraw:=True;
   //Screen.MenuFont.Size:=12;


   //init SimpleCON register so it can be displayed at editbox

   if ConsoleAddon[255]= 0 Then Begin
        for Idx:=1 to 255 do begin
            ConsoleAddon[Idx]:=ord(' ');
        end;
   End;

   ConsoleAddon[0]:=1; //set simpleCon index to 1, so it's ready.


 //arda  end

  Abort := False;

  GetOSVersion;


  RunningAck := Not Running;
  RunLine := 65536;
  GOTOStatement := 1;

  BASICList := TStringlist.Create;
  UndoList := TStringlist.Create;
  RedoList := TStringlist.Create;
  MRUList := TStringlist.Create;

  ViewLine := 0;
  ViewColumn := 0;
  CursOffset := 1;
  Panel1.visible := false;
  ScrollBox1 := TNewScrollBox.Create(Self);
  ScrollBox1.SetBounds(Panel1.Left, Panel1.Top, Panel1.ClientWidth, Panel1.ClientHeight);
  ScrollBox1.Align := AlClient;
  ScrollBox1.Parent := Self;
  ScrollBox1.AutoScroll := False;
  ScrollBox1.OnVerticalScroll := ScrollBox1VScroll;
  ScrollBox1.OnHorizontalScroll := ScrollBox1HScroll;

  ColourLabel1 := TColourLabel.Create(Panel2);
  ColourLabel1.AutoSize := False;
  ColourLabel1.SetBounds(4, 3, Panel2.Width - 8, 16);
  ColourLabel1.Parent := Panel2;
  ColourLabel1.Caption := '';
  ColourLabel1.Transparent := True;

  ColourLabel2 := TColourLabel.Create(Panel2);
  ColourLabel2.AutoSize := False;
  ColourLabel2.SetBounds(4, 19, Panel2.Width - 8, 16);
  ColourLabel2.Parent := Panel2;
  ColourLabel2.Caption := '';
  ColourLabel2.Transparent := True;

  StartingUp := True;

  For Idx := 0 to ControlCount -1 Do
     If Controls[Idx] is TWinControl Then
        (Controls[Idx] As TWinControl).DoubleBuffered := True;

  CreateEditorSounds;

  DragAcceptFiles(Handle, True);
  Application.HookMainWindow(AppKeyDownHook)

end;


Procedure TBASinOutput.FileIsDropped(Var Msg: TMessage);
Var
  hDrop: THandle;
  fName: Array[0..1024] of CHAR;
  LPos, CodeAddress, LoadLength: Integer;
  Name: String;
begin
  hDrop := Msg.WParam; Name := '';
  DragQueryFile(hDrop,0,fName,254);
  DragFinish(hDrop);
  DragAcceptFiles(Handle, True);

  Name := fName;
  If (Lowercase(ExtractFileExt(Name)) = '.bas') or
     (Lowercase(ExtractFileExt(Name)) = '.sna') or
     (Lowercase(ExtractFileExt(Name)) = '.tap') or
     (Lowercase(ExtractFileExt(Name)) = '.z80') then
     LoadQuoteQuote(Name);
  If Lowercase(ExtractFileExt(Name)) = '.bsc' then Begin
     Filename := Name;
     GetFile('.bsc');
     LoadLength := GetWord(@FileArray[$0B]);
     For LPos := 17 to LoadLength+17 -1 Do
        If LPos < 65535 Then
           Memory[LPos + CodeAddress - 17] := FileArray[LPos];
  End;
  If Lowercase(ExtractFileExt(Name)) = '.scr' then Begin
     Filename := Name;
     GetFile('.scr');
     For LPos := 0 to Min(Length(FileArray)-1, 6911) Do
        If LPos + 16384 < 65535 Then
           Memory[LPos + 16384] := FileArray[LPos];
     If TryEnterSection Then Begin
        UpdateDisplay;
        UpdateBASinDisplay;
        NeedDisplayUpdate := False;
        ResizeSectionBool := False;
     End;
  End;

End;

procedure TBASinOutput.ScrollBox1VScroll(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType);
begin
  If EventType = vsThumbTrack then
     ViewLine := ScrollBox1.ScrollInfo.nTrackPos Div (8*Opt_FontScale)
  Else
     ViewLine := ScrollBox1.ScrollInfo.nPos Div (8*Opt_FontScale);
  RepaintBASIC(Not PageMove);
end;

procedure TBASinOutput.ScrollBox1HScroll(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType);
begin
  If EventType = hsThumbTrack then
     ViewColumn := ScrollBox1.ScrollInfo.nTrackPos Div (8*Opt_FontScale)
  Else
     ViewColumn := ScrollBox1.ScrollInfo.nPos Div (8*Opt_FontScale);
  RepaintBASIC(Not PageMove);
end;

Procedure TBASinOutput.UpdateMenu;
Var
  Message: TMessage;
Begin
  OnEnterMenuLoop(Message);
End;

procedure TBASinOutput.OnEnterMenuLoop(var Message: TMessage);
Var
  F: Integer;
  ItemCaption: String;
  MenuItem: TMenuItem;
Label
  MRUBuild;
Begin

  If Registers.EmuRunning Then Begin
     ControlEmulation(True);
     DisplayWindow.WantsFront := True;
     InMenu := True;
     If ProgStateFlag = PS_RESET Then Begin
        DisplayWindow.BringToFront;
        DisplayWindow.SetFocus;
     End;
  End;

  ClearSoundBuffers;

  // Menu Items' Enabled states are decided here.
  // The RUN Items are decided by a call to RUNTimeWindow's Update
  // Buttons routines, as they are closely related.
  UpdateRunTimeButtons;
  ForceBreak1.Enabled := Running and Run2.Enabled;
  Undo1.Enabled := (Not (Running or Registers.EmuRunning)) and (UndoList.Count > 0);
  Redo1.Enabled := (Not (Running or Registers.EmuRunning)) and (RedoList.Count > 0);

  // These manipulate the edit line, so there must be an edit line to manipulate.
  CopyListing1.Enabled := Length(BASICMem) <> 0;
  Cut1.Enabled := Editing and (EditorSelStart <> EditorSelEnd);
  Copy1.Enabled := Cut1.Enabled;
  Paste1.Enabled := Editing and (ClipBoard.HasFormat(CF_TEXT)) and (ClipBoard.AsText <> '');
  Delete1.Enabled := Cut1.Enabled;

  // These need to be enabled while the program editor is running, and there is a
  // program to work on.
  TraceExecution1.Checked := Opt_FollowProgram;
  Find1.Enabled := Not Running and (Length(BASICMem) > 1);
  FindNext1.Enabled := Find1.Enabled and (LastFindText <> '');
  Replace1.Enabled := Find1.Enabled;
  ReplaceNext1.Enabled := FindNext1.Enabled;
  GoToLineNumber1.Enabled := Find1.Enabled;
  GotoError1.Enabled := (Not Running) and (LastErrorLine <> 65534);

  // Build the MRU List
  MRUBuild:
  MenuItem := nil;
  Item11.Visible := False;
  Item21.Visible := False;
  Item31.Visible := False;
  Item41.Visible := False;
  Item51.Visible := False;
  Item61.Visible := False;
  Item71.Visible := False;
  Item81.Visible := False;
  N12.Visible := False;
  If MRUList.Count > 0 then Begin
     ReLOAD1.Enabled := True;
     For F := 0 To 7 Do Begin
        Case F of
           0: MenuItem := Item11;
           1: MenuItem := Item21;
           2: MenuItem := Item31;
           3: MenuItem := Item41;
           4: MenuItem := Item51;
           5: MenuItem := Item61;
           6: MenuItem := Item71;
           7: MenuItem := Item81;
        End;
        If F < MRUList.Count then Begin
           ItemCaption := MRUList[F];
           If FileExists(ItemCaption) Then Begin
              MenuItem.Caption := '&'+IntToStr(F+1)+' '+ShrinkFileName(ItemCaption, 200);
              MenuItem.Visible := True;
              N12.Visible := True;
           End Else Begin
              MRUList.Delete(F);
              Goto MRUBuild;
           End;
        End Else
           MenuItem.Visible := False;
     End;
  End Else Begin
     If FileExists(BASinDir+'\Session.bin') Then
        ReLOAD1.Enabled := True
     Else
        ReLOAD1.Enabled := False;
  End;

  // The Save... and Save As... Items need updating according to the current project name
  Save1.Enabled := (CurProjectFileName <> DefaultProjectName);
  SaveBASICAs1.Enabled := True;

  // The Window Size items need to be set accordingly
  N100320x2401.Checked := False;
  N200640x4801.Checked := False;
  Custom1.Checked := False;
  Custom1.Enabled := False;
  Custom1.Visible := False;
  If (DisplayWindow.DisplayIMG.Width = 320) and (DisplayWindow.DisplayIMG.Height = 240) Then
     N100320x2401.Checked := True
  Else If (DisplayWindow.DisplayIMG.Width = 640) and (DisplayWindow.DisplayIMG.Height = 480) Then
     N200640x4801.Checked := True
  Else Begin
     Custom1.Visible := True;
     Custom1.Enabled := True;
     Custom1.Checked := True;
     Custom1.Caption := 'Custom Size ('+IntToStr(DisplayWindow.DisplayIMG.Width)+'x'+IntToStr(DisplayWindow.DisplayIMG.Height)+')';
  End;
  Force11Aspect1.Checked := Opt_MaintainAspect or (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]);
  Force11Aspect1.Enabled := Not (Opt_RenderMethod in [rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle]);

  // Source Markers need to be enabled if they've been assigned.

  Marker02.Enabled := SourceMarkers[0].Assigned;
  Marker12.Enabled := SourceMarkers[1].Assigned;
  Marker22.Enabled := SourceMarkers[2].Assigned;
  Marker32.Enabled := SourceMarkers[3].Assigned;
  Marker42.Enabled := SourceMarkers[4].Assigned;
  Marker52.Enabled := SourceMarkers[5].Assigned;
  Marker62.Enabled := SourceMarkers[6].Assigned;
  Marker72.Enabled := SourceMarkers[7].Assigned;
  Marker82.Enabled := SourceMarkers[8].Assigned;
  Marker92.Enabled := SourceMarkers[9].Assigned;

  // And last but not least...
  Variables1.Checked := VariablesWindow.Visible;
  SystemVariables1.Checked := SysVarsWindow.Visible;
  Watches1.Checked := WatchWindow.Visible;
  GOSUBStack1.Checked := GOSUBWindow.Visible;
  Breakpoints1.Checked := BreakpointsWindow.Visible;
  LogWindow1.Checked := LogWindow.Visible;
  MemoryViewer1.Checked := HexWindow.Visible;
  MemoryMap1.Checked := MemMapWindow.Visible;
  SpeedButton10.Enabled := AddBreakpoint1.Enabled;
  DisplayWindow1.Checked := DisplayWindow.Visible;
  CharacterRuler1.Checked := Opt_CharacterRuler;
  CodeControlIcons1.Checked := Opt_Controlicons;
  EnableProfiling1.Checked := ProfilingEnabled;

End;

procedure TBASinOutput.OnExitMenuLoop(var Message: TMessage);
Begin
  InMenu := False;
End;

Procedure TBASinOutput.AddToMRUList(Filename: String);
Begin
  If MRUList.IndexOf(Filename) <> -1 Then MRUList.Delete(MRUList.IndexOf(Filename));
  MRUList.Insert(0, FileName);
  While MRUlist.Count > 8 Do MRUList.Delete(MRUList.Count -1);
End;

procedure TBASinOutput.FormActivate(Sender: TObject);
begin
  If Not AppClosing Then Begin
     DisplayWindow.WantsFront := False;
     SetCapsLock;
     UpdateRunTimeButtons;
  End;
end;

procedure TBASinOutput.Timer2Timer(Sender: TObject);
begin
  // Update the Free Memory Display and switch to the Display window if necessary.
  If DisplayWindow.WantsFront and Not InMenu Then Begin
     If Not DisplayWindow.Visible Then ShowWindow(DisplayWindow, False);
     DisplayWindow.BringToFront;
     DisplayWindow.WantsFront := False;
  End;
  OnHint(Nil);
end;

procedure TBASinOutput.StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
Var
  CursorDIB: TFastDIB;
begin

  StatusBar1.Canvas.Brush.Style := BsClear;

  CursorDIB := TFastDIB.Create;
  CursorDIB.SetSize(8, 8, 32);

  If Opt_ShowingSyntax or Opt_Predictive Then Begin
     StatusBar1.Panels[0].Width := 25;
     If CursorType = '!' Then
        SpecTextToDIB(CursorDIB, 0, 1, '?', 7, 1, 0, False, False)
     Else
        If CursorType = '?' Then
           SpecTextToDIB(CursorDIB, 0, 1, CursorType, 7, 2, 0, False, False)
        Else
           If Running Then Begin
              If Not Registers.EmuRunning Then
                 SpecTextToDIB(CursorDIB, 0, 1, #170, 2, 0, 0, False, False)
              Else
                 SpecTextToDIB(CursorDIB, 0, 1, #169, 4, 0, 0, False, False);
           End Else
              SpecTextToDIB(CursorDIB, 0, 1, CursorType, 7, 0, 1, False, False);
  End Else Begin
     StatusBar1.Panels[0].Width := 0;
  End;

  If InsertMode Then
     StatusBar1.Panels[2].Text := ' Insert'
  Else
     StatusBar1.Panels[2].Text := ' Overwrite';

  CursorDIB.Stretch(StatusBar1.Canvas.Handle, 4, 6, 16, 16);
  CursorDIB.Free;
  StatusBar1.Canvas.TextOut(26, 6, StatusBar1.Panels[0].Text)

end;

Procedure TBASinOutput.OnUpdateCursor(var Message: TMessage);
Begin
  StatusBar1.Repaint;
End;

Procedure TBASinOutput.SetCaption;
Begin
  Caption := ReleaseName;
  Application.Title := CurProjectName + ' - BasinC';


End;

procedure TBASinOutput.Token1DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
Var
  Str: String;
begin
  Str := Token1.Caption;
  While Pos('&', Str) <> 0 Do
     Str := Copy(Str, 1, Pos('&', Str) -1)+Copy(Str, Pos('&', Str)+1, 999999);
  ACanvas.Brush.Color := ClMenu;
  ACanvas.Font.Color := ClMenuText;
  ACAnvas.Font.Style := [fsBold];
  Acanvas.TextOut(ARect.Left+6, ARect.Top+4, Str);
end;

// BASIC Editor routines

Procedure TBASinOutput.GetBASIC;
Var
  TempStr, LineText, NoTokens, LineNumStr: String;
  LineLen, LineNum: Word;
  tPos, tLen: Integer;
  UndoString: String;
Begin

  SplitStatements := True;

  // Grab the current BASIC area as a memory dump, then converts to
  // plain text - preserving colour controls and graphics chars.

  UndoString := BASICMem;

  BASICChanged := True;
  BASICMem := '';
  TempStr := GetMemoryString(GetWord(@Memory[PROG]), (GetWord(@Memory[VARS]) - GetWord(@Memory[PROG])), Memory);
  tPos := 1;
  tLen := Length(TempStr);

  If TempStr <> '' Then Begin

     While tPos < tLen Do Begin

        // Insert Line Number

        LineNum := (Byte(TempStr[tPos]) Shl 8)+Byte(TempStr[tPos+1]);
        LineNumStr := IntToStr(LineNum);
        BASICMem := BASICMem + LineNumStr+' ';

        // Get Line Length (including terminal #13)

        LineLen := GetWord(@TempStr[tPos+2]);
        Inc(tPos, 4);

        // Sanity check - is the line as long as it says it is?

        If tPos + LineLen -1 <= Length(TempStr) Then Begin
           If TempStr[tPos+LineLen -1] <> #13 Then Begin
              LineText := Copy(TempStr, tPos, LineLen);
              NoTokens := '';
              NoTokens := DetokeniseLine(LineText, False);
              BASICMem := BASICMem + NoTokens;
              Break;
           End;
        End;

        // Add the text to the text array.

        LineText := Copy(TempStr, tPos, LineLen);
        NoTokens := '';
        NoTokens := DetokeniseLine(LineText, False);
        BASICMem := BASICMem + NoTokens;
        Inc(tPos, LineLen);

     End;

     If BASICMem[Length(BASICMem)] <> #13 Then BASICMem := BASICMem + #13;

  End Else Begin

     BASICMem := #13;

  End;

  If BASICMem <> UndoString Then Begin

     If UndoString <> '' Then Begin

        // BASIC Changed, so set an undo point. No need to worry about cursoffset - it's not changed here.

        TempStr := BASICMem;
        BASICMem := UndoString;
        AddUndo;
        BASICMem := TempStr;

     End;

  End;

  UpdateParseText;
  CodeError := False;
  If Not BPs_Updating Then Begin
     BreakpointsWindow.BuildBreakpointsList;
     //LabelDebug.Caption :='*2' + LastLineBuffer  ;
  End;
  BASinOutput.LastLineBuffer := BASinOutput.BASICMem;

End;

Procedure TBASinOutput.RepaintBASIC(DoPaint: Boolean);
Var
  Ink, Paper, Bright, pInk, pPaper, pBright, pTemp, Inverse: Byte;
  CurChar: Char;
  LineText, TempStr, LineStr, CurWord: String;
  CurWordOrg, ePPC, eSUBPPC, XPos, YPos, SizeOpt, TempVal, ViewX, LineNum, StatementNum, CurWordPos, Idx: Integer;
  CurPos, NumLines, TempPrevLine, TempPrevLine2,
  CurLine, LineLen, MaxLineLen, Offset, CurFontScale: DWord;
  AtCursor, InString, NewLine, NewStatement, LineBreak, REMCommand, GotCursString, InSelection: Boolean;
  BreakType: Byte;
  DIB: TFastDIB;
  tempscale: integer; //used for downsizing zx stripes temporarily.
Const
  HexChars: String = '0123456789ABCDEF';
Label
  ColonChar;
Begin

  MaxLineLen := 0;
  NumLines := 0;
  LineNum := 0;
  StatementNum := 1;
  CurLine := 0;
  CurPos := 1;
  LineLen := 0;
  SizeOpt := 8 * Opt_FontScale;

  CursorVisible := False;
  InString := False;
  NewLine := True;
  TempPrevLine2 := 1;
  TempPrevLine := 1;

  pInk := Opt_Foreground;
  pPaper := Opt_Background;
  pBright := 0;
  Inverse := 0;

  ViewOffset := -1;
  CursPageStart := -1;
  CursPageEnd := -1;
  CursProgLineStart := -1;
  CursProgLineEnd := -1;

  DisplayPalette[16] := TFSpecDarkA;

  ePPC := GetWord(@Memory[PPC]);
  eSUBPPC := Memory[SUBPPC];
  LineBreak := False;

  If CursOffset > DWord(Length(BASICMem)) Then CursOffset := Length(BASICMem);
  If CursOffset < 1 Then CursOffset := 1;

  CurFontScale := Opt_FontScale;
  XPos := - ViewColumn *8;
  YPos := - ViewLine *8;

  ViewX := -ViewColumn *8;

  CursStringEnd := 0;
  CursStringStart := 0;
  CursStringOffset := 0;
  GotCursString := False;

  CurWord := '';
  CurWordOrg := XPos;
  CurWordPos := 1;

  WordOffset:=0; //Flist
  WordStack:='';
  If DoPaint Then Begin
     FastIMG1.Bmp.SetSize(((ScrollBox1.Width+(8*Opt_FontScale)) Div (8*Opt_FontScale))*(8*Opt_FontScale), ((ScrollBox1.Height+16) Div (8*Opt_FontScale))*(8*Opt_FontScale), 32);
     If Opt_FontScale = 1 Then Begin
        DIB := FastIMG1.Bmp;
     End Else Begin
        CharDIB.SetSize(FastIMG1.Bmp.Width Div Opt_FontScale, FastIMG1.Bmp.AbsHeight Div Opt_FontScale, 32);
        DIB := CharDIB;
        Opt_FontScale := 1;
     End;
     DIB.Clear(TFColorAToTFColor(DisplayPalette[Opt_Background]));
     FillRect(DIB, Max(XPos, 0), 0, Max(XPos+40, 0), DIB.AbsHeight -1, TFSpecDark);
  End Else Begin
     If Opt_FontScale = 1 Then Begin
        DIB := FastIMG1.Bmp;
     End Else Begin
        CharDIB.SetSize(FastIMG1.Bmp.Width Div Opt_FontScale, FastIMG1.Bmp.AbsHeight Div Opt_FontScale, 32);
        DIB := CharDIB;
        Opt_FontScale := 1;
     End;
  End;

  While CurPos <= DWord(Length(BASICMem)) Do Begin

     If (YPos >= 0) and (ViewOffset = -1) Then ViewOffset := CurPos;
     If YPos < DIB.Height Then CursPageEnd := TempPrevLine;

     // Draw the characters one by one - using an optimised version
     // of SpecTextToDIB(), from Utility.pas.

     CurChar := BASICMem[CurPos];

     // Test for a line number, and if found, pad it out.

     If NewLine Then Begin
        LineStr := '';
        LastWord := '';
        Offset := CurPos;


        If BASICMem[Offset] in [#48..#57] Then Begin
           While BASICMem[Offset] in [#48..#57] Do Begin
              LineStr := LineStr + BASICMem[Offset];
              Inc(Offset);
           End;
           LineNum := StrToIntDef(LineStr, -1);
           StatementNum := 1;
           If LineNum >= 0 Then
              Inc(XPos, 40 - Min((Length(LineStr)*8), 32))
           Else Begin
              LineNum := 0;
              XPos := ViewX+40;
           End;
           REMCommand := False;
        End Else Begin
              LineNum := 0;
              XPos := ViewX+40;
        End;
        If CurPos <= CursOffset Then
           CursProgLineStart := CurPos;
        CurWord := '';
        CurWordOrg := XPos;

     End;
           //OutputDebugString (PChar('BBEQU-'+IntToStr(FastIMG1.Bmp.Height - GetSystemMetrics(SM_CYHSCROLL)- (40*Opt_FontScale))+'hi,'+ IntToStr( CursorPoint.Y )+'y, '+ IntToStr( YPos )));

     // Test for and store the params of the cursor, if we're at the right position.

     If CurPos = CursOffset Then Begin
        CursorVisible := (YPos >= 0) and (YPos < DIB.AbsHeight - 8) and (XPos >= 0) and (XPos < DIB.Width -8);
        CursorChar := BASICMem[CurPos];

        CursorPoint := Point(XPos, YPos);

        CursPrevLineStart := TempPrevLine2;
        CursLineStart := TempPrevLine;
        CursLineNum := LineNum;
        CursListLine := NumLines;
        CursStatementNum := StatementNum;
        CursorInString := InString;
        If CursorChar = #13 Then CursorChar := ' ';
        If CursorChar < ' ' Then Begin
           TempVal := CurPos;
           Repeat
              Inc(TempVal);
              CursorChar := BASICMem[TempVal];
           Until CursorChar >= ' ';
        End;
     End;
     If Not LineBreak Then Begin
        If XPos > ViewX+39 Then Begin
           If Inverse = 0 Then Begin
              Ink := pInk;
              Paper := pPaper;
           End Else Begin
              Ink := pPaper;
              Paper := pInk;
           End;
           Bright := pBright;
        End Else Begin
           Ink := Opt_Foreground;
           Paper := 16;
           Bright := 0;
        End;
     End Else Begin
        Case BreakType of
           1: Begin
                 Paper := 1;
                 Ink := 7;
                 Bright := 0;
              End;
           2: Begin
                 Paper := 2;
                 Ink := 7;
                 Bright := 1;
              End;
           3: Begin
                 Paper := 2;
                 Ink := 7;
                 Bright := 0;
              End;
        End;
     End;

     // If in a selection, invert the selection.

     If Not Running Then
        If (CurPos >= EditorSelStart) and (CurPos < EditorSelEnd) Then
           If (EditorSelStart <> EditorSelEnd) Then Begin
              pTemp := Ink;
              Ink := Paper;
              Paper := pTemp;
           End;

     // Test for line highlights - Breakpoints and current PPC/SUBPPC, also source markers

     If (Newline or NewStatement) and (LineNum < 10000) and (LineNum > 0) Then Begin

        If Running Then
           If (CursLineNum = LineNum) and (StatementNum = CursStatementNum) Then Begin
              If DoPaint Then
                 FillRect(DIB, 0, (DIB.AbsHeight -1) - YPos, DIB.Width -1, (DIB.AbsHeight -1) - (Ypos +7), TFSpecBlue);
              LineBreak := True;
              Paper := 1;
              Ink := 7;
              BreakType := 1;
           End;

        If BreakpointsList[LineNum][StatementNum] <> #0 Then Begin
              If (YPos >= 0) and (YPos < DIB.AbsHeight) Then
                 If BreakArray[Byte(BreakPointsList[LineNum][StatementNum])].Enabled Then Begin
                    If DoPaint Then
                       FillRect(DIB, 0, (DIB.AbsHeight -1) - YPos, DIB.Width -1, (DIB.AbsHeight -1) - (Ypos +7), TFSpecRedB);
                    Bright := 1;
                    BreakType := 2;
                 End Else Begin
                    If DoPaint Then
                       FillRect(DIB, 0, (DIB.AbsHeight -1) - YPos, DIB.Width -1, (DIB.AbsHeight -1) - (Ypos +7), TFSpecRed);
                    Bright := 0;
                    BreakType := 3;
                 End;
              LineBreak := True;
              Paper := 2;
              Ink := 7;
           End;

        // Test the source markers

        If ViewX < 8 Then
           For Idx := 0 To 9 Do
              If SourceMarkers[Idx].Assigned Then
                 If SourceMarkers[Idx].MarkedLine = LineNum Then
                    If SourceMarkers[Idx].MarkedStatement = StatementNum Then Begin
                       FillRect(DIB, 2, DIB.AbsHeight - YPos -1, 6, (DIB.AbsHeight -1) - (YPos + 6), TfSpecGreen);
                       SmallTextOut(DIB, Chr(Idx+48), 3, (DIB.Absheight - YPos) -6, DisplayPalette[Opt_Foreground]);
                    End;


     End;

     If NewLine Then NewLine := False;
     If NewStatement Then NewStatement := False;

     If (ProgramIs128k And (CurChar in [#0..#12, #14..#31, #163..#255])) or ((Not ProgramIs128k) and (CurChar in [#0..#12, #14..#31, #165..#255])) Then Begin
        If CurWord <> '' Then Begin
           NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
           CurWord := '';
        End;
        If XPos +8 < DIB.Width Then
           If XPos > 0 Then
              SmallTextOut(DIB, HexChars[(((Ord(CurChar) div 16)*16) div 16)+1]+HexChars[(Ord(CurChar) Mod 16)+1], XPos, (DIB.AbsHeight - YPos)-7, DisplayPalette[Ink]);
        Inc(XPos, 8);
        CurChar := #0;
     End;


     Case CurChar of
        #0:
           Begin
              // Dummy
           End;
        #16, #17:
           Begin // INK/PAPER control
              If Not Opt_SyntaxHighlight Then Begin
                 If CurWord <> '' Then Begin
                    NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                    CurWord := '';
                 End;
                 If (BASICMem[CurPos+1] in [#0..#9]) and Not LineBreak Then Begin
                    pTemp := Ord(BASICMem[CurPos+1]);
                    If pTemp <> 8 Then Begin
                       If pTemp = 9 Then Begin
                          If CurChar = #16 Then Begin
                             If pPaper in [0..3] Then
                                pTemp := 7
                             Else
                                pTemp := 0;
                          End Else Begin
                             If pInk in [0..3] Then
                                pTemp := 7
                             Else
                                pTemp := 0;
                          End;
                       End;
                       If CurChar = #16 Then
                          pInk := pTemp
                       Else
                          pPaper := pTemp;
                    End;
                 End;
              End;
           End;
        #18..#20:
           Begin // FLASH/BRIGHT/INVERSE control
              If Not Opt_SyntaxHighlight Then Begin
                 If (BASICMem[CurPos+1] in [#0, #1]) and Not LineBreak Then Begin
                    If CurWord <> '' Then Begin
                       NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                       CurWord := '';
                    End;
                    If CurChar = #19 Then pBright := Ord(BASICMem[CurPos+1]);
                    If CurChar = #20 Then Inverse := Byte(BASICMem[CurPos+1]);
                 End;
              End;
           End;
        //R15
        #22:
           Begin    // AT character
              If CurWord <> '' Then Begin
                 NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                 CurWord := '';
              End;
              If XPos +8 < DIB.Width Then
                 If XPos > 0 Then
                    SmallTextOut(DIB, HexChars[(((Ord(CurChar) div 16)*16) div 16)+1]+HexChars[(Ord(CurChar) Mod 16)+1], XPos, (DIB.AbsHeight - YPos)-7, DisplayPalette[Ink]);
              Inc(XPos, 8);
              If CurPos < cardinal(Length(BASICMem)-1) Then Begin
                 Inc(CurPos);
                 Inc(LineLen);
                 CurChar := BASICMem[CurPos];
                 If CurPos = CursOffset Then Begin
                    CursorChar := CurChar;
                    CursorPoint := Point(XPos, YPos);
                    CursPrevLineStart := TempPrevLine2;
                    CursLineStart := TempPrevLine;
                    CursLineNum := LineNum;
                    CursListLine := NumLines;
                    CursStatementNum := StatementNum;
                    CursorInString := InString;
                 End;
                 If XPos +8 < DIB.Width Then
                    If XPos > 0 Then
                       SmallTextOut(DIB, HexChars[(((Ord(CurChar) div 16)*16) div 16)+1]+HexChars[(Ord(CurChar) Mod 16)+1], XPos, (DIB.AbsHeight - YPos)-7, DisplayPalette[Ink]);
                 Inc(XPos, 8);
              End;
              If CurPos < Length(BASICMem)-1 Then Begin
                 Inc(CurPos);
                 Inc(LineLen);
                 CurChar := BASICMem[CurPos];
                 If CurPos = CursOffset Then Begin
                    CursorChar := CurChar;
                    CursorPoint := Point(XPos, YPos);
                    CursPrevLineStart := TempPrevLine2;
                    CursLineStart := TempPrevLine;
                    CursLineNum := LineNum;
                    CursListLine := NumLines;
                    CursStatementNum := StatementNum;
                    CursorInString := InString;
                 End;
                 If XPos +8 < DIB.Width Then
                    If XPos > 0 Then
                       SmallTextOut(DIB, HexChars[(((Ord(CurChar) div 16)*16) div 16)+1]+HexChars[(Ord(CurChar) Mod 16)+1], XPos, (DIB.AbsHeight - YPos)-7, DisplayPalette[Ink]);
                 Inc(XPos, 8);
              End;
           End;
        #23:
           Begin    // TAB Character
              If CurWord <> '' Then Begin
                 NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                 CurWord := '';
              End;
              If XPos +8 < DIB.Width Then
                 If XPos > 0 Then
                    SmallTextOut(DIB, HexChars[(((Ord(CurChar) div 16)*16) div 16)+1]+HexChars[(Ord(CurChar) Mod 16)+1], XPos, (DIB.AbsHeight - YPos)-7, DisplayPalette[Ink]);
              Inc(XPos, 8);
              If CurPos < Length(BASICMem)-1 Then Begin
                 Inc(CurPos);
                 Inc(LineLen);
                 CurChar := BASICMem[CurPos];
                 If CurPos = CursOffset Then Begin
                    CursorChar := CurChar;
                    CursorPoint := Point(XPos*CurFontScale, YPos*CurFontScale);
                    CursPrevLineStart := TempPrevLine2;
                    CursLineStart := TempPrevLine;
                    CursLineNum := LineNum;
                    CursListLine := NumLines;
                    CursStatementNum := StatementNum;
                    CursorInString := InString;
                 End;
                 If XPos +8 < DIB.Width Then
                    If XPos > 0 Then
                       SmallTextOut(DIB, HexChars[(((Ord(CurChar) div 16)*16) div 16)+1]+HexChars[(Ord(CurChar) Mod 16)+1], XPos, (DIB.AbsHeight - YPos)-7, DisplayPalette[Ink]);
                 Inc(XPos, 8);
              End;
           End;
        // R15end
        #13:
           Begin // Carriage Return
              If InString and (CursStringStart <> 0) Then
                 If CursStringEnd = 0 Then Begin
                    CursStringEnd := CurPos;
                    If (CursOffset < DWord(CursStringStart)) or (CursOffset > DWord(CursStringEnd)) Then Begin
                       If not GotCursString Then Begin
                          CursStringStart := 0;
                          CursStringEnd := 0;
                          CursStringOffset := 0;
                       End;
                    End Else
                       GotCursString := True;
                 End;
              If (CurPos >= CursOffset) And (CursProgLineEnd = -1) Then
                 CursProgLineEnd := CurPos;
              If (Integer(NumLines) >= ViewLine) and (YPos < DIB.AbsHeight) Then
                 If CurWord <> '' Then Begin
                    NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                    CurWord := '';
                 End;

              if Opt_Indenting Then Begin
                 //this is a new line!
                 if (Copy(WordStack, Length(WordStack), 1)='I') Then     //if last keyword was "IF" then get back
                 Begin
                     Wordstack:= Copy(WordStack, 1, Length(WordStack)-1);
                     Dec(ViewX, ((Opt_Indentsize*8)*Opt_FontScale)); //flist
                     Dec(WordOffset,1);

                End;
                 WordOffset:=0;
              End;

              XPos := ViewX;
              Inc(YPos, 8*Opt_FontScale); //arda
              Inc(NumLines);
              If LineLen > MaxLineLen Then MaxLineLen := LineLen;
              LineLen := 0;
              InString := False;
              NewLine := True;
              If Integer(NumLines) = ViewLine Then YPos := 0;
              TempPrevLine2 := TempPrevLine;
              TempPrevLine := CurPos+1;
              LineBreak := False;
              REMCommand := False;
           End;

        #58:
           Begin // ':' Separator
              If InString or REMCommand Then Goto ColonChar;
              If DoPaint Then begin
                 If (Integer(NumLines) >= ViewLine) and (YPos < DIB.AbsHeight) Then
                    If Not (REMCommand or InString) Then Begin
                       If CurWord <> '' Then Begin
                          NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, False, InString, DoPaint, CurWordPos);
                          CurWord := '';
                       End;
                       NewWord(DIB, ':', XPos, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurPos);
                    End Else
                       CurWord := CurWord + CurChar;
              End;
              If Not InString and Not REMCommand Then Begin
                 If SplitStatements Then Begin
                    If not GotCursString Then
                       CursStringOffset := 0;
                    NewStatement := True;
                    XPos := ViewX+32;
                    Inc(YPos, 8*Opt_FontScale); //arda
                    Inc(NumLines);
                    If LineLen > MaxLineLen Then MaxLineLen := LineLen;
                    LineLen := 0;
                    InString := False;
                    If Integer(NumLines) = ViewLine Then YPos := 0;
                    TempPrevLine2 := TempPrevLine;
                    TempPrevLine := CurPos+1;
                 End;
                 Inc(StatementNum);
                 LineBreak := False;
                 CurWordOrg := XPos +8;
                 CurWordPos := CurPos;
              End;
              Inc(XPos, 8);
           End;
        #32..#57, #59..#164:
           Begin // Alpha-numeric/symbols/UDGs/Chequerboard chars
              ColonChar:
              If DoPaint Then
                 If ProgramIs128k Then
                    If CurChar in [#163, #164] Then Begin
                       Continue;
                    End;
                 If (Integer(NumLines) >= ViewLine) and (YPos < DIB.AbsHeight) Then Begin
                    If (CurChar in ['0'..'9', 'A'..'Z', 'a'..'z']) or REMCommand or InString Then Begin
                       If CurWord = '' Then Begin
                          CurWordOrg := XPos;
                          CurWordPos := CurPos;
                       End;
                       If CurWord <> '' Then
                          If (CurChar in ['A'..'Z', 'a'..'z']) and ((CurWord[1] in ['0'..'9'])) Then Begin
                             NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                             CurWordOrg := XPos;
                             CurWord := '';
                          End;
                       CurWord := CurWord + CurChar;
                    End Else Begin
                       If (CurChar = ' ') and ((UpperCase(CurWord) = 'GO') or (UpperCase(CurWord) = 'DEF')) or InString Then
                          CurWord := CurWord + ' '
                       Else Begin
                          If CurWord <> '' Then begin
                                //new keyword, off screen
                                If Opt_Indenting and (UpperCase(CurWord) = 'NEXT') and (WordOffset>0) Then begin //flist
                                        if (Copy(WordStack, Length(WordStack), 1)='F') Then
                                        Begin
                                                Wordstack:= Copy(WordStack, 1, Length(WordStack)-1);
                                                Dec(WordOffset , 1); //flist
                                                Dec(ViewX, (Opt_Indentsize*8)*Opt_FontScale); //flist



                                                Dec(CurWordOrg, (Opt_Indentsize*8)*Opt_FontScale);
                                                //XPos:=CurWordOrg+ViewX+32;
                                                Dec(XPos,(Opt_Indentsize*8)*Opt_FontScale);
                                        End Else Begin
                                                //oops this program is not valid for indenting!
                                                //if x Then Next y   <-- conditional next.
                                                //let it sink.
                                        End ;
                                End;   //flist

                                NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                                //indenting178
                                //If Opt_Indenting and (UpperCase(CurWord) = 'NEXT') Then Begin//and (WordOffset>0) Then begin //flist
                                //        Dec(XPos,(Opt_Indentsize*8)*Opt_FontScale);
                                //End;
                                //enindent78
                          End;
                          If Not (CurChar in ['$', '0'..'9', 'A'..'Z', 'a'..'z']) Then
                             NewWord(DIB, CurChar, XPos, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurPos)
                          Else If CurChar = '$' Then
                             If CurWord <> '' Then Begin
                                CurWord := CurWord + '$';
                                NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                             End Else
                                NewWord(DIB, CurChar, XPos, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurPos);
                          CurWord := '';
                       End;
                    End;
                 End Else Begin
                        If Opt_Indenting and (CurWord <> '' ) Then Begin
                                 if  (UpperCase(CurWord) = 'NEXT') and (WordOffset>0) Then begin //flist
                                        if (Copy(WordStack, Length(WordStack), 1)='F') Then
                                        Begin
                                         Wordstack:= Copy(WordStack, 1, Length(WordStack)-1);
                                        Dec(WordOffset , 1); //flist
                                        Dec(ViewX, (Opt_Indentsize*8)*Opt_FontScale); //flist

                                        Dec(CurWordOrg, (Opt_Indentsize*8)*Opt_FontScale);
                                        //XPos:=CurWordOrg+ViewX+32;
                                        Dec(XPos,(Opt_Indentsize*8)*Opt_FontScale);

                                        End;
                                End;   //flist
                        End;
                 End;
              If CurChar = '"' Then Begin   // quotes
                 If CurWord <> '' Then
                    If InString Then Begin
                       NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                       CurWordOrg := XPos +8;
                       CurWord := '';
                    End Else Begin
                       NewWord(DIB, Copy(CurWord, 1, Length(CurWord) -1), CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, REMCommand, InString, DoPaint, CurWordPos);
                       CurWordOrg := XPos;
                       CurWord := '"';
                    End;
                 If Not REMCommand Then
                    InString := Not InString;
                 If Not GotCursString Then
                    If InString Then Begin // Start of a string
                       CursStringStart := CurPos;
                       If CursStringOffset = 0 Then
                          If StatementNum = 1 Then
                             CursStringOffset := Length(LineStr);
                    End Else Begin // End of a string - is the cursor inside this string?
                       CursStringEnd := CurPos;
                       If (CursOffset < CursStringStart) or (CursOffset > CursStringEnd) Then Begin
                          If not GotCursString Then Begin
                             CursStringStart := 0;
                             CursStringEnd := 0;
                             CursStringOffset := 0;
                          End;
                       End Else Begin
                          GotCursString := True;
                       End;
                 End;
              End;
              If CurChar in ['m', 'M'] Then Begin
                 // REM Statement?
                 If Not (InString or REMCommand) Then Begin
                    If CurPos > 4 Then
                       If UpperCase(Copy(BASICMem, CurPos -2, 4)) = 'REM ' Then Begin
                          If (Integer(NumLines) >= ViewLine) and (YPos < DIB.AbsHeight) Then
                             If CurWord <> '' Then Begin
                                NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, False, InString, DoPaint, CurWordPos);
                                CurWord := '';
                             End;
                          REMCommand := True;
                          InString := False;
                       End;
                 End;
              End;


              If Opt_Indenting and (CurChar in ['t', 'T']) and (Integer(NumLines) < ViewLine)  Then Begin  //flist Blocks
                 // Next statement?
                 If Not InString and Not REMCommand Then Begin
                    If CurPos > 3 Then
                       If (WordOffset>0) and (UpperCase(Copy(BASICMem, CurPos -3, 5)) = 'NEXT ') Then Begin
                               if (Copy(WordStack, Length(WordStack), 1)='F') Then
                                        Begin
                                         Wordstack:= Copy(WordStack, 1, Length(WordStack)-1);
                                         Dec(WordOffset , 1); //flist lediz
                                         Dec(ViewX, (Opt_Indentsize*8)*Opt_FontScale); //flist
                                        End;
                       End;
                 End;
              End;

              If Opt_Indenting and (CurChar in ['r', 'R']) Then Begin  //flist Blocks
                 // FOR statement?
                 If Not InString and Not REMCommand Then Begin
                    If CurPos > 3 Then
                       If UpperCase(Copy(BASICMem, CurPos -2, 4)) = 'FOR ' Then Begin
                                Inc(WordOffset , 1); //flist  lediz
                                WordStack:=WordStack+'F';
                                Inc(ViewX, (Opt_Indentsize*8)*Opt_FontScale); //flist

                       End;
                 End;
              End;

              If CurChar in ['n', 'N'] Then Begin
                 // THEN statement?
                 If Not InString and Not REMCommand Then Begin
                    If CurPos > 4 Then
                       If UpperCase(Copy(BASICMem, CurPos -3, 5)) = 'THEN ' Then Begin
                          If (Integer(NumLines) >= ViewLine) and (YPos < DIB.AbsHeight) Then
                             If CurWord <> '' Then Begin

                                NewWord(DIB, CurWord, CurWordOrg, YPos, Ink, Paper, Bright, LineBreak, False, InString, DoPaint, CurWordPos);
                                CurWord := '';

                             End;
                          NewStatement := True;
                          If not GotCursString Then
                             CursStringOffset := 0;

                          if Opt_Indenting Then Begin
                                Inc(WordOffset , 1); //flist
                                WordStack:=WordStack+'I';
                                Inc(ViewX, (Opt_Indentsize*8)*Opt_FontScale); //flist
                          End;

                          XPos := ViewX+32;
                          Inc(YPos, 8*Opt_FontScale); //arda



                          Inc(NumLines);
                          If LineLen > MaxLineLen Then MaxLineLen := LineLen;
                          LineLen := 0;
                          InString := False;
                          If Integer(NumLines) = ViewLine Then YPos := 0;
                          TempPrevLine2 := TempPrevLine;
                          TempPrevLine := CurPos+1;
                          Inc(StatementNum);
                          LineBreak := False;
                       End;
                 End;
              End;
              Inc(XPos, 8);
           End;
     End;

     Inc(CurPos);
     Inc(LineLen);

  End;

  Opt_FontScale := CurFontScale;



  If CursStringStart > 0 Then Begin
     CursStringStart := (Integer(CursStringStart) - CursLineStart) +1;
     CursStringEnd := (Integer(CursStringEnd) - CursLineStart) +1;
  End;

  If DoPaint Then Begin

     If Opt_FontScale > 1 Then Begin
        CharDIB.Stretch(FastIMG1.Bmp.hDc, 0, 0, FastIMG1.Bmp.Width, FastIMG1.Bmp.AbsHeight);
        CharDIB.SetSize(8, 8, 32);
        CursorPoint := Point(CursorPoint.X * Opt_FontScale, CursorPoint.Y * Opt_FontScale);
     End;

     ScrollBox1.VertScrollBar.Range := (1+Integer(NumLines)) * (8*Opt_FontScale);
     ScrollBox1.VertScrollBar.Increment := 8*Opt_FontScale;
     ScrollBox1.VertScrollBar.Visible := True;

     ScrollBox1.HorzScrollBar.Range := (8+Integer(MaxLineLen)) * (8*Opt_FontScale);
     ScrollBox1.HorzScrollBar.Increment := 8*Opt_FontScale;
     ScrollBox1.HorzScrollBar.Visible := True;

     FillRect(FastIMG1.Bmp, 0, FastIMG1.Bmp.Height - FastIMG1.Height, FastIMG1.Width -1, (FastIMG1.Bmp.Height - FastIMG1.Height) + 7, TfBlack);

     If ProgramIs128k Then
        SpecTextToDIB(FastIMG1.Bmp, 0, FastIMG1.Height - 7, '128 BASIC', 7, 0, 1, False, False)
     Else
        SpecTextToDIB(FastIMG1.Bmp, 0, FastIMG1.Height - 7, '48 BASIC', 7, 0, 1, False, False);

     //now draw stripes
     tempscale:=Opt_FontScale;
     Opt_FontScale:=1;
     DrawChar(FastIMG1.Bmp, @EditorChars[944], FastIMG1.Width -54, FastIMG1.Height -8, 2, 0, 1);
     DrawChar(FastIMG1.Bmp, @EditorChars[944], FastIMG1.Width -46, FastIMG1.Height -8, 6, 2, 1);
     DrawChar(FastIMG1.Bmp, @EditorChars[944], FastIMG1.Width -38, FastIMG1.Height -8, 4, 6, 1);
     DrawChar(FastIMG1.Bmp, @EditorChars[944], FastIMG1.Width -30, FastIMG1.Height -8, 5, 4, 1);
     DrawChar(FastIMG1.Bmp, @EditorChars[944], FastIMG1.Width -22, FastIMG1.Height -8, 0, 5, 1);
     Opt_FontScale:=tempscale;
     If CursorVisible and (CursorChar = #13) Then RepaintCursor;
     FastIMG1.BringToFront;
     If Not RepaintCursor Then Begin
        FastIMG1.Repaint;
        DoPaint := False;
     End;

  End;

  If Opt_CharacterRuler and Not DoPaint Then DrawRuler;

End;

Procedure NewWord(DIB: TFastDIB; CurWord: String; XPos, YPos, Ink, Paper, Bright: Integer; LineBreak, REMLine, InString, DoPaint: Boolean; CurPos: Integer);
Var
  Extras, Keyword, NewText: String;
  InvertFrom, InvertTo, KeyWordNum, AnchorX: Integer;
  Bold, Italic: Boolean;
Begin
  If CurWord = '' Then Exit;
  NewText := UpperCase(CurWord);
  If CompareStrToSubStr('GOTO', NewText, 1, 4) Then
     NewText := 'GO TO'+Copy(NewText, 5, 999999)
  Else
     If CompareStrToSubStr('GOSUB', NewText, 1, 5) Then
        NewText := 'GO SUB'+Copy(NewText, 6, 999999)
     Else
        If CompareStrToSubStr('DEFFN', NewText, 1, 5) Then
           NewText := 'DEF FN'+Copy(NewText, 6, 999999);

  Extras := '';
  Bold := False;
  Italic := False;
  If Opt_SyntaxHighlight and not LineBreak Then Begin
     Paper := -1;
     If REMLine Then Begin // A Comment
        If Opt_HighlightComments Then Begin
           Ink := Opt_CommentsColour;
           Bold := Opt_CommentsBold;
           Italic := Opt_CommentsItalic;
        End;
     End Else Begin
        If InString or (NewText[1] = '"') Then Begin // A string literal
           If Opt_HighlightStrings Then Begin
              Ink := Opt_StringsColour;
              Bold := Opt_StringsBold;
              Italic := Opt_StringsItalic;
              InString := True;
           End;
        End Else Begin
           If Not (Curword[1] in ['"', '$', '0'..'9', 'A'..'Z', 'a'..'z']) Then Begin // A Symbol
              If Opt_HighlightSymbols Then Begin
                 Ink := Opt_SymbolsColour;
                 Bold := Opt_SymbolsBold;
                 Italic := Opt_SymbolsItalic;
              End;
           End Else Begin
              If CompareStrToSubStr('FN', LastWord, Length(LastWord) -1, 2) Then Begin // A Function
                 If Opt_HighlightFunctions Then Begin
                    Ink := Opt_FunctionsColour;
                    Bold := Opt_FunctionsBold;
                    Italic := Opt_FunctionsItalic;
                 End;
              End Else Begin
                 KeyWordNum := IndexIsReserved(NewText);
                 If KeyWordNum <> 0 Then Begin // A Keyword
                    If Opt_HighlightKeywords Then Begin
                       Ink := Opt_KeywordsColour;
                       Bold := Opt_KeywordsBold;
                       Italic := Opt_KeywordsItalic;
                    End;
                    Keyword := Copy(Keywords[KeywordNum], 1, Pos('-', Keywords[KeywordNum])-1);
                    If Length(NewText) > Length(Keyword) then Begin
                       If Length(NewText) > Length(CurWord) Then Begin
                          Extras := Copy(CurWord, Length(Keyword), 999999);
                          CurWord := Copy(CurWord, 1, Length(Keyword)-1);
                       End Else Begin
                          Extras := Copy(CurWord, Length(Keyword)+1, 999999);
                          CurWord := Copy(CurWord, 1, Length(Keyword));
                       End;
                    End;
                 End Else Begin
                    If IsNumber(NewText) Then Begin
                       If XPos < 40 Then Begin // LineNumber
                          If Opt_HighlightLineNums Then Begin
                             Ink := Opt_LineNumsColour;
                             Bold := Opt_LineNumsBold;
                             Italic := Opt_LineNumsItalic;
                          End;
                       End Else Begin // A number in the code
                          If Opt_HighlightNumbers Then Begin
                             Ink := Opt_NumbersColour;
                             Bold := Opt_NumbersBold;
                             Italic := Opt_NumbersItalic;
                          End;
                       End;
                    End Else Begin
                       If VariablesWindow.IsVariable(NewText) Then Begin // A variable - defined in VARS space
                          If Opt_HighlightVars Then Begin
                             Ink := Opt_VarsColour;
                             Bold := Opt_VarsBold;
                             Italic := Opt_VarsItalic;
                          End;
                       End Else Begin // Whereas this is an undefined variable.
                          If Opt_HighlightVarsUnDef Then Begin
                             Ink := Opt_VarsUnDefColour;
                             Bold := Opt_VarsUnDefBold;
                             Italic := Opt_VarsUnDefItalic;
                          End;
                       End;
                    End;
                 End;
              End;
           End;
        End;
     End;
     AnchorX := XPos;
     If Not InString Then Begin
        If DoPaint Then Begin
           If Bold Then Begin
              SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
              SpecTextToDIB(DIB, XPos +1, YPos +1, CurWord, Ink, -1, Bright, Italic, InString);
           End Else
              SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
        End;
     End Else Begin // Quotes are a pain in the Arse.
        If CurWord[1] = '"' Then Begin
           If DoPaint Then Begin
              If Bold Then Begin
                 SpecTextToDIB(DIB, XPos, YPos +1, '"', Ink, Paper, Bright, Italic, False);
                 SpecTextToDIB(DIB, XPos +1, YPos +1, '"', Ink, -1, Bright, Italic, False);
              End Else
                 SpecTextToDIB(DIB, XPos, YPos +1, '"', Ink, Paper, Bright, Italic, False);
           End;
           CurWord := Copy(CurWord, 2, 999999);
           Inc(XPos, 8);
        End;
        If CurWord <> '' Then
           If CurWord[Length(CurWord)] = '"' Then Begin
              CurWord := Copy(CurWord, 1, Length(CurWord)-1);
              If CurWord <> '' Then Begin
                 If DoPaint Then Begin
                    If Bold Then Begin
                       SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
                       SpecTextToDIB(DIB, XPos +1, YPos +1, CurWord, Ink, -1, Bright, Italic, InString);
                    End Else
                       SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
                 End;
                 Inc(XPos, Length(CurWord)*8);
              End;
              If DoPaint Then Begin
                 If Bold Then Begin
                    SpecTextToDIB(DIB, XPos, YPos +1, '"', Ink, Paper, Bright, Italic, False);
                    SpecTextToDIB(DIB, XPos +1, YPos +1, '"', Ink, -1, Bright, Italic, False);
                 End Else
                    SpecTextToDIB(DIB, XPos, YPos +1, '"', Ink, Paper, Bright, Italic, False);
              End;
           End Else If DoPaint Then Begin
              If Bold Then Begin
                 SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
                 SpecTextToDIB(DIB, XPos +1, YPos +1, CurWord, Ink, -1, Bright, Italic, InString);
              End Else
                 SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, Italic, InString);
           End;
     End;
  End Else Begin
     If DoPaint Then Begin
        If XPos < 40 Then Paper := -1;
        SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Ink, Paper, Bright, False, False);
     End;
  End;

  If Extras <> '' then Begin
     Inc(XPos, Length(CurWord)*8);
     NewWord(DIB, Extras, XPos, YPos, Ink, Paper, Bright, LineBreak, REMLine, InString, DoPaint, 0);
  End Else If CurPos <> 0 Then Begin
     If EditorSelStart <> EditorSelEnd Then
        If (Integer(EditorSelStart) <= (CurPos + Length(CurWord))) and (Integer(EditorSelEnd) >= CurPos) Then Begin
           InvertFrom := (Max(EditorSelStart, CurPos) - CurPos) +1;
           InvertTo := (Min(EditorSelEnd, CurPos + Length(CurWord)-1) - CurPos) +1;
           XPos := AnchorX + 8 * (InvertFrom -1);
           CurWord := Copy(CurWord, InvertFrom, (InvertTo - InvertFrom)+1);
           If DoPaint Then
              If BASinOutput.ShowingPrediction Then
                 SpecTextToDIB(DIB, XPos, YPos +1, CurWord, 17, Opt_Background, Bright, False, False)
              Else
                 SpecTextToDIB(DIB, XPos, YPos +1, CurWord, Opt_Background, Opt_Foreground, Bright, False, InString);
        End;
  End;

  If NewText <> ' ' Then Begin
     LastWord := NewText;
     LatestWord:= LastWord;
     End;

End;

Function IsNumber(Text: AnsiString): Boolean;
Var
  Idx: Integer;
  SciFlag, GotSign: Boolean;
Begin
  SciFlag := False;
  GotSign := False;
  Result := True;
  For Idx := 1 To Length(Text) do
     If Not (Text[Idx] in ['0'..'9']) Then Begin
        If Text[Idx] in ['E', 'e'] Then Begin
           If SciFlag Then Begin
              Result := False;
              Break;
           End Else
              SciFlag := True;
        End Else
           If Text[Idx] in ['+', '-'] Then Begin
              If Not SciFlag Then Begin
                 Result := False;
                 Break;
              End Else
                 If GotSign Then Begin
                    Result := False;
                    Break;
                 End Else
                    GotSign := True;
           End Else Begin
              Result := False;
              Break;
           End;
     End;
End;


Function TBASinOutput.RepaintCursor: Boolean;
Var
  Ink, Paper, Bright: Byte;
Const
  HexChars: String = '0123456789ABCDEF';
Begin
  Result := False;
  If Running or (Screen.ActiveForm <> Self) Then Exit;
  If Integer(CursOffset) +1 <= Length(BASICMem) Then
     CursorChar := BASICMem[CursOffset];
  If CursorChar < #32 Then
     If Not Opt_SyntaxHighlight or (CursorChar = #13) Then
        CursorChar := ' ';
  Bright := 1;
  If CursorVisible Then Begin
     If CodeError Then Begin
        If CursorState Then Begin
           Ink := 2;
           Paper := 7;
        End Else Begin
           Ink := 7;
           Paper := 2;
        End;
     End Else
        If OverWriteCursor Then Begin
           Bright := 0;
           If CursorState Then Begin
              Ink := 4;
              Paper := 0;
           End Else Begin
              Ink := 0;
              Paper := 4;
           End;
        End Else
           If CursorState Then Begin
              Ink := 1;
              Paper := 7;
           End Else Begin
              Ink := 7;
              Paper := 1;
           End;

     If ProgramIs128k Then Begin
        Case CursorChar of
           #32..#143:
              DrawChar(FastIMG1.Bmp, @EditorChars[(Byte(CursorChar)-32)*8], CursorPoint.X, CursorPoint.Y, Ink, Paper, Bright);
           #144..#162:
              DrawChar(FastIMG1.Bmp, @Memory[((Byte(CursorChar)-144)*8)+GetWord(@Memory[UDG])-1], CursorPoint.X, CursorPoint.Y, Ink, Paper, Bright);
        Else
           Begin
              DrawChar(FastIMG1.Bmp, @EditorChars[1], CursorPoint.X, CursorPoint.Y, Ink, Paper, Bright);
              SmallTextOut(FastIMG1.Bmp, HexChars[(((Ord(CursorChar) div 16)*16) div 16)+1]+HexChars[(Ord(CursorChar) Mod 16)+1], CursorPoint.X, (FastIMG1.Bmp.AbsHeight - CursorPoint.Y) -7, DisplayPalette[Ink]);
           End;
        End;
     End Else Begin
        Case CursorChar of
           #32..#143:
              DrawChar(FastIMG1.Bmp, @EditorChars[(Byte(CursorChar)-32)*8], CursorPoint.X, CursorPoint.Y, Ink, Paper, Bright);
           #144..#164:
              DrawChar(FastIMG1.Bmp, @Memory[((Byte(CursorChar)-144)*8)+GetWord(@Memory[UDG])-1], CursorPoint.X, CursorPoint.Y, Ink, Paper, Bright);
        Else
           Begin
              DrawChar(FastIMG1.Bmp, @EditorChars[1], CursorPoint.X, CursorPoint.Y, Ink, Paper, Bright);
              SmallTextOut(FastIMG1.Bmp, HexChars[(((Ord(CursorChar) div 16)*16) div 16)+1]+HexChars[(Ord(CursorChar) Mod 16)+1], CursorPoint.X, (FastIMG1.Bmp.AbsHeight - CursorPoint.Y) -7, DisplayPalette[Ink]);
           End;
        End;
     End;

     If Opt_CharacterRuler Then DrawRuler;
     Result := True;
     FastIMG1.Repaint;
  End;
End;

Procedure DrawChar(DIB: TFastDIB; CharPtr: PByte; X, Y: Integer; Ink, Paper, Bright: Byte);
Var
  Bit: Byte;
  Line, Xp, Yp: DWord;
  PixelPtr, InkPtr, PaperPtr: PfColorA;

Begin

  If (X < 0) or (X > DIB.Width -(8*Opt_FontScale)) or (Y < 0) or (Y > DIB.AbsHeight -(8*Opt_FontScale)) Then Exit;

  Xp := X;
  Yp := Y;

  If Opt_FontScale = 1 Then Begin
     Y := (DIB.AbsHeight -1) - Y;
     PixelPtr := Pointer(DIB.Bits);
     Inc(PixelPtr, (Y * DIB.Width) + X);
  End Else Begin
     PixelPtr := Pointer(CharDIB.Bits);
     Y := (CharDIB.AbsHeight -1);
     Inc(PixelPtr, Y * CharDIB.Width);
  End;

  If Bright = 1 Then Begin
     Inc(Ink, 8);
     Inc(Paper, 8);
  End;

  InkPtr := @DisplayPalette[Ink];
  PaperPtr := @DisplayPalette[Paper];

  Inc(CharPtr);

  For Line := 0 To 7 Do Begin

     Bit := 128;
     While Bit > 0 Do Begin

        If Byte(CharPtr^) And Bit = 0 Then
           PixelPtr^ := PaperPtr^
        Else
           PixelPtr^ := InkPtr^;

        Bit := Bit Shr 1;
        Inc(PixelPtr);

     End;

     If Opt_FontScale = 1 Then
        Dec(PixelPtr, DIB.Width+8)
     Else
        Dec(PixelPtr, CharDIB.Width+8);
     Inc(CharPtr);

  End;

  If Opt_FontScale > 1 Then
     CharDIB.Stretch(DIB.hDc, Xp, Yp, 8*Opt_FontScale, 8*Opt_FontScale);

End;

Procedure TBASinOutput.PerformTokenIn(Tokens: String);
Begin
  If Tokens <> '' Then Begin
     AddUndo;
     If EditorSelStart <> EditorSelEnd Then Begin
        BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
        CursOffset := EditorSelStart;
        UpdateCursorPos(CursOffset, False);
     End Else Begin
        If Not InsertMode Then
           If BASICMem[CursOffset] <> #13 Then Begin
              BASICMem := Copy(BASICMem, 1, CursOffset -1)+Copy(BASICMem, CursOffset+1, 999999);
           End;
     End;
     ShowingPrediction := False;
     BASICMem := Copy(BASICMem, 1, CursOffset -1)+tokens+Copy(BASICMem, CursOffset, 999999);
     BASICChanged := True;
     UpdateCursorPos(Integer(CursOffset) +Length(Tokens), False);
     MakeSound(1);
  End;
End;

procedure TBASinOutput.CMDialogKey(var msg: TCMDialogKey);
begin
  if msg.Charcode <> VK_TAB then inherited;
end;

Procedure TBASinOutput.SendToEditor(Token: Byte);
Begin
  PerformTokenIn(TokenForm.TranslateToTokens(Token));
  RepaintBASIC(True);
  SetFocus;
End;

Procedure TBASinOutput.PerformKeyDown(Key: Word; Shift: TShiftState; Repaint: Boolean);
Var
  ColourToken: Byte;
  CurPos, PrevLine, NextLine, LineNumLen, XStart, TempPos, Idx, sndType: Integer;
  InsertChar: String;
  TempStr, KeyString, KeyChar, WordFragment: String;
	Caps, Shifted, CtrlDown, InString, REMCommand, IsDirect: Boolean;
begin

  If ProgStateFlag = PS_Reset then exit;

	Shifted := ssShift in Shift;
  CtrlDown := ssCtrl in Shift;
	Caps := DWord(GetKeyState(VK_CAPITAL) and 1) = 1;
  SetCapsLock;
  If CtrlDown Then UpdateMenu;

  CodeError := False;
  IsDirect := False;

  If Key <> VK_RETURN Then
     OverWriteCursor := False;

  ColourToken := 0;

  Case Key of

		VK_F1: // Blue
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $11  // Blue Paper
					Else
						ColourToken := $19; // Blue Ink
              SendToEditor(ColourToken);
           End Else
              If CtrlDown or Shifted Then
                 MenuItemClick(Contents1);
			End;
		VK_F2: // Red
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $12  // Red Paper
					Else
						ColourToken := $1A; // Red Ink
                                SendToEditor(ColourToken);
                                End Else Begin
                                   Opt_Indenting:= not Opt_Indenting;
                                   OptionsWindow.CheckBox30.Enabled:=True;
                                End;

			End;
		VK_F3: // Inverse Video, Magenta
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $13  // Magenta Paper
					Else
						ColourToken := $1B; // Magenta Ink
				End Else If Shifted Then
					ColourToken := $05;    // Inverse Video
           If ColourToken <> 0 Then
              SendToEditor(ColourToken);
			End;
		VK_F4: // True Video, Green
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $14  // Green Paper
					Else
						ColourToken := $1C; // Green Ink
				End Else If Shifted Then
					ColourToken := $04;    // True Video
           If ColourToken <> 0 Then
              SendToEditor(ColourToken);
			End;
		VK_F5: // Bright On, Cyan
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $15  // Cyan Paper
					Else
						ColourToken := $1D; // Cyan Ink
				End Else If Shifted Then
					ColourToken := $03;    // Bright On
           If ColourToken <> 0 Then
              SendToEditor(ColourToken);
			End;
		VK_F6: // Bright Off, Yellow
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $16  // Yellow Paper
					Else
						ColourToken := $1E; // Yellow Ink
				End Else If Shifted Then
					ColourToken := $02;    // Bright Off
           If ColourToken <> 0 Then
              SendToEditor(ColourToken);
			End;
		VK_F7: // Flash On, White
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $17  // White Paper
					Else
						ColourToken := $1F; // White Ink
				End Else If Shifted Then
					ColourToken := $01;    // Flash On
           If ColourToken <> 0 Then
              SendToEditor(ColourToken);
			End;
		VK_F8: // Flash Off, Black
			Begin
				If EditorGFXMode Then Begin
					If Shifted Then
						ColourToken := $10  // Black Paper
					Else
						ColourToken := $18; // Black Ink
				End Else If Shifted Then
              ColourToken := $00;    // Flash Off
           If ColourToken <> 0 Then
              SendToEditor(ColourToken);
			End;

     VK_TAB:
        Begin
           If Opt_EditorSounds Then MakeSound(1);
           If Not (ssShift in Shift) Then Begin
              AddUndo;
              If EditorSelStart <> EditorSelEnd Then Begin
                 ShowingPrediction := False;
                 BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
                 CursOffset := EditorSelStart;
                 UpdateCursorPos(CursOffset, False);
              End;
              TempStr := '   ';
              BASICMem := Copy(BASICMem, 1, CursOffset -1)+TempStr+Copy(BASICMem, CursOffset, 999999);
              BASICChanged := True;
              UpdateCursorPos(Integer(CursOffset) + Length(TempStr), False);
           End;
        End;
     VK_NUMLOCK:
        Begin
           If opt_GraphicsMethod = gmNumLock Then Begin
              EditorGFXMode := Not EditorGFXMode;
           End;
        End;

     VK_SCROLL:
        Begin
           If opt_GraphicsMethod = gmScrollLock Then Begin
              EditorGFXMode := Not EditorGFXMode;
           End;
        End;

     VK_ESCAPE:
        Begin
           If Opt_EditorSounds Then MakeSound(1);
           If Running and Registers.EmuRunning Then
              DisplayWindow.FormKeyDown(Nil, Key, Shift);
        End;

		VK_CONTROL:
        Begin
           // For those of you lucky enough to own an english keyboard and XP/Nt,
           // Graphics mode can be a modifier on Alt-GR.
           If opt_GraphicsMethod = gmAltGr Then Begin
              // Put Graphics mode on if it isn't already
              // Alt-Gr comes through as a CONTROL key for some reason.
              If DWord(GetASyncKeyState(VK_RMENU)) <> 0 Then Begin
                 EditorGFXMode := True;
              End;
           End Else
              inherited;
        End;

     VK_INSERT:
        Begin
           InsertMode := Not InsertMode;
        End;

     VK_RETURN:
        Begin // Pressing Return indicates that you have finished your current line and
              // would like it checked and inserted into the program.

           AddUndo;
           If ShowingPrediction Then
              If EditorSelStart <> EditorSelEnd Then Begin
                 ShowingPrediction := False;
                 BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
              End;
           If (Not (Registers.EmuRunning or Running)) Then Begin
              If BASICChanged Then Begin
                 If Not TestLine('', True, IsDirect) Then Begin
                    RepaintBASIC(True);
                    If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
                    MakeCursorVisible;
                    Exit;
                 End Else Begin
                    If Opt_EditorSounds Then MakeSound(2);
                 End;
              End Else Begin
                 Idx := 1;
                 While Idx < Length(BASICMem)-1 Do Begin
                    If BASICMem[Idx] = #13 Then
                       If BASICMem[Idx+1] = #13 Then Begin
                          Delete(BASICMem, Idx, 1);
                          If Idx <= Integer(CursOffset) Then
                             Dec(CursOffset);
                          If Idx <= CursLineStart Then
                             Dec(CursLineStart);
                          If Idx <= CursProgLineStart Then
                             Dec(CursProgLineStart);
                          Dec(Idx);
                    End;
                    Inc(Idx);
                 End;
              End;
              If BASICMem = '' Then Begin // Did the command or line obliterate the BASIC memory?
                 BASICMem := #13;
                 RepaintBASIC(True);
                 Exit;
              End;
              If ((BASICMem = #13) and (CursOffset = 1)) or
                 (Copy(BASICMem, CursOffset -1, 2) = #13#13) Then Begin
                 RepaintBASIC(False);
                 ScrollBox1.HorzScrollBar.Position := 0;
                 MakeCursorVisible;
                 Exit;
              End;
              While BASICMem[CursOffset] <> #13 Do
                 UpdateCursorPos(CursOffset+1, False);
              KeyString := GetCharFromVKey(Key);
              If KeyString <> '' Then Begin
                 InsertChar := KeyString[1];
                 If InsertChar <> '' Then Begin
                    If EditorSelStart <> EditorSelEnd Then Begin
                       ShowingPrediction := False;
                       BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
                    End;
                    BASICMem := Copy(BASICMem, 1, CursOffset -1)+InsertChar+Copy(BASICMem, CursOffset, 999999);
                    UpdateCursorPos(CursOffset+1, False);
                 End;
              End;
           End;
           ScrollBox1.HorzScrollBar.Position := 0;
        End;

     VK_LEFT:
        Begin
           CurPos := CursOffset;
           If ShowingPrediction Then ClearPrediction;
           If (Integer(CursOffset) = CursLineStart) Then Begin
              If BASICChanged Then sndType := 2 else sndType := 1;
              If Not TestLine('', True, IsDirect) Then Begin
                 UpdateCursorPos(CurPos, False);
                 RepaintBASIC(False);
                 If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
                 Exit;
              End Else Begin
                 If CursOffset > 1 Then Begin
                    UpdateCursorPos(CursOffset-1, Shifted);
                 End;
                 If Opt_EditorSounds and Not IsDirect Then MakeSound(sndType);
              End;
           End Else Begin
              If CursOffset > 1 Then Begin
                 UpdateCursorPos(CursOffset-1, Shifted);
              End;
              If Opt_EditorSounds Then MakeSound(1);
           End;
        End;

     VK_RIGHT:
        Begin
           CurPos := CursOffset;
           If ShowingPrediction and (CursOffset < EditorSelEnd) Then Begin
              AddUndo;
              AcceptPrediction;
              Exit;
           End;
           If (BASICMem[CursOffset] = #13) Then Begin
              // If this is a direct command, then we should just stop and not move.
              Idx := CursOffset;
              Repeat
                 Dec(Idx);
              Until (Idx = 0) or (BASICMem[Idx] = #13);
              Inc(Idx);
              IsDirect := True;
              If BASICMem[Idx] in ['0'..'9'] Then Begin
                 While BASICMem[Idx] in ['0'..'9'] Do Inc(Idx);
                 While BASICMem[Idx] = ' ' Do Inc(Idx);
                 If BASICMem[Idx] in ['A'..'Z', 'a'..'z'] Then IsDirect := False;
              End;
              If Not IsDirect Then Begin
                 If BASICChanged then
                    sndType := 2
                 Else
                    sndType := 1;
                 If Not TestLine('', True, IsDirect) Then Begin
                    UpdateCursorPos(CurPos, False);
                    If Opt_EditorSounds Then
                       If Not IsDirect Then MakeSound(3);
                    Exit;
                 End Else Begin
                    If Integer(CursOffset) < Length(BASICMem) Then Begin
                       UpdateCursorPos(CursOffset+1, Shifted);
                    End;
                    If Opt_EditorSounds and Not IsDirect Then MakeSound(sndType);
                 End;
              End;
           End Else Begin
              If Integer(CursOffset) < Length(BASICMem) Then Begin
                 UpdateCursorPos(CursOffset+1, Shifted);
              End;
              If Opt_EditorSounds Then MakeSound(1);
           End;
        End;

     VK_UP:
        Begin // I'm not entirely sure how this works.
           If ShowingPrediction Then ClearPrediction;
           If (ViewLine = 0) And (CursorPoint.y = 0) Then Exit;
           XStart := (-ViewColumn * (8*Opt_FontScale)) +(8*Opt_FontScale);
           CurPos := CursPrevLineStart;
           sndType := 1;
           If (BASICMem[CursLineStart] in [#13, '0'..'9']) Then
              If CursorChar <> '?' Then Begin
                 If BASICChanged Then
                    sndType := 2;
                 If Not TestLine('', True, IsDirect) Then Begin
                    If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
                    Exit;
                 End Else
                    If IsDirect Then sndType := 1;
              End;
           If BASICMem[CurPos] in ['0'..'9'] Then Begin
              LineNumLen := 0;
              While BASICMem[CurPos] in ['0'..'9'] Do Begin
                 Inc(LineNumLen);
                 Inc(CurPos);
              End;
              If LineNumLen < 4 Then
                 Inc(XStart, (32*Opt_FontScale) - Min((LineNumLen * (8*Opt_FontScale)), (32*Opt_FontScale)));
           End Else
              Inc(XStart, (32*Opt_FontScale));
           CurPos := CursPrevLineStart;
           While (XStart < CursorPoint.X) and (CurPos < (CursLineStart -1)) Do Begin
              Inc(CurPos);
              If (BASICMem[CurPos] >= ' ') or (Opt_SyntaxHighlight) Then
                 Inc(XStart, (8*Opt_FontScale));
           End;
           UpdateCursorPos(CurPos, Shifted);
           If SndType = 1 Then Begin
              If Opt_EditorSounds Then
                 MakeSound(sndType);
           End Else
              If Opt_EditorSounds and Not IsDirect Then
                 MakeSound(sndType);
        End;

     VK_DOWN:
        Begin
           If ShowingPrediction Then ClearPrediction;
           sndType := 1;
           InString := False;
           CurPos := CursLineStart;
           While Uppercase(BASICMem[CurPos]) < 'A' Do Inc(CurPos);
           REMCommand := Uppercase(Copy(BASICMem, CurPos, 4)) = 'REM ';
           CurPos := CursLineStart;
           If CurPos < Length(BASICMem) Then
           While Not ((BASICMem[CurPos] = #13) or
              (Not Instring and Not REMCommand and (BASICMem[CurPos] in ['n', 'N']) and (Uppercase(Copy(BASICMem, CurPos-3, 5)) = 'THEN ')) or
              (Not REMCommand and (BASICMem[CurPos] = ':') and SplitStatements and Not Instring) or
              (CurPos >= Length(BASICMem))) Do Begin
              If BASICMem[CurPos] = '"' Then InString := Not InString;
              Inc(CurPos);
           End;
           If ((BASICMem[CurPos] = #13) or (CurPos >= Length(BASICMem))) Then Begin
              If BASICChanged Then
                 sndType := 2;
              If Not TestLine('', True, IsDirect) Then Begin
                 If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
                 Exit;
              End Else
                 If IsDirect Then sndType := 1;
           End;
           If CurPos >= Length(BASICMem) Then Exit;
           Inc(CurPos);
           XStart := (-ViewColumn * (8*Opt_FontScale))+(8*Opt_FontScale);
           CursOffset := CurPos;
           If BASICMem[CurPos] in ['0'..'9'] Then Begin
              LineNumLen := 0;
              While BASICMem[CurPos] in ['0'..'9'] Do Begin
                 Inc(LineNumLen);
                 Inc(CurPos);
              End;
              If LineNumLen < 4 Then
                 Inc(XStart, (32*Opt_FontScale) - Min((LineNumLen * (8*Opt_FontScale)), (32*Opt_FontScale)));
           End Else
              Inc(XStart, (32*Opt_FontScale));
           InString := False;
           CurPos := CursOffset;
           While Uppercase(BASICMem[CurPos]) < 'A' Do Inc(CurPos);
           REMCommand := REMCommand or (Uppercase(Copy(BASICMem, CurPos, 4)) = 'REM ');
           CurPos := CursOffset;
           While (BASICMem[CurPos] <> #13) and
              Not ((BASICMem[CurPos] = ':') and SplitStatements and Not InString and Not REMCommand) and
              Not (Not Instring and Not REMCommand and (BASICMem[CurPos] in ['n', 'N']) and (Uppercase(Copy(BASICMem, CurPos-3, 5)) = 'THEN ')) and
              (XStart < CursorPoint.X) Do Begin
              Inc(CurPos);
              If (BASICMem[CurPos] >= ' ') or (Opt_SyntaxHighlight) Then Begin
                 Inc(XStart, (8*Opt_FontScale));
                 If BASICMem[CurPos] = '"' Then InString := Not InString;
              End;
           End;
           UpdateCursorPos(CurPos, Shifted);
           If Opt_EditorSounds and Not IsDirect Then MakeSound(sndType);
        End;

     VK_HOME:
        Begin
           If ShowingPrediction Then ClearPrediction;
           If CtrlDown Then Begin
              If Not TestLine('', True, IsDirect) Then Begin
                 If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
                 Exit;
              End Else
                 UpdateCursorPos(1, Shifted);
           End Else
              UpdateCursorPos(CursLineStart, Shifted);
           If Opt_EditorSounds Then MakeSound(1);
        End;

     VK_END:
        Begin
           If ShowingPrediction Then ClearPrediction;
           If Not CtrlDown Then Begin
              InString := False;
              CurPos := CursLineStart -1;
              REMCommand := False;
              Repeat
                 Inc(CurPos);
                 If BASICMem[CurPos] = '"' Then InString := Not InString;
                 If Uppercase(Copy(BASICMem, CurPos, 4)) = 'REM ' Then REMCommand := True;
              Until (BASICMem[CurPos] = #13) or
                    (Not REMCommand and (BASICMem[CurPos] = ':') and SplitStatements and Not InString) or
                    (Not REMCommand and Not Instring and (BASICMem[CurPos] in ['n', 'N']) and (Uppercase(Copy(BASICMem, CurPos-3, 5)) = 'THEN '));
              UpdateCursorPos(CurPos, Shifted);
           End Else
              If Not TestLine('', True, IsDirect) Then Begin
                 If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
                 Exit;
              End Else
                 UpdateCursorPos(Length(BASICMem), Shifted);
           If Opt_EditorSounds Then MakeSound(1);
        End;

     VK_PRIOR:
        Begin
           If ShowingPrediction Then ClearPrediction;
           If BASICChanged Then
              sndType := 2
           Else
              sndType := 1;
           If Not TestLine('', True, IsDirect) Then Begin
              If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
              Exit;
           End;
           PageMove := True;
           ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - (FastIMG1.Height - (16*(8 * Opt_FontScale)));
           ViewLine := ScrollBox1.VertScrollBar.Position Div (8*Opt_FontScale);
           UpdateCursorPos(ViewOffset, Shifted);
           PageMove := False;
           If Opt_EditorSounds and Not IsDirect Then MakeSound(sndType);
        End;

     VK_NEXT:
        Begin
           If ShowingPrediction Then ClearPrediction;
           If BASICChanged Then
              sndType := 2
           Else
              sndType := 1;
           If Not TestLine('', True, IsDirect) Then Begin
              If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
              Exit;
           End;
           PageMove := True;
           ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + (FastIMG1.Height - (16*(8* Opt_FontScale)));
           ViewLine := ScrollBox1.VertScrollBar.Position Div (8*( Opt_FontScale));
           UpdateCursorPos(CursPageEnd, Shifted);
           PageMove := False;
           If Opt_EditorSounds and Not IsDirect Then MakeSound(sndType);
        End;

     VK_BACK:
        Begin
           If (Not (Registers.EmuRunning or Running)) And (CursOffset > 1) and (BASICMem[CursOffset -1] <> #13) Then Begin
              AddUndo;
              If EditorSelStart = EditorSelEnd Then Begin
                 If (Integer(CursOffset) <= CursProgLineStart) and (BASICMem[CursOffset] in ['0'..'9']) Then Exit;
                 BASICMem := Copy(BASICMem, 1, CursOffset -2)+Copy(BASICMem, CursOffset, 999999);
              End Else Begin
                 ShowingPrediction := False;
                 BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
                 CursOffset := EditorSelStart +1;
              End;
              BASICChanged := True;
              UpdateCursorPos(CursOffset -1, False);
              If Opt_EditorSounds Then MakeSound(1);
           End;
        End;

     VK_DELETE:
        Begin
           If (Not (Registers.EmuRunning or Running)) And (Integer(CursOffset) < Length(BASICMem)) and (BASICMem[CursOffset] <> #13) Then Begin
              AddUndo;
              BASICChanged := True;
              If EditorSelStart = EditorSelEnd Then Begin
                 BASICMem := Copy(BASICMem, 1, CursOffset -1)+Copy(BASICMem, CursOffset+1, 999999);
              End Else Begin
                 ShowingPrediction := False;
                 BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
                 LastLineBuffer := BASICMem;
                 CursOffset := EditorSelStart;
                 UpdateCursorPos(CursOffset, False);
                 LabelDebug.Caption :='*1' + LastLineBuffer  ;
              End;
              If Opt_EditorSounds Then MakeSound(1);
           End;
        End;
  Else
     If (Not (Registers.EmuRunning or Running)) Then Begin
	      If Not EditorGFXMode Then Begin

           If Key in [48..57] Then Begin
              // Ctrl-Shift-number sets a source marker
              // Ctrl-number jumps to a source marker.
              // Result remains $FF as no key is passed to the editor as such.
              If CtrlDown Then Begin
                 If Shifted Then Begin
                    BASinOutput.SetSourceMarker(Key - 48);
                 End Else Begin
                    BASinOutput.GetSourceMarker(Key - 48);
                 End;
                 //Exit;  //fixed *again* by arda :) as this prevents altgr to be
                         // used as it's normal operation when using niche keyboard layouts
              End;
           End;

           KeyChar := GetCharFromVKey(Key);
           If KeyChar = '' Then Begin
              RepaintBASIC(False);
              UpdateParseText;
              MakeCursorVisible;
              Exit;
           End;
           Key := Ord(KeyChar[1]);
		      Case Key Of

			      65..90:
				      Begin
					      If Memory[MODE] <> 2 Then Begin
						      If Not Shifted And Not Caps Then Inc(Key, 32);
                    End Else Begin
						      If Key < $56 Then
							      Inc(Key, $4F)
						      Else
                          Key := $FF;
                    End;
				      End;
           Else
              If Key = 96 Then Key := 127;
              If Key = 163 Then Key := 96;
              If Key > 126 Then Key := 127;
		      End;
	      End Else Begin
		      Case Key Of
			      49: If Not Shifted Then Key := $81 Else Key := $8E;
			      50: If Not Shifted Then Key := $82 Else Key := $8D;
			      51: If Not Shifted Then Key := $83 Else Key := $8C;
			      52: If Not Shifted Then Key := $84 Else Key := $8B;
			      53: If Not Shifted Then Key := $85 Else Key := $8A;
			      54: If Not Shifted Then Key := $86 Else Key := $89;
			      55: If Not Shifted Then Key := $87 Else Key := $88;
			      56: If Not Shifted Then	Key := $80 Else Key := $8F;
			      65..90: If Key < $56 Then Inc(Key, $4F) Else Key := $FF;
           Else
              Key := $FF;
          	End;
           If ProgramIs128k Then
              If Chr(Key and 255) in [#163, #164] Then
                 Key := $FF;
		   End;
        If Key <> $FF Then KeyString := Chr(Key);
        If KeyString <> '' Then Begin
           AddUndo;
           If ShowingPrediction and (KeyString = '.') Then Begin
              AcceptPrediction;
              Exit;
           End;
           If EditorSelStart <> EditorSelEnd Then Begin
              ShowingPrediction := False;
              BASICMem := Copy(BASICMem, 1, EditorSelStart -1)+Copy(BASICMem, EditorSelEnd, 999999);
              CursOffset := EditorSelStart;
              UpdateCursorPos(CursOffset, False);
           End;
           If Not InsertMode Then
              If BASICMem[CursOffset] <> #13 Then Begin
                 BASICMem := Copy(BASICMem, 1, CursOffset -1)+Copy(BASICMem, CursOffset+1, 999999);
              End;
           If Opt_AutoList And (KeyString = '.') Then Begin
              If (Not (Registers.EmuRunning or Running)) Then Begin
                 CurPos := CursOffset;
                 If ((BASICMem[CurPos] = #13) and (BASICMem[CurPos-1] = #13)) or (CurPos = 1) Then Begin
                    // Ok for the start of a new line, so find the previous line number
                    While (BASICMem[CurPos] = #13) and (CurPos > 1) Do Dec(CurPos);
                    While (BASICMem[CurPos] <> #13) and (CurPos > 1) Do Dec(CurPos);
                    If BASICMem[CurPos] = #13 Then Inc(CurPos);
                    TempStr := '';
                    While BASICMem[CurPos] in ['0'..'9'] Do Begin
                       TempStr := TempStr + BASICMem[CurPos];
                       Inc(CurPos);
                    End;
                    If CurPos = 2 Then
                       PrevLine := 0
                    Else
                       PrevLine := StrToIntDef(TempStr, -1);
                    If PrevLine > -1 Then Begin
                       // Now find the next.
                       CurPos := CursOffset;
                       While (BASICMem[CurPos] = #13) and (CurPos < Length(BASICMem)) Do Inc(CurPos);
                       If CurPos = Length(BASICMem) Then
                          NextLine := PrevLine + 20
                       Else Begin
                          TempStr := '';
                          While BASICMem[CurPos] in ['0'..'9'] Do Begin
                             TempStr := TempStr + BASICMem[CurPos];
                             Inc(CurPos);
                          End;
                          NextLine := StrToIntDef(TempStr, -1);
                       End;
                       // And finally, calculate the new line number
                       TempStr := IntToStr(Min(10000, PrevLine + ((NextLine - PrevLine) Div 2)));
                       If TempStr = '10000' Then TempStr := '.';
                       BASICChanged := True;
                       BASICMem := Copy(BASICMem, 1, CursOffset -1)+TempStr+Copy(BASICMem, CursOffset, 999999);
                       Inc(CursOffset, Length(TempStr));
                    End Else Begin
                       BASICMem := Copy(BASICMem, 1, CursOffset -1)+'.'+Copy(BASICMem, CursOffset, 999999);
                       BASICChanged := True;
                       Inc(CursOffset);
                    End;
                 End Else Begin
                    BASICMem := Copy(BASICMem, 1, CursOffset -1)+'.'+Copy(BASICMem, CursOffset, 999999);
                    BASICChanged := True;
                    Inc(CursOffset);
                 End;
                 UpdateCursorPos(CursOffset, False);
                 If Opt_EditorSounds Then MakeSound(1);
              End;
           End Else Begin
              InsertChar := KeyString[1];
              If InsertChar <> '' Then Begin
                 TempStr := InsertChar;
                 If InsertChar = '(' Then // Test for Automatic bracket placement
                    Case Opt_AutoBracket of
                       bmAutoType:
                          Begin
                             If BASICMem[CursOffset] in [#13, ')'] Then
                                TempStr := TempStr + ')';
                          End;
                       bmComplete:
                          Begin
                             Inc(BracketLevel);
                          End;
                    End;
                 If Opt_AutoBracket = bmComplete Then
                    If InsertChar = ')' Then
                       Dec(BracketLevel);
                 WordFragment := '';
                 If BASICMem[CursOffset] = #13 Then // only at the end of a line
                    If CursStringStart = 0 Then // and not in string literals
                       If Opt_Predictive Then // and of course, it's got to be enabled
                          If TempStr[1] in [#32, #48..#57, #65..#90, #97..#122] Then Begin // and be an alpha char to start with
                             // Get the current word
                             TempPos := CursOffset -1;
                             WordFragment := TempStr;
                             While (TempPos > 0) and (BASICMem[TempPos] in [#32, #48..#57, #65..#90, #97..#122]) Do Begin
                                If BASICMem[TempPos] = ' ' Then Begin
                                   If Uppercase(Copy(BASICMem, TempPos-2, 2)) = 'GO' Then
                                      WordFragment := 'GO '+WordFragment;
                                   If Uppercase(Copy(BASICMem, TempPos-3, 3)) = 'DEF' Then
                                      WordFragment := 'DEF '+WordFragment;
                                   Break;
                                End;
                                WordFragment := BASICMem[TempPos]+WordFragment;
                                Dec(TempPos);
                             End;
                             If CursorType = 'K' Then
                                WordFragment := GetPredictiveText(WordFragment, 0)
                             Else
                                If CursorType = '!' Then
                                   WordFragment := GetPredictiveText(WordFragment, 2)
                                Else
                                   If CursorType in ['C', 'L'] Then
                                      WordFragment := GetPredictiveText(WordFragment, 1)
                                   Else
                                      WordFragment := '';
                             If WordFragment <> '' Then Begin
                                If TempStr[1] in [#97..#122] Then
                                   WordFragment := Lowercase(WordFragment);
                                WordFragment := TempStr+Copy(WordFragment, Length(TempStr)+1, 999999);
                             End;
                          End;
                 If WordFragment <> '' Then Begin
                    BASICMem := Copy(BASICMem, 1, CursOffset -1)+WordFragment+Copy(BASICMem, CursOffset, 999999);
                    UpdateCursorPos(CursOffset +1, True);
                    EditorSelLength := Length(WordFragment) -1;
                    EditorSelAnchor := CursOffset;
                    ShowingPrediction := True;
                    If Opt_EditorSounds Then MakeSound(1);
                 End Else Begin
                    BASICMem := Copy(BASICMem, 1, CursOffset -1)+TempStr+Copy(BASICMem, CursOffset, 999999);
                    UpdateCursorPos(CursOffset +1, False);
                    ShowingPrediction := False;
                    If Opt_EditorSounds Then MakeSound(1);
                 End;
                 BASICChanged := True;
              End;
           End;
        End;
     End;
  End;


  RepaintBASIC(False);


  UpdateParseText;
  AdjustCursorPoint;
  MakeCursorVisible;

end;

Procedure TBASinOutput.MakeCursorVisible;
Var
  OldPoint: TPoint;
  debuge: Integer;
Begin
   // OutputDebugString (PChar('EVAL-'+IntToStr(FastIMG1.Bmp.Height - GetSystemMetrics(SM_CYHSCROLL)- (40*Opt_FontScale))+'hi,'+ IntToStr( CursorPoint.Y )+'y'));

   //this proc relies in CursorPoint variable


  OldPoint := Point(ViewLine, ViewColumn);
  If CursorPoint.X < (64*Opt_FontScale) Then
     ViewColumn := Max(ViewColumn - ((-CursorPoint.X+(64*Opt_FontScale)) Div (8*Opt_FontScale)), 0);
  If CursorPoint.X > FastIMG1.Bmp.Width -(64*Opt_FontScale) Then
     Inc(ViewColumn, (CursorPoint.X - (FastIMG1.Bmp.Width -(64*Opt_FontScale))) Div (8*Opt_FontScale));
  If CursorPoint.Y < (24*Opt_FontScale) Then
  Begin
     ViewLine := Max(ViewLine - ((-CursorPoint.Y+(24*Opt_FontScale)) Div (8*Opt_FontScale)), 0);
  End Else If CursorPoint.Y  > (FastIMG1.Bmp.Height - GetSystemMetrics(SM_CYHSCROLL)) - (40*Opt_FontScale) Then   Begin

      debuge:=(CursorPoint.Y-(FastIMG1.Bmp.Height - GetSystemMetrics(SM_CYHSCROLL) - (40*Opt_FontScale))) Div (8*Opt_FontScale);

     Inc(ViewLine, debuge);

     End;

  ScrollBox1.VertScrollBar.Position := ViewLine * (8*Opt_FontScale);
  ScrollBox1.HorzScrollBar.Position := ViewColumn * (8*Opt_FontScale);
  RepaintBASIC(True);
End;

Function TBASinOutput.GetCharPos(X, Y: Integer): DWord;
Var
  InString, REMCommand: Boolean;
  indentX, LineNumLen, CurPos, XStart, YPos: Integer;
Begin
  YPos := 0;
  InString := False;
  CurPos := ViewOffset;
  REMCommand := False;
  indentX:= 0;

  If YPos+(8*Opt_FontScale) < Y Then Begin
     While (CurPos < Length(BASICMem)) and (YPos+(8*Opt_FontScale) < Y) Do Begin
        Inc(CurPos);
        If Not InString Then
           If BASICMem[CurPos] in ['R', 'r'] Then
              If (UpperCase(Copy(BASICMem, CurPos, 4)) = 'REM ') Then REMCommand := True;
        If BASICMem[CurPos] = '"' Then InString := Not InString;
        If BASICMem[CurPos] = #13 Then Begin
           indentX:=0;
           Inc(YPos, (8*Opt_FontScale));
           REMCommand := False;
        End;
        If (BASICMem[CurPos] = ':') and SplitStatements and Not InString and Not REMCommand Then Inc(YPos, (8*Opt_FontScale));
        If Not Instring and Not REMCommand Then
           If (BASICMem[CurPos] in ['n', 'N']) Then Begin
              If (Uppercase(Copy(BASICMem, CurPos-3, 5)) = 'THEN ') Then Begin
                  Inc (indentX);
                  Inc(YPos, (8*Opt_FontScale));
              End;
           End Else If  (BASICMem[CurPos] in ['r', 'R']) Then Begin
              If (Uppercase(Copy(BASICMem, CurPos-2, 4)) = 'FOR ') Then Begin
                  Inc (indentX);
              End;
           End Else If  (BASICMem[CurPos] in ['t', 'T']) Then Begin
              If (Uppercase(Copy(BASICMem, CurPos-3, 5)) = 'NEXT ') Then Begin
                  Dec (indentX);
              End;
           End;
     End;
     Inc(CurPos);
  End;
  If CurPos >= Length(BASICMem) Then Begin
     CurPos := Length(BASICMem);
     Result := CurPos;
  End Else Begin
     Result := CurPos;
     XStart :=  -ViewColumn * (8*Opt_FontScale);  //(indentX*Opt_Indentsize*8)+
     If BASICMem[CurPos] in ['0'..'9'] Then Begin
        LineNumLen := 0;
        While BASICMem[CurPos] in ['0'..'9'] Do Begin
           Inc(LineNumLen);
           Inc(CurPos);
        End;
        Inc(XStart, (40*Opt_FontScale) - Min((LineNumLen * (8*Opt_FontScale)), (32*Opt_FontScale)));
     End Else
        Inc(XStart, (40*Opt_FontScale));//+(8*Opt_Indentsize*indentX));
     InString := False;
     CurPos := Result;
     While Uppercase(BASICMem[CurPos]) < 'A' Do Inc(CurPos);
     REMCommand := REMCommand or ((BASICMem[CurPos] in ['R', 'r']) and (Uppercase(Copy(BASICMem, CurPos, 4)) = 'REM '));
     CurPos := Result;

     if Opt_Indenting Then Dec(X, ((Opt_FontScale*8)*Opt_Indentsize*indentX));   // arda ident fix

     While (BASICMem[CurPos] <> #13) and
           Not ((BASICMem[CurPos] = ':') and SplitStatements and Not InString and Not REMCommand) and
           Not (Not Instring and Not REMCommand and (BASICMem[CurPos] in ['n', 'N']) and (Uppercase(Copy(BASICMem, CurPos-3, 5)) = 'THEN ')) and
           ((XStart) < X-(8*Opt_FontScale)) Do Begin
        Inc(CurPos);
        If (BASICMem[CurPos] >= ' ') or (Opt_SyntaxHighlight) Then Begin
           Inc(XStart, (8*Opt_FontScale));
           If BASICMem[CurPos] = '"' Then InString := Not InString;
        End;
     End;
     Result := CurPos ;
  End;
end;

procedure TBASinOutput.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ViewLine := Max(0, Min((ScrollBox1.VertScrollBar.Range Div (8*Opt_FontScale)) - (FastIMG1.Height Div (8*Opt_FontScale)), ViewLine +3));
  ScrollBox1.VertScrollBar.Position := ViewLine * (8*Opt_FontScale);
  RepaintBASIC(True);
  RepaintCursor;
end;

procedure TBASinOutput.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  If ViewLine > 3 Then
     ViewLine := ViewLine -3
  Else
     ViewLine := 0;
  ScrollBox1.VertScrollBar.Position := ViewLine * (8*Opt_FontScale);
  RepaintBASIC(True);
  RepaintCursor;
end;

procedure TBASinOutput.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  CurPos, sndType: Integer;
  isDirect: Boolean;
begin
  sndType := 1;
  IsDirect := False;
  If (Button = mbLeft) and (ViewOffset > 0) Then Begin
     CurPos := GetCharPos(X, Y);
     If CurPos <> Integer(CursOffset) Then Begin
        If Not Running Then
           If (CurPos < CursProgLineStart) or (CurPos > CursProgLineEnd) Then Begin
               If BASICChanged Then sndType := 2;
               If Not TestLine('', True, IsDirect) Then Begin
                 If Opt_EditorSounds Then If Not IsDirect Then MakeSound(3);
                 Exit;
               End Else
                 If IsDirect Then sndType := 1;
               End;
            UpdateCursorPos(CurPos, ssShift in Shift);
            RepaintBASIC(True);
            If Opt_EditorSounds and Not IsDirect Then MakeSound(sndType);

            UpdateParseText;
            MakeCursorVisible;

           End;
       MouseDown := True;
     End;
  MousePos.X := X;
  MousePos.Y := Y;
End;

procedure TBASinOutput.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If (DWord(GetKeyState(VK_LMENU)) <= 1) Then Begin
     PerformKeyDown(Key, Shift, True);
  End;
  Inherited;
end;

procedure TBASinOutput.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Opt_GraphicsMethod = gmALtGr Then
     If Key = VK_CONTROL Then
        If GetKeyState(VK_RMENU) < 2 Then
           If EditorGFXMode Then Begin
              EditorGFXMode := False;
              UpdateParseText;
           End;


  //when f1 keydownevent doesn't fire - total workaround
  If (Not EditorGFXMode) And (Key=VK_F1) Then Begin
       MenuItemClick(CommandHelp1);
  End;
  StatusBar1.Repaint;
  Inherited;
end;

procedure TBASinOutput.Timer1Timer(Sender: TObject);
begin
  if (DumpDoneQuit) Then Close;

  CursorState := Not CursorState;
  If Screen.ActiveForm = Self Then Begin
     RepaintCursor;
     NonActivePaint := False;
  End Else
     If Not NonActivePaint Then Begin
        RepaintBASIC(True);
        FastIMG1.Repaint;
        NonActivePaint := True;
     End;
end;

Procedure TBASinOutput.PopUp;
Begin
  If PleaseContinue Then Begin
     DoContinue;
  End Else Begin
     If AddCodeWindow.Memo1.Lines.Count > 0 Then
        If Not AddCodeWindow.PasteLines Then Begin
           CentreFormOnForm(AddCodeWindow, Self);
           ShowWindow(AddCodeWindow, True);
        End Else Begin
           VariablesWindow.BuildVarsList;
           UpdateBASinDisplay;
           Exit;
        End;
     DisplayWindow.WantsFront := False;
     ControlEmulation(False);
     BringToFront;
     GetBASIC;
     CheckFor128kCommands;
     VariablesWindow.BuildVarsList;
     RepaintBASIC(True);
     ProgStateFlag := PS_Unknown;
     UpdateParseText;
     UpdateRuntimeButtons;
     UpdateDisplay;
     UpdateBASinDisplay;
     If WantNewline Then Begin
        BASICChanged := False;
        PerformKeyDown(VK_RETURN, [], True);
        WantNewLine := False;
     End;
     If TokenForm.Visible Then
        TokenForm.PageControl1Change(nil);
     If UDGWindow.FirstRun and UDGWindow.Visible Then
        UDGWindow.FormShow(nil);
     VariablesWindow.Button1.Enabled := True;
     UpdateCursorPos(CursOffset, False);
     File1.Enabled := True;
     View1.Enabled := True;
     Edit1.Enabled := True;
     Search1.Enabled := True;
     Run1.Enabled := True;
     Tools1.Enabled := True;
     Help1.Enabled := True;
  End;
End;

procedure TBASinOutput.FastIMG1DblClick(Sender: TObject);
begin
  // Toggle a breakpoint at this line - the RepaintBASIC has already got us to the correct
  // line/statement numbers
  If BASICMem <> '' Then
     BreakpointsWindow.ToggleBreakpoint(CursLineNum, CursStatementNum);
end;

Function TBASinOutput.GetCurrentEditorLine: String;
Var
  IdxA, IdxB: Integer;
  REMCommand, InString: Boolean;
Begin

  // CursLineStart might not have been updated here - so we need to use CursOffset as a base.

  IdxA := CursLineStart;
  IdxB := IdxA;

  REMCommand := False;
  InString := False;

  Repeat
     If IdxB >= Length(BASICMem) Then Break;
     Inc(IdxB);
     If BASICMem[IdxB] = '"' Then InString := Not InString;
     If Not InString Then
        If BASICMem[IdxB] in ['R', 'r'] Then
           If (UpperCase(Copy(BASICMem, IdxB, 4)) = 'REM ') Then
              REMCommand := True;
     If ((BASICMem[IdxB] = ':') and Not InString and Not REMCommand) or
        (Not Instring and Not REMCommand and (BASICMem[IdxB -1] in ['n', 'N']) and (Uppercase(Copy(BASICMem, IdxB -4, 5)) = 'THEN ')) Then
           Break;
  Until BASICMem[IdxB] = #13;

  If ShowingPrediction Then
     Result := Copy(BASICMem, IdxA, EditorSelAnchor - IdxA)
  Else
     Result := Copy(BASICMem, IdxA, IdxB - IdxA);

  While Copy(Result, 1, 1) = #13 Do
     Result := Copy(Result, 2, 999999);

  While Copy(Result, Length(Result), 1) = #13 Do
     Result := Copy(Result, 1, Length(Result) -1);

End;

Procedure TBASinOutput.UpdateCursorPos(Offset: Integer; Shift: Boolean);
Var
  Idx: Integer;
  InString, REMCommand: Boolean;
  LineNum, StatementNum, LineAddr, LineStart: Integer;
Begin
  If Offset > Length(BASICMem) Then
     Offset := Length(BASICMem);
  CursOffset := Offset;
  LineNum := 1;
  StatementNum := 1;
  Idx := CursOffset;
  If BASICMem <> '' Then Begin
     While (Idx > 1) and (BASICMem[Idx] = #13) Do Dec(Idx);
     While (Idx > 1) and (BASICMem[Idx] <> #13) Do Dec(Idx);
     LineStart := Idx;
     If BASICMem[Idx] = #13 Then Inc(Idx);
     LineNum := 0;
     While BASICMem[Idx] in ['0'..'9'] Do Begin
        LineNum := (LineNum * 10)+Ord(BASICMem[Idx])-48;
        Inc(Idx);
     End;
     InString := False;
     REMCommand := False;
     Repeat
        Inc(Idx);
        If Not InString Then
           If BASICMem[Idx] in ['R', 'r'] Then
              If (UpperCase(Copy(BASICMem, Idx, 4)) = 'REM ') Then Begin
                 Break;
              End;
        If BASICMem[Idx] = '"' Then InString := Not InString;
        If ((BASICMem[Idx-1] = ':') and Not InString and Not REMCommand) or
           (Not Instring and Not REMCommand and (BASICMem[Idx-1] in ['n', 'N']) and (Uppercase(Copy(BASICMem, Idx-4, 5)) = 'THEN ')) Then Begin
              Inc(StatementNum);
              LineStart := Idx;
           End;
     Until (BASICMem[Idx] = #13) or (Idx >= Integer(CursOffset));
  End;
  CursLineNum := LineNum;
  CursStatementNum := StatementNum;

  If Shift Then Begin
     If Not Running Then
        If EditorSelLength < 0 Then
           EditorSelLength := -(EditorSelAnchor - Integer(CursOffset))
        Else
           EditorSelLength := Integer(CursOffset) - EditorSelAnchor;
  End Else Begin
     EditorSelAnchor := CursOffset;
     EditorSelLength := 0;
  End;
  ShowingPrediction := False;

  LineAddr := GetLineAddress(CursLineNum, CursStatementNum, 0);
  If LineAddr < 16384 Then
     LineAddr := GetWord(@Memory[PROG]);

  Label1.Caption := 'Project ' + CurProjectName + ', Line ' + IntToStr(CursLineNum) + ':' + IntToStr(CursStatementNum) + ', Char ' + IntTostr(Offset - LineStart +1) + ', Address '+IntToStr(LineAddr)+' ';

End;

Function EditorSelStart: DWord;
Begin
  If EditorSelLength < 0 Then
     Result := (EditorSelAnchor + EditorSelLength)
  Else
     Result := EditorSelAnchor;
End;

Function EditorSelEnd: DWord;
Begin
  If EditorSelLength < 0 Then
     Result := EditorSelAnchor
  Else
     Result := EditorSelAnchor + EditorSelLength;
End;

Function TBASinOutput.AddLine(Line: String): Boolean;
Var
  NewLine: String;
  Error, IsDirect: Boolean;
  ParseError: TParseError;
Begin

  Result := False;
  NewLine := FormatEscapes(Line);
  If Line = '' Then Exit;

  Error := False;
  ParseError := ParseInputLine(NewLine);
  If (ParseError.Error <> '') or (Pos(#$10+#2, ParseError.Syntax) <> 0) Then
     If (ParseError.ErrorCode = -1) or (ParseError.ErrorCode = 99) Then
        Error := True
     Else
        If ParseError.ErrorCode < 0 Then Error := True;

  If Error Then Begin
     Log('Could not paste line: '+#39+NewLine+#39);
     Exit;
  End;

  CursOffset := Max(1, Length(BASICMem));
  BASICMem := BASICMem + #13 + NewLine + #13;
  While BASICMem[CursOffset] = #13 Do Inc(CursOffset);
  CursLineStart := CursOffset;
  CursProgLineStart := CursOffset ;
  BASICChanged := True;
  Result := TestLine('', False, IsDirect);
  FormResize(nil);
  MakeCursorVisible;

End;

Function TBASinOutput.TestLine(LineAdd: String; ErrorCheck: Boolean; var IsDirect: Boolean): Boolean;
Var
  TempWord: Word;
  ParseError: TParseError;
  InString, REMCommand, Done, BlockDelete: Boolean;
  EditLine, Original, Tokenised, TempStr, LineStr, TempStr2: String;
  LineStart, LineEnd, LineNum, Idx, Idx2, StartLine, EndLine, LinePos, TempCur: Integer;
Label
  Label1;
Begin

  // I'm not proud of this proc. There has to be a much better way to do this.
  // Basically, it takes your current line, and updates the emulated BASIC space in memory with it.
  // It detects if it's a new line, a line to be replaced, or an edited line (there's a fine distinction
  // between these last two), and also handles the difference between a direct command and a
  // program line. Handles block delete and bracket completion too - though those aren't too badly done.

  Result := True;
  BlockDelete := False;
  AddUndo;

  If LineAdd = '' Then Begin

     // Get the extents of the line in the BASICMem buffer

     Idx := 1;
     While Idx < Length(BASICMem)-1 Do Begin
        If BASICMem[Idx] = #13 Then
           If BASICMem[Idx+1] = #13 Then Begin
              Delete(BASICMem, Idx, 1);
              If Idx <= Integer(CursOffset) Then
                 Dec(CursOffset);
              If Idx <= CursLineStart Then
                 Dec(CursLineStart);
              If Idx <= CursProgLineStart Then
                 Dec(CursProgLineStart);
              Dec(Idx);
        End;
        Inc(Idx);
     End;
     ViewColumn := 0;

     If Not BASICChanged Then Exit;

     Idx := CursProgLineStart;
     If BASICMem[Idx] = #13 Then Begin
        BASICChanged := False;
        IsDirect := True;
        TokeniseEditText(False);
        GetBASIC;
        CheckFor128kCommands;
        Exit;
     End Else Begin
        While (BASICMem[Idx] <> #13) and (Idx > 1) Do
           Dec(Idx);

        If BASICMem[Idx] = #13 Then Inc(Idx);
        LineStart := Idx;
        TempCur := LineStart;

        While (BASICMem[Idx] <> #13) and (Idx < Length(BASICMem)) Do
           Inc(Idx);

        LineEnd := Idx -1;

     End;

     // Now grab a copy of the line, as we're going to remove it for insertion later
     // Tokenisation can screw up the line length (and hence our cursor position) so store
     // an additional copy for comparison later.

     EditLine := Copy(BASICMem, LineStart, (LineEnd - LineStart)+1);
     TempStr := '';
     If EditLine <> '' Then
        If EditLine[1] in ['0'..'9'] Then
           While (EditLine <> '') And (EditLine[1] in ['0'..'9']) Do Begin
              TempStr := TempStr + EditLine[1];
              Delete(EditLine, 1, 1);
           End;
     If StrToIntDef(TempStr, -1) <> -1 Then
        EditLine := IntToStr(StrToInt(TempStr))+EditLine;

     Original := Uppercase(EditLine); // See later for the reason behind UpperCase

  End Else Begin

     EditLine := LineAdd;

  End;

  // Balance any remaining brackets for the autocomplete method

  If Opt_AutoBracket = bmComplete Then Begin
     BracketLevel := 0;
     Idx := 1;
     InString := False;
     REMCommand := False;
     While Idx <= Length(EditLine) Do Begin
        If EditLine[Idx] = '"' Then
           InString := Not InString;
        If Not (InString or REMCommand) Then Begin
           If EditLine[Idx] = '(' Then
              Inc(BracketLevel);
           If EditLine[Idx] = ')' Then
              Dec(BracketLevel);
           If EditLine[Idx] in ['N', 'n'] Then
              If Uppercase(Copy(EditLine, Idx -5, 7)) = 'DEF FN ' Then Begin
                 While (Idx <= Length(EditLine)) and (EditLine[Idx] <> '=') Do
                    Inc(Idx);
                 If Idx <= Length(EditLine) Then
                    If EditLine[Idx -1] <> ')' Then
                       EditLine := Copy(EditLine, 1, Idx -1)+')'+Copy(EditLine, Idx, 999999);
              End;
           If EditLine[Idx] in ['M', 'n'] Then
              If UpperCase(Copy(EditLine, Idx -2, 4)) = 'REM ' Then
                 REMCommand := True;
           If (EditLine[Idx] = ':') or (UpperCase(Copy(EditLine, Idx, 5)) = 'THEN ') or (Idx = Length(EditLine)) Then Begin
              If Idx = Length(EditLine) Then
                 Inc(Idx);
              If UpperCase(Copy(EditLine, Idx, 5)) = 'THEN ' Then
                 Dec(Idx);
              While BracketLevel > 0 Do Begin
                 EditLine := Copy(EditLine, 1, Idx -1)+')'+Copy(EditLine, Idx, 999999);
                 Inc(Idx);
                 Dec(BracketLevel);
              End;
              If UpperCase(Copy(EditLine, Idx+1, 5)) = 'THEN ' Then
                 Inc(Idx);
           End;
        End;
        Inc(Idx);
     End;
  End;

  Tokenised := TokeniseLine(EditLine, False);
  EditLine := '';
  EditLine := DeTokeniseLine(Tokenised, False);

  // Exit if the Tokenisation/Detokenisation reduced the line to nothing

  If EditLine = '' Then Begin
     CheckFor128kCommands;
     RepaintBASIC(True);
     Exit;
  End;

  // Need to see if this is a block delete operation!
  // we test that the line starts with a numeric... so skip it and look for #13, "..", or ","

  Idx := 1;
  Done := False;
  TempStr := #13;
  EditLine := EditLine + #13;
  While Not Done Do Begin
     If Not (Editline[Idx] in ['0'..'9']) Then Begin
        TempStr := '';
        Break;
     End;
     While EditLine[Idx] in ['0'..'9'] Do Begin
        TempStr := TempStr + EditLine[Idx];
        Inc(Idx);
     End;
     TempStr := TempStr + #13;
     If EditLine[Idx] = ',' Then
        Inc(Idx)
     Else Begin
        If EditLine[Idx] = #13 Then
           Break;
        If Copy(EditLine, Idx, 2) = '..' Then Begin
           Idx2 := Length(TempStr) -1;
           While TempStr[Idx2] in ['0'..'9'] Do
              Dec(Idx2);
           Inc(Idx2);
           StartLine := StrToInt(Copy(TempStr, Idx2, Length(TempStr) -Idx2));
           Inc(Idx, 2);
           If Not (EditLine[Idx] in ['0'..'9']) Then Begin
              TempStr := '';
              Break;
           End Else Begin
              EndLine := 0;
              While EditLine[Idx] in ['0'..'9'] Do Begin
                 EndLine := (EndLine * 10)+Ord(EditLine[Idx])-48;
                 Inc(Idx);
              End;
              Idx2 := 1;
              While Idx2 < Length(BASICMem) Do Begin
                 LineNum := 0;
                 While BASICMem[Idx2] in ['0'..'9'] Do Begin
                    LineNum := (LineNum * 10)+Ord(BASICMem[Idx2])-48;
                    Inc(Idx2);
                 End;
                 If (LineNum >= StartLine) and (LineNum <= EndLine) then
                    If Pos(IntToStr(LineNum)+#13, TempStr) = 0 Then
                       TempStr := TempStr + IntToStr(LineNum) + #13;
                 While BASICMem[Idx2] <> #13 do
                    Inc(Idx2);
                 While (Idx2 < Length(BASICMem)) and (BASICMem[Idx2] = #13) do
                    Inc(Idx2);
              End;
              If EditLine[Idx] = #13 Then
                 Break;
              If EditLine[Idx] = ',' Then
                 Inc(Idx);
           End;
        End Else Begin
           TempStr := '';
           Break;
        End;
     End;
  End;

  // The comparison later will corrupt EditLine, so store *another* copy which we can add into
  // the BASICMem buffer when all is done.

  If TempStr = '' Then
     TempStr := Copy(EditLine, 1, Length(EditLine) -1)
  Else Begin
     TempStr := Copy(TempStr, 2, 999999);
     ErrorCheck := False;
     BlockDelete := True;
  End;

  // Now test for errors using the Parser.

  If ErrorCheck Then Begin

     ParseError := ParseInputLine(EditLine);
     If (ParseError.Error <> '') or (Pos(#$10+#2, ParseError.Syntax) <> 0) Then Begin
        If (ParseError.ErrorCode = -1) or (ParseError.ErrorCode = 99) Then Begin
           If Copy(ParseError.Error, Length(ParseError.Error), 1) = '.' Then
              ParseError.Error := Copy(ParseError.Error, 1, Length(ParseError.Error)-1);
           ColourLabel2.Str := HighlightReserved(ParseError.Error+' in Statement '+IntToStr(ParseError.Statement), False);
           Result := False;
        End Else Begin
           If Copy(ParseError.Error, Length(ParseError.Error), 1) = ',' Then
              ParseError.Error := Copy(ParseError.Error, 1, Length(ParseError.Error)-2);
           ColourLabel2.Str := HighlightReserved(ParseError.Error, False);
           If ParseError.ErrorCode < 0 Then Result := False;
        End;
     End;
     ColourLabel1.Repaint;
     ColourLabel2.Repaint;

     // And if we errored, bail out setting CodeError so we get the red cursor.

     If Not Result Then Begin

        CodeError := True;
        While (CursLineStart > 1) And Not (BASICMem[CursLineStart] = #13) Do Dec(CursLineStart);
        While BASICMem[CursLineStart] = #13 Do Inc(CursLineStart);
        UpdateCursorPos(CursLineStart + ParseError.Position -1, False);
        CheckFor128kCommands;
        RepaintBASIC(True);
        UpdateParseText;
        UpdateMenu;
        Exit;
     End;

  End;

  // If the user typed an expression, then evaluate it as if it had "PRINT" in front of it.

  If (Lowercase(Copy(ParseError.Syntax, 3, 3)) = 'num') or
     (Lowercase(Copy(ParseError.Syntax, 3, 3)) = 'str') or
     (ParseError.ErrorCode = 98) Then
     Tempstr := 'PRINT '+TempStr;

  // But is it a Direct command, rather than a line? :-)
  // If so, poke it to the editline, and make the new line a single <cr>.

  IsDirect := False;
  If Not (TempStr[1] in ['0'..'9']) Then Begin

     If Result Then Begin

        BASICMem := Copy(BASICMem, 1, LineStart -1) + Copy(BASICMem, LineEnd +2, 999999);

        // Removed TokeniseEditText(False); - fixes the "remove line number to make direct command, but lose line" bug,
        // don't know what other effects it may have.
        // Thing is, if this is a direct command, why does the BASIC need to be tokenised?

        SaveEmulationState(UndoState);
        SaveEmulationState(BREAKState);
        PutEditLine(TempStr, Memory);
        EmulateRET;
        Registers.A := 13;
        TempStr := '';
        UpdateCursorPos(CursLineStart, False);
        //BASICMem := Copy(BASICMem, 1, LineStart -1) + Copy(BASICMem, LineStart +1, 999999);   //ardafix 1.78 This causes erasing first characher of next line if an immediate command executed between two lines.
        If Not DisplayWindow.Visible Then ShowWindow(DisplayWindow, False);
        BracketLevel := 0;

        // Setting result to false will prevent any possible <cr> from
        // being added to the line in the calling proc.

        Result := False;
        Registers.PC := $F48; // Skip the "key click" sound
        ControlEmulation(True);
        IsDirect := True;
        Exit;

     End Else

        Exit;

  End Else Begin

     // Compare old and new cursor positions for changes in tokenisation - Spaces will have been added and
     // the cursor position will no longer be where it was. If this was the end of the line, and a <cr> is
     // coming, then bad things will happen.

     While Length(EditLine) > Length(Original) Do Original := Original + ' ';

     EditLine := UpperCase(EditLine);
     Idx := 1; Idx2 := 1;
     While Idx < (Integer(CursOffset) - LineStart) -1 Do Begin
        If EditLine[Idx2] <> Original[Idx] Then Begin
           Inc(Idx2);
           Inc(CursOffset);
           If (Idx > Length(Original)) or (Idx2 > Length(EditLine)) Then Break;
        End Else Begin
           Inc(Idx);
           Inc(Idx2);
        End;
     End;

     If Not BlockDelete Then Begin

        Idx := 1;
        While TempStr[Idx] in ['0'..'9'] Do Inc(Idx);
        LineNum := StrToint(Copy(TempStr, 1, Idx -1));
        LinePos := GetLineMatch(LineNum);
        If LinePos > 0 Then Begin
           // A Match. Check for Overwrite Protection..
           If OverWriteCursor Or Not Opt_OverwriteProtect Then Begin // Second hit - Overwrite.
              // ...and set the Insert address.
              TempWord := GetWord(@Memory[LinePos+2])+4;
              MoveSpectrumMemory(TempWord + LinePos, -TempWord);
              // And remove the old line.
              BASICMem := LastLineBuffer;
              Idx := LineExists(LineNum);
              If Idx > -1 Then Begin
                 While (Idx > 1) and (BASICMem[Idx] <> #13) Do
                    Dec(Idx);
                 While BASICMem[Idx] = #13 Do
                    Inc(Idx);
                 Idx2 := Idx;
                 While BASICMem[Idx2] <> #13 Do
                    Inc(Idx2);
                 BASICMem := Copy(BASICMem, 1, Idx -1)+Copy(BASICMem, Idx2 +1, 999999);

                 LastLineBuffer := BASICMem;


              End;
              OverWriteCursor := False;
           End Else Begin
              If ErrorCheck Then Begin // First hit - error out and return with the green cursor
                 While Copy(TempStr, 1, 1) = '0' Do
                    TempStr := Copy(TempStr, 2, 999999);
                 BASICMem := Copy(BASICMem, 1, LineStart -1) + Copy(BASICMem, LineEnd +2, 999999);
                 CursOffset := LineExists(LineNum);
                 If CursOffset > 99999999 Then Begin // An edited line (exists in RAM, but not in editor - returned negative)
                    If Not Opt_ProtectNewOnly Then Begin
                       CursOffset := TempCur;
                       While (CursOffset > 0) and (BASICMem[CursOffset] <> #13) Do
                          Dec(CursOffset);
                       While (CursOffset > 1) and (BASICMem[CursOffset -1] = #13) Do
                          Dec(CursOffset);
                    End Else Begin
                       LastLineBuffer := BASICMem;
                       LabelDebug.Caption :='*3' + LastLineBuffer  ;
                       TempWord := GetWord(@Memory[LinePos+2])+4;
                       MoveSpectrumMemory(TempWord + LinePos, -TempWord);
                       OverWriteCursor := False;
                       Goto Label1;
                    End;
                 End Else
                    While (CursOffset < Length(BASICMem)) and (BASICMem[CursOffset] <> #13) Do
                       Inc(CursOffset);
                 If CursOffset < Length(BASICMem) Then
                    Inc(CursOffset);
                 If BASICMem[CursOffset] <> #13 Then
                    TempStr := TempStr + #13;
                 If BASICMem[CursOffset -1] <> #13 Then
                    TempStr := #13 + TempStr;
                 BASICMem := Copy(BASICMem, 1, CursOffset -1) + TempStr + Copy(BASICMem, CursOffset, 999999);
                 While (CursOffset < Length(BASICMem)) and (BASICMem[CursOffset] = #13) Do
                     Inc(CursOffset);
                 UpdateCursorPos(CursOffset, False);
                 CheckFor128kCommands;
                 RepaintBASIC(True);
                 MakeCursorVisible;
              End;
              OverWriteCursor := True;
              Result := False;
              Exit;
           End;
        End Else Begin
           // Line does not exist, and it's greater than the last line.
           LinePos := -LinePos;
        End;

        Label1:

        // Create and insert the new line.
        Idx := 1;
        While Not (Tokenised[Idx] in ['0'..'9']) Do Inc(Idx);
        While Tokenised[Idx] in ['0'..'9'] Do Inc(Idx);
        Tokenised := Copy(Tokenised, Idx, 999999);
        Tokenised := Insert5Bytes(Tokenised);
        If Tokenised[Length(Tokenised)] <> #13 Then Tokenised := Tokenised + #13;
        // Now add the four byte line-header - LSB Line num, MSB Length
        Tokenised := Chr(Word(LineNum) Shr 8) + Chr(Word(LineNum) And 255) + Chr(Word(Length(Tokenised)) And 255) + Chr(Word(Length(Tokenised)) Shr 8) + Tokenised;
        TempWord := GetWord(@Memory[PROG]);
        MoveSpectrumMemory(LinePos, Length(Tokenised));
        For Idx := LinePos To LinePos + Length(Tokenised) -1 Do
           Memory[Idx] := Ord(Tokenised[(Idx - LinePos) +1]);
        PutWord(@Memory[PROG], TempWord);
        BASICMem := LastLineBuffer;
        TempCur := 1;
        // Find the position to insert at. Will return length(basicmem) if it's greater than any lines
        // returns the line number's start position after the line if it's to be inserted.
        CursOffset := GetSourcePos(LineNum, TempCur);
        If TempStr[Length(TempStr)] <> #13 Then
           TempStr := TempStr + #13;
        BASICMem := Copy(BASICMem, 1, CursOffset -1)+TempStr+Copy(BASICMem, CursOffset, 999999);
        LastLineBuffer := BASICMem;
          LabelDebug.Caption :='*4' + LastLineBuffer  ;
     End Else Begin

        BASICMem := Copy(BASICMem, 1, LineStart -1) + Copy(BASICMem, LineEnd +2, 999999);
        DeleteEditorLines(TempStr);
        DeleteBASICLines(TempStr);
        TokeniseEditText(False);

     End;

     UpdateCursorPos(CursOffset, False);
     If BreakpointsWindow.Visible Then
        BreakPointsWindow.BuildBreakPointsList;

  End;

  CheckFor128kCommands;
  BASICChanged := False;
  UpdateMenu;

End;

procedure TBASinOutput.FormDeactivate(Sender: TObject);
begin
  If Not AppClosing Then
     RepaintBASIC(True);
end;

procedure TBASinOutput.SpeedButton4Click(Sender: TObject);
begin
end;

// Debug Procedures

Procedure TBASinOutput.ClearDebugPoints;
Var
  F: Integer;
Begin
  WantBreak := False;
  ViewLine := 0;
  ViewColumn := 0;
  AbortStatement := False;
  GOTOStatement := 1;
  Runline := 65536;
  RunStatement := 0;
  NextLine := False;
  SingleStep := False;
  BreakpointsList[0] := '';
  For F := 0 to 255 Do BreakArray[F].Valid := False;
  For F := 1 to 128 Do
     BreakpointsList[0] := BreakpointsList[0] + #0;
  For F := 1 To 9999 Do
     BreakpointsList[F] := BreakpointsList[0];
  UpdateRunTimeButtons;
End;

Procedure TBASinOutput.FindAndActivateLine(LineNumber, Statement: Integer);
Var
  Idx: Integer;
Begin

  // Find the specified line number in the BASIC listing and point the cursor to it.
  // if this is zero, then get the current line number pointed to by the program
  // cursor. If *this* is zero, then list from the first line.

  If BASICMem <> '' Then Begin

     If LineNumber = 59999 Then Begin

        If Not Running Then
           LineNumber := GetWord(@Memory[E_PPC])
        Else Begin
           LineNumber := GetWord(@Memory[PPC]);
           If Statement = 0 Then
              Statement := Memory[SUBPPC];
        End;

        If LineNumber = 0 Then Begin
           Idx := 1;
           While Not (BASICMem[Idx] in ['0'..'9']) Do Inc(Idx);
           If Idx >= Length(BASICMem) Then Begin
              MakeSound(3);
              Exit;
           End;
           LineNumber := 0;
           While BASICMem[Idx] in ['0'..'9'] Do Begin
              LineNumber := (LineNumber * 10) + Ord(BASICMem[Idx])-48;
              Inc(Idx);
           End;
           Statement := 0;
        End;

     End Else If LineNumber >= 60000 Then Begin

           LineNumber := GetWord(@Memory[PPC]);
           If Statement = 0 Then
              Statement := Memory[SUBPPC];

           If LineNumber > 9999 Then
              LineNumber := 0;

     End;

     // Got a line Number, now find it.

     Idx := GetSourcePos(LineNumber, Statement);

     UpdateCursorPos(Idx+1, False);
     CursLineNum := LineNumber;
     CursStatementNum := Statement;

    RepaintBASIC(False);
     MakeCursorVisible;

  End;

End;

Function  TBASinOutput.LineExists(LineNumber: Integer): Integer;
Var
  Done: Boolean;
  Idx, BASNumber: Integer;
Begin
  Idx := 1;
  Done := False;
  Result := -1;
  If BASICMem <> '' Then Repeat
     While (Idx < Length(BASICMem)) and Not (BASICMem[Idx] in ['0'..'9']) Do Inc(Idx);
     BASNumber := 0;
     While BASICMem[Idx] in ['0'..'9'] Do Begin
        BASNumber := (BASNumber * 10) + Ord(BASICMem[Idx])-48;
        Inc(Idx);
     End;
     If BASNumber = LineNumber Then Begin
        // Found the line number, now find the statement.
        Dec(Idx);
        Result := 0;
        Done := True;
     End Else
        While BASICMem[Idx] <> #13 Do Inc(Idx);
     If Idx >= Length(BASICMem) Then Begin
        Done := True;
        Result := -1;
     End;
  Until Done;
  If Result > -1 Then Begin
     Inc(Idx); // Step to the correct statement.
     If Idx >= Length(BASICMem) Then
        Result := Length(BASICMem) -1
     Else
        Result := Idx;
  End;
End;

Function  TBASinOutput.GetSourcePos(var LineNumber, Statement: Integer): Integer;
Var
  Done, InString, REMCommand: Boolean;
  Idx, BASNumber, BASStatement, LastPos: Integer;
Begin
  Idx := 1;
  Done := False;
  Result := 0;
  If BASICMem = '' Then Exit;
  Repeat
     While (Idx < Length(BASICMem)) and Not (BASICMem[Idx] in ['0'..'9']) Do Inc(Idx);
     BASNumber := 0;
     LastPos := Idx;
     While BASICMem[Idx] in ['0'..'9'] Do Begin
        BASNumber := (BASNumber * 10) + Ord(BASICMem[Idx])-48;
        Inc(Idx);
     End;
     If BASNumber > LineNumber Then Begin
        Result := -LastPos; // line does not exist in the listing
        Break;
     End Else
        If BASNumber = LineNumber Then Begin
           // Found the line number, now find the statement.
           Dec(Idx);
           BASStatement := 1;
           InString := False;
           REMCommand := False;
           While BASStatement < Statement Do Begin
              Inc(Idx);
              If (UpperCase(Copy(BASICMem, Idx, 4)) = 'REM ') and not InString then
                 REMCommand := True;
              If BASICMem[Idx] = '"' Then InString := Not InString;
              If Not InString and Not REMCommand Then Begin
                 If BASICMem[Idx] = ':' Then Inc(BASStatement);
                 If (BASICMem[Idx] in ['n', 'N']) and (Uppercase(Copy(BASICMem, Idx-3, 5)) = 'THEN ') Then Inc(BASStatement);
              End;
              If BASICMem[Idx] = #13 Then Begin
                 While (Idx < Length(BASICMem)) and (BASICMem[Idx] = #13) Do
                    Inc(Idx);
                 BASNumber := 0;
                 While BASICMem[Idx] in ['0'..'9'] Do Begin
                    BASNumber := (BASNumber * 10) + Ord(BASICMem[Idx])-48;
                    Inc(Idx);
                 End;
                 LineNumber := BASNumber;
                 Statement := 1;
                 BASStatement := Statement;
              End;
           End;
           Done := True;
        End Else
           While BASICMem[Idx] <> #13 Do Inc(Idx);
     If Idx >= Length(BASICMem) Then Begin
        Done := True;
        If Idx = 1 Then
           Result := -1
        Else Begin
           While BASICMem[Idx -1] = #13 Do
              Dec(Idx);
           Result := (-Idx)-1;
        End;
     End;
  Until Done;
  If Result > -1 Then Begin
     Inc(Idx); // Step to the correct statement.
     If Idx >= Length(BASICMem) Then
        Result := Length(BASICMem) -1
     Else Begin
        Result := Idx;
     End;
  End Else Begin
     Result := -Result;
  End;
End;

Function  TBASinOutput.GetSourceLine(LineNum, Statement: Integer): String;
Var
  Done, REMCommand, InString: Boolean;
  Idx: Integer;
Begin
  Result := '';
  Done := False;
  Idx := GetSourcePos(LineNum, Statement) -1;
  If Idx <> 0 Then Begin
     InString := False;
     REMCommand := False;
     While Not Done Do Begin
        Inc(Idx);
        If (UpperCase(Copy(BASICMem, Idx, 4)) = 'REM ') and not InString then
           REMCommand := True;
        If BASICMem[Idx] = '"' Then InString := Not InString;
        If Not InString and Not REMCommand Then Begin
           If BASICMem[Idx] = ':' Then Done := True;
           If (BASICMem[Idx] in ['n', 'N']) and (Uppercase(Copy(BASICMem, Idx-3, 5)) = 'THEN ') Then Begin
              Result := Result + BASICMem[Idx];
              Done := True;
           End;
        End;
        If BASICMem[Idx] = #13 Then Done := True;
        If Not Done Then
           Result := Result + BASICMem[Idx];
     End;
  End;
End;

Procedure TBASinOutput.UpdateRuntimeButtons;
Begin
  Case Running of
     True:
        Begin
           If Registers.EmuRunning Then Begin
              SpeedButton4.Hint := 'Pause the Program|';
              SpeedButton4.Glyph.Assign(Image3.Picture.Bitmap); // Pause
              SpeedButton4.Enabled := True;  // Enable pause button
              SpeedButton7.Enabled := False; // Disable Single Step
              SpeedButton5.Enabled := False; // Disable GOTO
              SpeedButton8.Enabled := False; // Disable Step Over
              SpeedButton9.Enabled := False; // Disable RUN To
              SpeedButton6.Enabled := True;  // Enable system BREAK
              SpeedButton6.Hint := 'BREAK program execution|';
              SpeedButton6.Glyph.Assign(Image4.Picture.Bitmap); // Break
              Run2.Caption := 'Pause';
              Continue1.Enabled := SpeedButton6.Enabled;
              Continue1.Caption := 'System BREAK';
              Continue1.Hint := SpeedButton6.Hint;
           End Else Begin
              SpeedButton4.Hint := 'Resume the program|';
              SpeedButton4.Glyph.Assign(Image2.Picture.Bitmap); // Run
              SpeedButton4.Enabled := BASICMem <> #13;      // Enable Run button if there's a program
              SpeedButton7.Enabled := SpeedButton4.Enabled; // Enable Single Step
              SpeedButton5.Enabled := SpeedButton4.Enabled; // Enable GOTO
              SpeedButton8.Enabled := SpeedButton4.Enabled; // Enable Step Over
              SpeedButton9.Enabled := SpeedButton4.Enabled; // Enable RUN To
              SpeedButton6.Enabled := False;                // Disable system BREAK
              SpeedButton6.Hint := 'System BREAK|';
              SpeedButton6.Glyph.Assign(Image4.Picture.Bitmap); // Break
              Continue1.Enabled := SpeedButton6.Enabled;
              Continue1.Caption := 'System BREAK';
              Continue1.Hint := SpeedButton6.Hint;
              Run2.Caption := 'Resume';
           End;
        End;
     False:
        Begin
           SpeedButton4.Hint := 'Run the program|';
           SpeedButton4.Glyph.Assign(Image2.Picture.Bitmap); // Run
           SpeedButton4.Enabled := Length(BASICMem) > 1; // Enable Run button if there's a program
           SpeedButton7.Enabled := SpeedButton4.Enabled; // Enable Single Step
           SpeedButton5.Enabled := SpeedButton4.Enabled; // Enable GOTO
           SpeedButton8.Enabled := SpeedButton4.Enabled; // Enable Step Over
           SpeedButton9.Enabled := SpeedButton4.Enabled; // Enable RUN To
           SpeedButton6.Glyph.Assign(Image5.Picture.Bitmap); // Continue
           SpeedButton6.Enabled := ContinueReady; // Enable system CONTINUE?
           Run2.Caption := 'Run';
           SpeedButton6.Hint := 'Continue the program.|';
           Continue1.Enabled := SpeedButton6.Enabled;
           Continue1.Caption := 'Continue';
           Continue1.Hint := SpeedButton6.Hint;
        End;
  End;
  RunningAck := Running;

  // Some Menu Items in the main window are closely related to these.
  Run2.Enabled := SpeedButton4.Enabled;
  Goto1.Enabled := Not Registers.EmuRunning and SpeedButton4.Enabled;
  SingleStepStatement1.Enabled := SpeedButton7.Enabled;
  StepToNext1.Enabled := SpeedButton8.Enabled;
  RunTo1.Enabled := SpeedButton9.Enabled;
End;

procedure TBASinOutput.DoSingleStep(Sender: TObject);
begin
  // Single Step.
  // Firstly, the boolean var SingleStep is set true - stop the emulation
  // After the current statement has executed. Then the program is started...
  If Not Running Then Begin
     RunStatement := Memory[OSPCC];
     RunLine := GetWord(@Memory[OLDPPC]);
  End Else Begin
     SingleStep := True;
  End;
  RunProgram(65535);
end;

Function TBASinOutput.GetFirstLineNum: Integer;
Var
  Idx: Integer;
  LineNumber: Integer;
Begin
  Result := 0;
  If BASICMem <> '' Then Exit;
  Idx := 1;
  While Not (BASICMem[Idx] in ['0'..'9']) Do Inc(Idx);
  LineNumber := 0;
  While BASICMem[Idx] in ['0'..'9'] Do Begin
     LineNumber := (LineNumber * 10) + Ord(BASICMem[Idx])-48;
     Inc(Idx);
  End;
  Result := LineNumber;
End;

Procedure TBASinOutput.RunProgram(Line: Word);
Begin
  // A Real headache.
  // First, if the emulation has been paused, it's a safe bet that we're already
  // in runtime. Otherwise, start the program, by calling the RUN ROM routine.
  If Not Registers.EmuRunning Then Begin
     If Running Then Begin
        ControlEmulation(True);
        UpdateRunTimeButtons;
     End Else Begin
        If Line = 65535 Then Begin
           PutEditLine('RUN', Memory);
           EmulateRET;
           Registers.A := 13;
           Registers.PC := $F48; // Skip the "key click" sound
           ControlEmulation(True);
        End Else Begin
           PutEditLine('GO TO'+IntToStr(Line), Memory);
           EmulateRET;
           Registers.A := 13;
           Registers.PC := $F48; // Skip the "key click" sound
           ControlEmulation(True);
        End;
     End;
  End Else Begin
     // Emulation is running - so we need to execute a ROM routine.
     // GOTO would be best, as RUN clears variables.
     If Line = 65535 Then Begin
        PutEditLine('RUN', Memory);
        EmulateRET;
        Registers.A := 13;
        Registers.PC := $F48; // Skip the "key click" sound
        ControlEmulation(True);
     End Else Begin
        PutEditLine('GO TO'+IntToStr(Line), Memory);
        EmulateRET;
        Registers.A := 13;
        Registers.PC := $F48; // Skip the "key click" sound
        ControlEmulation(True);
     End;
  End;
  RepaintBASIC(True);
End;

procedure TBASinOutput.RunOrResume(Sender: TObject);
begin
  If Not Registers.EmuRunning and Not Running Then
     If Not TokeniseEditText(True) Then
        Exit;

  Opt_CPUSpeed := SpeedBackup;  // after modifying cpu speed with a Rem command, reset speed back to it's original value


  If Not Registers.EmuRunning or Not Running Then Begin
     SaveEmulationState(BREAKState); //Workaround for Force Break -- Arda
     RunProgram(65535);
  End Else Begin
     // To stop, simply simulate a single step operation.
     // then we *know* we'll stop between statements.
     If Editing Then Begin
        BufferKey(0, 13);
        BufferKey(1, 13);
     End;
     SingleStep := True;
  End;
end;

procedure TBASinOutput.StepOver(Sender: TObject);
Var
  Idx: Integer;
  Done, InString, REMCommand: Boolean;
begin
  // Step Over. Basically, Set a RUN-TO point as the next statement/line, and
  // start the RUN TO process.

  If BASICMem = '' Then Exit;

  // First, find the next statement.

  Idx := CursLineStart;
  InString := False;
  Done := False;
  REMCommand := False;
  While Not Done Do Begin
     If (UpperCase(Copy(BASICMem, Idx, 4)) = 'REM ') and not InString then
        REMCommand := True;
     If BASICMem[Idx] = '"' Then InString := Not InString;
     If Not InString And Not REMCommand Then
        If (BASICMem[Idx] = ':') or ((BASICMem[Idx] in ['n', 'N']) and (UpperCase(Copy(BASICMem, Idx -3, 5)) = 'THEN ')) or (BASICMem[Idx] = #13) Then
           Done := True;
     If BASICMem[Idx] = #13 Then
        Done := True;
     Inc(Idx);
  End;
  While BASICMem[Idx] in [#0..#32, ':'] Do Inc(Idx);

  // If this starts now with a digit, then it's a new line - so grab the line
  // Else it's the next statement along - use CursStatementNum +1.

  If BASICMem[Idx] in ['0'..'9'] Then Begin
     RunStatement := 1;
     RunLine := 0;
     While BASICMem[Idx] in ['0'..'9'] Do Begin
        RunLine := (RunLine*10)+Ord(BASICMem[Idx])-48;
        Inc(Idx);
     End;
  End Else Begin
     RunLine := CursLineNum;
     RunStatement := CursStatementNum +1;
  End;
  RunProgram(65535);

end;

procedure TBASinOutput.RunTo(Sender: TObject);
begin
  // Continue Execution until the selected statement or line
  // is about to be executed.
  RunStatement := CursStatementNum;
  RunLine := CursLineNum;
  RunProgram(65535);
end;

procedure TBASinOutput.SetGoTo(Sender: TObject);
begin
  // GO TO - this begins (or resumes) emulation/execution
  // at the line/Statement highlighted.
  SetGOTOPoint(CursLineNum, CursStatementNum);
End;

Procedure SetGOTOPoint(Line, Statement: DWord);
Var
  TempVal, TempLine: DWord;
Begin

  If BASinOutput.Running Then Begin

     // At this point, the Emulation is paused - the only way that
     // can happen is during program execution, so we simply poke.
     // Find the line, and set the runtime loop to point to it.

     If Not Registers.EmuRunning Then
        TempLine := GetLineAddress(Line, Statement+1, GetWord(@Memory[PROG]))
     Else
        TempLine := GetLineAddress(Line, Statement, GetWord(@Memory[PROG]));

     If TempLine > 0 Then Begin

        // At this point, TempLine holds the address of the line
        // to jump to.
        // First, before we modify to point at the first valid keyword,
        // we need to update the sysvar NXTLIN to point to the line after.

        TempVal := TempLine;
        Repeat
           Inc(TempVal);
        Until (Memory[TempVal] = 13) or (TempVal = GetWord(@Memory[VARS]));
        PutWord(@Memory[NXTLIN], TempVal+1);

        // Now perform the aforementioned modification on TempLine.
        // If the Statement = 1 then we need to skip the line number
        // and length (4 bytes)

        If Statement = 1 Then Inc(Templine, 4);

        // Now find the first valid keyword, and update A with it,
        // as the preceding RST 20h would have done.

        While Memory[TempLine] < $CE Do Inc(TempLine);
        Registers.A := Memory[TempLine];

        // now update the system variables with the new
        // line number and statement offset.

        PutWord(@Memory[PPC], Line);
        Memory[SUBPPC] := Statement;
        PutWord(@MEMORY[CH_ADD], TempLine);

        // and restart execution, if the emulation is *not* paused.
        // this is so that we can single step from any point we choose.

        CurKeyDown := 0;
        KeyBuffer := '';
        ControlEmulation(True);
        Exit;

     End;

  End Else Begin

     // We're sitting at the edit prompt, so we need to utilise a ROM trap.
     // Setting GOTOStatement to <> 1 will trap and jump to that statement later on.

     BASinOutput.GOTOLine := Line;
     PutEditLine('GO TO '+IntToStr(Line), Memory);
     BufferToken(13);
     BASinOutput.GOTOStatement := Statement;
     EmulateRET;
     Registers.A := 13;
     ControlEmulation(True);

  End;

end;

Function TBASinOutput.TokeniseEditText(SyntaxCheck: Boolean): Boolean;
Var
  InString: Boolean;
  BASIC, CurLine: String;
  OldPROG: Word;
  Idx, CurIdx, LineNum, LineStart: Integer;
Begin

  // Send the Editor Text to memory as a BASIC memory block.
  // First, Get each line of BASIC.

  Result := True;
  BASIC := '';

  Idx := 1;
  LineStart := 1;
  InString := False;
  While Idx <= Length(BASICMem) Do Begin
     CurLine := CurLine + BASICMem[Idx];
     If BASICMem[Idx] = '"' Then InString := Not InString;
     If BASICMem[Idx] = #13 Then Begin
        If CurLine <> #13 Then Begin
           If SyntaxCheck Then Begin
              // Check for errors
              ParseError := ParseInputLine(Copy(CurLine, 1, Pos(#13, CurLine) -1));
              If ParseError.Error <> '' Then Begin
                 If ParseError.ErrorCode = -1 Then Begin
                    If ParseError.Error[Length(ParseError.Error)] = '.' Then
                       ParseError.Error := Copy(ParseError.Error, 1, Length(ParseError.Error)-1);
                    ColourLabel2.Str := HighlightReserved(ParseError.Error+' in Statement '+IntToStr(ParseError.Statement), False);
                    Result := False;
                 End Else Begin
                    If ParseError.Error[Length(ParseError.Error)-1] = ',' Then
                       ParseError.Error := Copy(ParseError.Error, 1, Length(ParseError.Error)-2);
                    ColourLabel2.Str := HighlightReserved(ParseError.Error, False);
                    If ParseError.ErrorCode < 0 Then Result := False;
                 End;
              End;
              ColourLabel1.Repaint;
              ColourLabel2.Repaint;
              // And if we errored, bail out setting CodeError so we get the red cursor.
              If Not Result Then Begin
                 CodeError := True;
                 UpdateCursorPos(LineStart + ParseError.Position -1, False);
                 RepaintBASIC(True);
                 UpdateParseText;
                 Exit;
              End;
           End;
           // Get the line Number
           CurIdx := 1;
           If CurLine[CurIdx] in ['0'..'9'] Then
              LineNum := 0
           Else
              LineNum := -1;
           While (CurIdx <= Length(CurLine)) and Not (CurLine[CurIdx] in ['0'..'9']) Do Inc(CurIdx);
           While CurLine[CurIdx] in ['0'..'9'] Do Begin
              LineNum := (LineNum*10)+Ord(CurLine[CurIdx])-48;
              Inc(CurIdx);
           End;
           If (LineNum >= 0) and (LineNum < 10000) Then Begin
              // Convert the remaining line to tokens, and remove the line number
              CurLine := Copy(CurLine, CurIdx, 999999);
              CurLine := TokeniseLine(CurLine, False);
              CurLine := #0#0#0#0+Insert5Bytes(CurLine);
              If CurLine[Length(CurLine)] <> #13 Then CurLine := CurLine + #13;
              Delete(CurLine, 1, 4);
              // Now add the four byte line-header - LSB Line num, MSB Length
              BASIC := BASIC + Chr(Word(LineNum) Shr 8) + Chr(Word(LineNum) And 255);
              BASIC := BASIC + Chr(Word(Length(CurLine)) And 255) + Chr(Word(Length(CurLine)) Shr 8);
              BASIC := BASIC + CurLine;
              LineStart := Idx + 1;
              InString := False;
           End;
           CurLine := '';
        End Else
           CurLine := '';
     End;
     Inc(Idx);
  End;

  // we need to insert the new program - without upsetting the VARS or WORKSPaces

  OldPROG := GetWord(@Memory[PROG]);
  MoveSpectrumMemory(GetWord(@Memory[VARS]), Length(BASIC) - (GetWord(@Memory[VARS]) - GetWord(@Memory[PROG])));
  PutWord(@Memory[PROG], OldPROG);
  CopyMemory(@Memory[GetWord(@Memory[Prog])], @Basic[1], Length(BASIC));

  BracketLevel := 0;

End;

Function TBASinOutput.FindNextForward(Term: AnsiString; Pos: Integer; MatchCase, WholeWord: Boolean): TFindResult;
Var
  Idx: Integer;
  Done: Boolean;
Begin
  If Term <> '' Then Begin
     Idx := Pos -1;
     Done := False;
     If Idx < 0 Then Idx := 0;
     If Not MatchCase Then Term := Uppercase(Term);
     While Not Done Do Begin
        Inc(Idx);
        If Idx = Length(BASICMem)+1 Then Break;
        If Not MatchCase Then Begin
           If UpperCase(BASICMem[Idx]) = Term[1] Then
              If UpperCase(Copy(BASICMem, Idx, Length(Term))) = Term Then Begin
                 Done := True;
                 If WholeWord Then Begin
                    If Idx > 1 Then
                       If BASICMem[Idx -1] > ' ' Then
                          Done := False;
                    If Idx + Length(Term) < Length(BASICMem) Then
                       If BASICMem[Idx + Length(Term)] > ' ' Then
                          Done := False;
                 End;
              End;
        End Else Begin
           If BASICMem[Idx] = Term[1] Then
              If Copy(BASICMem, Idx, Length(Term)) = Term Then
                 Done := True;
        End;
     End;
     If Done Then
        Result.Position := Idx
     Else
        Result.Position := 0;
  End;
End;

Function TBASinOutput.FindNextBackward(Term: AnsiString; Pos: Integer; MatchCase, WholeWord: Boolean): TFindResult;
Var
  Idx: Integer;
  Done: Boolean;
Begin
  If Term <> '' Then Begin
     Idx := Pos +1;
     Done := False;
     If Idx > Length(BASICMem) Then Idx := Length(BASICMem);
     If Not MatchCase Then Term := Uppercase(Term);
     While Not Done Do Begin
        Dec(Idx);
        If Idx = 0 Then Break;
        If Not MatchCase Then Begin
           If UpperCase(BASICMem[Idx]) = Term[1] Then
              If UpperCase(Copy(BASICMem, Idx, Length(Term))) = Term Then
                 Done := True;
        End Else Begin
           If BASICMem[Idx] = Term[1] Then
              If Copy(BASICMem, Idx, Length(Term)) = Term Then Begin
                 If WholeWord Then Begin
                    If Idx > 1 Then
                       If BASICMem[Idx -1] > ' ' Then
                          Done := False;
                    If Idx + Length(Term) < Length(BASICMem) Then
                       If BASICMem[Idx + Length(Term)] > ' ' Then
                          Done := False;
                 End;
              End;
        End;
     End;
     If Done Then
        Result.Position := Idx
     Else
        Result.Position := 0;
  End;
End;

procedure TBASinOutput.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  // Update the selection if the user drags with the LMB.
  If Sender = RulerIMG Then Dec(X, 2);
  If (MousePos.X <> X) or (MousePos.Y <> Y) Then
     If MouseDown Then Begin
        If GetCharPos(X, Y) <> CursOffset Then Begin
           UpdateCursorPos(GetCharPos(X, Y), True);
           RepaintBASIC(True);
           UpdateParseText;
           MakeCursorVisible;
        End;
     End;
  // Otherwise, just update the Ruler
  If Opt_CharacterRuler Then DrawRuler;
  MousePos := Point(X, Y);
end;

procedure TBASinOutput.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown := False;
end;

Function TBASinOutput.GetEditorState: String;
Var
  TempMemStr, BASICLen: String;
Begin

  SetLength(TempMemStr, 49152);
  CopyMemory(@TempMemStr[1], @Memory[16384], 49152);

  BASICLen := #0#0#0#0;
  PutDWord(@BASICLen[1], Length(BASICMem));

  //Result := LastLineBuffer+#255+        //ardafix
  Result:=   LastLineBuffer+#255+         IntToStr(CursOffset)+#255+
            IntToStr(ViewLine)+#255+
            IntToStr(ViewColumn)+#255+
            IntToStr(EditorSelAnchor)+#255+
            IntToStr(EditorSelLength)+#255+
            IntToStr(Integer(ShowingPrediction))+#255+
            BASICLen+BASICMem+TempMemStr;

End;

Procedure TBASinOutput.AddUndo;
Var
  TempStr: String;
Begin

  // Store the current state of the BASIC in the editor.
  // Store current cursor pos, slider viewpoint, selection coords
  // and BASIC text.

  TempStr := GetEditorState;
  Undolist.Add(TempStr);
  If UndoList.Count > 1000 Then
     UndoList.Delete(0);

  //RedoList.Clear;

End;

Procedure TBASinOutput.AddRedo;
Var
  TempStr: String;
Begin

  TempStr := GetEditorState;
  Redolist.Add(TempStr);

End;


Procedure TBASinOutput.PerformUndo;
Var
  Item: String;
  BASICLen: DWord;
Begin
  If UndoList.Count = 0 Then Exit;

  Item := UndoList[UndoList.Count -1];
  UndoList.Delete(UndoList.Count -1);

  AddRedo;

  LastLineBuffer := Copy(Item, 1, Pos(#255, Item)-1);            //ardafixu
  Item := Copy(Item, Pos(#255, Item)+1, 999999);                //ardafixu
  CursOffset := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  ViewLine := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  ViewColumn := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  EditorSelAnchor := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  EditorSelLength := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  ShowingPrediction := Boolean(StrToInt(Copy(Item, 1, Pos(#255, Item)-1)));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  BASICLen := GetDWord(@Item[1]);
  Item := Copy(Item, 5, 999999);
  BASICMem := Copy(Item, 1, BASICLen);
  Item := Copy(Item, BASICLen+1, 999999);

  CopyMemory(@Memory[16384], @Item[1], 49152);
  Item := '';

  If DisplayWindow.Showing Then Begin
     UpdateDisplay;
     UpdateBASinDisplay;
  End;
  NeedDisplayUpdate := False;
  RepaintBASIC(False);
  MakeCursorVisible;
  //BASICChanged:=true;  //ardafix

End;

Procedure TBASinOutput.PerformRedo;
Var
  Item: String;
  BASICLen: DWord;
Begin
  If RedoList.Count = 0 Then Exit;

  Item := RedoList[RedoList.Count -1];
  RedoList.Delete(RedoList.Count -1);
  AddUndo;

  CursOffset := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);                  //ardafix
  ViewLine := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  ViewColumn := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  EditorSelAnchor := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  EditorSelLength := StrToInt(Copy(Item, 1, Pos(#255, Item)-1));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  ShowingPrediction := Boolean(StrToInt(Copy(Item, 1, Pos(#255, Item)-1)));
  Item := Copy(Item, Pos(#255, Item)+1, 999999);
  BASICLen := GetDWord(@Item[1]);
  Item := Copy(Item, 5, 999999);
  BASICMem := Copy(Item, 1, BASICLen);
  Item := Copy(Item, BASICLen+1, 999999);
  CopyMemory(@Memory[16384], @Item[1], 49152);
  Item := '';

  If DisplayWindow.Showing Then Begin
     UpdateDisplay;
     UpdateBASinDisplay;
  End;
  NeedDisplayUpdate := False;
  RepaintBASIC(False);
  MakeCursorVisible;

End;

Function TBASinOutput.GetUndoNumber(var Item: String): Integer;
Begin
  Result := 0;
  While Item[1] <> ',' Do Begin
     Result := (Result * 10)+Ord(Item[1])-48;
     Item := Copy(Item, 2, 999999);
  End;
  Item := Copy(Item, 2, 999999);
End;

Procedure TBASinOutput.SetRuler;
Begin
  RulerIMG.Visible := Opt_CharacterRuler;
  Bevel5.Visible := Opt_ShowStatusbar and (Opt_ShowingSyntax or Opt_CharacterRuler);
  CoolBar1.Visible := Opt_ShowToolbar;

  SpeedButton10.Visible:= Opt_Controlicons;
  SpeedButton8.Visible:= Opt_Controlicons;
  SpeedButton7.Visible:= Opt_Controlicons;
  SpeedButton9.Visible:= Opt_Controlicons;
  //Bevel3.Visible:= Opt_Controlicons;
  if (Not Opt_Controlicons) Then
  Begin

        Bevel3.Left:=SpeedButton12.Left+SpeedButton12.Width+4;
  End Else Begin

        Bevel3.Left:=SpeedButton9.Left+SpeedButton9.Width+4;;

  End;
  SetSyntaxHelper;
  FormResize(nil);
End;

Procedure TBASinOutput.DrawRuler;
Var
  PosInString: Boolean;
  X, Xp, LineX, StringLen, LineStart, StatementStartA,
  Len, Count, NonAsciiMod, Idx: Integer;
Begin

  RulerIMG.Bmp.SetSize(ClientWidth, RulerIMG.Height, 32);
  FillRect(RulerIMG.Bmp, 0, 0, RulerIMG.Bmp.Width -1, RulerIMG.Bmp.AbsHeight -1, TColorToTFColor(ClBtnHighlight));

  // Non-Ascii chars have no width.
   StatementStartA :=0; //marks the start of a statement
  StringLen := 0;
  NonAsciiMod := 0;

  If BASICMem <> '' Then Begin

     Idx := CursOffset -1;
     While (Idx >1) and (BASICMem[Idx] <> #13) Do
        Dec(Idx);
     If BASICMem[Idx] = #13 Then
        LineStart := Idx +1
     Else
        LineStart := Idx;



     NonAsciiMod := 0;
     CursStringStart := 0;
     PosInString := False;
     While Idx <= CursOffset Do Begin
        If BASICMem[Idx] = '"' Then Begin
           If Not PosInString Then Begin
              // String starts here
              PosInString := True;

              StringStart := Idx;
              StringLen := 0;
              NonAsciiMod := 0;
           End Else // In a String
              If BASICMem[Idx+1] <> '"' Then Begin // Next char is a quote mark?
                 PosInString := False;
                 If CursOffset = Idx Then
                    CursStringStart := StringStart;
              End Else Begin
                 Inc(Idx);
                 Inc(StringLen);
                 If Idx+1 <= CursOffset Then
                    Inc(NonAsciiMod);
              End;
        End Else
           If PosInString Then
              If BASICMem[Idx] >= ' ' Then
                 Inc(StringLen)
              Else Begin
                 If BASICMem[Idx] <> #13 Then
                    Inc(NonAsciiMod);
              End;

        //ardafix 1.70
        If BASICMem[Idx] = ':' Then
                StatementStartA := Idx +1  ;
        If (BASICMem[Idx] = 'N') or (BASICMem[Idx] = 'n') Then
           If UpperCase(Copy(BASICMem, Idx -3, 5)) = 'THEN ' Then
              StatementStartA := Idx +1  ;
        //end ardafix

        Inc(Idx);
     End;
     If PosInString Then
        CursStringStart := StringStart;
     While (Idx <= Length(BASICMem)) and PosInString Do begin
        If BASICMem[Idx] = '"' Then Begin
           If BASICMem[Idx +1] <> '"' Then
              PosInString := False
           Else Begin
              Inc(StringLen);
              Inc(Idx);
           End;
        End Else
           If BASICMem[Idx] = #13 Then
              PosInString := False
           Else
              Inc(StringLen);
        Inc(Idx);
     End;
     If CursStringStart <> 0 Then Begin
        Inc(StringLen, StringStart +1);
     End;
  End;

  // Draw the mouse position
  If (MousePos.X >= 0) and (MousePos.X < RulerIMG.Bmp.Width) Then
     Line(RulerIMG.Bmp, MousePos.X +2, 0, MousePos.X +2, 14, TfShadow);
 
  If CursStringStart = 0 Then
     Count := ViewColumn - 5
  Else
        if CursStatementNum=1 Then  Begin
            Count := -((Integer(CursStringStart) - Integer(CursStringOffset)) - ViewColumn) -5 + LineStart -1;
        End else Begin

            Count:= Integer(StatementStartA) - (Integer(CursStringStart)  + Integer(CursStringOffset) + 6)  ;
        End;
  // Draw the ruler tics and their numbers
  For X := 0 To (RulerIMG.Width - 2) Div (8*Opt_FontScale) - 1 Do Begin
     LineX := 2 + (X * 8 * Opt_FontScale); //put a tick here
     If Count Mod 8 = 0 Then Begin   //tick at every character position
        Len := 5;
        SmallTextOut(RulerIMG.Bmp, IntToStr(Count), LineX - ((Length(IntToStr(Count))*4) Div 2) +1, RulerIMG.Height - 15, TFColorToTFColorA(TfSpecBlack));
     End Else
        Len := 3;
     If Count Mod 2 = 0 Then Inc(Len, 2);
     Line(RulerIMG.Bmp, LineX, 0, LineX, Len, TfBlack);
     If (CursStringStart <> 0) and (Count >= 0) and (Count +1 < (StringLen - Integer(CursStringStart))) Then Begin     //if it's in a string
        Xp := 8 * Opt_FontScale;
        FillRect(RulerIMG.Bmp, LineX+1, 12, LineX+Xp, 21, TFGreen);
        If Count Mod 32 = 0 Then Begin     //put red pointer when in string and beginning of a line
           SmallTextOut(RulerIMG.Bmp, ' ', LineX+(XP Div 2)-1, 10, TFColorToTFColorA(TfSpecRed));
        End;
     End;
     Inc(Count);
  End;

  // Draw the Cursor pointer
  If Not Running and CursorVisible and (Active and (Screen.ActiveForm = Self)) Then Begin
     X := ((8 * Opt_FontScale) Div 2) +1;
     SmallTextOut(RulerIMG.Bmp, ' ', CursorPoint.X +X -(8*NonAsciiMod), 7, TFColortoTFColorA(TfBlack));
  End;

  RulerIMG.Repaint;

End;

Procedure SmallTextOut(Bmp: TFastDIB; Text: String; X, Y: Integer; Clr: TFColorA);
Var
  Xp, Yp: Integer;
  Ch, Tx, F: Integer;
Const
  SmallNumbers: Array[0..11, 0..4] of String =
    (('01110', '01010', '01010', '01010', '01110'),  // 0
     ('00100', '00100', '00100', '00100', '00100'),  // 1
     ('01110', '00010', '01110', '01000', '01110'),  // 2
     ('01110', '00010', '01110', '00010', '01110'),  // 3
     ('01010', '01010', '01110', '00010', '00010'),  // 4
     ('01110', '01000', '01110', '00010', '01110'),  // 5
     ('01110', '01000', '01110', '01010', '01110'),  // 6
     ('01110', '00010', '00010', '00010', '00010'),  // 7
     ('01110', '01010', '01110', '01010', '01110'),  // 8
     ('01110', '01010', '01110', '00010', '01110'),  // 9
     ('00000', '00000', '01110', '00000', '00000'),  // -
     ('00000', '00100', '01110', '11111', '11111')); // Arrow
  SmallLetters: Array[0..5, 0..4] of String =
    (('00100', '01010', '01110', '01010', '01010'),  // A
     ('01100', '01010', '01100', '01010', '01100'),  // B
     ('00100', '01010', '01000', '01010', '00100'),  // C
     ('01100', '01010', '01010', '01010', '01100'),  // D
     ('01110', '01000', '01100', '01000', '01110'),  // E
     ('01110', '01000', '01100', '01000', '01000')); // F
Begin
  Xp := X -1;
  If Y +4 > BMP.AbsHeight Then Exit;
  For Tx := 1 To Length(Text) Do Begin
     Yp := Y;
     If Text[Tx] in ['a'..'f', 'A'..'F'] Then Begin
        Ch := Ord(Text[Tx]) - 65;
        If Ch > 31 Then
           Dec(Ch, 32);
        If Xp > 0 Then
           For F := 0 To 4 Do Begin
              If Yp >= 0 Then Begin
                 If SmallLetters[Ch, 4-F][1] = '1' Then Bmp.Pixels32[Yp, Xp] := Clr;
                 If SmallLetters[Ch, 4-F][2] = '1' Then Bmp.Pixels32[Yp, Xp +1] := Clr;
                 If SmallLetters[Ch, 4-F][3] = '1' Then Bmp.Pixels32[Yp, Xp +2] := Clr;
                 If SmallLetters[Ch, 4-F][4] = '1' Then Bmp.Pixels32[Yp, Xp +3] := Clr;
                 If SmallLetters[Ch, 4-F][5] = '1' Then Bmp.Pixels32[Yp, Xp +4] := Clr;
              End;
              Inc(Yp);
           End;
        Inc(Xp, 4);
        If Xp >= BMP.Width then Break;
     End Else Begin
        If Text[Tx] <> '-' Then Begin
           If Text[Tx] = ' ' Then
              Ch := 11
           Else
              Ch := Ord(Text[Tx]) - 48
        End Else
           Ch := 10;
        If Xp > 0 then
           For F := 0 To 4 Do Begin
              If Yp >= 0 Then Begin
                 If SmallNumbers[Ch, 4-F][1] = '1' Then Bmp.Pixels32[Yp, Xp] := Clr;
                 If SmallNumbers[Ch, 4-F][2] = '1' Then Bmp.Pixels32[Yp, Xp +1] := Clr;
                 If SmallNumbers[Ch, 4-F][3] = '1' Then Bmp.Pixels32[Yp, Xp +2] := Clr;
                 If SmallNumbers[Ch, 4-F][4] = '1' Then Bmp.Pixels32[Yp, Xp +3] := Clr;
                 If SmallNumbers[Ch, 4-F][5] = '1' Then Bmp.Pixels32[Yp, Xp +4] := Clr;
              End;
              Inc(Yp);
           End;
        Inc(Xp, 4);
        If Xp >= BMP.Width then Break;
     End;
  End;
End;

Procedure TBAsinOutput.SetDark;
Var
  R, G, B: Integer;
Begin

  R := DisplayPalette[Opt_Background].r;
  G := DisplayPalette[Opt_Background].g;
  B := DisplayPalette[Opt_Background].b;

  If R < 10 Then R := 10;
  If G < 10 Then G := 10;
  If B < 10 Then B := 10;

  TFSpecDark.r := R -10;
  TFSpecDark.g := G -10;
  TFSpecDark.b := B -10;

  TFSpecDarkA.r := R -10;
  TFSpecDarkA.g := G -10;
  TFSpecDarkA.b := B -10;

  DisplayPalette[17].r := (DisplayPalette[Opt_BackGround].r + DisplayPalette[Opt_ForeGround].r) Div 2;
  DisplayPalette[17].g := (DisplayPalette[Opt_BackGround].g + DisplayPalette[Opt_ForeGround].g) Div 2;
  DisplayPalette[17].b := (DisplayPalette[Opt_BackGround].b + DisplayPalette[Opt_ForeGround].b) Div 2;

End;

Function GetPredictiveText(CurWord: String; Context: Integer): String;
Var
  Idx, CurWordLen: Integer;
  LowerWord: String;
Begin

  // Returns the most likely word that matches what you have typed so far. The returned string is the "remaining" characters of the word.
  // Context based: 0 - Keyword needed, 1 - Function or variable.

  Result := '';

  CurWord := Uppercase(CurWord);
  If CurWord = '' Then Exit;

  If CompareStrToSubStr('GO', CurWord, 1, 2) or CompareStrToSubStr('DEF', CurWord, 1, 3) Then Begin
     If CompareStrToSubStr('GO', CurWord, 1, 2) Then
        If Length(CurWord) > 2 Then
           If (CurWord[3] = 'S') or (CurWord[3] = 'T') Then
              CurWord := Copy(CurWord, 1, 2)+' '+Copy(CurWord, 3, 999);
     If CompareStrToSubStr('DEF', CurWord, 1, 3) Then
        If CurWord[4] = 'F' Then
           CurWord := Copy(CurWord, 1, 3)+' '+Copy(CurWord, 4, 999);
  End;
  If Not (CurWord[1] in [#36, #65..#90, #97..#122]) Then Exit;
  CurWordLen := Length(CurWord);

  Case Context of
     0: Begin // Keyword?
           Idx := KeywordsHash[Ord(Curword[1])-65];
           While (Not CompareStrToSubStr(CurWord, Keywords[Idx], 1, CurWordLen)) or
                 (KeywordsIDXTable[Idx] >= KeywordCutoff) Do Begin
              Inc(Idx);
              If Idx > NumReserved Then Break;
           End;
           If Idx > NumReserved Then
              Result := ''
           Else Begin
              Result := Copy(Keywords[Idx], 1, Pos('-', Keywords[Idx]) -1);
              If Length(Result) <> CurWordLen Then
                 Result := Copy(Result, CurWordLen, Length(Keywords[Idx]) - CurWordLen)
              Else
                 Result := '';
           End;
        End;
     1: Begin // Function?
           Idx := KeywordsHash[Ord(Curword[1])-65];
           While (Not CompareStrToSubStr(CurWord, Keywords[Idx], 1, CurWordLen)) or
                 (KeywordsIDXTable[Idx] < KeywordCutoff) Do Begin
              Inc(Idx);
              If Idx > NumReserved Then Break;
           End;
           If Idx > NumReserved Then Begin
              // Variable?
              If VariablesWindow.ListView1.Items.Count > 0 Then Begin
                 Idx := 0;
                 LowerWord := Lowercase(CurWord);
                 While (Idx < VariablesWindow.ListView1.Items.Count) and
                       (Not CompareStrToSubStr(LowerWord, VariablesWindow.ListView1.Items[Idx].Caption, 1, CurWordLen)) Do
                    Inc(Idx);
                 If Idx >= VariablesWindow.ListView1.Items.Count Then
                    Result := ''
                 Else Begin
                    Result := VariablesWindow.ListView1.Items[Idx].Caption;
                    Result := Copy(Result, CurWordLen, Length(VariablesWindow.ListView1.Items[Idx].Caption)+1 - CurWordLen);
                 End;
              End;
           End Else Begin
              Result := Copy(Keywords[Idx], 1, Pos('-', Keywords[Idx]) -1);
              If Length(Result) <> CurWordLen Then
                 Result := Copy(Result, Length(CurWord), Length(Keywords[Idx]) - CurWordLen)
              Else
                 Result := '';
           End;
        End;
     2: Begin // Indeterminate - so test for all keywords, then vars - user may be entering a direct expression.
           Idx := KeywordsHash[Ord(Curword[1])-65];
           While (Not CompareStrToSubStr(CurWord, Keywords[Idx], 1, CurWordLen)) or
                 (KeywordsIDXTable[Idx] >= KeywordCutoff) Do Begin
              Inc(Idx);
              If Idx > NumReserved Then Break;
           End;
           If Idx > NumReserved Then Begin // Function?
              Idx := KeywordsHash[Ord(Curword[1])-65];
              While (Not CompareStrToSubStr(CurWord, Keywords[Idx], 1, CurWordLen)) or
                    (KeywordsIDXTable[Idx] < KeywordCutoff) Do Begin
                 Inc(Idx);
                 If Idx > NumReserved Then Break;
              End;
              If Idx > NumReserved Then Begin // Variable?
                 If VariablesWindow.ListView1.Items.Count > 0 Then Begin
                    Idx := 0;
                    LowerWord := Lowercase(CurWord);
                    While (Idx < VariablesWindow.ListView1.Items.Count) and
                          (Not CompareStrToSubStr(LowerWord, VariablesWindow.ListView1.Items[Idx].Caption, 1, CurWordLen)) Do
                       Inc(Idx);
                    If Idx >= VariablesWindow.ListView1.Items.Count Then
                       Result := ''
                    Else Begin
                       Result := VariablesWindow.ListView1.Items[Idx].Caption;
                       Result := Copy(Result, CurWordLen, Length(VariablesWindow.ListView1.Items[Idx].Caption)+1 - CurWordLen);
                    End;
                 End;
              End Else Begin
                 Result := Copy(Keywords[Idx], 1, Pos('-', Keywords[Idx]) -1);
                 If Length(Result) <> CurWordLen Then
                    Result := Copy(Result, Length(CurWord), Length(Keywords[Idx]) - CurWordLen)
                 Else
                    Result := '';
              End;
           End Else Begin
              Result := Copy(Keywords[Idx], 1, Pos('-', Keywords[Idx]) -1);
              If Length(Result) <> CurWordLen Then
                 Result := Copy(Result, Length(CurWord), Length(Keywords[Idx]) - CurWordLen)
              Else
                 Result := '';
           End;
        End;
  End;
End;

Procedure TBASinOutput.AcceptPrediction;
Begin
  UpdateCursorPos(EditorSelEnd+1, False);
  While BASICMem[CursOffset -1] = #13 Do
     UpdateCursorPos(CursOffset -1, False);
  RepaintBASIC(True);
  If Opt_EditorSounds Then MakeSound(1);
  ShowingPrediction := False;
End;

Procedure TBASinOutput.ClearPrediction;
Begin
  BASICMem := Copy(BASICMem, 1, EditorSelAnchor -1)+Copy(BASICMem, EditorSelAnchor + EditorSelLength, 999999);
  EditorSelAnchor := CursOffset;
  EditorSelLength := 0;
  ShowingPrediction := False;
End;

Procedure TBASinOutput.DoMessages;
Begin
  Application.ProcessMessages;
End;

procedure TBASinOutput.PopupMenu1Popup(Sender: TObject);
Var
  PosInString, IsToken, InString, REMCommand, Done: Boolean;
  SavePos, CurPos, Idx, Idx2, Index, LineStart, StartPos, SearchPos, MaxLen, MaxIdx: Integer;
  TempStr: AnsiString;
  FoundTokens: TStringlist;
  Expr: TExpression;
Label
  Exit_Proc;
begin

  // Get the cursor pos, then figure out what the hell it is.
   FoundTokens := TStringlist.Create;

  If Sender <> nil Then
     CurPos := GetCharPos(MousePos.X, MousePos.Y)
  Else
     CurPos := CursOffset;

  SavePos := CurPos;

  Idx := CurPos;
  While (Idx >1) and (BASICMem[Idx] <> #13) Do
     Dec(Idx);
  If BASICMem[Idx] = #13 Then
     LineStart := Idx +1
  Else
     LineStart := Idx;

  PosInString := False;
  While Idx <= CurPos Do Begin
     If BASICMem[Idx] = '"' Then Begin
        If Not PosInString Then Begin
           // String starts here
           PosInString := True;
           StringStart := Idx;
           StringLen := 0;
        End Else // In a String
           If BASICMem[Idx+1] <> '"' Then // Next char is a quote mark?
              PosInString := False
           Else Begin
              Inc(Idx);
              Inc(StringLen);
           End;
     End Else
        If PosInString Then
           Inc(StringLen);
     Inc(Idx);
  End;

  InString := True;
  While (Idx <= Length(BASICMem)) and InString Do begin
     If BASICMem[Idx] = '"' Then Begin
        If BASICMem[Idx +1] <> '"' Then
           InString := False
        Else Begin
           Inc(StringLen);
           Inc(Idx);
        End;
     End Else
        If BASICMem[Idx] = #13 Then
           InString := False
        Else
           Inc(StringLen);
     Inc(Idx);
  End;

  N16.Visible := True;
  Token1.Visible := True;
  For Idx := 0 To PopupMenu1.Items.Count -1 Do
     (PopUpMenu1.Items[Idx] as TMenuItem).ImageIndex := -1;

  Idx := 0;
  If (ProgramIs128k and (BASICMem[CurPos] < #163)) or ((Not ProgramIs128k) and (BASICMem[CurPos] < #165)) Then Begin

     IsToken := False;

     Done := False;
     While (CurPos > 1) and (BASICMem[CurPos] <> #13) and (BASICMem[CurPos] in ['0'..'9', 'a'..'z', '$', 'A'..'Z']) Do
        Dec(CurPos);
     While Not (BASICMem[CurPos] in ['0'..'9', 'a'..'z', '$', 'A'..'Z']) Do
        Inc(CurPos);

     If CurPos > Length(BASICMem) Then
        CurPos := Max(Length(BASICMem)-1, 1);

     While Not Done Do Begin

        If Pos(Uppercase(BASICMem[CurPos]), AsciiKeywords[Idx]) > 0 Then Begin
           Index := 1;
           While Index <= Length(AsciiKeywords[Idx]) Do Begin
              If AsciiKeywords[Idx][Index] = UpperCase(BASICMem[CurPos]) Then Begin
                 StartPos := CurPos -(Index -1);
                 SearchPos := StartPos;
                 While SearchPos < StartPos + Length(AsciiKeywords[Idx]) Do Begin
                 If UpperCase(BASICMem[SearchPos]) <> AsciiKeywords[Idx][(SearchPos - StartPos)+1] Then
                       Break
                    Else
                       Inc(SearchPos);
                 End;
                 If SearchPos = StartPos + Length(AsciiKeywords[Idx]) Then
                    FoundTokens.Add(AnsiChar(Idx)+#255+IntToStr(StartPos));
              End;
              Inc(Index);
           End;
        End;

        If Not Done Then Begin
           Inc(Idx);
           If Idx = 93 Then Done := True;
        End;

     End;

  End Else Begin

     // If it's a keyword Token, then it can be detokenised.
     Idx := Ord(BASICMem[CurPos]) - 163;
     FoundTokens.Add(AnsiChar(Idx)+#255+IntToStr(CurPos));
     IsToken := True;

  End;


  //r15
     MaxLen := 0;
  MaxIdx := 0;
  If FoundTokens.Count > 0 Then Begin
     For Idx := 0 To FoundTokens.Count -1 Do
        If Length(AsciiKeywords[Ord(FoundTokens[Idx][1])]) > MaxLen Then Begin
           MaxLen := Length(AsciiKeywords[Ord(FoundTokens[Idx][1])]);
           MaxIdx := Idx;
        End;
     Idx := Ord(FoundTokens[MaxIdx][1]);
     TokenisePoint := StrToInt(Copy(FoundTokens[MaxIdx], Pos(#255, FoundTokens[MaxIdx])+1, 9999));
     If IsToken Then
        TokeniseLen := 1
     Else
        TokeniseLen := Length(AsciiKeywords[Idx]);
  End Else
     Idx := 93;
  //r15


  If Idx < 93 Then Begin // Found a keyword

     Token1.Caption := AsciiKeywords[Idx];
     Token1.ImageIndex := Idx;
     Help2.Visible := True;
     AddNote1.Caption := 'Add Note to Line ' + IntToStr(CursLineNum) + ':' + IntToStr(CursStatementNum);

     If IsToken Then Begin
        Tokenise1.Caption := 'Detokenise';
        Tokenise1.Tag := -Idx;
     End Else Begin
        Tokenise1.Caption := 'Tokenise';
        Tokenise1.Tag := Idx;
     End;
     Tokenise1.Visible := IsToken or PosInString;
     EditVariable1.Visible := False;
     FindLine1.Visible := False;
     FindLine1.Caption := 'Find Line';
     // If it's a GOSUB/GOTO etc then get the line number
     If Idx in [39, 62, 66, 73, 74, 77, 80, 84] Then Begin
        If Idx <> 80 Then
           While (CurPos < Length(BASICMem)) and Not (BASICMem[CurPos] in ['0'..'9']) Do Begin
              If BASICMem[CurPos] = #13 Then Goto Exit_Proc;
              Inc(CurPos);
           End;
        If CurPos < Length(BASICMem) Then Begin
           If Idx = 77 Then Begin // LIST can have a channel assigned, so skip it
              If BASICMem[CurPos] = '#' Then Begin
                 While (CurPos < Length(BASICMem)) and (BASICMem[CurPos] <> ',') Do
                    Inc(CurPos);
                 If CurPos = Length(BASICMem) Then Goto Exit_proc;
                 Inc(CurPos);
              End;
           End;
           // Get the text that forms the number or the expression, or for NEXT, get the variable.
           TempStr := '';
           If Idx <> 80 Then Begin // NEXT is a special case dealt with seperately.
              While (CurPos < Length(BASICMem)) and Not (BASICMem[CurPos] in [':', #13]) Do Begin
                 TempStr := TempStr + BASICMem[CurPos];
                 Inc(CurPos);
              End;
              // Now evaluate the string.
              Expr.Expression := TempStr;
              Expr.SyntaxChecked := False;
              EvaluateExpr(Expr);
              If Expr.ResultType = 1 Then Begin
                 FindLine1.ImageIndex := Round(Expr.ResultNum);
                 FindLine1.Visible := True;
              End;
           End Else Begin
              // Get the variable and it's looping point
              If VariablesWindow.ListView1.Items.Count > 0 Then Begin
                 Idx := 1;
                 Inc(CurPos, 4);
                 While (CurPos < Length(BASICMem)) and Not (BASICMem[CurPos] in ['a'..'z', 'A'..'Z']) Do
                    Inc(CurPos);
                 While Idx < VariablesWindow.ListView1.Items.Count Do Begin
                    If Uppercase(VariablesWindow.ListView1.Items[Idx].Caption) = UpperCase(BASICMem[CurPos]) Then
                       If VariablesWindow.ListView1.Items[Idx].SubItems[0] = 'FOR Var' Then
                          Break;
                    Inc(Idx);
                 End;
                 If Idx < VariablesWindow.ListView1.Items.Count Then Begin
                    TempStr := VariablesWindow.ListView1.Items[Idx].SubItems[1];
                    TempStr := Copy(TempStr, Pos('[', TempStr)+1, 999999);
                    FindLine1.ImageIndex := StrToIntDef(Copy(TempStr, 1, Pos(':', TempStr)-1), -1);
                    TempStr := Copy(TempStr, Pos(':', TempStr)+1, 999999);
                    TempStr := Copy(TempStr, 1, Pos(']', TempStr) -1);
                    EditVariable1.ImageIndex := StrToIntDef(TempStr, -1);
                    If (EditVariable1.ImageIndex > -1) and (FindLine1.ImageIndex > -1) Then Begin
                       FindLine1.Caption := 'Find Loop point';
                       FindLine1.Visible := True;
                    End;
                 End;
              End;
           End;
        End;
     End;
     WatchVariable1.Visible := False;

  End Else If VariablesWindow.ListView1.Items.Count > 0 Then Begin // Search for a variable instead.

     Idx := 0;
     Done := False;
     FoundTokens.Clear;

     While Not Done Do Begin

        TempStr := Uppercase(VariablesWindow.ListView1.Items[Idx].Caption);
        If Pos(Uppercase(BASICMem[CurPos]), TempStr) > 0 Then Begin
           Index := 1;
           While Index <= Length(TempStr) Do Begin
              If TempStr[Index] = UpperCase(BASICMem[CurPos]) Then Begin
                 StartPos := CurPos -(Index -1);
                 SearchPos := StartPos;
                 While SearchPos < StartPos + Length(TempStr) Do Begin
                    If UpperCase(BASICMem[SearchPos]) <> TempStr[(SearchPos - StartPos)+1] Then
                       Break
                    Else
                       Inc(SearchPos);
                 End;
                  If SearchPos = StartPos + Length(TempStr) Then
                    FoundTokens.Add(IntToStr(Idx));
              End;
              Inc(Index);
           End;
        End;

        If Not Done Then Begin
           Inc(Idx);
           If Idx = VariablesWindow.ListView1.Items.Count Then Done := True;
        End;

     End;

      //r15
        MaxLen := 0;
     MaxIdx := 0;
     If FoundTokens.Count > 0 Then Begin
        For Idx := 0 To FoundTokens.Count -1 Do
           If Length(VariablesWindow.ListView1.Items[StrToInt(FoundTokens[Idx])].Caption) > MaxLen Then Begin
              MaxLen := Length(VariablesWindow.ListView1.Items[StrToInt(FoundTokens[Idx])].Caption);
              MaxIdx := Idx;
           End;
        Idx := StrToInt(FoundTokens[MaxIdx]);
     End Else
        Idx := VariablesWindow.ListView1.Items.Count;
      //r15

     If Idx < VariablesWindow.ListView1.Items.Count Then Begin // Found a variable

        //Token1.Caption := //TempStr;
        EditVariable1.ImageIndex := Idx;
        WatchVariable1.ImageIndex:= Idx;
        Token1.Caption := VariablesWindow.ListView1.Items[Idx].Caption;
        if  Copy(Token1.Caption, Length(Token1.Caption), 1)='$' Then Begin
                Token1.Caption:= 'String Variable: '+Token1.Caption;
                end else begin
                Token1.Caption:= 'Float Variable: '+Token1.Caption ;
        End;

        Help2.Visible := False;
        Tokenise1.Visible := False;
        EditVariable1.Visible := True;
        FindLine1.Visible := False;
        WatchVariable1.Visible := True;

     End Else Begin

        N16.Visible := False;
        Token1.Visible := False;
        Help2.Visible := False;
        EditVariable1.Visible := False;
        FindLine1.Visible := False;
        WatchVariable1.Visible := False;

     End;

  End Else Begin

     Tokenise1.Visible := False;
     N16.Visible := False;
     Token1.Visible := False;
     Help2.Visible := False;
     EditVariable1.Visible := False;
     FindLine1.Visible := False;
     WatchVariable1.Visible := False;



  End;

  if Not Token1.Visible Then Begin
        //There is nothing there, so show selection
        if (EditorSelEnd - EditorSelStart)<>0 Then Begin
        TempStr := InsertEscapes(Copy(BASICMem, EditorSelStart, (EditorSelEnd - EditorSelStart)));
        End Else
        TempStr := InsertEscapes(Copy(BASICMem, EditorSelStart, 1));

        if Length(TempStr)>15 Then Token1.Caption:= 'Selection: '+Copy(TempStr, 1, 14)+'...'  Else Token1.Caption:= 'Selection: '+TempStr;
        Token1.Visible:=True;
  End;
  Exit_Proc:

  Paste2.Enabled := ClipBoard.AsText <> '';
  Cut2.Enabled := (EditorSelLength <> 0);
  Copy2.Enabled := Cut2.Enabled;
  AddSnippet1.Enabled := Cut2.Enabled;
  AddNote1.Enabled := Cut2.Enabled;
  
  StringOperation1.Visible := PosInString;
  WordWrapString1.Visible := PosInString and (StringLen > 32);
  If PosInString Then Begin
     // If it's a PRINT, then you can split with "," - else it has to be spaces.
     While BASICMem[LineStart] in ['0'..'9', ' '] Do
        Inc(LineStart);
     TempStr := '';
     While BASICMem[LineStart] in ['A'..'Z', 'a'..'z', '$'] Do Begin
        TempStr := TempStr + BASICMem[LineStart];
        Inc(LineStart);
     End;
     SplitAt32Chars1.Enabled := (Uppercase(TempStr) = 'PRINT') or (UpperCase(TempStr) = 'INPUT');
     WordWrapString1.Tag := StringStart;
  End;

  // If we've clicked a line with a valid number, then get that number for the debug items.
  If CurPos >= Length(BASICMem) Then Dec(CurPos);

  While (CurPos > 1) and (BASICMem[CurPos] <> #13) Do
     Dec(CurPos);

  While (CurPos < Length(BASICMem)) and (BASICMem[CurPos] = #13) Do
     Inc(CurPos);

  Idx2 := CurPos;
  TempStr := '';
  While BASICMem[CurPos] in ['0'..'9'] Do Begin
     TempStr := TempStr + BASICMem[CurPos];
     Inc(CurPos);
  End;

  // Now Find the Statement number.

  CurPos := Idx2;
  InString := False;
  REMCommand := False;
  RunToCursor1.ImageIndex := 1;
  While CurPos < SavePos Do begin
     If (UpperCase(Copy(BASICMem, CurPos, 4)) = 'REM ') and not InString then
        REMCommand := True;
     If BASICMem[CurPos] = '"' then
        InString := Not InString;
     If Not InString and Not REMCommand Then
        If (BASICMem[CurPos] = ':') or (UpperCase(Copy(BASICMem, CurPos-4, 5)) = 'THEN ') Then
           RunToCursor1.ImageIndex := RunToCursor1.ImageIndex +1;
     Inc(CurPos);
  End;

  If TempStr <> '' Then Begin
     ToggleBreakPoint1.Enabled := True;
     RunToCursor1.Enabled := True;
     GoToCursor1.Enabled := True;
     ToggleBreakPoint1.ImageIndex := StrToInt(TempStr);
  End Else Begin
     ToggleBreakPoint1.Enabled := False;
     RunToCursor1.Enabled := False;
     GoToCursor1.Enabled := False;
  End;

end;

procedure TBASinOutput.Help2MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
Var
  mnu: TMenuItem;
begin
  Mnu := Sender as TMenuItem;
  width := aCanvas.TextWidth(mnu.caption) +32 +aCanvas.TextWidth(ShortCutToText(mnu.Shortcut));

end;

procedure TBASinOutput.Tokenise1Click(Sender: TObject);
begin

  // Can't go through the normal MenuItemClick() proc, as we need the tag.

  AddUndo;

   If Tokenise1.Tag > 0 Then Begin
     BASICMem := Copy(BASICMem, 1, TokenisePoint -1)+Copy(BASICMem, TokenisePoint + TokeniseLen, 999999);
     BASICMem := Copy(BASICMem, 1, TokenisePoint -1)+AnsiChar(Tokenise1.Tag+163)+Copy(BASICMem, TokenisePoint, 999999);
     If CursOffset >= TokenisePoint Then
        Dec(CursOffset, TokeniseLen -1);
  End Else Begin
     BASICMem := Copy(BASICMem, 1, TokenisePoint -1)+Copy(BASICMem, TokenisePoint + 1, 999999);
     BASICMem := Copy(BASICMem, 1, TokenisePoint -1)+AsciiKeywords[-Tokenise1.Tag]+Copy(BASICMem, TokenisePoint, 999999);
     If CursOffset >= TokenisePoint Then
        Inc(CursOffset, Length(AsciiKeywords[-Tokenise1.Tag])-1);
  End;
  BASICChanged := True;
  TokeniseEditText(False);
  RepaintBASIC(True);

end;

Procedure TBASinOutput.WordWrap(Start, Len, Mode: Integer);
Var
  Idx, Count: Integer;
  Content, Result: String;
Begin

  Count := Len;
  Content := '';
  Idx := Start +1;
  While Count > 0 Do Begin
     Content := Content + BASICMem[Idx];
     Inc(Idx);
     Dec(Count);
  End;

  Result := '"';

  If Mode = 0 Then Begin // Split

     While Length(Content) > 32 Do Begin

        Count := 32;
        While (Count > 0) and (Content[Count] <> ' ') Do
           Dec(Count);
        If Count = 0 Then Count := 32;
        Result := Result + Copy(Content, 1, Count -1) + '"'#39'"';
        Content := Copy(Content, Count +1, 999999);
        While Copy(Content, 1, 1) = ' ' Do
           Content := Copy(Content, 2, 999999);
     End;

     If Length(Content) > 0 Then
        Result := Result + Content + '"'
     Else
        Result := Copy(Result, 1, Length(Result) -2);

  End Else Begin // Insert

     Idx := 32;
     While Idx < Length(Content) Do Begin
        Count := Idx;
        While (Count > 0) and (Content[Count] <> ' ') Do Dec(Count);
        If Count > 0 Then
           While Content[Idx] <> ' ' Do
              Content := Copy(Content, 1, Count)+' '+Copy(Content, Count+1, 999999);
        Inc(Idx, 32);
     End;

     Result := '"'+Content+'"';

  End;

  // Remove the original String, and replace with the new split or expanded string(s).

  AddUndo;
  BASICMem := Copy(BASICMem, 1, StringStart -1) + Result + Copy(BASICMem, StringStart + StringLen +2, 999999);
  TokeniseEditText(False);
  MakeSound(2);
  If CursOffset > StringStart then
     UpdateCursorPos(CursOffset + (Length(Result) -2) - Len, False);

  RepaintBASIC(False);
  MakeCursorVisible;

End;

Procedure TBASinOutput.TokeniseString;
Var
  Count, Idx: Integer;
  Content, NewStr: String;
Begin

  AddUndo;

  Count := StringLen;
  Content := '';
  Idx := StringStart +1;
  While Count > 0 Do Begin
     Content := Content + BASICMem[Idx];
     Inc(Idx);
     Dec(Count);
  End;

  NewStr := TokeniseLine(Content, True);
  If CursOffset >= StringStart Then
     Dec(CursOffset, Length(Content) - Length(NewStr));

  BASICMem := Copy(BASICMem, 1, StringStart)+Copy(BASICMem, StringStart + 1 + StringLen, 999999);
  BASICMem := Copy(BASICMem, 1, StringStart)+NewStr+Copy(BASICMem, StringStart +1, 999999);

  MakeSound(2);
  TokeniseEditText(False);
  RepaintBASIC(true);
  MakeCursorVisible;

End;

Procedure TBASinOutput.DetokeniseString;
Var
  Count, Idx: Integer;
  Content, NewStr: String;
Begin

  Count := StringLen;
  Content := '';
  Idx := StringStart +1;
  While Count > 0 Do Begin
     Content := Content + BASICMem[Idx];
     Inc(Idx);
     Dec(Count);
  End;

  NewStr := DeTokeniseLine(Content, True);
  If CursOffset >= StringStart Then
     Inc(CursOffset, Length(NewStr) - Length(Content));

  AddUndo;
  BASICMem := Copy(BASICMem, 1, StringStart)+Copy(BASICMem, StringStart + 1 + StringLen, 999999);
  BASICMem := Copy(BASICMem, 1, StringStart)+NewStr+Copy(BASICMem, StringStart +1, 999999);

  MakeSound(2);
  TokeniseEditText(False);
  RepaintBASIC(true);
  MakeCursorVisible;

End;



Procedure TBASinOutput.SetSourceMarker(Index: Integer);
Begin
  SourceMarkers[Index].MarkedLine := CursLineNum;
  SourceMarkers[Index].MarkedStatement := CursStatementNum;
  SourceMarkers[Index].Assigned := True;
End;

Procedure TBASinOutput.GetSourceMarker(Index: Integer);
Begin
  If SourceMarkers[Index].Assigned Then
     FindAndActivateLine(SourceMarkers[Index].MarkedLine, SourceMarkers[Index].MarkedStatement);
End;

Procedure TBASinOutput.ClearSourceMarkers;
Var
  Idx: Integer;
Begin
  For Idx := 0 To 9 Do
     SourceMarkers[Idx].Assigned := False;
  RepaintBASIC(True);
End;

procedure TBASinOutput.simplecon1Click(Sender: TObject);
begin

      ShowWindow(ConsoleOutForm,False);

      If (not Opt_ConsoleAddon)Then Begin
        ConsoleOutForm.Caption:= 'SimpleCON i/o window - INACTIVE';
        ConsoleOutForm.Edit1.Text:='[Interface Disabled]';
          End Else Begin
        ConsoleOutForm.Caption:= 'SimpleCON i/o window - Active' ;

      End;

end;








procedure TBASinOutput.WMCopyData(var Msg: TWMCopyData);
var
  s : string;
begin
  s := PChar(Msg.copyDataStruct.lpData);
  if (s<>'') then loadquotequote(s);

end;




function TBASinOutput.GetShortName(sLongName: string): string;

var
  sShortName    : string;
  nShortNameLen : integer;
begin
  SetLength(sShortName, MAX_PATH);
  nShortNameLen := GetShortPathName(
    PChar(sLongName), PChar(sShortName), MAX_PATH - 1
  );
  if (0 = nShortNameLen) then
  begin
    // handle errors...
  end;
  SetLength(sShortName, nShortNameLen);
  Result := sShortName;
end;



procedure TBASinOutput.AdjustCursorPoint;
begin
        CursorPoint.Y:=CursorPoint.Y*Opt_FontScale;    // Arda Workaround
        CursorPoint.X:=CursorPoint.X*Opt_FontScale;    // Arda Workaround
end;

procedure TBASinOutput.Timer3Timer(Sender: TObject);
begin
  if (Opt_AutoBackup) and not Registers.EmuRunning Then Begin

        if  (trim(Copy(CurProjectName,1,8))<>'autoback') Then  Begin
          If ProjectSaved Then  Begin
              SaveCurrentProgram(BASinDIR+'\autoback\autoback'+IntToStr(AutoBack)+'.bas');
              AutoBack:=AutoBack+1;
              If (AutoBack>9) Then AutoBack:=0;
          End;
        End;
  End;
end;


end.

// history & todo:

// 1.79.4

// Added - UDG Editor Window, "Save" file option, CTRL+S, CTRL+O shortcuts, keeps track of open file and notify if there is a pixel modified as * in the title bar.
// Fixed - SimpleCON register couldn't be displayed as text because it was initialized as 0, now it is spaces.
// Changed-SimpleCON window was popping up in every character output. Now it only pops up when linefeed.


// 1.79.3
// Fixed - Profile window was showing wrong token in strings. (reported by: lippmaje)



// 1.79.1 19.05.2022
// Added - BasinC Snippets
// Changed - SimpleCon behaviour. Not port 1515 commands sets the byte *then* increment the index. Port 1259,0 feeds the line to the log instead of 255.

// 1.78 (24.10.2021)
// Fixed - IF x THEN REM couldn't parsed by basinc, now patched, but may present new issues, due it's just a workaround.
// Fixed - When you execute a direct command, basic was corrupted temporarily. It's fixed by removing a wild syntax cropper.


// 1.77 (14.10.2021)
// Added - UDG editor character setup option
// Added - UDG editor keyboard shortcuts
// Added - Add/Edit a note to Notes Window by right clicking on a statement in listing
//         This is a step to overlay comments over program listing in a future release
// Added - Auto Show Note option in BasinC Options window. Disabling this will prevent project window to popup, even if a BAS file does contain notes.
// Fixed - save as.. Does not modify project name. (A workaround rather than a fix. Needs a bit more time)
// Fixed - Undo/redo buttonts on Image Editor (former Screen Paintbox) wouldn't work at start.
// Changed-Some Ctrl+Alt+ keyboard shortcuts was clashing with AltGR+A..U graphics mode shortcut. So all of them changed. Sorry.
// Added - Hotkey Toggle Tools. You may open *and* close following tools with single keycombo:
//         Crtl-T Tokens, Ctrl+Alt+Z Notes, Ctrl+Alt+X Tapes, Ctrl+I Image Editor, Ctrl+U Udg Editor
// Changed-English tool names Graphics/Sprite Editor become UDG Editor, and Screen Paintbox become Image Editor for simplicity.


// 1.76 (11.10.2020)
// Added - Project Notes Window
// Fixed - tooltip variables not showing correctly (hopefully fixed)

//1.75 (22.05.2019)
// Fixed - Parser fix (reported by: James Davis)

//1.74
// Added - Save Display Window as BMP
// Added - basin now is on github! https://github.com/ref-xx/basinc


//1.73
// Fixed - Undocumented fixes

// 1.72
// Added: -dumptxt commandline parameter. Extracts and saves BASIC portion of a basinc compatible program (eg. BAS/SNA/TAP).
//                  Usage: basinc <filename> -dumptxt   
//                  Example: basinc oregon.tap -dumptxt   (will create oregon.tap_BASIC.txt in same folder)

// 1.71

// Added: 4x,5x,6x font sizes for hi-dpi non-zoomed screens.

// Fixed: variables with 90ish numbers like a91, b95 weren't properly tokenized.


// 1.7      20.11.2017

// Added new toolbar buttons
// Added toggle breakpoint navigation buttons (see view menu)
// Added auto backup feature. Basinc saves your work in the background every 3 minutes.
//                            it keeps 10 copies going back up to 30 minutes in /autoback folder.
//                            To turn this feature off, use: Options > Filing > Auto Backup
// Changed - Due to large changes in ini and bin files, basinc now requires basinC.bin and basinC.ini, but it still uses basin.chm as help file.

// 1.7a

//  Changed - Broken Undo/Redo behaviour. BETA - Save often to avoid data loss.

//  Fixed - Right button popup menu context operators should detect *declared* variables correctly now
//  Fixed - Green Ruler Mark at the bottom of the screen should work ok even when cursor in a string.
//  Fixed - Drag&Drop files to main window works again.
//  Fixed - Esc key no longer breaks the program if Basinc is not in focus
//  Fixed - Token table wasn't printing first keywords correctly (eg. RND/SPECTRUM).
//  Fixed - File dialogs won't stuck at search results anymore
//  Fixed - Cosmetic fixes of profiling window
//  Fixed - While using double and triple size fonts, find and replace was not showing the result in the editor

//  Added - Shift+Clicking token table inserts byte code rather than token itself
//  Added - Tape Browser now accept files via drag&drop
//  Added - When in full speed emulation, a small arrow is shown at the bottom right corner of display window
//  Added - Display window preset sizes: 300%,400%,600%
//  Added - German Translation (Uwe Geiken)
//  Added - External Utility menu item. This item sends basinc's memory snapshot to an external utility, such as an emulator.


// 1.697

//  Added - SimpleCon window warns user more if it's disabled in the options
//  Added - REM speed <x> command. x is 1-50 (an integer equivalent to Mhz). It may not perform accordingly with commands which perform screen update.


// 1.696
//  Added - single instance mode
//  Added - Command line option: give filename to load
//  Added - Profiling window New (clears profiling results and starts new profiling) and Refresh (shows immediate results) button.
//  fixed - token table was printing wrong keywords (again).
//  fixed - Force Break (Undo) will go back to earlier state than intended (thus overwriting your program).

// BUGS to fix  - arda

  // pd- The printing process happened to be such a high consuming task that It almost froze my whole system for several minutes...
  // a- top toolbar "bytes free/address" display corrupts when resizing horizontally
  // a- tracing in double size will not follow cursor
  // a- undo still causes corruptions




// FEATURES to add (in order of priority)

  // colour code (string) creator
  // convert tape block to binary import
  // UDG editor - animation preview, mask generation (transparency with sprite?)
  // BEEP/PLAY editor
  // RAMDisk explorer
  // editor scratch pad - plain text in a speccy style
  // REM SPEED <x>   - done v1.697
  // REM Killer/Source optimiser
  // Compiler

  // a- Bascloud - internet repository
  // a- Mem manager

// HELP File entries left to complete

  // Paintbox
  // Document the right-click "Tokenise/detokenise all" menu items.
  // Binary import system
  // UDG editor's new import/export system
  // Printing.html?
  // 128k sysvars
  // update/bugfix ascii chart, add undoc opcodes

  // ULA64
  // SimpleCon
  // Languages
  // Kempston Mouse Support
  // Allow instances
  // Project Notes Editor
  // Snippets




