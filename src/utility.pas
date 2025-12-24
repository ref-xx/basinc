unit Utility;

interface

Uses StdCtrls, ExtCtrls, Dialogs, Graphics, Windows, Classes, SysUtils,
     Messages, Parser, Forms, Controls, FastDIB, FastCore, Basinet;

Type
  TByteArray = array of Byte;

  TRenderMethod = (rmGDI, rmBilinear, rmScale2x, rmHq2x, rmSuper2xSAI, rmSuperEagle);
  TGraphicsMethod = (gmAltGr, gmScrollLock, gmNumLock);
  TBracketMethod = (bmNone, bmAutoType, bmComplete);
  TMouseMode = (miNone, miWindows, miCrosshair);

  TWorkerThread = Class(TThread)
     NeedSuspend: Boolean;
     DisplaySuspended: Boolean;
     procedure Execute; override;
     procedure HandleMessages;
  End;

  // A new scrolling control, which overrides TScrollbox to provide OnScroll messages.

  TVScrollEventType = (vsLineUp, vsLineDown, vsPageUp, vsPageDown, vsThumbPos, vsThumbTrack, vsTop, vsBottom, vsEndScroll);
  THScrollEventType = (hsLineLeft, hsLineRight, hsPageLeft, hsPageRight, hsThumbPos, hsThumbTrack, hsLeft, hsRight, hsEndScroll);
  TVScrollEvent = procedure(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType) of object;
  THScrollEvent = procedure(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType) of object;

  TNewScrollBox = class(TScrollBox)
  private
    FOnVScroll: TVScrollEvent;
    FOnHScroll: THScrollEvent;
    procedure WMVScroll(var Message: TWMScroll); message WM_VScroll;
    procedure WMHScroll(var Message: TWMScroll); message WM_HScroll;
  protected
    procedure VScroll(Pos: Integer; EventType: TVScrollEventType); virtual;
    procedure HScroll(Pos: Integer; EventType: THScrollEventType); virtual;
  public
    ScrollInfo: TScrollInfo;
    constructor Create(AOwner: TComponent); override;
  published
    property OnVerticalScroll: TVScrollEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: THScrollEvent read FOnHScroll write FOnHScroll;
  end;



  TColourLabel = Class(TLabel)
  public
    Str: String;
    Procedure Paint; Override;
  End;

  Procedure INITWorkerThread;
  Procedure CloseWorkerThread;
  Procedure SuspendWorkerThread;
  Procedure ResumeWorkerThread;
  Function  LoadEditorFont(Handle: Hwnd; Filename: String; Enabled: Boolean): Boolean;
  Function  RolWord(Value: Word; Count: Byte): Word;
  Function  FastIntToHex(Value: Word): String;
  Procedure AlignAnchors(Form: TForm);
  Procedure MoveForm(Form: TForm; X, Y: Integer);
  Procedure CentreForm(Form: TForm; X, Y: Integer);
  Procedure CentreFormOnForm(FormToMove: TForm; FormToMoveTo: TForm);
  Procedure ShowWindow(FormToShow: TForm; Modal: Boolean);
  Function  ShrinkFilename(Value: String; Width: Integer): String;
  Procedure StoreEmulationState(Filename: String; Var State: TEmulationState);
  Procedure RestoreEmulationState(Filename: String);
  Procedure SetProjectName(Name: String);
  Function  TrimExtension(Name: String): String;

  Function ArrayToTBytes(Value: array of byte; Ln: integer; Start: integer = 0): TByteArray;
  function ColorToHex(Color: TFColor): string;

  Procedure GetBASinDIR;

  function ShowCustomDlg(const Msg: string; IconType: TMsgDlgType; const Btn1, Btn2, Btn3: string): Integer;


  Procedure LoadOptions;
  Procedure SaveOptions;

  Function  INIFindSection(Section: String): Integer;
  Function  INIFindEntry(Section, Entry: String): Integer;

  Function  INIRead(Section, Entry: String; Default: Boolean): Boolean; Overload;
  Function  INIRead(Section, Entry: String; Default: Extended): Extended; Overload;
  Function  INIRead(Section, Entry: String; Default: String): String; Overload;
  Function  INIRead(Section, Entry: String; Default: Integer): Integer; OverLoad;
  Function  INIRead(Section, Entry: String; Default: DWord): DWord; OverLoad;
  Function  INIRead(Section, Entry: String; Default: Word): Word; OverLoad;

  Procedure INIWrite(Section, Entry: String; Value: Boolean); OverLoad;
  Procedure INIWrite(Section, Entry: String; Value: String); Overload;
  Procedure INIWrite(Section, Entry: String; Value: Integer); Overload;
  Procedure INIWrite(Section, Entry: String; Value: Extended); Overload;
  Procedure INIWrite(Section, Entry: String; Value: DWord); Overload;
  Procedure INIWrite(Section, Entry: String; Value: Word); Overload;

  Procedure SPECTextToDIB(DIB: TFastDIB; X, Y: Integer; Text: String; Ink, Paper, Bright: Integer; Italics, useCHARS: Boolean);
  Procedure Scale2xDIB(Src, Dst: TFastDIB);
  Procedure Scale3xDIB(Src, Dst: TFastDIB);
  Procedure Scale4xDIB(Src, Dst: TFastDIB);

  Procedure GetShiftState(Var Shift: TShiftState);
  procedure LockControl(c: TWinControl; lock: boolean);

  Function  FloatToStrEx(Number: Extended): String;
  function HexToColor(HexValue: string): TFColor;

  Procedure SizeForm(Form: TForm; X, Y, W, H: Integer);

  function GetBuildInfoAsString(BuildIncluded: Boolean): string;
  procedure GetBuildInfo(var V1, V2, V3, V4: word);



Var

  WorkerThread:           TWorkerThread;
  ParseResult:            String;
  ParseError:             TParseError;
  BASinDir:               String;
  INI:                    TStringList;
  DisplayScale:           Integer;

  ReleaseName:            String = 'BasinC'; //is now set from the Project Options > Version Info.
  iniReleaseName:         String = '';
  ReleaseBuild:           String = 'Private Build';
  ReleaseDate:            String = 'Mar,24 2025';
  DefaultProjectName:     String = 'My Basic';
  CurProjectName:         String = 'My Basic';
  CurProjectFilename:     String = 'My Basic';
  CurProjectFullPathFile: String = '';
  SessionProjectName:     String;
  SessionProjectFilename: String;
  SessionID:              String = '';



    dx,dy: dword;
    KempMouseX,
    KempMouseY:                 Byte;
    OldMousePos:                TPoint;

    ConsoleAddon:               Array[0..255] of byte; // Console Addon Ram.  arda


  // Options Variables
  Opt_FastResets:         Boolean = True;               // Faster Resets
  Opt_Language:           String='English';             // interface language

  Opt_ConsoleAddon:       Boolean = True;               // Console Output Window  arda
  Opt_CheckUpdates:       Boolean = False;               // Alert when update available

  Opt_Indenting:          Boolean = False;              // Text indenting of special loop keywords: FOR..NEXT & IF  arda
  Opt_IndentSize:         Integer = 4;                  // Indent size
  Opt_ShowNotes:          Boolean = True;
  Opt_EnableAI:           Boolean = False;
  Opt_SelectedAIModel:    String = '';
  Opt_BYOK_APIKEY:        String = '';

  SpeedBackup:            Integer = 69888;              // to store original speed of emulation temporarily
  Opt_CPUSpeed:           Integer = 69888;              // The speed (in TStates) of the CPU - Ts/Frame
  Opt_EmulationSpeed:     Integer = 19;                  // adjust sleep duration while silent emulation
  Opt_64Colours:          Boolean = True;               // Use the new 64 colour ULA?
  Opt_AllowMultipleInstances: Boolean = True;           // Allow running multiple instances of basin
  Opt_CheqEditAvailable:  Boolean = False;              // checks for cheq_edit.exe
  Opt_KMouse:             Boolean = True;               // Use Kempston Mouse
  Opt_PCStyleMouse:       Boolean = False;              // When emulating kempston use PC mouse coordinates
  Opt_RenderMethod:       TRenderMethod = rmGDI;        // The method used to draw (and scale) the graphics.
  Opt_Scanlines:          Boolean = False;              // Scanlines - (every other line at 75% brightness)
  Opt_IntegerScaling:     Boolean = False;              // Only allow integer window scales (1x, 2x, 3x etc)
  Opt_MaintainAspect:     Boolean = True;               // Maintain a correct aspect ratio?
  Opt_DisplayFillBorder:   Boolean = True;
  Opt_GraphicsMethod:     TGraphicsMethod = gmAltGr;    // Which key activates graphics mode?
  Opt_ClipCorners:        Boolean = False;              // Rounded corners on the main window?
  Opt_AvgShades:          Boolean = False;              // Stupid Graphic thing.
  Opt_FontScale:          Integer = 2;                  // Size of the Editor Font - x8 pixels.
  Opt_8BitStretch:        Boolean = True;               // On some cards, GDI will fly with this option enabled.

  Opt_Frameskip:          Integer = 1;                  // Frameskip setting
  Opt_AutoFrameSkip:      Boolean = True;

  Opt_FullThrottle:       Boolean = False;              // Run at the most amazing speed possible.
  Opt_ShowRemCommands:    Boolean = False;              // 1.81 - shows raw keywords in rem
  Opt_ShowingSyntax:      Boolean = True;               // Show syntax-checker at startup?
  Opt_AutoList:           Boolean = True;               // Use the "." to automatically number your lines?
  Opt_AutoBracket:        TBracketMethod = bmComplete;  // Use the Autobracket system?
  Opt_CursorToError:      Boolean = True;               // Should the editor cursor jump to the line that errored?
  Opt_SeperateDisplay:    Boolean = True;               // Use a seperate Display window?
  Opt_FollowProgram:      Boolean = False;              // Track program execution in the editor? - a CPU hog though.
  Opt_CharacterRuler:     Boolean = True;               // A ruler device for measuring characters in PRINT statements
  Opt_Controlicons:       Boolean = False;              // Breakpoint control icons
  Opt_ViewInfoLine:       Boolean = False;              // Info Header
  Opt_SubRoutines:        Boolean = True;               // Show Sub-Jump Combobox
  Opt_AutoCollectSubs:    Boolean = True;               // Enable detecting GO SUB commands and add it to Sub-Combo.
  Opt_AutoCollectJumps:   Boolean = True;               // Enable detecting GO TO targets
  Opt_ShowJumpOrigins:    Boolean = True;               // Enable origin previews
  Opt_ShowStatusBar:      Boolean = True;               // Display the status bar?
  Opt_ShowToolBar:        Boolean = True;               // Display the Toolbar?
  Opt_ShowAscii:          Boolean = True;               // Display non-alphanumeric/symbol chars (ie, colour changes) as special glyphs?
  Opt_Predictive:         Boolean = True;               // Use predictive auto-type-ahead text in the editor?
  Opt_OverwriteProtect:   Boolean = False;              // Use Line overwrite protection (green error cursor on overwrite).
  Opt_ProtectNewOnly:     Boolean = False;              // When using overwrite protection, only protect new lines (not edited lines)
  Opt_EvalTimeOut:        DWord   = 5000;               // A five second Time-Out for evaluated expressions.
  Opt_SyntaxHighlight:    Boolean = True;               // Highlight Syntax items?
  Opt_TabSize:            Integer = 16;
  Opt_EnableBasincTips:   Boolean = True;               // Show BasinC tips on startup

  Opt_HighlightKeywords:  Boolean = True;               // Highlight Keywords?
  Opt_KeywordsColour:     Integer = 0;                  // What colour should keywords be?
  Opt_KeywordsBold:       Boolean = True;               // Should they be BOLD type?
  Opt_KeywordsItalic:     Boolean = False;              // With or Without Italics?

  Opt_HighlightFunctions: Boolean = True;
  Opt_FunctionsColour:    Integer = 0;
  Opt_FunctionsBold:      Boolean = True;
  Opt_FunctionsItalic:    Boolean = False;

  Opt_HighlightComments:  Boolean = True;
  Opt_CommentsColour:     Integer = 6;
  Opt_CommentsBold:       Boolean = False;
  Opt_CommentsItalic:     Boolean = True;

  Opt_HighlightSymbols:   Boolean = True;
  Opt_SymbolsColour:      Integer = 0;
  Opt_SymbolsBold:        Boolean = False;
  Opt_SymbolsItalic:      Boolean = False;

  Opt_HighlightVars:      Boolean = True;
  Opt_VarsColour:         Integer = 1;
  Opt_VarsBold:           Boolean = False;
  Opt_VarsItalic:         Boolean = False;

  Opt_HighlightVarsUnDef: Boolean = True;
  Opt_VarsUnDefColour:    Integer = 2;
  Opt_VarsUnDefBold:      Boolean = False;
  Opt_VarsUnDefItalic:    Boolean = False;

  Opt_HighlightNumbers:   Boolean = True;
  Opt_NumbersColour:      Integer = 0;
  Opt_NumbersBold:        Boolean = False;
  Opt_NumbersItalic:      Boolean = False;

  Opt_HighlightLineNums:  Boolean = True;
  Opt_LineNumsColour:     Integer = 0;
  Opt_LineNumsBold:       Boolean = False;
  Opt_LineNumsItalic:     Boolean = False;

  Opt_HighlightStrings:   Boolean = True;
  Opt_StringsColour:      Integer = 0;
  Opt_StringsBold:        Boolean = False;
  Opt_StringsItalic:      Boolean = False;

  Opt_Foreground:         Integer = 0;
  Opt_Background:         Integer = 15;

  Opt_OnlineHelp:         Boolean = True;               //use web help
  Opt_AutoBackup:         Boolean = True;               //Automatically save backup files as BAS
  Opt_AutoBackInterval:   Integer = 3;                  //10 minutes by default
  Opt_AutoLoadSession:    Boolean = False;              // Load the previous session on startup?
  Opt_LoadAutoStart:      Boolean = False;               // Allow BAS files to autostart on loading?
  Opt_AutoStart:          Boolean = False;               // Automatically save programs with Autostart?
  Opt_AutoStartLine:      Word = 1;                     // The line to autostart programs from.
  Opt_z80Version:         DWord = 2;                    // Saves .z80 snapshots as V1.45, V2.01 or V3.5 snaps
  Opt_Always128k:         Integer = 0;                     // Saves snaps as which hardware?   default=always 48
  Opt_SavePretty:         Boolean = False;              // Saves .bas files nicely by splitting up multistatement lines.
  Opt_TapeRewind:         Boolean = True;               // Automatically rewind TZX/TAP tapes when the reach the end.

  Opt_DisplaySnap:        Boolean = False;              // snap display to editor window v1.795
  Opt_DisplayOnTop:       Boolean = False;              // make Display window on top

  Opt_FastPrinting:       Boolean = True;               // Accelerate the ROM Printing routine?
  Opt_SavePrinting:       Boolean = True;               // Store the printed output between sessions?

  Opt_SoundEnabled:       Boolean = True;               // Do we want sounds at all in the Emulation?
  Opt_SoundVolume:        Word = 30000;                 // Volume modifier for sound volume.
  Opt_SoundFrequency:     DWord = 44100;                // Sample frequency in Hz
  Opt_SoundBits:          DWord = 16;                   // 16/8 Bit?
  Opt_SoundStereo:        DWord = 1;                    // 0 = Mono, 1 = Stereo (expansion for ABC/ACB etc later).
  Opt_NumSoundBuffers:    DWord = 8;                    // Number of subdivisions (frames) that the sound buffer is comprised of.
  Opt_SoundLatency:       DWord = 2;                    // Sound latency in frames - compatibility with high latency sound drivers.
  Opt_EditorSounds:       Boolean = True;               // 128k Sounds in the editor?
  Opt_KeyClick48k:        Boolean = True;               // Use a 48k Keyclick sound instead of the 128k version?
  Opt_DSoundSynch:        Boolean = True;               // Use DirectSound buffer synchronisation for timing

  Opt_AsmPasmoAvailable:  Boolean = False;              //if pasmo exists
  Opt_ZX0Available:       Boolean = False;              //if zx0.exe exists

  Opt_AsmStatusBar:       Boolean = True;               // Show the Assembler Statusbar?
  Opt_AsmLabelList:       Boolean = True;               // Show the Assembler's label list to the left of the editor?
  Opt_AsmHexValues:       Boolean = False;              // Display all values as Hex in the Debugger
  Opt_AsmAsciiBytes:      Boolean = False;              // Show the byte values of opcodes as ASCII codes in the debugger

  Opt_AsmForeground:      Integer = 0;                  // As for the above BASIC syntax highlighting, these are for the Assembler's editor.
  Opt_AsmBackground:      Integer = 7;

  Opt_AsmHLRegs:          Boolean = True;
  Opt_AsmRegsColour:      Integer = 3;
  Opt_AsmRegsBold:        Boolean = False;
  Opt_AsmRegsItalic:      Boolean = False;

  Opt_AsmHLComments:      Boolean = True;
  Opt_AsmCommentsColour:  Integer = 0;
  Opt_AsmCommentsBold:    Boolean = False;
  Opt_AsmCommentsItalic:  Boolean = True;

  Opt_AsmHLReserved:      Boolean = True;
  Opt_AsmReservedColour:  Integer = 0;
  Opt_AsmReservedBold:    Boolean = True;
  Opt_AsmReservedItalic:  Boolean = False;

  Opt_AsmHLOpcodes:       Boolean = True;
  Opt_AsmOpcodeColour:    Integer = 0;
  Opt_AsmOpcodeBold:      Boolean = True;
  Opt_AsmOpcodeItalic:    Boolean = False;

  Opt_AsmHLLabels:        Boolean = True;
  Opt_AsmLabelColour:     Integer = 1;
  Opt_AsmLabelBold:       Boolean = True;
  Opt_AsmLabelItalic:     Boolean = False;

  Opt_AsmHLEquates:       Boolean = True;
  Opt_AsmEquateColour:    Integer = 1;
  Opt_AsmEquateBold:      Boolean = True;
  Opt_AsmEquateItalic:    Boolean = False;

  Opt_AsmHLMacros:        Boolean = True;
  Opt_AsmMacroColour:     Integer = 1;
  Opt_AsmMacroBold:       Boolean = True;
  Opt_AsmMacroItalic:     Boolean = False;

  Opt_AsmHLStructs:       Boolean = True;
  Opt_AsmStructColour:    Integer = 1;
  Opt_AsmStructBold:      Boolean = True;
  Opt_AsmStructItalic:    Boolean = False;

  Opt_AsmHLDefines:       Boolean = True;
  Opt_AsmDefineColour:    Integer = 1;
  Opt_AsmDefineBold:      Boolean = True;
  Opt_AsmDefineItalic:    Boolean = False;

  Opt_AsmHLSymbols:       Boolean = True;
  Opt_AsmSymbolColour:    Integer = 0;
  Opt_AsmSymbolBold:      Boolean = False;
  Opt_AsmSymbolItalic:    Boolean = False;

  Opt_AsmHLNumbers:       Boolean = True;
  Opt_AsmNumberColour:    Integer = 0;
  Opt_AsmNumberBold:      Boolean = False;
  Opt_AsmNumberItalic:    Boolean = False;

  Opt_AsmHLStrings:       Boolean = True;
  Opt_AsmStringColour:    Integer = 0;
  Opt_AsmStringBold:      Boolean = False;
  Opt_AsmStringItalic:    Boolean = False;

  Opt_AsmHLErrors:        Boolean = True;
  Opt_AsmErrorColour:     Integer = 2;
  Opt_AsmErrorBold:       Boolean = False;
  Opt_AsmErrorItalic:     Boolean = False;


  Opt_EditorCustomFont:   Boolean = False;
  Opt_EditorFontFolder:   String =  '.\Fonts\';
  Opt_EditorFontFilename: String =  '';

  Opt_ToolFontSize:       Integer = 0;

  Opt_ExternalExec:       String = '<not set>';   //arda add
  Opt_CompoSize:          Bool = False;           // to show special program size calculation for Facebook compos

  Opt_TFCol24:           String = '#08080A';
  Opt_TFCol25:           String = '#57ABFF';
  Opt_TFCol26:           String = '#BFB290';
  Opt_TFCol27:           String = '#A45A70';
  Opt_TFCol28:           String = '#6A5EA3';
  Opt_TFCol29:           String = '#B39487';
  Opt_TFCol30:           String = '#4FDFAF';
  Opt_TFCol31:           String = '#6A625F';

  Opt_CursorColor1:      Integer = 1;
  Opt_CursorColor2:      Integer = 7;
  Opt_CursorBlinking:    Boolean = True;

  Opt_TapeTrapSAVE:      Boolean = True;
  Opt_TapeTrapLOAD:      Boolean = True;


  Opt_MouseImage:         TMouseMode = miCrosshair;

  OSIs95,
  OSIs98,
  OSIsME,
  OSIsNT,
  OSIsXP,
  OSIs9xBased,
  OSIsNTBased:            Boolean;                      // What OS are we running on? These flags are filled at runtime.

  EditorChars:            Array[1..952] of Byte;        // The font used by the editor.

  ProgramIs128k:          Boolean = False;              // Does this program contain any 128k commands?

  Spectranet:             Boolean = False;              // Does this program uses spectranet-like % prefixed commands?

  ProjectSaved:           Boolean = False;              // Has the project been saved?

Const

  WM_PARSEEDIT         =  WM_USER + 1;
  WM_UPDATEPARSETEXT   =  WM_USER + 2;
  WM_UPDATEPROGRAM     =  WM_USER + 3;
  WM_UPDATEVARS        =  WM_USER + 4;
  WM_UPDATEPROGCURSOR  =  WM_USER + 5;
  WM_UPDATEPROGBUTTONS =  WM_USER + 6;
  WM_UPDATECURSOR      =  WM_USER + 7;
  WM_RESIZE            =  WM_USER + 8;

  a128kYes             =  1;
  a128kNo              =  0;
  a128kAsk             =  2;

  // Graphics characters

  Bits: Array[0..7] of DWord = (128, 64, 32, 16, 8, 4, 2, 1);

  RealChars: Array[1..952] of Byte =
  (0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 16, 0, 16, 0, 0, 36, 36, 0, 0, 0, 0, 0,
   0, 36, 126, 36, 36, 126, 36, 0, 0, 8, 62, 40, 62, 10, 62, 8, 0, 98, 100, 8, 16, 38, 70, 0,
   0, 16, 40, 16, 42, 68, 58, 0, 0, 8, 16, 0, 0, 0, 0, 0, 0, 4, 8, 8, 8, 8, 4, 0,
   0, 32, 16, 16, 16, 16, 32, 0, 0, 0, 20, 8, 62, 8, 20, 0, 0, 0, 8, 8, 62, 8, 8, 0,
   0, 0, 0, 0, 0, 8, 8, 16, 0, 0, 0, 0, 62, 0, 0, 0, 0, 0, 0, 0, 0, 24, 24, 0,
   0, 0, 2, 4, 8, 16, 32, 0, 0, 60, 70, 74, 82, 98, 60, 0, 0, 24, 40, 8, 8, 8, 62, 0,
   0, 60, 66, 2, 60, 64, 126, 0, 0, 60, 66, 12, 2, 66, 60, 0, 0, 8, 24, 40, 72, 126, 8, 0,
   0, 126, 64, 124, 2, 66, 60, 0, 0, 60, 64, 124, 66, 66, 60, 0, 0, 126, 2, 4, 8, 16, 16, 0,
   0, 60, 66, 60, 66, 66, 60, 0, 0, 60, 66, 66, 62, 2, 60, 0, 0, 0, 0, 16, 0, 0, 16, 0,
   0, 0, 16, 0, 0, 16, 16, 32, 0, 0, 4, 8, 16, 8, 4, 0, 0, 0, 0, 62, 0, 62, 0, 0,
   0, 0, 16, 8, 4, 8, 16, 0, 0, 60, 66, 4, 8, 0, 8, 0, 0, 60, 74, 86, 94, 64, 60, 0,
   0, 60, 66, 66, 126, 66, 66, 0, 0, 124, 66, 124, 66, 66, 124, 0, 0, 60, 66, 64, 64, 66, 60, 0,
   0, 120, 68, 66, 66, 68, 120, 0, 0, 126, 64, 124, 64, 64, 126, 0, 0, 126, 64, 124, 64, 64, 64, 0,
   0, 60, 66, 64, 78, 66, 60, 0, 0, 66, 66, 126, 66, 66, 66, 0, 0, 62, 8, 8, 8, 8, 62, 0,
   0, 2, 2, 2, 66, 66, 60, 0, 0, 68, 72, 112, 72, 68, 66, 0, 0, 64, 64, 64, 64, 64, 126, 0,
   0, 66, 102, 90, 66, 66, 66, 0, 0, 66, 98, 82, 74, 70, 66, 0, 0, 60, 66, 66, 66, 66, 60, 0,
   0, 124, 66, 66, 124, 64, 64, 0, 0, 60, 66, 66, 82, 74, 60, 0, 0, 124, 66, 66, 124, 68, 66, 0,
   0, 60, 64, 60, 2, 66, 60, 0, 0, 254, 16, 16, 16, 16, 16, 0, 0, 66, 66, 66, 66, 66, 60, 0,
   0, 66, 66, 66, 66, 36, 24, 0, 0, 66, 66, 66, 66, 90, 36, 0, 0, 66, 36, 24, 24, 36, 66, 0,
   0, 130, 68, 40, 16, 16, 16, 0, 0, 126, 4, 8, 16, 32, 126, 0, 0, 14, 8, 8, 8, 8, 14, 0,
   0, 0, 64, 32, 16, 8, 4, 0, 0, 112, 16, 16, 16, 16, 112, 0, 0, 16, 56, 84, 16, 16, 16, 0,
   0, 0, 0, 0, 0, 0, 0, 255, 0, 28, 34, 120, 32, 32, 126, 0, 0, 0, 56, 4, 60, 68, 60, 0,
   0, 32, 32, 60, 34, 34, 60, 0, 0, 0, 28, 32, 32, 32, 28, 0, 0, 4, 4, 60, 68, 68, 60, 0,
   0, 0, 56, 68, 120, 64, 60, 0, 0, 12, 16, 24, 16, 16, 16, 0, 0, 0, 60, 68, 68, 60, 4, 56,
   0, 64, 64, 120, 68, 68, 68, 0, 0, 16, 0, 48, 16, 16, 56, 0, 0, 4, 0, 4, 4, 4, 36, 24,
   0, 32, 40, 48, 48, 40, 36, 0, 0, 16, 16, 16, 16, 16, 12, 0, 0, 0, 104, 84, 84, 84, 84, 0,
   0, 0, 120, 68, 68, 68, 68, 0, 0, 0, 56, 68, 68, 68, 56, 0, 0, 0, 120, 68, 68, 120, 64, 64,
   0, 0, 60, 68, 68, 60, 4, 6, 0, 0, 28, 32, 32, 32, 32, 0, 0, 0, 56, 64, 56, 4, 120, 0,
   0, 16, 56, 16, 16, 16, 12, 0, 0, 0, 68, 68, 68, 68, 56, 0, 0, 0, 68, 68, 40, 40, 16, 0,
   0, 0, 68, 84, 84, 84, 40, 0, 0, 0, 68, 40, 16, 40, 68, 0, 0, 0, 68, 68, 68, 60, 4, 56,
   0, 0, 124, 8, 16, 32, 124, 0, 0, 14, 8, 48, 8, 8, 14, 0, 0, 8, 8, 8, 8, 8, 8, 0,
   0, 112, 16, 12, 16, 16, 112, 0, 0, 20, 40, 0, 0, 0, 0, 0, 60, 66, 153, 161, 161, 153, 66, 60,

   // Chequerboard Characters

   0, 0, 0, 0, 0, 0, 0, 0, 15, 15, 15, 15, 0, 0, 0, 0, 240, 240, 240, 240, 0, 0, 0, 0,
   255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
   240, 240, 240, 240, 15, 15, 15, 15, 255, 255, 255, 255, 15, 15, 15, 15, 0, 0, 0, 0, 240, 240, 240, 240,
   15, 15, 15, 15, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240,
   255, 255, 255, 255, 240, 240, 240, 240, 0, 0, 0, 0, 255, 255, 255, 255, 15, 15, 15, 15, 255, 255, 255, 255,
   240, 240, 240, 240, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,

   // Misc Characters

   24, 20, 242, 129, 242, 20, 24, 0,        // Right Arrow
   24, 40, 79, 129, 79, 40, 24, 0,          // Left Arrow
   8, 20, 34, 65, 119, 20, 20, 28,          // Up Arrow
   28, 20, 20, 119, 65, 34, 20, 8,          // Down Arrow
   0, 96, 120, 126, 126, 120, 96, 0,        // PLAY icon
   0, 60, 126, 126, 126, 126, 60, 0,        // STOP icon
   1, 3, 7, 15, 31, 63, 127, 255);          // Stripe


implementation

Uses BasinMain, Filing, ROMUtils, RLEUnit, Display, VarsWindow, CPUDisplay, TokenWindow, PaintBox;


Procedure TWorkerThread.Execute;
Var
  MSG: TMsg;
Begin
  NeedSuspend := False;
  DisplaySuspended := True;
  Priority := TpIdle;
  FreeOnTerminate := False;
  While PeekMessage(MSG, 0, 0, 0, PM_REMOVE) Do
     {loop};

  While Not Terminated Do Begin
     If Opt_AutoFrameSkip Then Begin
        If DisplayWindow.WantResize Then
           DisplayWindow.SizeDIBs;
        If NeedDisplayUpdate and Not DisplaySuspended Then Begin
           Dec(FrameCounter);
           If FrameCounter <= 0 Then Begin
              If DisplayWindow.Showing Then Begin
                If TryEnterSection Then Begin
                    UpdateDisplay;
                    UpdateBASinDisplay;
                    NeedDisplayUpdate := False;
                    ResizeSectionBool := False;
                 End;
              End Else
                 NeedDisplayUpdate := False;
              FrameCounter := opt_FrameSkip;
           End Else
              NeedDisplayUpdate := False;
        End Else
           Sleep(1);
     End Else
        Sleep(1);
  End;

  While PeekMessage(MSG, 0, 0, 0, PM_REMOVE) Do
     {loop};
End;

Procedure TWorkerThread.HandleMessages;
Var
  MSG: TMsg;
Begin
  While PeekMessage(MSG, 0, 0, 0, PM_REMOVE) Do Begin
     Case MSG.Message of
        WM_PARSEEDIT:
           Begin
              BASinOutput.UpdateParseText;
           End;
     End;
  End;
End;

Procedure INITWorkerThread;
Begin
  If WorkerThread = nil Then Begin
     WorkerThread := TWorkerThread.Create(False);
     WorkerThread.FreeOnTerminate := False;
  End;
End;

Procedure CloseWorkerThread;
Begin
  If WorkerThread <> Nil Then Begin
     WorkerThread.Terminate;
     WorkerThread.WaitFor;
     WorkerThread.Free;
     WorkerThread := nil;
  End;
End;

Procedure SuspendWorkerThread;
Begin
  If WorkerThread <> Nil Then Begin
     WorkerThread.DisplaySuspended := True;
     UpdateDisplay;
     UpdateBASinDisplay;
     NeedDisplayUpdate := False;
  End;
End;

Procedure ResumeWorkerThread;
Begin
  WorkerThread.DisplaySuspended := False;
End;

Function LoadEditorFont(Handle: HWnd; Filename: String; Enabled: Boolean): Boolean;
Var
  FStream: TFileStream;
  Idx: Integer;
Begin

  Result := False;

  For Idx := 1 To 952 Do
     EditorChars[Idx] := RealChars[Idx];

  If Enabled Then

     If FileExists(Filename) Then

        If OpenFileStream(FStream, fmOpenRead or fmShareDenyNone, Filename) Then Begin

           If Lowercase(ExtractFileExt(Filename)) = '.bsc' Then Begin

              If FStream.Size = 785 Then Begin

                 FStream.Seek(17, soFromBeginning);
                 FStream.Read(EditorChars[1], 768);
                 FStream.Free;
                 Result := True;

              End;

           End Else Begin

              If FStream.Size = 768 Then Begin

                 FStream.Read(EditorChars[1], 768);
                 FStream.Free;
                 Result := True;

              End;

           End;

           If Not Result Then Begin

              MessageBox(Handle, pChar('Your file '+#39+ExtractFilename(Filename)+#39#13+'Does not appear to be a compatible'#13'font file.'), pChar('Invalid Font File'), mb_IconWarning or mb_OK);
              FStream.Free;

           End;

        End;

  If TokenForm.Visible Then
     TokenForm.DrawTokens;

End;

Function RolWord(Value: Word; Count: Byte): Word;
Asm
  push cx
  mov  cl, dl
  rol  ax, cl
  pop  cx
End;

Function FastIntToHex(Value: Word): String;
Begin
  Value := RolWord(Value, 4);
  Result := HexChars[Value and 15];
  Value := RolWord(Value, 4);
  Result := Result + HexChars[Value and 15];
  Value := RolWord(Value, 4);
  Result := Result + HexChars[Value and 15];
  Value := RolWord(Value, 4);
  Result := Result + HexChars[Value and 15];
End;



function HexToColor( HexValue: string): TFColor;    //arda - used for  converting hex color string to TFColor
var
  HexStr: string;
  R, G, B: Byte;
begin
  // Remove '#' if present
  HexStr := HexValue;
  if HexStr[1] = '#' then
    Delete(HexStr, 1, 1);

  // Convert hex string to RGB values
  R := StrToIntDef('$' + Copy(HexStr, 1, 2), 0);
  G := StrToIntDef('$' + Copy(HexStr, 3, 2), 0);
  B := StrToIntDef('$' + Copy(HexStr, 5, 2), 0);

  // Assign values to TFColor record
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function ColorToHex(Color: TFColor): string;
begin
  // Format the RGB values as a hex string
  Result := Format('#%.2x%.2x%.2x', [Color.R, Color.G, Color.B]);
end;


Procedure AlignAnchors(Form: TForm);
Var
  F: Integer;
  Anchors: TAnchors;
Begin
  For F := 0 To Form.ControlCount -1 Do Begin
     If Form.Controls[F] is TControl Then Begin
        Anchors := (Form.Controls[F] As TControl).Anchors;
        (Form.Controls[F] As TControl).Anchors := Anchors;
     End;
  End;
End;

Procedure MoveForm(Form: TForm; X, Y: Integer);
Begin
  If Form.Width+X > Screen.Width Then X := Screen.Width - Form.Width;
  If Form.Height+Y > Screen.Height Then Y := Screen.Height - Form.Height;
  If X < 0 Then X := 0;
  If Y < 0 Then Y := 0;
  Form.SetBounds(X, Y, Form.Width, Form.Height);
End;

Procedure CentreForm(Form: TForm; X, Y: Integer);
Begin
  MoveForm(Form, X-(Form.Width Div 2), Y-(Form.Height Div 2));
End;

Procedure CentreFormOnForm(FormToMove: TForm; FormToMoveTo: TForm);
Begin
  If FormToMoveTo <> nil Then
     CentreForm(FormToMove, FormToMoveTo.Left + (FormToMoveTo.Width Div 2), FormToMoveTo.Top + (FormToMoveTo.Height Div 2))
  Else
     CentreForm(FormToMove, Screen.Width Div 2, Screen.Height Div 2);
End;

Procedure ShowWindow(FormToShow: TForm; Modal: Boolean);
Var
  RunningEmu: Boolean;
Begin
  MoveForm(FormToShow, FormToShow.Left, FormToShow.Top);
  DisplayWindow.WantsFront := False;
  If Not Modal Then Begin
   try
    FormToShow.Show;
    Application.ProcessMessages;
   except
    on E: Exception do
    ShowMessage('Catch: ' + E.Message);
   end;
  End Else Begin
     RunningEmu := Registers.EmuRunning;
     ControlEmulation(False);
     If FormToShow.Enabled = False Then
        FormToShow.Enabled := True;
     If FormToShow.Visible Then
        FormToShow.Hide;
     FormToShow.ShowModal;
     If RunningEmu or Registers.EmuRunning Then
        ControlEmulation(True);
  End;
End;

Function ShrinkFilename(Value: String; Width: Integer): String;
var
  BeginP, EndP: Integer;
  St: String;
label
  LAB, LAB2;
begin
  Result := Value;
  if BASinOutput.Canvas.TextWidth(Value) > Width then begin
     St := Value + '\...';
     while BASinOutput.Canvas.TextWidth(St) > Width do begin
        BeginP := Pos('\', St);
        if BeginP = 0 then Goto LAB2;
        Delete(St, BeginP, 1);
        EndP := Pos('\', St);
        if EndP = Length(St) - 3 then begin
           Insert('\', St, 3);
           Goto LAB;
        end;
        Delete(St, BeginP, EndP - BeginP);
     end;
    SetLength(St, Length(St) - 4);
  LAB:
    Insert('\...', St, 3);
    Result := St;
  LAB2:
    If Copy(Result, Length(Result)-3, 4) = '\...' Then Result := Copy(Result, 1, Length(Result)-4);
  end;
end;

Procedure StoreEmulationState(Filename: String; Var State: TEmulationState);
Var
  StateArray: Array of Byte;
  Dir, CompactedFile: String;
  List: TStringlist;
  FLen: DWord;
Begin
  Dir := GetCurrentDir;
  ZeroMemory(@State.Memory[0], 16384);
  ZeroMemory(@UndoState.Memory[0], 16384);
  ZeroMemory(@BreakState.Memory[0], 16384);
  SetLength(StateArray, Length(Dir) + SizeOf(TEmulationState)*3);
  CopyMemory(@StateArray[0], @State.Memory[0], SizeOf(TEmulationState));
  CopyMemory(@StateArray[SizeOf(TEmulationState)], @UndoState.Memory[0], SizeOf(TEmulationState));
  CopyMemory(@StateArray[SizeOf(TEmulationState)*2], @BreakState.Memory[0], SizeOf(TEmulationState));
  CopyMemory(@StateArray[SizeOf(TEmulationState)*3], @Dir[1], Length(Dir));
  CompactedFile := RLEPackArray(StateArray);
  FLen := Length(CompactedFile);
  CompactedFile := #1+'Session   ' + Chr(FLen and 255)+Chr((FLen Shr 8) And 255)+Chr((FLen Shr 16) And 255)+Chr((FLen Shr 24) And 255) + CompactedFile;
  List := TStringlist.Create;
  List.Add(CompactedFile);
  List.SaveToFile(Filename);
  List.Free;
End;

Procedure RestoreEmulationState(Filename: String);
Var
  Dir: String;
  State: TEmulationState;
Begin
  If FileExists(Filename) Then Begin
     RLEUnpackFile(Filename, 'Session');
     CopyMemory(@State.Memory[0], @RLEArray[0], SizeOf(TEmulationState));
     CopyMemory(@UndoState.Memory[0], @RLEArray[SizeOf(TEmulationState)], SizeOf(TEmulationState));
     CopyMemory(@BreakState.Memory[0], @RLEArray[SizeOf(TEmulationState)*2], SizeOf(TEmulationState));
     CopyMemory(@State.Memory[0], @Memory[0], 16384);
     CopyMemory(@BreakState.Memory[0], @Memory[0], 16384);
     CopyMemory(@UndoState.Memory[0], @Memory[0], 16384);
     If Length(RLEArray) > SizeOf(TEmulationState)*3 Then Begin
        SetLength(DIR, Length(RLEArray) - (SizeOf(TEmulationState)*3));
        CopyMemory(@Dir[1], @RLEArray[SizeOf(TEmulationState)*3], Length(Dir));
        SetCurrentDir(Dir);
     End;
     LoadEmulationState(State, False);
     CurProjectName := SessionProjectName;
     CurProjectFilename := SessionProjectFileName;
     BASinOutput.SetCaption;
     BASinOutput.GetBASIC;
     BASinOutput.TokeniseEditText(False);
     BASinOutput.UpdateCursorPos(1, False);
     BASinOutput.BASICChanged := False;
     BASinOutput.RepaintBASIC(True);
     BASinOutput.FormResize(nil);
     VariablesWindow.BuildVarsList;
     If State.Running Then
        BASinOutput.MenuItemClick(BASinOutput.ForceBREAK1);
  End Else Begin
     MessageBox(BASinOutput.Handle, pChar('The previous BASin Session'#13'could not be restored.'), pChar('Auto Session Restore Error'), MB_OK or MB_ICONWARNING);
  End;
End;

Function FloatToStrEx(Number: Extended): String;
Var
  Idx: Integer;
Begin

  Result := FloatToStr(Number);

  If Result <> '' Then
     For Idx := 1 To Length(Result) Do
        If Not (Result[Idx] in ['0'..'9', '-']) Then
           Result[Idx] := '.';

End;

Procedure SetProjectName(Name: String);

Begin
  CurProjectFullPathFile:=ExtractFilename(Name);
  CurProjectName := TrimExtension(CurProjectFullPathFile);
  CurProjectFilename := Name;
  ProjectSaved := True;
  BASinOutput.Label1.Caption := 'Project ' + CurProjectName ;
  BASinOutput.SetCaption;


End;

{
function ArrayToTBytes(Value: array of byte; Ln: integer, Start: integer=0):  TByteArray;
var
i: Integer;
Begin
   if Ln=0 Then Ln:=Length(Value);

   SetLength(Result, Ln);
  for i := 0 to Ln-1 do
    Result[i] := Value[i];
End;
 }

function ArrayToTBytes(Value: array of byte; Ln: integer; Start: integer = 0): TByteArray;
begin

   if Ln = 0 then 
      Ln := Length(Value) - Start;


   if (Start + Ln) > Length(Value) then
      Ln := Length(Value) - Start;


   if Ln < 0 then Ln := 0;


   SetLength(Result, Ln);


   if Ln > 0 then
      Move(Value[Start], Result[0], Ln);
end;

Function TrimExtension(Name: String): String;
Var
  N: Integer;
Begin
  If Name <> '' Then Begin
     N := Length(Name);
     While (Name[N] <> '.') and (N > 1) Do Dec(N);
     If N = 1 Then N := Length(Name);
     Result := Copy(Name, 1, N);
     If Result[Length(Result)] = '.' Then
        Result := Copy(Result, 1, Length(Result) -1);
  End;
End;

{Options}

Procedure LoadOptions;
Var
  Vs, Idx, Lt, Tp, Wd, Ht: Integer;
  Value, NewEntry: String;
Begin



  INI := TStringlist.Create;
  If FileExists(BASinDIR+'\basinC.ini') Then
     INI.LoadFromFile(BASinDIR+'\basinC.ini');

  // Resume

  Value :=                   INIRead('LastSession', 'LastProjectName', '');
  Opt_AutoLoadSession :=     INIRead('LastSession', 'Opt_AutoLoadSession', Opt_AutoLoadSession);
  BASICCheckSum :=           INIRead('LastSession', 'Checksum', Integer(0));
  SessionProjectName :=      INIRead('LastSession', 'SessionProjectName', SessionProjectName);
  SessionProjectFilename :=  INIRead('LastSession', 'SessionProjectFileName', SessionProjectFileName);
  SessionID :=               INIRead('LastSession', 'SessionID', SessionID);

  If Opt_AutoLoadSession Then
     If Value <> '' Then
        SetProjectName(Value);

  // Programming Aids

  Opt_ShowingSyntax :=       INIRead('Programming', 'Opt_ShowingSyntax', Opt_ShowingSyntax);
  Opt_AutoList :=            INIRead('Programming', 'Opt_AutoList', Opt_AutoList);
  Opt_AutoBracket :=         TBracketMethod(INIRead('Programming', 'Opt_AutoBracket', Ord(Opt_AutoBracket)));
  Opt_GraphicsMethod :=      TGraphicsMethod(INIRead('Programming', 'Opt_GraphicsMethod', Ord(Opt_GraphicsMethod)));
  Opt_SeperateDisplay :=     INIRead('Programming', 'Opt_SeperateDisplay', Opt_SeperateDisplay);
  Opt_CharacterRuler :=      INIRead('Programming', 'Opt_CharacterRuler', Opt_CharacterRuler);
  Opt_Controlicons :=        INIRead('Programming', 'Opt_Controlicons', Opt_Controlicons);
  Opt_ViewInfoLine :=        INIRead('Programming', 'Opt_ViewInfoLine', Opt_ViewInfoLine);
  Opt_SubRoutines:=          INIRead('Programming', 'Opt_SubRoutines', Opt_SubRoutines);           //1.8
  Opt_AutoCollectSubs:=      INIRead('Programming', 'Opt_AutoCollectSubs', Opt_AutoCollectSubs);   //1.8
  Opt_AutoCollectJumps:=     INIRead('Programming', 'Opt_AutoCollectJumps', Opt_AutoCollectJumps);   //1.83
  Opt_ShowJumpOrigins:=      INIRead('Programming', 'Opt_ShowJumpOrigins', Opt_ShowJumpOrigins);   //1.83
  Opt_ShowAscii :=           INIRead('Programming', 'Opt_ShowASCII', Opt_ShowAscii);
  Opt_Predictive :=          INIRead('Programming', 'Opt_Predictive', Opt_Predictive);
  Opt_OverwriteProtect :=    INIRead('Programming', 'Opt_OverwriteProtect', Opt_OverwriteProtect);
  Opt_ProtectNewOnly :=      INIRead('Programming', 'Opt_ProtectNewOnly', Opt_ProtectNewOnly);
  Opt_Language :=            INIRead('Programming', 'Opt_Language', Opt_Language);
  Opt_ShowRemCommands :=     INIRead('Programming', 'Opt_ShowRemCommands', Opt_ShowRemCommands);
  Opt_EnableBasincTips :=    INIRead('Programming', 'Opt_EnableBasincTips', Opt_EnableBasincTips);

  // AI Options

  Opt_SelectedAIModel :=     INIRead('AI', 'Opt_SelectedAIModel', Opt_SelectedAIModel);
  Opt_BYOK_APIKEY :=         INIRead('AI', 'Opt_BYOK_APIKEY', Opt_BYOK_APIKEY);

  // Syntax Highlight Options

  Opt_SyntaxHighlight :=     INIRead('Highlight', 'Opt_SyntaxHightlight', Opt_SyntaxHighlight);

  Opt_HighlightKeywords :=   INIRead('Highlight', 'Opt_HighlightKeywords', Opt_HighlightKeywords);
  Opt_KeywordsColour :=      INIRead('Highlight', 'Opt_KeywordsColour', Opt_KeywordsColour);
  Opt_KeywordsBold :=        INIRead('Highlight', 'Opt_KeywordsBold', Opt_KeywordsBold);
  Opt_KeywordsItalic :=      INIRead('Highlight', 'Opt_KeywordsItalic', Opt_KeywordsItalic);

  Opt_HighlightFunctions :=  INIRead('Highlight', 'Opt_HighlightFunctions', Opt_HighlightFunctions);
  Opt_FunctionsColour :=     INIRead('Highlight', 'Opt_FunctionsColour', Opt_FunctionsColour);
  Opt_FunctionsBold :=       INIRead('Highlight', 'Opt_FunctionsBold', Opt_FunctionsBold);
  Opt_FunctionsItalic :=     INIRead('Highlight', 'Opt_FunctionsItalic', Opt_FunctionsItalic);

  Opt_HighlightComments :=   INIRead('Highlight', 'Opt_HighlightComments', Opt_HighlightComments);
  Opt_CommentsColour :=      INIRead('Highlight', 'Opt_CommentsColour', Opt_CommentsColour);
  Opt_CommentsBold :=        INIRead('Highlight', 'Opt_CommentsBold', Opt_CommentsBold);
  Opt_CommentsItalic :=      INIRead('Highlight', 'Opt_CommentsItalic', Opt_CommentsItalic);

  Opt_HighlightSymbols :=    INIRead('Highlight', 'Opt_HighlightSymbols', Opt_HighlightSymbols);
  Opt_SymbolsColour :=       INIRead('Highlight', 'Opt_SymbolsColour', Opt_SymbolsColour);
  Opt_SymbolsBold :=         INIRead('Highlight', 'Opt_SymbolsBold', Opt_SymbolsBold);
  Opt_SymbolsItalic :=       INIRead('Highlight', 'Opt_SymbolsItalic', Opt_SymbolsItalic);

  Opt_HighlightVars :=       INIRead('Highlight', 'Opt_HighlightVars', Opt_HighlightVars);
  Opt_VarsColour :=          INIRead('Highlight', 'Opt_VarsColour', Opt_VarsColour);
  Opt_VarsBold :=            INIRead('Highlight', 'Opt_VarsBold', Opt_VarsBold);
  Opt_VarsItalic :=          INIRead('Highlight', 'Opt_VarsItalic', Opt_VarsItalic);

  Opt_HighlightVarsUnDef :=  INIRead('Highlight', 'Opt_HighlightVarsUnDef', Opt_HighlightVarsUnDef);
  Opt_VarsUnDefColour :=     INIRead('Highlight', 'Opt_VarsUnDefColour', Opt_VarsUnDefColour);
  Opt_VarsUnDefBold :=       INIRead('Highlight', 'Opt_VarsUnDefBold', Opt_VarsUnDefBold);
  Opt_VarsUnDefItalic :=     INIRead('Highlight', 'Opt_VarsUnDefItalic', Opt_VarsUnDefItalic);

  Opt_HighlightNumbers :=    INIRead('Highlight', 'Opt_HighlightNumbers', Opt_HighlightNumbers);
  Opt_NumbersColour :=       INIRead('Highlight', 'Opt_NumbersColour', Opt_NumbersColour);
  Opt_NumbersBold :=         INIRead('Highlight', 'Opt_NumbersBold', Opt_NumbersBold);
  Opt_NumbersItalic :=       INIRead('Highlight', 'Opt_NumbersItalic', Opt_NumbersItalic);

  Opt_HighlightLineNums :=   INIRead('Highlight', 'Opt_HighlightLineNums', Opt_HighlightLineNums);
  Opt_LineNumsColour :=      INIRead('Highlight', 'Opt_LineNumsColour', Opt_LineNumsColour);
  Opt_LineNumsBold :=        INIRead('Highlight', 'Opt_LineNumsBold', Opt_LineNumsBold);
  Opt_LineNumsItalic :=      INIRead('Highlight', 'Opt_LineNumsItalic', Opt_LineNumsItalic);

  Opt_HighlightStrings :=    INIRead('Highlight', 'Opt_Highlight', Opt_HighlightStrings);
  Opt_StringsColour :=       INIRead('Highlight', 'Opt_StringsColour', Opt_StringsColour);
  Opt_StringsBold :=         INIRead('Highlight', 'Opt_StringsBold', Opt_StringsBold);
  Opt_StringsItalic :=       INIRead('Highlight', 'Opt_StringsItalic', Opt_StringsItalic);

  Opt_Foreground :=          INIRead('Highlight', 'Opt_Foreground', Opt_Foreground);
  Opt_Background :=          INIRead('Highlight', 'Opt_Background', Opt_Background);

  Opt_EditorCustomFont :=    INIRead('Fonts', 'Opt_EditorCustomFont', Opt_EditorCustomFont);
  Opt_EditorFontFilename :=  INIRead('Fonts', 'Opt_EditorFontFilename', Opt_EditorFontFilename);
  Opt_EditorFontFolder :=    INIRead('Fonts', 'Opt_EditorFontFolder', Opt_EditorFontFolder);

  Opt_ToolFontSize :=        INIRead('Fonts', 'Opt_ToolFontSize', Opt_ToolFontSize);   //Arda


  Opt_Indenting :=   INIRead('Highlight', 'Opt_Indenting', Opt_Indenting);   //Arda
  Opt_IndentSize :=   INIRead('Highlight', 'Opt_IndentSize', Opt_IndentSize);   //Arda

  Opt_TFCol24:=   INIRead('CustomColours', 'Opt_TFCol24', Opt_TFCol24);   //Arda
  Opt_TFCol25:=   INIRead('CustomColours', 'Opt_TFCol25', Opt_TFCol25);
  Opt_TFCol26:=   INIRead('CustomColours', 'Opt_TFCol26', Opt_TFCol26);
  Opt_TFCol27:=   INIRead('CustomColours', 'Opt_TFCol27', Opt_TFCol27);
  Opt_TFCol28:=   INIRead('CustomColours', 'Opt_TFCol28', Opt_TFCol28);
  Opt_TFCol29:=   INIRead('CustomColours', 'Opt_TFCol29', Opt_TFCol29);
  Opt_TFCol30:=   INIRead('CustomColours', 'Opt_TFCol30', Opt_TFCol30);
  Opt_TFCol31:=   INIRead('CustomColours', 'Opt_TFCol31', Opt_TFCol31);


  TFCol24:=HexToColor(Opt_TFCol24);
  TFCol25:=HexToColor(Opt_TFCol25);
  TFCol26:=HexToColor(Opt_TFCol26);
  TFCol27:=HexToColor(Opt_TFCol27);
  TFCol28:=HexToColor(Opt_TFCol28);
  TFCol29:=HexToColor(Opt_TFCol29);
  TFCol30:=HexToColor(Opt_TFCol30);
  TFCol31:=HexToColor(Opt_TFCol31);

  Opt_CursorColor1:=   INIRead('CursorColor1', 'Opt_CursorColor1', Opt_CursorColor1);
  Opt_CursorColor2:=   INIRead('CursorColor2', 'Opt_CursorColor2', Opt_CursorColor2);

  Opt_CursorBlinking:=    INIRead('CustomColours', 'Opt_CursorBlinking', Opt_CursorBlinking);
  BasinOutput.Timer1.Enabled:=Opt_CursorBlinking;

  BuildPalette([TFSpecBlack, TFSpecBlue,  TFSpecRed,  TFSpecMagenta,  TFSpecGreen,  TFSpecCyan,  TFSpecYellow,  TFSpecWhite,
                TFSpecBlack, TFSpecBlueB, TFSpecRedB, TFSpecMagentaB, TFSpecGreenB, TFSpecCyanB, TFSpecYellowB, TFSpecWhiteB,
                TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,
                TFCol24,TFCol25,TFCol26,TFCol27,TFCol28,TFCol29,TFCol30,TFCol31]);

  BASinoutput.SetDark;

  // Error Notification

  For Idx := 0 To 43 Do
     ErrorAddresses[Idx].Notify := INIRead('ErrorNotify', 'Error'+IntToStr(Idx), ErrorAddresses[Idx].Notify);
  ErrorAddresses[33].Notify := INIRead('ErrorNotify', 'Unknown', ErrorAddresses[33].Notify);

  Opt_CursorToError :=       INIRead('ErrorNotify', 'Opt_CursorToError', Opt_CursorToError);

  // Assembler options
  if FileExists(ExtractFilePath(Application.ExeName) + 'cheq_edit.exe') then Opt_CheqEditAvailable:= True;
  if FileExists(ExtractFilePath(Application.ExeName) + 'pasmo.exe') then Opt_AsmPasmoAvailable:= True;
  if FileExists(ExtractFilePath(Application.ExeName) + 'zx0.dll') then Opt_ZX0Available:= True;

  Opt_AsmStatusBar :=        INIRead('Assembler', 'Opt_AsmStatusBar', Opt_AsmStatusBar);
  Opt_AsmLabelList :=        INIRead('Assembler', 'Opt_AsmLabelList', Opt_AsmLabelList);

  // Debugger Options

  Opt_AsmHexValues :=        INIRead('Debugger', 'Opt_AsmHexValues', Opt_AsmHexValues);
  Opt_AsmAsciiBytes :=       INIRead('Debugger', 'Opt_AsmAsciiBytes', Opt_AsmAsciiBytes);

  // Printing Options

  Opt_FastPrinting :=        INIRead('Printing', 'Opt_FastPrinting', Opt_FastPrinting);
  Opt_SavePrinting :=        INIRead('Printing', 'Opt_SavePrinting', Opt_SavePrinting);

    // Enhanced Hardware   R15

  Opt_64Colours :=           INIRead('EnhancedHardware', 'Opt_64Colours', Opt_64Colours);
  Opt_ConsoleAddon :=        INIRead('EnhancedHardware', 'Opt_ConsoleAddon', Opt_ConsoleAddon);
  Opt_KMouse :=              INIRead('EnhancedHardware', 'Opt_KMouse', Opt_KMouse);
  Opt_AllowMultipleInstances := INIRead('Programming', 'Opt_AllowMultipleInstances', Opt_AllowMultipleInstances);

   // Basinet
  Opt_CheckUpdates:=         INIRead('Basinet', 'Opt_CheckUpdates', Opt_CheckUpdates);

  // Display Options

  Opt_DisplayOnTop:=   INIRead('DisplayOnTop', 'Opt_DisplayOnTop', Opt_DisplayOnTop);
  Opt_DisplaySnap :=   INIRead('DisplaySnap', 'Opt_DisplaySnap', Opt_DisplaySnap);
  
  // Scaling Options

  Opt_FontScale :=           INIRead('Scaling', 'Opt_FontScale', Opt_FontScale);
  Opt_MaintainAspect :=      INIRead('Scaling', 'Opt_MaintainAspect', Opt_MaintainAspect);
  Opt_IntegerScaling :=      INIRead('Scaling', 'Opt_IntegerScaling', Opt_IntegerScaling);
  Opt_RenderMethod :=        TRenderMethod(INIRead('Scaling', 'Opt_RenderMethod', Ord(Opt_RenderMethod)));
  Opt_8BitStretch :=         INIRead('Scaling', 'Opt_8BitStretch', Opt_8BitStretch);

  // FrameSkip

  Opt_FrameSkip :=           INIRead('FrameSkip', 'Opt_FrameSkip', Opt_FrameSkip);
  Opt_AutoFrameSkip :=       INIRead('FrameSkip', 'Opt_AutoFrameSkip', Opt_AutoFrameSkip);

  // Sound Options

  Opt_SoundFrequency :=      INIRead('Sound', 'Opt_SoundFrequency', Opt_SoundFrequency);
  Opt_SoundVolume :=         INIRead('Sound', 'Opt_SoundVolume', Opt_SoundVolume);
  Opt_SoundBits :=           INIRead('Sound', 'Opt_SoundBits', Opt_SoundBits);
  Opt_SoundStereo :=         INIRead('Sound', 'Opt_SoundStereo', Opt_SoundStereo);
  Opt_NumSoundBuffers :=     INIRead('Sound', 'Opt_NumSoundBuffers', Opt_NumSoundBuffers);
  Opt_SoundLatency :=        INIRead('Sound', 'Opt_SoundLatency', Opt_SoundLatency);
  Opt_SoundEnabled :=        INIRead('Sound', 'Opt_SoundEnabled', Opt_SoundEnabled);
  Opt_EditorSounds :=        INIRead('Sound', 'Opt_EditorSounds', Opt_EditorSounds);
  Opt_KeyClick48k :=         INIRead('Sound', 'Opt_KeyClick48k', Opt_KeyClick48k);
  Opt_DSoundSynch :=         INIRead('Sound', 'Opt_DSoundSynch', Opt_DSoundSynch);

  // Embellishments

  Opt_Scanlines :=           INIRead('Embellishments', 'Opt_Scanlines', Opt_Scanlines);
  Opt_ClipCorners :=         INIRead('Embellishments', 'Opt_ClipCorners', Opt_ClipCorners);
  Opt_FastResets :=         INIRead('Embellishments', 'Opt_FastResets', Opt_FastResets);

  // Snapshots

  Opt_z80Version:=           INIRead('Snapshots', 'Opt_z80Version', Opt_z80Version);
  Opt_ExternalExec:=         INIRead('Snapshots', 'Opt_ExternalExec', Opt_ExternalExec);
  Opt_Always128k:=           INIRead('Snapshots', 'Opt_Always128k', Opt_Always128k);

  // .BAS Files

  Opt_LoadAutoStart :=       INIRead('BASFiles', 'Opt_LoadAutoStart', Opt_LoadAutoStart);
  Opt_Autostart :=           INIRead('BASFiles', 'Opt_Autostart', Opt_Autostart);
  Opt_SavePretty :=          INIRead('BASFiles', 'Opt_SavePretty', Opt_SavePretty);
  Opt_AutoBackup :=          INIRead('BASFiles', 'Opt_AutoBackup', Opt_AutoBackup);
  Opt_AutoBackInterval:=     INIRead('BASFiles', 'Opt_AutoBackInterval', Opt_AutoBackInterval);
  Opt_ShowNotes :=           INIRead('BASFiles', 'Opt_ShowNotes', Opt_ShowNotes);

  // Tape Images

  Opt_TapeRewind :=          INIRead('TAPFiles', 'Opt_TapeRewind', Opt_TapeRewind);
  Opt_TapeTrapSAVE:=         INIRead('TAPFiles', 'Opt_TapeTrapSAVE', Opt_TapeTrapSAVE);    //1.83
  Opt_TapeTrapLOAD:=         INIRead('TAPFiles', 'Opt_TapeTrapLOAD', Opt_TapeTrapLOAD);    //1.83

  //Paintbox

  Opt_MouseImage :=          TMouseMode(INIRead('Paintbox', 'Opt_MouseImage', Ord(Opt_MouseImage)));

  // MRU List

  Idx := INIRead('MRUList', 'NumEntries', -1) +1;
  While Idx > 0 Do Begin
     NewEntry := INIRead('MRUList', 'File'+IntToStr(Idx-1), '');
     If NewEntry <> '' Then MRUList.Insert(0, NewEntry);
     Dec(Idx);
  End;

  // MRU - Screen paintbox

  ScrPaintForm.RecentPics.Clear;
  Idx := INIRead('SCR_MRUList', 'NumEntries', -1) +1;
  While Idx > 0 Do Begin
     NewEntry := INIRead('SCR_MRUList', 'File'+IntToStr(Idx-1), '');
     If NewEntry <> '' Then ScrPaintForm.RecentPics.Insert(0, NewEntry);
     Dec(Idx);
  End;

  // Window Placements

  For Idx := 1 to Application.ComponentCount -1 Do Begin
     Value := INIRead('WindowPlacements', Application.Components[Idx].Name, '');
     If Value <> '' Then Begin
        If Application.Components[Idx] is TForm Then Begin
           Vs := StrToIntDef(Copy(Value, 1, Pos(',', Value)-1), (Application.Components[Idx] as TForm).Left);
           Value := Copy(Value, Pos(',', Value)+1, 999999);
           Lt := StrToIntDef(Copy(Value, 1, Pos(',', Value)-1), (Application.Components[Idx] as TForm).Left);
           Value := Copy(Value, Pos(',', Value)+1, 999999);
           Tp := StrToIntDef(Copy(Value, 1, Pos(',', Value)-1), (Application.Components[Idx] as TForm).Top);
           Value := Copy(Value, Pos(',', Value)+1, 999999);
           Wd := StrToIntDef(Copy(Value, 1, Pos(',', Value)-1), (Application.Components[Idx] as TForm).Width);
           Value := Copy(Value, Pos(',', Value)+1, 999999);
           Ht := StrToIntDef(Value, (Application.Components[Idx] as TForm).Height);
           If (Application.Components[Idx] As TForm).BorderStyle = BsSingle Then Begin
              Wd := (Application.Components[Idx] as TForm).Width;
              Ht := (Application.Components[Idx] as TForm).Height;
           End;
           (Application.Components[Idx] as TForm).SetBounds(Lt, Tp, Wd, Ht);
           If Boolean(Vs) Then
              (Application.Components[Idx] As TForm).Tag := (Application.Components[Idx] As TForm).Tag + 2;
        End;
     End;
  End;

  LoadEditorFont(BASinOutput.Handle, Opt_EditorFontFilename, Opt_EditorCustomFont);
  Basinoutput.Timer3.Interval:=(1+(Opt_AutoBackInterval*3))*60000;
  Basinoutput.Timer3.Enabled:=Opt_AutoBackup;
  

  iniReleaseName :=  INIRead('Version', 'BasinCVersion', ReleaseName);
        if iniReleaseName <> ReleaseName then
        begin
                //ShowMessage('Welcome to New Version Of Basinc');
                //eger yeni version zorlanacak ayarlari varsa burada yazilabilir
        end;


  INI.Free;
  ReleaseName := 'BasinC v'+ GetBuildInfoAsString(False);
  ReleaseBuild:= 'BasinC v'+ GetBuildInfoAsString(True);
  
End;

Procedure SaveOptions;
Var
  Idx, Vs, Lt, Tp, Wd, Ht: Integer;
Begin

  INI := TStringlist.Create;

  // Version

  INIWrite('Version', 'BasinCVersion', ReleaseName);

  // Resume

  INIWrite('LastSession', 'LastProjectName', CurProjectName);
  INIWrite('LastSession', 'Opt_AutoLoadSession', Opt_AutoLoadSession);
  INIWrite('LastSession', 'Checksum', BASICCheckSum);
  INIWrite('LastSession', 'SessionProjectName', CurProjectName);
  INIWrite('LastSession', 'SessionProjectFileName', CurProjectFileName);
  INIWrite('LastSession', 'LastFileOpened', CurProjectFullPathFile);
  If (Length(SessionID)<7) Then Begin
        SessionID := inttostr(Round((Now() - 25569.0 ) * 86400));
        SessionID := inttostr(100 + Random(899)) + SessionID;
        INIWrite('LastSession', 'SessionID', SessionID);
  End Else Begin
        INIWrite('LastSession', 'SessionID', SessionID);
  End;

  // Programming Aids

  INIWrite('Programming', 'Opt_ShowingSyntax', Opt_ShowingSyntax);
  INIWrite('Programming', 'Opt_AutoList', Opt_AutoList);
  INIWrite('Programming', 'Opt_GraphicsMethod', Ord(Opt_GraphicsMethod));
  INIWrite('Programming', 'Opt_AutoBracket', Ord(Opt_AutoBracket));
  INIWrite('Programming', 'Opt_SeperateDisplay', Opt_SeperateDisplay);
  INIWrite('Programming', 'Opt_CharacterRuler', Opt_CharacterRuler);
  INIWrite('Programming', 'Opt_Controlicons', Opt_Controlicons);
  INIWrite('Programming', 'Opt_ViewInfoLine', Opt_ViewInfoLine);
  INIWrite('Programming', 'Opt_SubRoutines', Opt_SubRoutines);         //1.8
  INIWrite('Programming', 'Opt_AutoCollectSubs', Opt_AutoCollectSubs); //1.8
  INIWrite('Programming', 'Opt_AutoCollectJumps', Opt_AutoCollectJumps); //1.83
  INIWrite('Programming', 'Opt_ShowJumpOrigins', Opt_ShowJumpOrigins); //1.83
  INIWrite('Programming', 'Opt_ShowASCII', Opt_ShowAscii);
  INIWrite('Programming', 'Opt_Predictive', Opt_Predictive);
  INIWrite('Programming', 'Opt_OverwriteProtect', Opt_OverwriteProtect);
  INIWrite('Programming', 'Opt_ProtectNewOnly', Opt_ProtectNewOnly);
  INIWrite('Programming', 'Opt_Language', Opt_Language);
  INIWrite('Programming', 'Opt_AllowMultipleInstances', Opt_AllowMultipleInstances);
  INIWrite('Programming', 'Opt_ShowRemCommands', Opt_ShowRemCommands); //1.81
  INIWrite('Programming', 'Opt_EnableBasincTips', Opt_EnableBasincTips);

  // AI Options

  INIWrite('AI', 'Opt_SelectedAIModel', Opt_SelectedAIModel);
  INIWrite('AI', 'Opt_BYOK_APIKEY', Opt_BYOK_APIKEY);

  // Syntax Highlight and Colour Options

  INIWrite('Highlight', 'Opt_SyntaxHightlight', Opt_SyntaxHighlight);

  INIWrite('Highlight', 'Opt_HighlightKeywords', Opt_HighlightKeywords);
  INIWrite('Highlight', 'Opt_KeywordsColour', Opt_KeywordsColour);
  INIWrite('Highlight', 'Opt_KeywordsBold', Opt_KeywordsBold);
  INIWrite('Highlight', 'Opt_KeywordsItalic', Opt_KeywordsItalic);

  INIWrite('Highlight', 'Opt_HighlightFunctions', Opt_HighlightFunctions);
  INIWrite('Highlight', 'Opt_FunctionsColour', Opt_FunctionsColour);
  INIWrite('Highlight', 'Opt_FunctionsBold', Opt_FunctionsBold);
  INIWrite('Highlight', 'Opt_FunctionsItalic', Opt_FunctionsItalic);

  INIWrite('Highlight', 'Opt_HighlightComments', Opt_HighlightComments);
  INIWrite('Highlight', 'Opt_CommentsColour', Opt_CommentsColour);
  INIWrite('Highlight', 'Opt_CommentsBold', Opt_CommentsBold);
  INIWrite('Highlight', 'Opt_CommentsItalic', Opt_CommentsItalic);

  INIWrite('Highlight', 'Opt_HighlightSymbols', Opt_HighlightSymbols);
  INIWrite('Highlight', 'Opt_SymbolsColour', Opt_SymbolsColour);
  INIWrite('Highlight', 'Opt_SymbolsBold', Opt_SymbolsBold);
  INIWrite('Highlight', 'Opt_SymbolsItalic', Opt_SymbolsItalic);

  INIWrite('Highlight', 'Opt_HighlightVars', Opt_HighlightVars);
  INIWrite('Highlight', 'Opt_VarsColour', Opt_VarsColour);
  INIWrite('Highlight', 'Opt_VarsBold', Opt_VarsBold);
  INIWrite('Highlight', 'Opt_VarsItalic', Opt_VarsItalic);

  INIWrite('Highlight', 'Opt_HighlightVarsUnDef', Opt_HighlightVarsUnDef);
  INIWrite('Highlight', 'Opt_VarsUnDefColour', Opt_VarsUnDefColour);
  INIWrite('Highlight', 'Opt_VarsUnDefBold', Opt_VarsUnDefBold);
  INIWrite('Highlight', 'Opt_VarsUnDefItalic', Opt_VarsUnDefItalic);

  INIWrite('Highlight', 'Opt_HighlightNumbers', Opt_HighlightNumbers);
  INIWrite('Highlight', 'Opt_NumbersColour', Opt_NumbersColour);
  INIWrite('Highlight', 'Opt_NumbersBold', Opt_NumbersBold);
  INIWrite('Highlight', 'Opt_NumbersItalic', Opt_NumbersItalic);

  INIWrite('Highlight', 'Opt_HighlightLineNums', Opt_HighlightLineNums);
  INIWrite('Highlight', 'Opt_LineNumsColour', Opt_LineNumsColour);
  INIWrite('Highlight', 'Opt_LineNumsBold', Opt_LineNumsBold);
  INIWrite('Highlight', 'Opt_LineNumsItalic', Opt_LineNumsItalic);

  INIWrite('Highlight', 'Opt_HighlightStrings', Opt_HighlightStrings);
  INIWrite('Highlight', 'Opt_StringsColour', Opt_StringsColour);
  INIWrite('Highlight', 'Opt_StringsBold', Opt_StringsBold);
  INIWrite('Highlight', 'Opt_StringsItalic', Opt_StringsItalic);

  INIWrite('Highlight', 'Opt_Foreground', Opt_Foreground);
  INIWrite('Highlight', 'Opt_Background', Opt_Background);

  INIWrite('Fonts', 'Opt_ToolFontSize', Opt_ToolFontSize);   //Arda
  INIWrite('Fonts', 'Opt_EditorCustomFont', Opt_EditorCustomFont);
  INIWrite('Fonts', 'Opt_EditorFontFilename', Opt_EditorFontFilename);
  INIWrite('Fonts', 'Opt_EditorFontFolder', Opt_EditorFontFolder);



  INIWrite('Highlight', 'Opt_Indenting', Opt_Indenting);   //Arda
  INIWrite('Highlight', 'Opt_IndentSize', Opt_IndentSize);   //Arda


  Opt_TFCol24:=ColorToHex(TFCol24);
  Opt_TFCol25:=ColorToHex(TFCol25);
  Opt_TFCol26:=ColorToHex(TFCol26);
  Opt_TFCol27:=ColorToHex(TFCol27);
  Opt_TFCol28:=ColorToHex(TFCol28);
  Opt_TFCol29:=ColorToHex(TFCol29);
  Opt_TFCol30:=ColorToHex(TFCol30);
  Opt_TFCol31:=ColorToHex(TFCol31);

  INIWrite('CustomColours', 'Opt_TFCol24', Opt_TFCol24);        //arda
  INIWrite('CustomColours', 'Opt_TFCol25', Opt_TFCol25);
  INIWrite('CustomColours', 'Opt_TFCol26', Opt_TFCol26);
  INIWrite('CustomColours', 'Opt_TFCol27', Opt_TFCol27);
  INIWrite('CustomColours', 'Opt_TFCol28', Opt_TFCol28);
  INIWrite('CustomColours', 'Opt_TFCol29', Opt_TFCol29);
  INIWrite('CustomColours', 'Opt_TFCol30', Opt_TFCol30);
  INIWrite('CustomColours', 'Opt_TFCol31', Opt_TFCol31);

  INIWrite('CursorColor1', 'Opt_CursorColor1', Opt_CursorColor1);
  INIWrite('CursorColor2', 'Opt_CursorColor2', Opt_CursorColor2);
  INIWrite('CustomColours', 'Opt_CursorBlinking', Opt_CursorBlinking);


  // Error Notification



  For Idx := 0 To 43 Do
     INIWrite('ErrorNotify', 'Error'+IntToStr(Idx), ErrorAddresses[Idx].Notify);
  INIWrite('ErrorNotify', 'Unknown', ErrorAddresses[33].Notify);
  INIWrite('ErrorNotify', 'Opt_CursorToError', Opt_CursorToError);

  // Assembler options

  INIWrite('Assembler', 'Opt_AsmStatusBar', Opt_AsmStatusBar);
  INIWrite('Assembler', 'Opt_AsmLabelList', Opt_AsmLabelList);

  // Debugger Options

  INIWrite('Debugger', 'Opt_AsmHexValues', Opt_AsmHexValues);
  INIWrite('Debugger', 'Opt_AsmAsciiBytes', Opt_AsmAsciiBytes);

  // Printing Options

  INIWrite('Printing', 'Opt_FastPrinting', Opt_FastPrinting);
  INIWrite('Printing', 'Opt_SavePrinting', Opt_SavePrinting);

    // Enhanced Hardware   R15

  INIWrite('EnhancedHardware', 'Opt_64Colours', Opt_64Colours);
  INIWrite('EnhancedHardware', 'Opt_ConsoleAddon', Opt_ConsoleAddon);
  INIWrite('EnhancedHardware', 'Opt_KMouse', Opt_KMouse);

    //Basinet
   INIWrite('Basinet', 'Opt_CheckUpdates', Opt_CheckUpdates);

   // Display window options
   INIWrite('DisplaySnap', 'Opt_DisplaySnap', Opt_DisplaySnap);
   INIWrite('DisplayOnTop', 'Opt_DisplayOnTop', Opt_DisplayOnTop);


  // Scaling Options

  INIWrite('Scaling', 'Opt_FontScale', Opt_FontScale);
  INIWrite('Scaling', 'Opt_MaintainAspect', Opt_MaintainAspect);
  INIWrite('Scaling', 'Opt_IntegerScaling', Opt_IntegerScaling);
  INIWrite('Scaling', 'Opt_RenderMethod', Ord(Opt_RenderMethod));
  INIWrite('Scaling', 'Opt_8BitStretch', Opt_8BitStretch);

  // FrameSkip

  INIWrite('FrameSkip', 'Opt_FrameSkip', Opt_FrameSkip);
  INIWrite('FrameSkip', 'Opt_AutoFrameSkip', Opt_AutoFrameSkip);

  // Sound Options

  INIWrite('Sound', 'Opt_SoundFrequency', Opt_SoundFrequency);
  INIWrite('Sound', 'Opt_SoundVolume', Opt_SoundVolume);
  INIWrite('Sound', 'Opt_SoundBits', Opt_SoundBits);
  INIWrite('Sound', 'Opt_SoundStereo', Opt_SoundStereo);
  INIWrite('Sound', 'Opt_NumSoundBuffers', Opt_NumSoundBuffers);
  INIWrite('Sound', 'Opt_SoundLatency', Opt_SoundLatency);
  INIWrite('Sound', 'Opt_SoundEnabled', Opt_SoundEnabled);
  INIWrite('Sound', 'Opt_EditorSounds', Opt_EditorSounds);
  INIWrite('Sound', 'Opt_KeyClick48k', Opt_KeyClick48k);
  INIWrite('Sound', 'Opt_DSoundSynch', Opt_DSoundSynch);

  // Embellishments

  INIWrite('Embellishments', 'Opt_Scanlines', Opt_Scanlines);
  INIWrite('Embellishments', 'Opt_ClipCorners', Opt_ClipCorners);
  INIWrite('Embellishments', 'Opt_FastResets', Opt_FastResets);


  // Snapshots

  INIWrite('Snapshots', 'Opt_z80Version', Opt_z80Version);
  INIWrite('Snapshots', 'Opt_ExternalExec', Opt_ExternalExec);
  INIWrite('Snapshots', 'Opt_Always128k', Opt_Always128k);

  // .BAS Files

  INIWrite('BASFiles', 'Opt_SavePretty', Opt_SavePretty);
  INIWrite('BASFiles', 'Opt_LoadAutoStart', Opt_LoadAutoStart);
  INIWrite('BASFiles', 'Opt_Autostart', Opt_Autostart);
  INIWrite('BASFiles', 'Opt_AutoBackup', Opt_AutoBackup);
  INIWrite('BASFiles', 'Opt_AutoBackInterval', Opt_AutoBackInterval);
  INIWrite('BASFiles', 'Opt_ShowNotes', Opt_ShowNotes);


  // Tape Images

  INIWrite('TAPFiles', 'Opt_TapeRewind', Opt_TapeRewind);
  INIWrite('TAPFiles', 'Opt_TapeTrapSAVE', Opt_TapeTrapSAVE);    //1.83
  INIWrite('TAPFiles', 'Opt_TapeTrapLOAD', Opt_TapeTrapLOAD);    //1.83


  //PaintBox

  INIWrite('Paintbox', 'Opt_MouseImage', Ord(Opt_MouseImage));

  // MRU List

  INIWrite('MRUList', 'NumEntries', MRUList.Count);
  For Idx := 0 To MRUList.Count -1 Do
     INIWrite('MRUList', 'File'+IntToStr(Idx), MRUList[Idx]);

  // SCR paintbox MRU list

  INIWrite('SCR_MRUList', 'NumEntries', ScrPaintForm.RecentPics.Count);
  For Idx := 0 To ScrPaintForm.RecentPics.Count -1 Do
     INIWrite('SCR_MRUList', 'File'+IntToStr(Idx), ScrPaintForm.RecentPics[Idx]);

  // Window Placements

  For Idx := 1 to Application.ComponentCount -1 Do Begin
     If Application.Components[Idx] is TForm then Begin
        If (Application.Components[Idx] As TForm).WindowState <> WsNormal Then
           (Application.Components[Idx] As TForm).WindowState := WsNormal;
        If (Application.Components[Idx] As TForm).Tag = 0 Then
           Vs := Ord((Application.Components[Idx] as TForm).Visible)
        Else
           Vs := Ord(False);
        Lt := (Application.Components[Idx] as TForm).Left;
        Tp := (Application.Components[Idx] as TForm).Top;
        Wd := (Application.Components[Idx] as TForm).Width;
        Ht := (Application.Components[Idx] as TForm).Height;
        INIWrite('WindowPlacements', Application.Components[Idx].Name, IntToStr(Vs)+','+IntToStr(Lt)+','+IntToStr(Tp)+','+IntToStr(Wd)+','+IntToStr(Ht));
     End;
  End;

  Try
     INI.SaveToFile(BASinDIR+'\basinC.ini');
  Except
     On EFCreateError Do
        MessageBox(BASinOutput.Handle, PChar('Could not save BasinC'#39's settings.'#13'The file may be in use by another process,'#13'or you might not have sufficient permissions to perform this operation.'), PChar('Save error'), MB_OK or MB_ICONWARNING);
  End;
  INI.Free;

End;

{INI File Routines}

Function INIFindSection(Section: String): Integer;
Begin
  Result := 0;
  While (Result < INI.Count) and (Lowercase(INI[Result]) <> '['+Lowercase(Section)+']') Do Inc(Result);
  If Result = INI.Count Then Begin
     If Result <> 0 Then Begin
        INI.Add(' ');
        Inc(Result);
     End;
     INI.Add('['+Section+']');
  End;
End;

Function INIFindEntry(Section, Entry: String): Integer;
Var
  SavedPosition: Integer;
Begin
  Result := INIFindSection(Section)+1;
  SavedPosition := Result;
  If Result = INI.Count Then Dec(Result);
  While Result < INI.Count Do Begin
     If Copy(INI[Result], 1, 1) <> '[' Then Begin
        If LowerCase(Copy(INI[Result], 1, Length(Entry)+1)) = Lowercase(Entry+'=') Then
           Exit
        Else
           Inc(Result);
     End Else Break;
  End;
  If Result = INI.Count Then Begin
     Result := SavedPosition;
     INI.Insert(Result, Entry+'=');
  End Else If Copy(INI[Result], 1, 1) = '[' Then Begin
     Result := SavedPosition;
     INI.Insert(Result, Entry+'=');
  End;
End;

Function INIRead(Section, Entry: String; Default: Boolean): Boolean;
Var
  EntryPos: Integer;
  Value: String;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  Value := Copy(INI[EntryPos], Length(Entry)+2, 999999);
  If Value = '1' Then Result := True;
  If Value = '0' Then Result := False;
  If Value = '' Then Begin
     Result := Default;
     If Result Then
        INI[EntryPos] := INI[EntryPos] + '1'
     Else
        INI[EntryPos] := INI[EntryPos] + '0';
  End;
End;

Function INIRead(Section, Entry: String; Default: String): String;
Var
  EntryPos: Integer;
  Value: String;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  Value := Copy(INI[EntryPos], Length(Entry)+2, 999999);
  If Value = '' Then Begin
     Result := Default;
     INI[EntryPos] := INI[EntryPos] + Default;
  End Else Result := Value;
End;

Function INIRead(Section, Entry: String; Default: Extended): Extended;
Var
  EntryPos: Integer;
  Value: String;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  Value := Copy(INI[EntryPos], Length(Entry)+2, 999999);
  If Value = '' Then Begin
     Result := Default;
     INI[EntryPos] := INI[EntryPos] + FloatToStrEx(Default);
  End Else Result := StrToFloat(Value);
End;

Function INIRead(Section, Entry: String; Default: Integer): Integer;
Var
  EntryPos: Integer;
  Value: String;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  Value := Copy(INI[EntryPos], Length(Entry)+2, 999999);
  If Value = '' Then Begin
     Result := Default;
     INI[EntryPos] := INI[EntryPos] + IntToStr(Default);
  End Else Result := StrToIntDef(Value, 0);
End;

Function INIRead(Section, Entry: String; Default: DWord): DWord;
Var
  EntryPos: Integer;
  Value: String;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  Value := Copy(INI[EntryPos], Length(Entry)+2, 999999);
  If Value = '' Then Begin
     Result := Default;
     INI[EntryPos] := INI[EntryPos] + IntToStr(Default);
  End Else Result := StrToIntDef(Value, 0);
End;

Function INIRead(Section, Entry: String; Default: Word): Word;
Var
  EntryPos: Integer;
  Value: String;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  Value := Copy(INI[EntryPos], Length(Entry)+2, 999999);
  If Value = '' Then Begin
     Result := Default;
     INI[EntryPos] := INI[EntryPos] + IntToStr(Default);
  End Else Result := StrToIntDef(Value, 0);
End;

Procedure INIWrite(Section, Entry: String; Value: Boolean);
Var
  EntryPos: Integer;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  If Value Then
    INI[EntryPos] := INI[EntryPos] + '1'
  Else
    INI[EntryPos] := INI[EntryPos] + '0';
End;

Procedure INIWrite(Section, Entry: String; Value: String);
Var
  EntryPos: Integer;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  INI[EntryPos] := INI[EntryPos] + Value;
End;

Procedure INIWrite(Section, Entry: String; Value: Integer);
Var
  EntryPos: Integer;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  INI[EntryPos] := INI[EntryPos] + IntToStr(Value);
End;

Procedure INIWrite(Section, Entry: String; Value: DWord);
Var
  EntryPos: Integer;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  INI[EntryPos] := INI[EntryPos] + IntToStr(Value);
End;

Procedure INIWrite(Section, Entry: String; Value: Word);
Var
  EntryPos: Integer;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  INI[EntryPos] := INI[EntryPos] + IntToStr(Value);
End;

Procedure INIWrite(Section, Entry: String; Value: Extended);
Var
  EntryPos: Integer;
Begin
  EntryPos := INIFindEntry(Section, Entry);
  INI[EntryPos] := INI[EntryPos] + FloatToStrEx(Value);
End;

// CPU Detection

Procedure GetOSVersion;
var
  verInfo : TOSVERSIONINFO;
begin
  verInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  OSIs95 := False;
  OSIs98 := False;
  OSIsME := False;
  OSIsNT := False;
	OSIsXP := False;
  OSIsNTBased := False;
  OSIs9xBased := False;
  If GetVersionEx(verInfo) then Begin
     OSIs95 := (verInfo.dwMajorVersion = 4) and (verInfo.dwMinorVersion = 0);
     OSIs98 := (verInfo.dwMajorVersion = 4) and (verInfo.dwMinorVersion = 10);
     OSIsME := (verInfo.dwMajorVersion = 4) and (verInfo.dwMinorVersion = 90);
     OSIsNT := verInfo.dwPlatformId = VER_PLATFORM_WIN32_NT;
     OSIsXP := (verInfo.dwMajorVersion > 5) or ((verInfo.dwMajorVersion = 5) and (verInfo.dwMinorVersion > 0));
  End;
  OSIsNTBased := OsIsNT or OSIsXP;
  OSIs9xBased := OSIs95 or OSIs98 or OSIsME;
End;

// Misc Graphics Routines

Procedure SPECTextToDIB(DIB: TFastDIB; X, Y: Integer; Text: String; Ink, Paper, Bright: Integer; Italics, useCHARS: Boolean);
Var
  F, G, H, Offset, ArrayOffset, S, ItalicPos: Integer;
  Bits, RealInk, RealPaper: Integer;
  NormalChars: Boolean;
Begin
  Y := DIB.Height - Y;
  For F := 1 To Length(Text) Do Begin
     S := Ord(Text[F]);
     If S > 127 Then useCHARS := False;
     If (F > 1) and (Ord(Text[F-1]) = 16) Then Begin
        Ink := S;
     End Else If (F > 1) and (Ord(Text[F-1]) = 17) Then Begin
        Paper := S;
     End Else If (F > 1) and (Ord(Text[F-1]) = 19) Then Begin
        Bright := S;
     End Else If S > 31 Then Begin
        If S < 144 Then Begin
           Offset := S - 32;
           NormalChars := True;
        End Else Begin
           If S < 165 Then Begin
              OffSet := S - 144;
              NormalChars := False;
           End Else Begin
              Offset := S-53;
              NormalChars := True;
           End;
        End;

        ArrayOffset := OffSet*8;

        If Ink = 16 Then Begin
           If (Paper < 4) or ((Paper > 7) and (Paper < 12)) Then
              RealInk := 7
           Else
              RealInk := 0;
        End Else
           RealInk := Ink;

        If Paper = 16 Then Begin
           If (RealInk < 4) or ((RealInk > 7) and (RealInk < 12)) Then
              RealPaper := 7
           Else
              RealPaper := 0;
        End Else
           RealPaper := Paper;

        If RealInk < 8 Then RealInk := RealInk + (Bright * 8);
        If RealPaper < 8 Then RealPaper := RealPaper + (Bright * 8);

        If Not Italics Then Begin
           For G := 0 To 7 Do Begin
              If NormalChars Then Begin
                 If UseCHARS Then
                    Bits := Memory[G+ArrayOffset+GetWord(@Memory[CHARS])+256]
                 Else
                    Bits := EditorChars[G+ArrayOffset+1];
              End Else
                 Bits := Memory[G+ArrayOffset+65368];
              For H := 7 DownTo 0 Do Begin
                 If PtInRect(Rect(0, 0, DIB.Width, DIB.AbsHeight), Point(X+(7-H), Y-G)) Then Begin
                    If Bits and (1 Shl H) > 0 Then Begin
                       DIB.Pixels32[Y-G, X+(7-H)] := DisplayPalette[RealInk]
                    End Else If Paper > -1 Then
                       DIB.Pixels32[Y-G, X+(7-H)] := DisplayPalette[RealPaper];
                 End;
              End;
           End;
        End Else Begin
           For G := 0 To 7 Do Begin
              ItalicPos := G Div 4;
              If NormalChars Then Begin
                 If UseCHARS Then
                    Bits := Memory[G+ArrayOffset+GetWord(@Memory[CHARS])+256]
                 Else
                    Bits := EditorChars[G+ArrayOffset+1]
              End Else
                 Bits := Memory[G+ArrayOffset+65368];
              For H := 7 DownTo 0 Do Begin
                 If PtInRect(Rect(0, 0, DIB.Width, DIB.AbsHeight), Point(X+(7-H), Y-G)) Then Begin
                    If Bits and (1 Shl H) > 0 Then Begin
                       DIB.Pixels32[Y-G, X+(7-H)-ItalicPos] := DisplayPalette[RealInk]
                    End Else If Paper > -1 Then
                       DIB.Pixels32[Y-G, X+(7-H)-ItalicPos] := DisplayPalette[RealPaper];
                 End;
              End;
           End;
        End;
        Inc(X, 8);
     End;
  End;
End;

Procedure Scale2xDIB(Src, Dst: TFastDIB); // 8bpp only!
Begin
  asm

     Pushad

     mov esi, [dst]
     mov edi, [esi+TFastDIB.bits]
     Mov ecx, 15376
     Mov eax, BorderDWord

     Rep Stosd                     // Fill the Top border

     mov edx, 335

  @Loop:
     Add edi, 512
     mov ecx, 32

     rep stosd                     // Fill Right then overflow to Left border

     dec edx
     jnz @Loop

     Add edi, 512
     mov ecx, 7696

     rep stosd                     // Fill Bottom Border

     mov edi, [esi+TFastDIB.Bits]
     mov esi, [src]
     mov esi, [esi+TFastDIB.Bits]  // esi and edi point to src and dst bits respectively

     add edi, 640*48+64            // edi is the destination pixel - top left of a 2x2 matrix (dy, dx)
     add esi, 320*23+32            // esi is the source pixel - top of a 3x3 matrix (y-1, x)

     mov dh, 192

  @NextRow:

     mov dl, 0

  @PixelTest:

     mov al, [esi]                 // al = B
     mov bx, [esi+319]             // bl = D, bh = E
     mov ah, [esi+321]             // ah = F
     mov cl, [esi+640]             // cl = H

     cmp al, cl                    // If (B <> H) and (D <> F) Then ...
     je  @FillAllE
     cmp bl, ah
     je  @FillAllE

  @Test1:
     cmp bl, al
     jne @TopLeftE
     Mov [edi], bl
     jmp @Test2

  @TopLeftE:
     Mov [edi], bh

  @Test2:
     cmp al, ah
     jne @TopRightE
     Mov [edi+1], ah
     jmp @Test3

  @TopRightE:
     Mov [edi+1], bh

  @Test3:
     cmp bl, cl
     jne @BottomLeftE
     Mov [edi+640], bl
     jmp @Test4

  @BottomLeftE:
     Mov [edi+640], bh

  @Test4:
     cmp cl, ah
     jne @BottomRightE
     Mov [edi+641], ah
     jmp @NextPixel

  @BottomRightE:
     Mov [edi+641], bh
     jmp @NextPixel

  @FillAllE:

     mov bl, bh
     Mov [edi], bx
     Mov [edi+640], bx

  @NextPixel:

     Inc esi
     Add edi,2
     inc dl
     jnz @PixelTest

     add esi, 64
     add edi, 640+128

     Dec dh
     jnz @NextRow

     popad
  End;
End;

Procedure Scale3xDIB(Src, Dst: TFastDIB); // 8bpp only!
Begin
  asm
     Pushad

     mov esi, [dst]
     mov edi, [esi+TFastDIB.bits]
     Mov ecx, 17304
     Mov eax, BorderDWord

     Rep Stosd                     // Fill the Top border

     mov edx, 575

  @Loop:
     Add edi, 768
     mov ecx, 48

     rep stosd                     // Fill Right then overflow to Left border

     dec edx
     jnz @Loop

     Add edi, 768
     mov ecx, 17304

     rep stosd                     // Fill Bottom Border

     mov edi, [esi+TFastDIB.Bits]
     mov esi, [src]
     mov esi, [esi+TFastDIB.Bits]  // esi and edi point to src and dst bits respectively

     add edi, 960*72+96            // edi is the destination pixel - top left of a 3x3 matrix (dy, dx)
     add esi, 320*23+32            // esi is the source pixel - top of a 3x3 matrix (y-1, x)

     mov dh, 192

  @NextRow:

     mov dl, 0

  @PixelTest:

     mov al, [esi]                 // al = B
     mov bx, [esi+319]             // bl = D, bh = E
     mov ah, [esi+321]             // ah = F
     mov cl, [esi+640]             // cl = H

  @Test1:
     cmp bl, al
     jne @TopLeftE
     cmp al, ah
     je  @TopLeftE
     cmp bl, cl
     je  @TopLeftE
     Mov [edi], bl
     jmp @Test2

  @TopLeftE:
     Mov [edi], bh

  @Test2:
     Mov [edi+1], bh

     cmp al, ah
     jne @TopRightE
     cmp bl, al
     je  @TopRightE
     cmp ah, cl
     je  @TopRightE
     Mov [edi+2], ah
     jmp @Test3

  @TopRightE:
     Mov [edi+2], bh

  @Test3:
     Mov [edi+960], bh
     Mov [edi+961], bh
     Mov [edi+962], bh

     cmp bl, cl
     jne @BottomLeftE
     cmp bl, al
     je  @BottomLeftE
     cmp ah, cl
     je  @BottomLeftE
     Mov [edi+1920], bl
     jmp @Test4

  @BottomLeftE:
     Mov [edi+1920], bh

  @Test4:
     Mov [edi+1921], bh

     cmp cl, ah
     jne @BottomRightE
     cmp bl, cl
     je  @BottomRightE
     cmp al, ah
     je  @BottomRightE
     Mov [edi+1922], ah
     jmp @NextPixel

  @BottomRightE:
     Mov [edi+1922], bh

  @NextPixel:

     Inc esi
     Add edi, 3
     inc dl
     jnz @PixelTest

     add esi, 64
     add edi, 1920+192

     Dec dh
     jnz @NextRow

     popad

  End;

End;

Procedure Scale4xDIB(Src, Dst: TFastDIB);
Var
  Sw, Sh: Integer;
  X, Y: DWord;
  DX, DY: DWord;
  B, D, E, F, H: Byte;
Begin

  Sw := Src.Width;
  Sh := Src.Height;

  Dst.SetSize(Src.Width *2, Src.Height *2, 8);

  DX := 64;
  DY := 48;
  For Y := 24 to 215 Do Begin
     For X := 32 To 287 Do Begin
        B := Src.Pixels8[Y-1, X  ];
        D := Src.Pixels8[Y  , X-1];
        E := Src.Pixels8[Y  , X  ];
        F := Src.Pixels8[Y  , X+1];
        H := Src.Pixels8[Y+1, X  ];
        If (B <> H) and (D <> F) then begin
	         If D = B Then Dst.Pixels8[DY  , DX  ] := D Else Dst.Pixels8[DY  , DX  ] := E;
	         If B = F Then Dst.Pixels8[DY  , DX+1] := F Else Dst.Pixels8[DY  , DX+1] := E;
	         If D = H Then Dst.Pixels8[DY+1, DX  ] := D Else Dst.Pixels8[DY+1, DX  ] := E;
	         If H = F Then Dst.Pixels8[DY+1, DX+1] := F Else Dst.Pixels8[DY+1, DX+1] := E;
        End Else Begin
           Dst.Pixels8[DY  , DX  ] := E;
           Dst.Pixels8[DY  , DX+1] := E;
           Dst.Pixels8[DY+1, DX  ] := E;
           Dst.Pixels8[DY+1, DX+1] := E;
        End;
        Inc(DX, 2);
     End;
     Inc(DY, 2);
     DX := 64;
  End;

  Src.SetSize(Dst.Width *2, Dst.Height *2, 8);

  B := BorderDWord and $FF;
  For DY := 0 To 191 Do
     For DX := 0 To 1279 Do
        Src.Pixels8[DY, DX] := B;
  For DY := 192 To 863 Do
     For DX := 0 To 255 Do
        Src.Pixels8[DY, DX] := B;
  For DY := 192 To 863 Do
     For DX := 1152 To 1279 Do
        Src.Pixels8[DY, DX] := B;
  For DY := 863 To 959 Do
     For DX := 0 To 1279 Do
        Src.Pixels8[DY, DX] := B;

  DX := 128;
  DY := 96;
  For Y := 48 to 431 Do Begin
     For X := 64 To 575 Do Begin
        B := Dst.Pixels8[Y-1, X  ];
        D := Dst.Pixels8[Y  , X-1];
        E := Dst.Pixels8[Y  , X  ];
        F := Dst.Pixels8[Y  , X+1];
        H := Dst.Pixels8[Y+1, X  ];
        If (B <> H) and (D <> F) then begin
	         If D = B Then Src.Pixels8[DY  , DX  ] := D Else Src.Pixels8[DY  , DX  ] := E;
	         If B = F Then Src.Pixels8[DY  , DX+1] := F Else Src.Pixels8[DY  , DX+1] := E;
	         If D = H Then Src.Pixels8[DY+1, DX  ] := D Else Src.Pixels8[DY+1, DX  ] := E;
	         If H = F Then Src.Pixels8[DY+1, DX+1] := F Else Src.Pixels8[DY+1, DX+1] := E;
        End Else Begin
           Src.Pixels8[DY  , DX  ] := E;
           Src.Pixels8[DY  , DX+1] := E;
           Src.Pixels8[DY+1, DX  ] := E;
           Src.Pixels8[DY+1, DX+1] := E;
        End;
        Inc(DX, 2);
     End;
     Inc(DY, 2);
     DX := 128;
  End;

  Dst.SetSize(Src.Width, Src.Height, 8);
  Src.Draw(Dst.hDc, 0, 0);
  Src.SetSize(Sw, Sh, 8);

  Src.Colors := @DisplayPalette;
  Src.UpdateColors;
  Dst.Colors := @DisplayPalette;
  Dst.UpdateColors;

End;

Procedure GetShiftState(Var Shift: TShiftState);
begin
  Shift := [];
  If GetASyncKeyState(VK_SHIFT) and 1 = 1 Then Include(Shift, ssShift);
  If GetASyncKeyState(VK_CONTROL) and 1 = 1 then Include(Shift, ssCtrl);
  If GetASyncKeyState(VK_MENU) and 1 = 1 then Include(Shift, ssAlt);
end;

procedure TNewScrollBox.VScroll(Pos: Integer; EventType: TVScrollEventType);
begin
  if assigned(FOnVScroll) then FOnVScroll(Self, Pos, EventType);
end;

procedure TNewScrollBox.HScroll(Pos: Integer; EventType: THScrollEventType);
begin
  if assigned(FOnHScroll) then FOnHScroll(Self, Pos, EventType);
end;

procedure TNewScrollBox.WMVScroll(var Message: TWMScroll);
var
  EventType : TVScrollEventType;
begin
  inherited;
  FillChar(ScrollInfo, SizeOf(TScrollInfo), 0);
  ScrollInfo.cbsize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  EventType := TVScrollEventType(Message.ScrollCode);
  if EventType in [vsThumbPos, vsThumbTrack] then
    VScroll(Message.Pos, EventType)
  else
    VScroll(VertScrollBar.Position, EventType)
end;

procedure TNewScrollBox.WMHScroll(var Message: TWMScroll);
var
  EventType : THScrollEventType;
begin
  inherited;
  FillChar(ScrollInfo, SizeOf(TScrollInfo), 0);
  ScrollInfo.cbsize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  EventType := THScrollEventType(Message.ScrollCode);
  if EventType in [hsThumbPos, hsThumbTrack] then
    HScroll(Message.Pos, EventType)
  else
    HScroll(HorzScrollBar.Position, EventType)
end;

constructor TNewScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FOnVScroll := nil;
  FOnHScroll := nil;
end;




Procedure TColourLabel.Paint;
Var
  F, X, Y: Integer;
  txtRect: TRect;
  Text: String;
Const
  Colors: Array[0..7] of TColor = (CLBlack, CLNavy, CLMaroon, CLPurple, CLGreen, CLTeal, CLOlive, CLSilver);
Begin
  X := 0; Y := 0;
  Text := Str;
  Canvas.Brush.Style := BsClear;
  If Canvas.Font.Name <> 'Tahoma' Then
     Canvas.Font.Name := 'Tahoma';
  txtRect := Rect(0, 0, Width, Height);
  For F := 1 to Length(Text) Do Begin
     If Ord(Text[F]) < 31 Then Begin
        If Text[F] = Chr(16) Then Canvas.Font.Color := Colors[Ord(Text[F+1])];
        If Text[F] = Chr(25) Then If FsBold in Canvas.Font.Style Then Canvas.Font.Style := Canvas.Font.Style - [FsBold] Else Canvas.Font.Style := Canvas.Font.Style + [FsBold];
        If Text[F] = Chr(17) Then Canvas.Font.Color := Colors[Ord(Text[F+1])];
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

procedure LockControl(c: TWinControl; lock: boolean);
begin
  if (c=nil) or (c.Handle=0) then exit;
  if lock then
     SendMessage(c.Handle,WM_SETREDRAW,0,0)
  else begin
     SendMessage(c.Handle,WM_SETREDRAW,1,0);
     RedrawWindow(c.Handle,nil,0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

     //R15
Procedure SizeForm(Form: TForm; X, Y, W, H: Integer);
Begin

  If X + W > Screen.Width - 16 Then Dec(X, (X + W) - (Screen.Width + 16));
  If Y + H > Screen.Height - 16 Then Dec(Y, (Y + H) - (Screen.Height + 16));
  If Y < 16 Then Y := 16;
  If X < 16 Then X := 16;
  Form.SetBounds(X, Y, W, H);

End;
    //eR15



procedure GetBuildInfo(var V1, V2, V3, V4: word);
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize > 0 then
  begin
      GetMem(VerInfo, VerInfoSize);
      try
        if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
        begin
          VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
          with VerValue^ do
          begin
            V1 := dwFileVersionMS shr 16;
            V2 := dwFileVersionMS and $FFFF;
            V3 := dwFileVersionLS shr 16;
            V4 := dwFileVersionLS and $FFFF;
          end;
        end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
  end;
end;


function ShowCustomDlg(const Msg: string; IconType: TMsgDlgType;
  const Btn1, Btn2, Btn3: string): Integer;
var
  Dlg: TForm;
  B1, B2, B3: TButton;
  BtnTop, BtnLeft, BtnSpacing: Integer;
begin
  Dlg := CreateMessageDialog(Msg, IconType, []);
  try
    Dlg.Position := poScreenCenter;
    BtnTop := Dlg.ClientHeight - 50;
    BtnLeft := 40;
    BtnSpacing := 100;

    // Button 1
    B1 := TButton.Create(Dlg);
    B1.Parent := Dlg;
    B1.Caption := Btn1;
    B1.ModalResult := 1;
    B1.Left := BtnLeft;
    B1.Top := BtnTop;

    // Button 2 (optional)
    if Btn2 <> '' then
    begin
      B2 := TButton.Create(Dlg);
      B2.Parent := Dlg;
      B2.Caption := Btn2;
      B2.ModalResult := 2;
      B2.Left := B1.Left + BtnSpacing;
      B2.Top := BtnTop;
    end;

    // Button 3 (optional)
    if Btn3 <> '' then
    begin
      B3 := TButton.Create(Dlg);
      B3.Parent := Dlg;
      B3.Caption := Btn3;
      B3.ModalResult := 3;
      B3.Left := B1.Left + 2 * BtnSpacing;
      B3.Top := BtnTop;
    end;

    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;



function GetBuildInfoAsString(BuildIncluded: Boolean): string;
var
  V1, V2, V3, V4: word;
begin
  GetBuildInfo(V1, V2, V3, V4);
  If (BuildIncluded) Then Begin
    If (V3>0) Then Begin
        Result := IntToStr(V1) + '.' + IntToStr(V2) + ' Iteration ' +IntToStr(V3) + ' Build ' + IntToStr(V4);
    End Else Begin
        Result := IntToStr(V1) + '.' + IntToStr(V2) + ' Build' + IntToStr(V4);
    End;
  End Else Begin
    If (V3>0) Then Begin
        Result := IntToStr(V1) + '.' + IntToStr(V2) + IntToStr(V3) ;
    End Else Begin
        Result := IntToStr(V1) + '.' + IntToStr(V2) ;
    End;
  End;

end;

Procedure GetBASinDIR;
Begin
  BASinDir := ExtractFilePath(Application.Exename);
  While Copy(BASinDir, Length(BASinDir), 1) = '\' Do
     BASinDir := Copy(BASinDir, 1, Length(BASinDir)-1);
End;

Initialization

BASinDir := ExtractFilePath(Application.Exename);
While Copy(BASinDir, Length(BASinDir), 1) = '\' Do
   BASinDir := Copy(BASinDir, 1, Length(BASinDir)-1);
Opt_EditorFontFolder := BASinDir+'\';

end.
