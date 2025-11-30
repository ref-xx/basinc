// **********************************************
// *                                            *
// * ROMUtils.pas                               *
// *                                            *
// * Routines for altering the standard 48k ROM *
// * for PC keyboard access, and other          *
// * functions such as loading and saving to    *
// * hard disk rather than tape.                *
// *                                            *
// * (C) 2002-2007 By Paul Dunn.                *
// * (C) 2008-2025 By Arda Erdikmen             *
// **********************************************


unit ROMUtils;

interface

Uses Windows, Dialogs,Controls, Classes, SysUtils, Clipbrd, Math, Messages;

Type

  TSpectrumError    = Record Modified: Boolean; Address: Word; Desc: AnsiString; Notify: Boolean; End;
  TFindResult       = Record LineNum, Position, Address: Word; End;
  TSymbol           = Record sType: Integer; Desc: AnsiString; Address: DWord; End;

	// ROM Traps and interface management

  Procedure CheckFor128kCommands;
  Procedure SetRAMDisk;
  Function  LoadRom(Var Location: Array of Byte): Boolean;
  Procedure LoadRom128k(Var Location: Array of Byte);
  Procedure PageROM(Page128k: Boolean);
  Procedure Page7FFD(Value: Byte);
  Procedure Page7FFDTransparent(Value, Paged: Byte);
  Procedure ROMTrap;
	Procedure ModifyROM;
  Function  GetCurrentStatement(Line: AnsiString): AnsiString;
	Function  GetEditLine: AnsiString;
  Procedure PutEditLine(Tokens: AnsiString; Var Mem: Array of Byte);
  Function  TokeniseLine(Line: AnsiString; TokeniseStrings: Boolean): AnsiString;
  Function  DeTokeniseLine(Line: AnsiString; DetokeniseStrings: Boolean): AnsiString;
	Function  EditCursorMove(MoveMode: Byte): Byte;
  Function  GetMemoryString(Address, Count: Word; var Mem: Array of Byte): AnsiString;
  Procedure DoError(Code: Byte; ErrorText: AnsiString);
  Procedure RestoreErrors;
  Function  GetLineMatch(Line: Word): Integer;
  Function  GetLineAddress(Line, Statement: DWord; Default: Word): Word;
  Procedure DeleteEditorLines(Lines: AnsiString);
  Procedure DeleteBASICLines(Lines: AnsiString);
  Procedure SendLineToEditor(Line: Word; Statement: Byte);
  Procedure MoveSpectrumMemory(StartAddr: Word; Dist: Integer);
  Function  GetVariable(VarName: AnsiString; Address: Word): AnsiString;
  Procedure GetVarsList(var List: TStringlist);
  Procedure CopyListing;
  Procedure CutEditLine;
  Procedure CopyEditLine;
  Procedure PasteEditLine;
  Procedure DeleteEditLine;
  Procedure ClearSelection;
  Procedure ReplaceSelection(NewText: AnsiString);
  Procedure InsertText(Text: AnsiString; Posn: Integer);
  Function  GetCursorPos: Word;
  Function  FindNextForward(FindText: AnsiString; MatchCase: Boolean): TFindResult;
  Function  FindNextBackward(FindText: AnsiString; MatchCase: Boolean): TFindResult;
  Function  ReversePos(SubString: AnsiString; Text: AnsiString): Integer;
  Procedure ReplaceText(Address: Word; Text: AnsiString; Bytes: Word);
  Procedure LOADQuoteQuote(FName: AnsiString);
  Function  AutoLine(Tokens: AnsiString): AnsiString;
  Function  InsertLine(Line: AnsiString; Overwrite: Boolean): AnsiString;
  Procedure GenerateBASICChecksum(var Check: DWord);
  Function  GetProgramLineCount: Word;
  Procedure RenumberBASIC(Start, Step: Word);
  Procedure RenumberBASICEx(RangeStart, RangeEnd, StartLine, Step: Integer);
  Function  GetClipText(Index: Integer): AnsiString;
  Function  Editing: Boolean;
  Function  InInput: Boolean;

  Procedure DoContinue;

Var

  Rom48k:              Array[0..16383] of Byte;
  Rom128k:             Array[0..32767] of Byte;
  RAMDiskBank:         Array[0..65535] of Byte;

  RAMDiskNeedsInit,
  n128kAvailable,
  UsesUDGsTU,
  In128k:              Boolean;
  Last7FFD,
  LastFFFD,
  CurROM:              Byte;
  FASTMode:            Boolean;
  CurrentCommand:      Byte;
  Trapped:             Boolean;
  ProgStateFlag:       Byte = 2; // Set to PS_RESET as default
  InsertList,
  ClipList:            TStringlist;
  NeedVarsUpdate,
  CLS_LOWER,
  NeedParseUpdate:	   Boolean;
  ResetCounter:        DWord;
  EditTokenPos:        Word;
  EditToken:           Byte;
  CurPPC:              Word;
  CurSUBPPC:           Byte;
  LastError:           Byte;
  LastErrorLine:       Word;
  LastErrorStatement:  Byte;
  LastFind_E_PPC:      Word;
  LastFind_SUBPPC:     Word;
  LastFindText:        AnsiString;
  PrevLineAddr:        Word;
  GOSUBStackPos:       Word;
  GOSUBStackSize:      Word;
  CursorType:          AnsiChar;
  SelStartAddr:        Word;
  SelSaveAttrs:        Byte;
  NeedAutoList:        Boolean;
  AttrsSaved:          Boolean;
  BASICCheckSum:       DWord;
  WantBreak:           Boolean;

  TrapPrint:           Boolean = False;
  //TrapCounter:         Integer=0;

  RAMDiskArray:        Array[0..10] of Byte;

  // Addresses of the BASIC error reports in the 48k ROM.
  // Not CONSTs as the Modified part needs to be set or unset
  // as we use these errors.

  Errors: Array[0..43, 1..4] of AnsiString =
  (('0', 'Ok', 'Successful completion, or jump to a line number bigger than any existing. This report does not change the line and statement jumped to by CONTINUE.', 'Any'),
   ('1', 'NEXT without FOR', 'The control variable does not exist (it has not been set up by a FOR statement), but there is an ordinary variable with the same name.', 'NEXT'),
   ('2', 'Variable not found', 'For a simple variable, this will happen if the variable is used'+' before it has been assigned to by a LET, READ or INPUT statement, loaded from disk, or set up in a FOR statement. For a subscripted variable, it will happen if the variable is used before it has been dimensioned in DIM statement, or loaded from disk.', 'Any'),
   ('3', 'Subscript Wrong', 'A subscript is beyond the dimension of the array, or there are the wrong number of subscripts. If the subscript is negative or bigger than 65535 then error B will result.', 'Subscripted variables, Substrings'),
   ('4', 'Out of memory', 'There is not enough room in memory for what you are trying to do. If the interpreter really seems to be stuck in this state then you may need to issue a CLEAR statement to make room by removing in-memory variables.', 'LET, INPUT, FOR, DIM, GO SUB, LOAD, MERGE. Sometimes during expression evaluation.'),
   ('5', 'Out of screen space', 'An INPUT statement has tried to generate more than 23 lines in the lower half of the screen. Also occurs with PRINT AT 22,xx.', 'INPUT, PRINT AT'),
   ('6', 'Number too big ', 'Calculations have yielded a number bigger than 10 raised to the power of 38.', 'Any arithmetic.'),
   ('7', 'RETURN without GO SUB', 'There has been one more RETURN than there were GO SUBs.', 'RETURN'),
   ('8', 'Fatal unknown error message', 'The interpreter has come across a statement or expression which it cannot handle. The program flow will be terminated.', 'Any expression or statement.'),
   ('9', 'STOP statement', 'After this, CONTINUE will not repeat the STOP, but will carry on with the statement after.', 'STOP'),
   ('A', 'Invalid Argument', 'The argument for a function is unsuitable (for some reason).', 'SQR, LN, ASC, ACS, USR (with String argument)'),
   ('B', 'Integer out of range', 'When an integer is required, the floating point argument is rounded to the nearest integer. If this is outside a suitable range, then this error results. For Array Access, also see Error 3.', 'RUN, RANDOMIZE, POKE, DIM, GO TO, GO SUB, LIST, LLIST, PAUSE, PLOT, CHR$, PEEK, USR (with numeric argument)'),
   ('C', 'Nonsense in BASIC', 'The text of the (string) argument does not form a valid expression. Also used when the argument for a function or command is outrageously wrong.', 'VAL, VAL$'),
   ('D', 'BREAK - CONTINUE repeats', 'BREAK was pressed during some peripheral operation. The behaviour of CONTINUE after this report is normal in that it repeats the statement. Compare with report L.', 'LOAD, SAVE, VERIFY, MERGE. Also when the computer asks "Scroll?" and you press N, BREAK or the space bar.'),
   ('E', 'Out of DATA', 'You have tried to READ past the end of the DATA list.', 'READ'),
   ('F', 'Invalid Filename', 'SAVE with a filename not descriptive of a file on your hard drive.', 'SAVE'),
   ('G', 'No room for line', 'There is not enough room left in program memory to accommodate the new line.', 'Entering a line into the program.'),
   ('H', 'STOP in INPUT', 'Some INPUT data started with the keyword "STOP". Unlike the case with report 9, after this report, CONTINUE will behave normally, by repeating the INPUT statement.', 'INPUT'),
   ('I', 'FOR without NEXT', 'There was a FOR loop to be executed no times (eg. FOR n=1 TO 0) and the corresponding NEXT statement could not be found.', 'FOR'),
   ('J', 'Invalid I/O device', 'You are attempting to input characters from (or output characters'+' to) a device that doesn`t support it. For example, it is not possible to input characters from the screen stream, or to write characters to a read-only file. A command such as INPUT #2,A$ will therefore result in this error.', 'Stream operations; OPEN #, CLOSE #, INPUT #, PRINT # etc.'),
   ('K', 'Invalid colour', 'The number specified is not an appropriate colour. Colours for INK, PAPER and BORDER range from 0 to 7, BRIGHT, FLASH, INVERSE and OVER use 0, 1, and 8.', 'INK, PAPER, BORDER, FLASH, BRIGHT, INVERSE, OVER; also after any one of the corresponding control characters.'),
   ('L', 'BREAK into program', 'BREAK pressed. This is detected between two statements. The line and '+'statement number on the report refer to the statement before BREAK was pressed, but CONTINUE goes to the statement after (allowing for any jumps to be made), so that it does not repeat any statements.', 'Any'),
   ('M', 'RAMTOP no good', 'The number specified for RAMTOP is either too big or too small.', 'CLEAR, possibly in RUN'),
   ('N', 'Statement lost', 'Jump to a statement that no longer exists.', 'RETURN, NEXT, CONTINUE'),
   ('O', 'Invalid stream', 'Trying to input from (or output to) a stream that isn`t open or that is out of range (0 to 15); or trying to open a stream that is out of range.', 'INPUT #, OPEN #, PRINT #'),
   ('P', 'FN without DEF FN', 'User-defined function used without a corresponding DEF FN in the program.', 'FN'),
   ('Q', 'Parameter error', 'Wrong number of arguments, or one of them is the wrong type (String instead of a number, or vice-versa).', 'Any function.'),
   ('R', 'File loading error', 'A file was found on disk, but could not be loaded for some reason, or would not verify.', 'VERIFY, LOAD, MERGE'),
   ('a', 'MERGE error', 'MERGE ! would not execute for some reason - either size or filetype is wrong.', 'MERGE !'),
   ('b', 'Wrong file type', 'A file of inappropriate type was specified during RAM disk operation, for instance a CODE file in LOAD !"name".', 'MERGE !, LOAD !'),
   ('c', 'CODE error', 'The size of the file would lead to an overflow past the top of the memory.', 'LOAD! file CODE'),
   ('d', 'Too many brackets', 'Too many brackets in a repeated phrase around one of the arguments.', 'PLAY'),
   ('e', 'File already exists', 'The filename specified has already been used.', 'SAVE !'),
   ('f', 'Invalid name', 'The filename specified is empty or above 10 characters in length.', 'SAVE !, ERASE !'),
   ('g', 'RAMDisk error', 'This error will never be shown, and represents a catastrophic RAM disk failure.', 'LOAD !, SAVE !, CAT !, ERASE !'),
   ('h', 'File does not exist', 'There is no file in the RAM disk that has the name specified.', 'LOAD !, MERGE !, ERASE !'),
   ('i', 'Invalid device', 'The device name following the FORMAT command does not exist or correspond to a physical device.', 'FORMAT'),
   ('j', 'Invalid baud rate', 'The baud rate for the RS232 was set to zero.', 'FORMAT LINE'),
   ('k', 'Invalid note name', 'PLAY came across a note or command that it didn'#39't recognise, or a command which was in lower case.', 'PLAY'),
   ('l', 'Number too big', 'A parameter for a command is an order of magnitude too big.', 'PLAY'),
   ('m', 'Note out of range', 'A series of sharps or flats has taken a note beyond the range of the sound chip.', 'PLAY'),
   ('n', 'Out of range', 'A parameter for a command is too big or too small. If the error is very large, error "l" results.', 'PLAY'),
   ('o', 'Too many tied notes', 'An attempt was made to tie too many notes together.', 'PLAY'),
   ('?', 'Unknown Error', 'The statement caused a jump to the ROM error routine at address 8, with an invalid error code in the system variable "ERR NR".', 'Typically occurs when a USR 8 is executed from BASIC'));

  ErrorAddresses: Array[0..43] Of TSpectrumError =
     ((Address:$1392; Desc:'0 OK'; Notify:True),
      (Address:$1394; Desc:'1 NEXT without FOR'; Notify:True),
      (Address:$13A4; Desc:'2 Variable not found'; Notify:True),
      (Address:$13B6; Desc:'3 Subscript wrong'; Notify:True),
      (Address:$13C5; Desc:'4 Out of memory'; Notify:True),
      (Address:$13D2; Desc:'5 Out of screen'; Notify:True),
      (Address:$13DF; Desc:'6 Number too big'; Notify:True),
      (Address:$13ED; Desc:'7 RETURN without GO SUB'; Notify:True),
      (Address:$1401; Desc:'8 End of file'; Notify:True),
      (Address:$140C; Desc:'9 STOP statement'; Notify:True),
      (Address:$141A; Desc:'A Invalid argument'; Notify:True),
      (Address:$142A; Desc:'B Integer out of range'; Notify:True),
      (Address:$143E; Desc:'C Nonsense in BASIC'; Notify:True),
      (Address:$144F; Desc:'D BREAK - CONT repeats'; Notify:True),
      (Address:$1463; Desc:'E Out of DATA'; Notify:True),
      (Address:$146E; Desc:'F Invalid filename'; Notify:True),
      (Address:$147F; Desc:'G No room for line'; Notify:True),
      (Address:$148F; Desc:'H STOP in INPUT'; Notify:True),
      (Address:$149C; Desc:'I NEXT without FOR'; Notify:True),
      (Address:$14AC; Desc:'J Invalid I/O device'; Notify:True),
      (Address:$14BE; Desc:'K Invalid colour'; Notify:True),
      (Address:$14CC; Desc:'L BREAK into program'; Notify:True),
      (Address:$14DE; Desc:'M RAMTOP no good'; Notify:True),
      (Address:$14EC; Desc:'N Statement lost'; Notify:True),
      (Address:$14FA; Desc:'O Invalid Stream'; Notify:True),
      (Address:$1508; Desc:'P FN without DEF'; Notify:True),
      (Address:$1516; Desc:'Q Parameter error'; Notify:True),
      (Address:$1525; Desc:'R File loading error'; Notify:True),
      (Address:$1392; Desc:'a MERGE error'; Notify:True),
      (Address:$1392; Desc:'b Wrong file type'; Notify:True),
      (Address:$1392; Desc:'c CODE error'; Notify:True),
      (Address:$1392; Desc:'d Too many brackets'; Notify:True),
      (Address:$1392; Desc:'e File already exists'; Notify:True),
      (Address:$1392; Desc:'f Invalid name'; Notify:True),
      (Address:$1392; Desc:'g RAMDisk error'; Notify:True),
      (Address:$1392; Desc:'h File does not exist'; Notify:True),
      (Address:$1392; Desc:'i Invalid device'; Notify:True),
      (Address:$1392; Desc:'j Invalid baud rate'; Notify:True),
      (Address:$1392; Desc:'k Invalid note name'; Notify:True),
      (Address:$1392; Desc:'l Number too big'; Notify:True),
      (Address:$1392; Desc:'m Note out of range'; Notify:True),
      (Address:$1392; Desc:'n Out of range'; Notify:True),
      (Address:$1392; Desc:'o Too many tied notes'; Notify:True),
      (Address:$1392; Desc:'? Unknown error'; Notify:True));

Const

  ROMSymbols: Array[0..1116] of TSymbol =
    ((sType: 0; Desc:'START'; Address: $0000), (sType: 0; Desc:'ERROR-1'; Address: $0008), (sType: 0; Desc:'PRINT-A'; Address: $0010), (sType: 0; Desc:'GET-CHAR'; Address: $0018), (sType: 0; Desc:'TEST-CHAR'; Address: $001C),
     (sType: 0; Desc:'NEXT-CHAR'; Address: $0020), (sType: 0; Desc:'FP-CALC'; Address: $0028), (sType: 0; Desc:'BC-SPACES'; Address: $0030), (sType: 0; Desc:'MASK-INT'; Address: $0038), (sType: 0; Desc:'KEY-INT'; Address: $0048),
     (sType: 0; Desc:'ERROR-2'; Address: $0053), (sType: 0; Desc:'ERROR-3'; Address: $0055), (sType: 0; Desc:'RESET'; Address: $0066), (sType: 0; Desc:'NO-RESET'; Address: $0070), (sType: 0; Desc:'CH-ADD+1'; Address: $0074),
     (sType: 0; Desc:'TEMP-PTR1'; Address: $0077), (sType: 0; Desc:'TEMP-PTR2'; Address: $0078), (sType: 0; Desc:'SKIP-OVER'; Address: $007D), (sType: 0; Desc:'SKIPS'; Address: $0090), (sType: 0; Desc:'TKN-TABLE'; Address: $0095),
     (sType: 0; Desc:'MAIN-KEYS'; Address: $0205), (sType: 0; Desc:'E-UNSHIFT'; Address: $022C), (sType: 0; Desc:'EXT-SHIFT'; Address: $0246), (sType: 0; Desc:'CTL-CODES'; Address: $0260), (sType: 0; Desc:'SYM-CODES'; Address: $026A),
     (sType: 0; Desc:'E-DIGITS'; Address: $0284), (sType: 0; Desc:'KEY-SCAN'; Address: $028E), (sType: 0; Desc:'KEY-LINE'; Address: $0296), (sType: 0; Desc:'KEY-3KEYS'; Address: $029F), (sType: 0; Desc:'KEY-BITS'; Address: $02A1),
     (sType: 0; Desc:'KEY-DONE'; Address: $02AB), (sType: 0; Desc:'KEYBOARD'; Address: $02BF), (sType: 0; Desc:'K-ST-LOOP'; Address: $02C6), (sType: 0; Desc:'K-CH-SET'; Address: $02D1), (sType: 0; Desc:'K-NEW'; Address: $02F1),
     (sType: 0; Desc:'K-END'; Address: $0308), (sType: 0; Desc:'K-REPEAT'; Address: $0310), (sType: 0; Desc:'K-TEST'; Address: $031E), (sType: 0; Desc:'K-MAIN'; Address: $032C), (sType: 0; Desc:'K-DECODE'; Address: $0333),
     (sType: 0; Desc:'K-E-LET'; Address: $0341), (sType: 0; Desc:'K-LOOK-UP'; Address: $034A), (sType: 0; Desc:'K-KLC-LET'; Address: $034F), (sType: 0; Desc:'K-TOKENS'; Address: $0364), (sType: 0; Desc:'K-DIGIT'; Address: $0367),
     (sType: 0; Desc:'K-8-&-9'; Address: $0382), (sType: 0; Desc:'K-GRA-DGT'; Address: $0389), (sType: 0; Desc:'K-KLC-DGT'; Address: $039D), (sType: 0; Desc:'K-@-CHAR'; Address: $03B2), (sType: 0; Desc:'BEEPER'; Address: $03B5),
     (sType: 0; Desc:'BE-IX+3'; Address: $03D1), (sType: 0; Desc:'BE-IX+2'; Address: $03D2), (sType: 0; Desc:'BE-IX+1'; Address: $03D3), (sType: 0; Desc:'BE-IX+0'; Address: $03D4), (sType: 0; Desc:'BE-H&L-LP'; Address: $03D6),
     (sType: 0; Desc:'BE-AGAIN'; Address: $03F2), (sType: 0; Desc:'BE-END'; Address: $03F6), (sType: 0; Desc:'beep'; Address: $03F8), (sType: 0; Desc:'BE-I-OK'; Address: $0425), (sType: 0; Desc:'BE-OCTAVE'; Address: $0427),
     (sType: 0; Desc:'REPORT-B'; Address: $046C), (sType: 0; Desc:'semi-tone'; Address: $046E), (sType: 0; Desc:'zx81-name'; Address: $04AA), (sType: 0; Desc:'SA-BYTES'; Address: $04C2), (sType: 0; Desc:'SA-FLAG'; Address: $04D0),
     (sType: 0; Desc:'SA-LEADER'; Address: $04D8), (sType: 0; Desc:'SA-SYNC-1'; Address: $04EA), (sType: 0; Desc:'SA-SYNC-2'; Address: $04F2), (sType: 0; Desc:'SA-LOOP'; Address: $04FE), (sType: 0; Desc:'SA-LOOP-P'; Address: $0505),
     (sType: 0; Desc:'SA-START'; Address: $0507), (sType: 0; Desc:'SA-PARITY'; Address: $050E), (sType: 0; Desc:'SA-BIT-2'; Address: $0511), (sType: 0; Desc:'SA-BIT-1'; Address: $0514), (sType: 0; Desc:'SA-SET'; Address: $051A),
     (sType: 0; Desc:'SA-OUT'; Address: $051C), (sType: 0; Desc:'SA-8-BITS'; Address: $0525), (sType: 0; Desc:'SA-DELAY'; Address: $053C), (sType: 0; Desc:'SA/LD-RET'; Address: $053F), (sType: 0; Desc:'REPORT-Da'; Address: $0552),
     (sType: 0; Desc:'SA/LD-END'; Address: $0554), (sType: 0; Desc:'LD-BYTES'; Address: $0556), (sType: 0; Desc:'LD-BREAK'; Address: $056B), (sType: 0; Desc:'LD-START'; Address: $056C), (sType: 0; Desc:'LD-WAIT'; Address: $0574),
     (sType: 0; Desc:'LD-LEADER'; Address: $0580), (sType: 0; Desc:'LD-SYNC'; Address: $058F), (sType: 0; Desc:'LD-LOOP'; Address: $05A9), (sType: 0; Desc:'LD-FLAG'; Address: $05B3), (sType: 0; Desc:'LD-VERIFY'; Address: $05BD),
     (sType: 0; Desc:'LD-NEXT'; Address: $05C2), (sType: 0; Desc:'LD-DEC'; Address: $05C4), (sType: 0; Desc:'LD-MARKER'; Address: $05C8), (sType: 0; Desc:'LD-8-BITS'; Address: $05CA), (sType: 0; Desc:'LD-EDGE-2'; Address: $05E3),
     (sType: 0; Desc:'LD-EDGE-1'; Address: $05E7), (sType: 0; Desc:'LD-DELAY'; Address: $05E9), (sType: 0; Desc:'LD-SAMPLE'; Address: $05ED), (sType: 0; Desc:'SAVE-ETC'; Address: $0605), (sType: 0; Desc:'SA-SPACE'; Address: $0621),
     (sType: 0; Desc:'SA-BLANK'; Address: $0629), (sType: 0; Desc:'REPORT-Fa'; Address: $0642), (sType: 0; Desc:'SA-NULL'; Address: $0644), (sType: 0; Desc:'SA-NAME'; Address: $064B), (sType: 0; Desc:'SA-DATA'; Address: $0652),
     (sType: 0; Desc:'REPORT-2a'; Address: $0670), (sType: 0; Desc:'SA-V-OLD'; Address: $0672), (sType: 0; Desc:'SA-V-NEW'; Address: $0685), (sType: 0; Desc:'SA-V-TYPE'; Address: $068F), (sType: 0; Desc:'SA-DATA-1'; Address: $0692),
     (sType: 0; Desc:'SA-SCR$'; Address: $06A0), (sType: 0; Desc:'SA-CODE'; Address: $06C3), (sType: 0; Desc:'SA-CODE-1'; Address: $06E1), (sType: 0; Desc:'SA-CODE-2'; Address: $06F0), (sType: 0; Desc:'SA-CODE-3'; Address: $06F5),
     (sType: 0; Desc:'SA-CODE-4'; Address: $06F9), (sType: 0; Desc:'SA-TYPE-3'; Address: $0710), (sType: 0; Desc:'SA-LINE'; Address: $0716), (sType: 0; Desc:'SA-LINE-1'; Address: $0723), (sType: 0; Desc:'SA-TYPE-0'; Address: $073A),
     (sType: 0; Desc:'SA-ALL'; Address: $075A), (sType: 0; Desc:'LD-LOOK-H'; Address: $0767), (sType: 0; Desc:'LD-TYPE'; Address: $078A), (sType: 0; Desc:'LD-NAME'; Address: $07A6), (sType: 0; Desc:'LD-CH-PR'; Address: $07AD),
     (sType: 0; Desc:'VR-CONTRL'; Address: $07CB), (sType: 0; Desc:'VR-CONT-1'; Address: $07E9), (sType: 0; Desc:'VR-CONT-2'; Address: $07F4), (sType: 0; Desc:'VR-CONT-3'; Address: $0800), (sType: 0; Desc:'LD-BLOCK'; Address: $0802),
     (sType: 0; Desc:'REPORT-R'; Address: $0806), (sType: 0; Desc:'LD-CONTRL'; Address: $0808), (sType: 0; Desc:'LD-CONT-1'; Address: $0819), (sType: 0; Desc:'LD-CONT-2'; Address: $0825), (sType: 0; Desc:'LD-DATA'; Address: $082E),
     (sType: 0; Desc:'LD-DATA-1'; Address: $084C), (sType: 0; Desc:'LD-PROG'; Address: $0873), (sType: 0; Desc:'LD-PROG-1'; Address: $08AD), (sType: 0; Desc:'ME-CONTRL'; Address: $08B6), (sType: 0; Desc:'ME-NEW-LP'; Address: $08D2),
     (sType: 0; Desc:'ME-OLD-LP'; Address: $08D7), (sType: 0; Desc:'ME-OLD-L1'; Address: $08DF), (sType: 0; Desc:'ME-NEW-L2'; Address: $08EB), (sType: 0; Desc:'ME-VAR-LP'; Address: $08F0), (sType: 0; Desc:'ME-OLD-VP'; Address: $08F9),
     (sType: 0; Desc:'ME-OLD-V1'; Address: $0901), (sType: 0; Desc:'ME-OLD-V2'; Address: $0909), (sType: 0; Desc:'ME-OLD-V3'; Address: $0912), (sType: 0; Desc:'ME-OLD-V4'; Address: $091E), (sType: 0; Desc:'ME-VAR-L1'; Address: $0921),
     (sType: 0; Desc:'ME-VAR-L2'; Address: $0923), (sType: 0; Desc:'ME-ENTER'; Address: $092C), (sType: 0; Desc:'ME-ENT-1'; Address: $093E), (sType: 0; Desc:'ME-ENT-2'; Address: $0955), (sType: 0; Desc:'ME-ENT-3'; Address: $0958),
     (sType: 0; Desc:'SA-CONTRL'; Address: $0970), (sType: 0; Desc:'SA-1-SEC'; Address: $0991), (sType: 0; Desc:'tape-msgs'; Address: $09A1), (sType: 0; Desc:'PRINT-OUT'; Address: $09F4), (sType: 0; Desc:'ctlchrtab'; Address: $0A11),
     (sType: 0; Desc:'PO-BACK-1'; Address: $0A23), (sType: 0; Desc:'PO-BACK-2'; Address: $0A38), (sType: 0; Desc:'PO-BACK-3'; Address: $0A3A), (sType: 0; Desc:'PO-RIGHT'; Address: $0A3D), (sType: 0; Desc:'PO-ENTER'; Address: $0A4F),
     (sType: 0; Desc:'PO-COMMA'; Address: $0A5F), (sType: 0; Desc:'PO-QUEST'; Address: $0A69), (sType: 0; Desc:'PO-TV-2'; Address: $0A6D), (sType: 0; Desc:'PO-2-OPER'; Address: $0A75), (sType: 0; Desc:'PO-1-OPER'; Address: $0A7A),
     (sType: 0; Desc:'PO-TV-1'; Address: $0A7D), (sType: 0; Desc:'PO-CHANGE'; Address: $0A80), (sType: 0; Desc:'PO-CONT'; Address: $0A87), (sType: 0; Desc:'PO-AT-ERR'; Address: $0AAC), (sType: 0; Desc:'PO-AT-SET'; Address: $0ABF),
     (sType: 0; Desc:'PO-TAB'; Address: $0AC2), (sType: 0; Desc:'PO-FILL'; Address: $0AC3), (sType: 0; Desc:'PO-SPACE'; Address: $0AD0), (sType: 0; Desc:'PO-ABLE'; Address: $0AD9), (sType: 0; Desc:'PO-STORE'; Address: $0ADC),
     (sType: 0; Desc:'PO-ST-E'; Address: $0AF0), (sType: 0; Desc:'PO-ST-PR'; Address: $0AFC), (sType: 0; Desc:'PO-FETCH'; Address: $0B03), (sType: 0; Desc:'PO-F-PR'; Address: $0B1D), (sType: 0; Desc:'PO-ANY'; Address: $0B24),
     (sType: 0; Desc:'PO-GR-1'; Address: $0B38), (sType: 0; Desc:'PO-GR-2'; Address: $0B3E), (sType: 0; Desc:'PO-GR-3'; Address: $0B4C), (sType: 0; Desc:'PO-T&UDG'; Address: $0B52), (sType: 0; Desc:'PO-T'; Address: $0B5F),
     (sType: 0; Desc:'PO-CHAR'; Address: $0B65), (sType: 0; Desc:'PO-CHAR-2'; Address: $0B6A), (sType: 0; Desc:'PO-CHAR-3'; Address: $0B76), (sType: 0; Desc:'PR-ALL'; Address: $0B7F), (sType: 0; Desc:'PR-ALL-1'; Address: $0B93),
     (sType: 0; Desc:'PR-ALL-2'; Address: $0BA4), (sType: 0; Desc:'PR-ALL-3'; Address: $0BB6), (sType: 0; Desc:'PR-ALL-4'; Address: $0BB7), (sType: 0; Desc:'PR-ALL-5'; Address: $0BC1), (sType: 0; Desc:'PR-ALL-6'; Address: $0BD3),
     (sType: 0; Desc:'PO-ATTR'; Address: $0BDB), (sType: 0; Desc:'PO-ATTR-1'; Address: $0BFA), (sType: 0; Desc:'PO-ATTR-2'; Address: $0C08), (sType: 0; Desc:'PO-MSG'; Address: $0C0A), (sType: 0; Desc:'PO-TOKENS'; Address: $0C10),
     (sType: 0; Desc:'PO-TABLE'; Address: $0C14), (sType: 0; Desc:'PO-EACH'; Address: $0C22), (sType: 0; Desc:'PO-TR-SP'; Address: $0C35), (sType: 0; Desc:'PO-SAVE'; Address: $0C3B), (sType: 0; Desc:'PO-SEARCH'; Address: $0C41),
     (sType: 0; Desc:'PO-STEP'; Address: $0C44), (sType: 0; Desc:'PO-SCR'; Address: $0C55), (sType: 0; Desc:'REPORT-5'; Address: $0C86), (sType: 0; Desc:'PO-SCR-2'; Address: $0C88), (sType: 0; Desc:'PO-SCR-3'; Address: $0CD2),
     (sType: 0; Desc:'PO-SCR-3A'; Address: $0CF0), (sType: 0; Desc:'scrl-mssg'; Address: $0CF8), (sType: 0; Desc:'REPORT-D'; Address: $0D00), (sType: 0; Desc:'PO-SCR-4'; Address: $0D02), (sType: 0; Desc:'PO-SCR-4A'; Address: $0D1C),
     (sType: 0; Desc:'PO-SCR-4B'; Address: $0D2D), (sType: 0; Desc:'TEMPS'; Address: $0D4D), (sType: 0; Desc:'TEMPS-1'; Address: $0D5B), (sType: 0; Desc:'TEMPS-2'; Address: $0D65), (sType: 0; Desc:'CLS'; Address: $0D6B),
     (sType: 0; Desc:'CLS-LOWER'; Address: $0D6E), (sType: 0; Desc:'CLS-1'; Address: $0D87), (sType: 0; Desc:'CLS-2'; Address: $0D89), (sType: 0; Desc:'CLS-3'; Address: $0D8E), (sType: 0; Desc:'CL-CHAN'; Address: $0D94),
     (sType: 0; Desc:'CL-CHAN-A'; Address: $0DA0), (sType: 0; Desc:'CL-ALL'; Address: $0DAF), (sType: 0; Desc:'CL-SET'; Address: $0DD9), (sType: 0; Desc:'CL-SET-1'; Address: $0DEE), (sType: 0; Desc:'CL-SET-2'; Address: $0DF4),
     (sType: 0; Desc:'CL-SC-ALL'; Address: $0DFE), (sType: 0; Desc:'CL-SCROLL'; Address: $0E00), (sType: 0; Desc:'CL-SCR-1'; Address: $0E05), (sType: 0; Desc:'CL-SCR-2'; Address: $0E0D), (sType: 0; Desc:'CL-SCR-3'; Address: $0E19),
     (sType: 0; Desc:'CL-LINE'; Address: $0E44), (sType: 0; Desc:'CL-LINE-1'; Address: $0E4A), (sType: 0; Desc:'CL-LINE-2'; Address: $0E4D), (sType: 0; Desc:'CL-LINE-3'; Address: $0E80), (sType: 0; Desc:'CL-ATTR'; Address: $0E88),
     (sType: 0; Desc:'CL-ADDR'; Address: $0E9B), (sType: 0; Desc:'COPY'; Address: $0EAC), (sType: 0; Desc:'COPY-1'; Address: $0EB2), (sType: 0; Desc:'COPY-2'; Address: $0EC9), (sType: 0; Desc:'COPY-BUFF'; Address: $0ECD),
     (sType: 0; Desc:'COPY-3'; Address: $0ED3), (sType: 0; Desc:'COPY-END'; Address: $0EDA), (sType: 0; Desc:'CLEAR-PRB'; Address: $0EDF), (sType: 0; Desc:'PRB-BYTES'; Address: $0EE7), (sType: 0; Desc:'COPY-LINE'; Address: $0EF4),
     (sType: 0; Desc:'COPY-L-1'; Address: $0EFD), (sType: 0; Desc:'REPORT-Dc'; Address: $0F0A), (sType: 0; Desc:'COPY-L-2'; Address: $0F0C), (sType: 0; Desc:'COPY-L-3'; Address: $0F14), (sType: 0; Desc:'COPY-L-4'; Address: $0F18),
     (sType: 0; Desc:'COPY-L-5'; Address: $0F1E), (sType: 0; Desc:'EDITOR'; Address: $0F2C), (sType: 0; Desc:'ED-AGAIN'; Address: $0F30), (sType: 0; Desc:'ED-LOOP'; Address: $0F38), (sType: 0; Desc:'ED-CONTR'; Address: $0F6C),
     (sType: 0; Desc:'ADD-CHAR'; Address: $0F81), (sType: 0; Desc:'ADD-CH-1'; Address: $0F8B), (sType: 0; Desc:'ED-KEYS'; Address: $0F92), (sType: 0; Desc:'ed-keys-t'; Address: $0FA0), (sType: 0; Desc:'ED-EDIT'; Address: $0FA9),
     (sType: 0; Desc:'ED-DOWN'; Address: $0FF3), (sType: 0; Desc:'ED-STOP'; Address: $1001), (sType: 0; Desc:'ED-LEFT'; Address: $1007), (sType: 0; Desc:'ED-RIGHT'; Address: $100C), (sType: 0; Desc:'ED-CUR'; Address: $1011),
     (sType: 0; Desc:'ED-DELETE'; Address: $1015), (sType: 0; Desc:'ED-IGNORE'; Address: $101E), (sType: 0; Desc:'ED-ENTER'; Address: $1024), (sType: 0; Desc:'ED-END'; Address: $1026), (sType: 0; Desc:'ED-EDGE'; Address: $1031),
     (sType: 0; Desc:'ED-EDGE-1'; Address: $103E), (sType: 0; Desc:'ED-EDGE-2'; Address: $1051), (sType: 0; Desc:'ED-UP'; Address: $1059), (sType: 0; Desc:'ED-LIST'; Address: $106E), (sType: 0; Desc:'ED-SYMBOL'; Address: $1076),
     (sType: 0; Desc:'ED-GRAPH'; Address: $107C), (sType: 0; Desc:'ED-ERROR'; Address: $107F), (sType: 0; Desc:'CLEAR-SP'; Address: $1097), (sType: 0; Desc:'KEY-INPUT'; Address: $10A8), (sType: 0; Desc:'KEY-M-CL'; Address: $10DB),
     (sType: 0; Desc:'KEY-MODE'; Address: $10E6), (sType: 0; Desc:'KEY-FLAG'; Address: $10F4), (sType: 0; Desc:'KEY-CONTR'; Address: $10FA), (sType: 0; Desc:'KEY-DATA'; Address: $1105), (sType: 0; Desc:'KEY-NEXT'; Address: $110D),
     (sType: 0; Desc:'KEY-CHAN'; Address: $1113), (sType: 0; Desc:'KEY-DONE2'; Address: $111B), (sType: 0; Desc:'ED-COPY'; Address: $111D), (sType: 0; Desc:'ED-BLANK'; Address: $1150), (sType: 0; Desc:'ED-SPACES'; Address: $115E),
     (sType: 0; Desc:'ED-FULL'; Address: $1167), (sType: 0; Desc:'ED-C-DONE'; Address: $117C), (sType: 0; Desc:'ED-C-END'; Address: $117E), (sType: 0; Desc:'SET-HL'; Address: $1190), (sType: 0; Desc:'SET-DE'; Address: $1195),
     (sType: 0; Desc:'REMOVE-FP'; Address: $11A7), (sType: 0; Desc:'NEW'; Address: $11B7), (sType: 0; Desc:'START-NEW'; Address: $11CB), (sType: 0; Desc:'ram-check'; Address: $11DA), (sType: 0; Desc:'RAM-FILL'; Address: $11DC),
     (sType: 0; Desc:'RAM-READ'; Address: $11E2), (sType: 0; Desc:'RAM-DONE'; Address: $11EF), (sType: 0; Desc:'RAM-SET'; Address: $1219), (sType: 0; Desc:'NMI_VECT'; Address: $121C), (sType: 0; Desc:'MAIN-EXEC'; Address: $12A2),
     (sType: 0; Desc:'MAIN-1'; Address: $12A9), (sType: 0; Desc:'MAIN-2'; Address: $12AC), (sType: 0; Desc:'MAIN-3'; Address: $12CF), (sType: 0; Desc:'MAIN-4'; Address: $1303), (sType: 0; Desc:'MAIN-G'; Address: $1313),
     (sType: 0; Desc:'MAIN-5'; Address: $133C), (sType: 0; Desc:'MAIN-6'; Address: $1373), (sType: 0; Desc:'MAIN-7'; Address: $1376), (sType: 0; Desc:'MAIN-8'; Address: $1384), (sType: 0; Desc:'MAIN-9'; Address: $1386),
     (sType: 0; Desc:'rpt-mesgs'; Address: $1391), (sType: 0; Desc:'comma-sp   '; Address: $1537), (sType: 0; Desc:'copyright'; Address: $1539), (sType: 0; Desc:'REPORT-G'; Address: $1555), (sType: 0; Desc:'MAIN-ADD'; Address: $155D),
     (sType: 0; Desc:'MAIN-ADD1'; Address: $157D), (sType: 0; Desc:'MAIN-ADD2'; Address: $15AB), (sType: 0; Desc:'init-chan'; Address: $15AF), (sType: 0; Desc:'REPORT-J'; Address: $15C4), (sType: 0; Desc:'init-strm'; Address: $15C6),
     (sType: 0; Desc:'WAIT-KEY'; Address: $15D4), (sType: 0; Desc:'WAIT-KEY1'; Address: $15DE), (sType: 0; Desc:'REPORT-8'; Address: $15E4), (sType: 0; Desc:'INPUT-AD'; Address: $15E6), (sType: 0; Desc:'OUT-CODE'; Address: $15EF),
     (sType: 0; Desc:'PRINT-A-2'; Address: $15F2), (sType: 0; Desc:'CALL-SUB'; Address: $15F7), (sType: 0; Desc:'CHAN-OPEN'; Address: $1601), (sType: 0; Desc:'REPORT-Oa'; Address: $160E), (sType: 0; Desc:'CHAN-OP-1'; Address: $1610),
     (sType: 0; Desc:'CHAN-FLAG'; Address: $1615), (sType: 0; Desc:'CALL-JUMP'; Address: $162C), (sType: 0; Desc:'chn-cd-lu'; Address: $162D), (sType: 0; Desc:'CHAN-K'; Address: $1634), (sType: 0; Desc:'CHAN-S'; Address: $1642),
     (sType: 0; Desc:'CHAN-S-1'; Address: $1646), (sType: 0; Desc:'CHAN-P'; Address: $164D), (sType: 0; Desc:'ONE-SPACE'; Address: $1652), (sType: 0; Desc:'MAKE-ROOM'; Address: $1655), (sType: 0; Desc:'POINTERS'; Address: $1664),
     (sType: 0; Desc:'PTR-NEXT'; Address: $166B), (sType: 0; Desc:'PTR-DONE'; Address: $167F), (sType: 0; Desc:'LINE-ZERO'; Address: $168F), (sType: 0; Desc:'LINE-NO-A'; Address: $1691), (sType: 0; Desc:'LINE-NO'; Address: $1695),
     (sType: 0; Desc:'RESERVE'; Address: $169E), (sType: 0; Desc:'SET-MIN'; Address: $16B0), (sType: 0; Desc:'SET-WORK'; Address: $16BF), (sType: 0; Desc:'SET-STK'; Address: $16C5), (sType: 0; Desc:'REC-EDIT'; Address: $16D4),
     (sType: 0; Desc:'INDEXER-1'; Address: $16DB), (sType: 0; Desc:'INDEXER'; Address: $16DC), (sType: 0; Desc:'CLOSE'; Address: $16E5), (sType: 0; Desc:'CLOSE-1'; Address: $16FC), (sType: 0; Desc:'CLOSE-2'; Address: $1701),
     (sType: 0; Desc:'ROM_TRAP'; Address: $1708), (sType: 0; Desc:'cl-str-lu'; Address: $1716), (sType: 0; Desc:'CLOSE-STR'; Address: $171C), (sType: 0; Desc:'STR-DATA'; Address: $171E), (sType: 0; Desc:'REPORT-Ob'; Address: $1725),
     (sType: 0; Desc:'STR-DATA1'; Address: $1727), (sType: 0; Desc:'OPEN'; Address: $1736), (sType: 0; Desc:'OPEN-1'; Address: $1756), (sType: 0; Desc:'OPEN-2'; Address: $175D), (sType: 0; Desc:'REPORT-Fb'; Address: $1765),
     (sType: 0; Desc:'OPEN-3'; Address: $1767), (sType: 0; Desc:'op-str-lu'; Address: $177A), (sType: 0; Desc:'OPEN-K'; Address: $1781), (sType: 0; Desc:'OPEN-S'; Address: $1785), (sType: 0; Desc:'OPEN-P'; Address: $1789),
     (sType: 0; Desc:'OPEN-END'; Address: $178B), (sType: 0; Desc:'CAT-ETC'; Address: $1793), (sType: 0; Desc:'AUTO-LIST'; Address: $1795), (sType: 0; Desc:'AUTO-L-1'; Address: $17CE), (sType: 0; Desc:'AUTO-L-2'; Address: $17E1),
     (sType: 0; Desc:'AUTO-L-3'; Address: $17E4), (sType: 0; Desc:'AUTO-L-4'; Address: $17ED), (sType: 0; Desc:'LLIST'; Address: $17F5), (sType: 0; Desc:'LIST'; Address: $17F9), (sType: 0; Desc:'LIST-1'; Address: $17FB),
     (sType: 0; Desc:'LIST-2'; Address: $1814), (sType: 0; Desc:'LIST-3'; Address: $181A), (sType: 0; Desc:'LIST-4'; Address: $181F), (sType: 0; Desc:'LIST-5'; Address: $1822), (sType: 0; Desc:'LIST-ALL'; Address: $1833),
     (sType: 0; Desc:'LIST-ALL-2'; Address: $1835), (sType: 0; Desc:'OUT-LINE'; Address: $1855), (sType: 0; Desc:'OUT-LINE1'; Address: $1865), (sType: 0; Desc:'OUT-LINE2'; Address: $187D), (sType: 0; Desc:'OUT-LINE3'; Address: $1881),
     (sType: 0; Desc:'OUT-LINE4'; Address: $1894), (sType: 0; Desc:'OUT-LINE5'; Address: $18A1), (sType: 0; Desc:'OUT-LINE6'; Address: $18B4), (sType: 0; Desc:'NUMBER'; Address: $18B6), (sType: 0; Desc:'OUT-FLASH'; Address: $18C1),
     (sType: 0; Desc:'OUT-CURS'; Address: $18E1), (sType: 0; Desc:'OUT-C-1'; Address: $18F3), (sType: 0; Desc:'OUT-C-2'; Address: $1909), (sType: 0; Desc:'LN-FETCH'; Address: $190F), (sType: 0; Desc:'LN-STORE'; Address: $191C),
     (sType: 0; Desc:'OUT-SP-2'; Address: $1925), (sType: 0; Desc:'OUT-SP-NO'; Address: $192A), (sType: 0; Desc:'OUT-SP-1'; Address: $192B), (sType: 0; Desc:'OUT-CHAR'; Address: $1937), (sType: 0; Desc:'OUT-CH-1'; Address: $195A),
     (sType: 0; Desc:'OUT-CH-2'; Address: $1968), (sType: 0; Desc:'OUT-CH-3'; Address: $196C), (sType: 0; Desc:'LINE-ADDR'; Address: $196E), (sType: 0; Desc:'LINE-AD-1'; Address: $1974), (sType: 0; Desc:'CP-LINES'; Address: $1980),
     (sType: 0; Desc:'not-used'; Address: $1988), (sType: 0; Desc:'EACH-STMT'; Address: $198B), (sType: 0; Desc:'EACH-S-1'; Address: $1990), (sType: 0; Desc:'EACH-S-2'; Address: $1998), (sType: 0; Desc:'EACH-S-3'; Address: $199A),
     (sType: 0; Desc:'EACH-S-4'; Address: $19A5), (sType: 0; Desc:'EACH-S-5'; Address: $19AD), (sType: 0; Desc:'EACH-S-6'; Address: $19B1), (sType: 0; Desc:'NEXT-ONE'; Address: $19B8), (sType: 0; Desc:'NEXT-O-1'; Address: $19C7),
     (sType: 0; Desc:'NEXT-O-2'; Address: $19CE), (sType: 0; Desc:'NEXT-O-3'; Address: $19D5), (sType: 0; Desc:'NEXT-O-4'; Address: $19D6), (sType: 0; Desc:'NEXT-O-5'; Address: $19DB), (sType: 0; Desc:'DIFFER'; Address: $19DD),
     (sType: 0; Desc:'RECLAIM-1'; Address: $19E5), (sType: 0; Desc:'RECLAIM-2'; Address: $19E8), (sType: 0; Desc:'E-LINE-NO'; Address: $19FB), (sType: 0; Desc:'E-L-1'; Address: $1A15), (sType: 0; Desc:'OUT-NUM-1'; Address: $1A1B),
     (sType: 0; Desc:'OUT-NUM-2'; Address: $1A28), (sType: 0; Desc:'OUT-NUM-3'; Address: $1A30), (sType: 0; Desc:'OUT-NUM-4'; Address: $1A42), (sType: 0; Desc:'offst-tbl'; Address: $1A48), (sType: 0; Desc:'P-LET'; Address: $1A7A),
     (sType: 0; Desc:'P-GO-TO'; Address: $1A7D), (sType: 0; Desc:'P-IF'; Address: $1A81), (sType: 0; Desc:'P-GO-SUB'; Address: $1A86), (sType: 0; Desc:'P-STOP'; Address: $1A8A), (sType: 0; Desc:'P-RETURN'; Address: $1A8D),
     (sType: 0; Desc:'P-FOR'; Address: $1A90), (sType: 0; Desc:'P-NEXT'; Address: $1A98), (sType: 0; Desc:'P-PRINT'; Address: $1A9C), (sType: 0; Desc:'P-INPUT'; Address: $1A9F), (sType: 0; Desc:'P-DIM'; Address: $1AA2),
     (sType: 0; Desc:'P-REM'; Address: $1AA5), (sType: 0; Desc:'P-NEW'; Address: $1AA8), (sType: 0; Desc:'P-RUN'; Address: $1AAB), (sType: 0; Desc:'P-LIST'; Address: $1AAE), (sType: 0; Desc:'P-POKE'; Address: $1AB1),
     (sType: 0; Desc:'P-RANDOM'; Address: $1AB5), (sType: 0; Desc:'P-CONT'; Address: $1AB8), (sType: 0; Desc:'P-CLEAR'; Address: $1ABB), (sType: 0; Desc:'P-CLS'; Address: $1ABE), (sType: 0; Desc:'P-PLOT'; Address: $1AC1),
     (sType: 0; Desc:'P-PAUSE'; Address: $1AC5), (sType: 0; Desc:'P-READ'; Address: $1AC9), (sType: 0; Desc:'P-DATA'; Address: $1ACC), (sType: 0; Desc:'P-RESTORE'; Address: $1ACF), (sType: 0; Desc:'P-DRAW'; Address: $1AD2),
     (sType: 0; Desc:'P-COPY'; Address: $1AD6), (sType: 0; Desc:'P-LPRINT'; Address: $1AD9), (sType: 0; Desc:'P-LLIST'; Address: $1ADC), (sType: 0; Desc:'P-SAVE'; Address: $1ADF), (sType: 0; Desc:'P-LOAD'; Address: $1AE0),
     (sType: 0; Desc:'P-VERIFY'; Address: $1AE1), (sType: 0; Desc:'P-MERGE'; Address: $1AE2), (sType: 0; Desc:'P-BEEP'; Address: $1AE3), (sType: 0; Desc:'P-CIRCLE'; Address: $1AE7), (sType: 0; Desc:'P-INK'; Address: $1AEB),
     (sType: 0; Desc:'P-PAPER'; Address: $1AEC), (sType: 0; Desc:'P-FLASH'; Address: $1AED), (sType: 0; Desc:'P-BRIGHT'; Address: $1AEE), (sType: 0; Desc:'P-INVERSE'; Address: $1AEF), (sType: 0; Desc:'P-OVER'; Address: $1AF0),
     (sType: 0; Desc:'P-OUT'; Address: $1AF1), (sType: 0; Desc:'P-BORDER'; Address: $1AF5), (sType: 0; Desc:'P-DEF-FN'; Address: $1AF9), (sType: 0; Desc:'P-OPEN'; Address: $1AFC), (sType: 0; Desc:'P-CLOSE'; Address: $1B02),
     (sType: 0; Desc:'P-FORMAT'; Address: $1B06), (sType: 0; Desc:'P-MOVE'; Address: $1B0A), (sType: 0; Desc:'P-ERASE'; Address: $1B10), (sType: 0; Desc:'P-CAT'; Address: $1B14), (sType: 0; Desc:'LINE-SCAN'; Address: $1B17),
     (sType: 0; Desc:'STMT-LOOP'; Address: $1B28), (sType: 0; Desc:'STMT-L-1'; Address: $1B29), (sType: 0; Desc:'SCAN-LOOP'; Address: $1B52), (sType: 0; Desc:'GET-PARAM'; Address: $1B55), (sType: 0; Desc:'SEPARATOR'; Address: $1B6F),
     (sType: 0; Desc:'STMT-RET'; Address: $1B76), (sType: 0; Desc:'REPORT-L'; Address: $1B7B), (sType: 0; Desc:'STMT-R-1'; Address: $1B7D), (sType: 0; Desc:'LINE-RUN'; Address: $1B8A), (sType: 0; Desc:'LINE-NEW'; Address: $1B9E),
     (sType: 0; Desc:'REPORT-0'; Address: $1BB0), (sType: 0; Desc:'REM'; Address: $1BB2), (sType: 0; Desc:'LINE-END'; Address: $1BB3), (sType: 0; Desc:'LINE-USE'; Address: $1BBF), (sType: 0; Desc:'NEXT-LINE'; Address: $1BD1),
     (sType: 0; Desc:'REPORT-N'; Address: $1BEC), (sType: 0; Desc:'CHECK-END'; Address: $1BEE), (sType: 0; Desc:'STMT-NEXT'; Address: $1BF4), (sType: 0; Desc:'class-tbl'; Address: $1C01), (sType: 0; Desc:'CLASS-03'; Address: $1C0D),
     (sType: 0; Desc:'CLASS-00'; Address: $1C10), (sType: 0; Desc:'CLASS-05'; Address: $1C11), (sType: 0; Desc:'CLASS-01'; Address: $1C1F), (sType: 0; Desc:'VAR-A-1'; Address: $1C22), (sType: 0; Desc:'REPORT-2'; Address: $1C2E),
     (sType: 0; Desc:'VAR-A-2'; Address: $1C30), (sType: 0; Desc:'VAR-A-3'; Address: $1C46), (sType: 0; Desc:'CLASS-02'; Address: $1C4E), (sType: 0; Desc:'VAL-FET-1'; Address: $1C56), (sType: 0; Desc:'VAL-FET-2'; Address: $1C59),
     (sType: 0; Desc:'CLASS-04'; Address: $1C6C), (sType: 0; Desc:'NEXT-2NUM'; Address: $1C79), (sType: 0; Desc:'EXPT-2NUM'; Address: $1C7A), (sType: 0; Desc:'EXPT-1NUM'; Address: $1C82), (sType: 0; Desc:'REPORT-C'; Address: $1C8A),
     (sType: 0; Desc:'EXPT-EXP'; Address: $1C8C), (sType: 0; Desc:'CLASS-07'; Address: $1C96), (sType: 0; Desc:'CLASS-09'; Address: $1CBE), (sType: 0; Desc:'CL-09-1'; Address: $1CD6), (sType: 0; Desc:'CLASS-0B'; Address: $1CDB),
     (sType: 0; Desc:'FETCH-NUM'; Address: $1CDE), (sType: 0; Desc:'USE-ZERO'; Address: $1CE6), (sType: 0; Desc:'STOP'; Address: $1CEE), (sType: 0; Desc:'IF'; Address: $1CF0), (sType: 0; Desc:'IF-1'; Address: $1D00),
     (sType: 0; Desc:'FOR'; Address: $1D03), (sType: 0; Desc:'F-USE-1'; Address: $1D10), (sType: 0; Desc:'F-REORDER'; Address: $1D16), (sType: 0; Desc:'F-L-S'; Address: $1D34), (sType: 0; Desc:'F-LOOP'; Address: $1D64),
     (sType: 0; Desc:'F-FOUND'; Address: $1D7C), (sType: 0; Desc:'REPORT-I'; Address: $1D84), (sType: 0; Desc:'LOOK-PROG'; Address: $1D86), (sType: 0; Desc:'LOOK-P-1'; Address: $1D8B), (sType: 0; Desc:'LOOK-P-2'; Address: $1DA3),
     (sType: 0; Desc:'NEXT'; Address: $1DAB), (sType: 0; Desc:'REPORT-1'; Address: $1DD8), (sType: 0; Desc:'NEXT-LOOP'; Address: $1DDA), (sType: 0; Desc:'NEXT-1'; Address: $1DE2), (sType: 0; Desc:'NEXT-2'; Address: $1DE9),
     (sType: 0; Desc:'READ-3'; Address: $1DEC), (sType: 0; Desc:'READ'; Address: $1DED), (sType: 0; Desc:'REPORT-E'; Address: $1E08), (sType: 0; Desc:'READ-1'; Address: $1E0A), (sType: 0; Desc:'READ-2'; Address: $1E1E),
     (sType: 0; Desc:'DATA'; Address: $1E27), (sType: 0; Desc:'DATA-1'; Address: $1E2C), (sType: 0; Desc:'DATA-2'; Address: $1E37), (sType: 0; Desc:'PASS-BY'; Address: $1E39), (sType: 0; Desc:'RESTORE'; Address: $1E42),
     (sType: 0; Desc:'REST-RUN'; Address: $1E45), (sType: 0; Desc:'RANDOMIZE'; Address: $1E4F), (sType: 0; Desc:'RAND-1'; Address: $1E5A), (sType: 0; Desc:'CONTINUE'; Address: $1E5F), (sType: 0; Desc:'GO-TO'; Address: $1E67),
     (sType: 0; Desc:'GO-TO-2'; Address: $1E73), (sType: 0; Desc:'OUT'; Address: $1E7A), (sType: 0; Desc:'POKE'; Address: $1E80), (sType: 0; Desc:'TWO-PARAM'; Address: $1E85), (sType: 0; Desc:'TWO-P-1'; Address: $1E8E),
     (sType: 0; Desc:'FIND-INT1'; Address: $1E94), (sType: 0; Desc:'FIND-INT2'; Address: $1E99), (sType: 0; Desc:'FIND-I-1'; Address: $1E9C), (sType: 0; Desc:'REPORT-Bb'; Address: $1E9F), (sType: 0; Desc:'RUN'; Address: $1EA1),
     (sType: 0; Desc:'CLEAR'; Address: $1EAC), (sType: 0; Desc:'CLEAR-RUN'; Address: $1EAF), (sType: 0; Desc:'CLEAR-1'; Address: $1EB7), (sType: 0; Desc:'REPORT-M'; Address: $1EDA), (sType: 0; Desc:'CLEAR-2'; Address: $1EDC),
     (sType: 0; Desc:'GO-SUB'; Address: $1EED), (sType: 0; Desc:'TEST-ROOM'; Address: $1F05), (sType: 0; Desc:'REPORT-4'; Address: $1F15), (sType: 0; Desc:'free-mem'; Address: $1F1A), (sType: 0; Desc:'RETURN'; Address: $1F23),
     (sType: 0; Desc:'REPORT-7'; Address: $1F36), (sType: 0; Desc:'PAUSE'; Address: $1F3A), (sType: 0; Desc:'PAUSE-1'; Address: $1F3D), (sType: 0; Desc:'PAUSE-2'; Address: $1F49), (sType: 0; Desc:'PAUSE-END'; Address: $1F4F),
     (sType: 0; Desc:'BREAK-KEY'; Address: $1F54), (sType: 0; Desc:'DEF-FN'; Address: $1F60), (sType: 0; Desc:'DEF-FN-1'; Address: $1F6A), (sType: 0; Desc:'DEF-FN-2'; Address: $1F7D), (sType: 0; Desc:'DEF-FN-3'; Address: $1F86),
     (sType: 0; Desc:'DEF-FN-4'; Address: $1F89), (sType: 0; Desc:'DEF-FN-5'; Address: $1F94), (sType: 0; Desc:'DEF-FN-6'; Address: $1FA6), (sType: 0; Desc:'DEF-FN-7'; Address: $1FBD), (sType: 0; Desc:'UNSTACK-Z'; Address: $1FC3),
     (sType: 0; Desc:'LPRINT'; Address: $1FC9), (sType: 0; Desc:'PRINT'; Address: $1FCD), (sType: 0; Desc:'PRINT-1'; Address: $1FCF), (sType: 0; Desc:'PRINT-2'; Address: $1FDF), (sType: 0; Desc:'PRINT-3'; Address: $1FE5),
     (sType: 0; Desc:'PRINT-4'; Address: $1FF2), (sType: 0; Desc:'PRINT-CR'; Address: $1FF5), (sType: 0; Desc:'PR-ITEM-1'; Address: $1FFC), (sType: 0; Desc:'PR-ITEM-2'; Address: $200E), (sType: 0; Desc:'PR-AT-TAB'; Address: $201E),
     (sType: 0; Desc:'PR-ITEM-3'; Address: $2024), (sType: 0; Desc:'PR-STRING'; Address: $203C), (sType: 0; Desc:'PR-END-Z'; Address: $2045), (sType: 0; Desc:'PR-ST-END'; Address: $2048), (sType: 0; Desc:'PR-POSN-1'; Address: $204E),
     (sType: 0; Desc:'PR-POSN-2'; Address: $2061), (sType: 0; Desc:'PR-POSN-3'; Address: $2067), (sType: 0; Desc:'PR-POSN-4'; Address: $206E), (sType: 0; Desc:'STR-ALTER'; Address: $2070), (sType: 0; Desc:'INPUT'; Address: $2089),
     (sType: 0; Desc:'INPUT-1'; Address: $2096), (sType: 0; Desc:'INPUT-2'; Address: $20AD), (sType: 0; Desc:'IN-ITEM-1'; Address: $20C1), (sType: 0; Desc:'IN-ITEM-2'; Address: $20D8), (sType: 0; Desc:'IN-ITEM-3'; Address: $20ED),
     (sType: 0; Desc:'IN-PROMPT'; Address: $20FA), (sType: 0; Desc:'IN-PR-1'; Address: $211A), (sType: 0; Desc:'IN-PR-2'; Address: $211C), (sType: 0; Desc:'IN-PR-3'; Address: $2129), (sType: 0; Desc:'IN-VAR-1'; Address: $213A),
     (sType: 0; Desc:'IN-VAR-2'; Address: $2148), (sType: 0; Desc:'IN-VAR-3'; Address: $215E), (sType: 0; Desc:'IN-VAR-4'; Address: $2161), (sType: 0; Desc:'IN-VAR-5'; Address: $2174), (sType: 0; Desc:'IN-VAR-6'; Address: $219B),
     (sType: 0; Desc:'IN-NEXT-1'; Address: $21AF), (sType: 0; Desc:'IN-NEXT-2'; Address: $21B2), (sType: 0; Desc:'IN-ASSIGN'; Address: $21B9), (sType: 0; Desc:'REPORT-Cb'; Address: $21CE), (sType: 0; Desc:'IN-STOP'; Address: $21D0),
     (sType: 0; Desc:'REPORT-H'; Address: $21D4), (sType: 0; Desc:'IN-CHAN-K'; Address: $21D6), (sType: 0; Desc:'CO-TEMP-1'; Address: $21E1), (sType: 0; Desc:'CO-TEMP-2'; Address: $21E2), (sType: 0; Desc:'CO-TEMP-3'; Address: $21F2),
     (sType: 0; Desc:'CO-TEMP-4'; Address: $21FC), (sType: 0; Desc:'CO-TEMP-5'; Address: $2211), (sType: 0; Desc:'CO-TEMP-6'; Address: $2228), (sType: 0; Desc:'CO-TEMP-7'; Address: $2234), (sType: 0; Desc:'CO-TEMP-8'; Address: $223E),
     (sType: 0; Desc:'REPORT-K'; Address: $2244), (sType: 0; Desc:'CO-TEMP-9'; Address: $2246), (sType: 0; Desc:'CO-TEMP-A'; Address: $2257), (sType: 0; Desc:'CO-TEMP-B'; Address: $2258), (sType: 0; Desc:'CO-CHANGE'; Address: $226C),
     (sType: 0; Desc:'CO-TEMP-C'; Address: $2273), (sType: 0; Desc:'CO-TEMP-D'; Address: $227D), (sType: 0; Desc:'CO-TEMP-E'; Address: $2287), (sType: 0; Desc:'BORDER'; Address: $2294), (sType: 0; Desc:'BORDER-1'; Address: $22A6),
     (sType: 0; Desc:'PIXEL-ADD'; Address: $22AA), (sType: 0; Desc:'POINT-SUB'; Address: $22CB), (sType: 0; Desc:'POINT-LP'; Address: $22D4), (sType: 0; Desc:'PLOT'; Address: $22DC), (sType: 0; Desc:'PLOT-SUB'; Address: $22E5),
     (sType: 0; Desc:'PLOT-LOOP'; Address: $22F0), (sType: 0; Desc:'PL-TST-IN'; Address: $22FD), (sType: 0; Desc:'PLOT-END'; Address: $2303), (sType: 0; Desc:'STK-TO-BC'; Address: $2307), (sType: 0; Desc:'STK-TO-A'; Address: $2314),
     (sType: 0; Desc:'CIRCLE'; Address: $2320), (sType: 0; Desc:'C-R-GRE-1'; Address: $233B), (sType: 0; Desc:'C-ARC-GE1'; Address: $235A), (sType: 0; Desc:'DRAW'; Address: $2382), (sType: 0; Desc:'DR-3-PRMS'; Address: $238D),
     (sType: 0; Desc:'DR-SIN-NZ'; Address: $23A3), (sType: 0; Desc:'DR-PRMS'; Address: $23C1), (sType: 0; Desc:'DRW-STEPS'; Address: $2420), (sType: 0; Desc:'ARC-LOOP'; Address: $2425), (sType: 0; Desc:'ARC-START'; Address: $2439),
     (sType: 0; Desc:'ARC-END'; Address: $245F), (sType: 0; Desc:'LINE-DRAW'; Address: $2477), (sType: 0; Desc:'CD-PRMS1'; Address: $247D), (sType: 0; Desc:'USE-252'; Address: $2495), (sType: 0; Desc:'DRAW-SAVE'; Address: $2497),
     (sType: 0; Desc:'DRAW-LINE'; Address: $24B7), (sType: 0; Desc:'DL-X-GE-Y'; Address: $24C4), (sType: 0; Desc:'DL-LARGER'; Address: $24CB), (sType: 0; Desc:'D-L-LOOP'; Address: $24CE), (sType: 0; Desc:'D-L-DIAG'; Address: $24D4),
     (sType: 0; Desc:'D-L-HR-VT'; Address: $24DB), (sType: 0; Desc:'D-L-STEP'; Address: $24DF), (sType: 0; Desc:'D-L-PLOT'; Address: $24EC), (sType: 0; Desc:'D-L-RANGE'; Address: $24F7), (sType: 0; Desc:'REPORT-Bc'; Address: $24F9),
     (sType: 0; Desc:'SCANNING'; Address: $24FB), (sType: 0; Desc:'S-LOOP-1'; Address: $24FF), (sType: 0; Desc:'S-QUOTE-S'; Address: $250F), (sType: 0; Desc:'S-2-COORD'; Address: $2522), (sType: 0; Desc:'S-RPORT-C'; Address: $252D),
     (sType: 0; Desc:'SYNTAX-Z'; Address: $2530), (sType: 0; Desc:'S-SCRN$-S'; Address: $2535), (sType: 0; Desc:'S-SCRN-LP'; Address: $254F), (sType: 0; Desc:'S-SC-MTCH'; Address: $255A), (sType: 0; Desc:'S-SC-ROWS'; Address: $255D),
     (sType: 0; Desc:'S-SCR-NXT'; Address: $2573), (sType: 0; Desc:'S-SCR-STO'; Address: $257D), (sType: 0; Desc:'S-ATTR-S'; Address: $2580), (sType: 0; Desc:'scan-func'; Address: $2596), (sType: 0; Desc:'S-U-PLUS'; Address: $25AF),
     (sType: 0; Desc:'S-QUOTE'; Address: $25B3), (sType: 0; Desc:'S-Q-AGAIN'; Address: $25BE), (sType: 0; Desc:'S-Q-COPY'; Address: $25CB), (sType: 0; Desc:'S-Q-PRMS'; Address: $25D9), (sType: 0; Desc:'S-STRING'; Address: $25DB),
     (sType: 0; Desc:'S-BRACKET'; Address: $25E8), (sType: 0; Desc:'S-FN'; Address: $25F5), (sType: 0; Desc:'S-RND'; Address: $25F8), (sType: 0; Desc:'S-RND-END'; Address: $2625), (sType: 0; Desc:'S-PI'; Address: $2627),
     (sType: 0; Desc:'S-PI-END'; Address: $2630), (sType: 0; Desc:'S-INKEY$'; Address: $2634), (sType: 0; Desc:'S-IK$-STK'; Address: $2660), (sType: 0; Desc:'S-INK$-EN'; Address: $2665), (sType: 0; Desc:'S-SCREEN$'; Address: $2668),
     (sType: 0; Desc:'S-ATTR'; Address: $2672), (sType: 0; Desc:'S-POINT'; Address: $267B), (sType: 0; Desc:'S-ALPHNUM'; Address: $2684), (sType: 0; Desc:'S-DECIMAL'; Address: $268D), (sType: 0; Desc:'S-STK-DEC'; Address: $26B5),
     (sType: 0; Desc:'S-SD-SKIP'; Address: $26B6), (sType: 0; Desc:'S-NUMERIC'; Address: $26C3), (sType: 0; Desc:'S-LETTER'; Address: $26C9), (sType: 0; Desc:'S-CONT-1'; Address: $26DD), (sType: 0; Desc:'S-NEGATE'; Address: $26DF),
     (sType: 0; Desc:'S-NO-TO-$'; Address: $2707), (sType: 0; Desc:'S-PUSH-PO'; Address: $270D), (sType: 0; Desc:'S-CONT-2'; Address: $2712), (sType: 0; Desc:'S-CONT-3'; Address: $2713), (sType: 0; Desc:'S-OPERTR'; Address: $2723),
     (sType: 0; Desc:'S-LOOP'; Address: $2734), (sType: 0; Desc:'S-STK-LST'; Address: $274C), (sType: 0; Desc:'S-SYNTEST'; Address: $275B), (sType: 0; Desc:'S-RPORT-C2'; Address: $2761), (sType: 0; Desc:'S-RUNTEST'; Address: $2764),
     (sType: 0; Desc:'S-LOOPEND'; Address: $2770), (sType: 0; Desc:'S-TIGHTER'; Address: $2773), (sType: 0; Desc:'S-NOT-AND'; Address: $2788), (sType: 0; Desc:'S-NEXT'; Address: $2790), (sType: 0; Desc:'tbl-of-ops'; Address: $2795),
     (sType: 0; Desc:'tbl-priors'; Address: $27B0), (sType: 0; Desc:'S-FN-SBRN'; Address: $27BD), (sType: 0; Desc:'SF-BRKT-1'; Address: $27D0), (sType: 0; Desc:'SF-ARGMTS'; Address: $27D9), (sType: 0; Desc:'SF-BRKT-2'; Address: $27E4),
     (sType: 0; Desc:'SF-RPRT-C'; Address: $27E6), (sType: 0; Desc:'SF-FLAG-6'; Address: $27E9), (sType: 0; Desc:'SF-SYN-EN'; Address: $27F4), (sType: 0; Desc:'SF-RUN'; Address: $27F7), (sType: 0; Desc:'SF-ARGMT1'; Address: $2802),
     (sType: 0; Desc:'SF-FND-DF'; Address: $2808), (sType: 0; Desc:'REPORT-P'; Address: $2812), (sType: 0; Desc:'SF-CP-DEF'; Address: $2814), (sType: 0; Desc:'SF-NOT-FD'; Address: $2825), (sType: 0; Desc:'SF-VALUES'; Address: $2831),
     (sType: 0; Desc:'SF-ARG-LP'; Address: $2843), (sType: 0; Desc:'SF-ARG-VL'; Address: $2852), (sType: 0; Desc:'SF-R-BR-2'; Address: $2885), (sType: 0; Desc:'REPORT-Q'; Address: $288B), (sType: 0; Desc:'SF-VALUE'; Address: $288D),
     (sType: 0; Desc:'FN-SKPOVR'; Address: $28AB), (sType: 0; Desc:'LOOK-VARS'; Address: $28B2), (sType: 0; Desc:'V-CHAR'; Address: $28D4), (sType: 0; Desc:'V-STR-VAR'; Address: $28DE), (sType: 0; Desc:'V-TEST-FN'; Address: $28E3),
     (sType: 0; Desc:'V-RUN/SYN'; Address: $28EF), (sType: 0; Desc:'V-RUN'; Address: $28FD), (sType: 0; Desc:'V-EACH'; Address: $2900), (sType: 0; Desc:'V-MATCHES'; Address: $2912), (sType: 0; Desc:'V-SPACES'; Address: $2913),
     (sType: 0; Desc:'V-GET-PTR'; Address: $2929), (sType: 0; Desc:'V-NEXT'; Address: $292A), (sType: 0; Desc:'V-80-BYTE'; Address: $2932), (sType: 0; Desc:'V-SYNTAX'; Address: $2934), (sType: 0; Desc:'V-FOUND-1'; Address: $293E),
     (sType: 0; Desc:'V-FOUND-2'; Address: $293F), (sType: 0; Desc:'V-PASS'; Address: $2943), (sType: 0; Desc:'V-END'; Address: $294B), (sType: 0; Desc:'STK-F-ARG'; Address: $2951), (sType: 0; Desc:'SFA-LOOP'; Address: $295A),
     (sType: 0; Desc:'SFA-CP-VR'; Address: $296B), (sType: 0; Desc:'SFA-MATCH'; Address: $2981), (sType: 0; Desc:'SFA-END'; Address: $2991), (sType: 0; Desc:'STK-VAR'; Address: $2996), (sType: 0; Desc:'SV-SIMPLE$'; Address: $29A1),
     (sType: 0; Desc:'SV-ARRAYS'; Address: $29AE), (sType: 0; Desc:'SV-PTR'; Address: $29C0), (sType: 0; Desc:'SV-COMMA'; Address: $29C3), (sType: 0; Desc:'SV-CLOSE'; Address: $29D8), (sType: 0; Desc:'SV-CH-ADD'; Address: $29E0),
     (sType: 0; Desc:'SV-COUNT'; Address: $29E7), (sType: 0; Desc:'SV-LOOP'; Address: $29EA), (sType: 0; Desc:'SV-MULT'; Address: $29FB), (sType: 0; Desc:'SV-RPT-C'; Address: $2A12), (sType: 0; Desc:'REPORT-3'; Address: $2A20),
     (sType: 0; Desc:'SV-NUMBER'; Address: $2A22), (sType: 0; Desc:'SV-ELEM$'; Address: $2A2C), (sType: 0; Desc:'SV-SLICE'; Address: $2A45), (sType: 0; Desc:'SV-DIM'; Address: $2A48), (sType: 0; Desc:'SV-SLICE?'; Address: $2A49),
     (sType: 0; Desc:'SLICING'; Address: $2A52), (sType: 0; Desc:'SL-RPT-C'; Address: $2A7A), (sType: 0; Desc:'SL-SECOND'; Address: $2A81), (sType: 0; Desc:'SL-DEFINE'; Address: $2A94), (sType: 0; Desc:'SL-OVER'; Address: $2AA8),
     (sType: 0; Desc:'SL-STORE'; Address: $2AAD), (sType: 0; Desc:'STK-ST-0'; Address: $2AB1), (sType: 0; Desc:'STK-STO-$'; Address: $2AB2), (sType: 0; Desc:'STK-STORE'; Address: $2AB6), (sType: 0; Desc:'INT-EXP1'; Address: $2ACC),
     (sType: 0; Desc:'INT-EXP2'; Address: $2ACD), (sType: 0; Desc:'I-CARRY'; Address: $2AE8), (sType: 0; Desc:'I-RESTORE'; Address: $2AEB), (sType: 0; Desc:'DE,(DE+1)'; Address: $2AEE), (sType: 0; Desc:'GET-HL*DE'; Address: $2AF4),
     (sType: 0; Desc:'LET'; Address: $2AFF), (sType: 0; Desc:'L-EACH-CH'; Address: $2B0B), (sType: 0; Desc:'L-NO-SP'; Address: $2B0C), (sType: 0; Desc:'L-TEST-CH'; Address: $2B1F), (sType: 0; Desc:'L-SPACES'; Address: $2B29),
     (sType: 0; Desc:'L-CHAR'; Address: $2B3E), (sType: 0; Desc:'L-SINGLE'; Address: $2B4F), (sType: 0; Desc:'L-NUMERIC'; Address: $2B59), (sType: 0; Desc:'L-EXISTS'; Address: $2B66), (sType: 0; Desc:'L-DELETE$'; Address: $2B72),
     (sType: 0; Desc:'L-LENGTH'; Address: $2B9B), (sType: 0; Desc:'L-IN-W/S'; Address: $2BA3), (sType: 0; Desc:'L-ENTER'; Address: $2BA6), (sType: 0; Desc:'L-ADD$'; Address: $2BAF), (sType: 0; Desc:'L-NEW$'; Address: $2BC0),
     (sType: 0; Desc:'L-STRING'; Address: $2BC6), (sType: 0; Desc:'L-FIRST'; Address: $2BEA), (sType: 0; Desc:'STK-FETCH'; Address: $2BF1), (sType: 0; Desc:'DIM'; Address: $2C02), (sType: 0; Desc:'D-RPORT-C'; Address: $2C05),
     (sType: 0; Desc:'D-RUN'; Address: $2C15), (sType: 0; Desc:'D-LETTER'; Address: $2C1F), (sType: 0; Desc:'D-SIZE'; Address: $2C2D), (sType: 0; Desc:'D-NO-LOOP'; Address: $2C2E), (sType: 0; Desc:'DIM-CLEAR'; Address: $2C7C),
     (sType: 0; Desc:'DIM-SIZES'; Address: $2C7F), (sType: 0; Desc:'ALPHANUM'; Address: $2C88), (sType: 0; Desc:'ALPHA'; Address: $2C8D), (sType: 0; Desc:'DEC-TO-FP'; Address: $2C9B), (sType: 0; Desc:'BIN-DIGIT'; Address: $2CA2),
     (sType: 0; Desc:'BIN-END'; Address: $2CB3), (sType: 0; Desc:'NOT-BIN'; Address: $2CB8), (sType: 0; Desc:'DECIMAL'; Address: $2CCB), (sType: 0; Desc:'DEC-RPT-C'; Address: $2CCF), (sType: 0; Desc:'DEC-STO-1'; Address: $2CD5),
     (sType: 0; Desc:'NXT-DGT-1'; Address: $2CDA), (sType: 0; Desc:'E-FORMAT'; Address: $2CEB), (sType: 0; Desc:'SIGN-FLAG'; Address: $2CF2), (sType: 0; Desc:'SIGN-DONE'; Address: $2CFE), (sType: 0; Desc:'ST-E-PART'; Address: $2CFF),
     (sType: 0; Desc:'E-FP-JUMP'; Address: $2D18), (sType: 0; Desc:'NUMERIC'; Address: $2D1B), (sType: 0; Desc:'STK-DIGIT'; Address: $2D22), (sType: 0; Desc:'STACK-A'; Address: $2D28), (sType: 0; Desc:'STACK-BC'; Address: $2D2B),
     (sType: 0; Desc:'INT-TO-FP'; Address: $2D3B), (sType: 0; Desc:'NXT-DGT-2'; Address: $2D40), (sType: 0; Desc:'E-TO-FP'; Address: $2D4F), (sType: 0; Desc:'E-SAVE'; Address: $2D55), (sType: 0; Desc:'E-LOOP'; Address: $2D60),
     (sType: 0; Desc:'E-DIVSN'; Address: $2D6D), (sType: 0; Desc:'E-FETCH'; Address: $2D6E), (sType: 0; Desc:'E-TST-END'; Address: $2D71), (sType: 0; Desc:'E-END'; Address: $2D7B), (sType: 0; Desc:'INT-FETCH'; Address: $2D7F),
     (sType: 0; Desc:'p-int-sto'; Address: $2D8C), (sType: 0; Desc:'INT-STORE'; Address: $2D8E), (sType: 0; Desc:'FP-TO-BC'; Address: $2DA2), (sType: 0; Desc:'FP-DELETE'; Address: $2DAD), (sType: 0; Desc:'LOG(2^A)'; Address: $2DC1),
     (sType: 0; Desc:'FP-TO-A'; Address: $2DD5), (sType: 0; Desc:'FP-A-END'; Address: $2DE1), (sType: 0; Desc:'PRINT-FP'; Address: $2DE3), (sType: 0; Desc:'PF-NEGTVE'; Address: $2DF2), (sType: 0; Desc:'PF-POSTVE'; Address: $2DF8),
     (sType: 0; Desc:'PF-LOOP'; Address: $2E01), (sType: 0; Desc:'PF-SAVE'; Address: $2E1E), (sType: 0; Desc:'PF-SMALL'; Address: $2E24), (sType: 0; Desc:'PF-LARGE'; Address: $2E56), (sType: 0; Desc:'PF-MEDIUM'; Address: $2E6F),
     (sType: 0; Desc:'PF-BITS'; Address: $2E7B), (sType: 0; Desc:'PF-BYTES'; Address: $2E8A), (sType: 0; Desc:'PF-DIGITS'; Address: $2EA1), (sType: 0; Desc:'PF-INSERT'; Address: $2EA9), (sType: 0; Desc:'PF-TEST-2'; Address: $2EB3),
     (sType: 0; Desc:'PF-ALL-9'; Address: $2EB8), (sType: 0; Desc:'PF-MORE'; Address: $2ECB), (sType: 0; Desc:'PF-FRACTN'; Address: $2ECF), (sType: 0; Desc:'PF-FRN-LP'; Address: $2EDF), (sType: 0; Desc:'PF-FR-DGT'; Address: $2EEC),
     (sType: 0; Desc:'PF-FR-EXX'; Address: $2EEF), (sType: 0; Desc:'PF-ROUND'; Address: $2F0C), (sType: 0; Desc:'PF-RND-LP'; Address: $2F18), (sType: 0; Desc:'PF-R-BACK'; Address: $2F25), (sType: 0; Desc:'PF-COUNT'; Address: $2F2D),
     (sType: 0; Desc:'PF-NOT-E'; Address: $2F46), (sType: 0; Desc:'PF-E-SBRN'; Address: $2F4A), (sType: 0; Desc:'PF-OUT-LP'; Address: $2F52), (sType: 0; Desc:'PF-OUT-DT'; Address: $2F59), (sType: 0; Desc:'PF-DC-OUT'; Address: $2F5E),
     (sType: 0; Desc:'PF-DEC-0S'; Address: $2F64), (sType: 0; Desc:'PF-E-FRMT'; Address: $2F6C), (sType: 0; Desc:'PF-E-POS'; Address: $2F83), (sType: 0; Desc:'PF-E-SIGN'; Address: $2F85), (sType: 0; Desc:'CA-10*A+C'; Address: $2F8B),
     (sType: 0; Desc:'PREP-ADD'; Address: $2F9B), (sType: 0; Desc:'NEG-BYTE'; Address: $2FAF), (sType: 0; Desc:'FETCH-TWO'; Address: $2FBA), (sType: 0; Desc:'SHIFT-FP'; Address: $2FDD), (sType: 0; Desc:'ONE-SHIFT'; Address: $2FE5),
     (sType: 0; Desc:'ADDEND-0'; Address: $2FF9), (sType: 0; Desc:'ZEROS-4/5'; Address: $2FFB), (sType: 0; Desc:'ADD-BACK'; Address: $3004), (sType: 0; Desc:'ALL-ADDED'; Address: $300D), (sType: 0; Desc:'subtract'; Address: $300F),
     (sType: 0; Desc:'addition'; Address: $3014), (sType: 0; Desc:'ADDN-OFLW'; Address: $303C), (sType: 0; Desc:'FULL-ADDN'; Address: $303E), (sType: 0; Desc:'SHIFT-LEN'; Address: $3055), (sType: 0; Desc:'TEST-NEG'; Address: $307C),
     (sType: 0; Desc:'ADD-REP-6'; Address: $309F), (sType: 0; Desc:'END-COMPL'; Address: $30A3), (sType: 0; Desc:'GO-NC-MLT'; Address: $30A5), (sType: 0; Desc:'HL-HL*DE'; Address: $30A9), (sType: 0; Desc:'HL-LOOP'; Address: $30B1),
     (sType: 0; Desc:'HL-AGAIN'; Address: $30BC), (sType: 0; Desc:'HL-END'; Address: $30BE), (sType: 0; Desc:'PREP-M/D'; Address: $30C0), (sType: 0; Desc:'multiply'; Address: $30CA), (sType: 0; Desc:'MULT-RSLT'; Address: $30EA),
     (sType: 0; Desc:'MULT-OFLW'; Address: $30EF), (sType: 0; Desc:'MULT-LONG'; Address: $30F0), (sType: 0; Desc:'MLT-LOOP'; Address: $3114), (sType: 0; Desc:'NO-ADD'; Address: $311B), (sType: 0; Desc:'STRT-MLT'; Address: $3125),
     (sType: 0; Desc:'MAKE-EXPT'; Address: $313B), (sType: 0; Desc:'DIVN-EXPT'; Address: $313D), (sType: 0; Desc:'OFLW1-CLR'; Address: $3146), (sType: 0; Desc:'OFLW2-CLR'; Address: $3151), (sType: 0; Desc:'TEST-NORM'; Address: $3155),
     (sType: 0; Desc:'NEAR-ZERO'; Address: $3159), (sType: 0; Desc:'ZERO-RSLT'; Address: $315D), (sType: 0; Desc:'SKIP-ZERO'; Address: $315E), (sType: 0; Desc:'NORMALISE'; Address: $316C), (sType: 0; Desc:'SHIFT-ONE'; Address: $316E),
     (sType: 0; Desc:'NORML-NOW'; Address: $3186), (sType: 0; Desc:'OFLOW-CLR'; Address: $3195), (sType: 0; Desc:'REPORT-6'; Address: $31AD), (sType: 0; Desc:'division'; Address: $31AF), (sType: 0; Desc:'DIV-LOOP'; Address: $31D2),
     (sType: 0; Desc:'div-34th'; Address: $31DB), (sType: 0; Desc:'DIV-START'; Address: $31E2), (sType: 0; Desc:'SUBN-ONLY'; Address: $31F2), (sType: 0; Desc:'NO-RSTORE'; Address: $31F9), (sType: 0; Desc:'COUNT-ONE'; Address: $31FA),
     (sType: 0; Desc:'truncate'; Address: $3214), (sType: 0; Desc:'T-GR-ZERO'; Address: $3221), (sType: 0; Desc:'T-FIRST'; Address: $3233), (sType: 0; Desc:'T-SMALL'; Address: $323F), (sType: 0; Desc:'T-NUMERIC'; Address: $3252),
     (sType: 0; Desc:'T-TEST'; Address: $325E), (sType: 0; Desc:'T-SHIFT'; Address: $3261), (sType: 0; Desc:'T-STORE'; Address: $3267), (sType: 0; Desc:'T-EXPNENT'; Address: $326C), (sType: 0; Desc:'X-LARGE'; Address: $326D),
     (sType: 0; Desc:'NIL-BYTES'; Address: $3272), (sType: 0; Desc:'BYTE-ZERO'; Address: $327E), (sType: 0; Desc:'BITS-ZERO'; Address: $3283), (sType: 0; Desc:'LESS-MASK'; Address: $328A), (sType: 0; Desc:'IX-END'; Address: $3290),
     (sType: 0; Desc:'RE-ST-TWO'; Address: $3293), (sType: 0; Desc:'RESTK-SUB'; Address: $3296), (sType: 0; Desc:'re-stack'; Address: $3297), (sType: 0; Desc:'RS-NRMLSE'; Address: $32B1), (sType: 0; Desc:'RSTK-LOOP'; Address: $32B2),
     (sType: 0; Desc:'RS-STORE'; Address: $32BD), (sType: 0; Desc:'stk-zero'; Address: $32C5), (sType: 0; Desc:'stk-one'; Address: $32C8), (sType: 0; Desc:'stk-half'; Address: $32CC), (sType: 0; Desc:'stk-pi/2'; Address: $32CE),
     (sType: 0; Desc:'stk-ten'; Address: $32D3), (sType: 0; Desc:'tbl-addrs'; Address: $32D7), (sType: 0; Desc:'CALCULATE'; Address: $335B), (sType: 0; Desc:'GEN-ENT-1'; Address: $335E), (sType: 0; Desc:'GEN-ENT-2'; Address: $3362),
     (sType: 0; Desc:'RE-ENTRY'; Address: $3365), (sType: 0; Desc:'SCAN-ENT'; Address: $336C), (sType: 0; Desc:'FIRST-3D'; Address: $3380), (sType: 0; Desc:'DOUBLE-A'; Address: $338C),(sType: 0; Desc:'ENT-TABLE'; Address: $338E),
     (sType: 0; Desc:'delete'; Address: $33A1), (sType: 0; Desc:'fp-calc-2'; Address: $33A2), (sType: 0; Desc:'TEST-5-SP'; Address: $33A9), (sType: 0; Desc:'STACK-NUM'; Address: $33B4), (sType: 0; Desc:'MOVE-FP'; Address: $33C0),
     (sType: 0; Desc:'stk-data'; Address: $33C6), (sType: 0; Desc:'STK-CONST'; Address: $33C8), (sType: 0; Desc:'FORM-EXP'; Address: $33DE), (sType: 0; Desc:'STK-ZEROS'; Address: $33F1), (sType: 0; Desc:'SKIP-CONS'; Address: $33F7),
     (sType: 0; Desc:'SKIP-NEXT'; Address: $33F8), (sType: 0; Desc:'LOC-MEM'; Address: $3406), (sType: 0; Desc:'get-mem-xx'; Address: $340F), (sType: 0; Desc:'stk-const-xx'; Address: $341B), (sType: 0; Desc:'st-mem-xx'; Address: $342D),
     (sType: 0; Desc:'exchange'; Address: $343C), (sType: 0; Desc:'SWAP-BYTE'; Address: $343E), (sType: 0; Desc:'series-xx'; Address: $3449), (sType: 0; Desc:'G-LOOP'; Address: $3453), (sType: 0; Desc:'abs'; Address: $346A),
     (sType: 0; Desc:'NEGATE'; Address: $346E), (sType: 0; Desc:'NEG-TEST'; Address: $3474), (sType: 0; Desc:'INT-CASE'; Address: $3483), (sType: 0; Desc:'sgn'; Address: $3492), (sType: 0; Desc:'in'; Address: $34A5),
     (sType: 0; Desc:'peek'; Address: $34AC), (sType: 0; Desc:'IN-PK-STK'; Address: $34B0), (sType: 0; Desc:'usr-no'; Address: $34B3), (sType: 0; Desc:'usr-$'; Address: $34BC), (sType: 0; Desc:'USR-RANGE'; Address: $34D3),
     (sType: 0; Desc:'USR-STACK'; Address: $34E4), (sType: 0; Desc:'REPORT-A'; Address: $34E7), (sType: 0; Desc:'TEST-ZERO'; Address: $34E9), (sType: 0; Desc:'GREATER-0'; Address: $34F9), (sType: 0; Desc:'NOT'; Address: $3501),
     (sType: 0; Desc:'less-0'; Address: $3506), (sType: 0; Desc:'SIGN-TO-C'; Address: $3507), (sType: 0; Desc:'FP-0/1'; Address: $350B), (sType: 0; Desc:'or'; Address: $351B), (sType: 0; Desc:'no-&-no'; Address: $3524),
     (sType: 0; Desc:'str-&-no'; Address: $352D), (sType: 0; Desc:'no-l-eql,etc.'; Address: $353B), (sType: 0; Desc:'EX-OR-NOT'; Address: $3543), (sType: 0; Desc:'NU-OR-STR'; Address: $354E), (sType: 0; Desc:'STRINGS'; Address: $3559),
     (sType: 0; Desc:'BYTE-COMP'; Address: $3564), (sType: 0; Desc:'SECND-LOW'; Address: $356B), (sType: 0; Desc:'BOTH-NULL'; Address: $3572), (sType: 0; Desc:'SEC-PLUS'; Address: $3575), (sType: 0; Desc:'FRST-LESS'; Address: $3585),
     (sType: 0; Desc:'STR-TEST'; Address: $3588), (sType: 0; Desc:'END-TESTS'; Address: $358C), (sType: 0; Desc:'strs-add'; Address: $359C), (sType: 0; Desc:'OTHER-STR'; Address: $35B7), (sType: 0; Desc:'STK-PNTRS'; Address: $35BF),
     (sType: 0; Desc:'chrs'; Address: $35C9), (sType: 0; Desc:'REPORT-Bd'; Address: $35DC), (sType: 0; Desc:'val/val$'; Address: $35DE), (sType: 0; Desc:'V-RPORT-C'; Address: $360C), (sType: 0; Desc:'str$'; Address: $361F),
     (sType: 0; Desc:'read-in'; Address: $3645), (sType: 0; Desc:'R-I-STORE'; Address: $365F), (sType: 0; Desc:'code'; Address: $3669), (sType: 0; Desc:'STK-CODE'; Address: $3671), (sType: 0; Desc:'len'; Address: $3674),
     (sType: 0; Desc:'dec-jr-nz'; Address: $367A), (sType: 0; Desc:'JUMP'; Address: $3686), (sType: 0; Desc:'JUMP-2'; Address: $3687), (sType: 0; Desc:'jump-true'; Address: $368F), (sType: 0; Desc:'end-calc'; Address: $369B),
     (sType: 0; Desc:'n-mod-m'; Address: $36A0), (sType: 0; Desc:'int'; Address: $36AF), (sType: 0; Desc:'X-NEG'; Address: $36B7), (sType: 0; Desc:'EXIT'; Address: $36C2), (sType: 0; Desc:'EXP'; Address: $36C4),
     (sType: 0; Desc:'REPORT-6b'; Address: $3703), (sType: 0; Desc:'N-NEGTV'; Address: $3705), (sType: 0; Desc:'RESULT-OK'; Address: $370C), (sType: 0; Desc:'RSLT-ZERO'; Address: $370E), (sType: 0; Desc:'ln'; Address: $3713),
     (sType: 0; Desc:'REPORT-Ab'; Address: $371A), (sType: 0; Desc:'VALID'; Address: $371C), (sType: 0; Desc:'GRE.8'; Address: $373D), (sType: 0; Desc:'get-argt'; Address: $3783), (sType: 0; Desc:'ZPLUS'; Address: $37A1),
     (sType: 0; Desc:'YNEG'; Address: $37A8), (sType: 0; Desc:'cos'; Address: $37AA), (sType: 0; Desc:'sin'; Address: $37B5), (sType: 0; Desc:'C-ENT'; Address: $37B7), (sType: 0; Desc:'tan'; Address: $37DA), (sType: 0; Desc:'atn'; Address: $37E2),
     (sType: 0; Desc:'SMALL'; Address: $37F8), (sType: 0; Desc:'CASES'; Address: $37FA), (sType: 0; Desc:'asn'; Address: $3833), (sType: 0; Desc:'acs'; Address: $3843), (sType: 0; Desc:'sqr'; Address: $384A), (sType: 0; Desc:'to-power'; Address: $3851),
     (sType: 0; Desc:'XIS0'; Address: $385D), (sType: 0; Desc:'ONE'; Address: $386A), (sType: 0; Desc:'LAST'; Address: $386C), (sType: 0; Desc:'spare'; Address: $386E), (sType: 0; Desc:'CHAR-set'; Address: $3D00));

	// BASIC Keywords in ASCII (token) order, for the tokeniser.

  AsciiKeywords: Array[0..102] of AnsiString = ('SPECTRUM', 'PLAY', 'RND', 'INKEY$', 'PI', 'FN', 'POINT', 'SCREEN$', 'ATTR',
                                            'AT', 'TAB', 'VAL$', 'CODE', 'VAL', 'LEN', 'SIN', 'COS',
					                              'TAN', 'ASN', 'ACS', 'ATN', 'LN', 'EXP', 'INT', 'SQR',
														   'SGN', 'ABS', 'PEEK', 'IN', 'USR', 'STR$','CHR$', 'NOT',
														   'BIN', 'OR', 'AND', '<=', '>=', '<>', 'LINE', 'THEN',
														   'TO', 'STEP', 'DEF FN', 'CAT', 'FORMAT', 'MOVE', 'ERASE',
														   'OPEN #', 'CLOSE #', 'MERGE', 'VERIFY', 'BEEP', 'CIRCLE',
														   'INK', 'PAPER', 'FLASH', 'BRIGHT', 'INVERSE', 'OVER',
														   'OUT', 'LPRINT', 'LLIST', 'STOP', 'READ', 'DATA', 'RESTORE',
														   'NEW', 'BORDER', 'CONTINUE', 'DIM', 'REM', 'FOR', 'GO TO',
														   'GO SUB', 'INPUT', 'LOAD', 'LIST', 'LET', 'PAUSE', 'NEXT',
                                             'POKE', 'PRINT', 'PLOT', 'RUN', 'SAVE', 'RANDOMIZE', 'IF',
														   'CLS', 'DRAW', 'CLEAR', 'RETURN', 'COPY', 'DUMMY', 'GOTO',
                                            'GOSUB', 'DEFFN', 'INKEY', 'VAL', 'SCREEN', 'STR', 'CHR', 'DUMMY');

  SpaceKeywords: Array[0..93] of AnsiString = ('SPECTRUM ', 'PLAY ', 'RND', 'INKEY$', 'PI', 'FN ', 'POINT ', ' SCREEN$ ', 'ATTR ',
                                                'AT ', 'TAB ', 'VAL$ ', ' CODE ', 'VAL ', 'LEN ', 'SIN ', 'COS ',
					        'TAN ', 'ASN ', 'ACS ', 'ATN ', 'LN ', 'EXP ', 'INT ', 'SQR ',
														  'SGN ', 'ABS ', 'PEEK ', 'IN ', 'USR ', 'STR$ ','CHR$ ', 'NOT ',
														  'BIN ', 'OR ', 'AND ', '<=', '>=', '<>', 'LINE ', 'THEN ',
														  'TO ', 'STEP ', 'DEF FN ', 'CAT ', 'FORMAT ', 'MOVE ', 'ERASE ',
														  'OPEN #', ' CLOSE #', 'MERGE ', 'VERIFY ', 'BEEP ', 'CIRCLE ',
														  'INK ', 'PAPER ', 'FLASH ', 'BRIGHT ', 'INVERSE ', 'OVER ',
														  'OUT ', 'LPRINT ', 'LLIST ', 'STOP ', 'READ ', 'DATA ', 'RESTORE ',
														  'NEW ', 'BORDER ', 'CONTINUE ', 'DIM ', 'REM ', 'FOR ', 'GO TO ',
														  'GO SUB ', 'INPUT ', 'LOAD ', 'LIST ', 'LET ', 'PAUSE ', 'NEXT ',
                                           'POKE ', 'PRINT ', 'PLOT ', 'RUN ', 'SAVE ', 'RANDOMIZE ', 'IF ',
														  'CLS ', 'DRAW ', 'CLEAR ', 'RETURN ', 'COPY ', 'DUMMY');

  // Colour names

  ColourNames: Array[0..9] of AnsiString = ('Black', 'Blue', 'Red', 'Magenta', 'Green', 'Cyan', 'Yellow', 'White', 'No Change', 'Contrast');

	// System Variables (Descriptions from 48k Manual)

	KSTATE:  Word = 23552;	// Used in reading the keyboard.
	LAST_K:  Word = 23560;	// Stores newly pressed key.
	REPDEL:  Word = 23561;	// Time (in 50ths of a second, 60ths of a second in N. America) that a key
									// must be held down before it repeats. This starts off at 35, but you can
									// POKE in other values.
	REPPER:  Word = 23562;	// Delay (in 50ths of a second in 60ths of a second in N. America) between
									// successive repeats of a key held down: initially 5.
	DEFADD:  Word = 23563;	// Address of arguments of user defined function if one is being evaluated;
									// otherwise 0.
	K_DATA:  Word = 23565;	// Stores 2nd byte of colour controls entered from keyboard .
	TVDATA:  Word = 23566;	// Stores bytes of colour, AT and TAB controls going to television.
	STRMS:   Word = 23568;	// Addresses of channels attached to streams.
	CHARS:   Word = 23606;	// 256 less than address of character set (which starts with space and
									// carries on to the copyright symbol). Normally in ROM, but you can set
									// up your own in RAM and make CHARS point to it.
	RASP:    Word = 23608;	// Length of warning buzz.
	PIP:     Word = 23609;	// Length of keyboard click.
	ERR_NR:  Word = 23610;	// 1 less than the report code. Starts off at 255 (for 1) so PEEK 23610
									// gives 255.
	FLAGS:   Word = 23611;	// Various flags to control the BASIC system.
	TV_FLAG: Word = 23612;	// Flags associated with the television.
	ERR_SP:  Word = 23613;	// Address of item on machine stack to be used as error return.
	LIST_SP: Word = 23615;	// Address of return address from automatic listing.
	MODE:    Word = 23617;	// Specifies K, L, C. E or G cursor.
	NEWPPC:  Word = 23618;	// Line to be jumped to.
	NSPPC:   Word = 23620;	// Statement number in line to be jumped to. Poking first NEWPPC and then
									// NSPPC forces a jump to a specified statement in a line.
	PPC:     Word = 23621;	// Line number of statement currently being executed.
	SUBPPC:  Word = 23623;	// Number within line of statement being executed.
	BORDCR:  Word = 23624;	// Border colour * 8; also contains the attributes normally used for the
									// lower half of the screen.
	E_PPC:   Word = 23625;	// Number of current line (with program cursor).
	VARS:		Word = 23627;	// Address of variables.
	DEST:    Word = 23629;	// Address of variable in assignment.
	CHANS: 	Word = 23631;	// Address of channel data.
	CURCHL:	Word = 23633;	// Address of information currently being used for input and output.
	PROG:    Word = 23635;	// Address of BASIC program.
	NXTLIN:	Word = 23637;	// Address of next line in program.
	DATADD:  Word = 23639;	// Address of terminator of last DATA item.
	E_LINE:  Word = 23641;	// Address of command being typed in.
	K_CUR:   Word = 23643;	// Address of cursor.
	CH_ADD:	Word = 23645;	// Address of the next character to be interpreted; the character after the
									// argument of PEEK, or the NEWLINE at the end of a POKE statement.
	X_PTR:   Word = 23647;	// Address of the character after the ? marker.
	WORKSP:  Word = 23649;	// Address of temporary work space.
	STKBOT:  Word = 23651;	// Address of bottom of calculator stack.
	STKEND:  Word = 23653;	// Address of start of spare space.
	BREG:    Word = 23655;	// Calculator's b register.
	MEM:		Word = 23656;	// Address of area used for calculator's memory. (Usually MEMBOT, but not
									// always.)
	FLAGS2:  Word = 23658;	// More flags.
	DF_SZ:	Word = 23659;	// The number of lines (including one blank line) in the lower part of the
									// screen.
	S_TOP:	Word = 23660;	// The number of the top program line in automatic listings.
	OLDPPC:  Word = 23662;	// Line number to which CONTINUE jumps.
	OSPCC:   Word = 23664;	// Number within line of statement to which CONTINUE jumps.
	FLAGX:   Word = 23665;	// Various flags.
	STRLEN:  Word = 23666;	// Length of AnsiString type destination in assignment.
	T_ADDR:  Word = 23668;	// Address of next item in syntax table (very unlikely to be useful).
	SEED:    Word = 23670;	// The seed for RND. This is the variable that is set by RANDOMIZE.
  FRAMES:  Word = 23672;	// 3 byte (least significant first), frame counter. Incremented every 20ms.
	UDG:		Word = 23675;	// Address of 1st user defined graphic. You can change this for instance to
									// save space by having fewer user defined graphics.
	COORDSX: Word = 23677;	// x-coordinate of last point plotted.
	COORDSY:	Word = 23678;	// y-coordinate of last point plotted.
	P_POSN:  Word = 23679;	// 33 column number of printer position
	PR_CC:   Word = 23680;	// Less significant byte of address of next position for LPRINT to print at
									// (in printer buffer).
	UNUSED1: Word = 23681;	// Not used.
	ECHO_E:  Word = 23682;	// 33 column number and 24 line number (in lower half) of end of input buffer.
	DF_CC:   Word = 23684;	// Address in display file of PRINT position.
	DF_CCL:	Word = 23686;	// Like DF CC for lower part of screen.
	S_POSN:	Word = 23688;	// 33 column number for PRINT position
	PRPOSN:  Word = 23689;	// 24 line number for PRINT position.
	SPOSNL:  Word = 23690;	// Like S POSN for lower part
	SCR_CT:  Word = 23692;	// Counts scrolls: it is always 1 more than the number of scrolls that will
									// be done before stopping with scroll? If you keep poking this with a
									// number bigger than 1 (say 255), the screen will scroll on and on without
									// asking you.
	ATTR_P:  Word = 23693;	// Permanent current colours, etc (as set up by colour statements).
	MASP_P:  Word = 23694;	// Used for transparent colours, etc. Any bit that is 1 shows that the
									// corresponding attribute bit is taken not from ATTR P, but from what is
									// already on the screen.
	ATTR_T:  Word = 23695;	// Temporary current colours, etc (as set up by colour items).
	MASK_T:  Word = 23696;	// Like MASK P, but temporary.
	P_FLAG:  Word = 23697;	// More flags.
	MEMBOT:  Word = 23698;	// Calculator's memory area; used to store numbers that cannot conveniently
									// be put on the calculator stack.
	UNUSED2: Word = 23728;	// Not used.
	RAMTOP:  Word = 23730;	// Address of last byte of BASIC system area.
	P_RAMT:  Word = 23732;	// Address of last byte of physical RAM.

  // Program States

  PS_Unknown: Byte = 0;   // No state of any importance.
  PS_Stopped: Byte = 1;   // User or Error has stopped program.
  PS_Reset:   Byte = 2;   // Reset Sequence running
  PS_Paused:  Byte = 3;   // PAUSE m - paused for m frames.

implementation

Uses BASinMain, Watches, Breakpoints, RLEUnit, Display, TokenWindow, AddCode, CPUDisplay,
     FastCore, InputUtils, Filing, BASSupport, Utility, ErrorWindow, Parser, Profiling,
     VarsWindow, SysVars, LogWind, CommandHistory, Evaluate, GOSUB, Sound, Tapes, RomPrintOutputUnit;

Function LoadRom(Var Location: Array of Byte): Boolean;
Var
	F: TFileStream;
  OldProtect: Cardinal;
Begin

  DebugLog('Load 48k ROM from '+Filename);
  Result:=False;
  If Not FileExists(Filename) Then Begin
     DebugLog('ROM load failed');
     MessageBox(BASinOutput.Handle, 'The 48k Spectrum Rom could not be opened.'#13'BasinC cannot run without this file,'#13'and will now close.', PChar('Missing ROM File'), MB_OK or MB_ICONWARNING);
     Result := False;
     Exit;
  End;
	F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
	F.Read(Location[0], 16384);
	F.Free;




  Filename := '';
  Result := True;

End;

Procedure LoadRom128k(Var Location: Array of Byte);
Var
	F: TFileStream;
Begin
  DebugLog('Load 128k FROM from '+Filename);
  n128kAvailable := False;
  If Not FileExists(Filename) Then Begin
     MessageBox(BASinOutput.Handle, 'The 128k Spectrum Rom could not be opened.'#13'The PLAY and RAMDisk commands are'#13'unavailable.', PChar('Missing ROM File'), MB_OK or MB_ICONWARNING);
     DebugLog('128k ROM load failed');
     Exit;
  End;
	F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
	F.Read(Location[0], 32768);
	F.Free;
  Filename := '';
  n128kAvailable := True;
End;

Procedure PageROM(Page128k: Boolean);
Var
  Idx: Integer;
Begin

  If Page128k Then Begin
     If Not n128kAvailable Then Exit;
     In128k := True;
     For Idx := $6b To $c2 Do
        Memory[23296 + (Idx - $6b)] := Rom128k[Idx];
     If CurROM = 1 Then
        CopyMemory(@Memory[0], @Rom128k[16384], 16384)
     Else
        CopyMemory(@Memory[0], @Rom128k[0], 16384);
  End Else Begin
     CopyMemory(@Memory[0], @Rom48k[0], 16384);
     In128k := False;
  End;

End;

Procedure Page7FFD(Value: Byte);
Var
  TempByte: Byte;
  Idx: Integer;
Begin

  If Not n128kAvailable Then Exit;
  If Registers.PC = 7806 Then
     If Value = 0 Then Begin
        If In128k Then
           PageROM(False);
        Exit;
     End;


  If Not DisablePaging Then Begin

     Last7FFD := Value;

     For Idx := $6b To $c2 Do
        Memory[23296 + (Idx - $6b)] := Rom128k[Idx];

     If Value and 16 = 16 Then
        CurROM := 1
     Else
        CurROM := 0;
     PageROM(True);

     TempByte := Value And 7;
     CopyMemory(@RAMBanks[PagedBank], @Memory[49152], 16384);    // Update the current RAM page with current contents.
     CopyMemory(@Memory[49152], @RAMBanks[TempByte, 0], 16384);  // Copy the Bank to be paged in to upper memory
     PagedBank := TempByte;

     If Value and 8 = 8 Then Begin
        ScreenPointer := @RAMBanks[7, 0];
        ShadowScreen := True;
     End Else Begin
        ScreenPointer := @Memory[16384];
        ShadowScreen := False;
     End;

    If Value and 32 = 32 Then DisablePaging := True;

  End;
  UpdateDisplay;
End;

Procedure Page7FFDTransparent(Value, Paged: Byte);
Var
  Idx: Word;
  TempByte: Byte;
Begin

  For Idx := $6b To $c2 Do
     Memory[23296 + (Idx - $6b)] := Rom128k[Idx];

  TempByte := Value And 7;
  CopyMemory(@RAMBanks[Paged], @Memory[49152], 16384);        // Update the current RAM page with current contents.
  CopyMemory(@Memory[49152], @RAMBanks[TempByte, 0], 16384);  // Copy the Bank to be paged in to upper memory

End;




Procedure SetRAMDisk;
Begin

  // If not already done (ie, when a LOAD! command is performed after a reset),
  // Sets up various 128k Sysvars for use by the RAMDisk.

  If Not n128kAvailable Then Exit;

  If RAMDiskNeedsInit Then Begin

     PutWord(@Memory[$5B83], $ebec); // Pointer to End of catalogue marker in RAM 7
     RAMBanks[7, $ebec + $a - 49152] := $00; // Marker in RAM 7 (3 Bytes)
     RAMBanks[7, $ebec + $b - 49152] := $C0;
     RAMBanks[7, $ebec + $c - 49152] := $00;

     PutWord(@Memory[$5B85], $2BEC); // Space free in RAMdisk (17 bit)
     Memory[$5B87] := $01;

     RAMDiskNeedsInit := False;

  End;

End;

Function InsertRAMDiskCodes(Text: AnsiString): AnsiString;
Var
  InString: Boolean;
  TempStr: AnsiString;
  Idx, Idx2: Integer;
Begin

  // Check for LOAD!, SAVE!, VERIFY! and insert number codes if necessary.

  Result := '';
  Idx := 1;
  InString := False;

  While Idx <= Length(Text) Do Begin
     If Text[Idx] = '"' then InString := Not InString;
     If Not InString And (Text[Idx] in [#214, #239, #248]) Then Begin
        // Possible LOAD, SAVE, VERIFY - RamDisk?
        Idx2 := Idx;
        TempStr := Text[Idx];
        Repeat
           Inc(Idx2);
           TempStr := TempStr + Text[Idx2];
           If Text[Idx2] = '"' Then InString := Not InString;
        Until Text[Idx2] >= #33;
        If Text[Idx2] = #33 Then Begin
           // It's a RAMDisk command - get it and pad out with 5Byte numerics.
           Repeat
              Inc(Idx2);
              If Text[Idx2] = '"' Then InString := Not InString;
              TempStr := TempStr + Text[Idx2];
           Until (Idx2 = Length(Text)) or (Not InString and (Text[Idx2] in [#13, #58]));
           TempStr := Insert5Bytes(TempStr);
        End;
        Result := Result + TempStr;
        Idx := Idx2;
     End Else
        Result := Result + Text[Idx];
     Inc(Idx);
  End;
  Result := Result + #0;
End;


Procedure ROMTrap;
Var
  Col,Row, LoadLen, MemPtr, F, Idx, ModAddr, PokeAddr, TempIX, TempWord, DataLoadLen: Word;
  TempByte, MemAtIX, ChkSum, SaveByte: Byte;
  NeedProgPosUpdate, n128Command, TapeOp, TempBool1, InString, Done: Boolean;
  TokenStr, TempStr, RegionSet: AnsiString;
  KeyState: TKeyboardState;
  TAPSaveStart,
  TAPSaveLen: Word;
  Loop: Integer;
Label
  TapeLoad, Finished;
Begin
  // ROM Traps
  Trapped := True;

  //if Trapcounter>0 Then TrapCounter:=TrapCounter+1;
  //if (TrapCounter>0) Then DebugLog(IntToStr(TrapCounter)+'. '+ IntToStr(Registers.PC-1)+': ['+ IntToStr(ElapsedFrames) +':'+ IntToStr(Registers.TotalTS)+'] S:' +IntToStr(GetWord(@Memory[$5C1A])) );

	Case Registers.PC Of

     {//interrupt handler trap to periodically check for some integrity.
     $39:
     Begin
        //CheckAndHealROM;
        EmulatePUSH((Registers.A shl 8) + Registers.F);
        EmulatePUSH((Registers.H shl 8) + Registers.L);
        Registers.PC := $003A;
     End;
     }

     $0297:
        Begin
           // Prevent the ROM keyboard routine from getting keys from the ports,
           // as it screws up the INPUT routines in BASin, with the PC key support.
           SendNextToken;
           If (Memory[FLAGS] And 32) = 32 Then Registers.F := Registers.F or 64;
           EmulateRET;
        End;
     $04C3:
        Begin
           // Trap Saves.
           If Registers.A = $00 Then Begin
              // Saving the header. Grab it now,
              // and we can use it for info later on.
              // IX Points to the header.
              FileHeader := GetMemoryString(Registers.IX, 17, Memory);
              // Exit through the normal route.
              Registers.PC := $053F;
           End Else Begin
              If Opt_TapeTrapSave Then Begin

                 // Get the body of the file.

                 TAPSaveStart := Registers.IX;
                 TAPSaveLen := GetWord(@Registers.E);
                 FileBody := '   ';
                 PutWord(@FileBody[1], TAPSaveLen + 2);  // write length of the block
                 FileBody[3] := AnsiChar(Registers.A);        // flag byte (in A reg)


                 ChkSum := Registers.A;                  // checksum starts as flag byte
                 If TAPSaveLen <> 0 Then
                    For Loop := 0 to TAPSaveLen-1 Do Begin
                       SaveByte := Memory[TAPSaveStart + Loop];
                       FileBody := FileBody + AnsiChar(SaveByte);
                       ChkSum := ChkSum Xor SaveByte;
                    End;
                 FileBody := FileBody + AnsiChar(ChkSum);     // finally, write the checksum

                 // Prepare the header checksum
                 ChkSum := 0;
                 For Loop := 1 to Length(FileHeader) Do
                    ChkSum := ChkSum Xor Ord(FileHeader[Loop]);

                 FileBody := '  ' + AnsiChar(0) + FileHeader + AnsiChar(ChkSum) + FileBody;
                 PutWord(@FileBody[1], 19);

                 // Now write the tape block

                 TapeWindow.ListView1.Selected := nil;
                 TapeBlockAdd(FileBody);
                 TapeWindow.UpdateTapeList;

              End Else Begin
                 // Fileheader contains the previously saved Header, so
                 // now get the type of save:
                 Case Ord(FileHeader[1]) of
                    0: Begin
                          BASinOutput.GetBASIC;
                          BASinOutput.RepaintBASIC(True);
                          SaveProgram;
                       End;
                    1: Begin
                          SaveDataNum;
                       End;
                    2: Begin
                          SaveDataStr;
                       End;
                    3: Begin
                          SaveCode;
                       End;
                 End;
              End;
              If Registers.PC <> 8 Then // If no errors then...
                 Registers.PC := $053F; // Exit via SA/LD-RET
           End;
        End;
     $0557:
        Begin
           // Trap the LOAD as it happens.
           LoadLen := GetWord(@Registers.E);
           If Registers.A = $00 Then Begin
              // If we haven't yet LOADed a header, then do so now.
              // There is a return address on the stack, and then
              // IX is next. This Stacked IX points to the 2nd "header"
              // that the ROM will use to test that the file loaded matches
              // What we expect. What we are expecting is at (IX-17).
              TempIX := Registers.IX -$11;
              MemAtIX := Memory[TempIX];
              If Opt_TapeTrapLOAD and (TapeBlocks.Count > 0) and (TapePosition < TapeBlocks.Count) Then Begin
                 // LOADing from a Tape Image
                 // Get the header + Body from the current tape block.
              TapeLoad:
                 // If the block is headerless then don't grab it now - do that when the next LOAD trap fires.
                 If TapeBlocks[TapePosition][3] = #$00 Then Begin
                    FileHeader := Copy(TapeBlocks[TapePosition], 4, 17);
                    DataLoadLen := GetWord(@FileHeader[12]);
                    FileBody := Copy(TapeBlocks[TapePosition], 25, DataLoadLen +1);
                    Inc(TapePosition);
                    If Opt_TapeRewind Then
                       If TapePosition >= TapeBlocks.Count Then
                          TapePosition := 0;
                    TapeWindow.UpdateTapeList;
                    ShowWindow(TapeWindow, False);
                 End Else
                    FileBody := 'HEADERLESS';
              End Else Begin
                 Case MemAtIX of
                    0: Begin
                          LoadProgram;

                       End;
                    1: Begin
                          LoadDATANum;
                       End;
                    2: Begin
                          LoadDATAStr;
                       End;
                    3: Begin
                          LoadCode;
                       End;
                 End;
                 If Opt_TapeTrapLOAD and (TapeBlocks.Count > 0) and (TapePosition < TapeBlocks.Count) Then Begin
                    // Did the load end up loading from a tape image? If so, then loop back to catch the block.
                    Goto TapeLoad;
                 End;
              End;
              // If no errors, then proceed.
              NeedVarsUpdate := True;
              If Registers.PC <> $0806 Then Begin
                 // Now we have a header (and a body for later use), we dump it to
                 // the memory pointed to by IX.
                    For MemPtr := Registers.IX to Registers.IX+LoadLen-1 Do
                       Memory[MemPtr] := Ord(FileHeader[(MemPtr-Registers.IX)+1]);
                 FileHeaderLoc := Registers.IX;
                 // Emulate a RET To complete the load.
                 EmulateRET;
                 // Loaded a program? If so, add its' name to the MRU list.
                 If MemAtIX = 0 Then Begin
                    BASinOutput.UpdateCursorPos(1, False);
                    if (trim(Copy(ExtractFilename(Filename),1,8))<>'autoback') Then Begin
                        BASinOutput.AddToMRUList(Filename);

                    End;
                    FASTMode := False;
                 End;
              End Else Begin
                 If Memory[0] = $FF Then Begin           // We've returned from a cancelled LOAD menu item.
                    LoadEmulationState(TempState, True); // Restore the emulation state
                    BASinOutput.UpdateRuntimeButtons;
                    Memory[0] := $FD;                    // Doing this here prevents the next test from triggering.
                 End;
              End;

              If Memory[0] = $FF Then Begin
                 // This is a LOAD "" from the menu, proceeding to load.
                 // So copy the tempstate to the undostate.
                 CopyMemory(@UndoState, @TempState, SizeOf(TEmulationState));
              End;
              Memory[0] := $FD; // Set the DI back at PC:0000
           End Else Begin
              // If A is $FF then we are expecting the BODY of the file.
              If FileHeader = '' Then Exit;
              If Opt_TapeTrapLOAD Then
                 If (TapeBlocks.Count > 0) and (TapePosition < TapeBlocks.Count) Then Begin
                    If FileBody = 'HEADERLESS' Then Begin
                       If TapeBlocks[TapePosition][3] = #$FF Then Begin // This is a headerless block
                          FileBody := Copy(TapeBlocks[TapePosition], 4, 999999);
                          FileHeader[1] := #0; // Headerless blocks, by their nature, can overwrite the BASIC, so make sure it gets updated.
                       End;
                       Inc(TapePosition);
                       If Opt_TapeRewind Then
                          If TapePosition = TapeBlocks.Count Then
                             TapePosition := 0;
                       TapeWindow.UpdateTapeList;
                    End;
                 End Else Begin
                    EmulateRET;
                    Exit;
                 End;


              LoadLen := GetWord(@FileHeader[12]);
              For MemPtr := Registers.IX to Registers.IX+LoadLen-1 Do Begin

                 Memory[MemPtr] := Ord(FileBody[(MemPtr-Registers.IX)+1]);
                   End;
              // If this was a program, then we need to issue an update message
              // if the BASIC window is open. We also need to clear breakpoints and
              // suchlike, as they are no longer relevant.
              If FileHeader[1] = #0 Then Begin
                 BASinOutput.ClearDebugPoints;
                 BASinOutput.GetBASIC;
                 BASinOutput.FormResize(nil);

                 if Filename<>'' Then Begin       //Set title for TAP files //arda
                        SetProjectName(Filename);
                        If (paramstr(2)='-dumptxt') Then Begin   //add commandline option for dump TXT file  arda
                                SaveBAS(False, False);
                                SetLength(FileArray, Length(FileBody));
                                CopyMemory(@FileArray[0], @FileBody[1], Length(FileBody));
                                If Lowercase(ExtractFileExt(Filename)) <> '.txt' Then Filename := Filename + '_BASIC.txt';
                                If SaveFile Then Begin
                                        DumpDoneQuit:=true;
                                End;
                        End;
                        //SetProjectName(ExtractFilename(Filename));
                 end else begin
                        SetProjectName(trim(Copy(FileHeader,1,10)));
                 end;                             //end arda

                 PostMessage(BASinOutput.Handle, WM_UPDATEPROGRAM, 1, 0);
                 PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
                 If ProfilingEnabled Then
                    NewProfile;
              End;
              // And Emulate a RET to exit.
              EmulateRET;
              // Also save the state here for later retrieval if necessary.
              If FileHeader[1] = #0 Then Begin
                 SaveEmulationState(BREAKState);
              End;
           End;
        End;
     $05CC:
        Begin
           // Error in the 128k ROM trap
           SaveByte := Memory[EmulatePOP];
           Memory[ERR_NR] := SaveByte;
           PageROM(False);
           Registers.PC := $132D;
           If CurrentCommand in [207, 210, 213, 214, 239, 248] Then Begin
              // If this was a RAMDisk error then restore SP, as it's been corrupted.
              Registers.SP := GetWord(@Memory[$5b81])+4;
              // Set the RAMDisk to how it was before the error
              PutWord(@Memory[$5B83], GetWord(@RamDiskArray[3]));
              PutWord(@Memory[$5B85], GetWord(@RamDiskArray[0]));
              Memory[$5B87] := RamDiskArray[2];
           End;

        End;
     $063B:
        Begin
           If Memory[0] <> $FF Then Begin // If it's $FF Then Filename was specified previously.
              // LOAD Trap for filenames
              // Grab the filename for later use.
              // DE points to the filename, BC is the length.
              Filename := GetMemoryString(GetWord(@Registers.E), GetWord(@Registers.C), Memory);
           End;
           // We will use this later on.
           // Skip past 10 CHAR filenames limitation - but maintain SAVE tests.
           Registers.zByte1 := Memory[T_ADDR];
           If Registers.zByte1 <> 0 Then Begin
              If Not Opt_TapeTrapLOAD Then
                 // We're LOADing, so anything goes.
                 Registers.PC := $064B  //ardafix--previously $0644 (reported by ignacobo) v1795
              Else
                 If Registers.F and 1 = 0 Then
                    Registers.PC := $64B
                 Else
                    Registers.PC := $63C;
           End Else Begin
              // SAVE detected, but null filenames are legal now (Save As)
              If Opt_TapeTrapSAVE Then Begin
                 If Registers.F and 1 = 0 Then
                    Registers.PC := $64B
                 Else
                    Registers.PC := $63C;
              End Else
                 If Filename <> '' Then
                    Registers.PC := $064B
                 Else
                    Registers.PC := $0644;
           End;
        End;
     $0B53:
        Begin
           // Select the UDG cut off point for 128k or 48k mode
           If ProgramIs128k Then Begin
              If Registers.A < $A3 Then Begin
                 Registers.F := Registers.F or 1;
                 Dec(Registers.A, $A5);
              End Else
                 Dec(Registers.A, $A3);
           End Else Begin
              If Registers.A < $A5 Then
                 Registers.F := Registers.F or 1;
              Dec(Registers.A, $A5);
           End;
        End;
     $0BC6:
        Begin
           //arda begin

        // Output printed character to console



             if TrapPrint then Begin
                tempWord:=((Registers.H shl 8)+Registers.L) -1;
                tempWord:=((tempWord-getword(@Memory[CHARS])) div 8 )and $FF;

                //RomPrintOutputWindow.Memo1.Text:= RomPrintOutputWindow.Memo1.Text+ chr(tempByte)+ '* ';
             End;

             //EX DE,HL
             tempByte:=Registers.D;
             Registers.D:=Registers.H;
             Registers.H:=tempByte;
             tempByte:=Registers.E;
             Registers.E:=Registers.L;
             Registers.L:=tempByte;


             //DEC H

             tempByte := (Registers.H- 1) and $FF;

            Registers.F := Registers.F and $01;

            if tempByte = 0 then Registers.F := Registers.F or $40;      // Z
            if (tempByte and $80) <> 0 then Registers.F := Registers.F or $80; // S    
            if (Registers.H and $0F) = 0 then Registers.F := Registers.F or $10; // H
            if (tempByte = $7F) then Registers.F := Registers.F or $04; // P/V (overflow)
            Registers.F := Registers.F or $02; // N
            Registers.H:= tempByte;


             if TrapPrint then Begin

              tempbyte:=tempWord;
              tempWord := (((Registers.H shr 3) and $03) or $58) shl 8 or Registers.L;

              // Attribute adresinden satir/sütun hesapla
              col := (tempWord -22528 ) mod 32;
              row := (tempWord -22528 ) div 32;
              //RomPrintOutputWindow.Memo1.Text:= RomPrintOutputWindow.Memo1.Text+ Chr(tempByte)+ ':'+ inttoStr(tempWord)+' | ';

              if (row >= 0) and (row < 24) and (col >= 0) and (col < 32) then
              begin
              // Modify Memo1
              TempStr := RomPrintOutputWindow.Memo1.Lines[row];
              TempStr[col+1] := Chr(tempByte);
              RomPrintOutputWindow.Memo1.Lines[row] := TempStr;
              End;

              end;



             Registers.PC := $0BC7;

          //arda end
        End;


    $0DB0:
      Begin
        //CLS Trap for Screen Text Capture
        //Emulate LD HL,0
        Registers.H:=0;
        Registers.L:=0;
        Registers.PC:=$0DB2;

        if TrapPrint Then RomPrintOutputWindow.InitSpectrumScreen;


      End;

    $0E3C:
      Begin
        //SCROLL TRAP
        Registers.H:=$FF;
        Registers.L:=$E0;
        Registers.PC:=$0E3E;
        if TrapPrint Then RomPrintOutputWindow.ScrollOneUp;

      End;


     $0C11:
        Begin
           // Switch to the real token table at $95 or the new one at $386E depending on 128k mode
           If ProgramIs128k Then
              PutWord(@Registers.E, $386E)
           Else
              PutWord(@Registers.E, $0095);
           Inc(Registers.PC, 2);
        End;
     $0C37:
        Begin
           // Test for lower chars for trailing space.
           If ProgramIs128k Then Begin
              If (Registers.A > 1) and (Registers.A < 5) Then
                 Registers.F := Registers.F or 1;
           End Else Begin
              If Registers.A < 3 Then
                 Registers.F := Registers.F or 1;
           End;
        End;
     $0C4F:
        Begin
           // Test for lower chars for trailing space, again.
           If ProgramIs128k Then Begin
              If Registers.A < $22 Then
                 If Not (Registers.A in [0, 1]) Then
                    Registers.F := Registers.F or 1;
           End Else Begin
              If Registers.A < $20 Then
                 Registers.F := Registers.F or 1;
           End;
        End;

		$0F2D:
			Begin
				// Editor De-Tokeniser trap.
           FullSpeed := True;
				PutWord(@Registers.L, GetWord(@Memory[$5C3D]));
				TempStr := GetEditLine;
				TokenStr := DeTokeniseLine(TempStr, False);
				PutEditLine(TokenStr, Memory);
				Inc(Registers.PC, 2);
           SelStartAddr := GetWord(@Memory[K_CUR]);
           If CommandWindow.Visible Then CommandWindow.UpdateButton;
			End;
     $0F39:
        Begin // A small trap for the multiline clipboard paste.
           FullSpeed := False;
           If ClipList.Count > 0 Then Begin
              Registers.A := 13;
              Registers.PC := $0F3B;
           End Else Begin
              Dec(Registers.PC);
              EmulateCall($15D4);
              If Editing Then Begin
                 VariablesWindow.BuildVarsList;
                 PageROM(False);
                 CheckFor128kCommands;
                 BASinOutput.PopUp;
              End;
           End;
           NeedParseUpdate := True;

        End;
     $0F80:
        Begin
           // Trap for inserting Colour items in the edit line,
           // overwriting the selection if necessary.
           // Colour Items take *two* chars!
           If SelStartAddr <> GetWord(@Memory[K_CUR]) Then ClearSelection;
           SelStartAddr := GetWord(@Memory[K_CUR])+2;
           Registers.PC := $0F8B;
        End;
     $0F86:
        Begin
           // Trap for inserting normal (alpha-numeric) tokens in the edit line,
           // overwriting the selection if necessary.
           If SelStartAddr <> GetWord(@Memory[K_CUR]) Then ClearSelection;
           SelStartAddr := GetWord(@Memory[K_CUR])+1;
           PutWord(@Registers.L, SelStartAddr-1);
           Inc(Registers.PC, 2);
        End;
     $0F94:
        Begin
           // A CHAR 14d has been Sent? If so, there may be a special op to perform
           Registers.D := 0;
           Inc(Registers.PC);
           If NeedAutoList Then Begin
              // An automatic refresh of the BASIC display!
              Dec(Registers.PC, 3);
              EmulateCall($1795);
              NeedAutoList := False;
           End;
           If InputDelete and (Registers.A = 14) Then Begin
              // Remove the character at the cursor, if not at line end.
              If SelStartAddr <> GetWord(@Memory[K_CUR]) Then Begin
                 If GetWord(@Memory[K_CUR]) < SelStartAddr Then Begin
                    ClearSelection;
                    DecWord(@Memory[K_CUR]);
                 End Else
                    ClearSelection;
                 SelStartAddr := GetWord(@Memory[K_CUR]);
                 InputDelete := False;
              End Else Begin
                 TempStr := GetEditLine;
                 If Memory[FLAGX] And 32 = 32 Then
                    ModAddr := GetWord(@Memory[WORKSP])
                 Else
                    ModAddr := GetWord(@Memory[E_LINE]);
                 ModAddr := GetWord(@Memory[K_CUR]) - ModAddr;
                 If Length(TempStr) = 1 Then
                    TempStr := #13
                 Else
                    TempStr := Copy(TempStr, 1, ModAddr)+Copy(TempStr, ModAddr+2, 999999);
                 ModAddr := GetWord(@Memory[K_CUR]);
                 PutEditLine(TempStr, Memory);
                 PutWord(@Memory[K_CUR], ModAddr);
                 SelStartAddr := ModAddr;
                 InputDelete := False;
              End;
           End;
        End;
		$0FF0:
			Begin
				// EDIT key trap
				TempStr := GetEditLine;
				TokenStr := DeTokeniseLine(TempStr, False);
				PutEditLine(TokenStr, Memory);
				Dec(Registers.SP, 2);
				PutWord(@Memory[Registers.SP], $0FF2);
				Registers.PC := $1615;
			End;
     $1016:
        Begin
           // Trap the DELETE operation.
           If SelStartAddr <> GetWord(@Memory[K_CUR]) Then Begin
              ClearSelection;
              EmulateRET;
              SelStartAddr := GetWord(@Memory[K_CUR]);
           End Else Begin
              Dec(Registers.PC);
              EmulateCALL($1031);
              If Memory[FLAGX] And 32 = 32 Then
                 SelStartAddr := GetWord(@Memory[WORKSP])
	            Else
                 SelStartAddr := GetWord(@Memory[E_LINE]);
              SelStartAddr := Max(GetWord(@Memory[K_CUR])-1, SelStartAddr);
           End;
        End;
		$1028:
			Begin
				// Tokeniser trap
				PutWord(@Memory[ERR_SP], GetWord(@Registers.L));
           Memory[ERR_NR] := $FF;
				If (Memory[FLAGX] and 160) in [0, 32, 128] Then Begin
					TempStr := GetEditLine;
					TokenStr := TokeniseLine(TempStr, False);
              TokenStr := InsertRAMDiskCodes(TokenStr);
					PutEditLine(TokenStr, Memory);
           End;
				Inc(Registers.PC, 2);
           If CommandWindow.Visible Then CommandWindow.UpdateButton;
			End;
     $10A0, $0FEC, $1012:
        Begin
           // Trap to set the Selection Start address to K_CUR
           If (Registers.PC = $1012) Then Begin
              If DWord(GetKeyState(VK_SHIFT)) <= 1 Then // Shift is Pressed?
                 SelStartAddr := GetWord(@Registers.L);
           End Else Begin
              SelStartAddr := GetWord(@Registers.L);
           End;
           PutWord(@Memory[K_CUR], GetWord(@Registers.L));
           Inc(Registers.PC, 2);
        End;
     $1142:
        Begin
           // Trap the "is this line too long" routine -
           // Should in reality just CALL the OUT-LINE2 routine, but we modify the offset (HL)
           // if the line is too long.
           Dec(Registers.PC);
           EmulateCALL($187D);
           TempWord := GetWord(@Registers.L);
           If TempWord = GetWord(@Memory[E_LINE]) Then Begin
              // This is an Edit line
              TempWord := GetWord(@Memory[WORKSP]) - TempWord;
              If TempWord > 672 Then Begin
                 TempWord := GetWord(@Registers.L)+(TempWord - 672);
                 PutWord(@Registers.L, TempWord);
              End;
           End Else Begin
              // an INPUT line.
              ModAddr := TempWord;
              While Memory[ModAddr] <> 13 Do Inc(ModAddr);
              TempWord := ModAddr - TempWord;
              If TempWord > 672 Then Begin
                 TempWord := GetWord(@Registers.L)+(TempWord - 672);
                 PutWord(@Registers.L, TempWord);
              End;
           End;
        End;
     $11B9:
        Begin
           If Not CheckForSave Then Begin
              LoadEmulationState(UndoState, True);
              ControlEmulation(False);
              BASinOutput.Running := False;
           End Else Begin
              Registers.A := $FF;
           End;
        End;
     $11DD:
        Begin

            // Reset Sequence Trap 1
           BASinOutput.UpdateCursorPos(1, False);
           If ResetCounter = 0 Then Begin
                if Opt_FastResets Then Begin
                //SpeedBackup:=Opt_CPUSpeed ; //arda no more needed
                //Opt_CPUSpeed := 200000;
                        FASTMode := True; //arda
                        ResetSound; //arda
                end;

              // restore RAM banks etc if necessary
              CopyMemory(@RAMBanks[PagedBank], @Memory[49152], 16384);
              PageROM(False);
              // This is the first call,
              // So wipe memory, insert the ROM afresh and reset all the
              // ROM traps.
              If Registers.B = 0 Then Begin
                 // If B is zero, this is a reset - anything else is NEW, and needs UDGs etc preserving.
                 For F := 23296 to 65535 Do
                    Memory[F] := 0;
                 RAMDiskNeedsInit := True;
              End Else Begin
                 // Need to preserve the RAMDisk contents for later if this is NEW.
                 If Not RAMDiskNeedsInit Then Begin // RAM Disk has been used?
                    For F := 23296 to 23551 Do
                       RAMDiskBank[F] := Memory[F];
                    RAMDiskBank[0] := $FF;
                 End;
              End;
              Filename := BASinDir+'\48.Rom';
              //LoadROM(Memory);
	            ModifyROM;
              DisplayWindow.BringToFront;
              ProgStateFlag := PS_Reset;
              NeedParseUpdate := True;
              LastError := 0;
              LastErrorLine := 65534;
              LastErrorStatement := 0;
              CursorType := 'K';
              PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
              Memory[GetWord(@Registers.L)] := $02; // LD (HL),$02
              Inc(Registers.PC);
              Inc(ResetCounter);
              Registers.HaltEmu := False;
              
           End Else Begin
              Memory[GetWord(@Registers.L)] := $02; // LD (HL),$02
              Inc(Registers.PC);
              Inc(ResetCounter);
              If ResetCounter = 2388 Then Begin
                 ResetCounter := 1;
                 if not Opt_FastResets Then Registers.HaltEmu := True;
              End;
           End;
           CLS_LOWER := True;
        End;
     $11E4:
        Begin
           // Reset Sequence Trap 2
           OpED52; // SBC HL,DE
           Inc(ResetCounter);
           If ResetCounter >= 1000 Then Begin
              ResetCounter := 1;
              if not Opt_FastResets Then Registers.HaltEmu := True;
           End;
           CLS_LOWER := True;
        End;
     $11F0:
        Begin
           // Reset Sequence Trap 3
           Op2B; // DEC HL
           OpD9; // EXX
           ResetCounter := 0;
           Registers.HaltEmu := False;
           Dec(Registers.PC);
           BASinOutput.ClearDebugPoints;
           // If the BASIC window is open, then signal that the
           // contents will have changed.
           BASinOutput.Running := False;
           BASinOutput.SingleStep := False;
           BASinOutput.RunLine := 65536;
           BASinOutput.GOTOStatement := 1;
           If BASinOutput.RunningAck <> BASinOutput.Running Then
              PostMessage(BASinOutPut.Handle, WM_UPDATEPROGBUTTONS, 0, 0);
           PostMessage(BASinOutput.handle, WM_UPDATEPROGRAM, 0, 0);
           PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
           SetProjectName(DefaultProjectName);
           ProjectSaved := False;
           FASTMode := False;
           BorderUpdate := True;
           CLS_LOWER := True;
           //if Opt_fastresets then Opt_CpuSpeed:=SpeedBackup;

        End;
     $121A:
        Begin
           // Initialise RAMTOP and the GOSUB Stack vars.
           PutWord(@Memory[RAMTOP], GetWord(@Registers.L));
           Inc(Registers.PC, 2);
           // Set to the top of the GOSUB stack, zero entries.
           GOSUBStackpos := GetWord(@Registers.L);
           GOSUBStackSize := 0;
           If GOSUBWindow.Visible Then
              GOSUBWindow.ClearStack;
        End;
     $12A1:
        Begin
           // restore the RAMDisk if necessary following a NEW
           If Not RAMDiskNeedsInit Then Begin
              For F := 23296 to 23551 Do
                 Memory[F] := RAMDiskBank[F];
              RAMDiskBank[0] := 0;
           End;
           // Continue into MAIN-1
           Registers.PC := $12A9;
        End;
     $12B5:
        Begin
           // Error Restore trap
           ProgStateFlag := PS_Unknown;
           RestoreErrors;
           Dec(Registers.PC);
           EmulateCALL($1B17);
        End;
     $12C4:
        Begin
           // An Error occurred - If there's a clipboard multi-line paste pending, allow it through
           // as BASIC will trap the error at runtime or if the user edits the offending lines.
           If ClipList.Count > 0 Then Begin
              // Jump to MAIN-3 and allow the error...
              Registers.PC := $12CF;
           End Else Begin
              // Else stop with the red flashing cursor.
              PutWord(@Registers.L, GetWord(@Memory[E_LINE]));
              Inc(Registers.PC, 2);
           End;
        End;
     $12EA:
        Begin
           // If we're not reporting errors ourselves, and an error-clear is pending then
           // we need to CALL $0D6E.
           If CLS_LOWER Then Begin
              Dec(Registers.PC);
              EmulateCall($0D6E);
              CLS_LOWER := False;
           End Else
              Registers.PC := $12EC;
        End;
     $12ED:
        Begin
           // Syntax Helper - RUN program trap.
           // Also the point that a Direct Command has just been entered.
           PageROM(False);
           Registers.A := $19;
           Inc(Registers.PC);
           NeedParseUpdate := True;
           BASinOutput.Running := True;
           CPUWindow.CPURunning := True;
           BASinOutput.Running := True;
           RestoreErrors;
           If BASinOutput.RunningAck <> BASinOutput.Running Then Begin
              PostMessage(BASinOutPut.Handle, WM_UPDATEPROGBUTTONS, 0, 0);
              PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
              PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
           End;
           EditLineToHistory;
           If ProfilingEnabled Then
              NewProfile;
           If CPUWindow.Showing Then CPUWindow.Formshow(nil);
        End;
     $132E:
        Begin
           // Program Stop (error report) trap.
           {LastError := Memory[ERR_NR]+1;
           LastErrorLine := GetWord(@Memory[PPC]);
           LastErrorStatement := Memory[SUBPPC];
           If LastError > 28 Then Dec(LastError);
           NeedParseUpdate := True;
           ProgStateFlag := PS_Stopped; }

           LastError := Memory[ERR_NR]; // ardafix 109
           If LastError = 255 Then
             LastError := 0  // 255 ('OK') ise indeksi 0 yap
           Else
             Inc(LastError);  // 0-42 ('1'-'?') ise indeksi 1-43 yap
           // --- DÜZELTME SONU ---

           LastErrorLine := GetWord(@Memory[PPC]);
           LastErrorStatement := Memory[SUBPPC];
           // If LastError > 28 Then Dec(LastError); // end ardafix 109
           NeedParseUpdate := True;
           ProgStateFlag := PS_Stopped;

           // As the program or direct command has just finished,
           // If there are any visible watches, they need updating.
           UpdateWatches;
           BASinOutput.UpdateMenu;
           // Set some debugger flags, and if the BASIC window is open,
           // update it.
           BASinOutput.Running := False;
           BASinOutput.SingleStep := False;
           BASinOutput.RunLine := 65536;
           BASinOutput.GOTOStatement := 1;
           If BASinOutput.RunningAck <> BASinOutput.Running Then
              PostMessage(BASinOutPut.Handle, WM_UPDATEPROGBUTTONS, 0, 0);
           // Now, if we asked to be notified of errors, then test the notification flag and
           // Raise the info window.
           If Opt_CursorToError Then
              If (LastErrorLine > 0) and (LastErrorLine < 10000) Then
                 BASinOutput.FindAndActivateLine(LastErrorLine, LastErrorStatement);
           BASinOutput.WantNewLine := True;
           ResetSound;
           If ErrorAddresses[LastError].Notify Then Begin
              CLS_LOWER := False;
              Registers.PC := $1362;
              PutWord(@Registers.F, GetWord(@Memory[Registers.SP]));
              Inc(Registers.SP, 2);
              DisplayWindow.SizeDIBs;
              If AddCodeWindow.Memo1.Lines.Count = 0 Then ErrorForm.ShowError(True);
              DisplayWindow.WantsFront := False;
              ControlEmulation(True);
           End Else Begin
              CLS_LOWER := True;
              DisplayWindow.WantsFront := True;
              Dec(Registers.PC);
              EmulateCall($0D6E);
              ControlEmulation(True);
           End;
           // Also drop back to speccy speed
           ContinueReady := True;
           TempBool1 := FASTMode;
           FASTMode := False;
           FASTMode := TempBool1;
        End;
     $1335:
        Begin
           // Trap to sort out error codes.
           // Need to POP AF and LD B,A - luckily this is the behaviour we want to override.
           PutWord(@Registers.F, EmulatePOP);
           Registers.B := Registers.A;
           If LastError > 28 Then Begin
              TempStr := ErrorAddresses[LastError].Desc;
              Registers.B := 0;
              Registers.A := Ord(TempStr[1]) - 48 - 7;
              TempWord := ErrorAddresses[LastError].Address;
              For F := 3 To Length(TempStr)-1 Do Begin
                 Memory[TempWord] := Ord(TempStr[F]);
                 Inc(TempWord);
              End;
              Memory[TempWord] := Ord(TempStr[Length(TempStr)])+$80;
              ErrorAddresses[27].Modified := True;
           End;
        End;
     $155E:
        Begin
           // Another Fullspeed Trap - Accelerates adding a line to a long program.
           PutWord(@Memory[E_PPC], GetWord(@Registers.C));
           Inc(Registers.PC, 3);
           FullSpeed := True;
        End;
     $15A0:
        Begin
           // Very close to the next trap, but this will only fire on a proper line
           // (hopefully), rather than a simple line clear.
           PutWord(@Registers.L, GetWord(@Memory[$5C49]));
           Registers.PC := $15A2;
           BASinOutput.UpdateMenu;
           EditLineToHistory;
        End;
     $15AD:
        Begin
           // The UPDATEPROGRAM trap - updates the BASIC runtime listing
           // if visible. Also set the highlight to the new line.
           // Line Number in DE at this point.
           PostMessage(BASinOutput.handle, WM_UPDATEPROGRAM, GetWord(@Registers.E), 0);
           Registers.PC := $12A2;
        End;
     $15E3:
        Begin
           // Traps the Edit WAIT-KEY routine to open the BASIC editor
           // replacement if needed.
           Registers.PC := $15DE;
        End;
     $16D0:
        Begin
           // Begin the EDIT process - so test for multi-line Clipboard paste.
           // Also a nice time to start inserting BASIC into the program Memory,
           // If Necessary.
           Registers.PC := $16D2;
           SelStartAddr := GetWord(@Memory[K_CUR]);
           PutWord(@Memory[MEM], GetWord(@Registers.L));
           If GetWord(@Memory[Registers.SP+2]) = $12AC Then Begin
              If ClipList.Count > 0 Then Begin
                 PutEditLine(GetClipText(0), Memory);
                 ClipList.Delete(0);
                 PutWord(@Registers.L, GetWord(@Memory[Registers.SP]));
                 Inc(Registers.SP, 2);
                 EmulateRET;
              End;
              If InsertList.Count > 0 Then Begin
                 While InsertList.Count > 0 Do Begin
                    InsertLine(InsertList[0], True);
                    InsertList.Delete(0);
                 End;
              End;
           End;
        End;
     $1796:
        Begin
           // Accelerate the AutoLIST feature of BASIC.
           FullSpeed := True;
           PutWord(@Memory[LIST_SP], Registers.SP);
           Inc(Registers.PC, 3);
        End;
     $17F1:
        Begin
           // End the AutoLIST acceleration
           Memory[TV_FLAG] := Memory[TV_FLAG] and 239;
           Inc(Registers.PC, 3);
           FullSpeed := False;
        End;
     $1834:
        Begin
           // Traps the edit cursor, so that the BASIC listing window
           // follows the current edit line cursor. Trapped for LIST and AutoList
           PostMessage(BASinOutput.handle, WM_UPDATEPROGCURSOR, 0, 0);
           Inc(Registers.PC);
           Registers.E := $01;
        End;
     $187E:
        Begin
           // Accelerate (2 of 2 traps) the EDIT line display
           Memory[FLAGS] := Memory[FLAGS] or 1;
           Inc(Registers.PC, 3);
           FullSpeed := True;
        End;


     $189D:
        Begin
           // When an error occurs, suppress the "?" and use a red cursor instead.
           // Also Move the cursor to the error.
           PutWord(@Memory[K_CUR], GetWord(@Memory[X_PTR]));
           SelStartAddr := GetWord(@Memory[X_PTR]);
           Registers.A := Memory[GetWord(@Memory[K_CUR])];
           If Registers.A = 13 Then Begin
              Registers.A := 32;
              Registers.PC := $18A1;
              EmulateCall($18C1);
           End Else
              PutWord(@Registers.E, GetWord(@Registers.E)+1);
        End;
     $18B0:
        Begin
           // OUT-CHAR call, trapped to invert colours if address is inside the selection.
           Dec(Registers.PC);
           EmulateCALL($1937);
           MemPtr := GetWord(@Memory[K_CUR]);
           // ModAddr is the address of the CHAR to be printed.
           ModAddr := GetWord(@Registers.E)-1;
           If Memory[FLAGX] And 32 = 32 Then
              TempWord := GetWord(@Memory[WORKSP])
           Else
              TempWord := GetWord(@Memory[E_LINE]);
           If Not AttrsSaved Then Begin
              AttrsSaved := False;
              If (ModAddr >= TempWord) and (MemPtr > 255) Then
                 If SelStartAddr < MemPtr Then Begin
                    If (ModAddr >= SelStartAddr) and (ModAddr < MemPtr) Then Begin
                       SelSaveAttrs := Memory[ATTR_T];
                       Memory[ATTR_T] := (SelSaveAttrs And 192)+((SelSaveAttrs and 7) Shl 3)+((SelSaveAttrs and 56) Shr 3);
                       AttrsSaved := True;
                    End;
                 End Else If SelStartAddr > MemPtr Then Begin
                    If (ModAddr > MemPtr) and (ModAddr <= SelStartAddr) Then Begin
                       SelSaveAttrs := Memory[ATTR_T];
                       Memory[ATTR_T] := (SelSaveAttrs And 192)+((SelSaveAttrs and 7) Shl 3)+((SelSaveAttrs and 56) Shr 3);
                       AttrsSaved := True;
                    End;
                 End;
           End;
        End;
     $18B3:
        Begin
           // Restore Selection colours
           If AttrsSaved Then Begin
              Memory[ATTR_T] := SelSaveAttrs;
              AttrsSaved := False;
           End;
           Registers.PC := $1894;
        End;
     $18B5:
        Begin
           // Knock off EDIT line acceleration
           If Memory[TV_FLAG] and 16 = 0 Then FullSpeed := False;
           PutWord(@Registers.E, GetWord(@Memory[Registers.SP]));
           Inc(Registers.SP, 2);
           EmulateRET;
        End;
     $18C7:
        Begin
           // A BRIGHT cursor! in OUT-FLASH
           PutWord(@Registers.L, 249);
           // Is this an error?
           MemPtr := GetWord(@Memory[Registers.SP+2]);
           If (MemPtr = $18A1) or (MemPtr = $18A4) Then Begin
              // If so, set the colours to red/white.
              // Set RED, and knock off BLUE (or we get purple errors...)
              Registers.L := (Registers.L or 250) And 254;
              CursorType := '?';
              PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
           End;
           Registers.PC := $18CA;
        End;
     $18E9:
        Begin
           // Get the correct cursor before the modified ROM alters it.
           Case (Memory[MODE] and 2) of
              0, 1:
                 Begin
                    // KLC mode - Further processing required
                    If Memory[FLAGX] and 32 = 32 Then
                       CursorType := 'L'
                    Else Begin
                       TempStr := TokeniseLine(GetEditLine, False);
                       CursorType := 'K';
                       If Length(TempStr) > 0 Then Begin
                          For F := 1 to Length(TempStr) Do Begin
                             If TempStr[F] > #32 Then Begin
                                If ProgramIs128k Then Begin
                                   If TempStr[F] > #162 Then CursorType := 'L';
                                End Else
                                   If TempStr[F] > #164 Then CursorType := 'L';
                                If TempStr[F] in [':', #203] Then CursorType := 'K';
                             End;
                          End;
                       End;
                    End;
                    If CursorType = 'L' Then
                       If Memory[FLAGS2] and 8 = 8 Then
                          CursorType := 'C';
                 End;
              2:
                 Begin
                    // Graphics Mode
                    CursorType := 'G'
                 End;
           End;
           Registers.A := Memory[MODE];
           Inc(Registers.PC, 2);
           PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
        End;
     $190B:
        Begin
           // Modify the cursor to "overprint" the current CHAR.
           // Get the correct character.
           Dec(Registers.PC);
           EmulateCall($18C1);
           ModAddr := GetWord(@Memory[Registers.SP+2]);
           Registers.A := Memory[ModAddr];
           If Registers.A < 31 Then Begin
              If Registers.A = 13 Then Begin
                 // A carriage return, the end of the edit line.
                 Registers.A := 32;
              End Else Begin
                 // Less than a Space, probably a colour control.
                 Registers.A := Ord('?');
              End;
           End Else
              PutWord(@Memory[Registers.SP+2], ModAddr+1);
        End;
     $1B2A:
        Begin
           // Blocks the SPECTRUM command with a simple RET
           If In128k Then EmulateRet;
        End;
     $1B34:
        Begin
           // This trap simulates the LD B, $00 in STMT-L-1, and if the
           // BASIC window is open, finds the current line/statement.
           // also handles (during runtime) some of the Debugging features.
           Inc(Registers.PC);
           FullSpeed := False;
           Registers.B := $00;
           If Memory[FLAGS] and 128 = 128 Then Begin       // Not checking syntax?
              If NeedVarsUpdate Then Begin
                 PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
                 NeedVarsUpdate := False;
              End;
              CurPPC := GetWord(@Memory[PPC]);
              If CurPPC <> 65534 Then Begin                // Valid Line number?
                 CurSUBPPC := Memory[SUBPPC];
                 NeedProgPosUpdate := True;
                 If BreakpointsList[CurPPC][CurSUBPPC] <> #0 Then Begin
                    // Test for Breakpoints - and stop if necessary.
                    If TestBreakpoint(Ord(BreakpointsList[CurPPC][CurSUBPPC])) Then Begin
                       ControlEmulation(False);
                    End;
                    NeedProgPosUpdate := True;
                 End Else If BASinOutput.AbortStatement Then Begin
                    // The GO TO... Menu item was used, and the current statement needs
                    // To be stopped. This is simply achieved by setting A to a #13 carriage
                    // return - the following cp and jump will catch this.
                    PutWord(@Memory[NXTLIN], BASinOutput.NXTLIN);
                    Registers.A := 13;
                    BASinOutput.AbortStatement := False;
                    NeedProgPosUpdate := True;
                 End;
                 // Process Single Step, Next Line and RUN TO.
                 If BASinOutput.SingleStep Then Begin
                    ControlEmulation(False);
                    BASinOutput.SingleStep := False;
                    NeedProgPosUpdate := True;
                 End Else If BASinOutput.NextLine Then Begin
                    If CurSUBPPC = 1 Then Begin
                       ControlEmulation(False);
                       BASinOutput.NextLine := False;
                       NeedProgPosUpdate := True;
                    End;
                 End Else If BASinOutput.RunLine <> 65536 Then Begin
                    // More complex than a singlestep or nextline operation
                    // Get the current line/statement number, and only stop if
                    // the RUNTO has been reached.
                    If CurPPC = BASinOutput.RunLine Then
                       If CurSUBPPC = BASinOutput.RunStatement Then Begin
                          ControlEmulation(False);
                          BASinOutput.RunLine := 65536;
                          NeedProgPosUpdate := True;
                       End;
                 End Else
                    If BASinOutput.GotoLine <> 0 Then
                       If (BASinOutput.GOTOStatement <> 1) or (BASinOutput.GOTOLine <> CurPPC) Then Begin
                       // A total kludge. It traps the single step point, and
                       // if a GOTO is outstanding, calls the GOTO routine *again*
                       // which sets up in runtime. It works though.
                       SetGOTOPoint(BASinOutput.GOTOLine, BASinOutput.GotoStatement);
                       BASinOutput.GOTOStatement := 1;
                       BASinOutput.GOTOLine := 0;
                       CurKeyDown := 0;
                    End;
                 // We also need to update all the watches now, if they're visible.
                 // We do this *now* so they can stop the emulation if necessary.
                 If UpdateWatches Then
                    ControlEmulation(False);
                 // If the runtime window is not visible, it should be.
                 If Not Registers.EmuRunning Then Begin
                    BASinOutput.UpdateRunTimeButtons;
                    If SysVarsWindow.Visible and SysVarsWindow.CanUpdate Then
                       SysVarsWindow.UpdateSysVars(0);
                    BASinOutput.BringToFront;
                 End;
                 // Finally, test for REM debug commands
                 If Registers.A = 234 Then Begin
                    TempStr := Lowercase(GetMemoryString(GetWord(@Memory[CH_ADD])+1, 5, Memory));
                    If TempStr = 'fast'+#13 Then Begin
                       FASTMode := True;
                       ResetSound;
                       BorderUpdate := True;
                    End Else If TempStr = 'slow'+#13 Then Begin
                       FASTMode := False;
                       Opt_CPUSpeed := 69888;
                       BorderUpdate := True;
                    End Else If copy(TempStr, 1, 3) = 'log' Then Begin
                       TempStr := '';
                       F := GetWord(@Memory[CH_ADD])+4;
                       While Memory[F] <> 13 Do Begin
                          TempStr := TempStr + AnsiChar(Memory[F]);
                          Inc(F);
                       End;
                       LogTextExpression(CurPPC, CurSUBPPC, TempStr);
                    End Else If TempStr = 'speed' Then Begin
                       TempStr := '';
                       F := GetWord(@Memory[CH_ADD])+6;
                       While Memory[F] <> 13 Do Begin
                          TempStr := TempStr + AnsiChar(Memory[F]);
                          Inc(F);
                       End;
                       Opt_CPUSpeed := 4935*strtoint(TempStr);
                       //19742=1mhz
                       //4935=0.25 mhz
                    End;
                 End;
                 If NeedProgPosUpdate Then
                    If Opt_FollowProgram or Not Registers.EmuRunning Then
                       PostMessage(BASinOutPut.Handle,
                                   WM_UPDATEPROGCURSOR,
                                   CurPPC,                    // Line Number
                                   CurSUBPPC);                // Statement
              End;
           End;
        End;
     $1B45:
        Begin
           // Command is in C - is it SPECTRUM/PLAY/CAT/ERASE/MERGE/VERIFY/LOAD/SAVE?
           If Registers.C in [163, 164, 207, 210, 213, 214, 239, 248] Then Begin
              n128Command := True;
              Idx := GetWord(@Memory[CH_ADD]);
              While Memory[Idx] < 33 Do
                 Inc(Idx);
              If Registers.C in [207, 210, 213, 214, 239, 248] Then
                 n128Command := Memory[Idx] = 33;
              If n128Command and (Memory[FLAGS] and 128 = 0) then Begin
                 // Scanning, and need to skip any 128k Commands.
                 Memory[ERR_NR] := $FF;
                 Idx := GetWord(@Memory[CH_ADD]);
                 Done := False;
                 InString := False;
                 While Not Done Do Begin
                    If Memory[Idx] = 34 Then InString := Not InString;
                    If Memory[Idx] in [58, 203] Then
                       If Not InString Then
                          Done := True;
                    If Memory[Idx] = 13 Then
                       Done := true;
                    Inc(Idx);
                 End;
                 // Update the current position, to the next statement
                 PutWord(@Memory[CH_ADD], Word(Idx -1));
                 // And set PC back a bit to re-scan for the next command.
                 Registers.PC := $1B2C;
              End Else Begin
                 If n128Command Then Begin
                    Memory[ERR_NR] := $FF;
                    CurrentCommand := Registers.C;
                    Registers.C := Registers.A;
                    EmulatePUSH($1B52);
                    Registers.PC := $1B6C;
                    // Need to add a profiling point here, as it will get skipped by the above PC change.
                    If ProfilingEnabled Then Begin
                       F := Registers.PC;
                       Registers.PC := $1B56;
                       AddProfileEntry;
                       Registers.PC := F;
                    End;
                 End Else Begin
                    If Registers.A < $CE Then
                       Registers.F := Registers.F or 1
                    Else
                       Registers.F := Registers.F and 254;
                    CurrentCommand := Registers.A;
                    Dec(Registers.A, $CE);
                 End;
              End;
           End Else Begin
              // It's a normal command or error. Carry on with the SUB $CE and
              // then allow the jump to go ahead if carry (error) is set.
              If Registers.A < $CE Then
                 Registers.F := Registers.F or 1
              Else
                 Registers.F := Registers.F and 254;
              CurrentCommand := Registers.A;
              Dec(Registers.A, $CE);
           End;
        End;
     $1B53:
        Begin
           // 128k trap - finished the PLAY.
           If In128k Then Begin
              PageROM(False);
              PutWord(@Memory[CH_ADD], GetWord(@Memory[CH_ADD])-1);
              Registers.PC := $1B28;
              EmulatePop;
           End Else Begin
              PutWord(@Registers.L, GetWord(@Memory[T_ADDR]));
              Registers.PC := $1B55;
           End;
        End;
     $1B6E:
        Begin
           // This trap will launch the 128k commands.
           If (CurrentCommand in [163, 164, 207, 210, 213, 214, 239, 248]) and (Memory[FLAGS] and 128 = 128) Then Begin
              If n128kAvailable and (CurrentCommand <> 163) Then Begin
                 If CurrentCommand in [207, 210, 213, 214, 239, 248] Then Begin // CAT, ERASE, MERGE, VERIFY, LOAD, SAVE
                    Idx := GetWord(@Memory[CH_ADD]);
                    While Memory[Idx] < 33 Do
                       Inc(Idx);
                    If Memory[Idx] <> 33 Then Begin
                       Dec(Registers.B);
                       If Registers.B = 0 Then
                          Registers.F := Registers.F or 64
                       Else
                          Registers.F := Registers.F and 191;
                       EmulateRET;
                       Goto Finished;
                    End;
                 End;
                 CurROM := 0;
                 PageROM(True);
                 PutWord(@RamDiskArray[3], GetWord(@Memory[$5B83]));
                 PutWord(@RamDiskArray[0], GetWord(@Memory[$5B85]));
                 RamDiskArray[2] := Memory[$5B87];
                 Case CurrentCommand of
                    164: Begin Registers.PC := $2336; End; // PLAY
                    207: Begin Registers.PC := $1C04; SetRAMDisk; End; // CAT
                    210: Begin Registers.PC := $1C2B; SetRAMDisk; End; // ERASE
                    213: Begin Registers.PC := $121F; Memory[$5B66] := 1; Memory[$5C74] := 3; SetRAMDisk; End; // MERGE
                    214: Begin Registers.PC := $1218; Memory[$5B66] := 1; Memory[$5C74] := 2; SetRAMDisk; End; // VERIFY
                    239: Begin Registers.PC := $1211; Memory[$5B66] := 1; Memory[$5C74] := 1; SetRAMDisk; End; // LOAD
                    248: Begin Registers.PC := $120A; Memory[$5B66] := 1; Memory[$5C74] := 0; SetRAMDisk; End; // SAVE
                 End;
              End Else Begin
                 // 128k Commands are not available, so skip them.
                 Memory[ERR_NR] := $FF;
                 Idx := GetWord(@Memory[CH_ADD]);
                 Done := False;
                 InString := False;
                 While Not Done Do Begin
                    If Memory[Idx] = 34 Then InString := Not InString;
                    If Memory[Idx] in [58, 203] Then
                       If Not InString Then
                          Done := True;
                    If Memory[Idx] = 13 Then
                       Done := true;
                    Inc(Idx);
                 End;
                 // Update the current position, to the next statement
                 PutWord(@Memory[CH_ADD], Word(Idx -1));
                 // And skip to the "next statement" routine - by popping off HL, BC and then STMT-RET
                 Registers.PC := EmulatePOP;
                 Registers.PC := EmulatePOP;
                 EmulateRET;
              End;
           End Else Begin
              Registers.B := (Registers.B - 1) and $FF; //avoiding range check error ardafix 109 dec(B)
              If Registers.B = 0 Then
                 Registers.F := Registers.F or 64
              Else
                 Registers.F := Registers.F and 191;
              EmulateRET;
           End;
        End;
     $1B56:
        Begin
           // Profiling trap.
           If ProfilingEnabled Then
              AddProfileEntry;
           Registers.A := Memory[GetWord(@Registers.L)];
           IncWord(@Registers.L);
        End;
     $1DC5:
        Begin
           // Trap the NEXT routine to update the vars window
           Dec(Registers.PC);
           EmulateCall($1DDA);
           NeedVarsUpdate := True;
        End;
     $1E84:
        Begin
           // Trap POKES that set the CAPSLOCK on or off.
           PokeAddr := GetWord(@Registers.C);
           Memory[PokeAddr] := Registers.A; // LD (BC),A
           If PokeAddr = FLAGS2 Then Begin
              GetKeyboardState(Keystate);
              If Registers.A and 8 = 8 Then Begin // POKE will turn CAPS on
                 If Keystate[VK_CAPITAL] = 0 Then Begin
                    keybd_event(VK_CAPITAL, 0, 0, 0);
                    keybd_event(VK_CAPITAL, 0, KEYEVENTF_KEYUP, 0);
                 End;
              End Else // Turning CAPS off.
                 If Keystate[VK_CAPITAL] = 1 Then Begin
                    keybd_event(VK_CAPITAL, 0, 0, 0);
                    keybd_event(VK_CAPITAL, 0, KEYEVENTF_KEYUP, 0);
                 End;
           End Else Begin
              If PokeAddr > GetWord(@Memory[UDG]) Then // Poked a UDG?
                 If TokenForm.Visible Then
                    TokenForm.PageControl1Change(nil);
              If (PokeAddr > GetWord(@Memory[CHARS])+255) and (PokeAddr < GetWord(@Memory[CHARS])+1023) Then Begin // Poked to CHARS?
                 If TokenForm.Visible Then
                    TokenForm.PageControl1Change(nil);
                 BASinOutput.RepaintBASIC(True);
              End;
           End;
           EmulateRET; // RET
        End;
     $1EDE:
        Begin
           // Another GOSUB Stack Reset - a carbon copy of the RESET sequence trap.
           // Initialise RAMTOP and the GOSUB Stack vars.
           PutWord(@Memory[RAMTOP], GetWord(@Registers.L));
           Inc(Registers.PC, 2);
           // Set to the top of the GOSUB stack, zero entries.
           GOSUBStackpos := GetWord(@Registers.L);
           GOSUBStackSize := 0;
           If GOSUBWindow.Visible Then
              GOSUBWindow.ClearStack;
        End;
     $1F03:
        Begin
           // GOSUB Trap - Increment the GOSUB Stack size
           Registers.PC := $1F05;
           PutWord(@Registers.C, $0014);
           If GOSUBStackSize < $FFFF Then Begin
              Inc(GOSUBStackSize);
              Dec(GOSUBStackPos, 3);
              If GOSUBWindow.Visible Then
                 GOSUBWindow.PushOne(GetWord(@Memory[PPC]), Memory[SUBPPC]);
           End;
        End;
     $1F2F:
        Begin
           // RETURN Trap - Decrement the GOSUB Stack Size.
           Registers.PC := $1F32;
           PutWord(@Memory[ERR_SP], Registers.SP);
           If GOSUBStackSize > $0000 Then Begin
              Dec(GOSUBStackSize);
              Inc(GOSUBStackPos, 3);
              If GOSUBWindow.Visible Then
                 GOSUBWindow.PopOne;
           End;
        End;
     $1F3B:
        Begin
           // Detect PAUSE Start
           Dec(Registers.PC);
           EmulateCALL($1E99);
           ProgStateFlag := PS_Paused;
        End;
     $1F50:
        Begin
           // Detect end of PAUSE.
           Memory[Registers.IY+1] := Memory[Registers.IY+1] and 223; // RES 5, (IY+01)
           Registers.PC := $1F53;
           ProgStateFlag := PS_Unknown;
        End;
     $1F55:
        Begin
           // A BREAK trap
				Registers.F := Registers.F Or 1;
           If ((FindControl(GetForegroundWindow()) <> nil) and (DWord(GetKeyState(VK_ESCAPE)) > 1)) or WantBreak Then Begin
             // CurProjectFilename:=FindControl(GetForegroundWindow());
              BASinOutput.SetCaption;
              Registers.F := Registers.F And -2;
              WantBreak := False;
           End;
           EmulateRET;
        End;
     $208F:
        Begin
           // Trap the INPUT variable update - see $2B00 below for info.
           // This is another of "those" points.
           Registers.A := $01;
           Registers.PC := $2090;
           SelStartAddr := GetWord(@Memory[K_CUR]);
           NeedVarsUpdate := True;
        End;
		$2647:
			Begin
				// The key decode trap for INKEY$
				If CurKeyDown <> 0 Then Begin
              If (DWord(GetKeyState(VK_SHIFT)) > 1) or (DWord(GetKeyState(VK_CAPITAL) and 1) = 1) or (Memory[FLAGS2] and 8 = 8) Then Begin
                 If AnsiChar(CurKeyDown) in ['a'..'z'] Then
                    CurKeyDown := CurKeyDown -32;
              End Else Begin
                 If AnsiChar(CurKeyDown) in ['A'..'Z'] Then
                    CurKeyDown := CurKeyDown +32;
              End;
					Registers.A := CurKeyDown;
					// Set FLAGS to no key delay
              Memory[FLAGS] := Memory[FLAGS] Or 8;
					Registers.PC := $2657;
				End Else Begin
					Registers.F := Registers.F and -65; // Set NZ for no key
              Registers.PC := $2649;
				End;
        End;
     $2B00:
        Begin
           // Vars Window - Variable is updated in LET - with a couple of exceptions,
           // all variable alterations pass through here.
           NeedVarsUpdate := True;
           Registers.PC := $2B02;
           PutWord(@Registers.L, GetWord(@Memory[$5C4D]));
        End;
     $2C22:
        Begin
           // This is one of them - a DIM assignment.
           Registers.B := 0;
           Registers.PC := $2C23;
           NeedVarsUpdate := True;
        End;
	End;

Finished:

  If DisplayWindow.WantsFront Then Begin
     If Not DisplayWindow.Visible Then ShowWindow(DisplayWindow, False);
     DisplayWindow.BringToFront;
     DisplayWindow.WantsFront := False;
  End;

  Trapped := False;

End;

Procedure ModifyROM;
Var
  Idx, MemIdx: Integer;
  Str: AnsiString;
Begin

  DebugLog('Modify ROMs');

  Trapped := False;

  // 48K ROM TRAPS

  // Just for a laugh... Commodore mode!
  // Memory[$11CD] := $05; // Cyan Border
  // Memory[$1266] := $0D; // Cyan on Blue


  //trap ROM Interrupt to check rom integrity (uncomment to enable. see CheckAndHealROM )
  //PutWord(@Memory[$38], $00ED);

	// Never use K or E mode cursor
	// G, L, C modes work fine.

	PutWord(@Memory[$18F6], $DECB);
	PutWord(@Memory[$18FC], $0000);
	PutWord(@Memory[$18EB], $02FE);
	PutWord(@Memory[$18EF], $473E);
	PutDWord(@Memory[$0352], $00000000);
	PutDWord(@Memory[$0356], $00000000);

  // Various mods which alter the keyboard behaviour

  Memory[$10E7] := $0F;
	Memory[$18ED] := $38;
	Memory[$18F9] := $4C;
	Memory[$1900] := $00;
	Memory[$033D] := $0C;
	Memory[$0364] := 201;

  // Trap the POKE command

  PutWord(@Memory[$1E83], $00ED);

  // Trap the KEY-LINE routine

  PutWord(@Memory[$0296], $00ED);

  // Trap the cursor routine - so we can display the correct cursor
  // in the Status Bar.

  PutWord(@Memory[$189C], $00ED);
  PutWord(@Memory[$18E8], $00ED);
  PutWord(@Memory[$190A], $00ED);
  PutWord(@Memory[$18C6], $00ED);

	// Alter Copyright. And why not?

	// PutDWord(@Memory[$153B], $33303032); //arda, and why?

	// Fix the scroll-into-ROM bug, as the ROM is currently
	// unprotected.

	Memory[$0D2C] := $17;  //patch lower part of the screen scroll 17 times instead of 18

	// $ED Unused Opcode Trap for Keyboard (INKEY$)

	PutWord(@Memory[$2646], $00ED);

	// $ED Unused Opcode Trap for the tokeniser

	PutWord(@Memory[$1027], $00ED);
	PutWord(@Memory[$0F2C], $00ED);
	PutWord(@Memory[$0FEF], $00ED);

	// $ED Unused Opcode Trap for BREAK detection

 	PutWord(@Memory[$1F54], $00ED);

  // $ED Unused Opcode Trap for the Error Reconstruction
  // (See Procedure DoError() for Details)

  PutWord(@Memory[$12B4], $00ED);

  // Traps to implement file loading
  // First, a trap to enable us to get the filename the user specified,
  // before the ROM truncates it to 10 chars max.

  PutWord(@Memory[$063A], $00ED);

  // Now a trap at LD-BYTES, where we can get the information about
  // what the user wants to LOAD, be it "", CODE, DATA or SCREEN$

  PutWord(@Memory[$0556], $00ED);

  // And another patch at SA_BYTES, so we can do similar for saving :)

  PutWord(@Memory[$04C2], $00ED);

  // Patch the ROM to bypass the "Start tape, and press any key" message,
  // and remove the 1 second pause between header and body.

  PutWord(@Memory[$097A], $0000);
  PutWord(@Memory[$0981], $0000);
  Memory[$097C] := $00;
  Memory[$0983] := $00;
  Memory[$0990] := $01;

  // Patch the PAUSE command, so that now
  // a keypress will not prematurely end a pause
  // unless it is PAUSE 0. This means for instance,
  // that PAUSE 1 can be used as a WAIT VBL command.
  // The only key that can stop a pause command is now the BREAK key.
  {
  Memory[$1F47] := $F5;
  PutWord(@Memory[$1F4F], $00ED);
  PutWord(@Memory[$1F3A], $00ED);
  }
  // Patch the ROM Reset Routine - it moves at normal speccy
  // speed now. This is entirely due to the fact that it looks
  // cool, and BASin is poorer without a visible reset
  // sequence.

  PutWord(@Memory[$11B8], $00ED); // Trap to test for save
  PutWord(@Memory[$11DC], $00ED); // Trap 1 - Fill with $02
  PutWord(@Memory[$11E3], $00ED); // Trap 2 - Test and clear
  PutWord(@Memory[$11EF], $00ED); // Trap 3 - cleanup Trap vars.

  // Now for a Patch to trigger the Syntax helper (if enabled).

  PutWord(@Memory[$0F38], $00ED); // This traps the editor loop, for each keypress
  PutWord(@Memory[$12EC], $00ED); // This traps the command runner
  PutWord(@Memory[$132D], $00ED); // This traps the program stop (Error) routine.
  PutWord(@Memory[$12E9], $00ED); // Trap the call to CLS-LOWER. Don't want that if we're reporting the errors ourselves.

  // Trap the Editor's program line entry routine
  // and update the window when the program is altered.

  PutWord(@Memory[$15AC], $00ED);
  PutWord(@Memory[$1833], $00ED);

  // Trap the "Editing, waiting for a key" routine -
  // The one routine called every time an edit is in progress.
  // If this fires, and we have asked to, we open the Editor replacement.

  PutWord(@Memory[$15E2], $00ED);

  // Trap the line pointer as the program runs.

  PutWord(@Memory[$1B33], $00ED);

  // Variables Window update traps

  PutWord(@Memory[$2AFF], $00ED); // FOR
  PutWord(@Memory[$2C21], $00ED); // DIM
  PutWord(@Memory[$208E], $00ED); // INPUT
  PutWord(@Memory[$1DC4], $00ED); // NEXT

  // A trap to get the program line just successfully entered into the command
  // history.

  PutWord(@Memory[$159F], $00ED);

  // Trap the GOSUB Stack updates

  PutWord(@Memory[$1219], $00ED); // NEW
  PutWord(@Memory[$1EDD], $00ED); // CLEAR
  PutWord(@Memory[$1F02], $00ED); // GOSUB
  PutWord(@Memory[$1F2E], $00ED); // RETURN

  // Accelerating the AutoLIST and the EDIT line makes the editor more fun to use.

  PutWord(@Memory[$1795], $00ED); // Two Traps for the AutoLIST
  PutWord(@Memory[$17F0], $00ED);
  PutWord(@Memory[$187D], $00ED); // A trap for the EDIT line
  PutWord(@Memory[$18B4], $00ED); // And one to knock off Acceleration.
  PutWord(@Memory[$155D], $00ED);

  // Alter the table to point to a null entry for a character 14 in input,
  // and also a trap to catch that for a DELETE operation.

  Memory[$0FA7] := 202;
  PutWord(@Memory[$0F93], $00ED);

  // Multi-line program pasting, anyone?

  PutWord(@Memory[$16CF], $00ED);
  PutWord(@Memory[$12C3], $00ED);

  // Selection Traps and Modifications.

  PutWord(@Memory[$0FEB], $00ED); // Set Selection to K_CUR
  PutWord(@Memory[$1011], $00ED); //  ""      ""       ""
  PutWord(@Memory[$109F], $00ED); //  ""      ""       ""

  PutWord(@Memory[$18AF], $00ED); // Invert colours if inside Selection
  PutWord(@Memory[$18B2], $00ED); // Restore Colours

  PutWord(@Memory[$0F7F], $00ED); // Clear Selection - Adding a colour item
  PutWord(@Memory[$0F85], $00ED); // Clear Selection - Adding a normal CHAR
  PutWord(@Memory[$1015], $00ED); // Trap DELETE to clear selection.

  PutWord(@Memory[$1141], $00ED); // Fix the "line too long" bug

  // Profiling Trap - other trap is in the core, HALT instruction.

  PutWord(@Memory[$1B55], $00ED);

  // Copy the token table at $0095 out to $386E, with the two new commands SPECTRUM and PLAY.

  Memory[$386E] := 128 + Ord('?'); // Skip-over byte
  MemIdx := $386F;

  Str := 'SPECTRUM';               // Insert "SPECTRUM".
  For Idx := 1 To Length(Str) -1 Do Begin
     Memory[MemIdx] := Ord(Str[Idx]);
     Inc(MemIdx);
  End;
  Memory[MemIdx] := 128 + Ord(Str[Length(Str)]);
  Inc(MemIdx);

  Str := 'PLAY';                   // Insert "PLAY".
  For Idx := 1 To Length(Str) -1 Do Begin
     Memory[MemIdx] := Ord(Str[Idx]);
     Inc(MemIdx);
  End;
  Memory[MemIdx] := 128 + Ord(Str[Length(Str)]);
  Inc(MemIdx);

  For Idx := $96 To $204 Do Begin  // Copy the old token table over.
     Memory[MemIdx] := Memory[Idx];
     Inc(MemIdx);
  End;

  PutWord(@Memory[$0C10], $00ED);  // Insert the ROM trap which will switch in the 128k set if necessary
  putWord(@Memory[$0B52], $00ED);  // Insert the trap which checks for UDGs or tokens
  PutWord(@Memory[$0C36], $00ED);  // Set a trailing space for SPECTRUM and PLAY.
  PutWord(@Memory[$0C4E], $00ED);  // Again, set a leading space.

  PutWord(@Memory[$1B44], $00ED);  // Trap at STMT-L-1 to initialise PLAY/SPECTRUM if necessary.
  PutWord(@Memory[$1B6D], $00ED);  // Trap launch the PLAY routine (if necessary) into the 128k ROM.

  // 128K Command Traps

  PutWord(@Memory[$12A0], $00ED);  // Trap the very end of the NEW sequence to set up the RAMDisk again.
  PutWord(@Memory[$1334], $00ED);  // Trap error reports in general so we can display the 128k messages.
  PutWord(@Rom128k[$05CB], $00ED); // Trap the 128k Error report.
  PutWord(@Rom128k[$1B52], $00ED); // Trap the PLAY return - This is actually a 48k ROM trap, but we trap it in the 128k ROM.
  PutWord(@Rom128k[$234F], $0000); // Remove the EOL test in PLAY, as we don't need it.
  Rom128k[$2351] := 0;             // That takes three bytes to NOP out a CALL

  For Idx := 0 To 16383 Do
     Rom48k[Idx] := Memory[Idx];  // backup copy patched rom

End;



Procedure CheckFor128kCommands;
Var
  InString, REMCommand: Boolean;
  Addr, AddrLimit, Idx: DWord;
Begin

  // Parses the current BASIC Memory for 128k commands - SPECTRUM, PLAY and
  // Any tape commands suffixed by a "!".

  InString := False;
  REMCommand := False;
  UsesUDGsTU := False;
  ProgramIs128k := False;

  Addr := GetWord(@Memory[PROG]);
  AddrLimit := GetWord(@Memory[VARS]);

  While Addr < AddrLimit Do Begin

     If Memory[Addr] = 34 Then InString := Not InString;
     If Memory[Addr] = 13 Then Begin
        REMCommand := False;
        InString := False;
        Inc(Addr, 5);
     End Else
        //If Memory[Addr] = 37 Then
        //               Spectranet := True; //A Spectranet program -by arda-!

        If Memory[Addr] = 14 Then Begin
           If Not (REMCommand or InString) Then
              Inc(Addr, 6)
           Else
              Inc(Addr);
        End Else Begin
           If Memory[Addr] = 234 Then REMCommand := True;
           If Not (REMCommand or InString) Then Begin
              If Memory[Addr] in [163, 164] Then // SPECTRUM, PLAY
                 ProgramIs128k := True
              Else
                 If Memory[Addr] in [207, 210, 213, 214, 239, 248] Then Begin // CAT, ERASE, MERGE, VERIFY, LOAD, SAVE
                    Idx := Addr+1;
                    While Memory[Idx] < 33 Do
                       Inc(Idx);
                    If Memory[Idx] = 33 Then
                       ProgramIs128k := True;   // ! is used
                 End;


           End Else
              If Not REMCommand then
                 If Memory[Addr] in [163, 164] Then // UDGs T/U - not 128k compatible
                    UsesUDGsTU := True;

           Inc(Addr);

        End;

  End;

  If TokenForm.Visible Then
     TokenForm.DrawTokens;

End;

Function GetCursorPos: Word;
Begin
  If Memory[FLAGX] And 32 = 32 Then
     Result := GetWord(@Memory[K_CUR]) - GetWord(@Memory[WORKSP])
	Else
     Result := GetWord(@Memory[K_CUR]) - GetWord(@Memory[E_LINE]);
End;

Function GetCurrentStatement(Line: AnsiString): AnsiString;
Var
  CursorAt: Word;
  StatementStart, StatementEnd: Word;
Begin

  If Line = '' Then Exit;

  // Get the cursor position as an offset from
  // the beginning of the edit line.

  CursorAt := GetCursorPos;

  // Now walk to the end of the current statement -
  // or the end of the line, whichever comes first.

  StatementStart := 1;
  While (CursorAt < Length(Line)) and (Line[CursorAt] <> ':') Do Inc(CursorAt);
  StatementEnd := CursorAt +1;

  // Now return the completed statement.

  Result := Copy(Line, StatementStart, StatementEnd-StatementStart);
  While Result[Length(Result)] = ':' Do Result := Copy(Result, 1, Length(Result)-1);

End;

Function GetEditLine: AnsiString;
Var
	F: DWord;
Begin
	// Returns the current Edit line as a AnsiString of characters.
	Result := '';
	TokenBuffer := '';
	F := Memory[FLAGX];
	If F and 32 = 32 Then
		// In Input Mode - gather from WorkSpace
		F := GetWord(@Memory[WORKSP])
	Else
		// In Edit Mode - gather from Editor buffer
		F := GetWord(@Memory[E_LINE]);
	While Memory[F] <> $0D Do Begin
		Result := Result + AnsiChar(Memory[F]);
		Inc(F);
	End;
End;

Procedure PutEditLine(Tokens: AnsiString; Var Mem: Array of Byte);
Var
	G: DWord;
	InputMode: Boolean;
	LengthMod, TempAddr32: Integer;
	VarAddr, Count, StartAddr, EndAddr, TempWord: Word;
Begin

	// Fills the edit buffer with a new text.

	If Tokens = '' Then Exit;

	// Add the terminator AnsiString $0D - if in editmode, add an extra $80 later
	Tokens := Tokens+#$0D;

	// First need to clear some room - so set the sysvars accordingly.
	// We calculate the difference between what room we have, and set
	// to use the room we need. More often than not, this will be
	// a shrink as the tokeniser converts to tokens.

	InputMode := Mem[FLAGX] And 32 = 32;
  If InputMode Then Begin
		StartAddr := GetWord(@Mem[WORKSP]);
		EndAddr := GetWord(@Mem[STKBOT]);
	End Else Begin
		StartAddr := GetWord(@Mem[E_LINE]);
		EndAddr := GetWord(@Mem[WORKSP]);
		Tokens := Tokens + #$80;
     If opt_AutoList Then
        If Tokens[1] = '.' Then
           Tokens := AutoLine(Tokens);
	End;

	LengthMod := (EndAddr - StartAddr) - Length(Tokens);

	// This simulates the ROM Routine MAKE-ROOM

	VarAddr := VARS;
	Count := 15;
	While Count > 0 Do Begin
		If GetWord(@Mem[VarAddr]) > StartAddr Then Begin
			TempWord := GetWord(@Mem[VarAddr]);
			Dec(TempWord, LengthMod);
			PutWord(@Mem[VarAddr], TempWord);
		End;
		Inc(VarAddr, 2);
		Dec(Count);
	End;

  {
  VarAddr := VARS;
	Count := 15;
	While Count > 0 Do Begin
		TempWord := GetWord(@Mem[VarAddr]); // 16-bit adresi oku
		If TempWord > StartAddr Then Begin
            
            // --- DÜZELTME BASLANGICI ---
            TempAddr32 := TempWord; // 32-bit tamsayiya güvenle ata

            // Matematiksel islemi 32-bit üzerinde yap
            TempAddr32 := TempAddr32 - LengthMod;

            // Sonucu 0-65535 araligina kelepçele (clamp)
            If TempAddr32 > 65535 Then
                TempWord := 65535
            Else If TempAddr32 < 0 Then
                TempWord := 0
            Else
                TempWord := TempAddr32; // Artik 16-bit'e geri atamak güvenli
            // --- DÜZELTME SONU ---
            
			PutWord(@Mem[VarAddr], TempWord);
		End;
		Inc(VarAddr, 2);
		Dec(Count);
	End;
        }
	// Now fill the buffer.

	For G := 1 To Length(Tokens) Do
		Mem[G+StartAddr-1] := Ord(Tokens[G]);

	// K_CUR is a special case, so set it now - but not in input mode,
	// as the cursor will appear outside of the quotes :-)

	If Not InputMode Then
		If Registers.PC <> $0FF0 Then
			// Ensure that we don't set the cursor pos to the very end
			// as that's the $0D80 terminator pair.
			PutWord(@Mem[K_CUR], StartAddr+Length(Tokens)-2)
		Else Begin
			TempWord := StartAddr;
			While Tokens[TempWord - (StartAddr-1)] in ['0'..'9'] Do Inc(TempWord);
			PutWord(@Mem[K_CUR], TempWord);
		End;

  SelStartAddr := GetWord(@Mem[K_CUR]);

	// and clear the token buffer.

	TokenBuffer := '';
  If Not Evaluating Then Begin
     DisplayWindow.BringToFront;
     PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
  End;

End;

Function TokeniseLine(Line: AnsiString; TokeniseStrings: Boolean): AnsiString;
Var
	Index, LastIndex, ReservedIndex, F: Integer;
	CurWord: AnsiString;
	CurChar: AnsiChar;
	InString, GotKeyword, CanGo, IsEditing, DEFFNPresent, IsItReallyDEFFN, MightBeToken: Boolean;
Label
  VarName, Start;
Begin
	// Converts normal text to Sinclair BASIC tokens
  // so that the new BASIC entry system will work.
	Result := '';
	InString := False;
  GotKeyword := False;
  DEFFNPresent := False;
  IsEditing := Editing;
	Index := 1;
	While Index < Length(Line)+1 Do Begin
     Start:
		LastIndex := Index;
		CurChar := Line[Index];
     CanGo := True;
		If ((Not Instring) or TokeniseStrings) and (CurChar in ['A'..'Z', 'a'..'z']) Then Begin
			// Alphas can be tokens, so we need to test if it is.
			// Gather all chars until a non-alpha is encountered.
			// This may be a $ (for Screen$, Inkey$) etc, a # (For Open# etc)
			// Or a Space from a DEF FN, GO TO or GO SUB.
			CurWord := '';
        If IsEditing and Not GotKeyword Then Begin
			   While (CurChar in ['A'..'Z', 'a'..'z']) and (Index < Length(Line)+1) Do Begin
				   CurWord := CurWord + UpperCase(CurChar);
				   Inc(Index);
				   If Index < Length(Line)+1 Then    //ardafix 109
             CurChar := Line[Index]
           Else
             CurChar := #0;  //end ardafix 109
			   End;
        End Else Begin
           // For some insane reason, you cannot have a variable "TO2" in 128k BASIC. This code will
           // trap these, and seperate the "TO" out. I'm not sure if this is a bug that NEEDS fixing, tbh.
           // Also means that variables that start with a keyword are verboten :(
           MightBeToken := True;
			   While (CurChar in ['0'..'9', 'A'..'Z', 'a'..'z']) and (Index < Length(Line)+1) Do Begin
              If CurChar in ['0'..'9'] Then
                 If MightBeToken Then Begin
                    If CurWord = 'GOTO' Then
                       CurWord := 'GO TO'
                    Else
                       If CurWord = 'GOSUB' Then
                          CurWord := 'GO SUB'
                       Else
                          If CurWord = 'DEFFN' Then
                             CurWord := 'DEF FN';
                    ReservedIndex := IndexIsReserved(CurWord);
                    If ReservedIndex <> 0 Then Begin
                       CurWord := Copy(Keywords[ReservedIndex], 1, Pos('-', Keywords[ReservedIndex]) -1);
                       F := 0;
			               While CurWord <> AsciiKeywords[F] Do Inc(F);
                       Result := Result + AnsiChar(F+163);
                       Goto Start;
                    End;
                    MightBeToken := False;
                 End;
				   CurWord := CurWord + UpperCase(CurChar);
				   Inc(Index);
				   CurChar := Line[Index];
           If Index < Length(Line)+1 Then   //ardafix 109 begin
             CurChar := Line[Index]
           Else
             CurChar := #0; //end ardafix
			   End;
               Result := Result;
           // So, for now - I'll add in the original behaviour here, so the fix above can be reversed. The fix is
           // also quite a lot slower on tokenisation, but that should be ok as it's only called when entering a line.

           // While (CurChar in ['0'..'9', 'A'..'Z', 'a'..'z']) and (Index < Length(Line)+1) Do Begin
				//    CurWord := CurWord + UpperCase(CurChar);
				//    Inc(Index);
				//    CurChar := Line[Index];
			   // End;

        End;
			// Now test to see if the word is a token.
			// First, test the "GO" words, "GO TO", "GO SUB", also "DEF" for "DEF FN"
        If CurChar in [' ', '#'] Then Begin
           If (CurWord = 'GO') or (CurWord = 'DEF') Then Begin
				   Inc(Index);
				   CurWord := CurWord + ' ';
				   If Index < Length(Line)+1 Then CurChar := Line[Index];
				   While (CurChar in ['A'..'Z', 'a'..'z']) and (Index < Length(Line)+1) Do Begin
					   CurWord := CurWord + UpperCase(CurChar);
					   Inc(Index);
					   CurChar := Line[Index];
				   End;
			   End Else If (CurWord = 'OPEN') or (CurWord = 'CLOSE') Then Begin
				   If CurChar = '#' Then Begin
					   CurWord := CurWord + ' #';
					   Inc(Index);
				   End Else If CurChar = ' ' Then Begin
					   // May Be more than one space after OPEN or CLOSE
                 While (CurChar = ' ') and (Index < Length(Line)+1) Do Begin
						   Inc(Index);
						   CurChar := Line[Index];
					   End;
					   If CurChar = '#' Then Begin
						   CurWord := CurWord + ' #';
						   Inc(Index);
					   End;
				   End;
           End;
			End;
			// Now Test - is it a reserved word?
			F := 0;
			While (F < 102) And (CurWord <> AsciiKeywords[F]) Do Inc(F);
			If F <> 102 Then Begin
           // Is it one of those odd ones with spaces in the name, or a $?
           IsItReallyDEFFN := False; //arda181
           Case F of
              94: Begin // GOTO
                    F := 73;
                  End;
              95: Begin // GOSUB
                    F := 74;
                  End;
              96: Begin // DEFFN
                    F := 43;
                    IsItReallyDEFFN := True;
                  End;
              97: Begin // INKEY
                    If CurChar = #36 Then Begin
                       F := 3;
                       Inc(Index);
                    End Else
                       Goto VarName;
                  End;
              13: Begin // VAL
                    If CurChar = #36 Then Begin
                       F := 11;
                       Inc(Index);
                    End;
                  End;
              99: Begin // SCREEN
                    If CurChar = #36 Then Begin
                       F := 7;
                       Inc(Index);
                    End Else
                       Goto VarName;
                  End;
             100: Begin // STR
                    If CurChar = #36 Then Begin
                       F := 30;
                       Inc(Index);
                    End Else
                       Goto VarName;
                  End;
             101: Begin // CHR
                    If CurChar = #36 Then Begin
                       F := 31;
                       Inc(Index);
                    End Else
                       Goto VarName;
                  End;
             102: Begin
                    Goto VarName;
                  End;
           End;

	         // Now test - Does the token have a leading space?
           If F > 31 Then Begin
              // Leading Space, Remove the space.
              If Result <> '' Then
		            If Result[Length(Result)] = ' ' Then
			            Result := Copy(Result, 1, Length(Result) -1);

              If F = 43 Then
                DEFFNPresent := True;

              If F = 71 Then Begin
                 // REM tells us to discard the entire of the rest of the line.
                 Result := Result + #234;

				      If Line[Index] = ' ' Then Inc(Index);
                 While Index < Length(Line)+1 Do Begin
                    Result := Result + Line[Index];
                    Inc(Index);
                 End;
                 If DEFFNPresent Then ProcessDEFFN(Result);
                 Exit;
              End;
				End;

				Result := Result + AnsiChar(F+163);
           GotKeyword := True;
				// All Alphabetic keywords have a trailing space,
				// so remove it now.
				If (Index <= Length(Line)) and (Line[Index] = ' ') Then Inc(Index);
			End Else Begin
				// Else copy the word verbatim to the result.
           VarName:
				For F := LastIndex To Index -1 Do
					Result := Result + Line[F];
			End;
			Dec(Index);
		End Else Begin
			// All other symbols can go straight out, with the exception of <>, <= and >=
			If ((Not Instring) or TokeniseStrings) Then
				If CurChar = '>' Then Begin
					// Consider >=
					If Index < Length(Line) Then
						If Line[Index+1] = '=' Then Begin
                    Inc(Index); // Step over the '='
							CurChar := #$C8; // Modify CurChar up to a >=
						End
				End Else If CurChar = '<' Then Begin
					// Consider <= and <>
					If Index < Length(Line) Then
						If Line[Index+1] = '=' Then Begin
                    Inc(Index); // Step over the '='
							CurChar := #$C7; // Modify CurChar up to a <=
						End Else If Line[Index+1] = '>' Then Begin
                    Inc(Index); // Step over the '>'
							CurChar := #$C9; // Modify CurChar up to a <>
						End;
				End Else If CurChar = ':' Then Begin
              GotKeyword := False;
           End Else
              If CurChar = ' ' Then
                 CanGo := False
              Else
                 If CurChar = #$E Then Begin
                    Result := Result + Copy(Line, Index, 6);
                    Inc(Index, 5);
                    CanGo := False;
                 End;

  		// This is a normal AnsiChar.
        If CanGo Then
           Result := Result + CurChar;
			If CurChar = '"' Then InString := Not InString;
		End;
		Inc(Index);
	End;
  If DEFFNPresent Then
     ProcessDEFFN(Result);
  // Finally, strip trailing spaces
  While Copy(Result, Length(Result), 1) = ' '
     Do Result := Copy(Result, 1, Length(Result)-1);
End;

Function DeTokeniseLine(Line: AnsiString; DetokeniseStrings: Boolean): AnsiString;
Var
	Token,
  LastToken,
	TempLine: AnsiString;
	CurChar: Byte;
	Index: Integer;
  InString, REMCommand: Boolean;
Begin
  // Converts a AnsiString of Sinclair BASIC tokens to plain text
	TempLine := Line;
	// Strip leading spaces
	While Copy(TempLine, 1, 1) = ' ' Do TempLine := Copy(TempLine, 2, 999999);
  // Now iterate through, and test for each AnsiChar,
	// if > $A4 then replace with AnsiString from LUT, and pad with spaces.
	Index := 1;
  LastToken := '';
  InString := False;
  REMCommand := False;
	While Index < Length(TempLine)+1 Do Begin
		CurChar := Ord(TempLine[Index]);
     If CurChar = 34 Then InString := Not InString;
     If Not InString and (CurChar = 14) and Not REMCommand Then Begin
        // Skip 5byte floats
        Inc(Index, 5);
     End Else	If (((Not InString) or DetokeniseStrings) or (REMCommand)) and (CurChar > $A2) Then Begin
			// Token found - expand
        If Not REMCommand Then Begin
			   Token := SpaceKeyWords[CurChar - $A3];
			   If Token[1] in ['A'..'Z'] Then Begin
				   // Keyword tokens (as opposed to functions)
				   // have a leading space if:
				   // 1. The token is not the first on the line
				   // 2. There is already no space present before the token.
				   If (CurChar > $C4) or (CurChar in [$A3, $A4]) Then
					   If Result <> '' Then
                    If Copy(LastToken, Length(LastToken), 1) <> ' ' Then
                       Token := ' '+Token;
			   End;
        End Else
           Token := AnsiChar(CurChar);
			Result := Result + Token;
        LastToken := Token;
        If CurChar = $EA Then
           REMCommand := True;
		End Else Begin
			Result := Result + TempLine[Index];
        LastToken := '';
		End;
		Inc(Index);
	End;
	// remove trailing spaces
	While Copy(Result, Length(Result), 1) = ' ' Do
		Result := Copy(Result, 1, Length(Result) -1);
End;

Function EditCursorMove(MoveMode: Byte): Byte;
Var
	STARTAddr,
	EndAddr,
	CursAddr: Word;
	InputMode: Boolean;
Begin

	// Move the cursor in the edit space.
	// HOME, END (2/3) simply set KCUR to the start or
	// End address of the edit space, while UP/DOWN (0/1) add or
	// remove 32 from KCUR if appropriate.

  If TokenBuffer <> '' Then Exit;

	Result := $FF;
	InputMode := Memory[FLAGX] And 32 = 32;
  If InputMode Then Begin
		StartAddr := GetWord(@Memory[WORKSP]);
		EndAddr := GetWord(@Memory[STKBOT]);
	End Else Begin
		StartAddr := GetWord(@Memory[E_LINE]);
		EndAddr := GetWord(@Memory[WORKSP])-1;
	End;
	CursAddr := GetWord(@Memory[K_CUR]);
	Case MoveMode of
		0: Begin // Up
				If CursAddr -32 >= StartAddr Then Begin
					Dec(CursAddr, 32);
					Result := 11;
				End Else
              Result := 11;
			End;
		1: Begin // Down
				If CursAddr +32 < EndAddr Then Begin
					Inc(CursAddr, 32);
					Result := 10;
				End Else Begin
              CursAddr := EndAddr-1;
              Result := 10;
           End;
			End;
		2: Begin // Home
           If (DWord(GetKeyState(VK_CONTROL)) <= 1) Then Begin
				   CursAddr := StartAddr;
				   Result := $0E;
           End Else Begin
              // CTRL-HOME jumps to start of prog.
              If GetWord(@Memory[VARS]) <> GetWord(@Memory[PROG]) Then Begin
                 StartAddr := GetWord(@Memory[PROG]);
                 StartAddr := Memory[StartAddr+1]+(Memory[StartAddr] Shl 8);
                 PutWord(@Memory[E_PPC], StartAddr);
                 BufferToken($0B);
              End;
              Exit;
           End;
			End;
		3: Begin // End;
           If (DWord(GetKeyState(VK_CONTROL)) <= 1) Then Begin
				   CursAddr := EndAddr-1;
				   Result := $0E;
           End Else Begin
              // CTRL-End Jumps to the last line.
              If GetWord(@Memory[VARS]) <> GetWord(@Memory[PROG]) Then Begin
                 StartAddr := GetWord(@Memory[VARS])-2;
                 While Not (Memory[StartAddr] in [13, 128]) Do Dec(StartAddr);
                 If Memory[StartAddr] = 13 Then Begin
                    Inc(StartAddr);
                    StartAddr := Memory[StartAddr+1]+(Memory[StartAddr] Shl 8);
                    PutWord(@Memory[E_PPC], StartAddr);
                    BufferToken($0A);
                 End;
              End;
              Exit;
           End;
			End;
     4: Begin // PgUp
           RetrieveHistory(-1);
           CursAddr := StartAddr;
           Result := $0E;
        End;
     5: Begin // PgDn
           RetrieveHistory(1);
           CursAddr := StartAddr;
           Result := $0E;
        End;
	End;
	PutWord(@Memory[K_CUR], CursAddr);
  If (DWord(GetKeyState(VK_SHIFT)) <= 1) or (MoveMode in [4, 5]) Then
     SelStartAddr := CursAddr;
End;

Function GetMemoryString(Address, Count: Word; Var Mem: Array of Byte): AnsiString;
Begin
  // Grabs a AnsiString of bytes from Spectrum Memory and
  // returns it as a AnsiString.
  Result := '';
  While Count > 0 Do Begin
     Result := Result + AnsiChar(Mem[Address]);
     Inc(Address);
     Dec(Count);
  End;
End;

Procedure DoError(Code: Byte; ErrorText: AnsiString);
Var
  Addr, F: Word;
  B: Byte;
Begin
  // Puts an error AnsiString into the ROM, replacing the error specified.
  // CODE is the Letter of the Error (Eg, R from "R Tape Loading Error")
  Registers.PC := $0806;
  Case Code of
     32: B := Ord('d') - 48 - 7;
     38: B := Ord('k');
     39: B := Ord('l');
     40: B := Ord('m');
     41: B := Ord('n');
     43: B := Ord('o');
  Else
     B := Code;
  End;
  Memory[$0807] := B;
  // Increase the code, as the list is zero based
  If Code < 32 Then Inc(Code);
  Addr := ErrorAddresses[Code].Address;
  For F := 1 To Length(ErrorText)-1 Do Begin
     Memory[Addr] := Ord(ErrorText[F]);
     Inc(Addr);
  End;
  Memory[Addr] := Ord(ErrorText[Length(ErrorText)])+$80;
  // And flag that code for later restoration.
  ErrorAddresses[Code].Modified := True;
End;

Procedure RestoreErrors;
Var
  F, G: Integer;
Begin
  // Restores the error messages that have been modified by a call
  // to DoError().
  For F := 0 To 41 Do Begin
     If ErrorAddresses[F].Modified Then Begin
        For G := 2 To Length(ErrorAddresses[F].Desc) Do
           Memory[(G-2)+ErrorAddresses[F].Address] := Ord(ErrorAddresses[F].Desc[G]);
        ErrorAddresses[F].Modified := False;
        Memory[ErrorAddresses[F].Address+Length(ErrorAddresses[F].Desc)-2] :=
           Ord(ErrorAddresses[F].Desc[Length(ErrorAddresses[F].Desc)])+$80;
     End;
  End;
End;

Function GetLineMatch(Line: Word): Integer; 
Var
  CurAddress, LineNum, TempWord: Word;
Begin

  CurAddress := GetWord(@Memory[PROG]);
  PrevLineAddr := CurAddress;
  LineNum := 65535;
  Result := 0;

  While LineNum <> Line Do Begin

     // Line Numbers are stored big-endian
     TempWord := GetWord(@Memory[CurAddress]);
     LineNum  := (TempWord Shr 8) + ((TempWord and 255) Shl 8);

     // If we're all of a sudden matching, or greater than, we stop.
     If LineNum >= Line Then Begin
        If LineNum > Line Then Result := -CurAddress;
        Break;
     End;

     // Not a match - so now Address the Length
     // Get the line length (includes terminal #13)
     Inc(CurAddress, GetWord(@Memory[CurAddress+2])+4);
     If CurAddress >= GetWord(@Memory[VARS]) Then Begin
        Result := -GetWord(@Memory[VARS]);
        Break;
     End;
  End;

  If Result = 0 Then
     Result := CurAddress;

End;

Function GetLineAddress(Line, Statement: DWord; Default: Word): Word; 
Var
  Stat, CurAddress, LineStart, LineNum, LineEnd, TempWord: DWord;
Begin
  CurAddress := GetWord(@Memory[PROG]);
  If CurAddress = $FFFF Then Exit;
  PrevLineAddr := CurAddress;
  LineNum := 65535;
  While LineNum <> Line Do Begin
     // Line Numbers are stored big-endian
     TempWord := GetWord(@Memory[CurAddress]);
     LineNum  := (TempWord Shr 8) + ((TempWord and 255) Shl 8);
     // If we're all of a sudden matching, or greater than, we stop.
     If LineNum >= Line Then Break;
     PrevLineAddr := CurAddress;
     // Not a match - so now Address the Length
     // Get the line length (includes terminal #13)
     Inc(CurAddress, GetWord(@Memory[CurAddress+2])+4);
     If CurAddress >= GetWord(@Memory[VARS]) Then Break;
  End;

  If LineNum >= Line Then Begin
     // Now find the statement specified - if the line number
     // matches exactly. If not, ignore the statement.
     LineStart := CurAddress;
     If LineNum = Line Then Begin
        // Found the line - now find the statement.
        LineEnd := CurAddress;
        Stat := 1;
        Inc(LineEnd, GetWord(@Memory[CurAddress+2])+4);
        While (Statement <> 1) and (CurAddress < LineEnd) Do Begin
           If (Memory[CurAddress] <> Ord(':')) and (Memory[CurAddress] <> $CB) Then
              Inc(CurAddress)
           Else Begin
              Inc(Stat);
              Inc(CurAddress);
              If Stat = Statement Then Begin
                 Result := CurAddress;
                 Exit;
              End;
           End;
        End;
     End;
     Result := LineStart;
  End Else
     Result := Default;
End;

Procedure DeleteEditorLines(Lines: AnsiString);
Var
  Idx, BASNumber, LineStart, LineNumber: Integer;
  Done: Boolean;
  LineList: TStringlist;
Begin

  // Removes a list of lines from BASIC memory if they exist.
  // Line format is <line>#13<line>#13...

  If BASinOutput.BASICMem = '' Then Exit;

  LineList := TStringlist.Create;
  While Lines <> '' Do Begin
     LineList.Add(Copy(Lines, 1, Pos(#13, Lines) -1));
     Lines := Copy(Lines, Pos(#13, Lines)+1, 9999999);
  End;
  LineList.Sort;

  Idx := 1;
  Done := False;
  LineNumber := StrToInt(LineList[0]);
  Repeat
     While (Idx < Length(BASinOutput.BASICMem)) and Not (BASinOutput.BASICMem[Idx] in ['0'..'9']) Do Inc(Idx);
     BASNumber := 0;
     LineStart := Idx;
     While BASinOutput.BASICMem[Idx] in ['0'..'9'] Do Begin
        BASNumber := (BASNumber * 10) + Ord(BASinOutput.BASICMem[Idx])-48;
        Inc(Idx);
     End;
     If BASNumber = LineNumber Then Begin
        While BASinOutput.BASICMem[Idx] <> #13 Do Inc(Idx);
        While BASinOutput.BASICMem[Idx] = #13 Do Inc(Idx);
        BASinOutput.BASICMem := Copy(BASinOutput.BASICMem, 1, LineStart -1)+Copy(BASinOutput.BASICMem, Idx, 999999);
        Idx := LineStart;
        LineList.Delete(0);
        If LineList.Count = 0 Then
           Done := True
        Else
           LineNumber := StrToInt(LineList[0]);
     End Else
        While BASinOutput.BASICMem[Idx] <> #13 Do Inc(Idx);
     If Idx >= Length(BASinOutput.BASICMem) Then
        Done := True;
  Until Done;

  If Copy(BASinOutput.BASICMem, Length(BASinOutput.BASICMem), 1) <> #13 Then
     BASinOutput.BASICMem := BASinOutput.BASICMem + #13;

  BASinOutput.LastLineBuffer := BASinOutput.BASICMem;

End;

Procedure DeleteBASICLines(Lines: AnsiString);
Var
  TempWord, CurAddress, LineNum, BASICNum: Word;
  LineList: TStringlist;
Begin

  // Removes a list of lines from BASIC memory if they exist.
  // Line format is <line>#13<line>#13...

  LineList := TStringlist.Create;
  While Lines <> '' Do Begin
     LineList.Add(Copy(Lines, 1, Pos(#13, Lines) -1));
     Lines := Copy(Lines, Pos(#13, Lines)+1, 999999);
  End;
  LineList.Sort;

  CurAddress := GetWord(@Memory[PROG]);

  While LineList.Count > 0 Do Begin

     LineNum := StrToInt(LineList[0]);

     // Get the current line number from BASIC Memory

     TempWord := GetWord(@Memory[CurAddress]);
     BASICNum  := (TempWord Shr 8) + ((TempWord and 255) Shl 8);

     If BASICNum = LineNum Then Begin

        // A Match, so delete it and leave the current address alone.

        TempWord := GetWord(@Memory[CurAddress+2])+4;
        MoveSpectrumMemory(TempWord + CurAddress, -TempWord);
        LineList.Delete(0);

     End Else

        If BASICNum < LineNum Then Begin

           // Less than the line we want, so jump past this line.

           TempWord := GetWord(@Memory[CurAddress+2])+4;
           Inc(CurAddress, TempWord);

           If CurAddress >= GetWord(@Memory[VARS]) Then
              Break;

        End Else

           // For all lines where the BASIC line is greater, delete them.

           While BASICNum > StrToInt(LineList[0]) Do Begin
              LineList.Delete(0);
              If LineList.Count = 0 Then
                 Break;
           End;

  End;

  LineList.Free;

End;

Procedure SendLineToEditor(Line: Word; Statement: Byte);
Var
  LineAddr, LineLen, F, CurStat: Word;
  NewLine: AnsiString;
Begin
  If Not BASinOutput.Running Then Begin
     // Find the line, and get the address in memory
     // BASIC memory, that is.
     LineAddr := GetLineAddress(Line, 1, GetWord(@Memory[PROG]));
     LineLen  := GetWord(@Memory[LineAddr+2])+4;
     Line := (Memory[LineAddr] Shl 8)+Memory[LineAddr+1];
     NewLine  := IntToStr(Line)+' '+DetokeniseLine(GetMemoryString(LineAddr+4, LineLen - 5, Memory), False);
     PutEditLine(NewLine, Memory);
     // The line is now in the edit workspace - now to set the
     // cursor to the correct statement... :-)
     F := 1;
     CurStat := 1;
     While CurStat < Statement Do Begin
        If NewLine[F] = ':' Then
           Inc(CurStat)
        Else If Copy(NewLine, F, 4) = 'THEN' Then Begin
           Inc(F, 4); // Step past the rest of the THEN
           Inc(CurStat);
        End;
        Inc(F);
     End;
     // Now write the new cursor address. Adjust to skip the line number
     // if it's the first statement.
     If Statement = 1 Then Inc(F, Length(IntToStr(Line))+1);
     If Statement = 0 Then Inc(F, Length(NewLine));
     PutWord(@Memory[K_CUR], F+GetWord(@Memory[E_LINE]));
     SelStartAddr := GetWord(@Memory[K_CUR]);
     // And force an update of the current line by sending a dummy
     // cursor movement.
     BufferToken($08);
  End;
End;

Procedure MoveSpectrumMemory(StartAddr: Word; Dist: Integer); 
Var
  F: DWord;
  WordVal: Word;
  MemStr: AnsiString;
Begin
  // Get the block of memory to be moved
  MemStr := GetMemoryString(StartAddr, (GetWord(@Memory[STKEND])-StartAddr)+1, Memory);
  // First, sort out the sysvars, if necessary.
  F := VARS;
  While F <= STKEND Do Begin
     WordVal := GetWord(@Memory[F]);
     If WordVal >= StartAddr Then PutWord(@Memory[F], Word(WordVal+Dist));
     Inc(F, 2);
  End;
  // Now move the memory.
  Inc(StartAddr, Dist);
  For F := 1 To Length(MemStr) Do
     Memory[F+StartAddr-1] := Ord(MemStr[F]);
End;

Procedure GetVarsList(var List: TStringlist); 
Var
  VarAddress: Word;
  TempStr: AnsiString;
Begin
  // Simply (!) gets a list of variables currently declared in memory.
  // used mostly by the Watch properties window.
  List.Clear;
  VarAddress := GetWord(@Memory[VARS]);
  While Memory[VarAddress] <> 128 Do Begin
     Case (Memory[VarAddress] and 224) of
        64:
           Begin
              // AnsiString
              List.Add(AnsiChar((Memory[VarAddress] and 31)+96) + '$');
              Inc(VarAddress, GetWord(@Memory[VarAddress+1])+3);
           End;
        96:
           Begin
              // Simple Numeric
              List.Add(AnsiChar((Memory[VarAddress] and 31)+96));
              Inc(VarAddress, 6);
           End;
        160:
           Begin
              // Complex Numeric
              TempStr := AnsiChar((Memory[VarAddress] and 31)+96);
              Inc(VarAddress);
              While Memory[VarAddress] and 128 = 0 Do Begin
                 TempStr := TempStr + LowerCase(AnsiChar(Memory[VarAddress]));
                 Inc(VarAddress);
              End;
              List.Add(TempStr + LowerCase(AnsiChar(Memory[VarAddress] and 127)));
              Inc(VarAddress, 6);
           End;
        128:
           Begin
              // Numeric Array
              List.Add(AnsiChar((Memory[VarAddress] and 31)+96));
              Inc(VarAddress, GetWord(@Memory[VarAddress+1])+3);
             End;
        192:
           Begin
              // AnsiString Array
              List.Add(AnsiChar((Memory[VarAddress] and 31)+96)+'$');
              Inc(VarAddress, GetWord(@Memory[VarAddress+1])+3);
           End;
        224:
           Begin
              // FOR Variable
              List.Add(AnsiChar((Memory[VarAddress] and 31)+96));
              Inc(VarAddress, 19);
           End;
     End;

  End;
End;

Function GetVariable(VarName: AnsiString; Address: Word): AnsiString; 
Var
  TempByte: Byte;
  Done: Boolean;
  CurName,
  TempStr,
  Contents: AnsiString;
  VarAddress,
  TempWord: Word;
  SaveDWord,
  TempDWord: DWord;
  TempValue: Extended;
Begin
  // Gets a variable (must be named, Address can be optional) and returns its' contents
  // in a AnsiString. If the Address is 0 (address unknown) then the entire VARS memory section
  // is searched, until a match for VarName is found.
  VarName := Lowercase(VarName);
  If Address = 0 Then
     VarAddress := GetWord(@Memory[VARS])
  Else
     VarAddress := Address;
  Done := False;
  CurName := '';
  While Not Done Do Begin
     // Is this the variable we were looking for?
     If lowercase(CurName) = VarName Then Begin
        Result := Contents;
        Exit;
     End Else Begin
        // These are not the variables you are looking for.
        // He can go on about his business.
        If Memory[VarAddress] = 128 Then Begin
           Result := 'Variable not found';
           Exit;
        End;
     End;
     Case (Memory[VarAddress] and 224) of
        64:
           Begin
              // AnsiString
              CurName := AnsiChar((Memory[VarAddress] and 31)+96) + '$';
              If CurName = VarName Then Begin
                 TempWord := GetWord(@Memory[VarAddress+1]);
                 Contents := '"'+InsertEscapes(GetMemoryString(VarAddress+3, TempWord, Memory))+'"';
                 Inc(VarAddress, TempWord+3);
              End Else Begin
                 Inc(VarAddress, GetWord(@Memory[VarAddress+1])+3);
              End;
           End;
        96:
           Begin
              // Simple Numeric
              CurName := AnsiChar((Memory[VarAddress] and 31)+96);
              If CurName = VarName Then Begin
                 TempByte := Memory[VarAddress+1];
                 TempDWord := GetDWord(@Memory[VarAddress+2]);
                 TempValue := Byte5ToFloat(TempByte, TempDWord);
                 Contents := FloatToStrEx(TempValue);
              End;
              Inc(VarAddress, 6);
           End;
        160:
           Begin
              // Complex Numeric
              TempStr := AnsiChar((Memory[VarAddress] and 31)+96);
              Inc(VarAddress);
              While Memory[VarAddress] and 128 = 0 Do Begin
                 TempStr := TempStr + LowerCase(AnsiChar(Memory[VarAddress]));
                 Inc(VarAddress);
              End;
              CurName := TempStr + LowerCase(AnsiChar(Memory[VarAddress] and 127));
              If CurName = VarName Then Begin
                 TempByte := Memory[VarAddress+1];
                 TempDWord := GetDWord(@Memory[VarAddress+2]);
                 TempValue := Byte5ToFloat(TempByte, TempDWord);
                 Contents := FloatToStrEx(TempValue);
              End;
              Inc(VarAddress, 6);
           End;
        128:
           Begin
              // Numeric Array
              CurName := AnsiChar((Memory[VarAddress] and 31)+96);
              If CurName = VarName Then Begin
                 TempByte := Memory[VarAddress+3];
                 SaveDWord := 1;
                 Inc(VarAddress, 4);
                 While TempByte > 0 Do Begin
                    TempWord := GetWord(@Memory[VarAddress]);
                    SaveDWord := SaveDWord * TempWord;
                    Dec(TempByte);
                    Inc(VarAddress, 2);
                 End;
                 TempStr := '';
                 While SaveDWord > 0 Do Begin
                    TempByte := Memory[VarAddress];
                    TempDWord := GetDWord(@Memory[VarAddress+1]);
                    TempValue := Byte5ToFloat(TempByte, TempDWord);
                    TempStr := TempStr + FloatToStrEx(TempValue);
                    If SaveDWord > 1 Then TempStr := TempStr + ', ';
                    Inc(VarAddress, 5);
                    Dec(SaveDWord);
                 End;
                 Contents := TempStr;
              End Else Begin
                 Inc(VarAddress, GetWord(@Memory[VarAddress+1])+3);
              End;
           End;
        192:
           Begin
              // AnsiString Array
              CurName := AnsiChar((Memory[VarAddress] and 31)+96)+'$';
              If CurName = VarName Then Begin
                 TempByte := Memory[VarAddress+3];
                 SaveDWord := 1;
                 Inc(VarAddress, 4);
                 While TempByte > 0 Do Begin
                    TempWord := GetWord(@Memory[VarAddress]);
                    SaveDWord := SaveDWord * TempWord;
                    Dec(TempByte);
                    Inc(VarAddress, 2);
                 End;
                 TempStr := '"';
                 While SaveDWord > 0 Do Begin
                    TempStr := TempStr + AnsiChar(Memory[VarAddress]);
                    If SaveDWord = 1 Then TempStr := TempStr + '"';
                    Inc(VarAddress);
                    Dec(SaveDWord);
                 End;
                 TempStr := InsertEscapes(TempStr);
                 Contents := TempStr;
              End Else Begin
                 Inc(VarAddress, GetWord(@Memory[VarAddress+1])+3);
              End;
           End;
        224:
           Begin
              // FOR Variable
              CurName := AnsiChar((Memory[VarAddress] and 31)+96);
              If CurName = VarName Then Begin
                 // START
                 TempStr := FloatToStrEx(Byte5ToFloat(Memory[VarAddress+1], GetDWord(@Memory[VarAddress+2])))+' TO ';
                 Inc(VarAddress, 6);
                 // TO
                 TempStr := TempStr + FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])))+' STEP ';
                 Inc(VarAddress, 5);
                 // STEP
                 TempStr := TempStr + FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])))+' [';
                 Inc(VarAddress, 5);
                 // LINE/STATEMENT
                 TempStr := TempStr + IntToStr(GetWord(@Memory[VarAddress]))+':';
                 TempStr := TempStr + IntToStr(Memory[VarAddress+2])+']';
                 Inc(VarAddress, 3);
                 Contents := TempStr;
              End Else Begin
                 Inc(VarAddress, 19);
              End;
           End;
     End;
  End;
End;

Procedure CopyListing;
Var
  Listing,
  BASICStr,
  TempStr: AnsiString;
  TempWord,
  CurAddress: Word;
Begin

  Listing := '';

  CurAddress := GetWord(@Memory[PROG]);

  If CurAddress <> GetWord(@Memory[VARS]) Then Repeat

     TempWord := GetWord(@Memory[CurAddress]);
     TempStr := IntToStr((TempWord Shr 8) + ((TempWord and 255) Shl 8));
     While Length(TempStr) < 4 Do TempStr := ' '+TempStr;
     Listing := Listing + TempStr +' ';
     Inc(CurAddress, 2);
     TempWord := GetWord(@Memory[CurAddress]);
     Inc(CurAddress, 2);
     TempStr := '';
     TempStr := GetMemoryString(CurAddress, TempWord-1, Memory);
     Inc(CurAddress, TempWord);
     BASICStr := '';
     BASICStr := DetokeniseLine(TempStr, False);
     BASICStr := InsertEscapes(BASICStr);
     Listing := listing + BASICStr + #13#10;

  Until CurAddress = GetWord(@Memory[VARS]);

  ClipBoard.SetTextBuf(PChar(Listing));

End;

Procedure CutEditLine;
Begin
  CopyEditLine;
  DeleteEditLine;
End;

Procedure CopyEditLine;
Var
  EditLine: AnsiString;
  EditStart, RangeStart, RangeEnd, CursorPos: Word;
Begin
  If SelStartAddr <> GetWord(@Memory[K_CUR]) Then Begin
     EditLine := GetEditLine;
     If Memory[FLAGX] And 32 = 32 Then
        EditStart := GetWord(@Memory[WORKSP])
	   Else
        EditStart := GetWord(@Memory[E_LINE]);
     CursorPos := GetWord(@Memory[K_CUR]);
     RangeStart := Min(SelStartAddr, CursorPos) - EditStart;
     RangeEnd := Max(SelStartAddr, CursorPos) - EditStart;
     If CursorPos > SelStartAddr Then Dec(RangeEnd) Else Inc(RangeStart);
     EditLine := Copy(EditLine, RangeStart+1, (RangeEnd-RangeStart)+1);
     ClipBoard.SetTextBuf(PChar(InsertEscapes(EditLine)));
  End Else Begin
     EditLine := InsertEscapes(GetEditLine);
     ClipBoard.SetTextBuf(PChar(EditLine));
  End;
End;

Procedure PasteEditLine;
Var
  ClipText, CurLine: AnsiString;
  CurPos: Word;
Begin
  If ClipBoard.HasFormat(CF_TEXT) Then Begin
     ClipList.Clear;
     CurLine := '';
     ClipText := FormatEscapes(ClipBoard.AsText);
     While ClipText <> '' Do Begin
        If ClipText[1] <> #13 Then
           CurLine := CurLine + ClipText[1]
        Else Begin
           ClipList.Add(CurLine);
           CurLine := '';
        End;
        ClipText := Copy(ClipText, 2, 999999);
     End;
     If Curline <> '' Then ClipList.Add(CurLine);
     If ClipList.Count = 1 Then Begin
        If SelStartAddr <> GetWord(@Memory[K_CUR]) Then ClearSelection;
        CurPos := GetWord(@Memory[K_CUR]);
        InsertText(GetClipText(0), GetCursorPos+1);
        PutWord(@Memory[K_CUR], CurPos+Length(GetClipText(0))-1);
        ClipList.Clear;
        BufferToken($09);
     End Else If ClipList.Count > 1 Then Begin
        ClipList.Add('');
        PutEditLine(GetClipText(0), Memory);
        ClipList.Delete(0);
        BufferToken(13);
     End;
  End;
End;

Procedure DeleteEditLine;
Begin
  If Editing Then Begin
     If SelStartAddr <> GetWord(@Memory[K_CUR]) Then Begin
        ClearSelection;
        SelStartAddr := GetWord(@Memory[K_CUR]);
        BufferToken(14);
     End Else Begin
        PutEditLine(' ', Memory);
        BufferToken($0C);
     End;
  End;
End;

Procedure InsertText(Text: AnsiString; Posn: Integer);
Var
  TempStr: AnsiString;
Begin
  TempStr := GetEditLine;
  TempStr := Copy(TempStr, 1, Posn-1)+Text+Copy(TempStr, Posn, 999999);
  PutEditLine(TempStr, Memory);
End;

Procedure ClearSelection;
Begin
  ReplaceSelection('');
End;

Procedure ReplaceSelection(NewText: AnsiString);
Var
  TempStr: AnsiString;
  EditStart, RangeStart, RangeEnd, CursorPos: Word;
Begin
  If InInput Then Begin
     TempStr := GetEditLine;
     If TempStr = '' Then Exit;
     CursorPos := GetWord(@Memory[K_CUR]);
     If Memory[FLAGX] And 32 = 32 Then Begin
        EditStart := GetWord(@Memory[WORKSP]);
        If CursorPos < EditStart Then
           Exit;
	   End Else Begin
        EditStart := GetWord(@Memory[E_LINE]);
        If CursorPos >= EditStart + Length(TempStr) Then
           Exit;
     End;
     RangeStart := Min(SelStartAddr, CursorPos) - EditStart;
     RangeEnd := Max(SelStartAddr, CursorPos) - EditStart;
     If RangeStart < RangeEnd Then Begin
        If CursorPos > SelStartAddr Then Begin
           TempStr := Copy(TempStr, 1, RangeStart) + NewText + Copy(TempStr, RangeEnd +2, 999999);
           PutEditLine(TempStr+#13, Memory);
           PutWord(@Memory[K_CUR], EditStart + RangeStart);
        End Else Begin
           TempStr := Copy(TempStr, 1, RangeStart) + NewText + Copy(TempStr, RangeEnd +2, 999999);
           PutEditLine(TempStr+#13, Memory);
           PutWord(@Memory[K_CUR], EditStart + RangeStart +1);
        End;
     End;
  End;
End;

Function FindNextForward(FindText: AnsiString; MatchCase: Boolean): TFindResult;
Var
  SearchLine: Word;
  SearchLinePos: Word;
  LineAddr: Word;
  LineLen: Word;
  TempStr,
  LineText: AnsiString;
  Found, SearchComplete: Boolean;
  FoundPos: Word;
  F: Integer;
Begin

  // Finds text in the BASIC listing.
  // Uses this method to find text:
  // LastFind_E_PPC: The last line that the find was used
  // LastFind_SUBPPC: The last position within that line that the find occurred
  // LastFindText: The last text that was searched for.

  // If LastFind_E_PPC <> E_PPC or the text has changed, then the search begins
  // At position 0 of E_PPC. Otherwise, It begins at the last find spot.

  SearchLine := GetWord(@Memory[E_PPC]);
  If (LastFind_E_PPC <> SearchLine) or (FindText <> LastFindText) Then Begin
     SearchLinePos := 0;
  End Else Begin
     SearchLinePos := LastFind_SUBPPC;
  End;

  If (LastFind_E_PPC = 0) and (LastFind_SUBPPC = 0) Then Begin
     SearchLine := GetWord(@Memory[PROG]);
     SearchLine := (Memory[SearchLine] Shl 8)+Memory[SearchLine+1];
  End;

  LastFindText := FindText;

  TempStr := '';
  F := 1;
  While F < Length(FindText)+1 Do Begin
     If FindText[F] = '\' Then Begin
        ProcessEscapeChars(FindText, F, TempStr)
     End Else
        TempStr := TempStr + FindText[F];
     Inc(F);
  End;
  FindText := TempStr;

  // Modify the case to lower if not matching case.
  If Not MatchCase Then FindText := LowerCase(FindText);

  // Now, starting at Searchline, fetch the line as text and do a find.
  Found := False;
  SearchComplete := False;
  While Not Found Do Begin
     // This gets the line
     LineText := '';
     LineAddr := GetLineAddress(SearchLine, 1, GetWord(@Memory[PROG]));
     LineLen  := GetWord(@Memory[LineAddr+2])+4;
     SearchLine := (Memory[LineAddr] Shl 8)+Memory[LineAddr+1];
     TempStr := GetMemoryString(LineAddr+4, LineLen - 5, Memory);
     LineText := DetokeniseLine(TempStr, False);
     LineText := IntToStr(SearchLine)+' '+LineText;
     // Now Crop from SearchLinePos.
     LineText := Copy(LineText, SearchLinePos, 999999);
     // Now find. Modify the case if not case sensitive.
     If Not MatchCase Then LineText := LowerCase(LineText);
     // Finally, search.
     FoundPos := Pos(FindText, LineText);
     If FoundPos <> 0 Then Begin
        // We have a match!
        SearchComplete := True;
        Found := True;
     End Else Begin
        // No match on this line - let's get the line number of the next one.
        // Or bail out if not.
        Inc(LineAddr, LineLen);
        If LineAddr = GetWord(@Memory[VARS]) Then Begin
           // End of program, no more matches.
           Found := True;
           SearchComplete := False;
        End Else Begin
           // Set to next line, position zero.
           SearchLine := Memory[LineAddr+1]+(Memory[LineAddr] Shl 8);
           SearchLinePos := 0;
        End;
     End;
  End;
  If SearchComplete Then Begin
     // We have found - so fill the result, and update the "LastFind" vars.
     Result.LineNum := SearchLine;
     Result.Position := FoundPos+SearchLinePos;
     Result.Address := LineAddr;
     LastFind_E_PPC := SearchLine;
     LastFind_SUBPPC := SearchLinePos + FoundPos + Length(FindText);
  End Else Begin
     // Fill the result with zeros for a search failed.
     Result.LineNum := 65534;
     Result.Position := 0;
     LastFind_E_PPC := 0;
     LastFind_SUBPPC := 0;
  End;
End;

Function FindNextBackward(FindText: AnsiString; MatchCase: Boolean): TFindResult;
Var
  SearchLine: Word;
  SearchLinePos: Word;
  LineAddr: Word;
  LineLen: Word;
  TempStr,
  LineText: AnsiString;
  Found, SearchComplete: Boolean;
  FoundPos: Word;
  F: Integer;

Begin

  // As Above, Finds text in the BASIC listing, only backwards.
  // See comments above for details.

  SearchLine := GetWord(@Memory[E_PPC]);
  If (LastFind_E_PPC <> SearchLine) or (FindText <> LastFindText) Then Begin
     SearchLinePos := 65535;
  End Else Begin
     SearchLinePos := LastFind_SUBPPC;
  End;
  LastFindText := FindText;

  If Not MatchCase Then FindText := LowerCase(FindText);

  TempStr := '';
  F := 1;
  While F < Length(FindText)+1 Do Begin
     If FindText[F] = '\' Then Begin
        ProcessEscapeChars(FindText, F, TempStr)
     End Else
        TempStr := TempStr + FindText[F];
     Inc(F);
  End;
  FindText := TempStr;

  Found := False;
  SearchComplete := False;
  While Not Found Do Begin
     LineText := '';
     LineAddr := GetLineAddress(SearchLine, 1, GetWord(@Memory[PROG]));
     SearchLine := (Memory[LineAddr] Shl 8)+Memory[LineAddr+1];
     LineLen  := GetWord(@Memory[LineAddr+2])+4;
     TempStr := GetMemoryString(LineAddr+4, LineLen - 5, Memory);
     LineText := DetokeniseLine(TempStr, False);
     LineText := IntToStr(SearchLine)+' '+LineText;
     If SearchLinePos = 65535 Then SearchLinePos := Length(LineText);
     LineText := Copy(LineText, 1, SearchLinePos);
     If Not MatchCase Then LineText := LowerCase(LineText);
     FoundPos := ReversePos(FindText, LineText);
     If FoundPos <> 0 Then Begin
        SearchComplete := True;
        Found := True;
     End Else Begin
        If LineAddr <= GetWord(@Memory[PROG]) Then Begin
           Found := True;
           SearchComplete := False;
        End Else Begin
           LineAddr := PrevLineAddr;
           SearchLine := Memory[LineAddr+1]+(Memory[LineAddr] Shl 8);
           SearchLinePos := 65535;
        End;
     End;
  End;
  If SearchComplete Then Begin
     Result.LineNum := SearchLine;
     Result.Position := FoundPos;
     Result.Address := LineAddr;
     LastFind_E_PPC := SearchLine;
     LastFind_SUBPPC := FoundPos -1;
  End Else Begin
     Result.LineNum := 65534;
     Result.Position := 0;
     LastFind_E_PPC := 0;
     LastFind_SUBPPC := 0;
  End;
End;

Function ReversePos(SubString: AnsiString; Text: AnsiString): Integer;
Begin
  // Start at the end, minus the length of the Substring
  Result := Length(Text) - Length(SubString);
  While (Result > 0) and (Copy(Text, Result, Length(SubString)) <> SubString) Do Dec(Result);
  If Result < 0 Then Result := 0;
End;

Procedure ReplaceText(Address: Word; Text: AnsiString; Bytes: Word);
Var
  F, Dist: Integer;
Begin
  Dist := Length(Text)-Bytes;
  MoveSpectrumMemory(Address+Bytes, Dist);
  If Length(Text) > 0 Then
     For F := Address To Address+Length(Text)-1 Do
        Memory[F] := Ord(Text[(F-Address)+1]);
End;

Procedure LOADQuoteQuote(FName: AnsiString);
Var
  F: Integer;
Begin
  Filename := FName;
  If Lowercase(ExtractFileExt(Filename)) = '.bas' Then Begin
        if (trim(Copy(ExtractFilename(Filename),1,8))='autoback') Then Begin
                MessageDlg('This is a automatic backup file.'+#13+#13+'You should save this file as different filename to keep it.'+#13+#13+'Autosave feature will not work if your project is named as Autoback.', mtWarning, [mbOk], 0);

        End;
  End;

  SaveEmulationState(TempState);
  RLEUnpackFile(BASinDir+'\basinC.bin', 'LOAD.sna');
  SetLength(FileArray, Length(RLEArray));
  For F := 0 to Length(RLEArray)-1 do
     FileArray[F] := RLEArray[F];
  SetLength(RLEArray, 0);
  LoadSna;
  Dec(Registers.PC);
  BASinOutput.Running := True;
  If BASinOutput.RunningAck <> BASinOutput.Running Then Begin
     PostMessage(BASinOutPut.Handle, WM_UPDATEPROGBUTTONS, 0, 0);
     PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
  End;
  Memory[0] := $FF;
  ControlEmulation(True);
  NeedParseUpdate := True;
  PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
End;

Function AutoLine(Tokens: AnsiString): AnsiString;
Var
  CurLineNum,
  NextLineNum,
  NewLineNum,
  CurLineAddress,
  NextLineAddress: Word;
Begin
  // Gets the first line number
  // and the next.
  // then replaces the first AnsiChar of the line with that number in characters
  // If no lines, it's 10. If only one line, it's cur line + 10.
  CurLineNum := GetWord(@Memory[E_PPC]);
  CurLineAddress := GetLineAddress(CurLineNum, 1, GetWord(@Memory[PROG]));
  If CurLineAddress >= GetWord(@Memory[VARS]) Then Begin
     // No Lines, so it's just line 10.
     NewLineNum := 10;
  End Else Begin
     // A line was returned. Get the next line number.
     CurLineNum := (Memory[CurLineAddress] Shl 8)+Memory[CurLineAddress+1];
     NextLineAddress := CurLineAddress + GetWord(@Memory[CurLineAddress+2]) + 4;
     If NextLineAddress >= GetWord(@Memory[VARS]) Then Begin
        // The current line was the last one.
        // So simply add 10.
        If CurLineNum + 10 < 9999 Then
           NewLineNum := CurLineNum + 10
        Else
           NewLineNum := 0;
     End Else Begin
        // Calculate the new line number
        // If there's room, do curnum + 10, else do a line between the two.
        NextLineNum := (Memory[NextLineAddress] Shl 8)+Memory[NextLineAddress+1];
        If NextLineNum > CurLineNum Then Begin
           NewLineNum := Min(CurLineNum + ((NextLineNum - CurLineNum) Div 2), CurLineNum + 10);
           // If this new line exists, (ie, it's curline or nextline) then
           // there's no room for the line.
           If (NewLineNum = CurLineNum) or (NewLineNum = NextLineNum) Then
              NewLineNum := 0;
        End Else Begin
           If CurLineNum + 10 < 9999 Then
              NewLineNum := CurLineNum + 10
           Else
              NewLineNum := 0;
        End;
     End;
  End;
  If NewLineNum <> 0 Then Begin
     Tokens := Copy(Tokens, 2, 999999);
     Result := IntToStr(NewLineNum) + Tokens;
  End Else
     Result := Tokens;
End;

procedure DebugMsg(const S: string);
begin
  OutputDebugString(PChar(S));
end;

Function InsertLine(Line: AnsiString; Overwrite: Boolean): AnsiString;
Var
  Tokenised: AnsiString;
  LineNum: Integer;
  LineAddr, TempWord, F: Word;
Begin

  If Not Registers.EmuRunning Then Begin
     InsertList.Add(Line);
     Exit;
  End Else Begin
     Repeat
        Sleep(1);
     Until Registers.PC = $38;
  End;

  Result := 'Error';

  // Adds or replaces a line of BASIC into your program area.
  // Takes plain text (with "\" escape codes) and checks (and prompts for action)
  // if it already exists, and inserts/replaces properly.

  Tokenised := ProcessBASLine(Line);

  If Tokenised = '' Then Begin
     // An error occured converting the BASIC text to tokens.
     MessageBox(BASinOutput.Handle, pChar(LogWindow.Memo1.Lines[LogWindow.Memo1.Lines.Count -1]), pChar('Error in BASIC'), MB_OK or MB_ICONWARNING);
     Exit;
  End;

  // Grab the line number, and the address that we're inserting at.

  LineNum := (Ord(Tokenised[1]) Shl 8) + Ord(Tokenised[2]);
  LineAddr := GetLineAddress(LineNum, 1, 0);

  // Is there any BASIC in memory at the minute?

  If GetWord(@Memory[PROG]) = GetWord(@Memory[VARS]) Then Begin

     // No - so nothing to reclaim.
     // We do, however, need to Make some space.
     // Preserve PROG, as it will get moved.

     TempWord := GetWord(@Memory[PROG]);
     MoveSpectrumMemory(TempWord, Length(Tokenised));
     PutWord(@Memory[PROG], TempWord);

  End Else Begin

     // Yes - The hard part here is getting the sysvars right.
     // Again, Preserve PROG as it's possible it will get moved.

     TempWord := GetWord(@Memory[PROG]);

     If LineNum <> (Memory[LineAddr] Shl 8)+Memory[LineAddr+1] Then Begin

        // The line does not already exist, so just shunt it all up from here to make room.

        MoveSpectrumMemory(LineAddr, Length(Tokenised));
        PutWord(@Memory[PROG], TempWord);

     End Else Begin

        // More complex still - reclaim the current line, as we need to overwrite.
        // Do this by dragging the next line back to LineAddr, then using a clone of the above
        // routine to shunt it forwards by the right amount.

        MoveSpectrumMemory(LineAddr + 4 + GetWord(@Memory[LineAddr+2]), -(GetWord(@Memory[LineAddr+2])+4));
        PutWord(@Memory[PROG], TempWord);

        MoveSpectrumMemory(LineAddr, Length(Tokenised));
        PutWord(@Memory[PROG], TempWord);

     End;

  End;

  // Finally, poke the line in.

  For F := 1 To Length(Tokenised) Do Begin
     Memory[LineAddr] := Ord(Tokenised[F]);
     Inc(LineAddr);
  End;

End;

Procedure GenerateBASICChecksum(var Check: DWord);
Var
  Addr, EndAddr: Word;
Begin
  Addr := GetWord(@Memory[PROG]);
  EndAddr := GetWord(@Memory[VARS]) -2;
  Check := Addr;
  While Addr < EndAddr -1 Do Begin
     Check := Check Xor GetWord(@Memory[Addr]);
     Inc(Addr);
  End;
End;

Function GetProgramLineCount: Word;
Var
  Addr, VarsAddr: Word;
Begin
  Result := 0;
  Addr := GetWord(@Memory[PROG]);
  VarsAddr := GetWord(@Memory[VARS]);
  While Addr < VarsAddr Do Begin
     Inc(Result);
     Inc(Addr, 2);
     Inc(Addr, GetWord(@Memory[Addr])+2);
  End;
End;

Procedure RenumberBASIC(Start, Step: Word);
Var
  CommandType: Byte;
  LineList: TStringlist; // A simple list of lines that have been renumbered.
  Addr, VarAddr, CurLineNum, NewLineNum, TempWord, LineLen, OldLineLen: Word;
  LineIndex, TempValue, MemPointer: Integer;
  InString, Modified, RenumErrors: Boolean;
  TempStr, Line, LogError: AnsiString;
Begin

  BASinOutput.TokeniseEditText(False);

  // A two-pass line renumber.
  // First pass, renumber the lines.
  // Second pass, search and replace GOTOs, GOSUBs, RESTOREs etc.

  RenumErrors := False;

  Addr := GetWord(@Memory[PROG]);
  VarAddr := GetWord(@Memory[VARS]);
  If Addr = VarAddr Then Exit; // No BASIC to work on

  LineList := TStringlist.Create;
  NewLineNum := Start;
  While Addr < VarAddr Do Begin
     CurLineNum := (Memory[Addr] Shl 8)+Memory[Addr+1];
     Memory[Addr] := NewLineNum Shr 8;
     Memory[Addr+1] := NewLineNum And 255;
     LineList.Add(IntToStr(CurLineNum));
     Inc(Addr, 2);
     Inc(Addr, GetWord(@Memory[Addr])+2);
     Inc(NewLineNum, Step);
  End;

  // Now to run through looking for keywords that will be affected by the renumber.

  Addr := GetWord(@Memory[PROG]);
  While Addr < VarAddr Do Begin
     CurLineNum := (Memory[Addr] Shl 8)+Memory[Addr+1];
     LineLen := GetWord(@Memory[Addr+2]);
     OldLineLen := LineLen+4;
     Line := GetMemoryString(Addr+4, LineLen, Memory);
     Mempointer := 1;
     InString := False;
     Modified := False;
     While MemPointer < LineLen Do Begin
        Case Ord(Line[MemPointer]) of

           // Special Chars

           14: Inc(Mempointer, 6); // Skip numbers
           16..21: Inc(MemPointer, 2);
           22: Inc(MemPointer, 3);
           23: Inc(MemPointer, 2);

           // Alpha numerics

           34: // Quote Marks
              Begin
                 Instring := Not Instring;
                 Inc(MemPointer);
              End;

           // Keywords - LIST, LLIST, RESTORE, GOTO, GOSUB, RUN

           229, 236, 237, 247, 225, 240:
              // RESTORE, GOTO, GOSUB (mandatory line number)
              // Also RUN, LLIST (optional line number)
              // And LIST (optional # stream)
              Begin
                 CommandType := Ord(Line[MemPointer]);
                 Inc(MemPointer);
                 While Ord(Line[MemPointer]) < 33 Do Inc(MemPointer);
                 If Line[MemPointer] = '#' Then Begin
                    While (Line[MemPointer] <> ',') and (MemPointer < LineLen) Do Inc(MemPointer);
                    If Line[MemPointer] = ',' Then Inc(MemPointer);
                 End;
                 If Line[MemPointer] <> #13 Then Begin
                    TempWord := MemPointer;
                    // Test to see if this is a valid number
                    If Line[MemPointer] in [#48..#57] Then Begin
                       TempValue := Round(GetNumber(Line, MemPointer, TempStr, False));
                       If Not GetNumError Then Begin
                          While Line[TempWord] in ['0'..'9'] Do
                             Line := Copy(Line, 1, TempWord -1)+Copy(Line, TempWord+1, 999999);
                          // Now find the closest line number
                          LineIndex := 0;
                          While (TempValue > StrToInt(LineList[LineIndex])) and (LineIndex < LineList.Count) Do Begin
                             Inc(lineIndex);
                             If LineIndex = LineList.Count then Break;
                          End;
                          NewLineNum := (LineIndex*Step)+Start;
                          Line := Copy(Line, 1, TempWord -1)+IntToStr(NewLineNum)+Copy(Line, TempWord, 999999);
                          LineLen := Length(Line);
                          Modified := True;
                       End;
                    End Else Begin
                       // An error occurred whilst getting the line number
                       // probably a "GOTO Line" type, ie, NAN.
                       RenumErrors := True;
                       LogError := 'Renumber Error at line '+IntToStr(CurLineNum)+' - Bad ';
                       Case CommandType of
                          229: LogError := LogError + 'RESTORE';
                          236: LogError := LogError + 'GO TO';
                          237: LogError := LogError + 'GO SUB';
                          247: LogError := LogError + 'RUN';
                          225: LogError := LogError + 'LLIST';
                          240: LogError := LogError + 'LIST ';
                       End;
                       Log(LogError);
                    End;
                 End;
              End;
        Else
           Inc(Mempointer);
        End;
     End;
     If Modified Then Begin
        // If we have replaced a line number, then update the line in memory.
        TempStr := Strip5Bytes(Line);
        TempStr := Insert5Bytes(TempStr);
        LineLen := Length(TempStr);
        TempStr := AnsiChar(CurLineNum Shr 8)+AnsiChar(CurLineNum And 255)+'  '+TempStr;
        PutWord(@TempStr[3], LineLen);
        ReplaceText(Addr, TempStr, OldLineLen);
        VarAddr := GetWord(@Memory[VARS]);
     End;
     Inc(Addr, LineLen+4);
  End;

  If RenumErrors Then ShowWindow(LogWindow, False);

  LineList.Free;
  PostMessage(BASinOutput.Handle, WM_UPDATEPROGRAM, 0, 0);

End;

Procedure RenumberBASICEx(RangeStart, RangeEnd, StartLine, Step: Integer);
Var
  BASICList, LineList, RenumList: TStringlist;
  LineStr, TempStr, TempStr2, LogError: AnsiString;
  LineNum, Pass, Idx, Idx2, NewLineNum, NewLineNumLen, LineIndex, TempValue: Integer;
  InString, RenumErrors, RenumError: Boolean;
  CommandType: Byte;
  TempWord: Word;
Begin

  BASICList := TStringlist.Create;
  RENUMList := TStringlist.Create;
  BASinOutput.GetBASIC;

  RenumErrors := False;

  Idx := 1;
  LineStr := '';
  While Idx <= Length(BASinOutput.BASICMem) Do Begin
     If BASinOutput.BASICMem[Idx] = #13 Then Begin
        If LineStr <> '' Then Begin
           Idx2 := 1;
           LineNum := 0;
           While LineStr[Idx2] in ['0'..'9'] Do Begin
              LineNum := (LineNum * 10) +Ord(LineStr[Idx2]) -48;
              Inc(Idx2);
           End;
           While Idx2 < 5 Do Begin
              LineStr := '0'+LineStr;
              Inc(Idx2);
           End;
           If (LineNum < RangeStart) or (LineNum > RangeEnd) Then
              BASICList.Add(LineStr)
           Else
              RENUMList.Add(LineStr);
        End;
        LineStr := '';
     End Else
        LineStr := LineStr + BASinOutput.BASICMem[Idx];
     Inc(Idx);
  End;
  If LineStr <> '' Then
     If (LineNum < RangeStart) or (LineNum > RangeEnd) Then
        BASIClist.Add(LineStr)
     Else
        RENUMList.Add(LineStr);

  For Pass := 0 To 1 Do Begin
     If Pass = 0 Then
        LineList := BASICList
     Else
        LineList := RENUMList;
     InString := False;
     For Idx := 0 to LineList.Count -1 Do Begin
        RenumError := False;
        TempStr := '';
        TempStr := TokeniseLine(LineList[Idx], False);
        Idx2 := 1;
        While Idx2 < Length(TempStr) Do Begin
           Case TempStr[Idx2] of
              #34:
                 Begin
                    InString := Not InString;
                 End;
              #229, #236, #237, #247, #225, #240:
                 Begin
                    NewLineNum := -1;
                    CommandType := Ord(TempStr[Idx2]);
                    Inc(Idx2);
                    While Ord(TempStr[Idx2]) < 33 Do Inc(Idx2);
                    If TempStr[Idx2] = '#' Then Begin
                       While (TempStr[Idx2] <> ',') and (Idx2 < Length(TempStr)) Do Inc(Idx2);
                       If TempStr[Idx2] = ',' Then Inc(Idx2);
                    End;
                    If TempStr[Idx2] <> #13 Then Begin
                       TempWord := Idx2;
                       If TempStr[Idx2] in [#48..#57] Then Begin
                          TempStr2 := '';
                          TempValue := Round(GetNumber(TempStr, Idx2, TempStr2, False));
                          If Not GetNumError Then Begin
                             If (TempValue >= RangeStart) and (TempValue <= RangeEnd) Then Begin
                                While TempStr[TempWord] in ['0'..'9'] Do
                                   TempStr := Copy(TempStr, 1, TempWord -1)+Copy(TempStr, TempWord+1, 999999);
                                LineIndex := 0;

                                While TempValue > StrToInt(Copy(RENUMList[LineIndex], 1, Pos(' ', RENUMList[LineIndex]) -1)) Do Begin
                                   Inc(lineIndex);
                                   If LineIndex = LineList.Count then Break;
                                End;
                                NewLineNum := (LineIndex * Step) + StartLine;
                                NewLineNumLen := length(IntToStr(NewLineNum));
                                TempStr := Copy(TempStr, 1, TempWord -1)+IntToStr(NewLineNum)+Copy(TempStr, TempWord, 999999);
                                idx2:=tempword+NewLineNumLen;     // renumber fix by arda
                             End;
                          End Else Begin
                             RenumError := True;
                          End;
                       End Else Begin
                          RenumError := True;
                       End;
                       If RenumError Then Begin
                          RenumErrors := True;
                          TempValue := StrToInt(Copy(LineList[Idx], 1, 4));
                          If (TempValue >= RangeStart) and (TempValue <= RangeEnd) Then Begin
                             LineIndex := 0;
                             While TempValue > StrToInt(Copy(RENUMList[LineIndex], 1, Pos(' ', RENUMList[LineIndex]) -1)) Do Begin
                                Inc(lineIndex);
                                If LineIndex = LineList.Count then Break;
                             End;
                             TempValue := (LineIndex * Step) + StartLine;
                          End;
                          LogError := 'Renumber Error at line '+IntToStr(TempValue)+' - Bad or Computed ';
                          Case CommandType of
                             229: LogError := LogError + 'RESTORE';
                             236: LogError := LogError + 'GO TO';
                             237: LogError := LogError + 'GO SUB';
                             247: LogError := LogError + 'RUN';
                             225: LogError := LogError + 'LLIST';
                             240: LogError := LogError + 'LIST ';
                          End;
                          Log(LogError);
                       End;
                    End;
                 End;
              #234:
                 Begin
                    Break;
                 End;
           End;
           Inc(Idx2);
        End;
        LineStr := '';
        LineStr := DeTokeniseLine(TempStr, False);
        LineList[Idx] := LineStr;
     End;
  End;

  For Idx := 0 To RENUMList.Count -1 Do Begin
     TempStr := IntToStr((Idx * Step) + StartLine);
     While Length(TempStr) < 4 Do
        TempStr := '0'+TempStr;
     RENUMList[Idx] := TempStr + Copy(RENUMList[Idx], 5, 999999);
     LineNum := StrToInt(Copy(RENUMlist[Idx], 1, 4));
     Idx2 := 0;
     While Idx2 < BASIClist.Count Do Begin
        If LineNum < StrToInt(Copy(BASICList[Idx2], 1, 4)) Then
           Break;
        Inc(Idx2);
     End;
     If Idx2 = BASIClist.Count Then
        BASICList.Add(RENUMList[Idx])
     Else
        If TempStr = Copy(BASICList[Idx2], 1, 5) Then Begin
           BASICList[Idx2] := RENUMList[Idx];
           Log('Renumber Warning - Line '+IntToStr(StrToInt(TempStr))+' was overwritten.');
           RenumErrors := True;
        End Else
           BASICList.Insert(Idx2, RENUMList[Idx]);
  End;

  BASinOutput.BASICMem := '';
  BASinOutput.LastLineBuffer := '';
  For Idx := 0 To BASICList.Count -1 Do Begin
     TempStr := BASICList[Idx];
     While Copy(TempStr, 1, 1) = '0' Do
        TempStr := Copy(TempStr, 2, 999999);
     BASinOutput.BASICMem := BASinOutput.BASICMem + TempStr + #13;
  End;

  BASinOutput.BASICMem := BASinOutput.BASICMem + #13;
  BASinOutput.LastLineBuffer := BASinOutput.BASICMem;
  BASinOutput.BASICChanged := False;
  BASinOutput.TokeniseEditText(False);
  BASinOutput.UpdateCursorPos(1, False);
  If BreakpointsWindow.Visible Then
     BreakPointsWindow.BuildBreakPointsList;

  If RenumErrors Then
     ShowWindow(LogWindow, False);

  BASinOutput.RepaintBASIC(True);

  BASICList.Free;
  RENUMList.Free;

End;

Function GetClipText(Index: Integer): AnsiString;
Begin
  Result := FormatEscapes(ClipList[Index]);
End;

Function Editing: Boolean;
Begin
  Result := Memory[FLAGX] And 32 = 0;
End;

Function InInput: Boolean;
Begin
  Result := Memory[TV_FLAG] and 1 = 1;
End;

Procedure DoContinue;
Begin
  If BASinOutput.Running Then Begin
     WantBreak := True;
     BASinOutput.PerformKeyDown(VK_RETURN, [], True);
  End Else Begin
     PutEditLine(#232, Memory);
     EmulateRET;
     Registers.A := 13;
     ControlEmulation(True);
  End;
End;

end.


