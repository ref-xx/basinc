// **********************************************
// *                                            *
// * FastCore.pas                               *
// *                                            *
// * A modified Z80 Core from SPIN.             *
// * Very(?) simple emulation, but very fast.   *
// *                                            *
// * (C) 2001-2008 By Paul Dunn.                *
// *                                            *
// **********************************************

unit FastCore;

interface

Uses Windows, Classes, SysUtils, FastDIB, Math, MMSystem,
     ROMUtils, InputUtils, Filing, FastDraw;

Type

  TZ80Registers =
     Record PC, TotalTS, LastTotalTS, Time: DWord;
            IX, IY, SP, SPn,
            Word1, Word2, Word3, TargetAddr: Word;
            R, I, F, A, C, B, E, D, L, H,
            Fn, An, Cn, Bn, En, Dn, Ln, Hn,
            ZByte1, ZByte2, ZByte3, RBit7, TempFlags,
            FlashState, IntMode, LastFE: Byte;
            EmuRunning, HaltEmu, CanInterrupt, IntsEnabled: Boolean;
     End;

  TEmulationState =
     Record Memory: Array[0..65535] of Byte;
            Registers: TZ80Registers;
            Running: Boolean;
            CursorType: AnsiChar;
            Editing: Boolean;
            ProgState: Byte;
            BorderDWord: DWord;
     End;

  // Emulation setup Procs

  Function  INITEMulation: Boolean;
  Procedure Reset;
  Procedure TrashScreenMem;
  Procedure BuildTables;
  Procedure SetUpScreenLUT;
  Function  GetScreenAddr(X, Y: DWord): DWord;
  Function  GetAttrAddr(X, Y: Integer): DWord;
  Procedure BuildPalette(Clrs: Array of TFColor);
  Procedure PrepareSpecColourTable;
  Procedure SetUpGraphics;

  Procedure SaveEmulationState(var State: TEmulationState);
  Procedure LoadEmulationState(var State: TEmulationState; Notify: Boolean);

  Procedure CloseEmulation;

  // Minimal Z80 Core Functions

  Procedure EmulateRET;
  Procedure EmulateCALL(Address: Word);
  Procedure EmulatePUSH(Value: Word);
  Function  EmulatePOP: Word;

  Function  GetByte(WordPtr: Pointer): Byte;
  Function  GetWord(WordPtr: Pointer): Word;
  Function  GetDWord(WordPtr: Pointer): DWord;
  Procedure PutByte(WordPtr: Pointer; Value: Byte);
  Procedure PutWord(WordPtr: Pointer; Value: Word);
  Procedure PutDWord(WordPtr: Pointer; Value: DWord);
  Procedure IncWord(WordPtr: Pointer);
  Procedure DecWord(WordPtr: Pointer);
  Function  GetPortByte(Port: Word): Byte;
  Procedure SetPortByte(Port: Word; Value: Byte);

  // Opcode Procedures

  Procedure NULLOP;
  Procedure NULLED;

  Procedure Op00; Procedure Op01; Procedure Op02; Procedure Op03; Procedure Op04; Procedure Op05;
  Procedure Op06; Procedure Op07; Procedure Op08; Procedure Op09; Procedure Op0A; Procedure Op0B;
  Procedure Op0C; Procedure Op0D; Procedure Op0E; Procedure Op0F; Procedure Op10; Procedure Op11;
  Procedure Op12; Procedure Op13; Procedure Op14; Procedure Op15; Procedure Op16; Procedure Op17;
  Procedure Op18; Procedure Op19; Procedure Op1A; Procedure Op1B; Procedure Op1C; Procedure Op1D;
  Procedure Op1E; Procedure Op1F; Procedure Op20; Procedure Op21; Procedure Op22; Procedure Op23;
  Procedure Op24; Procedure Op25; Procedure Op26; Procedure Op27; Procedure Op28; Procedure Op29;
  Procedure Op2A; Procedure Op2B; Procedure Op2C; Procedure Op2D; Procedure Op2E; Procedure Op2F;
  Procedure Op30; Procedure Op31; Procedure Op32; Procedure Op33; Procedure Op34; Procedure Op35;
  Procedure Op36; Procedure Op37; Procedure Op38; Procedure Op39; Procedure Op3A; Procedure Op3B;
  Procedure Op3C; Procedure Op3D; Procedure Op3E; Procedure Op3F; Procedure Op40; Procedure Op41;
  Procedure Op42; Procedure Op43; Procedure Op44; Procedure Op45; Procedure Op46; Procedure Op47;
  Procedure Op48; Procedure Op49; Procedure Op4A; Procedure Op4B; Procedure Op4C; Procedure Op4D;
  Procedure Op4E; Procedure Op4F; Procedure Op50; Procedure Op51; Procedure Op52; Procedure Op53;
  Procedure Op54; Procedure Op55; Procedure Op56; Procedure Op57; Procedure Op58; Procedure Op59;
  Procedure Op5A; Procedure Op5B; Procedure Op5C; Procedure Op5D; Procedure Op5E; Procedure Op5F;
  Procedure Op60; Procedure Op61; Procedure Op62; Procedure Op63; Procedure Op64; Procedure Op65;
  Procedure Op66; Procedure Op67; Procedure Op68; Procedure Op69; Procedure Op6A; Procedure Op6B;
  Procedure Op6C; Procedure Op6D; Procedure Op6E; Procedure Op6F; Procedure Op70; Procedure Op71;
  Procedure Op72; Procedure Op73; Procedure Op74; Procedure Op75; Procedure Op76; Procedure Op77;
  Procedure Op78; Procedure Op79; Procedure Op7A; Procedure Op7B; Procedure Op7C; Procedure Op7D;
  Procedure Op7E; Procedure Op7F; Procedure Op80; Procedure Op81; Procedure Op82; Procedure Op83;
  Procedure Op84; Procedure Op85; Procedure Op86; Procedure Op87; Procedure Op88; Procedure Op89;
  Procedure Op8A; Procedure Op8B; Procedure Op8C; Procedure Op8D; Procedure Op8E; Procedure Op8F;
  Procedure Op90; Procedure Op91; Procedure Op92; Procedure Op93; Procedure Op94; Procedure Op95;
  Procedure Op96; Procedure Op97; Procedure Op98; Procedure Op99; Procedure Op9A; Procedure Op9B;
  Procedure Op9C; Procedure Op9D; Procedure Op9E; Procedure Op9F; Procedure OpA0; Procedure OpA1;
  Procedure OpA2; Procedure OpA3; Procedure OpA4; Procedure OpA5; Procedure OpA6; Procedure OpA7;
  Procedure OpA8; Procedure OpA9; Procedure OpAA; Procedure OpAB; Procedure OpAC; Procedure OpAD;
  Procedure OpAE; Procedure OpAF; Procedure OpB0; Procedure OpB1; Procedure OpB2; Procedure OpB3;
  Procedure OpB4; Procedure OpB5; Procedure OpB6; Procedure OpB7; Procedure OpB8; Procedure OpB9;
  Procedure OpBA; Procedure OpBB; Procedure OpBC; Procedure OpBD; Procedure OpBE; Procedure OpBF;
  Procedure OpC0; Procedure OpC1; Procedure OpC2; Procedure OpC3; Procedure OpC4; Procedure OpC5;
  Procedure OpC6; Procedure OpC7; Procedure OpC8; Procedure OpC9; Procedure OpCA; Procedure OpCB;
  Procedure OpCC; Procedure OpCD; Procedure OpCE; Procedure OpCF; Procedure OpD0; Procedure OpD1;
  Procedure OpD2; Procedure OpD3; Procedure OpD4; Procedure OpD5; Procedure OpD6; Procedure OpD7;
  Procedure OpD8; Procedure OpD9; Procedure OpDA; Procedure OpDB; Procedure OpDC; Procedure OpDD;
  Procedure OpDE; Procedure OpDF; Procedure OpE0; Procedure OpE1; Procedure OpE2; Procedure OpE3;
  Procedure OpE4; Procedure OpE5; Procedure OpE6; Procedure OpE7; Procedure OpE8; Procedure OpE9;
  Procedure OpEA; Procedure OpEB; Procedure OpEC; Procedure OpED; Procedure OpEE; Procedure OpEF;
  Procedure OpF0; Procedure OpF1; Procedure OpF2; Procedure OpF3; Procedure OpF4; Procedure OpF5;
  Procedure OpF6; Procedure OpF7; Procedure OpF8; Procedure OpF9; Procedure OpFA; Procedure OpFB;
  Procedure OpFC; Procedure OpFD; Procedure OpFE; Procedure OpFF;

  Procedure OpCB00; Procedure OpCB01; Procedure OpCB02; Procedure OpCB03; Procedure OpCB04; Procedure OpCB05;
  Procedure OpCB06; Procedure OpCB07; Procedure OpCB08; Procedure OpCB09; Procedure OpCB0A; Procedure OpCB0B;
  Procedure OpCB0C; Procedure OpCB0D; Procedure OpCB0E; Procedure OpCB0F; Procedure OpCB10; Procedure OpCB11;
  Procedure OpCB12; Procedure OpCB13; Procedure OpCB14; Procedure OpCB15; Procedure OpCB16; Procedure OpCB17;
  Procedure OpCB18; Procedure OpCB19; Procedure OpCB1A; Procedure OpCB1B; Procedure OpCB1C; Procedure OpCB1D;
  Procedure OpCB1E; Procedure OpCB1F; Procedure OpCB20; Procedure OpCB21; Procedure OpCB22; Procedure OpCB23;
  Procedure OpCB24; Procedure OpCB25; Procedure OpCB26; Procedure OpCB27; Procedure OpCB28; Procedure OpCB29;
  Procedure OpCB2A; Procedure OpCB2B; Procedure OpCB2C; Procedure OpCB2D; Procedure OpCB2E; Procedure OpCB2F;
  Procedure OpCB30; Procedure OpCB31; Procedure OpCB32; Procedure OpCB33; Procedure OpCB34; Procedure OpCB35;
  Procedure OpCB36; Procedure OpCB37; Procedure OpCB38; Procedure OpCB39; Procedure OpCB3A; Procedure OpCB3B;
  Procedure OpCB3C; Procedure OpCB3D; Procedure OpCB3E; Procedure OpCB3F; Procedure OpCB40; Procedure OpCB41;
  Procedure OpCB42; Procedure OpCB43; Procedure OpCB44; Procedure OpCB45; Procedure OpCB46; Procedure OpCB47;
  Procedure OpCB48; Procedure OpCB49; Procedure OpCB4A; Procedure OpCB4B; Procedure OpCB4C; Procedure OpCB4D;
  Procedure OpCB4E; Procedure OpCB4F; Procedure OpCB50; Procedure OpCB51; Procedure OpCB52; Procedure OpCB53;
  Procedure OpCB54; Procedure OpCB55; Procedure OpCB56; Procedure OpCB57; Procedure OpCB58; Procedure OpCB59;
  Procedure OpCB5A; Procedure OpCB5B; Procedure OpCB5C; Procedure OpCB5D; Procedure OpCB5E; Procedure OpCB5F;
  Procedure OpCB60; Procedure OpCB61; Procedure OpCB62; Procedure OpCB63; Procedure OpCB64; Procedure OpCB65;
  Procedure OpCB66; Procedure OpCB67; Procedure OpCB68; Procedure OpCB69; Procedure OpCB6A; Procedure OpCB6B;
  Procedure OpCB6C; Procedure OpCB6D; Procedure OpCB6E; Procedure OpCB6F; Procedure OpCB70; Procedure OpCB71;
  Procedure OpCB72; Procedure OpCB73; Procedure OpCB74; Procedure OpCB75; Procedure OpCB76; Procedure OpCB77;
  Procedure OpCB78; Procedure OpCB79; Procedure OpCB7A; Procedure OpCB7B; Procedure OpCB7C; Procedure OpCB7D;
  Procedure OpCB7E; Procedure OpCB7F; Procedure OpCB80; Procedure OpCB81; Procedure OpCB82; Procedure OpCB83;
  Procedure OpCB84; Procedure OpCB85; Procedure OpCB86; Procedure OpCB87; Procedure OpCB88; Procedure OpCB89;
  Procedure OpCB8A; Procedure OpCB8B; Procedure OpCB8C; Procedure OpCB8D; Procedure OpCB8E; Procedure OpCB8F;
  Procedure OpCB90; Procedure OpCB91; Procedure OpCB92; Procedure OpCB93; Procedure OpCB94; Procedure OpCB95;
  Procedure OpCB96; Procedure OpCB97; Procedure OpCB98; Procedure OpCB99; Procedure OpCB9A; Procedure OpCB9B;
  Procedure OpCB9C; Procedure OpCB9D; Procedure OpCB9E; Procedure OpCB9F; Procedure OpCBA0; Procedure OpCBA1;
  Procedure OpCBA2; Procedure OpCBA3; Procedure OpCBA4; Procedure OpCBA5; Procedure OpCBA6; Procedure OpCBA7;
  Procedure OpCBA8; Procedure OpCBA9; Procedure OpCBAA; Procedure OpCBAB; Procedure OpCBAC; Procedure OpCBAD;
  Procedure OpCBAE; Procedure OpCBAF; Procedure OpCBB0; Procedure OpCBB1; Procedure OpCBB2; Procedure OpCBB3;
  Procedure OpCBB4; Procedure OpCBB5; Procedure OpCBB6; Procedure OpCBB7; Procedure OpCBB8; Procedure OpCBB9;
  Procedure OpCBBA; Procedure OpCBBB; Procedure OpCBBC; Procedure OpCBBD; Procedure OpCBBE; Procedure OpCBBF;
  Procedure OpCBC0; Procedure OpCBC1; Procedure OpCBC2; Procedure OpCBC3; Procedure OpCBC4; Procedure OpCBC5;
  Procedure OpCBC6; Procedure OpCBC7; Procedure OpCBC8; Procedure OpCBC9; Procedure OpCBCA; Procedure OpCBCB;
  Procedure OpCBCC; Procedure OpCBCD; Procedure OpCBCE; Procedure OpCBCF; Procedure OpCBD0; Procedure OpCBD1;
  Procedure OpCBD2; Procedure OpCBD3; Procedure OpCBD4; Procedure OpCBD5; Procedure OpCBD6; Procedure OpCBD7;
  Procedure OpCBD8; Procedure OpCBD9; Procedure OpCBDA; Procedure OpCBDB; Procedure OpCBDC; Procedure OpCBDD;
  Procedure OpCBDE; Procedure OpCBDF; Procedure OpCBE0; Procedure OpCBE1; Procedure OpCBE2; Procedure OpCBE3;
  Procedure OpCBE4; Procedure OpCBE5; Procedure OpCBE6; Procedure OpCBE7; Procedure OpCBE8; Procedure OpCBE9;
  Procedure OpCBEA; Procedure OpCBEB; Procedure OpCBEC; Procedure OpCBED; Procedure OpCBEE; Procedure OpCBEF;
  Procedure OpCBF0; Procedure OpCBF1; Procedure OpCBF2; Procedure OpCBF3; Procedure OpCBF4; Procedure OpCBF5;
  Procedure OpCBF6; Procedure OpCBF7; Procedure OpCBF8; Procedure OpCBF9; Procedure OpCBFA; Procedure OpCBFB;
  Procedure OpCBFC; Procedure OpCBFD; Procedure OpCBFE; Procedure OpCBFF;

  Procedure OpDD09; Procedure OpDD19; Procedure OpDD21; Procedure OpDD22; Procedure OpDD23; Procedure OpDD24;
  Procedure OpDD25; Procedure OpDD26; Procedure OpDD29; Procedure OpDD2A; Procedure OpDD2B; Procedure OpDD2C;
  Procedure OpDD2D; Procedure OpDD2E; Procedure OpDD34; Procedure OpDD35; Procedure OpDD36; Procedure OpDD39;
  Procedure OpDD44; Procedure OpDD45; Procedure OpDD46; Procedure OpDD4C; Procedure OpDD4D; Procedure OpDD4E;
  Procedure OpDD54; Procedure OpDD55; Procedure OpDD56; Procedure OpDD5C; Procedure OpDD5D; Procedure OpDD5E;
  Procedure OpDD60; Procedure OpDD61; Procedure OpDD62; Procedure OpDD63; Procedure OpDD64; Procedure OpDD65;
  Procedure OpDD66; Procedure OpDD67; Procedure OpDD68; Procedure OpDD69; Procedure OpDD6A; Procedure OpDD6B;
  Procedure OpDD6C; Procedure OpDD6D; Procedure OpDD6E; Procedure OpDD6F; Procedure OpDD70; Procedure OpDD71;
  Procedure OpDD72; Procedure OpDD73; Procedure OpDD74; Procedure OpDD75; Procedure OpDD77; Procedure OpDD7C;
  Procedure OpDD7D; Procedure OpDD7E; Procedure OpDD84; Procedure OpDD85; Procedure OpDD86; Procedure OpDD8C;
  Procedure OpDD8D; Procedure OpDD8E; Procedure OpDD94; Procedure OpDD95; Procedure OpDD96; Procedure OpDD9C;
  Procedure OpDD9D; Procedure OpDD9E; Procedure OpDDA4; Procedure OpDDA5; Procedure OpDDA6; Procedure OpDDAC;
  Procedure OpDDAD; Procedure OpDDAE; Procedure OpDDB4; Procedure OpDDB5; Procedure OpDDB6; Procedure OpDDBC;
  Procedure OpDDBD; Procedure OpDDBE; Procedure OpDDCB; Procedure OpDDDD; Procedure OpDDE1; Procedure OpDDE3;
  Procedure OpDDE5; Procedure OpDDE9; Procedure OpDDF9; Procedure OpDDFD;

  Procedure OpDDCB00; Procedure OpDDCB01; Procedure OpDDCB02; Procedure OpDDCB03; Procedure OpDDCB04; Procedure OpDDCB05;
  Procedure OpDDCB06; Procedure OpDDCB07; Procedure OpDDCB08; Procedure OpDDCB09; Procedure OpDDCB0A; Procedure OpDDCB0B;
  Procedure OpDDCB0C; Procedure OpDDCB0D; Procedure OpDDCB0E; Procedure OpDDCB0F; Procedure OpDDCB10; Procedure OpDDCB11;
  Procedure OpDDCB12; Procedure OpDDCB13; Procedure OpDDCB14; Procedure OpDDCB15; Procedure OpDDCB16; Procedure OpDDCB17;
  Procedure OpDDCB18; Procedure OpDDCB19; Procedure OpDDCB1A; Procedure OpDDCB1B; Procedure OpDDCB1C; Procedure OpDDCB1D;
  Procedure OpDDCB1E; Procedure OpDDCB1F; Procedure OpDDCB20; Procedure OpDDCB21; Procedure OpDDCB22; Procedure OpDDCB23;
  Procedure OpDDCB24; Procedure OpDDCB25; Procedure OpDDCB26; Procedure OpDDCB27; Procedure OpDDCB28; Procedure OpDDCB29;
  Procedure OpDDCB2A; Procedure OpDDCB2B; Procedure OpDDCB2C; Procedure OpDDCB2D; Procedure OpDDCB2E; Procedure OpDDCB2F;
  Procedure OpDDCB30; Procedure OpDDCB31; Procedure OpDDCB32; Procedure OpDDCB33; Procedure OpDDCB34; Procedure OpDDCB35;
  Procedure OpDDCB36; Procedure OpDDCB37; Procedure OpDDCB38; Procedure OpDDCB39; Procedure OpDDCB3A; Procedure OpDDCB3B;
  Procedure OpDDCB3C; Procedure OpDDCB3D; Procedure OpDDCB3E; Procedure OpDDCB3F; Procedure OpDDCB40; Procedure OpDDCB48;
  Procedure OpDDCB50; Procedure OpDDCB58; Procedure OpDDCB60; Procedure OpDDCB68; Procedure OpDDCB70; Procedure OpDDCB78;
  Procedure OpDDCB80; Procedure OpDDCB81; Procedure OpDDCB82; Procedure OpDDCB83; Procedure OpDDCB84; Procedure OpDDCB85;
  Procedure OpDDCB86; Procedure OpDDCB87; Procedure OpDDCB88; Procedure OpDDCB89; Procedure OpDDCB8A; Procedure OpDDCB8B;
  Procedure OpDDCB8C; Procedure OpDDCB8D; Procedure OpDDCB8E; Procedure OpDDCB8F; Procedure OpDDCB90; Procedure OpDDCB91;
  Procedure OpDDCB92; Procedure OpDDCB93; Procedure OpDDCB94; Procedure OpDDCB95; Procedure OpDDCB96; Procedure OpDDCB97;
  Procedure OpDDCB98; Procedure OpDDCB99; Procedure OpDDCB9A; Procedure OpDDCB9B; Procedure OpDDCB9C; Procedure OpDDCB9D;
  Procedure OpDDCB9E; Procedure OpDDCB9F; Procedure OpDDCBA0; Procedure OpDDCBA1; Procedure OpDDCBA2; Procedure OpDDCBA3;
  Procedure OpDDCBA4; Procedure OpDDCBA5; Procedure OpDDCBA6; Procedure OpDDCBA7; Procedure OpDDCBA8; Procedure OpDDCBA9;
  Procedure OpDDCBAA; Procedure OpDDCBAB; Procedure OpDDCBAC; Procedure OpDDCBAD; Procedure OpDDCBAE; Procedure OpDDCBAF;
  Procedure OpDDCBB0; Procedure OpDDCBB1; Procedure OpDDCBB2; Procedure OpDDCBB3; Procedure OpDDCBB4; Procedure OpDDCBB5;
  Procedure OpDDCBB6; Procedure OpDDCBB7; Procedure OpDDCBB8; Procedure OpDDCBB9; Procedure OpDDCBBA; Procedure OpDDCBBB;
  Procedure OpDDCBBC; Procedure OpDDCBBD; Procedure OpDDCBBE; Procedure OpDDCBBF; Procedure OpDDCBC0; Procedure OpDDCBC1;
  Procedure OpDDCBC2; Procedure OpDDCBC3; Procedure OpDDCBC4; Procedure OpDDCBC5; Procedure OpDDCBC6; Procedure OpDDCBC7;
  Procedure OpDDCBC8; Procedure OpDDCBC9; Procedure OpDDCBCA; Procedure OpDDCBCB; Procedure OpDDCBCC; Procedure OpDDCBCD;
  Procedure OpDDCBCE; Procedure OpDDCBCF; Procedure OpDDCBD0; Procedure OpDDCBD1; Procedure OpDDCBD2; Procedure OpDDCBD3;
  Procedure OpDDCBD4; Procedure OpDDCBD5; Procedure OpDDCBD6; Procedure OpDDCBD7; Procedure OpDDCBD8; Procedure OpDDCBD9;
  Procedure OpDDCBDA; Procedure OpDDCBDB; Procedure OpDDCBDC; Procedure OpDDCBDD; Procedure OpDDCBDE; Procedure OpDDCBDF;
  Procedure OpDDCBE0; Procedure OpDDCBE1; Procedure OpDDCBE2; Procedure OpDDCBE3; Procedure OpDDCBE4; Procedure OpDDCBE5;
  Procedure OpDDCBE6; Procedure OpDDCBE7; Procedure OpDDCBE8; Procedure OpDDCBE9; Procedure OpDDCBEA; Procedure OpDDCBEB;
  Procedure OpDDCBEC; Procedure OpDDCBED; Procedure OpDDCBEE; Procedure OpDDCBEF; Procedure OpDDCBF0; Procedure OpDDCBF1;
  Procedure OpDDCBF2; Procedure OpDDCBF3; Procedure OpDDCBF4; Procedure OpDDCBF5; Procedure OpDDCBF6; Procedure OpDDCBF7;
  Procedure OpDDCBF8; Procedure OpDDCBF9; Procedure OpDDCBFA; Procedure OpDDCBFB; Procedure OpDDCBFC; Procedure OpDDCBFD;
  Procedure OpDDCBFE; Procedure OpDDCBFF;

  Procedure OpED00; // ED00 is a ROM trap, and as such is not part of the core.

  Procedure OpED40; Procedure OpED41; Procedure OpED42; Procedure OpED43; Procedure OpED44; Procedure OpED45;
  Procedure OpED46; Procedure OpED47; Procedure OpED48; Procedure OpED49; Procedure OpED4A; Procedure OpED4B;
  Procedure OpED4C; Procedure OpED4D; Procedure OpED4E; Procedure OpED4F; Procedure OpED50; Procedure OpED51;
  Procedure OpED52; Procedure OpED53; Procedure OpED54; Procedure OpED55; Procedure OpED56; Procedure OpED57;
  Procedure OpED58; Procedure OpED59; Procedure OpED5A; Procedure OpED5B; Procedure OpED5C; Procedure OpED5D;
  Procedure OpED5E; Procedure OpED5F; Procedure OpED60; Procedure OpED61; Procedure OpED62; Procedure OpED63;
  Procedure OpED64; Procedure OpED65; Procedure OpED66; Procedure OpED67; Procedure OpED68; Procedure OpED69;
  Procedure OpED6A; Procedure OpED6B; Procedure OpED6C; Procedure OpED6D; Procedure OpED6E; Procedure OpED6F;
  Procedure OpED70; Procedure OpED71; Procedure OpED72; Procedure OpED73; Procedure OpED74; Procedure OpED75;
  Procedure OpED76; Procedure OpED78; Procedure OpED79; Procedure OpED7A; Procedure OpED7B; Procedure OpED7C;
  Procedure OpED7D; Procedure OpED7E; Procedure OpEDA0; Procedure OpEDA1; Procedure OpEDA2; Procedure OpEDA3;
  Procedure OpEDA8; Procedure OpEDA9; Procedure OpEDAA; Procedure OpEDAB; Procedure OpEDB0; Procedure OpEDB1;
  Procedure OpEDB2; Procedure OpEDB3; Procedure OpEDB8; Procedure OpEDB9; Procedure OpEDBA; Procedure OpEDBB;

  // Main Emulation loop procs

  Procedure ExecuteEmulationLoop_FullSpeed;
  Procedure ExecuteEmulationLoop_SpectrumSpeed;
  Procedure ExecuteEndOfFrame;
  Procedure InvokeInterrupt;
  Procedure UpdateDisplay;
  Procedure Activate64Colours(Activate: Boolean);
  Function  Get64ColourByte(Clr: Byte): Byte;
  Procedure Update64ColourDIBs;
  Procedure SetPalette64Entry(Index: Integer; Value: Byte);

Var

  InInterrupt:      Boolean;

  BreakState,
  TempState,
  UndoState:        TEmulationState;

  FullSpeed:        Boolean;

  BitSetCount:      Array[0..255] of DWord;
  SetCount:         Array[0..255] of Byte;
  TwoComp:          Array[0..255] of Integer;
  Parity:           Array[0..255] of Byte;

  Ports,
  Memory:           Array[0..65535] of Byte;
  RAMBanks:         Array[0..8, 0..16383] of Byte;

  AttrAddresses:    Array[0..6144] Of DWord;
  ScreenAddresses:  Array[0..191] of DWord;
  ScreenOffsets:    Array[0..6143] of DWord;
  DrawTSColTable:   Array[0..(2*8*8*256*8)] of Byte;
  FlashSwapTable:   Array[0..127] of Byte;

  DisplayPalette:   TFColorTable;
  DisplayPalette64: TFColorTable;

  DisplayHandle:    HWnd;
  sDc:              hDC;
  Display:          TFastDIB;
  ScreenPointer:    Pointer;
  P1, P2, P3, P4:   Pointer;

  Registers:        TZ80Registers;

  PagedBank,
  ElapsedFrames,
  DisplayByte,
  AttrByte:         Byte;

  DisablePaging,
  ShadowScreen,
  NeedDisplayUpdate,
  BorderUpdate:     Boolean;

  DisplaybitsPtr:   Pointer;

  BorderDWord:      DWord;

  WinWidth,
  WinHeight:        Integer;

  FrameTime,
  OldTotalTs,
  TempTotalTs:      DWord;

  OpCode:           Byte;

  IntTimer:         DWord;
  InterruptDue:     Boolean;
  StartLogging:     Boolean = False;

  DDCBPtr:          Pointer;
  MemAccess:        Array[0..65535] of Byte;

  Palette64:        Array[0..63] of Byte;
  Active64Colours:  Boolean;
  Last64Port:       Byte;
  
Const

  // Memory access flags

  MemRead      = 1;
  MemWrite     = 2;
  MemReadWrite = 3;
  MovePC       = 4;

  // OpCode Jump Tables

  Ops: Array[0..$FF] of TProcedure =
       (Op00,Op01,Op02,Op03,Op04,Op05,Op06,Op07,Op08,Op09,Op0A,Op0B,Op0C,Op0D,Op0E,Op0F,
        Op10,Op11,Op12,Op13,Op14,Op15,Op16,Op17,Op18,Op19,Op1A,Op1B,Op1C,Op1D,Op1E,Op1F,
        Op20,Op21,Op22,Op23,Op24,Op25,Op26,Op27,Op28,Op29,Op2A,Op2B,Op2C,Op2D,Op2E,Op2F,
        Op30,Op31,Op32,Op33,Op34,Op35,Op36,Op37,Op38,Op39,Op3A,Op3B,Op3C,Op3D,Op3E,Op3F,
        Op40,Op41,Op42,Op43,Op44,Op45,Op46,Op47,Op48,Op49,Op4A,Op4B,Op4C,Op4D,Op4E,Op4F,
        Op50,Op51,Op52,Op53,Op54,Op55,Op56,Op57,Op58,Op59,Op5A,Op5B,Op5C,Op5D,Op5E,Op5F,
        Op60,Op61,Op62,Op63,Op64,Op65,Op66,Op67,Op68,Op69,Op6A,Op6B,Op6C,Op6D,Op6E,Op6F,
        Op70,Op71,Op72,Op73,Op74,Op75,Op76,Op77,Op78,Op79,Op7A,Op7B,Op7C,Op7D,Op7E,Op7F,
        Op80,Op81,Op82,Op83,Op84,Op85,Op86,Op87,Op88,Op89,Op8A,Op8B,Op8C,Op8D,Op8E,Op8F,
        Op90,Op91,Op92,Op93,Op94,Op95,Op96,Op97,Op98,Op99,Op9A,Op9B,Op9C,Op9D,Op9E,Op9F,
        OpA0,OpA1,OpA2,OpA3,OpA4,OpA5,OpA6,OpA7,OpA8,OpA9,OpAA,OpAB,OpAC,OpAD,OpAE,OpAF,
        OpB0,OpB1,OpB2,OpB3,OpB4,OpB5,OpB6,OpB7,OpB8,OpB9,OpBA,OpBB,OpBC,OpBD,OpBE,OpBF,
        OpC0,OpC1,OpC2,OpC3,OpC4,OpC5,OpC6,OpC7,OpC8,OpC9,OpCA,OpCB,OpCC,OpCD,OpCE,OpCF,
        OpD0,OpD1,OpD2,OpD3,OpD4,OpD5,OpD6,OpD7,OpD8,OpD9,OpDA,OpDB,OpDC,OpDD,OpDE,OpDF,
        OpE0,OpE1,OpE2,OpE3,OpE4,OpE5,OpE6,OpE7,OpE8,OpE9,OpEA,OpEB,OpEC,OpED,OpEE,OpEF,
        OpF0,OpF1,OpF2,OpF3,OpF4,OpF5,OpF6,OpF7,OpF8,OpF9,OpFA,OpFB,OpFC,OpFD,OpFE,OpFF);

  CBOps: Array[0..$FF] of TProcedure =
       (OpCB00,OpCB01,OpCB02,OpCB03,OpCB04,OpCB05,OpCB06,OpCB07,OpCB08,OpCB09,OpCB0A,OpCB0B,OpCB0C,OpCB0D,OpCB0E,OpCB0F,
        OpCB10,OpCB11,OpCB12,OpCB13,OpCB14,OpCB15,OpCB16,OpCB17,OpCB18,OpCB19,OpCB1A,OpCB1B,OpCB1C,OpCB1D,OpCB1E,OpCB1F,
        OpCB20,OpCB21,OpCB22,OpCB23,OpCB24,OpCB25,OpCB26,OpCB27,OpCB28,OpCB29,OpCB2A,OpCB2B,OpCB2C,OpCB2D,OpCB2E,OpCB2F,
        OpCB30,OpCB31,OpCB32,OpCB33,OpCB34,OpCB35,OpCB36,OpCB37,OpCB38,OpCB39,OpCB3A,OpCB3B,OpCB3C,OpCB3D,OpCB3E,OpCB3F,
        OpCB40,OpCB41,OpCB42,OpCB43,OpCB44,OpCB45,OpCB46,OpCB47,OpCB48,OpCB49,OpCB4A,OpCB4B,OpCB4C,OpCB4D,OpCB4E,OpCB4F,
        OpCB50,OpCB51,OpCB52,OpCB53,OpCB54,OpCB55,OpCB56,OpCB57,OpCB58,OpCB59,OpCB5A,OpCB5B,OpCB5C,OpCB5D,OpCB5E,OpCB5F,
        OpCB60,OpCB61,OpCB62,OpCB63,OpCB64,OpCB65,OpCB66,OpCB67,OpCB68,OpCB69,OpCB6A,OpCB6B,OpCB6C,OpCB6D,OpCB6E,OpCB6F,
        OpCB70,OpCB71,OpCB72,OpCB73,OpCB74,OpCB75,OpCB76,OpCB77,OpCB78,OpCB79,OpCB7A,OpCB7B,OpCB7C,OpCB7D,OpCB7E,OpCB7F,
        OpCB80,OpCB81,OpCB82,OpCB83,OpCB84,OpCB85,OpCB86,OpCB87,OpCB88,OpCB89,OpCB8A,OpCB8B,OpCB8C,OpCB8D,OpCB8E,OpCB8F,
        OpCB90,OpCB91,OpCB92,OpCB93,OpCB94,OpCB95,OpCB96,OpCB97,OpCB98,OpCB99,OpCB9A,OpCB9B,OpCB9C,OpCB9D,OpCB9E,OpCB9F,
        OpCBA0,OpCBA1,OpCBA2,OpCBA3,OpCBA4,OpCBA5,OpCBA6,OpCBA7,OpCBA8,OpCBA9,OpCBAA,OpCBAB,OpCBAC,OpCBAD,OpCBAE,OpCBAF,
        OpCBB0,OpCBB1,OpCBB2,OpCBB3,OpCBB4,OpCBB5,OpCBB6,OpCBB7,OpCBB8,OpCBB9,OpCBBA,OpCBBB,OpCBBC,OpCBBD,OpCBBE,OpCBBF,
        OpCBC0,OpCBC1,OpCBC2,OpCBC3,OpCBC4,OpCBC5,OpCBC6,OpCBC7,OpCBC8,OpCBC9,OpCBCA,OpCBCB,OpCBCC,OpCBCD,OpCBCE,OpCBCF,
        OpCBD0,OpCBD1,OpCBD2,OpCBD3,OpCBD4,OpCBD5,OpCBD6,OpCBD7,OpCBD8,OpCBD9,OpCBDA,OpCBDB,OpCBDC,OpCBDD,OpCBDE,OpCBDF,
        OpCBE0,OpCBE1,OpCBE2,OpCBE3,OpCBE4,OpCBE5,OpCBE6,OpCBE7,OpCBE8,OpCBE9,OpCBEA,OpCBEB,OpCBEC,OpCBED,OpCBEE,OpCBEF,
        OpCBF0,OpCBF1,OpCBF2,OpCBF3,OpCBF4,OpCBF5,OpCBF6,OpCBF7,OpCBF8,OpCBF9,OpCBFA,OpCBFB,OpCBFC,OpCBFD,OpCBFE,OpCBFF);

  DDOps: Array[0..$FF] of TProcedure =
       (NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDD09,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDD19,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,
        NULLOP,OpDD21,OpDD22,OpDD23,OpDD24,OpDD25,OpDD26,NULLOP,NULLOP,OpDD29,OpDD2A,OpDD2B,OpDD2C,OpDD2D,OpDD2E,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,OpDD34,OpDD35,OpDD36,NULLOP,NULLOP,OpDD39,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,OpDD44,OpDD45,OpDD46,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDD4C,OpDD4D,OpDD4E,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,OpDD54,OpDD55,OpDD56,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDD5C,OpDD5D,OpDD5E,NULLOP,
        OpDD60,OpDD61,OpDD62,OpDD63,OpDD64,OpDD65,OpDD66,OpDD67,OpDD68,OpDD69,OpDD6A,OpDD6B,OpDD6C,OpDD6D,OpDD6E,OpDD6F,
        OpDD70,OpDD71,OpDD72,OpDD73,OpDD74,OpDD75,NULLOP,OpDD77,NULLOP,NULLOP,NULLOP,NULLOP,OpDD7C,OpDD7D,OpDD7E,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,OpDD84,OpDD85,OpDD86,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDD8C,OpDD8D,OpDD8E,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,OpDD94,OpDD95,OpDD96,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDD9C,OpDD9D,OpDD9E,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,OpDDA4,OpDDA5,OpDDA6,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDDAC,OpDDAD,OpDDAE,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,OpDDB4,OpDDB5,OpDDB6,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDDBC,OpDDBD,OpDDBE,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDDCB,NULLOP,NULLOP,NULLOP,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDDDD,NULLOP,NULLOP,
        NULLOP,OpDDE1,NULLOP,OpDDE3,NULLOP,OpDDE5,NULLOP,NULLOP,NULLOP,OpDDE9,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,
        NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,NULLOP,OpDDF9,NULLOP,NULLOP,NULLOP,OpDDFD,NULLOP,NULLOP);

  DDCBOps: Array[0..$FF] of TProcedure =
       (OpDDCB00,OpDDCB01,OpDDCB02,OpDDCB03,OpDDCB04,OpDDCB05,OpDDCB06,OpDDCB07,OpDDCB08,OpDDCB09,OpDDCB0A,OpDDCB0B,OpDDCB0C,
        OpDDCB0D,OpDDCB0E,OpDDCB0F,OpDDCB10,OpDDCB11,OpDDCB12,OpDDCB13,OpDDCB14,OpDDCB15,OpDDCB16,OpDDCB17,OpDDCB18,OpDDCB19,
        OpDDCB1A,OpDDCB1B,OpDDCB1C,OpDDCB1D,OpDDCB1E,OpDDCB1F,OpDDCB20,OpDDCB21,OpDDCB22,OpDDCB23,OpDDCB24,OpDDCB25,OpDDCB26,
        OpDDCB27,OpDDCB28,OpDDCB29,OpDDCB2A,OpDDCB2B,OpDDCB2C,OpDDCB2D,OpDDCB2E,OpDDCB2F,OpDDCB30,OpDDCB31,OpDDCB32,OpDDCB33,
        OpDDCB34,OpDDCB35,OpDDCB36,OpDDCB37,OpDDCB38,OpDDCB39,OpDDCB3A,OpDDCB3B,OpDDCB3C,OpDDCB3D,OpDDCB3E,OpDDCB3F,OpDDCB40,
        OpDDCB40,OpDDCB40,OpDDCB40,OpDDCB40,OpDDCB40,OpDDCB40,OpDDCB40,OpDDCB48,OpDDCB48,OpDDCB48,OpDDCB48,OpDDCB48,OpDDCB48,
        OpDDCB48,OpDDCB48,OpDDCB50,OpDDCB50,OpDDCB50,OpDDCB50,OpDDCB50,OpDDCB50,OpDDCB50,OpDDCB50,OpDDCB58,OpDDCB58,OpDDCB58,
        OpDDCB58,OpDDCB58,OpDDCB58,OpDDCB58,OpDDCB58,OpDDCB60,OpDDCB60,OpDDCB60,OpDDCB60,OpDDCB60,OpDDCB60,OpDDCB60,OpDDCB60,
        OpDDCB68,OpDDCB68,OpDDCB68,OpDDCB68,OpDDCB68,OpDDCB68,OpDDCB68,OpDDCB68,OpDDCB70,OpDDCB70,OpDDCB70,OpDDCB70,OpDDCB70,
        OpDDCB70,OpDDCB70,OpDDCB70,OpDDCB78,OpDDCB78,OpDDCB78,OpDDCB78,OpDDCB78,OpDDCB78,OpDDCB78,OpDDCB78,OpDDCB80,OpDDCB81,
        OpDDCB82,OpDDCB83,OpDDCB84,OpDDCB85,OpDDCB86,OpDDCB87,OpDDCB88,OpDDCB89,OpDDCB8A,OpDDCB8B,OpDDCB8C,OpDDCB8D,OpDDCB8E,
        OpDDCB8F,OpDDCB90,OpDDCB91,OpDDCB92,OpDDCB93,OpDDCB94,OpDDCB95,OpDDCB96,OpDDCB97,OpDDCB98,OpDDCB99,OpDDCB9A,OpDDCB9B,
        OpDDCB9C,OpDDCB9D,OpDDCB9E,OpDDCB9F,OpDDCBA0,OpDDCBA1,OpDDCBA2,OpDDCBA3,OpDDCBA4,OpDDCBA5,OpDDCBA6,OpDDCBA7,OpDDCBA8,
        OpDDCBA9,OpDDCBAA,OpDDCBAB,OpDDCBAC,OpDDCBAD,OpDDCBAE,OpDDCBAF,OpDDCBB0,OpDDCBB1,OpDDCBB2,OpDDCBB3,OpDDCBB4,OpDDCBB5,
        OpDDCBB6,OpDDCBB7,OpDDCBB8,OpDDCBB9,OpDDCBBA,OpDDCBBB,OpDDCBBC,OpDDCBBD,OpDDCBBE,OpDDCBBF,OpDDCBC0,OpDDCBC1,OpDDCBC2,
        OpDDCBC3,OpDDCBC4,OpDDCBC5,OpDDCBC6,OpDDCBC7,OpDDCBC8,OpDDCBC9,OpDDCBCA,OpDDCBCB,OpDDCBCC,OpDDCBCD,OpDDCBCE,OpDDCBCF,
        OpDDCBD0,OpDDCBD1,OpDDCBD2,OpDDCBD3,OpDDCBD4,OpDDCBD5,OpDDCBD6,OpDDCBD7,OpDDCBD8,OpDDCBD9,OpDDCBDA,OpDDCBDB,OpDDCBDC,
        OpDDCBDD,OpDDCBDE,OpDDCBDF,OpDDCBE0,OpDDCBE1,OpDDCBE2,OpDDCBE3,OpDDCBE4,OpDDCBE5,OpDDCBE6,OpDDCBE7,OpDDCBE8,OpDDCBE9,
        OpDDCBEA,OpDDCBEB,OpDDCBEC,OpDDCBED,OpDDCBEE,OpDDCBEF,OpDDCBF0,OpDDCBF1,OpDDCBF2,OpDDCBF3,OpDDCBF4,OpDDCBF5,OpDDCBF6,
        OpDDCBF7,OpDDCBF8,OpDDCBF9,OpDDCBFA,OpDDCBFB,OpDDCBFC,OpDDCBFD,OpDDCBFE,OpDDCBFF);

  EDOps: Array [0..$FF] of TProcedure =
       (OpED00,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        OpED40,OpED41,OpED42,OpED43,OpED44,OpED45,OpED46,OpED47,OpED48,OpED49,OpED4A,OpED4B,OpED4C,OpED4D,OpED4E,OpED4F,
        OpED50,OpED51,OpED52,OpED53,OpED54,OpED55,OpED56,OpED57,OpED58,OpED59,OpED5A,OpED5B,OpED5C,OpED5D,OpED5E,OpED5F,
        OpED60,OpED61,OpED62,OpED63,OpED64,OpED65,OpED66,OpED67,OpED68,OpED69,OpED6A,OpED6B,OpED6C,OpED6D,OpED6E,OpED6F,
        OpED70,OpED71,OpED72,OpED73,OpED74,OpED75,OpED76,NULLED,OpED78,OpED79,OpED7A,OpED7B,OpED7C,OpED7D,OpED7E,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        OpEDA0,OpEDA1,OpEDA2,OpEDA3,NULLED,NULLED,NULLED,NULLED,OpEDA8,OpEDA9,OpEDAA,OpEDAB,NULLED,NULLED,NULLED,NULLED,
        OpEDB0,OpEDB1,OpEDB2,OpEDB3,NULLED,NULLED,NULLED,NULLED,OpEDB8,OpEDB9,OpEDBA,OpEDBB,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,
        NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED,NULLED);

  // Spectrum Colour Palette

  TFSpecBack:     TFColor = (b:$26;g:$26;r:$26);
  TFSpecBlack:    TFcolor = (b:0;g:0;r:0);
  TFSpecBlue:     TFColor = (b:202;g:0;r:0);
  TFSpecRed:      TFColor = (b:0;g:0;r:202);
  TFSpecMagenta:  TFColor = (b:202;g:0;r:202);
  TFSpecGreen:    TFColor = (b:0;g:202;r:0);
  TFSpecCyan:     TFColor = (b:202;g:202;r:0);
  TFSpecYellow:   TFColor = (b:0;g:202;r:202);
  TFSpecWhite:    TFColor = (b:197;g:199;r:197);
  TFSpecBlueB:    TFColor = (b:255;g:0;r:0);
  TFSpecRedB:     TFColor = (b:0;g:0;r:255);
  TFSpecMagentaB: TFColor = (b:255;g:0;r:255);
  TFSpecGreenB:   TFColor = (b:0;g:255;r:0);
  TFSpecCyanB:    TFColor = (b:255;g:255;r:0);
  TFSpecYellowB:  TFColor = (b:0;g:255;r:255);
  TFSpecWhiteB:   TFColor = (b:255;g:255;r:255);

implementation

Uses ConsoleOutput,BASinMain, Utility, Display, LogWind, Sound, Evaluate, PrinterOutput, Profiling, CPUDisplay, MemMap;  //basinmidi

Function INITEmulation: Boolean;
Begin

  DebugLog('Init Emulation');

  Filename := BASinDir+'\48.Rom';
  Result := LoadROM(Memory);

  If Result Then Begin

     Filename := BASinDir+'\Plus2.Rom';
     LoadROM128k(Rom128k);
     ModifyROM;
     BuildTables;
     SetUpGraphics;

     Reset;
     InInterrupt := False;
     ControlEmulation(True);
     InsertList := TStringlist.Create;
     ClipList := TStringlist.Create;
     BASinOutput.Running := False;
     SaveEmulationState(UndoState);

     InitWorkerThread;

  End;

End;

Procedure CloseEmulation;
Begin
  DebugLog('Close Emulation');
  CloseWorkerThread;
  InsertList.Free;
  ClipList.Free;
  FastCore.Display.Free;
  FastCore.Display := Nil;
  CloseSound;
End;

Procedure Reset;
Begin
  PagedBank := 0;
  RAMDiskNeedsInit := True;
  PageROM(False);
  ScreenPointer := @Memory[16384];
  ShadowScreen := False;
  DisablePaging := False;
  PutWord(@Memory[PROG], 65535);
  PutWord(@Memory[VARS], 65535);
  With Registers do Begin
     PC := 0; F  := 0; I  := 0; R  := 0;
     A  := 0; B  := 0; C  := 0; D   := 0; H := 0;  L := 0;
     An := 0; Bn := 0; Cn := 0; Dn  := 0; Hn := 0; Ln := 0;
     IX := 0; IY := 0; SP := 0; SPn := 0;
     IntMode := 0;
     IntsEnabled := False;
     HaltEMU := False;
     FlashState := 0;
  End;
  ElapsedFrames := 0;
  ClearKeys;
  ResetCounter := 0;
   TrashScreenMem;
  BorderDWord := 0;
  AYRegisters.R7 := 255;
  AYRegisters.RandomSeed1 := 0;
  AYRegisters.RandomSeed2 := 1;
End;

Procedure TrashScreenMem;
Var
  F: DWord;
Begin
  // Produces screen trash - a nice touch on a reset.
  For F := 0 To 6911 Do
     Memory[F+16384] := Random(256);
End;

Procedure BuildTables;
Var
  B: Byte;
  F, G, R, SetCounter: Integer;
  P: Boolean;
  fstream: tfilestream;
Begin
  DebugLog('Build Emulation LUTs');
  For F := 0 To 255 Do Begin
     P := True;
     SetCounter := 0;
     For G := 0 To 7 do
        If (F And (1 Shl G)) <> 0 Then Begin
           P := Not P;
           Inc(SetCounter);
        End;
     If P then Parity[F] := 1 Else Parity[F] := 0;
     B := F;
     If ((B And 128) = 128) Then
        R := -(256 - B)
     Else
        R := B;
     TwoComp[F] := R;
     SetCount[F] := SetCounter;
     BitSetCount[F] := 192 - (SetCounter * 24);
     BitSetCount[F] := (BitSetCount[F] Shl 16) + (BitSetCount[F] Shl 8) + BitSetCount[F];
  End;

End;

Procedure SetUpGraphics;
Begin

  DebugLog('Set up Emulation Graphics');
  SetUpScreenLUT;
  PrepareSpecColourTable;

  NeedDisplayUpdate := True;

  FastCore.Display := TFastDIB.Create;
  FastCore.Display.SetSize(320, -240, 8);
  FastCore.Display.Colors := @DisplayPalette;
  FastCore.Display.UpdateColors;

End;

Procedure SetUpScreenLUT;
Var
  YPoint, XPoint, row, Address, Block, Offset: DWord;
Begin
  DebugLog('Create Screen LUT');
  For Address := 0 To 6143 Do Begin
     XPoint := Address And 31;
     Row := (Address And 224) Div 32;
     Offset := (Address And 1792) Div 256;
     Block := (Address And 6144) Div 2048;
     YPoint := Offset+Row*8+Block*64;
     ScreenOffsets[Address] := YPoint;
     AttrAddresses[Address] := 6144 + ((((YPoint) Div 8)*32)+XPoint);
     If Address Mod 32 = 0 Then ScreenAddresses[YPoint] := Address;
  End;
  BuildPalette([TFSpecBlack, TFSpecBlue,  TFSpecRed,  TFSpecMagenta,  TFSpecGreen,  TFSpecCyan,  TFSpecYellow,  TFSpecWhite,
                TFSpecBlack, TFSpecBlueB, TFSpecRedB, TFSpecMagentaB, TFSpecGreenB, TFSpecCyanB, TFSpecYellowB, TFSpecWhiteB]);
End;

Function GetScreenAddr(X, Y: DWord): DWord;
Begin
  Result := ScreenAddresses[Y]+(X Div 8);
End;

Function GetScreenCoord(Address: Word): TPoint;
Begin
  Dec(Address, 16384);
  Result.X := Address and 31;
  Result.Y := ScreenOffsets[Address];
End;

Function GetAttrAddr(X, Y: Integer): DWord;
Begin
  Result := X Div 8 + ((Y Div 8) * 32);
End;

Procedure BuildPalette(Clrs: Array of TFColor);
Var
  F: Integer;
Begin
  For F := 0 To 15 Do begin
     DisplayPalette[F].r := Clrs[F].r;
     DisplayPalette[F].g := Clrs[F].g;
     DisplayPalette[F].b := Clrs[F].b;
     _2xSaIColours16[F] := (((Clrs[F].r and 248) Shr 3) shl 10)
                         + (((Clrs[F].g and 248) Shr 3) shl 5)
                         + (((Clrs[F].b and 248) Shr 3));
     _Hq2xColours16[F]  := ((Clrs[F].r and 248) Shl 8)
                         + ((Clrs[F].g and 252) Shl 3)
                         + ((Clrs[F].b and 248) Shr 3);
  End;
End;

Procedure PrepareSpecColourTable;
Var
  TabPtr:  DWord;
  Bright, Paper, Ink, N: Byte;
  BitMask, BitCount: Byte;
Begin
  TabPtr := 0;
  For Bright := 0 To 1 Do Begin
     For Paper := 0 To 7 Do Begin
        For Ink := 0 To 7 Do Begin
           For N := 0 To 255 Do Begin
              BitMask := 128;
              For BitCount := 0 To 7 Do Begin
                 If N And BitMask > 0 Then
                    DrawTSColTable[TabPtr] := (8*Bright) + Ink
                 Else DrawTSColTable[TabPtr] := (8*Bright) + Paper;
                 Inc(TabPtr);
                 BitMask := BitMask Shr 1;
              End;
           End;
        End;
     End;
  End;
  For N := 0 To 127 Do Begin
     FlashSwapTable[N] := (N And 64) Or ((N And 7) Shl 3) Or ((N And 56) Shr 3);
  End;
End;

Procedure EmulateRET;
Begin
  Registers.PC := GetWord(@Memory[Registers.SP]);
  Inc(Registers.SP, 2);
End;

Procedure EmulateCALL(Address: Word);
Begin
  Dec(Registers.SP, 2);
  PutWord(@Memory[Registers.SP], Registers.PC+3);
  Registers.PC := Address;
End;

Procedure EmulatePUSH(Value: Word);
Begin
  Dec(Registers.SP, 2);
  PutWord(@Memory[Registers.SP], Value);
End;

Function EmulatePOP: Word;
Begin
  Result := GetWord(@Memory[Registers.SP]);
  Inc(Registers.SP, 2);
End;

Function GetByte(WordPtr: Pointer): Byte;
asm
  mov al, [eax]
End;

Function GetWord(WordPtr: Pointer): Word;
Asm
  mov ax, [eax]
End;

Function GetDWord(WordPtr: Pointer): DWord;
Asm
  mov eax, [eax]
End;

Procedure PutWord(WordPtr: Pointer; Value: Word);
Asm
  mov word [eax], dx
End;

Procedure PutByte(WordPtr: Pointer; Value: Byte);
Asm
  mov byte [eax], dl
End;

Procedure PutDWord(WordPtr: Pointer; Value: DWord);
Asm
  mov dword [eax], edx
End;

Procedure IncWord(WordPtr: Pointer);
Asm
  inc Word [eax]
End;

Procedure DecWord(WordPtr: Pointer);
Asm
  dec Word [eax]
End;

// start core procs

Procedure NULLOP;
Begin
  asm
     mov edx, 8
     ret
  end;
End;

Procedure NULLED;
Begin
  asm
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx, 8
  end;
End;

Procedure Op00;
Begin // NOP
  asm
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx, 4
  end;
End;

Procedure Op01;
Begin // LD BC,NN
  asm
     mov   Word [esi+TZ80Registers.C], dx // dx = PC+1, PC+2
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,10
  end;
End;

Procedure Op02;
Begin // LD (BC),A
  asm
     mov   ax, Word [esi+TZ80Registers.C]
     mov   dl, [esi+TZ80Registers.A]
     Inc   Word [esi+TZ80Registers.PC]
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax], dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov   edx,7
  end;
End;

Procedure Op03;
Begin // INC BC
  asm
     inc Word [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     mov edx,6
  end;
End;

Procedure Op04;
Begin // INC B
  asm
     lea  ecx, [esi+TZ80Registers.B]
     mov  ah, [esi+TZ80Registers.F]
     mov  al, [ecx]
     sahf
     inc  al
     lahf
     mov  Byte [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op05;
Begin // DEC B
  asm
     lea  ecx, [esi+TZ80Registers.B]
     mov  ah, [esi+TZ80Registers.F]
     mov  al, [ecx]
     sahf
     dec  al
     lahf
     mov  Byte [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op06;
Begin // LD B,N
  asm
     add word [esi+TZ80Registers.PC], 2
     mov byte [esi+TZ80Registers.B],dl
     mov edx,7
  end;
End;

Procedure Op07;
Begin // RLCA  --503-0C
  Asm
     mov cl,[esi+TZ80Registers.F]
     mov al,[esi+TZ80Registers.A]
     and cl,11000100b
     rol al,1
     adc cl,0
     mov Byte [esi+TZ80Registers.A],al
     and al,00101000b
     or  cl,al
     mov Byte [esi+TZ80Registers.F],cl
     inc Word [esi+TZ80Registers.PC]
     mov edx,4
  End;
End;

Procedure Op08;
Begin // EX AF,AF'
  asm
     mov cx, Word [esi+TZ80Registers.F]
     mov dx, Word [esi+TZ80Registers.Fn]
     mov Word [esi+TZ80Registers.Fn], cx
     mov Word [esi+TZ80Registers.F],  dx
     inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op09;
Begin // ADD HL,BC
  asm
     mov  ax,Word [esi+TZ80Registers.C]
     mov  cx,Word [esi+TZ80Registers.L]
     add  cx,ax
     lahf
     mov  Word [esi+TZ80Registers.L],cx
     mov  cl,[esi+TZ80Registers.F]
     and  ah,00010001b // H,C affected by addition. N is cleared
     and  cl,11000100b // S,Z,V are unaffected
     and  ch,00101000b // 5,3 from H after addition
     or   ah,cl
     or   ah,ch
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,11
  end;
End;

Procedure Op0A;
Begin // LD A,(BC)
  asm
     mov  ax, Word [esi+TZ80Registers.C]
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [edi+eax]
     mov  Byte [esi+TZ80Registers.A], al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure Op0B;
Begin // DEC BC
  Asm
     dec Word [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     mov edx,6
  End;
End;

Procedure Op0C;
Begin // INC C
  asm
     lea  ecx, [esi+TZ80Registers.C]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op0D;
Begin // DEC C
  asm
     lea  ecx, [esi+TZ80Registers.C]
     mov  ah,  [esi+TZ80Registers.F]
     mov  al,  [ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op0E;
Begin // LD C,N
  asm
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.C],dl
     mov edx,7
  end;
End;

Procedure Op0F;
Begin // RRCA  --503-0C
  Asm
     mov cl,[esi+TZ80Registers.F]
     mov al,[esi+TZ80Registers.A]
     and cl,11000100b
     ror al,1
     adc cl,0
     mov Byte [esi+TZ80Registers.A],al
     and al,00101000b
     or  cl,al
     mov Byte [esi+TZ80Registers.F],cl
     inc Word [esi+TZ80Registers.PC]
     mov edx,4
  End;
End;

Procedure Op10;
Begin // DJNZ e
  asm
     Add Word [esi+TZ80Registers.PC], 2
     Dec Byte [esi+TZ80Registers.B]
     mov al, dl
     jz @skip
     cbw
     add Word [esi+TZ80Registers.PC], ax
     mov edx,13
     ret
  @Skip:
     mov edx,8
  end;
End;

Procedure Op11;
Begin // LD DE,NN
  asm
     mov   Word [esi+TZ80Registers.E],dx
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,10
  end;
End;

Procedure Op12;
Begin // LD (DE),A
  asm
     mov   ax,Word [esi+TZ80Registers.E]
     mov   dl,[esi+TZ80Registers.A]
     inc   Word [esi+TZ80Registers.PC]
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax], dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov   edx,7
  end;
End;

Procedure Op13;
Begin // INC DE
  asm
     inc Word [esi+TZ80Registers.E]
     inc Word [esi+TZ80Registers.PC]
     mov edx,6
  end;
End;

Procedure Op14;
Begin // INC D
  asm
     lea  ecx, [esi+TZ80Registers.D]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op15;
Begin // DEC D
  asm
     lea  ecx, [esi+TZ80Registers.D]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op16;
Begin // LD D,N
  asm
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.D],dl
     mov edx,7
  end;
End;

Procedure Op17;
Begin // RLA   --503-0C
  Asm
     mov dl,[esi+TZ80Registers.F]
     mov cl,dl
     and dl,11000100b
     mov al,[esi+TZ80Registers.A]
     ror cl,1
     rcl al,1
     adc dl,0
     mov Byte [esi+TZ80Registers.A],al
     and al,00101000b
     or  dl,al
     mov Byte [esi+TZ80Registers.F],dl
     inc Word [esi+TZ80Registers.PC]
     mov edx,4
  End;
End;

Procedure Op18;
Begin // JR $+2
  asm
     Add Word [esi+TZ80Registers.PC], 2
     mov al, dl
     cbw
     add Word [esi+TZ80Registers.PC], ax
     mov edx,12
  end;
End;

Procedure Op19;
Begin // ADD HL,DE

  asm
     mov   ax,Word [esi+TZ80Registers.E]
     mov   cx,Word [esi+TZ80Registers.L]
     add   cx,ax
     lahf
     mov   Word [esi+TZ80Registers.L],cx
     mov   cl,[esi+TZ80Registers.F]
     and   ah,00010001b // H,C affected by addition. N is cleared
     and   cl,11000100b // S,Z,V are unaffected
     and   ch,00101000b // 5,3 from H after addition
     or    ah,cl
     or    ah,ch
     mov   Byte [esi+TZ80Registers.F],ah
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,11
  end;
End;

Procedure Op1A;
Begin // LD A,(DE)
  asm
     mov  ax, Word [esi+TZ80Registers.E]
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [edi+eax]
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure Op1B;
Begin // DEC DE
  asm
     dec Word [esi+TZ80Registers.E]
     inc Word [esi+TZ80Registers.PC]
     mov  edx,6
  end;
End;

Procedure Op1C;
Begin // INC E
  asm
     lea  ecx, [esi+TZ80Registers.E]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op1D;
Begin // DEC E
  asm
     lea  ecx, [esi+TZ80Registers.E]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op1E;
Begin // LD E,N
  asm
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.E],dl
     mov edx,7
  end;
End;

Procedure Op1F;
Begin // RRA   --503-0C
  Asm
     mov dl,[esi+TZ80Registers.F]
     mov cl,dl
     and dl,11000100b
     mov al,[esi+TZ80Registers.A]
     ror cl,1
     rcr al,1
     adc dl,0
     mov Byte [esi+TZ80Registers.A],al
     and al,00101000b
     or  dl,al
     mov Byte [esi+TZ80Registers.F],dl
     inc Word [esi+TZ80Registers.PC]
     mov edx,4
  End;
End;

Procedure Op20;
Begin // JR NZ,$+2
  asm
     Add Word [esi+TZ80Registers.PC], 2
     mov bl, [esi+TZ80Registers.F]
     and bl, 64 // Z = 64
     jnz @Skip

     mov al, dl
     cbw
     add Word [esi+TZ80Registers.PC], ax
     mov edx,12
     ret

  @Skip:
     mov edx,7
  end;
End;

Procedure Op21;
Begin // LD HL,NN
  asm
     mov   Word [esi+TZ80Registers.L], dx
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,10
  end;
End;

Procedure Op22;
Begin // LD (NN),HL
  asm
     and   edx, $FFFF
     mov   bx, Word [esi+TZ80Registers.L]
     add   Word [esi+TZ80Registers.PC], 3
     cmp   edx, $4000
     jb    @NoWrite
     mov   Word [edi+edx], bx
     or    Byte [MemAccess[0]+edx], MemWrite
  @NoWrite:
     mov   edx,16
  End;
End;

Procedure Op23;
Begin // INC HL
  asm
     inc Word [esi+TZ80Registers.L]
     inc Word [esi+TZ80Registers.PC]
     mov edx,6
  end;
End;

Procedure Op24;
Begin // INC H
  asm
     lea  ecx, [esi+TZ80Registers.H]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op25;
Begin // DEC H
  asm
     lea  ecx, [esi+TZ80Registers.H]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op26;
Begin // LD H,N
  asm
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.H],dl
     mov edx,7
  end;
End;

Procedure Op27;
Begin // DAA
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[esi+TZ80Registers.A]
     mov  dh,ah
     test ah,2        // if last op was a subtraction
     jne  @Sub        // then adjust for subtraction
     sahf
     daa
     jmp  @SetDAAFlags
  @Sub:
     sahf
     das
  @SetDAAFlags:
     lahf
     and  ah,11010101b    // S,Z,H,PV,C
     mov  Byte [esi+TZ80Registers.A],al
     and  al,00101000b    // F5,F3
     or   ah,al
     and  Registers.F,2   // preserve N flag only
     or   Byte [esi+TZ80Registers.F],ah  // apply new flags
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op28;
Begin // JR Z,$+2
  asm
     Add Word [esi+TZ80Registers.PC], 2
     mov bl, [esi+TZ80Registers.F]
     and bl, 64 // Z = 64
     jz  @NonZero

     mov al, dl
     cbw
     add Word [esi+TZ80Registers.PC], ax
     mov edx,12
     ret

  @NonZero:
     mov edx,7
  end;
End;

Procedure Op29;
Begin // ADD HL,HL
  asm
     mov  cx,Word [esi+TZ80Registers.L]
     add  cx,cx
     lahf
     mov  Word [esi+TZ80Registers.L],cx
     mov  cl,[esi+TZ80Registers.F]
     and  ah,00010001b // H,C affected by addition. N is cleared
     and  cl,11000100b // S,Z,V are unaffected
     and  ch,00101000b // 5,3 from H after addition
     or   ah,cl
     or   ah,ch
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,11
  end;
End;

Procedure Op2A;
Begin // LD HL,(NN)
  asm
     and  edx, $FFFF
     or   Byte [MemAccess[0]+edx], MemRead
     mov  ax, Word [edi+edx]
     mov  Word [esi+TZ80Registers.L],ax
     Add  Word [esi+TZ80Registers.PC], 3
     mov  edx,16
  end;
End;

Procedure Op2B;
Begin // DEC HL
  asm
     dec Word [esi+TZ80Registers.L]
     inc Word [esi+TZ80Registers.PC]
     mov edx,6
  end;
End;

Procedure Op2C;
Begin // INC L
  asm
     lea  ecx, [esi+TZ80Registers.L]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op2D;
Begin // DEC L
  asm
     lea  ecx, [esi+TZ80Registers.L]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op2E;
Begin // LD L,N
  asm
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.L],dl
     mov edx,7
  end;
End;

Procedure Op2F;
Begin // CPL   --*1*-1-   F5, F3 from A register
  asm
     mov ax, Word [esi+TZ80Registers.F]
     xor ah, $FF
     and al, 197
     mov bl, ah
     or  al, 18
     and bl, 40
     inc Word [esi+TZ80Registers.PC]
     add al, bl
     mov Word [esi+TZ80Registers.F], ax
     mov edx,4
  end;
End;

Procedure Op30;
Begin // JR NC,$+2
  asm
     Add Word [esi+TZ80Registers.PC], 2
     mov bl, [esi+TZ80Registers.F]
     and bl, 1 // C = 1
     jnz @Skip

     mov al, dl
     cbw
     add Word [esi+TZ80Registers.PC], ax
     mov edx,12
     ret

  @Skip:
     mov edx,7
  end;
End;

Procedure Op31;
Begin // LD SP,NN
  asm
     Add   Word [esi+TZ80Registers.PC], 3
     mov   Word [esi+TZ80Registers.&SP], dx
     mov   edx,10
  end;
End;

Procedure Op32;
Begin // LD (NN),A
  asm
     and   edx, $FFFF
     mov   bl, [esi+TZ80Registers.A]
     Add   Word [esi+TZ80Registers.PC], 3
     cmp   edx, $4000
     jb    @NoWrite
     or    Byte [MemAccess[0]+edx], MemWrite
     mov   Byte [edi+edx], bl
  @NoWrite:
     mov   edx,13
  end;
End;

Procedure Op33;
Begin // INC SP
  asm
     inc Word [esi+TZ80Registers.&SP]
     inc Word [esi+TZ80Registers.PC]
     mov edx,6
  end;
End;

Procedure Op34;
Begin // INC (HL)
  asm
     mov  bx, Word [esi+TZ80Registers.L]
     Inc  Word [esi+TZ80Registers.PC]
     or   Byte [MemAccess[0]+ebx], MemRead
     lea  ecx, DWord [edi+ebx]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     cmp  ebx, $4000
     jb   @NoWrite
     or   Byte [MemAccess[0]+ebx], MemWrite
     mov  [ecx],al
  @NoWrite:
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     mov  edx,11
  end;
End;

Procedure Op35;
Begin // DEC (HL)
  asm
     mov  bx, Word [esi+TZ80Registers.L]
     Inc  Word [esi+TZ80Registers.PC]
     or   Byte [MemAccess[0]+ebx], MemRead
     lea  ecx, DWord [edi+ebx]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     cmp  ebx, $4000
     jb   @NoWrite
     or   Byte [MemAccess[0]+ebx], MemWrite
     mov  [ecx],al
  @NoWrite:
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  edx,11
  end;
End;

Procedure Op36;
Begin // LD (HL),N
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     Add   Word [esi+TZ80Registers.PC], 2
     cmp   ebx, $4000
     jb    @NoWrite
     or    Byte [MemAccess[0]+ebx], MemWrite
     mov   Byte [edi+ebx], dl
  @NoWrite:
     mov   edx,10
  End;
End;

Procedure Op37;
Begin // SCF
  asm
     mov ah,[esi+TZ80Registers.F]
     mov dl,[esi+TZ80Registers.A]
     or  ah,1
     and ah,11000101b
     and dl,00101000b
     or  ah,dl
     mov Byte [esi+TZ80Registers.F],ah
     Inc Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op38;
Begin // JR C,$+2
  asm
     Add Word [esi+TZ80Registers.PC], 2
     mov bl, [esi+TZ80Registers.F]
     and bl, 1 // C = 1
     jz  @Skip

     mov al, dl
     cbw
     add Word [esi+TZ80Registers.PC], ax
     mov edx,12
     ret

  @Skip:
     mov edx,7
  end;
End;

Procedure Op39;
Begin // ADD HL,SP
  asm
     mov  ax,Word [esi+TZ80Registers.&SP]
     mov  cx,Word [esi+TZ80Registers.L]
     add  cx,ax
     lahf
     mov  Word [esi+TZ80Registers.L],cx
     mov  cl,[esi+TZ80Registers.F]
     and  ah,00010001b // H,C affected by addition. N is cleared
     and  cl,11000100b // S,Z,V are unaffected
     and  ch,00101000b // 5,3 from H after addition
     or   ah,cl
     or   ah,ch
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,11
  end;
End;

Procedure Op3A;
Begin // LD A,(NN)
  asm
     and   edx, $FFFF
     add   Word [esi+TZ80Registers.PC], 3
     or    Byte [MemAccess[0]+edx], MemRead
     mov   bl, Byte [edi+edx]
     mov   Byte [esi+TZ80Registers.A], bl
     mov   edx,13
  end;
End;

Procedure Op3B;
Begin // DEC SP
  asm
     dec Word [esi+TZ80Registers.&SP]
     inc Word [esi+TZ80Registers.PC]
     mov edx,6
  end;
End;

Procedure Op3C;
Begin // INC A
  asm
     lea  ecx, [esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op3D;
Begin // DEC A
  asm
     lea  ecx, [esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op3E;
Begin // LD A,N
  asm
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.A],dl
     mov edx,7
  end;
End;

Procedure Op3F;
Begin // CCF   --***-0*  C=1-C, H as old C; F5, F3 from A register
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  cl,al
     and  cl,11000101b // Clr 5,H,3,N
     test al,1
     je   @Fwd
     or   cl,00010000b // SetH
  @Fwd:
     xor  cl,1         // C=1-C
     mov  al,[esi+TZ80Registers.A]
     and  al,00101000b
     or   cl,al
     mov  Byte [esi+TZ80Registers.F],cl
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op40;
Begin // LD B,B
  asm
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op41;
Begin // LD B,C
  asm
     mov al, [esi+TZ80Registers.C]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,4
  end;
End;

Procedure Op42;
Begin // LD B,D
  asm
     mov al, [esi+TZ80Registers.D]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,4
  end;
End;

Procedure Op43;
Begin // LD B,E
  asm
     mov al, [esi+TZ80Registers.E]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,4
  end;
End;

Procedure Op44;
Begin // LD B,H
  asm
     mov al, [esi+TZ80Registers.H]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,4
  end;
End;

Procedure Op45;
Begin // LD B,L
  asm
     mov al, [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,4
  end;
End;

Procedure Op46;
Begin // LD B,(HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov bl, Byte [edi+eax]
     or  Byte [MemAccess[0]+eax], MemRead
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], bl
     mov edx,7
  end;
End;

Procedure Op47;
Begin // LD B,A
  asm
     mov al, [esi+TZ80Registers.A]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,4
  end;
End;

Procedure Op48;
Begin // LD C,B
  asm
     mov al, [esi+TZ80Registers.B]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,4
  end;
End;

Procedure Op49;
Begin // LD C,C
  asm
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op4A;
Begin // LD C,D
  asm
     mov al, [esi+TZ80Registers.D]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,4
  end;
End;

Procedure Op4B;
Begin // LD C,E
  asm
     mov al, [esi+TZ80Registers.E]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,4
  end;
End;

Procedure Op4C;
Begin // LD C,H
  asm
     mov al, [esi+TZ80Registers.H]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,4
  end;
End;

Procedure Op4D;
Begin // LD C,L
  asm
     mov al, [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,4
  end;
End;

Procedure Op4E;
Begin // LD C,(HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov bl, Byte [edi+eax]
     or  Byte [MemAccess[0]+eax], MemRead
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], bl
     mov edx,7
  end;
End;

Procedure Op4F;
Begin // LD C,A
  asm
     mov al, [esi+TZ80Registers.A]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,4
  end;
End;

Procedure Op50;
Begin // LD D,B
  asm
     mov al, [esi+TZ80Registers.B]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,4
  end;
End;

Procedure Op51;
Begin // LD D,C
  asm
     mov al, [esi+TZ80Registers.C]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,4
  end;
End;

Procedure Op52;
Begin // LD D,D
  asm
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op53;
Begin // LD D,E
  asm
     mov al, [esi+TZ80Registers.E]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,4
  end;
End;

Procedure Op54;
Begin // LD D,H
  asm
     mov al, [esi+TZ80Registers.H]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,4
  end;
End;

Procedure Op55;
Begin // LD D,L
  asm
     mov al, [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,4
  end;
End;

Procedure Op56;
Begin // LD D,(HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov bl, Byte [edi+eax]
     or  Byte [MemAccess[0]+eax], MemRead
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], bl
     mov edx,7
  end;
End;

Procedure Op57;
Begin // LD D,A
  asm
     mov al, [esi+TZ80Registers.A]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,4
  end;
End;

Procedure Op58;
Begin // LD E,B
  asm
     mov al, [esi+TZ80Registers.B]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,4
  end;
End;

Procedure Op59;
Begin // LD E,C
  asm
     mov al, [esi+TZ80Registers.C]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,4
  end;
End;

Procedure Op5A;
Begin // LD E,D
  asm
     mov al, [esi+TZ80Registers.D]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,4
  end;
End;

Procedure Op5B;
Begin // LD E,E
  asm
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op5C;
Begin // LD E,H
  asm
     mov al, [esi+TZ80Registers.H]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,4
  end;
End;

Procedure Op5D;
Begin // LD E,L
  asm
     mov al, [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,4
  end;
End;

Procedure Op5E;
Begin // LD E,(HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov bl, Byte [edi+eax]
     or  Byte [MemAccess[0]+eax], MemRead
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], bl
     mov edx,7
  end;
End;

Procedure Op5F;
Begin // LD E,A
  asm
     mov al, [esi+TZ80Registers.A]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,4
  end;
End;

Procedure Op60;
Begin // LD H,B
  asm
     mov al, [esi+TZ80Registers.B]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.H], al
     mov edx,4
  end;
End;

Procedure Op61;
Begin // LD H,C
  asm
     mov al, [esi+TZ80Registers.C]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.H], al
     mov edx,4
  end;
End;

Procedure Op62;
Begin // LD H,D
  asm
     mov al, [esi+TZ80Registers.D]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.H], al
     mov edx,4
  end;
End;

Procedure Op63;
Begin // LD H,E
  asm
     mov al, [esi+TZ80Registers.E]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.H], al
     mov edx,4
  end;
End;

Procedure Op64;
Begin // LD H,H
  asm
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op65;
Begin // LD H,L
  asm
     mov al, [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.H], al
     mov edx,4
  end;
End;

Procedure Op66;
Begin // LD H,(HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov bl, Byte [edi+eax]
     or  Byte [MemAccess[0]+eax], MemRead
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.H], bl
     mov edx,7
  end;
End;

Procedure Op67;
Begin // LD H,A
  asm
     mov al, [esi+TZ80Registers.A]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.H], al
     mov edx,4
  end;
End;

Procedure Op68;
Begin // LD L,B
  asm
     mov al, [esi+TZ80Registers.B]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.L], al
     mov edx,4
  end;
End;

Procedure Op69;
Begin // LD L,C
  asm
     mov al, [esi+TZ80Registers.C]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.L], al
     mov edx,4
  end;
End;

Procedure Op6A;
Begin // LD L,D
  asm
     mov al, [esi+TZ80Registers.D]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.L], al
     mov edx,4
  end;
End;

Procedure Op6B;
Begin // LD L,E
  asm
     mov al, [esi+TZ80Registers.E]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.L], al
     mov edx,4
  end;
End;

Procedure Op6C;
Begin // LD L,H
  asm
     mov al, [esi+TZ80Registers.H]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.L], al
     mov edx,4
  end;
End;

Procedure Op6D;
Begin // LD L,L
  asm
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op6E;
Begin // LD L,(HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov bl, Byte [edi+eax]
     or  Byte [MemAccess[0]+eax], MemRead
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.L], bl
     mov edx,7
  end;
End;

Procedure Op6F;
Begin // LD L,A
  asm
     mov al, [esi+TZ80Registers.A]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.L], al
     mov edx,4
  end;
End;

Procedure Op70;
Begin // LD (HL),B
  asm
     mov bl, [esi+TZ80Registers.B]
     mov ax, Word [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Byte [edi+eax], bl
     or  Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx,7
  end;
End;

Procedure Op71;
Begin // LD (HL),C
  asm
     mov bl, [esi+TZ80Registers.C]
     mov ax, Word [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Byte [edi+eax], bl
     or  Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx,7
  end;
End;

Procedure Op72;
Begin // LD (HL),D
  asm
     mov bl, [esi+TZ80Registers.D]
     mov ax, Word [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Byte [edi+eax], bl
     or  Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx,7
  end;
End;

Procedure Op73;
Begin // LD (HL),E
  asm
     mov bl, [esi+TZ80Registers.E]
     mov ax, Word [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Byte [edi+eax], bl
     or  Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx,7
  end;
End;

Procedure Op74;
Begin // LD (HL),H
  asm
     mov bl, [esi+TZ80Registers.H]
     mov ax, Word [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Byte [edi+eax], bl
     or  Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx,7
  end;
End;

Procedure Op75;
Begin // LD (HL),L
  asm
     mov bl, [esi+TZ80Registers.L]
     mov ax, Word [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Byte [edi+eax], bl
     or  Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx,7
  end;
End;

Procedure Op76;
Begin // HALT
  asm
     cmp ProfilingEnabled, True
     jne @SkipProfile

     cmp bx, $1303
     jne @SkipProfile

     pushad
     call AddProfileEntry
     popad

  @SkipProfile:
     mov Byte [esi+TZ80Registers.HaltEmu], True
     mov edx,8
  end;

  If Not Registers.IntsEnabled Then Begin

     StepOperation := True;

  End;

End;

Procedure Op77;
Begin // LD (HL),A
  asm
     mov bl, [esi+TZ80Registers.A]
     mov ax, Word [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Byte [edi+eax], bl
     or  Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx,7
  end;
End;

Procedure Op78;
Begin // LD A,B
  asm
     mov al, [esi+TZ80Registers.B]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,4
  end;
End;

Procedure Op79;
Begin // LD A,C
  asm
     mov al, [esi+TZ80Registers.C]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,4
  end;
End;

Procedure Op7A;
Begin // LD A,D
  asm
     mov al, [esi+TZ80Registers.D]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,4
  end;
End;

Procedure Op7B;
Begin // LD A,E
  asm
     mov al, [esi+TZ80Registers.E]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,4
  end;
End;

Procedure Op7C;
Begin // LD A,H
  asm
     mov al, [esi+TZ80Registers.H]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,4
  end;
End;

Procedure Op7D;
Begin // LD A,L
  asm
     mov al, [esi+TZ80Registers.L]
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,4
  end;
End;

Procedure Op7E;
Begin // LD A,(HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov bl, Byte [edi+eax]
     or  Byte [MemAccess[0]+eax], MemRead
     Inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], bl
     mov edx,7
  end;
End;

Procedure Op7F;
Begin // LD A,A
  asm
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure Op80;
Begin // ADD A,B
  asm
     mov  al,[esi+TZ80Registers.B]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op81;
Begin // ADD A,C
  asm
     mov  al,[esi+TZ80Registers.C]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op82;
Begin // ADD A,D
  asm
     mov  al,[esi+TZ80Registers.D]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op83;
Begin // ADD A,E
  asm
     mov  al,[esi+TZ80Registers.E]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op84;
Begin // ADD A,H
  asm
     mov  al,[esi+TZ80Registers.H]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op85;
Begin // ADD A,L
  asm
     mov  al,[esi+TZ80Registers.L]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op86;
Begin // ADD A,(HL)
  asm
     mov  ax, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [edi+eax]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure Op87;
Begin // ADD A,A
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,al
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op88;
Begin // ADC A,B
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,[esi+TZ80Registers.B]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op89;
Begin // ADC A,C
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,[esi+TZ80Registers.C]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op8A;
Begin // ADC A,D
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,[esi+TZ80Registers.D]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op8B;
Begin // ADC A,E
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,[esi+TZ80Registers.E]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op8C;
Begin // ADC A,H
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,[esi+TZ80Registers.H]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op8D;
Begin // ADC A,L
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,[esi+TZ80Registers.L]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op8E;
Begin // ADC A,(HL)
  asm
     mov  bx, Word [esi+TZ80Registers.L]
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al, Byte [edi+ebx]
     or   Byte [MemAccess[0]+ebx], MemRead
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure Op8F;
Begin // ADC A,A
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[esi+TZ80Registers.A]
     sahf
     adc  al,al
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op90;
Begin // SUB B
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,[esi+TZ80Registers.B]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op91;
Begin // SUB C
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,[esi+TZ80Registers.C]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op92;
Begin // SUB D
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,[esi+TZ80Registers.D]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op93;
Begin // SUB E
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,[esi+TZ80Registers.E]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op94;
Begin // SUB H
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,[esi+TZ80Registers.H]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op95;
Begin // SUB L
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,[esi+TZ80Registers.L]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op96;
Begin // SUB (HL)
  asm
     xor  ebx,ebx
     mov  al,[esi+TZ80Registers.A]
     mov  bx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ebx], MemRead
     sub  al, byte [edi+ebx]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure Op97;
Begin // SUB A
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,al
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op98;
Begin // SBC B
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,[esi+TZ80Registers.B]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op99;
Begin // SBC C
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,[esi+TZ80Registers.C]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op9A;
Begin // SBC D
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,[esi+TZ80Registers.D]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op9B;
Begin // SBC E
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,[esi+TZ80Registers.E]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op9C;
Begin // SBC H
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,[esi+TZ80Registers.H]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op9D;
Begin // SBC L
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,[esi+TZ80Registers.L]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure Op9E;
Begin // SBC (HL)
  asm
     mov  ax, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+eax], MemRead
     mov  bl, Byte [edi+eax]
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,bl
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure Op9F;
Begin // SBC A
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,[esi+TZ80Registers.A]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpA0;
Begin // AND B
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,[esi+TZ80Registers.B]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA1;
Begin // AND C
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,[esi+TZ80Registers.C]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA2;
Begin // AND D
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,[esi+TZ80Registers.D]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA3;
Begin // AND E
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,[esi+TZ80Registers.E]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA4;
Begin // AND H
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,[esi+TZ80Registers.H]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA5;
Begin // AND L
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,[esi+TZ80Registers.L]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA6;
Begin // AND (HL)
  asm
     mov  ax, Word [esi+TZ80Registers.L]
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+eax], MemRead
     and  dl,Byte [edi+eax]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  End;
End;

Procedure OpA7;
Begin // AND A
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,dl
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA8;
Begin // XOR B
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,[esi+TZ80Registers.B]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpA9;
Begin // XOR C
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,[esi+TZ80Registers.C]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpAA;
Begin // XOR D
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,[esi+TZ80Registers.D]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpAB;
Begin // XOR E
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,[esi+TZ80Registers.E]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpAC;
Begin // XOR H
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,[esi+TZ80Registers.H]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpAD;
Begin // XOR L
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,[esi+TZ80Registers.L]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpAE;
Begin // XOR (HL)
  asm
     mov  dl,[esi+TZ80Registers.A]
     mov  ax,Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+eax], MemRead
     xor  dl, Byte [edi+eax]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure OpAF;
Begin // XOR A
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,dl
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB0;
Begin // OR B
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,[esi+TZ80Registers.B]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB1;
Begin // OR C
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,[esi+TZ80Registers.C]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB2;
Begin // OR D
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,[esi+TZ80Registers.D]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB3;
Begin // OR E
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,[esi+TZ80Registers.E]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB4;
Begin // OR H
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,[esi+TZ80Registers.H]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB5;
Begin // OR L
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,[esi+TZ80Registers.L]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB6;
Begin // OR (HL)
  asm
     mov  ax, Word [esi+TZ80Registers.L]
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+eax], MemRead
     or   dl,byte [edi+eax]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  End;
End;

Procedure OpB7;
Begin // OR A
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,dl
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  End;
End;

Procedure OpB8;
Begin // CP B
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,[esi+TZ80Registers.B]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpB9;
Begin // CP C
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,[esi+TZ80Registers.C]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpBA;
Begin // CP D
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,[esi+TZ80Registers.D]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpBB;
Begin // CP E
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,[esi+TZ80Registers.E]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpBC;
Begin // CP H
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,[esi+TZ80Registers.H]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpBD;
Begin // CP L
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,[esi+TZ80Registers.L]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpBE;
Begin // CP (HL)
  asm
     mov  ax, Word [esi+TZ80Registers.L]
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+eax], MemRead
     cmp  dl, Byte [edi+eax]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,7
  end;
End;

Procedure OpBF;
Begin // CP A
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,dl
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,4
  end;
End;

Procedure OpC0;
Begin // RET NZ
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 64
     jnz @C0NonZero

     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov ax, Word [edi+eax]
     Add Word [esi+TZ80Registers.&SP], 2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @C0NonZero:
     Inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpC1;
Begin // POP BC
  asm
     mov ax, [esi+TZ80Registers.&SP]
     Add Word [esi+TZ80Registers.&SP], 2
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov ax, Word [edi+eax]
     mov Word [esi+TZ80Registers.C], ax
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 10
  end;
End;

Procedure OpC2;
Begin // JP NZ,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 64
     jnz @C2Zero

     Mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
     ret

  @C2Zero:
     Add Word [esi+TZ80Registers.PC], 3
     mov edx,10
  End;
End;

Procedure OpC3;
Begin // JP $+3
  asm
     mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
  end;
End;

Procedure OpC4;
Begin // CALL NZ,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 64
     jnz @C4Zero

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [edi+eax], bx

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @C4Zero:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpC5;
Begin // PUSH BC
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov cx, Word [esi+TZ80Registers.C]
     Sub ax, 2
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     Mov Word [edi+eax], cx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx, 11
  end;
End;

Procedure OpC6;
Begin // ADD A,N
  asm
     mov  al,dl
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC], 2
     mov  edx,7
  end;
End;

Procedure OpC7;
Begin // RST 0
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [esi+TZ80Registers.PC]
     sub ax, 2
     inc bx
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.PC], 0
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,11
  end;
End;

Procedure OpC8;
Begin // RET Z
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 64
     jz  @C8Zero

     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @C8Zero:
     Inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpC9;
Begin // RET
  asm
     cmp Registers.PC, $0052
     jne @NotInt
     mov InInterrupt, False
  @NotInt:
     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpCA;
Begin // JP Z,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 64
     jz  @C2Zero

     Mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
     ret

  @C2Zero:
     Add Word [esi+TZ80Registers.PC], 3
     mov edx,10
  End;
End;

Procedure OpCB;
Begin // Handle CB Prefix
  asm
     and  edx, $FF                        // Trim EDX down to get the opcode
     Add  Word [esi+TZ80Registers.PC], 2  // Advance PC
     call DWord [CBOps+edx*4]             // And call the new opcode handler
  End;
End;

Procedure OpCB00;
Begin // RLC B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B],al
     mov edx,8
  end;
End;

Procedure OpCB01;
Begin // RLC C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov edx,8
  end;
End;

Procedure OpCB02;
Begin // RLC D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov edx,8
  end;
End;

Procedure OpCB03;
Begin // RLC E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov edx,8
  end;
End;

Procedure OpCB04;
Begin // RLC H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov edx,8
  end;
End;

Procedure OpCB05;
Begin // RLC L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov edx,8
  end;
End;

Procedure OpCB06;
Begin // RLC (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     or   Byte [MemAccess[0]+ecx], MemWrite
     mov  Byte [edi+ecx],al
  @NoWrite:
     mov edx,15
  End;
End;

Procedure OpCB07;
Begin // RLC A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov edx,8
  end;
End;

Procedure OpCB08;
Begin // RRC B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     mov edx,8
  end;
End;

Procedure OpCB09;
Begin // RRC C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov edx,8
  end;
End;

Procedure OpCB0A;
Begin // RRC D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov edx,8
  end;
End;

Procedure OpCB0B;
Begin // RRC E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov edx,8
  end;
End;

Procedure OpCB0C;
Begin // RRC H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov edx,8
  end;
End;

Procedure OpCB0D;
Begin // RRC L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov edx,8
  end;
End;

Procedure OpCB0E;
Begin // RRC (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     or   Byte [MemAccess[0]+ecx], MemWrite
     mov  Byte [edi+ecx],al
  @NoWrite:
     mov edx,15
  End;
End;

Procedure OpCB0F;
Begin // RRC A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov edx,8
  end;
End;

Procedure OpCB10;
Begin // RL  B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     mov edx,8
  end;
End;

Procedure OpCB11;
Begin // RL  C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov edx,8
  end;
End;

Procedure OpCB12;
Begin // RL  D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov edx,8
  end;
End;

Procedure OpCB13;
Begin // RL  E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov edx,8
  end;
End;

Procedure OpCB14;
Begin // RL  H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov edx,8
  end;
End;

Procedure OpCB15;
Begin // RL  L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov edx,8
  end;
End;

Procedure OpCB16;
Begin // RL  (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or   Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx,15
  End;
End;

Procedure OpCB17;
Begin // RL  A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov edx,8
  end;
End;

Procedure OpCB18;
Begin // RR  B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     mov edx,8
  end;
End;

Procedure OpCB19;
Begin // RR  C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov edx,8
  end;
End;

Procedure OpCB1A;
Begin // RR  D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov edx,8
  end;
End;

Procedure OpCB1B;
Begin // RR  E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov edx,8
  end;
End;

Procedure OpCB1C;
Begin // RR  H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov edx,8
  end;
End;

Procedure OpCB1D;
Begin // RR  L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov edx,8
  end;
End;

Procedure OpCB1E;
Begin // RR  (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or   Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx,15
  end;
End;

Procedure OpCB1F;
Begin // RR  A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov edx,8
  end;
End;

Procedure OpCB20;
Begin // SLA B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,8
  end;
End;

Procedure OpCB21;
Begin // SLA C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,8
  end;
End;

Procedure OpCB22;
Begin // SLA D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,8
  end;
End;

Procedure OpCB23;
Begin // SLA E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,8
  end;
End;

Procedure OpCB24;
Begin // SLA H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,8
  end;
End;

Procedure OpCB25;
Begin // SLA L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,8
  end;
End;

Procedure OpCB26;
Begin // SLA (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or   Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov  edx,15
  End;
End;

Procedure OpCB27;
Begin // SLA A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,8
  end;
End;

Procedure OpCB28;
Begin // SRA B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,8
  end;
End;

Procedure OpCB29;
Begin // SRA C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,8
  end;
End;

Procedure OpCB2A;
Begin // SRA D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,8
  end;
End;

Procedure OpCB2B;
Begin // SRA E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,8
  end;
End;

Procedure OpCB2C;
Begin // SRA H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,8
  end;
End;

Procedure OpCB2D;
Begin // SRA L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,8
  end;
End;

Procedure OpCB2E;
Begin // SRA (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or   Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov  edx,15
  End;
End;

Procedure OpCB2F;
Begin // SRA A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,8
  end;
End;

Procedure OpCB30;
Begin // SLL B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,8
  end;
End;

Procedure OpCB31;
Begin // SLL C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,8
  end;
End;

Procedure OpCB32;
Begin // SLL D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,8
  end;
End;

Procedure OpCB33;
Begin // SLL E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,8
  end;
End;

Procedure OpCB34;
Begin // SLL H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,8
  end;
End;

Procedure OpCB35;
Begin // SLL L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,8
  end;
End;

Procedure OpCB36;
Begin // SLL (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or   Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov  edx,15
  End;
End;

Procedure OpCB37;
Begin // SLL A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,8
  end;
End;

Procedure OpCB38;
Begin // SRL B
  asm
     mov  al, [esi+TZ80Registers.B]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,8
  end;
End;

Procedure OpCB39;
Begin // SRL C
  asm
     mov  al, [esi+TZ80Registers.C]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,8
  end;
End;

Procedure OpCB3A;
Begin // SRL D
  asm
     mov  al, [esi+TZ80Registers.D]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,8
  end;
End;

Procedure OpCB3B;
Begin // SRL E
  asm
     mov  al, [esi+TZ80Registers.E]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,8
  end;
End;

Procedure OpCB3C;
Begin // SRL H
  asm
     mov  al, [esi+TZ80Registers.H]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,8
  end;
End;

Procedure OpCB3D;
Begin // SRL L
  asm
     mov  al, [esi+TZ80Registers.L]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,8
  end;
End;

Procedure OpCB3E;
Begin // SRL (HL)
  asm
     mov  cx, Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+ecx], MemRead
     mov  al, Byte [edi+ecx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or   Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov  edx,15
  End;
End;

Procedure OpCB3F;
Begin // SRL A
  asm
     mov  al, [esi+TZ80Registers.A]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,8
  end;
End;

Procedure OpCB40;
Begin // BIT 0,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,1   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB41;
Begin // BIT 0,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,1   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB42;
Begin // BIT 0,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,1   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB43;
Begin // BIT 0,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,1   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB44;
Begin // BIT 0,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,1   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB45;
Begin // BIT 0,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,1   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB46;
Begin // BIT 0,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah,[esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah,1  // clear all bits except carry
     and   bl,1   // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB47;
Begin // BIT 0,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,1   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB48;
Begin // BIT 1,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,2   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB49;
Begin // BIT 1,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,2   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB4A;
Begin // BIT 1,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,2   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB4B;
Begin // BIT 1,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,2   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB4C;
Begin // BIT 1,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,2   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB4D;
Begin // BIT 1,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,2   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB4E;
Begin // BIT 1,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah,[esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah,1  // clear all bits except carry
     and   bl,2   // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB4F;
Begin // BIT 1,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,2   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB50;
Begin // BIT 2,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,4   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB51;
Begin // BIT 2,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,4   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB52;
Begin // BIT 2,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,4   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB53;
Begin // BIT 2,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,4   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB54;
Begin // BIT 2,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,4   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB55;
Begin // BIT 2,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,4   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB56;
Begin // BIT 2,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah,[esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah,1  // clear all bits except carry
     and   bl,4  // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB57;
Begin // BIT 2,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,4   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB58;
Begin // BIT 3,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,8   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB59;
Begin // BIT 3,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,8   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB5A;
Begin // BIT 3,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,8   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB5B;
Begin // BIT 3,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,8   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB5C;
Begin // BIT 3,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,8   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB5D;
Begin // BIT 3,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,8   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB5E;
Begin // BIT 3,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah, [esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah, 1   // clear all bits except carry
     and   bl, 8   // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB5F;
Begin // BIT 3,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,8   // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB60;
Begin // BIT 4,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,16  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB61;
Begin // BIT 4,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,16  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB62;
Begin // BIT 4,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,16  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB63;
Begin // BIT 4,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,16  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB64;
Begin // BIT 4,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,16  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB65;
Begin // BIT 4,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,16  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB66;
Begin // BIT 4,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah, [esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah,1  // clear all bits except carry
     and   bl,16  // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB67;
Begin // BIT 4,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,16  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB68;
Begin // BIT 5,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,32  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB69;
Begin // BIT 5,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,32  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB6A;
Begin // BIT 5,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,32  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB6B;
Begin // BIT 5,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,32  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB6C;
Begin // BIT 5,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,32  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB6D;
Begin // BIT 5,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,32  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB6E;
Begin // BIT 5,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah, [esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah,1   // clear all bits except carry
     and   bl,32  // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB6F;
Begin // BIT 5,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,32  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB70;
Begin // BIT 6,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,64  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB71;
Begin // BIT 6,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,64  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB72;
Begin // BIT 6,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,64  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB73;
Begin // BIT 6,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,64  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB74;
Begin // BIT 6,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,64  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB75;
Begin // BIT 6,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,64  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB76;
Begin // BIT 6,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah, [esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah,1  // clear all bits except carry
     and   bl,64  // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB77;
Begin // BIT 6,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,64  // Bit to test
     jne  @BitFwd

     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB78;
Begin // BIT 7,B
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.B]
     and  al,1   // clear all bits except carry
     and  bl,128 // Bit to test
     je   @BitClear

     or   al,128
     jmp  @BitFwd

@BitClear:
     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB79;
Begin // BIT 7,C
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.C]
     and  al,1   // clear all bits except carry
     and  bl,128 // Bit to test
     je   @BitClear

     or   al,128
     jmp  @BitFwd

@BitClear:
     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB7A;
Begin // BIT 7,D
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.D]
     and  al,1   // clear all bits except carry
     and  bl,128 // Bit to test
     je   @BitClear

     or   al,128
     jmp  @BitFwd

@BitClear:
     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB7B;
Begin // BIT 7,E
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.E]
     and  al,1   // clear all bits except carry
     and  bl,128 // Bit to test
     je   @BitClear

     or   al,128
     jmp  @BitFwd

@BitClear:
     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB7C;
Begin // BIT 7,H
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.H]
     and  al,1   // clear all bits except carry
     and  bl,128 // Bit to test
     je   @BitClear

     or   al,128
     jmp  @BitFwd

@BitClear:
     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB7D;
Begin // BIT 7,L
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.L]
     and  al,1   // clear all bits except carry
     and  bl,128 // Bit to test
     je   @BitClear

     or   al,128
     jmp  @BitFwd

@BitClear:
     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB7E;
Begin // BIT 7,(HL)
  asm
     mov   bx, Word [esi+TZ80Registers.L]
     mov   ah,[esi+TZ80Registers.F]
     or    Byte [MemAccess[0]+ebx], MemRead
     mov   bl, Byte [edi+ebx]
     and   ah,1   // clear all bits except carry
     and   bl,128 // the Bit
     je    @BitClear

     or    ah,128
     jmp   @BitFwd

@BitClear:
     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,12
  End;
End;

Procedure OpCB7F;
Begin // BIT 7,A
  asm
     mov  al,[esi+TZ80Registers.F]
     mov  bl,[esi+TZ80Registers.A]
     and  al,1   // clear all bits except carry
     and  bl,128 // Bit to test
     je   @BitClear

     or   al,128
     jmp  @BitFwd

@BitClear:
     or   al,68  // set Z + V
@BitFwd:
     or   al,16  // set H
     mov  Byte [esi+TZ80Registers.F],al
     mov  edx,8
  end;
End;

Procedure OpCB80;
Begin // RES 0,B
  asm
     and Byte [esi+TZ80Registers.B], 254
     mov edx, 8
  end;
End;

Procedure OpCB81;
Begin // RES 0,C
  asm
     and Byte [esi+TZ80Registers.C], 254
     mov edx, 8
  end;
End;

Procedure OpCB82;
Begin // RES 0,D
  asm
     and Byte [esi+TZ80Registers.D], 254
     mov edx, 8
  end;
End;

Procedure OpCB83;
Begin // RES 0,E
  asm
     and Byte [esi+TZ80Registers.E], 254
     mov edx, 8
  end;
End;

Procedure OpCB84;
Begin // RES 0,H
  asm
     and Byte [esi+TZ80Registers.H], 254
     mov edx, 8
  end;
End;

Procedure OpCB85;
Begin // RES 0,L
  asm
     and Byte [esi+TZ80Registers.L], 254
     mov edx, 8
  end;
End;

Procedure OpCB86;
Begin // RES 0,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl, 254
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp   ecx, $4000
     jb    @NoWrite
     mov   Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCB87;
Begin // RES 0,A
  asm
     and Byte [esi+TZ80Registers.A], 254
     mov edx, 8
  end;
End;

Procedure OpCB88;
Begin // RES 1,B
  asm
     and Byte [esi+TZ80Registers.B], 253
     mov edx, 8
  end;
End;

Procedure OpCB89;
Begin // RES 1,C
  asm
     and Byte [esi+TZ80Registers.C], 253
     mov edx, 8
  end;
End;

Procedure OpCB8A;
Begin // RES 1,D
  asm
     and Byte [esi+TZ80Registers.D], 253
     mov edx, 8
  end;
End;

Procedure OpCB8B;
Begin // RES 1,E
  asm
     and Byte [esi+TZ80Registers.E], 253
     mov edx, 8
  end;
End;

Procedure OpCB8C;
Begin // RES 1,H
  asm
     and Byte [esi+TZ80Registers.H], 253
     mov edx, 8
  end;
End;

Procedure OpCB8D;
Begin // RES 1,L
  asm
     and Byte [esi+TZ80Registers.L], 253
     mov edx, 8
  end;
End;

Procedure OpCB8E;
Begin // RES 1,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl, 253
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp   ecx, $4000
     jb    @NoWrite
     mov   Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCB8F;
Begin // RES 1,A
  asm
     and Byte [esi+TZ80Registers.A], 253
     mov edx, 8
  end;
End;

Procedure OpCB90;
Begin // RES 2,B
  asm
     and Byte [esi+TZ80Registers.B], 251
     mov edx, 8
  end;
End;

Procedure OpCB91;
Begin // RES 2,C
  asm
     and Byte [esi+TZ80Registers.C], 251
     mov edx, 8
  end;
End;

Procedure OpCB92;
Begin // RES 2,D
  asm
     and Byte [esi+TZ80Registers.D], 251
     mov edx, 8
  end;
End;

Procedure OpCB93;
Begin // RES 2,E
  asm
     and Byte [esi+TZ80Registers.E], 251
     mov edx, 8
  end;
End;

Procedure OpCB94;
Begin // RES 2,H
  asm
     and Byte [esi+TZ80Registers.H], 251
     mov edx, 8
  end;
End;

Procedure OpCB95;
Begin // RES 2,L
  asm
     and Byte [esi+TZ80Registers.L], 251
     mov edx, 8
  end;
End;

Procedure OpCB96;
Begin // RES 2,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl, 251
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp   ecx, $4000
     jb    @NoWrite
     mov   Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov   edx, 15
  End;
End;

Procedure OpCB97;
Begin // RES 2,A
  asm
     and Byte [esi+TZ80Registers.A], 251
     mov edx, 8
  end;
End;

Procedure OpCB98;
Begin // RES 3,B
  asm
     and Byte [esi+TZ80Registers.B], 247
     mov edx, 8
  end;
End;

Procedure OpCB99;
Begin // RES 3,C
  asm
     and Byte [esi+TZ80Registers.C], 247
     mov edx, 8
  end;
End;

Procedure OpCB9A;
Begin // RES 3,D
  asm
     and Byte [esi+TZ80Registers.D], 247
     mov edx, 8
  end;
End;

Procedure OpCB9B;
Begin // RES 3,E
  asm
     and Byte [esi+TZ80Registers.E], 247
     mov edx, 8
  end;
End;

Procedure OpCB9C;
Begin // RES 3,H
  asm
     and Byte [esi+TZ80Registers.H], 247
     mov edx, 8
  end;
End;

Procedure OpCB9D;
Begin // RES 3,L
  asm
     and Byte [esi+TZ80Registers.L], 247
     mov edx, 8
  end;
End;

Procedure OpCB9E;
Begin // RES 3,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl, 247
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCB9F;
Begin // RES 3,A
  asm
     and Byte [esi+TZ80Registers.A], 247
     mov edx, 8
  end;
End;

Procedure OpCBA0;
Begin // RES 4,B
  asm
     and Byte [esi+TZ80Registers.B], 239
     mov edx, 8
  end;
End;

Procedure OpCBA1;
Begin // RES 4,C
  asm
     and Byte [esi+TZ80Registers.C], 239
     mov edx, 8
  end;
End;

Procedure OpCBA2;
Begin // RES 4,D
  asm
     and Byte [esi+TZ80Registers.D], 239
     mov edx, 8
  end;
End;

Procedure OpCBA3;
Begin // RES 4,E
  asm
     and Byte [esi+TZ80Registers.E], 239
     mov edx, 8
  end;
End;

Procedure OpCBA4;
Begin // RES 4,H
  asm
     and Byte [esi+TZ80Registers.H], 239
     mov edx, 8
  end;
End;

Procedure OpCBA5;
Begin // RES 4,L
  asm
     and Byte [esi+TZ80Registers.L], 239
     mov edx, 8
  end;
End;

Procedure OpCBA6;
Begin // RES 4,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl, 239
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBA7;
Begin // RES 4,A
  asm
     and Byte [esi+TZ80Registers.A], 239
     mov edx, 8
  end;
End;

Procedure OpCBA8;
Begin // RES 5,B
  asm
     and Byte [esi+TZ80Registers.B], 223
     mov edx, 8
  end;
End;

Procedure OpCBA9;
Begin // RES 5,C
  asm
     and Byte [esi+TZ80Registers.C], 223
     mov edx, 8
  end;
End;

Procedure OpCBAA;
Begin // RES 5,D
  asm
     and Byte [esi+TZ80Registers.D], 223
     mov edx, 8
  end;
End;

Procedure OpCBAB;
Begin // RES 5,E
  asm
     and Byte [esi+TZ80Registers.E], 223
     mov edx, 8
  end;
End;

Procedure OpCBAC;
Begin // RES 5,H
  asm
     and Byte [esi+TZ80Registers.H], 223
     mov edx, 8
  end;
End;

Procedure OpCBAD;
Begin // RES 5,L
  asm
     and Byte [esi+TZ80Registers.L], 223
     mov edx, 8
  end;
End;

Procedure OpCBAE;
Begin // RES 5,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl, 223
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBAF;
Begin // RES 5,A
  asm
     and Byte [esi+TZ80Registers.A], 223
     mov edx, 8
  end;
End;

Procedure OpCBB0;
Begin // RES 6,B
  asm
     and Byte [esi+TZ80Registers.B], 191
     mov edx, 8
  end;
End;

Procedure OpCBB1;
Begin // RES 6,C
  asm
     and Byte [esi+TZ80Registers.C], 191
     mov edx, 8
  end;
End;

Procedure OpCBB2;
Begin // RES 6,D
  asm
     and Byte [esi+TZ80Registers.D], 191
     mov edx, 8
  end;
End;

Procedure OpCBB3;
Begin // RES 6,E
  asm
     and Byte [esi+TZ80Registers.E], 191
     mov edx, 8
  end;
End;

Procedure OpCBB4;
Begin // RES 6,H
  asm
     and Byte [esi+TZ80Registers.H], 191
     mov edx, 8
  end;
End;

Procedure OpCBB5;
Begin // RES 6,L
  asm
     and Byte [esi+TZ80Registers.L], 191
     mov edx, 8
  end;
End;

Procedure OpCBB6;
Begin // RES 6,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl, 191
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBB7;
Begin // RES 6,A
  asm
     and Byte [esi+TZ80Registers.A], 191
     mov edx, 8
  end;
End;

Procedure OpCBB8;
Begin // RES 7,B
  asm
     and Byte [esi+TZ80Registers.B], 127
     mov edx, 8
  end;
End;

Procedure OpCBB9;
Begin // RES 7,C
  asm
     and Byte [esi+TZ80Registers.C], 127
     mov edx, 8
  end;
End;

Procedure OpCBBA;
Begin // RES 7,D
  asm
     and Byte [esi+TZ80Registers.D], 127
     mov edx, 8
  end;
End;

Procedure OpCBBB;
Begin // RES 7,E
  asm
     and Byte [esi+TZ80Registers.E], 127
     mov edx, 8
  end;
End;

Procedure OpCBBC;
Begin // RES 7,H
  asm
     and Byte [esi+TZ80Registers.H], 127
     mov edx, 8
  end;
End;

Procedure OpCBBD;
Begin // RES 7,L
  asm
     and Byte [esi+TZ80Registers.L], 127
     mov edx, 8
  end;
End;

Procedure OpCBBE;
Begin // RES 7,(HL)
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   bl,127
     or    Byte [MemAccess[0]+ecx], MemRead
     mov   al, Byte [edi+ecx]
     and   eax,ebx
     cmp  ecx, $4000
     jb   @NoWrite
     mov  Byte [edi+ecx],al
     or    Byte [MemAccess[0]+ecx], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBBF;
Begin // RES 7,A
  asm
     and Byte [esi+TZ80Registers.A], 127
     mov edx, 8
  end;
End;

Procedure OpCBC0;
Begin // SET 0,B
  asm
     or  Byte [esi+TZ80Registers.B], 1
     mov edx, 8
  end;
End;

Procedure OpCBC1;
Begin // SET 0,C
  asm
     or  Byte [esi+TZ80Registers.C], 1
     mov edx, 8
  end;
End;

Procedure OpCBC2;
Begin // SET 0,D
  asm
     or  Byte [esi+TZ80Registers.D], 1
     mov edx, 8
  end;
End;

Procedure OpCBC3;
Begin // SET 0,E
  asm
     or  Byte [esi+TZ80Registers.E], 1
     mov edx, 8
  end;
End;

Procedure OpCBC4;
Begin // SET 0,H
  asm
     or  Byte [esi+TZ80Registers.H], 1
     mov edx, 8
  end;
End;

Procedure OpCBC5;
Begin // SET 0,L
  asm
     or  Byte [esi+TZ80Registers.L], 1
     mov edx, 8
  end;
End;

Procedure OpCBC6;
Begin // SET 0,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,1 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBC7;
Begin // SET 0,A
  asm
     or  Byte [esi+TZ80Registers.A], 1
     mov edx, 8
  end;
End;

Procedure OpCBC8;
Begin // SET 1,B
  asm
     or  Byte [esi+TZ80Registers.B], 2
     mov edx, 8
  end;
End;

Procedure OpCBC9;
Begin // SET 1,C
  asm
     or  Byte [esi+TZ80Registers.C], 2
     mov edx, 8
  end;
End;

Procedure OpCBCA;
Begin // SET 1,D
  asm
     or  Byte [esi+TZ80Registers.D], 2
     mov edx, 8
  end;
End;

Procedure OpCBCB;
Begin // SET 1,E
  asm
     or  Byte [esi+TZ80Registers.E], 2
     mov edx, 8
  end;
End;

Procedure OpCBCC;
Begin // SET 1,H
  asm
     or  Byte [esi+TZ80Registers.H], 2
     mov edx, 8
  end;
End;

Procedure OpCBCD;
Begin // SET 1,L
  asm
     or  Byte [esi+TZ80Registers.L], 2
     mov edx, 8
  end;
End;

Procedure OpCBCE;
Begin // SET 1,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,2 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBCF;
Begin // SET 1,A
  asm
     or  Byte [esi+TZ80Registers.A], 2
     mov edx, 8
  end;
End;

Procedure OpCBD0;
Begin // SET 2,B
  asm
     or  Byte [esi+TZ80Registers.B], 4
     mov edx, 8
  end;
End;

Procedure OpCBD1;
Begin // SET 2,C
  asm
     or  Byte [esi+TZ80Registers.C], 4
     mov edx, 8
  end;
End;

Procedure OpCBD2;
Begin // SET 2,D
  asm
     or  Byte [esi+TZ80Registers.D], 4
     mov edx, 8
  end;
End;

Procedure OpCBD3;
Begin // SET 2,E
  asm
     or  Byte [esi+TZ80Registers.E], 4
     mov edx, 8
  end;
End;

Procedure OpCBD4;
Begin // SET 2,H
  asm
     or  Byte [esi+TZ80Registers.H], 4
     mov edx, 8
  end;
End;

Procedure OpCBD5;
Begin // SET 2,L
  asm
     or  Byte [esi+TZ80Registers.L], 4
     mov edx, 8
  end;
End;

Procedure OpCBD6;
Begin // SET 2,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,4 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 8
  End;
End;

Procedure OpCBD7;
Begin // SET 2,A
  asm
     or  Byte [esi+TZ80Registers.A], 4
     mov edx, 8
  end;
End;

Procedure OpCBD8;
Begin // SET 3,B
  asm
     or  Byte [esi+TZ80Registers.B], 8
     mov edx, 8
  end;
End;

Procedure OpCBD9;
Begin // SET 3,C
  asm
     or  Byte [esi+TZ80Registers.C], 8
     mov edx, 8
  end;
End;

Procedure OpCBDA;
Begin // SET 3,D
  asm
     or  Byte [esi+TZ80Registers.D], 8
     mov edx, 8
  end;
End;

Procedure OpCBDB;
Begin // SET 3,E
  asm
     or  Byte [esi+TZ80Registers.E], 8
     mov edx, 8
  end;
End;

Procedure OpCBDC;
Begin // SET 3,H
  asm
     or  Byte [esi+TZ80Registers.H], 8
     mov edx, 8
  end;
End;

Procedure OpCBDD;
Begin // SET 3,L
  asm
     or  Byte [esi+TZ80Registers.L], 8
     mov edx, 8
  end;
End;

Procedure OpCBDE;
Begin // SET 3,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,8 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBDF;
Begin // SET 3,A
  asm
     or  Byte [esi+TZ80Registers.A], 8
     mov edx, 8
  end;
End;

Procedure OpCBE0;
Begin // SET 4,B
  asm
     or  Byte [esi+TZ80Registers.B], 16
     mov edx, 8
  end;
End;

Procedure OpCBE1;
Begin // SET 4,C
  asm
     or  Byte [esi+TZ80Registers.C], 16
     mov edx, 8
  end;
End;

Procedure OpCBE2;
Begin // SET 4,D
  asm
     or  Byte [esi+TZ80Registers.D], 16
     mov edx, 8
  end;
End;

Procedure OpCBE3;
Begin // SET 4,E
  asm
     or  Byte [esi+TZ80Registers.E], 16
     mov edx, 8
  end;
End;

Procedure OpCBE4;
Begin // SET 4,H
  asm
     or  Byte [esi+TZ80Registers.H], 16
     mov edx, 8
  end;
End;

Procedure OpCBE5;
Begin // SET 4,L
  asm
     or  Byte [esi+TZ80Registers.L], 16
     mov edx, 8
  end;
End;

Procedure OpCBE6;
Begin // SET 4,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,16 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBE7;
Begin // SET 4,A
  asm
     or  Byte [esi+TZ80Registers.A], 16
     mov edx, 8
  end;
End;

Procedure OpCBE8;
Begin // SET 5,B
  asm
     or  Byte [esi+TZ80Registers.B], 32
     mov edx, 8
  end;
End;

Procedure OpCBE9;
Begin // SET 5,C
  asm
     or  Byte [esi+TZ80Registers.C], 32
     mov edx, 8
  end;
End;

Procedure OpCBEA;
Begin // SET 5,D
  asm
     or  Byte [esi+TZ80Registers.D], 32
     mov edx, 8
  end;
End;

Procedure OpCBEB;
Begin // SET 5,E
  asm
     or  Byte [esi+TZ80Registers.E], 32
     mov edx, 8
  end;
End;

Procedure OpCBEC;
Begin // SET 5,H
  asm
     or  Byte [esi+TZ80Registers.H], 32
     mov edx, 8
  end;
End;

Procedure OpCBED;
Begin // SET 5,L
  asm
     or  Byte [esi+TZ80Registers.L], 32
     mov edx, 8
  end;
End;

Procedure OpCBEE;
Begin // SET 5,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,32 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBEF;
Begin // SET 5,A
  asm
     or  Byte [esi+TZ80Registers.A], 32
     mov edx, 8
  end;
End;

Procedure OpCBF0;
Begin // SET 6,B
  asm
     or  Byte [esi+TZ80Registers.B], 64
     mov edx, 8
  end;
End;

Procedure OpCBF1;
Begin // SET 6,C
  asm
     or  Byte [esi+TZ80Registers.C], 64
     mov edx, 8
  end;
End;

Procedure OpCBF2;
Begin // SET 6,D
  asm
     or  Byte [esi+TZ80Registers.D], 64
     mov edx, 8
  end;
End;

Procedure OpCBF3;
Begin // SET 6,E
  asm
     or  Byte [esi+TZ80Registers.E], 64
     mov edx, 8
  end;
End;

Procedure OpCBF4;
Begin // SET 6,H
  asm
     or  Byte [esi+TZ80Registers.H], 64
     mov edx, 8
  end;
End;

Procedure OpCBF5;
Begin // SET 6,L
  asm
     or  Byte [esi+TZ80Registers.L], 64
     mov edx, 8
  end;
End;

Procedure OpCBF6;
Begin // SET 6,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,64 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBF7;
Begin // SET 6,A
  asm
     or  Byte [esi+TZ80Registers.A], 64
     mov edx, 8
  end;
End;

Procedure OpCBF8;
Begin // SET 7,B
  asm
     or  Byte [esi+TZ80Registers.B], 128
     mov edx, 8
  end;
End;

Procedure OpCBF9;
Begin // SET 7,C
  asm
     or  Byte [esi+TZ80Registers.C], 128
     mov edx, 8
  end;
End;

Procedure OpCBFA;
Begin // SET 7,D
  asm
     or  Byte [esi+TZ80Registers.D], 128
     mov edx, 8
  end;
End;

Procedure OpCBFB;
Begin // SET 7,E
  asm
     or  Byte [esi+TZ80Registers.E], 128
     mov edx, 8
  end;
End;

Procedure OpCBFC;
Begin // SET 7,H
  asm
     or  Byte [esi+TZ80Registers.H], 128
     mov edx, 8
  end;
End;

Procedure OpCBFD;
Begin // SET 7,L
  asm
     or  Byte [esi+TZ80Registers.L], 128
     mov edx, 8
  end;
End;

Procedure OpCBFE;
Begin // SET 7,(HL)
  asm
     mov   ax, Word [esi+TZ80Registers.L]
     mov   bl,128 // Bit to SET
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     or    edx,ebx
     cmp   eax, $4000
     jb    @NoWrite
     mov   Byte [edi+eax],dl
     or    Byte [MemAccess[0]+eax], MemWrite
  @NoWrite:
     mov edx, 15
  End;
End;

Procedure OpCBFF;
Begin // SET 7,A
  asm
     or  Byte [esi+TZ80Registers.A], 128
     mov edx, 8
  end;
End;

Procedure OpCC;
Begin // CALL Z,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 64
     jz  @CCNonZero

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @CCNonZero:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpCD;
Begin // CALL NN
  asm
     inc CALLCounter
     add bx, 3
     mov ax, [esi+TZ80Registers.&SP]
     sub ax, 2
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.&SP], ax
     mov Word [esi+TZ80Registers.PC], dx
     mov edx,17
  end;
End;

Procedure OpCE;
Begin // ADC A,N
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[esi+TZ80Registers.A]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC], 2
     mov  edx,7
  end;
End;

Procedure OpCF;
Begin // RST 8H
  asm
     mov ax, Word [esi+TZ80Registers.PC]
     mov bx, [esi+TZ80Registers.&SP]
     inc ax
     sub bx, 2
     mov Word [esi+TZ80Registers.PC], 8
     mov Word [edi+ebx], ax
     or  Byte [MemAccess[0]+ebx], MemWrite
     or  Byte [MemAccess[0]+ebx+1], MemWrite
     mov Word [esi+TZ80Registers.&SP], bx
     mov edx, 11
  end;
End;

Procedure OpD0;
Begin // RET NC
  asm
     mov cl, [esi+TZ80Registers.F]
     and cl, 1
     jnz @D0Carry

     Mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [edi+eax]
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

     add Word [esi+TZ80Registers.&SP], 2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @D0Carry:
     inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpD1;
Begin // POP DE
  asm
     mov ax, [esi+TZ80Registers.&SP]
     Add Word [esi+TZ80Registers.&SP], 2
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov ax, Word [edi+eax]
     mov Word [esi+TZ80Registers.E], ax
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 10
  end;
End;

Procedure OpD2;
Begin // JP NC,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 1
     jnz @D2Carry

     mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
     ret

  @D2Carry:
     Add Word [esi+TZ80Registers.PC], 3
     mov edx,10
  end;
End;

Procedure OpD3;
Begin // OUT (N),A
  asm
     mov   al, dl
     mov   ah,[esi+TZ80Registers.A]
     mov   dl,ah
     call  SetPortByte
     add   Word [esi+TZ80Registers.PC],2
     mov   edx, 11
  end;
End;

Procedure OpD4;
Begin // CALL NC,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 1
     jnz @D4CarrySet

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @D4CarrySet:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpD5;
Begin // PUSH DE
  asm
     mov ax, [esi+TZ80Registers.&SP]    // ax=sp
     mov cx, Word [esi+TZ80Registers.E] // cx=de
     Sub ax, 2                          // dec sp
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000                     // in rom?
     jb  @NoWrite
     Mov Word [edi+eax], cx             // write to mem
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax    // update sp
     mov edx, 11
  end;
End;

Procedure OpD6;
Begin // SUB N
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,dl
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,7
  end;
End;

Procedure OpD7;
Begin // RST 10H
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [esi+TZ80Registers.PC]
     sub ax, 2
     inc bx
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.PC], 16
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,11
  end;
End;

Procedure OpD8;
Begin // RET C
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 1
     jz  @D8NoCarry

     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @D8NoCarry:
     Inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpD9;
Begin // EXX
  asm
     mov cx, Word [esi+TZ80Registers.L]
     mov dx, Word [esi+TZ80Registers.Ln]
     mov Word [esi+TZ80Registers.L],  dx
     mov Word [esi+TZ80Registers.Ln], cx
     mov cx, Word [esi+TZ80Registers.E]
     mov dx, Word [esi+TZ80Registers.En]
     mov Word [esi+TZ80Registers.E],  dx
     mov Word [esi+TZ80Registers.En], cx
     mov cx, Word [esi+TZ80Registers.C]
     mov dx, Word [esi+TZ80Registers.Cn]
     mov Word [esi+TZ80Registers.C],  dx
     mov Word [esi+TZ80Registers.Cn], cx
     Inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure OpDA;
Begin // JP C,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 1
     jz  @DANoCarry

     Mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
     ret

  @DANoCarry:
     Add Word [esi+TZ80Registers.PC], 3
     mov edx,10
  End;
End;

Procedure OpDB;
Begin // IN A,(N)
  asm
     mov ah, [esi+TZ80Registers.A]
     add Word [esi+TZ80Registers.PC], 2
     mov al, dl
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.A], al
     mov edx,11
  end;
End;

Procedure OpDC;
Begin // CALL C,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 1
     jz  @DCNoCarry

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @DCNoCarry:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpDD;
Begin // Handle DD Prefix
  asm
     xor  eax, eax
     xor  ebx, ebx
     lea  ecx, [esi+TZ80Registers.IX] // DD Ops use IX a *lot*, so point ECX at it.
     mov  al, dl                      // EDX has been set up previously, so dump the opcode into EAX
     mov  bx, Word [esi+TZ80Registers.IX]
     Inc  Word [esi+TZ80Registers.PC]
     Call DWord [DDOps+eax*4]         // And Jump to the new Opcode
  End;
End;

Procedure OpDD09;
Begin // ADD IX,BC
  asm
     mov   ax,[ecx]
     mov   dx,Word [esi+TZ80Registers.C]
     add   dx,ax
     lahf
     mov   bl,[esi+TZ80Registers.F]
     and   ah,00010001b // H,C affected by addition. N is cleared
     and   bl,11000100b // S,Z,V are unaffected
     or    ah,bl
     mov   bl,dh
     and   bl,00101000b // 5,3 from H after addition
     or    ah,cl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   [ecx],dx
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,15
  end;
End;

Procedure OpDD19;
Begin // ADD IX,DE
  asm
     mov   ax,[ecx]
     mov   dx,Word [esi+TZ80Registers.E]
     add   dx,ax
     lahf
     mov   bl,[esi+TZ80Registers.F]
     and   ah,00010001b // H,C affected by addition. N is cleared
     and   bl,11000100b // S,Z,V are unaffected
     or    ah,bl
     mov   bl,dh
     and   bl,00101000b // 5,3 from H after addition
     or    ah,bl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   [ecx],dx
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,15
  end;
End;

Procedure OpDD21;
Begin // LD IX,nn
  asm
     shr edx,8
     mov [ecx], dx
     add Word [esi+TZ80Registers.PC], 3
     mov edx,14
  end;
End;

Procedure OpDD22;
Begin // LD (nn),IX
  asm
     shr   edx, 8
     and   edx, $FFFF
     mov   bx, Word [ecx]
     cmp   edx, $4000
     jb    @NoWrite
     mov   Word [edi+edx], bx
     or    Byte [MemAccess[0]+edx], MemWrite
     or    Byte [MemAccess[0]+edx+1], MemWrite

  @NoWrite:
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,20
  End;
End;

Procedure OpDD23;
Begin // INC IX
  asm
     inc Word [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov edx,10
  end;
End;

Procedure OpDD24;
Begin // INC IXH
  Asm
     lea  ecx,[ecx+1]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDD25;
Begin // DEC IXH
  Asm
     lea  ecx,[ecx+1]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDD26;
Begin // LD IXH,n
  asm
     mov Byte [ecx+1], dh
     add Word [esi+TZ80Registers.PC], 2
     mov edx,11
  end;
End;

Procedure OpDD29;
Begin // ADD IX,IX
  asm
     mov   dx,[ecx]
     add   dx,dx
     lahf
     mov   bl,[esi+TZ80Registers.F]
     and   ah,00010001b // H,C affected by addition. N is cleared
     and   bl,11000100b // S,Z,V are unaffected
     or    ah,bl
     mov   bl,dh
     and   bl,00101000b // 5,3 from H after addition
     or    ah,bl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   [ecx],dx
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,15
  end;
End;

Procedure OpDD2A;
Begin // LD IX,(nn)
  asm
     shr  edx, 8
     and  edx, $FFFF
     or   Byte [MemAccess[0]+edx], MemRead
     or   Byte [MemAccess[0]+edx+1], MemRead
     mov  ax, Word [edi+edx]
     Add  Word [esi+TZ80Registers.PC], 3
     mov  Word [ecx],ax
     mov edx,20
  end;
End;

Procedure OpDD2B;
Begin // DEC IX
  asm
     dec Word [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov edx,10
  end;
End;

Procedure OpDD2C;
Begin // INC IXL
  Asm
     lea  ecx,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     mov  [ecx],al
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDD2D;
Begin // DEC IXL
  Asm
     lea  ecx,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDD2E;
Begin // LD IXL,n
  asm
     Add Word [esi+TZ80Registers.PC], 2
     mov byte [ecx], dh
     mov edx,11
  end;
End;

Procedure OpDD34;
Begin // INC (IX+d)
  asm
     mov  bx, Word [ecx]
     mov  al, dh
     cbw
     add  bx, ax
     or   Byte [MemAccess[0]+ebx], MemRead
     lea  ecx, [edi+ebx]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     inc  al
     lahf
     cmp  ebx, $4000
     jb   @NoWrite
     mov  [ecx],al
     or   Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     seto ch
     mov  cl,al
     shl  ch,2
     and  ah,11010001b
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,23
  end;
End;

Procedure OpDD35;
Begin // DEC (IX+d)
  asm
     mov  bx, Word [ecx]
     mov  al, dh
     cbw
     add  bx, ax
     or   Byte [MemAccess[0]+ebx], MemRead
     lea  ecx, [edi+ebx]
     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     cmp  ebx, $4000
     jb   @NoWrite
     mov  [ecx],al
     or   Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC], 2
     mov  edx,23
  end;
End;

Procedure OpDD36;
Begin // LD (IX+d),n
  asm
     shr edx,8
     mov bx, Word [ecx]
     mov al, dl
     cbw
     add bx, ax
     cmp ebx, $4000
     jb  @NoWrite
     mov Byte [edi+ebx],dh
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     add Word [esi+TZ80Registers.PC], 3
     mov edx,19
  end;
End;

Procedure OpDD39;
Begin // ADD IX,SP
  asm
     mov   ax, [esi+TZ80Registers.&SP]
     mov   dx, [ecx]
     add   dx,ax
     lahf
     mov   bl,[esi+TZ80Registers.F]
     and   ah,00010001b // H,C affected by addition. N is cleared
     and   bl,11000100b // S,Z,V are unaffected
     or    ah,bl
     mov   bl,dh
     and   bl,00101000b // 5,3 from H after addition
     or    ah,bl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   [ecx],dx
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,15
  end;
End;

Procedure OpDD44;
Begin // LD B,IXH
  asm
     mov al, Byte [ecx+1]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,8
  end;
End;

Procedure OpDD45;
Begin // LD B,IXL
  asm
     mov al, Byte [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.B], al
     mov edx,8
  end;
End;

Procedure OpDD46;
Begin // LD B,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     or  Byte [MemAccess[0]+ebx], MemRead
     mov cl, byte [edi+ebx]
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.B], cl
     mov edx,19
  end;
End;

Procedure OpDD4C;
Begin // LD C,IXH
  asm
     mov al, Byte [ecx+1]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,8
  end;
End;

Procedure OpDD4D;
Begin // LD C,IXL
  asm
     mov al, Byte [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.C], al
     mov edx,8
  end;
End;

Procedure OpDD4E;
Begin // LD C,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     or  Byte [MemAccess[0]+ebx], MemRead
     mov cl, byte [edi+ebx]
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.C], cl
     mov edx,19
  end;
End;

Procedure OpDD54;
Begin // LD D,IXH
  asm
     mov al, Byte [ecx+1]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,8
  end;
End;

Procedure OpDD55;
Begin // LD D,IXL
  asm
     mov al, Byte [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.D], al
     mov edx,8
  end;
End;

Procedure OpDD56;
Begin // LD D,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     or  Byte [MemAccess[0]+ebx], MemRead
     mov cl, byte [edi+ebx]
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.D], cl
     mov edx,19
  end;
End;

Procedure OpDD5C;
Begin // LD E,IXH
  asm
     mov al, Byte [ecx+1]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,8
  end;
End;

Procedure OpDD5D;
Begin // LD E,IXL
  asm
     mov al, Byte [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.E], al
     mov edx,8
  end;
End;

Procedure OpDD5E;
Begin // LD E,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     or  Byte [MemAccess[0]+ebx], MemRead
     mov cl, byte [edi+ebx]
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.E], cl
     mov edx,19
  end;
End;

Procedure OpDD60;
Begin // LD IXH,B
  asm
     mov al, [esi+TZ80Registers.B]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx+1], al
     mov edx,8
  end;
End;

Procedure OpDD61;
Begin // LD IXH,C
  asm
     mov al, [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx+1], al
     mov edx,8
  end;
End;

Procedure OpDD62;
Begin // LD IXH,D
  asm
     mov al, [esi+TZ80Registers.D]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx+1], al
     mov edx,8
  end;
End;

Procedure OpDD63;
Begin // LD IXH,E
  asm
     mov al, [esi+TZ80Registers.E]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx+1], al
     mov edx,8
  end;
End;

Procedure OpDD64;
Begin // LD IXH,IXH
  asm
     inc Word [esi+TZ80Registers.PC]
     mov edx,8
  end;
End;

Procedure OpDD65;
Begin // LD IXH,IXL
  asm
     mov al, Byte [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx+1], al
     mov edx,8
  end;
End;

Procedure OpDD66;
Begin // LD H,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     or  Byte [MemAccess[0]+ebx], MemRead
     mov cl, byte [edi+ebx]
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.H], cl
     mov edx,19
  end;
End;

Procedure OpDD67;
Begin // LD IXH,A
  asm
     mov al, [esi+TZ80Registers.A]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx+1], al
     mov edx,8
  end;
End;

Procedure OpDD68;
Begin // LD IXL,B
  asm
     mov al, [esi+TZ80Registers.B]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx], al
     mov edx,8
  end;
End;

Procedure OpDD69;
Begin // LD IXL,C
  asm
     mov al, [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx], al
     mov edx,8
  end;
End;

Procedure OpDD6A;
Begin // LD IXL,D
  asm
     mov al, [esi+TZ80Registers.D]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx], al
     mov edx,8
  end;
End;

Procedure OpDD6B;
Begin // LD IXL,E
  asm
     mov al, [esi+TZ80Registers.E]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx], al
     mov edx,8
  end;
End;

Procedure OpDD6C;
Begin // LD IXL,IXH
  asm
     mov al, byte [ecx+1]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx], al
     mov edx,8
  end;
End;

Procedure OpDD6D;
Begin // LD IXL,IXL
  asm
     inc Word [esi+TZ80Registers.PC]
     mov edx,8
  end;
End;

Procedure OpDD6E;
Begin // LD L,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     or  Byte [MemAccess[0]+ebx], MemRead
     mov cl, byte [edi+ebx]
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.L], cl
     mov edx,19
  end;
End;

Procedure OpDD6F;
Begin // LD IXL,A
  asm
     mov al, [esi+TZ80Registers.A]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [ecx], al
     mov edx,8
  end;
End;

Procedure OpDD70;
Begin // LD (IX+d),B
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov cl, [esi+TZ80Registers.B]
     add Word [esi+TZ80Registers.PC], 2
     cmp ebx, $4000
     jb  @NoWrite
     mov byte [edi+ebx], cl
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov edx,19
  end;
End;

Procedure OpDD71;
Begin // LD (IX+d),C
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov cl, [esi+TZ80Registers.C]
     add Word [esi+TZ80Registers.PC], 2
     cmp ebx, $4000
     jb  @NoWrite
     mov byte [edi+ebx], cl
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov edx,19
  end;
End;

Procedure OpDD72;
Begin // LD (IX+d),D
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov cl, [esi+TZ80Registers.D]
     add Word [esi+TZ80Registers.PC], 2
     cmp ebx, $4000
     jb  @NoWrite
     mov byte [edi+ebx], cl
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov edx,19
  end;
End;

Procedure OpDD73;
Begin // LD (IX+d),E
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov cl, [esi+TZ80Registers.E]
     add Word [esi+TZ80Registers.PC], 2
     cmp ebx, $4000
     jb  @NoWrite
     mov byte [edi+ebx], cl
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov edx,19
  end;
End;

Procedure OpDD74;
Begin // LD (IX+d),H
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov cl, [esi+TZ80Registers.H]
     add Word [esi+TZ80Registers.PC], 2
     cmp ebx, $4000
     jb  @NoWrite
     mov byte [edi+ebx], cl
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov edx,19
  end;
End;

Procedure OpDD75;
Begin // LD (IX+d),L
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov cl, [esi+TZ80Registers.L]
     add Word [esi+TZ80Registers.PC], 2
     cmp ebx, $4000
     jb  @NoWrite
     mov byte [edi+ebx], cl
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov edx,19
  end;
End;

Procedure OpDD77;
Begin // LD (IX+d),A
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov cl, [esi+TZ80Registers.A]
     add Word [esi+TZ80Registers.PC], 2
     cmp ebx, $4000
     jb  @NoWrite
     mov byte [edi+ebx], cl
     or  Byte [MemAccess[0]+ebx], MemWrite
  @NoWrite:
     mov edx,19
  end;
End;

Procedure OpDD7C;
Begin // LD A,IXH
  asm
     mov al, Byte [ecx+1]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,8
  end;
End;

Procedure OpDD7D;
Begin // LD A,IXL
  asm
     mov al, Byte [ecx]
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.A], al
     mov edx,8
  end;
End;

Procedure OpDD7E;
Begin // LD A,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     or  Byte [MemAccess[0]+ebx], MemRead
     mov cl, byte [edi+ebx]
     add Word [esi+TZ80Registers.PC], 2
     mov Byte [esi+TZ80Registers.A], cl
     mov edx,19
  end;
End;

Procedure OpDD84;
Begin // ADD A,IXH
  asm
     mov  al, Byte [ecx+1]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD85;
Begin // ADD A,IXL
  asm
     mov  al, byte [ecx]
     mov  dl,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD86;
Begin // ADD A,(IX+d)
  asm
     mov  bx, Word [ecx]
     mov  al, dh
     cbw
     add  bx, ax
     or   Byte [MemAccess[0]+ebx], MemRead
     mov  al, byte [edi+ebx]
     mov  dl, [esi+TZ80Registers.A]
     add  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,19
  end;
End;

Procedure OpDD8C;
Begin // ADC A,IXH
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,Byte [ecx+1]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD8D;
Begin // ADC A,IXL
  asm
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     mov  al,Byte [ecx]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD8E;
Begin // ADC A,(IX+d)
  asm
     mov bx, Word [ecx]
     mov al, dh
     cbw
     add bx, ax
     mov  ah,[esi+TZ80Registers.F]
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+ebx], MemRead
     mov  al,Byte [edi+ebx]
     sahf
     adc  al,dl
     lahf
     seto dh
     mov  dl,al
     shl  dh,2
     and  ah,11010001b
     and  dl,00101000b
     or   ah,dh
     mov  Byte [esi+TZ80Registers.A],al
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC], 2
     mov  edx,19
  end;
End;

Procedure OpDD94;
Begin // SUB IXH
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,byte [ecx+1]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD95;
Begin // SUB IXL
  asm
     mov  al,[esi+TZ80Registers.A]
     sub  al,byte [ecx]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD96;
Begin // SUB (IX+d)
  asm
     mov  al, dh
     mov  cx, [ecx]
     and  ecx, $FFFF
     cbw
     add  cx, ax
     mov  al,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+ecx], MemRead
     sub  al,byte [edi+ecx]
     lahf
     mov  Byte [esi+TZ80Registers.A],al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   ah,al
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,19
  end;
End;

Procedure OpDD9C;
Begin // SBC A,IXH
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al, Byte [ecx+1]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD9D;
Begin // SBC A,IXL
  asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,Byte [ecx]
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDD9E;
Begin // SBC A,(IX+d)
  asm
     mov  al, dh
     and  ebx, $FFFF
     mov  bx, [ecx]
     cbw
     add  bx, ax
     or   Byte [MemAccess[0]+ebx], MemRead
     mov  cl, byte [edi+ebx]
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,cl
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,19
  end;
End;

Procedure OpDDA4;
Begin // AND IXH
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,byte [ecx+1]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDDA5;
Begin // AND IXL
  Asm
     mov  dl,[esi+TZ80Registers.A]
     and  dl,byte [ecx]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDDA6;
Begin // AND (IX+d)
  Asm
     mov  al, dh
     and  ebx, $FFFF
     mov  bx, [ecx]
     cbw
     add  bx, ax
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+ebx], MemRead
     and  dl,byte [edi+ebx]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     or   ah,16
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,19
  End;
End;

Procedure OpDDAC;
Begin // XOR IXH
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,byte [ecx+1]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDDAD;
Begin // XOR IXL
  Asm
     mov  dl,[esi+TZ80Registers.A]
     xor  dl,byte [ecx]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDDAE;
Begin // XOR (IX+d)
  Asm
     mov  al, dh
     and  ebx, $FFFF
     mov  bx, [ecx]
     cbw
     add  bx, ax
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+ebx], MemRead
     xor  dl,byte [edi+ebx]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,19
  End;
End;

Procedure OpDDB4;
Begin // OR IXH
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,Byte [ecx+1]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDDB5;
Begin // OR IXL
  Asm
     mov  dl,[esi+TZ80Registers.A]
     or   dl,Byte [ecx]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  End;
End;

Procedure OpDDB6;
Begin // OR (IX+d)
  Asm
     mov  al, dh
     and  ebx, $FFFF
     mov  bx, [ecx]
     cbw
     add  bx, ax
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+ebx], MemRead
     or   dl,byte [edi+ebx]
     lahf
     mov  Byte [esi+TZ80Registers.A],dl
     and  ah,11000100b
     and  dl,00101000b
     or   ah,dl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,19
  End;
End;

Procedure OpDDBC;
Begin // CP IXH
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,byte [ecx+1]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDDBD;
Begin // CP IXL
  asm
     mov  dl,[esi+TZ80Registers.A]
     cmp  dl,byte [ecx]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpDDBE;
Begin // CP (IX+d)
  asm
     mov  al, dh
     and  ebx, $FFFF
     mov  bx, [ecx]
     cbw
     add  bx, ax
     mov  dl,[esi+TZ80Registers.A]
     or   Byte [MemAccess[0]+ebx], MemRead
     cmp  dl,byte [edi+ebx]
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,19
  end;
End;

Procedure OpDDCB;
Begin // Handle xDCB Prefix
  asm
     shr   edx, 8          // dh  = opcode, dl = offset
     mov   al, dl
     cbw                   // ax  = signed extension (+d)
     movzx edx, dh
     add   ax, [ecx]       // ecx = address of IX/IY
     Add   Word [esi+TZ80Registers.PC],  3
     lea   ebx, [MemAccess[0] + eax]
     mov   DDCBPtr, ebx
     lea   ebx, [edi+eax]  // ebx = address of IX+d/IY+d
     Call  DWord [DDCBOps+edx*4]
  End;
End;

Procedure OpDDCB00;
Begin // LD B,RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B], al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB01;
Begin // LD C,RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C], al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB02;
Begin // LD D,RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D], al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB03;
Begin // LD E,RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E], al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB04;
Begin // LD H,RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H], al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB05;
Begin // LD L,RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L], al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB06;
Begin // RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCB07;
Begin // LD A,RLC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     rol  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A], al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB08;
Begin // LD B,RRC (IX+d)
  asm
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB09;
Begin // LD C,RRC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB0A;
Begin // LD D,RRC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB0B;
Begin // LD E,RRC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB0C;
Begin // LD H,RRC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB0D;
Begin // LD L,RRC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB0E;
Begin // RRC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCB0F;
Begin // LD A,RRC (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     ror  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB10;
Begin // LD B,RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB11;
Begin // LD C,RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB12;
Begin // LD D,RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB13;
Begin // LD E,RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB14;
Begin // LD H,RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB15;
Begin // LD L,RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB16;
Begin // RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCB17;
Begin // LD A,RL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcl  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB18;
Begin // LD B,RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB19;
Begin // LD C,RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB1A;
Begin // LD D,RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB1B;
Begin // LD E,RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB1C;
Begin // LD H,RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB1D;
Begin // LD L,RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB1E;
Begin // RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCB1F;
Begin // LD A,RR (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     mov  dh,[esi+TZ80Registers.F]
     ror  dh,1    // Set x86 Carry as with Z80 Carry
     rcr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB20;
Begin // LD B,SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB21;
Begin // LD C,SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB22;
Begin // LD D,SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB23;
Begin // LD E,SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB24;
Begin // LD H,SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB25;
Begin // LD L,SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB26;
Begin // SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCB27;
Begin // LD A,SLA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sal  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB28;
Begin // LD B,SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB29;
Begin // LD C,SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB2A;
Begin // LD D,SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB2B;
Begin // LD E,SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB2C;
Begin // LD H,SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB2D;
Begin // LD L,SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB2E;
Begin // SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCB2F;
Begin // LD A,SRA (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     sar  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB30;
Begin // LD B,SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB31;
Begin // LD C,SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB32;
Begin // LD D,SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB33;
Begin // LD E,SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB34;
Begin // LD H,SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB35;
Begin // LD L,SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB36;
Begin // SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCB37;
Begin // LD A,SLL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shl  al,1     // also clears bit 0
     adc  dl,0
     inc  al       // SLL sets bit 0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB38;
Begin // LD B,SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.B],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB39;
Begin // LD C,SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.C],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB3A;
Begin // LD D,SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.D],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB3B;
Begin // LD E,SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.E],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB3C;
Begin // LD H,SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.H],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB3D;
Begin // LD L,SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.L],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB3E;
Begin // SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCB3F;
Begin // LD A,SRL (IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [ebx]
     xor  dl,dl
     shr  al,1
     adc  dl,0
     test al,255
     lahf
     and  ah,11000100b
     or   dl,ah
     mov  ah,al
     and  ah,00101000b
     or   dl,ah
     mov  Byte [esi+TZ80Registers.F],dl
     mov  Byte [esi+TZ80Registers.A],al
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,24
  end;
End;

Procedure OpDDCB40;
Begin // BIT 0,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1   // clear all bits except carry
     and   bl,1   // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB48;
Begin // BIT 1,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1  // clear all bits except carry
     and   bl,2   // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB50;
Begin // BIT 2,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1   // clear all bits except carry
     and   bl,4   // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB58;
Begin // BIT 3,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1  // clear all bits except carry
     and   bl,8   // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB60;
Begin // BIT 4,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1   // clear all bits except carry
     and   bl,16  // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB68;
Begin // BIT 5,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1   // clear all bits except carry
     and   bl,32  // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB70;
Begin // BIT 6,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1   // clear all bits except carry
     and   bl,64  // the Bit
     jne   @BitFwd

     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB78;
Begin // BIT 7,(IX+d)
  asm
     or    Byte [MemAccess[0]+eax], MemRead
     mov   bl, Byte [ebx]
     mov   ah,[esi+TZ80Registers.F]
     and   ah,1   // clear all bits except carry
     and   bl,128 // the Bit
     je    @BitClear

     or    ah,128
     jmp   @BitFwd

@BitClear:
     or    ah,68  // set Z + V
@BitFwd:
     or    ah,16  // set H
     mov   Byte [esi+TZ80Registers.F],ah
     mov   edx,20
  end;
End;

Procedure OpDDCB80;
Begin // LD B,RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 254
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCB81;
Begin // LD C,RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 254
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCB82;
Begin // LD D,RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 254
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCB83;
Begin // LD E,RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 254
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCB84;
Begin // LD H,RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 254
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCB85;
Begin // LD L,RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 254
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCB86;
Begin // RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,1
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCB87;
Begin // LD A,RES 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 254
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCB88;
Begin // LD B,RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 253
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCB89;
Begin // LD C,RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 253
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCB8A;
Begin // LD D,RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 253
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCB8B;
Begin // LD E,RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 253
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCB8C;
Begin // LD H,RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 253
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCB8D;
Begin // LD L,RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 253
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCB8E;
Begin // RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,2
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCB8F;
Begin // LD A,RES 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 253
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCB90;
Begin // LD B,RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 251
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCB91;
Begin // LD C,RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 251
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCB92;
Begin // LD D,RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 251
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCB93;
Begin // LD E,RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 251
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCB94;
Begin // LD H,RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 251
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCB95;
Begin // LD L,RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 251
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCB96;
Begin // RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,4
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCB97;
Begin // LD A,RES 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 251
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCB98;
Begin // LD B,RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 247
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCB99;
Begin // LD C,RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 247
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCB9A;
Begin // LD D,RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov al, [ebx]
     and al, 247
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCB9B;
Begin // LD E,RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 247
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCB9C;
Begin // LD H,RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 247
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCB9D;
Begin // LD L,RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 247
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCB9E;
Begin // RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,8
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCB9F;
Begin // LD A,RES 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 247
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA0;
Begin // LD B,RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 239
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA1;
Begin // LD C,RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 239
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA2;
Begin // LD D,RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 239
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA3;
Begin // LD E,RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 239
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA4;
Begin // LD H,RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 239
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA5;
Begin // LD L,RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 239
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA6;
Begin // RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,16
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCBA7;
Begin // LD A,RES 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 239
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA8;
Begin // LD B,RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 223
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBA9;
Begin // LD C,RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 223
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBAA;
Begin // LD D,RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 223
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBAB;
Begin // LD E,RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 223
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBAC;
Begin // LD H,RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 223
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBAD;
Begin // LD L,RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 223
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBAE;
Begin // RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,32
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCBAF;
Begin // LD A,RES 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 223
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB0;
Begin // LD B,RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 191
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB1;
Begin // LD C,RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 191
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB2;
Begin // LD D,RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 191
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB3;
Begin // LD E,RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 191
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB4;
Begin // LD H,RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 191
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB5;
Begin // LD L,RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 191
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB6;
Begin // RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,64
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCBB7;
Begin // LD A,RES 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 191
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB8;
Begin // LD B,RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 127
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBB9;
Begin // LD C,RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 127
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBBA;
Begin // LD D,RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 127
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBBB;
Begin // LD E,RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 127
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBBC;
Begin // LD H,RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 127
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBBD;
Begin // LD L,RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 127
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBBE;
Begin // RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,128
     mov   al, Byte [ebx]
     not   cl
     mov   edx,eax
     and   edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  end;
End;

Procedure OpDDCBBF;
Begin // LD A,RES 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     and  al, 127
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC0;
Begin // LD B,SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 1
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC1;
Begin // LD C,SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 1
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC2;
Begin // LD D,SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 1
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC3;
Begin // LD E,SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 1
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC4;
Begin // LD H,SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 1
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC5;
Begin // LD L,SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 1
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC6;
Begin // SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,1 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBC7;
Begin // LD A,SET 0,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 1
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC8;
Begin // LD B,SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 2
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBC9;
Begin // LD C,SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 2
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBCA;
Begin // LD D,SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 2
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBCB;
Begin // LD E,SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 2
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBCC;
Begin // LD H,SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 2
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBCD;
Begin // LD L,SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 2
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBCE;
Begin // SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,2 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBCF;
Begin // LD A,SET 1,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 2
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD0;
Begin // LD B,SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 4
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD1;
Begin // LD C,SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 4
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD2;
Begin // LD D,SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 4
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD3;
Begin // LD E,SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 4
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD4;
Begin // LD H,SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 4
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD5;
Begin // LD L,SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 4
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD6;
Begin // SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,4 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBD7;
Begin // LD A,SET 2,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 4
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD8;
Begin // LD B,SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 8
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBD9;
Begin // LD C,SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 8
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBDA;
Begin // LD D,SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 8
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBDB;
Begin // LD E,SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 8
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBDC;
Begin // LD H,SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 8
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBDD;
Begin // LD L,SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 8
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBDE;
Begin // SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,8 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBDF;
Begin // LD A,SET 3,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 8
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE0;
Begin // LD B,SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 16
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE1;
Begin // LD C,SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 16
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE2;
Begin // LD D,SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 16
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE3;
Begin // LD E,SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 16
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE4;
Begin // LD H,SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 16
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE5;
Begin // LD L,SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 16
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE6;
Begin // SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,16 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBE7;
Begin // LD A,SET 4,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 16
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE8;
Begin // LD B,SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 32
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBE9;
Begin // LD C,SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 32
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBEA;
Begin // LD D,SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 32
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBEB;
Begin // LD E,SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 32
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBEC;
Begin // LD H,SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 32
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBED;
Begin // LD L,SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 32
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBEE;
Begin // SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,32 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBEF;
Begin // LD A,SET 5,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 32
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF0;
Begin // LD B,SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 64
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF1;
Begin // LD C,SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 64
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF2;
Begin // LD D,SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 64
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF3;
Begin // LD E,SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 64
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF4;
Begin // LD H,SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 64
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF5;
Begin // LD L,SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 64
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF6;
Begin // SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,64 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBF7;
Begin // LD A,SET 6,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 64
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF8;
Begin // LD B,SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 128
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.B], al
     mov  edx,24
  end;
End;

Procedure OpDDCBF9;
Begin // LD C,SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 128
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.C], al
     mov  edx,24
  end;
End;

Procedure OpDDCBFA;
Begin // LD D,SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 128
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.D], al
     mov  edx,24
  end;
End;

Procedure OpDDCBFB;
Begin // LD E,SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 128
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.E], al
     mov  edx,24
  end;
End;

Procedure OpDDCBFC;
Begin // LD H,SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 128
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.H], al
     mov  edx,24
  end;
End;

Procedure OpDDCBFD;
Begin // LD L,SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 128
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.L], al
     mov  edx,24
  end;
End;

Procedure OpDDCBFE;
Begin // SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov   cl,128 // Bit to SET
     mov   dl, Byte [ebx]
     or    edx,ecx
     cmp   ebx, $4000
     jb    @NoWrite
     mov   Byte [ebx], dl
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  edx,23
  End;
End;

Procedure OpDDCBFF;
Begin // LD A,SET 7,(IX+d)
  asm
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, [ebx]
     or   al, 128
     cmp  ebx, $4000
     jb   @NoWrite
     mov  Byte [ebx], al
     mov  Byte [DDCBPtr], MemWrite
  @NoWrite:
     mov  Byte [esi+TZ80Registers.A], al
     mov  edx,24
  end;
End;

Procedure OpDDDD;
Begin // DD/DD sequence
  Registers.CanInterrupt := False;
End;

Procedure OpDDE1;
Begin // POP IX
  asm
     mov ax, [esi+TZ80Registers.&SP]
     Add Word [esi+TZ80Registers.&SP], 2
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov ax, Word [edi+eax]
     mov [ecx], ax
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 14
  end;
End;

Procedure OpDDE3;
Begin // EX (SP),IX
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov dx, [ecx]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Word [edi+eax], dx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
  @NoWrite:
     mov [ecx], bx
     mov edx,23
  end;
End;

Procedure OpDDE5;
Begin // PUSH IX
  asm
     mov ax, [esi+TZ80Registers.&SP]
     Sub eax, 2
     mov cx, [ecx]
     cmp eax, $4000
     jb  @NoWrite
     Mov Word [edi+eax], cx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 15
  end;
End;

Procedure OpDDE9;
Begin // JP (IX)
  asm
     mov ax, [ecx]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,8
  end;
End;

Procedure OpDDF9;
Begin // LD SP,IX
  asm
     mov ax, [ecx]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov Word [esi+TZ80Registers.&SP], ax
     inc Word [esi+TZ80Registers.PC]
     mov edx,10
  end;
End;

Procedure OpDDFD;
Begin // DD/FD sequence
  Registers.CanInterrupt := False;
End;

Procedure OpDE;
Begin // SBC A,N
  Asm
     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  al,dl
     lahf
     mov  dl,al
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  dl,00101000b
     or   ah,dl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  Byte [esi+TZ80Registers.A],al
     add  Word [esi+TZ80Registers.PC], 2
     mov  edx,7
  End;
End;

Procedure OpDF;
Begin // RST 18H
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [esi+TZ80Registers.PC]
     sub ax, 2
     inc bx
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.PC], 24
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,11
  end;
End;

Procedure OpE0;
Begin // RET PO
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 4
     jnz @E0ParityEven

     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @E0ParityEven:
     Inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpE1;
Begin // POP HL
  asm
     mov ax, [esi+TZ80Registers.&SP]
     Add Word [esi+TZ80Registers.&SP], 2
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov ax, Word [edi+eax]
     mov Word [esi+TZ80Registers.L], ax
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 10
  end;
End;

Procedure OpE2;
Begin // JP PO,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 4
     jnz @E2ParityOverflow

     Mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
     ret

  @E2ParityOverflow:
     Add Word [esi+TZ80Registers.PC], 3
     mov edx,10
  End;
End;

Procedure OpE3;
Begin // EX (SP),HL
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov cx, Word [esi+TZ80Registers.L]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     mov Word [edi+eax], cx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
  @NoWrite:
     mov Word [esi+TZ80Registers.L], bx
     mov edx,19
  end;
End;

Procedure OpE4;
Begin // CALL PO,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 4
     jnz @E4ParityEven

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @E4ParityEven:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpE5;
Begin // PUSH HL
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov cx, Word [esi+TZ80Registers.L]
     Sub ax, 2
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     Mov Word [edi+eax], cx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx, 11
  end;
End;

Procedure OpE6;
Begin // AND N
  Asm
     mov  cl,[esi+TZ80Registers.A]
     and  cl,dl
     lahf
     mov  Byte [esi+TZ80Registers.A],cl
     and  ah,11000100b
     or   ah,16
     and  cl,00101000b
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,7
  End;
End;

Procedure OpE7;
Begin // RST 20H
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [esi+TZ80Registers.PC]
     sub ax, 2
     inc bx
     mov Word [esi+TZ80Registers.PC], 32
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,11
  end;
End;

Procedure OpE8;
Begin // RET PE
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 4
     jz  @E8NoParity

     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     Add Word [esi+TZ80Registers.&SP], 2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @E8NoParity:
     inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpE9;
Begin // JP (HL)
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov Word [esi+TZ80Registers.PC], ax
     mov edx, 4
  end;
End;

Procedure OpEA;
Begin // JP PE,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 4
     jz  @EAPEZero

     Mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
     ret

  @EAPEZero:
     Add Word [esi+TZ80Registers.PC], 3
     mov edx,10
  End;
End;

Procedure OpEB;
Begin // EX DE,HL
  asm
     mov cx, Word [esi+TZ80Registers.L]
     mov dx, Word [esi+TZ80Registers.E]
     mov Word [esi+TZ80Registers.E],  cx
     mov Word [esi+TZ80Registers.L],  dx
     inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure OpEC;
Begin // CALL PE,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 4
     jz  @ECOddParity

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @ECOddParity:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpED;
Begin // Handle ED Prefix
  asm
     xor  ecx, ecx
     mov  cl, dl
     Inc  Word [esi+TZ80Registers.PC]
     Call DWord [EDOps+ecx*4]
  end;
End;

Procedure OpED00;
Begin
  asm
     Call ROMTrap;
     mov  edx, 0
  end;
End;

Procedure OpED40;
Begin // IN B,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.B], al
     mov edx,12
  end;
End;

Procedure OpED41;
Begin // OUT (C),B
  asm
     mov  ax, Word [esi+TZ80Registers.C]
     mov  dl, [esi+TZ80Registers.B]
     Call SetPortByte
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx, 13
  end;
End;

Procedure OpED42;
Begin // SBC HL,BC
  asm
     mov  dx,Word [esi+TZ80Registers.C]
     mov  cx,Word [esi+TZ80Registers.L]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  cx,dx
     lahf
     mov  Word [esi+TZ80Registers.L],cx
     seto cl
     shl  cl,2
     and  ah,11010001b
     or   ah,cl
     and  ch,00101000b    // 5,3 from high byte
     or   ah,ch
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc Word [esi+TZ80Registers.PC]
     mov  edx,15
  end;
End;

Procedure OpED43;
Begin // LD (nn),BC
   asm
     shr   edx, 8
     and   edx, $FFFF
     mov   bx, Word [esi+TZ80Registers.C]
     cmp   edx, $4000
     jb    @NoWrite
     mov   Word [edi+edx], bx
     or  Byte [MemAccess[0]+edx], MemWrite
     or  Byte [MemAccess[0]+edx+1], MemWrite
   @NoWrite:
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,20
   End;
End;

Procedure OpED44;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED45;
Begin // RETN
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED46;
Begin // IM 0
  asm
     mov Byte [esi+TZ80Registers.IntMode], 0
     inc Word [esi+TZ80Registers.PC]
     mov edx,8
  end;
End;

Procedure OpED47;
Begin // LD I,A
  asm
     mov al, byte [esi+TZ80Registers.A]
     mov Byte [esi+TZ80Registers.I], al
     inc Word [esi+TZ80Registers.PC]
     mov edx,9
  end;
End;

Procedure OpED48;
Begin // IN C,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.C], al
     mov edx,12
  end;
End;

Procedure OpED49;
Begin // OUT (C),C
  asm
     mov ax, Word [esi+TZ80Registers.C]
     mov dl, [esi+TZ80Registers.C]
     Call SetPortByte
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 12
  end;
End;

Procedure OpED4A;
Begin // ADC HL,BC
  asm
     mov   cx,Word [esi+TZ80Registers.L]
     mov   dx,Word [esi+TZ80Registers.C]
     mov   ah,[esi+TZ80Registers.F]
     sahf
     adc   dx,cx
     lahf
     seto  cl
     and   ah,11010001b
     shl   cl,2
     or    ah,cl
     mov   cl,dh
     and   cl,00101000b
     or    ah,cl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   Word [esi+TZ80Registers.L],dx
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,15
  end;
End;

Procedure OpED4B;
Begin // LD BC,(nn)
  asm
     shr  edx, 8
     and  edx, $FFFF
     or  Byte [MemAccess[0]+edx], MemRead
     or  Byte [MemAccess[0]+edx+1], MemRead
     mov  ax, Word [edi+edx]
     Add  Word [esi+TZ80Registers.PC], 3
     mov  Word [esi+TZ80Registers.C],ax
     mov  edx,20
  end;
End;

Procedure OpED4C;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     Inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED4D;
Begin // RETI
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED4E;
Begin // IM 0/1
  asm
     mov Byte [esi+TZ80Registers.IntMode], 0
     inc Word [esi+TZ80Registers.PC]
     mov edx,8
  end;
End;

Procedure OpED4F;
Begin // LD R,A
  asm
     mov al, [esi+TZ80Registers.A]
     mov Byte [esi+TZ80Registers.R], al
     and al, 128
     inc Word [esi+TZ80Registers.PC]
     mov Byte [esi+TZ80Registers.RBit7], al
     mov edx,9
  end;
End;

Procedure OpED50;
Begin // IN D,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.D], al
     mov edx,12
  end;
End;

Procedure OpED51;
Begin // OUT (C),D
  asm
     mov ax, Word [esi+TZ80Registers.C]
     mov dl, [esi+TZ80Registers.D]
     Call SetPortByte
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 12
  end;
End;

Procedure OpED52;
Begin // SBC HL,DE
  asm
     mov  dx,Word [esi+TZ80Registers.E]
     mov  cx,Word [esi+TZ80Registers.L]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  cx,dx
     lahf
     mov  Word [esi+TZ80Registers.L],cx
     seto cl
     shl  cl,2
     and  ah,11010001b
     or   ah,cl
     and  ch,00101000b    // 5,3 from high byte
     or   ah,ch
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,15
  End;
End;

Procedure OpED53;
Begin // LD (nn),DE
   asm
     shr   edx, 8
     and   edx, $FFFF
     mov   bx, Word [esi+TZ80Registers.E]
     cmp   edx, $4000
     jb    @NoWrite
     mov   Word [edi+edx], bx
     or  Byte [MemAccess[0]+edx], MemWrite
     or  Byte [MemAccess[0]+edx+1], MemWrite
   @NoWrite:
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,20
   End;
End;

Procedure OpED54;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED55;
Begin // RETN
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED56;
Begin // IM 1
  asm
     mov Byte [esi+TZ80Registers.IntMode], 1
     inc Word [esi+TZ80Registers.PC]
     mov edx,8
  end;
End;

Procedure OpED57;
Begin // LD A,I
  asm
     mov     dl,[esi+TZ80Registers.F]
     mov     al,[esi+TZ80Registers.I]
     and     dl,1            // preserve Carry
     test    al,255
     lahf
     and     ah,11000000b
     or      dl,ah
     cmp     Byte [esi+TZ80Registers.IntsEnabled],True
     sete    dh       // bit 0 = int status
     mov     Byte [esi+TZ80Registers.A], al
     shl     dh,2     // shift to PV bit
     and     al,00101000b
     or      dl,dh    // PV = Interrupt status
     or      dl,al
     mov     Byte [esi+TZ80Registers.F],dl
     inc     Word [esi+TZ80Registers.PC]
     mov     edx,9
  end;
End;

Procedure OpED58;
Begin // IN E,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.E], al
     mov edx,12
  end;
End;

Procedure OpED59;
Begin // OUT (C),E
  asm
     mov ax, Word [esi+TZ80Registers.C]
     mov dl, [esi+TZ80Registers.E]
     Call SetPortByte
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 12
  end;
End;

Procedure OpED5A;
Begin // ADC HL,DE
  asm
     mov   cx, Word [esi+TZ80Registers.L]
     mov   dx, Word [esi+TZ80Registers.E]
     mov   ah,[esi+TZ80Registers.F]
     sahf
     adc   dx,cx
     lahf
     seto  cl
     and   ah,11010001b
     shl   cl,2
     or    ah,cl
     mov   cl,dh
     and   cl,00101000b
     or    ah,cl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   Word [esi+TZ80Registers.L], dx
     Inc   Word [esi+TZ80Registers.PC]
     mov   edx,15
  end;
End;

Procedure OpED5B;
Begin // LD DE,(nn)
  asm
     shr  edx, 8
     and  edx, $FFFF
     or  Byte [MemAccess[0]+edx], MemRead
     or  Byte [MemAccess[0]+edx+1], MemRead
     mov  ax, Word [edi+edx]
     Add  Word [esi+TZ80Registers.PC], 3
     mov  Word [esi+TZ80Registers.E],ax
     mov  edx,20
  end;
End;

Procedure OpED5C;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED5D;
Begin // RETN
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED5E;
Begin // IM 2
  asm
     mov Byte [esi+TZ80Registers.IntMode], 2
     inc Word [esi+TZ80Registers.PC]
     mov edx,8
  end;
End;

Procedure OpED5F;
Begin // LD A,R
  asm
     inc  Word [esi+TZ80Registers.PC]
     mov  al,[esi+TZ80Registers.R]
     and  al,127
     mov  dl,[esi+TZ80Registers.F]
     add  al,[esi+TZ80registers.RBit7]
     and  dl,1            // preserve Carry
     test al,255
     lahf
     and  ah,11000000b
     or   dl,ah
     cmp  Byte [esi+TZ80Registers.IntsEnabled],True
     sete dh       // bit 0 = int status
     shl  dh,2     // shift to PV bit
     and  al,00101000b
     or   dl,dh    // PV = Interrupt status
     or   dl,al
     mov  Byte [esi+TZ80Registers.F],dl
     mov  edx,9
  end;
End;

Procedure OpED60;
Begin // IN H,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.H], al
     mov edx,12
  end;
End;

Procedure OpED61;
Begin // OUT (C),H
  asm
     mov ax, Word [esi+TZ80Registers.C]
     mov dl, [esi+TZ80Registers.H]
     Call SetPortByte
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 12
  end;
End;

Procedure OpED62;
Begin // SBC HL,HL
  asm
     mov  dx, Word [esi+TZ80Registers.L]
     mov  cx,Word [esi+TZ80Registers.L]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  cx,dx
     lahf
     mov  Word [esi+TZ80Registers.L],cx
     seto cl
     shl  cl,2
     and  ah,11010001b
     or   ah,cl
     and  ch,00101000b    // 5,3 from high byte
     or   ah,ch
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,15
  End;
End;

Procedure OpED63;
Begin // LD (nn),HL
   asm
     shr   edx, 8
     and   edx, $FFFF
     mov   bx, Word [esi+TZ80Registers.L]
     cmp   edx, $4000
     jb    @NoWrite
     mov   Word [edi+edx], bx
     or  Byte [MemAccess[0]+edx], MemWrite
     or  Byte [MemAccess[0]+edx+1], MemWrite
   @NoWrite:
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,16
   End;
End;

Procedure OpED64;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED65;
Begin // RETN
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED66;
Begin // IM 0
  asm
     mov  Byte [esi+TZ80Registers.IntMode], 0
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpED67;
Begin // RRD
  Asm
     mov  ax,Word [esi+TZ80Registers.L]
     mov  dl, Byte [edi+eax]
     mov  cl,[esi+TZ80Registers.A]   //cl = A

     mov  dh,cl
     and  dh,240  // preserve 4 MSB of A in dh

     rcr  cl,1
     rcr  dl,1
     rcr  cl,1
     rcr  dl,1
     rcr  cl,1
     rcr  dl,1
     rcr  cl,1
     rcr  dl,1
     rcr  cl,1
     rcr  cl,4

     and  cl,15
     or   cl,dh
     mov  Byte [esi+TZ80Registers.A],cl

     mov  bl,[esi+TZ80Registers.F]
     and  bl,1    // preserve Carry
     test cl,255
     lahf
     and  ah,11000100b    //S, Z, V
     or   bl,ah
     and  cl,00101000b    // 5, 3
     or   bl,cl
     mov  Byte [esi+TZ80Registers.F],bl

     mov  ax,Word [esi+TZ80Registers.L]
     mov  Byte [edi+eax], dl

     inc  Word [esi+TZ80Registers.PC]
     mov  edx, 18
  End;
End;

Procedure OpED68;
Begin // IN L,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.L], al
     mov edx,12
  end;
End;

Procedure OpED69;
Begin // OUT (C),L
  asm
     mov ax, Word [esi+TZ80Registers.C]
     mov dl, [esi+TZ80Registers.L]
     Call SetPortByte
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 12
  end;
End;

Procedure OpED6A;
Begin // ADC HL,HL
  asm
     mov   dx,Word [esi+TZ80Registers.L]
     mov   ah,[esi+TZ80Registers.F]
     sahf
     adc   dx,dx
     lahf
     seto  cl
     and   ah,11010001b
     shl   cl,2
     or    ah,cl
     mov   cl,dh
     and   cl,00101000b
     or    ah,cl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   Word [esi+TZ80Registers.L],dx
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,15
  end;
End;

Procedure OpED6B;
Begin // LD HL,(nn)
  asm
     shr  edx, 8
     and  edx, $FFFF
     or  Byte [MemAccess[0]+edx], MemRead
     or  Byte [MemAccess[0]+edx+1], MemRead
     mov  ax, Word [edi+edx]
     Add  Word [esi+TZ80Registers.PC], 3
     mov  Word [esi+TZ80Registers.L],ax
     mov  edx,16
  end;
End;

Procedure OpED6C;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED6D;
Begin // RETN
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED6E;
Begin // IM 0/1
  asm
     mov  Byte [esi+TZ80Registers.IntMode], 1
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpED6F;
Begin // RLD
  Asm
     mov  ax,Word [esi+TZ80Registers.L]
     mov  cl,[esi+TZ80Registers.A]   //cl = A
     mov  dl, Byte [edi+eax]

     mov  dh,cl
     and  dh,240  // preserve 4 MSB of A in dh

     rcl  cl,4
     rcl  cl,1
     rcl  dl,1
     rcl  cl,1
     rcl  dl,1
     rcl  cl,1
     rcl  dl,1
     rcl  cl,1
     rcl  dl,1
     rcl  cl,1

     and  cl,15
     or   cl,dh
     mov  Byte [esi+TZ80Registers.A],cl

     mov  bl,[esi+TZ80Registers.F]
     and  bl,1    // preserve Carry
     test cl,255
     lahf
     and  ah,11000100b    //S, Z, V
     or   bl,ah
     and  cl,00101000b    // 5, 3
     or   bl,cl
     mov  Byte [esi+TZ80Registers.F],bl

     mov  ax,Word [esi+TZ80Registers.L]
     mov   Byte [edi+eax], dl
     inc  Word [esi+TZ80Registers.PC]
     mov  edx, 18
  End;
End;

Procedure OpED70;
Begin // IN F,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.F], al
     mov edx,12
  end;
End;

Procedure OpED71;
Begin // OUT (C),0
  asm
     mov ax, Word [esi+TZ80Registers.C]
     mov dl, 0
     Call SetPortByte
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 12
  end;
End;

Procedure OpED72;
Begin // SBC HL,SP
  Asm
     mov  dx,[esi+TZ80Registers.&SP]
     mov  cx,Word [esi+TZ80Registers.L]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     sbb  cx,dx
     lahf
     mov  Word [esi+TZ80Registers.L],cx
     seto cl
     shl  cl,2
     and  ah,11010001b
     or   ah,cl
     and  ch,00101000b    // 5,3 from high byte
     or   ah,ch
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,15
  End;
End;

Procedure OpED73;
Begin // LD (nn),SP
  asm
     shr   edx, 8
     and   edx, $FFFF
     mov   bx, Word [esi+TZ80Registers.&SP]
     cmp   edx, $4000
     jb    @NoWrite
     mov   Word [edi+edx], bx
     or  Byte [MemAccess[0]+edx], MemWrite
     or  Byte [MemAccess[0]+edx+1], MemWrite
  @NoWrite:
     add   Word [esi+TZ80Registers.PC], 3
     mov   edx,20
  End;
End;

Procedure OpED74;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED75;
Begin // RETN
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]

     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED76;
Begin // IM 1
  asm
     mov  Byte [esi+TZ80Registers.IntMode], 1
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpED78;
Begin // IN A,(C)
  asm
     mov ax, Word Ptr [esi+TZ80Registers.C]
     inc Word [esi+TZ80Registers.PC]
     Call GetPortByte;
     mov Byte [esi+TZ80Registers.A], al
     mov edx,12
  end;
End;

Procedure OpED79;
Begin // OUT (C),A
  asm
     mov ax, Word [esi+TZ80Registers.C]
     mov dl, [esi+TZ80Registers.A]
     Call SetPortByte
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 12
  end;
End;

Procedure OpED7A;
Begin // ADC HL,SP
  asm
     mov   cx,Word [esi+TZ80Registers.L]
     mov   dx,[esi+TZ80Registers.&SP]
     mov   ah,[esi+TZ80Registers.F]
     sahf
     adc   dx,cx
     lahf
     seto  cl
     and   ah,11010001b
     shl   cl,2
     or    ah,cl
     mov   cl,dh
     and   cl,00101000b
     or    ah,cl
     mov   Byte [esi+TZ80Registers.F],ah
     mov   Word [esi+TZ80Registers.L],dx
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,4
  end;
End;

Procedure OpED7B;
Begin // LD SP,(nn)
  asm
     shr  edx, 8
     and  edx, $FFFF
     or  Byte [MemAccess[0]+edx], MemRead
     or  Byte [MemAccess[0]+edx+1], MemRead
     mov  ax, Word [edi+edx]
     Add  Word [esi+TZ80Registers.PC], 3
     mov  Word [esi+TZ80Registers.&SP],ax
     mov  edx,20
  end;
End;

Procedure OpED7C;
Begin // NEG
  Asm
     mov   dl,2  // set N
     neg   [esi+TZ80Registers.A]
     lahf
     seto  dh
     shl   dh,2
     or    dl,dh
     and   ah,11010001b
     or    dl,ah
     mov   al,[esi+TZ80Registers.A]
     and   al,00101000b
     or    dl,al
     mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,8
  End;
End;

Procedure OpED7D;
Begin // RETN
  asm
     mov bx, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+ebx], MemRead
     or  Byte [MemAccess[0]+ebx+1], MemRead
     mov ax, Word [edi+ebx]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], ax
     mov edx,14

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

  end;
End;

Procedure OpED7E;
Begin // IM 2
  asm
     mov  Byte [esi+TZ80Registers.IntMode], 2
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,8
  end;
End;

Procedure OpEDA0;
Begin // LDI
  Asm
     mov   ax,Word [esi+TZ80Registers.L]
     mov   cl,[esi+TZ80Registers.A]
     or    Byte [MemAccess[0]+eax], MemRead
     mov   al, Byte [edi+eax]
     mov   bx,Word [esi+TZ80Registers.E]
     add   cl,al

     cmp   ebx, $4000
     jb    @NoWrite

     mov   Byte [edi+ebx],al
     or    Byte [MemAccess[0]+ebx], MemWrite

  @NoWrite:
     mov   dl,[esi+TZ80Registers.F]
     inc   Word [esi+TZ80Registers.L]
     and   dl,193   //clear 5,H,3,V,N
     inc   Word [esi+TZ80Registers.E]
     dec   Word [esi+TZ80Registers.C]
     jz    @S1
     or    dl,4

@S1: test  cl,2
     jz    @S2
     or    dl,32

@S2: test  cl,8
     jz    @S3
     or    dl,8

@S3: mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,16
  End;
End;

Procedure OpEDA1;
Begin // CPI
  Asm
     mov  ax,Word [esi+TZ80Registers.L]
     or   Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [edi+eax]   // al=(HL)

     mov  bl,al  // bl=(HL)

     mov  ah,[esi+TZ80Registers.F]
     mov  al,[esi+TZ80Registers.A]
     mov  bh,ah
     and  bh,1
     sahf
     sub  al,bl
     lahf
     and  ah,11010000b // S,Z,H from A-(HL)
     or   ah,2

     inc  Word [esi+TZ80Registers.L]

     mov  dl,ah
     and  dl,11010011b  // clear F5,F3,PV
     mov  al,[esi+TZ80Registers.A]
     sub  al,bl

     test dl,16
     jz   @Fwd1       // jump if H clear
     dec  al

@Fwd1:
     test al,2        // test bit 1
     jz   @Fwd2
     or   dl,32       // F5

@Fwd2:
     test al,8        // test bit 3
     jz   @Fwd3
     or   dl,8        // F3

@Fwd3:
     dec  Word [esi+TZ80Registers.C]
     jz   @Fwd4
     or   dl,4        // PV

@Fwd4:
     and  dl,254
     or   dl,bh
     mov  Byte [esi+TZ80Registers.F],dl
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  End;
End;

Procedure OpEDA2;
Begin // INI
  Asm
     mov   ax,Word [esi+TZ80Registers.C]
     call  GetPortByte
     mov   dl,al
     mov   ax,Word [esi+TZ80Registers.L]
     mov   Byte [edi+eax], dl
     or    Byte [MemAccess[0]+eax], MemWrite
     inc   Word [esi+TZ80Registers.L]
     lea   ecx,[esi+TZ80Registers.B]

     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  End;
End;

Procedure OpEDA3;
Begin // OUTI
  Asm
     lea  ecx,[esi+TZ80Registers.B]
     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov   ax, Word [[esi+TZ80Registers.L]]
     and   eax, $FFFF
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     mov   ax, Word [esi+TZ80Registers.C]
     call  SetPortByte
     inc   Word [esi+TZ80Registers.L]
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,16
  End;
End;

Procedure OpEDA8;
Begin // LDD
  Asm
     mov   ax,Word [esi+TZ80Registers.L]
     mov   cl,[esi+TZ80Registers.A]
     or    Byte [MemAccess[0]+eax], MemRead
     mov   al, Byte [edi+eax]
     mov   bx,Word [esi+TZ80Registers.E]
     add   cl,al

     cmp   ebx, $4000
     jb    @NoWrite

     mov   Byte [edi+ebx],al
     or    Byte [MemAccess[0]+ebx], MemWrite

  @NoWrite:
     mov   dl,[esi+TZ80Registers.F]
     dec   Word [esi+TZ80Registers.L]
     and   dl,193   //clear 5,H,3,V,N
     dec   Word [esi+TZ80Registers.E]
     dec   Word [esi+TZ80Registers.C]
     jz    @S1
     or    dl,4

@S1: test  cl,2
     jz    @S2
     or    dl,32

@S2: test  cl,8
     jz    @S3
     or    dl,8

@S3: mov   Byte [esi+TZ80Registers.F],dl
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,16
  End;
End;

Procedure OpEDA9;
Begin // CPD
  Asm
     mov  ax, Word [esi+TZ80Registers.L]
     or    Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [edi+eax]   // al=(HL)

     mov  bl,al  // bl=(HL)

     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     mov  bh,ah
     and  bh,1
     sahf
     sub  al,bl
     lahf
     and  ah,11010000b // S,Z,H from A-(HL)
     or   ah,2

     dec  Word [esi+TZ80Registers.L]

     mov  dl,ah
     and  dl,11010011b  // clear F5,F3,PV
     mov  al,[esi+TZ80Registers.A]
     sub  al,bl

     test dl,16
     jz   @Fwd1       // jump if H clear
     dec  al

@Fwd1:
     test al,2        // test bit 1
     jz   @Fwd2
     or   dl,32       // F5

@Fwd2:
     test al,8        // test bit 3
     jz   @Fwd3
     or   dl,8        // F3

@Fwd3:
     dec  Word [esi+TZ80Registers.C]
     jz   @Fwd4
     or   dl,4        // PV

@Fwd4:
     and  dl,254
     or   dl,bh
     mov  Byte [esi+TZ80Registers.F],dl
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  End;
End;

Procedure OpEDAA;
Begin // IND
  Asm
     mov   ax,Word [esi+TZ80Registers.C]
     call  GetPortByte
     mov   dl,al
     mov   ax,Word [esi+TZ80Registers.L]
     mov   Byte [edi+eax], dl
     or    Byte [MemAccess[0]+eax], MemWrite
     dec   Word [esi+TZ80Registers.L]
     lea   ecx,[esi+TZ80Registers.B]

     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  End;
End;

Procedure OpEDAB;
Begin // OUTD
  Asm
     lea  ecx,[esi+TZ80Registers.B]
     mov  ah,[esi+TZ80Registers.F]
     mov  al,[ecx]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov   ax, Word [[esi+TZ80Registers.L]]
     and   eax, $FFFF
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     mov   ax,Word [esi+TZ80Registers.C]
     call  SetPortByte
     dec   Word [esi+TZ80Registers.L]
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,16
  End;
End;

Procedure OpEDB0;
Begin // LDIR
  Asm
     mov   ax,Word [esi+TZ80Registers.L]
     mov   cl,[esi+TZ80Registers.A]
     or    Byte [MemAccess[0]+eax], MemRead
     mov   al, Byte [edi+eax]
     mov   bx,Word [esi+TZ80Registers.E]
     add   cl,al

     cmp   ebx, $4000
     jb    @NoWrite

     mov   Byte [edi+ebx],al
     or    Byte [MemAccess[0]+ebx], MemWrite

  @NoWrite:
     mov   dl,[esi+TZ80Registers.F]
     inc   Word [esi+TZ80Registers.L]
     and   dl,193   //clear 5,H,3,V,N
     inc   Word [esi+TZ80Registers.E]
     dec   Word [esi+TZ80Registers.C]
     jz    @S1
     or    dl,4

@S1: test  cl,2
     jz    @S2
     or    dl,32

@S2: test  cl,8
     jz    @S3
     or    dl,8

@S3: mov   Byte [esi+TZ80Registers.F],dl
     cmp   Word [esi+TZ80Registers.C],0
     je    @norepeat

     dec   Word [esi+TZ80Registers.PC]
     mov   edx,21
     ret

  @norepeat:
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,16
  End;
End;

Procedure OpEDB1;
Begin // CPIR
  Asm
     mov  ax,Word [esi+TZ80Registers.L]
     or    Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [edi+eax]   // al=(HL)

     mov  bl,al  // bl=(HL)

     mov  ah,[esi+TZ80Registers.F]
     mov  al,[esi+TZ80Registers.A]
     mov  bh,ah
     and  bh,1
     sahf
     sub  al,bl
     lahf
     and  ah,11010000b // S,Z,H from A-(HL)
     or   ah,2

     inc  Word [esi+TZ80Registers.L]

     mov  dl,ah
     and  dl,11010011b  // clear F5,F3,PV
     mov  al,[esi+TZ80Registers.A]
     sub  al,bl

     test dl,16
     jz   @Fwd1       // jump if H clear
     dec  al

@Fwd1:
     test al,2        // test bit 1
     jz   @Fwd2
     or   dl,32       // F5

@Fwd2:
     test al,8        // test bit 3
     jz   @Fwd3
     or   dl,8        // F3

@Fwd3:
     dec  Word [esi+TZ80Registers.C]
     jz   @Fwd4
     or   dl,4        // PV

@Fwd4:
     and  dl,254
     or   dl,bh
     mov  Byte [esi+TZ80Registers.F],dl
     and  dl,64
     jnz  @IncPC

     mov  dl,[esi+TZ80Registers.B]
     or   dl,[esi+TZ80Registers.C]
     jz   @IncPC

     dec  Word [esi+TZ80Registers.PC]
     mov  edx,21

     ret

@IncPC:
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  End;
End;

Procedure OpEDB2;
Begin // INIR
  Asm
     mov   ax,Word [esi+TZ80Registers.C]
     call  GetPortByte
     mov   dl,al
     mov   ax,Word [esi+TZ80Registers.L]
     mov   Byte [edi+eax], dl
     or    Byte [MemAccess[0]+eax], MemWrite
     inc   Word [esi+TZ80Registers.L]
     lea   ecx,[esi+TZ80Registers.B]

     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     cmp  Byte [esi+TZ80Registers.B],0
     jne  @IncPC

     Dec  Word [esi+TZ80Registers.PC]
     mov  edx,21
     ret

@IncPC:
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  End;
End;

Procedure OpEDB3;
Begin // OTIR
  Asm
     lea  ecx,[esi+TZ80Registers.B]
     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov   ax, Word [[esi+TZ80Registers.L]]
     and   eax, $FFFF
     or    Byte [MemAccess[0]+eax], MemRead
     mov   dl, Byte [edi+eax]
     mov   ax, Word [esi+TZ80Registers.C]
     call  SetPortByte
     inc   Word [esi+TZ80Registers.L]
     cmp  Byte [esi+TZ80Registers.B],0
     jne  @IncPC

     Dec  Word [esi+TZ80Registers.PC]
     mov  edx, 21
     ret

@IncPC:
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx, 16
  End;
End;

Procedure OpEDB8;
Begin // LDDR
  Asm
     mov   ax,Word [esi+TZ80Registers.L]
     mov   cl,[esi+TZ80Registers.A]
     or    Byte [MemAccess[0]+eax], MemRead
     mov   al, Byte [edi+eax]
     mov   bx,Word [esi+TZ80Registers.E]
     add   cl,al

     cmp   ebx, $4000
     jb    @NoWrite

     mov   Byte [edi+ebx],al
     or    Byte [MemAccess[0]+ebx], MemWrite

  @NoWrite:
     mov   dl,[esi+TZ80Registers.F]
     dec   Word [esi+TZ80Registers.L]
     and   dl,193   //clear 5,H,3,V,N
     dec   Word [esi+TZ80Registers.E]
     dec   Word [esi+TZ80Registers.C]
     jz    @S1
     or    dl,4

@S1: test  cl,2
     jz    @S2
     or    dl,32

@S2: test  cl,8
     jz    @S3
     or    dl,8

@S3: mov   Byte [esi+TZ80Registers.F],dl
     cmp   Word [esi+TZ80Registers.C],0
     je    @norepeat

     dec   Word [esi+TZ80Registers.PC]
     mov   edx,21
     ret

  @norepeat:
     inc   Word [esi+TZ80Registers.PC]
     mov   edx,16
  End;
End;

Procedure OpEDB9;
Begin // CPDR
  Asm
     mov  ax, Word [esi+TZ80Registers.L]
     or    Byte [MemAccess[0]+eax], MemRead
     mov  al, Byte [edi+eax]   // al=(HL)

     mov  bl,al  // bl=(HL)

     mov  al,[esi+TZ80Registers.A]
     mov  ah,[esi+TZ80Registers.F]
     mov  bh,ah
     and  bh,1
     sahf
     sub  al,bl
     lahf
     and  ah,11010000b // S,Z,H from A-(HL)
     or   ah,2

     dec  Word [esi+TZ80Registers.L]

     mov  dl,ah
     and  dl,11010011b  // clear F5,F3,PV
     mov  al,[esi+TZ80Registers.A]
     sub  al,bl

     test dl,16
     jz   @Fwd1       // jump if H clear
     dec  al

@Fwd1:
     test al,2        // test bit 1
     jz   @Fwd2
     or   dl,32       // F5

@Fwd2:
     test al,8        // test bit 3
     jz   @Fwd3
     or   dl,8        // F3

@Fwd3:
     dec  Word [esi+TZ80Registers.C]
     jz   @Fwd4
     or   dl,4        // PV

@Fwd4:
     and  dl,254
     or   dl,bh
     mov  Byte [esi+TZ80Registers.F],dl
     and  dl,64
     jnz  @IncPC

     mov  dl,[esi+TZ80Registers.B]
     or   dl,[esi+TZ80Registers.C]
     jz   @IncPC

     dec  Word [esi+TZ80Registers.PC]
     mov  edx,21

     ret

@IncPC:
     inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  End;
End;

Procedure OpEDBA;
Begin // INDR
  Asm
     mov   ax,Word [esi+TZ80Registers.C]
     call  GetPortByte
     mov   dl,al
     mov   ax,Word [esi+TZ80Registers.L]
     mov   Byte [edi+eax], dl
     or    Byte [MemAccess[0]+eax], MemWrite
     dec   Word [esi+TZ80Registers.L]
     lea   ecx,[esi+TZ80Registers.B]

     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     cmp  Byte [esi+TZ80Registers.B],0
     jne  @IncPC

     Dec  Word [esi+TZ80Registers.PC]
     mov  edx,21
     ret

@IncPC:
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx,16
  end;
End;

Procedure OpEDBB;
Begin // OTDR
  Asm
     lea  ecx,[esi+TZ80Registers.B]
     mov  al,[ecx]
     mov  ah,[esi+TZ80Registers.F]
     sahf
     dec  al
     lahf
     mov  [ecx],al
     mov  cl,al
     seto ch
     and  ah,11010011b
     shl  ch,2
     and  cl,00101000b
     or   ah,ch
     or   ah,cl
     or   ah,2
     mov  Byte [esi+TZ80Registers.F],ah
     mov  ax, Word [[esi+TZ80Registers.L]]
     and  eax, $FFFF
     or    Byte [MemAccess[0]+eax], MemRead
     mov  dl, Byte [edi+eax]
     mov  ax,Word [esi+TZ80Registers.C]
     call SetPortByte
     dec  Word [esi+TZ80Registers.L]
     cmp  Byte [esi+TZ80Registers.B],0
     jne  @IncPC

     Dec  Word [esi+TZ80Registers.PC]
     mov  edx, 21
     ret

@IncPC:
     Inc  Word [esi+TZ80Registers.PC]
     mov  edx, 16
  End;
End;

Procedure OpEE;
Begin // XOR N
  Asm
     mov  cl,[esi+TZ80Registers.A]
     xor  cl,dl
     lahf
     mov  Byte [esi+TZ80Registers.A],cl
     and  ah,11000100b
     and  cl,00101000b
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,7
  End;
End;

Procedure OpEF;
Begin // RST 28H
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [esi+TZ80Registers.PC]
     sub ax, 2
     inc bx
     mov Word [esi+TZ80Registers.PC], 40
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,11
  end;
End;

Procedure OpF0;
Begin // RET P
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 128
     jnz @F0Negative

     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @F0Negative:
     Inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpF1;
Begin // POP AF
  asm
     mov ax, [esi+TZ80Registers.&SP]
     Add Word [esi+TZ80Registers.&SP], 2
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov ax, Word [edi+eax]
     mov Word [esi+TZ80Registers.F], ax
     Inc Word [esi+TZ80Registers.PC]
     mov edx, 10
  end;
End;

Procedure OpF2;
Begin // JP P,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 128
     jnz @F2Negative

     mov Word [esi+TZ80Registers.PC], dx
     mov edx,10
     ret

  @F2Negative:
     Add Word [esi+TZ80Registers.PC],3
     mov edx,10
  end;
End;

Procedure OpF3;
Begin // DI
  TokenBuffer := '';
  asm
     mov Byte [esi+TZ80Registers.IntsEnabled], False
     inc Word [esi+TZ80Registers.PC];
     mov edx,4
  end;
End;

Procedure OpF4;
Begin // CALL P,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 128
     jnz @F4Negative

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @F4Negative:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpF5;
Begin // PUSH AF
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov cx, Word [esi+TZ80Registers.F]
     Sub ax, 2
     Inc Word [esi+TZ80Registers.PC]
     cmp eax, $4000
     jb  @NoWrite
     Mov Word [edi+eax], cx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx, 11
  end;
End;

Procedure OpF6;
Begin // OR N
  Asm
     mov  cl,[esi+TZ80Registers.A]
     or   cl,dl
     lahf
     mov  Byte [esi+TZ80Registers.A],cl
     and  ah,11000100b
     and  cl,00101000b
     or   ah,cl
     mov  Byte [esi+TZ80Registers.F],ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,7
  End;
End;

Procedure OpF7;
Begin // RST 30H
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [esi+TZ80Registers.PC]
     sub ax, 2
     inc bx
     mov Word [esi+TZ80Registers.PC], 48
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,11
  end;
End;

Procedure OpF8;
Begin // RET M
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 128
     jz  @F8Positive

     mov ax, [esi+TZ80Registers.&SP]
     or  Byte [MemAccess[0]+eax], MemRead
     or  Byte [MemAccess[0]+eax+1], MemRead
     mov bx, Word [edi+eax]
     add Word [esi+TZ80Registers.&SP],2
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,11

     cmp ExitProcOperation, True
     jne @NoExit

     dec CALLCounter
     jnz @NoExit

     mov StepOperation, True

  @NoExit:

     ret

  @F8Positive:
     Inc Word [esi+TZ80Registers.PC]
     mov edx,5
  end;
End;

Procedure OpF9;
Begin // LD SP,HL
  asm
     mov ax, Word [esi+TZ80Registers.L]
     mov Word [esi+TZ80Registers.&SP], ax
     Inc Word [esi+TZ80Registers.PC]
     mov edx,6
  end;
End;

Procedure OpFA;
Begin // JP M,$+3
  asm
     mov al, [esi+TZ80Registers.F]
     and al, 128
     jz  @FAPositive

     mov ax, Word [esi+TZ80Registers.PC]
     mov ax, Word [edi+eax+1]
     Mov Word [esi+TZ80Registers.PC], ax
     mov edx,10
     ret

  @FAPositive:
     Add Word [esi+TZ80Registers.PC], 3
     mov edx,10
  End;
End;

Procedure OpFB;
Begin // EI
  TokenBuffer := '';
  asm
     mov Byte [esi+TZ80Registers.IntsEnabled], True
     Mov Byte [esi+TZ80Registers.CanInterrupt], False
     inc Word [esi+TZ80Registers.PC]
     mov edx,4
  end;
End;

Procedure OpFC;
Begin // CALL M,NN
  asm
     mov cl, [esi+TZ80Registers.F]
     add bx, 3
     and cl, 128
     jz  @FCPositive

     inc CALLCounter

     mov ax, [esi+TZ80Registers.&SP]
     mov Word [esi+TZ80Registers.PC], dx
     sub ax, 2
     cmp eax, $4000
     jb  @NoWrite

     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite

  @NoWrite:
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,17
     ret

  @FCPositive:
     mov Word [esi+TZ80Registers.PC], bx
     mov edx,10
  end;
End;

Procedure OpFD;
Begin // Handle FD Prefix
  asm
     xor  eax, eax
     xor  ebx, ebx
     lea  ecx, [esi+TZ80Registers.IY]
     mov  al, dl
     mov  bx, Word [esi+TZ80Registers.IY]
     Inc  Word [esi+TZ80Registers.PC]
     Call DWord [DDOps+eax*4]
  End;
End;

Procedure OpFE;
Begin // CP N
  asm
     mov  cl,[esi+TZ80Registers.A]
     cmp  cl,dl
     lahf
     seto dh
     shl  dh,2
     and  ah,11010001b
     or   ah,dh
     and  al,00101000b
     or   al,2
     or   ah,al
     mov  Byte [esi+TZ80Registers.F], ah
     add  Word [esi+TZ80Registers.PC],2
     mov  edx,7
  end;
End;

Procedure OpFF;
Begin // RST 38H
  asm
     mov ax, [esi+TZ80Registers.&SP]
     mov bx, Word [esi+TZ80Registers.PC]
     sub ax, 2
     inc bx
     mov Word [esi+TZ80Registers.PC], 56
     mov Word [edi+eax], bx
     or  Byte [MemAccess[0]+eax], MemWrite
     or  Byte [MemAccess[0]+eax+1], MemWrite
     mov Word [esi+TZ80Registers.&SP], ax
     mov edx,11
  end;
End;

Function GetPortByte(Port: Word): Byte;
Var
  MSB: Byte;
Label
  SetFlags;
Begin

  Result := 255;

  If Not Evaluating Then Begin

     If (Port And 1) = 0 Then Begin // Port FE
        MSB := Port Shr 8;
        If KeyBuffer <> '' Then UnBufferKey;
        Asm
           push  eax
           mov   ah, MSB
           mov   al, 255
           shr   ah, 1
           jc    @Fwd1
           and   al, Byte Ptr Ports[$FEFE]
@Fwd1:     shr   ah, 1
           jc    @Fwd2
           and   al, Byte Ptr Ports[$FDFE]
@Fwd2:     shr   ah, 1
           jc    @Fwd3
           and   al, Byte Ptr Ports[$FBFE]
@Fwd3:     shr   ah, 1
           jc    @Fwd4
           and   al, Byte Ptr Ports[$F7FE]
@Fwd4:     shr   ah, 1
           jc    @Fwd5
           and   al, Byte Ptr Ports[$EFFE]
@Fwd5:     shr   ah, 1
           jc    @Fwd6
           and   al, Byte Ptr Ports[$DFFE]
@Fwd6:     shr   ah, 1
           jc    @Fwd7
           and   al, Byte Ptr Ports[$BFFE]
@Fwd7:     shr   ah, 1
           jc    @Fwd8
           and   al, Byte Ptr Ports[$7FFE]
@Fwd8:
           and   al, 31
           or    al, 160
           mov   [@Result], al
           pop   eax
        End;
        Goto SetFlags;
     End;

     If (Port and 4) = 0 Then Begin // ZX Printer
        If opt_ConsoleAddon then begin
          if Port=1515 then begin
            Result:=ConsoleAddon[ConsoleAddon[0]+1];
            goto SetFlags;
          End;
        End;

        If (Port = $FF3B) And (Opt_64Colours) Then Begin
           If Last64Port And 64 = 0 Then
              Result := Palette64[Last64Port and 63]
           Else
              If Active64Colours Then
                 Result := 1
              Else
                 Result := 0;
        End Else
           Result := PrinterForm.PrinterIn;
        Goto Setflags;
     End;

     //Kempston mouse added by arda

     If Opt_Kmouse then begin
     Readkempstonmouse;
     Case Port of
       64223, 1751: Begin
                //If MouseHooked Then Begin
			if DisplayMouseBtn < 4 then
			        Result := 255 xor (((DisplayMouseBtn And 1) Shl 1) + ((DisplayMouseBtn And 2) Shr 1))
                        Else
				Result := 255;
				goto SetFlags;
			End;
     		//End;
       64479, 4929: Begin Result := KempMouseX; Goto SetFlags; End;
       65503, 1951: Begin Result := KempMouseY; Goto SetFlags; End;
     end;
  end;

  //added by arda

     If Port And 49154 = 49152 then Begin   // port FFFD
        Case AYRegSelected Of
           0: Result := ShadowAYRegs.R0;
           1: Result := ShadowAYRegs.R1;
           2: Result := ShadowAYRegs.R2;
           3: Result := ShadowAYRegs.R3;
           4: Result := ShadowAYRegs.R4;
           5: Result := ShadowAYRegs.R5;
           6: Result := ShadowAYRegs.R6;
           7: Result := ShadowAYRegs.R7;
           8: Result := ShadowAYRegs.R8;
           9: Result := ShadowAYRegs.R9;
          10: Result := ShadowAYRegs.R10;
          11: Result := ShadowAYRegs.R11;
          12: Result := ShadowAYRegs.R12;
          13: Result := ShadowAYRegs.R13;
          14: Result := ShadowAYRegs.R14;
          15: If ShadowAYRegs.R7 < 128 Then Result := 255 Else Result := ShadowAYRegs.R15;
        End;
        Goto Setflags;
     End;

SetFlags:

     Registers.F := (Registers.F And 41) Or (Result And 128) Or (Parity[Result] Shl 2);
     If Result = 0 Then Registers.F := Registers.F Or 64;

  End Else Begin

     EvalRegisters.F := (EvalRegisters.F And 41) Or (Result And 128) Or (Parity[Result] Shl 2);
     If Result = 0 Then EvalRegisters.F := EvalRegisters.F Or 64;

  End;

End;

Procedure SetPortByte(Port: Word; Value: Byte);
var
x: byte;
ConsText: string;
Begin

  If Evaluating Then Exit;

  If (Port And 1) = 0 Then Begin
     // Port $FE
     Registers.LastFE := Value;
     asm
        and  dl, 7
        cmp  dl, Byte [BorderDWord]
        je   @Skip

        mov  dh, dl
        mov  cx, dx
        shl  edx, 16
        mov  dx, cx

        cmp  Evaluating, True
        je   @Skip

        mov  BorderDWord, edx
        mov  BorderUpdate, True // border area requires rendering
     @Skip:
     End;
     If Not Evaluating Then
        EarBit := ((Value and 16) Shr 4);
  End;

  If Port and 4 = 0 Then Begin
     If Opt_ConsoleAddon Then Begin
         // SimpleCon Console Addon is handled here
         // user sets the index of console text.
         // if 04EB(1259) value is 255 it's regarded as linefeed.
         // values of 0..254 can be set without changing other parts of text
         // port 05EB(1515) sets the text and increases the index value of port 1259
         // see example in examples folder
         If Port = $04EB then begin
             ConsoleAddon[0]:=value;

             if (value=255) then begin
                if(ConsoleOutForm.CheckBox1.Checked) then begin
                 ConsoleOutForm.memo1.Lines.add (Trim(ConsoleOutForm.Edit1.Text));
                end else begin
                 ConsoleOutForm.memo1.Lines.add (ConsoleOutForm.Edit1.Text);
                end;
             end;

         end;

         If (Port = $05EB) then begin
             if (ConsoleAddon[0]<>255) then begin
                ConsoleAddon[0]:=ConsoleAddon[0]+1;
                ConsoleAddon[ConsoleAddon[0]]:=value;
                ConsText:='';
                For x:=1 to 255 Do Begin
                  ConsText:=ConsText + Chr(ConsoleAddon[x]);
                end;
                ConsoleOutForm.Edit1.Text:=ConsText;
                ShowWindow(ConsoleOutForm,False);
              End;
         End;
     End;

     If Opt_64Colours Then Begin
        If Port = $BF3B Then
           Last64Port := Value
        Else
           If Port = $FF3B Then Begin
              If Last64Port and 64 = 0 Then
                 SetPalette64Entry(Last64Port And 63, Value)
              Else Begin
                 If Value And 1 = 0 Then
                    Active64Colours := False
                 Else
                    Active64Colours := True;
                 Activate64Colours(Active64Colours);
              End;
           End Else
              PrinterForm.PrinterOut(Value);
     End Else
        PrinterForm.PrinterOut(Value);
  End;


  If Port and 32770 = 0 Then
     Page7FFD(Value);

  Case Port And 49154 of
    32768: Begin // port $BFFD
              Case AYRegSelected Of
                 0: Begin AYRegisters.R0 := Value; ShadowAYRegs.R0 := Value; End;
                 1: Begin AYRegisters.R1 := Value and 15; ShadowAYRegs.R1 := Value; End;
                 2: Begin AYRegisters.R2 := Value; ShadowAYRegs.R2 := Value; End;
                 3: Begin AYRegisters.R3 := Value and 15; ShadowAYRegs.R3 := Value; End;
                 4: Begin AYRegisters.R4 := Value; ShadowAYRegs.R4 := Value; End;
                 5: Begin AYRegisters.R5 := Value and 15; ShadowAYRegs.R5 := Value; End;
                 6: Begin AYRegisters.R6 := Value and 31; ShadowAYRegs.R6 := Value; End;
                 7: Begin AYRegisters.R7 := Value; ShadowAYRegs.R7 := Value; End;
                 8: Begin AYRegisters.R8 := Value and 31; ShadowAYRegs.R8 := Value; End;
                 9: Begin AYRegisters.R9 := Value and 31; ShadowAYRegs.R9 := Value; End;
                10: Begin AYRegisters.R10 := Value and 31; ShadowAYRegs.R10 := Value; End;
                11: Begin AYRegisters.R11 := Value; ShadowAYRegs.R11 := Value; End;
                12: Begin AYRegisters.R12 := Value; ShadowAYRegs.R12 := Value; End;
                13: Begin
                       AYRegisters.R13 := Value and 15;
                       ShadowAYRegs.R13 := Value;
                       AYRegisters.EnvCounter := 0;
                       Case AYRegisters.R13 Of
                          0..3: Begin
                                   AYRegisters.EnvMode := env_DECAY;
                                   AYRegisters.EnvVolume := 15;
                                End;
                          4..7: Begin
                                   AYRegisters.EnvMode := env_ATTACK;
                                   AYRegisters.EnvVolume := 0;
                                End;
                         8..11: Begin
                                   AYRegisters.EnvMode := env_DECAY;
                                   AYRegisters.EnvVolume := 15;
                                End;
                        12..15: Begin
                                   AYRegisters.EnvMode := env_ATTACK;
                                   AYRegisters.EnvVolume := 0;
                                End;
                       End;
                    End;
                14: Begin AYRegisters.R14 := Value; ShadowAYRegs.R14 := Value;  End; // MIDIGatherBits(Value);
                15: Begin AYRegisters.R15 := Value; ShadowAYRegs.R15 := Value; End;
              End;
           End;
    49152: Begin
             AYRegSelected := Value And 15; // port $FFFD
             LastFFFD := Value;
           End;
  End;

  If BorderUpdate Then
     DisplayWindow.Panel1.Color := TFColorAToTColor(DisplayPalette[BorderDWord And 7]);

End;

// This version of the emulation loop will run at normal speed
// and uses the sound buffer to synchronise the emulation.

Procedure ExecuteEmulationLoop_SpectrumSpeed;
Begin

  If MemMapWindow.Visible Then
     ZeroMemory(@MemAccess[0], 65536);
  
  asm

     pushad

     lea  edi, Memory
     lea  esi, Registers

     push ebp
     xor  ebp, ebp

  @OpcodeLoop:

     xor  ebx, ebx
     mov  bx,  Word [esi+TZ80Registers.PC] // ESI points to the start of the Registers Structure, so EBX is now PC
     or   Byte [MemAccess[0]+ebx], MovePC

     xor  ecx, ecx
     mov  cl,  Byte [edi+ebx]              // ECX now holds the current opcode - EDI points to the start of memory.
     mov  edx, DWord [edi+ebx+1]           // Pre-fill EDX with any params this opcode may take, and if this is a prefix,
                                           // The prefixed opcode itself.
     xor  eax, eax
     call DWord [Ops+ecx*4]                // Now use ECX as a vector into the Opcodes table declared at the start of this file.

     add  PrinterTs, edx                   // Done enough Ts for the Printer to have updated?
     mov  eax, PrinterUpdateTs
     cmp  PrinterTs, eax
     jb   @SkipPrinter

     pushad
     call UpdatePrinter
     popad

  @SkipPrinter:

     add  AYTs, edx                        // Done enough TStates for the AY to need updating?
     mov  eax, AYSampleTs
     cmp  AYTs, eax
     jb   @SkipAY

     pushad
     call UpdateAYState
     popad

  @SkipAY:

     add  SoundTs, edx                     // Done enough T-States to get a sample?
     mov  eax, SampleTs
     cmp  SoundTs, eax
     jb   @SkipSound

     Pushad
     Call BufferSoundSample
     Popad

  @SkipSound:

     cmp  ProfilingEnabled, True            // If profiling is enabled, then we need to update the TStates.
     jne  @SkipProfile

     pushad
     mov  eax, edx
     Call UpdateProfileTs;
     popad

  @SkipProfile:

     xor  ebx, ebx
     lea  eax, CPUBreakpoints
     mov  bx,  Word [esi+TZ80Registers.PC]
     mov  cl,  Byte [eax+ebx]
     cmp  cl,  0
     je   @NoBreak

     mov  StepOperation, True
     jp   @Stop

  @NoBreak:

     cmp  Registers.EmuRunning, True
     jne  @Stop

     cmp  SoundAvailable, True
     je   @SkipHalt

     cmp  Registers.HaltEmu, True
     je   @Stop

  @SkipHalt:

     cmp  StepOperation, True
     je   @Stop

     add  ebp, edx
     cmp  ebp, Opt_CPUSpeed
     jc   @OpcodeLoop

  @Stop:

     pop  ebp
     Popad

     cmp  CPUShowing, True
     jne  @SkipCPU

     Pushad
     Call UpdateCPU;
     Popad

  @SkipCPU:

  End;

End;

{$O-}

Procedure ExecuteEmulationLoop_FullSpeed;
Begin

  asm

     pushad

     push ebp
     call gettickcount
     mov  ebp, eax

     lea  edi, Memory
     lea  esi, Registers

  @OpcodeLoop:

     cmp  Registers.haltemu, true
     mov  edx, 4
     je   @Halted

     xor  ebx, ebx
     mov  bx,  Word [esi+TZ80Registers.PC]
     or   Byte [MemAccess+ebx], MovePC
     xor  ecx, ecx
     mov  cl,  Byte [edi+ebx]
     mov  edx, DWord [edi+ebx+1]
     xor  eax, eax
     call DWord [Ops+ecx*4]

  @Halted:

     cmp  ProfilingEnabled, True
     jne  @SkipProfile

     pushad
     mov  eax, edx
     Call UpdateProfileTs;
     popad

  @SkipProfile:

     xor  ecx, ecx
     mov  cx,  Word [esi+TZ80Registers.PC]
     lea  eax, CPUBreakpoints
     mov  bl, byte [eax+ecx]
     cmp  bl, 0
     jz   @NoBreak

     mov  StepOperation, True
     jp   @Abort

  @NoBreak:

     cmp  Registers.EmuRunning, False
     je   @Abort

     cmp  StepOperation, True
     je   @Abort

     call GetTickCount
     sub  eax, ebp
     cmp  eax, 20

     jb   @OpcodeLoop

  @Abort:

     pop  ebp
     popad

     cmp  CPUShowing, True
     jne  @SkipCPU

     Pushad
     Call UpdateCPU;
     Popad

  @SkipCPU:

  End;

End;

{$O+}

Procedure ExecuteEndOfFrame;
Begin
  If Registers.EmuRunning Then Begin

     Inc(ElapsedFrames);

     If ElapsedFrames = 16 Then Begin
        Registers.FlashState := 1 - Registers.FlashState;
        ElapsedFrames := 0;
     End;
     NeedDisplayUpdate := True;

     If Registers.IntsEnabled then
        InvokeInterrupt
     Else
        If ResetCounter > 0 Then
           Registers.HaltEmu := False;
  End;
End;

Procedure InvokeInterrupt;
Begin

 InInterrupt := True;

 asm

   pushad

   lea  edi, Memory
   lea  esi, Registers

   cmp Byte [esi+TZ80Registers.HaltEmu], True
   jne @SkipHalt

   inc Word [esi+TZ80Registers.PC]
   mov Byte [esi+TZ80Registers.HaltEmu], False

 @SkipHalt:
   Mov Byte [esi+TZ80Registers.IntsEnabled], False

   cmp Byte [esi+TZ80Registers.IntMode], 2
   jz  @IM2

   xor eax, eax
   mov ax, [esi+TZ80Registers.&SP]
   mov bx, Word [esi+TZ80Registers.PC]
   sub ax, 2
   mov Word [esi+TZ80Registers.PC], 56
   mov Word [edi+eax], bx
   mov Word [esi+TZ80Registers.&SP], ax
   popad
   ret

 @IM2:
   xor eax,eax
   xor ebx,ebx
   xor ecx, ecx
   mov ax, [esi+TZ80Registers.&SP]
   mov bx, Word [esi+TZ80Registers.PC]
   sub ax, 2
   mov cx, Word [esi+TZ80Registers.R]
   mov [edi+eax], bx
   or  cx, $FF
   mov bx, [edi+ecx]
   mov Word [esi+TZ80Registers.PC], bx

   popad

 end;

End;

Procedure UpdateDisplay;
Var
  CharCount,
  DisplayOffset,
  AttrOffset,
  CurrDisplayLine:  DWord;
  LocalFlashState:  Byte;
  Y, X, Xc, Addr, ByteVal, AttrVal, INK, PAPER, BRIGHT, FLASH, ClrFG, ClrBG, Z: DWord;
Begin

  DisplayBitsPtr := FastCore.Display.Bits;
  LocalFlashState := Registers.FlashState;

  If Active64Colours And Opt_64Colours Then Begin


     Asm
        pushad

        mov   edi, DisplayBitsPtr

        // 320 x 240 x 8bpp DIB

        // draw the top border area
        // no border update is now required
        mov   BorderUpdate, False
        mov   ecx, (320*24)/4
        mov   eax, BorderDWord
        add   eax, 134744072
        rep   stosd

        mov   edx, 192
  @borderedges:
        // left border
        mov   ecx, 32/4
        rep   stosd
        add   edi, 256
        // right border
        mov   ecx, 32/4
        rep   stosd

        dec   edx
        jnz   @borderedges

        // draw the bottom border area
        mov   ecx, (320*24)/4
        rep   stosd

        popad

     End;

     For Y := 0 To 191 Do Begin
        Addr := ScreenAddresses[Y]+16384;
        Xc := 0;
        For X := 0 To 31 Do Begin
           ByteVal := Memory[Addr + X];
           AttrVal := Memory[AttrAddresses[(Addr + X) - 16384] + 16384];
           INK := AttrVal And 7;
           PAPER := (AttrVal Shr 3) And 7;
           BRIGHT := (AttrVal And 64) Shr 6;
           FLASH := (AttrVal And 128) Shr 7;
           ClrFg := (FLASH * 2 + BRIGHT) * 16 + INK;
           ClrBg := (FLASH * 2 + BRIGHT) * 16 + PAPER + 8;
           For Z := 7 Downto 0 Do Begin
              If ByteVal and (1 Shl Z) = 0 Then
                 FastCore.Display.Pixels8[Y + 24, Xc + 32] := ClrBg
              Else
                 FastCore.Display.Pixels8[Y + 24, Xc + 32] := ClrFg;
              Inc(Xc);
           End;
        End;
     End;

  End Else Begin

     Asm
        pushad

        mov   edi, DisplayBitsPtr

        // 320 x 240 x 8bpp DIB

        cmp   BorderUpdate, True

        // jump if no border update is required
        jne   @StartScreenUpdate

        // draw the top border area
        // no border update is now required
        mov   BorderUpdate, False
        mov   ecx, (320*24)/4
        mov   eax, BorderDWord
        rep   stosd

        mov   edx, 192
  @borderedges:
        // left border
        mov   ecx, 32/4
        rep   stosd
        add   edi, 256
        // right border
        mov   ecx, 32/4
        rep   stosd

        dec   edx
        jnz   @borderedges

        // draw the bottom border area
        mov   ecx, (320*24)/4
        rep   stosd

  @StartScreenUpdate:
        mov   DisplayOffset, 0
        mov   AttrOffset, 6144
        mov   CurrDisplayLine, 0

        mov   edi, DisplayBitsPtr
        add   edi, (320*24)+32
        // edi = screen area of DIB
        // esi = speccy display memory
        mov   esi, ScreenPointer

  @NextScreenLine:
        mov   CharCount,32

  @DrawScreenLine:
        mov   ecx,DisplayOffset
        mov   edx,AttrOffset

        mov   bl,[esi+ecx]
        mov   al,[esi+edx]
        and   ebx, 255
        and   eax, 255

        // Flash bit clear?
        test  al,128
        je    @Next
        // clear Flash bit
        and   al,127

        // jump if not a Flashing frame
        cmp   LocalFlashState,1
        jne   @Next

        // read the appropriate swapped colour entry
        mov   al,[Byte Ptr FlashSwapTable+eax]

  @Next:

        // * 2048 (256 entries per colour * 8 bytes/entry)
        shl   eax,11
        lea   eax,[DrawTSColTable+eax+ebx*8]
        mov   ecx,[eax]
        mov   edx,[eax+4]
        mov   [edi],ecx
        mov   [edi+4],edx
        lea   edi,[edi+8]

        inc   DisplayOffset
        inc   AttrOffset

        // loop for 32 chars
        dec   CharCount
        jnz   @DrawScreenLine

        // move to next screen line
        add   edi, 320-256

        sub   AttrOffset,32
        mov   eax,DisplayOffset
        add   ax,256-32
        test  ah,7
        jnz   @Skipoos

        sub   ah,8
        add   AttrOffset,32
        add   al,32
        test  al,224
        jnz   @Skipoos
        add   ah,8

  @Skipoos:
        inc   CurrDisplayLine
        mov   DisplayOffset,eax
        cmp   CurrDisplayLine,192
        jne   @NextScreenLine

  @SCRExit:
        popad
     End;

  End;

  if  (FASTMode) Then Begin
      FastCore.Display.Pixels8[220,300] := 0;
      FastCore.Display.Pixels8[221,300] := 0;
      FastCore.Display.Pixels8[222,300] := 0;
      FastCore.Display.Pixels8[221,301] := 0;
      //End Else Begin

  End;
End;

Procedure SaveEmulationState(var State: TEmulationState);
Begin
  CopyMemory(@State.Memory[0], @Memory[0], 65536);
  CopyMemory(@State.Registers.PC, @Registers.PC, SizeOf(TZ80Registers));
  State.Registers.EmuRunning := Registers.EmuRunning;
  State.Running := BASinOutput.Running;
  State.CursorType := CursorType;
  State.Editing := Editing;
  State.ProgState := ProgStateFlag;
  State.BorderDWord := BorderDWord;
End;

Procedure LoadEmulationState(var State: TEmulationState; Notify: Boolean);
Begin
  CopyMemory(@Memory[0], @State.Memory[0], 65536);
  CopyMemory(@Registers.PC, @State.Registers.PC, SizeOf(TZ80Registers));
  BorderDWord := State.BorderDWord;
  BorderUpdate := True;
  SetPortByte($FE, BorderDWord And 255);
  UpdateDisplay;
  UpdateBASinDisplay;
  SetPortByte($FE, Registers.LastFE);
  ControlEmulation(State.Registers.EmuRunning);
  BASinOutput.Running := State.Running;
  FullSpeed := False;
  CursorType := State.CursorType;
  ProgStateFlag := State.ProgState;
  If Notify Then Begin
     PostMessage(BASinOutPut.Handle, WM_UPDATEPROGBUTTONS, 0, 0);
     PostMessage(BASinOutput.Handle, WM_UPDATEPROGRAM, 0, 0);
     PostMessage(BASinOutput.Handle, WM_UPDATEVARS, 0, 0);
     PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);
  End;
End;

Procedure Activate64Colours(Activate: Boolean);
Var
  Index: Integer;
Begin

  If Activate Then Begin
     SetUpScreenLUT;
     Update64ColourDIBs;
  End Else Begin

     //ardafix
     //returning back from ula64 set the default colours back
     FastCore.Display.Colors := @DisplayPalette;
     FastCore.Display.UpdateColors;

     DisplayWindow.DisplayIMG.Bmp.Colors := @DisplayPalette;
     DisplayWindow.DisplayIMG.Bmp.UpdateColors;
     BorderUpdate:= true;
     // end ardafix

     For Index := 0 To 63 Do Begin
        _2xSaIColours16[Index] := (((DisplayPalette64[Index].r and 248) Shr 3) shl 10)
                                + (((DisplayPalette64[Index].g and 248) Shr 3) shl 5)
                                + (((DisplayPalette64[Index].b and 248) Shr 3));
        _Hq2xColours16[Index]  := ((DisplayPalette64[Index].r and 248) Shl 8)
                                + ((DisplayPalette64[Index].g and 252) Shl 3)
                                + ((DisplayPalette64[Index].b and 248) Shr 3);
     End;
  End;

End;

Function Get64ColourByte(Clr: Byte): Byte;
Begin

  Result := (((Clr And 4) Shr 2) * (128+16)) + (((Clr And 2) Shr 1) * (64+8+2)) + ((Clr And 1) * (32+4+1));

End;



Procedure Update64ColourDIBs;
Begin

  If Active64Colours Then Begin

     FastCore.Display.Colors := @DisplayPalette64;
     FastCore.Display.UpdateColors;

     DisplayWindow.DisplayIMG.Bmp.Colors := @DisplayPalette64;
     DisplayWindow.DisplayIMG.Bmp.UpdateColors;



  End;

End;

Procedure SetPalette64Entry(Index: Integer; Value: Byte);
Begin

  Palette64[Index] := Value;

  DisplayPalette64[Index].b := Get64ColourByte(((Value and 3) Shl 1) + Value And 1);
  DisplayPalette64[Index].r := Get64ColourByte((Value and 28) Shr 2);
  DisplayPalette64[Index].g := Get64ColourByte((Value and 224) Shr 5);

  _2xSaIColours16[Index] := (((DisplayPalette64[Index].r and 248) Shr 3) shl 10)
                          + (((DisplayPalette64[Index].g and 248) Shr 3) shl 5)
                          + (((DisplayPalette64[Index].b and 248) Shr 3));
  _Hq2xColours16[Index]  := ((DisplayPalette64[Index].r and 248) Shl 8)
                          + ((DisplayPalette64[Index].g and 252) Shl 3)
                          + ((DisplayPalette64[Index].b and 248) Shr 3);

  If Active64Colours Then Begin
     Update64ColourDIBs;
     UpdateBASinDisplay;
  End;

End;

end.


