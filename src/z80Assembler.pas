unit Z80Assembler;

interface

uses
  Windows, Classes, SysUtils, Math;

type
  PDWordArray = ^TDWordArray;
  TDWordArray = array[0..8191] of DWord;

  TZ80MemBlock = class
    Name: ShortString;
    Z80Start: DWord;                                    
    Z80End: DWord;
    StartAddress: Pointer;
    AltLo: Integer;
    AltHi: Integer;
    Tag: Integer;
  end;
  TAsmMacroRecord = Packed Record
    Source: TStringList;
    Line: Integer;
  end;
  PAsmWord = ^TAsmWord;
  TAsmRepRecord = Packed Record
    repLine: Integer;
    repCount: Integer;
  end;
  TAsmWord = packed record
    S: String;
    I: Integer;
  end;
  PAsmError = ^TAsmError;
  TAsmError = packed record
    ErrorNum: Integer;
    ErrorLine: Integer;
    SourceFileNum: Integer;
    null: Integer;
  end;
  PAsmDbgLine = ^TAsmDbgLine;
  TAsmDbgLine = packed record
    StartAddress: DWord; // (macro: index of next macro with same name)
    SetData: Word; // flags
    SrcFileNum: Word; // source file number
    Page: DWord;
    SymbolNum: Integer;
    Len: Integer;
    res1, res2, res3: Integer;
  end;
  PAsmSymbol = ^TAsmSymbol;
  TAsmSymbol = packed record
    StartAddress: DWord; // (macro: index of next macro with same name)
    SetData: Word; // flags
    SrcFileNum: Word; // source file number
    Page: DWord;  // Pointer to TZ80MemBlock containing symbol
                  //(macro: bits 16-31: (WORD) Source File Number)
                  //(macro: bits 08-15: (BYTE) Highest number of args)
                  //(macro: bits 00-07: (BYTE) Lowest number of args)
    DefinedAt: DWord; // Pointer to symbol definition (macro: Integer = Source line number)
  end;
  PAsmMacro = ^TAsmSymbol;
  TAsmMacro = packed record
    nextMacro: DWord; // (macro: index of next macro with same name)
    SetData: DWord; // flags
    srcFileNum: Word; // source file number
    loArgs: Byte; // Lowest number of args;
    hiArgs: Byte; // Highest number of args;
    lineNum: Integer; // Source line number;
  end;
  TAsmSrcFile = record
    src: TStringList;
    filename: String;
  end;
  TAsmCallback = procedure(sender: TObject; Addr: Word; var Len: Word; page: TZ80MemBlock; srcFile, srcLine: Integer);
  TZ80Assembler = class(TObject)
    DEFSDefault: Byte;
    val1: Integer;
    resultCode: Integer;
    PageList: Array of TZ80MemBlock;
    AsmSymbols: Array[0..31] of TStringList;
    AsmDbgLines: Array of TAsmDbgLine;
    AsmSymbolsExt: Array of Integer;
    NumAsmSymbols: Integer;
    NumDbgLines: Integer;
    SourceFiles: TStringList;
    SourceFilenames: TStringList;
    CurPage: TZ80MemBlock;
    oldPage: TZ80MemBlock;
    CurPageName: String;
    OrgAddr: Integer;
    AsmAddr: Integer;
    AsmVPtr: Integer;
    AsmLineAddr: DWord;
    CreateExtraInfo: Boolean;
    NumErrors: Integer;
    Errors: TStringList;
    QErrors: Array of Integer;
    srcFiles: Array of TAsmSrcFile;
    PassNum: Integer;
    totalBytes: Integer;
    DefaultPage: TZ80MemBlock;
    LastNumErrors, LastSymErrors: Integer;
    LastSymbolDef: Pointer;
    LastSymbolDefLen: Integer;
    atStartOfPass, atEndOfPass: TNotifyEvent;
    afterLine: TAsmCallback;

    Constructor Create;
    Destructor  Destroy;

    Procedure ResetMemBlocks;
    procedure SetMem48(buffer: Pointer);
    function AddMem(Name: ShortString; Start, Size: Integer;
      buffer: Pointer): TZ80MemBlock;
    procedure AddTZ80MemBlock(m: TZ80MemBlock);
    function  GetMemParams(Name: ShortString; var AltLo, AltHi: DWord): Pointer;
    function  evaluate(S: Pointer; c: Integer): Integer;
    Function  Assemble(Source: TStringList; Name: ShortString; StartAddress: Integer;
      StartPage: ShortString): Integer;
    Function  doPass(Source: TStringList; var Name: ShortString; StartAddress: Integer;
      var StartPage: ShortString; Pass: Integer): Boolean;
    procedure eval1(S: String; var V, Code: Integer);
    function FindChar(S: String; C, A, Q: Char; P: Integer): Integer;
    Function ReadMemByte(Address: Word): Byte;
    Function ReadMemWord(Address: Word): Word;
    procedure AddErrorQuick(errorNum, Line: Integer; FileNum: Integer);
    function GetErrorString(index: Integer): String;
    function GetError(index: Integer; var Line: Integer; var FileName: String;
      var ErrorType: Integer; var Text: String): Boolean;
    function retFileSize(Filename: String):Integer;
    function GetSymbol(S: ShortString; var value: DWord): Boolean;
    procedure AddSource(source: TStringList; filename: String);
    function RemoveSource(filename: String): boolean;
  end;

  function isInstruction(S: PChar): Integer;

Const

  SYM_SET     = $8000;
  SYM_DEAD    = $4000;
  SYM_MACRO   = $2000;

  DBG_BYTE    = $1000;
  DBG_WORD    = $2000;
  DBG_TEXT    = $4000;

// assembler reserved words. these can't be used for label/macro definitions etc.
// (S: text; I: IDCode)
// must be sorted alphabetically by S
// Assembler acts on IDCode, not index, in case we want to insert a word later
// next free code = 178
  AsmWords: array [0..186] of TAsmWord = (
    (S: '(BC)'; I: 138), (S: '(C)'; I: 157),
    (S: '(DE)'; I: 139), (S: '(E)'; I: 158),
    (S: '(HL)'; I: 140), (S: '(SP)'; I: 141),
    (S: '.IF'; I: 170),
    (S: 'A'; I: 120),
    (S: 'A'#39; I: 128), (S: 'ADC'; I: 172),
    (S: 'ADD'; I: 1), (S: 'AF'; I: 112),
    (S: 'AF'#39; I: 116), (S: 'ALIGN'; I: 2),
    (S: 'AND'; I: 3), (S: 'AT'; I: 183), (S: 'B'; I: 122),
    (S: 'B'#39; I: 130), (S: 'BASE'; I: 177),
    (S: 'BC'; I: 113),
    (S: 'BC'#39; I: 117), (S: 'BIT'; I: 4),
    (S: 'BSS'; I: 5), (S: 'C'; I: 123),
    (S: 'C'#39; I: 131), (S: 'CALL'; I: 6),
    (S: 'CCF'; I: 7), (S: 'CODE'; I: 8),
    (S: 'CP'; I: 9), (S: 'CPD'; I: 10),
    (S: 'CPDR'; I: 11), (S: 'CPI'; I: 12),
    (S: 'CPIR'; I: 13), (S: 'CPL'; I: 14),
    (S: 'D'; I: 124), (S: 'D'#39; I: 132),
    (S: 'DAA'; I: 15), (S: 'DATA'; I: 16),
    (S: 'DB'; I: 20), (S: 'DD'; I: 18),
    (S: 'DE'; I: 114), (S: 'DE'#39; I: 118),
    (S: 'DEC'; I: 19), (S: 'DEF'; I: 171),
    (S: 'DEFB'; I: 20), (S: 'DEFINE'; I: 150),
    (S: 'DEFM'; I: 21), (S: 'DEFS'; I: 22),
    (S: 'DEFSW'; I: 173),
    (S: 'DEFW'; I: 23), (S: 'DI'; I: 24),
    (S: 'DISABLE'; I: 25), (S: 'DJNZ'; I: 26),
    (S: 'DS'; I: 27), (S: 'DW'; I: 23),
    (S: 'E'; I: 125), (S: 'E'#39; I: 133),
    (S: 'EI'; I: 29), (S: 'ELIF'; I: 165),
    (S: 'ELSE'; I: 30), (S: 'ENABLE'; I: 31),
    (S: 'END'; I: 32), (S: 'ENDIF'; I: 33),
    (S: 'ENDM'; I: 169),
    (S: 'ENDR'; I: 176), (S: 'ENDREP'; I: 176), (S: 'ENDREPEAT'; I: 176),
    (S: 'ENDSTRUC'; I: 174),
    (S: 'ENDSTRUCT'; I: 174), (S: 'EQU'; I: 34),
    (S: 'EX'; I: 35), (S: 'EXX'; I: 36),
    (S: 'F'; I: 121), (S: 'F'#39; I: 129),
    (S: 'FCB'; I: 37), (S: 'FCC'; I: 38),
    (S: 'FCW'; I: 39), (S: 'FDB'; I: 40),
    (S: 'H'; I: 126), (S: 'H'#39; I: 134),
    (S: 'HALT'; I: 41), (S: 'HL'; I: 115),
    (S: 'HL'#39; I: 119), (S: 'I'; I: 136),
    (S: 'IF'; I: 42), (S: 'IFDEF'; I: 163),
    (S: 'IFEQ'; I: 166), (S: 'IFNDEF'; I: 164),
    (S: 'IFNE'; I: 167), (S: 'IM'; I: 43),
    (S: 'IN'; I: 44), (S: 'INC'; I: 45),
    (S: 'INCBIN'; I: 149), (S: 'INCLUDE'; I: 46),
    (S: 'IND'; I: 47), (S: 'INDR'; I: 48),
    (S: 'INI'; I: 49), (S: 'INIR'; I: 50),
    (S: 'IX'; I: 108), (S: 'IXH'; I: 153),
    (S: 'IXL'; I: 154), (S: 'IY'; I: 109),
    (S: 'IYH'; I: 155), (S: 'IYL'; I: 156),
    (S: 'JP'; I: 51), (S: 'JR'; I: 52),
    (S: 'L'; I: 127), (S: 'L'#39; I: 135),
    (S: 'LD'; I: 53), (S: 'LDD'; I: 54),
    (S: 'LDDR'; I: 55), (S: 'LDI'; I: 56),
    (S: 'LDIR'; I: 57), (S: 'LIST'; I: 58),
    (S: 'LOAD'; I: 177),
    (S: 'M'; I: 142), (S: 'MACRO'; I: 168),
    (S: 'NC'; I: 146), (S: 'NDEF'; I: 172),
    (S: 'NEG'; I: 59), (S: 'NOLIST'; I: 60),
    (S: 'NOOPT'; I: 61), (S: 'NOP'; I: 62),
    (S: 'NZ'; I: 144), (S: 'OFFSET'; I: 183), (S: 'OPT'; I: 63),
    (S: 'OR'; I: 64), (S: 'ORG'; I: 65),
    (S: 'OTDR'; I: 66), (S: 'OTIR'; I: 67),
    (S: 'OUT'; I: 68), (S: 'OUTD'; I: 69),
    (S: 'OUTI'; I: 70), (S: 'P'; I: 143),
    (S: 'PAGE'; I: 71), (S: 'PC'; I: 111),
    (S: 'PE'; I: 148), (S: 'PO'; I: 147),
    (S: 'POP'; I: 72), (S: 'PUSH'; I: 73),
    (S: 'R'; I: 137),
    (S: 'REP'; I: 175), (S: 'REPEAT'; I: 175), (S: 'REPT'; I: 175),
    (S: 'RES'; I: 74),
    (S: 'RESB'; I: 22), (S: 'RESW'; I: 173),
    (S: 'RET'; I: 75), (S: 'RETI'; I: 76),
    (S: 'RETN'; I: 77), (S: 'RIM'; I: 78),
    (S: 'RL'; I: 79), (S: 'RLA'; I: 80),
    (S: 'RLC'; I: 81), (S: 'RLCA'; I: 82),
    (S: 'RLD'; I: 83), (S: 'RMB'; I: 84),
    (S: 'RR'; I: 85), (S: 'RRA'; I: 86),
    (S: 'RRC'; I: 87), (S: 'RRCA'; I: 88),
    (S: 'RRD'; I: 89), (S: 'RST'; I: 90),
    (S: 'SBC'; I: 91), (S: 'SCF'; I: 92),
    (S: 'SET'; I: 93),
    (S: 'SHL'; I: 94), (S: 'SHR'; I: 95),
    (S: 'SIM'; I: 96), (S: 'SL'; I: 97),
    (S: 'SLA'; I: 98), (S: 'SLL'; I: 152),
    (S: 'SP'; I: 110), (S: 'SR'; I: 99),
    (S: 'SRA'; I: 100), (S: 'SRL'; I: 101),
    (S: 'STC'; I: 102), (S: 'STRUCT'; I: 103),
    (S: 'SUB'; I: 104), (S: 'TITLE'; I: 105),
    (S: 'TSTI'; I: 106), (S: 'XH'; I: 159),
    (S: 'XL'; I: 160), (S: 'XOR'; I: 107),
    (S: 'YH'; I: 161), (S: 'YL'; I: 162),
    (S: 'Z'; I: 145));

// Opcode list:
// Opcodes  0-255:      Normal Instructions ie. no prefix
//          256-511:    CB instructions
//          512-767:    DD instructions
//          768-1023:   DD CB instructions
//          1024-1279:  ED instructions
//          1280-1535:  FD instructions
//          1536-1791:  FD CB instructions
	Opcodes: Array [0..1791] of String = (
    // Opcodes  0-255: Normal Instructions ie. no prefix
		'NOP', '@LD BC, $NNNN', 'LD (BC), A', 'INC BC',
		'INC B', 'DEC B', '!LD B, $NN', 'RLCA',
		'EX AF, AF'#39, 'ADD HL, BC', 'LD A, (BC)', 'DEC BC',
		'INC C', 'DEC C', '!LD C, $NN', 'RRCA',
		'#DJNZ $$$+e', '@LD DE, $NNNN', 'LD (DE), A', 'INC DE',
		'INC D', 'DEC D', '!LD D, $NN', 'RLA',
		'#JR $$$+2', 'ADD HL, DE', 'LD A, (DE)', 'DEC DE',
		'INC E', 'DEC E', '!LD E, $NN', 'RRA',
		'#JR NZ, $$$+2', '@LD HL, $NNNN', '%LD ($NNNN), HL', 'INC HL',
		'INC H', 'DEC H', '!LD H, $NN', 'DAA',
		'#JR Z, $$$+2', 'ADD HL, HL', '%LD HL, ($NNNN)', 'DEC HL',
		'INC L', 'DEC L', '!LD L, $NN', 'CPL',
		'#JR NC, $$$+2', '@LD SP, $NNNN', '&LD ($NNNN), A', 'INC SP',
		'INC (HL)', 'DEC (HL)', '!LD (HL), $NN', 'SCF',
		'#JR C, $$$+2', 'ADD HL, SP', '&LD A, ($NNNN)', 'DEC SP',
		'INC A', 'DEC A', '!LD A, $NN', 'CCF',
		'LD B, B', 'LD B, C', 'LD B, D', 'LD B, E',
		'LD B, H', 'LD B, L', 'LD B, (HL)', 'LD B, A',
		'LD C, B', 'LD C, C', 'LD C, D', 'LD C, E',
		'LD C, H', 'LD C, L', 'LD C, (HL)', 'LD C, A',
		'LD D, B', 'LD D, C', 'LD D, D', 'LD D, E',
		'LD D, H', 'LD D, L', 'LD D, (HL)', 'LD D, A',
		'LD E, B', 'LD E, C', 'LD E, D', 'LD E, E',
		'LD E, H', 'LD E, L', 'LD E, (HL)', 'LD E, A',
		'LD H, B', 'LD H, C', 'LD H, D', 'LD H, E',
		'LD H, H', 'LD H, L', 'LD H, (HL)', 'LD H, A',
		'LD L, B', 'LD L, C', 'LD L, D', 'LD L, E',
		'LD L, H', 'LD L, L', 'LD L, (HL)', 'LD L, A',
		'LD (HL), B', 'LD (HL), C', 'LD (HL), D', 'LD (HL), E',
		'LD (HL), H', 'LD (HL), L', 'HALT', 'LD (HL), A',
		'LD A, B', 'LD A, C', 'LD A, D', 'LD A, E',
		'LD A, H', 'LD A, L', 'LD A, (HL)', 'LD A, A',
		'ADD A, B', 'ADD A, C', 'ADD A, D', 'ADD A, E',
		'ADD A, H', 'ADD A, L', 'ADD A, (HL)', 'ADD A, A',
		'ADC A, B', 'ADC A, C', 'ADC A, D', 'ADC A, E',
		'ADC A, H', 'ADC A, L', 'ADC A, (HL)', 'ADC A, A',
		'SUB B', 'SUB C', 'SUB D', 'SUB E',
		'SUB H', 'SUB L', 'SUB (HL)', 'SUB A',
		'SBC A, B', 'SBC A, C', 'SBC A, D', 'SBC A, E',
		'SBC A, H', 'SBC A, L', 'SBC A, (HL)', 'SBC A, A',
		'AND B', 'AND C', 'AND D', 'AND E',
		'AND H', 'AND L', 'AND (HL)', 'AND A',
		'XOR B', 'XOR C', 'XOR D', 'XOR E',
		'XOR H', 'XOR L', 'XOR (HL)', 'XOR A',
		'OR B', 'OR C', 'OR D', 'OR E',
		'OR H', 'OR L', 'OR (HL)', 'OR A',
		'CP B', 'CP C', 'CP D', 'CP E',
		'CP H', 'CP L', 'CP (HL)', 'CP A',
		'RET NZ', 'POP BC', '=JP NZ, $$$+3', '=JP $$$+3',
		'@CALL NZ, $NNNN', 'PUSH BC', '!ADD A, $NN', '.RST $NN',
		'RET Z', 'RET', '=JP Z, $$$+3', '',
		'@CALL Z, $NNNN', '@CALL $NNNN', '!ADC A, $NN', '.RST $NN',
		'RET NC', 'POP DE', '=JP NC, $$$+3', '>OUT ($NN), A',
		'@CALL NC, $NNNN', 'PUSH DE', '<SUB $NN', '.RST $NN',
		'RET C', 'EXX', '=JP C, $$$+3', '!IN A, ($NN)',
		'@CALL C, $NNNN', '', '!SBC A, $NN', '.RST $NN',
		'RET PO', 'POP HL', '=JP PO, $$$+3', 'EX (SP), HL',
		'@CALL PO, $NNNN', 'PUSH HL', '<AND $NN', '.RST $NN',
		'RET PE', 'JP (HL)', '=JP PE, $$$+3', 'EX DE, HL',
		'@CALL PE, $NNNN', '', '<XOR $NN', '.RST $NN',
		'RET P', 'POP AF', '=JP P, $$$+3', 'DI',
		'@CALL P, $NNNN', 'PUSH AF', '<OR $NN', '.RST $NN',
		'RET M', 'LD SP, HL', '=JP M, $$$+3', 'EI',
		'@CALL M, $NNNN', '', '<CP $NN', '.RST $NN',

    // Opcodes 256-511: CB instructions
		'RLC B', 'RLC C', 'RLC D', 'RLC E',
		'RLC H', 'RLC L', 'RLC (HL)', 'RLC A',
		'RRC B', 'RRC C', 'RRC D', 'RRC E',
		'RRC H', 'RRC L', 'RRC (HL)', 'RRC A',
		'RL B', 'RL C', 'RL D', 'RL E',
		'RL H', 'RL L', 'RL (HL)', 'RL A',
		'RR B', 'RR C', 'RR D', 'RR E',
		'RR H', 'RR L', 'RR (HL)', 'RR A',
		'SLA B', 'SLA C', 'SLA D', 'SLA E',
		'SLA H', 'SLA L', 'SLA (HL)', 'SLA A',
		'SRA B', 'SRA C', 'SRA D', 'SRA E',
		'SRA H', 'SRA L', 'SRA (HL)', 'SRA A',
		'SLL B', 'SLL C', 'SLL D', 'SLL E',
		'SLL H', 'SLL L', 'SLL (HL)', 'SLL A',
		'SRL B', 'SRL C', 'SRL D', 'SRL E',
		'SRL H', 'SRL L', 'SRL (HL)', 'SRL A',
		'BIT 0, B', 'BIT 0, C', 'BIT 0, D', 'BIT 0, E',
		'BIT 0, H', 'BIT 0, L', 'BIT 0, (HL)', 'BIT 0, A',
		'BIT 1, B', 'BIT 1, C', 'BIT 1, D', 'BIT 1, E',
		'BIT 1, H', 'BIT 1, L', 'BIT 1, (HL)', 'BIT 1, A',
		'BIT 2, B', 'BIT 2, C', 'BIT 2, D', 'BIT 2, E',
		'BIT 2, H', 'BIT 2, L', 'BIT 2, (HL)', 'BIT 2, A',
		'BIT 3, B', 'BIT 3, C', 'BIT 3, D', 'BIT 3, E',
		'BIT 3, H', 'BIT 3, L', 'BIT 3, (HL)', 'BIT 3, A',
		'BIT 4, B', 'BIT 4, C', 'BIT 4, D', 'BIT 4, E',
		'BIT 4, H', 'BIT 4, L', 'BIT 4, (HL)', 'BIT 4, A',
		'BIT 5, B', 'BIT 5, C', 'BIT 5, D', 'BIT 5, E',
		'BIT 5, H', 'BIT 5, L', 'BIT 5, (HL)', 'BIT 5, A',
		'BIT 6, B', 'BIT 6, C', 'BIT 6, D', 'BIT 6, E',
		'BIT 6, H', 'BIT 6, L', 'BIT 6, (HL)', 'BIT 6, A',
		'BIT 7, B', 'BIT 7, C', 'BIT 7, D', 'BIT 7, E',
		'BIT 7, H', 'BIT 7, L', 'BIT 7, (HL)', 'BIT 7, A',
		'RES 0, B', 'RES 0, C', 'RES 0, D', 'RES 0, E',
		'RES 0, H', 'RES 0, L', 'RES 0, (HL)', 'RES 0, A',
		'RES 1, B', 'RES 1, C', 'RES 1, D', 'RES 1, E',
		'RES 1, H', 'RES 1, L', 'RES 1, (HL)', 'RES 1, A',
		'RES 2, B', 'RES 2, C', 'RES 2, D', 'RES 2, E',
		'RES 2, H', 'RES 2, L', 'RES 2, (HL)', 'RES 2, A',
		'RES 3, B', 'RES 3, C', 'RES 3, D', 'RES 3, E',
		'RES 3, H', 'RES 3, L', 'RES 3, (HL)', 'RES 3, A',
		'RES 4, B', 'RES 4, C', 'RES 4, D', 'RES 4, E',
		'RES 4, H', 'RES 4, L', 'RES 4, (HL)', 'RES 4, A',
		'RES 5, B', 'RES 5, C', 'RES 5, D', 'RES 5, E',
		'RES 5, H', 'RES 5, L', 'RES 5, (HL)', 'RES 5, A',
		'RES 6, B', 'RES 6, C', 'RES 6, D', 'RES 6, E',
		'RES 6, H', 'RES 6, L', 'RES 6, (HL)', 'RES 6, A',
		'RES 7, B', 'RES 7, C', 'RES 7, D', 'RES 7, E',
		'RES 7, H', 'RES 7, L', 'RES 7, (HL)', 'RES 7, A',
		'SET 0, B', 'SET 0, C', 'SET 0, D', 'SET 0, E',
		'SET 0, H', 'SET 0, L', 'SET 0, (HL)', 'SET 0, A',
		'SET 1, B', 'SET 1, C', 'SET 1, D', 'SET 1, E',
		'SET 1, H', 'SET 1, L', 'SET 1, (HL)', 'SET 1, A',
		'SET 2, B', 'SET 2, C', 'SET 2, D', 'SET 2, E',
		'SET 2, H', 'SET 2, L', 'SET 2, (HL)', 'SET 2, A',
		'SET 3, B', 'SET 3, C', 'SET 3, D', 'SET 3, E',
		'SET 3, H', 'SET 3, L', 'SET 3, (HL)', 'SET 3, A',
		'SET 4, B', 'SET 4, C', 'SET 4, D', 'SET 4, E',
		'SET 4, H', 'SET 4, L', 'SET 4, (HL)', 'SET 4, A',
		'SET 5, B', 'SET 5, C', 'SET 5, D', 'SET 5, E',
		'SET 5, H', 'SET 5, L', 'SET 5, (HL)', 'SET 5, A',
		'SET 6, B', 'SET 6, C', 'SET 6, D', 'SET 6, E',
		'SET 6, H', 'SET 6, L', 'SET 6, (HL)', 'SET 6, A',
		'SET 7, B', 'SET 7, C', 'SET 7, D', 'SET 7, E',
		'SET 7, H', 'SET 7, L', 'SET 7, (HL)', 'SET 7, A',

    // Opcodes 512-767: DD instructions
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IX, BC', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IX, DE', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', '@LD IX, $NNNN', '%LD ($NNNN), IX', 'INC IX',
		'INC IXH', 'DEC IXH', '!LD IXH, $NN', 'NOP',
		'NOP', 'ADD IX, IX', '%LD IX, ($NNNN)', 'DEC IX',
		'INC IXL', 'DEC IXL', '!LD IXL, $NN', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'?INC (IX+$DD)', '?DEC (IX+$DD)', '/LD (IX+$DD), $NN', 'NOP',
		'NOP', 'ADD IX, SP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD B, IXH', 'LD B, IXL', '?LD B, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD C, IXH', 'LD C, IXL', '?LD C, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD D, IXH', 'LD D, IXL', '?LD D, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD E, IXH', 'LD E, IXL', '?LD E, (IX+$DD)', 'NOP',
		'LD IXH, B', 'LD IXH, C', 'LD IXH, D', 'LD IXH, E',
		'LD IXH, IXH', 'LD IXH, IXL', '?LD H, (IX+$DD)', 'LD IXH, A',
		'LD IXL, B', 'LD IXL, C', 'LD IXL, D', 'LD IXL, E',
		'LD IXL, IXH', 'LD IXL, IXL', '?LD L, (IX+$DD)', 'LD IXL, A',
		'?LD (IX+$DD), B', '?LD (IX+$DD), C', '?LD (IX+$DD), D', '?LD (IX+$DD), E',
		'?LD (IX+$DD), H', '?LD (IX+$DD), L', 'NOP', '?LD (IX+$DD), A',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD A, IXH', 'LD A, IXL', '?LD A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADD A, IXH', 'ADD A, IXL', '?ADD A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADC A, IXH', 'ADC A, IXL', '?ADC A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SUB IXH', 'SUB IXL', '?SUB (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SBC A, IXH', 'SBC A, IXL', '?SBC A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'AND IXH', 'AND IXL', '?AND (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'XOR IXH', 'XOR IXL', '?XOR (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'OR IXH', 'OR IXL', '?OR (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'CP IXH', 'CP IXL', '?CP (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', '',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'POP IX', 'NOP', 'EX (SP), IX',
		'NOP', 'PUSH IX', 'NOP', 'NOP',
		'NOP', 'JP (IX)', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'LD SP, IX', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',

    // Opcodes 768-1023: DD CB instructions
		'?LD B, RLC (IX+$DD)', '?LD C, RLC (IX+$DD)', '?LD D, RLC (IX+$DD)', '?LD E, RLC (IX+$DD)',
		'?LD H, RLC (IX+$DD)', '?LD L, RLC (IX+$DD)', '?RLC (IX+$DD)', '?LD A, RLC (IX+$DD)',
		'?LD B, RRC (IX+$DD)', '?LD C, RRC (IX+$DD)', '?LD D, RRC (IX+$DD)', '?LD E, RRC (IX+$DD)',
		'?LD H, RRC (IX+$DD)', '?LD L, RRC (IX+$DD)', '?RRC (IX+$DD)', '?LD A, RRC (IX+$DD)',
		'?LD B, RL (IX+$DD)', '?LD C, RL (IX+$DD)', '?LD D, RL (IX+$DD)', '?LD E, RL (IX+$DD)',
		'?LD H, RL (IX+$DD)', '?LD L, RL (IX+$DD)', '?RL (IX+$DD)', '?LD A, RL (IX+$DD)',
		'?LD B, RR (IX+$DD)', '?LD C, RR (IX+$DD)', '?LD D, RR (IX+$DD)', '?LD E, RR (IX+$DD)',
		'?LD H, RR (IX+$DD)', '?LD L, RR (IX+$DD)', '?RR (IX+$DD)', '?LD A, RR (IX+$DD)',
		'?LD B, SLA (IX+$DD)', '?LD C, SLA (IX+$DD)', '?LD D, SLA (IX+$DD)', '?LD E, SLA (IX+$DD)',
		'?LD H, SLA (IX+$DD)', '?LD L, SLA (IX+$DD)', '?SLA (IX+$DD)', '?LD A, SLA (IX+$DD)',
		'?LD B, SRA (IX+$DD)', '?LD C, SRA (IX+$DD)', '?LD D, SRA (IX+$DD)', '?LD E, SRA (IX+$DD)',
		'?LD H, SRA (IX+$DD)', '?LD L, SRA (IX+$DD)', '?SRA (IX+$DD)', '?LD A, SRA (IX+$DD)',
		'?LD B, SLL (IX+$DD)', '?LD C, SLL (IX+$DD)', '?LD D, SLL (IX+$DD)', '?LD E, SLL (IX+$DD)',
		'?LD H, SLL (IX+$DD)', '?LD L, SLL (IX+$DD)', '?SLL (IX+$DD)', '?LD A, SLL (IX+$DD)',
		'?LD B, SRL (IX+$DD)', '?LD C, SRL (IX+$DD)', '?LD D, SRL (IX+$DD)', '?LD E, SRL (IX+$DD)',
		'?LD H, SRL (IX+$DD)', '?LD L, SRL (IX+$DD)', '?SRL (IX+$DD)', '?LD A, SRL (IX+$DD)',
		'?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)',
		'?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)',
		'?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)',
		'?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)',
		'?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)',
		'?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)',
		'?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)',
		'?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)',
		'?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)',
		'?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)',
		'?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)',
		'?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)',
		'?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)',
		'?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)',
		'?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)',
		'?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)',
		'?LD B, RES 0 (IX+$DD)', '?LD C, RES 0 (IX+$DD)', '?LD D, RES 0 (IX+$DD)', '?LD E, RES 0 (IX+$DD)',
		'?LD H, RES 0 (IX+$DD)', '?LD L, RES 0 (IX+$DD)', '?RES 0, (IX+$DD)', '?LD A, RES 0 (IX+$DD)',
		'?LD B, RES 1 (IX+$DD)', '?LD C, RES 1 (IX+$DD)', '?LD D, RES 1 (IX+$DD)', '?LD E, RES 1 (IX+$DD)',
		'?LD H, RES 1 (IX+$DD)', '?LD L, RES 1 (IX+$DD)', '?RES 1, (IX+$DD)', '?LD A, RES 1 (IX+$DD)',
		'?LD B, RES 2 (IX+$DD)', '?LD C, RES 2 (IX+$DD)', '?LD D, RES 2 (IX+$DD)', '?LD E, RES 2 (IX+$DD)',
		'?LD H, RES 2 (IX+$DD)', '?LD L, RES 2 (IX+$DD)', '?RES 2, (IX+$DD)', '?LD A, RES 2 (IX+$DD)',
		'?LD B, RES 3 (IX+$DD)', '?LD C, RES 3 (IX+$DD)', '?LD D, RES 3 (IX+$DD)', '?LD E, RES 3 (IX+$DD)',
		'?LD H, RES 3, (IX+$DD)', '?LD L, RES 3 (IX+$DD)', '?RES 3, (IX+$DD)', '?LD A, RES 3 (IX+$DD)',
		'?LD B, RES 4 (IX+$DD)', '?LD C, RES 4 (IX+$DD)', '?LD D, RES 4 (IX+$DD)', '?LD E, RES 4 (IX+$DD)',
		'?LD H, RES 4 (IX+$DD)', '?LD L, RES 4 (IX+$DD)', '?RES 4, (IX+$DD)', '?LD A, RES 4 (IX+$DD)',
		'?LD B, RES 5 (IX+$DD)', '?LD C, RES 5 (IX+$DD)', '?LD D, RES 5 (IX+$DD)', '?LD E, RES 5 (IX+$DD)',
		'?LD H, RES 5 (IX+$DD)', '?LD L, RES 5 (IX+$DD)', '?RES 5, (IX+$DD)', '?LD A, RES 5 (IX+$DD)',
		'?LD B, RES 6 (IX+$DD)', '?LD C, RES 6 (IX+$DD)', '?LD D, RES 6 (IX+$DD)', '?LD E, RES 6 (IX+$DD)',
		'?LD H, RES 6 (IX+$DD)', '?LD L, RES 6 (IX+$DD)', '?RES 6, (IX+$DD)', '?LD A, RES 6 (IX+$DD)',
		'?LD B, RES 7, (IX+$DD)', '?LD C, RES 7 (IX+$DD)', '?LD D, RES 7 (IX+$DD)', '?LD E, RES 7 (IX+$DD)',
		'?LD H, RES 7 (IX+$DD)', '?LD L, RES 7 (IX+$DD)', '?RES 7, (IX+$DD)', '?LD A, RES 7 (IX+$DD)',
		'?LD B, SET 0 (IX+$DD)', '?LD C, SET 0 (IX+$DD)', '?LD D, SET 0 (IX+$DD)', '?LD E, SET 0 (IX+$DD)',
		'?LD H, SET 0 (IX+$DD)', '?LD L, SET 0 (IX+$DD)', '?SET 0, (IX+$DD)', '?LD A, SET 0 (IX+$DD)',
		'?LD B, SET 1 (IX+$DD)', '?LD C, SET 1 (IX+$DD)', '?LD D, SET 1 (IX+$DD)', '?LD E, SET 1 (IX+$DD)',
		'?LD H, SET 1 (IX+$DD)', '?LD L, SET 1 (IX+$DD)', '?SET 1, (IX+$DD)', '?LD A, SET 1 (IX+$DD)',
		'?LD B, SET 2, (IX+$DD)', '?LD C, SET 2 (IX+$DD)', '?LD D, SET 2 (IX+$DD)', '?LD E, SET 2 (IX+$DD)',
		'?LD H, SET 2 (IX+$DD)', '?LD L, SET 2 (IX+$DD)', '?SET 2, (IX+$DD)', '?LD A, SET 2 (IX+$DD)',
		'?LD B, SET 3 (IX+$DD)', '?LD C, SET 3 (IX+$DD)', '?LD D, SET 3 (IX+$DD)', '?LD E, SET 3 (IX+$DD)',
		'?LD H, SET 3 (IX+$DD)', '?LD L, SET 3 (IX+$DD)', '?SET 3, (IX+$DD)', '?LD A, SET 3 (IX+$DD)',
		'?LD B, SET 4 (IX+$DD)', '?LD C, SET 4 (IX+$DD)', '?LD D, SET 4 (IX+$DD)', '?LD E, SET 4 (IX+$DD)',
		'?LD H, SET 4 (IX+$DD)', '?LD L, SET 4 (IX+$DD)', '?SET 4, (IX+$DD)', '?LD A, SET 4 (IX+$DD)',
		'?LD B, SET 5, (IX+$DD)', '?LD C, SET 5 (IX+$DD)', '?LD D, SET 5 (IX+$DD)', '?LD E, SET 5 (IX+$DD)',
		'?LD H, SET 5 (IX+$DD)', '?LD L, SET 5 (IX+$DD)', '?SET 5, (IX+$DD)', '?LD A, SET 5 (IX+$DD)',
		'?LD B, SET 6 (IX+$DD)', '?LD C, SET 6 (IX+$DD)', '?LD D, SET 6 (IX+$DD)', '?LD E, SET 6 (IX+$DD)',
		'?LD H, SET 6 (IX+$DD)', '?LD L, SET 6 (IX+$DD)', '?SET 6, (IX+$DD)', '?LD A, SET 6 (IX+$DD)',
		'?LD B, SET 7 (IX+$DD)', '?LD C, SET 7 (IX+$DD)', '?LD D, SET 7 (IX+$DD)', '?LD E, SET 7 (IX+$DD)',
		'?LD H, SET 7 (IX+$DD)', '?LD L, SET 7 (IX+$DD)', '?SET 7, (IX+$DD)', '?LD A, SET 7 (IX+$DD)',

    // Opcodes 1024-1279: ED instructions
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'IN B, (C)', 'OUT (C), B', 'SBC HL, BC', '%LD ($NNNN), BC',
		'NEG', 'RETN', 'IM 0', 'LD I, A',
		'IN C, (C)', 'OUT (C), C', 'ADC HL, BC', '%LD BC, ($NNNN)',
		'NEG', 'RETI', 'IM 0', 'LD R, A',
		'IN D, (C)', 'OUT (C), D', 'SBC HL, DE', '%LD ($NNNN), DE',
		'NEG', 'RETN', 'IM 1', 'LD A, I',
		'IN E, (C)', 'OUT (C), E', 'ADC HL, DE', '%LD DE, ($NNNN)',
		'NEG', 'RETN', 'IM 2', 'LD A, R',
		'IN H, (C)', 'OUT (C), B', 'SBC HL, HL', '%LD ($NNNN), HL',
		'NEG', 'RETN', 'IM 0', 'RRD',
		'IN L, (C)', 'OUT (C), L', 'ADC HL, HL', '%LD HL, ($NNNN)',
		'NEG', 'RETN', 'IM 0/1', 'RLD',
		'IN F, (C)', 'OUT (C), 0', 'SBC HL, SP', '%LD ($NNNN), SP',
		'NEG', 'RETN', 'IM 1', 'NOP',
		'IN A, (C)', 'OUT (C), A', 'ADC HL, SP', '%LD SP, ($NNNN)',
		'NEG', 'RETN', 'IM 2', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDI', 'CPI', 'INI', 'OUTI',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDD', 'CPD', 'IND', 'OUTD',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDIR', 'CPIR', 'INIR', 'OTIR',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDDR', 'CPDR', 'INDR', 'OTDR',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',

    // Opcodes 1280-1535: FD instructions
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IY, BC', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IY, DE', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', '@LD IY, $NNNN', '%LD ($NNNN), IY', 'INC IY',
		'INC IYH', 'DEC IYH', '!LD IYH, $NN', 'NOP',
		'NOP', 'ADD IY, IY', '%LD IY, ($NNNN)', 'DEC IY',
		'INC IYL', 'DEC IYL', '!LD IYL, $NN', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'?INC (IY+$DD)', '?DEC (IY+$DD)', '/LD (IY+$DD), $NN', 'NOP',
		'NOP', 'ADD IY, SP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD B, IYH', 'LD B, IYL', '?LD B, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD C, IYH', 'LD C, IYL', '?LD C, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD D, IYH', 'LD D, IYL', '?LD D, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD E, IYH', 'LD E, IYL', '?LD E, (IY+$DD)', 'NOP',
		'LD IYH, B', 'LD IYH, C', 'LD IYH, D', 'LD IYH, E',
		'LD IYH, IYH', 'LD IYH, IYL', '?LD H, (IY+$DD)', 'LD IYH, A',
		'LD IYL, B', 'LD IYL, C', 'LD IYL, D', 'LD IYL, E',
		'LD IYL, IYH', 'LD IYL, IYL', '?LD L, (IY+$DD)', 'LD IYL, A',
		'?LD (IY+$DD), B', '?LD (IY+$DD), C', '?LD (IY+$DD), D', '?LD (IY+$DD), E',
		'?LD (IY+$DD), H', '?LD (IY+$DD), L', 'NOP', '?LD (IY+$DD), A',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD A, IYH', 'LD A, IYL', '?LD A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADD A, IYH', 'ADD A, IYL', '?ADD A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADC A, IYH', 'ADC A, IYL', '?ADC A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SUB IYH', 'SUB IYL', '?SUB (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SBC A, IYH', 'SBC A, IYL', '?SBC A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'AND IYH', 'AND IYL', '?AND (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'XOR IYH', 'XOR IYL', '?XOR (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'OR IYH', 'OR IYL', '?OR (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'CP IYH', 'CP IYL', '?CP (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', '',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'POP IY', 'NOP', 'EX (SP), IY',
		'NOP', 'PUSH IY', 'NOP', 'NOP',
		'NOP', 'JP (IY)', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'LD SP, IY', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',

    // Opcodes 1536-1791: FD CB instructions
		'?LD B, RLC (IY+$DD)', '?LD C, RLC (IY+$DD)', '?LD D, RLC (IY+$DD)', '?LD E, RLC (IY+$DD)',
		'?LD H, RLC (IY+$DD)', '?LD L, RLC (IY+$DD)', '?RLC (IY+$DD)', '?LD A, RLC (IY+$DD)',
		'?LD B, RRC (IY+$DD)', '?LD C, RRC (IY+$DD)', '?LD D, RRC (IY+$DD)', '?LD E, RRC (IY+$DD)',
		'?LD H, RRC (IY+$DD)', '?LD L, RRC (IY+$DD)', '?RRC (IY+$DD)', '?LD A, RRC (IY+$DD)',
		'?LD B, RL (IY+$DD)', '?LD C, RL (IY+$DD)', '?LD D, RL (IY+$DD)', '?LD E, RL (IY+$DD)',
		'?LD H, RL (IY+$DD)', '?LD L, RL (IY+$DD)', '?RL (IY+$DD)', '?LD A, RL (IY+$DD)',
		'?LD B, RR (IY+$DD)', '?LD C, RR (IY+$DD)', '?LD D, RR (IY+$DD)', '?LD E, RR (IY+$DD)',
		'?LD H, RR (IY+$DD)', '?LD L, RR (IY+$DD)', '?RR (IY+$DD)', '?LD A, RR (IY+$DD)',
		'?LD B, SLA (IY+$DD)', '?LD C, SLA (IY+$DD)', '?LD D, SLA (IY+$DD)', '?LD E, SLA (IY+$DD)',
		'?LD H, SLA (IY+$DD)', '?LD L, SLA (IY+$DD)', '?SLA (IY+$DD)', '?LD A, SLA (IY+$DD)',
		'?LD B, SRA (IY+$DD)', '?LD C, SRA (IY+$DD)', '?LD D, SRA (IY+$DD)', '?LD E, SRA (IY+$DD)',
		'?LD H, SRA (IY+$DD)', '?LD L, SRA (IY+$DD)', '?SRA (IY+$DD)', '?LD A, SRA (IY+$DD)',
		'?LD B, SLL (IY+$DD)', '?LD C, SLL (IY+$DD)', '?LD D, SLL (IY+$DD)', '?LD E, SLL (IY+$DD)',
		'?LD H, SLL (IY+$DD)', '?LD L, SLL (IY+$DD)', '?SLL (IY+$DD)', '?LD A, SLL (IY+$DD)',
		'?LD B, SRL (IY+$DD)', '?LD C, SRL (IY+$DD)', '?LD D, SRL (IY+$DD)', '?LD E, SRL (IY+$DD)',
		'?LD H, SRL (IY+$DD)', '?LD L, SRL (IY+$DD)', '?SRL (IY+$DD)', '?LD A, SRL (IY+$DD)',
		'?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)',
		'?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)',
		'?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)',
		'?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)',
		'?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)',
		'?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)',
		'?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)',
		'?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)',
		'?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)',
		'?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)',
		'?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)',
		'?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)',
		'?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)',
		'?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)',
		'?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)',
		'?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)',
		'?LD B, RES 0 (IY+$DD)', '?LD C, RES 0 (IY+$DD)', '?LD D, RES 0 (IY+$DD)', '?LD E, RES 0 (IY+$DD)',
		'?LD H, RES 0 (IY+$DD)', '?LD L, RES 0 (IY+$DD)', '?RES 0, (IY+$DD)', '?LD A, RES 0 (IY+$DD)',
		'?LD B, RES 1 (IY+$DD)', '?LD C, RES 1 (IY+$DD)', '?LD D, RES 1 (IY+$DD)', '?LD E, RES 1 (IY+$DD)',
		'?LD H, RES 1 (IY+$DD)', '?LD L, RES 1 (IY+$DD)', '?RES 1, (IY+$DD)', '?LD A, RES 1 (IY+$DD)',
		'?LD B, RES 2 (IY+$DD)', '?LD C, RES 2 (IY+$DD)', '?LD D, RES 2 (IY+$DD)', '?LD E, RES 2 (IY+$DD)',
		'?LD H, RES 2 (IY+$DD)', '?LD L, RES 2 (IY+$DD)', '?RES 2, (IY+$DD)', '?LD A, RES 2 (IY+$DD)',
		'?LD B, RES 3 (IY+$DD)', '?LD C, RES 3 (IY+$DD)', '?LD D, RES 3 (IY+$DD)', '?LD E, RES 3 (IY+$DD)',
		'?LD H, RES 3, (IY+$DD)', '?LD L, RES 3 (IY+$DD)', '?RES 3, (IY+$DD)', '?LD A, RES 3 (IY+$DD)',
		'?LD B, RES 4 (IY+$DD)', '?LD C, RES 4 (IY+$DD)', '?LD D, RES 4 (IY+$DD)', '?LD E, RES 4 (IY+$DD)',
		'?LD H, RES 4 (IY+$DD)', '?LD L, RES 4 (IY+$DD)', '?RES 4, (IY+$DD)', '?LD A, RES 4 (IY+$DD)',
		'?LD B, RES 5 (IY+$DD)', '?LD C, RES 5 (IY+$DD)', '?LD D, RES 5 (IY+$DD)', '?LD E, RES 5 (IY+$DD)',
		'?LD H, RES 5 (IY+$DD)', '?LD L, RES 5 (IY+$DD)', '?RES 5, (IY+$DD)', '?LD A, RES 5 (IY+$DD)',
		'?LD B, RES 6 (IY+$DD)', '?LD C, RES 6 (IY+$DD)', '?LD D, RES 6 (IY+$DD)', '?LD E, RES 6 (IY+$DD)',
		'?LD H, RES 6 (IY+$DD)', '?LD L, RES 6 (IY+$DD)', '?RES 6, (IY+$DD)', '?LD A, RES 6 (IY+$DD)',
		'?LD B, RES 7, (IY+$DD)', '?LD C, RES 7 (IY+$DD)', '?LD D, RES 7 (IY+$DD)', '?LD E, RES 7 (IY+$DD)',
		'?LD H, RES 7 (IY+$DD)', '?LD L, RES 7 (IY+$DD)', '?RES 7, (IY+$DD)', '?LD A, RES 7 (IY+$DD)',
		'?LD B, SET 0 (IY+$DD)', '?LD C, SET 0 (IY+$DD)', '?LD D, SET 0 (IY+$DD)', '?LD E, SET 0 (IY+$DD)',
		'?LD H, SET 0 (IY+$DD)', '?LD L, SET 0 (IY+$DD)', '?SET 0, (IY+$DD)', '?LD A, SET 0 (IY+$DD)',
		'?LD B, SET 1 (IY+$DD)', '?LD C, SET 1 (IY+$DD)', '?LD D, SET 1 (IY+$DD)', '?LD E, SET 1 (IY+$DD)',
		'?LD H, SET 1 (IY+$DD)', '?LD L, SET 1 (IY+$DD)', '?SET 1, (IY+$DD)', '?LD A, SET 1 (IY+$DD)',
		'?LD B, SET 2, (IY+$DD)', '?LD C, SET 2 (IY+$DD)', '?LD D, SET 2 (IY+$DD)', '?LD E, SET 2 (IY+$DD)',
		'?LD H, SET 2 (IY+$DD)', '?LD L, SET 2 (IY+$DD)', '?SET 2, (IY+$DD)', '?LD A, SET 2 (IY+$DD)',
		'?LD B, SET 3 (IY+$DD)', '?LD C, SET 3 (IY+$DD)', '?LD D, SET 3 (IY+$DD)', '?LD E, SET 3 (IY+$DD)',
		'?LD H, SET 3 (IY+$DD)', '?LD L, SET 3 (IY+$DD)', '?SET 3, (IY+$DD)', '?LD A, SET 3 (IY+$DD)',
		'?LD B, SET 4 (IY+$DD)', '?LD C, SET 4 (IY+$DD)', '?LD D, SET 4 (IY+$DD)', '?LD E, SET 4 (IY+$DD)',
		'?LD H, SET 4 (IY+$DD)', '?LD L, SET 4 (IY+$DD)', '?SET 4, (IY+$DD)', '?LD A, SET 4 (IY+$DD)',
		'?LD B, SET 5, (IY+$DD)', '?LD C, SET 5 (IY+$DD)', '?LD D, SET 5 (IY+$DD)', '?LD E, SET 5 (IY+$DD)',
		'?LD H, SET 5 (IY+$DD)', '?LD L, SET 5 (IY+$DD)', '?SET 5, (IY+$DD)', '?LD A, SET 5 (IY+$DD)',
		'?LD B, SET 6 (IY+$DD)', '?LD C, SET 6 (IY+$DD)', '?LD D, SET 6 (IY+$DD)', '?LD E, SET 6 (IY+$DD)',
		'?LD H, SET 6 (IY+$DD)', '?LD L, SET 6 (IY+$DD)', '?SET 6, (IY+$DD)', '?LD A, SET 6 (IY+$DD)',
		'?LD B, SET 7 (IY+$DD)', '?LD C, SET 7 (IY+$DD)', '?LD D, SET 7 (IY+$DD)', '?LD E, SET 7 (IY+$DD)',
		'?LD H, SET 7 (IY+$DD)', '?LD L, SET 7 (IY+$DD)', '?SET 7, (IY+$DD)', '?LD A, SET 7 (IY+$DD)');

// the following is a list of index numbers of instructions, when sorted alphabetically
// (takes less space than storing the instruction table twice)
OpcodeScanTable: Array [0..1128] of Word = (
		$ce, $8e, $28e, $58e, $8f, $88, $89, $8a,
		$8b, $8c, $28c, $28d, $58c, $58d, $8d, $44a, 
		$45a, $46a, $47a, $c6, $86, $286, $586, $87, 
		$80, $81, $82, $83, $84, $284, $285, $584, 
		$585, $85, $9, $19, $29, $39, $209, $219, 
		$229, $239, $509, $519, $529, $539, $e6, $a6, 
		$2a6, $5a6, $a7, $a0, $a1, $a2, $a3, $a4, 
		$2a4, $2a5, $5a4, $5a5, $a5, $146, $346, $646, 
		$147, $140, $141, $142, $143, $144, $145, $14e,
		$34e, $64e, $14f, $148, $149, $14a, $14b, $14c, 
		$14d, $156, $356, $656, $157, $150, $151, $152, 
		$153, $154, $155, $15e, $35e, $65e, $15f, $158, 
		$159, $15a, $15b, $15c, $15d, $166, $366, $666, 
		$167, $160, $161, $162, $163, $164, $165, $16e, 
		$36e, $66e, $16f, $168, $169, $16a, $16b, $16c,
		$16d, $176, $376, $676, $177, $170, $171, $172,
		$173, $174, $175, $17e, $37e, $67e, $17f, $178,
		$179, $17a, $17b, $17c, $17d, $cd, $dc, $fc, 
		$d4, $c4, $f4, $ec, $e4, $cc, $3f, $fe, 
		$be, $2be, $5be, $bf, $b8, $b9, $ba, $bb, 
		$bc, $2bc, $2bd, $5bc, $5bd, $bd, $4a9, $4b9, 
		$4a1, $4b1, $2f, $27, $35, $235, $535, $3d, 
		$5, $b, $d, $15, $1b, $1d, $25, $2b,
		$22b, $225, $22d, $52b, $525, $52d, $2d, $3b,
		$f3, $10, $fb, $e3, $2e3, $5e3, $8, $eb, 
		$d9, $76, $44e, $456, $45e, $db, $478, $440, 
		$448, $450, $458, $470, $460, $468, $34, $234, 
		$534, $3c, $4, $3, $c, $14, $13, $1c,
		$24, $23, $223, $224, $22c, $523, $524, $52c, 
		$2c, $33, $4aa, $4ba, $4a2, $4b2, $c3, $e9, 
		$2e9, $5e9, $da, $fa, $d2, $c2, $f2, $ea,
		$e2, $ca, $18, $38, $30, $20, $28, $32, 
		$443, $453, $22, $222, $522, $473, $2, $12, 
		$36, $77, $70, $71, $72, $73, $74, $75, 
		$236, $277, $270, $271, $272, $273, $274, $275, 
		$536, $577, $570, $571, $572, $573, $574, $575, 
		$3e, $3a, $a, $1a, $7e, $27e, $57e, $7f, 
		$78, $79, $7a, $7b, $7c, $457, $27c, $27d, 
		$57c, $57d, $7d, $45f, $387, $687, $38f, $68f, 
		$397, $697, $39f, $69f, $3a7, $6a7, $3af, $6af, 
		$3b7, $6b7, $3bf, $6bf, $317, $617, $307, $607, 
		$31f, $61f, $30f, $60f, $3c7, $6c7, $3cf, $6cf, 
		$3d7, $6d7, $3df, $6df, $3e7, $6e7, $3ef, $6ef, 
		$3f7, $6f7, $3ff, $6ff, $327, $627, $337, $637, 
		$32f, $62f, $33f, $63f, $6, $46, $246, $546,
		$47, $40, $41, $42, $43, $44, $244, $245, 
		$544, $545, $45, $380, $680, $388, $688, $390, 
		$690, $398, $698, $3a0, $6a0, $3a8, $6a8, $3b0, 
		$6b0, $3b8, $6b8, $310, $610, $300, $600, $318, 
		$618, $308, $608, $3c0, $6c0, $3c8, $6c8, $3d0, 
		$6d0, $3d8, $6d8, $3e0, $6e0, $3e8, $6e8, $3f0, 
		$6f0, $3f8, $6f8, $320, $620, $330, $630, $328,
		$628, $338, $638, $1, $44b, $e, $4e, $24e,
		$54e, $4f, $48, $49, $4a, $4b, $4c, $24c,
		$24d, $54c, $54d, $4d, $381, $681, $389, $689, 
		$391, $691, $399, $699, $3a1, $6a1, $3a9, $6a9, 
		$3b1, $6b1, $3b9, $6b9, $311, $611, $301, $601, 
		$319, $619, $309, $609, $3c1, $6c1, $3c9, $6c9, 
		$3d1, $6d1, $3d9, $6d9, $3e1, $6e1, $3e9, $6e9, 
		$3f1, $6f1, $3f9, $6f9, $321, $621, $331, $631, 
		$329, $629, $339, $639, $16, $56, $256, $556,
		$57, $50, $51, $52, $53, $54, $254, $255, 
		$554, $555, $55, $382, $682, $38a, $68a, $392, 
		$692, $39a, $69a, $3a2, $6a2, $3aa, $6aa, $3b2, 
		$6b2, $3ba, $6ba, $312, $612, $302, $602, $31a, 
		$61a, $30a, $60a, $3c2, $6c2, $3ca, $6ca, $3d2, 
		$6d2, $3da, $6da, $3e2, $6e2, $3ea, $6ea, $3f2, 
		$6f2, $3fa, $6fa, $322, $622, $332, $632, $32a, 
		$62a, $33a, $63a, $11, $45b, $1e, $5e, $25e, 
		$55e, $5f, $58, $59, $5a, $5b, $5c, $25c,
		$25d, $55c, $55d, $5d, $383, $683, $38b, $68b, 
		$393, $693, $39b, $69b, $3a3, $6a3, $3ab, $6ab, 
		$3b3, $6b3, $3bb, $6bb, $313, $613, $303, $603, 
		$31b, $61b, $30b, $60b, $3c3, $6c3, $3cb, $6cb,
		$3d3, $6d3, $3db, $6db, $3e3, $6e3, $3eb, $6eb,
		$3f3, $6f3, $3fb, $6fb, $323, $623, $333, $633, 
		$32b, $62b, $33b, $63b, $26, $66, $266, $566, 
		$67, $60, $61, $62, $63, $64, $65, $384, 
		$684, $38c, $68c, $394, $694, $39c, $69c, $3a4, 
		$6a4, $3ac, $6ac, $3b4, $6b4, $3bc, $6bc, $314,
		$614, $304, $604, $31c, $61c, $30c, $60c, $3c4, 
		$6c4, $3cc, $6cc, $3d4, $6d4, $3dc, $6dc, $3e4, 
		$6e4, $3ec, $6ec, $3f4, $6f4, $3fc, $6fc, $324,
		$624, $334, $634, $32c, $62c, $33c, $63c, $21,
		$2a, $447, $221, $22a, $226, $267, $260, $261, 
		$262, $263, $264, $265, $22e, $26f, $268, $269, 
		$26a, $26b, $26c, $26d, $521, $52a, $526, $567, 
		$560, $561, $562, $563, $564, $565, $52e, $56f, 
		$568, $569, $56a, $56b, $56c, $56d, $2e, $6e, 
		$26e, $56e, $6f, $68, $69, $6a, $6b, $6c, 
		$6d, $385, $685, $38d, $68d, $395, $695, $39d,
		$69d, $3a5, $6a5, $3ad, $6ad, $3b5, $6b5, $3bd, 
		$6bd, $315, $615, $305, $605, $31d, $61d, $30d, 
		$60d, $3c5, $6c5, $3cd, $6cd, $3d5, $6d5, $3dd, 
		$6dd, $3e5, $6e5, $3ed, $6ed, $3f5, $6f5, $3fd, 
		$6fd, $325, $625, $335, $635, $32d, $62d, $33d, 
		$63d, $44f, $31, $47b, $f9, $2f9, $5f9, $4a8,
		$4b8, $4a0, $4b0, $444, $0, $f6, $b6, $2b6, 
		$5b6, $b7, $b0, $b1, $b2, $b3, $b4, $2b4, 
		$2b5, $5b4, $5b5, $b5, $4bb, $4b3, $d3, $471,
		$479, $441, $461, $449, $451, $459, $469, $4ab, 
		$4a3, $f1, $c1, $d1, $e1, $2e1, $5e1, $f5, 
		$c5, $d5, $e5, $2e5, $5e5, $186, $386, $686,
		$187, $180, $181, $182, $183, $184, $185, $18e,
		$38e, $68e, $18f, $188, $189, $18a, $18b, $18c,
		$18d, $196, $396, $696, $197, $190, $191, $192,
		$193, $194, $195, $19e, $39e, $69e, $19f, $198, 
		$199, $19a, $19b, $19c, $19d, $1a6, $3a6, $6a6,
		$1a7, $1a0, $1a1, $1a2, $1a3, $1a4, $1a5, $1ae, 
		$3ae, $6ae, $1af, $1a8, $1a9, $1aa, $1ab, $1ac, 
		$1ad, $1b6, $3b6, $6b6, $1b7, $1b0, $1b1, $1b2, 
		$1b3, $1b4, $1b5, $1be, $3be, $6be, $1bf, $1b8, 
		$1b9, $1ba, $1bb, $1bc, $1bd, $c9, $d8, $f8,
		$d0, $c0, $f0, $e8, $e0, $c8, $44d, $445, $116,
		$316, $616, $117, $110, $111, $112, $113, $114, 
		$115, $17, $106, $306, $606, $107, $100, $101, 
		$102, $103, $104, $105, $7, $46f, $11e, $31e,
		$61e, $11f, $118, $119, $11a, $11b, $11c, $11d, 
		$1f, $10e, $30e, $60e, $10f, $108, $109, $10a, 
		$10b, $10c, $10d, $f, $467, $c7, $de, $9e, 
		$29e, $59e, $9f, $98, $99, $9a, $9b, $9c,
		$29c, $29d, $59c, $59d, $9d, $442, $452, $462, 
		$472, $37, $1c6, $3c6, $6c6, $1c7, $1c0, $1c1, 
		$1c2, $1c3, $1c4, $1c5, $1ce, $3ce, $6ce, $1cf, 
		$1c8, $1c9, $1ca, $1cb, $1cc, $1cd, $1d6, $3d6, 
		$6d6, $1d7, $1d0, $1d1, $1d2, $1d3, $1d4, $1d5, 
		$1de, $3de, $6de, $1df, $1d8, $1d9, $1da, $1db, 
		$1dc, $1dd, $1e6, $3e6, $6e6, $1e7, $1e0, $1e1, 
		$1e2, $1e3, $1e4, $1e5, $1ee, $3ee, $6ee, $1ef, 
		$1e8, $1e9, $1ea, $1eb, $1ec, $1ed, $1f6, $3f6, 
		$6f6, $1f7, $1f0, $1f1, $1f2, $1f3, $1f4, $1f5,
		$1fe, $3fe, $6fe, $1ff, $1f8, $1f9, $1fa, $1fb,
		$1fc, $1fd, $126, $326, $626, $127, $120, $121, 
		$122, $123, $124, $125, $136, $336, $636, $137, 
		$130, $131, $132, $133, $134, $135, $12e, $32e,
		$62e, $12f, $128, $129, $12a, $12b, $12c, $12d,
		$13e, $33e, $63e, $13f, $138, $139, $13a, $13b, 
		$13c, $13d, $d6, $96, $296, $596, $97, $90, 
		$91, $92, $93, $94, $294, $295, $594, $595, 
		$95, $ee, $ae, $2ae, $5ae, $af, $a8, $a9, 
		$aa, $ab, $ac, $2ac, $2ad, $5ac, $5ad, $ad);

  OpcodeASCIICodes: Array[0..255] of word = (
    $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,
    $0008,$0020,$000A,$000B,$000C,$000D,$000E,$000F,
    $0010,$0011,$0012,$0013,$0014,$0015,$0016,$0017,
    $0018,$0019,$001A,$001B,$001C,$001D,$001E,$001F,
    $0020,$0021,$0022,$0023,$FF24,$FF25,$FF26,$0027,
    $F028,$F129,$002A,$F22B,$002C,$F32D,$FF2E,$002F,
    $FF30,$FF31,$FF32,$FF33,$FF34,$FF35,$FF36,$FF37,
    $FF38,$FF39,$003A,$003B,$003C,$003D,$003E,$003F,
    $0040,$0141,$0142,$0143,$0144,$0145,$0146,$0147,
    $0148,$0149,$014A,$014B,$014C,$014D,$014E,$014F,
    $0150,$0151,$0152,$0153,$0154,$0155,$0156,$0157,
    $0158,$0159,$015A,$005B,$005C,$005D,$005E,$015F,
    $0060,$0141,$0142,$0143,$0144,$0145,$0146,$0147,
    $0148,$0149,$014A,$014B,$014C,$014D,$014E,$014F,
    $0150,$0151,$0152,$0153,$0154,$0155,$0156,$0157,
    $0158,$0159,$015A,$007B,$007C,$007D,$007E,$007F,
    $0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
    $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
    $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
    $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
    $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
    $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
    $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
    $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
    $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
    $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
    $00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D6,$00D7,
    $00D8,$00D9,$00DA,$00DB,$00DC,$00DD,$00DE,$00DF,
    $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
    $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
    $00F0,$00F1,$00F2,$00F3,$00F4,$00F5,$00F6,$00F7,
    $00F8,$00F9,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF);

implementation

Constructor TZ80Assembler.Create;
var i: Integer;
begin
  inherited create;
  DEFSDefault := $FF;
  Errors := TStringList.Create;
  for i := 0 to 31 do AsmSymbols[i] := TStringList.Create;
  ResetMemBlocks;
  atStartOfPass := nil;
  afterLine := nil;
  SetLength(srcFiles, 0);
end;
Destructor TZ80Assembler.Destroy;
var i: Integer;
begin
  Errors.Free;
  SetLength(AsmSymbolsExt, 0);
  NumAsmSymbols := 0;
  for i := 0 to 31 do AsmSymbols[i].Free;
  if DefaultPage <> nil then DefaultPage.Free;
  inherited destroy;
end;
procedure TZ80Assembler.ResetMemBlocks;
begin
  DefaultPage := nil;
  SetLength(PageList, 0);
end;
procedure TZ80Assembler.SetMem48(buffer: pointer);
begin
  if DefaultPage <> nil then DefaultPage.Free;
  DefaultPage := TZ80MemBlock.Create;
  DefaultPage.Name := '';
  DefaultPage.Z80Start := 16384;
  DefaultPage.Z80End := 65536;
  DefaultPage.StartAddress := buffer;
  DefaultPage.AltLo := 65536;
  DefaultPage.AltHi := 0;
  SetLength(PageList, 1);
  PageList[0] := DefaultPage;
end;
procedure TZ80Assembler.AddTZ80MemBlock(m: TZ80MemBlock);
begin
  SetLength(PageList, High(PageList)+2);
  PageList[High(PageList)] := m;
end;
function TZ80Assembler.AddMem(Name: ShortString; Start, Size: Integer;
  buffer: Pointer): TZ80MemBlock;
var m: TZ80MemBlock;
begin
  m := TZ80MemBlock.Create;
  m.Name := uppercase(Name);
  m.Z80Start := Start;
  m.Z80End := Start+Size;
  m.StartAddress := buffer;
  m.AltLo := 65536;
  m.AltHi := 0;
  AddTZ80MemBlock(m);
  result := m;
end;
function TZ80Assembler.GetMemParams(Name: ShortString; var AltLo, AltHi: DWord): Pointer;
var i: Integer;
begin
  result := nil;
  AltLo := 65536;
  AltHi := 0;
  Name := uppercase(Name);
  for i := 0 to High(PageList) do if PageList[i].Name = Name then begin
    AltLo := PageList[i].AltLo;
    AltHi := PageList[i].AltHi;
    result := PageList[i].StartAddress;
    break;
  end;
end;

Function TZ80Assembler.ReadMemByte(Address: Word): Byte;
begin
  Result := 0;
end;
Function TZ80Assembler.ReadMemWord(Address: Word): Word;
begin
  Result := 0;
end;
procedure TZ80Assembler.eval1(S: String; var V, Code: Integer);
var
i, i2, i3, len, value, errCode, errCode2, opValue, parseType, SymIndex: Integer;
opType: Word;
wantNum, sign, skip: Boolean;
SymbolArrayNum: Integer;
SymbolFlags: DWord;
cmd: string;
begin
  i := 1;
  len := length(S);
  wantNum := True;
  sign := False;
  opType := 0;
  opValue := 0;
  value := 0;
  parseType := 0;
  SymIndex := -1;
  errCode := 0;
  while i <= len do begin
    if wantNum then begin
      if IsDelimiter(' '#9, S, i) then inc(i)
      else if S[i] = '(' then begin
        parseType := parseType or 2;
        i2 := FindChar(S, ')', '(', Char(0), i+1);
        if i2 = 0 then break
        else begin
          opValue := value;
          eval1(Copy(S, i+1, i2-(i+1)), value, errCode);
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2 + 1;
          opType := opType xor $8000;
        end;
      end else if S[i] = '[' then begin
        parseType := parseType or 2;
        i2 := FindChar(S, ']', '[', Char(0), i+1);
        if i2 = 0 then break
        else begin
          opValue := value;
          eval1(Copy(S, i+1, i2-(i+1)), value, errCode);
          if errCode < 0 then break;
          value := byte(ReadMemByte(value));
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2 + 1;
          opType := opType xor $8000;
        end;
      end else if S[i] = '-' then begin
        if sign then break;
        sign := not sign;
        inc(i);
      end else begin
        if IsDelimiter('"''', S, i) then begin
          if i = len then break;
          for i2 := i+1 to len do if IsDelimiter(S[i], S, i2) then break;
          if (i2 <= len) and ((i2-i) > 1) and ((i2-i) < 6) then begin;
            opValue := value;
            cmd := StringOfChar(Char(0), 4-((i2-i)-1)) + Copy(S, i+1, (i2-i)-1);
            value := (ord(cmd[1]) shl 24) or (ord(cmd[2]) shl 16) or (ord(cmd[3]) shl 8) or ord(cmd[4]);
            if sign then begin
              sign := False;
              value := -value;
            end;
            parseType := (parseType or 1) and -3;
            wantNum := False;
            i := i2+1;
            opType := opType xor $8000;
          end else break

        end else if IsDelimiter('_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.', S, i) then begin
          if i = len then i2 := i+1
          else for i2 := i+1 to len do if not IsDelimiter('''_.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', S, i2) then break;
          if IsDelimiter('.', S, i) then begin
            if LastSymbolDef <> nil then
              cmd := Copy(pchar(LastSymbolDef), 1, LastSymbolDefLen)+Copy(S, i, i2-i)
            else cmd := UpperCase(Copy(S, i, i2-i));
          end else cmd := UpperCase(Copy(S, i, i2-i));
          opValue := value;
          parseType := parseType or 1;
          SymbolArrayNum := 0;
          for i := 1 to Length(cmd) do SymbolArrayNum := SymbolArrayNum + (ord(cmd[i]) and $DF);
          SymbolArrayNum := SymbolArrayNum and $1F;
          SymIndex := AsmSymbols[SymbolArrayNum].IndexOf(cmd);
          if SymIndex < 0 then SymIndex := $80000000;
          if SymIndex >= 0 then begin
            SymbolFlags := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])]).SetData;
            if ((SymbolFlags and (SYM_SET+SYM_DEAD)) = (SYM_SET+SYM_DEAD))
              or ((SymbolFlags and SYM_MACRO) > 0) then begin
              errCode := errcode or $C0000000;
              break;
            end
            else value := AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])];
          end else begin
            errCode := errcode or $C0000000;
            break;
          end;
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2;
          parseType := (parseType or 8) and -3;
          opType := opType xor $8000;
        end else if IsDelimiter('%&$0123456789', S, i) then begin
          if i = len then i2 := i+1
          else for i2 := i+1 to len do if not IsDelimiter('0123456789ABCDEFHOXabcdefhox', S, i2) then break;
          Cmd := UpperCase(Copy(S, i, i2-i));
          opValue := value;
          skip := False;
          if Length(Cmd)>1 then begin
            if (isDelimiter('0123456789',Cmd,1)) and (Cmd[Length(Cmd)] = 'H') then
              Cmd := '$'+Copy(Cmd,1,Length(Cmd)-1)
            else if (not isDelimiter('$', Cmd, 1)) and (not isDelimiter('&', Cmd, 1))
            and (isDelimiter('B',Cmd,Length(Cmd))) then
              Cmd := '%'+Copy(Cmd,1,Length(Cmd)-1)
            else if (isDelimiter('OQ',Cmd,Length(Cmd))) and (Length(Cmd) > 1) then begin
              value := 0;
              for i3 := 1 to Length(Cmd)-1 do
                if (value and $E0000000 <> 0) or (not IsDelimiter('01234567',Cmd,i3)) then break
                else value := (value shl 3) or (ord(Cmd[i3])-48);
              if (i3 < Length(Cmd)-1) then begin
                errCode := errCode or $A0000000; // $80000000=Error, $10000000 = Invalid octal digit
                break;
              end else skip := True;
            end else if (isDelimiter('&',Cmd,1)) then Cmd[1] := '$'
            else if Copy(Cmd,1,2) = '0X' then
              Cmd := '$'+Copy(Cmd,3,Length(Cmd)-2);
            if isDelimiter('%',Cmd,1) then begin
              value := 0;
              for i3 := 2 to Length(Cmd) do
                if (value and $80000000 <> 0) or (not IsDelimiter('01',Cmd,i3)) then break
                else value := (value shl 1) or (ord(Cmd[i3])-48);
              if (i3 <= Length(Cmd)) or (i3 = 2) then begin
                errCode := errCode or $A0000000; // $80000000=Error, $20000000 = Invalid binary digit
                break;
              end else skip := True;
            end;
          end else if (Cmd = '$') then begin
            value := AsmLineAddr;
            skip := True;
          end;
          if not skip then Val(Cmd, value, errCode2);
          if errCode2 <> 0 then begin
            errCode := errCode or $80000000;
            break;
          end;
          parseType := (parseType or 1) and -3;
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2;
          opType := opType xor $8000;
        end else break;
      end;
    end else begin
      if IsDelimiter(' '#9, S, i) then inc(i)
      else if S[i] = '+' then begin
        parseType := (parseType or 4) and -3;
        opType := $0001;
        wantNum := True;
        inc(i);
      end else if S[i] = '-' then begin
        parseType := (parseType or 4) and -3;
        opType := $0002;
        wantNum := True;
        inc(i);
      end else if S[i] = '*' then begin
        parseType := (parseType or 4) and -3;
        opType := $0003;
        wantNum := True;
        inc(i);
      end else if S[i] = '/' then begin
        parseType := (parseType or 4) and -3;
        opType := $0004;
        wantNum := True;
        inc(i);
      end else if S[i] = '&' then begin
        parseType := (parseType or 4) and -3;
        opType := $0005;
        wantNum := True;
        inc(i);
      end else if S[i] = '|' then begin
        parseType := (parseType or 4) and -3;
        opType := $0006;
        wantNum := True;
        inc(i);
      end else if S[i] = '^' then begin
        parseType := (parseType or 4) and -3;
        opType := $0007;
        wantNum := True;
        inc(i);
      end else if S[i] = '%' then begin
        parseType := (parseType or 4) and -3;
        opType := $0011;
        wantNum := True;
        inc(i);
      end else if IsDelimiter('<>=!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.', S, i) then begin
        if i = len then i2 := i+1
        else for i2 := i+1 to len do if not IsDelimiter('<>=!&ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', S, i2) then break;
        cmd := UpperCase(Copy(S, i, i2-i));
        if (cmd = 'AND') or (cmd = '&') then begin
          parseType := (parseType or 4) and -3;
          opType := $0005;
          wantNum := True;
        end else if cmd = 'OR' then begin
          parseType := (parseType or 4) and -3;
          opType := $0006;
          wantNum := True;
        end else if cmd = 'XOR' then begin
          parseType := (parseType or 4) and -3;
          opType := $0007;
          wantNum := True;
        end else if (cmd = 'SHL') or (cmd = '<<') then begin
          parseType := (parseType or 4) and -3;
          opType := $0008;
          wantNum := True;
        end else if (cmd = 'SHR') or (cmd = '>>') then begin
          parseType := (parseType or 4) and -3;
          opType := $0009;
          wantNum := True;
        end else if cmd = '<' then begin
          parseType := (parseType or 4) and -3;
          opType := $000B;
          wantNum := True;
        end else if cmd = '>' then begin
          parseType := (parseType or 4) and -3;
          opType := $000C;
          wantNum := True;
        end else if cmd = '<=' then begin
          parseType := (parseType or 4) and -3;
          opType := $000D;
          wantNum := True;
        end else if cmd = '>=' then begin
          parseType := (parseType or 4) and -3;
          opType := $000E;
          wantNum := True;
        end else if cmd = '=' then begin
          parseType := (parseType or 4) and -3;
          opType := $000F;
          wantNum := True;
        end else if (cmd = '<>') or (cmd = '!=') then begin
          parseType := (parseType or 4) and -3;
          opType := $0010;
          wantNum := True;
        end else if (cmd = 'MOD') then begin
          parseType := (parseType or 4) and -3;
          opType := $0011;
          wantNum := True;
        end else break;
        i := i2;
      end else break;
    end;
    if (opType and $8000) > 0 then begin
      opType := opType xor $8000;
      try
        case opType of
          1: value := opValue + value;  // x + y
          2: value := opValue - value;  // x - y
          3: value := opValue * value;  // x * y
          4: value := opValue div value;  // x / y
          5: value := opValue and value;  // x and y
          6: value := opValue or value;  // x or y
          7: value := opValue xor value;  // x xor y
          8: value := opValue shl value;  // x << y
          9: value := opValue shr value;  // x >> y
         $A: value := opValue;
         $B: value := -1 + dword(opValue >= value);  // x < y
         $C: value := -1 + dword(opValue <= value);  // x > y
         $D: value := -1 + dword(opValue > value);  // x <= y
         $E: value := -1 + dword(opValue < value);  // x >= y
         $F: value := -1 + dword(opValue <> value);  // x = y
        $10: value := -1 + dword(opValue = value);  // x <> y
        $11: value := (opValue mod value);  // x mod y
        end;
        opType := 0;
        opValue := value;
      except
        begin
          dec(i);
          break;
        end;
      end;
    end;
  end;
  if (i > len) and ((parseType = 0) or not wantNum) and (errCode >= 0) and (parseType > 0) then begin
    V := value;
    Code := parseType;
  end else begin
    Code := parseType or ((errCode and $FFFF0000) or $80000000);
  end;
end;
function TZ80Assembler.FindChar(S: String; C, A, Q: Char; P: Integer): Integer;
var i, par: Integer;
begin
  par := 1;
  for i := P to Length(S) do begin
    if Q <> Char(0) then begin
      if S[i] = Q then Q := Char(0);
    end else begin
      if S[i] = C then begin
        dec(par);
        if par = 0 then break;
      end else if S[i] = A then begin
        inc(par);
      end;
      if IsDelimiter('"''', S, i) then Q := S[i];
    end;
  end;
  if i <= Length(S) then result := i
  else result := 0;
end;

function TZ80Assembler.evaluate(S: Pointer; c: Integer): Integer;
var
  v1, v2: Integer;
  SS: String;
begin
  SS := Copy(pchar(S),1,c);
  v1 := 0;
  v2 := 0;
  eval1(SS, v1, v2);
  self.val1 := v1;
  self.resultCode := v2;
end;

function IsReservedWord(arr, S: Pointer; i, c: Integer): Integer;
// arr = array to test, S = pointer to string to match
// i = number of indices, c = length of string to match
var
  buf: string[255];
asm
  push  ebx
  push  esi
  push  edi
  push  eax
  push  ecx
  mov ecx, [c]
  lea edi, buf
  mov [edi], cl
  mov ebx, 'za'+$100
  inc edi
@UCLoop:
  mov al, [edx]
  inc edx
  cmp al, bl
  jc  @noLC
  cmp al, bh
  jnc @noLC
  sub al, 32
@noLC:
  mov [edi], al
  inc edi
  loop  @UCLoop
  pop ecx
  pop eax
  inc ecx
  lea edx, buf+1

  // If the format of TAsmWord is changed, this lot will need to be updated...

  // array *must* be sorted alphabetically by TAsmWord.S for this to work!

  // string array format: @array returns: pointer to pointer:
  //    dword [pointer - 4]: Number of elements in array
  //    dword [pointer]: pointer to list of pointers to array elements (strings)

  // string format: @string returns: pointer to pointer:
  //    dword [pointer - 4]: Length of string
  //    byte [pointer...]: String data

  push  ecx
  mov ebx, ecx
  xor ecx, ecx
  push  ecx
  shr ebx, 1  // ebx = mid

@StartScan:
  mov esi, [eax+ebx*8] // eax = address of idx[0], ebx = idx num, TAsmWord record is 8 bytes long
  mov ecx, [c]
  mov edi, edx
  cmp ecx, [esi-4]
  jle @gotLen
  mov ecx, [esi-4]
@gotLen:
  cld
  and ecx, ecx
  je  @notFound

@ScanLoop:
  repz cmpsb // <- speed this up later when I have time...
  jc  @sGreater
  jne @sLess
  mov esi, [eax+ebx*8]
  mov ecx, [esi-4]
  cmp ecx, [c]
  je  @Found
  jc  @sGreater
@sLess:
  mov ecx, [esp]
  sub ecx, [esp+4]
  je  @notFound
  mov [esp+4], ebx
  jmp @nextString
@sGreater:
  mov ecx, [esp+4]
  sub ecx, [esp]
  dec ecx
  mov [esp], ebx
  jle  @notFound
@nextString:
  mov ebx, [esp+4]
  mov ecx, [esp]
  sub ebx, ecx
  shr ebx, 1
  add ebx, ecx
  jmp @StartScan
@notFound:
  mov eax, -1
  jmp @done
@Found:
  mov eax, [eax+4+ebx*8]
@done:
  lea esp, [esp+8]
  pop edi
  pop esi
  pop ebx
end;

function isInstruction(S: PChar): Integer;
var
  lo, hi, mid: Integer;
  p, p2: Pointer;
begin
  hi := High(OpcodeScanTable)+1;
  p := @OpcodeScanTable;
  p2 := @Opcodes;

  asm
  mov dword [@result], -1
  push  ebx
  push  esi
  push  edi
  mov eax, [hi]
  mov dword [lo], 0
  shr eax, 1
  mov [mid], eax

@StartScan:
  mov esi, [S]
  mov edx, [p]
  xor ecx, ecx
  mov ebx, [mid]
  mov edi, [p2]
  mov cx, [edx+ebx*2]
  mov edi, [edi+ecx*4]

  mov al, [edi]
  cmp al, 'A'
  jc  @skip1
  cmp al, 'Z'+1
  jc  @noSkip1
@skip1:
  inc edi
@noSkip1:
@ScanLoop:
  mov al, [esi]
  mov bl, [edi]
  inc edi
  and al, al
  je  @zero
  inc esi
  and bl, bl
  je  @sLess
  cmp al, bl
  jne @diff
  cmp al, '$'
  jne @ScanLoop
  cmp word [esi], 'NN'
  mov bx, [edi]
  jne @ScanLoop
  mov al, [esi+2]
  cmp bx, 'NN'
  je  @skipNN
  cmp bx, 'DD'
  je  @skipDD
  cmp bx, '$$'
  jne @scanLoop
@skipSS:
  add edi, 4
  add esi, 2
  jmp @ScanLoop
@skipNN:
  add edi, 4
  add esi, 2
  cmp bx, [edi-2]
  je  @scanLoop
  sub edi, 2
  jmp @ScanLoop
@skipDD:
  add edi, 2
  add esi, 2
  jmp @ScanLoop
@diff:
  jc  @sGreater
@sLess:
  mov eax, [mid]
  cmp eax, [lo]
  je  @Done
  mov [lo], eax
  jmp @nextString
@sGreater:
  mov edx, [hi]
  mov ecx, [mid]
  mov ebx, [lo]
  sub edx, ebx
  mov [hi], ecx
  jle  @Done
@nextString:
  mov ebx, [hi]
  mov edx, [lo]
  sub ebx, edx
  shr ebx, 1
  add ebx, edx
  mov [mid], ebx
  jmp @StartScan
@zero:
  cmp al,bl
  jne @sGreater
  mov [@result], ecx
@Done:
  pop edi
  pop esi
  pop ebx
  end;

end;

procedure TZ80Assembler.AddErrorQuick(errorNum, Line: Integer; FileNum: Integer);
var
  i: Integer;
  e: PAsmError;
  S1: ShortString;
begin
  i := NumErrors shl 2;
  if (High(QErrors)) <= (i+4) then
    SetLength(QErrors, i+256);
  e := PAsmError(@QErrors[i]);
  e.ErrorNum := errorNum;
  e.ErrorLine := Line+1;
  e.SourceFileNum := FileNum;
  e.Null := 0;
  inc(NumErrors);
  if (errorNum < -62) or (errorNum >= 0) then begin
    S1 := '';
  end;
end;

function TZ80Assembler.GetErrorString(index: Integer): String;
var
  Line, Code: Integer;
  S, FileName: String;
begin
  if GetError(index, Line, Filename, Code, S) then begin
    if Length(Filename) > 0 then
      result := ('('+IntToStr(Line)+') in "'+Filename+'": '+S)
    else result := ('('+IntToStr(Line)+'): '+S);
  end else Result := '';
end;

function TZ80Assembler.GetError(index: Integer; var Line: Integer; var FileName: String;
  var ErrorType: Integer; var Text: String): Boolean;
var
  e: PAsmError;
begin
  result := False;
  index := index * 4;
  if index > High(QErrors) then exit;
  e := PAsmError(@QErrors[index]);
  if e.SourceFileNum > 0 then FileName := SourceFiles[e.SourceFileNum] else FileName := '';
  ErrorType := e.ErrorNum;
  Line := e.ErrorLine;
  case e.ErrorNum of
    -01: Text :='Error';
    -02: Text :='Jump out of range';
    -03: Text :='Invalid byte data';
    -04: Text :='Bad index register displacement';
    -05: Text :='Invalid word data';
    -06: Text :='Invalid RST';
    -07: Text :='Undefined symbol';
    -08: Text :='Invalid filename';
    -09: Text :='Unrecognised instruction';
    -10: Text :='Bad ORG expression or ORG out of range';
    -11: Text :='Value out of range';
    -12: Text :='File not found';
    -13: Text :='Invalid file size';
    -14: Text :='File read error';
    -15: Text :='Can''t allocate memory';
    -16: Text :='No ORG specified';
    -17: Text :='Spurious ENDIF';
    -18: Text :='ELSE outside conditional';
    -19: Text :='ENDIF expected';
    -20: Text :='ELIF outside conditional';
    -21: Text :='Unrecognised directive';
    -22: Text :='Invalid symbol name';
    -23: Text :='Source expired prematurely';
    -24: Text :='Malformed line';
    -25: Text :='Nested macro definition';
    -26: Text :='ENDM outside macro definition';
    -27: Text :='Invalid PAGE or ORG out of range';
    -28: Text :='Malformed label';
    -29: Text :='Cannot use reserved word as label';
    -30: Text :='Local label before global label';
    -31: Text :='Symbol redefined';
    -32: Text :='Bad expression';
    -33: Text :='Expression result out of range';
    -34: Text :='Symbol declaration required';
    -35: Text :='Forward reference';
    -36: Text :='Bit number out of range';
    -37: Text :='Bad bit number expression';
    -38: Text :='Instruction too long';
    -39: Text :='Unbalanced parenthesis';
    -40: Text :='Opcode takes no operands';
    -41: Text :='Invalid combination of opcode and operands';
    -42: Text :='Invalid interrupt mode';
    -43: Text :='Byte value out of range';
    -44: Text :='Word value out of range';
    -45: Text :='Expression expected';
    -46: Text :='Instruction violates page bounds';
    -47: Text :='Immediate data expected';
    -48: Text :='Garbage following instruction';
    -49: Text :='ORG violates page bounds';
    -50: Text :='Expression must evaluate';
    -51: Text :='Recursive include';
    -52: Text :='Filename expected';
    -53: Text :='Error reading external source file';
    -54: Text :='Current output mode does not support pages';
    -55: Text :='Cannot change symbol type';
    -56: Text :='Nested structure definition';
    -57: Text :='ENDSTRUCT outside structure definition';
    -58: Text :='Instruction/directive not allowed inside structure definition';
    -59: Text :='Symbol expected';
    -60: Text :='Expression type mismatch';
    -61: Text :='IF Stack exceeds maximum length';
    -62: Text :='Invalid ALIGN value';
    -63: Text :='ENDR expected';
    -64: Text :='Invalid REPEAT count';
    -65: Text :='Bad REPEAT statement';
    -66: Text :='Cannot nest single-line REPEATs';
    -67: Text :='REPEAT stack overflow';
    -68: Text :='Spurious ENDR';
    $40000000: Text :='Undefined Symbol';
    else Text :='Error';
  end;
  result := True;
end;

Function TZ80Assembler.Assemble(Source: TStringList; Name: ShortString; StartAddress: Integer;
  StartPage: ShortString): Integer;
var
  c, i: Integer;
begin
  PassNum := 1;
  if SourceFiles <> nil then SourceFiles.Free;
  SourceFiles := TStringList.Create;
  SourceFiles.Add('');
  SourceFiles.Objects[0] := TObject(Source);
  for c := 0 to high(srcFiles) do begin
    i := SourceFiles.Add(srcFiles[c].filename);
    SourceFiles.Objects[i] := TObject(srcFiles[c].src);
  end;
  while doPass(Source, Name, StartAddress, StartPage, PassNum) do inc(PassNum);
  result := NumErrors;
  for i := 2+high(srcFiles) to SourceFiles.Count-1 do begin
    TStringList(SourceFiles.Objects[i]).Free;
    SourceFiles.Objects[i] := nil;
  end;
end;

procedure TZ80Assembler.AddSource(source: TStringList; filename: String);
var
  i: integer;
begin
  filename := uppercase(filename);
  for i := 0 to high(srcFiles) do if srcFiles[i].filename = filename then begin
    srcFiles[i].src := source;
    exit;
  end;
  i := high(srcFiles)+1;
  setLength(srcFiles, i+1);
  srcFiles[i].src := source;
  srcFiles[i].filename := filename;
end;
function TZ80Assembler.RemoveSource(filename: String): boolean;
var
  i, c: integer;
begin
  result := false;
  for i := 0 to high(srcFiles) do if srcFiles[i].filename = filename then begin
    for c := i to high(srcFiles)-1 do srcFiles[c] := srcFiles[c+1];
    setLength(srcFiles, high(srcFiles));
    result := true;
  end;
end;

function TZ80Assembler.GetSymbol(S: ShortString; var value: DWord): Boolean;
var
  SymbolArrayNum, SymIndex, SymbolFlags, i: Integer;
begin
  SymbolArrayNum := 0;
  for i := 1 to Length(S) do SymbolArrayNum := SymbolArrayNum + (ord(S[i]) and $DF);
  SymbolArrayNum := SymbolArrayNum and $1F;
  SymIndex := AsmSymbols[SymbolArrayNum].IndexOf(S);
  if SymIndex < 0 then Result := False else begin
    SymbolFlags := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])]).SetData;
    if ((SymbolFlags and (SYM_SET+SYM_DEAD)) = (SYM_SET+SYM_DEAD))
      or ((SymbolFlags and SYM_MACRO) > 0) then Result := False
    else begin
      Result := True;
      value := AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])];
    end;
  end;
end;

function TZ80Assembler.retFileSize(Filename: String):Integer;
var hnd: Integer;
begin
  hnd := CreateFile(Pchar(@Filename[1]), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
    0, OPEN_EXISTING, 0, 0);
  if hnd = INVALID_HANDLE_VALUE then result := -1 else begin
    result := GetFileSize(hnd, 0);
    CloseHandle(hnd);
  end;
end;

Function TZ80Assembler.doPass(Source: TStringList; var Name: ShortString; StartAddress: Integer;
  var StartPage: ShortString; Pass: Integer): Boolean;
var
  line, lineLen, i, i2, i3, i4, i5: Integer;
  Page: DWord;
  p, p2, p3: Pointer;
  FirstWord: Integer;
  CurSymbol: String[255];
  LastSymbol: Pointer;
  LastSymbolLen: Integer;
  SymbolNum: Integer;
  Instruction, tempString, tempstr, PageName: String[255];
  SymStart, SymLen: Integer;
  InsStart, InsLen, InsEnd, FWLen, InsWord2: Integer;
  LastInsSection, InsLen2, LastSrcSection, SrcSecLen: Integer;
  ExprStart, ExprLen, ExprMode, SectionNum, ParenthLevel: Integer;
  LastOpenBracket, IXDisp: DWord;
  ComStart, ComLen: Integer;
  errorNum: Integer;
  NumSymbolErrors: Integer;
  LastWords: DWord;
  InstrVal, DBSize: DWord;
  DBFlag: DWord;
  DBType: Boolean;
  IXSign, ValSet: Byte;
  CPJump: Pointer;

  LastSymbolBeforeStruct: Pointer;
  LastSymbolLenBeforeStruct: Integer;
  AsmWordsPtr, NumAsmWords: DWord;
  regEBX, regESI, regEDI: DWord;

  pSymbol: PAsmSymbol;
  minVal, maxVal: DWord;

  SS: ShortString;
  FileNum: Integer;
  IncludeSrc: TStringList;
  F: TFileStream;
  fBytesRead: Integer;
  defStruct: Boolean;
  StructOffset: DWord;
  IfStack: Array[0..63] of Byte;
  lastIF, InIF, allDirect: Boolean;
  IFlevel, IFValidLevel: Integer;
  IfTest, PageValid, defMacro: Boolean;
  macStack: Array [0..63] of TAsmMacroRecord;
  macLevel: Integer;
  repBase: Array [0..63] of DWord;
  repStack: Array [0..63] of TAsmRepRecord;
  repLevel, repBaseLevel: Integer;
  LineRepStart: Pointer;
  LineRepCount: Integer;
  D: PAsmDbgLine;
  TempLineLen: Word;

label
  AsmError, AsmErrorNoPush, AsmErrorReturn, doEval, AEQuick, doLine, noInstr, nextLine,
  noORGPage, done, AsmSkipWS, AsmGetExpr, AssembleSourceFile, AsmGetQuotedText,
  AddSymbol, doEQU_SET, EncodeInstruction, AsmGetArg, AsmGetSep, GetCurrentPage,
  DBLoop, DBError, DEFSFill, ProcessInstruction, nextFile;

  function AddDbgInfo(addr, len, flags: Integer): Boolean;
  begin
    if NumDbgLines >= (High(AsmDbgLines)+1) then SetLength(AsmDbgLines, NumDbgLines+64);
    D := @AsmDbgLines[NumDbgLines];
    D.StartAddress := addr;
    D.Page := DWord(@CurPage);
    D.Len := len;
    D.SetData := flags;
    D.SrcFileNum := FileNum;
    inc(NumDbgLines);
    result := True;
  end;

begin
  if Assigned(atStartOfPass) then atStartOfPass(self);
  AsmWordsPtr := DWord(@AsmWords);
  NumAsmWords := High(AsmWords);
  OrgAddr:=-1;

  totalBytes := 0;
  NumSymbolErrors := 0;
  NumErrors := 0;
  SetLength(QErrors, 0);
  Errors.Free;
  Errors := TStringList.Create;
  LastSymbol := nil;
  LastSymbolLen := 0;
  LastSymbolBeforeStruct := nil;
  LastSymbolLenBeforeStruct := 0;
  CurSymbol := '';
  defStruct := False;
  if pass = 1 then begin
    for i := 0 to High(PageList) do begin
      TZ80MemBlock(PageList[i]).AltLo := 65536;
      TZ80MemBlock(PageList[i]).AltHi := 0;
    end;
    for i := 0 to 31 do AsmSymbols[i].Free;
    for i := 0 to 31 do AsmSymbols[i] := TStringList.Create;
    NumAsmSymbols := 0;
    LastNumErrors := 0;
    LastSymErrors := 0;
    SetLength(AsmSymbolsExt, 2048);
    NumDbgLines := 0;
    SetLength(AsmDbgLines, 0);
  end;
  if SourceFilenames <> nil then SourceFilenames.Free;
  SourceFilenames := TStringList.Create;
  SourceFilenames.Add('');

  i := -1;
  AsmAddr := StartAddress;
  AsmVPtr := StartAddress;
  PageName := StartPage;
  if StartAddress >= 0 then asm
    mov [errorNum], -27
    call  GetCurrentPage
    jc  Done
  end else PageValid := False;

  for i := 0 to NumAsmSymbols-1 do begin
    pSymbol := PAsmSymbol(@AsmSymbolsExt[i shl 2]);
    pSymbol.SetData := pSymbol.SetData or SYM_DEAD;
  end;
  LastIf := True;
  IfLevel := 0;
  IfValidLevel := 0;
  InIf := False;
  line := 0;
  FileNum := 0;
  macLevel := 0;
  defMacro := False;
  repBaseLevel := 0;
  repLevel := 0;

AssembleSourceFile:
  while line < Source.Count do begin
    p := pointer(Source.Strings[line]);
    AsmLineAddr := AsmVPtr;
    if p <> nil then begin
    asm
    xor eax, eax
    mov ecx, [p]
    cld
    mov [symLen], eax
    test  byte [ecx], $FF
    mov [LineRepCount], eax
    mov [FWLen], eax
    mov [comLen], eax
    je  nextLine
    push  ebx
    push  esi
    push  edi
    mov edx, -30 // error number (if one occurs) - set to "bad symbol" for now
    xor ah, ah
    mov esi, [p]
    mov al, [esi]
    mov bx, $0920
    cmp al, '.'
    je  @localSym
    test [defStruct], 1
    je  @noLocalSym
    dec esi // force local symbol (in structure definitions)
@localSym:
    mov ecx, edx
    mov edx, [LastSymbol]
    inc esi
    and edx, edx
    mov edx, -30  // error -31 (Local symbol before global symbol)
    je  AsmError
    mov edx, ecx
    inc ah
@noLocalSym:
    mov ecx, -1
@checkForSymbol:
    mov al, [esi]
    inc ecx
    and al, al
    je  @gotEOL1
    cmp al, bl
    je  @gotWS1
    cmp al, bh
    je  @gotWS1
    cmp al, ':'
    je  @gotWS1
    cmp al, ';'
    je  @gotWS1
    and ecx, ecx
    je  @FirstChar
    cmp al, '0'
    jc  @invalidLabel
    cmp al, '9'+1
    jc  @validLabelChar
@FirstChar:
    cmp al, '_'
    je  @validLabelChar
    and al, $DF
    cmp al, 'A'
    jc  @invalidLabel
    cmp al, 'Z'+1
    jnc @invalidLabel
@validLabelChar:
    inc esi
    cmp ecx, 126
    jc  @checkForSymbol
@invalidLabel:
    mov edx, -22
    jmp AsmError
@gotWS1:
    and ecx, ecx
    je  @noSymbol
@gotWS2:
    and ah, ah
    lea edi, CurSymbol
    mov al, [edi]
    je  @defGlobalSym
    push  eax
    mov eax, [LastSymbolLen]
    add eax, ecx
    inc al
    mov [edi], al
    mov [symLen], eax
    inc edi
    pop eax
    mov [symStart], edi
    and al, al
    jne @GSCopied
    push  ecx
    push  esi
    push  edi
    mov ecx, [LastSymbolLen]
    mov esi, [LastSymbol]
    rep movsb
    pop   edi
    pop   esi
    pop   ecx
@GSCopied:
    add edi, [LastSymbolLen]
    mov byte [edi], '.'
    sub esi, ecx
    inc edi
    rep movsb
    mov byte [edi], 0
    jmp @gotSymbol
{@symbolReserved:
    mov dword [ErrorNum], -29 // "cannot use reserved word as label"
    call  AsmErrorReturn
    pop edx
    pop ecx
    push  ebx
    push  esi
    push  edi
    mov esi, [regESI]
    mov ebx, [regEBX]
    mov edi, [regEDI]
    add esi, ecx
    jmp @gotSymbol
}
@FWReserved:
    pop edx
    pop ecx
    push  ebx
    push  esi
    push  edi
    mov ebx, [regEBX]
    mov esi, [regESI]
    mov edi, [regEDI]
    cmp byte[esi+ecx], ':'
    je  @forceSymbol
    jmp @startInstr
@defGlobalSym:
    sub esi, ecx
    mov [regEDI], edi
    mov [regESI], esi
    mov [regEBX], ebx
    mov edx, esi
    pop edi
    pop esi
    pop ebx
    push  ecx
    push  edx
    push  ecx
    mov eax, [AsmWordsPtr]
    mov ecx, [NumAsmWords]
    call  IsReservedWord
    cmp eax, 0
    jge @FWReserved
    pop edx
    pop ecx
    push  ebx
    push  esi
    push  edi
    mov ebx, [regEBX]
    mov esi, [regESI]
@forceSymbol:
    mov edi, [LastSymbolLen]
    mov [LastSymbolLenBeforeStruct], edi
    mov [LastSymbolLen], ecx
    mov edi, [LastSymbol]
    mov [LastSymbolBeforeStruct], edi
    lea edi, CurSymbol
    mov byte [edi], cl
    inc edi
    mov [LastSymbol], esi
    mov [symLen], ecx
    mov [symStart], edi
    mov eax, ecx
    rep movsb
    mov byte [edi], 0

@gotSymbol:
    cmp byte [esi], ':'
    jne @noSymbol
    inc esi
@noSymbol:
    xor eax, eax
    dec esi
@checkForWS1:
    inc esi
    mov al, [esi]
    and al, al
    je  @gotEOL
    cmp al, bl
    je  @checkForWS1
    cmp al, bh
    je  @checkForWS1
@startInstr:
    lea edi, OpcodeASCIICodes
    mov ecx, esi
    mov bl, ';'
    lea edx, [esi+254]
    mov [InsStart], esi
@checkForInstr:
    and al, al
    je  @gotEOL2
    cmp al, bl
    je  @hasComment
    inc esi
    cmp byte [edi+eax*2], $20
    je  @noWS
    mov ecx, esi
    cmp ecx, edx
    jnc @Line2Long
@noWS:
    mov al, [esi]
    jmp @checkForInstr
@Line2Long:
    mov edx, -38
    jmp AsmError
@hasComment:
    mov eax, esi
    inc eax
    mov [ComStart], eax
@gotEOL2:
    sub ecx, [InsStart]
    je  @gotEOL
@gotIns:
    mov [InsLen], ecx
    mov esi, [InsStart]
    mov bx, $0920
    mov ecx, -1
    mov dx, ';'
@getInsFirstWord:
    mov al, [esi]
    inc ecx
    inc esi
    and al, al
    je  @gotFirstWord
    cmp al, bl
    je  @gotFirstWord
    cmp al, bh
    je  @gotFirstWord
    cmp al, dl
    jne @getInsFirstWord
@gotFirstWord:
    mov esi, [InsStart]
    mov eax, [InsLen]
    mov [FWLen], ecx
    add esi, ecx
    sub eax, ecx
    mov [InsWord2], esi
    mov [InsLen2], eax
    jmp @gotEOL
@gotEOL1:
    and ecx, ecx
    jne @gotWS2
@gotEOL:
    pop edi
    pop esi
    pop ebx
    end;
    goto doLine;

AsmError:
    asm
    pop edi
    pop esi
    pop ebx
    end;
AsmErrorNoPush:
    asm
    mov [errorNum], edx
    end;
    AddErrorQuick(errorNum, Line, FileNum);
    goto nextLine;
AsmErrorReturn:
    AddErrorQuick(errorNum, Line, FileNum);
    asm
    ret
    end;

doEval:
    Evaluate(Pointer(ExprStart), ExprLen);
    asm
    mov esi, Self
    mov eax, [esi+val1]
    mov edx, [esi+resultCode]
    ret
    end;

AEQuick:
    AddErrorQuick(ErrorNum, line, FileNum);
    asm
    ret
    end;

AsmSkipWS:
    asm
    push  ebx
    xor ecx, ecx
    xor ebx, ebx
    lea edx, OpcodeASCIICodes
    mov ch, ' '
@DBWSLoop:
    cmp eax, [InsEnd]
    je  @DBNoWS
    mov bl, [eax]
    mov cl, [edx+ebx*2]
    lea eax, [eax+1]
    and cl, cl
    je  @DBNoWS
    cmp cl, ch
    je  @DBWSLoop
@DBNoWS:
    pop ebx
    lea eax, [eax-1]
    ret
    end;

AsmGetQuotedText:
    asm
    push  ebx
    push  esi
    push  edi
    mov edx, $2227
    lea ebx, tempString+1
    xor ecx, ecx
    lea esi, [ebx+250]
    lea edi, [eax+1]
@QtLoop:
    cmp eax, [InsEnd]
    jnc @QtError
    mov cl, [eax]
    and ch, ch
    lea eax, [eax+1]
    jne @QtInQuote
    and cl, cl
    je  @QtError
    cmp cl, dl
    je  @QtSetQuote
    cmp cl, dh
    jne @QtLoop
@QtSetQuote:
    mov ch, cl
    cmp eax, edi
    je  @QtLoop
@QtError:
    mov byte [ebx], 0
    lea edi, tempString+1
    xor ch, ch
    sub ebx, edi
    mov [edi-1], bl
@QtError2:
    stc
    pop edi
    pop esi
    pop ebx
    ret
@QtInQuote:
    cmp ebx, esi
    mov [ebx], cl
    lea ebx, [ebx+1]
    jnc @QtError
    cmp cl, ch
    jne @QtLoop
    dec ebx
    lea edi, tempString+1
    mov byte [ebx], 0
    xor ch, ch
    sub ebx, edi
    mov [edi-1], bl
    je  @QtError2
    clc
    pop edi
    pop esi
    pop ebx
    ret
    end;

AsmGetSep:
    asm
    cmp byte [eax], ','
    jne @error
    inc eax
    call  AsmSkipWS
    je  @error
    clc
    ret
@error:
    stc
    ret
    end;

AsmGetArg:
    asm
    bswap ecx
    push  ebx
    push  esi
    push  edi
    mov edx, $2227
    lea ebx, tempString+1
    xor cx, cx
    lea esi, [ebx+250]
    lea edi, tempString+1
@QtLoop:
    cmp eax, [InsEnd]
    jnc @QtDone2
    mov cl, [eax]
    and ch, ch
    lea eax, [eax+1]
    jne @QtInQuote
    and cl, cl
    je  @QtDone
    cmp cl, ','
    je  @QtDone
    cmp cl, dl
    je  @QtSetQuote
    cmp cl, dh
    je  @QtSetQuote
    cmp cl, '<'
    je  @QtSetBrk
    bt  ecx, 31
    jnc @QtCopyByte
    cmp cl, ' '
    je  @QtDone
    cmp cl, 09
    je  @QtDone
@QtCopyByte:
    cmp edi, esi
    mov [edi], cl
    lea edi, [edi+1]
    jc  @QtLoop
    mov ecx, edi
    lea ebx, tempString+1
    sub ecx, ebx
    mov [ebx-1], cl
    pop edi
    pop esi
    pop ebx
    stc
    ret
@QtSetBrk:
    mov cl, '>'
@QtSetQuote:
    cmp edi, ebx
    jne @QtCopyByte
    mov ch, cl
    jmp @QtLoop
@QtInQuote:
    cmp cl, ch
    jne @QtCopyByte
    jmp @QtDone2
@QtDone:
    dec eax
@QtDone2:
    mov ecx, edi
    lea ebx, tempString+1
    sub ecx, ebx
    mov [ebx-1], cl
    pop edi
    pop esi
    pop ebx
    clc
    ret
    end;

AsmGetExpr:
    asm
    mov [exprStart], eax
    mov [ValSet], cl
    push  ebx
    push  esi
    push  edi
    mov dword [InstrVal], 0
    mov esi, eax
    xor edx, edx
    xor ebx, ebx
    xor eax, eax
    xor ch, ch
    lea edi, OpcodeASCIICodes
    dec esi
@AsmExpLoop:
    inc esi // need to preserve the Z flag
    cmp esi, [InsEnd]
    jnc @AsmExpEnd
    mov al, [esi]
    and ch, ch
    jne @ExpInQuote
    mov dx, [edi+eax*2]
    cmp dl, ' '
    je  @AsmExpNextByte
    cmp dl, '"'
    je  @AsmSetQuote
    cmp dl, 39
    je  @AsmSetQuote
    cmp dl, ','
    je  @AsmExpEnd
    xor cl, cl
@AsmExpNextByte:
    jmp @AsmExpLoop
@AsmExpEnd:
    and cl, cl
    jne @ExpAllQuotes
    lea ecx, [esi]
    pop edi
    mov eax, esi
    sub ecx, [exprStart]
    pop esi
    mov [exprLen], ecx
    pop ebx
    call doEval
    mov ecx, 1
    bt  edx, 31
    jnc @ExprOK
    bt  edx, 30
    mov edx, -07  // "symbol undefined"
    jc  @ExprError
    mov edx, -32 // "bad expression"
    jmp @ExprError
@AsmSetQuote:
    mov ch, al
    jmp @AsmExpNextByte
@AsmSetForFullEval:
    xor cl, cl
    jmp @AsmExpNextByte
@ExpInQuote:
    inc ebx
    cmp al, ch
    jne @AsmExpNextByte
    cmp ebx, 3
    jnc @ExpAllQuotes
    xor cx, cx
    jmp @AsmExpNextByte
@ExpAllQuotes:
    lea eax, [esi+1]
    pop edi
    mov ecx, eax
    pop esi
    sub ecx, [exprStart]
    pop ebx
    cmp ecx, ecx
    mov [exprLen], ecx
    ret
@ExprOK:
    xor edx, edx
    inc edx
    ret
@ExprError:
    bt  edx, 31
    ret
    end;

GetCurrentPage:
  asm
    pushad
  end;
  for i := 0 to High(PageList) do
    if (PageName = PageList[i].Name) and
    (AsmAddr >= PageList[i].Z80Start) and
    (AsmAddr < PageList[i].Z80End) then break;
  if (i < 0) or (i > High(PageList)) then begin
    AddErrorQuick(errorNum, Line, FileNum);
    asm
      mov [PageValid], 0
      popad
      stc
      ret
    end;
  end else CurPage := PageList[i];
  asm
    mov [PageValid], 1
    popad
    clc
    ret
  end;

AddSymbol:
    i := 0;
    for i2 := 1 to Length(CurSymbol) do i := i + (ord(CurSymbol[i2]) and $DF);
    i := i and $1F;
    i2 := AsmSymbols[i].IndexOf(CurSymbol);
    if i2 >= 0 then begin
      pSymbol := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[i].Objects[i2])]);
      if (pSymbol.SetData and (SYM_SET+SYM_MACRO)) > 0 then begin
        if FirstWord <> 93 then begin
          AddErrorQuick(-55, Line, FileNum); // "cannot change symbol type"
          i2 := -1;
        end;
      end else begin
        if Integer(p) <> AsmSymbolsExt[Integer(AsmSymbols[i].Objects[i2])+3] then
          AddErrorQuick(-31, Line, FileNum); // "symbol redefined"
      end;
    end;
    if (i2 < 0) or (FirstWord = 93) then begin
      if i2 < 0 then begin
        i2 := NumAsmSymbols shl 2;
        inc(NumAsmSymbols);
      end;
      if FirstWord = 93 then i3 := SYM_SET else i3 := 0;

      if (High(AsmSymbolsExt)) <= (i2+4) then
        SetLength(AsmSymbolsExt, i2+256);
      pSymbol := PAsmSymbol(@AsmSymbolsExt[i2]);
      pSymbol.StartAddress := InstrVal;
      pSymbol.SetData := i3;
      pSymbol.SrcFileNum := FileNum;
      pSymbol.Page := DWord(CurPage);
      pSymbol.DefinedAt := DWord(p);
      AsmSymbols[i].Sorted := False;
      AsmSymbols[i].Add(CurSymbol);
      AsmSymbols[i].Objects[AsmSymbols[i].Count-1] := TObject(i2);
    end else i := -1;
    asm
    ret
    end;

doLine:
    asm
    mov ecx, [FWLen]
    push  ebx
    push  esi
    push  edi
    xor eax, eax
    mov esi, [InsStart]
    lea edi, Instruction+1
    lea ebx, OpcodeASCIICodes
    inc ecx
    jmp @FWCheck
@UCFirstWordLoop:
    mov al, [esi]
    inc esi
    mov al, [ebx+eax*2]
    mov [edi], al
    inc edi
@FWCheck:
    loop  @UCFirstWordLoop
    pop edi
    pop esi
    pop ebx
@endOfInstr:
    end;
ProcessInstruction:
    oldPage := curPage;
    if FWLen > 0 then begin
      FirstWord := IsReservedWord(@AsmWords, Pointer(InsStart), High(AsmWords), FWLen);
      if (FirstWord < 0) and (FWLen = 1) then begin
        if pChar(InsStart)[0] = '=' then FirstWord := 34;
      end;
    end
    else FirstWord := 1000;
    case FirstWord of
      168:
        begin // macro
          if defMacro then AddErrorQuick(-25, Line, FileNum) // "nested macro definition"
          else begin
            defMacro := True;
          end;
        end;
      169:
        begin // endmacro
          if not defMacro then AddErrorQuick(-26, Line, FileNum) // "ENDM outside macro def"
          else begin
            defMacro := False;
            goto nextLine;
          end;
        end;
     end;
    if defMacro then goto nextLine;
    if LastIf then begin
      if SymLen > 0 then begin
        LastSymbolDef := LastSymbol;
        LastSymbolDefLen := LastSymbolLen;
      end;
      if (SymLen > 0) and defStruct then begin
        InstrVal := StructOffset;
        asm
        call  AddSymbol
        end;
      end else if (SymLen > 0) and ((FirstWord <> 34) and (FirstWord <> 183) and (FirstWord <> 103)) then begin
        if FirstWord = 93 then goto EncodeInstruction;
        if not PageValid then begin
          AddErrorQuick(-16, Line, FileNum); // "No ORG specified"
          goto Done;
        end;
        InstrVal := AsmVPtr;
        asm
        call  AddSymbol
        end;
      end;
    end;

    case FirstWord of
    1000: goto NextLine;
      65, 177: // ORG , LOAD / BASE
        if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        add eax, [FWLen]
        mov [InsEnd], ecx
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [LastInsSection], eax
        je  AsmErrorNoPush
        end;

        asm
          mov eax, [LastInsSection]
          mov edx, [InsEnd]
          dec eax
          mov ch, ':'
          lea edx, tempString[0]
@getORGPage:
          cmp eax, [InsEnd]
          lea edx, [edx+1]
          jnc noORGPage
          mov cl, [eax+1]
          inc eax
          cmp cl, ch
          je  @gotORGPage
          mov [edx], cl
          jmp @getORGPage
@gotORGPage:
          inc eax
          lea ecx, tempString[1]
          mov dword [LastInsSection], eax
          mov eax, edx
          sub eax, ecx
          mov [ecx-1], al
        end;
        if Length(tempString) = 0 then goto noORGPage;
        tempString := uppercase(tempString);
        if High(PageList) = 0 then begin
            AddErrorQuick(-54, Line, FileNum); // "output mode does not support pages"
            goto done;
        end else PageName := TempString;
        asm
noORGPage:
        mov eax, [LastInsSection]
        mov ecx, [InsEnd]
        mov edx, -10
        sub ecx, eax
        je  @invalidORG
        mov [ExprStart], eax
        mov [ExprLen], ecx
        push  ebx
        push  esi
        push  edi
        call  doEval
        pop edi
        pop esi
        pop ebx
        bt  edx, 31
        mov edx, -10 // "bad ORG expression"
        jnc @orgExprOK
        bt  edx, 30
        jnc @InvalidORG
        mov edx, -50  // "expression must evaluate"
@InvalidORG:
        jmp AsmErrorNoPush
@orgExprOK:
        mov [InstrVal], eax
        end;

        for i := 0 to High(PageList) do
          if (PageName = PageList[i].Name) and
          (InstrVal >= PageList[i].Z80Start) and
          (InstrVal < PageList[i].Z80End) then break;
        if (i < 0) or (i > High(PageList)) then begin
          AddErrorQuick(-27, Line, FileNum);
          PageValid := False;
          goto Done;
        end else begin
          CurPage := PageList[i];
          PageValid := True;
          if FirstWord = 65 then begin
          AsmAddr := InstrVal;
          OrgAddr := InstrVal;
          end;
          AsmVPtr := InstrVal;
          AsmLineAddr := AsmAddr;
        end;
        goto  nextLine;
        end;
      42, 163, 164, 166, 167: // IF, IFDEF, IFNDEF, IFEQ, IFNE
        begin
          if IFValidLevel = IFLevel then inc(IFValidLevel);
          inc(IFLevel);
          if IfLevel > 62 then begin
            AddErrorQuick(-61, Line, FileNum);
          end else if LastIF then begin
            asm
            mov eax, [InsStart]
            mov ecx, [InsLen]
            add ecx, eax
            mov [InsEnd], ecx
            add eax, [FWLen]
            call  AsmSkipWS
            mov ecx, [FirstWord]
            jne @IfHasExpr
            inc ecx
            mov edx, -59 // "symbol expected"
            and ecx, $FE
            cmp ecx, 164
            je  AsmErrorNoPush
            mov edx, -45 // "expression expected"
            jmp AsmErrorNoPush
@IfHasExpr: mov [exprStart], eax
            mov ecx, [InsEnd]
            sub ecx, eax
            mov [exprLen], ecx
            end;
            case FirstWord of
              163, 164: // IFDEF, IFNDEF
                begin
                  asm
                  mov eax, [exprStart]
                  mov ecx, 1
                  call  AsmGetArg
                  mov edx, -59 // "symbol expected"
                  je  AsmErrorNoPush
                  call  AsmSkipWS
                  mov edx, -48 // "garbage following instruction"
                  jne AsmErrorNoPush
                  end;

                  i := 0;
                  for i2 := 1 to Length(tempString) do i := i + (ord(tempString[i2]) and $DF);
                  i := i and $1F;
                  i2 := AsmSymbols[i].IndexOf(tempString);
                  IfTest := (FirstWord = 164);
                  if i2 >= 0 then begin
                    pSymbol := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[i].Objects[i2])]);
                    IfTest := IfTest xor ((pSymbol.SetData and SYM_DEAD) = 0);
                  end;
                end;
              42, 166, 167: // IF, IFEQ, IFNE
                begin
                  asm
                  mov eax, [exprStart]
                  call  AsmGetExpr
                  jc  AsmErrorNoPush
                  mov edx, -60
                  je  AsmErrorNoPush
                  mov [InstrVal], eax
                  end;
                  IfTest := (InstrVal <> 0) xor (FirstWord = 167);
                end;
              end;
            IfStack[IFLevel-1] := (byte(LastIF) and 1) + ((byte(InIF) and 1) shl 1);
            LastIF := IfTest;
            InIF := True;
            if not LastIF then dec(IFValidLevel);
            goto nextLine
          end;
        end;

    30: // ELSE
      begin
        if IFLevel = 0 then AddErrorQuick(-18, Line, FileNum) // ELSE
        else if not InIF then AddErrorQuick(-19, Line, FileNum)
        else begin
          LastIF := not LastIF;
          InIF := False;
        end;
        goto NextLine;
      end;

    31: // ELSEIF
      begin
        if IFLevel = 0 then AddErrorQuick(-20, Line, FileNum) // ELIF
        else if not InIF then AddErrorQuick(-19, Line, FileNum)
        else begin
          LastIF := not LastIF;
          InIF := False;
        end;
        goto NextLine;
      end;

    33: // ENDIF
      begin
        if IfLevel = 0 then begin
          AddErrorQuick(-17, Line, FileNum);
        end else begin
          if IFValidLevel = IFLevel then dec(IFValidLevel);
          dec(IFLevel);
          if IFLevel = 0 then begin
            LastIF := True;
            InIF := False;
          end else begin
            if IFLevel <= IFValidLevel then begin
              LastIF := Boolean(IfStack[IFLevel-1] and 1);
              InIF := Boolean((IfStack[IFLevel-1] and 2) shr 1);
            end;
          end;
        end;
        goto NextLine;
      end;

      end;
    if not LastIF then goto nextLine;
    case FirstWord of
      103: // STRUCT
        begin
          if SymLen = 0 then AddErrorQuick(-34, Line, FileNum) // "symbol required"
          else if defStruct then AddErrorQuick(-56, Line, FileNum) // "nested structure"
          else begin
            InstrVal := 0;
            asm
              mov eax, [InsStart]
              mov ecx, [InsLen]
              add ecx, eax
              mov [InsEnd], ecx
              add eax, [FWLen]
              call  AsmSkipWS
              mov edx, -45 // "expression expected"
              mov [exprStart], eax
              mov ecx, [InsEnd]
              je  @struct_ok
              sub ecx, eax
              mov [exprLen], ecx

              mov [minVal], -128
              mov [maxVal], 256
              mov [DBSize], 1
              mov [DBFlag], 1
              mov cl, byte [DBFlag]
              call  AsmGetExpr
              mov [InstrVal], eax
              setne [DBType]
              jc  AsmErrorNoPush

              mov eax, [exprStart]
              add eax, [exprLen]
              call  AsmSkipWS
              je  @struct_OK
              mov edx, -48 // "garbage following instruction"
              jmp AsmErrorNoPush
@struct_ok: end;
            defStruct := True;
            StructOffset := InstrVal;
          end;
          goto NextLine;
        end;
      183: // OFFSET (AT)
        begin
          if not defStruct then AddErrorQuick(-56, Line, FileNum) // "nested structure"
          else begin
            asm
              mov eax, [InsStart]
              mov ecx, [InsLen]
              add ecx, eax
              mov [InsEnd], ecx
              add eax, [FWLen]
              call  AsmSkipWS
              mov edx, -45 // "expression expected"
              mov [exprStart], eax
              mov ecx, [InsEnd]
              je  AsmErrorNoPush
              sub ecx, eax
              mov [exprLen], ecx

              mov [minVal], -128
              mov [maxVal], 256
              mov [DBSize], 1
              mov [DBFlag], 1
              mov cl, byte [DBFlag]
              call  AsmGetExpr
              mov [InstrVal], eax
              setne [DBType]
              jc  AsmErrorNoPush

              mov eax, [exprStart]
              add eax, [exprLen]
              call  AsmSkipWS
              je  @struct_OK
              mov edx, -48 // "garbage following instruction"
              jmp AsmErrorNoPush
@struct_ok: end;
            StructOffset := InstrVal;
            if SymLen > 0 then asm
              call  AddSymbol
            end;
          end;
          goto NextLine;
        end;
      174: // ENDSTRUC (ENDSTRUCT)
        begin
          if not defStruct then AddErrorQuick(-57, Line, FileNum) // "ENDSTRUCT outside structure def"
          else begin
            defStruct := False;
            InstrVal := StructOffset;
            CurSymbol := Copy(CurSymbol, 1, LastSymbolLen)+'.sizeof';
            asm
              call  AddSymbol
            end;
            LastSymbol := LastSymbolBeforeStruct;
            LastSymbolLen := LastSymbolLenBeforeStruct;
            if LastSymbol <> nil then
              CurSymbol := Copy(pchar(LastSymbol),1,LastSymbolLen)
            else CurSymbol := '';
          end;
          goto nextLine;
        end;
      34: // EQU
DoEQU_SET:
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        add eax, [FWLen]
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [exprStart], eax
        mov ecx, [InsEnd]
        je  AsmErrorNoPush
        sub ecx, eax
        mov [exprLen], ecx
        call doEval
        bt  edx, 31
        mov [InstrVal], eax
        jnc @EQU_OK
@EQU_Err:
        bt  edx, 30
        mov edx, -50 // "expression must evaluate"
        jnc @EQU_Err2
        mov edx, -07 // "symbol undefined"
        inc dword [NumSymbolErrors]
@EQU_Err2:
        jmp AsmErrorNoPush
@EQU_OK:
        end;
        if SymLen = 0 then AddErrorQuick(-34, Line, FileNum) // "symbol required"
        else asm
        call  AddSymbol
        end;
        goto nextLine
        end;
      46: // INCLUDE
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        mov eax, [InsWord2]
        call  AsmSkipWS
        mov edx, -52 // "filename expected"
        mov [LastInsSection], eax
        je  AsmErrorNoPush
        call  AsmGetQuotedText
        mov edx, -08 // "invalid filename"
        jc  AsmErrorNoPush
        call  AsmSkipWS
        mov edx, -48 // "garbage following instruction"
        jne AsmErrorNoPush
        end;
        tempString := uppercase(tempString);
        i := SourceFilenames.IndexOf(tempString); // check for recursive include
        if i >= 0 then AddErrorQuick(-51, Line, FileNum) // "recursive include"
        else begin // not recursive, but file might have been loaded already
          i := SourceFiles.IndexOf(tempString);
          if i < 0 then begin // file isn't in memory so load it
            IncludeSrc := TStringList.Create;
            ErrorNum := 0;
            if FileExists(tempString) then begin
              try
                IncludeSrc.LoadFromFile(tempString);
              except // error if we can't read the source file
                IncludeSrc.Free;
                ErrorNum := -53;  // "can't read source file"
              end;
            end else begin // can't find the file on disk
              IncludeSrc.Free;
              ErrorNum := -12; // "file not found"
            end;
            if ErrorNum <> 0 then begin
              AddErrorQuick(errorNum, Line, FileNum);
              goto nextLine;
            end;
            i := SourceFiles.Add(tempString);
            SourceFiles.Objects[i] := TObject(IncludeSrc);
          end;
          // if we got this far the source is in memory
          i2 := SourceFilenames.Add(tempString);
          SourceFilenames.Objects[i2] := TObject(Line);
          repBase[i2] := repBaseLevel;
          FileNum := i;
          Line := -1;
          Source := TStringList(SourceFiles.Objects[i]);
          goto nextLine
        end;
        goto nextLine;
        end;
      end;
    if not PageValid then begin
      AddErrorQuick(-16, Line, FileNum); // "no ORG specified"
      goto Done;
    end else case FirstWord of
      149: // INCBIN
        if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
        asm
          mov eax, [InsStart]
          mov ecx, [InsLen]
          add ecx, eax
          mov [InsEnd], ecx
          add eax, [FWLen]
          call  AsmSkipWS
          mov edx, -52 // "filename expected"
          mov [LastInsSection], eax
          je  AsmErrorNoPush
          call  AsmGetQuotedText
          mov edx, -08 // "invalid filename"
          jc  AsmErrorNoPush
          call  AsmSkipWS
          mov edx, -48 // "garbage following instruction"
          jne AsmErrorNoPush
        end;
        i2 := retFileSize(tempString);
        if i2 = -1 then
          AddErrorQuick(-12, Line, FileNum) // "file not found"
        else if ((i2 < 0) or (i2 > 65535)) then
          AddErrorQuick(-13, Line, FileNum) // "invalid file size"
        else begin
          if pass=1 then
            try F := TFileStream.Create(tempString, fmOpenRead or fmShareDenyWrite);
            except
              AddErrorQuick(-14, Line, FileNum);
              F := nil;
          end else F := nil;
          while i2 > 0 do begin
            if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then asm
              mov [errorNum], -27
              call GetCurrentPage
            end;
            if not PageValid then goto nextLine;
            i3 := CurPage.Z80End - AsmAddr;
            if i3 > i2 then i3 := i2;
            if F <> nil then begin
              p2 := PByteArray(pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start)));
              try fBytesRead := F.Read(p2^, i3);
              except AddErrorQuick(-14, Line, FileNum);
              end;
              inc(TotalBytes, i3);
            end;
            if CreateExtraInfo then AddDbgInfo(AsmAddr, i3, DBG_BYTE);
            inc(AsmAddr, i3);
            inc(AsmVPtr, i3);
            dec(i2, i3);
          end;
          if F <> nil then F.Free;
        end;
        goto  nextLine;
        end;
      20,21,23: // DEFB, DEFW, DEFM
        if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
        asm
        mov eax, [InsStart]
        mov ecx, eax
        add eax, [FWLen]
        add ecx, [InsLen]
        mov [InsEnd], ecx
        call  AsmSkipWS
        mov edx, -47
        je  DBError
        cmp [FirstWord], 23
        mov [minVal], -32768
        mov [maxVal], 65536
        mov [DBFlag], 2
        mov [DBSize], 2
        je  DBLoop
        mov [minVal], -128
        mov [maxVal], 256
        mov [DBSize], 1
        mov [DBFlag], 1
        jmp DBLoop
DBLoop:
        mov cl, byte [DBFlag]
        call  AsmGetExpr
        mov [InstrVal], eax
        setne [DBType]
        jnc @DBWrite
DBError:
        mov [ErrorNum], edx
        push  eax
        call  AsmErrorReturn
        pop eax
        cmp dword [ErrorNum], -07
        mov [DBType], 1
        mov dword [InstrVal], 0
        jne nextLine
        inc dword [NumSymbolErrors]
@DBWrite:
        end;
        if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
          asm
            mov [errorNum], -46 // "instruction violates page bounds"
            call  GetCurrentPage
          end;
          if not PageValid then goto NextLine;
        end;
        p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
        if DBType then begin // DB 1, 2, 3 etc.
          if DBSize = 1 then begin
            if (InstrVal < $FFFFFF80) and (InstrVal > 255) then begin
              AddErrorQuick(-03, Line, FileNum);
              goto NextLine;
            end else begin
              AddDbgInfo(AsmAddr, 1, DBG_BYTE);
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              pByteArray(p2)[0] := InstrVal;
              inc(AsmAddr, DBSize);
              inc(AsmVPtr, DBSize);
              inc(TotalBytes);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end else begin
            if (InstrVal < $FFFF8000) and (InstrVal > 65535) then begin
              AddErrorQuick(-05, Line, FileNum);
              goto NextLine;
            end else if (CurPage.Z80End-AsmAddr) > 1 then begin
              AddDbgInfo(AsmAddr, 2, DBG_WORD);
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              pWordArray(p2)[0] := InstrVal;
              inc(AsmAddr, DBSize);
              inc(AsmVPtr, DBSize);
              inc(TotalBytes, 2);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end else begin
              AddDbgInfo(AsmAddr, 2, DBG_WORD);
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              pByteArray(p2)[0] := InstrVal and 255;
              inc(TotalBytes);
              inc(AsmAddr);
              inc(AsmVPtr);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
              asm
                mov [errorNum], -46 // "instruction violates page bounds"
                call  GetCurrentPage
              end;
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              inc(AsmAddr);
              inc(AsmVPtr);
              if not PageValid then goto nextLine;
              p2 := pointer(Integer(CurPage.StartAddress)+((AsmAddr-1)-CurPage.Z80Start));
              pByteArray(p2)[0] := InstrVal shr 8;
              inc(TotalBytes);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end;
        end else if ExprLen > 2 then begin // DB "string"
          i2 := ExprLen - 2;
          p3 := Pointer(ExprStart+1);
          if (AsmAddr+i2) <= CurPage.Z80End then begin
            AddDbgInfo(AsmAddr, i2, DBG_TEXT);
            if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
            for i := 0 to i2-1 do PByteArray(p2)[i] := PByteArray(p3)[i];
            inc(totalBytes, i2);
            inc(AsmAddr, i2);
            inc(AsmVPtr, i2);
            if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
          end else begin
            i4 := 0;
            AddDbgInfo(AsmAddr, i2, DBG_TEXT);
            while i2 > 0 do begin
              if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
                asm
                  mov [errorNum], -46 // "instruction violates page bounds"
                  call  GetCurrentPage
                end;
                if not PageValid then goto NextLine;
              end;
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              i3 := CurPage.Z80End - AsmAddr;
              if i3 > i2 then i3 := i2;
              p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
              for i5 := 0 to i3-1 do PByteArray(p2)[i5] := PByteArray(p3)[i5+i4];
              inc(i4, i3);
              inc(AsmAddr, i3);
              inc(AsmVPtr, i3);
              inc(totalBytes, i3);
              dec(i2, i3);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end;
        end;
        ExprStart := ExprStart + ExprLen;
        asm
          mov eax, [exprStart]
          call  AsmSkipWS
          je  @DBDone
          call  AsmGetSep
          mov edx, -48 // "garbage following instruction"
          jc  AsmErrorNoPush
          jmp DBLoop
@DBDone:
        end;
        goto nextLine;
        end;

      22, 27, 173: // DEFS (RESB), DEFSW (RESW)
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        add eax, [FWLen]
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [exprStart], eax
        mov ecx, [InsEnd]
        je  AsmErrorNoPush
        sub ecx, eax
        mov [exprLen], ecx

        mov [minVal], -128
        mov [maxVal], 256
        mov [DBSize], 1
        mov [DBFlag], 1
        mov cl, byte [DBFlag]
        call  AsmGetExpr
        mov [InstrVal], eax
        setne [DBType]
        jc  AsmErrorNoPush

        mov edx, [self]
        mov eax, [exprStart]
        mov cl, byte [edx].TZ80Assembler.DEFSDefault
        add eax, [exprLen]
        mov byte [ValSet], cl
        call  AsmSkipWS
        je  @DEFS_OK
        call  AsmGetSep
        mov edx, -48 // "garbage following instruction"
        jc  AsmErrorNoPush
        mov [minVal], -128
        mov [maxVal], 256
        mov [DBSize], 1
        mov [DBFlag], 1
        mov cl, byte [DBFlag]
        push  [InstrVal]
        call  AsmGetExpr
        pop [InstrVal]
        mov [ValSet], al
        setne [DBType]
        jc  AsmErrorNoPush

@DEFS_OK:
        end;
        if FirstWord = 173 then DBSize := 2 else DBSize := 1;
        i2 := InstrVal*DBSize;
        if defStruct then StructOffset := (StructOffset+i2) and $FFFF
        else begin
DEFSFill:
          if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
            asm
              mov [errorNum], -46 // "instruction violates page bounds"
              call  GetCurrentPage
            end;
            if not PageValid then goto NextLine;
          end;
          if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
          if (AsmAddr+i2) <= CurPage.Z80End then begin
            p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
            if DBSize = 1 then AddDbgInfo(AsmAddr, i2, DBG_BYTE)
            else AddDbgInfo(AsmAddr, i2, DBG_WORD);
            for i := 0 to i2-1 do PByteArray(p2)[i] := valSet;
            inc(totalBytes, i2);
            inc(AsmAddr, i2);
            inc(AsmVPtr, i2);
            if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
          end else begin
            while i2 > 0 do begin
              if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
                asm
                  mov [errorNum], -46 // "instruction violates page bounds"
                  call  GetCurrentPage
                end;
                if not PageValid then goto NextLine;
              end;
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              i3 := CurPage.Z80End - AsmAddr;
              if i3 > i2 then i3 := i2;
              p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
              if DBSize = 1 then AddDbgInfo(AsmAddr, i3, DBG_BYTE)
              else AddDbgInfo(AsmAddr, i3, DBG_WORD);
              for i5 := 0 to i3-1 do PByteArray(p2)[i5] := valSet;
              inc(AsmAddr, i3);
              inc(AsmVPtr, i3);
              inc(totalBytes, i3);
              dec(i2, i3);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end;
        end;
        goto nextLine;
        end;

      175: // REP, REPEAT, REPT
        begin
        asm
        cmp [LineRepCount], 0
        mov edx, -66 // "cannot nest single-line repeats"
        jne @repError
        mov eax, [InsStart]
        mov ecx, eax
        add eax, [FWLen]
        add ecx, [InsLen]
        mov [InsEnd], ecx
        call  AsmSkipWS
        mov edx, -47
        je  @repError

        mov cl, 02
        call  AsmGetExpr
        mov [InstrVal], eax
        jnc @RepValOK
@repError:
        mov [ErrorNum], edx
        push  eax
        call  AsmErrorReturn
        cmp dword [ErrorNum], -07
        pop eax
        jne nextLine
        inc dword [NumSymbolErrors]
        jmp nextLine
@repValOK:
        mov eax, [ExprStart]
        cmp [InstrVal], 1
        mov edx, -64 // "invalid repeat count"
        jc  @repError
        cmp [InstrVal], 65537
        jnc  @repError
        add eax, [ExprLen]
        cmp eax, [InsEnd]
        jnc @SetMultilineRepeat
        cmp byte [eax], ','
        je  @SetSingleLineRepeat
        call  AsmSkipWS
        je  @SetMultiLineRepeat
        mov edx, -48 // "garbage following instruction"
        jmp @repError
@SetSingleLineRepeat:
        inc eax
        call  AsmSkipWS
        mov edx, -65 // "bad repeat statement"
        mov ecx, [InsEnd]
        je  @repError
        mov [InsStart], eax
        sub ecx, eax
        mov edx, [InstrVal]
        mov [LineRepStart], eax
        dec edx
        mov [InsLen], ecx
        mov [LineRepCount], edx
        push  ebx
        push  esi
        push  edi
        mov esi, eax
        mov ecx, [InsEnd]
        mov bx, $0920
        lea edx, OpcodeASCIICodes
        lea edi, Instruction+1
        xor eax, eax
@getInsFirstWord:
        cmp esi, ecx
        jnc @gotFirstWord
        mov al, [esi]
        mov al, [edx+eax*2]
        and al, al
        je  @gotFirstWord
        cmp al, bl
        je  @gotFirstWord
        mov [edi], al
        inc esi
        inc edi
        jmp @getInsFirstWord
@gotFirstWord:
        mov eax, esi
        mov ecx, [InsLen]
        mov [InsWord2], esi
        sub eax, [InsStart]
        sub ecx, eax
        mov [FWLen], eax
        mov [InsLen2], ecx

        pop edi
        pop esi
        pop ebx
        jmp ProcessInstruction
@SetMultiLineRepeat:
        end;
        if repLevel = 63 then begin
          AddErrorQuick(-67, Line, FileNum); // "rep stack overflow"
        end else begin
          repStack[repLevel].repLine := Line;
          repStack[repLevel].repCount := InstrVal;
          inc(RepLevel);
        end;
        goto NextLine;
        end;

      176: // ENDR, ENDREPEAT, ENDREP
        begin
          if FWLen <> InsLen then begin
            AddErrorQuick(-48, Line, FileNum); // "garbage following instruction"
          end else if repLevel = repBaseLevel then begin
            AddErrorQuick(-68, Line, FileNum); // "spurious ENDR"
          end else begin
            dec(repStack[repLevel-1].repCount);
            if repStack[repLevel-1].repCount > 0 then begin
              Line := repStack[repLevel-1].repLine;
            end else begin
              dec(repLevel);
            end;
          end;
          goto nextLine;
        end;

      02: // ALIGN
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        add eax, [FWLen]
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [exprStart], eax
        mov ecx, [InsEnd]
        je  AsmErrorNoPush
        sub ecx, eax
        mov [exprLen], ecx
        call doEval
        bt  edx, 31
        mov [InstrVal], eax
        jnc @ALIGN_OK
@ALIGN_Err:
        bt  edx, 30
        mov edx, -50 // "expression must evaluate"
        jnc @ALIGN_Err2
        mov edx, -07 // "symbol undefined"
@ALIGN_Err2:
        jmp AsmErrorNoPush
@ALIGN_OK:
        end;
        if InstrVal <= 0 then begin
          AddErrorQuick(-62, Line, FileNum); // "invalid ALIGN value"
          goto nextLine
        end else if defStruct then begin
          if (StructOffset mod InstrVal) > 0 then inc(StructOffset, InstrVal-(StructOffset mod InstrVal));
        end else if (AsmAddr mod InstrVal) > 0 then begin
          i2 := InstrVal-(AsmVPtr mod InstrVal);
          goto DEFSFill;
        end;
        goto NextLine;
        end;

      32: // END
        goto NextFile;

      else if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
EncodeInstruction:
        asm
        mov eax, [FirstWord]
        push  ebx
        push  esi
        push  edi
        mov [regEBX], ebx
        mov [regESI], esi
        mov [regEDI], edi
        cmp eax, -1
        mov edx, -09 // "unrecognised instruction"
        mov dword [exprStart], 0
        je  AsmError
        mov ecx, [InsLen]
        mov edx, [FWLen]
        mov esi, [InsStart]
        lea edi, Instruction+1
        add ecx, esi
        mov [InsEnd], ecx
        add esi, edx
        add edi, edx
        lea edx, OpcodeASCIICodes
        mov [LastInsSection], edi
        mov dword [exprMode], 1 shl 21
        mov dword [ParenthLevel], 0
        mov dword [IXDisp], 0
        mov byte [IXSign], -1
        mov byte [ValSet], 0
        cmp eax, 04  // BIT
        je  @bitExpr
        cmp eax, 74  // RES
        je  @bitExpr
        cmp eax, 93  // SET
        jne  @startInstrSection
@bitExpr:
        bts dword [exprMode], 0
@startInstrSection:
        cmp esi, [insEnd]
        jnc @endOfInstr
        mov byte [edi], 32
        xor eax, eax
        mov ebx, $2227
        inc edi
        xor ecx, ecx
        mov [LastInsSection], edi
        mov dword [exprStart], 0
        mov dword [lastWords], 0
        je  @InstrSectionWS
        mov dword [exprStart], edi
@InstrSectionWS:
        mov al, [esi]
        cmp byte [edx+eax*2], 32
        jne @getInstrSectionBody
        inc esi
        cmp esi, [insEnd]
        jc  @InstrSectionWS
        jmp @endOfInstr
@getInstrSectionBody:
        mov [LastSrcSection], esi
@getInstrSectionBody2:
        mov al, [esi]
        and cl, cl
        lea esi, [esi+1] // need to preserve the Z flag
        mov ch, [edx+eax*2]
        jne @inQuote
@getInstrSectionBody3:
        cmp ch, ','
        je @nextInstrSection
        cmp byte [edx+1+eax*2], 1
        je  @alpha
        jc  @checkQuote
@charHasCode:
        push  ecx
        mov cl, [edx+1+eax*2]
        shl dword [LastWords], 8
        mov byte [LastWords], cl
        cmp cl, $FE
        jnc  @numeric
        cmp cl, $F0
        je  @openBracket
        cmp cl, $F1
        je  @closeBracket
        and cl, $FE
        cmp cl, $F2
        jne @noIXDispYet
        mov ecx, [LastWords]
        and ecx, $FFFEFE
        cmp ecx, $F06CF2
        je  @hasIXdisp
        jmp @numeric
@noIXDispYet:
        pop ecx
@checkQuote:
        cmp ch, bl
        je  @setQuote
        cmp ch, bh
        je  @SetQuote
@checkSpace:
        cmp ch, ' '
        mov al, byte [edi-1]
        jne @copyByte
        cmp byte [edi-1], ch
        mov al, [edx+1+eax*2]
        je  @noSetQuote
        cmp al, 1
        jne  @noSetQuote
@copyByte:
        mov byte [edi], ch
        inc edi
@noSetQuote:
        cmp esi, [insEnd]
        jc  @getInstrSectionBody2
        jmp @nextInstrSection
@setQuote:
        cmp dword [ParenthLevel], 0
        mov cl, ch
        jne  @setBrckt3
        bts dword [exprMode], 16
@setBrckt3:
        cmp dword [exprStart], 0
        jne @copyByte
        mov [exprStart], edi
        jmp @copyByte
@inQuote:
        cmp ch, cl
        je  @rmvQuote
        mov ch, al
        jmp @copyByte
@rmvQuote:
        xor cl, cl
        jmp @copyByte
@openBracket:
        mov [LastOpenBracket], edi
        mov ecx, [ParenthLevel]
        inc dword [ParenthLevel]
        and ecx, ecx
        pop ecx
        jne @copyByte
        cmp dword [exprStart], 0
        jne @copyByte
        mov [exprStart], edi
        bts dword [exprMode], 20 // set "expression has brackets" flag
        jmp @copyByte
@closeBracket:
        cmp byte [edi-1], ' '
        mov ecx, [LastWords]
        jne @noSpaceCloseBracket
        dec edi
@noSpaceCloseBracket:
        cmp ch, 192
        setnc ch
        and ecx, $FFFFFFFF
        cmp ecx, $F000F1
        jne @noRegMemAccess
        mov ecx, [exprStart]
        cmp ecx, [LastOpenBracket]
        jne @noRegMemAccess
        mov dword [exprStart], 0  // clear expression pointer
        bts dword [exprMode], 19 // set "register mem access" flag ie. "(HL)"
@noRegMemAccess:
        dec dword [ParenthLevel]
        pop ecx
        jge @copyByte
        btr dword [exprMode], 17
        mov dword [ParenthLevel], 0
        jnc @extraCloseBracket
// (IX+dd) or similar...
        push  edx
        push  ecx
        push  esi
        mov ecx, edi
        mov esi, [exprStart]
        sub ecx, esi
        mov edx, -34 // "invalid index register displacement"
        jle @invIXDisp
        call  @evalExpression
        jc  @gotIXDisp
@gotIXDisp:
        test  byte [IXSign], 01
        je  @noMinusIX
        neg eax
@noMinusIX:
        cmp eax, 128
        mov edx, -04 // "bad index register displacement"
        jge @invIXDisp
        cmp eax, -128
        jl  @invIXDisp
        and eax, $FF
        mov ah, $01
        mov edi, [exprStart]
        mov [IXDisp], eax
        pop esi
        mov dword [edi], $294E4E24
        pop ecx
        pop edx
        lea edi, [edi+4]
        xor eax, eax
        mov dword [exprStart], 0
        mov dword [exprMode], 0
        jmp @noSetQuote
@invIXDisp:
        pop esi
        pop ecx
        lea esp, [esp+4]
        jmp AsmError
@extraCloseBracket:
        mov edx, -39 // "unbalanced parenthesis"
        jmp AsmError
@numeric:
        pop ecx
        cmp dword [ParenthLevel], 0
        jne  @setBrckt
        bts dword [exprMode], 16
@setBrckt:
        cmp dword [exprStart], 0
        lea esi, [esi-1] // need to preserve Z
        jne @num2
        mov [exprStart], edi
@num2:  mov [edi], ch
        inc edi
        cmp esi, [insEnd]
        jnc @gotNum
        inc esi
        mov al, [esi]
        mov ch, [edx+eax*2]
        cmp byte [edx+1+eax*2], 1
        je  @num2
        cmp byte [edx+1+eax*2], $FF
        jnc @num2
@gotNum:
        push  eax
        mov al, $FE
        jmp @gotWordCode
@alpha:
        mov [edi], ch
        lea ebx, [edi]
        dec esi
        inc edi
@alpha2:
        cmp esi, [insEnd]
        jnc @gotWord
        inc esi
        mov al, [esi]
        mov cx, [edx+eax*2]
        cmp ch, $FF
        je  @alpha3
        cmp ch, 1
        je  @alpha3
        cmp al, 39
        jne @gotWord
@alpha3:
        mov [edi], cl
        inc edi
        jmp @alpha2
@gotWord:
        mov ch, cl
        push  eax
        xor cl, cl
        push  edi
        push  ecx
        push  edx
        push  esi
        mov ecx, edi
        sub ecx, ebx
        push  ebx
        push  ecx
        mov edx, ebx
        mov eax, [AsmWordsPtr]
        mov ecx, [NumAsmWords]
        call  IsReservedWord
        pop ebx
        pop esi
        pop edx
        pop ecx
        cmp eax, 0
        jl  @notReservedWord
        pop edi
        cmp al, 04  // BIT
        je  @bitExpr2
        cmp al, 74  // RES
        je  @bitExpr2
        cmp al, 93  // SET
        jne  @gotWordCode
@bitExpr2:
        bts dword [exprMode], 0
        mov [exprStart], edi
@gotWordCode:
        shl dword [LastWords], 8
        mov byte [LastWords], al
        pop eax
        cmp esi, [InsEnd]
        mov ebx, $2227
        jc  @getInstrSectionBody2
        jmp @NextInstrSection
@notReservedWord:
        cmp dword [ParenthLevel], 0
        jne  @setBrckt2
        bts dword [exprMode], 16
@setBrckt2:
        cmp dword [exprStart], 0
        jne @noSetExpr
        mov [exprStart], ebx
@noSetExpr:
        mov al, $FF
        pop edi
        jmp @gotWordCode
@hasIXDisp:
        cmp cl, [edx+1+eax*2]
        mov byte [edi], '+'
        push  edx
        push  esi
        setne [IXSign]
        inc edi
        mov esi, [exprStart]
        mov ecx, [LastOpenBracket]
        and esi, esi
        je  @IXnoPreExpr
        sub ecx, esi
        jle @IXnoPreExpr
        btr dword [exprMode], 0
        mov edx, -24 // "malformed line" ("(IX+dd)" inside brackets)
        jnc @badExprBeforeIX
@evalIXpreExpr:
        call  @evalExpression
        jnc @IXbitNumOK
        mov edx, -37 // "malformed line" (bad bit number expression before "(IX+dd)")
        jmp @badExprBeforeIX
@IXbitNumOK:
        cmp eax, 8
        mov dword [exprMode], 0
        jnc @bitNumError2 // bit number greater than 7
        or  eax, $49282030
        mov edi, [exprStart]
        mov edx, [LastWords]
        lea edi, [edi+6]
        sub dh, 20
        mov byte [edi-6], ' '
        mov [edi-5], eax
        mov [edi-1], dh
@IXnoPreExpr:
        pop esi
        pop edx
        pop ecx
        mov [exprStart], edi
        bts dword [exprMode], 17
        dec dword [ParenthLevel]
        mov eax, 0
        je @noSetQuote
        mov edx, -24 // "malformed line" ("(IX+dd)" inside brackets)
        jmp AsmError
@evalExpression:
        mov [exprLen], ecx
        push  ebx
        push  ecx
        push  esi
        push  edi
        mov ebx, [regEBX]
        mov esi, [regESI]
        mov edi, [regEDI]
        call doEval
        bt  edx, 30
        jnc @SymbolFound
        push  eax
        push  edx
        mov dword [ErrorNum], -07 // "undefined symbol"
        mov ebx, [regEBX]
        mov esi, [regESI]
        mov edi, [regEDI]
        call AEQuick
        pop edx
        pop eax
        inc dword [NumSymbolErrors]
@SymbolFound:
        bt  edx, 31
        pop edi
        pop esi
        pop ecx
        pop ebx
        ret
@badExprBeforeIX:
        pop esi
        lea esp, [esp+4]
        pop ecx
        jmp AsmError
@nextInstrSection:
        cmp byte [edi-1], 32
        jne @SectionTrailSpace
        dec edi
@sectionTrailSpace:
        cmp dword [exprStart], 0
        je  @nextInstrSection2
        bt  dword [exprMode], 0
        jc  @bitNum
        bt  dword [exprMode], 19
        jc  @invOperand
        push  esi
        push  eax
        push  edx
        mov esi, [exprStart]
        mov ecx, edi
        sub ecx, esi
        call  @evalExpression
        jc  @expressionOK
        mov byte [ValSet], 1
@expressionOK:
        bt  edx, 31
        jc  @invalidExprChar
@notInvalidExprChar:
        cmp [FirstWord], 43
        mov [InstrVal], eax
        je  @IMInstruction
        bt  dword [exprMode], 16
        mov edi, [exprStart]
        jc  @noBracket1
        mov byte [edi],'('
        inc edi
@noBracket1:
        mov dword [edi], 'NN$'
        bt  dword [exprMode], 16
        lea edi, [edi+3]
        jc  @noBracket2
        mov byte [edi],')'
        inc edi
@noBracket2:
        pop edx
        pop eax
        pop esi
@nextInstrSection2:
        cmp esi, [insEnd]
        jnc @endOfInstr
        mov byte [edi], ','
        mov dword [exprMode], 0
        inc edi
        jmp @startInstrSection
@IMInstruction:
        cmp eax, 3
        mov edi, [exprStart]
        jnc @InvalidIntMode
        add al, 48
        mov [edi], al
        inc edi
        jmp @noBracket2
@bitNum:
        cmp [FirstWord], 93
        push  esi
        push  eax
        push  edx
        jne @notSETDirective
        cmp esi, [InsEnd]
        jnc @doSETDirective
@notSETDirective:
        mov esi, [exprStart]
        mov ecx, edi
        sub ecx, esi
        call  @evalExpression
        jc  @bitNumOK
@bitNumOK:
        cmp eax, 8
        mov edi, [exprStart]
        jnc @bitNumError2
        or  al, $30
        mov [edi], al
        inc edi
        jmp @noBracket2
@doSETDirective:
        lea esp, [esp+12]
        pop edi
        pop esi
        pop ebx
        jmp doEQU_SET
@bitNumError2:
        pop edx
        pop eax
        pop esi
        jmp @bitNumError
@bitNumError1:
        pop esi
        pop edx
        pop ecx
@bitNumError:
        mov edx, -36 // "malformed operand" (bit number greater than 7)
        jmp AsmError
@invalidExprChar:
        bt  edx, 30
        jc  @notInvalidExprChar
        pop esi
        pop edx
        pop ecx
        mov edx, -32 // "bad expression"
        jmp AsmError
@invalidIntMode:
        pop esi
        pop edx
        pop ecx
        mov edx, -42 // "invalid interrupt mode"
        jmp AsmError
@invOperand:
        mov edx, -24 // "malformed line" (contains "(HL)" and an expression)
        jmp AsmError
@doEval:
        jmp @nextInstrSection
@checkInstrOneWord:
        xor edx, edx
        lea eax, Instruction+1
        mov ecx, [FWLen]
        mov dl, [eax-1]
        mov bl, [eax+ecx]
        mov [eax-1], cl
        add edx, eax
        mov [edx], bl
        mov byte [eax+ecx], 0
        push  edx
        call  IsInstruction
        xor ebx, ebx
        pop edx
        lea ecx, Instruction+1
        mov bl, [edx]
        mov byte [edx], 0
        sub edx, ecx
        mov dh, bl
        mov bl, [ecx-1]
        mov [ecx-1], dl
        mov [ecx+ebx], dh
        cmp eax, 0
        mov edx, -40 // "instruction takes no operands"
        jge AsmError
        mov edx, -41 // "invalid combination of opcode and operands"
        jmp AsmError
@insTypeJmpTable:
        dd  @NormalOpcodes, @CBOpcodes, @DDOpcodes, @DDCBOpcodes
        dd  @EDOpcodes, @FDOpcodes, @FDCBOpcodes
@NormalOpcodes:
        mov [edi], al
        inc edi
        cmp al, 199
        mov ebx, [InstrVal]
        jne @AddExtraBytes
        cmp ebx, 64
        mov edx, -06
        jnc AsmError
        mov bh, bl
        and bh, 7
        jne AsmError
        add al, bl
        mov [edi-1], al
        jmp @CopyInstruction
@AddExtraBytes:
        lea esi, [Opcodes]
        mov esi, [esi+eax*4]
        mov ah, [esi]
        cmp ah, '='
        je  @addNNNN
        cmp ah, '@'
        je  @addNNNN
        cmp ah, '%'
        je  @addNNNN
        cmp ah, '&'
        je  @addNNNN
        cmp ah, '!'
        je  @addNN
        cmp ah, '<'
        je  @addNN
        cmp ah, '>'
        je  @addNN
        cmp ah, '?'
        je  @addDD
        cmp ah, '/'
        je  @addDD
        cmp ah, '#'
        jne @CopyInstruction
@addRelative:
        mov ecx, [self]
        mov ecx, [ecx].TZ80Assembler.AsmVPtr
        add ecx, 2
        and ecx, $FFFF
        cmp byte [ValSet], 0
        je  @noRelSymbol
        mov ecx, [InstrVal]
@noRelSymbol:
        mov ebx, [self]
        mov ebx, [ebx].TZ80Assembler.AsmVPtr
        add bx, 2
        sub cx, bx
        cmp cx, 127
        jg  @RelError
        cmp cx, -128
        jl  @RelError
        mov byte [edi], cl
        inc edi
        jmp @CopyInstruction
@RelError:
        mov edx, -02 // "jump out of range"
        jmp AsmError // fix this later - currently jumps to next line, should continue processing
@addDD: mov ch, byte [IXDisp]
        inc edi
        cmp ah, '?'
        mov [edi-1], ch
        je  @CopyInstruction
@addNN: mov ecx, [InstrVal]
        inc edi
        cmp ecx, -128
        jnc @NNok
        cmp ecx, 256
        jnc @errorNotByte
@NNok:  mov byte [edi-1], cl
        jmp @CopyInstruction
@errorNotByte:
        mov edx, -43 // "byte value out of range"
        jmp AsmError
@addNNNN:
        mov ecx, [InstrVal]
        lea edi, [edi+2]
        cmp ecx, -32768
        jnc @NNNNok
        cmp ecx, 65536
        jnc @errorNotWord
@NNNNok:
        mov word [edi-2], cx
        jmp @CopyInstruction
@errorNotWord:
        mov edx, -44 // "byte value out of range"
        jmp AsmError
@CBOpcodes:
        mov byte [edi], $CB
        mov [edi+1], al
        lea edi, [edi+2]
        jmp @CopyInstruction
@DDOpcodes:
        mov byte [edi], $DD
        jmp @DDFDOpcodes
@FDOpcodes:
        mov byte [edi], $FD
@DDFDOpcodes:
        mov [edi+1], al
        lea edi, [edi+2]
        jmp @AddExtraBytes
@DDCBOpcodes:
        mov ecx, $DDCB0000
        jmp @DDFDCBOpcodes
@FDCBOpcodes:
        mov ecx, $FDCB0000
@DDFDCBOpcodes:
        mov ch, byte [IXDisp]
        mov cl, al
        bswap  ecx
        mov [edi], ecx
        lea edi, [edi+4]
        jmp @CopyInstruction
@EDOpcodes:
        mov byte [edi], $ED
        mov [edi+1], al
        lea edi, [edi+2]
        jmp @AddExtraBytes
@endOfInstr:
        lea eax, Instruction+1
        mov ecx, edi
        sub ecx, eax
        mov byte [edi], 0
        mov [eax-1], cl
        call  IsInstruction
        cmp eax, -1
        mov ebx, eax
        je  @checkInstrOneWord
        lea edi, TempStr+1
        shr ebx, 8
        mov ecx, dword [@insTypeJmpTable+ebx*4]
        mov esi, edi
        jmp  ecx
@CopyInstruction:
        lea esi, TempStr+1
        mov ebx, edi
        sub ebx, esi
        mov [esi-1], bl
        pop edi
        pop esi
        pop ebx
        end;
      end;
    end;
    if (SymLen > 0) and (FirstWord = 93) then begin
      InstrVal := AsmVPtr;
      FirstWord := 62; // code for NOP
      asm
      call  AddSymbol
      end;
    end;
    if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then asm
      mov [errorNum], -46 // "instruction violates page bounds"
      call  GetCurrentPage
    end;
    if PageValid then begin
      p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
      i2 := Length(TempStr);
      if (AsmAddr+i2) <= CurPage.Z80End then begin
        if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
        for i := 1 to Length(TempStr) do
          PByteArray(p2)[i-1] := ord(TempStr[i]);
        inc(totalBytes, i2);
        inc(AsmAddr, i2);
        inc(AsmVPtr, i2);
        if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
      end else begin
        i4 := 1;
        while i2 > 0 do begin
          if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then asm
            mov [errorNum], -46 // "instruction violates page bounds"
            call  GetCurrentPage
          end;
          if not PageValid then goto NextLine;
          if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
          i3 := CurPage.Z80End - AsmAddr;
          if i3 > i2 then i3 := i2;
          p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
          for i5 := 0 to i3-1 do PByteArray(p2)[i5] := ord(TempStr[i4+i5]);
          inc(i4, i3);
          inc(AsmAddr, i3);
          inc(AsmVPtr, i3);
          inc(totalBytes, i3);
          dec(i2, i3);
          if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
        end;
      end;
    end;
noInstr:
    end;
nextLine:
    if Assigned(afterLine) then begin
      TempLineLen := AsmVPtr-AsmLineAddr;
      afterLine(self, AsmLineAddr, TempLineLen, oldPage,
      SourceFilenames.Count-1, Line);
    end;
    if LineRepCount > 0 then begin
      dec(LineRepCount);
      goto ProcessInstruction;
    end else inc(Line);
  end;
nextFile:
  if (IfLevel > 0) or InIf then AddErrorQuick(-19, Line, FileNum)
  else if (repLevel > repBaseLevel) then AddErrorQuick(-63, Line, FileNum)
  else if SourceFilenames.Count > 1 then begin
    repBaseLevel := repBase[SourceFilenames.Count-1];
    Line := Integer(SourceFilenames.Objects[SourceFilenames.Count-1])+1;
    SourceFilenames.Delete(SourceFilenames.Count-1);
    i := SourceFiles.IndexOf(SourceFilenames[SourceFilenames.Count-1]);
    Source := TStringList(SourceFiles.Objects[i]);
    goto AssembleSourceFile
  end;
done:
  for i := 0 to 31 do AsmSymbols[i].Sorted := True;
  result := ((NumSymbolErrors > 0) and (NumSymbolErrors <> LastSymErrors))
    and (NumSymbolErrors = NumErrors);
  LastNumErrors := NumErrors;
  LastSymErrors := NumSymbolErrors;
  if not result and (NumErrors > NumSymbolErrors) then begin
    i := 0;
    i2 := 0;
    while i2 < NumErrors shl 2 do begin
      p := PAsmError(@QErrors[i]);
      p2 := PAsmError(@QErrors[i2]);
      if PAsmError(p2).ErrorNum = -07 then begin
      end else begin
        if i <> i2 then begin
          PAsmError(p).ErrorNum := PAsmError(p2).ErrorNum;
          PAsmError(p).ErrorLine := PAsmError(p2).ErrorLine;
          PAsmError(p).SourceFileNum := PAsmError(p2).SourceFileNum;
          PAsmError(p).null := PAsmError(p2).null;
        end;
        inc(i,4);
      end;
      inc(i2,4);
    end;
    dec(NumErrors, NumSymbolErrors);
  end;
  SetLength(QErrors, NumErrors shl 2);
  if Assigned(atEndOfPass) then atEndOfPass(self);
end;

end.


{unit Z80Assembler;

interface

uses
  Windows, Classes, SysUtils, Math;

type
  PDWordArray = ^TDWordArray;
  TDWordArray = array[0..8191] of DWord;

  TZ80MemBlock = class
    Name: ShortString;
    Z80Start: DWord;
    Z80End: DWord;
    StartAddress: Pointer;
    AltLo: Integer;
    AltHi: Integer;
    Tag: Integer;
  end;
  TAsmMacroRecord = Packed Record
    Source: TStringList;
    Line: Integer;
  end;
  PAsmWord = ^TAsmWord;
  TAsmRepRecord = Packed Record
    repLine: Integer;
    repCount: Integer;
  end;
  TAsmWord = packed record
    S: String;
    I: Integer;
  end;
  PAsmError = ^TAsmError;
  TAsmError = packed record
    ErrorNum: Integer;
    ErrorLine: Integer;
    SourceFileNum: Integer;
    null: Integer;
  end;
  PAsmDbgLine = ^TAsmDbgLine;
  TAsmDbgLine = packed record
    StartAddress: DWord; // (macro: index of next macro with same name)
    SetData: Word; // flags
    SrcFileNum: Word; // source file number
    Page: DWord;
    SymbolNum: Integer;
    Len: Integer;
    res1, res2, res3: Integer;
  end;
  PAsmSymbol = ^TAsmSymbol;
  TAsmSymbol = packed record
    StartAddress: DWord; // (macro: index of next macro with same name)
    SetData: Word; // flags
    SrcFileNum: Word; // source file number
    Page: DWord;  // Pointer to TZ80MemBlock containing symbol
                  //(macro: bits 16-31: (WORD) Source File Number)
                  //(macro: bits 08-15: (BYTE) Highest number of args)
                  //(macro: bits 00-07: (BYTE) Lowest number of args)
    DefinedAt: DWord; // Pointer to symbol definition (macro: Integer = Source line number)
  end;
  PAsmMacro = ^TAsmSymbol;
  TAsmMacro = packed record
    nextMacro: DWord; // (macro: index of next macro with same name)
    SetData: DWord; // flags
    srcFileNum: Word; // source file number
    loArgs: Byte; // Lowest number of args;
    hiArgs: Byte; // Highest number of args;
    lineNum: Integer; // Source line number;
  end;
  TAsmSrcFile = record
    src: TStringList;
    filename: String;
  end;
  TAsmCallback = procedure(sender: TObject; Addr: Word; var Len: Word; page: TZ80MemBlock; srcFile, srcLine: Integer);
  TZ80Assembler = class(TObject)
    DEFSDefault: Byte;
    val1: Integer;
    resultCode: Integer;
    PageList: Array of TZ80MemBlock;
    AsmSymbols: Array[0..31] of TStringList;
    AsmDbgLines: Array of TAsmDbgLine;
    AsmSymbolsExt: Array of Integer;
    NumAsmSymbols: Integer;
    NumDbgLines: Integer;
    SourceFiles: TStringList;
    SourceFilenames: TStringList;
    CurPage: TZ80MemBlock;
    oldPage: TZ80MemBlock;
    CurPageName: String;
    AsmAddr: Integer;
    AsmLineAddr: DWord;
    CreateExtraInfo: Boolean;
    NumErrors: Integer;
    Errors: TStringList;
    QErrors: Array of Integer;
    srcFiles: Array of TAsmSrcFile;
    PassNum: Integer;
    totalBytes: Integer;
    DefaultPage: TZ80MemBlock;
    LastNumErrors, LastSymErrors: Integer;
    LastSymbolDef: Pointer;
    LastSymbolDefLen: Integer;
    atStartOfPass, atEndOfPass: TNotifyEvent;
    afterLine: TAsmCallback;

    Constructor Create;
    Destructor  Destroy;

    Procedure ResetMemBlocks;
    procedure SetMem48(buffer: Pointer);
    function AddMem(Name: ShortString; Start, Size: Integer;
      buffer: Pointer): TZ80MemBlock;
    procedure AddTZ80MemBlock(m: TZ80MemBlock);
    function  GetMemParams(Name: ShortString; var AltLo, AltHi: DWord): Pointer;
    function  evaluate(S: Pointer; c: Integer): Integer;
    Function  Assemble(Source: TStringList; Name: ShortString; StartAddress: Integer;
      StartPage: ShortString): Integer;
    Function  doPass(Source: TStringList; var Name: ShortString; StartAddress: Integer;
      var StartPage: ShortString; Pass: Integer): Boolean;
    procedure eval1(S: String; var V, Code: Integer);
    function FindChar(S: String; C, A, Q: Char; P: Integer): Integer;
    Function ReadMemByte(Address: Word): Byte;
    Function ReadMemWord(Address: Word): Word;
    procedure AddErrorQuick(errorNum, Line: Integer; FileNum: Integer);
    function GetErrorString(index: Integer): String;
    function GetError(index: Integer; var Line: Integer; var FileName: String;
      var ErrorType: Integer; var Text: String): Boolean;
    function retFileSize(Filename: String):Integer;
    function GetSymbol(S: ShortString; var value: DWord): Boolean;
    procedure AddSource(source: TStringList; filename: String);
    function RemoveSource(filename: String): boolean;
  end;

  function isInstruction(S: PChar): Integer;

Const

  SYM_SET     = $8000;
  SYM_DEAD    = $4000;
  SYM_MACRO   = $2000;

  DBG_BYTE    = $1000;
  DBG_WORD    = $2000;
  DBG_TEXT    = $4000;

// assembler reserved words. these can't be used for label/macro definitions etc.
// (S: text; I: IDCode)
// must be sorted alphabetically by S
// Assembler acts on IDCode, not index, in case we want to insert a word later
// next free code = 178
  AsmWords: array [0..185] of TAsmWord = (
    (S: '(BC)'; I: 138), (S: '(C)'; I: 157),
    (S: '(DE)'; I: 139), (S: '(E)'; I: 158),
    (S: '(HL)'; I: 140), (S: '(SP)'; I: 141),
    (S: '.IF'; I: 170),
    (S: 'A'; I: 120),
    (S: 'A'#39; I: 128), (S: 'ADC'; I: 172),
    (S: 'ADD'; I: 1), (S: 'AF'; I: 112),
    (S: 'AF'#39; I: 116), (S: 'ALIGN'; I: 2),
    (S: 'AND'; I: 3), (S: 'AT'; I: 183), (S: 'B'; I: 122),
    (S: 'B'#39; I: 130), (S: 'BC'; I: 113),
    (S: 'BC'#39; I: 117), (S: 'BIT'; I: 4),
    (S: 'BSS'; I: 5), (S: 'C'; I: 123),
    (S: 'C'#39; I: 131), (S: 'CALL'; I: 6),
    (S: 'CCF'; I: 7), (S: 'CODE'; I: 8),
    (S: 'CP'; I: 9), (S: 'CPD'; I: 10),
    (S: 'CPDR'; I: 11), (S: 'CPI'; I: 12),
    (S: 'CPIR'; I: 13), (S: 'CPL'; I: 14),
    (S: 'D'; I: 124), (S: 'D'#39; I: 132),
    (S: 'DAA'; I: 15), (S: 'DATA'; I: 16),
    (S: 'DB'; I: 20), (S: 'DD'; I: 18),
    (S: 'DE'; I: 114), (S: 'DE'#39; I: 118),
    (S: 'DEC'; I: 19), (S: 'DEF'; I: 171),
    (S: 'DEFB'; I: 20), (S: 'DEFINE'; I: 150),
    (S: 'DEFM'; I: 21), (S: 'DEFS'; I: 22),
    (S: 'DEFSW'; I: 173),
    (S: 'DEFW'; I: 23), (S: 'DI'; I: 24),
    (S: 'DISABLE'; I: 25), (S: 'DJNZ'; I: 26),
    (S: 'DS'; I: 27), (S: 'DW'; I: 23),
    (S: 'E'; I: 125), (S: 'E'#39; I: 133),
    (S: 'EI'; I: 29), (S: 'ELIF'; I: 165),
    (S: 'ELSE'; I: 30), (S: 'ENABLE'; I: 31),
    (S: 'END'; I: 32), (S: 'ENDIF'; I: 33),
    (S: 'ENDM'; I: 169),
    (S: 'ENDR'; I: 176), (S: 'ENDREP'; I: 176), (S: 'ENDREPEAT'; I: 176),
    (S: 'ENDSTRUC'; I: 174),
    (S: 'ENDSTRUCT'; I: 174), (S: 'EQU'; I: 34),
    (S: 'EX'; I: 35), (S: 'EXX'; I: 36),
    (S: 'F'; I: 121), (S: 'F'#39; I: 129),
    (S: 'FCB'; I: 37), (S: 'FCC'; I: 38),
    (S: 'FCW'; I: 39), (S: 'FDB'; I: 40),
    (S: 'H'; I: 126), (S: 'H'#39; I: 134),
    (S: 'HALT'; I: 41), (S: 'HL'; I: 115),
    (S: 'HL'#39; I: 119), (S: 'I'; I: 136),
    (S: 'IF'; I: 42), (S: 'IFDEF'; I: 163),
    (S: 'IFEQ'; I: 166), (S: 'IFNDEF'; I: 164),
    (S: 'IFNE'; I: 167), (S: 'IM'; I: 43),
    (S: 'IN'; I: 44), (S: 'INC'; I: 45),
    (S: 'INCBIN'; I: 149), (S: 'INCLUDE'; I: 46),
    (S: 'IND'; I: 47), (S: 'INDR'; I: 48),
    (S: 'INI'; I: 49), (S: 'INIR'; I: 50),
    (S: 'IX'; I: 108), (S: 'IXH'; I: 153),
    (S: 'IXL'; I: 154), (S: 'IY'; I: 109),
    (S: 'IYH'; I: 155), (S: 'IYL'; I: 156),
    (S: 'JP'; I: 51), (S: 'JR'; I: 52),
    (S: 'L'; I: 127), (S: 'L'#39; I: 135),
    (S: 'LD'; I: 53), (S: 'LDD'; I: 54),
    (S: 'LDDR'; I: 55), (S: 'LDI'; I: 56),
    (S: 'LDIR'; I: 57), (S: 'LIST'; I: 58),
    (S: 'LOAD'; I: 177),
    (S: 'M'; I: 142), (S: 'MACRO'; I: 168),
    (S: 'NC'; I: 146), (S: 'NDEF'; I: 172),
    (S: 'NEG'; I: 59), (S: 'NOLIST'; I: 60),
    (S: 'NOOPT'; I: 61), (S: 'NOP'; I: 62),
    (S: 'NZ'; I: 144), (S: 'OFFSET'; I: 183), (S: 'OPT'; I: 63),
    (S: 'OR'; I: 64), (S: 'ORG'; I: 65),
    (S: 'OTDR'; I: 66), (S: 'OTIR'; I: 67),
    (S: 'OUT'; I: 68), (S: 'OUTD'; I: 69),
    (S: 'OUTI'; I: 70), (S: 'P'; I: 143),
    (S: 'PAGE'; I: 71), (S: 'PC'; I: 111),
    (S: 'PE'; I: 148), (S: 'PO'; I: 147),
    (S: 'POP'; I: 72), (S: 'PUSH'; I: 73),
    (S: 'R'; I: 137),
    (S: 'REP'; I: 175), (S: 'REPEAT'; I: 175), (S: 'REPT'; I: 175),
    (S: 'RES'; I: 74),
    (S: 'RESB'; I: 22), (S: 'RESW'; I: 173),
    (S: 'RET'; I: 75), (S: 'RETI'; I: 76),
    (S: 'RETN'; I: 77), (S: 'RIM'; I: 78),
    (S: 'RL'; I: 79), (S: 'RLA'; I: 80),
    (S: 'RLC'; I: 81), (S: 'RLCA'; I: 82),
    (S: 'RLD'; I: 83), (S: 'RMB'; I: 84),
    (S: 'RR'; I: 85), (S: 'RRA'; I: 86),
    (S: 'RRC'; I: 87), (S: 'RRCA'; I: 88),
    (S: 'RRD'; I: 89), (S: 'RST'; I: 90),
    (S: 'SBC'; I: 91), (S: 'SCF'; I: 92),
    (S: 'SET'; I: 93),
    (S: 'SHL'; I: 94), (S: 'SHR'; I: 95),
    (S: 'SIM'; I: 96), (S: 'SL'; I: 97),
    (S: 'SLA'; I: 98), (S: 'SLL'; I: 152),
    (S: 'SP'; I: 110), (S: 'SR'; I: 99),
    (S: 'SRA'; I: 100), (S: 'SRL'; I: 101),
    (S: 'STC'; I: 102), (S: 'STRUCT'; I: 103),
    (S: 'SUB'; I: 104), (S: 'TITLE'; I: 105),
    (S: 'TSTI'; I: 106), (S: 'XH'; I: 159),
    (S: 'XL'; I: 160), (S: 'XOR'; I: 107),
    (S: 'YH'; I: 161), (S: 'YL'; I: 162),
    (S: 'Z'; I: 145));

// Opcode list:
// Opcodes  0-255:      Normal Instructions ie. no prefix
//          256-511:    CB instructions
//          512-767:    DD instructions
//          768-1023:   DD CB instructions
//          1024-1279:  ED instructions
//          1280-1535:  FD instructions
//          1536-1791:  FD CB instructions
	Opcodes: Array [0..1791] of String = (
    // Opcodes  0-255: Normal Instructions ie. no prefix
		'NOP', '@LD BC, $NNNN', 'LD (BC), A', 'INC BC',
		'INC B', 'DEC B', '!LD B, $NN', 'RLCA',
		'EX AF, AF'#39, 'ADD HL, BC', 'LD A, (BC)', 'DEC BC',
		'INC C', 'DEC C', '!LD C, $NN', 'RRCA',
		'#DJNZ $$$+e', '@LD DE, $NNNN', 'LD (DE), A', 'INC DE',
		'INC D', 'DEC D', '!LD D, $NN', 'RLA',
		'#JR $$$+2', 'ADD HL, DE', 'LD A, (DE)', 'DEC DE',
		'INC E', 'DEC E', '!LD E, $NN', 'RRA',
		'#JR NZ, $$$+2', '@LD HL, $NNNN', '%LD ($NNNN), HL', 'INC HL',
		'INC H', 'DEC H', '!LD H, $NN', 'DAA',
		'#JR Z, $$$+2', 'ADD HL, HL', '%LD HL, ($NNNN)', 'DEC HL',
		'INC L', 'DEC L', '!LD L, $NN', 'CPL',
		'#JR NC, $$$+2', '@LD SP, $NNNN', '&LD ($NNNN), A', 'INC SP',
		'INC (HL)', 'DEC (HL)', '!LD (HL), $NN', 'SCF',
		'#JR C, $$$+2', 'ADD HL, SP', '&LD A, ($NNNN)', 'DEC SP',
		'INC A', 'DEC A', '!LD A, $NN', 'CCF',
		'LD B, B', 'LD B, C', 'LD B, D', 'LD B, E',
		'LD B, H', 'LD B, L', 'LD B, (HL)', 'LD B, A',
		'LD C, B', 'LD C, C', 'LD C, D', 'LD C, E',
		'LD C, H', 'LD C, L', 'LD C, (HL)', 'LD C, A',
		'LD D, B', 'LD D, C', 'LD D, D', 'LD D, E',
		'LD D, H', 'LD D, L', 'LD D, (HL)', 'LD D, A',
		'LD E, B', 'LD E, C', 'LD E, D', 'LD E, E',
		'LD E, H', 'LD E, L', 'LD E, (HL)', 'LD E, A',
		'LD H, B', 'LD H, C', 'LD H, D', 'LD H, E',
		'LD H, H', 'LD H, L', 'LD H, (HL)', 'LD H, A',
		'LD L, B', 'LD L, C', 'LD L, D', 'LD L, E',
		'LD L, H', 'LD L, L', 'LD L, (HL)', 'LD L, A',
		'LD (HL), B', 'LD (HL), C', 'LD (HL), D', 'LD (HL), E',
		'LD (HL), H', 'LD (HL), L', 'HALT', 'LD (HL), A',
		'LD A, B', 'LD A, C', 'LD A, D', 'LD A, E',
		'LD A, H', 'LD A, L', 'LD A, (HL)', 'LD A, A',
		'ADD A, B', 'ADD A, C', 'ADD A, D', 'ADD A, E',
		'ADD A, H', 'ADD A, L', 'ADD A, (HL)', 'ADD A, A',
		'ADC A, B', 'ADC A, C', 'ADC A, D', 'ADC A, E',
		'ADC A, H', 'ADC A, L', 'ADC A, (HL)', 'ADC A, A',
		'SUB B', 'SUB C', 'SUB D', 'SUB E',
		'SUB H', 'SUB L', 'SUB (HL)', 'SUB A',
		'SBC A, B', 'SBC A, C', 'SBC A, D', 'SBC A, E',
		'SBC A, H', 'SBC A, L', 'SBC A, (HL)', 'SBC A, A',
		'AND B', 'AND C', 'AND D', 'AND E',
		'AND H', 'AND L', 'AND (HL)', 'AND A',
		'XOR B', 'XOR C', 'XOR D', 'XOR E',
		'XOR H', 'XOR L', 'XOR (HL)', 'XOR A',
		'OR B', 'OR C', 'OR D', 'OR E',
		'OR H', 'OR L', 'OR (HL)', 'OR A',
		'CP B', 'CP C', 'CP D', 'CP E',
		'CP H', 'CP L', 'CP (HL)', 'CP A',
		'RET NZ', 'POP BC', '=JP NZ, $$$+3', '=JP $$$+3',
		'@CALL NZ, $NNNN', 'PUSH BC', '!ADD A, $NN', '.RST $NN',
		'RET Z', 'RET', '=JP Z, $$$+3', '',
		'@CALL Z, $NNNN', '@CALL $NNNN', '!ADC A, $NN', '.RST $NN',
		'RET NC', 'POP DE', '=JP NC, $$$+3', '>OUT ($NN), A',
		'@CALL NC, $NNNN', 'PUSH DE', '<SUB $NN', '.RST $NN',
		'RET C', 'EXX', '=JP C, $$$+3', '!IN A, ($NN)',
		'@CALL C, $NNNN', '', '!SBC A, $NN', '.RST $NN',
		'RET PO', 'POP HL', '=JP PO, $$$+3', 'EX (SP), HL',
		'@CALL PO, $NNNN', 'PUSH HL', '<AND $NN', '.RST $NN',
		'RET PE', 'JP (HL)', '=JP PE, $$$+3', 'EX DE, HL',
		'@CALL PE, $NNNN', '', '<XOR $NN', '.RST $NN',
		'RET P', 'POP AF', '=JP P, $$$+3', 'DI',
		'@CALL P, $NNNN', 'PUSH AF', '<OR $NN', '.RST $NN',
		'RET M', 'LD SP, HL', '=JP M, $$$+3', 'EI',
		'@CALL M, $NNNN', '', '<CP $NN', '.RST $NN',

    // Opcodes 256-511: CB instructions
		'RLC B', 'RLC C', 'RLC D', 'RLC E',
		'RLC H', 'RLC L', 'RLC (HL)', 'RLC A',
		'RRC B', 'RRC C', 'RRC D', 'RRC E',
		'RRC H', 'RRC L', 'RRC (HL)', 'RRC A',
		'RL B', 'RL C', 'RL D', 'RL E',
		'RL H', 'RL L', 'RL (HL)', 'RL A',
		'RR B', 'RR C', 'RR D', 'RR E',
		'RR H', 'RR L', 'RR (HL)', 'RR A',
		'SLA B', 'SLA C', 'SLA D', 'SLA E',
		'SLA H', 'SLA L', 'SLA (HL)', 'SLA A',
		'SRA B', 'SRA C', 'SRA D', 'SRA E',
		'SRA H', 'SRA L', 'SRA (HL)', 'SRA A',
		'SLL B', 'SLL C', 'SLL D', 'SLL E',
		'SLL H', 'SLL L', 'SLL (HL)', 'SLL A',
		'SRL B', 'SRL C', 'SRL D', 'SRL E',
		'SRL H', 'SRL L', 'SRL (HL)', 'SRL A',
		'BIT 0, B', 'BIT 0, C', 'BIT 0, D', 'BIT 0, E',
		'BIT 0, H', 'BIT 0, L', 'BIT 0, (HL)', 'BIT 0, A',
		'BIT 1, B', 'BIT 1, C', 'BIT 1, D', 'BIT 1, E',
		'BIT 1, H', 'BIT 1, L', 'BIT 1, (HL)', 'BIT 1, A',
		'BIT 2, B', 'BIT 2, C', 'BIT 2, D', 'BIT 2, E',
		'BIT 2, H', 'BIT 2, L', 'BIT 2, (HL)', 'BIT 2, A',
		'BIT 3, B', 'BIT 3, C', 'BIT 3, D', 'BIT 3, E',
		'BIT 3, H', 'BIT 3, L', 'BIT 3, (HL)', 'BIT 3, A',
		'BIT 4, B', 'BIT 4, C', 'BIT 4, D', 'BIT 4, E',
		'BIT 4, H', 'BIT 4, L', 'BIT 4, (HL)', 'BIT 4, A',
		'BIT 5, B', 'BIT 5, C', 'BIT 5, D', 'BIT 5, E',
		'BIT 5, H', 'BIT 5, L', 'BIT 5, (HL)', 'BIT 5, A',
		'BIT 6, B', 'BIT 6, C', 'BIT 6, D', 'BIT 6, E',
		'BIT 6, H', 'BIT 6, L', 'BIT 6, (HL)', 'BIT 6, A',
		'BIT 7, B', 'BIT 7, C', 'BIT 7, D', 'BIT 7, E',
		'BIT 7, H', 'BIT 7, L', 'BIT 7, (HL)', 'BIT 7, A',
		'RES 0, B', 'RES 0, C', 'RES 0, D', 'RES 0, E',
		'RES 0, H', 'RES 0, L', 'RES 0, (HL)', 'RES 0, A',
		'RES 1, B', 'RES 1, C', 'RES 1, D', 'RES 1, E',
		'RES 1, H', 'RES 1, L', 'RES 1, (HL)', 'RES 1, A',
		'RES 2, B', 'RES 2, C', 'RES 2, D', 'RES 2, E',
		'RES 2, H', 'RES 2, L', 'RES 2, (HL)', 'RES 2, A',
		'RES 3, B', 'RES 3, C', 'RES 3, D', 'RES 3, E',
		'RES 3, H', 'RES 3, L', 'RES 3, (HL)', 'RES 3, A',
		'RES 4, B', 'RES 4, C', 'RES 4, D', 'RES 4, E',
		'RES 4, H', 'RES 4, L', 'RES 4, (HL)', 'RES 4, A',
		'RES 5, B', 'RES 5, C', 'RES 5, D', 'RES 5, E',
		'RES 5, H', 'RES 5, L', 'RES 5, (HL)', 'RES 5, A',
		'RES 6, B', 'RES 6, C', 'RES 6, D', 'RES 6, E',
		'RES 6, H', 'RES 6, L', 'RES 6, (HL)', 'RES 6, A',
		'RES 7, B', 'RES 7, C', 'RES 7, D', 'RES 7, E',
		'RES 7, H', 'RES 7, L', 'RES 7, (HL)', 'RES 7, A',
		'SET 0, B', 'SET 0, C', 'SET 0, D', 'SET 0, E',
		'SET 0, H', 'SET 0, L', 'SET 0, (HL)', 'SET 0, A',
		'SET 1, B', 'SET 1, C', 'SET 1, D', 'SET 1, E',
		'SET 1, H', 'SET 1, L', 'SET 1, (HL)', 'SET 1, A',
		'SET 2, B', 'SET 2, C', 'SET 2, D', 'SET 2, E',
		'SET 2, H', 'SET 2, L', 'SET 2, (HL)', 'SET 2, A',
		'SET 3, B', 'SET 3, C', 'SET 3, D', 'SET 3, E',
		'SET 3, H', 'SET 3, L', 'SET 3, (HL)', 'SET 3, A',
		'SET 4, B', 'SET 4, C', 'SET 4, D', 'SET 4, E',
		'SET 4, H', 'SET 4, L', 'SET 4, (HL)', 'SET 4, A',
		'SET 5, B', 'SET 5, C', 'SET 5, D', 'SET 5, E',
		'SET 5, H', 'SET 5, L', 'SET 5, (HL)', 'SET 5, A',
		'SET 6, B', 'SET 6, C', 'SET 6, D', 'SET 6, E',
		'SET 6, H', 'SET 6, L', 'SET 6, (HL)', 'SET 6, A',
		'SET 7, B', 'SET 7, C', 'SET 7, D', 'SET 7, E',
		'SET 7, H', 'SET 7, L', 'SET 7, (HL)', 'SET 7, A',

    // Opcodes 512-767: DD instructions
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IX, BC', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IX, DE', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', '@LD IX, $NNNN', '%LD ($NNNN), IX', 'INC IX',
		'INC IXH', 'DEC IXH', '!LD IXH, $NN', 'NOP',
		'NOP', 'ADD IX, IX', '%LD IX, ($NNNN)', 'DEC IX',
		'INC IXL', 'DEC IXL', '!LD IXL, $NN', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'?INC (IX+$DD)', '?DEC (IX+$DD)', '/LD (IX+$DD), $NN', 'NOP',
		'NOP', 'ADD IX, SP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD B, IXH', 'LD B, IXL', '?LD B, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD C, IXH', 'LD C, IXL', '?LD C, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD D, IXH', 'LD D, IXL', '?LD D, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD E, IXH', 'LD E, IXL', '?LD E, (IX+$DD)', 'NOP',
		'LD IXH, B', 'LD IXH, C', 'LD IXH, D', 'LD IXH, E',
		'LD IXH, IXH', 'LD IXH, IXL', '?LD H, (IX+$DD)', 'LD IXH, A',
		'LD IXL, B', 'LD IXL, C', 'LD IXL, D', 'LD IXL, E',
		'LD IXL, IXH', 'LD IXL, IXL', '?LD L, (IX+$DD)', 'LD IXL, A',
		'?LD (IX+$DD), B', '?LD (IX+$DD), C', '?LD (IX+$DD), D', '?LD (IX+$DD), E',
		'?LD (IX+$DD), H', '?LD (IX+$DD), L', 'NOP', '?LD (IX+$DD), A',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD A, IXH', 'LD A, IXL', '?LD A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADD A, IXH', 'ADD A, IXL', '?ADD A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADC A, IXH', 'ADC A, IXL', '?ADC A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SUB IXH', 'SUB IXL', '?SUB (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SBC A, IXH', 'SBC A, IXL', '?SBC A, (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'AND IXH', 'AND IXL', '?AND (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'XOR IXH', 'XOR IXL', '?XOR (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'OR IXH', 'OR IXL', '?OR (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'CP IXH', 'CP IXL', '?CP (IX+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', '',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'POP IX', 'NOP', 'EX (SP), IX',
		'NOP', 'PUSH IX', 'NOP', 'NOP',
		'NOP', 'JP (IX)', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'LD SP, IX', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',

    // Opcodes 768-1023: DD CB instructions
		'?LD B, RLC (IX+$DD)', '?LD C, RLC (IX+$DD)', '?LD D, RLC (IX+$DD)', '?LD E, RLC (IX+$DD)',
		'?LD H, RLC (IX+$DD)', '?LD L, RLC (IX+$DD)', '?RLC (IX+$DD)', '?LD A, RLC (IX+$DD)',
		'?LD B, RRC (IX+$DD)', '?LD C, RRC (IX+$DD)', '?LD D, RRC (IX+$DD)', '?LD E, RRC (IX+$DD)',
		'?LD H, RRC (IX+$DD)', '?LD L, RRC (IX+$DD)', '?RRC (IX+$DD)', '?LD A, RRC (IX+$DD)',
		'?LD B, RL (IX+$DD)', '?LD C, RL (IX+$DD)', '?LD D, RL (IX+$DD)', '?LD E, RL (IX+$DD)',
		'?LD H, RL (IX+$DD)', '?LD L, RL (IX+$DD)', '?RL (IX+$DD)', '?LD A, RL (IX+$DD)',
		'?LD B, RR (IX+$DD)', '?LD C, RR (IX+$DD)', '?LD D, RR (IX+$DD)', '?LD E, RR (IX+$DD)',
		'?LD H, RR (IX+$DD)', '?LD L, RR (IX+$DD)', '?RR (IX+$DD)', '?LD A, RR (IX+$DD)',
		'?LD B, SLA (IX+$DD)', '?LD C, SLA (IX+$DD)', '?LD D, SLA (IX+$DD)', '?LD E, SLA (IX+$DD)',
		'?LD H, SLA (IX+$DD)', '?LD L, SLA (IX+$DD)', '?SLA (IX+$DD)', '?LD A, SLA (IX+$DD)',
		'?LD B, SRA (IX+$DD)', '?LD C, SRA (IX+$DD)', '?LD D, SRA (IX+$DD)', '?LD E, SRA (IX+$DD)',
		'?LD H, SRA (IX+$DD)', '?LD L, SRA (IX+$DD)', '?SRA (IX+$DD)', '?LD A, SRA (IX+$DD)',
		'?LD B, SLL (IX+$DD)', '?LD C, SLL (IX+$DD)', '?LD D, SLL (IX+$DD)', '?LD E, SLL (IX+$DD)',
		'?LD H, SLL (IX+$DD)', '?LD L, SLL (IX+$DD)', '?SLL (IX+$DD)', '?LD A, SLL (IX+$DD)',
		'?LD B, SRL (IX+$DD)', '?LD C, SRL (IX+$DD)', '?LD D, SRL (IX+$DD)', '?LD E, SRL (IX+$DD)',
		'?LD H, SRL (IX+$DD)', '?LD L, SRL (IX+$DD)', '?SRL (IX+$DD)', '?LD A, SRL (IX+$DD)',
		'?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)',
		'?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)',
		'?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)',
		'?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)',
		'?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)',
		'?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)',
		'?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)',
		'?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)',
		'?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)',
		'?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)',
		'?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)',
		'?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)',
		'?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)',
		'?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)',
		'?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)',
		'?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)',
		'?LD B, RES 0 (IX+$DD)', '?LD C, RES 0 (IX+$DD)', '?LD D, RES 0 (IX+$DD)', '?LD E, RES 0 (IX+$DD)',
		'?LD H, RES 0 (IX+$DD)', '?LD L, RES 0 (IX+$DD)', '?RES 0, (IX+$DD)', '?LD A, RES 0 (IX+$DD)',
		'?LD B, RES 1 (IX+$DD)', '?LD C, RES 1 (IX+$DD)', '?LD D, RES 1 (IX+$DD)', '?LD E, RES 1 (IX+$DD)',
		'?LD H, RES 1 (IX+$DD)', '?LD L, RES 1 (IX+$DD)', '?RES 1, (IX+$DD)', '?LD A, RES 1 (IX+$DD)',
		'?LD B, RES 2 (IX+$DD)', '?LD C, RES 2 (IX+$DD)', '?LD D, RES 2 (IX+$DD)', '?LD E, RES 2 (IX+$DD)',
		'?LD H, RES 2 (IX+$DD)', '?LD L, RES 2 (IX+$DD)', '?RES 2, (IX+$DD)', '?LD A, RES 2 (IX+$DD)',
		'?LD B, RES 3 (IX+$DD)', '?LD C, RES 3 (IX+$DD)', '?LD D, RES 3 (IX+$DD)', '?LD E, RES 3 (IX+$DD)',
		'?LD H, RES 3, (IX+$DD)', '?LD L, RES 3 (IX+$DD)', '?RES 3, (IX+$DD)', '?LD A, RES 3 (IX+$DD)',
		'?LD B, RES 4 (IX+$DD)', '?LD C, RES 4 (IX+$DD)', '?LD D, RES 4 (IX+$DD)', '?LD E, RES 4 (IX+$DD)',
		'?LD H, RES 4 (IX+$DD)', '?LD L, RES 4 (IX+$DD)', '?RES 4, (IX+$DD)', '?LD A, RES 4 (IX+$DD)',
		'?LD B, RES 5 (IX+$DD)', '?LD C, RES 5 (IX+$DD)', '?LD D, RES 5 (IX+$DD)', '?LD E, RES 5 (IX+$DD)',
		'?LD H, RES 5 (IX+$DD)', '?LD L, RES 5 (IX+$DD)', '?RES 5, (IX+$DD)', '?LD A, RES 5 (IX+$DD)',
		'?LD B, RES 6 (IX+$DD)', '?LD C, RES 6 (IX+$DD)', '?LD D, RES 6 (IX+$DD)', '?LD E, RES 6 (IX+$DD)',
		'?LD H, RES 6 (IX+$DD)', '?LD L, RES 6 (IX+$DD)', '?RES 6, (IX+$DD)', '?LD A, RES 6 (IX+$DD)',
		'?LD B, RES 7, (IX+$DD)', '?LD C, RES 7 (IX+$DD)', '?LD D, RES 7 (IX+$DD)', '?LD E, RES 7 (IX+$DD)',
		'?LD H, RES 7 (IX+$DD)', '?LD L, RES 7 (IX+$DD)', '?RES 7, (IX+$DD)', '?LD A, RES 7 (IX+$DD)',
		'?LD B, SET 0 (IX+$DD)', '?LD C, SET 0 (IX+$DD)', '?LD D, SET 0 (IX+$DD)', '?LD E, SET 0 (IX+$DD)',
		'?LD H, SET 0 (IX+$DD)', '?LD L, SET 0 (IX+$DD)', '?SET 0, (IX+$DD)', '?LD A, SET 0 (IX+$DD)',
		'?LD B, SET 1 (IX+$DD)', '?LD C, SET 1 (IX+$DD)', '?LD D, SET 1 (IX+$DD)', '?LD E, SET 1 (IX+$DD)',
		'?LD H, SET 1 (IX+$DD)', '?LD L, SET 1 (IX+$DD)', '?SET 1, (IX+$DD)', '?LD A, SET 1 (IX+$DD)',
		'?LD B, SET 2, (IX+$DD)', '?LD C, SET 2 (IX+$DD)', '?LD D, SET 2 (IX+$DD)', '?LD E, SET 2 (IX+$DD)',
		'?LD H, SET 2 (IX+$DD)', '?LD L, SET 2 (IX+$DD)', '?SET 2, (IX+$DD)', '?LD A, SET 2 (IX+$DD)',
		'?LD B, SET 3 (IX+$DD)', '?LD C, SET 3 (IX+$DD)', '?LD D, SET 3 (IX+$DD)', '?LD E, SET 3 (IX+$DD)',
		'?LD H, SET 3 (IX+$DD)', '?LD L, SET 3 (IX+$DD)', '?SET 3, (IX+$DD)', '?LD A, SET 3 (IX+$DD)',
		'?LD B, SET 4 (IX+$DD)', '?LD C, SET 4 (IX+$DD)', '?LD D, SET 4 (IX+$DD)', '?LD E, SET 4 (IX+$DD)',
		'?LD H, SET 4 (IX+$DD)', '?LD L, SET 4 (IX+$DD)', '?SET 4, (IX+$DD)', '?LD A, SET 4 (IX+$DD)',
		'?LD B, SET 5, (IX+$DD)', '?LD C, SET 5 (IX+$DD)', '?LD D, SET 5 (IX+$DD)', '?LD E, SET 5 (IX+$DD)',
		'?LD H, SET 5 (IX+$DD)', '?LD L, SET 5 (IX+$DD)', '?SET 5, (IX+$DD)', '?LD A, SET 5 (IX+$DD)',
		'?LD B, SET 6 (IX+$DD)', '?LD C, SET 6 (IX+$DD)', '?LD D, SET 6 (IX+$DD)', '?LD E, SET 6 (IX+$DD)',
		'?LD H, SET 6 (IX+$DD)', '?LD L, SET 6 (IX+$DD)', '?SET 6, (IX+$DD)', '?LD A, SET 6 (IX+$DD)',
		'?LD B, SET 7 (IX+$DD)', '?LD C, SET 7 (IX+$DD)', '?LD D, SET 7 (IX+$DD)', '?LD E, SET 7 (IX+$DD)',
		'?LD H, SET 7 (IX+$DD)', '?LD L, SET 7 (IX+$DD)', '?SET 7, (IX+$DD)', '?LD A, SET 7 (IX+$DD)',

    // Opcodes 1024-1279: ED instructions
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'IN B, (C)', 'OUT (C), B', 'SBC HL, BC', '%LD ($NNNN), BC',
		'NEG', 'RETN', 'IM 0', 'LD I, A',
		'IN C, (C)', 'OUT (C), C', 'ADC HL, BC', '%LD BC, ($NNNN)',
		'NEG', 'RETI', 'IM 0', 'LD R, A',
		'IN D, (C)', 'OUT (C), D', 'SBC HL, DE', '%LD ($NNNN), DE',
		'NEG', 'RETN', 'IM 1', 'LD A, I',
		'IN E, (C)', 'OUT (C), E', 'ADC HL, DE', '%LD DE, ($NNNN)',
		'NEG', 'RETN', 'IM 2', 'LD A, R',
		'IN H, (C)', 'OUT (C), B', 'SBC HL, HL', '%LD ($NNNN), HL',
		'NEG', 'RETN', 'IM 0', 'RRD',
		'IN L, (C)', 'OUT (C), L', 'ADC HL, HL', '%LD HL, ($NNNN)',
		'NEG', 'RETN', 'IM 0/1', 'RLD',
		'IN F, (C)', 'OUT (C), 0', 'SBC HL, SP', '%LD ($NNNN), SP',
		'NEG', 'RETN', 'IM 1', 'NOP',
		'IN A, (C)', 'OUT (C), A', 'ADC HL, SP', '%LD SP, ($NNNN)',
		'NEG', 'RETN', 'IM 2', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDI', 'CPI', 'INI', 'OUTI',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDD', 'CPD', 'IND', 'OUTD',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDIR', 'CPIR', 'INIR', 'OTIR',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LDDR', 'CPDR', 'INDR', 'OTDR',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',

    // Opcodes 1280-1535: FD instructions
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IY, BC', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'ADD IY, DE', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', '@LD IY, $NNNN', '%LD ($NNNN), IY', 'INC IY',
		'INC IYH', 'DEC IYH', '!LD IYH, $NN', 'NOP',
		'NOP', 'ADD IY, IY', '%LD IY, ($NNNN)', 'DEC IY',
		'INC IYL', 'DEC IYL', '!LD IYL, $NN', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'?INC (IY+$DD)', '?DEC (IY+$DD)', '/LD (IY+$DD), $NN', 'NOP',
		'NOP', 'ADD IY, SP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD B, IYH', 'LD B, IYL', '?LD B, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD C, IYH', 'LD C, IYL', '?LD C, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD D, IYH', 'LD D, IYL', '?LD D, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD E, IYH', 'LD E, IYL', '?LD E, (IY+$DD)', 'NOP',
		'LD IYH, B', 'LD IYH, C', 'LD IYH, D', 'LD IYH, E',
		'LD IYH, IYH', 'LD IYH, IYL', '?LD H, (IY+$DD)', 'LD IYH, A',
		'LD IYL, B', 'LD IYL, C', 'LD IYL, D', 'LD IYL, E',
		'LD IYL, IYH', 'LD IYL, IYL', '?LD L, (IY+$DD)', 'LD IYL, A',
		'?LD (IY+$DD), B', '?LD (IY+$DD), C', '?LD (IY+$DD), D', '?LD (IY+$DD), E',
		'?LD (IY+$DD), H', '?LD (IY+$DD), L', 'NOP', '?LD (IY+$DD), A',
		'NOP', 'NOP', 'NOP', 'NOP',
		'LD A, IYH', 'LD A, IYL', '?LD A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADD A, IYH', 'ADD A, IYL', '?ADD A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'ADC A, IYH', 'ADC A, IYL', '?ADC A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SUB IYH', 'SUB IYL', '?SUB (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'SBC A, IYH', 'SBC A, IYL', '?SBC A, (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'AND IYH', 'AND IYL', '?AND (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'XOR IYH', 'XOR IYL', '?XOR (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'OR IYH', 'OR IYL', '?OR (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'CP IYH', 'CP IYL', '?CP (IY+$DD)', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', '',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'POP IY', 'NOP', 'EX (SP), IY',
		'NOP', 'PUSH IY', 'NOP', 'NOP',
		'NOP', 'JP (IY)', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',
		'NOP', 'LD SP, IY', 'NOP', 'NOP',
		'NOP', 'NOP', 'NOP', 'NOP',

    // Opcodes 1536-1791: FD CB instructions
		'?LD B, RLC (IY+$DD)', '?LD C, RLC (IY+$DD)', '?LD D, RLC (IY+$DD)', '?LD E, RLC (IY+$DD)',
		'?LD H, RLC (IY+$DD)', '?LD L, RLC (IY+$DD)', '?RLC (IY+$DD)', '?LD A, RLC (IY+$DD)',
		'?LD B, RRC (IY+$DD)', '?LD C, RRC (IY+$DD)', '?LD D, RRC (IY+$DD)', '?LD E, RRC (IY+$DD)',
		'?LD H, RRC (IY+$DD)', '?LD L, RRC (IY+$DD)', '?RRC (IY+$DD)', '?LD A, RRC (IY+$DD)',
		'?LD B, RL (IY+$DD)', '?LD C, RL (IY+$DD)', '?LD D, RL (IY+$DD)', '?LD E, RL (IY+$DD)',
		'?LD H, RL (IY+$DD)', '?LD L, RL (IY+$DD)', '?RL (IY+$DD)', '?LD A, RL (IY+$DD)',
		'?LD B, RR (IY+$DD)', '?LD C, RR (IY+$DD)', '?LD D, RR (IY+$DD)', '?LD E, RR (IY+$DD)',
		'?LD H, RR (IY+$DD)', '?LD L, RR (IY+$DD)', '?RR (IY+$DD)', '?LD A, RR (IY+$DD)',
		'?LD B, SLA (IY+$DD)', '?LD C, SLA (IY+$DD)', '?LD D, SLA (IY+$DD)', '?LD E, SLA (IY+$DD)',
		'?LD H, SLA (IY+$DD)', '?LD L, SLA (IY+$DD)', '?SLA (IY+$DD)', '?LD A, SLA (IY+$DD)',
		'?LD B, SRA (IY+$DD)', '?LD C, SRA (IY+$DD)', '?LD D, SRA (IY+$DD)', '?LD E, SRA (IY+$DD)',
		'?LD H, SRA (IY+$DD)', '?LD L, SRA (IY+$DD)', '?SRA (IY+$DD)', '?LD A, SRA (IY+$DD)',
		'?LD B, SLL (IY+$DD)', '?LD C, SLL (IY+$DD)', '?LD D, SLL (IY+$DD)', '?LD E, SLL (IY+$DD)',
		'?LD H, SLL (IY+$DD)', '?LD L, SLL (IY+$DD)', '?SLL (IY+$DD)', '?LD A, SLL (IY+$DD)',
		'?LD B, SRL (IY+$DD)', '?LD C, SRL (IY+$DD)', '?LD D, SRL (IY+$DD)', '?LD E, SRL (IY+$DD)',
		'?LD H, SRL (IY+$DD)', '?LD L, SRL (IY+$DD)', '?SRL (IY+$DD)', '?LD A, SRL (IY+$DD)',
		'?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)',
		'?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)',
		'?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)',
		'?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)',
		'?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)',
		'?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)',
		'?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)',
		'?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)',
		'?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)',
		'?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)',
		'?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)',
		'?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)',
		'?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)',
		'?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)',
		'?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)',
		'?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)',
		'?LD B, RES 0 (IY+$DD)', '?LD C, RES 0 (IY+$DD)', '?LD D, RES 0 (IY+$DD)', '?LD E, RES 0 (IY+$DD)',
		'?LD H, RES 0 (IY+$DD)', '?LD L, RES 0 (IY+$DD)', '?RES 0, (IY+$DD)', '?LD A, RES 0 (IY+$DD)',
		'?LD B, RES 1 (IY+$DD)', '?LD C, RES 1 (IY+$DD)', '?LD D, RES 1 (IY+$DD)', '?LD E, RES 1 (IY+$DD)',
		'?LD H, RES 1 (IY+$DD)', '?LD L, RES 1 (IY+$DD)', '?RES 1, (IY+$DD)', '?LD A, RES 1 (IY+$DD)',
		'?LD B, RES 2 (IY+$DD)', '?LD C, RES 2 (IY+$DD)', '?LD D, RES 2 (IY+$DD)', '?LD E, RES 2 (IY+$DD)',
		'?LD H, RES 2 (IY+$DD)', '?LD L, RES 2 (IY+$DD)', '?RES 2, (IY+$DD)', '?LD A, RES 2 (IY+$DD)',
		'?LD B, RES 3 (IY+$DD)', '?LD C, RES 3 (IY+$DD)', '?LD D, RES 3 (IY+$DD)', '?LD E, RES 3 (IY+$DD)',
		'?LD H, RES 3, (IY+$DD)', '?LD L, RES 3 (IY+$DD)', '?RES 3, (IY+$DD)', '?LD A, RES 3 (IY+$DD)',
		'?LD B, RES 4 (IY+$DD)', '?LD C, RES 4 (IY+$DD)', '?LD D, RES 4 (IY+$DD)', '?LD E, RES 4 (IY+$DD)',
		'?LD H, RES 4 (IY+$DD)', '?LD L, RES 4 (IY+$DD)', '?RES 4, (IY+$DD)', '?LD A, RES 4 (IY+$DD)',
		'?LD B, RES 5 (IY+$DD)', '?LD C, RES 5 (IY+$DD)', '?LD D, RES 5 (IY+$DD)', '?LD E, RES 5 (IY+$DD)',
		'?LD H, RES 5 (IY+$DD)', '?LD L, RES 5 (IY+$DD)', '?RES 5, (IY+$DD)', '?LD A, RES 5 (IY+$DD)',
		'?LD B, RES 6 (IY+$DD)', '?LD C, RES 6 (IY+$DD)', '?LD D, RES 6 (IY+$DD)', '?LD E, RES 6 (IY+$DD)',
		'?LD H, RES 6 (IY+$DD)', '?LD L, RES 6 (IY+$DD)', '?RES 6, (IY+$DD)', '?LD A, RES 6 (IY+$DD)',
		'?LD B, RES 7, (IY+$DD)', '?LD C, RES 7 (IY+$DD)', '?LD D, RES 7 (IY+$DD)', '?LD E, RES 7 (IY+$DD)',
		'?LD H, RES 7 (IY+$DD)', '?LD L, RES 7 (IY+$DD)', '?RES 7, (IY+$DD)', '?LD A, RES 7 (IY+$DD)',
		'?LD B, SET 0 (IY+$DD)', '?LD C, SET 0 (IY+$DD)', '?LD D, SET 0 (IY+$DD)', '?LD E, SET 0 (IY+$DD)',
		'?LD H, SET 0 (IY+$DD)', '?LD L, SET 0 (IY+$DD)', '?SET 0, (IY+$DD)', '?LD A, SET 0 (IY+$DD)',
		'?LD B, SET 1 (IY+$DD)', '?LD C, SET 1 (IY+$DD)', '?LD D, SET 1 (IY+$DD)', '?LD E, SET 1 (IY+$DD)',
		'?LD H, SET 1 (IY+$DD)', '?LD L, SET 1 (IY+$DD)', '?SET 1, (IY+$DD)', '?LD A, SET 1 (IY+$DD)',
		'?LD B, SET 2, (IY+$DD)', '?LD C, SET 2 (IY+$DD)', '?LD D, SET 2 (IY+$DD)', '?LD E, SET 2 (IY+$DD)',
		'?LD H, SET 2 (IY+$DD)', '?LD L, SET 2 (IY+$DD)', '?SET 2, (IY+$DD)', '?LD A, SET 2 (IY+$DD)',
		'?LD B, SET 3 (IY+$DD)', '?LD C, SET 3 (IY+$DD)', '?LD D, SET 3 (IY+$DD)', '?LD E, SET 3 (IY+$DD)',
		'?LD H, SET 3 (IY+$DD)', '?LD L, SET 3 (IY+$DD)', '?SET 3, (IY+$DD)', '?LD A, SET 3 (IY+$DD)',
		'?LD B, SET 4 (IY+$DD)', '?LD C, SET 4 (IY+$DD)', '?LD D, SET 4 (IY+$DD)', '?LD E, SET 4 (IY+$DD)',
		'?LD H, SET 4 (IY+$DD)', '?LD L, SET 4 (IY+$DD)', '?SET 4, (IY+$DD)', '?LD A, SET 4 (IY+$DD)',
		'?LD B, SET 5, (IY+$DD)', '?LD C, SET 5 (IY+$DD)', '?LD D, SET 5 (IY+$DD)', '?LD E, SET 5 (IY+$DD)',
		'?LD H, SET 5 (IY+$DD)', '?LD L, SET 5 (IY+$DD)', '?SET 5, (IY+$DD)', '?LD A, SET 5 (IY+$DD)',
		'?LD B, SET 6 (IY+$DD)', '?LD C, SET 6 (IY+$DD)', '?LD D, SET 6 (IY+$DD)', '?LD E, SET 6 (IY+$DD)',
		'?LD H, SET 6 (IY+$DD)', '?LD L, SET 6 (IY+$DD)', '?SET 6, (IY+$DD)', '?LD A, SET 6 (IY+$DD)',
		'?LD B, SET 7 (IY+$DD)', '?LD C, SET 7 (IY+$DD)', '?LD D, SET 7 (IY+$DD)', '?LD E, SET 7 (IY+$DD)',
		'?LD H, SET 7 (IY+$DD)', '?LD L, SET 7 (IY+$DD)', '?SET 7, (IY+$DD)', '?LD A, SET 7 (IY+$DD)');

// the following is a list of index numbers of instructions, when sorted alphabetically
// (takes less space than storing the instruction table twice)
OpcodeScanTable: Array [0..1128] of Word = (
		$ce, $8e, $28e, $58e, $8f, $88, $89, $8a,
		$8b, $8c, $28c, $28d, $58c, $58d, $8d, $44a, 
		$45a, $46a, $47a, $c6, $86, $286, $586, $87, 
		$80, $81, $82, $83, $84, $284, $285, $584, 
		$585, $85, $9, $19, $29, $39, $209, $219, 
		$229, $239, $509, $519, $529, $539, $e6, $a6, 
		$2a6, $5a6, $a7, $a0, $a1, $a2, $a3, $a4, 
		$2a4, $2a5, $5a4, $5a5, $a5, $146, $346, $646, 
		$147, $140, $141, $142, $143, $144, $145, $14e, 
		$34e, $64e, $14f, $148, $149, $14a, $14b, $14c, 
		$14d, $156, $356, $656, $157, $150, $151, $152, 
		$153, $154, $155, $15e, $35e, $65e, $15f, $158, 
		$159, $15a, $15b, $15c, $15d, $166, $366, $666, 
		$167, $160, $161, $162, $163, $164, $165, $16e,
		$36e, $66e, $16f, $168, $169, $16a, $16b, $16c,
		$16d, $176, $376, $676, $177, $170, $171, $172, 
		$173, $174, $175, $17e, $37e, $67e, $17f, $178,
		$179, $17a, $17b, $17c, $17d, $cd, $dc, $fc, 
		$d4, $c4, $f4, $ec, $e4, $cc, $3f, $fe, 
		$be, $2be, $5be, $bf, $b8, $b9, $ba, $bb, 
		$bc, $2bc, $2bd, $5bc, $5bd, $bd, $4a9, $4b9, 
		$4a1, $4b1, $2f, $27, $35, $235, $535, $3d, 
		$5, $b, $d, $15, $1b, $1d, $25, $2b,
		$22b, $225, $22d, $52b, $525, $52d, $2d, $3b,
		$f3, $10, $fb, $e3, $2e3, $5e3, $8, $eb, 
		$d9, $76, $44e, $456, $45e, $db, $478, $440, 
		$448, $450, $458, $470, $460, $468, $34, $234, 
		$534, $3c, $4, $3, $c, $14, $13, $1c,
		$24, $23, $223, $224, $22c, $523, $524, $52c, 
		$2c, $33, $4aa, $4ba, $4a2, $4b2, $c3, $e9, 
		$2e9, $5e9, $da, $fa, $d2, $c2, $f2, $ea,
		$e2, $ca, $18, $38, $30, $20, $28, $32, 
		$443, $453, $22, $222, $522, $473, $2, $12, 
		$36, $77, $70, $71, $72, $73, $74, $75, 
		$236, $277, $270, $271, $272, $273, $274, $275, 
		$536, $577, $570, $571, $572, $573, $574, $575, 
		$3e, $3a, $a, $1a, $7e, $27e, $57e, $7f, 
		$78, $79, $7a, $7b, $7c, $457, $27c, $27d, 
		$57c, $57d, $7d, $45f, $387, $687, $38f, $68f, 
		$397, $697, $39f, $69f, $3a7, $6a7, $3af, $6af, 
		$3b7, $6b7, $3bf, $6bf, $317, $617, $307, $607, 
		$31f, $61f, $30f, $60f, $3c7, $6c7, $3cf, $6cf, 
		$3d7, $6d7, $3df, $6df, $3e7, $6e7, $3ef, $6ef, 
		$3f7, $6f7, $3ff, $6ff, $327, $627, $337, $637, 
		$32f, $62f, $33f, $63f, $6, $46, $246, $546,
		$47, $40, $41, $42, $43, $44, $244, $245, 
		$544, $545, $45, $380, $680, $388, $688, $390, 
		$690, $398, $698, $3a0, $6a0, $3a8, $6a8, $3b0, 
		$6b0, $3b8, $6b8, $310, $610, $300, $600, $318, 
		$618, $308, $608, $3c0, $6c0, $3c8, $6c8, $3d0, 
		$6d0, $3d8, $6d8, $3e0, $6e0, $3e8, $6e8, $3f0, 
		$6f0, $3f8, $6f8, $320, $620, $330, $630, $328,
		$628, $338, $638, $1, $44b, $e, $4e, $24e,
		$54e, $4f, $48, $49, $4a, $4b, $4c, $24c,
		$24d, $54c, $54d, $4d, $381, $681, $389, $689, 
		$391, $691, $399, $699, $3a1, $6a1, $3a9, $6a9, 
		$3b1, $6b1, $3b9, $6b9, $311, $611, $301, $601, 
		$319, $619, $309, $609, $3c1, $6c1, $3c9, $6c9, 
		$3d1, $6d1, $3d9, $6d9, $3e1, $6e1, $3e9, $6e9, 
		$3f1, $6f1, $3f9, $6f9, $321, $621, $331, $631,
		$329, $629, $339, $639, $16, $56, $256, $556,
		$57, $50, $51, $52, $53, $54, $254, $255, 
		$554, $555, $55, $382, $682, $38a, $68a, $392, 
		$692, $39a, $69a, $3a2, $6a2, $3aa, $6aa, $3b2, 
		$6b2, $3ba, $6ba, $312, $612, $302, $602, $31a, 
		$61a, $30a, $60a, $3c2, $6c2, $3ca, $6ca, $3d2, 
		$6d2, $3da, $6da, $3e2, $6e2, $3ea, $6ea, $3f2, 
		$6f2, $3fa, $6fa, $322, $622, $332, $632, $32a, 
		$62a, $33a, $63a, $11, $45b, $1e, $5e, $25e, 
		$55e, $5f, $58, $59, $5a, $5b, $5c, $25c, 
		$25d, $55c, $55d, $5d, $383, $683, $38b, $68b, 
		$393, $693, $39b, $69b, $3a3, $6a3, $3ab, $6ab, 
		$3b3, $6b3, $3bb, $6bb, $313, $613, $303, $603, 
		$31b, $61b, $30b, $60b, $3c3, $6c3, $3cb, $6cb,
		$3d3, $6d3, $3db, $6db, $3e3, $6e3, $3eb, $6eb,
		$3f3, $6f3, $3fb, $6fb, $323, $623, $333, $633, 
		$32b, $62b, $33b, $63b, $26, $66, $266, $566, 
		$67, $60, $61, $62, $63, $64, $65, $384, 
		$684, $38c, $68c, $394, $694, $39c, $69c, $3a4, 
		$6a4, $3ac, $6ac, $3b4, $6b4, $3bc, $6bc, $314,
		$614, $304, $604, $31c, $61c, $30c, $60c, $3c4, 
		$6c4, $3cc, $6cc, $3d4, $6d4, $3dc, $6dc, $3e4, 
		$6e4, $3ec, $6ec, $3f4, $6f4, $3fc, $6fc, $324,
		$624, $334, $634, $32c, $62c, $33c, $63c, $21,
		$2a, $447, $221, $22a, $226, $267, $260, $261, 
		$262, $263, $264, $265, $22e, $26f, $268, $269, 
		$26a, $26b, $26c, $26d, $521, $52a, $526, $567, 
		$560, $561, $562, $563, $564, $565, $52e, $56f, 
		$568, $569, $56a, $56b, $56c, $56d, $2e, $6e, 
		$26e, $56e, $6f, $68, $69, $6a, $6b, $6c, 
		$6d, $385, $685, $38d, $68d, $395, $695, $39d,
		$69d, $3a5, $6a5, $3ad, $6ad, $3b5, $6b5, $3bd, 
		$6bd, $315, $615, $305, $605, $31d, $61d, $30d, 
		$60d, $3c5, $6c5, $3cd, $6cd, $3d5, $6d5, $3dd, 
		$6dd, $3e5, $6e5, $3ed, $6ed, $3f5, $6f5, $3fd, 
		$6fd, $325, $625, $335, $635, $32d, $62d, $33d, 
		$63d, $44f, $31, $47b, $f9, $2f9, $5f9, $4a8,
		$4b8, $4a0, $4b0, $444, $0, $f6, $b6, $2b6, 
		$5b6, $b7, $b0, $b1, $b2, $b3, $b4, $2b4, 
		$2b5, $5b4, $5b5, $b5, $4bb, $4b3, $d3, $471, 
		$479, $441, $461, $449, $451, $459, $469, $4ab, 
		$4a3, $f1, $c1, $d1, $e1, $2e1, $5e1, $f5, 
		$c5, $d5, $e5, $2e5, $5e5, $186, $386, $686,
		$187, $180, $181, $182, $183, $184, $185, $18e,
		$38e, $68e, $18f, $188, $189, $18a, $18b, $18c,
		$18d, $196, $396, $696, $197, $190, $191, $192,
		$193, $194, $195, $19e, $39e, $69e, $19f, $198, 
		$199, $19a, $19b, $19c, $19d, $1a6, $3a6, $6a6,
		$1a7, $1a0, $1a1, $1a2, $1a3, $1a4, $1a5, $1ae, 
		$3ae, $6ae, $1af, $1a8, $1a9, $1aa, $1ab, $1ac, 
		$1ad, $1b6, $3b6, $6b6, $1b7, $1b0, $1b1, $1b2, 
		$1b3, $1b4, $1b5, $1be, $3be, $6be, $1bf, $1b8, 
		$1b9, $1ba, $1bb, $1bc, $1bd, $c9, $d8, $f8,
		$d0, $c0, $f0, $e8, $e0, $c8, $44d, $445, $116,
		$316, $616, $117, $110, $111, $112, $113, $114, 
		$115, $17, $106, $306, $606, $107, $100, $101, 
		$102, $103, $104, $105, $7, $46f, $11e, $31e,
		$61e, $11f, $118, $119, $11a, $11b, $11c, $11d, 
		$1f, $10e, $30e, $60e, $10f, $108, $109, $10a, 
		$10b, $10c, $10d, $f, $467, $c7, $de, $9e, 
		$29e, $59e, $9f, $98, $99, $9a, $9b, $9c,
		$29c, $29d, $59c, $59d, $9d, $442, $452, $462, 
		$472, $37, $1c6, $3c6, $6c6, $1c7, $1c0, $1c1, 
		$1c2, $1c3, $1c4, $1c5, $1ce, $3ce, $6ce, $1cf, 
		$1c8, $1c9, $1ca, $1cb, $1cc, $1cd, $1d6, $3d6, 
		$6d6, $1d7, $1d0, $1d1, $1d2, $1d3, $1d4, $1d5, 
		$1de, $3de, $6de, $1df, $1d8, $1d9, $1da, $1db, 
		$1dc, $1dd, $1e6, $3e6, $6e6, $1e7, $1e0, $1e1, 
		$1e2, $1e3, $1e4, $1e5, $1ee, $3ee, $6ee, $1ef, 
		$1e8, $1e9, $1ea, $1eb, $1ec, $1ed, $1f6, $3f6, 
		$6f6, $1f7, $1f0, $1f1, $1f2, $1f3, $1f4, $1f5, 
		$1fe, $3fe, $6fe, $1ff, $1f8, $1f9, $1fa, $1fb,
		$1fc, $1fd, $126, $326, $626, $127, $120, $121, 
		$122, $123, $124, $125, $136, $336, $636, $137, 
		$130, $131, $132, $133, $134, $135, $12e, $32e,
		$62e, $12f, $128, $129, $12a, $12b, $12c, $12d,
		$13e, $33e, $63e, $13f, $138, $139, $13a, $13b, 
		$13c, $13d, $d6, $96, $296, $596, $97, $90, 
		$91, $92, $93, $94, $294, $295, $594, $595, 
		$95, $ee, $ae, $2ae, $5ae, $af, $a8, $a9, 
		$aa, $ab, $ac, $2ac, $2ad, $5ac, $5ad, $ad);

  OpcodeASCIICodes: Array[0..255] of word = (
    $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,
    $0008,$0020,$000A,$000B,$000C,$000D,$000E,$000F,
    $0010,$0011,$0012,$0013,$0014,$0015,$0016,$0017,
    $0018,$0019,$001A,$001B,$001C,$001D,$001E,$001F,
    $0020,$0021,$0022,$0023,$FF24,$FF25,$FF26,$0027,
    $F028,$F129,$002A,$F22B,$002C,$F32D,$FF2E,$002F,
    $FF30,$FF31,$FF32,$FF33,$FF34,$FF35,$FF36,$FF37,
    $FF38,$FF39,$003A,$003B,$003C,$003D,$003E,$003F,
    $0040,$0141,$0142,$0143,$0144,$0145,$0146,$0147,
    $0148,$0149,$014A,$014B,$014C,$014D,$014E,$014F,
    $0150,$0151,$0152,$0153,$0154,$0155,$0156,$0157,
    $0158,$0159,$015A,$005B,$005C,$005D,$005E,$015F,
    $0060,$0141,$0142,$0143,$0144,$0145,$0146,$0147,
    $0148,$0149,$014A,$014B,$014C,$014D,$014E,$014F,
    $0150,$0151,$0152,$0153,$0154,$0155,$0156,$0157,
    $0158,$0159,$015A,$007B,$007C,$007D,$007E,$007F,
    $0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
    $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
    $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
    $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
    $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
    $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
    $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
    $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
    $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
    $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
    $00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D6,$00D7,
    $00D8,$00D9,$00DA,$00DB,$00DC,$00DD,$00DE,$00DF,
    $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
    $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
    $00F0,$00F1,$00F2,$00F3,$00F4,$00F5,$00F6,$00F7,
    $00F8,$00F9,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF);

implementation

Constructor TZ80Assembler.Create;
var i: Integer;
begin
  inherited create;
  DEFSDefault := $FF;
  Errors := TStringList.Create;
  for i := 0 to 31 do AsmSymbols[i] := TStringList.Create;
  ResetMemBlocks;
  atStartOfPass := nil;
  afterLine := nil;
  SetLength(srcFiles, 0);
end;
Destructor TZ80Assembler.Destroy;
var i: Integer;
begin
  Errors.Free;
  SetLength(AsmSymbolsExt, 0);
  NumAsmSymbols := 0;
  for i := 0 to 31 do AsmSymbols[i].Free;
  if DefaultPage <> nil then DefaultPage.Free;
  inherited destroy;
end;
procedure TZ80Assembler.ResetMemBlocks;
begin
  DefaultPage := nil;
  SetLength(PageList, 0);
end;
procedure TZ80Assembler.SetMem48(buffer: pointer);
begin
  if DefaultPage <> nil then DefaultPage.Free;
  DefaultPage := TZ80MemBlock.Create;
  DefaultPage.Name := '';
  DefaultPage.Z80Start := 16384;
  DefaultPage.Z80End := 65536;
  DefaultPage.StartAddress := buffer;
  DefaultPage.AltLo := 65536;
  DefaultPage.AltHi := 0;
  SetLength(PageList, 1);
  PageList[0] := DefaultPage;
end;
procedure TZ80Assembler.AddTZ80MemBlock(m: TZ80MemBlock);
begin
  SetLength(PageList, High(PageList)+2);
  PageList[High(PageList)] := m;
end;
function TZ80Assembler.AddMem(Name: ShortString; Start, Size: Integer;
  buffer: Pointer): TZ80MemBlock;
var m: TZ80MemBlock;
begin
  m := TZ80MemBlock.Create;
  m.Name := uppercase(Name);
  m.Z80Start := Start;
  m.Z80End := Start+Size;
  m.StartAddress := buffer;
  m.AltLo := 65536;
  m.AltHi := 0;
  AddTZ80MemBlock(m);
  result := m;
end;
function TZ80Assembler.GetMemParams(Name: ShortString; var AltLo, AltHi: DWord): Pointer;
var i: Integer;
begin
  result := nil;
  AltLo := 65536;
  AltHi := 0;
  Name := uppercase(Name);
  for i := 0 to High(PageList) do if PageList[i].Name = Name then begin
    AltLo := PageList[i].AltLo;
    AltHi := PageList[i].AltHi;
    result := PageList[i].StartAddress;
    break;
  end;
end;

Function TZ80Assembler.ReadMemByte(Address: Word): Byte;
begin
  Result := 0;
end;
Function TZ80Assembler.ReadMemWord(Address: Word): Word;
begin
  Result := 0;
end;
procedure TZ80Assembler.eval1(S: String; var V, Code: Integer);
var
i, i2, i3, len, value, errCode, errCode2, opValue, parseType, SymIndex: Integer;
opType: Word;
wantNum, sign, skip: Boolean;
SymbolArrayNum: Integer;
SymbolFlags: DWord;
cmd: string;
begin
  i := 1;
  len := length(S);
  wantNum := True;
  sign := False;
  opType := 0;
  opValue := 0;
  value := 0;
  parseType := 0;
  SymIndex := -1;
  errCode := 0;
  while i <= len do begin
    if wantNum then begin
      if IsDelimiter(' '#9, S, i) then inc(i)
      else if S[i] = '(' then begin
        parseType := parseType or 2;
        i2 := FindChar(S, ')', '(', Char(0), i+1);
        if i2 = 0 then break
        else begin
          opValue := value;
          eval1(Copy(S, i+1, i2-(i+1)), value, errCode);
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2 + 1;
          opType := opType xor $8000;
        end;
      end else if S[i] = '[' then begin
        parseType := parseType or 2;
        i2 := FindChar(S, ']', '[', Char(0), i+1);
        if i2 = 0 then break
        else begin
          opValue := value;
          eval1(Copy(S, i+1, i2-(i+1)), value, errCode);
          if errCode < 0 then break;
          value := byte(ReadMemByte(value));
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2 + 1;
          opType := opType xor $8000;
        end;
      end else if S[i] = '-' then begin
        if sign then break;
        sign := not sign;
        inc(i);
      end else begin
        if IsDelimiter('"''', S, i) then begin
          if i = len then break;
          for i2 := i+1 to len do if IsDelimiter(S[i], S, i2) then break;
          if (i2 <= len) and ((i2-i) > 1) and ((i2-i) < 6) then begin;
            opValue := value;
            cmd := StringOfChar(Char(0), 4-((i2-i)-1)) + Copy(S, i+1, (i2-i)-1);
            value := (ord(cmd[1]) shl 24) or (ord(cmd[2]) shl 16) or (ord(cmd[3]) shl 8) or ord(cmd[4]);
            if sign then begin
              sign := False;
              value := -value;
            end;
            parseType := (parseType or 1) and -3;
            wantNum := False;
            i := i2+1;
            opType := opType xor $8000;
          end else break

        end else if IsDelimiter('_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.', S, i) then begin
          if i = len then i2 := i+1
          else for i2 := i+1 to len do if not IsDelimiter('''_.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', S, i2) then break;
          if IsDelimiter('.', S, i) then begin
            if LastSymbolDef <> nil then
              cmd := Copy(pchar(LastSymbolDef), 1, LastSymbolDefLen)+Copy(S, i, i2-i)
            else cmd := UpperCase(Copy(S, i, i2-i));
          end else cmd := UpperCase(Copy(S, i, i2-i));
          opValue := value;
          parseType := parseType or 1;
          SymbolArrayNum := 0;
          for i := 1 to Length(cmd) do SymbolArrayNum := SymbolArrayNum + (ord(cmd[i]) and $DF);
          SymbolArrayNum := SymbolArrayNum and $1F;
          SymIndex := AsmSymbols[SymbolArrayNum].IndexOf(cmd);
          if SymIndex < 0 then SymIndex := $80000000;
          if SymIndex >= 0 then begin
            SymbolFlags := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])]).SetData;
            if ((SymbolFlags and (SYM_SET+SYM_DEAD)) = (SYM_SET+SYM_DEAD))
              or ((SymbolFlags and SYM_MACRO) > 0) then begin
              errCode := errcode or $C0000000;
              break;
            end
            else value := AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])];
          end else begin
            errCode := errcode or $C0000000;
            break;
          end;
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2;
          parseType := (parseType or 8) and -3;
          opType := opType xor $8000;
        end else if IsDelimiter('%&$0123456789', S, i) then begin
          if i = len then i2 := i+1
          else for i2 := i+1 to len do if not IsDelimiter('0123456789ABCDEFHOXabcdefhox', S, i2) then break;
          Cmd := UpperCase(Copy(S, i, i2-i));
          opValue := value;
          skip := False;
          if Length(Cmd)>1 then begin
            if (isDelimiter('0123456789',Cmd,1)) and (Cmd[Length(Cmd)] = 'H') then
              Cmd := '$'+Copy(Cmd,1,Length(Cmd)-1)
            else if (not isDelimiter('$', Cmd, 1)) and (not isDelimiter('&', Cmd, 1))
            and (isDelimiter('B',Cmd,Length(Cmd))) then
              Cmd := '%'+Copy(Cmd,1,Length(Cmd)-1)
            else if (isDelimiter('OQ',Cmd,Length(Cmd))) and (Length(Cmd) > 1) then begin
              value := 0;
              for i3 := 1 to Length(Cmd)-1 do
                if (value and $E0000000 <> 0) or (not IsDelimiter('01234567',Cmd,i3)) then break
                else value := (value shl 3) or (ord(Cmd[i3])-48);
              if (i3 < Length(Cmd)-1) then begin
                errCode := errCode or $A0000000; // $80000000=Error, $10000000 = Invalid octal digit
                break;
              end else skip := True;
            end else if (isDelimiter('&',Cmd,1)) then Cmd[1] := '$'
            else if Copy(Cmd,1,2) = '0X' then
              Cmd := '$'+Copy(Cmd,3,Length(Cmd)-2);
            if isDelimiter('%',Cmd,1) then begin
              value := 0;
              for i3 := 2 to Length(Cmd) do
                if (value and $80000000 <> 0) or (not IsDelimiter('01',Cmd,i3)) then break
                else value := (value shl 1) or (ord(Cmd[i3])-48);
              if (i3 <= Length(Cmd)) or (i3 = 2) then begin
                errCode := errCode or $A0000000; // $80000000=Error, $20000000 = Invalid binary digit
                break;
              end else skip := True;
            end;
          end else if (Cmd = '$') then begin
            value := AsmLineAddr;
            skip := True;
          end;
          if not skip then Val(Cmd, value, errCode2);
          if errCode2 <> 0 then begin
            errCode := errCode or $80000000;
            break;
          end;
          parseType := (parseType or 1) and -3;
          if sign then begin
            sign := False;
            value := -value;
          end;
          wantNum := False;
          i := i2;
          opType := opType xor $8000;
        end else break;
      end;
    end else begin
      if IsDelimiter(' '#9, S, i) then inc(i)
      else if S[i] = '+' then begin
        parseType := (parseType or 4) and -3;
        opType := $0001;
        wantNum := True;
        inc(i);
      end else if S[i] = '-' then begin
        parseType := (parseType or 4) and -3;
        opType := $0002;
        wantNum := True;
        inc(i);
      end else if S[i] = '*' then begin
        parseType := (parseType or 4) and -3;
        opType := $0003;
        wantNum := True;
        inc(i);
      end else if S[i] = '/' then begin
        parseType := (parseType or 4) and -3;
        opType := $0004;
        wantNum := True;
        inc(i);
      end else if S[i] = '&' then begin
        parseType := (parseType or 4) and -3;
        opType := $0005;
        wantNum := True;
        inc(i);
      end else if S[i] = '|' then begin
        parseType := (parseType or 4) and -3;
        opType := $0006;
        wantNum := True;
        inc(i);
      end else if S[i] = '^' then begin
        parseType := (parseType or 4) and -3;
        opType := $0007;
        wantNum := True;
        inc(i);
      end else if S[i] = '%' then begin
        parseType := (parseType or 4) and -3;
        opType := $0011;
        wantNum := True;
        inc(i);
      end else if IsDelimiter('<>=!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.', S, i) then begin
        if i = len then i2 := i+1
        else for i2 := i+1 to len do if not IsDelimiter('<>=!&ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', S, i2) then break;
        cmd := UpperCase(Copy(S, i, i2-i));
        if (cmd = 'AND') or (cmd = '&') then begin
          parseType := (parseType or 4) and -3;
          opType := $0005;
          wantNum := True;
        end else if cmd = 'OR' then begin
          parseType := (parseType or 4) and -3;
          opType := $0006;
          wantNum := True;
        end else if cmd = 'XOR' then begin
          parseType := (parseType or 4) and -3;
          opType := $0007;
          wantNum := True;
        end else if (cmd = 'SHL') or (cmd = '<<') then begin
          parseType := (parseType or 4) and -3;
          opType := $0008;
          wantNum := True;
        end else if (cmd = 'SHR') or (cmd = '>>') then begin
          parseType := (parseType or 4) and -3;
          opType := $0009;
          wantNum := True;
        end else if cmd = '<' then begin
          parseType := (parseType or 4) and -3;
          opType := $000B;
          wantNum := True;
        end else if cmd = '>' then begin
          parseType := (parseType or 4) and -3;
          opType := $000C;
          wantNum := True;
        end else if cmd = '<=' then begin
          parseType := (parseType or 4) and -3;
          opType := $000D;
          wantNum := True;
        end else if cmd = '>=' then begin
          parseType := (parseType or 4) and -3;
          opType := $000E;
          wantNum := True;
        end else if cmd = '=' then begin
          parseType := (parseType or 4) and -3;
          opType := $000F;
          wantNum := True;
        end else if (cmd = '<>') or (cmd = '!=') then begin
          parseType := (parseType or 4) and -3;
          opType := $0010;
          wantNum := True;
        end else if (cmd = 'MOD') then begin
          parseType := (parseType or 4) and -3;
          opType := $0011;
          wantNum := True;
        end else break;
        i := i2;
      end else break;
    end;
    if (opType and $8000) > 0 then begin
      opType := opType xor $8000;
      try
        case opType of
          1: value := opValue + value;  // x + y
          2: value := opValue - value;  // x - y
          3: value := opValue * value;  // x * y
          4: value := opValue div value;  // x / y
          5: value := opValue and value;  // x and y
          6: value := opValue or value;  // x or y
          7: value := opValue xor value;  // x xor y
          8: value := opValue shl value;  // x << y
          9: value := opValue shr value;  // x >> y
         $A: value := opValue;
         $B: value := -1 + dword(opValue >= value);  // x < y
         $C: value := -1 + dword(opValue <= value);  // x > y
         $D: value := -1 + dword(opValue > value);  // x <= y
         $E: value := -1 + dword(opValue < value);  // x >= y
         $F: value := -1 + dword(opValue <> value);  // x = y
        $10: value := -1 + dword(opValue = value);  // x <> y
        $11: value := (opValue mod value);  // x mod y
        end;
        opType := 0;
        opValue := value;
      except
        begin
          dec(i);
          break;
        end;
      end;
    end;
  end;
  if (i > len) and ((parseType = 0) or not wantNum) and (errCode >= 0) and (parseType > 0) then begin
    V := value;
    Code := parseType;
  end else begin
    Code := parseType or ((errCode and $FFFF0000) or $80000000);
  end;
end;
function TZ80Assembler.FindChar(S: String; C, A, Q: Char; P: Integer): Integer;
var i, par: Integer;
begin
  par := 1;
  for i := P to Length(S) do begin
    if Q <> Char(0) then begin
      if S[i] = Q then Q := Char(0);
    end else begin
      if S[i] = C then begin
        dec(par);
        if par = 0 then break;
      end else if S[i] = A then begin
        inc(par);
      end;
      if IsDelimiter('"''', S, i) then Q := S[i];
    end;
  end;
  if i <= Length(S) then result := i
  else result := 0;
end;

function TZ80Assembler.evaluate(S: Pointer; c: Integer): Integer;
var
  v1, v2: Integer;
  SS: String;
begin
  SS := Copy(pchar(S),1,c);
  v1 := 0;
  v2 := 0;
  eval1(SS, v1, v2);
  self.val1 := v1;
  self.resultCode := v2;
end;

function IsReservedWord(arr, S: Pointer; i, c: Integer): Integer;
// arr = array to test, S = pointer to string to match
// i = number of indices, c = length of string to match
var
  buf: string[255];
asm
  push  ebx
  push  esi
  push  edi
  push  eax
  push  ecx
  mov ecx, [c]
  lea edi, buf
  mov [edi], cl
  mov ebx, 'za'+$100
  inc edi
@UCLoop:
  mov al, [edx]
  inc edx
  cmp al, bl
  jc  @noLC
  cmp al, bh
  jnc @noLC
  sub al, 32
@noLC:
  mov [edi], al
  inc edi
  loop  @UCLoop
  pop ecx
  pop eax
  inc ecx
  lea edx, buf+1

  // If the format of TAsmWord is changed, this lot will need to be updated...

  // array *must* be sorted alphabetically by TAsmWord.S for this to work!

  // string array format: @array returns: pointer to pointer:
  //    dword [pointer - 4]: Number of elements in array
  //    dword [pointer]: pointer to list of pointers to array elements (strings)

  // string format: @string returns: pointer to pointer:
  //    dword [pointer - 4]: Length of string
  //    byte [pointer...]: String data

  push  ecx
  mov ebx, ecx
  xor ecx, ecx
  push  ecx
  shr ebx, 1  // ebx = mid

@StartScan:
  mov esi, [eax+ebx*8] // eax = address of idx[0], ebx = idx num, TAsmWord record is 8 bytes long
  mov ecx, [c]
  mov edi, edx
  cmp ecx, [esi-4]
  jle @gotLen
  mov ecx, [esi-4]
@gotLen:
  cld
  and ecx, ecx
  je  @notFound

@ScanLoop:
  repz cmpsb // <- speed this up later when I have time...
  jc  @sGreater
  jne @sLess
  mov esi, [eax+ebx*8]
  mov ecx, [esi-4]
  cmp ecx, [c]
  je  @Found
  jc  @sGreater
@sLess:
  mov ecx, [esp]
  sub ecx, [esp+4]
  je  @notFound
  mov [esp+4], ebx
  jmp @nextString
@sGreater:
  mov ecx, [esp+4]
  sub ecx, [esp]
  dec ecx
  mov [esp], ebx
  jle  @notFound
@nextString:
  mov ebx, [esp+4]
  mov ecx, [esp]
  sub ebx, ecx
  shr ebx, 1
  add ebx, ecx
  jmp @StartScan
@notFound:
  mov eax, -1
  jmp @done
@Found:
  mov eax, [eax+4+ebx*8]
@done:
  lea esp, [esp+8]
  pop edi
  pop esi
  pop ebx
end;

function isInstruction(S: PChar): Integer;
var
  lo, hi, mid: Integer;
  p, p2: Pointer;
begin
  hi := High(OpcodeScanTable)+1;
  p := @OpcodeScanTable;
  p2 := @Opcodes;

  asm
  mov dword [@result], -1
  push  ebx
  push  esi
  push  edi
  mov eax, [hi]
  mov dword [lo], 0
  shr eax, 1
  mov [mid], eax

@StartScan:
  mov esi, [S]
  mov edx, [p]
  xor ecx, ecx
  mov ebx, [mid]
  mov edi, [p2]
  mov cx, [edx+ebx*2]
  mov edi, [edi+ecx*4]

  mov al, [edi]
  cmp al, 'A'
  jc  @skip1
  cmp al, 'Z'+1
  jc  @noSkip1
@skip1:
  inc edi
@noSkip1:
@ScanLoop:
  mov al, [esi]
  mov bl, [edi]
  inc edi
  and al, al
  je  @zero
  inc esi
  and bl, bl
  je  @sLess
  cmp al, bl
  jne @diff
  cmp al, '$'
  jne @ScanLoop
  cmp word [esi], 'NN'
  mov bx, [edi]
  jne @ScanLoop
  mov al, [esi+2]
  cmp bx, 'NN'
  je  @skipNN
  cmp bx, 'DD'
  je  @skipDD
  cmp bx, '$$'
  jne @scanLoop
@skipSS:
  add edi, 4
  add esi, 2
  jmp @ScanLoop
@skipNN:
  add edi, 4
  add esi, 2
  cmp bx, [edi-2]
  je  @scanLoop
  sub edi, 2
  jmp @ScanLoop
@skipDD:
  add edi, 2
  add esi, 2
  jmp @ScanLoop
@diff:
  jc  @sGreater
@sLess:
  mov eax, [mid]
  cmp eax, [lo]
  je  @Done
  mov [lo], eax
  jmp @nextString
@sGreater:
  mov edx, [hi]
  mov ecx, [mid]
  mov ebx, [lo]
  sub edx, ebx
  mov [hi], ecx
  jle  @Done
@nextString:
  mov ebx, [hi]
  mov edx, [lo]
  sub ebx, edx
  shr ebx, 1
  add ebx, edx
  mov [mid], ebx
  jmp @StartScan
@zero:
  cmp al,bl
  jne @sGreater
  mov [@result], ecx
@Done:
  pop edi
  pop esi
  pop ebx
  end;

end;

procedure TZ80Assembler.AddErrorQuick(errorNum, Line: Integer; FileNum: Integer);
var
  i: Integer;
  e: PAsmError;
  S1: ShortString;
begin
  i := NumErrors shl 2;
  if (High(QErrors)) <= (i+4) then
    SetLength(QErrors, i+256);
  e := PAsmError(@QErrors[i]);
  e.ErrorNum := errorNum;
  e.ErrorLine := Line+1;
  e.SourceFileNum := FileNum;
  e.Null := 0;
  inc(NumErrors);
  if (errorNum < -62) or (errorNum >= 0) then begin
    S1 := '';
  end;
end;

function TZ80Assembler.GetErrorString(index: Integer): String;
var
  Line, Code: Integer;
  S, FileName: String;
begin
  if GetError(index, Line, Filename, Code, S) then begin
    if Length(Filename) > 0 then
      result := ('('+IntToStr(Line)+') in "'+Filename+'": '+S)
    else result := ('('+IntToStr(Line)+'): '+S);
  end else Result := '';
end;

function TZ80Assembler.GetError(index: Integer; var Line: Integer; var FileName: String;
  var ErrorType: Integer; var Text: String): Boolean;
var
  e: PAsmError;
begin
  result := False;
  index := index * 4;
  if index > High(QErrors) then exit;
  e := PAsmError(@QErrors[index]);
  if e.SourceFileNum > 0 then FileName := SourceFiles[e.SourceFileNum] else FileName := '';
  ErrorType := e.ErrorNum;
  Line := e.ErrorLine;
  case e.ErrorNum of
    -01: Text :='Error';
    -02: Text :='Jump out of range';
    -03: Text :='Invalid byte data';
    -04: Text :='Bad index register displacement';
    -05: Text :='Invalid word data';
    -06: Text :='Invalid RST';
    -07: Text :='Undefined symbol';
    -08: Text :='Invalid filename';
    -09: Text :='Unrecognised instruction';
    -10: Text :='Bad ORG expression or ORG out of range';
    -11: Text :='Value out of range';
    -12: Text :='File not found';
    -13: Text :='Invalid file size';
    -14: Text :='File read error';
    -15: Text :='Can''t allocate memory';
    -16: Text :='No ORG specified';
    -17: Text :='Spurious ENDIF';
    -18: Text :='ELSE outside conditional';
    -19: Text :='ENDIF expected';
    -20: Text :='ELIF outside conditional';
    -21: Text :='Unrecognised directive';
    -22: Text :='Invalid symbol name';
    -23: Text :='Source expired prematurely';
    -24: Text :='Malformed line';
    -25: Text :='Nested macro definition';
    -26: Text :='ENDM outside macro definition';
    -27: Text :='Invalid PAGE or ORG out of range';
    -28: Text :='Malformed label';
    -29: Text :='Cannot use reserved word as label';
    -30: Text :='Local label before global label';
    -31: Text :='Symbol redefined';
    -32: Text :='Bad expression';
    -33: Text :='Expression result out of range';
    -34: Text :='Symbol declaration required';
    -35: Text :='Forward reference';
    -36: Text :='Bit number out of range';
    -37: Text :='Bad bit number expression';
    -38: Text :='Instruction too long';
    -39: Text :='Unbalanced parenthesis';
    -40: Text :='Opcode takes no operands';
    -41: Text :='Invalid combination of opcode and operands';
    -42: Text :='Invalid interrupt mode';
    -43: Text :='Byte value out of range';
    -44: Text :='Word value out of range';
    -45: Text :='Expression expected';
    -46: Text :='Instruction violates page bounds';
    -47: Text :='Immediate data expected';
    -48: Text :='Garbage following instruction';
    -49: Text :='ORG violates page bounds';
    -50: Text :='Expression must evaluate';
    -51: Text :='Recursive include';
    -52: Text :='Filename expected';
    -53: Text :='Error reading external source file';
    -54: Text :='Current output mode does not support pages';
    -55: Text :='Cannot change symbol type';
    -56: Text :='Nested structure definition';
    -57: Text :='ENDSTRUCT outside structure definition';
    -58: Text :='Instruction/directive not allowed inside structure definition';
    -59: Text :='Symbol expected';
    -60: Text :='Expression type mismatch';
    -61: Text :='IF Stack exceeds maximum length';
    -62: Text :='Invalid ALIGN value';
    -63: Text :='ENDR expected';
    -64: Text :='Invalid REPEAT count';
    -65: Text :='Bad REPEAT statement';
    -66: Text :='Cannot nest single-line REPEATs';
    -67: Text :='REPEAT stack overflow';
    -68: Text :='Spurious ENDR';
    $40000000: Text :='Undefined Symbol';
    else Text :='Error';
  end;
  result := True;
end;

Function TZ80Assembler.Assemble(Source: TStringList; Name: ShortString; StartAddress: Integer;
  StartPage: ShortString): Integer;
var
  c, i: Integer;
begin
  PassNum := 1;
  if SourceFiles <> nil then SourceFiles.Free;
  SourceFiles := TStringList.Create;
  SourceFiles.Add('');
  SourceFiles.Objects[0] := TObject(Source);
  for c := 0 to high(srcFiles) do begin
    i := SourceFiles.Add(srcFiles[c].filename);
    SourceFiles.Objects[i] := TObject(srcFiles[c].src);
  end;
  while doPass(Source, Name, StartAddress, StartPage, PassNum) do inc(PassNum);
  result := NumErrors;
  for i := 2+high(srcFiles) to SourceFiles.Count-1 do begin
    TStringList(SourceFiles.Objects[i]).Free;
    SourceFiles.Objects[i] := nil;
  end;
end;

procedure TZ80Assembler.AddSource(source: TStringList; filename: String);
var
  i: integer;
begin
  filename := uppercase(filename);
  for i := 0 to high(srcFiles) do if srcFiles[i].filename = filename then begin
    srcFiles[i].src := source;
    exit;
  end;
  i := high(srcFiles)+1;
  setLength(srcFiles, i+1);
  srcFiles[i].src := source;
  srcFiles[i].filename := filename;
end;
function TZ80Assembler.RemoveSource(filename: String): boolean;
var
  i, c: integer;
begin
  result := false;
  for i := 0 to high(srcFiles) do if srcFiles[i].filename = filename then begin
    for c := i to high(srcFiles)-1 do srcFiles[c] := srcFiles[c+1];
    setLength(srcFiles, high(srcFiles));
    result := true;
  end;
end;

function TZ80Assembler.GetSymbol(S: ShortString; var value: DWord): Boolean;
var
  SymbolArrayNum, SymIndex, SymbolFlags, i: Integer;
begin
  SymbolArrayNum := 0;
  for i := 1 to Length(S) do SymbolArrayNum := SymbolArrayNum + (ord(S[i]) and $DF);
  SymbolArrayNum := SymbolArrayNum and $1F;
  SymIndex := AsmSymbols[SymbolArrayNum].IndexOf(S);
  if SymIndex < 0 then Result := False else begin
    SymbolFlags := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])]).SetData;
    if ((SymbolFlags and (SYM_SET+SYM_DEAD)) = (SYM_SET+SYM_DEAD))
      or ((SymbolFlags and SYM_MACRO) > 0) then Result := False
    else begin
      Result := True;
      value := AsmSymbolsExt[Integer(AsmSymbols[SymbolArrayNum].Objects[SymIndex])];
    end;
  end;
end;

function TZ80Assembler.retFileSize(Filename: String):Integer;
var hnd: Integer;
begin
  hnd := CreateFile(Pchar(@Filename[1]), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
    0, OPEN_EXISTING, 0, 0);
  if hnd = INVALID_HANDLE_VALUE then result := -1 else begin
    result := GetFileSize(hnd, 0);
    CloseHandle(hnd);
  end;
end;

Function TZ80Assembler.doPass(Source: TStringList; var Name: ShortString; StartAddress: Integer;
  var StartPage: ShortString; Pass: Integer): Boolean;
var
  line, lineLen, i, i2, i3, i4, i5: Integer;
  Page: DWord;
  p, p2, p3: Pointer;
  FirstWord: Integer;
  CurSymbol: String[255];
  LastSymbol: Pointer;
  LastSymbolLen: Integer;
  SymbolNum: Integer;
  Instruction, tempString, tempstr, PageName: String[255];
  SymStart, SymLen: Integer;
  InsStart, InsLen, InsEnd, FWLen, InsWord2: Integer;
  LastInsSection, InsLen2, LastSrcSection, SrcSecLen: Integer;
  ExprStart, ExprLen, ExprMode, SectionNum, ParenthLevel: Integer;
  LastOpenBracket, IXDisp: DWord;
  ComStart, ComLen: Integer;
  errorNum: Integer;
  NumSymbolErrors: Integer;
  LastWords: DWord;
  InstrVal, DBSize: DWord;
  DBFlag: DWord;
  DBType: Boolean;
  IXSign, ValSet: Byte;
  CPJump: Pointer;

  LastSymbolBeforeStruct: Pointer;
  LastSymbolLenBeforeStruct: Integer;
  AsmWordsPtr, NumAsmWords: DWord;
  regEBX, regESI, regEDI: DWord;

  pSymbol: PAsmSymbol;
  minVal, maxVal: DWord;

  SS: ShortString;
  FileNum: Integer;
  IncludeSrc: TStringList;
  F: TFileStream;
  fBytesRead: Integer;
  defStruct: Boolean;
  StructOffset: DWord;
  IfStack: Array[0..63] of Byte;
  lastIF, InIF, allDirect: Boolean;
  IFlevel, IFValidLevel: Integer;
  IfTest, PageValid, defMacro: Boolean;
  macStack: Array [0..63] of TAsmMacroRecord;
  macLevel: Integer;
  repBase: Array [0..63] of DWord;
  repStack: Array [0..63] of TAsmRepRecord;
  repLevel, repBaseLevel: Integer;
  LineRepStart: Pointer;
  LineRepCount: Integer;
  D: PAsmDbgLine;
  TempLineLen: Word;

label
  AsmError, AsmErrorNoPush, AsmErrorReturn, doEval, AEQuick, doLine, noInstr, nextLine,
  noORGPage, done, AsmSkipWS, AsmGetExpr, AssembleSourceFile, AsmGetQuotedText,
  AddSymbol, doEQU_SET, EncodeInstruction, AsmGetArg, AsmGetSep, GetCurrentPage,
  DBLoop, DBError, DEFSFill, ProcessInstruction, nextFile;

  function AddDbgInfo(addr, len, flags: Integer): Boolean;
  begin
    if NumDbgLines >= (High(AsmDbgLines)+1) then SetLength(AsmDbgLines, NumDbgLines+64);
    D := @AsmDbgLines[NumDbgLines];
    D.StartAddress := addr;
    D.Page := DWord(@CurPage);
    D.Len := len;
    D.SetData := flags;
    D.SrcFileNum := FileNum;
    inc(NumDbgLines);
    result := True;
  end;

begin
  if Assigned(atStartOfPass) then atStartOfPass(self);
  AsmWordsPtr := DWord(@AsmWords);
  NumAsmWords := High(AsmWords);

  totalBytes := 0;
  NumSymbolErrors := 0;
  NumErrors := 0;
  SetLength(QErrors, 0);
  Errors.Free;
  Errors := TStringList.Create;
  LastSymbol := nil;
  LastSymbolLen := 0;
  LastSymbolBeforeStruct := nil;
  LastSymbolLenBeforeStruct := 0;
  CurSymbol := '';
  defStruct := False;
  if pass = 1 then begin
    for i := 0 to High(PageList) do begin
      TZ80MemBlock(PageList[i]).AltLo := 65536;
      TZ80MemBlock(PageList[i]).AltHi := 0;
    end;
    for i := 0 to 31 do AsmSymbols[i].Free;
    for i := 0 to 31 do AsmSymbols[i] := TStringList.Create;
    NumAsmSymbols := 0;
    LastNumErrors := 0;
    LastSymErrors := 0;
    SetLength(AsmSymbolsExt, 2048);
    NumDbgLines := 0;
    SetLength(AsmDbgLines, 0);
  end;
  if SourceFilenames <> nil then SourceFilenames.Free;
  SourceFilenames := TStringList.Create;
  SourceFilenames.Add('');

  i := -1;
  AsmAddr := StartAddress;
  PageName := StartPage;
  if StartAddress >= 0 then asm
    mov [errorNum], -27
    call  GetCurrentPage
    jc  Done
  end else PageValid := False;

  for i := 0 to NumAsmSymbols-1 do begin
    pSymbol := PAsmSymbol(@AsmSymbolsExt[i shl 2]);
    pSymbol.SetData := pSymbol.SetData or SYM_DEAD;
  end;
  LastIf := True;
  IfLevel := 0;
  IfValidLevel := 0;
  InIf := False;
  line := 0;
  FileNum := 0;
  macLevel := 0;
  defMacro := False;
  repBaseLevel := 0;
  repLevel := 0;

AssembleSourceFile:
  while line < Source.Count do begin
    p := pointer(Source.Strings[line]);
    AsmLineAddr := AsmAddr;
    if p <> nil then begin
    asm
    xor eax, eax
    mov ecx, [p]
    cld
    mov [symLen], eax
    test  byte [ecx], $FF
    mov [LineRepCount], eax
    mov [FWLen], eax
    mov [comLen], eax
    je  nextLine
    push  ebx
    push  esi
    push  edi
    mov edx, -30 // error number (if one occurs) - set to "bad symbol" for now
    xor ah, ah
    mov esi, [p]
    mov al, [esi]
    mov bx, $0920
    cmp al, '.'
    je  @localSym
    test [defStruct], 1
    je  @noLocalSym
    dec esi // force local symbol (in structure definitions)
@localSym:
    mov ecx, edx
    mov edx, [LastSymbol]
    inc esi
    and edx, edx
    mov edx, -30  // error -31 (Local symbol before global symbol)
    je  AsmError
    mov edx, ecx
    inc ah
@noLocalSym:
    mov ecx, -1
@checkForSymbol:
    mov al, [esi]
    inc ecx
    and al, al
    je  @gotEOL1
    cmp al, bl
    je  @gotWS1
    cmp al, bh
    je  @gotWS1
    cmp al, ':'
    je  @gotWS1
    cmp al, ';'
    je  @gotWS1
    and ecx, ecx
    je  @FirstChar
    cmp al, '0'
    jc  @invalidLabel
    cmp al, '9'+1
    jc  @validLabelChar
@FirstChar:
    cmp al, '_'
    je  @validLabelChar
    and al, $DF
    cmp al, 'A'
    jc  @invalidLabel
    cmp al, 'Z'+1
    jnc @invalidLabel
@validLabelChar:
    inc esi
    cmp ecx, 126
    jc  @checkForSymbol
@invalidLabel:
    mov edx, -22
    jmp AsmError
@gotWS1:
    and ecx, ecx
    je  @noSymbol
@gotWS2:
    and ah, ah
    lea edi, CurSymbol
    mov al, [edi]
    je  @defGlobalSym
    push  eax
    mov eax, [LastSymbolLen]
    add eax, ecx
    inc al
    mov [edi], al
    mov [symLen], eax
    inc edi
    pop eax
    mov [symStart], edi
    and al, al
    jne @GSCopied
    push  ecx
    push  esi
    push  edi
    mov ecx, [LastSymbolLen]
    mov esi, [LastSymbol]
    rep movsb
    pop   edi
    pop   esi
    pop   ecx
@GSCopied:
    add edi, [LastSymbolLen]
    mov byte [edi], '.'
    sub esi, ecx
    inc edi
    rep movsb
    mov byte [edi], 0
    jmp @gotSymbol
@symbolReserved:
    mov dword [ErrorNum], -29 // "cannot use reserved word as label"
    call  AsmErrorReturn
    pop edx
    pop ecx
    push  ebx
    push  esi
    push  edi
    mov esi, [regESI]
    mov ebx, [regEBX]
    add esi, ecx
    jmp @gotSymbol
@defGlobalSym:
    mov edi, [LastSymbolLen]
    mov [LastSymbolLenBeforeStruct], edi
    mov [LastSymbolLen], ecx
    mov edi, [LastSymbol]
    mov [LastSymbolBeforeStruct], edi
    sub esi, ecx
    lea edi, CurSymbol
    mov byte [edi], cl
    inc edi
    mov [LastSymbol], esi
    mov [symLen], ecx
    mov [symStart], edi
    mov [regEDI], edi
    mov [regESI], esi
    mov [regEBX], ebx
    mov edx, esi
    pop edi
    pop esi
    pop ebx
    push  ecx
    push  edx
    push  ecx
    mov eax, [AsmWordsPtr]
    mov ecx, [NumAsmWords]
    call  IsReservedWord
    cmp eax, 0
    jge @symbolReserved
    pop edx
    pop ecx
    push  ebx
    push  esi
    push  edi
    mov ebx, [regEBX]
    mov esi, [regESI]
    mov edi, [regEDI]
    mov eax, ecx
    rep movsb
    mov byte [edi], 0
@gotSymbol:
    cmp byte [esi], ':'
    jne @noSymbol
    inc esi
@noSymbol:
    xor eax, eax
    dec esi
@checkForWS1:
    inc esi
    mov al, [esi]
    and al, al
    je  @gotEOL
    cmp al, bl
    je  @checkForWS1
    cmp al, bh
    je  @checkForWS1
    lea edi, OpcodeASCIICodes
    mov ecx, esi
    mov bl, ';'
    lea edx, [esi+254]
    mov [InsStart], esi
@checkForInstr:
    and al, al
    je  @gotEOL2
    cmp al, bl
    je  @hasComment
    inc esi
    cmp byte [edi+eax*2], $20
    je  @noWS
    mov ecx, esi
    cmp ecx, edx
    jnc @Line2Long
@noWS:
    mov al, [esi]
    jmp @checkForInstr
@Line2Long:
    mov edx, -38
    jmp AsmError
@hasComment:
    mov eax, esi
    inc eax
    mov [ComStart], eax
@gotEOL2:
    sub ecx, [InsStart]
    je  @gotEOL
@gotIns:
    mov [InsLen], ecx
    mov esi, [InsStart]
    mov bx, $0920
    mov ecx, -1
    mov dx, ';'
@getInsFirstWord:
    mov al, [esi]
    inc ecx
    inc esi
    and al, al
    je  @gotFirstWord
    cmp al, bl
    je  @gotFirstWord
    cmp al, bh
    je  @gotFirstWord
    cmp al, dl
    jne @getInsFirstWord
@gotFirstWord:
    mov esi, [InsStart]
    mov eax, [InsLen]
    mov [FWLen], ecx
    add esi, ecx
    sub eax, ecx
    mov [InsWord2], esi
    mov [InsLen2], eax
    jmp @gotEOL
@gotEOL1:
    and ecx, ecx
    jne @gotWS2
@gotEOL:
    pop edi
    pop esi
    pop ebx
    end;
    goto doLine;

AsmError:
    asm
    pop edi
    pop esi
    pop ebx
    end;
AsmErrorNoPush:
    asm
    mov [errorNum], edx
    end;
    AddErrorQuick(errorNum, Line, FileNum);
    goto nextLine;
AsmErrorReturn:
    AddErrorQuick(errorNum, Line, FileNum);
    asm
    ret
    end;

doEval:
    Evaluate(Pointer(ExprStart), ExprLen);
    asm
    mov esi, Self
    mov eax, [esi+val1]
    mov edx, [esi+resultCode]
    ret
    end;

AEQuick:
    AddErrorQuick(ErrorNum, line, FileNum);
    asm
    ret
    end;

AsmSkipWS:
    asm
    push  ebx
    xor ecx, ecx
    xor ebx, ebx
    lea edx, OpcodeASCIICodes
    mov ch, ' '
@DBWSLoop:
    cmp eax, [InsEnd]
    je  @DBNoWS
    mov bl, [eax]
    mov cl, [edx+ebx*2]
    lea eax, [eax+1]
    and cl, cl
    je  @DBNoWS
    cmp cl, ch
    je  @DBWSLoop
@DBNoWS:
    pop ebx
    lea eax, [eax-1]
    ret
    end;

AsmGetQuotedText:
    asm
    push  ebx
    push  esi
    push  edi
    mov edx, $2227
    lea ebx, tempString+1
    xor ecx, ecx
    lea esi, [ebx+250]
    lea edi, [eax+1]
@QtLoop:
    cmp eax, [InsEnd]
    jnc @QtError
    mov cl, [eax]
    and ch, ch
    lea eax, [eax+1]
    jne @QtInQuote
    and cl, cl
    je  @QtError
    cmp cl, dl
    je  @QtSetQuote
    cmp cl, dh
    jne @QtLoop
@QtSetQuote:
    mov ch, cl
    cmp eax, edi
    je  @QtLoop
@QtError:
    mov byte [ebx], 0
    lea edi, tempString+1
    xor ch, ch
    sub ebx, edi
    mov [edi-1], bl
@QtError2:
    stc
    pop edi
    pop esi
    pop ebx
    ret
@QtInQuote:
    cmp ebx, esi
    mov [ebx], cl
    lea ebx, [ebx+1]
    jnc @QtError
    cmp cl, ch
    jne @QtLoop
    dec ebx
    lea edi, tempString+1
    mov byte [ebx], 0
    xor ch, ch
    sub ebx, edi
    mov [edi-1], bl
    je  @QtError2
    clc
    pop edi
    pop esi
    pop ebx
    ret
    end;

AsmGetSep:
    asm
    cmp byte [eax], ','
    jne @error
    inc eax
    call  AsmSkipWS
    je  @error
    clc
    ret
@error:
    stc
    ret
    end;

AsmGetArg:
    asm
    bswap ecx
    push  ebx
    push  esi
    push  edi
    mov edx, $2227
    lea ebx, tempString+1
    xor cx, cx
    lea esi, [ebx+250]
    lea edi, tempString+1
@QtLoop:
    cmp eax, [InsEnd]
    jnc @QtDone2
    mov cl, [eax]
    and ch, ch
    lea eax, [eax+1]
    jne @QtInQuote
    and cl, cl
    je  @QtDone
    cmp cl, ','
    je  @QtDone
    cmp cl, dl
    je  @QtSetQuote
    cmp cl, dh
    je  @QtSetQuote
    cmp cl, '<'
    je  @QtSetBrk
    bt  ecx, 31
    jnc @QtCopyByte
    cmp cl, ' '
    je  @QtDone
    cmp cl, 09
    je  @QtDone
@QtCopyByte:
    cmp edi, esi
    mov [edi], cl
    lea edi, [edi+1]
    jc  @QtLoop
    mov ecx, edi
    lea ebx, tempString+1
    sub ecx, ebx
    mov [ebx-1], cl
    pop edi
    pop esi
    pop ebx
    stc
    ret
@QtSetBrk:
    mov cl, '>'
@QtSetQuote:
    cmp edi, ebx
    jne @QtCopyByte
    mov ch, cl
    jmp @QtLoop
@QtInQuote:
    cmp cl, ch
    jne @QtCopyByte
    jmp @QtDone2
@QtDone:
    dec eax
@QtDone2:
    mov ecx, edi
    lea ebx, tempString+1
    sub ecx, ebx
    mov [ebx-1], cl
    pop edi
    pop esi
    pop ebx
    clc
    ret
    end;

AsmGetExpr:
    asm
    mov [exprStart], eax
    mov [ValSet], cl
    push  ebx
    push  esi
    push  edi
    mov dword [InstrVal], 0
    mov esi, eax
    xor edx, edx
    xor ebx, ebx
    xor eax, eax
    xor ch, ch
    lea edi, OpcodeASCIICodes
    dec esi
@AsmExpLoop:
    inc esi // need to preserve the Z flag
    cmp esi, [InsEnd]
    jnc @AsmExpEnd
    mov al, [esi]
    and ch, ch
    jne @ExpInQuote
    mov dx, [edi+eax*2]
    cmp dl, ' '
    je  @AsmExpNextByte
    cmp dl, '"'
    je  @AsmSetQuote
    cmp dl, 39
    je  @AsmSetQuote
    cmp dl, ','
    je  @AsmExpEnd
    xor cl, cl
@AsmExpNextByte:
    jmp @AsmExpLoop
@AsmExpEnd:
    and cl, cl
    jne @ExpAllQuotes
    lea ecx, [esi]
    pop edi
    mov eax, esi
    sub ecx, [exprStart]
    pop esi
    mov [exprLen], ecx
    pop ebx
    call doEval
    mov ecx, 1
    bt  edx, 31
    jnc @ExprOK
    bt  edx, 30
    mov edx, -07  // "symbol undefined"
    jc  @ExprError
    mov edx, -32 // "bad expression"
    jmp @ExprError
@AsmSetQuote:
    mov ch, al
    jmp @AsmExpNextByte
@AsmSetForFullEval:
    xor cl, cl
    jmp @AsmExpNextByte
@ExpInQuote:
    inc ebx
    cmp al, ch
    jne @AsmExpNextByte
    cmp ebx, 3
    jnc @ExpAllQuotes
    xor cx, cx
    jmp @AsmExpNextByte
@ExpAllQuotes:
    lea eax, [esi+1]
    pop edi
    mov ecx, eax
    pop esi
    sub ecx, [exprStart]
    pop ebx
    cmp ecx, ecx
    mov [exprLen], ecx
    ret
@ExprOK:
    xor edx, edx
    inc edx
    ret
@ExprError:
    bt  edx, 31
    ret
    end;

GetCurrentPage:
  asm
    pushad
  end;
  for i := 0 to High(PageList) do
    if (PageName = PageList[i].Name) and
    (AsmAddr >= PageList[i].Z80Start) and
    (AsmAddr < PageList[i].Z80End) then break;
  if (i < 0) or (i > High(PageList)) then begin
    AddErrorQuick(errorNum, Line, FileNum);
    asm
      mov [PageValid], 0
      popad
      stc
      ret
    end;
  end else CurPage := PageList[i];
  asm
    mov [PageValid], 1
    popad
    clc
    ret
  end;

AddSymbol:
    i := 0;
    for i2 := 1 to Length(CurSymbol) do i := i + (ord(CurSymbol[i2]) and $DF);
    i := i and $1F;
    i2 := AsmSymbols[i].IndexOf(CurSymbol);
    if i2 >= 0 then begin
      pSymbol := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[i].Objects[i2])]);
      if (pSymbol.SetData and (SYM_SET+SYM_MACRO)) > 0 then begin
        if FirstWord <> 93 then begin
          AddErrorQuick(-55, Line, FileNum); // "cannot change symbol type"
          i2 := -1;
        end;
      end else begin
        if Integer(p) <> AsmSymbolsExt[Integer(AsmSymbols[i].Objects[i2])+3] then
          AddErrorQuick(-31, Line, FileNum); // "symbol redefined"
      end;
    end;
    if (i2 < 0) or (FirstWord = 93) then begin
      if i2 < 0 then begin
        i2 := NumAsmSymbols shl 2;
        inc(NumAsmSymbols);
      end;
      if FirstWord = 93 then i3 := SYM_SET else i3 := 0;

      if (High(AsmSymbolsExt)) <= (i2+4) then
        SetLength(AsmSymbolsExt, i2+256);
      pSymbol := PAsmSymbol(@AsmSymbolsExt[i2]);
      pSymbol.StartAddress := InstrVal;
      pSymbol.SetData := i3;
      pSymbol.SrcFileNum := FileNum;
      pSymbol.Page := DWord(CurPage);
      pSymbol.DefinedAt := DWord(p);
      AsmSymbols[i].Sorted := False;
      AsmSymbols[i].Add(CurSymbol);
      AsmSymbols[i].Objects[AsmSymbols[i].Count-1] := TObject(i2);
    end else i := -1;
    asm
    ret
    end;

doLine:
    asm
    mov ecx, [FWLen]
    push  ebx
    push  esi
    push  edi
    xor eax, eax
    mov esi, [InsStart]
    lea edi, Instruction+1
    lea ebx, OpcodeASCIICodes
    inc ecx
    jmp @FWCheck
@UCFirstWordLoop:
    mov al, [esi]
    inc esi
    mov al, [ebx+eax*2]
    mov [edi], al
    inc edi
@FWCheck:
    loop  @UCFirstWordLoop
    pop edi
    pop esi
    pop ebx
@endOfInstr:
    end;
ProcessInstruction:
    oldPage := curPage;
    if FWLen > 0 then begin
      FirstWord := IsReservedWord(@AsmWords, Pointer(InsStart), High(AsmWords), FWLen);
      if (FirstWord < 0) and (FWLen = 1) then begin
        if pChar(InsStart)[0] = '=' then FirstWord := 34;
      end;
    end
    else FirstWord := 1000;
    case FirstWord of
      168:
        begin // macro
          if defMacro then AddErrorQuick(-25, Line, FileNum) // "nested macro definition"
          else begin
            defMacro := True;
          end;
        end;
      169:
        begin // endmacro
          if not defMacro then AddErrorQuick(-26, Line, FileNum) // "ENDM outside macro def"
          else begin
            defMacro := False;
            goto nextLine;
          end;
        end;
     end;
    if defMacro then goto nextLine;
    if LastIf then begin
      if SymLen > 0 then begin
        LastSymbolDef := LastSymbol;
        LastSymbolDefLen := LastSymbolLen;
      end;
      if (SymLen > 0) and defStruct then begin
        InstrVal := StructOffset;
        asm
        call  AddSymbol
        end;
      end else if (SymLen > 0) and ((FirstWord <> 34) and (FirstWord <> 183) and (FirstWord <> 103)) then begin
        if FirstWord = 93 then goto EncodeInstruction;
        if not PageValid then begin
          AddErrorQuick(-16, Line, FileNum); // "No ORG specified"
          goto Done;
        end;
        InstrVal := AsmAddr;
        asm
        call  AddSymbol
        end;
      end;
    end;

    case FirstWord of
    1000: goto NextLine;
      65: // ORG
        if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        add eax, [FWLen]
        mov [InsEnd], ecx
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [LastInsSection], eax
        je  AsmErrorNoPush
        end;

        asm
          mov eax, [LastInsSection]
          mov edx, [InsEnd]
          dec eax
          mov ch, ':'
          lea edx, tempString[0]
@getORGPage:
          cmp eax, [InsEnd]
          lea edx, [edx+1]
          jnc noORGPage
          mov cl, [eax+1]
          inc eax
          cmp cl, ch
          je  @gotORGPage
          mov [edx], cl
          jmp @getORGPage
@gotORGPage:
          inc eax
          lea ecx, tempString[1]
          mov dword [LastInsSection], eax
          mov eax, edx
          sub eax, ecx
          mov [ecx-1], al
        end;
        if Length(tempString) = 0 then goto noORGPage;
        tempString := uppercase(tempString);
        if High(PageList) = 0 then begin
            AddErrorQuick(-54, Line, FileNum); // "output mode does not support pages"
            goto done;
        end else PageName := TempString;
        asm
noORGPage:
        mov eax, [LastInsSection]
        mov ecx, [InsEnd]
        mov edx, -10
        sub ecx, eax
        je  @invalidORG
        mov [ExprStart], eax
        mov [ExprLen], ecx
        push  ebx
        push  esi
        push  edi
        call  doEval
        pop edi
        pop esi
        pop ebx
        bt  edx, 31
        mov edx, -10 // "bad ORG expression"
        jnc @orgExprOK
        bt  edx, 30
        jnc @InvalidORG
        mov edx, -50  // "expression must evaluate"
@InvalidORG:
        jmp AsmErrorNoPush
@orgExprOK:
        mov [InstrVal], eax
        end;

        for i := 0 to High(PageList) do
          if (PageName = PageList[i].Name) and
          (InstrVal >= PageList[i].Z80Start) and
          (InstrVal < PageList[i].Z80End) then break;
        if (i < 0) or (i > High(PageList)) then begin
          AddErrorQuick(-27, Line, FileNum);
          PageValid := False;
          goto Done;
        end else begin
          CurPage := PageList[i];
          PageValid := True;
          AsmAddr := InstrVal;
          AsmLineAddr := AsmAddr;
        end;
        goto  nextLine;
        end;
      42, 163, 164, 166, 167: // IF, IFDEF, IFNDEF, IFEQ, IFNE
        begin
          if IFValidLevel = IFLevel then inc(IFValidLevel);
          inc(IFLevel);
          if IfLevel > 62 then begin
            AddErrorQuick(-61, Line, FileNum);
          end else if LastIF then begin
            asm
            mov eax, [InsStart]
            mov ecx, [InsLen]
            add ecx, eax
            mov [InsEnd], ecx
            add eax, [FWLen]
            call  AsmSkipWS
            mov ecx, [FirstWord]
            jne @IfHasExpr
            inc ecx
            mov edx, -59 // "symbol expected"
            and ecx, $FE
            cmp ecx, 164
            je  AsmErrorNoPush
            mov edx, -45 // "expression expected"
            jmp AsmErrorNoPush
@IfHasExpr: mov [exprStart], eax
            mov ecx, [InsEnd]
            sub ecx, eax
            mov [exprLen], ecx
            end;
            case FirstWord of
              163, 164: // IFDEF, IFNDEF
                begin
                  asm
                  mov eax, [exprStart]
                  mov ecx, 1
                  call  AsmGetArg
                  mov edx, -59 // "symbol expected"
                  je  AsmErrorNoPush
                  call  AsmSkipWS
                  mov edx, -48 // "garbage following instruction"
                  jne AsmErrorNoPush
                  end;

                  i := 0;
                  for i2 := 1 to Length(tempString) do i := i + (ord(tempString[i2]) and $DF);
                  i := i and $1F;
                  i2 := AsmSymbols[i].IndexOf(tempString);
                  IfTest := (FirstWord = 164);
                  if i2 >= 0 then begin
                    pSymbol := PAsmSymbol(@AsmSymbolsExt[Integer(AsmSymbols[i].Objects[i2])]);
                    IfTest := IfTest xor ((pSymbol.SetData and SYM_DEAD) = 0);
                  end;
                end;
              42, 166, 167: // IF, IFEQ, IFNE
                begin
                  asm
                  mov eax, [exprStart]
                  call  AsmGetExpr
                  jc  AsmErrorNoPush
                  mov edx, -60
                  je  AsmErrorNoPush
                  mov [InstrVal], eax
                  end;
                  IfTest := (InstrVal <> 0) xor (FirstWord = 167);
                end;
              end;
            IfStack[IFLevel-1] := (byte(LastIF) and 1) + ((byte(InIF) and 1) shl 1);
            LastIF := IfTest;
            InIF := True;
            if not LastIF then dec(IFValidLevel);
            goto nextLine
          end;
        end;

    30: // ELSE
      begin
        if IFLevel = 0 then AddErrorQuick(-18, Line, FileNum) // ELSE
        else if not InIF then AddErrorQuick(-19, Line, FileNum)
        else begin
          LastIF := not LastIF;
          InIF := False;
        end;
        goto NextLine;
      end;

    31: // ELSEIF
      begin
        if IFLevel = 0 then AddErrorQuick(-20, Line, FileNum) // ELIF
        else if not InIF then AddErrorQuick(-19, Line, FileNum)
        else begin
          LastIF := not LastIF;
          InIF := False;
        end;
        goto NextLine;
      end;

    33: // ENDIF
      begin
        if IfLevel = 0 then begin
          AddErrorQuick(-17, Line, FileNum);
        end else begin
          if IFValidLevel = IFLevel then dec(IFValidLevel);
          dec(IFLevel);
          if IFLevel = 0 then begin
            LastIF := True;
            InIF := False;
          end else begin
            if IFLevel <= IFValidLevel then begin
              LastIF := Boolean(IfStack[IFLevel-1] and 1);
              InIF := Boolean((IfStack[IFLevel-1] and 2) shr 1);
            end;
          end;
        end;
        goto NextLine;
      end;

      end;
    if not LastIF then goto nextLine;
    case FirstWord of
      103: // STRUCT
        begin
          if SymLen = 0 then AddErrorQuick(-34, Line, FileNum) // "symbol required"
          else if defStruct then AddErrorQuick(-56, Line, FileNum) // "nested structure"
          else begin
            InstrVal := 0;
            asm
              mov eax, [InsStart]
              mov ecx, [InsLen]
              add ecx, eax
              mov [InsEnd], ecx
              add eax, [FWLen]
              call  AsmSkipWS
              mov edx, -45 // "expression expected"
              mov [exprStart], eax
              mov ecx, [InsEnd]
              je  @struct_ok
              sub ecx, eax
              mov [exprLen], ecx

              mov [minVal], -128
              mov [maxVal], 256
              mov [DBSize], 1
              mov [DBFlag], 1
              mov cl, byte [DBFlag]
              call  AsmGetExpr
              mov [InstrVal], eax
              setne [DBType]
              jc  AsmErrorNoPush

              mov eax, [exprStart]
              add eax, [exprLen]
              call  AsmSkipWS
              je  @struct_OK
              mov edx, -48 // "garbage following instruction"
              jmp AsmErrorNoPush
@struct_ok: end;
            defStruct := True;
            StructOffset := InstrVal;
          end;
          goto NextLine;
        end;
      183: // OFFSET (AT)
        begin
          if not defStruct then AddErrorQuick(-56, Line, FileNum) // "nested structure"
          else begin
            asm
              mov eax, [InsStart]
              mov ecx, [InsLen]
              add ecx, eax
              mov [InsEnd], ecx
              add eax, [FWLen]
              call  AsmSkipWS
              mov edx, -45 // "expression expected"
              mov [exprStart], eax
              mov ecx, [InsEnd]
              je  AsmErrorNoPush
              sub ecx, eax
              mov [exprLen], ecx

              mov [minVal], -128
              mov [maxVal], 256
              mov [DBSize], 1
              mov [DBFlag], 1
              mov cl, byte [DBFlag]
              call  AsmGetExpr
              mov [InstrVal], eax
              setne [DBType]
              jc  AsmErrorNoPush

              mov eax, [exprStart]
              add eax, [exprLen]
              call  AsmSkipWS
              je  @struct_OK
              mov edx, -48 // "garbage following instruction"
              jmp AsmErrorNoPush
@struct_ok: end;
            StructOffset := InstrVal;
            if SymLen > 0 then asm
              call  AddSymbol
            end;
          end;
          goto NextLine;
        end;
      174: // ENDSTRUC (ENDSTRUCT)
        begin
          if not defStruct then AddErrorQuick(-57, Line, FileNum) // "ENDSTRUCT outside structure def"
          else begin
            defStruct := False;
            InstrVal := StructOffset;
            CurSymbol := Copy(CurSymbol, 1, LastSymbolLen)+'.sizeof';
            asm
              call  AddSymbol
            end;
            LastSymbol := LastSymbolBeforeStruct;
            LastSymbolLen := LastSymbolLenBeforeStruct;
            if LastSymbol <> nil then
              CurSymbol := Copy(pchar(LastSymbol),1,LastSymbolLen)
            else CurSymbol := '';
          end;
          goto nextLine;
        end;
      34: // EQU
DoEQU_SET:
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        add eax, [FWLen]
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [exprStart], eax
        mov ecx, [InsEnd]
        je  AsmErrorNoPush
        sub ecx, eax
        mov [exprLen], ecx
        call doEval
        bt  edx, 31
        mov [InstrVal], eax
        jnc @EQU_OK
@EQU_Err:
        bt  edx, 30
        mov edx, -50 // "expression must evaluate"
        jnc @EQU_Err2
        mov edx, -07 // "symbol undefined"
        inc dword [NumSymbolErrors]
@EQU_Err2:
        jmp AsmErrorNoPush
@EQU_OK:
        end;
        if SymLen = 0 then AddErrorQuick(-34, Line, FileNum) // "symbol required"
        else asm
        call  AddSymbol
        end;
        goto nextLine
        end;
      46: // INCLUDE
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        mov eax, [InsWord2]
        call  AsmSkipWS
        mov edx, -52 // "filename expected"
        mov [LastInsSection], eax
        je  AsmErrorNoPush
        call  AsmGetQuotedText
        mov edx, -08 // "invalid filename"
        jc  AsmErrorNoPush
        call  AsmSkipWS
        mov edx, -48 // "garbage following instruction"
        jne AsmErrorNoPush
        end;
        tempString := uppercase(tempString);
        i := SourceFilenames.IndexOf(tempString); // check for recursive include
        if i >= 0 then AddErrorQuick(-51, Line, FileNum) // "recursive include"
        else begin // not recursive, but file might have been loaded already
          i := SourceFiles.IndexOf(tempString);
          if i < 0 then begin // file isn't in memory so load it
            IncludeSrc := TStringList.Create;
            ErrorNum := 0;
            if FileExists(tempString) then begin
              try
                IncludeSrc.LoadFromFile(tempString);
              except // error if we can't read the source file
                IncludeSrc.Free;
                ErrorNum := -53;  // "can't read source file"
              end;
            end else begin // can't find the file on disk
              IncludeSrc.Free;
              ErrorNum := -12; // "file not found"
            end;
            if ErrorNum <> 0 then begin
              AddErrorQuick(errorNum, Line, FileNum);
              goto nextLine;
            end;
            i := SourceFiles.Add(tempString);
            SourceFiles.Objects[i] := TObject(IncludeSrc);
          end;
          // if we got this far the source is in memory
          i2 := SourceFilenames.Add(tempString);
          SourceFilenames.Objects[i2] := TObject(Line);
          repBase[i2] := repBaseLevel;
          FileNum := i;
          Line := -1;
          Source := TStringList(SourceFiles.Objects[i]);
          goto nextLine
        end;
        goto nextLine;
        end;
      end;
    if not PageValid then begin
      AddErrorQuick(-16, Line, FileNum); // "no ORG specified"
      goto Done;
    end else case FirstWord of
      149: // INCBIN
        if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
        asm
          mov eax, [InsStart]
          mov ecx, [InsLen]
          add ecx, eax
          mov [InsEnd], ecx
          add eax, [FWLen]
          call  AsmSkipWS
          mov edx, -52 // "filename expected"
          mov [LastInsSection], eax
          je  AsmErrorNoPush
          call  AsmGetQuotedText
          mov edx, -08 // "invalid filename"
          jc  AsmErrorNoPush
          call  AsmSkipWS
          mov edx, -48 // "garbage following instruction"
          jne AsmErrorNoPush
        end;
        i2 := retFileSize(tempString);
        if i2 = -1 then
          AddErrorQuick(-12, Line, FileNum) // "file not found"
        else if ((i2 < 0) or (i2 > 65535)) then
          AddErrorQuick(-13, Line, FileNum) // "invalid file size"
        else begin
          if pass=1 then
            try F := TFileStream.Create(tempString, fmOpenRead or fmShareDenyWrite);
            except
              AddErrorQuick(-14, Line, FileNum);
              F := nil;
          end else F := nil;
          while i2 > 0 do begin
            if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then asm
              mov [errorNum], -27
              call GetCurrentPage
            end;
            if not PageValid then goto nextLine;
            i3 := CurPage.Z80End - AsmAddr;
            if i3 > i2 then i3 := i2;
            if F <> nil then begin
              p2 := PByteArray(pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start)));
              try fBytesRead := F.Read(p2^, i3);
              except AddErrorQuick(-14, Line, FileNum);
              end;
              inc(TotalBytes, i3);
            end;
            if CreateExtraInfo then AddDbgInfo(AsmAddr, i3, DBG_BYTE);
            inc(AsmAddr, i3);
            dec(i2, i3);
          end;
          if F <> nil then F.Free;
        end;
        goto  nextLine;
        end;
      20,21,23: // DEFB, DEFW, DEFM
        if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
        asm
        mov eax, [InsStart]
        mov ecx, eax
        add eax, [FWLen]
        add ecx, [InsLen]
        mov [InsEnd], ecx
        call  AsmSkipWS
        mov edx, -47
        je  DBError
        cmp [FirstWord], 23
        mov [minVal], -32768
        mov [maxVal], 65536
        mov [DBFlag], 2
        mov [DBSize], 2
        je  DBLoop
        mov [minVal], -128
        mov [maxVal], 256
        mov [DBSize], 1
        mov [DBFlag], 1
        jmp DBLoop
DBLoop:
        mov cl, byte [DBFlag]
        call  AsmGetExpr
        mov [InstrVal], eax
        setne [DBType]
        jnc @DBWrite
DBError:
        mov [ErrorNum], edx
        push  eax
        call  AsmErrorReturn
        pop eax
        cmp dword [ErrorNum], -07
        mov [DBType], 1
        mov dword [InstrVal], 0
        jne nextLine
        inc dword [NumSymbolErrors]
@DBWrite:
        end;
        if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
          asm
            mov [errorNum], -46 // "instruction violates page bounds"
            call  GetCurrentPage
          end;
          if not PageValid then goto NextLine;
        end;
        p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
        if DBType then begin // DB 1, 2, 3 etc.
          if DBSize = 1 then begin
            if (InstrVal < $FFFFFF80) and (InstrVal > 255) then begin
              AddErrorQuick(-03, Line, FileNum);
              goto NextLine;
            end else begin
              AddDbgInfo(AsmAddr, 1, DBG_BYTE);
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              pByteArray(p2)[0] := InstrVal;
              inc(AsmAddr, DBSize);
              inc(TotalBytes);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end else begin
            if (InstrVal < $FFFF8000) and (InstrVal > 65535) then begin
              AddErrorQuick(-05, Line, FileNum);
              goto NextLine;
            end else if (CurPage.Z80End-AsmAddr) > 1 then begin
              AddDbgInfo(AsmAddr, 2, DBG_WORD);
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              pWordArray(p2)[0] := InstrVal;
              inc(AsmAddr, DBSize);
              inc(TotalBytes, 2);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end else begin
              AddDbgInfo(AsmAddr, 2, DBG_WORD);
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              pByteArray(p2)[0] := InstrVal and 255;
              inc(TotalBytes);
              inc(AsmAddr);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
              asm
                mov [errorNum], -46 // "instruction violates page bounds"
                call  GetCurrentPage
              end;
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              inc(AsmAddr);
              if not PageValid then goto nextLine;
              p2 := pointer(Integer(CurPage.StartAddress)+((AsmAddr-1)-CurPage.Z80Start));
              pByteArray(p2)[0] := InstrVal shr 8;
              inc(TotalBytes);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end;
        end else if ExprLen > 2 then begin // DB "string"
          i2 := ExprLen - 2;
          p3 := Pointer(ExprStart+1);
          if (AsmAddr+i2) <= CurPage.Z80End then begin
            AddDbgInfo(AsmAddr, i2, DBG_TEXT);
            if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
            for i := 0 to i2-1 do PByteArray(p2)[i] := PByteArray(p3)[i];
            inc(totalBytes, i2);
            inc(AsmAddr, i2);
            if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
          end else begin
            i4 := 0;
            AddDbgInfo(AsmAddr, i2, DBG_TEXT);
            while i2 > 0 do begin
              if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
                asm
                  mov [errorNum], -46 // "instruction violates page bounds"
                  call  GetCurrentPage
                end;
                if not PageValid then goto NextLine;
              end;
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              i3 := CurPage.Z80End - AsmAddr;
              if i3 > i2 then i3 := i2;
              p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
              for i5 := 0 to i3-1 do PByteArray(p2)[i5] := PByteArray(p3)[i5+i4];
              inc(i4, i3);
              inc(AsmAddr, i3);
              inc(totalBytes, i3);
              dec(i2, i3);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end;
        end;
        ExprStart := ExprStart + ExprLen;
        asm
          mov eax, [exprStart]
          call  AsmSkipWS
          je  @DBDone
          call  AsmGetSep
          mov edx, -48 // "garbage following instruction"
          jc  AsmErrorNoPush
          jmp DBLoop
@DBDone:
        end;
        goto nextLine;
        end;

      22, 27, 173: // DEFS (RESB), DEFSW (RESW)
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        add eax, [FWLen]
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [exprStart], eax
        mov ecx, [InsEnd]
        je  AsmErrorNoPush
        sub ecx, eax
        mov [exprLen], ecx

        mov [minVal], -128
        mov [maxVal], 256
        mov [DBSize], 1
        mov [DBFlag], 1
        mov cl, byte [DBFlag]
        call  AsmGetExpr
        mov [InstrVal], eax
        setne [DBType]
        jc  AsmErrorNoPush

        mov edx, [self]
        mov eax, [exprStart]
        mov cl, byte [edx].TZ80Assembler.DEFSDefault
        add eax, [exprLen]
        mov byte [ValSet], cl
        call  AsmSkipWS
        je  @DEFS_OK
        call  AsmGetSep
        mov edx, -48 // "garbage following instruction"
        jc  AsmErrorNoPush
        mov [minVal], -128
        mov [maxVal], 256
        mov [DBSize], 1
        mov [DBFlag], 1
        mov cl, byte [DBFlag]
        push  [InstrVal]
        call  AsmGetExpr
        pop [InstrVal]
        mov [ValSet], al
        setne [DBType]
        jc  AsmErrorNoPush

@DEFS_OK:
        end;
        if FirstWord = 173 then DBSize := 2 else DBSize := 1;
        i2 := InstrVal*DBSize;
        if defStruct then StructOffset := (StructOffset+i2) and $FFFF
        else begin
DEFSFill:
          if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
            asm
              mov [errorNum], -46 // "instruction violates page bounds"
              call  GetCurrentPage
            end;
            if not PageValid then goto NextLine;
          end;
          if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
          if (AsmAddr+i2) <= CurPage.Z80End then begin
            p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
            if DBSize = 1 then AddDbgInfo(AsmAddr, i2, DBG_BYTE)
            else AddDbgInfo(AsmAddr, i2, DBG_WORD);
            for i := 0 to i2-1 do PByteArray(p2)[i] := valSet;
            inc(totalBytes, i2);
            inc(AsmAddr, i2);
            if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
          end else begin
            while i2 > 0 do begin
              if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then begin
                asm
                  mov [errorNum], -46 // "instruction violates page bounds"
                  call  GetCurrentPage
                end;
                if not PageValid then goto NextLine;
              end;
              if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
              i3 := CurPage.Z80End - AsmAddr;
              if i3 > i2 then i3 := i2;
              p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
              if DBSize = 1 then AddDbgInfo(AsmAddr, i3, DBG_BYTE)
              else AddDbgInfo(AsmAddr, i3, DBG_WORD);
              for i5 := 0 to i3-1 do PByteArray(p2)[i5] := valSet;
              inc(AsmAddr, i3);
              inc(totalBytes, i3);
              dec(i2, i3);
              if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
            end;
          end;
        end;
        goto nextLine;
        end;

      175: // REP, REPEAT, REPT
        begin
        asm
        cmp [LineRepCount], 0
        mov edx, -66 // "cannot nest single-line repeats"
        jne @repError
        mov eax, [InsStart]
        mov ecx, eax
        add eax, [FWLen]
        add ecx, [InsLen]
        mov [InsEnd], ecx
        call  AsmSkipWS
        mov edx, -47
        je  @repError

        mov cl, 02
        call  AsmGetExpr
        mov [InstrVal], eax
        jnc @RepValOK
@repError:
        mov [ErrorNum], edx
        push  eax
        call  AsmErrorReturn
        cmp dword [ErrorNum], -07
        pop eax
        jne nextLine
        inc dword [NumSymbolErrors]
        jmp nextLine
@repValOK:
        mov eax, [ExprStart]
        cmp [InstrVal], 1
        mov edx, -64 // "invalid repeat count"
        jc  @repError
        cmp [InstrVal], 65537
        jnc  @repError
        add eax, [ExprLen]
        cmp eax, [InsEnd]
        jnc @SetMultilineRepeat
        cmp byte [eax], ','
        je  @SetSingleLineRepeat
        call  AsmSkipWS
        je  @SetMultiLineRepeat
        mov edx, -48 // "garbage following instruction"
        jmp @repError
@SetSingleLineRepeat:
        inc eax
        call  AsmSkipWS
        mov edx, -65 // "bad repeat statement"
        mov ecx, [InsEnd]
        je  @repError
        mov [InsStart], eax
        sub ecx, eax
        mov edx, [InstrVal]
        mov [LineRepStart], eax
        dec edx
        mov [InsLen], ecx
        mov [LineRepCount], edx
        push  ebx
        push  esi
        push  edi
        mov esi, eax
        mov ecx, [InsEnd]
        mov bx, $0920
        lea edx, OpcodeASCIICodes
        lea edi, Instruction+1
        xor eax, eax
@getInsFirstWord:
        cmp esi, ecx
        jnc @gotFirstWord
        mov al, [esi]
        mov al, [edx+eax*2]
        and al, al
        je  @gotFirstWord
        cmp al, bl
        je  @gotFirstWord
        mov [edi], al
        inc esi
        inc edi
        jmp @getInsFirstWord
@gotFirstWord:
        mov eax, esi
        mov ecx, [InsLen]
        mov [InsWord2], esi
        sub eax, [InsStart]
        sub ecx, eax
        mov [FWLen], eax
        mov [InsLen2], ecx

        pop edi
        pop esi
        pop ebx
        jmp ProcessInstruction
@SetMultiLineRepeat:
        end;
        if repLevel = 63 then begin
          AddErrorQuick(-67, Line, FileNum); // "rep stack overflow"
        end else begin
          repStack[repLevel].repLine := Line;
          repStack[repLevel].repCount := InstrVal;
          inc(RepLevel);
        end;
        goto NextLine;
        end;

      176: // ENDR, ENDREPEAT, ENDREP
        begin
          if FWLen <> InsLen then begin
            AddErrorQuick(-48, Line, FileNum); // "garbage following instruction"
          end else if repLevel = repBaseLevel then begin
            AddErrorQuick(-68, Line, FileNum); // "spurious ENDR"
          end else begin
            dec(repStack[repLevel-1].repCount);
            if repStack[repLevel-1].repCount > 0 then begin
              Line := repStack[repLevel-1].repLine;
            end else begin
              dec(repLevel);
            end;
          end;
          goto nextLine;
        end;

      02: // ALIGN
        begin
        asm
        mov eax, [InsStart]
        mov ecx, [InsLen]
        add ecx, eax
        mov [InsEnd], ecx
        add eax, [FWLen]
        call  AsmSkipWS
        mov edx, -45 // "expression expected"
        mov [exprStart], eax
        mov ecx, [InsEnd]
        je  AsmErrorNoPush
        sub ecx, eax
        mov [exprLen], ecx
        call doEval
        bt  edx, 31
        mov [InstrVal], eax
        jnc @ALIGN_OK
@ALIGN_Err:
        bt  edx, 30
        mov edx, -50 // "expression must evaluate"
        jnc @ALIGN_Err2
        mov edx, -07 // "symbol undefined"
@ALIGN_Err2:
        jmp AsmErrorNoPush
@ALIGN_OK:
        end;
        if InstrVal <= 0 then begin
          AddErrorQuick(-62, Line, FileNum); // "invalid ALIGN value"
          goto nextLine
        end else if defStruct then begin
          if (StructOffset mod InstrVal) > 0 then inc(StructOffset, InstrVal-(StructOffset mod InstrVal));
        end else if (AsmAddr mod InstrVal) > 0 then begin
          i2 := InstrVal-(AsmAddr mod InstrVal);
          goto DEFSFill;
        end;
        goto NextLine;
        end;

      32: // END
        goto NextFile;

      else if defStruct then AddErrorQuick(-58, Line, FileNum) // "not allowed inside structure def"
        else begin
EncodeInstruction:
        asm
        mov eax, [FirstWord]
        push  ebx
        push  esi
        push  edi
        mov [regEBX], ebx
        mov [regESI], esi
        mov [regEDI], edi
        cmp eax, -1
        mov edx, -09 // "unrecognised instruction"
        mov dword [exprStart], 0
        je  AsmError
        mov ecx, [InsLen]
        mov edx, [FWLen]
        mov esi, [InsStart]
        lea edi, Instruction+1
        add ecx, esi
        mov [InsEnd], ecx
        add esi, edx
        add edi, edx
        lea edx, OpcodeASCIICodes
        mov [LastInsSection], edi
        mov dword [exprMode], 1 shl 21
        mov dword [ParenthLevel], 0
        mov dword [IXDisp], 0
        mov byte [IXSign], -1
        mov byte [ValSet], 0
        cmp eax, 04  // BIT
        je  @bitExpr
        cmp eax, 74  // RES
        je  @bitExpr
        cmp eax, 93  // SET
        jne  @startInstrSection
@bitExpr:
        bts dword [exprMode], 0
@startInstrSection:
        cmp esi, [insEnd]
        jnc @endOfInstr
        mov byte [edi], 32
        xor eax, eax
        mov ebx, $2227
        inc edi
        xor ecx, ecx
        mov [LastInsSection], edi
        mov dword [exprStart], 0
        mov dword [lastWords], 0
        je  @InstrSectionWS
        mov dword [exprStart], edi
@InstrSectionWS:
        mov al, [esi]
        cmp byte [edx+eax*2], 32
        jne @getInstrSectionBody
        inc esi
        cmp esi, [insEnd]
        jc  @InstrSectionWS
        jmp @endOfInstr
@getInstrSectionBody:
        mov [LastSrcSection], esi
@getInstrSectionBody2:
        mov al, [esi]
        and cl, cl
        lea esi, [esi+1] // need to preserve the Z flag
        mov ch, [edx+eax*2]
        jne @inQuote
@getInstrSectionBody3:
        cmp ch, ','
        je @nextInstrSection
        cmp byte [edx+1+eax*2], 1
        je  @alpha
        jc  @checkQuote
@charHasCode:
        push  ecx
        mov cl, [edx+1+eax*2]
        shl dword [LastWords], 8
        mov byte [LastWords], cl
        cmp cl, $FE
        jnc  @numeric
        cmp cl, $F0
        je  @openBracket
        cmp cl, $F1
        je  @closeBracket
        and cl, $FE
        cmp cl, $F2
        jne @noIXDispYet
        mov ecx, [LastWords]
        and ecx, $FFFEFE
        cmp ecx, $F06CF2
        je  @hasIXdisp
        jmp @numeric
@noIXDispYet:
        pop ecx
@checkQuote:
        cmp ch, bl
        je  @setQuote
        cmp ch, bh
        je  @SetQuote
@checkSpace:
        cmp ch, ' '
        mov al, byte [edi-1]
        jne @copyByte
        cmp byte [edi-1], ch
        mov al, [edx+1+eax*2]
        je  @noSetQuote
        cmp al, 1
        jne  @noSetQuote
@copyByte:
        mov byte [edi], ch
        inc edi
@noSetQuote:
        cmp esi, [insEnd]
        jc  @getInstrSectionBody2
        jmp @nextInstrSection
@setQuote:
        cmp dword [ParenthLevel], 0
        mov cl, ch
        jne  @setBrckt3
        bts dword [exprMode], 16
@setBrckt3:
        cmp dword [exprStart], 0
        jne @copyByte
        mov [exprStart], edi
        jmp @copyByte
@inQuote:
        cmp ch, cl
        je  @rmvQuote
        mov ch, al
        jmp @copyByte
@rmvQuote:
        xor cl, cl
        jmp @copyByte
@openBracket:
        mov [LastOpenBracket], edi
        mov ecx, [ParenthLevel]
        inc dword [ParenthLevel]
        and ecx, ecx
        pop ecx
        jne @copyByte
        cmp dword [exprStart], 0
        jne @copyByte
        mov [exprStart], edi
        bts dword [exprMode], 20 // set "expression has brackets" flag
        jmp @copyByte
@closeBracket:
        cmp byte [edi-1], ' '
        mov ecx, [LastWords]
        jne @noSpaceCloseBracket
        dec edi
@noSpaceCloseBracket:
        cmp ch, 192
        setnc ch
        and ecx, $FFFFFFFF
        cmp ecx, $F000F1
        jne @noRegMemAccess
        mov ecx, [exprStart]
        cmp ecx, [LastOpenBracket]
        jne @noRegMemAccess
        mov dword [exprStart], 0  // clear expression pointer
        bts dword [exprMode], 19 // set "register mem access" flag ie. "(HL)"
@noRegMemAccess:
        dec dword [ParenthLevel]
        pop ecx
        jge @copyByte
        btr dword [exprMode], 17
        mov dword [ParenthLevel], 0
        jnc @extraCloseBracket
// (IX+dd) or similar...
        push  edx
        push  ecx
        push  esi
        mov ecx, edi
        mov esi, [exprStart]
        sub ecx, esi
        mov edx, -34 // "invalid index register displacement"
        jle @invIXDisp
        call  @evalExpression
        jc  @gotIXDisp
@gotIXDisp:
        test  byte [IXSign], 01
        je  @noMinusIX
        neg eax
@noMinusIX:
        cmp eax, 128
        mov edx, -04 // "bad index register displacement"
        jge @invIXDisp
        cmp eax, -128
        jl  @invIXDisp
        and eax, $FF
        mov ah, $01
        mov edi, [exprStart]
        mov [IXDisp], eax
        pop esi
        mov dword [edi], $294E4E24
        pop ecx
        pop edx
        lea edi, [edi+4]
        xor eax, eax
        mov dword [exprStart], 0
        mov dword [exprMode], 0
        jmp @noSetQuote
@invIXDisp:
        pop esi
        pop ecx
        lea esp, [esp+4]
        jmp AsmError
@extraCloseBracket:
        mov edx, -39 // "unbalanced parenthesis"
        jmp AsmError
@numeric:
        pop ecx
        cmp dword [ParenthLevel], 0
        jne  @setBrckt
        bts dword [exprMode], 16
@setBrckt:
        cmp dword [exprStart], 0
        lea esi, [esi-1] // need to preserve Z
        jne @num2
        mov [exprStart], edi
@num2:  mov [edi], ch
        inc edi
        cmp esi, [insEnd]
        jnc @gotNum
        inc esi
        mov al, [esi]
        mov ch, [edx+eax*2]
        cmp byte [edx+1+eax*2], 1
        je  @num2
        cmp byte [edx+1+eax*2], $FF
        jnc @num2
@gotNum:
        push  eax
        mov al, $FE
        jmp @gotWordCode
@alpha:
        mov [edi], ch
        lea ebx, [edi]
        dec esi
        inc edi
@alpha2:
        cmp esi, [insEnd]
        jnc @gotWord
        inc esi
        mov al, [esi]
        mov cx, [edx+eax*2]
        cmp ch, $FF
        je  @alpha3
        cmp ch, 1
        je  @alpha3
        cmp al, 39
        jne @gotWord
@alpha3:
        mov [edi], cl
        inc edi
        jmp @alpha2
@gotWord:
        mov ch, cl
        push  eax
        xor cl, cl
        push  edi
        push  ecx
        push  edx
        push  esi
        mov ecx, edi
        sub ecx, ebx
        push  ebx
        push  ecx
        mov edx, ebx
        mov eax, [AsmWordsPtr]
        mov ecx, [NumAsmWords]
        call  IsReservedWord
        pop ebx
        pop esi
        pop edx
        pop ecx
        cmp eax, 0
        jl  @notReservedWord
        pop edi
        cmp al, 04  // BIT
        je  @bitExpr2
        cmp al, 74  // RES
        je  @bitExpr2
        cmp al, 93  // SET
        jne  @gotWordCode
@bitExpr2:
        bts dword [exprMode], 0
        mov [exprStart], edi
@gotWordCode:
        shl dword [LastWords], 8
        mov byte [LastWords], al
        pop eax
        cmp esi, [InsEnd]
        mov ebx, $2227
        jc  @getInstrSectionBody2
        jmp @NextInstrSection
@notReservedWord:
        cmp dword [ParenthLevel], 0
        jne  @setBrckt2
        bts dword [exprMode], 16
@setBrckt2:
        cmp dword [exprStart], 0
        jne @noSetExpr
        mov [exprStart], ebx
@noSetExpr:
        mov al, $FF
        pop edi
        jmp @gotWordCode
@hasIXDisp:
        cmp cl, [edx+1+eax*2]
        mov byte [edi], '+'
        push  edx
        push  esi
        setne [IXSign]
        inc edi
        mov esi, [exprStart]
        mov ecx, [LastOpenBracket]
        and esi, esi
        je  @IXnoPreExpr
        sub ecx, esi
        jle @IXnoPreExpr
        btr dword [exprMode], 0
        mov edx, -24 // "malformed line" ("(IX+dd)" inside brackets)
        jnc @badExprBeforeIX
@evalIXpreExpr:
        call  @evalExpression
        jnc @IXbitNumOK
        mov edx, -37 // "malformed line" (bad bit number expression before "(IX+dd)")
        jmp @badExprBeforeIX
@IXbitNumOK:
        cmp eax, 8
        mov dword [exprMode], 0
        jnc @bitNumError2 // bit number greater than 7
        or  eax, $49282030
        mov edi, [exprStart]
        mov edx, [LastWords]
        lea edi, [edi+6]
        sub dh, 20
        mov byte [edi-6], ' '
        mov [edi-5], eax
        mov [edi-1], dh
@IXnoPreExpr:
        pop esi
        pop edx
        pop ecx
        mov [exprStart], edi
        bts dword [exprMode], 17
        dec dword [ParenthLevel]
        mov eax, 0
        je @noSetQuote
        mov edx, -24 // "malformed line" ("(IX+dd)" inside brackets)
        jmp AsmError
@evalExpression:
        mov [exprLen], ecx
        push  ebx
        push  ecx
        push  esi
        push  edi
        mov ebx, [regEBX]
        mov esi, [regESI]
        mov edi, [regEDI]
        call doEval
        bt  edx, 30
        jnc @SymbolFound
        push  eax
        push  edx
        mov dword [ErrorNum], -07 // "undefined symbol"
        mov ebx, [regEBX]
        mov esi, [regESI]
        mov edi, [regEDI]
        call AEQuick
        pop edx
        pop eax
        inc dword [NumSymbolErrors]
@SymbolFound:
        bt  edx, 31
        pop edi
        pop esi
        pop ecx
        pop ebx
        ret
@badExprBeforeIX:
        pop esi
        lea esp, [esp+4]
        pop ecx
        jmp AsmError
@nextInstrSection:
        cmp byte [edi-1], 32
        jne @SectionTrailSpace
        dec edi
@sectionTrailSpace:
        cmp dword [exprStart], 0
        je  @nextInstrSection2
        bt  dword [exprMode], 0
        jc  @bitNum
        bt  dword [exprMode], 19
        jc  @invOperand
        push  esi
        push  eax
        push  edx
        mov esi, [exprStart]
        mov ecx, edi
        sub ecx, esi
        call  @evalExpression
        jc  @expressionOK
        mov byte [ValSet], 1
@expressionOK:
        bt  edx, 31
        jc  @invalidExprChar
@notInvalidExprChar:
        cmp [FirstWord], 43
        mov [InstrVal], eax
        je  @IMInstruction
        bt  dword [exprMode], 16
        mov edi, [exprStart]
        jc  @noBracket1
        mov byte [edi],'('
        inc edi
@noBracket1:
        mov dword [edi], 'NN$'
        bt  dword [exprMode], 16
        lea edi, [edi+3]
        jc  @noBracket2
        mov byte [edi],')'
        inc edi
@noBracket2:
        pop edx
        pop eax
        pop esi
@nextInstrSection2:
        cmp esi, [insEnd]
        jnc @endOfInstr
        mov byte [edi], ','
        mov dword [exprMode], 0
        inc edi
        jmp @startInstrSection
@IMInstruction:
        cmp eax, 3
        mov edi, [exprStart]
        jnc @InvalidIntMode
        add al, 48
        mov [edi], al
        inc edi
        jmp @noBracket2
@bitNum:
        cmp [FirstWord], 93
        push  esi
        push  eax
        push  edx
        jne @notSETDirective
        cmp esi, [InsEnd]
        jnc @doSETDirective
@notSETDirective:
        mov esi, [exprStart]
        mov ecx, edi
        sub ecx, esi
        call  @evalExpression
        jc  @bitNumOK
@bitNumOK:
        cmp eax, 8
        mov edi, [exprStart]
        jnc @bitNumError2
        or  al, $30
        mov [edi], al
        inc edi
        jmp @noBracket2
@doSETDirective:
        lea esp, [esp+12]
        pop edi
        pop esi
        pop ebx
        jmp doEQU_SET
@bitNumError2:
        pop edx
        pop eax
        pop esi
        jmp @bitNumError
@bitNumError1:
        pop esi
        pop edx
        pop ecx
@bitNumError:
        mov edx, -36 // "malformed operand" (bit number greater than 7)
        jmp AsmError
@invalidExprChar:
        bt  edx, 30
        jc  @notInvalidExprChar
        pop esi
        pop edx
        pop ecx
        mov edx, -32 // "bad expression"
        jmp AsmError
@invalidIntMode:
        pop esi
        pop edx
        pop ecx
        mov edx, -42 // "invalid interrupt mode"
        jmp AsmError
@invOperand:
        mov edx, -24 // "malformed line" (contains "(HL)" and an expression)
        jmp AsmError
@doEval:
        jmp @nextInstrSection
@checkInstrOneWord:
        xor edx, edx
        lea eax, Instruction+1
        mov ecx, [FWLen]
        mov dl, [eax-1]
        mov bl, [eax+ecx]
        mov [eax-1], cl
        add edx, eax
        mov [edx], bl
        mov byte [eax+ecx], 0
        push  edx
        call  IsInstruction
        xor ebx, ebx
        pop edx
        lea ecx, Instruction+1
        mov bl, [edx]
        mov byte [edx], 0
        sub edx, ecx
        mov dh, bl
        mov bl, [ecx-1]
        mov [ecx-1], dl
        mov [ecx+ebx], dh
        cmp eax, 0
        mov edx, -40 // "instruction takes no operands"
        jge AsmError
        mov edx, -41 // "invalid combination of opcode and operands"
        jmp AsmError
@insTypeJmpTable:
        dd  @NormalOpcodes, @CBOpcodes, @DDOpcodes, @DDCBOpcodes
        dd  @EDOpcodes, @FDOpcodes, @FDCBOpcodes
@NormalOpcodes:
        mov [edi], al
        inc edi
        cmp al, 199
        mov ebx, [InstrVal]
        jne @AddExtraBytes
        cmp ebx, 64
        mov edx, -06
        jnc AsmError
        mov bh, bl
        and bh, 7
        jne AsmError
        add al, bl
        mov [edi-1], al
        jmp @CopyInstruction
@AddExtraBytes:
        lea esi, [Opcodes]
        mov esi, [esi+eax*4]
        mov ah, [esi]
        cmp ah, '='
        je  @addNNNN
        cmp ah, '@'
        je  @addNNNN
        cmp ah, '%'
        je  @addNNNN
        cmp ah, '&'
        je  @addNNNN
        cmp ah, '!'
        je  @addNN
        cmp ah, '<'
        je  @addNN
        cmp ah, '>'
        je  @addNN
        cmp ah, '?'
        je  @addDD
        cmp ah, '/'
        je  @addDD
        cmp ah, '#'
        jne @CopyInstruction
@addRelative:
        mov ecx, [self]
        mov ecx, [ecx].TZ80Assembler.AsmAddr
        add ecx, 2
        and ecx, $FFFF
        cmp byte [ValSet], 0
        je  @noRelSymbol
        mov ecx, [InstrVal]
@noRelSymbol:
        mov ebx, [self]
        mov ebx, [ebx].TZ80Assembler.AsmAddr
        add bx, 2
        sub cx, bx
        cmp cx, 127
        jg  @RelError
        cmp cx, -128
        jl  @RelError
        mov byte [edi], cl
        inc edi
        jmp @CopyInstruction
@RelError:
        mov edx, -02 // "jump out of range"
        jmp AsmError // fix this later - currently jumps to next line, should continue processing
@addDD: mov ch, byte [IXDisp]
        inc edi
        cmp ah, '?'
        mov [edi-1], ch
        je  @CopyInstruction
@addNN: mov ecx, [InstrVal]
        inc edi
        cmp ecx, -128
        jnc @NNok
        cmp ecx, 256
        jnc @errorNotByte
@NNok:  mov byte [edi-1], cl
        jmp @CopyInstruction
@errorNotByte:
        mov edx, -43 // "byte value out of range"
        jmp AsmError
@addNNNN:
        mov ecx, [InstrVal]
        lea edi, [edi+2]
        cmp ecx, -32768
        jnc @NNNNok
        cmp ecx, 65536
        jnc @errorNotWord
@NNNNok:
        mov word [edi-2], cx
        jmp @CopyInstruction
@errorNotWord:
        mov edx, -44 // "byte value out of range"
        jmp AsmError
@CBOpcodes:
        mov byte [edi], $CB
        mov [edi+1], al
        lea edi, [edi+2]
        jmp @CopyInstruction
@DDOpcodes:
        mov byte [edi], $DD
        jmp @DDFDOpcodes
@FDOpcodes:
        mov byte [edi], $FD
@DDFDOpcodes:
        mov [edi+1], al
        lea edi, [edi+2]
        jmp @AddExtraBytes
@DDCBOpcodes:
        mov ecx, $DDCB0000
        jmp @DDFDCBOpcodes
@FDCBOpcodes:
        mov ecx, $FDCB0000
@DDFDCBOpcodes:
        mov ch, byte [IXDisp]
        mov cl, al
        bswap  ecx
        mov [edi], ecx
        lea edi, [edi+4]
        jmp @CopyInstruction
@EDOpcodes:
        mov byte [edi], $ED
        mov [edi+1], al
        lea edi, [edi+2]
        jmp @AddExtraBytes
@endOfInstr:
        lea eax, Instruction+1
        mov ecx, edi
        sub ecx, eax
        mov byte [edi], 0
        mov [eax-1], cl
        call  IsInstruction
        cmp eax, -1
        mov ebx, eax
        je  @checkInstrOneWord
        lea edi, TempStr+1
        shr ebx, 8
        mov ecx, dword [@insTypeJmpTable+ebx*4]
        mov esi, edi
        jmp  ecx
@CopyInstruction:
        lea esi, TempStr+1
        mov ebx, edi
        sub ebx, esi
        mov [esi-1], bl
        pop edi
        pop esi
        pop ebx
        end;
      end;
    end;
    if (SymLen > 0) and (FirstWord = 93) then begin
      InstrVal := AsmAddr;
      FirstWord := 62; // code for NOP
      asm
      call  AddSymbol
      end;
    end;
    if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then asm
      mov [errorNum], -46 // "instruction violates page bounds"
      call  GetCurrentPage
    end;
    if PageValid then begin
      p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
      i2 := Length(TempStr);
      if (AsmAddr+i2) <= CurPage.Z80End then begin
        if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
        for i := 1 to Length(TempStr) do
          PByteArray(p2)[i-1] := ord(TempStr[i]);
        inc(totalBytes, i2);
        inc(AsmAddr, i2);
        if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
      end else begin
        i4 := 1;
        while i2 > 0 do begin
          if (AsmAddr < CurPage.Z80Start) or (AsmAddr >= CurPage.Z80End) then asm
            mov [errorNum], -46 // "instruction violates page bounds"
            call  GetCurrentPage
          end;
          if not PageValid then goto NextLine;
          if CurPage.AltLo > AsmAddr then CurPage.AltLo := AsmAddr;
          i3 := CurPage.Z80End - AsmAddr;
          if i3 > i2 then i3 := i2;
          p2 := pointer(Integer(CurPage.StartAddress)+(AsmAddr-CurPage.Z80Start));
          for i5 := 0 to i3-1 do PByteArray(p2)[i5] := ord(TempStr[i4+i5]);
          inc(i4, i3);
          inc(AsmAddr, i3);
          inc(totalBytes, i3);
          dec(i2, i3);
          if CurPage.AltHi < AsmAddr then CurPage.AltHi := AsmAddr;
        end;
      end;
    end;
noInstr:
    end;
nextLine:
    if Assigned(afterLine) then begin
      TempLineLen := AsmAddr-AsmLineAddr;
      afterLine(self, AsmLineAddr, TempLineLen, oldPage,
      SourceFilenames.Count-1, Line);
    end;
    if LineRepCount > 0 then begin
      dec(LineRepCount);
      goto ProcessInstruction;
    end else inc(Line);
  end;
nextFile:
  if (IfLevel > 0) or InIf then AddErrorQuick(-19, Line, FileNum)
  else if (repLevel > repBaseLevel) then AddErrorQuick(-63, Line, FileNum)
  else if SourceFilenames.Count > 1 then begin
    repBaseLevel := repBase[SourceFilenames.Count-1];
    Line := Integer(SourceFilenames.Objects[SourceFilenames.Count-1])+1;
    SourceFilenames.Delete(SourceFilenames.Count-1);
    i := SourceFiles.IndexOf(SourceFilenames[SourceFilenames.Count-1]);
    Source := TStringList(SourceFiles.Objects[i]);
    goto AssembleSourceFile
  end;
done:
  for i := 0 to 31 do AsmSymbols[i].Sorted := True;
  result := ((NumSymbolErrors > 0) and (NumSymbolErrors <> LastSymErrors))
    and (NumSymbolErrors = NumErrors);
  LastNumErrors := NumErrors;
  LastSymErrors := NumSymbolErrors;
  if not result and (NumErrors > NumSymbolErrors) then begin
    i := 0;
    i2 := 0;
    while i2 < NumErrors shl 2 do begin
      p := PAsmError(@QErrors[i]);
      p2 := PAsmError(@QErrors[i2]);
      if PAsmError(p2).ErrorNum = -07 then begin
      end else begin
        if i <> i2 then begin
          PAsmError(p).ErrorNum := PAsmError(p2).ErrorNum;
          PAsmError(p).ErrorLine := PAsmError(p2).ErrorLine;
          PAsmError(p).SourceFileNum := PAsmError(p2).SourceFileNum;
          PAsmError(p).null := PAsmError(p2).null;
        end;
        inc(i,4);
      end;
      inc(i2,4);
    end;
    dec(NumErrors, NumSymbolErrors);
  end;
  SetLength(QErrors, NumErrors shl 2);
  if Assigned(atEndOfPass) then atEndOfPass(self);
end;

end.

