unit CPUDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, FastIMG, FastDIB, FastDraw, ExtCtrls, StdCtrls, FastCore, Menus, ROMUtils;

type

//  TSymbol = Record Desc: String; Address: DWord; End;

  TJumpOpcode = Record IsJump: Boolean; Address, StartChar, Len: Integer; End;

  TCPUWindow = class(TForm)
    Panel1: TPanel;
    FastIMG1: TFastIMG;
    Edit1: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    Edit7: TEdit;
    Label8: TLabel;
    Edit8: TEdit;
    Label9: TLabel;
    Edit9: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Edit10: TEdit;
    Label12: TLabel;
    Edit11: TEdit;
    Label13: TLabel;
    Edit12: TEdit;
    Label14: TLabel;
    Edit13: TEdit;
    Label15: TLabel;
    Edit14: TEdit;
    Label16: TLabel;
    Edit15: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    PopupMenu1: TPopupMenu;
    HexValues1: TMenuItem;
    DecimalValues1: TMenuItem;
    N1: TMenuItem;
    ASCIIBytes1: TMenuItem;
    Edit16: TEdit;
    ScrollBar1: TScrollBar;
    Button7: TButton;
    Button8: TButton;
    Label27: TLabel;
    Edit17: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FastIMG1DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure HexValues1Click(Sender: TObject);
    procedure DecimalValues1Click(Sender: TObject);
    procedure ASCIIBytes1Click(Sender: TObject);
    procedure CheckBox2KeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Label3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Button7Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button8Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    LabelIn: TLabel;
    CurViewAddr: Integer;
    CPUViewPos: Word;
    HighlightAddr: Integer;
    HighlightAt: Integer;
    MaxHighlight: Integer;
    Jumps: Array of TJumpOpcode;
    Addresses: Array of Integer;
    JumpAddr: Integer;
    OldRegs: TZ80Registers;
    FirstRun: Boolean;
    CPURunning: Boolean;
    Init: Boolean;
    PrevAddr: Word;
    AddressHistory: TStringlist;
    Procedure BuildROMSymbols;
    Procedure AlterOpcodes;
    Procedure AddAssemblerSymbols;
    Procedure ClearLabels(MinAddr, MaxAddr: Integer);
    Function  GetDisassembly(Var nPC: Word; Var Jump: TJumpOpcode): ShortString;
    Procedure CreateDisassembly;
    Procedure UpdateRegs;
    Procedure UpdateRegsValues;
    Function  GetValue(Var EditBox: TEdit; MaxLimit: Integer; Desc: String): Integer;
    Procedure SetDebugColourEdt(Var Control: TEdit; Debug: Boolean);
    Procedure SetDebugColourCb(Var Control: TLabel; Debug: Boolean);
  end;

var
  CPUWindow: TCPUWindow;
  CPUBreakPoints: Array[0..65535] of Byte;
  StepOperation: Boolean;
  ExitProcOperation: Boolean;
  CallCounter: DWord;
  Symbols: Array[0..65535] of TSymbol;
  LastOp: Byte;
  CPUShowing: Boolean;

  HexChars: Array[0..15] of Char =
     ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

  HexBytes: Array[0..255] of String =
     ('00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '0A', '0B', '0C', '0D', '0E', '0F',
      '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '1A', '1B', '1C', '1D', '1E', '1F',
      '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '2A', '2B', '2C', '2D', '2E', '2F',
      '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '3A', '3B', '3C', '3D', '3E', '3F',
      '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '4A', '4B', '4C', '4D', '4E', '4F',
      '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '5A', '5B', '5C', '5D', '5E', '5F',
      '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', '6A', '6B', '6C', '6D', '6E', '6F',
      '70', '71', '72', '73', '74', '75', '76', '77', '78', '79', '7A', '7B', '7C', '7D', '7E', '7F',
      '80', '81', '82', '83', '84', '85', '86', '87', '88', '89', '8A', '8B', '8C', '8D', '8E', '8F',
      '90', '91', '92', '93', '94', '95', '96', '97', '98', '99', '9A', '9B', '9C', '9D', '9E', '9F',
      'A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'AA', 'AB', 'AC', 'AD', 'AE', 'AF',
      'B0', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'BA', 'BB', 'BC', 'BD', 'BE', 'BF',
      'C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'CA', 'CB', 'CC', 'CD', 'CE', 'CF',
      'D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'DA', 'DB', 'DC', 'DD', 'DE', 'DF',
      'E0', 'E1', 'E2', 'E3', 'E4', 'E5', 'E6', 'E7', 'E8', 'E9', 'EA', 'EB', 'EC', 'ED', 'EE', 'EF',
      'F0', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'FA', 'FB', 'FC', 'FD', 'FE', 'FF');

  OpsNoPrefix: Array[0..255] of String =
     ('NOP', '@LD BC, $NNNN', 'LD (BC), A', 'INC BC', 'INC B', 'DEC B', '!LD B, $NN', 'RLCA', 'EX AF, AF'+Chr(39), 'ADD HL, BC', 'LD A, (BC)',
      'DEC BC', 'INC C', 'DEC C', '!LD C, $NN', 'RRCA', '#DJNZ $$$+e', '@LD DE, $NNNN', 'LD (DE), A', 'INC DE', 'INC D', 'DEC D', '!LD D, $NN', 'RLA',
      '#JR $$$+2', 'ADD HL, DE', 'LD A, (DE)', 'DEC DE', 'INC E', 'DEC E', '!LD E, $NN', 'RRA', '#JR NZ, $$$+2', '@LD HL, $NNNN', '@LD ($NNNN), HL',
      'INC HL', 'INC H', 'DEC H', '!LD H, $NN', 'DAA', '#JR Z, $$$+2', 'ADD HL, HL', '@LD HL, ($NNNN)', 'DEC HL', 'INC L', 'DEC L', '!LD L, $NN', 'CPL',
      '#JR NC, $$$+2', '@LD SP, $NNNN', '&LD ($NNNN), A', 'INC SP', 'INC (HL)', 'DEC (HL)', '!LD (HL), $NN', 'SCF', '#JR C, $$$+2', 'ADD HL, SP', '&LD A, ($NNNN)',
      'DEC SP', 'INC A', 'DEC A', '!LD A, $NN', 'CCF', 'LD B, B', 'LD B, C', 'LD B, D', 'LD B, E', 'LD B, H', 'LD B, L', 'LD B, (HL)', 'LD B, A',
      'LD C, B', 'LD C, C', 'LD C, D', 'LD C, E', 'LD C, H', 'LD C, L', 'LD C, (HL)', 'LD C, A', 'LD D, B', 'LD D, C', 'LD D, D', 'LD D, E',
      'LD D, H', 'LD D, L', 'LD D, (HL)', 'LD D, A', 'LD E, B', 'LD E, C', 'LD E, D', 'LD E, E', 'LD E, H', 'LD E, L', 'LD E, (HL)',
      'LD E, A', 'LD H, B', 'LD H, C', 'LD H, D', 'LD H, E', 'LD H, H', 'LD H, L', 'LD H, (HL)', 'LD H, A', 'LD L, B', 'LD L, C', 'LD L, D',
      'LD L, E', 'LD L, H', 'LD L, L', 'LD L, (HL)', 'LD L, A', 'LD (HL), B', 'LD (HL), C', 'LD (HL), D', 'LD (HL), E', 'LD (HL), H',
      'LD (HL), L', 'HALT', 'LD (HL), A', 'LD A, B', 'LD A, C', 'LD A, D', 'LD A, E', 'LD A, H', 'LD A, L', 'LD A, (HL)', 'LD A, A',
      'ADD A, B', 'ADD A, C', 'ADD A, D', 'ADD A, E', 'ADD A, H', 'ADD A, L', 'ADD A, (HL)', 'ADD A, A', 'ADC A, B', 'ADC A, C', 'ADC A, D',
      'ADC A, E', 'ADC A, H', 'ADC A, L', 'ADC A, (HL)', 'ADC A, A', 'SUB B', 'SUB C', 'SUB D', 'SUB E', 'SUB H', 'SUB L', 'SUB (HL)',
      'SUB A', 'SBC A, B', 'SBC A, C', 'SBC A, D', 'SBC A, E', 'SBC A, H', 'SBC A, L', 'SBC A, (HL)', 'SBC A, A', 'AND B', 'AND C', 'AND D', 'AND E', 'AND H',
      'AND L', 'AND (HL)', 'AND A', 'XOR B', 'XOR C', 'XOR D', 'XOR E', 'XOR H', 'XOR L', 'XOR (HL)', 'XOR A', 'OR B', 'OR C', 'OR D',
      'OR E', 'OR H', 'OR L', 'OR (HL)', 'OR A', 'CP B', 'CP C', 'CP D', 'CP E', 'CP H', 'CP L', 'CP (HL)', 'CP A', 'RET NZ', 'POP BC',
      '=JP NZ, $$$+3', '=JP $$$+3', '@CALL NZ, $NNNN', 'PUSH BC', '!ADD A, $NN', '.RST $NN', 'RET Z', 'RET', '=JP Z, $$$+3', '', '@CALL Z, $NNNN', '@CALL $NNNN',
      '!ADC A, $NN', '.RST $NN', 'RET NC', 'POP DE', '=JP NC, $$$+3', '>OUT ($NN), A', '@CALL NC, $NNNN', 'PUSH DE', '<SUB $NN', '.RST $NN', 'RET C', 'EXX',
      '=JP C, $$$+3', '!IN A, ($NN)', '@CALL C, $NNNN', '', '!SBC A, $NN', '.RST $NN', 'RET PO', 'POP HL', '=JP PO, $$$+3', 'EX (SP), HL', '@CALL PO, $NNNN',
      'PUSH HL', '<AND $NN', '.RST $NN', 'RET PE', 'JP (HL)', '=JP PE, $$$+3', 'EX DE, HL', '@CALL PE, $NNNN', '', '<XOR $NN', '.RST $NN', 'RET P',
      'POP AF', '=JP P, $$$+3', 'DI', '@CALL P, $NNNN', 'PUSH AF', '<OR $NN', '.RST $NN', 'RET M', 'LD SP, HL', '=JP M, $$$+3', 'EI', '@CALL M, $NNNN',
      '', '<CP $NN', '.RST $NN');

  OpsCBPrefix: Array[0..255] of String =
     ('RLC B', 'RLC C', 'RLC D', 'RLC E', 'RLC H', 'RLC L', 'RLC (HL)', 'RLC A', 'RRC B', 'RRC C', 'RRC D', 'RRC E', 'RRC H', 'RRC L', 'RRC (HL)',
      'RRC A', 'RL B', 'RL C', 'RL D', 'RL E', 'RL H', 'RL L', 'RL (HL)', 'RL A', 'RR B', 'RR C', 'RR D', 'RR E', 'RR H', 'RR L',
      'RR (HL)', 'RR A', 'SLA B', 'SLA C', 'SLA D', 'SLA E', 'SLA H', 'SLA L', 'SLA (HL)', 'SLA A', 'SRA B', 'SRA C', 'SRA D', 'SRA E', 'SRA H',
      'SRA L', 'SRA (HL)', 'SRA A', 'SLL B', 'SLL C', 'SLL D', 'SLL E', 'SLL H', 'SLL L', 'SLL (HL)', 'SLL A', 'SRL B', 'SRL C', 'SRL D', 'SRL E',
      'SRL H', 'SRL L', 'SRL (HL)', 'SRL A', 'BIT 0, B', 'BIT 0, C', 'BIT 0, D', 'BIT 0, E', 'BIT 0, H', 'BIT 0, L', 'BIT 0, (HL)', 'BIT 0, A', 'BIT 1, B',
      'BIT 1, C', 'BIT 1, D', 'BIT 1, E', 'BIT 1, H', 'BIT 1, L', 'BIT 1, (HL)', 'BIT 1, A', 'BIT 2, B', 'BIT 2, C', 'BIT 2, D', 'BIT 2, E', 'BIT 2, H',
      'BIT 2, L', 'BIT 2, (HL)', 'BIT 2, A', 'BIT 3, B', 'BIT 3, C', 'BIT 3, D', 'BIT 3, E', 'BIT 3, H', 'BIT 3, L', 'BIT 3, (HL)', 'BIT 3, A', 'BIT 4, B',
      'BIT 4, C', 'BIT 4, D', 'BIT 4, E', 'BIT 4, H', 'BIT 4, L', 'BIT 4, (HL)', 'BIT 4, A', 'BIT 5, B', 'BIT 5, C', 'BIT 5, D', 'BIT 5, E', 'BIT 5, H',
      'BIT 5, L', 'BIT 5, (HL)', 'BIT 5, A', 'BIT 6, B', 'BIT 6, C', 'BIT 6, D', 'BIT 6, E', 'BIT 6, H', 'BIT 6, L', 'BIT 6, (HL)', 'BIT 6, A', 'BIT 7, B',
      'BIT 7, C', 'BIT 7, D', 'BIT 7, E', 'BIT 7, H', 'BIT 7, L', 'BIT 7, (HL)', 'BIT 7, A', 'RES 0, B', 'RES 0, C', 'RES 0, D', 'RES 0, E', 'RES 0, H',
      'RES 0, L', 'RES 0, (HL)', 'RES 0, A', 'RES 1, B', 'RES 1, C', 'RES 1, D', 'RES 1, E', 'RES 1, H', 'RES 1, L', 'RES 1, (HL)', 'RES 1, A', 'RES 2, B',
      'RES 2, C', 'RES 2, D', 'RES 2, E', 'RES 2, H', 'RES 2, L', 'RES 2, (HL)', 'RES 2, A', 'RES 3, B', 'RES 3, C', 'RES 3, D', 'RES 3, E', 'RES 3, H',
      'RES 3, L', 'RES 3, (HL)', 'RES 3, A', 'RES 4, B', 'RES 4, C', 'RES 4, D', 'RES 4, E', 'RES 4, H', 'RES 4, L', 'RES 4, (HL)', 'RES 4, A', 'RES 5, B',
      'RES 5, C', 'RES 5, D', 'RES 5, E', 'RES 5, H', 'RES 5, L', 'RES 5, (HL)', 'RES 5, A', 'RES 6, B', 'RES 6, C', 'RES 6, D', 'RES 6, E', 'RES 6, H',
      'RES 6, L', 'RES 6, (HL)', 'RES 6, A', 'RES 7, B', 'RES 7, C', 'RES 7, D', 'RES 7, E', 'RES 7, H', 'RES 7, L', 'RES 7, (HL)', 'RES 7, A', 'SET 0, B',
      'SET 0, C', 'SET 0, D', 'SET 0, E', 'SET 0, H', 'SET 0, L', 'SET 0, (HL)', 'SET 0, A', 'SET 1, B', 'SET 1, C', 'SET 1, D', 'SET 1, E', 'SET 1, H',
      'SET 1, L', 'SET 1, (HL)', 'SET 1, A', 'SET 2, B', 'SET 2, C', 'SET 2, D', 'SET 2, E', 'SET 2, H', 'SET 2, L', 'SET 2, (HL)', 'SET 2, A', 'SET 3, B',
      'SET 3, C', 'SET 3, D', 'SET 3, E', 'SET 3, H', 'SET 3, L', 'SET 3, (HL)', 'SET 3, A', 'SET 4, B', 'SET 4, C', 'SET 4, D', 'SET 4, E', 'SET 4, H',
      'SET 4, L', 'SET 4, (HL)', 'SET 4, A', 'SET 5, B', 'SET 5, C', 'SET 5, D', 'SET 5, E', 'SET 5, H', 'SET 5, L', 'SET 5, (HL)', 'SET 5, A', 'SET 6, B',
      'SET 6, C', 'SET 6, D', 'SET 6, E', 'SET 6, H', 'SET 6, L', 'SET 6, (HL)', 'SET 6, A', 'SET 7, B', 'SET 7, C', 'SET 7, D', 'SET 7, E', 'SET 7, H',
      'SET 7, L', 'SET 7, (HL)', 'SET 7, A');

  OpsDDPrefix: Array[0..255] of String =
     ('NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'ADD IX, BC', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'ADD IX, DE', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', '@LD IX, $NNNN', '@LD ($NNNN), IX',
      'INC IX', 'INC IXH', 'DEC IXH', '!LD IXH, $NN', 'NOP', 'NOP', 'ADD IX, IX', '@LD IX, ($NNNN)', 'DEC IX', 'INC IXL', 'DEC IXL', '!LD IXL, $NN',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', '?INC (IX+$DD)', '?DEC (IX+$DD)', '/LD (IX+$DD), $NN', 'NOP', 'NOP', 'ADD IX, SP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD B, IXH', 'LD B, IXL', '?LD B, (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD C, IXH',
      'LD C, IXL', '?LD C, (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD D, IXH', 'LD D, IXL', '?LD D, (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'LD E, IXH', 'LD E, IXL', '?LD E, (IX+$DD)', 'NOP', 'LD IXH, B', 'LD IXH, C', 'LD IXH, D', 'LD IXH, E', 'LD IXH, IXH', 'LD IXH, IXL',
      '?LD H, (IX+$DD)', 'LD IXH, A', 'LD IXL, B', 'LD IXL, C', 'LD IXL, D', 'LD IXL, E', 'LD IXL, IXH', 'LD IXL, IXL', '?LD L, (IX+$DD)', 'LD IXL, A',
      '?LD (IX+$DD), B', '?LD (IX+$DD), C', '?LD (IX+$DD), D', '?LD (IX+$DD), E', '?LD (IX+$DD), H', '?LD (IX+$DD), L', 'NOP', '?LD (IX+$DD), A', 'NOP', 'NOP', 'NOP',
      'NOP', 'LD A, IXH', 'LD A, IXL', '?LD A, (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'ADD A, IXH', 'ADD A, IXL', '?ADD A, (IX+$DD)', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'ADC A, IXH', 'ADC A, IXL', '?ADC A, (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'SUB IXH', 'SUB IXL',
      '?SUB (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'SBC A, IXH', 'SBC A, IXL', '?SBC A, (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'AND IXH',
      'AND IXL', '?AND (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'XOR IXH', 'XOR IXL', '?XOR (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'OR IXH', 'OR IXL', '?OR (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'CP IXH', 'CP IXL', '?CP (IX+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', '', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'POP IX', 'NOP', 'EX (SP), IX', 'NOP', 'PUSH IX', 'NOP', 'NOP', 'NOP',
      'JP (IX)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD SP, IX', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP');

  OpsDDCBPrefix: Array[0..255] of String =
     ('?LD B, RLC (IX+$DD)', '?LD C, RLC (IX+$DD)', '?LD D, RLC (IX+$DD)', '?LD E, RLC (IX+$DD)', '?LD H, RLC (IX+$DD)', '?LD L, RLC (IX+$DD)', '?RLC (IX+$DD)',
      '?LD A, RLC (IX+$DD)', '?LD B, RRC (IX+$DD)', '?LD C, RRC (IX+$DD)', '?LD D, RRC (IX+$DD)', '?LD E, RRC (IX+$DD)', '?LD H, RRC (IX+$DD)', '?LD L, RRC (IX+$DD)',
      '?RRC (IX+$DD)', '?LD A, RRC (IX+$DD)', '?LD B, RL (IX+$DD)', '?LD C, RL (IX+$DD)', '?LD D, RL (IX+$DD)', '?LD E, RL (IX+$DD)', '?LD H, RL (IX+$DD)',
      '?LD L, RL (IX+$DD)', '?RL (IX+$DD)', '?LD A, RL (IX+$DD)', '?LD B, RR (IX+$DD)', '?LD C, RR (IX+$DD)', '?LD D, RR (IX+$DD)', '?LD E, RR (IX+$DD)',
      '?LD H, RR (IX+$DD)', '?LD L, RR (IX+$DD)', '?RR (IX+$DD)', '?LD A, RR (IX+$DD)', '?LD B, SLA (IX+$DD)', '?LD C, SLA (IX+$DD)', '?LD D, SLA (IX+$DD)',
      '?LD E, SLA (IX+$DD)', '?LD H, SLA (IX+$DD)', '?LD L, SLA (IX+$DD)', '?SLA (IX+$DD)', '?LD A, SLA (IX+$DD)', '?LD B, SRA (IX+$DD)', '?LD C, SRA (IX+$DD)',
      '?LD D, SRA (IX+$DD)', '?LD E, SRA (IX+$DD)', '?LD H, SRA (IX+$DD)', '?LD L, SRA (IX+$DD)', '?SRA (IX+$DD)', '?LD A, SRA (IX+$DD)', '?LD B, SLL (IX+$DD)',
      '?LD C, SLL (IX+$DD)', '?LD D, SLL (IX+$DD)', '?LD E, SLL (IX+$DD)', '?LD H, SLL (IX+$DD)', '?LD L, SLL (IX+$DD)', '?SLL (IX+$DD)', '?LD A, SLL (IX+$DD)',
      '?LD B, SRL (IX+$DD)', '?LD C, SRL (IX+$DD)', '?LD D, SRL (IX+$DD)', '?LD E, SRL (IX+$DD)', '?LD H, SRL (IX+$DD)', '?LD L, SRL (IX+$DD)', '?SRL (IX+$DD)',
      '?LD A, SRL (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)', '?BIT 0, (IX+$DD)',
      '?BIT 0, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)', '?BIT 1, (IX+$DD)',
      '?BIT 1, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)', '?BIT 2, (IX+$DD)',
      '?BIT 2, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)', '?BIT 3, (IX+$DD)',
      '?BIT 3, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)', '?BIT 4, (IX+$DD)',
      '?BIT 4, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)', '?BIT 5, (IX+$DD)',
      '?BIT 5, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)', '?BIT 6, (IX+$DD)',
      '?BIT 6, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)', '?BIT 7, (IX+$DD)',
      '?BIT 7, (IX+$DD)', '?LD B, RES 0 (IX+$DD)', '?LD C, RES 0 (IX+$DD)', '?LD D, RES 0 (IX+$DD)', '?LD E, RES 0 (IX+$DD)', '?LD H, RES 0 (IX+$DD)',
      '?LD L, RES 0 (IX+$DD)', '?RES 0, (IX+$DD)', '?LD A, RES 0 (IX+$DD)', '?LD B, RES 1 (IX+$DD)', '?LD C, RES 1 (IX+$DD)', '?LD D, RES 1 (IX+$DD)',
      '?LD E, RES 1 (IX+$DD)', '?LD H, RES 1 (IX+$DD)', '?LD L, RES 1 (IX+$DD)', '?RES 1, (IX+$DD)', '?LD A, RES 1 (IX+$DD)', '?LD B, RES 2 (IX+$DD)',
      '?LD C, RES 2 (IX+$DD)', '?LD D, RES 2 (IX+$DD)', '?LD E, RES 2 (IX+$DD)', '?LD H, RES 2 (IX+$DD)', '?LD L, RES 2 (IX+$DD)', '?RES 2, (IX+$DD)',
      '?LD A, RES 2 (IX+$DD)', '?LD B, RES 3 (IX+$DD)', '?LD C, RES 3 (IX+$DD)', '?LD D, RES 3 (IX+$DD)', '?LD E, RES 3 (IX+$DD)', '?LD H, RES 3, (IX+$DD)',
      '?LD L, RES 3 (IX+$DD)', '?RES 3, (IX+$DD)', '?LD A, RES 3 (IX+$DD)', '?LD B, RES 4 (IX+$DD)', '?LD C, RES 4 (IX+$DD)', '?LD D, RES 4 (IX+$DD)',
      '?LD E, RES 4 (IX+$DD)', '?LD H, RES 4 (IX+$DD)', '?LD L, RES 4 (IX+$DD)', '?RES 4, (IX+$DD)', '?LD A, RES 4 (IX+$DD)', '?LD B, RES 5 (IX+$DD)',
      '?LD C, RES 5 (IX+$DD)', '?LD D, RES 5 (IX+$DD)', '?LD E, RES 5 (IX+$DD)', '?LD H, RES 5 (IX+$DD)', '?LD L, RES 5 (IX+$DD)', '?RES 5, (IX+$DD)',
      '?LD A, RES 5 (IX+$DD)', '?LD B, RES 6 (IX+$DD)', '?LD C, RES 6 (IX+$DD)', '?LD D, RES 6 (IX+$DD)', '?LD E, RES 6 (IX+$DD)', '?LD H, RES 6 (IX+$DD)',
      '?LD L, RES 6 (IX+$DD)', '?RES 6, (IX+$DD)', '?LD A, RES 6 (IX+$DD)', '?LD B, RES 7, (IX+$DD)', '?LD C, RES 7 (IX+$DD)', '?LD D, RES 7 (IX+$DD)',
      '?LD E, RES 7 (IX+$DD)', '?LD H, RES 7 (IX+$DD)', '?LD L, RES 7 (IX+$DD)', '?RES 7, (IX+$DD)', '?LD A, RES 7 (IX+$DD)', '?LD B, SET 0 (IX+$DD)',
      '?LD C, SET 0 (IX+$DD)', '?LD D, SET 0 (IX+$DD)', '?LD E, SET 0 (IX+$DD)', '?LD H, SET 0 (IX+$DD)', '?LD L, SET 0 (IX+$DD)', '?SET 0, (IX+$DD)',
      '?LD A, SET 0 (IX+$DD)', '?LD B, SET 1 (IX+$DD)', '?LD C, SET 1 (IX+$DD)', '?LD D, SET 1 (IX+$DD)', '?LD E, SET 1 (IX+$DD)', '?LD H, SET 1 (IX+$DD)',
      '?LD L, SET 1 (IX+$DD)', '?SET 1, (IX+$DD)', '?LD A, SET 1 (IX+$DD)', '?LD B, SET 2, (IX+$DD)', '?LD C, SET 2 (IX+$DD)', '?LD D, SET 2 (IX+$DD)',
      '?LD E, SET 2 (IX+$DD)', '?LD H, SET 2 (IX+$DD)', '?LD L, SET 2 (IX+$DD)', '?SET 2, (IX+$DD)', '?LD A, SET 2 (IX+$DD)', '?LD B, SET 3 (IX+$DD)',
      '?LD C, SET 3 (IX+$DD)', '?LD D, SET 3 (IX+$DD)', '?LD E, SET 3 (IX+$DD)', '?LD H, SET 3 (IX+$DD)', '?LD L, SET 3 (IX+$DD)', '?SET 3, (IX+$DD)',
      '?LD A, SET 3 (IX+$DD)', '?LD B, SET 4 (IX+$DD)', '?LD C, SET 4 (IX+$DD)', '?LD D, SET 4 (IX+$DD)', '?LD E, SET 4 (IX+$DD)', '?LD H, SET 4 (IX+$DD)',
      '?LD L, SET 4 (IX+$DD)', '?SET 4, (IX+$DD)', '?LD A, SET 4 (IX+$DD)', '?LD B, SET 5, (IX+$DD)', '?LD C, SET 5 (IX+$DD)', '?LD D, SET 5 (IX+$DD)',
      '?LD E, SET 5 (IX+$DD)', '?LD H, SET 5 (IX+$DD)', '?LD L, SET 5 (IX+$DD)', '?SET 5, (IX+$DD)', '?LD A, SET 5 (IX+$DD)', '?LD B, SET 6 (IX+$DD)',
      '?LD C, SET 6 (IX+$DD)', '?LD D, SET 6 (IX+$DD)', '?LD E, SET 6 (IX+$DD)', '?LD H, SET 6 (IX+$DD)', '?LD L, SET 6 (IX+$DD)', '?SET 6, (IX+$DD)',
      '?LD A, SET 6 (IX+$DD)', '?LD B, SET 7 (IX+$DD)', '?LD C, SET 7 (IX+$DD)', '?LD D, SET 7 (IX+$DD)', '?LD E, SET 7 (IX+$DD)', '?LD H, SET 7 (IX+$DD)',
      '?LD L, SET 7 (IX+$DD)', '?SET 7, (IX+$DD)', '?LD A, SET 7 (IX+$DD)');

  OpsEDPrefix: Array[0..255] of String =
     ('NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'IN B, (C)', 'OUT (C), B', 'SBC HL, BC', '@LD ($NNNN), BC', 'NEG', 'RETN', 'IM 0', 'LD I, A', 'IN C, (C)',
      'OUT (C), C', 'ADC HL, BC', '@LD BC, ($NNNN)', 'NEG', 'RETI', 'IM 0', 'LD R, A', 'IN D, (C)', 'OUT (C), D', 'SBC HL, DE', '@LD ($NNNN), DE',
      'NEG', 'RETN', 'IM 1', 'LD A, I', 'IN E, (C)', 'OUT (C), E', 'ADC HL, DE', '@LD DE, ($NNNN)', 'NEG', 'RETN', 'IM 2', 'LD A, R', 'IN H, (C)',
      'OUT (C), B', 'SBC HL, HL', '@LD ($NNNN), HL', 'NEG', 'RETN', 'IM 0', 'RRD', 'IN L, (C)', 'OUT (C), L', 'ADC HL, HL', '@LD HL, ($NNNN)', 'NEG',
      'RETN', 'IM 0/1', 'RLD', 'IN F, (C)', 'OUT (C), 0', 'SBC HL, SP', '@LD ($NNNN), SP', 'NEG', 'RETN', 'IM 1', 'NOP', 'IN A, (C)', 'OUT (C), A',
      'ADC HL, SP', '@LD SP, ($NNNN)', 'NEG', 'RETN', 'IM 2', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'LDI', 'CPI', 'INI', 'OUTI', 'NOP', 'NOP', 'NOP', 'NOP', 'LDD', 'CPD', 'IND', 'OUTD', 'NOP', 'NOP', 'NOP', 'NOP', 'LDIR', 'CPIR',
      'INIR', 'OTIR', 'NOP', 'NOP', 'NOP', 'NOP', 'LDDR', 'CPDR', 'INDR', 'OTDR', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP');

  OpsFDPrefix: Array[0..255] of String =
     ('NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'ADD IY, BC', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'ADD IY, DE', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', '@LD IY, $NNNN', '@LD ($NNNN), IY',
      'INC IY', 'INC IYH', 'DEC IYH', '!LD IYH, $NN', 'NOP', 'NOP', 'ADD IY, IY', '@LD IY, ($NNNN)', 'DEC IY', 'INC IYL', 'DEC IYL', '!LD IYL, $NN',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', '?INC (IY+$DD)', '?DEC (IY+$DD)', '/LD (IY+$DD), $NN', 'NOP', 'NOP', 'ADD IY, SP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD B, IYH', 'LD B, IYL', '?LD B, (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD C, IYH',
      'LD C, IYL', '?LD C, (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD D, IYH', 'LD D, IYL', '?LD D, (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'LD E, IYH', 'LD E, IYL', '?LD E, (IY+$DD)', 'NOP', 'LD IYH, B', 'LD IYH, C', 'LD IYH, D', 'LD IYH, E', 'LD IYH, IYH', 'LD IYH, IYL',
      '?LD H, (IY+$DD)', 'LD IYH, A', 'LD IYL, B', 'LD IYL, C', 'LD IYL, D', 'LD IYL, E', 'LD IYL, IYH', 'LD IYL, IYL', '?LD L, (IY+$DD)', 'LD IYL, A',
      '?LD (IY+$DD), B', '?LD (IY+$DD), C', '?LD (IY+$DD), D', '?LD (IY+$DD), E', '?LD (IY+$DD), H', '?LD (IY+$DD), L', 'NOP', '?LD (IY+$DD), A', 'NOP', 'NOP', 'NOP',
      'NOP', 'LD A, IYH', 'LD A, IYL', '?LD A, (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'ADD A, IYH', 'ADD A, IYL', '?ADD A, (IY+$DD)', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'ADC A, IYH', 'ADC A, IYL', '?ADC A, (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'SUB IYH', 'SUB IYL',
      '?SUB (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'SBC A, IYH', 'SBC A, IYL', '?SBC A, (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'AND IYH',
      'AND IYL', '?AND (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'XOR IYH', 'XOR IYL', '?XOR (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'OR IYH', 'OR IYL', '?OR (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'CP IYH', 'CP IYL', '?CP (IY+$DD)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', '', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'POP IY', 'NOP', 'EX (SP), IY', 'NOP', 'PUSH IY', 'NOP', 'NOP', 'NOP',
      'JP (IY)', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'NOP', 'LD SP, IY', 'NOP',
      'NOP', 'NOP', 'NOP', 'NOP', 'NOP');

  OpsFDCBPrefix: Array[0..255] of String =
     ('?LD B, RLC (IY+$DD)', '?LD C, RLC (IY+$DD)', '?LD D, RLC (IY+$DD)', '?LD E, RLC (IY+$DD)', '?LD H, RLC (IY+$DD)', '?LD L, RLC (IY+$DD)', '?RLC (IY+$DD)',
      '?LD A, RLC (IY+$DD)', '?LD B, RRC (IY+$DD)', '?LD C, RRC (IY+$DD)', '?LD D, RRC (IY+$DD)', '?LD E, RRC (IY+$DD)', '?LD H, RRC (IY+$DD)', '?LD L, RRC (IY+$DD)',
      '?RRC (IY+$DD)', '?LD A, RRC (IY+$DD)', '?LD B, RL (IY+$DD)', '?LD C, RL (IY+$DD)', '?LD D, RL (IY+$DD)', '?LD E, RL (IY+$DD)', '?LD H, RL (IY+$DD)',
      '?LD L, RL (IY+$DD)', '?RL (IY+$DD)', '?LD A, RL (IY+$DD)', '?LD B, RR (IY+$DD)', '?LD C, RR (IY+$DD)', '?LD D, RR (IY+$DD)', '?LD E, RR (IY+$DD)',
      '?LD H, RR (IY+$DD)', '?LD L, RR (IY+$DD)', '?RR (IY+$DD)', '?LD A, RR (IY+$DD)', '?LD B, SLA (IY+$DD)', '?LD C, SLA (IY+$DD)', '?LD D, SLA (IY+$DD)',
      '?LD E, SLA (IY+$DD)', '?LD H, SLA (IY+$DD)', '?LD L, SLA (IY+$DD)', '?SLA (IY+$DD)', '?LD A, SLA (IY+$DD)', '?LD B, SRA (IY+$DD)', '?LD C, SRA (IY+$DD)',
      '?LD D, SRA (IY+$DD)', '?LD E, SRA (IY+$DD)', '?LD H, SRA (IY+$DD)', '?LD L, SRA (IY+$DD)', '?SRA (IY+$DD)', '?LD A, SRA (IY+$DD)', '?LD B, SLL (IY+$DD)',
      '?LD C, SLL (IY+$DD)', '?LD D, SLL (IY+$DD)', '?LD E, SLL (IY+$DD)', '?LD H, SLL (IY+$DD)', '?LD L, SLL (IY+$DD)', '?SLL (IY+$DD)', '?LD A, SLL (IY+$DD)',
      '?LD B, SRL (IY+$DD)', '?LD C, SRL (IY+$DD)', '?LD D, SRL (IY+$DD)', '?LD E, SRL (IY+$DD)', '?LD H, SRL (IY+$DD)', '?LD L, SRL (IY+$DD)', '?SRL (IY+$DD)',
      '?LD A, SRL (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)', '?BIT 0, (IY+$DD)',
      '?BIT 0, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)', '?BIT 1, (IY+$DD)',
      '?BIT 1, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)', '?BIT 2, (IY+$DD)',
      '?BIT 2, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)', '?BIT 3, (IY+$DD)',
      '?BIT 3, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)', '?BIT 4, (IY+$DD)',
      '?BIT 4, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)', '?BIT 5, (IY+$DD)',
      '?BIT 5, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)', '?BIT 6, (IY+$DD)',
      '?BIT 6, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)', '?BIT 7, (IY+$DD)',
      '?BIT 7, (IY+$DD)', '?LD B, RES 0 (IY+$DD)', '?LD C, RES 0 (IY+$DD)', '?LD D, RES 0 (IY+$DD)', '?LD E, RES 0 (IY+$DD)', '?LD H, RES 0 (IY+$DD)',
      '?LD L, RES 0 (IY+$DD)', '?RES 0, (IY+$DD)', '?LD A, RES 0 (IY+$DD)', '?LD B, RES 1 (IY+$DD)', '?LD C, RES 1 (IY+$DD)', '?LD D, RES 1 (IY+$DD)',
      '?LD E, RES 1 (IY+$DD)', '?LD H, RES 1 (IY+$DD)', '?LD L, RES 1 (IY+$DD)', '?RES 1, (IY+$DD)', '?LD A, RES 1 (IY+$DD)', '?LD B, RES 2 (IY+$DD)',
      '?LD C, RES 2 (IY+$DD)', '?LD D, RES 2 (IY+$DD)', '?LD E, RES 2 (IY+$DD)', '?LD H, RES 2 (IY+$DD)', '?LD L, RES 2 (IY+$DD)', '?RES 2, (IY+$DD)',
      '?LD A, RES 2 (IY+$DD)', '?LD B, RES 3 (IY+$DD)', '?LD C, RES 3 (IY+$DD)', '?LD D, RES 3 (IY+$DD)', '?LD E, RES 3 (IY+$DD)', '?LD H, RES 3, (IY+$DD)',
      '?LD L, RES 3 (IY+$DD)', '?RES 3, (IY+$DD)', '?LD A, RES 3 (IY+$DD)', '?LD B, RES 4 (IY+$DD)', '?LD C, RES 4 (IY+$DD)', '?LD D, RES 4 (IY+$DD)',
      '?LD E, RES 4 (IY+$DD)', '?LD H, RES 4 (IY+$DD)', '?LD L, RES 4 (IY+$DD)', '?RES 4, (IY+$DD)', '?LD A, RES 4 (IY+$DD)', '?LD B, RES 5 (IY+$DD)',
      '?LD C, RES 5 (IY+$DD)', '?LD D, RES 5 (IY+$DD)', '?LD E, RES 5 (IY+$DD)', '?LD H, RES 5 (IY+$DD)', '?LD L, RES 5 (IY+$DD)', '?RES 5, (IY+$DD)',
      '?LD A, RES 5 (IY+$DD)', '?LD B, RES 6 (IY+$DD)', '?LD C, RES 6 (IY+$DD)', '?LD D, RES 6 (IY+$DD)', '?LD E, RES 6 (IY+$DD)', '?LD H, RES 6 (IY+$DD)',
      '?LD L, RES 6 (IY+$DD)', '?RES 6, (IY+$DD)', '?LD A, RES 6 (IY+$DD)', '?LD B, RES 7, (IY+$DD)', '?LD C, RES 7 (IY+$DD)', '?LD D, RES 7 (IY+$DD)',
      '?LD E, RES 7 (IY+$DD)', '?LD H, RES 7 (IY+$DD)', '?LD L, RES 7 (IY+$DD)', '?RES 7, (IY+$DD)', '?LD A, RES 7 (IY+$DD)', '?LD B, SET 0 (IY+$DD)',
      '?LD C, SET 0 (IY+$DD)', '?LD D, SET 0 (IY+$DD)', '?LD E, SET 0 (IY+$DD)', '?LD H, SET 0 (IY+$DD)', '?LD L, SET 0 (IY+$DD)', '?SET 0, (IY+$DD)',
      '?LD A, SET 0 (IY+$DD)', '?LD B, SET 1 (IY+$DD)', '?LD C, SET 1 (IY+$DD)', '?LD D, SET 1 (IY+$DD)', '?LD E, SET 1 (IY+$DD)', '?LD H, SET 1 (IY+$DD)',
      '?LD L, SET 1 (IY+$DD)', '?SET 1, (IY+$DD)', '?LD A, SET 1 (IY+$DD)', '?LD B, SET 2, (IY+$DD)', '?LD C, SET 2 (IY+$DD)', '?LD D, SET 2 (IY+$DD)',
      '?LD E, SET 2 (IY+$DD)', '?LD H, SET 2 (IY+$DD)', '?LD L, SET 2 (IY+$DD)', '?SET 2, (IY+$DD)', '?LD A, SET 2 (IY+$DD)', '?LD B, SET 3 (IY+$DD)',
      '?LD C, SET 3 (IY+$DD)', '?LD D, SET 3 (IY+$DD)', '?LD E, SET 3 (IY+$DD)', '?LD H, SET 3 (IY+$DD)', '?LD L, SET 3 (IY+$DD)', '?SET 3, (IY+$DD)',
      '?LD A, SET 3 (IY+$DD)', '?LD B, SET 4 (IY+$DD)', '?LD C, SET 4 (IY+$DD)', '?LD D, SET 4 (IY+$DD)', '?LD E, SET 4 (IY+$DD)', '?LD H, SET 4 (IY+$DD)',
      '?LD L, SET 4 (IY+$DD)', '?SET 4, (IY+$DD)', '?LD A, SET 4 (IY+$DD)', '?LD B, SET 5, (IY+$DD)', '?LD C, SET 5 (IY+$DD)', '?LD D, SET 5 (IY+$DD)',
      '?LD E, SET 5 (IY+$DD)', '?LD H, SET 5 (IY+$DD)', '?LD L, SET 5 (IY+$DD)', '?SET 5, (IY+$DD)', '?LD A, SET 5 (IY+$DD)', '?LD B, SET 6 (IY+$DD)',
      '?LD C, SET 6 (IY+$DD)', '?LD D, SET 6 (IY+$DD)', '?LD E, SET 6 (IY+$DD)', '?LD H, SET 6 (IY+$DD)', '?LD L, SET 6 (IY+$DD)', '?SET 6, (IY+$DD)',
      '?LD A, SET 6 (IY+$DD)', '?LD B, SET 7 (IY+$DD)', '?LD C, SET 7 (IY+$DD)', '?LD D, SET 7 (IY+$DD)', '?LD E, SET 7 (IY+$DD)', '?LD H, SET 7 (IY+$DD)',
      '?LD L, SET 7 (IY+$DD)', '?SET 7, (IY+$DD)', '?LD A, SET 7 (IY+$DD)');


  Procedure UpdateCPU;

implementation

{$R *.DFM}

Uses Utility, Display, ASMEditor, SysVars, BASinMain;

Procedure UpdateCPU;
Begin

  CPUWindow.CPUViewPos := Word(Max(0, Registers.PC - ((CPUWindow.FastIMG1.Bmp.AbsHeight Div 8) Div 2)));
  CPUWindow.HighlightAddr := Registers.PC;
  CPUWindow.CurViewAddr := Registers.PC;
  CPUWindow.CreateDisassembly;
  CPUWindow.UpdateRegs;

End;

Procedure TCPUWindow.AlterOpcodes;

  Procedure TrimStr(var Str: String);
  Var
     Ps: Integer;
  Begin
     Ps := Pos(' ', Str);
     If Ps <> 0 Then
        If Str[1] in ['!','<','@','#','=','?','>','/','.','%'] Then
           Str := Copy(Str, 1, Ps)+Copy('     ', 1, 5-(Ps-1))+Copy(Str, Ps+1, 9999)
        Else
           Str := Copy(Str, 1, Ps)+Copy('     ', 1, 5-Ps)+Copy(Str, Ps+1, 9999);

     While Pos(', ', Str) <> 0 Do Begin
        Ps := Pos(', ', Str);
        Str := Copy(Str, 1, Ps)+Copy(Str, Ps+2, 9999);
     End;
  End;

Var
  Idx: Integer;
Begin

  For Idx := 0 To High(OpsNoPrefix) Do TrimStr(OpsNoPrefix[Idx]);
  For Idx := 0 To High(OpsCBPrefix) Do TrimStr(OpsCBPrefix[Idx]);
  For Idx := 0 To High(OpsDDPrefix) Do TrimStr(OpsDDPrefix[Idx]);
  For Idx := 0 To High(OpsDDCBPrefix) Do TrimStr(OpsDDCBPrefix[Idx]);
  For Idx := 0 To High(OpsEDPrefix) Do TrimStr(OpsEDPrefix[Idx]);
  For Idx := 0 To High(OpsFDPrefix) Do TrimStr(OpsFDPrefix[Idx]);
  For Idx := 0 To High(OpsFDCBPrefix) Do TrimStr(OpsFDCBPrefix[Idx]);

End;

Procedure TCPUWindow.BuildROMSymbols;
Var
  Idx: Integer;
Begin

  ClearLabels(0, 65535);
  For Idx := 0 To 1116 Do
     Symbols[ROMSymbols[Idx].Address] := ROMSymbols[Idx];

  For Idx := 0 To 70 Do Begin
     Symbols[SystemVariables[Idx].Address].sType := SystemVariables[Idx].Bytes;
     Symbols[SystemVariables[Idx].Address].Desc := SystemVariables[Idx].Name;
     Symbols[SystemVariables[Idx].Address].Address := SystemVariables[Idx].Address;
  End;

End;

Procedure TCPUWindow.AddAssemblerSymbols;
Var
  Idx, Addr: Integer;
  Str: String;
Begin

  For Idx := 0 To AllLabels.Count -1 Do Begin
     Str := AllLabels[Idx];
     Addr := StrToInt(Copy(Str, 1, Pos(',', Str)-1));
     If (Addr > 0) and (Addr < 65536) Then Begin
        Symbols[Addr].Address := Addr;
        Symbols[Addr].Desc := Copy(Str, Pos(',', Str)+1, 99999);
     End;
  End;

End;

Procedure TCPUWindow.ClearLabels(MinAddr, MaxAddr: Integer);
Begin
  While MinAddr <> MaxAddr +1 Do Begin
     Symbols[MinAddr].Address := 65536;
     Inc(MinAddr);
  End;
End;

Function TCPUWindow.GetDisassembly(Var nPC: Word; Var Jump: TJumpOpcode): ShortString;
Var
  B, Len, IXOff, IXSign: Byte;
  Str, Str2: ShortString;
  OldnPC, F, LabelAddr, Idx: Word;
  IsCBPrefix: Boolean;
Label
  FillDetails;
Begin

  IsCBPrefix := False;
  OldnPC := nPC;

  If Symbols[nPC].sType > 0 Then Begin
     // Special bytes - probably sysvars
     If Symbols[nPC].sType = 1 Then Begin
        If Opt_AsmHexValues Then
           Result := 'DEFB $'+HexBytes[Memory[nPC]]
        Else
           Result := 'DEFB '+IntToStr(Memory[nPC]);
        Inc(nPC);
     End Else
        If Symbols[nPC].sType = 2 Then Begin
           If Opt_AsmHexValues Then
              Result := 'DEFW $'+HexBytes[Memory[nPC]]+HexBytes[Memory[nPC+1]]
           Else
              Result := 'DEFW '+IntToStr(GetWord(@Memory[nPC]));
           Inc(nPC, 2);
        End Else Begin // Multibyte symbol
           Result := 'DEFB ';
           For Idx := nPC to nPC + Symbols[nPC].sType -1 Do Begin
              If Opt_AsmHexValues Then
                 Result := Result+'$'+HexBytes[Memory[Idx]]
              Else
                 Result := Result+IntToStr(Memory[Idx]);
              If Idx < nPC + Symbols[nPC].sType -1 Then
                 Result := Result + ',';
           End;
           Inc(nPC, Symbols[nPC].sType);
        End;
  End Else If Symbols[nPC].sType = 0 Then Begin
     If LastOp = 1 Then Begin // Last op was an RST $08 - get the next byte
        If Opt_AsmHexValues Then
           Result := 'DEFB $'+HexBytes[Memory[nPC]]
        Else
           Result := 'DEFB '+IntToStr(Memory[nPC]);
        Inc(nPC);
        LastOp := 0;
     End Else
{        If LastOp = 2 Then Begin // Last op was RST $28 - get all bytes up to the next $38
           If Opt_AsmHexValues Then
              Result := 'DEFB $'+HexBytes[Memory[nPC]]
           Else
              Result := 'DEFB '+IntToStr(Memory[nPC]);
           If Memory[nPC] = $38 Then
              LastOp := 0;
           Inc(nPC);
        End Else} Begin
           LastOp := 0;
           B := Memory[nPC];
           Case B of
              $CB:
                 Begin
                    Result := OpsCBPrefix[Memory[nPC+1]];
                    Inc(nPC, 2);
                 End;
              $DD:
                 Begin
                    IXSign := 43;
                    B := Memory[nPC+1];
                    If B = $CB Then Begin
                       IsCBPrefix := True;
                       B := Memory[nPC +3];
                       Result := OpsDDCBPrefix[B];
                       Inc(nPC, 3);
                    End Else Begin
                       Result := OpsDDPrefix[B];
                       if Result = 'NOP' then begin
                          if Opt_AsmHexValues then
                             Result := 'DEFB $DD'
                          else
                             Result := 'DEFB 221';
                          Inc(nPC);
                       end else
                          Inc(nPC, 2);
                    End;
                 End;
              $ED:
                 Begin
                    B := Memory[nPC+1];
                    Result := OpsEDPrefix[B];
                    if Result = 'NOP' then begin
                       if (B <> $77) and (B <> $7F) then begin
                          if Opt_AsmHexValues then
                             Result := 'DEFB $ED'
                          else
                             Result := 'DEFB 237';
                          inc(nPC);
                       end else
                          inc(nPC, 2);
                    end else
                       Inc(nPC, 2);
                 End;
              $FD:
                 Begin
                    IXSign := 43;
                    B := Memory[nPC+1];
                    If B = $CB Then Begin
                       IsCBPrefix := True;
                       B := Memory[nPC +3];
                       Result := OpsFDCBPrefix[B];
                       Inc(nPC, 3);
                    End Else Begin
                       Result := OpsFDPrefix[B];
                       if Result = 'NOP' then begin
                          if Opt_AsmHexValues then
                             Result := 'DEFB $FD'
                          else
                             Result := 'DEFB 253';
                          Inc(nPC);
                       end else
                          Inc(nPC, 2);
                    End;
                 End;
           Else
              Begin
                 Result := OpsNoPrefix[B];
                 Inc(nPC);
              End;
           End;
        End;
     End;

  Jump.Address := -1;
  Jump.IsJump := False;

  Case Ord(Result[1]) Of
     33:
        Begin // [!] LD x, $NN types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           B := Memory[nPC];
           if Opt_AsmHexValues then
              Str := '$'+HexBytes[B] else Str := IntToStr(B);
           If Result[Len] = ')' then
              Result := Copy(Result, 1, Len-4) + Str + Result[Len]
           else
              Result := Copy(Result, 1, Len-3) + Str;
           Inc(nPC);
        End;
     60:
        Begin // [<] AND $NN types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           B := Memory[nPC];
           if Opt_AsmHexValues then
              Str := '$'+HexBytes[B]
           else
              Str := IntToStr(B);
           Result := Copy(Result, 1, Len-3) + Str;
           Inc(nPC);
        End;
     37, 38, 64:
        Begin // [@] LD HL, ($NNNN) and LD ($NNNN), A Types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           LabelAddr := GetWord(@Memory[nPC]);
           If Symbols[LabelAddr].Address <> 65536 Then
              Str := Symbols[LabelAddr].Desc
           else
              If Opt_AsmHexValues Then
                 Str := '$'+FastIntToHex(LabelAddr)
              else
                 Str := IntToStr(LabelAddr);
           Jump.Address := LabelAddr;
           Jump.Len := Length(Str);
           If Result[Len] = ')' Then Begin // LD HL, ($NNNN) type
              Result := Copy(Result, 1, Len - 6) + Str + Copy(Result, Len, 1);
              Jump.StartChar := Len -5;
           End Else If Result[Len] = 'N' Then Begin // CALL $NNNN type
              Result := Copy(Result, 1, Len - 5) + Str;
              Jump.StartChar := Len -4;
           End Else Begin // LD ($NNNN), C Type
              Result := Copy(Result, 1, 7) + Str + Copy(Result, 13, Len - 10);
              Jump.StartChar := 8;
           End;
           Inc(nPC, 2);
        End;
     35:
        Begin // [#] JR $$$+2 Types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           LabelAddr := OldnPC + 2 + TwoComp[Memory[nPC]];
           If Symbols[LabelAddr].Address <> 65536 Then
              Str := Symbols[LabelAddr].Desc
           else
              If Opt_AsmHexValues Then
                 Str := '$'+FastIntToHex(LabelAddr)
              else
                 Str := IntToStr(LabelAddr);
           Jump.Address := LabelAddr;
           Jump.Len := Length(Str);
           Jump.StartChar := Len - 4;
           Result := Copy(Result, 1, Len - 5) + Str;
           Inc(nPC);
        End;
     61:
        begin // [=] JP NZ $$$+3 Types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           LabelAddr := GetWord(@Memory[nPC]);
           If Symbols[LabelAddr].Address <> 65536 Then
              Str := Symbols[LabelAddr].Desc
           else
              If Opt_AsmHexValues Then
                 Str := '$'+FastIntToHex(LabelAddr)
              else
                 Str := IntToStr(LabelAddr);
           Jump.Address := LabelAddr;
           Jump.Len := Length(Str);
           Jump.StartChar := Len - 4;
           Result := Copy(Result, 1, Len - 5) + Str;
           Inc(nPC, 2);
        End;
     63:
        Begin // [?] LD A, (IX+$DD) types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           If IsCBPrefix Then
              IXOff := Memory[nPC-1]
           Else
              IXOff := Memory[nPC];
           if IXOff > 127 then begin
              IXSign := 45;
              IXOff := 256 - IXOff;
           end;
           if Opt_AsmHexValues then
              Str := '$'+HexBytes[IXOff]
           else
              Str := IntToStr(IXOff);
           If Result[Len] = ')' then begin
              Result := Copy(Result, 1, Len-5) + Chr(IXSign) + Str + Result[Len];
           End Else Begin
              Result := Copy(Result, 1, 7) + Chr(IXSign) + Str + Copy(Result, 12, Len);
           End;
           Inc(nPC);
        End;
     62:
        Begin // [>] OUT ($NN), A Type
           Result[1] := ' ';
           Len := Ord(Result[0]);
           B := Memory[nPC];
           if Opt_AsmHexValues then
              Str := '$'+HexBytes[B]
           else
              Str := IntToStr(B);
           Result := Copy(Result, 1, 7) + Str + Copy(Result, 11, Len);
           Inc(nPC);
        End;
     47:
        Begin // [/] LD (IX+$DD), $NN Types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           IXOff := Memory[nPC];
           if IXOff > 127 then begin
              IXSign := 45;
              IXOff := 256 - IXOff;
           end;
           if Opt_AsmHexValues then
              Str := '$'+HexBytes[IXOff]
           else
              Str := IntToStr(IXOff);
           Result := Copy(Result, 1, 7) + Chr(IXSign) + Str + Copy(Result, 12, Len);
           B := Memory[nPC +1];
           if Opt_AsmHexValues then
              Str := '$'+HexBytes[B]
           else
              Str := IntToStr(B);
           Len := Ord(Result[0]);
           Result := Copy(Result, 1, Len-3) + Str;
           Inc(nPC, 2);
        End;
     46:
        Begin // [.] RST $NN Types
           Result[1] := ' ';
           Len := Ord(Result[0]);
           if Opt_AsmHexValues then
              Str := '$'+HexBytes[B-199]
           else
              Str := IntToStr(B-199);
           Result := Copy(Result, 1, Len-3) + Str;
           If B-199 = 8 Then
              LastOp := 1
           Else
              If B-199 = 40 Then
                 LastOp := 2;
        end;
  End;

  // Fill in the new info (Address and bytes for the opcode)

  Str := '';
  While Result[1] = ' ' Do
     Result := Copy(Result, 2, 9999);

  If (Copy(Result, 1, 4) = 'CALL') or (Result[1] = 'J') Then
     Jump.IsJump := True
  Else
     Jump.IsJump := False;

  If nPC -1 - OldnPC <= 5 Then
     Idx := nPC -1
  Else
     Idx := OldnPC + 4;

  For F := OldnPC to Idx Do begin
     B := Memory[F];
     If Opt_AsmAsciiBytes Then Begin
        If B in [32..164] Then
           Str := Str + Chr(B)
        Else
           Str := Str + '.';
     End Else
        if Opt_AsmHexValues then
           Str := Str + HexBytes[B] + ' '
        else
           Str := Str + IntToStr(B) + Copy('   ',1,4-Length(IntToStr(B)));
  end;

  if Opt_AsmHexValues then Begin
     While Length(Str) < 15 Do
        Str := Str + ' ';
     Str := '$'+FastIntToHex(OldnPC)+': ' + Str;
     Inc(Jump.StartChar, 14);
  End else begin
     While Length(Str) < 20 Do
        Str := Str + ' ';
     Str2 := IntToStr(OldnPC);
     Str := Copy('     ',1,5-Length(Str2))+Str2+': ' + Str;
     Inc(Jump.StartChar, 19);
  end;

  Result := Str + Result;

End;


procedure TCPUWindow.FormCreate(Sender: TObject);
Var
  Idx: Integer;
begin

  For Idx := 0 to 65535 Do
     CPUBreakpoints[Idx] := 0;

  AlterOpcodes;
  BuildROMSymbols;

  LabelIn := Nil;

  AddressHistory := TStringlist.Create;

  Edit14.SetBounds(ClientWidth - Edit14.Width - 8, Edit1.Height + 12, Edit14.Width, Edit14.Height);
  Label10.SetBounds(Edit14.Left - 20, (Edit14.Top + (Edit14.Height - Label10.Height) Div 2), Label10.Width, Label10.Height);
  Edit10.SetBounds(Edit14.Left, Edit14.Top + Edit14.Height + 8, Edit10.Width, Edit10.Height);
  Label11.SetBounds(Edit10.Left - 20, (Edit10.Top + (Edit10.Height - Label11.Height) Div 2), Label11.Width, Label11.Height);
  Edit11.SetBounds(Edit10.Left, Edit10.Top + Edit10.Height + 8, Edit11.Width, Edit11.Height);
  Label12.SetBounds(Edit11.Left - 20, (Edit11.Top + (Edit11.Height - Label12.Height) Div 2), Label12.Width, Label12.Height);
  Edit12.SetBounds(Edit11.Left, Edit11.Top + Edit11.Height + 8, Edit12.Width, Edit12.Height);
  Label13.SetBounds(Edit12.Left - 20, (Edit12.Top + (Edit12.Height - Label13.Height) Div 2), Label13.Width, Label13.Height);
  Edit13.SetBounds(Edit12.Left, Edit12.Top + Edit12.Height + 8, Edit13.Width, Edit13.Height);
  Label14.SetBounds(Edit13.Left - 20, (Edit13.Top + (Edit13.Height - Label14.Height) Div 2), Label14.Width, Label14.Height);
  Edit9.SetBounds(Edit13.Left, Edit13.Top + Edit13.Height + 8, Edit9.Width, Edit9.Height);
  Label15.SetBounds(Edit9.Left - 20, (Edit9.Top + (Edit9.Height - Label15.Height) Div 2), Label15.Width, Label15.Height);
  Edit15.SetBounds(Edit9.Left, Edit9.Top + Edit9.Height + 8, Edit15.Width, Edit15.Height);
  Label16.SetBounds(Edit15.Left - 20, (Edit15.Top + (Edit15.Height - Label16.Height) Div 2), Label16.Width, Label16.Height);
  Edit2.SetBounds(Edit14.Left - 28 - Edit2.Width, Edit14.Top, Edit2.Width, Edit2.Height);
  Label3.SetBounds(Edit2.Left - 20, (Edit2.Top + (Edit2.Height - Label3.Height) Div 2), Label3.Width, Label3.Height);
  Edit3.SetBounds(Edit2.Left, Edit2.Top + Edit2.Height + 8, Edit3.Width, Edit3.Height);
  Label4.SetBounds(Edit3.Left - 20, (Edit3.Top + (Edit3.Height - Label4.Height) Div 2), Label4.Width, Label4.Height);
  Edit4.SetBounds(Edit3.Left, Edit3.Top + Edit3.Height + 8, Edit4.Width, Edit4.Height);
  Label5.SetBounds(Edit4.Left - 20, (Edit4.Top + (Edit4.Height - Label5.Height) Div 2), Label5.Width, Label5.Height);
  Edit5.SetBounds(Edit4.Left, Edit4.Top + Edit4.Height + 8, Edit5.Width, Edit5.Height);
  Label6.SetBounds(Edit5.Left - 20, (Edit5.Top + (Edit5.Height - Label6.Height) Div 2), Label6.Width, Label6.Height);
  Edit6.SetBounds(Edit5.Left, Edit5.Top + Edit5.Height + 8, Edit6.Width, Edit6.Height);
  Label7.SetBounds(Edit6.Left - 20, (Edit6.Top + (Edit6.Height - Label7.Height) Div 2), Label7.Width, Label7.Height);
  Edit7.SetBounds(Edit6.Left, Edit6.Top + Edit6.Height + 8, Edit7.Width, Edit7.Height);
  Label8.SetBounds(Edit7.Left - 20, (Edit7.Top + (Edit7.Height - Label8.Height) Div 2), Label8.Width, Label8.Height);
  Edit8.SetBounds(Edit7.Left, Edit7.Top + Edit7.Height + 8, Edit8.Width, Edit8.Height);
  Label9.SetBounds(Edit8.Left - 20, (Edit8.Top + (Edit8.Height - Label9.Height) Div 2), Label9.Width, Label9.Height);

  Label25.SetBounds(Edit8.Left, Edit8.Top + Edit8.Height + 16, Label25.Width, Label25.Height);
  CheckBox2.SetBounds(Edit8.Left, Label25.Top + Label25.Height + 8 + Label17.Height, CheckBox2.Width, CheckBox2.Height);
  CheckBox3.SetBounds(CheckBox2.Left + CheckBox2.Width + 3, CheckBox2.Top, CheckBox2.Width, CheckBox2.Height);
  CheckBox4.SetBounds(CheckBox3.Left + CheckBox2.Width + 3, CheckBox2.Top, CheckBox2.Width, CheckBox2.Height);
  CheckBox5.SetBounds(CheckBox4.Left + CheckBox2.Width + 3, CheckBox2.Top, CheckBox2.Width, CheckBox2.Height);
  CheckBox6.SetBounds(CheckBox5.Left + CheckBox2.Width + 3, CheckBox2.Top, CheckBox2.Width, CheckBox2.Height);
  CheckBox7.SetBounds(CheckBox6.Left + CheckBox2.Width + 3, CheckBox2.Top, CheckBox2.Width, CheckBox2.Height);
  CheckBox8.SetBounds(CheckBox7.Left + CheckBox2.Width + 3, CheckBox2.Top, CheckBox2.Width, CheckBox2.Height);
  CheckBox9.SetBounds(CheckBox8.Left + CheckBox2.Width + 3, CheckBox2.Top, CheckBox2.Width, CheckBox2.Height);

  Label17.SetBounds(CheckBox2.Left + ((CheckBox2.Width - Label17.Width) Div 2), CheckBox2.Top - Label17.Height - 4, Label17.Width, Label17.Height);
  Label18.SetBounds(CheckBox3.Left + ((CheckBox3.Width - Label18.Width) Div 2), CheckBox2.Top - Label18.Height - 4, Label18.Width, Label18.Height);
  Label19.SetBounds(CheckBox4.Left + ((CheckBox4.Width - Label19.Width) Div 2), CheckBox2.Top - Label19.Height - 4, Label19.Width, Label19.Height);
  Label20.SetBounds(CheckBox5.Left + ((CheckBox5.Width - Label20.Width) Div 2), CheckBox2.Top - Label20.Height - 4, Label20.Width, Label20.Height);
  Label21.SetBounds(CheckBox6.Left + ((CheckBox6.Width - Label21.Width) Div 2), CheckBox2.Top - Label21.Height - 4, Label21.Width, Label21.Height);
  Label22.SetBounds(CheckBox7.Left + ((CheckBox7.Width - Label22.Width) Div 2), CheckBox2.Top - Label22.Height - 4, Label22.Width, Label22.Height);
  Label23.SetBounds(CheckBox8.Left + ((CheckBox8.Width - Label23.Width) Div 2), CheckBox2.Top - Label23.Height - 4, Label23.Width, Label23.Height);
  Label24.SetBounds(CheckBox9.Left + ((CheckBox9.Width - Label24.Width) Div 2), CheckBox2.Top - Label24.Height - 4, Label24.Width, Label24.Height);

  CheckBox1.SetBounds(CheckBox2.Left, CheckBox2.Top + CheckBox2.Height + 9, CheckBox1.Width, CheckBox1.Height);

  Button7.SetBounds(Edit2.Left - 28 - Button7.Width, ClientHeight - 8 - Button7.Height, Button7.Width, Button7.Height);
  Button6.SetBounds(Button7.Left - 8 - Button6.Width, Button7.top, Button6.Width, Button6.Height);
  Button3.SetBounds(Button6.Left - 8 - Button3.Width, Button6.Top, Button3.Width, Button3.Height);
  Button2.SetBounds(Button3.Left - 4 - Button2.Width, Button6.Top, Button2.Width, Button2.Height);
  button4.SetBounds(Button2.Left - 8 - Button4.Width, Button6.Top, Button4.Width, Button4.Height);
  Button5.SetBounds(Button4.Left - 4 - Button5.Width, Button6.Top, Button5.Width, Button5.Height);
  Button1.SetBounds(8, Button6.Top, Button1.Width, Button1.Height);

  Panel1.SetBounds(8, 12 + Edit1.Height, Edit2.Left - 36, ClientHeight - 16 - Edit1.Height - 12 - Button1.Height);
  ScrollBar1.SetBounds(Panel1.Left + Panel1.Width, Panel1.Top, ScrollBar1.Width, ScrollBar1.Height);
  Edit1.SetBounds((Panel1.Left + Panel1.Width) - Edit1.Width - Button8.Width - 4, 8, Edit1.Width, Edit1.Height);
  Button8.SetBounds(Edit1.Left + 4 + Edit1.Width, Edit1.Top, Edit1.Height, Edit1.Height);
  Label2.SetBounds(Edit1.Left - Label2.Width - 4, (Edit1.Top + (Edit1.Height - Label2.Height) Div 2), Label2.Width, Label2.Height);
  Label1.SetBounds(Edit2.Left, Label2.Top, Label1.Width, Label1.Height);
  Label26.SetBounds(8, Label2.Top, Label26.Width, Label26.Height);

  Constraints.MinHeight := Edit17.Top + Edit17.Height + 53;
  Constraints.MinWidth := Edit14.Width + Button7.Width + Button1.Width + Button5.Width + Button4.Width + Button2.Width + Button3.Width + Button6.Width + Edit2.Width + 120;
  CALLCounter := 0;
  FirstRun := True;
  StepOperation := False;
  ExitProcOperation := False;



  FormResize(nil);

end;

procedure TCPUWindow.FormShow(Sender: TObject);
Var
  Idx: Integer;
begin

  If FirstRun Then Begin
     CopyMemory(@OldRegs.PC, @Registers.PC, SizeOf(TZ80Registers));
     FirstRun := False;
  End;

  CPUShowing := True;

  CPUViewPos := Word(Max(0, Registers.PC - ((FastIMG1.Bmp.AbsHeight Div 8) Div 2)));
  HighlightAddr := Registers.PC;
  CurViewAddr := Registers.PC;
  For Idx := 0 To 65535 Do Begin
     If CPUBreakpoints[Idx] and 2 = 2 Then
        CPUBreakpoints[Idx] := CPUBreakpoints[Idx] And -3;
  End;
  CALLCounter := 0;
  StepOperation := False;
  ExitProcOperation := False;
  If Sender <> nil Then Begin
     If CPURunning Then Begin
        If Registers.EmuRunning Then Begin
           Button5.Caption := 'Stop';
           Button5.Enabled := True;
        End Else Begin
           Button5.Caption := 'Run';
           Button5.Enabled := Not Registers.EmuRunning;
        End;
     End Else Begin
        Button5.Caption := 'Run';
        Button5.Enabled := Not Registers.EmuRunning;
     End;
     Button2.Enabled := Not CPURunning;
     Button3.Enabled := Not CPURunning;
     Button4.Enabled := Not CPURunning;
     Button6.Enabled := Not CPURunning;
  End;
  CreateDisassembly;
  UpdateRegs;

end;

Procedure TCPUWindow.CreateDisassembly;
Var
  X, Y, YPos, GutterW, Ink, Paper, Bright: Integer;
  nPC, OldPC: Word;
  DisAsmStr: String;
  SpecDark: TFColor;
  Jump: TJumpOpcode;
Const
  PCArray: Array[0..7, 0..7] of Byte =
     ((7,7,7,7,7,7,7,7),
      (7,7,0,0,0,0,7,7),
      (7,0,0,4,4,0,0,7),
      (7,0,4,4,4,4,0,7),
      (7,0,4,4,4,4,0,7),
      (7,0,0,4,4,0,0,7),
      (7,7,0,0,0,0,7,7),
      (7,7,7,7,7,7,7,7));
Begin

  SetLength(Addresses, (FastIMG1.Bmp.AbsHeight Div 8) +8);
  SetLength(Jumps, Length(Addresses));

  ScrollBar1.Position := CPUViewPos;
  ScrollBar1.PageSize := FastIMG1.Bmp.AbsHeight Div 8;
  ScrollBar1.LargeChange := FastIMG1.Bmp.AbsHeight Div 8;

  SpecDark.r := Max(0, DisplayPalette[7].R -10);
  SpecDark.g := Max(0, DisplayPalette[7].G -10);
  SpecDark.b := Max(0, DisplayPalette[7].B -10);

  If Opt_AsmHexValues Then
     GutterW := 48
  Else
     GutterW := 48;

  FastIMG1.Bmp.Clear(TFColorAToTFColor(DisplayPalette[7]));
  FillRect(FastIMG1.Bmp, 0, 0, GutterW -1, FastIMG1.Bmp.AbsHeight -1, SpecDark);
  If Opt_AsmHexValues Then
     Edit1.Text := '$'+FastIntToHex(HighlightAddr)
  Else
     Edit1.Text := IntToStr(HighlightAddr);

  YPos := 1;
  nPC := Max(0, CPUViewPos - 128);
  PrevAddr := nPC;

  While YPos < FastIMG1.Bmp.AbsHeight *2 Do Begin

     OldPc := nPC;
     DisasmStr := GetDisassembly(nPC, Jump);
     If OldPC < CPUViewPos Then PrevAddr := OldPC;
     If OldPC >= CPUViewPos Then Begin
        If YPos < FastIMG1.Bmp.AbsHeight -1 Then Begin

           If Symbols[OldPC].Address <> 65536 Then Begin
              Ink := 1;
              If OldPC > 16383 Then
                 Bright := 1
              Else
                 Bright := 0;
              Addresses[YPos Div 8] := -1;
              SpecTextToDIB(FastIMG1.Bmp, GutterW +8, YPos, Symbols[OldPC].Desc+':', Ink, -1, Bright, False, False);
              Inc(Ypos, 8);
           End;

           Addresses[YPos Div 8] := OldPC;
           Addresses[(YPos Div 8) +1] := -1;
           MaxHighlight := YPos Div 8;

           If (OldPC <= HighlightAddr) and (nPC > HighlightAddr) Then Begin
              Paper := 1;
              Ink := 7;
              Bright := 1;
              While Length(DisAsmStr) < (FastIMG1.Bmp.Width +8) Div 8 Do
                 DisAsmStr := DisAsmStr + ' ';
              HighlightAt := YPos Div 8;
              HighlightAddr := OldPC;
              If CPUBreakpoints[HighlightAddr] <> 0 Then Begin
                 Paper := 1;
                 Ink := 2;
              End;
           End Else
              If CPUBreakpoints[OldPC] and 1 = 1 Then Begin
                 Paper := 2;
                 Ink := 7;
                 Bright := 1;
                 While Length(DisAsmStr) < (FastIMG1.Bmp.Width +8) Div 8 Do
                    DisAsmStr := DisAsmStr + ' ';
              End Else Begin
                 Paper := 7;
                 Ink := 0;
                 Bright := 0;
              End;

           If Paper = 7 Then
              SpecTextToDIB(FastIMG1.Bmp, 0, YPos, Copy(DisasmStr, 1, (GutterW Div 8)), Ink, -1, Bright, False, False)
           Else
              SpecTextToDIB(FastIMG1.Bmp, 0, YPos, Copy(DisasmStr, 1, (GutterW Div 8)), Ink, Paper, Bright, False, False);
           SpecTextToDIB(FastIMG1.Bmp, GutterW, YPos, Copy(DisasmStr, (GutterW Div 8) +1, 9999), Ink, Paper, Bright, False, False);

           If Jump.Address <> -1 Then Begin
              Jumps[YPos Div 8] := Jump;
              If Jump.Address > 16383 Then Begin
                 SpecTextToDIB(FastIMG1.Bmp, GutterW + Jump.StartChar*8, YPos, Copy(DisasmStr, Jump.StartChar + 7, Jump.Len), Ink, -1, 1, False, False);
                 SpecTextToDIB(FastIMG1.Bmp, 1 + GutterW + Jump.StartChar*8, YPos, Copy(DisasmStr, Jump.StartChar + 7, Jump.Len), Ink, -1, 1, False, False);
              End Else Begin
                 SpecTextToDIB(FastIMG1.Bmp, GutterW + Jump.StartChar*8, YPos, Copy(DisasmStr, Jump.StartChar + 7, Jump.Len), Ink, -1, 0, False, False);
                 SpecTextToDIB(FastIMG1.Bmp, 1 + GutterW + Jump.StartChar*8, YPos, Copy(DisasmStr, Jump.StartChar + 7, Jump.Len), Ink, -1, 0, False, False);
              End;
           End Else
              Jumps[YPos Div 8] := Jump;

           If OldPC = Registers.PC Then Begin
              For Y := 0 To 7 Do
                 For X := 0 To 7 Do
                    If PCArray[Y, X] <> 7 Then
                       If FastIMG1.Bmp.AbsHeight - (YPos + Y) >= 0 Then
                          FastIMG1.Bmp.Pixels32[FastIMG1.Bmp.AbsHeight - (YPos + Y), 48+X] := DisplayPalette[PCArray[Y, X]];
           End;
        End;
        If Jump.IsJump Then Inc(YPos, 8);
        Inc(YPos, 8);
     End;
     If OldPC > nPC Then Break;
  End;

  FastIMG1.Repaint;

End;

procedure TCPUWindow.FormResize(Sender: TObject);
begin

  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height +8, 32);
  CreateDisassembly;

end;

procedure TCPUWindow.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  NewHighlightAddr: Word;
begin

  Edit16.SetFocus;

  If JumpAddr <> -1 Then
     If Screen.Cursor = crHandPoint Then Begin
        CPUViewPos := Max(0, JumpAddr - ((FastIMG1.Bmp.AbsHeight Div 8) Div 2));
        AddressHistory.Add(IntToStr(CurViewAddr));
        CurViewAddr := JumpAddr;
        HighlightAddr := JumpAddr;
        CreateDisassembly;
        Exit;                              
     End;

  NewHighlightAddr := Y Div 8;
  While (NewHighlightAddr > 0) and (Addresses[NewHighlightAddr] = -1) Do
     If NewHighlightAddr < HighlightAt Then
        Dec(NewHighlightAddr)
     Else
        Inc(NewHighlightAddr);

  While Addresses[NewHighlightAddr] = -1 Do
     Inc(NewHighlightAddr);
  HighlightAddr := Addresses[NewHighlightAddr];

  CreateDisassembly;

end;

procedure TCPUWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
  NewHighlightAddr: Word;
begin
  Edit16.Text := '';
  Case Key of
     VK_DOWN:
        Begin
           NewHighlightAddr := HighlightAt +1;
              While Addresses[NewHighlightAddr] = -1 Do
                 Inc(NewHighlightAddr);
           HighlightAddr := Addresses[NewHighlightAddr];
           If NewHighlightAddr >= MaxHighlight Then
              CPUViewPos := Min(65535, CPUViewPos + ((FastIMG1.Bmp.AbsHeight Div 2) Div 8));
           CreateDisassembly;
        End;
     VK_UP:
        Begin
           If HighLightAt = 0 Then Begin
              If HighlightAddr > 0 then
                 Dec(HighlightAddr);
              CPUViewPos := Max(0, CPUViewPos - ((FastIMG1.Bmp.AbsHeight Div 2) Div 8));
           End Else Begin
              NewHighlightAddr := HighlightAt -1;
              While (NewHighlightAddr > 0) and (Addresses[NewHighlightAddr] = -1) Do
                 Dec(NewHighlightAddr);
              If Addresses[NewHighlightAddr] = -1 Then Begin
                 While Addresses[NewHighlightAddr] = -1 Do
                    Inc(NewHighlightAddr);
                 CPUViewPos := Max(0, CPUViewPos - ((FastIMG1.Bmp.AbsHeight Div 2) Div 8));
              End;
              HighlightAddr := Addresses[NewHighlightAddr];
              If NewHighlightAddr = 0 Then
                 CPUViewPos := Max(0, CPUViewPos - ((FastIMG1.Bmp.AbsHeight Div 2) Div 8));
           End;
           CreateDisassembly;
        End;
     VK_PRIOR:
        Begin
           HighlightAddr := Addresses[0];
           CPUViewPos := Max(0, CPUViewPos - ((FastIMG1.Bmp.AbsHeight Div 2) Div 8));
           CreateDisassembly;
        End;
     VK_NEXT:
        Begin
           HighlightAddr := Addresses[MaxHighlight];
           CPUViewPos := Max(0, CPUViewPos + ((FastIMG1.Bmp.AbsHeight Div 2) Div 8));
           CreateDisassembly;
        End;
  End;
end;

Procedure TCPUWindow.UpdateRegs;
Begin
  SetDebugColourEdt(Edit2, Registers.PC <> OldRegs.PC);
  SetDebugColourEdt(Edit3, GetWord(@Registers.F) <> GetWord(@OldRegs.F));
  SetDebugColourEdt(Edit4, GetWord(@Registers.C) <> GetWord(@OldRegs.C));
  SetDebugColourEdt(Edit5, GetWord(@Registers.E) <> GetWord(@OldRegs.E));
  SetDebugColourEdt(Edit6, GetWord(@Registers.L) <> GetWord(@OldRegs.L));
  SetDebugColourEdt(Edit7, Registers.IX <> OldRegs.IX);
  SetDebugColourEdt(Edit8, Registers.IY <> OldRegs.IY);

  SetDebugColourEdt(Edit14, Registers.SP <> OldRegs.SP);
  SetDebugColourEdt(Edit10, GetWord(@Registers.Fn) <> GetWord(@OldRegs.Fn));
  SetDebugColourEdt(Edit11, GetWord(@Registers.Cn) <> GetWord(@OldRegs.Cn));
  SetDebugColourEdt(Edit12, GetWord(@Registers.En) <> GetWord(@OldRegs.En));
  SetDebugColourEdt(Edit13, GetWord(@Registers.Ln) <> GetWord(@OldRegs.Ln));
  SetDebugColourEdt(Edit9, GetWord(@Registers.R) <> GetWord(@OldRegs.R));
  SetDebugColourEdt(Edit15, Registers.IntMode <> OldRegs.IntMode);

  SetDebugColourCb(Label17, (Registers.F and 128) <> (OldRegs.F and 128));
  SetDebugColourCb(Label18, (Registers.F and 64) <> (OldRegs.F and 64));
  SetDebugColourCb(Label19, (Registers.F and 32) <> (OldRegs.F and 32));
  SetDebugColourCb(Label20, (Registers.F and 16) <> (OldRegs.F and 16));
  SetDebugColourCb(Label21, (Registers.F and 8) <> (OldRegs.F and 8));
  SetDebugColourCb(Label22, (Registers.F and 4) <> (OldRegs.F and 4));
  SetDebugColourCb(Label23, (Registers.F and 2) <> (OldRegs.F and 2));
  SetDebugColourCb(Label24, (Registers.F and 1) <> (OldRegs.F and 1));

  If Opt_AsmHexValues Then Begin
     Edit2.Text := '$'+FastIntToHex(Registers.PC);
     Edit3.Text := '$'+HexBytes[Registers.A]+HexBytes[Registers.F];
     Edit4.Text := '$'+HexBytes[Registers.B]+HexBytes[Registers.C];
     Edit5.Text := '$'+HexBytes[Registers.D]+HexBytes[Registers.E];
     Edit6.Text := '$'+HexBytes[Registers.H]+HexBytes[Registers.L];
     Edit7.Text := '$'+FastIntToHex(Registers.IX);
     Edit8.Text := '$'+FastIntToHex(Registers.IY);
     Edit14.Text := '$'+FastIntToHex(Registers.SP);
     Edit10.Text := '$'+HexBytes[Registers.An]+HexBytes[Registers.Fn];
     Edit11.Text := '$'+HexBytes[Registers.Bn]+HexBytes[Registers.Cn];
     Edit12.Text := '$'+HexBytes[Registers.Dn]+HexBytes[Registers.En];
     Edit13.Text := '$'+HexBytes[Registers.Hn]+HexBytes[Registers.Ln];
     Edit9.Text := '$'+HexBytes[Registers.I]+HexBytes[Registers.R];
     Edit15.Text := '$'+IntToStr(Registers.IntMode);
  End Else Begin
     Edit2.Text := IntToStr(Registers.PC);
     Edit3.Text := IntToStr((Registers.A Shl 8)+Registers.F);
     Edit4.Text := IntToStr((Registers.B Shl 8)+Registers.C);
     Edit5.Text := IntToStr((Registers.D Shl 8)+Registers.E);
     Edit6.Text := IntToStr((Registers.H Shl 8)+Registers.L);
     Edit7.Text := IntToStr(Registers.IX);
     Edit8.Text := IntToStr(Registers.IY);
     Edit14.Text := IntToStr(Registers.SP);
     Edit10.Text := IntToStr((Registers.An Shl 8)+Registers.Fn);
     Edit11.Text := IntToStr((Registers.Bn Shl 8)+Registers.Cn);
     Edit12.Text := IntToStr((Registers.Dn Shl 8)+Registers.En);
     Edit13.Text := IntToStr((Registers.Hn Shl 8)+Registers.Ln);
     Edit9.Text := IntToStr((Registers.I Shl 8)+Registers.R);
     Edit15.Text := IntToStr(Registers.IntMode);
  End;
  Init := True;
  CheckBox1.Checked := Registers.IntsEnabled;
  CheckBox2.Checked := Registers.F and 128 = 128;
  CheckBox3.Checked := Registers.F and 64 = 64;
  CheckBox4.Checked := Registers.F and 32 = 32;
  CheckBox5.Checked := Registers.F and 16 = 16;
  CheckBox6.Checked := Registers.F and 8 = 8;
  CheckBox7.Checked := Registers.F and 4 = 4;
  CheckBox8.Checked := Registers.F and 2 = 2;
  CheckBox9.Checked := Registers.F and 1 = 1;
  Init := False;

  Edit17.Text := 'Rom: ' + IntToStr(CurROM) + ' - Bank: ' + IntToStr(PagedBank); //Show page arda add
End;

Procedure TCPUWindow.SetDebugColourEdt(Var Control: TEdit; Debug: Boolean);
Begin
  If Debug Then Begin
     Control.Color := ClHighlight;
     Control.Font.Color := clHighlightText;
  End Else Begin
     Control.Color := ClWindow;
     Control.Font.Color := ClWindowText;
  End;
End;

Procedure TCPUWindow.SetDebugColourCb(Var Control: TLabel; Debug: Boolean);
Begin
  If Debug Then Begin
     Control.Font.Color := clBlue;
     Control.Font.Style := Control.Font.Style + [FsBold];
  End Else Begin
     Control.Font.Color := ClWindowText;
     Control.Font.Style := Control.Font.Style - [FsBold];
  End;
End;

procedure TCPUWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CPUShowing := False;
  FormDeactivate(Sender);
  CopyMemory(@OldRegs.PC, @Registers.PC, SizeOf(TZ80Registers));
  If CPURunning Then
     Registers.EmuRunning := True;
  StepOperation := False;
end;

procedure TCPUWindow.Edit2KeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
  // Update the registers if RETURN pressed
  If Key = VK_RETURN Then
     UpdateRegsValues;
end;

Procedure TCPUWindow.UpdateRegsValues;
Var
  Value: Integer;
Begin
  Value := GetValue(Edit2, 65535, 'Register PC');
  If Value = -1 Then Begin Edit2.SetFocus; Exit; End;
  Registers.PC := Value;

  Value := GetValue(Edit3, 65535, 'Register AF');
  If Value = -1 Then Begin Edit3.SetFocus; Exit; End;
  PutWord(@Registers.F, Value);

  Value := GetValue(Edit4, 65535, 'Register BC');
  If Value = -1 Then Begin Edit4.SetFocus; Exit; End;
  PutWord(@Registers.C, Value);

  Value := GetValue(Edit5, 65535, 'Register DE');
  If Value = -1 Then Begin Edit5.SetFocus; Exit; End;
  PutWord(@Registers.E, Value);

  Value := GetValue(Edit6, 65535, 'Register HL');
  If Value = -1 Then Begin Edit6.SetFocus; Exit; End;
  PutWord(@Registers.L, Value);

  Value := GetValue(Edit7, 65535, 'Register IX');
  If Value = -1 Then Begin Edit7.SetFocus; Exit; End;
  Registers.IX := Value;

  Value := GetValue(Edit8, 65535, 'Register IY');
  If Value = -1 Then Begin Edit8.SetFocus; Exit; End;
  Registers.IY := Value;

  Value := GetValue(Edit14, 65535, 'Register SP');
  If Value = -1 Then Begin Edit14.SetFocus; Exit; End;
  Registers.SP := Value;

  Value := GetValue(Edit10, 65535, 'Register AF'#39);
  If Value = -1 Then Begin Edit10.SetFocus; Exit; End;
  PutWord(@Registers.Fn, Value);

  Value := GetValue(Edit11, 65535, 'Register BC'#39);
  If Value = -1 Then Begin Edit11.SetFocus; Exit; End;
  PutWord(@Registers.Cn, Value);

  Value := GetValue(Edit12, 65535, 'Register DE'#39);
  If Value = -1 Then Begin Edit12.SetFocus; Exit; End;
  PutWord(@Registers.En, Value);

  Value := GetValue(Edit13, 65535, 'Register HL'#39);
  If Value = -1 Then Begin Edit13.SetFocus; Exit; End;
  PutWord(@Registers.Ln, Value);

  Value := GetValue(Edit9, 65535, 'Register IR');
  If Value = -1 Then Begin Edit9.SetFocus; Exit; End;
  PutWord(@Registers.R, Value);

  Value := GetValue(Edit15, 2, 'Interrupt Mode');
  If Value = -1 Then Begin Edit15.SetFocus; Exit; End;
  Registers.IntMode := Value;

  Registers.IntsEnabled := CheckBox1.Checked;

  CreateDisassembly;
  UpdateRegs;

End;

Function TCPUWindow.GetValue(Var EditBox: TEdit; MaxLimit: Integer; Desc: String): Integer;
Var
  Idx: Integer;
  ValueText, HexStr: String;
Begin

  Result := -1;

  HexStr := '';
  If MaxLimit > 9 Then
     if MaxLimit <= 255 Then
        HexStr := ' ($'+HexBytes[MaxLimit]+')'
     Else
        HexStr := ' ($'+FastIntToHex(MaxLimit)+')';

  ValueText := Uppercase(EditBox.Text);

  If ValueText = 'PC' Then Result := Registers.PC;
  If ValueText = 'AF' Then Result := GetWord(@Registers.F);
  If ValueText = 'BC' Then Result := GetWord(@Registers.C);
  If ValueText = 'DE' Then Result := GetWord(@Registers.E);
  If ValueText = 'HL' Then Result := GetWord(@Registers.L);
  If ValueText = 'IX' Then Result := Registers.IX;
  If ValueText = 'IY' Then Result := Registers.IY;
  If ValueText = 'SP' Then Result := Registers.SP;
  If ValueText = 'AF'+#39 Then Result := GetWord(@Registers.Fn);
  If ValueText = 'BC'+#39 Then Result := GetWord(@Registers.Cn);
  If ValueText = 'DE'+#39 Then Result := GetWord(@Registers.En);
  If ValueText = 'HL'+#39 Then Result := GetWord(@Registers.Ln);

  If Result = -1 Then
     For Idx := 0 To 65535 Do
        If UpperCase(Symbols[Idx].Desc) = ValueText Then Begin
           Result := Symbols[Idx].Address;
           Break;
        End;
  If Result = -1 Then
     Result := StrToIntDef(ValueText, -1);
  If (Result = -1) or (Result > MaxLimit) Then Begin
     MessageBox(Handle, pChar('Invalid value for '+Desc+'.'#13'Values may range from 0 to '+IntToStr(MaxLimit)+HexStr+'.'), pChar('Value Error'), MB_OK or MB_ICONWARNING);
     EditBox.Text := IntToStr(Result);
     Result := -1;
  End Else
     EditBox.Text := IntToStr(Result);
End;


procedure TCPUWindow.CheckBox1Click(Sender: TObject);
Var
  Value: Byte;
begin
  If Init Then Exit;
  If Sender <> CheckBox1 Then Begin
     Value := 0;
     If CheckBox2.Checked Then Value := Value + 128;
     If CheckBox3.Checked Then Value := Value + 64;
     If CheckBox4.Checked Then Value := Value + 32;
     If CheckBox5.Checked Then Value := Value + 16;
     If CheckBox6.Checked Then Value := Value + 8;
     If CheckBox7.Checked Then Value := Value + 4;
     If CheckBox8.Checked Then Value := Value + 2;
     If CheckBox9.Checked Then Value := Value + 1;
     Registers.F := Value;
     UpdateRegs;
     exit;
  End;
  UpdateRegsValues;
end;

procedure TCPUWindow.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
  Value: Integer;
begin
  If Key = VK_RETURN Then Begin
     Value := GetValue(Edit1, 65535, 'Display Address');
     If Value = -1 Then Begin
        Edit1.SetFocus;
        Exit;
     End;
     CPUViewPos := Max(0, Value - ((FastIMG1.Bmp.AbsHeight Div 8) Div 2));
     AddressHistory.Add(IntToStr(CurViewAddr));
     CurViewAddr := Value;
     HighlightAddr := Value;
     CreateDisassembly;
     Key := 0;
  End;
end;

procedure TCPUWindow.FastIMG1DblClick(Sender: TObject);
begin
  If CPUBreakPoints[HighlightAddr] = 0 Then
     CPUBreakpoints[HighlightAddr] := 1
  Else
     CPUBreakpoints[HighlightAddr] := 0;
  CreateDisassembly;
end;

procedure TCPUWindow.Button1Click(Sender: TObject);
Var
  TP: TPoint;
begin
  TP := ClientToScreen(Point(Button1.Left+(Button1.Width Div 2), Button1.Top+(Button1.Height Div 2)));
  PopupMenu1.Popup(TP.X, TP.Y);
end;

procedure TCPUWindow.PopupMenu1Popup(Sender: TObject);
begin
  HexValues1.Checked := Opt_AsmHexValues;
  DecimalValues1.Checked := Not Opt_AsmHexValues;
  ASCIIBytes1.Checked := Opt_AsmASCIIBytes;
end;

procedure TCPUWindow.HexValues1Click(Sender: TObject);
begin
  Opt_AsmHexValues := True;
  CreateDisassembly;
  UpdateRegs;
end;

procedure TCPUWindow.DecimalValues1Click(Sender: TObject);
begin
  Opt_AsmHexValues := False;
  CreateDisassembly;
  UpdateRegs;
end;

procedure TCPUWindow.ASCIIBytes1Click(Sender: TObject);
begin
  Opt_AsmASCIIBytes := Not Opt_AsmASCIIBytes;
  CreateDisassembly;
  UpdateRegs;
end;

procedure TCPUWindow.CheckBox2KeyPress(Sender: TObject; var Key: Char);
begin
  UpdateRegsValues;
end;

procedure TCPUWindow.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
Begin
  Inc(CPUViewPos, 10);
  CreateDisassembly;
  Handled := True;
end;

procedure TCPUWindow.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  If CPUViewPos > 0 Then Begin
     CPUViewPos := Max(CPUViewPos -10, 0);
     CreateDisassembly;
     Handled := True;
  End;
end;

procedure TCPUWindow.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  CPUViewPos := ScrollPos;
  CreateDisassembly;
end;

procedure TCPUWindow.Button5Click(Sender: TObject);
begin
  If Button5.Caption = 'Run' Then Begin
     If Not Registers.EmuRunning Then
        If CPURunning Then
           Registers.EmuRunning := True;
     StepOperation := False;
     Button2.Enabled := False;
     Button3.Enabled := False;
     Button4.Enabled := False;
     Button6.Enabled := False;
  End Else Begin
     StepOperation := True;
     Button2.Enabled := True;
     Button3.Enabled := True;
     Button4.Enabled := True;
     Button6.Enabled := True;
  End;

  CopyMemory(@OldRegs.PC, @Registers.PC, SizeOf(TZ80Registers));

  If CPURunning Then Begin
     If Not StepOperation Then Begin
        Button5.Caption := 'Stop';
        Button5.Enabled := True;
     End Else Begin
        Button5.Caption := 'Run';
        Button5.Enabled := Registers.EmuRunning;
     End;
  End Else Begin
     Button5.Caption := 'Run';
     Button5.Enabled := Registers.EmuRunning;
  End;

end;

procedure TCPUWindow.Button4Click(Sender: TObject);
begin
  If CPURunning Then Begin
     CPUBreakPoints[HighlightAddr] := CPUBreakPoints[HighlightAddr] Or 2;
     CopyMemory(@OldRegs.PC, @Registers.PC, SizeOf(TZ80Registers));
     Registers.EmuRunning := True;
  End;
end;

procedure TCPUWindow.Button3Click(Sender: TObject);
Var
  Idx: Integer;
begin
  Idx := HighlightAt +1;
  While Addresses[Idx] = -1 Do
     Inc(Idx);
  CPUBreakpoints[Addresses[Idx]] := CPUBreakPoints[Addresses[Idx]] Or 2;
  CopyMemory(@OldRegs.PC, @Registers.PC, SizeOf(TZ80Registers));
  Registers.EmuRunning := True;
  StepOperation := False;
  Button2.Enabled := False;
  Button3.Enabled := False;
  Button4.Enabled := False;
  Button6.Enabled := False;
  If CPURunning Then Begin
     If Not StepOperation Then Begin
        Button5.Caption := 'Stop';
        Button5.Enabled := True;
     End Else Begin
        Button5.Caption := 'Run';
        Button5.Enabled := Registers.EmuRunning;
     End;
  End Else Begin
     Button5.Caption := 'Run';
     Button5.Enabled := Registers.EmuRunning;
  End;
end;

procedure TCPUWindow.Button2Click(Sender: TObject);
begin
  StepOperation := True;
  Registers.HaltEmu := False;
  CopyMemory(@OldRegs.PC, @Registers.PC, SizeOf(TZ80Registers));
  ExecuteEmulationLoop_SpectrumSpeed;
  UpdateDisplay;
  UpdateBASinDisplay;
  FormShow(Nil);
end;

procedure TCPUWindow.Button6Click(Sender: TObject);
begin
  CopyMemory(@OldRegs.PC, @Registers.PC, SizeOf(TZ80Registers));
  ExitProcOperation := True;
  CallCounter := 1;
  StepOperation := False;
  Button2.Enabled := False;
  Button3.Enabled := False;
  Button4.Enabled := False;
  Button6.Enabled := False;
  If CPURunning Then Begin
     If Not StepOperation Then Begin
        Button5.Caption := 'Stop';
        Button5.Enabled := True;
     End Else Begin
        Button5.Caption := 'Run';
        Button5.Enabled := Registers.EmuRunning;
     End;
  End Else Begin
     Button5.Caption := 'Run';
     Button5.Enabled := Registers.EmuRunning;
  End;
end;

procedure TCPUWindow.Label3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  If LabelIn <> nil Then Begin
     (LabelIn as TLabel).Font.Color := ClWindowText;
     (LabelIn as TLabel).Font.Style := (LabelIn as TLabel).Font.Style + [FsBold];
  End;
  LabelIn := Sender As TLabel;
  (Sender As TLabel).Font.Color := ClBlue;
  Screen.Cursor := crHandPoint;
end;

procedure TCPUWindow.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FormDeactivate(Sender);
end;

procedure TCPUWindow.FormDeactivate(Sender: TObject);
begin
  If LabelIn <> nil Then Begin
     (LabelIn as TLabel).Font.Color := ClWindowText;
     (LabelIn as TLabel).Font.Style := (LabelIn as TLabel).Font.Style - [FsBold];
  End;
  LabelIn := nil;
  Screen.Cursor := crDefault;
end;

procedure TCPUWindow.FormActivate(Sender: TObject);
begin
  FormDeactivate(Sender);
end;

procedure TCPUWindow.Label3Click(Sender: TObject);
Var
  Addr: Integer;
begin
  Addr := 65536;
  Case (Sender As TLabel).Tag of
     1: Addr := Registers.PC;
     2: Addr := GetWord(@Registers.F);
     3: Addr := GetWord(@Registers.C);
     4: Addr := GetWord(@Registers.E);
     5: Addr := GetWord(@Registers.L);
     6: Addr := Registers.IX;
     7: Addr := Registers.IY;
     8: Addr := Registers.SP;
     9: Addr := GetWord(@Registers.Fn);
    10: Addr := GetWord(@Registers.Cn);
    11: Addr := GetWord(@Registers.En);
    12: Addr := GetWord(@Registers.Ln);
  End;
  If Addr <> 65536 Then Begin
     CPUViewPos := Max(0, Addr - ((FastIMG1.Bmp.AbsHeight Div 8) Div 2));
     AddressHistory.Add(IntToStr(CurViewAddr));
     CurViewAddr := Addr;
     HighlightAddr := Addr;
     CreateDisassembly;
  End;
end;

procedure TCPUWindow.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  YPos, XPos: Integer;
begin
  YPos := Y Div 8;
  JumpAddr := -1;
  If Addresses[YPos] <> -1 Then Begin
     XPos := 48 + Jumps[YPos].StartChar*8;
     If (X >= XPos) and (X < XPos + (Jumps[YPos].Len * 8)) Then Begin
        Screen.Cursor := CrHandpoint;
        JumpAddr := Jumps[YPos].Address;
     End Else
        Screen.Cursor := CrDefault;
  End Else
     Screen.Cursor := CrDefault;
end;

procedure TCPUWindow.Button7Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_cpu.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TCPUWindow.FormDestroy(Sender: TObject);
begin

  AddressHistory.Free;

end;

procedure TCPUWindow.Button8Click(Sender: TObject);
Var
  Addr: Integer;
begin

  If AddressHistory.Count > 0 Then Begin

     Addr := StrToInt(AddressHistory[AddressHistory.Count -1]);
     AddressHistory.Delete(AddressHistory.Count -1);
     CPUViewPos := Max(0, Addr - ((FastIMG1.Bmp.AbsHeight Div 8) Div 2));
     CurViewAddr := Addr;
     HighlightAddr := Addr;
     CreateDisassembly;

  End;

end;



end.
