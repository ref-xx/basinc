unit Filing;

interface

Uses Windows, Classes, SysUtils, CommDlg, Dialogs, Controls, FastFiles;

Type

  TBASICFileTypes   = (FTSession, FTBas, FTSnap, FTBsc, FTBsd, FTScr, FTTape, FTBmp, FTGif, FTPng, FTJPeg, FTLoadPics, FTSavePics, FTBin,
                       FTSpecCHR, FTExec, FTAssembly, FTSelection, FTTap, FTTZX, FTSna, FTZ80, FTAll);
  TBASICFiles       = Set Of TBASICFileTypes;

	// Filing and disk access procs

	Procedure DebugLog(Text: AnsiString);
  Function  GetFile(DefExt: AnsiString): AnsiString;
  Function  OpenFile(hWnd: Integer; Caption: PChar; Types: TBASICFiles; CurFile: AnsiString; Save, Multifile: Boolean): AnsiString;
  Procedure PrepareBlankFileHeader;
  Procedure LoadDATANum;
  Procedure LoadDATAStr;
  Procedure LoadCode;
  Procedure LoadProgram;

  Function  OpenFileStream(var FileStream: TFileStream; StreamOptions: Word; Filename: AnsiString): Boolean;
  Procedure LoadSna;
  Procedure DecodeSNA;
  Procedure ConvertZ80;

  Function  SaveFile: Boolean;
  Function  SaveProgram: Boolean;
  Procedure SaveCode;
  Procedure SaveSna(Filename: AnsiString);
  Procedure Savez80(Filename: AnsiString);
  Procedure SaveDATANum;
  Procedure SaveDATAStr;

  Function  SaveCurrentProgram(NewFilename: AnsiString): Boolean;
  Function  CheckForSave: Boolean;
  Function  Get128kOptions: Boolean;

Var

  DumpDoneQuit:     Boolean;
  FileName,
  FileHeader,
  FileBody:         AnsiString;
  FileArray:        Array of Byte;
  FileHeaderLoc:    Word;

Const

  Snap128: Array [0..7195] of Byte =
     (0, 255, 255, 7, 0, 26, 10, 68, 0, 59, 92, 111, 47, 0, 1, 58, 92, 108, 253, 4, 181, 92, 29, 251, 91, 1, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 254, 1, 254, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 24, 60, 60, 0, 0, 0, 0, 0, 3, 252, 3, 252, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40, 66, 66, 0, 0, 0, 0, 0, 7, 248, 7, 248, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 2, 60, 0, 0, 0, 0, 0, 15, 240, 15, 240, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 60, 66, 0, 0, 0, 0, 0, 31, 224, 31, 224, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 64, 66, 0, 0, 0, 0, 0, 63, 192, 63, 192, 63, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 126, 60, 0, 0, 0, 0, 0, 127,
      128, 127, 128, 127, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 255, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 254, 0, 0, 0, 0, 64, 0, 0, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 24, 60, 60,
      0, 124, 60, 60, 62, 60, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 60, 0, 16, 0, 0, 16, 0, 16, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 8, 60, 0, 124, 60, 60, 62, 60, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 254, 0, 0, 0, 0, 254, 0, 0, 16, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 56, 120, 56, 0, 64, 56, 56, 4, 56, 28, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 40, 66, 66, 0, 66, 66, 64, 8, 66, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 66, 56, 16, 28, 68, 16, 56, 56, 56, 28, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 128, 24, 66, 0, 66, 66, 64, 8, 66, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 56, 120, 56, 0, 16, 56, 56, 56, 56, 28, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 128, 16, 4, 68, 68, 0, 64, 68, 4, 60, 68, 32, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 8, 2, 60, 0, 124, 66, 60, 8, 64, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 64, 4, 16, 32, 68,
      16, 4, 16, 68, 32, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 40, 60, 0, 124, 66, 60, 8, 64, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 4, 68, 68, 0, 16, 68, 64, 16, 68, 32, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 60, 68, 120, 0, 64, 68, 60, 68, 120, 32, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 8, 60, 66, 0, 66, 126, 2, 8, 64, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 64, 60, 16, 32, 68, 16, 60, 16, 68, 32, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 72, 66, 0, 66, 126, 2, 8, 64, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 128, 16, 60, 68, 120, 0, 16, 120, 56, 16, 120, 32, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 68, 120, 64, 0, 64, 68, 68, 68, 64, 32, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 128, 8, 64, 66, 0, 66, 66, 66, 8, 66, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 66, 68, 16, 32, 68, 16, 68, 16, 68, 32, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 126, 66, 0, 66,
      66, 66, 8, 66, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 68, 120, 64, 0, 16, 64, 4, 16, 64, 32, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 60, 64, 60,
      0, 126, 56, 60, 60, 60, 32, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 62, 126, 60, 0, 124, 66, 60, 62, 60, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 60, 60, 12, 28, 56, 12, 60, 12, 56, 32, 0, 0, 1,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 8, 60, 0, 124, 66, 60, 62, 60, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 16, 60, 64, 60, 0, 16, 60, 120, 12, 60, 32, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 66, 0, 24, 60, 60, 60, 0, 60, 16, 0, 0, 16, 0, 16, 0, 0, 124, 0, 0, 0, 0, 0, 0, 64, 0, 64, 16, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 153, 0, 40, 66, 66, 64, 0, 64, 0, 120, 28, 16, 56, 0, 28, 0, 66, 56, 56, 56, 56, 28, 28, 64, 0, 64, 56, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 161, 0, 8, 66, 60, 124, 0, 60, 48, 68, 32, 16, 4, 48, 32, 0, 66, 68, 64, 68, 4, 32, 32, 120, 0, 64, 16, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 161, 0, 8, 62, 66, 66, 0, 2, 16, 68, 32, 16, 60, 16, 32, 0, 124, 120, 56, 120, 60, 32, 32,
      68, 0, 64, 16, 68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 153, 0, 8, 2, 66, 66, 0, 66, 16, 68, 32, 16,
      68, 16, 32, 0, 68, 64, 4, 64, 68, 32, 32, 68, 0, 64, 16, 68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 66,
      0, 62, 60, 60, 60, 0, 60, 56, 68, 28, 12, 60, 56, 32, 0, 66, 60, 120, 60, 60, 32, 28, 68, 0, 126, 12, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 71, 71, 71, 71, 71, 71, 71, 71, 66, 114, 116, 108, 104, 64, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
      104, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 120, 120, 120, 120, 120, 120,
      120, 120, 120, 120, 120, 120, 120, 120, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 245, 197, 1, 253, 127, 58, 92, 91, 238, 16, 243, 50, 92, 91, 237, 121, 251, 193, 241, 201, 205, 0, 91, 229, 42, 90, 91, 227, 201, 243, 58, 92, 91, 230, 239, 50, 92, 91, 1, 253, 127, 237, 121, 251, 195, 195, 0, 33, 216, 6,
      24, 3, 33, 202, 7, 8, 1, 253, 127, 58, 92, 91, 245, 230, 239, 243, 50, 92, 91, 237, 121, 195, 230, 5, 8, 241, 1, 253, 127, 243, 50, 92, 91, 237, 121, 251, 8, 201, 233, 34, 34, 55, 7, 207, 0, 11, 0, 0, 0, 0, 80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 88, 255, 236, 235, 236, 43, 1, 1, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 10, 0, 1, 3, 7, 15, 31, 63, 127, 255, 254, 252, 248, 240, 224, 192, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 69, 57, 163, 57, 219, 2, 124, 56, 69, 57, 163, 57, 219, 2, 124, 56, 69, 57, 163, 57, 219, 2, 124, 56, 108, 253, 77, 0, 111, 47, 0, 1, 56, 0, 0, 1, 92, 29, 72, 0, 59, 92, 131, 54, 102, 91,
      92, 38, 0, 0);

implementation

Uses BASinMain, Display, QueryForm, FastCore, ROMUtils, BASSupport, Utility, Tapes, Sound;

Procedure DebugLog(Text: AnsiString);
Var
	F: TFileStream;
  TempText: AnsiString;
Begin
	Text := TimeToStr(Now) + ' ' + Text + #13#10;
  If BASinDIR = '' Then GetBASinDIR;
	If Not FileExists(BASinDir+'\basinC.log') Then Begin
     TempText := 'BasinC started at ' + DateTimeToStr(Now) +#13#10;
		F := TFileStream.Create(BASinDir+'\basinC.log', fmCreate or fmShareDenyNone);
     F.Seek(0, soFromEnd);
     F.Write(TempText[1], Length(TempText));
	End Else Begin
     F := TFileStream.Create(BASinDir+'\basinC.log', fmOpenWrite or fmShareDenyNone);
  End;
	F.Seek(0, soFromEnd);
	F.Write(Text[1], Length(Text));
	F.Free;
End;

Procedure AddFilter(Var AllFilter, ExpFilters: AnsiString; Filter: AnsiString);
Begin

  If Pos(Filter, AllFilter) = 0 Then Begin
     AllFilter := AllFilter + Filter+';';
     If ExpFilters <> '' Then
        ExpFilters := ExpFilters + ', '+Filter
     Else
        ExpFilters := ExpFilters + Filter;
  End;

End;

Function OpenFile(hWnd: Integer; Caption: PChar; Types: TBASICFiles; CurFile: AnsiString; Save, MultiFile: Boolean): AnsiString;
var
  ofn: TOpenFileName;
  szFile: array[0..65534] of Char;
  Directory, FileN: AnsiString;
  Dir, Title, Filter: PChar;
  TempFilter, AllFilter, ExpFilters: AnsiString;
  NumFilters, Idx, MsgVal: Integer;
Begin

  DisplayWindow.WantsFront := False;

  Title := Caption;
  TempFilter := '';
  AllFilter := '';
  ExpFilters := '';
  NumFilters := 0;

  Directory := ExtractFilePath(CurFile);
  If Directory = '' Then Dir := '.' Else Dir := PChar(Directory);
  FileN := ExtractFilename(CurFile);
  CopyMemory(@FileN[1], @szFile[0], Length(FileN));

  // Setup the filters based on what we want to load
  // Note that including FTAll in the list removes the
  // "All Supported Types" option. Having only one filter will
  // also display the "All Supported Types" too.

  If FTBas in Types Then Begin
     TempFilter := TempFilter + 'Basinc Program Files (*.bas)'#0'*.bas'#0;
     ExpFilters := ExpFilters + '*.bas';
     AllFilter := AllFilter +'*.bas;';
     Inc(Numfilters);
  End;

  If FTSession in Types Then Begin
     TempFilter := TempFilter + 'Basinc Session Snapshot (*.bcs)'#0'*.bcs'#0;
     ExpFilters := ExpFilters + '*.bcs';
     AllFilter := AllFilter +'*.bcs;';
     Inc(Numfilters);
  End;

  If FTSnap in Types Then Begin
     TempFilter := TempFilter + 'Spectrum Snapshots (*.sna, *.z80)'#0'*.sna;*.z80'#0;
     AddFilter(AllFilter, ExpFilters, '*.sna');
     AddFilter(AllFilter, ExpFilters, '*.z80');
     Inc(Numfilters);
     If Not (FTSna in Types) Then Types := Types + [FTSna];
     If Not (FTZ80 in Types) Then Types := Types + [FTZ80];
  End;
  If FTLoadPics in Types Then Begin
     TempFilter := TempFilter + 'Image files (*.bmp, *.jpg, *.jpeg, *.gif; *.png)'#0'*.bmp;*.jpg;*.jpeg;*.gif;*.png'#0;
     AddFilter(AllFilter, ExpFilters, '*.bmp');
     AddFilter(AllFilter, ExpFilters, '*.jpg');
     AddFilter(AllFilter, ExpFilters, '*.jpeg');
     AddFilter(AllFilter, ExpFilters, '*.gif');
     AddFilter(AllFilter, ExpFilters, '*.png');
     Inc(Numfilters);
     If Not (FTBmp in Types) Then Types := Types + [FTBmp];
     If Not (FTJpeg in Types) Then Types := Types + [FTJPEG];
     If Not (FTGIF in Types) Then Types := Types + [FTGIF];
     If Not (FTPNG in Types) Then Types := Types + [FTPNG];
  End;
  If FTSavePics in Types Then Begin
     TempFilter := TempFilter + 'Image files (*.bmp, *.gif)'#0'*.bmp;*.gif'#0;
     AddFilter(AllFilter, ExpFilters, '*.bmp');
     AddFilter(AllFilter, ExpFilters, '*.gif');
     Inc(Numfilters);
     If Not (FTBmp in Types) Then Types := Types + [FTBmp];
     If Not (FTGif in Types) Then Types := Types + [FTGif];
  End;
  If FTTape in Types Then Begin
     TempFilter := TempFilter + 'Spectrum Tape Images (*.tap, *.tzx)'#0'*.tap;*.tzx'#0;
     AddFilter(AllFilter, ExpFilters, '*.tap');
     AddFilter(AllFilter, ExpFilters, '*.tzx');
     Inc(Numfilters);
     If Not (FTTap in Types) Then Types := Types + [FTTap];
     If Not (FTTzx in Types) Then Types := Types + [FTTzx];
  End;
  If FTTap  in Types Then Begin
     TempFilter := TempFilter + 'Simple tape image (*.tap)'#0'*.tap'#0;
     AddFilter(AllFilter, ExpFilters, '*.tap');
     Inc(Numfilters);
  End;
  If FTTZX in Types Then Begin
     TempFilter := TempFilter + 'Enhanced Tape Image File (*.tzx)'#0'*.tzx'#0;
     AddFilter(AllFilter, ExpFilters, '*.tzx');
     Inc(Numfilters);
  End;
  If FTSna  in Types Then Begin
     TempFilter := TempFilter + 'Spectrum Snapshot (*.sna)'#0'*.sna'#0;
     AddFilter(AllFilter, ExpFilters, '*.sna');
     Inc(Numfilters);
  End;
  If FTZ80  in Types Then Begin
     TempFilter := TempFilter + 'Z80 Snapshot (*.z80)'#0'*.z80'#0;
     AddFilter(AllFilter, ExpFilters, '*.z80');
     Inc(Numfilters);
  End;
  If FTPng  in Types Then Begin
     TempFilter := TempFilter + 'PNG Image Files (*.png)'#0'*.png'#0;
     AddFilter(AllFilter, ExpFilters, '*.png');
     Inc(Numfilters);
  End;
  If FTJpeg  in Types Then Begin
     TempFilter := TempFilter + 'JPEG Image (*.jpeg, *.jpg)'#0'*.jpeg;*.jpg'#0;
     AddFilter(AllFilter, ExpFilters, '*.jpeg');
     AddFilter(AllFilter, ExpFilters, '*.jpg');
     Inc(Numfilters);
  End;
  If FTBin in Types Then Begin
     TempFilter := TempFilter + 'Binary Files (*.bin)'#0'*.bin'#0;
     AddFilter(AllFilter, ExpFilters, '*.bin');
     Inc(Numfilters);
  End;
  If FTGif in Types Then Begin
     TempFilter := TempFilter + 'Compuserv GIF images (*.gif)'#0'*.gif'#0;
     AddFilter(AllFilter, ExpFilters, '*.gif');
     Inc(Numfilters);
  End;
  If FTSpecCHR in Types Then Begin
     TempFilter := TempFilter + 'Spectrum Font Files (*.specchr)'#0'*.specchr'#0;
     AddFilter(AllFilter, ExpFilters, '*.specchr');
     Inc(Numfilters);
  End;
  If FTAssembly in Types Then Begin
     TempFilter := TempFilter + 'Assembly Language Files (*.asm)'#0'*.asm'#0;
     AddFilter(AllFilter, ExpFilters, '*.asm');
     Inc(Numfilters);
  End;

  If FTExec in Types Then Begin
     TempFilter := TempFilter + 'Executable Program Files (*.exe)'#0'*.exe'#0;
     AddFilter(AllFilter, ExpFilters, '*.exe');
     Inc(Numfilters);
  End;

  If FTBsc  in Types Then Begin
     TempFilter := TempFilter + 'Basinc CODE Files (*.bsc)'#0'*.bsc'#0;
     AddFilter(AllFilter, ExpFilters, '*.bsc');
     Inc(Numfilters);
  End;
  If FTBsd  in Types Then Begin
     TempFilter := TempFilter + 'Basinc variable DATA Files (*.bsd)'#0'*.bsd'#0;
     AddFilter(AllFilter, ExpFilters, '*.bsd');
     Inc(Numfilters);
  End;
  If FTScr  in Types Then Begin
     TempFilter := TempFilter + 'Spectrum Screen Dumps (*.scr)'#0'*.scr'#0;
     AddFilter(AllFilter, ExpFilters, '*.scr');
     Inc(Numfilters);
  End;
  If FTBmp in Types Then Begin
     TempFilter := TempFilter + 'Windows Bitmap Files (*.bmp)'#0'*.bmp'#0;
     AddFilter(AllFilter, ExpFilters, '*.bmp');
     Inc(Numfilters);
  End;
  If FTSelection in Types Then Begin
     TempFilter := TempFilter + 'BasinC SCREEN$ Selection (*.bsl)'#0'*.bsl'#0;
     AddFilter(AllFilter, ExpFilters, '*.bsl');
     Inc(Numfilters);
  End;
  If ExpFilters <> '' Then
     If NumFilters > 1 Then Begin
        AllFilter := 'All Supported Types ('+ExpFilters+')'#0+AllFilter;
        TempFilter := AllFilter+#0+TempFilter;
     End;

  If FTAll in Types Then
     TempFilter := TempFilter + 'All Files (*.*)'#0'*.*'#0;

  // Set up the TOpenFilename parameters.
  // We use this, rather than the Borland TOpenDialog
  // So that we can use the enhanced Dialogs of 2k/XP.

  Filter := PChar(TempFilter);
  FillChar(ofn, SizeOf(TOpenFileName), 0);

  ofn.lStructSize     := SizeOf(TOpenFileName);
  ofn.hwndOwner       := Hwnd;
  ofn.lpstrFile       := szFile;
  ofn.nMaxFile        := SizeOf(szFile);
  ofn.lpstrTitle      := Title;
  ofn.lpstrInitialDir := Dir;
  ofn.lpstrFilter     := Filter;

  If Not Save Then
     ofn.Flags        := 4096
  Else
     ofn.Flags        := 6150 - OFN_OVERWRITEPROMPT;

  If MultiFile Then
     ofn.Flags := Ofn.Flags or OFN_ALLOWMULTISELECT or OFN_EXPLORER;

  StrPCopy(ofn.lpstrFile, Result);

  If Not Save Then Begin
     If GetOpenFileName(ofn) then Begin
        If MultiFile Then Begin
           Result := '';
           Idx := 0;
           While Ord(szFile[Idx])+Ord(szFile[Idx+1]) <> 0 Do Begin
              Result := Result + szFile[Idx];
              Inc(Idx);
           End;
           If Pos(#0, Result) > 0 Then
              Result := Result + #0;
        End Else
           Result := StrPas(szFile);
     End;
  End Else Begin
     If GetSaveFileName(ofn) then Begin
        Result := StrPas(szFile);
        If ExtractFileExt(Result) = '' Then Begin
           // No extension found - use the first in the filter list pointed to by
           //ofn.nFilterIndex.
           For Idx := 1 To (ofn.nFilterIndex * 2) -1 Do
              TempFilter := Copy(TempFilter, Pos(#0, TempFilter) +1, Length(TempFilter));
           TempFilter := Copy(TempFilter, Pos('.', TempFilter), Length(TempFilter));
           If Pos(';', TempFilter) < Pos(#0, TempFilter) Then
              Result := Result + Copy(TempFilter, 1, Pos(';', TempFilter) -1)
           Else
              Result := Result + Copy(TempFilter, 1, Pos(#0, TempFilter) -1);
        End;
        If FileExists(Result) Then Begin
           MsgVal := MessageBox(hWnd,
                                PChar('File '+ExtractFilename(Result)+' already exists.'+#13+'Are you sure you want to overwrite it?'),
                                PChar('Overwrite file?'),
                                MB_YESNO or MB_ICONQUESTION or MB_APPLMODAL or MB_SETFOREGROUND or MB_TOPMOST);
           If MsgVal = IDYES Then
              Exit
           Else
              Result := '';
        End;
     End;
  End;

End;

Function GetFile(DefExt: AnsiString): AnsiString;
Var
  OpenFileStream: TFileStream;
Begin
  If Not FileExists(Filename) Then FileName := Filename + DefExt;
  If FileExists(Filename) Then Begin
     OpenFileStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
     SetLength(FileArray, OpenFileStream.Size);
     OpenFileStream.Read(FileArray[0], OpenFileStream.Size);
     OpenFileStream.Free;
     Result := 'Ok';
  End Else Begin
     Result := 'File not found';
     DoError($1A, Result);
  End;
End;

Procedure LoadDATANum;
Var
  F: Integer;
  DataLen: Word;
Begin

  // Load a numeric variable.

  If Filename = '' Then Begin
     Filename := OpenFile(DisplayHandle, 'Load Numeric Variable', [FTBsd, FTTape], '', False, False);
     If Filename = '' Then Begin
        DoError($1A, 'DATA Load cancelled');
        Exit;
     End;
  End;

  If (Lowercase(ExtractFileExt(Filename)) = '.tzx') or (Lowercase(ExtractFileExt(Filename)) = '.tap') then Begin

     TapeWindow.FromFile1Click(Nil);
     TapeTrapLOAD := True;

  End Else Begin

     If GetFile('.bsd') <> 'Ok' Then Exit;

     If FileArray[0] <> 1 Then Begin
        DoError($1A, 'Wrong Var Type');
        Exit;
     End;

     FileHeader := '';
     For F := 0 To 16 Do
        FileHeader := FileHeader + AnsiChar(FileArray[F]);

     DataLen := GetWord(@FileArray[$0B]);
     SetLength(FileBody, DataLen);
     CopyMemory(@FileBody[1], @FileArray[17], DataLen);

     SetLength(FileArray, 0);

  End;

End;

Procedure LoadDATAStr;
Var
  F: Integer;
  DataLen: Word;
Begin

  // Load a AnsiString variable.

  If Filename = '' Then Begin
     Filename := OpenFile(DisplayHandle, 'Load String Variable', [FTBsd], '', False, False);
     If Filename = '' Then Begin
        DoError($1A, 'DATA Load cancelled');
        Exit;
     End;
  End;

  If (Lowercase(ExtractFileExt(Filename)) = '.tzx') or (Lowercase(ExtractFileExt(Filename)) = '.tap') then Begin

     TapeWindow.FromFile1Click(Nil);
     TapeTrapLOAD := True;

  End Else Begin

     If GetFile('.bsd') <> 'Ok' Then Exit;

     If FileArray[0] <> 2 Then Begin //was 1.. 2 for string mybe?
        DoError($1A, 'Wrong Var Type');
        Exit;
     End;

     FileHeader := '';
     For F := 0 To 16 Do
        FileHeader := FileHeader + AnsiChar(FileArray[F]);

     DataLen := GetWord(@FileArray[$0B]);
     SetLength(FileBody, DataLen);
     CopyMemory(@FileBody[1], @FileArray[17], DataLen);

     SetLength(FileArray, 0);

  End;

End;

Procedure LoadCode;
Var
  LPos, Offset, TempIX, CodeAddress,
  CodeLength, LoadLength: Word;
  Extension: AnsiString;
  FTypes: TBASICFiles;
Begin

  // This is a bit different. The user can optionally specify the address,
  // and if they do, they can also optionally specify the length to load.
  // The file may need trimming to fit if they have specified a length.

  // Again, a stacked IX points to the header to be expected.

  TempIX := Registers.IX -$11;
  CodeAddress := GetWord(@Memory[TempIX+$0D]);
  CodeLength := GetWord(@Memory[TempIX+$0B]);

  // If the user did not specify an address to load to, then
  // we can't load anything that has no info about where in memory
  // it came from. Hence, only .bsc files will be allowed in that instance.

  If CodeAddress = 0 Then FTypes := [FTBsc, FTTape] Else FTypes := [FTBsc, FTScr, FTTape, FTAll];


  If Filename = '' Then Begin
     Filename := OpenFile(BASinOutput.Handle, 'Load Code Block', FTypes, '', False, False);
     If Filename = '' Then Begin
        DoError($1A, 'CODE Load cancelled');
        Exit;
     End;
  End;

  If (Lowercase(ExtractFileExt(Filename)) = '.tzx') or (Lowercase(ExtractFileExt(Filename)) = '.tap') then Begin

     TapeWindow.FromFile1Click(Nil);
     TapeTrapLOAD := True;

  End Else Begin


     If GetFile('.bsc') <> 'Ok' Then Exit;



     Extension := Lowercase(ExtractFileExt(Filename)); //Lowercase(Copy(Filename, Length(Filename)-3, 4));

     PrepareBlankFileHeader;

     If Extension = '.bsc' Then Begin

        // .bsc files are saved by BASin with SAVE "" CODE x, y
        // And have a 17Byte header of their own.

        LoadLength := GetWord(@FileArray[$0B]);

        // The Address can go in as it is, as the ROM will use the user
        // specified address for loading, but we can muck about with the length.
        // If the user length is non-zero, and less than the file length, we can
        // just load that much :)

        If CodeAddress = 0 Then CodeAddress := GetWord(@FileArray[$D]);
        PutWord(@FileHeader[$0D], CodeAddress);

        If CodeLength <> 0 Then Begin
           If CodeLength > LoadLength Then Begin
              // Cannot load more than there is, so error
              // if the file is too small.
              DoError($1A, 'File too small');
              Exit;
           End;
        End Else Begin
           CodeLength := LoadLength;
        End;

        Offset := 17;

     End Else Begin

        // Now we get to the fun part - loading any file as raw binary data
        // which will require an address at least.

        If CodeAddress = 0 Then Begin
           // We *must* have an address to load to, unless we're loading
           // a .scr file.
           If Extension <> '.scr' Then Begin
              DoError($1A, 'Address not specified');
              Exit;
           End Else Begin
              CodeAddress := 16384;
              CodeLength := 6912;
           End;
        End;

        PutWord(@FileHeader[$0D], CodeAddress);

        If CodeLength > Length(FileArray) Then Begin
           // Cannot load more than there is, so error
           // if the file is too small.
           DoError($1A, 'File too small');
           Exit;
        End Else If CodeLength = 0 Then Begin
           CodeLength := Length(FileArray);
        End;

        Offset := 0;

     End;

     // Adjust codelength so that it won't overrun the memory

     If CodeAddress + CodeLength -1 > 65535 Then
        CodeLength := 65535 - CodeAddress;

     PutWord(@FileHeader[$0B], CodeLength);

     // Now build a filebody from the file.

     FileBody := '';
     For LPos := Offset to CodeLength +Offset -1 Do
        FileBody := FileBody + AnsiChar(FileArray[LPos]);

     // All done, now go back to the LOAD trap.

     FileHeader := #3+FileHeader;

     SetLength(FileArray, 0);

  End;

End;



Procedure LoadProgram;
Var
  Extension, Directory: AnsiString;
  MemPtr: Word;

Begin

  If Filename = '' Then Begin
     Filename := OpenFile(BASinOutput.Handle, 'Load BASIC Program', [FTSession, FTBas, FTSnap, FTTape], SessionProjectFileName, False, False);
     If Filename = '' Then Begin
        DoError($1A, 'PROG Load cancelled');
        Exit;
     End;
  End;

  // Builds a PROGRAM HEADER and a BODY based on the selected
  // Filename. Because of ROM restrictions, only the first
  // ten characters of the filename will be matched. No matter,
  // This routine knows which file to load, even if the ROM doesn't :)

  // First test the extension, as it's the best
  // idea we have of what the file *is*.

  If (Lowercase(ExtractFileExt(Filename)) = '.tzx') or (Lowercase(ExtractFileExt(Filename)) = '.tap') then Begin

     TapeWindow.FromFile1Click(Nil);
     TapeTrapLOAD := True;

  End Else Begin

     If GetFile('.bas') <> 'Ok' Then Exit;

     Extension := Lowercase(ExtractFileExt(Filename));
     Directory := ExtractFilePath(Filename);
     If Directory <> '' Then
        ChDir(Directory);

     PrepareBlankFileHeader;

     If Extension = '.z80' Then Begin

        // Z80 files have to be decoded before we can use them.
        // This will return with a .sna style snapshot in FileArray,
        // so set the extension as a .sna, and process accordingly.

        ConvertZ80;
        Extension := '.sna';

     End;

     If Extension = '.bcs' Then Begin

        // load a lite basinc session

        RestoreEmulationState(filename);
        Memory[FLAGX] := Memory[FLAGX] and 223;
        basinoutput.Running := False;
        //basinoutput.BufferKey(0, 13);
        //basinoutput.BufferKey(1, 13);
        ControlEmulation(False);
        Exit;
     End;

     If Extension = '.sna' Then Begin

        If Length(FileArray) = 0 Then Begin
           DoError($1A, 'Snapshot Error');
           Exit;
        End;

        DecodeSNA;

        // Grab the UDGs now as they won't be extracted from the body.

        // temporary code       Commented out by arda: probably test code,  breaks snapshot loading.
        //For MemPtr := 0 to 65535 - 16384 Do
        //   Memory[MemPtr+16384] := FileArray[MemPtr+27];

        // end

        For MemPtr := GetWord(@FileArray[27+UDG-16384])-16384 to Length(FileArray)-28 Do
           Memory[MemPtr+16384] := FileArray[MemPtr+27];

     End Else If (Extension = '.bas') or (Extension = '') Then Begin

        // .bas files are ascii text, so must be "compiled" into a TAP block
        // for loading.

        DecodeBAS;

        If GetWord(@FileHeader[$D]) <> $8000 Then
           DisplayWindow.BringToFront;

        // Now the FileHeader and FileBody vars are set up.

     End Else Begin

        // The extension didn't match .bas or .sna/.z80,
        // So it can't be loaded as a program.
        // Set the flags as a loading error, and exit.

        Registers.F := Registers.F and 254;
        DoError($1A, 'File not recognised');
        FileHeader := #0+FileHeader;
        SetLength(FileArray, 0);
        Exit;

     End;

     // Finally add the data type to the header at position 0
     // It is a program - type 0.
     FileHeader := #0+FileHeader;

     // And free the loaded file.
     SetLength(FileArray, 0);

     // Set the caption for a successful *LOAD*, not a MERGE

     If Memory[T_ADDR] = 1 Then Begin
        SetProjectName(Filename);
     End;
        //SetProjectName(ExtractFilename(Filename));

     CheckFor128kCommands;

  End;


End;

Procedure PrepareBlankFileHeader;
Var
  MemPtr: Word;
  LoopVar: Byte;
  Extension,
  NewFileName: AnsiString;
Begin
  // Prepare a 17 Byte header, and an empty body:
  // A 10 Character Filename, and pad bytes for the header info.
  // Tidy up the filename - remove the path and extension.

  FileBody := '';
  Extension := ExtractFileExt(Filename);
  NewFileName := ExtractFilename(Filename);
  NewFilename := Copy(NewFilename, 1, Length(NewFilename)-Length(Extension));
  FileHeader := Copy(ExtractFilename(NewFilename), 1, 10);

  // Header is 16 bytes, as the Data type byte is added later.
  While Length(FileHeader) < 16 Do
     FileHeader := FileHeader+' ';

  // If the filename is less than 10 chars, the ROM expects to find
  // a " mark and then spaces. Alter this, it should only expect spaces.

  If Length(Filename) < 10 Then Begin

     // address the first AnsiChar of the ROM header
     MemPtr := Registers.IX -$11;

     For LoopVar := 1 to 10 Do
        Memory[LoopVar+MemPtr] := Ord(FileHeader[LoopVar]);

  End;

  // Now that a real header, with a real filename (with default extension added)
  // has been created, we need to test if we're loading that file (IX-$10 <> $FF)
  // and that the filenames *match*

  If Memory[Registers.IX - $10] <> $FF then Begin
     For MemPtr := 1 to 10 Do
        Memory[MemPtr + (Registers.IX - $11)] := Ord(FileHeader[MemPtr]);
  End;

End;

Function SaveFile: Boolean;
Var
  OpenFile: TFileStream;
Begin
  // Saves a preset FileBody to the filename specified.
  Result := True;
  If FileExists(Filename) Then DeleteFile(Filename);
  If OpenFileStream(OpenFile, fmCreate or fmShareDenyWrite, Filename) Then begin
     OpenFile.Write(FileBody[1], Length(FileBody));
     OpenFile.Free;
  End Else Begin
     DoError($1A, 'Save Failed');
     Result := False;
  End;
End;

// Most of the following routines, especially the Z80 block compressor are lifted
// from ZX-Spin. Mark Woodmass had a lot to do with these.

Function OpenFileStream(var FileStream: TFileStream; StreamOptions: Word; Filename: AnsiString): Boolean;
Var
  Idx: Integer;
Begin
  Idx := 1;
  While Idx < Length(FileName)+1 Do Begin
     If Filename[Idx] < ' ' Then Begin
        MessageBox(BASinOutput.Handle, PChar('Invalid Filename:'#13+ExtractFilename(Filename)), PChar('File Error'), MB_OK or MB_ICONWARNING);
        Result := False;
        Exit;
     End;
     Inc(Idx);
  End;
  If (StreamOptions <> fmCreate) and (StreamOptions and fmOpenWrite = 0) Then
     If GetFileAttributes(Pchar(Filename)) = $FFFFFFFF Then Begin
        MessageBox(BASinOutput.Handle, PChar('Could not open the file:'#13+ExtractFilename(Filename)+#13'The file does not exist.'), PChar('File Error'), MB_OK or MB_ICONWARNING);
        Result := False;
        Exit;
     End;
  Try
     FileStream := TFileStream.Create(FileName, StreamOptions)
  Except
     on EFOpenError do Begin
        MessageBox(BASinOutput.Handle, PChar('Could not open the file:'#13+ExtractFilename(Filename)+#13'Another app may be using this file.'), PChar('File Error'), MB_OK or MB_ICONWARNING);
        Result := False;
        Exit;
     End;
  End;
  Result := True;
End;


Procedure LoadSna;
Var
  G: Integer;
Begin
  Registers.I := FileArray[0];  Registers.R := FileArray[20] And 127;
  Registers.Ln := FileArray[1]; Registers.Hn := FileArray[2];
  Registers.En := FileArray[3]; Registers.Dn := FileArray[4];
  Registers.Cn := FileArray[5]; Registers.Bn := FileArray[6];
  Registers.Fn := FileArray[7]; Registers.An := FileArray[8];
  Registers.L := FileArray[9];  Registers.H := FileArray[10];
  Registers.E := FileArray[11]; Registers.D := FileArray[12];
  Registers.C := FileArray[13]; Registers.B := FileArray[14];
  Registers.F := FileArray[21]; Registers.A := FileArray[22];
  Registers.IY := (FileArray[16] Shl 8)+FileArray[15];
  Registers.IX := (FileArray[18] Shl 8)+FileArray[17];
  Registers.SP := (FileArray[24] Shl 8)+FileArray[23];
  Registers.RBit7 := 0;
  If FileArray[19] and 4 > 0 Then
     Registers.IntsEnabled := True
  Else
     Registers.IntsEnabled := False;
  Registers.IntMode := FileArray[25];
  SetPortByte(2, FileArray[26]);
  For G := 0 To 49151 Do Memory[G+16384] := FileArray[G+27];
  Registers.PC := GetWord(@Memory[Registers.SP]);
  PutWord(@Memory[Registers.SP], 0);
  Inc(Registers.SP,2);
End;

Procedure SaveSna(Filename: AnsiString);
Var
  TempByte1: Byte;
  TempWord: Word;
  FileSize: LongWord;
  Sna48k: Boolean;
  Snap: Array[0..147487] of Byte;
  F: TFileStream;
  G, Bank, BankOffset: Integer;
Begin

  Sna48k := Not Get128kOptions;

  If GetFileAttributes(Pchar(Filename)) <> $FFFFFFFF then DeleteFile(Filename);
  If Not OpenFileStream(F, fmCreate or fmShareDenyWrite, Filename) Then Exit;
  ZeroMemory(@Snap[0], 147488);
  Snap[0]  := Registers.I;  Snap[20] := Registers.R;
  Snap[1]  := Registers.Ln; Snap[2]  := Registers.Hn;
  Snap[3]  := Registers.En; Snap[4]  := Registers.Dn;
  Snap[5]  := Registers.Cn; Snap[6]  := Registers.Bn;
  Snap[7]  := Registers.Fn; Snap[8]  := Registers.An;
  Snap[9]  := Registers.L;  Snap[10] := Registers.H;
  Snap[11] := Registers.E;  Snap[12] := Registers.D;
  Snap[13] := Registers.C;  Snap[14] := Registers.B;
  Snap[21] := Registers.F;  Snap[22] := Registers.A;
  Snap[15] := Registers.IY and 255;
  Snap[16] := Registers.IY shr 8;
  Snap[17] := Registers.IX and 255;
  Snap[18] := Registers.IX shr 8;
  If Registers.IntsEnabled Then Snap[19] := 4 Else Snap[19] := 0;
  Snap[25] := Registers.IntMode;
  Snap[26] := Registers.LastFE;
  TempWord := Registers.SP;

  If Sna48k Then Begin

     FileSize := 49179;

     For G := 0 to 49151 Do
        Snap[G+27] := Memory[16384+G];

     Memory[Registers.SP-1] := Registers.PC Shr 8;
     Memory[Registers.SP-2] := Registers.PC And 255;
     Dec(Registers.SP, 2);
     Snap[23] := Registers.SP and 255;
     Snap[24] := Registers.SP Shr 8;

  End Else Begin

     // 128k Snaps require bank 7 to be paged in at the menu, and the RAMDisk to be
     // initialised.

     SetRAMDisk;
     Page7FFDTransparent(7, PagedBank);

     // Set Bit 4 of FLAGS so that ROM 1 displays PLAY properly
     TempByte1 := Memory[Flags];
     Memory[FLAGS] := Memory[FLAGS] or 16;

     For G := 0 to 49151 Do
        Snap[G+27] := Memory[16384+G];

     BankOffset := 49183;

     For Bank := 0 To 7 Do Begin

        If Not (Bank in [5, 2, 7]) Then Begin

           For G := 0 To 16383 Do
              Snap[BankOffset + G] := RAMBanks[Bank, G];

           Inc(BankOffset, 16384);

        End;

     End;

     FileSize := BankOffset;

     // Copy the default "template" 128k snap header in, which
     // gives the snap the 128k sysvars that BASin doesn't handle.

     CopyMemory(@Snap[0], @Snap128[0], 7195);

     For G := 23399 to 23431 Do
        Snap[G-16384+27] := Memory[G];

     // IMPORTANT:
     // BASin is set to use a 48k model, and PC will be set incorrectly for
     // the 128k ROM. So, we set PC and the last 7FFD as if we really were at the 128k
     // Menu.

     Snap[49179] := 07; // PC, Word
     Snap[49180] := 02;
     Snap[49181] := 07; // 7FFD, Byte

     // Set SP to point to the correct location

     Snap[23] := $58;
     Snap[24] := $FF;

     // Restore Bit 4 so that BASin's 48k ROM doesn't complain (or crash, as it does).
     Memory[FLAGS] := TempByte1;
     Page7FFDTransparent(PagedBank, 7);

  End;

  Registers.SP := TempWord;
  F.Write(Snap[0], FileSize);
  F.Free;
  SetProjectName(Filename);
  //SetProjectName(ExtractFilename(Filename));

End;

Procedure DecodeSNA;
Var
  MemPtr, BASIClen, BASICPos, VARSPos, ELINEPos: Word;
Begin

  // The total program length (with vars) - get Data from Snapshot.
  // Increase all offsets by 27 to the start of Screen memory in the snap.
  // Bear in mind that there is no ROM, so all offsets need to be
  // dropped by 16384!

  If Length(FileArray) > 49179 Then

     // Find block 0 and copy it to the right place if necessary.

     If FileArray[49181] And 7 <> 0 Then

        CopyMemory(@FileArray[32795], @FileArray[49183], 16384);

  BASICPos := GetWord(@FileArray[27+PROG-16384])-16384;
  VARSPos := GetWord(@FileArray[27+VARS-16384])-16384;
  ELINEPos := GetWord(@FileArray[27+E_LINE-16384])-16384;
  BASICLen := VARSPos - BASICPos;

  // Total Block Length is ELINEPos - BASICPos -1
  PutWord(@FileHeader[$B], Word(ELINEPos - BASICPos -1));
  // The AutoStart - for a .sna, just a $80 in the high Byte.
  PutWord(@FileHeader[$D], $8000);

  // Now Just add the length of the BASIC
  PutWord(@FileHeader[$F], BASICLen);

  // The correct Header is built, now to grab the correct bytes from
  // the snap - this is the BASIC and the VARS
  For MemPtr := BASICPos to ELINEPos -1 Do
     FileBody := FileBody + AnsiChar(FileArray[MemPtr+27]);

End;

Procedure ConvertZ80;
Var
  MemBanks: Array[0..7, 0..16383] of Byte;
  Z80Array: Array of Byte;
  Offset: Integer;
  Is48k,
  BlockIsCompressed: Boolean;
  Bank, CurPaged, HeaderLen, BankOffset,
  BlockOffset, BlockLength, HeaderOffset, CompressedOffset: DWord;
Begin

  // Converts a .z80 snapshot into a .sna snapshot for
  // loading by DecodeSNA. Just for the record, .z80 is possibly the most needlessly
  // complex format ever designed.

  SetLength(Z80Array, Length(FileArray));
  CopyMemory(@Z80Array[0], @FileArray[0], Length(FileArray));

  // Now convert the 27 byte header

  FileArray[0] := Z80Array[10]; // I Register

  PutWord(@FileArray[01], GetWord(@Z80Array[19])); // HL' Registers
  PutWord(@FileArray[03], GetWord(@Z80Array[17])); // DE' Registers
  PutWord(@FileArray[05], GetWord(@Z80Array[15])); // BC' Registers

  FileArray[7] := Z80Array[22]; // F' Register
  FileArray[8] := Z80Array[21]; // A' Register

  PutWord(@FileArray[09], GetWord(@Z80Array[04])); // HL Registers
  PutWord(@FileArray[11], GetWord(@Z80Array[13])); // DE Registers
  PutWord(@FileArray[13], GetWord(@Z80Array[02])); // BC Registers
  PutWord(@FileArray[15], GetWord(@Z80Array[23])); // IY Registers
  PutWord(@FileArray[17], GetWord(@Z80Array[25])); // IX Registers

  FileArray[19] := Z80Array[27];                   // IFF2
  FileArray[20] := (Z80Array[11] And 127) +
                   ((Z80Array[12] and 1) Shl 7);   // R Register

  FileArray[21] := Z80Array[1]; // F Register
  FileArray[22] := Z80Array[0]; // A Register

  PutWord(@FileArray[23], GetWord(@Z80Array[08])); // SP Register

  FileArray[25] := Z80Array[29] And 2;             // Int Mode
  FileArray[26] := (Z80Array[12] Shr 1) And 7;     // Border Colour

  // Now for the Memory.

  If GetWord(@Z80Array[6]) <> 0 Then Begin

     // A Z80 1.45 Snapshot - 48k Only.

     SetLength(FileArray, 49179);
     PutWord(@FileArray[23], GetWord(@Z80Array[08])-2);
     PutWord(@FileArray[GetWord(@FileArray[23])+27], GetWord(@Z80Array[6])); // PUSH PC

     If Z80Array[12] And 32 = 32 Then Begin

        // The Block is compressed

        Offset := 30;
        BlockOffset := 27;
        Repeat
           If (Z80Array[Offset] = $ED) And (Z80Array[Offset+1] = $ED) Then Begin // 4-Byte Compressed Sequence
              HeaderLen := Z80Array[Offset+2];
              For CompressedOffset := 1 To HeaderLen Do Begin
                 FileArray[BlockOffset] := Z80Array[Offset+3];
                 Inc(BlockOffset);
              End;
              Inc(Offset, 4);
           End Else Begin
              FileArray[BlockOffset] := Z80Array[Offset];
              Inc(BlockOffset);
              Inc(Offset);
           End;
        Until Offset >= Length(Z80Array);

     End Else Begin

        // The Block is a straight memory dump.

        CopyMemory(@FileArray[27], @Z80Array[30], 16384);

     End;

  End Else Begin

     // Z80 2.01 or 3.05 Snapshot - Time to figure out which it is.

     If Z80Array[30] = 23 Then Begin

        // V2.01 Snapshot

        Is48k  := Z80Array[34] < 3;
        Offset := 55;

     End Else Begin

        // v3.05 Snapshot

        Is48k  := Z80Array[34] < 4;
        Offset := 86;

     End;

     // Now, if we're a 48k snapshot we're fine.
     // If we happen to be 128k, we need to redimension FileArray to
     // Accommodate the extra RAM blocks (even though we're going to discard most of them).

     If Not Is48k Then Begin
        CurPaged := Z80Array[35] and 7;
        If CurPaged in [2, 5] Then
           SetLength(FileArray, 147487)
        Else
           SetLength(FileArray, 131103);
     End Else
        SetLength(FileArray, 49179);

     // Now decompress the blocks to temporary storage.

     Repeat

        BlockLength := GetWord(@Z80Array[Offset]);
        If BlockLength = $FFFF Then Begin
           BlockLength := 16384;
           BlockIsCompressed := False;
        End Else Begin
           BlockIsCompressed := True;
        End;

        Inc(Offset, 2);
        Bank := Z80Array[Offset];
        If Is48k Then Begin
           Case Bank of
              4: Bank := 2;
              5: Bank := 0;
              8: Bank := 5;
           Else
              Bank := $FF;
           End;
        End Else Begin
           If Bank in [3..10] Then Begin
              Bank := Bank -3;
           End Else
              Bank := $FF;
        End;

        Inc(Offset);
        If Bank <> $FF Then Begin

           If Not BlockIsCompressed Then Begin

              For CompressedOffset := 0 To 16383 Do Begin
                 MemBanks[Bank, CompressedOffset] := Z80Array[Offset];
                 Inc(Offset);
              End;

           End Else Begin // Block is compressed.

              CompressedOffset := 0; // Position within Memory Block
              BankOffset := Offset;  // Start of Block

              Repeat

                 If (Offset > Length(Z80Array)-1) or (CompressedOffset > 16383) Then Begin
                    SetLength(FileArray, 0);
                    SetLength(Z80Array, 0);
                    MessageBox(BASinOutput.Handle,
                               PChar('This .z80 File contains a block'#13'that decompresses to greater'#13'than 16384 bytes.'),
                               PChar('Z80 Decompression Error'),
                               MB_OK or MB_ICONWARNING);
                    Exit;
                 End;

                 If GetWord(@Z80Array[Offset]) = $EDED Then Begin // 4-Byte Compressed Sequence

                    HeaderLen := Z80Array[Offset+2];
                    For HeaderOffset := 1 To HeaderLen Do Begin
                       MemBanks[Bank, CompressedOffset] := Z80Array[Offset+3];
                       Inc(CompressedOffset);
                    End;
                    Inc(Offset, 4);

                 End Else Begin

                    MemBanks[Bank, CompressedOffset] := Z80Array[Offset];
                    Inc(CompressedOffset);
                    Inc(Offset);

                 End;

              Until Offset = BankOffset + BlockLength;

           End;

        End Else

           Inc(Offset, BlockLength);

     Until Offset >= Length(Z80Array);

     // We now have the blocks extracted to a sensible arrangement - now to
     // send them to the appropriate places in the .sna array.

     If Is48k then Begin

        CopyMemory(@FileArray[27],    @MemBanks[5, 0], 16384);
        CopyMemory(@FileArray[16411], @MemBanks[2, 0], 16384);
        CopyMemory(@FileArray[32795], @MemBanks[0, 0], 16384);

     End Else Begin

        CopyMemory(@FileArray[27],    @MemBanks[5, 0], 16384);
        CopyMemory(@FileArray[16411], @MemBanks[2, 0], 16384);
        CopyMemory(@FileArray[32795], @MemBanks[CurPaged, 0], 16384);

        BankOffset := 49183;

        For Bank := 0 To 7 Do Begin

           If Not (Bank in [CurPaged, 5, 2]) Then Begin

              CopyMemory(@FileArray[BankOffset], @MemBanks[Bank, 0], 16384);
              Inc(BankOffset, 16384);

           End;

        End;

        // Finally, set up the extra info (where appropriate).

        PutWord(@FileArray[49179], GetWord(@Z80Array[32])); // PC
        FileArray[49181] := Z80Array[35];
        FileArray[49182] := 0;

     End;

  End;

  SetLength(Z80Array, 0);

End;

Function Get128kOptions: Boolean;
Begin

  CheckFor128kCommands;

  If ProgramIs128k Then
     Result := True
  Else
     If Not RAMDiskNeedsInit Then Begin
        If Opt_Always128k = a128kYes Then
           Result := True
        Else
           If Opt_Always128k = a128kNo Then
              Result := False
           Else
              Result := False;
     End Else
        If UsesUDGsTU Then
           Result := False
        Else
           If Opt_Always128k = a128kYes Then
              Result := True
           Else
              If Opt_Always128k = a128kNo Then
                 Result := False
              Else
                 Result := MessageDlg('Your snapshot can be saved as either 48k or 128k.'+#13+'Would you like to save a 128k Snapshot?'+#13+#13+'Tip: You can set a default save mode in Options.', mtWarning, [mbYes, mbNo], 0) = mrYes;

End;

Procedure SaveZ80;
Var
  Addr, BankAddr, BankSize, BankOffset: Word;
  BankHigh, RAMBank, Bank, BankNum: Byte;
  RunByte: Byte;
  RunCount, G: Word;
  RunStart: Word;
  Offset: DWord;
  FileSize: LongWord;
  StoreByte: Boolean;
  Snap: Array[0..147487*2] of Byte;
  F: TFileStream;
  Z80Is128k: Boolean;
Begin

  Z80Is128k := Get128kOptions;

  If GetFileAttributes(Pchar(Filename)) <> $FFFFFFFF then DeleteFile(Filename);
  If Not OpenFileStream(F, fmCreate or fmShareDenyWrite, Filename) Then Exit;

  ZeroMemory(@Snap[0], 147488);

  // Write the z80 Header - the first 30 bytes are common to all versions.

  Snap[0] := Registers.A;
  Snap[1] := Registers.F;
  PutWord(@Snap[2], GetWord(@Registers.C));
  PutWord(@Snap[4], GetWord(@Registers.L));
  PutWord(@Snap[6], GetWord(@Registers.PC));
  PutWord(@Snap[8], GetWord(@Registers.SP));
  Snap[10] := Registers.I;
  Snap[11] := Registers.R and 127;
  Snap[12] := ((Registers.RBit7) Shr 7)+((BorderDWord And 7) Shl 1);
  PutWord(@Snap[13], GetWord(@Registers.E));

  PutWord(@Snap[15], GetWord(@Registers.Cn));
  PutWord(@Snap[17], GetWord(@Registers.En));
  PutWord(@Snap[19], GetWord(@Registers.Ln));
  Snap[21] := Registers.An;
  Snap[22] := Registers.Fn;
  PutWord(@Snap[23], Registers.IY);
  PutWord(@Snap[25], Registers.IX);

  If Registers.IntsEnabled Then
     Snap[27] := 255
  Else
     Snap[27] := 0;

  Snap[28] := 255;
  Snap[29] := Registers.IntMode and 3;

  // Now write the memory if in an old 1.45 snap,
  // or the extended headers if a later version is selected.
  // 48k ONLY! If the user is running the ramdisk or extended commands, they will fail.

  If Opt_z80Version = 1 Then Begin

     // Write Memory block

     Addr := 16384;
     Offset := 30;

     While Addr < 65535 Do Begin

        StoreByte := True;

        If (Memory[Addr] = Memory[Addr+1]) Then Begin

           // Possible run? - but does it follow a $ED?

           If Addr > 16384 Then Begin

              If Memory[Addr-1] <> $ED Then Begin

                 RunByte := Memory[Addr];
                 RunStart := Addr;
                 RunCount := 1;

                 Repeat
                    Inc(RunCount);
                    Inc(RunStart);
                 Until (Memory[RunStart] <> Memory[RunStart+1]) or (RunCount > 254);

                 If (RunCount > 4) or ((RunCount > 1) and (RunByte = $ED)) Then Begin

                    // A Valid run of 5 bytes, or more than one $ED Byte.

                    StoreByte := False;
                    PutWord(@Snap[Offset], $EDED);
                    Inc(Offset, 2);
                    Snap[Offset] := RunCount;
                    Snap[Offset+1] := RunByte;
                    Inc(Offset, 2);
                    Addr := RunStart +1;

                 End;

              End;

           End;

        End;

        If StoreByte Then Begin

           Snap[Offset] := Memory[Addr];
           Inc(Addr);
           Inc(Offset);

        End;

     End;

     If Addr = 65535 Then Begin

        Snap[Offset] := Memory[Addr];
        Inc(Offset);

     End;

     If Offset <= 49181 Then Begin

        // Write the "Compressed" bit 5 of Byte 12

        Snap[12] := Snap[12] or 32;

        // Finally, write the terminating sequence.

        PutWord(@Snap[Offset], $ED00);
        PutWord(@Snap[Offset+2], $00ED);
        Inc(Offset, 4);

     End Else Begin

        // Didn't compress well, so store "as is".

        Offset := 30+49152;
        CopyMemory(@Snap[30], @Memory[16384], 49152);

     End;

     FileSize := Offset;

  End Else Begin

     // The next header section is common to both v2.01 and v3.05 types
     // Both these formats can handle the 128k stuff - but we need to save a very different
     // snap if the user chooses 128k mode.

     If Z80Is128k Then Begin

        // For a 128k snap, we need to convert to the .sna stored as consts at the top of this
        // file.

        Snap[0] := Snap128[22];                            // Registers.A
        Snap[1] := Snap128[21];                            // Registers.F
        PutWord(@Snap[2], GetWord(@Snap128[13]));          // Registers.C
        PutWord(@Snap[4], GetWord(@Snap128[9]));           // Registers.L
        PutWord(@Snap[32], $0207);                         // Registers.PC
        PutWord(@Snap[8], $FF58);                          // Registers.SP
        Snap[10] := Snap128[0];                            // Registers.I
        Snap[11] := Snap128[20] and 127;                   // Registers.R
        Snap[12] := ((Snap[20] and 128) shr 7)+(7 Shl 1);  // Registers.R Bit 7, Border
        PutWord(@Snap[13], GetWord(@Snap128[11]));         // Registers.E
        PutWord(@Snap[15], GetWord(@Snap128[5]));          // Registers.Cn
        PutWord(@Snap[17], GetWord(@Snap128[3]));          // Registers.En
        PutWord(@Snap[19], GetWord(@Snap128[1]));          // Registers.Ln
        Snap[21] := Snap128[8];                            // Registers.An
        Snap[22] := Snap128[7];                            // Registers.Fn
        PutWord(@Snap[23], GetWord(@Snap128[15]));         // Registers.IY
        PutWord(@Snap[25], GetWord(@Snap128[17]));         // Registers.IX
        Snap[27] := Snap128[19];                           // Registers.IntsEnabled
        Snap[28] := 255;                                   // IM 2, not used
        Snap[29] := Snap128[25] and 3;                     // Registers.IntMode

        PutWord(@Snap[6], 0);
        PutWord(@Snap[30], 23);

        If Opt_z80Version = 3 Then Begin
           Snap[34] := 3; // Hardware mode is 3 for a 128k snap in v2
        End Else Begin
           Snap[34] := 4; // Hardware mode is 4 for a 128k snap in v3.05
        End;

        PutWord(@Snap[35], 7);
        Snap[37] := 131; // LDIR, AY and R Register emulation on

        Snap[38] := LastFFFD;

        Snap[39] := AYRegisters.R0; // Fill soundchips
        Snap[39] := AYRegisters.R1;
        Snap[41] := AYRegisters.R2;
        Snap[39] := AYRegisters.R3;
        Snap[43] := AYRegisters.R4;
        Snap[39] := AYRegisters.R5;
        Snap[45] := AYRegisters.R6;
        Snap[39] := AYRegisters.R7;
        Snap[47] := AYRegisters.R8;
        Snap[39] := AYRegisters.R9;
        Snap[49] := AYRegisters.R10;
        Snap[39] := AYRegisters.R11;
        Snap[51] := AYRegisters.R12;
        Snap[39] := AYRegisters.R13;
        Snap[53] := AYRegisters.R14;
        Snap[39] := AYRegisters.R15;

        Offset := 55;

     End Else Begin

        PutWord(@Snap[6], $0000);
        PutWord(@Snap[30], 23);
        PutWord(@Snap[32], Registers.PC);

        Snap[34] := 0; // Hardware mode is 0 for a 48k snap.
        PutWord(@Snap[35], $0000);
        Snap[37] := 2; // LDIR and R Register emulation on

        Snap[38] := 0;

        PutWord(@Snap[39], $0000); // Fill soundchips with zeros
        PutWord(@Snap[41], $0000);
        PutWord(@Snap[43], $0000);
        PutWord(@Snap[45], $0000);
        PutWord(@Snap[47], $0000);
        PutWord(@Snap[49], $0000);
        PutWord(@Snap[51], $0000);
        PutWord(@Snap[53], $0000);

        Offset := 55;

     End;

     If Opt_z80Version = 3 Then Begin

        PutWord(@Snap[30], 54); // Signal a v3.05 file, and fill the extra info with zeros
        For Offset := 55 to 85 do
           Snap[Offset] := 0;

        Offset := 86;

     End;

     // Now write the extended version's memory blocks.

     CopyMemory(@RAMBanks[5, 0], @Memory[16384], 16384);
     CopyMemory(@RAMBanks[2, 0], @Memory[32768], 16384);
     CopyMemory(@RAMBanks[PagedBank, 0], @Memory[49152], 16384);

     If z80Is128k Then Begin

        BankHigh := 7;
        CopyMemory(@RAMBanks[5, 0], @Snap128[27], 7169);
        SetRAMDisk;

        // Set Bit 4 of FLAGS so that ROM 1 displays PLAY properly
        RAMBanks[5, FLAGS - 16384] := RAMBanks[5, FLAGS - 16384] or 16;

        For G := 23399 to 23431 Do
           RAMBanks[5, G-16384] := Memory[G];

     End Else Begin

        BankHigh := 2;

     End;

     For Bank := 0 to BankHigh Do Begin

        If Z80Is128k Then Begin
           BankNum := Bank + 3;
           RAMBank := Bank;
        End Else Begin
           Case Bank of
              0: Begin BankNum := 4; RAMBank := 2;         End;
              1: Begin BankNum := 5; RAMBank := PagedBank; End;
              2: Begin BankNum := 8; RAMBank := 5;         End;
           End;
        End;

        BankAddr := 0;
        BankOffset := Offset;
        Snap[Offset+2] := BankNum;
        Inc(Offset, 3);

        BankSize := 0;
        Addr := 0;

        While Addr < 16383 Do Begin

           StoreByte := True;

           If (RAMBanks[RAMBank, Addr+BankAddr] = RAMBanks[RAMBank, Addr+BankAddr+1]) Then Begin

              // Possible run? - but does it follow a $ED?

              If Addr > 0 Then Begin

                 If RAMBanks[RAMBank, Addr+BankAddr-1] <> $ED Then Begin

                    RunByte := RAMBanks[RAMBank, Addr+BankAddr];
                    RunStart := Addr;
                    RunCount := 1;

                    Repeat
                       Inc(RunCount);
                       Inc(RunStart);
                    Until (RAMBanks[RAMBank, RunStart+BankAddr] <> RAMBanks[RAMBank, RunStart+BankAddr+1]) or
                          (RunCount > 254) or
                          (RunStart > 16382);

                    If (RunCount > 4) or ((RunCount > 1) and (RunByte = $ED)) Then Begin

                       // A Valid run of 5 bytes, or more than one $ED Byte.

                       StoreByte := False;
                       PutWord(@Snap[Offset], $EDED);
                       Inc(Offset, 2);
                       Snap[Offset] := RunCount;
                       Snap[Offset+1] := RunByte;
                       Inc(Offset, 2);
                       Inc(BankSize, 4);
                       Addr := RunStart +1;

                    End;

                 End;

              End;

           End;

           If StoreByte Then Begin

              Snap[Offset] := RAMBanks[RAMBank, Addr+BankAddr];
              Inc(Addr);
              Inc(Offset);
              Inc(BankSize);

           End;

        End;

        If Addr = 16383 Then Begin

           Snap[Offset] := RAMBanks[RAMBank, Addr+BankAddr];
           Inc(Offset);
           Inc(BankSize);

        End;

        If BankSize > 16384 Then Begin

           // Didn't compress well - just store.

           PutWord(@Snap[BankOffset], $FFFF);
           CopyMemory(@Snap[BankOffset+3], @RAMBanks[RAMBank, 0], 16384);

        End Else Begin

           // Compressed, so write the size in.

           PutWord(@Snap[BankOffset], BankSize);

        End;

     End;

     FileSize := Offset;
     CopyMemory(@RAMBanks[5, 0], @Memory[16384], 7169);

  End;

  F.Write(Snap[0], FileSize);
  F.Free;
  SetProjectName(Filename);
  //SetProjectName(ExtractFilename(Filename));

End;

Function SaveProgram: Boolean;
Var
  Ext: AnsiString;
  State: TEmulationState;
Begin

  Result := True;
  If Filename = '' Then Begin
     Filename := OpenFile(BASinOutput.Handle, 'Save BASIC Program', [FTSession, FTBas, FTSnap], '', True, False);
     If Filename = '' Then Begin
        Result := False;
        Exit;
     End;
  End;

  Ext := Lowercase(ExtractFileExt(Filename));

  If Ext = '.bas' Then Begin

     BASinOutput.TokeniseEditText(False);
     SaveBAS(False, True);

  End Else If (Ext = '.sna') Then Begin

     BASinOutput.TokeniseEditText(False);
     SaveSna(Filename);

  End Else If (Ext = '.z80') Then Begin

     BASinOutput.TokeniseEditText(False);
     SaveZ80(Filename);

  End Else If (Ext = '.bcs') Then Begin

     //basin session
           SaveEmulationState(State);
           StoreEmulationState(Filename, State);

  End Else Begin

     BASinOutput.TokeniseEditText(False);
     Filename := Filename + '.bas';
     SaveBAS(False, True);

  End;


  if (trim(Copy(ExtractFilename(Filename),1,8))<>'autoback') Then BASinOutput.AddToMRUList(Filename);
  GenerateBASICChecksum(BASICChecksum);
  

End;

Procedure SaveCode;
Var
  Ext,FilePath, NewFilename: AnsiString;
  StartAddr, DataLen: Word;

Begin

  //arda comments v1.76
  // it looks like basin can load RAW BINARY files.
  // so a Legal Zx Spectrum Code file has .bsc extension, if user will not give an extension it will be saved as a bsc
  // if user wants to save a raw binary they must be specify an extension other than bsc

  If Filename = '' Then Begin
     Filename := OpenFile(BASinOutput.Handle, 'Save CODE Block', [FTBsc, FTAll], '', True, False);
     If Filename = '' Then Begin
        Exit;
     End;
  End;


  FilePath := ExtractFilePath(Filename);
  Ext := Lowercase(ExtractFileExt(Filename));
  NewFileName := ExtractFilename(Filename); //this will be used as blockname

  If Length(Ext)>0 Then Begin
        NewFilename := Copy(NewFilename, 1, Length(NewFilename)-Length(Ext));
  End Else Begin
        Ext:='.bsc';
        Filename:=Filename + '.bsc';
  End;

  CopyMemory(@FileHeader[2], @NewFilename[1], 10);

  DataLen := GetWord(@FileHeader[$C]);
  StartAddr := GetWord(@FileHeader[$E]);

  If Ext = '.bsc' Then Begin

     SetLength(FileBody, DataLen+17);
     CopyMemory(@FileBody[1], @FileHeader[1], 17);
     CopyMemory(@FileBody[18], @Memory[StartAddr], DataLen);

  End Else Begin

     SetLength(FileBody, DataLen);
     CopyMemory(@FileBody[1], @Memory[StartAddr], DataLen);

  End;

  SaveFile;

End;

Procedure SaveDATANum;
Begin
  SaveDataStr;
End;

Procedure SaveDATAStr;
Var
  Ext, FilePath, NewFilename: AnsiString;
  DataLen: Word;
Begin

  // Basically, transfer the contents of the save to Filebody,
  // Header first.

  If Filename = '' Then Begin
     Filename := OpenFile(BASinOutput.Handle, 'Save Variable Array', [FTBsd], '', True, False);
     If Filename = '' Then Begin
        Exit;
     End;
  End;

  FilePath := ExtractFilePath(Filename);
  Ext := Lowercase(ExtractFileExt(Filename));

  if Ext<>'.bsd' Then Begin
        Filename:=Filename+'.bsd'; //this is the filename that will written on disk
        Ext:='.bsd';
  End;

  NewFileName := ExtractFilename(Filename);
  NewFilename := Copy(NewFilename, 1, Length(NewFilename)-Length(Ext));  // this is used in the tape header
  //Filename := FilePath+NewFilename + '.bsd'; //as of 1.76 we will not change the extension if specified

  While Length(NewFilename) < 10 Do NewFilename := NewFilename + ' ';
  CopyMemory(@FileHeader[2], @NewFilename[1], 10);

  DataLen := GetWord(@FileHeader[$C]);
  SetLength(FileBody, DataLen+17);
  CopyMemory(@FileBody[1], @FileHeader[1], 17);

  // The Variable is pointed to by IX

  CopyMemory(@FileBody[18], @Memory[Registers.IX], GetWord(@Registers.E));

  // Now Save.

  SaveFile;

End;

Function SaveCurrentProgram(NewFilename: AnsiString): Boolean;
Begin
  Filename := NewFilename;
  If Filename = DefaultProjectName Then
     Filename := '';

  FileHeader := '                    ';
  If Opt_AutoStart Then
     PutWord(@FileHeader[$0E], Opt_AutoStartLine);

  Result := SaveProgram;

End;

Function CheckForSave: Boolean;
Var
  OldCheck: Word;
  MsgVal: Integer;
Begin
  Result := True;
  OldCheck := BASICChecksum;
  GenerateBASICChecksum(BASICChecksum);
  If Length(BASinOutput.BASICMem) < 2 Then
     Exit;
  If OldCheck <> BASICChecksum Then Begin
     BASICChecksum := OldCheck;
     MsgVal := MessageBox(BasinOutput.Handle,
                          PChar('Save changes to project '#13#39+CurProjectName+#39'?'),
                          PChar('Save changes?'),
                          MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL or MB_SETFOREGROUND or MB_TOPMOST);
     If MsgVal = IDYES Then Begin
        Result := SaveCurrentProgram(CurProjectFilename);
     End Else If MsgVal = IDCANCEL then Begin
        Result := False
     End Else
        Result := True;
  End;
End;

end.
