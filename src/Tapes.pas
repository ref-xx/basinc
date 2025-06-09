unit Tapes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, Buttons, ComCtrls, ImgList;

type
  TTapeWindow = class(TForm)
    ListView1: TListView;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    PopupMenu1: TPopupMenu;
    FromFile1: TMenuItem;
    CurrentProgram1: TMenuItem;
    Program1: TMenuItem;
    CodeBlock1: TMenuItem;
    Screen1: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenTapeImage1: TMenuItem;
    SaveImageAs1: TMenuItem;
    AddFromImage1: TMenuItem;
    N1: TMenuItem;
    SaveImage1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Edit5: TMenuItem;
    CutBlock1: TMenuItem;
    CopyBlock1: TMenuItem;
    PasteBlock1: TMenuItem;
    DeleteBlock1: TMenuItem;
    Block1: TMenuItem;
    AddFromFile1: TMenuItem;
    FromCurrent1: TMenuItem;
    MemoryBlock1: TMenuItem;
    Screen2: TMenuItem;
    Program2: TMenuItem;
    N3: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    Streaming1: TMenuItem;
    RedirectLOADcommands1: TMenuItem;
    RedirectSAVEcommands1: TMenuItem;
    N4: TMenuItem;
    SaveAs1: TMenuItem;
    Button4: TButton;
    N5: TMenuItem;
    Properties1: TMenuItem;
    ImageList1: TImageList;
    Help1: TMenuItem;
    TapeCreatorHelp1: TMenuItem;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Program1Click(Sender: TObject);
    procedure Screen1Click(Sender: TObject);
    procedure CodeBlock1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateTapeList;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FromFile1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    Function  GetTZXBlockLen(BlockType: Byte; Idx: Integer): DWord;
    procedure SaveImage1Click(Sender: TObject);
    procedure SaveImageAs1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure SaveAs1Click(Sender: TObject);
    procedure CutBlock1Click(Sender: TObject);
    procedure CopyBlock1Click(Sender: TObject);
    procedure PasteBlock1Click(Sender: TObject);
    procedure RedirectLOADcommands1Click(Sender: TObject);
    procedure RedirectSAVEcommands1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TapeCreatorHelp1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure OnEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
  public
    { Public declarations }
    UpdatingList: Boolean;
    procedure SaveTape(Filename: String);
    Procedure TapeToBAS(Block: String);
    Procedure TapeToBSC(Block: String);
    Procedure TapeToBSD(Block: String);
    procedure ExportTape;

    Procedure FileIsDropped(Var Msg: TMessage); Message WM_DropFiles; //arda
  end;

  Function  DATAToTape(Filename: String; VarName: Char; VarType: Byte): String;
  Function  CODEToTape(Filename: String; StartAddr: Word): String;
  Function  BASToTape(Filename: String; AutoStart: Word): String;
  Procedure RecalcChecksum(Index: Integer);
  Procedure TapeBlockAdd(Block: String);

var
  TapeClip:      String;
  TapeWindow:    TTapeWindow;
  TapeBlocks:    TStringlist;
  TapePosition:  Integer;
  TapeFilename:  String;
  TapeTrapLOAD:  Boolean;
  TapeTrapSAVE:  Boolean;

implementation

{$R *.DFM}

Uses BlockProps, FastCore, Filing, BASSupport, MemBlockAdd, Utility, QueryForm, ROMUtils,
  BasinMain, ShellAPI;

Procedure TTapeWindow.FileIsDropped(Var Msg: TMessage);
Var
  hDrop: THandle;
  fName: Array[0..1024] of Char;
  Name: AnsiString;
begin
  hDrop := Msg.WParam; Name := '';
  DragQueryFile(hDrop,0,fName,254);
  DragFinish(hDrop);
  DragAcceptFiles(Handle, True);

  Name := fName;

  {
  If (Lowercase(ExtractFileExt(Name)) = '.tap') or
  (Lowercase(ExtractFileExt(Name)) = '.tzx') or
  (Lowercase(ExtractFileExt(Name)) = '.scr') or
  (Lowercase(ExtractFileExt(Name)) = '.bin') or
  (Lowercase(ExtractFileExt(Name)) = '.z80') or
  (Lowercase(ExtractFileExt(Name)) = '.sna') or
  (Lowercase(ExtractFileExt(Name)) = '.raw') or
  (Lowercase(ExtractFileExt(Name)) = '.txt') or
  (Lowercase(ExtractFileExt(Name)) = '.bas') or
  (Lowercase(ExtractFileExt(Name)) = '.bsd') or
  (Lowercase(ExtractFileExt(Name)) = '.dat') or
  (Lowercase(ExtractFileExt(Name)) = '.asm')
      then Begin
  }
     Filename := Name;
     FromFile1Click(Self);
  //End;

End;

procedure TTapeWindow.Button1Click(Sender: TObject);
Var
  TP: TPoint;
begin
  TP := ClientToScreen(Point(Button1.Left+(Button1.Width Div 2), Button1.Top+(Button1.Height Div 2)));
  PopupMenu1.Popup(TP.X, TP.Y);
end;

procedure TTapeWindow.Program1Click(Sender: TObject);
Var
  NewFilename: String;
begin
  FileHeader := #$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80;
  SaveBAS(False, False);
  SetLength(FileArray, Length(FileBody));
  CopyMemory(@FileArray[0], @FileBody[1], Length(FileBody));
  DecodeBAS;
  NewFilename := Copy(CurProjectName, 1, 10);
  While Length(NewFilename) < 10 Do NewFilename := NewFilename + ' ';
  TapeBlockAdd(BASToTape(NewFilename, GetWord(@FileHeader[$D])));
  UpdateTapeList;
end;

procedure TTapeWindow.Screen1Click(Sender: TObject);
begin
  FileHeader := #$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80;
  SetLength(FileArray, 6912);
  CopyMemory(@FileArray[0], @Memory[16384], 6912);
  TapeBlockAdd(CODEToTape('Unnamed', 16384));
  UpdateTapeList;
end;

procedure TTapeWindow.CodeBlock1Click(Sender: TObject);
begin
  CentreFormOnForm(MemBlockWindow, TapeWindow);
  ShowWindow(MemBlockWindow, True);
  If (MemBlockWindow.StartAddr = 0) and (MemBlockWindow.DataLen = 0) Then Exit;
  SetLength(FileArray, MemBlockWindow.DataLen);
  CopyMemory(@FileArray[0], @Memory[MemBlockWindow.StartAddr], MemBlockWindow.DataLen);
  TapeBlockAdd(CODEToTape('Unnamed', MemBlockWindow.StartAddr));
  UpdateTapeList;
end;

Function DATAToTape(Filename: String; VarName: Char; VarType: Byte): String;
Var
  F: Integer;
  Header: String;
  DataLen: Word;
  Checksum: Byte;
Begin

  // Grabs an Array variable, and returns a .tap block. The Array should be in [FileArray].

  Result := '';
  DataLen := Length(FileArray);
  SetLength(FileBody, DataLen);
  CopyMemory(@FileBody[1], @FileArray[0], DataLen);

  // And build an appropriate header

  While Length(Filename) < 10 Do
     Filename := Filename + ' ';

  Header := #19+#0+#0+Chr(VarType)+Copy(Filename, 1, 10)+
            Chr(DataLen And 255)+Chr(DataLen Shr 8)+
            #0+VarName+#0#0;

  CheckSum := 0;
  For F := 4 to Length(Header) Do
     CheckSum := CheckSum Xor (Ord(Header[F]));

  Header := Header + Chr(CheckSum);
  Result := Result + Header;

  // Now Add the Variable contents block

  Inc(DataLen, 2); // Add the extra checksum and flag bytes :-)
  Result := Result + Chr(DataLen And 255)+Chr(DataLen Shr 8) + #255 + FileBody;

  FileBody := #255 + FileBody; // Add the flag byte
  CheckSum := Ord(FileBody[1]);
  For F := 2 to Length(FileBody) Do
     CheckSum := CheckSum Xor (Ord(FileBody[F]));

  Result := Result + Chr(CheckSum);

  // All done - Result contains a new .tap code block :)

End;



Function CODEToTape(Filename: String; StartAddr: Word): String;
Var
  F: Integer;
  Header: String;
  DataLen: Word;
  Checksum: Byte;
Begin

  // Grabs a block of memory, and returns a .tap block. The block should be in [FileArray].

  Result := '';
  DataLen := Length(FileArray);
  SetLength(FileBody, DataLen);
  CopyMemory(@FileBody[1], @FileArray[0], DataLen);

  // And build an appropriate header

  While Length(Filename) < 10 Do
     Filename := Filename + ' ';

  Header := #19+#0+#0+#3+Copy(Filename, 1, 10)+
            Chr(DataLen And 255)+Chr(DataLen Shr 8)+
            Chr(StartAddr And 255)+Chr(StartAddr Shr 8)+
            #0#0;

  CheckSum := 0;
  For F := 4 to Length(Header) Do
     CheckSum := CheckSum Xor (Ord(Header[F]));

  Header := Header + Chr(CheckSum);
  Result := Result + Header;

  // Now add the body (CODE block)

  Inc(DataLen, 2); // Add the extra checksum and flag bytes :-)
  Result := Result + Chr(DataLen And 255)+Chr(DataLen Shr 8) + #255 + FileBody;

  FileBody := #255 + FileBody; // Add the flag byte
  CheckSum := Ord(FileBody[1]);
  For F := 2 to Length(FileBody) Do
     CheckSum := CheckSum Xor (Ord(FileBody[F]));

  Result := Result + Chr(CheckSum);

  // All done - Result contains a new .tap code block :)

End;

Function BASToTape(Filename: String; AutoStart: Word): String;
Var
  F: Integer;
  DataLen, ProgLen: Word;
  Header: String;
  CheckSum: Byte;
Begin

  // Converts the supplied .BAS file (ascii text, in [FileBody]) to a .tap block

  Result := '';

  // Now, convert to a Spectrum loadable data block, which will be
  // stored in [FileBody], with extra info in [FileHeader]
  // And build an appropriate header

  While Length(Filename) < 10 Do
     Filename := Filename + ' ';

  DataLen := Length(FileBody);
  Header := #19+#0+#0+#0+Copy(Filename, 1, 10)+Chr(DataLen And 255)+Chr(DataLen Shr 8);

  If AutoStart <> 65535 Then
     Header := Header + Chr(AutoStart And 255) + Chr(AutoStart Shr 8)
  Else
     Header := Header + Chr(0) + Chr(128);

  ProgLen := GetWord(@FileHeader[$F]);
  Header := Header + Chr(ProgLen and 255) + Chr(ProgLen Shr 8);

  CheckSum := 0;
  For F := 4 to Length(Header) Do
     CheckSum := CheckSum Xor (Ord(Header[F]));

  Header := Header + Chr(CheckSum);

  Result := Result + Header;

  // Now add the file body.

  Inc(DataLen, 2); // Add the extra checksum and flag bytes :-)
  Result := Result + Chr(DataLen And 255)+Chr(DataLen Shr 8) + #255 + FileBody;

  FileBody := #255 + FileBody; // Add the flag byte
  CheckSum := Ord(FileBody[1]);
  For F := 2 to Length(FileBody) Do
     CheckSum := CheckSum Xor (Ord(FileBody[F]));

  Result := Result + Chr(CheckSum);

  // All done - Result contains a new .tap program block :)

End;

Procedure RecalcChecksum(Index: Integer);
Var
  F: Integer;
  CheckSum: Byte;
  TempStr: String;
Begin
  TempStr := TapeBlocks[Index];
  CheckSum := 0;
  For F := 4 to 20 Do
     CheckSum := CheckSum Xor (Ord(TempStr[F]));
  TempStr[21] := Chr(CheckSum);
  TapeBlocks[Index] := TempStr;
End;

Procedure TapeBlockAdd(Block: String);
Begin

  TapeBlocks.Add(Block);

End;

procedure TTapeWindow.FormShow(Sender: TObject);
begin
  Constraints.MinWidth := Button4.Left + Button4.Width + 32 + BitBtn1.Width + BitBtn2.Width;
  Button4.Enabled := False;
  Button2.Enabled := False;
  BitBtn1.Enabled := False;
  BitBtn2.Enabled := False;
  if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;
  ListView1.Columns[1].Width := Listview1.ClientWidth - ListView1.Columns[0].Width;
end;

Procedure TTapeWindow.UpdateTapeList;
Var
  CurSelected, F: Integer;
  ListItem: TListItem;
  TempStr: String;
  TempWord: Word;
Begin
  UpdatingList := True;
  Listview1.Items.BeginUpdate;

     If ListView1.Selected <> Nil Then
        CurSelected := ListView1.Selected.Index
     Else
        CurSelected := -1;

     ListView1.Items.Clear;

     For F := 0 To TapeBlocks.Count -1 Do Begin

        ListItem := ListView1.Items.Add;
        Case TapeBlocks[F][4] Of
           #0:
              Begin
                 // PROGRAM Block
                 TempStr := Copy(TapeBlocks[F], 5, 10);
                 While Copy(TempStr, Length(TempStr), 1) = ' ' Do
                    TempStr := Copy(TempStr, 1, Length(TempStr) -1);
                 TempStr := 'Program: ' + TempStr;
                 TempWord := GetWord(@TapeBlocks[F][17]);
                 If TempWord And $8000 = 0 Then
                    TempStr := TempStr + ' LINE ' + IntToStr(TempWord and $7FFF);
                 ListItem.SubItems.Add(TempStr);
              End;
           #1:
              Begin
                 // Num Array
                 TempStr := Copy(TapeBlocks[F], 5, 10);
                 While Copy(TempStr, Length(TempStr), 1) = ' ' Do
                    TempStr := Copy(TempStr, 1, Length(TempStr) -1);
                 TempStr := 'Number Array: ' + TempStr + '[' + Chr((Ord(TapeBlocks[F][18]) and 31) + $60) + ']';
                 ListItem.SubItems.Add(TempStr);
              End;
           #2:
              Begin
                 // Str Array
                 TempStr := Copy(TapeBlocks[F], 5, 10);
                 While Copy(TempStr, Length(TempStr), 1) = ' ' Do
                    TempStr := Copy(TempStr, 1, Length(TempStr) -1);
                 TempStr := 'Character Array: ' + TempStr + '[' + Chr((Ord(TapeBlocks[F][18]) and 31) + $60) + '$]';
                 ListItem.SubItems.Add(TempStr);
              End;
           #3:
              Begin
                 // BYTES Block
                 TempStr := Copy(TapeBlocks[F], 5, 10);
                 While Copy(TempStr, Length(TempStr), 1) = ' ' Do
                    TempStr := Copy(TempStr, 1, Length(TempStr) -1);
                 TempStr := 'Bytes: ' + TempStr + ' CODE ' + IntToStr(GetWord(@TapeBlocks[F][17])) + ',' + IntToStr(GetWord(@TapeBlocks[F][15]));
                 ListItem.SubItems.Add(TempStr);
              End;
        End;

        If F = CurSelected Then ListItem.Selected := True;
        If F = TapePosition Then
           ListItem.ImageIndex := 0
        Else
           ListItem.ImageIndex := -1;

     End;

  ListView1.Items.EndUpdate;
  If Visible Then ListView1.SetFocus;
  UpdatingList := False;

End;

procedure TTapeWindow.Button3Click(Sender: TObject);
begin
  TapeBlocks.Clear;
  UpdateTapeList;
end;

procedure TTapeWindow.Button2Click(Sender: TObject);
Var
  SelIndex: Integer;
begin
  If ListView1.Selected <> Nil Then Begin
     SelIndex := ListView1.Selected.Index;
     ListView1.Items.Delete(SelIndex);
     TapeBlocks.Delete(SelIndex);
  End;
end;

procedure TTapeWindow.FromFile1Click(Sender: TObject);
Var
  VarType, Checksum: Byte;
  VarName: Char;
  DataLen, StartAddr: Word;
  BlockLen, TAPLen: DWord;
  Ext, NewFilename, NewBlock, Header, TempStr: String;
  OpenFileStream: TFileStream;
  F, Offset, LoadLength, CodeAddress, Idx, TZXMajor, TZXMinor: Integer;
Label
  NewTZXHeader;
begin


 if Sender <> Self Then Begin

  If Sender = FromFile1 Then
     Filename := OpenFile(Handle, 'Insert From File', [FTBas, FTSnap, FTBsc, FTBsd, FTScr, FTTape, FTAll], '', False, False)
  Else
     If Sender = AddFromImage1 Then
        Filename := OpenFile(Handle, 'Insert From Tape File', [FTTape], '', False, False)
     Else
        If Sender = BASinOutput Then Begin
           TapeTrapLOAD := True;
           Filename := OpenFile(Handle, 'Open Tape File', [FTTape], '', False, False);
        End Else
           If Sender = OpenTapeImage1 Then
              Filename := OpenFile(Handle, 'Open Tape File', [FTTape], '', False, False)
           Else If (Filename = '') and (Sender <> nil) Then
              Filename := OpenFile(Handle, 'Insert From File', [FTBas, FTSnap, FTBsc, FTBsd, FTScr, FTTape, FTAll], '', False, False);

  end;
  If FileExists(Filename) Then Begin
     OpenFileStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
     SetLength(FileArray, OpenFileStream.Size);
     OpenFileStream.Read(FileArray[0], OpenFileStream.Size);
     OpenFileStream.Free;
  End Else
     Filename := '';

  If Filename = '' Then Exit;   

  If (Sender = OpenTapeImage1) or (Sender = BASinOutput) or (Sender = nil) Then Begin
     TapeBlocks.Clear;
     TapePosition := 0;
  End;

  FileBody := '';
  NewFileName := ExtractFilename(Filename);
  NewFilename := Copy(NewFilename, 1, Length(NewFilename)-4);
  FileHeader := #0+Copy(ExtractFilename(NewFilename), 1, 10);
  While Length(FileHeader) < 19 Do FileHeader := FileHeader + ' ';

  Ext := Lowercase(ExtractFileExt(Filename));
  If (Ext = '.bas') Then Begin

     // A BASin .bas (BASIC) file

     DecodeBAS;

     TapeBlockAdd(BASToTape(Copy(FileHeader, 2, 9), GetWord(@FileHeader[$D])));
     UpdateTapeList;

  End Else If (Ext = '.sna') or (Ext = '.z80') Then Begin

     // A Spectrum Snapshot - extract the BASIC and (optionally) the UDGs.

     DecodeSna;

     TapeBlockAdd(BASToTape(Copy(FileHeader, 2, 9), GetWord(@FileHeader[$D])));

  End Else If (Ext = '.bsc') Then Begin

     // A BASin Code Block

     LoadLength := GetWord(@FileArray[$0B]);
     CodeAddress := GetWord(@FileArray[$D]);
     Offset := 17;
     FileBody := '';
     For F := Offset To Offset + LoadLength -1 Do
        FileBody := FileBody + Chr(FileArray[F]);

     SetLength(FileArray, Length(FileBody));
     CopyMemory(@FileArray[0], @FileBody[1], Length(FileBody));

     TapeBlockAdd(CODEToTape(Copy(FileHeader, 2, 9), CodeAddress));

  End Else If (Ext = '.bsd') Then Begin

     // A BASin saved DATA Variable Array.

     VarType := FileArray[0];
     VarName := Chr(FileArray[$E]);

     FileBody := '';
     For F := 17 To Length(FileArray) -1 Do
        FileBody := FileBody + Chr(FileArray[F]);
     SetLength(FileArray, Length(FileBody));
     CopyMemory(@FileArray[0], @FileBody[1], Length(FileBody));

     TapeBlockAdd(DATAToTape(Copy(FileHeader, 2, 9), VarName, VarType));

  End Else If (Ext = '.scr') Then Begin

     For F := 0 To 6911 Do
        FileBody := FileBody + Chr(FileArray[F]);

     TapeBlockAdd(CODEToTape(Copy(FileHeader, 2, 9), 16384));

  End Else If (Ext = '.tap') or (Ext = '.tzx') Then Begin

     // Tapes have all their ID $10 blocks extracted.

     If Ext = '.tzx' Then Begin

        // TZX Extractor
        // Test validity of the file.

        TempStr := '';
        For Idx := 0 To 7 Do
           TempStr := TempStr + Chr(FileArray[Idx]);
        If TempStr <> 'ZXTape!'+#26 Then Begin
           MessageBox(Handle, pChar('The file '#39+ExtractFilename(Filename)+#39#13'does not appear to be a valid TZX File.'), pChar('Unknown Tape File'),  MB_OK or MB_ICONWARNING);
           Exit;
        End;

        TZXMajor := FileArray[8];
        TZXMinor := FileArray[9];

        If (TZXMajor > 1) or (TZXMinor > 13) Then
           MessageBox(Handle, pChar('The file '#39+ExtractFilename(Filename)+#39#13'is of a newer TZX version than BasinC'#13'recognises. There may be problems extracting from it.'), pChar('Unknown Tape File'),  MB_OK or MB_ICONWARNING);

        // Begin extraction

        Idx := 10;

        While Idx < Length(FileArray) Do Begin

           Inc(Idx); // Skip the ID byte.

           If FileArray[Idx -1] = $10 Then Begin

              NewTZXHeader:

              // We only support extraction from ROM Style blocks.
              // Get the block length and add it to the current index position for later seeking.

              BlockLen := GetTZXBlockLen(FileArray[Idx -1], Idx);

              // At this point, we're expecting a header block. If we don't get one, then it's a headerless block.

              If FileArray[Idx +4] = 0 Then Begin

                 // A file header. Send that out to the new tape block and then look for the next $10 block.

                 NewBlock := '';
                 TAPLen := FileArray[Idx +2] + (FileArray[Idx +3] Shl 8) +2;
                 SetLength(TempStr, TAPLen);
                 CopyMemory(@TempStr[1], @FileArray[Idx +2], TAPLen);
                 NewBlock := NewBlock + TempStr;

                 // Now look for the DATA Block - the next $10 block.

                 Inc(Idx, BlockLen);

                 While Idx < Length(FileArray) Do Begin

                    Inc(Idx); // Skip the ID Byte.

                    If FileArray[Idx -1] = $10 Then Begin

                       // Test for a DATA block. If it's a Header then jump back to set up for a new tape block and discard this one.

                       If FileArray[Idx +4] = $FF Then Begin

                          // Gather up the data for this block, and add it to the array.

                          TAPLen := FileArray[Idx +2] + (FileArray[Idx +3] Shl 8) +2;
                          SetLength(TempStr, TAPLen);
                          CopyMemory(@TempStr[1], @FileArray[Idx +2], TAPLen);
                          NewBlock := NewBlock + TempStr;

                          Inc(Idx, GetTZXBlockLen($10, Idx));
                          TapeBlockAdd(NewBlock);
                          Break;

                       End Else Begin

                          // A New header. Head back for a new block. The ID Byte has already been skipped.

                          Goto NewTZXHeader;

                       End;

                    End Else Begin

                       Inc(Idx, GetTZXBlockLen(FileArray[Idx -1], Idx));

                    End;

                 End;

                 If Idx >= Length(FileArray) Then Begin

                    // No more blocks, so bail out.

                    UpdateTapeList;
                    Exit;

                 End;

              End Else Begin

                 // No header, this is DATA. Send it out with a dummy CODE block header.

                 SetLength(NewBlock, 19 + BlockLen);
                 DataLen := GetWord(@FileArray[Idx +2]);
                 StartAddr := 0;
                 Filename := 'Headerless';
                 CopyMemory(@Filename[1], @FileArray[Idx +6], 10);
                 Header := #19+#0+#0+#3+Copy(Filename, 1, 10)+
                    Chr(DataLen And 255)+Chr(DataLen Shr 8)+
                    Chr(StartAddr And 255)+Chr(StartAddr Shr 8)+
                    #0#0;

                 CheckSum := 0;
                 For F := 4 to Length(Header) Do
                    CheckSum := CheckSum Xor (Ord(Header[F]));

                 Header := Header + Chr(CheckSum);
                 CopyMemory(@NewBlock[1], @Header[1], Length(Header));
                 CopyMemory(@NewBlock[20], @FileArray[Idx +2], DataLen);

                 Inc(Idx, BlockLen);

              End;


           End Else Begin

              // All the rest we skip.

              BlockLen := GetTZXBlockLen(FileArray[Idx -1], Idx);
              Inc(Idx, BlockLen);

           End;

        End;


     End Else Begin

        // TAP Extractor - all blocks are $10 blocks :)

        While Idx < Length(FileArray) Do Begin

           BlockLen := GetWord(@FileArray[Idx]) +2;

           // At this point, we're expecting a header block. If we don't get one, then it's a headerless block.

           If FileArray[Idx +2] = 0 Then Begin

              // A file header. Send that out to the new tape block and then look for the next $10 block.

              NewBlock := '';
              TAPLen := FileArray[Idx] + (FileArray[Idx +1] Shl 8) +2;
              SetLength(TempStr, TAPLen);
              CopyMemory(@TempStr[1], @FileArray[Idx], TAPLen);
              NewBlock := NewBlock + TempStr;

              // Now look for the DATA Block - the next $10 block.

              Inc(Idx, BlockLen);

              While Idx < Length(FileArray) Do Begin

                 // Test for a DATA block. If it's a Header then jump back to set up for a new tape block and discard this one.

                 If FileArray[Idx +2] = $FF Then Begin

                    // Gather up the data for this block, and add it to the array.

                    TAPLen := FileArray[Idx] + (FileArray[Idx +1] Shl 8) +2;
                    SetLength(TempStr, TAPLen);
                    CopyMemory(@TempStr[1], @FileArray[Idx], TAPLen);
                    NewBlock := NewBlock + TempStr;

                    Inc(Idx, TAPLen);
                    TapeBlockAdd(NewBlock);
                    Break;

                 End Else

                    Break;

              End;

              If Idx >= Length(FileArray) Then Begin

                 // No more blocks, so bail out.

                 UpdateTapeList;
                 Exit;

              End;

           End Else Begin

              // No header, this is DATA. Send it out with a dummy CODE block header.

              SetLength(NewBlock, 21 + BlockLen);
              DataLen := GetWord(@FileArray[Idx]);
              StartAddr := 0;
              Filename := 'Headerless';
              CopyMemory(@Filename[1], @FileArray[Idx +4], 10);
              Header := #19+#0+#0+#3+Copy(Filename, 1, 10)+
                 Chr(DataLen And 255)+Chr(DataLen Shr 8)+
                 Chr(StartAddr And 255)+Chr(StartAddr Shr 8)+
                 #0#0;

              CheckSum := 0;
              For F := 4 to Length(Header) Do
                 CheckSum := CheckSum Xor (Ord(Header[F]));
              Header := Header + Chr(CheckSum);
              CopyMemory(@NewBlock[1], @Header[1], Length(Header));
              CopyMemory(@NewBlock[20], @FileArray[Idx], DataLen +2);

              Inc(Idx, BlockLen);

           End;

        End;

     End;

  End Else Begin

     // All the rest are treated as CODE blocks.

     TapeBlockAdd(CODEToTape(Copy(FileHeader, 2, 9), 32768));

  End;

  UpdateTapeList;

end;

Function TTapeWindow.GetTZXBlockLen(BlockType: Byte; Idx: Integer): DWord;
Begin

  // Returns the length of a block held at FileArray[idx]

  Case BlockType of

     $10: Result := FileArray[Idx+2]+(FileArray[Idx+3] Shl 8)+4;
     $11: Result := FileArray[Idx+15]+(FileArray[Idx+16] Shl 8)+(FileArray[Idx+17] Shl 16)+18;
     $12: Result := 4;
     $13: Result := 1+(FileArray[Idx]*2);
     $14: Result := FileArray[Idx+7]+(FileArray[Idx+8] Shl 8)+(FileArray[Idx+9] Shl 16)+10;
     $15: Result := FileArray[Idx+5]+(FileArray[Idx+6] Shl 8)+(FileArray[Idx+7] Shl 16)+8;
     $20: Result := 2;
     $21: Result := FileArray[Idx]+1;
     $22: Result := 0;
     $23: Result := 2;
     $24: Result := 2;
     $25: Result := 0;
     $26: Result := (FileArray[Idx]+(FileArray[Idx+1] Shl 8)*2)+2;
     $27: Result := 0;
     $28: Result := FileArray[Idx]+(FileArray[Idx+1] Shl 8)+2;
     $2A: Result := 4;
     $30: Result := FileArray[Idx]+1;
     $31: Result := FileArray[Idx+1]+2;
     $32: Result := FileArray[Idx]+(FileArray[Idx+1] Shl 8)+2;
     $33: Result := (FileArray[Idx]*3)+1;
     $34: Result := 8;
     $35: Result := FileArray[Idx+16]+(FileArray[Idx+17] Shl 8)+(FileArray[Idx+18] Shl 16)+(FileArray[Idx+19] Shl 24)+20;
     $40: Result := FileArray[Idx+1]+(FileArray[Idx+2] Shl 8)+(FileArray[Idx+3] Shl 16)+4;
     $5A: Result := 9;

  Else

     Result := FileArray[Idx]+(FileArray[Idx+1] Shl 8)+(FileArray[Idx+2] Shl 16)+(FileArray[Idx+3] Shl 24)+4;

  End;

End;

procedure TTapeWindow.BitBtn1Click(Sender: TObject);
Var
  TempStr: String;
begin
  If ListView1.Selected <> Nil Then Begin
     If ListView1.Selected.Index > 0 Then Begin
        TempStr := TapeBlocks[ListView1.Selected.Index];
        TapeBlocks.Delete(ListView1.Selected.Index);
        TapeBlocks.Insert(ListView1.Selected.Index -1, TempStr);
        ListView1.Items[ListView1.Selected.Index-1].Selected := True;
        BitBtn1.Enabled := True and (ListView1.Selected.Index > 0);
        BitBtn2.Enabled := True and (ListView1.Selected.Index < ListView1.Items.Count -1);
        UpdateTapeList;
     End;
  End;
 

end;

procedure TTapeWindow.BitBtn2Click(Sender: TObject);
Var
  TempStr: String;
begin
  If ListView1.Selected <> Nil Then Begin
     If ListView1.Selected.Index < ListView1.Items.Count -1 Then Begin
        TempStr := TapeBlocks[ListView1.Selected.Index];
        TapeBlocks.Delete(ListView1.Selected.Index);
        TapeBlocks.Insert(ListView1.Selected.Index +1, TempStr);
        ListView1.Items[ListView1.Selected.Index+1].Selected := True;
        BitBtn1.Enabled := True and (ListView1.Selected.Index > 0);
        BitBtn2.Enabled := True and (ListView1.Selected.Index < ListView1.Items.Count -1);
        UpdateTapeList;
     End;
  End;
end;

procedure TTapeWindow.SaveTape(Filename: String);
Var
  SaveFile: TFilestream;
  Ext, TempStr, TempHdr: String;
  DataLen: Word;
  F: Integer;
begin

  If Filename = '' Then Begin

     // Now get a filename.
     Filename := OpenFile(Handle, 'Save Tape Image', [FTTape], '', True, False);
     If Filename = '' Then Exit;

  End;

  If FileExists(Filename) Then DeleteFile(Filename);
  If FileExists(Filename) Then Begin
     MessageBox(Handle, pChar('Could not save the file'#13+ExtractFilename(Filename)+' - another'#13'application may be using this file.'), pChar('Save Error'), MB_OK or MB_ICONWARNING);
     Exit;
  End;

  Ext := Lowercase(ExtractFileExt(Filename));
  If Ext = '.tap' Then Begin

     If OpenFileStream(SaveFile, fmCreate or fmShareDenyWrite, Filename) Then Begin
        For F := 0 To TapeBlocks.Count -1 Do Begin
           TempStr := TapeBlocks[F];
           SaveFile.Write(TempStr[1], Length(TempStr));
        End;
        SaveFile.Free;
     End;

  End Else If Ext = '.tzx' Then Begin

     If OpenFileStream(SaveFile, fmCreate or fmShareDenyWrite, Filename) Then Begin
        TempHdr := 'Created with '+ReleaseName;
        TempStr := 'ZXTape!'#$1A#1#13#$30+Chr(Length(TempHdr))+TempHdr;
        SaveFile.Write(TempStr[1], Length(TempStr));
        For F := 0 To TapeBlocks.Count -1 Do Begin
           // ID Byte, two PAUSE bytes
           TempStr := #$10+#232+#3;
           SaveFile.Write(TempStr[1], 3);

           // Write the .tap DATA out
           TempStr := TapeBlocks[F];
           DataLen := GetWord(@TempStr[1]);
           SaveFile.Write(TempStr[1], DataLen+2);

           TempStr := Copy(TempStr, DataLen+3, 999999);
           TempHdr := #$10+#232+#3;
           SaveFile.Write(TempHdr[1], 3);
           SaveFile.Write(TempStr[1], Length(TempStr));
        End;
        SaveFile.Free;
     End;

  End;

end;

Procedure TTapeWindow.TapeToBAS(Block: String);
Var
  Mem_Backup: Array[0..65535] of Byte;
  ProgLen: Word;
  Idx: Integer;
Begin

  // Saves a tape block (PROGRAM: type) out to a .bas file.
  // This involves a fair bit of cheating - we store a copy of the Memory Array, dump the current block's Data
  // To it, then set up the VARS and E_LINE sysvars before calling SaveBAS, which returns the .bas file in FileBody[].

  CopyMemory(@Mem_Backup[0], @Memory[0], 65535);

  // TAP Blocks always have a header in the first 21 bytes, so copy that to FileHeader.

  SetLength(FileHeader, 17);
  CopyMemory(@FileHeader[1], @Block[4], 17);

  ProgLen := GetWord(@FileHeader[16]); // This is the length of the PROGRAM, not the vars.
  If ProgLen > GetWord(@FileHeader[12]) Then
     ProgLen := GetWord(@FileHeader[12]);

  // Now skip out to the DATA

  Idx := 27;
  While Idx <= Length(Block) Do Begin
     Memory[GetWord(@Memory[PROG])+Idx-27] := Ord(Block[Idx]);
     Inc(Idx);
  End;
  Memory[GetWord(@Memory[PROG])+Idx-27] := $80;

  PutWord(@Memory[VARS], GetWord(@Memory[PROG])+ProgLen);
  PutWord(@Memory[E_LINE], GetWord(@Memory[PROG])+Length(Block));
  SaveBAS(False, False);

  Filename := OpenFile(Handle, 'Save as .bas file...', [FTBas], '', True, False);
  If Filename <> '' Then Begin
     If FileExists(Filename) Then DeleteFile(Filename);
     If Not FileExists(Filename) Then
        SaveFile
     Else
        MessageBox(Handle, pChar('Could not save the file'#13+ExtractFilename(Filename)+' - another'#13'application may be using this file.'), pChar('Save Error'), MB_OK or MB_ICONWARNING);
  End;

  // Finally, restore the emulation state. You'll never even know it happened.

  CopyMemory(@Memory[0], @Mem_Backup[0], 65535);

End;

Procedure TTapeWindow.TapeToBSC(Block: String);
Var
  Ext: String;
Begin

  // Saves a BYTES: block out to a file as a .bsc file, or a binary dump if no .bsc extension is present.
  // Trivial to do, as emulation doesn't get affected by this method.

  Filename := OpenFile(Handle, 'Save as CODE file...', [FTBsc, FTScr, FTAll], '', True, False);

  Ext := Lowercase(ExtractFileExt(Filename));

  If Ext = '.bsc' Then Begin

     // A .bsc file with the included 17 byte header - just take a copy of the file.

     FileBody := Copy(Block, 4, 17) + Copy(Block, 25, Length(Block) -25);

  End Else If Ext = '.scr' Then Begin

     // Take a maximum of 6912 bytes, and set the address to 16384
     // these files can be less than 6912 bytes, but no more.

     PutWord(@Block[17], 16384);
     FileBody := Copy(Block, 25, 6912);
     PutWord(@Block[15], Length(FileBody));

  End Else Begin

     FileBody := Copy(Block, 25, Length(Block) -25);

  End;

  If Filename <> '' Then Begin
     If FileExists(Filename) Then DeleteFile(Filename);
     If Not FileExists(Filename) Then
        SaveFile
     Else
        MessageBox(Handle, pChar('Could not save the file'#13+ExtractFilename(Filename)+' - another'#13'application may be using this file.'), pChar('Save Error'), MB_OK or MB_ICONWARNING);
  End;

End;

Procedure TTapeWindow.TapeToBSD(Block: String);
Begin

  // Saves a Character Array: or Number Array: block out to a file as a .bsd file.
  // No other filetypes are allowed - we need a context to hang it on when we reload.
  // Again, trivial to do, as the .tap block is very similar in format to the .bsd format.

  FileBody := Copy(Block, 4, 17) + Copy(Block, 25, Length(Block) -25);

  Filename := OpenFile(Handle, 'Save as DATA file...', [FTBsd], '', True, False);
  If Filename <> '' Then Begin
     If FileExists(Filename) Then DeleteFile(Filename);
     If Not FileExists(Filename) Then
        SaveFile
     Else
        MessageBox(Handle, pChar('Could not save the file'#13+ExtractFilename(Filename)+' - another'#13'application may be using this file.'), pChar('Save Error'), MB_OK or MB_ICONWARNING);
  End;

End;

procedure TTapeWindow.SaveImage1Click(Sender: TObject);
begin
  SaveTape(TapeFilename);
end;

procedure TTapeWindow.SaveImageAs1Click(Sender: TObject);
begin
  SaveTape('');
end;

procedure TTapeWindow.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TTapeWindow.Button4Click(Sender: TObject);
begin

  // Open the Tape Block properties dialog

  If ListView1.Selected <> nil Then Begin
     CentreForm(BlockProperties, Left + Width, Top + Height);
     ShowWindow(BlockProperties, true);
     UpdateTapeList;
  End;

end;

procedure TTapeWindow.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin

  If UpdatingList Then Exit;

  If ListView1.Selected <> nil Then Begin

     Button2.enabled := True;
     Button4.Enabled := True;
     BitBtn1.Enabled := True and (ListView1.Selected.Index > 0);
     BitBtn2.Enabled := True and (ListView1.Selected.Index < ListView1.Items.Count -1);

  End Else Begin

     Button2.enabled := False;
     Button4.Enabled := False;
     BitBtn1.Enabled := False;
     BitBtn2.Enabled := False;

  End;

end;

procedure TTapeWindow.SaveAs1Click(Sender: TObject);
Var
  Block: String;
begin

  // Saves the current Block depending upon it's type to a file.

  If ListView1.Selected <> nil Then Begin

     Block := TapeBlocks[ListView1.Selected.Index];
     Case Block[4] of
        #0: // Program, save a .bas file
           TapeToBAS(Block);
        #1: // Numeric Array, save as a .bsd file
           TapeToBSD(Block);
        #2: // Character Array, save as a .bsd file also
           TapeToBSD(Block);
        #3: // Bytes block, save as .bsc/.scr/*.*
           TapeToBSC(Block);
     End;

  End;

end;

procedure TTapeWindow.OnEnterMenuLoop(var Message: TMessage);
Begin

  // Set the menu items enabled states depending on selections etc.

  SaveImage1.Enabled := False;
  SaveImageAs1.Enabled := False;
  CutBlock1.Enabled := False;
  CopyBlock1.Enabled := False;
  PasteBlock1.Enabled := False;
  DeleteBlock1.Enabled := False;
  Properties1.Enabled := False;
  SaveAs1.Enabled := False;
  MoveUp1.Enabled := False;
  MoveDown1.Enabled := False;

  If ListView1.Items.Count > 0 Then Begin

     SaveImage1.Enabled := True;
     SaveImageAs1.Enabled := True;

     If ListView1.Selected <> nil Then Begin

        CutBlock1.Enabled := True;
        CopyBlock1.Enabled := True;
        DeleteBlock1.Enabled := True;
        Properties1.Enabled := True;
        SaveAs1.Enabled := True;
        MoveUp1.Enabled := True and (ListView1.Selected.Index > 0);
        MoveDown1.Enabled := True and (ListView1.Selected.Index < ListView1.Items.Count -1);

     End;

  End;

  PasteBlock1.Enabled := TapeClip <> '';

  RedirectLOADCommands1.Checked := TapeTrapLOAD;
  RedirectSAVECommands1.Checked := TapeTrapSAVE;

End;

procedure TTapeWindow.CutBlock1Click(Sender: TObject);
begin

  TapeClip := TapeBlocks[ListView1.Selected.Index];
  TapeBlocks.Delete(ListView1.Selected.Index);

  UpdateTapeList;

end;

procedure TTapeWindow.CopyBlock1Click(Sender: TObject);
begin

  TapeClip := TapeBlocks[ListView1.Selected.Index];

end;

procedure TTapeWindow.PasteBlock1Click(Sender: TObject);
begin

  If ListView1.Selected <> nil Then
     TapeBlocks.Insert(ListView1.Selected.Index, TapeClip)
  Else
     TapeBlocks.Add(TapeClip);

  UpdateTapeList;

end;

procedure TTapeWindow.RedirectLOADcommands1Click(Sender: TObject);
begin
  TapeTrapLOAD := Not TapeTrapLOAD;
  RedirectLOADCommands1.Checked := TapeTrapLOAD;
end;

procedure TTapeWindow.RedirectSAVEcommands1Click(Sender: TObject);
begin
  TapeTrapSAVE := Not TapeTrapSAVE;
  RedirectSAVECommands1.Checked := TapeTrapSAVE;
end;

procedure TTapeWindow.ListView1DblClick(Sender: TObject);
begin

  // Move the tape header position.

  If ListView1.Selected <> nil Then Begin
     TapePosition := ListView1.Selected.Index;
     UpdateTapeList;
  End;

end;

procedure TTapeWindow.FormCreate(Sender: TObject);
begin
  if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;
  Button3.SetBounds(8, ClientHeight - 8 - Button3.Height, Button3.Width, Button3.Height);
  Button1.SetBounds(Button3.Left + Button3.Width + 8, Button3.Top, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left + Button1.Width + 4, Button1.Top, Button2.Width, Button2.Height);
  Button4.SetBounds(Button2.Left + Button2.Width + 4, Button2.Top, Button4.Width, Button4.Height);
  Button5.SetBounds(Button4.Left + Button4.Width + 4, Button4.Top, Button4.Width, Button4.Height);

  BitBtn2.SetBounds(ClientWidth - BitBtn2.Width - 8, Button3.Top, BitBtn2.Width, BitBtn2.Height);
  BitBtn1.SetBounds(BitBtn2.Left - BitBtn1.Width - 4, Button3.Top, BitBtn1.Width, BitBtn1.Height);

  Label1.SetBounds(8, 8, Label1.Width, Label1.Height);
  ListView1.SetBounds(8, Label1.Top + Label1.Height + 8, ClientWidth - 16, ClientHeight - Button3.Height - Label1.Height - 32);

  DragAcceptFiles(Handle, True);
end;

procedure TTapeWindow.TapeCreatorHelp1Click(Sender: TObject);
begin
  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_tape_creator.html'), HH_DISPLAY_TOPIC, 0);
end;

procedure TTapeWindow.Button5Click(Sender: TObject);
var TempTrap: boolean;
begin
if ListView1.Items.Count>0 then Begin
        TapeTrapLOAD:=true;
        If Not Registers.EmuRunning Then Begin
              TempTrap := TapeTrapLoad;
              TapeTrapLoad := False;
              LOADQuoteQuote('');
              TapeTrapLoad := TempTrap;
        End;
End;
 
end;

procedure TTapeWindow.ExportTape;  // Quick and dirty workaround for exporting tap from Main Screen. --arda
Var
Idx:Integer;
tempTrap:Boolean;
NewFilename:String;
begin
Idx:= TapeBlocks.Count;
if Idx>0 then Begin
  //Idx := MessageDlg('Image file already stored in Tape Creator will be lost.'+#13+'Please use Tools>Tape Creator for exporting tapes.'+#13+#13+'Continue?', mtWarning, [mbYes, mbCancel], 0);
  case ShowCustomDlg('Image file already stored in Tape Creator will be lost.'+#13+'Please use Tools>Tape Creator for exporting tapes.'+#13+#13+'Continue?', mtWarning, 'Clear Tape Window', 'Cancel', '') of
     1: Idx :=  10;
     2: Idx :=  0;
  end;

  if Idx<>10 then Exit;
End;

TapeBlocks.Clear;
UpdateTapeList;

  FileHeader := #$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80#$80;
  SaveBAS(False, False);
  SetLength(FileArray, Length(FileBody));
  CopyMemory(@FileArray[0], @FileBody[1], Length(FileBody));
  DecodeBAS;
  NewFilename := Copy(CurProjectName, 1, 10);
  While Length(NewFilename) < 10 Do NewFilename := NewFilename + ' ';
  TapeBlockAdd(BASToTape(NewFilename, GetWord(@FileHeader[$D])));
  UpdateTapeList;


SaveTape('');

TapeBlocks.Clear;
UpdateTapeList;


end;

procedure TTapeWindow.FormDestroy(Sender: TObject);
begin
    DragAcceptFiles(Handle, False);
end;

Initialization

TapeClip := '';
TapeBlocks := TStringlist.Create;
TapeTrapLOAD := False;
TapeTrapSAVE := False;
TapePosition := 0;
TapeFilename := '';

end.
