unit RLEUnit;

interface

uses      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
          StdCtrls, ComCtrls;

Procedure BuildBinFile(Filename: String; Files: TStrings);
Function  ListCompressedFiles(Filename: String): TStringList;
Function  RLEPackArray(Var TempArray: Array of Byte): String;
Function  RLEPackFile(Filename: String): String;
Procedure RLEUnpackFile(ArchiveName, FileName: String);
Procedure RLEUnpackToFile(ArchiveName, PackedFilename, FileName: String);

var       RLEArray: Array of Byte;

implementation

Uses Filing;

Procedure BuildBinFile(Filename: String; Files: TStrings);
Var
  BinFile: TFileStream;
  F: integer;
  Compressed, TotalFile, FName, Header: String;
  FLen: DWord;
begin
  Header := Chr(Files.Count);
  TotalFile := '';
  If GetFileAttributes(Pchar(Filename)) <> $FFFFFFFF Then DeleteFile(Filename);
  BinFile := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
  For F := 0 To Files.Count -1 Do Begin
     FName := ExtractFileName(Files[F]);
     While Length(FName) < 10 Do FName := FName + ' ';
     Compressed := RLEPackFile(Files[F]);
     FLen := Length(Compressed);
     Header := Header + FName + Chr(FLen and 255)+Chr((FLen Shr 8) And 255)+Chr((FLen Shr 16) And 255)+Chr((FLen Shr 24) And 255);
     TotalFile := TotalFile + Compressed;
  End;
  TotalFile := Header+TotalFile;
  BinFile.Write(TotalFile[1], Length(TotalFile));
  BinFile.Free;
End;

Function RLEPackArray(Var TempArray: Array of Byte): String;
Var
  MinTagCount, ArrayPos, RepeatCount, BytesRead, UnCompressedSize, PackedSize, F: DWord;
  CurByte, TagByte, RepeatByte: Byte;
  PossibleTags: Array[0..255] of DWord;
Begin
  UnCompressedSize := Length(TempArray);
  MinTagCount := $FFFFFFFF;
  FillChar(PossibleTags, SizeOf(PossibleTags), 0);
  If UnCompressedSize > 0 Then
     For F := 0 To UnCompressedSize -1 Do
        Inc(PossibleTags[TempArray[F]]);
  For F := 0 To 255 Do If PossibleTags[F] < MinTagCount Then MinTagCount := PossibleTags[F];
  F := 0; While PossibleTags[F] <> MinTagCount Do Inc(F); TagByte := F;
  SetLength(RLEArray, (MinTagCount*6)+UnCompressedSize+1);
  PackedSize := 1;
  BytesRead := 0;
  ArrayPos := 1;
  RLEArray[0] := TagByte;
  While BytesRead < UnCompressedSize Do Begin
     CurByte := TempArray[BytesRead];
     Inc(BytesRead);
     RepeatByte := CurByte;
     RepeatCount := 0;
     While (RepeatByte = CurByte) And (BytesRead <= UnCompressedSize) Do Begin
        CurByte := TempArray[BytesRead];
        Inc(RepeatCount);
        Inc(BytesRead);
     End;
     If (RepeatCount >= 6) Or (RepeatByte = TagByte) Then Begin
        RLEArray[ArrayPos] := TagByte;
        RLEArray[ArrayPos+1] := RepeatByte;
        RLEArray[ArrayPos+2] := RepeatCount And 255;
        RLEArray[ArrayPos+3] := (RepeatCount Shr 8) And 255;
        RLEArray[ArrayPos+4] := (RepeatCount Shr 16) And 255;
        RLEArray[ArrayPos+5] := (RepeatCount Shr 24) And 255;
        Inc(ArrayPos, 6);
        Inc(PackedSize, 6);
     End Else Begin
        For F := 1 To RepeatCount Do Begin
           RLEArray[ArrayPos] := RepeatByte;
           Inc(ArrayPos);
        End;
        Inc(PackedSize, RepeatCount);
     End;
     Dec(BytesRead);
  End;
  Result := Chr(UnCompressedSize And 255) + Chr((UnCompressedSize Shr 8) And 255) + Chr((UnCompressedSize Shr 16) And 255) + Chr((UnCompressedSize Shr 24) And 255);
  For F := 0 To PackedSize -1 Do Result := Result + Chr(RLEArray[F]);
  SetLength(RLEArray, 0);
End;

Function RLEPackFile(Filename: String): String;
Var
  UnpackedFile: TFileStream;
  UnCompressedSize: DWord;
  TempArray: Array of Byte;
Begin
  UnpackedFile := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  UnCompressedSize := UnpackedFile.Size;
  SetLength(TempArray, UnCompressedSize);
  UnPackedFile.Read(TempArray[0], UnCompressedSize);
  UnpackedFile.Free;
  Result := RLEPackArray(TempArray);
  SetLength(TempArray, 0);
End;

Function ListCompressedFiles(Filename: String): TStringList;
Var
  BinFile: TFileStream;
  NumFiles, F: Byte;
  Listname: String;
Begin
  Result := TStringlist.Create;
  BinFile := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  BinFile.Read(NumFiles, 1);
  For F := 1 To NumFiles Do Begin
     ListName := '              ';
     BinFile.Read(Listname[1], 14);
     ListName := Copy(ListName, 1, 10);
     While ListName[Length(ListName)] = ' ' Do ListName := Copy(ListName, 1, Length(ListName)-1);
     Result.Add(ListName);
  End;
  BinFile.Free;
End;

Procedure RLEUnpackFile(ArchiveName, FileName: String);
Var
  BinFile: TFileStream;
  F, CurByte, RepeatByte, NumFiles, TagByte: Byte;
  Offset, Size, FileSize, UnpackPos, RepeatLength, BytesRead: DWord;
  FName: String;
Begin
  FName := '          ';
  Filename := Lowercase(Filename);
  BinFile := TFileStream.Create(ArchiveName, fmOpenRead or fmShareDenyNone);
  BinFile.Read(NumFiles, 1);
  Offset := (NumFiles*14)+1;
  For F := 1 To NumFiles Do Begin
     BinFile.Read(FName[1], 10);
     BinFile.Read(Size, 4);
     If Copy(Lowercase(FName), 1, Length(Filename)) = Filename Then Break;
     Inc(Offset, Size);
  End;
  Dec(Size);
  BinFile.Seek(Offset, soFromBeginning);
  BinFile.Read(FileSize, 4);
  BinFile.Read(TagByte, 1);
  SetLength(RLEArray, FileSize);
  UnpackPos := 0;
  BytesRead := 0;
  While (BytesRead < Size) and (UnpackPos < FileSize) Do Begin
     BinFile.Read(CurByte, 1);
     Inc(BytesRead);
     If CurByte = TagByte Then Begin
        BinFile.Read(RepeatByte, 1);
        BinFile.Read(RepeatLength, 4);
        While RepeatLength > 0 Do Begin
           RLEArray[UnpackPos] := RepeatByte;
           Inc(UnpackPos);
           Dec(RepeatLength);
        End;
        Inc(BytesRead, 5);
     End Else Begin
        RLEArray[UnpackPos] := CurByte;
        Inc(UnpackPos);
     End;
  End;
  BinFile.Free;
End;

Procedure RLEUnpackToFile(ArchiveName, PackedFilename, FileName: String);
Var
  WriteFile: TFileStream;
begin
  RLEUnpackFile(ArchiveName, PackedFileName);
  If GetFileAttributes(Pchar(Filename)) <> $FFFFFFFF Then DeleteFile(FileName);
  If Not OpenFileStream(WriteFile, fmCreate or fmShareDenyWrite, Filename) Then Exit;
  WriteFile.Write(RLEArray[0], Length(RLEArray));
  WriteFile.Free;
  SetLength(RLEArray, 0);
end;

end.
