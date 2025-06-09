unit CompressionUnit;

interface

uses
  Windows, SysUtils;

type
  TBytes = array of Byte;

procedure CompressData(const Scr: array of Byte; var compressedScr: TBytes);

implementation

procedure CompressData(const Scr: array of Byte; var compressedScr: TBytes);
var
  SourceFileName, OutputFileName: string;
begin
  // Writing the data to a temporary file
  SourceFileName := 'temp.dat';
  with TFileStream.Create(SourceFileName, fmCreate) do
  begin
    try
      WriteBuffer(Scr[0], Length(Scr));
    finally
      Free;
    end;
  end;

  // Executing pack.exe with the temporary file as input
  OutputFileName := ChangeFileExt(SourceFileName, '.pck');
  if ShellExecute(0, 'open', 'pack.exe', PChar('"' + SourceFileName + '"'), nil, SW_HIDE) <= 32 then
  begin
    // Handle error if pack.exe couldn't be executed
    raise Exception.Create('Error executing pack.exe');
  end;

  // Reading the compressed data from the output file created by pack.exe
  with TFileStream.Create(OutputFileName, fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(compressedScr, Size);
      ReadBuffer(compressedScr[0], Size);
    finally
      Free;
    end;
  end;

  // Clean up temporary and output files
  DeleteFile(SourceFileName);
  DeleteFile(OutputFileName);
end;

end.

