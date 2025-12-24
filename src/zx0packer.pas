unit zx0packer;

interface

uses
SysUtils;

type
  TZX0ByteArray = array of Byte;

//==============================================================================
//  ANA FONKSIYON
//==============================================================================
// in: a byte array
// out: another byte array
// if fails, returns empty byte array
//==============================================================================
function PackZX0(const InputData: TZX0ByteArray): TZX0ByteArray;


implementation

//------------------------------------------------------------------------------
//  uses zx0_32.dll functions, dll specially compiled by Arda for BasinC but can be used anywhere
//  see implementation
//------------------------------------------------------------------------------
function ZX0_Compress(input_data: PByte; input_size: Integer; var output_size: Integer): PByte; cdecl; external 'zx0_32.dll';
procedure ZX0_Free(ptr: Pointer); cdecl; external 'zx0_32.dll';


//==============================================================================
//  PackZX0
//==============================================================================
function PackZX0(const InputData: TZX0ByteArray): TZX0ByteArray;
var
  pCompressedData: PByte;
  pInputData: PByte;
  InputSize: Integer;
  CompressedSize: Integer;
begin
  // default result is
  Result := nil;

  InputSize := Length(InputData);

  // if nothing to compress.
  if InputSize = 0 then
    Exit;


  // we need the pointer to the array so we can pass it on to DLL.
  pInputData := @InputData[0];

  pCompressedData := nil; // prepare output
  try
    // call compress function from DLL
    pCompressedData := ZX0_Compress(pInputData, InputSize, CompressedSize);

    // if it's successfull, pCompressedData should be an address
    // and CompressedSize would be > 0
    if Assigned(pCompressedData) and (CompressedSize > 0) then
    begin
      // prepare return value
      SetLength(Result, CompressedSize);

      // copy data now
      Move(pCompressedData^, Result[0], CompressedSize);
    end;
  finally
    // free memory to prevent leaks
    if Assigned(pCompressedData) then
      ZX0_Free(pCompressedData);
  end;
end;

end.
