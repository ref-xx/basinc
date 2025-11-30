unit OptimizerUnit;

interface

uses
  Math, BlockUnit, Utility; // Import the unit where the Block class is defined

const
  INITIAL_OFFSET = 1;
  MAX_SCALE = 50;

type
  
  TOptimizer = class


  private
    FLastLiteral: array of TBlock;
    FLastMatch: array of TBlock;
    FOptimal: array of TBlock;
    FMatchLength: array of Integer;
    FBestLength: array of Integer;
  public

    function EliasGammaBits(value: Integer): Integer;
    function OffsetCeiling(index, offsetLimit: Integer): Integer;
    function ProcessTask(initialOffset, finalOffset, index, skip: Integer; input: TByteArray): TBlock;
    Function Optimize( skip, offsetLimit: Integer): TBlock;

  end;

var
  zx0input,
  zx0output:           Array[0..65535] of Byte;
  zx0Status,
  zx0iLength,
  zx0iPointer,
  zx0oLength,
  zx0oPointer:          Integer;

implementation

function TOptimizer.ProcessTask(initialOffset, finalOffset, index, skip: Integer; input: TByteArray): TBlock;
var
  bestLengthSize, offset, bits, bits2, length: Integer;
  optimalBlock: TBlock;
begin
  bestLengthSize := 2;
  optimalBlock := nil;
for offset := initialOffset to finalOffset do
  begin
    if (index <> skip) and (index >= offset) and (input[index] = input[index-offset]) then
    begin
        if (Assigned(FLastLiteral[offset])) then
            begin
              length := index - FLastLiteral[offset].Index;
              bits := FLastLiteral[offset].Bits + 1 + EliasGammaBits(length);
              FLastMatch[offset] := TBlock.Create(bits, index, offset, FLastLiteral[offset]);
              if not Assigned(optimalBlock) or (optimalBlock.Bits > bits) then
              begin
                optimalBlock := FLastMatch[offset];
              end;
            end;

        Inc(FMatchLength[offset]);
        if FMatchLength[offset] > 1 then
        begin
          if bestLengthSize < FMatchLength[offset] then
          begin
            bits := FOptimal[index - FBestLength[bestLengthSize]].Bits + EliasGammaBits(FBestLength[bestLengthSize] - 1);
            repeat
              Inc(bestLengthSize);
              bits2 := FOptimal[index - FBestLength[bestLengthSize]].Bits + EliasGammaBits(FBestLength[bestLengthSize] - 1);
              if bits2 <= bits then
              begin
                FBestLength[bestLengthSize] := bestLengthSize;
                bits := bits2;
              end
              else
              begin
                FBestLength[bestLengthSize] := FBestLength[bestLengthSize - 1];
              end;
            until bestLengthSize >= FMatchLength[offset];
          end;
          length := FBestLength[FMatchLength[offset]];
          bits := FOptimal[index - length].Bits + 8 + EliasGammaBits((offset - 1) div 128 + 1) + EliasGammaBits(length - 1);
          if not Assigned(FLastMatch[offset]) or (FLastMatch[offset].Index <> index) or (FLastMatch[offset].Bits > bits) then
          begin
            FLastMatch[offset] := TBlock.Create(bits, index, offset, FOptimal[index - length]);
            if not Assigned(optimalBlock) or (optimalBlock.Bits > bits) then
              optimalBlock := FLastMatch[offset];
          end;
        end;


    end
    else
    begin

          FMatchLength[offset] := 0;
          if (Assigned(FLastMatch[offset])) then
          begin
            length := index - FLastMatch[offset].Index;
            bits := FLastMatch[offset].Bits + 1 + EliasGammaBits(length) + length * 8;
            FLastLiteral[offset] := TBlock.Create(bits, index, 0, FLastMatch[offset]);
            if not Assigned(optimalBlock) or (optimalBlock.Bits > bits) then
            begin
              optimalBlock := FLastLiteral[offset];
            end;
          end;

    end;
  end;
  Result := optimalBlock;

end;


function TOptimizer.OffsetCeiling(index, offsetLimit: Integer): Integer;
begin
  Result := Min(Max(index, INITIAL_OFFSET), offsetLimit);
end;


function TOptimizer.EliasGammaBits(value: Integer): Integer;
var
  bits: Integer;
begin
  bits := 1; // Initialize bits to 1

  // Calculate the number of bits required
  while value > 1 do
  begin
    bits := bits + 2; // Increment bits by 2
    value := value shr 1; // Right shift value by 1 (divide by 2)
  end;

  Result := bits; // Return the final number of bits required
end;

function TOptimizer.Optimize( skip, offsetLimit: Integer): TBlock;
var             //optimize(input TByteArray, 0,   2176 ya da 32640)
  arraySize, dots, index, maxOffset: Integer;
  taskBlock: TBlock;
  input: TByteArray;
begin
  input:=ArrayToTBytes(zx0input,zx0iLength);
  // Allocate all main data structures
  arraySize := OffsetCeiling(Length(input) - 1, offsetLimit) + 1;
  SetLength(FLastLiteral, arraySize);
  SetLength(FLastMatch, arraySize);
  SetLength(FOptimal, Length(input));
  SetLength(FMatchLength, arraySize);
  SetLength(FBestLength, Length(input));
  if Length(FBestLength) > 2 then
    FBestLength[2] := 2;

  // Start with fake block
  FLastMatch[INITIAL_OFFSET] := TBlock.Create(-1, skip - 1, INITIAL_OFFSET, nil);

  dots := 2;
  for index := skip to Length(input) - 1 do
  begin
    maxOffset := OffsetCeiling(index, offsetLimit);
    taskBlock := ProcessTask(1, maxOffset, index, skip, input);
    if (taskBlock <> nil) and ((FOptimal[index] = nil) or (FOptimal[index].GetBits > taskBlock.GetBits)) then
      FOptimal[index] := taskBlock;
  end;

  Result := FOptimal[High(input)];
end;




end.

