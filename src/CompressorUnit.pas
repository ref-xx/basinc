unit CompressorUnit;

interface

uses
  BlockUnit, OptimizerUnit, Utility;

type

  TIntegerArray = array of Integer;

  TCompressor = class


  private
    FOutput: TByteArray;
    FOutputIndex: Integer;
    FInputIndex: Integer;
    FBitIndex: Integer;
    FBitMask: Integer;
    FDiff: Integer;
    FBacktrack: Boolean;
  private
    procedure ReadBytes(n: Integer; var delta: Array of Integer );
    procedure WriteByte(value: Integer);
    procedure WriteBit(value: Integer);
    procedure WriteInterlacedEliasGamma(value: Integer; backwardsMode, invertMode: Boolean);


  public
    //function CompressX(optimal: TBlock; skip: Integer; backwardsMode, invertMode: Boolean; var delta: Array of Integer ; var input: Array of Byte): TByteArray;
     Procedure Compress(optimal: TBlock;  skip: Integer; backwardsMode, invertMode: Boolean;  input: Array of Byte; delta: TIntegerArray);

  end;

implementation

procedure TCompressor.ReadBytes(n: Integer; var delta: Array of Integer );
begin
  FInputIndex := FInputIndex + n;
  FDiff := FDiff + n;
  if delta[0] < FDiff then
    delta[0] := FDiff;
end;


procedure TCompressor.WriteByte(value: Integer);
begin
  FOutput[FOutputIndex] := value and $FF; // Masking with $FF to ensure only the lower byte is kept
  Inc(FOutputIndex);
  Dec(FDiff);
end;

procedure TCompressor.WriteBit(value: Integer);
begin
  if FBacktrack then
  begin
    if value > 0 then
      FOutput[FOutputIndex - 1] := FOutput[FOutputIndex - 1] or 1;
    FBacktrack := False;
  end
  else
  begin
    if FBitMask = 0 then
    begin
      FBitMask := 128;
      FBitIndex := FOutputIndex;
      WriteByte(0);
    end;
    if value > 0 then
      FOutput[FBitIndex] := FOutput[FBitIndex] or FBitMask;
    FBitMask := FBitMask shr 1;
  end;
end;



procedure TCompressor.WriteInterlacedEliasGamma(value: Integer; backwardsMode, invertMode: Boolean);
var
  i: Integer;
begin
  i := 2;
  while i <= value do
    i := i shl 1;
  i := i shr 1;
  while (i shr 1) > 0 do
  begin
    WriteBit(Ord(backwardsMode));
    WriteBit(Ord(invertMode = ((value and i) = 0)));
    i := i shr 1;
  end;
  WriteBit(Ord(not backwardsMode));
end;


//function TCompressor.CompressX(optimal: TBlock; skip: Integer; backwardsMode, invertMode: Boolean; var delta: Array of Integer ; var input: Array of Byte): TByteArray;

//compressor proc -> input: zx0input array. output: zx0output array (see MemManager Vars)
Procedure TCompressor.Compress(optimal: TBlock;  skip: Integer; backwardsMode, invertMode: Boolean;  input: Array of Byte; delta: TIntegerArray);
var
  i, lastOffset, diff, inputIndex, outputIndex, bitMask, blength: Integer;
  backtrack: Boolean;
  prev, next: TBlock;
begin
  lastOffset := INITIAL_OFFSET;

  // Calculate and allocate output buffer
  SetLength(FOutput, (optimal.GetBits + 25) div 8);

  // Un-reverse optimal sequence
  prev := nil;
  while optimal <> nil do
  begin
    next := optimal.GetChain;
    optimal.SetChain(prev);
    prev := optimal;
    optimal := next;
  end;

  // Initialize data
  diff := Length(FOutput) - Length(input) + skip;
  delta[0] := 0;
  inputIndex := skip;
  outputIndex := 0;
  bitMask := 0;
  backtrack := True;

  // Generate output
  prev := prev.GetChain;
  optimal := prev; // Start from prev's chain

  while Assigned(optimal) do
  begin
    // Calculate the length between prev and optimal
    blength := optimal.GetIndex - prev.GetIndex;

    // Check if the offset is 0
    if optimal.GetOffset = 0 then
    begin
      // Copy literals indicator
      WriteBit(0);

      // Copy literals length
      WriteInterlacedEliasGamma(blength, backwardsMode, False);

      // Copy literals values
      for i := 0 to blength - 1 do
      begin
        WriteByte(input[FInputIndex]);
        ReadBytes(1, delta);
      end;
    end;

    // Move to the next block
    prev := optimal;
    optimal := optimal.GetChain;
  end;


end;


end.

