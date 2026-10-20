unit CheqMap;

{ The document has one character and one Spectrum attribute per cell.
  GRAPH is a brush, not a second storage format. Its four pixels map to
  the ROM's 128..143 characters (TR=1, TL=2, BR=4, BL=8). }

interface

uses SysUtils, Classes, Types;

type
  TCheqSpaceMethod = (csLiteral, csAT, csTAB, csControlAT, csControlTAB);
  TCheqBridgeMethod = (cbCharacters, cbPairs, cbPlanes, cbColorChanges,
    cbColors, cbHiResDecimal, cbHiResBinary);
  TCheqCompressionMethod = (cmAutomatic, cmLetters, cmGraph, cmGraphCut,
    cmColorStep2);
  TCheqExportOptions = record
    Start, Step: Integer;
    SkipEmpty, InitColors, Colored, Ink, Paper, Bright, Flash: Boolean;
    ControlColors, UseCHR, Compressed, JoinFull: Boolean;
    CompressionMethod: TCheqCompressionMethod;
    CompressionChunkSize: Integer;
    CompressionDataOnly: Boolean;
    SpaceMethod: TCheqSpaceMethod;
    PageAttribute: Byte;
  end;
  TCheqMap = class
  private
    FWidth, FHeight: Integer;
    FCharacters, FAttributes: AnsiString;
    FPageAttribute: Byte;
    FColored: Boolean;
    FUndo, FRedo: TStringList;
    FBefore, FSaved: AnsiString;
    FEditing: Boolean;
    function GetCharacter(X, Y: Integer): Byte;
    function GetAttribute(X, Y: Integer): Byte;
    function GetGraphPixel(X, Y: Integer): Boolean;
    procedure PutGraphPixel(X, Y: Integer; Value: Boolean);
    function GetDirty: Boolean;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    procedure Restore(const State: AnsiString);
    procedure TrimHistory(List: TStringList);
    procedure GenerateCompressed(const Options: TCheqExportOptions;
      Lines: TStrings; var LineNo: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure NewMap(AWidth, AHeight: Integer);
    procedure Resize(AWidth, AHeight: Integer);
    procedure BeginEdit;
    procedure EndEdit;
    procedure CancelEdit;
    procedure Undo;
    procedure Redo;
    procedure MarkSaved;
    function Snapshot: AnsiString;
    procedure SetCell(X, Y: Integer; Character, Attribute: Byte);
    procedure Paint(X, Y, Brush: Integer; Erase, PaintEnabled: Boolean;
      Ink, Paper, Bright, Flash: Integer);
    procedure SetPageColors(Attribute: Byte; Recolor: Boolean);
    procedure SetColored(Value: Boolean);
    procedure Clear(const Area: TRect);
    procedure Invert(const Area: TRect);
    procedure Crop(const Area: TRect);
    function CopyArea(const Area: TRect): AnsiString;
    function ExtractArea(const Area: TRect): TCheqMap;
    procedure PasteArea(const Data: AnsiString; X, Y: Integer);
    function IsGraph: Boolean;
    function AreaIsGraph(const Area: TRect): Boolean;
    function NormalizeSelectionPixels(const Area: TRect; FineGrid: Boolean): TRect;
    procedure ShiftPixels(const Area: TRect; DX, DY: Integer);
    procedure EditGraphPixels(const Area: TRect; InvertPixels: Boolean);
    function CopyPixels(const Area: TRect): AnsiString;
    procedure PastePixels(const Data: AnsiString; X, Y: Integer);
    function IsEmpty: Boolean;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    function BridgeData(Method: TCheqBridgeMethod): AnsiString;
    procedure GenerateBasic(const Options: TCheqExportOptions; Lines: TStrings);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Characters[X, Y: Integer]: Byte read GetCharacter;
    property Attributes[X, Y: Integer]: Byte read GetAttribute;
    property GraphPixels[X, Y: Integer]: Boolean read GetGraphPixel;
    property PageAttribute: Byte read FPageAttribute;
    property Colored: Boolean read FColored;
    property Dirty: Boolean read GetDirty;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  end;

function CheqAttribute(Ink, Paper, Bright, Flash: Integer): Byte;
function CheqApplyAttribute(Original: Byte; Ink, Paper, Bright, Flash: Integer): Byte;
function CheqDefaultExportOptions: TCheqExportOptions;
function CheqCellArea(const PixelArea: TRect): TRect;

const
  CheqMaxCells = 256 * 256;

implementation

function CheqAttribute(Ink, Paper, Bright, Flash: Integer): Byte;
begin
  Result := (Ink and 7) or ((Paper and 7) shl 3) or
    ((Bright and 1) shl 6) or ((Flash and 1) shl 7);
end;

function CheqApplyAttribute(Original: Byte; Ink, Paper, Bright, Flash: Integer): Byte;
begin
  Result := Original;
  if Ink >= 0 then Result := (Result and $F8) or (Ink and 7);
  if Paper >= 0 then Result := (Result and $C7) or ((Paper and 7) shl 3);
  if Bright >= 0 then Result := (Result and $BF) or ((Bright and 1) shl 6);
  if Flash >= 0 then Result := (Result and $7F) or ((Flash and 1) shl 7);
end;

function CheqDefaultExportOptions: TCheqExportOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Start := 10;
  Result.Step := 10;
  Result.SkipEmpty := True;
  Result.InitColors := True;
  Result.Ink := True;
  Result.Paper := True;
  Result.Bright := True;
  Result.Flash := True;
  Result.JoinFull := True;
  Result.CompressionChunkSize := 224;
  Result.SpaceMethod := csAT;
  Result.PageAttribute := 56;
end;

procedure CheckDimensions(W, H: Integer);
begin
  if (W < 1) or (H < 1) or (W > 256) or (H > 256) then
    raise ERangeError.Create('Canvas dimensions must not exceed 256 x 256 cells.');
end;

function PackInt(Value: Integer): AnsiString;
begin
  SetLength(Result, 4);
  Move(Value, Result[1], 4);
end;

function UnpackInt(const S: AnsiString; Index: Integer): Integer;
begin
  if (Index < 1) or (Index + 3 > Length(S)) then
    raise EReadError.Create('Incomplete graph data.');
  Move(S[Index], Result, 4);
end;

function EmptyCharacter(C: Byte): Boolean;
begin
  Result := (C = 32) or (C = 128);
end;

constructor TCheqMap.Create;
begin
  inherited;
  FUndo := TStringList.Create;
  FRedo := TStringList.Create;
  NewMap(32, 24);
end;

destructor TCheqMap.Destroy;
begin
  FUndo.Free;
  FRedo.Free;
  inherited;
end;

procedure TCheqMap.NewMap(AWidth, AHeight: Integer);
begin
  CheckDimensions(AWidth, AHeight);
  FWidth := AWidth;
  FHeight := AHeight;
  FPageAttribute := 56;
  FColored := False;
  FCharacters := StringOfChar(#128, FWidth * FHeight);
  FAttributes := StringOfChar(Chr(FPageAttribute), FWidth * FHeight);
  FUndo.Clear;
  FRedo.Clear;
  FBefore := '';
  FEditing := False;
  MarkSaved;
end;

function TCheqMap.Snapshot: AnsiString;
begin
  Result := PackInt(FWidth) + PackInt(FHeight) + Chr(FPageAttribute) +
    Chr(Ord(FColored)) + FCharacters + FAttributes;
end;

procedure TCheqMap.Restore(const State: AnsiString);
var W, H, N: Integer;
begin
  W := UnpackInt(State, 1);
  H := UnpackInt(State, 5);
  CheckDimensions(W, H);
  N := W * H;
  if Length(State) <> 10 + 2 * N then
    raise EReadError.Create('Invalid graph data length.');
  FWidth := W;
  FHeight := H;
  FPageAttribute := Ord(State[9]);
  FColored := State[10] <> #0;
  FCharacters := Copy(State, 11, N);
  FAttributes := Copy(State, 11 + N, N);
end;

procedure TCheqMap.TrimHistory(List: TStringList);
var I: Integer; Total: Int64;
begin
  Total := 0;
  for I := 0 to List.Count - 1 do Total := Total + Length(List[I]);
  while (List.Count > 64) or ((Total > 32 * 1024 * 1024) and (List.Count > 1)) do
  begin
    Total := Total - Length(List[0]);
    List.Delete(0);
  end;
end;

procedure TCheqMap.BeginEdit;
begin
  if FEditing then Exit;
  FBefore := Snapshot;
  FEditing := True;
end;

procedure TCheqMap.EndEdit;
begin
  if not FEditing then Exit;
  if FBefore <> Snapshot then
  begin
    FUndo.Add(FBefore);
    TrimHistory(FUndo);
    FRedo.Clear;
  end;
  FBefore := '';
  FEditing := False;
end;

procedure TCheqMap.CancelEdit;
begin
  if FEditing then Restore(FBefore);
  FBefore := '';
  FEditing := False;
end;

procedure TCheqMap.Undo;
begin
  EndEdit;
  if not CanUndo then Exit;
  FRedo.Add(Snapshot);
  Restore(FUndo[FUndo.Count - 1]);
  FUndo.Delete(FUndo.Count - 1);
end;

procedure TCheqMap.Redo;
begin
  if not CanRedo then Exit;
  FUndo.Add(Snapshot);
  TrimHistory(FUndo);
  Restore(FRedo[FRedo.Count - 1]);
  FRedo.Delete(FRedo.Count - 1);
end;

procedure TCheqMap.MarkSaved;
begin
  FSaved := Snapshot;
end;

function TCheqMap.GetDirty: Boolean;
begin
  Result := FSaved <> Snapshot;
end;

function TCheqMap.GetCanUndo: Boolean;
begin
  Result := FUndo.Count > 0;
end;

function TCheqMap.GetCanRedo: Boolean;
begin
  Result := FRedo.Count > 0;
end;

function TCheqMap.GetCharacter(X, Y: Integer): Byte;
begin
  Result := 128;
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    Result := Ord(FCharacters[Y * FWidth + X + 1]);
end;

function TCheqMap.GetAttribute(X, Y: Integer): Byte;
begin
  Result := FPageAttribute;
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    Result := Ord(FAttributes[Y * FWidth + X + 1]);
end;

procedure TCheqMap.SetCell(X, Y: Integer; Character, Attribute: Byte);
var I: Integer;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then Exit;
  I := Y * FWidth + X + 1;
  FCharacters[I] := Chr(Character);
  FAttributes[I] := Chr(Attribute);
end;

procedure TCheqMap.Paint(X, Y, Brush: Integer; Erase, PaintEnabled: Boolean;
  Ink, Paper, Bright, Flash: Integer);
var CX, CY, C, Mask: Integer; A: Byte;
begin
  CX := X; CY := Y;
  if Brush < 0 then begin CX := X div 2; CY := Y div 2; end;
  if (X < 0) or (Y < 0) or (CX >= FWidth) or (CY >= FHeight) then Exit;
  C := Characters[CX, CY];
  A := Attributes[CX, CY];
  if FColored then A := CheqApplyAttribute(A, Ink, Paper, Bright, Flash);
  if PaintEnabled or not FColored then
    if Brush < 0 then
    begin
      if not (C in [128..143]) then C := 128;
      Mask := 1 shl ((Y mod 2) * 2 + 1 - (X mod 2));
      if Erase then C := 128 + ((C - 128) and (15 xor Mask))
      else C := 128 + ((C - 128) or Mask);
    end
    else if Erase then C := 128 else C := Brush;
  SetCell(CX, CY, C, A);
end;

procedure TCheqMap.SetPageColors(Attribute: Byte; Recolor: Boolean);
var I: Integer;
begin
  BeginEdit;
  if Recolor then
    for I := 1 to Length(FAttributes) do
      if Ord(FAttributes[I]) = FPageAttribute then FAttributes[I] := Chr(Attribute);
  FPageAttribute := Attribute;
  EndEdit;
end;

procedure TCheqMap.SetColored(Value: Boolean);
var OwnEdit: Boolean;
begin
  OwnEdit := not FEditing;
  if OwnEdit then BeginEdit;
  FColored := Value;
  if OwnEdit then EndEdit;
end;

procedure TCheqMap.Resize(AWidth, AHeight: Integer);
var X, Y, W, H: Integer; C, A: AnsiString;
begin
  CheckDimensions(AWidth, AHeight);
  W := FWidth; H := FHeight; C := FCharacters; A := FAttributes;
  BeginEdit;
  FWidth := AWidth; FHeight := AHeight;
  FCharacters := StringOfChar(#128, FWidth * FHeight);
  FAttributes := StringOfChar(Chr(FPageAttribute), FWidth * FHeight);
  for Y := 0 to H - 1 do
  begin
    if Y >= FHeight then Break;
    for X := 0 to W - 1 do
    begin
      if X >= FWidth then Break;
      SetCell(X, Y, Ord(C[Y * W + X + 1]), Ord(A[Y * W + X + 1]));
    end;
  end;
  EndEdit;
end;

function ClipArea(const Area: TRect; W, H: Integer): TRect;
begin
  Result := Area;
  if Result.Left < 0 then Result.Left := 0;
  if Result.Top < 0 then Result.Top := 0;
  if Result.Right > W then Result.Right := W;
  if Result.Bottom > H then Result.Bottom := H;
end;

procedure TCheqMap.Clear(const Area: TRect);
var R: TRect; X, Y: Integer;
begin
  R := ClipArea(Area, FWidth, FHeight);
  BeginEdit;
  for Y := R.Top to R.Bottom - 1 do
    for X := R.Left to R.Right - 1 do SetCell(X, Y, 128, FPageAttribute);
  EndEdit;
end;

function CheqCellArea(const PixelArea: TRect): TRect;
begin
  Result := Rect(PixelArea.Left div 2, PixelArea.Top div 2,
    (PixelArea.Right+1) div 2, (PixelArea.Bottom+1) div 2);
end;

function TCheqMap.AreaIsGraph(const Area: TRect): Boolean;
var R: TRect; X, Y: Integer;
begin
  R := ClipArea(Area,FWidth,FHeight);
  Result := False;
  for Y := R.Top to R.Bottom-1 do for X := R.Left to R.Right-1 do
    if not (Characters[X,Y] in [32,128..143]) then Exit;
  Result := True;
end;

function TCheqMap.NormalizeSelectionPixels(const Area: TRect; FineGrid: Boolean): TRect;
var R: TRect;
begin
  Result := ClipArea(Area,FWidth*2,FHeight*2);
  R := CheqCellArea(Result);
  if not FineGrid or not AreaIsGraph(R) then
    Result := Rect(R.Left*2,R.Top*2,R.Right*2,R.Bottom*2);
end;

function TCheqMap.GetGraphPixel(X, Y: Integer): Boolean;
var C: Integer;
begin
  Result := False;
  if (X<0) or (Y<0) or (X>=FWidth*2) or (Y>=FHeight*2) then Exit;
  C := Characters[X div 2,Y div 2];
  if C in [128..143] then
    Result := ((C-128) and (1 shl ((Y mod 2)*2+1-X mod 2))) <> 0;
end;

procedure TCheqMap.PutGraphPixel(X, Y: Integer; Value: Boolean);
var C, Mask: Integer;
begin
  if (X<0) or (Y<0) or (X>=FWidth*2) or (Y>=FHeight*2) then Exit;
  C := Characters[X div 2,Y div 2];
  if C in [128..143] then Dec(C,128) else C := 0;
  Mask := 1 shl ((Y mod 2)*2+1-X mod 2);
  if Value then C := C or Mask else C := C and (15 xor Mask);
  SetCell(X div 2,Y div 2,128+C,Attributes[X div 2,Y div 2]);
end;

procedure TCheqMap.ShiftPixels(const Area: TRect; DX, DY: Integer);
var R, Cells: TRect; Data: AnsiString;
  X, Y, W, H, SX, SY, N: Integer; Fine: Boolean;
begin
  R := NormalizeSelectionPixels(Area,not FColored); Cells := CheqCellArea(R);
  if (R.Right<=R.Left) or (R.Bottom<=R.Top) then Exit;
  Fine := not FColored and AreaIsGraph(Cells);
  if Fine then
  begin
    W := R.Right-R.Left; H := R.Bottom-R.Top;
    SetLength(Data,W*H);
    for Y := 0 to H-1 do for X := 0 to W-1 do
      Data[Y*W+X+1] := Chr(Ord(GraphPixels[R.Left+X,R.Top+Y]));
  end
  else
  begin
    W := Cells.Right-Cells.Left; H := Cells.Bottom-Cells.Top;
    Data := CopyArea(Cells);
  end;
  if (W<=0) or (H<=0) then Exit;
  BeginEdit;
  for Y := 0 to H-1 do for X := 0 to W-1 do
  begin
    SX := ((X-DX) mod W+W) mod W;
    SY := ((Y-DY) mod H+H) mod H;
    if Fine then PutGraphPixel(R.Left+X,R.Top+Y,Data[SY*W+SX+1]<>#0)
    else
    begin
      N := SY*W+SX;
      SetCell(Cells.Left+X,Cells.Top+Y,Ord(Data[9+N]),Ord(Data[9+W*H+N]));
    end;
  end;
  EndEdit;
end;

procedure TCheqMap.EditGraphPixels(const Area: TRect; InvertPixels: Boolean);
var R: TRect; X, Y: Integer;
begin
  R := ClipArea(Area,FWidth*2,FHeight*2);
  BeginEdit;
  for Y := R.Top to R.Bottom-1 do for X := R.Left to R.Right-1 do
    PutGraphPixel(X,Y,InvertPixels and not GraphPixels[X,Y]);
  EndEdit;
end;

function TCheqMap.CopyPixels(const Area: TRect): AnsiString;
var R: TRect; W, H, X, Y, N: Integer;
begin
  R := ClipArea(Area,FWidth*2,FHeight*2);
  if not AreaIsGraph(CheqCellArea(R)) then raise EConvertError.Create('Pixel selection requires GRAPH characters.');
  W := R.Right-R.Left; H := R.Bottom-R.Top;
  if (W<1) or (H<1) then raise ERangeError.Create('Select at least one GRAPH pixel.');
  N := W*H;
  Result := 'GPX1'+PackInt(W)+PackInt(H)+StringOfChar(#0,N*2);
  for Y := 0 to H-1 do for X := 0 to W-1 do
  begin
    Result[13+Y*W+X] := Chr(Ord(GraphPixels[R.Left+X,R.Top+Y]));
    Result[13+N+Y*W+X] := Chr(Attributes[(R.Left+X) div 2,(R.Top+Y) div 2]);
  end;
end;

procedure TCheqMap.PastePixels(const Data: AnsiString; X, Y: Integer);
var W, H, N, I, J, CX, CY, SX, SY: Integer; R: TRect;
begin
  if Copy(Data,1,4)<>'GPX1' then raise EReadError.Create('Invalid GRAPH clipboard data.');
  W:=UnpackInt(Data,5); H:=UnpackInt(Data,9);
  if (W<1) or (H<1) or (W>512) or (H>512) then raise EReadError.Create('Invalid GRAPH clipboard size.');
  N:=W*H;
  if Length(Data)<>12+N*2 then raise EReadError.Create('Invalid GRAPH clipboard length.');
  BeginEdit;
  for J:=0 to H-1 do for I:=0 to W-1 do PutGraphPixel(X+I,Y+J,Data[13+J*W+I]<>#0);
  R:=CheqCellArea(ClipArea(Rect(X,Y,X+W,Y+H),FWidth*2,FHeight*2));
  for CY:=R.Top to R.Bottom-1 do for CX:=R.Left to R.Right-1 do
    if (CX*2>=X) and (CY*2>=Y) and (CX*2+2<=X+W) and (CY*2+2<=Y+H) then
    begin
      SX:=CX*2-X; SY:=CY*2-Y;
      SetCell(CX,CY,Characters[CX,CY],Ord(Data[13+N+SY*W+SX]));
    end;
  EndEdit;
end;

procedure TCheqMap.Invert(const Area: TRect);
var R: TRect; X, Y, C, A: Integer;
begin
  R := ClipArea(Area, FWidth, FHeight);
  BeginEdit;
  for Y := R.Top to R.Bottom - 1 do
    for X := R.Left to R.Right - 1 do
    begin
      C := Characters[X, Y]; A := Attributes[X, Y];
      if EmptyCharacter(C) then C := 143
      else if C in [128..143] then C := 128 + ((C - 128) xor 15)
      else A := (A and $C0) or ((A and 7) shl 3) or ((A shr 3) and 7);
      SetCell(X, Y, C, A);
    end;
  EndEdit;
end;

function TCheqMap.CopyArea(const Area: TRect): AnsiString;
var R: TRect; Y: Integer; C, A: AnsiString;
begin
  R := ClipArea(Area, FWidth, FHeight);
  CheckDimensions(R.Right - R.Left, R.Bottom - R.Top);
  C := ''; A := '';
  for Y := R.Top to R.Bottom - 1 do
  begin
    C := C + Copy(FCharacters, Y * FWidth + R.Left + 1, R.Right - R.Left);
    A := A + Copy(FAttributes, Y * FWidth + R.Left + 1, R.Right - R.Left);
  end;
  Result := PackInt(R.Right - R.Left) + PackInt(R.Bottom - R.Top) + C + A;
end;

function TCheqMap.ExtractArea(const Area: TRect): TCheqMap;
var R: TRect; S: AnsiString; N: Integer;
begin
  R := ClipArea(Area, FWidth, FHeight);
  S := CopyArea(R);
  Result := TCheqMap.Create;
  try
    Result.NewMap(R.Right - R.Left, R.Bottom - R.Top);
    N := Result.Width * Result.Height;
    Result.FCharacters := Copy(S, 9, N);
    Result.FAttributes := Copy(S, 9 + N, N);
    Result.FPageAttribute := FPageAttribute;
    Result.FColored := FColored;
    Result.MarkSaved;
  except Result.Free; raise; end;
end;

procedure TCheqMap.PasteArea(const Data: AnsiString; X, Y: Integer);
var W, H, I, J, N: Integer;
begin
  W := UnpackInt(Data, 1); H := UnpackInt(Data, 5);
  CheckDimensions(W, H); N := W * H;
  if Length(Data) <> 8 + 2 * N then raise EReadError.Create('Invalid clipboard data.');
  BeginEdit;
  for J := 0 to H - 1 do
    for I := 0 to W - 1 do
      SetCell(X + I, Y + J, Ord(Data[9 + J * W + I]), Ord(Data[9 + N + J * W + I]));
  EndEdit;
end;

procedure TCheqMap.Crop(const Area: TRect);
var R: TRect; Data: AnsiString; W, H, N: Integer;
begin
  R := ClipArea(Area, FWidth, FHeight);
  if (R.Right - R.Left < 2) or (R.Bottom - R.Top < 2) then
    raise ERangeError.Create('The cropped canvas must be at least 2 x 2 cells.');
  Data := CopyArea(R); W := R.Right - R.Left; H := R.Bottom - R.Top; N := W * H;
  BeginEdit;
  FWidth := W; FHeight := H;
  FCharacters := Copy(Data, 9, N); FAttributes := Copy(Data, 9 + N, N);
  EndEdit;
end;

function TCheqMap.IsGraph: Boolean;
var I: Integer;
begin
  Result := False;
  for I := 1 to Length(FCharacters) do
    if not (Ord(FCharacters[I]) in [32, 128..143]) then Exit;
  Result := True;
end;

function TCheqMap.IsEmpty: Boolean;
var I: Integer;
begin
  Result := False;
  for I := 1 to Length(FCharacters) do
    if not EmptyCharacter(Ord(FCharacters[I])) then Exit;
  Result := True;
end;

{ SBG v1, little endian, 20-byte header:
    0..2 'SBG'; 3 version=1; 4..7 cell width; 8..11 cell height;
    12 INK; 13 PAPER; 14 BRIGHT; 15 FLASH; 16 flags (bit 0: color mode);
    17..19 reserved=0. Then width*height character bytes, followed by
    width*height packed Spectrum attribute bytes, in row-major order.
  UDG glyphs are resolved from the current emulator character set. }
procedure TCheqMap.SaveToStream(Stream: TStream);
var Header: AnsiString; A: Byte;
begin
  A := FPageAttribute;
  Header := 'SBG'#1 + PackInt(FWidth) + PackInt(FHeight) +
    Chr(A and 7) + Chr((A shr 3) and 7) + Chr((A shr 6) and 1) + Chr(A shr 7) +
    Chr(Ord(FColored)) + #0#0#0;
  Stream.WriteBuffer(Header[1], Length(Header));
  Stream.WriteBuffer(FCharacters[1], Length(FCharacters));
  Stream.WriteBuffer(FAttributes[1], Length(FAttributes));
end;

procedure TCheqMap.LoadFromStream(Stream: TStream);
var Header, C, A: AnsiString; W, H, N, I: Integer; Page: Byte; Mode: Boolean;
begin
  if Stream.Size - Stream.Position < 20 then raise EReadError.Create('Incomplete SBG header.');
  SetLength(Header, 20); Stream.ReadBuffer(Header[1], 20);
  if Copy(Header, 1, 3) <> 'SBG' then raise EReadError.Create('This is not a Sinclair Basic Graph file.');
  if Header[4] <> #1 then raise EReadError.Create('Unsupported SBG version.');
  W := UnpackInt(Header, 5); H := UnpackInt(Header, 9);
  CheckDimensions(W, H);
  if (W < 2) or (H < 2) then raise EReadError.Create('An SBG canvas must be at least 2 x 2 cells.');
  if (Ord(Header[13]) > 7) or (Ord(Header[14]) > 7) or
    (Ord(Header[15]) > 1) or (Ord(Header[16]) > 1) or
    (Ord(Header[17]) > 1) or (Copy(Header, 18, 3) <> #0#0#0) then
    raise EReadError.Create('Invalid SBG colors or flags.');
  N := W * H;
  if Stream.Size - Stream.Position <> Int64(N) * 2 then
    raise EReadError.Create('SBG data length does not match its dimensions.');
  SetLength(C, N); SetLength(A, N);
  Stream.ReadBuffer(C[1], N); Stream.ReadBuffer(A[1], N);
  for I := 1 to N do
    if not (Ord(C[I]) in [32..164]) then
      raise EReadError.Create('SBG contains an invalid printable character.');
  Page := CheqAttribute(Ord(Header[13]), Ord(Header[14]), Ord(Header[15]), Ord(Header[16]));
  Mode := Header[17] <> #0;
  { Commit only after the complete file is validated. }
  FWidth := W; FHeight := H; FCharacters := C; FAttributes := A;
  FPageAttribute := Page; FColored := Mode;
  FEditing := False; FBefore := ''; FUndo.Clear; FRedo.Clear;
  MarkSaved;
end;

function TCheqMap.BridgeData(Method: TCheqBridgeMethod): AnsiString;
var I, X, Y, Bit, N, LastA, P: Integer; C, A: Byte;
begin
  Result := ''; N := Length(FCharacters);
  case Method of
    cbCharacters: Result := FCharacters;
    cbPlanes: Result := FCharacters + FAttributes;
    cbColors: Result := FAttributes;
    cbPairs:
      begin
        SetLength(Result, N * 2);
        for I := 1 to N do begin Result[I * 2 - 1] := FAttributes[I]; Result[I * 2] := FCharacters[I]; end;
      end;
    cbColorChanges:
      begin
        LastA := -1;
        for I := 1 to N do
        begin
          A := Ord(FAttributes[I]); C := Ord(FCharacters[I]);
          if A <> LastA then begin Result := Result + #128 + Chr(A); LastA := A; end;
          if C = 128 then C := 32;
          Result := Result + Chr(C);
        end;
      end;
    cbHiResDecimal, cbHiResBinary:
      begin
        if not IsGraph then raise EConvertError.Create('Hi-res data requires GRAPH characters only.');
        if Method = cbHiResDecimal then SetLength(Result, FWidth * FHeight * 4)
        else SetLength(Result, ((FWidth * 2 + 7) div 8) * FHeight * 2);
        P := 1;
        for Y := 0 to FHeight * 2 - 1 do
        begin
          A := 0;
          for X := 0 to FWidth * 2 - 1 do
          begin
            C := Characters[X div 2, Y div 2];
            if C = 32 then C := 128;
            Bit := Ord(((C - 128) and (1 shl ((Y mod 2) * 2 + 1 - (X mod 2)))) <> 0);
            if Method = cbHiResDecimal then begin Result[P] := Chr(Bit); Inc(P); end
            else
            begin
              A := A or (Bit shl (7 - (X mod 8)));
              if ((X mod 8) = 7) or (X = FWidth * 2 - 1) then
              begin Result[P] := Chr(A); Inc(P); A := 0; end;
            end;
          end;
        end;
      end;
  end;
end;

{ LET expressions use +, while PRINT items use semicolons. Quotes in
  raw coordinate bytes must not end the compressed string literal. }
function BasicStringExpression(const S: AnsiString): AnsiString;
var I: Integer; Literal: AnsiString;
  procedure Flush;
  begin
    if Literal = '' then Exit;
    if Result <> '' then Result := Result + '+';
    Result := Result + '"' + Literal + '"'; Literal := '';
  end;
begin
  Result := ''; Literal := '';
  for I := 1 to Length(S) do
    if S[I] = '"' then
    begin
      Flush;
      if Result <> '' then Result := Result + '+';
      Result := Result + 'CHR$ 34';
    end
    else Literal := Literal + S[I];
  Flush;
  if Result = '' then Result := '""';
end;

procedure TCheqMap.GenerateCompressed(const Options: TCheqExportOptions;
  Lines: TStrings; var LineNo: Integer);
var
  Method: TCheqCompressionMethod;
  Data, Prefix, Expression, Position, Advance, BiasText: AnsiString;
  Decoder: TStringList;
  X, Y, First, Last, C, A, LastA, Spaces, Amount, Offset, Take,
    Bias, NextLine, I: Integer;
  UsedColors: array[0..3] of Boolean;
  procedure Emit(const S: AnsiString);
  begin
    if (LineNo < 1) or (LineNo > 9999) then
      raise ERangeError.Create('BASIC line numbers must be between 1 and 9999.');
    if Length(IntToStr(LineNo)) + 1 + Length(S) > 255 then
      raise ERangeError.Create('A compressed BASIC line exceeds 255 characters.');
    Lines.Add(IntToStr(LineNo) + ' ' + S); Inc(LineNo, Options.Step);
  end;
  procedure FlushSpaces;
  begin
    while Spaces > 0 do
    begin
      Amount := Spaces; if Amount > 28 then Amount := 28;
      Data := Data + Chr(47 + Amount); Dec(Spaces, Amount);
    end;
  end;
  procedure ColorChange(Index, Mask, Shift, Base: Integer; Enabled: Boolean);
  begin
    if not Enabled or (((A xor LastA) and Mask) = 0) then Exit;
    FlushSpaces;
    Data := Data + Chr(Base + ((A and Mask) shr Shift));
    UsedColors[Index] := True;
  end;
begin
  if not IsGraph then
    raise EConvertError.Create('Compressed strings support GRAPH characters only. Use normal PRINT or Bridge for tilemaps.');
  if (Options.CompressionChunkSize < 1) or (Options.CompressionChunkSize > 255) then
    raise ERangeError.Create('Compressed string chunks must contain between 1 and 255 bytes.');
  Method := Options.CompressionMethod;
  if Method = cmAutomatic then
    if Options.Colored then Method := cmColorStep2 else Method := cmLetters;
  Data := ''; Spaces := 0; LastA := Options.PageAttribute;
  FillChar(UsedColors, SizeOf(UsedColors), 0);
  { Keep the C# printable coordinate bias for ordinary maps. Tall maps
    use raw byte coordinates, with the corresponding decoder expression. }
  if FHeight <= 191 then Bias := 65 else Bias := 0;
  if Bias = 0 then BiasText := '' else BiasText := '-65';
  if Method = cmColorStep2 then
  begin
    { Form1.cs ColorCompressRaw / GetBasicDecoder_Step2:
      '0'..'K' = 1..28 spaces; 'L'..'Z' = GRAPH 129..143;
      'a'..'h' = INK; 'i'..'p' = PAPER.
      Unused 'q'..'t' extend it with BRIGHT 0/1 and FLASH 0/1. }
    for Y := 0 to FHeight - 1 do for X := 0 to FWidth - 1 do
    begin
      A := Attributes[X,Y];
      if Options.Colored then
      begin
        ColorChange(0,7,0,97,Options.Ink);
        ColorChange(1,56,3,105,Options.Paper);
        ColorChange(2,64,6,113,Options.Bright);
        ColorChange(3,128,7,115,Options.Flash);
      end;
      LastA := A; C := Characters[X,Y];
      if EmptyCharacter(C) then Inc(Spaces)
      else begin FlushSpaces; Data := Data + Chr(C - 53); end;
    end;
    FlushSpaces;
  end
  else
    { Form1.cs compress(): remove each row's leading/trailing blanks.
      kod / kodGraph use Q,row,column; kodGraphCut uses row,column,len-1. }
    for Y := 0 to FHeight - 1 do
    begin
      First := 0;
      while (First < FWidth) and EmptyCharacter(Characters[First,Y]) do Inc(First);
      if First = FWidth then Continue;
      Last := FWidth - 1;
      while EmptyCharacter(Characters[Last,Y]) do Dec(Last);
      if Method <> cmGraphCut then Data := Data + 'Q';
      Data := Data + Chr(Y + Bias) + Chr(First + Bias);
      if Method = cmGraphCut then Data := Data + Chr(48 + Last - First);
      for X := First to Last do
      begin
        C := Characters[X,Y]; if C = 32 then C := 128;
        if Method = cmLetters then Data := Data + Chr(C - 63)
        else Data := Data + Chr(C);
      end;
    end;
  Offset := 1;
  repeat
    if Offset = 1 then Prefix := 'LET g$=' else Prefix := 'LET g$=g$+';
    Take := Length(Data) - Offset + 1;
    if Take > Options.CompressionChunkSize then Take := Options.CompressionChunkSize;
    repeat
      Expression := BasicStringExpression(Copy(Data,Offset,Take));
      if Length(IntToStr(LineNo)) + 1 + Length(Prefix) + Length(Expression) <= 255 then Break;
      Dec(Take);
    until Take = 0;
    if (Take = 0) and (Data <> '') then
      raise ERangeError.Create('A compressed string item exceeds 255 characters.');
    Emit(Prefix + Expression); Inc(Offset,Take);
  until Offset > Length(Data);
  if Options.CompressionDataOnly or (Data = '') then Exit;
  if Method = cmGraphCut then
    Emit('FOR x=1 TO LEN g$: PRINT AT CODE g$(x)' + BiasText +
      ',CODE g$(x+1)' + BiasText + ';: LET l=CODE g$(x+2)-48: LET x=x+3+l:' +
      ' PRINT g$(x-l TO x);: NEXT x')
  else if Method in [cmLetters,cmGraph] then
  begin
    Emit('FOR x=1 TO LEN g$: IF g$(x)="Q" THEN PRINT AT CODE (g$(x+1))' +
      BiasText + ',CODE (g$(x+2))' + BiasText + ';: LET x=x+2: NEXT x');
    if Method = cmLetters then Emit('PRINT CHR$ (63+CODE (g$(x)));: NEXT x')
    else Emit('PRINT g$(x);: NEXT x');
  end
  else
  begin
    Decoder := TStringList.Create;
    try
      if FWidth = 32 then
      begin Emit('PRINT AT 0,0;'); Position := ''; Advance := ''; end
      else
      begin
        Emit('LET p=0');
        Position := 'AT INT (p/' + IntToStr(FWidth) + '),p-' + IntToStr(FWidth) + '*INT (p/' + IntToStr(FWidth) + ');';
        Advance := ' LET p=p+1:';
      end;
      Decoder.Add('FOR i=1 TO LEN g$');
      Decoder.Add('LET c=CODE g$(i)');
      Decoder.Add('IF c>=48 AND c<=75 THEN FOR s=1 TO c-47: PRINT ' +
        Position + '" ";:' + Advance + ' NEXT s: GO TO @NEXT@');
      Decoder.Add('IF c>=76 AND c<=90 THEN PRINT ' + Position +
        'CHR$ (c+53);:' + Advance + ' GO TO @NEXT@');
      if UsedColors[0] then Decoder.Add('IF c>=97 AND c<=104 THEN INK c-97: GO TO @NEXT@');
      if UsedColors[1] then Decoder.Add('IF c>=105 AND c<=112 THEN PAPER c-105: GO TO @NEXT@');
      if UsedColors[2] then Decoder.Add('IF c>=113 AND c<=114 THEN BRIGHT c-113: GO TO @NEXT@');
      if UsedColors[3] then Decoder.Add('IF c>=115 AND c<=116 THEN FLASH c-115: GO TO @NEXT@');
      Decoder.Add('NEXT i');
      NextLine := LineNo + (Decoder.Count-1)*Options.Step;
      for I := 0 to Decoder.Count-1 do
        Emit(StringReplace(Decoder[I], '@NEXT@', IntToStr(NextLine), [rfReplaceAll]));
    finally Decoder.Free; end;
  end;
end;

procedure TCheqMap.GenerateBasic(const Options: TCheqExportOptions; Lines: TStrings);
var
  O: TCheqExportOptions;
  LineNo, X, Y, H, C, A, LastA: Integer;
  PrintPosition, ColorMask: Integer;
  Body, Literal: AnsiString;
  RowEmpty: Boolean;
  procedure Emit(const S: AnsiString);
  begin
    if (LineNo < 1) or (LineNo > 9999) then raise ERangeError.Create('BASIC line numbers must be between 1 and 9999.');
    Lines.Add(IntToStr(LineNo) + ' ' + S); Inc(LineNo, O.Step);
  end;
  procedure FlushLiteral;
  begin
    if Literal = '' then Exit;
    Body := Body + '"' + Literal + '";'; Literal := '';
  end;
  procedure AppendKeyword(const S: AnsiString);
  begin
    FlushLiteral;
    Body := Body + S + ';';
  end;
  procedure AppendByte(Value: Byte);
  begin
    if O.UseCHR or (Value = 34) then AppendKeyword('CHR$ ' + IntToStr(Value))
    else Literal := Literal + Chr(Value);
  end;
  procedure AppendColors(Value: Byte);
  var K, V, P, Mask: Integer;
  const Names: array[0..3] of String = ('INK ', 'PAPER ', 'BRIGHT ', 'FLASH ');
    Codes: array[0..3] of Byte = (16, 17, 19, 18);
    Masks: array[0..3] of Byte = (7, 56, 64, 128);
    Shifts: array[0..3] of Byte = (0, 3, 6, 7);
  begin
    for K := 0 to 3 do
    begin
      Mask := Masks[K];
      if ((ColorMask and Mask) = 0) or (((Value xor LastA) and Mask) = 0) then Continue;
      V := (Value and Mask) shr Shifts[K];
      if O.ControlColors then begin AppendByte(Codes[K]); AppendByte(V); end
      else AppendKeyword(Names[K] + IntToStr(V));
    end;
    P := LastA and (255 xor ColorMask);
    LastA := P or (Value and ColorMask);
  end;
  procedure PositionAt(Col, Row: Integer);
  var Target: Integer;
  begin
    Target := Row * 32 + Col;
    if PrintPosition = Target then Exit;
    case O.SpaceMethod of
      csControlAT:
        begin AppendByte(22); AppendByte(Row); AppendByte(Col); end;
      csTAB, csControlTAB:
        begin
          if (PrintPosition < 0) or (PrintPosition div 32 <> Row) or
            (PrintPosition mod 32 > Col) then
          begin
            AppendKeyword('AT ' + IntToStr(Row) + ',0');
            PrintPosition := Row * 32;
          end;
          if PrintPosition <> Target then
          begin
            { TAB prints spaces, so the skipped cells need their page colors.
              AT only moves the cursor and leaves the current colors alone. }
            AppendColors(O.PageAttribute);
            if O.SpaceMethod = csTAB then AppendKeyword('TAB ' + IntToStr(Col))
            else begin AppendByte(23); AppendByte(Col); AppendByte(0); end;
          end;
        end;
    else AppendKeyword('AT ' + IntToStr(Row) + ',' + IntToStr(Col));
    end;
    PrintPosition := Target;
  end;
  function SourceLength: Integer;
  begin
    Result := Length(IntToStr(LineNo)) + 1 + Length(Body);
    if Literal <> '' then Inc(Result, Length(Literal) + 3);
  end;
  procedure FinishPrint;
  begin
    FlushLiteral;
    if Body <> 'PRINT ' then Emit(Body);
    Body := 'PRINT ';
    { Each PRINT statement starts with the permanent Colors-tab settings.
      Its temporary attribute survives wrapping, but not a new PRINT. }
    LastA := O.PageAttribute;
  end;
  procedure AppendCell(Col, Row: Integer);
  var SavedBody, SavedLiteral: AnsiString; SavedA, SavedPosition: Integer;
    HadContent: Boolean;
  begin
    SavedBody := Body; SavedLiteral := Literal;
    SavedA := LastA; SavedPosition := PrintPosition;
    HadContent := (Body <> 'PRINT ') or (Literal <> '');
    PositionAt(Col, Row);
    AppendColors(Attributes[Col, Row]);
    AppendByte(Characters[Col, Row]);
    Inc(PrintPosition);
    if SourceLength <= 255 then Exit;
    { Split between complete cells: never inside AT/TAB or a color pair.
      Semicolons preserve the cursor; the new PRINT restores only the
      colors needed for its first cell (and for TAB's spaces, if any). }
    Body := SavedBody; Literal := SavedLiteral;
    LastA := SavedA; PrintPosition := SavedPosition;
    if not HadContent then raise ERangeError.Create('A PRINT item exceeds 255 characters.');
    FinishPrint;
    PositionAt(Col, Row);
    AppendColors(Attributes[Col, Row]);
    AppendByte(Characters[Col, Row]);
    Inc(PrintPosition);
    if SourceLength > 255 then raise ERangeError.Create('A PRINT item exceeds 255 characters.');
  end;

begin
  O := Options; Lines.Clear;
  if (O.Start < 1) or (O.Start > 9999) or (O.Step < 1) or (O.Step > 9999) then
    raise ERangeError.Create('Use a start and step between 1 and 9999.');
  H := FHeight;
  if FWidth > 32 then raise ERangeError.Create('BASIC output requires a width of 32 cells or less. Export a selection or use Bridge.');
  LineNo := O.Start;
  if O.InitColors and (not O.Compressed or not O.CompressionDataOnly) then
    Emit('PAPER ' + IntToStr((O.PageAttribute shr 3) and 7) +
    ': INK ' + IntToStr(O.PageAttribute and 7) + ': BRIGHT ' +
    IntToStr((O.PageAttribute shr 6) and 1) + ': FLASH ' +
    IntToStr(O.PageAttribute shr 7) + ': CLS');
  if O.Compressed then
  begin
    GenerateCompressed(O, Lines, LineNo);
    Exit;
  end;
  ColorMask := 0;
  if O.Colored then
  begin
    if O.Ink then ColorMask := ColorMask or 7;
    if O.Paper then ColorMask := ColorMask or 56;
    if O.Bright then ColorMask := ColorMask or 64;
    if O.Flash then ColorMask := ColorMask or 128;
  end;
  PrintPosition := -1; LastA := O.PageAttribute;
  Body := 'PRINT '; Literal := '';
  for Y := 0 to H - 1 do
  begin
    RowEmpty := True;
    for X := 0 to FWidth - 1 do
      if not EmptyCharacter(Characters[X, Y]) or
        (((Attributes[X, Y] xor O.PageAttribute) and ColorMask) <> 0) then begin RowEmpty := False; Break; end;
    if RowEmpty then
    begin
      FinishPrint;
      if not O.SkipEmpty then Inc(LineNo, O.Step);
      Continue;
    end;
    for X := 0 to FWidth - 1 do
    begin
      C := Characters[X, Y]; A := Attributes[X, Y];
      if (O.SpaceMethod <> csLiteral) and EmptyCharacter(C) and
        (((A xor O.PageAttribute) and ColorMask) = 0) then Continue;
      AppendCell(X, Y);
    end;
    if not O.JoinFull or (PrintPosition <> (Y + 1) * 32) then FinishPrint;
  end;
  FinishPrint;
end;

end.
