unit BlockUnit;

interface

type
  TBlock = class
  private
    FBits: Integer;
    FIndex: Integer;
    FOffset: Integer;
    FChain: TBlock;
  public
    constructor Create(bits, index, offset: Integer; chain: TBlock);
    function GetBits: Integer;
    function GetIndex: Integer;
    function GetOffset: Integer;
    function GetChain: TBlock;
    procedure SetChain(chain: TBlock);
    property Bits: Integer read GetBits;
    property Index: Integer read GetIndex;
    property Offset: Integer read GetOffset;
    property Chain: TBlock read GetChain write SetChain;
  end;

implementation

constructor TBlock.Create(bits, index, offset: Integer; chain: TBlock);
begin
  inherited Create;
  FBits := bits;
  FIndex := index;
  FOffset := offset;
  FChain := chain;
end;

function TBlock.GetBits: Integer;
begin
  Result := FBits;
end;

function TBlock.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TBlock.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TBlock.GetChain: TBlock;
begin
  Result := FChain;
end;

procedure TBlock.SetChain(chain: TBlock);
begin
  FChain := chain;
end;

end.

