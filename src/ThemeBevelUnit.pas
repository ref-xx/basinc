unit ThemeBevelUnit;

interface

uses
  Classes, Controls, ExtCtrls, Windows, Graphics;

type
  TThemeBevel = class(TBevel)
  public
    procedure Paint; override;
  end;

procedure Register;

implementation

procedure TThemeBevel.Paint;
var
  Rct: TRect;
  Edge, Flags: Integer;
begin
  Rct := Rect(0, 0, Width - 1, Height - 1);
  case Style of
    bsLowered: Edge := EDGE_RAISED;
    bsRaised:  Edge := EDGE_SUNKEN;
  end;

  Flags := BF_FLAT;

  case Shape of
    bsBox, bsFrame:
      begin
        Flags := Flags or BF_BOTTOM or BF_TOP or BF_LEFT or BF_RIGHT;
        if Shape = bsFrame then Edge := EDGE_ETCHED;
      end;
    bsTopLine:    Flags := Flags or BF_TOP;
    bsBottomLine: Flags := Flags or BF_BOTTOM;
    bsLeftLine:   Flags := Flags or BF_LEFT;
    bsRightLine:  Flags := Flags or BF_RIGHT;
  end;

  if Shape <> bsSpacer then
    Windows.DrawEdge(Canvas.Handle, Rct, Edge, Flags);
end;

procedure Register;
begin
  RegisterComponents('XP', [TThemeBevel]);
end;

end.

