unit ScrPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FastDIB, FastIMG, FastDrawEx, Math;

type
  TScrPreviewForm = class(TForm)
    FastIMG1: TFastIMG;
    procedure FormCreate(Sender: TObject);
    procedure FastIMG1Exit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    Bmp: TFastDIB;
    MouseDown: Boolean;
    OffsetX, OffsetY, Bw, Bh: Integer;
  end;

var
  ScrPreviewForm: TScrPreviewForm;

implementation

uses Paintbox;

{$R *.DFM}

procedure TScrPreviewForm.FormCreate(Sender: TObject);
begin

  Bmp := TFastDIB.Create;
  Bmp.SetSize(256, 192, 32);
  FastIMG1.Bmp.SetSize(256, 192, 32);

end;

procedure TScrPreviewForm.FastIMG1Exit(Sender: TObject);
Var
  Tp, Pt: TPoint;
  Mx, My: Integer;
begin

  If Not MouseDown Then Begin
     GetCursorPos(Tp);
     Pt := Point(ScrPaintForm.FastIMG6.Left, ScrPaintForm.FastIMG6.Top);
     Mx := ScrPaintForm.Panel3.ClientToScreen(Pt).X;
     My := ScrPaintForm.Panel3.ClientToScreen(Pt).Y;
     If (Tp.X >= Mx) And (Tp.Y >= My) and (Tp.X < MX + ScrPaintForm.FastIMG6.Width) and (Tp.Y < MY + ScrPaintForm.FastIMG6.Height) Then
        ScrPaintForm.CanEnter := False
     Else
        ScrPaintForm.CanEnter := True;
     Close;
  End;

end;

procedure TScrPreviewForm.FormDestroy(Sender: TObject);
begin

  Bmp.Free;

end;

procedure TScrPreviewForm.FormShow(Sender: TObject);
Var
  X, Y, X1, Y1, X2, Y2: Integer;
  Clr: TFColorA;
begin

  If Sender <> FastIMG1 Then Begin
     ClientWidth := 258;
     ClientHeight := 194;
  End;

  Bmp.Draw(FastIMG1.Bmp.hDc, 0, 0);

  If ScrPaintForm.CurScreen^.ZoomLevel > 1 Then Begin

     X1 := Min(ScrPaintForm.FastIMG1.DIBLeft Div ScrPaintForm.CurScreen^.ZoomLevel, 255);
     Y1 := Min((ScrPaintForm.FastIMG1.DIBTop Div ScrPaintForm.CurScreen^.ZoomLevel), 191);
     X2 := Min(X1 + (ScrPaintForm.FastIMG1.Bmp.Width Div ScrPaintForm.CurScreen^.ZoomLevel), 255);
     Y2 := Min((Y1 + (ScrPaintForm.FastIMG1.Bmp.AbsHeight Div ScrPaintForm.CurScreen^.ZoomLevel)), 191);

     If (X1 > 0) or (Y1 > 0) or (X2 < 255) or (Y2 < 191) Then Begin
        For X := X1 To X2 Do Begin
           Clr := FastIMG1.Bmp.Pixels32[191-Y1, X];
           Clr.r := 255 - Clr.r; Clr.g := 255 - Clr.g; Clr.b := 255 - Clr.b;
           FastIMG1.Bmp.Pixels32[191-Y1, X] := Clr;
           Clr := FastIMG1.Bmp.Pixels32[191-Y2, X];
           Clr.r := 255 - Clr.r; Clr.g := 255 - Clr.g; Clr.b := 255 - Clr.b;
           FastIMG1.Bmp.Pixels32[191-Y2, X] := Clr;
        End;
        For Y := Y1 +1 To Y2 -1 Do Begin
           Clr := FastIMG1.Bmp.Pixels32[191-Y, X1];
           Clr.r := 255 - Clr.r; Clr.g := 255 - Clr.g; Clr.b := 255 - Clr.b;
           FastIMG1.Bmp.Pixels32[191-Y, X1] := Clr;
           Clr := FastIMG1.Bmp.Pixels32[191-Y, X2];
           Clr.r := 255 - Clr.r; Clr.g := 255 - Clr.g; Clr.b := 255 - Clr.b;
           FastIMG1.Bmp.Pixels32[191-Y, X2] := Clr;
        End;
     End;
  End;

  Bw := (X2 - X1) +1;
  Bh := (Y2 - Y1) +1;
  OffsetX := Bw Div 2;
  OffsetY := Bh Div 2;

  FastIMG1.Repaint;

end;

procedure TScrPreviewForm.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  MouseDown := True;
  FastIMG1MouseMove(Sender, Shift, X, Y);

end;

procedure TScrPreviewForm.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin

  If (Bw <> 256) or (Bh <> 192) Then Begin

     If MouseDown Then Begin

        Dec(X, OffsetX);
        If X < 0 Then
           X := 0
        Else
           If X + Bw > 255 Then
              X := 256 - Bw;

        Dec(Y, OffsetY);
        If Y < 0 Then
           Y := 0
        Else
           If Y + Bh > 192 Then
              Y := 192 - Bh;

        ScrPaintForm.ScrollBox1.HorzScrollBar.Position := X * ScrPaintForm.CurScreen^.ZoomLevel;
        ScrPaintForm.ScrollBox1.VertScrollBar.Position := Y * ScrPaintForm.CurScreen^.ZoomLevel;
        ScrPaintForm.RenderScreen(ScrPaintForm.CurScreenIndex, ScrPaintForm.ShowingAttrs);
        FormShow(Sender);

     End;

  End;

end;

procedure TScrPreviewForm.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  MouseDown := False;
  FastIMG1Exit(Sender);

end;

end.
