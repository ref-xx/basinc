unit BEEP;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FastIMG, FastDIB, FastDrawEx, FastDraw, ToolWin, ComCtrls, ImgList, Utility,
  Buttons, ExtCtrls, Math,ThemeBevelUnit;

type
  TBEEPWindow = class(TForm)
    FastIMG1: TFastIMG;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Bevel1: TThemeBevel;
    SpeedButton10: TSpeedButton;
    Bevel2: TThemeBevel;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    FastIMG3: TFastIMG;
    FastIMG2: TFastIMG;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButtonClick(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1Exit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Function  GetScorePosition(X: Integer): Integer;

  private
    { Private declarations }
    ScrollBox1: TNewScrollBox;
    EditType: Integer;
    CursImage: TFastDIB;
    MousePosX,
    MousePosY: Integer;
    procedure ScrollBox1HScroll(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType);
    Procedure GetEditType(Sender: TObject);
    Procedure DrawMouseImage;
  public
    { Public declarations }
    Procedure DrawClefs;
    Procedure DrawCurrentSequence;
  end;

var
  BEEPWindow: TBEEPWindow;
  SequenceList: TStringlist;
  SequenceViewOffset, SequenceCursOffset,
  SequenceSelectionStart, SequenceSelectionLength: Integer;

implementation

{$R *.DFM}

Procedure TBEEPWindow.DrawClefs;
Var
  Y: Integer;
Begin

  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 24);
  FastIMG1.Bmp.Clear(TfWhite);
  FastIMG2.Bmp.Draw(FastIMG1.Bmp.hDc, 0, 0);

  FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Width-1, FastIMG1.Height -1, TfBlack);

  Line(FastIMG1.Bmp, 44, 21, 44, 121, TFBlack);
  Line(FastIMG1.Bmp, 45, 21, 45, 121, TFBlack);

  Line(FastIMG1.Bmp, 48, 21, 48, 121, TFBlack);
  Line(FastIMG1.Bmp, 49, 21, 49, 121, TFBlack);

  For Y := 0 To 4 Do Begin
     Line(FastIMG1.Bmp, 40, (Y*10)+21, FastIMG1.Bmp.Width -1, (Y*10)+21, TFBlack);
     Line(FastIMG1.Bmp, 40, (Y*10)+22, FastIMG1.Bmp.Width -1, (Y*10)+22, TFBlack);
  End;

  For Y := 0 To 4 Do Begin
     Line(FastIMG1.Bmp, 40, (Y*10)+81, FastIMG1.Bmp.Width -1, (Y*10)+81, TFBlack);
     Line(FastIMG1.Bmp, 40, (Y*10)+82, FastIMG1.Bmp.Width -1, (Y*10)+82, TFBlack);
  End;

  DrawCurrentSequence;

End;

procedure TBEEPWindow.FormShow(Sender: TObject);
Var
  Tp: TPoint;
begin
  If ScrollBox1 = Nil THen Begin
     ScrollBox1 := TNewScrollBox.Create(Self);
     ScrollBox1.SetBounds(8, 8, FastIMG1.Width, FastIMG1.Height + GetSystemMetrics(SM_CYHSCROLL));
     ScrollBox1.Parent := Self;
     ScrollBox1.AutoScroll := False;
     ScrollBox1.HorzScrollBar.Visible := True;
     ScrollBox1.HorzScrollBar.Range := ScrollBox1.ClientWidth;
     ScrollBox1.ClientWidth := FastIMG1.Width;
     ScrollBox1.ClientHeight := FastIMG1.Height;
     ScrollBox1.OnHorizontalScroll := ScrollBox1HScroll;
     TP := Self.ScreenToClient(ScrollBox1.ClientOrigin);
     FastIMG1.SetBounds(Tp.x, Tp.y, FastIMG1.Width, FastIMG1.Height);
     FastIMG1.BringToFront;
  End;
  ClientWidth := ScrollBox1.Width + 16;
  If CursImage = nil Then Begin
     CursImage := TFastDIB.Create;
     CursImage.SetSize(32, 32, 24);
  End;
  GetEditType(nil);
  DrawClefs;
  DrawCurrentSequence;
  FastIMG1.Repaint;

  SpeedButton1.Down := False;
  SpeedButton2.Down := False;
  SpeedButton3.Down := False;
  SpeedButton4.Down := False;
  SpeedButton5.Down := False;
  SpeedButton6.Down := False;
  SpeedButton7.Down := False;
  SpeedButton8.Down := False;
  SpeedButton9.Down := False;
  SpeedButton10.Down := False;
  SpeedButton11.Down := False;
  SpeedButton12.Down := False;
  SpeedButton13.Down := False;
  SpeedButton14.Down := False;
  SpeedButton15.Down := False;

end;

procedure TBEEPWindow.FormCreate(Sender: TObject);
Var
  Tp: TPoint;
  x:byte;
begin

  ScrollBox1 := TNewScrollBox.Create(Self);
  ScrollBox1.SetBounds(8, 8, FastIMG1.Width, FastIMG1.Height + GetSystemMetrics(SM_CYHSCROLL));
  ScrollBox1.Parent := Self;
  ScrollBox1.AutoScroll := False;
  ScrollBox1.HorzScrollBar.Visible := True;
  ScrollBox1.HorzScrollBar.Range := ScrollBox1.ClientWidth;
  ScrollBox1.ClientWidth := FastIMG1.Width;
  ScrollBox1.ClientHeight := FastIMG1.Height;
  ScrollBox1.OnHorizontalScroll := ScrollBox1HScroll;
  TP := Self.ScreenToClient(ScrollBox1.ClientOrigin);
  FastIMG1.SetBounds(Tp.x, Tp.y, FastIMG1.Width, FastIMG1.Height);
  FastIMG1.BringToFront;

  CursImage := TFastDIB.Create;
  CursImage.SetSize(32, 32, 24);

  SequenceList := TStringlist.Create;

  for x := 1 to 40 do
     sequencelist.add(chr(x)+chr(x+1));

  SequenceViewOffset := 0;
  SequenceCursOffset := 0;
  SequenceSelectionStart := 0;
  SequenceSelectionLength := 0;

end;

procedure TBEEPWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ScrollBox1.Free;
  CursImage.Free;
  CursImage := nil;
  ScrollBox1 := nil;
end;

procedure TBEEPWindow.ScrollBox1HScroll(Sender: TObject; Pos: SmallInt; EventType: THScrollEventType);
begin
  // nothing doing... yet
end;

procedure TBEEPWindow.ToolButtonClick(Sender: TObject);
begin
  GetEditType(Sender);
end;

procedure TBEEPWindow.SpeedButton11Click(Sender: TObject);
begin
  GetEditType(Sender);
  If (Sender as TSpeedButton).Down Then Begin
     SpeedButton9.Down := False;
     SpeedButton10.Down := False;
  End;
end;

procedure TBEEPWindow.SpeedButton9Click(Sender: TObject);
begin
  GetEditType(Sender);
  If (Sender as TSpeedButton).Down Then Begin
     SpeedButton11.Down := False;
     SpeedButton12.Down := False;
     SpeedButton13.Down := False;
     SpeedButton14.Down := False;
     SpeedButton15.Down := False;
  End;
end;

Procedure TBEEPWindow.GetEditType(Sender: TObject);
Begin
  If Sender <> Nil Then Begin
     If (Sender = SpeedButton9) or (Sender = SpeedButton10) Then Begin
        If (EditType = 0) or (EditType > 10) Then Begin
           If (Sender as TSpeedButton).Down Then Begin
              EditType := (Sender as TSpeedButton).Tag;
              FastIMG1.Cursor := CrNone;
           End Else Begin
              FastIMG1.Cursor := crDefault;
           End;
           CursImage.LoadFromHandle((Sender as TSpeedButton).Glyph.Handle);
           Exit;
        End Else
           If Not (Sender as TSpeedButton).Down Then Begin
              EditType := 0;
              FastIMG1.Cursor := CrDefault;
           End;
           Exit;
     End Else Begin
        If (Sender as TSpeedButton).Down Then Begin
           EditType := (Sender as TSpeedButton).Tag;
           FastIMG1.Cursor := CrNone;
        End Else Begin
           If SpeedButton9.Down or SpeedButton10.Down Then Begin
              If SpeedButton9.Down  Then Sender := SpeedButton9;
              If SpeedButton10.Down Then Sender := SpeedButton10;
              EditType := (Sender as TSpeedButton).Tag;
              FastIMG1.Cursor := CrNone;
           End Else Begin
              EditType := 0;
              FastIMG1.Cursor := CrDefault;
           End;
        End;
        CursImage.LoadFromHandle((Sender as TSpeedButton).Glyph.Handle);
        Exit;
     End;
  End;
  EditType := 0;
  FastIMG1.Cursor := CrDefault;
End;

procedure TBEEPWindow.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  X := Max(Min(X, FastIMG1.Bmp.Width -16), 16);
  Y := Max(Min(Y, FastIMG1.Bmp.Height -16), 16);
  If MousePosX <> 0 Then
     DrawMouseImage;
  MousePosX := X;
  MousePosY := FastIMG1.Bmp.AbsHeight - Y;
  DrawMouseImage;
end;

Procedure TBEEPWindow.DrawMouseImage;
Var
  X, Y: Integer;
  r: Byte;
Begin
  If EditType <> 0 Then
     For X := 0 To 31 Do
        For Y := 0 To 31 Do
           If CursImage.Pixels24[Y, X].r <> 255 Then Begin
              r := 255-FastIMG1.Bmp.Pixels24[(MousePosY -16)+Y, (MousePosX -16)+X].r;
              FastIMG1.Bmp.Pixels24[(MousePosY -16)+Y, (MousePosX -16)+X].b := r;
              FastIMG1.Bmp.Pixels24[(MousePosY -16)+Y, (MousePosX -16)+X].g := r;
              FastIMG1.Bmp.Pixels24[(MousePosY -16)+Y, (MousePosX -16)+X].r := r;
           End;
     FastIMG1.Repaint;
End;

procedure TBEEPWindow.FastIMG1Exit(Sender: TObject);
begin
  DrawClefs;
  FastIMG1.Repaint;
  MousePosX := 0;
end;

Procedure TBEEPWindow.DrawCurrentSequence;
Var
  TempStr: String;
  NotePitch, NoteLen, r: Byte;
  nX, nY, X, Y, Idx: Integer;
  NoteImage: TFastDIB;
Begin
  If SequenceViewOffset < SequenceList.Count Then Begin

     NoteImage := TFastDIB.Create;
     Idx := SequenceViewOffset;
     While (Idx < Sequencelist.Count) and ((Idx *32) +64 < FastIMG1.Bmp.Width) Do Begin

        TempStr := SequenceList[Idx];
        NotePitch := Ord(TempStr[1]);
        NoteLen := Ord(TempStr[2]);

        nX := 64 + (Idx * 32);
        nY := (NotePitch * 5) + 14;

        Case NoteLen Of
           1: NoteImage.LoadFromHandle(SpeedButton1.Glyph.Handle);
           2: NoteImage.LoadFromHandle(SpeedButton2.Glyph.Handle);
           3: NoteImage.LoadFromHandle(SpeedButton3.Glyph.Handle);
           4: NoteImage.LoadFromHandle(SpeedButton4.Glyph.Handle);
           5: NoteImage.LoadFromHandle(SpeedButton5.Glyph.Handle);
           6: NoteImage.LoadFromHandle(SpeedButton6.Glyph.Handle);
           7: NoteImage.LoadFromHandle(SpeedButton7.Glyph.Handle);
           8: NoteImage.LoadFromHandle(SpeedButton8.Glyph.Handle);
           9: NoteImage.LoadFromHandle(SpeedButton9.Glyph.Handle);
          10: NoteImage.LoadFromHandle(SpeedButton10.Glyph.Handle);
          11: NoteImage.LoadFromHandle(SpeedButton11.Glyph.Handle);
          12: NoteImage.LoadFromHandle(SpeedButton12.Glyph.Handle);
          13: NoteImage.LoadFromHandle(SpeedButton13.Glyph.Handle);
          14: NoteImage.LoadFromHandle(SpeedButton14.Glyph.Handle);
          15: NoteImage.LoadFromHandle(SpeedButton15.Glyph.Handle);
        End;

        If Not Odd(NotePitch) Then Begin

           Line(NoteImage, 4, 26, 22, 26, TFBlack);
           Line(NoteImage, 4, 27, 22, 27, TFBlack);

        End;

        For Y := 0 To 31 Do
           For X := 0 To 31 Do
              If NoteImage.Pixels24[Y, X].r <> 255 Then Begin
                 r := NoteImage.Pixels24[Y, X].r;
                 FastIMG1.Bmp.Pixels24[(nY -16)+Y, (nX -16)+X].b := r;
                 FastIMG1.Bmp.Pixels24[(nY -16)+Y, (nX -16)+X].g := r;
                 FastIMG1.Bmp.Pixels24[(nY -16)+Y, (nX -16)+X].r := r;
              End;

        Inc(Idx);

     End;

     NoteImage.Free;

  End;

End;

procedure TBEEPWindow.FormDestroy(Sender: TObject);
begin
  SequenceList.Free;
end;

procedure TBEEPWindow.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  EditPos: Integer;
begin
  // Mouse down - if no operation, then we're going to edit the note at our current position.
  // This is performed by dragging the note up or down. If an operation is specified, then we
  // insert at the position immediately before that point.

  EditPos := GetScorePosition(X);

end;

Function TBEEPWindow.GetScorePosition(X: Integer): Integer;
Begin
  // Gets the list position of the mouse.

  Result := ((X - 64) Div 40)+SequenceViewOffset;

End;

end.
