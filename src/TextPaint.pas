unit TextPaint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Math, Buttons, PaintBox, FastDIB, FastCore, FastFX, FastIMG,
  ExtCtrls, FastDrawEx;

type
  TTextForm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    ComboBox3: TComboBox;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    ComboBox4: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    FastIMG1: TFastIMG;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    LastWinFont, LastSpecFont: String;
    Fontlist: TStringlist;
    Backup: TScrImage;
    TextX, TextY: Integer;
    Rotation: Integer;
    Cancelled: Boolean;
    CharArray: Array[0..767] of Byte;
    R_MouseIsDown: Boolean;
    Procedure MakeRotationIcon;
    Procedure GetSpecFonts;
    Procedure CreateText;
    Procedure CalculateNewRotation(X, Y: Integer);
    Procedure SpecTextOut(DIB: TFastDIB; X, Y: Integer; Text: String; CharSet: Pointer);
  end;

var
  TextForm: TTextForm;

implementation

Uses Options, Utility;

{$R *.DFM}

Procedure TTextForm.MakeRotationIcon;
Var
  X, Y: Integer;
Begin

  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 24);
  FastIMG1.Bmp.Clear(TFWindow);
  FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width -1, FastIMG1.Bmp.Height -1, TfBlack);

  X := (FastIMG1.Bmp.Width Div 2) + Round((Cos(DegToRad(Rotation)) * (FastIMG1.Bmp.Width Div 2)));
  Y := (FastIMG1.Bmp.Height Div 2) + Round((Sin(DegToRad(Rotation)) * (FastIMG1.Bmp.Height Div 2)));
  SmoothLine(FastIMG1.Bmp, FastIMG1.Bmp.Width Div 2, FastIMG1.Bmp.Height Div 2, X, Y, TfBlack);

  FastIMG1.Repaint;

End;


procedure TTextForm.FormShow(Sender: TObject);
begin

  ScrPaintForm.CreateScreen(Backup);
  ScrPaintForm.CopyScreen(ScrPaintForm.CurScreen^, Backup);
  ComboBox4Change(nil);
  Memo1.SetFocus;
  MakeRotationIcon;
  Cancelled := True;

end;

procedure TTextForm.FormCreate(Sender: TObject);
begin

  FontList := TStringlist.Create;
  LastWinFont := 'MS Sans Serif';
  LastSpecFont := 'Default Sinclair';
  ComboBox2.ItemIndex := 1;
  ComboBox3.ItemIndex := 0;
  ComboBox4.ItemIndex := 0;
  Rotation := 0;
  Memo1.Lines.Clear;

end;

Procedure TTextForm.GetSpecFonts;
Var
  Idx: Integer;
  S: String;
Begin

  ComboBox1.Items.BeginUpdate;
  ComboBox1.Items.Clear;
  OptionsWindow.Edit1.Text := Opt_EditorFontFolder;
  OptionsWindow.GatherFonts;
  FontList.Clear;
  For Idx := 0 To InternalFontList.Count -1 Do Begin
     S := ExtractFilename(InternalFontList[Idx]);
     FontList.Add(S + #255 + InternalFontList[Idx]);
  End;
  FontList.Sort;
  For Idx := 0 To FontList.Count -1 Do Begin
     If FontList[Idx] = 'Default Sinclair'#255'Default Sinclair' Then
        ComboBox1.Items.Add('Default Sinclair')
     Else Begin
        S := Copy(FontList[Idx], 1, Pos('.', FontList[Idx]) -1);
        ComboBox1.Items.Add(S);
        S := Copy(FontList[Idx], Pos(#255, FontList[Idx])+1, 99999);
        FontList[Idx] := S;
     End;
  End;                  
  ComboBox1.Items.EndUpdate;

End;

procedure TTextForm.ComboBox4Change(Sender: TObject);
begin

  If ComboBox4.ItemIndex = 0 Then Begin
     ComboBox1.Items := Screen.Fonts;
     ComboBox1.ItemIndex := Max(0, ComboBox1.Items.IndexOf(LastWinFont));
     ComboBox2.Visible := True;
     SpeedButton1.Visible := True;
     SpeedButton2.Visible := True;
     SpeedButton3.Visible := True;
     Label4.Visible := True;
     Label5.Visible := True;
  End Else Begin
     GetSpecFonts;
     ComboBox1.ItemIndex := Max(0, ComboBox1.Items.IndexOf(LastSpecFont));
     ComboBox2.Visible := False;
     SpeedButton1.Visible := False;
     SpeedButton2.Visible := False;
     SpeedButton3.Visible := False;
     Label4.Visible := False;
     Label5.Visible := False;
     ComboBox1Change(Sender);
  End;

  CreateText;

end;

procedure TTextForm.FormDestroy(Sender: TObject);
begin

  FontList.Free;

end;

procedure TTextForm.ComboBox1Change(Sender: TObject);
Var
  Fl: TFileStream;
begin

  If ComboBox4.ItemIndex = 0 Then
     LastWinFont := ComboBox1.Items[ComboBox1.ItemIndex]
  Else Begin
     LastSpecFont := ComboBox1.Items[ComboBox1.ItemIndex];
     If FontList[ComboBox1.ItemIndex] = 'Default Sinclair'#255'Default Sinclair' Then 
        CopyMemory(@CharArray[0], @RealChars[1], 768)
     Else Begin
        FL := TFileStream.Create(FontList[ComboBox1.ItemIndex], fmOpenRead or fmShareDenyNone);
        FL.Read(CharArray[0], 768);
        FL.Free;
     End;
  End;

  CreateText;

end;

Procedure TTextForm.CreateText;
Var
  DIB, DIB2: TFastDIB;
  Idx, TextHeight, Offset, X, Y, Tx, Ty, MinX, MinY, MaxX, MaxY: Integer;
Begin

  ScrPaintForm.CopyScreen(Backup, ScrPaintForm.CurScreen^);

  // Create the text as a graphic first of all.

  DIB := TFastDIB.Create;
  DIB.SetSize(512, 384, 8);
  DIB.Colors := @DisplayPalette;
  DIB.UpdateColors;

  X := TextX;
  Y := TextY;

  Case ComboBox4.ItemIndex of

     0: // Windows font
        Begin

           Self.Canvas.Font.Name := ComboBox1.Items[ComboBox1.ItemIndex];
           Self.Canvas.Font.Size := StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]);
           If SpeedButton1.Down Then Self.Canvas.Font.Style := [fsBold] else Self.Canvas.Font.Style := [];

           DIB.Clear(TFBlack);
           DIB.SetFontEx(ComboBox1.Items[ComboBox1.ItemIndex], 0, StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]), 400 + (300 * Integer(SpeedButton1.Down)), SpeedButton2.Down, SpeedButton3.Down, False);
           DIB.SetTextColor(ClWhite);
           DIB.SetBkColor(ClBlack);

           For Idx := 0 To Memo1.Lines.Count -1 Do Begin
              TextHeight := Canvas.TextHeight(Memo1.Lines[Idx]);
              DIB.TextOut(X, Y, Memo1.Lines[Idx]);
              Inc(Y, TextHeight);
           End;

        End;

     1: // Spectrum font
        Begin
           DIB.Clear(TFBlack);
           For Idx := 0 To Memo1.Lines.Count -1 Do Begin
              SpecTextOut(DIB, X, Y - 8, Memo1.Lines[Idx], @CharArray[0]);
              Inc(Y, 8);
           End;
        End;

  End;

  Flop(DIB);
  DIB2 := TFastDIB.Create;
  DIB2.SetSize(DIB.Width, DIB.Height, DIB.Bpp);
  DIB2.Colors := DIB.Colors;
  DIB2.UpdateColors;
  ScrPaintForm.RotatePen(DIB, DIB2, TextX * 2, TextY *2, -Rotation, False);

  MinX := 9999; MinY := 9999;
  MaxX := 0; MaxY := 0;
  For X := 0 To DIB2.Width -1 Do
     For Y := 0 To DIB2.AbsHeight -1 Do
        If DIB2.Pixels8[Y, X] <> 0 Then Begin
           If X > MaxX Then MaxX := X;
           If X < MinX Then MinX := X;
           If Y > MaxY Then MaxY := Y;
           If Y < MinY Then MinY := Y;
        End;

  DIB.ClearB(0);
  For X := MinX To MaxX Do
     For Y := MinY To MaxY Do Begin
        Tx := TextX + (X - MinX) +2;
        Ty := TextY + (Y - MinY) - (MaxY - MinY) -1;
        If (Tx > 0) And (Ty > 0) and (Tx < 256) and (Ty < 192) Then
           DIB.Pixels8[Ty, Tx] := DIB2.Pixels8[Y, X];
     End;

  Case ComboBox3.ItemIndex of

     0: // Create as a graphic on the screen.
        Begin

           For X := 0 To 255 Do
              For Y := 0 To 191 Do
                 If DIB.Pixels8[Y, X] <> 0 Then Begin
                    ScrPaintForm.SetPixel(X, Y, 1, 0, 0, ptClone);
                 End;

        End;

     1: // Create as new selection.
        Begin

           ScrPaintForm.ClearSelection;
           MinX := 999; MinY := 999; MaxX := 0; MaxY := 0;
           For X := 0 To 255 Do
              For Y := 0 To 191 Do
                 If DIB.Pixels8[Y, X] <> 0 Then Begin
                    If X > MaxX Then MaxX := X;
                    If X < MinX Then MinX := X;
                    If Y > MaxY Then MaxY := Y;
                    If Y < MinY Then MinY := Y;
                 End;
           ScrPaintForm.CurScreen^.SelOrigin := Point(MinX, MinY);
           ScrPaintForm.CurScreen^.SelWidth := (MaxX - MinX) +1;
           ScrPaintForm.CurScreen^.SelHeight := (MaxY - MinY) +1;
           ScrPaintForm.CurScreen^.SelActive := True;
           SetLength(ScrPaintForm.CurScreen^.SelMask, ScrPaintForm.CurScreen^.SelWidth * ScrPaintForm.CurScreen^.SelHeight);
           SetLength(ScrPaintForm.CurScreen^.SelDetail, Length(ScrPaintForm.CurScreen^.SelMask));
           SetLength(ScrPaintForm.CurScreen^.SelAttrDetail, Length(ScrPaintForm.CurScreen^.SelMask));
           For X := MinX To MaxX Do
              For Y := MinY To MaxY Do
                 If DIB.Pixels8[Y, X] <> 0 Then Begin
                    OffSet := (X - MinX) + ((Y - MinY) * ScrPaintForm.CurScreen^.SelWidth);
                    ScrPaintForm.CurScreen^.SelDetail[Offset] := 1;
                    ScrPaintForm.CurScreen^.SelMask[Offset] := 1;
                    If ScrPaintForm.ColourMode in [cmBoth, cmAttrs] Then
                       ScrPaintForm.CurScreen^.SelAttrDetail[Offset] :=
                       ScrPaintForm.GetNewAttr(ScrPaintForm.CurScreen^.SelAttrDetail[Offset],
                       ScrPaintForm.CurINK,
                       ScrPaintForm.CurPAPER,
                       ScrPaintForm.CurBRIGHT,
                       ScrPaintForm.CurFlash,
                       ScrPaintForm.AttrFlags);
                 End;


        End;

  End;

  ScrPaintForm.RenderScreen(ScrPaintForm.CurScreenIndex, ScrPaintForm.ShowingAttrs);
  DIB.Free;
  DIB2.Free;

End;

procedure TTextForm.Memo1Change(Sender: TObject);
begin

  CreateText;

end;

procedure TTextForm.ComboBox2Change(Sender: TObject);
begin

  CreateText;

end;

procedure TTextForm.Button1Click(Sender: TObject);
begin

  Close;

end;

procedure TTextForm.Button2Click(Sender: TObject);
begin

  Cancelled := False;
  Close;

end;

Procedure TTextForm.SpecTextOut(DIB: TFastDIB; X, Y: Integer; Text: String; CharSet: Pointer);
Var
  F, G, H, Offset, ArrayOffset, S, ItalicPos: Integer;
  Bits, RealInk, RealPaper: Integer;
  NormalChars: Boolean;
  Ptr: PByte;
Begin

  Y := DIB.Height - 1 - Y;

  For F := 1 To Length(Text) Do Begin

     S := Ord(Text[F]);

     If Not (S in [32..127]) Then S := Ord('?');
     Offset := S - 32;
     ArrayOffset := OffSet*8;
     Ptr := Pointer(DWord(CharSet) + ArrayOffset);

     For G := 0 To 7 Do Begin
        Bits := Byte(Ptr^);
        Inc(Ptr);
        For H := 7 DownTo 0 Do Begin
           If PtInRect(Rect(0, 0, DIB.Width, DIB.AbsHeight), Point(X+(7-H), Y-G)) Then Begin
              If Bits and (1 Shl H) > 0 Then
                 DIB.Pixels8[Y-G, X+(7-H)] := 7
              Else
                 DIB.Pixels8[Y-G, X+(7-H)] := 0;
           End;
        End;
     End;

     Inc(X, 8);

  End;

End;

procedure TTextForm.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  CalculateNewRotation(X, Y);
  R_MouseIsDown := True;

end;

Procedure TTextForm.CalculateNewRotation(X, Y: Integer);
Begin

  Rotation := 360 - Round(RadToDeg(Arctan2(Y - (FastIMG1.Height Div 2), X - (FastIMG1.Width Div 2))));
  MakeRotationIcon;
  CreateText;

End;

procedure TTextForm.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin

  If R_MouseIsDown Then CalculateNewRotation(X, Y);

end;

procedure TTextForm.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  R_MouseIsDown := False;

end;

procedure TTextForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  ScrPaintForm.ToolButton1.Enabled := True;
  ScrPaintForm.ToolButton2.Enabled := True;
  ScrPaintForm.ToolButton4.Enabled := True;
  ScrPaintForm.ToolButton5.Enabled := True;
  ScrPaintForm.ToolButton6.Enabled := True;
  ScrPaintForm.ToolButton8.Enabled := True;

  ScrPaintForm.PaintMode := pmTextPlace;

  If Cancelled Then ScrPaintForm.CopyScreen(TextForm.Backup, ScrPaintForm.CurScreen^);

  ScrPaintForm.MouseIsDown := False;
  If ComboBox3.ItemIndex = 1 Then
     ScrPaintForm.MenuItemClick(ScrPaintForm.Select1);

  ScrPaintForm.RenderScreen(ScrPaintForm.CurScreenIndex, ScrPaintForm.ShowingAttrs);
  ScrPaintForm.MakeUndo('Text', True);

end;

end.
