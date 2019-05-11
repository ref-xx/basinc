unit AnimPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  Buttons, FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, FastDIBList, FastFiles;

type
  TAnimPreviewWindow = class(TForm)
    Panel1: TPanel;
    FastIMG1: TFastIMG;
    SpeedButton21: TSpeedButton;
    Label22: TLabel;
    Label21: TLabel;
    TrackBar3: TTrackBar;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    Image1: TImage;
    Image2: TImage;
    Label2: TLabel;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton21Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SizeDIB: TFastDIB;
    DIBList: TFastDIBList;
    Direction,
    CurrentFrame: Integer;
    Procedure RenderFrame(Index: Integer);
  end;

var
  AnimPreviewWindow: TAnimPreviewWindow;

implementation

{$R *.DFM}

Uses UDGEdit, FastCore, Filing;

procedure TAnimPreviewWindow.FormShow(Sender: TObject);
Var
  TempDIB: TFastDIB;
  Idx, Idx2, Idx3, Y, X: Integer;
  ByteVal, BitValue: Byte;
begin

  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 32);
  FastIMG1.Bmp.Clear(TfBtnFace);

  TempDIB := TFastDIB.Create;
  TempDIB.SetSize(UDGWindow.DataWidth * 8, UDGWindow.DataHeight, 8);
  TempDIB.Colors[0] := DisplayPalette[7];
  TempDIB.Colors[1] := DisplayPalette[0];
  TempDIB.UpdateColors;
  SizeDIB.SetSize(TempDIB.Width, TempDIB.Height, 8);
  DIBList.Clear;

  For Idx := 0 To UDGWindow.AnimList.Count -1 Do Begin

     X := 0;
     Y := TempDIB.AbsHeight -1;
     Idx2 := (GetWord(@UDGWindow.AnimList[Idx][3]) -1) * UDGWindow.DataWidth * UDGWindow.DataHeight +1;
     Idx3 := 0;

     While Y >= 0 Do Begin
        ByteVal := Ord(UDGWindow.CurChars[Idx2]);
        BitValue := 128;
        While BitValue > 0 Do Begin
           If ByteVal And BitValue <> 0 Then
              TempDIB.Pixels8[Y, X] := 1
           Else
              TempDIB.Pixels8[Y, X] := 0;
           BitValue := BitValue Shr 1;
           Inc(X);
        End;
        Inc(Idx3);
        If Idx3 Mod UDGWindow.DataWidth = 0 Then Begin
           Dec(Y);
           X := 0;
        End;
        Inc(Idx2);
     End;

     DIBList.Add(TempDIB);

  End;

  TempDIB.Free;

  CurrentFrame := 0;
  Direction := 1;
  RenderFrame(CurrentFrame);
  TrackBar3Change(Self);

  X := UDGWindow.DataWidth * 8;
  Y := UDGWindow.DataHeight;

  Idx := Max(X, Y);
  TrackBar1.Max := 1 + (256 Div Idx);
  TrackBar1.Position := 0;

  SpeedButton21.Glyph := Image1.Picture.Bitmap;

end;

procedure TAnimPreviewWindow.FormCreate(Sender: TObject);
begin

  DIBList := TFastDIBList.Create;
  SizeDIB := TFastDIB.Create;

end;

procedure TAnimPreviewWindow.FormDestroy(Sender: TObject);
begin

  DIBList.Clear;
  DIBList.Free;
  SizeDIB.Free;

end;

Procedure TAnimPreviewWindow.RenderFrame(Index: Integer);
Var
  SizeX, SizeY: Integer;
Begin

  If SizeDIB.Width <> 0 Then Begin

     Case Trackbar1.Position Of

        0:    Begin
                 SizeX := FastIMG1.Width Div SizeDIB.Width;
                 SizeY := FastIMG1.Height Div SizeDIB.Height;
              End;
     Else
        Begin
           SizeX := TrackBar1.Position;
           SizeY := SizeX;
        End;

     End;

     SizeX := Min(SizeX, SizeY);
     SizeY := SizeX;

     If (FastIMG1.Width <> SizeX * SizeDIB.Width) or (FastIMG1.Height <> SizeY * SizeDIB.Height) Then Begin
        FastIMG1.Bmp.SetSize(SizeX * SizeDIB.Width, SizeY * SizeDIB.Height, 32);
     End;

     DIBList.AssignTo(SizeDIB, Index);
     SizeDIB.Colors[0] := DisplayPalette[7];
     SizeDIB.Colors[1] := DisplayPalette[0];
     SizeDIB.UpdateColors;
     SizeDIB.Stretch(FastIMG1.Bmp.hDc, 0, 0, FastIMG1.bmp.Width, FastIMG1.Bmp.AbsHeight);
     FastIMG1.Repaint;

  End;

End;

procedure TAnimPreviewWindow.TrackBar3Change(Sender: TObject);
begin

  If TrackBar3.Position = 1 Then
     Label21.Caption := '1 Frame'
  Else
     Label21.Caption := IntToStr(TrackBar3.Position) + ' Frames';

  Timer1.Interval := 20 * TrackBar3.Position;
  Button1.Setfocus;

end;

procedure TAnimPreviewWindow.Timer1Timer(Sender: TObject);
begin

  RenderFrame(CurrentFrame);
  If Not Checkbox1.Checked Then Begin
     Inc(CurrentFrame);
     If CurrentFrame = UDGWindow.AnimList.Count -1 Then CurrentFrame := 0;
  End Else Begin
     Inc(CurrentFrame, Direction);
     If (CurrentFrame < 0) or (CurrentFrame = UDGWindow.AnimList.Count -1) Then Begin
        Direction := -Direction;
        If CurrentFrame < 0 Then Begin
           If UDGWindow.AnimList.Count = 1 Then
              CurrentFrame := 0
           Else
              CurrentFrame := 1;
        End Else Begin
           If UDGWindow.AnimList.Count = 1 Then
              CurrentFrame := 0
           Else
              CurrentFrame := UDGWindow.AnimList.Count -2;
        End;
     End;
  End;

end;

procedure TAnimPreviewWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  Timer1.Enabled := False;
  DIBList.Clear;

end;

procedure TAnimPreviewWindow.SpeedButton21Click(Sender: TObject);
begin

  Timer1.Enabled := Not Timer1.Enabled;
  If Timer1.Enabled Then
     SpeedButton21.Glyph.Assign(Image2.Picture.Bitmap)
  Else
     SpeedButton21.Glyph.Assign(Image1.Picture.Bitmap);

end;

procedure TAnimPreviewWindow.Button1Click(Sender: TObject);
begin

  Close;

end;

procedure TAnimPreviewWindow.Button2Click(Sender: TObject);
Var
  Idx, SizeX, SizeY: Integer;
  TimerEnabled: Boolean;
  TempDIB: TFastDIB;
begin

  TimerEnabled := Timer1.Enabled;
  Timer1.Enabled := False;

  Filename := OpenFile(Handle, 'Save GIF File', [FTGIF], '', True, False);
  If Filename = '' Then Exit;

  Case Trackbar1.Position Of
     0: Begin
           SizeX := FastIMG1.Width Div SizeDIB.Width;
           SizeY := FastIMG1.Height Div SizeDIB.Height;
        End;
  Else
     Begin
        SizeX := TrackBar1.Position;
        SizeY := SizeX;
     End;
  End;

  SizeX := Min(SizeX, SizeY);
  SizeY := SizeX;

  TempDIB := TFastDIB.Create;
  TempDIB.SetSize(SizeX * SizeDIB.Width, SizeY * SizeDIB.AbsHeight, 8);
  TempDIB.Colors[0] := DisplayPalette[7];
  TempDIB.Colors[1] := DisplayPalette[0];
  TempDIB.UpdateColors;

  For Idx := 0 To DIBList.Count -2 Do Begin
     DIBList.AssignTo(SizeDIB, Idx);
     SizeDIB.Stretch(TempDIB.hDc, 0, 0, TempDIB.Width, TempDIB.Height);
     If Idx = 0 Then
        FastFiles.SaveGIFFile(TempDIB, Filename, True, TrackBar3.Position * 2, True)
     Else
        FastFiles.AddGIFFrame(TempDIB, Filename, TrackBar3.Position * 2, True);
  End;
  AddGIFFrame(Nil, Filename, 0, True);
  TempDIB.Free;
  FormShow(Nil);

  Timer1.Enabled := TimerEnabled;

end;

procedure TAnimPreviewWindow.FormResize(Sender: TObject);
begin

  Label1.Caption := 'Size: '+IntToStr(FastIMG1.Bmp.Width) + 'x' + IntToStr(FastIMG1.Bmp.AbsHeight);

end;

procedure TAnimPreviewWindow.ComboBox1Change(Sender: TObject);
begin

  Button1.SetFocus;
  RenderFrame(CurrentFrame);
  FormResize(Nil);

end;

end.
