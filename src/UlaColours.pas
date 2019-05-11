unit UlaColours;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FastIMG, CheckLst, ExtCtrls, FastDrawEx, FastDIB, FastDraw, Math,
  Utility;

type
   TUlaColoursWindow = class(TForm)
    Label2: TLabel;
    PresetPalette: TFastIMG;
    Label1: TLabel;
    Bevel1: TThemeBevel;
    ThemeBevel1: TThemeBevel;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure PresetPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);

  private
    { Private declarations }

  public
    { Public declarations }
    Procedure DrawPresets;
    Function FindIndex(Clor: TFColorA): Integer;
    Function GetRGB(Clor: Integer): String;
    Procedure WriteULAPresets;
  end;

var
  UlaColoursWindow: TUlaColoursWindow;
  UlaPalette256:    TFColorTable;
  SelectedUla64: integer;
  SelectedPreset: integer;

implementation

{$R *.DFM}

uses BASinMain, FastCore, AddCode;

procedure TUlaColoursWindow.FormShow(Sender: TObject);
begin
  DrawPresets;
  SelectedUla64:=0;
  SelectedPreset:=0;


end;

Procedure TUlaColoursWindow.DrawPresets;
var
  Idx, Clr, Boxes, BoxSize, PosPal, BoxX, BoxY: Integer;
  Italic, Bold: Boolean;
  Col: TfColor;
Begin
  PosPal:=300; //X position of Palette
  BoxX:=PosPal;
  BoxY:=1;
  BoxSize:=16;
  Boxes:=16; // how many boxes side by side?

  PresetPalette.Width:=302+((BoxSize)*Boxes);
  PresetPalette.Height:=4+((BoxSize)*Boxes);
  PresetPalette.Bmp.SetSize(PresetPalette.Width, PresetPalette.Height, 24);
  PresetPalette.Bmp.Clear(FastDraw.TColorToTFColor(ClBtnFace));
  For Idx := 0 To 255 Do Begin
     //FastDrawEx.Rectangle(PresetPalette.Bmp, (Idx * 16)+2, 2, (Idx * 16)+16, 15, TfBlack);

        UlaPalette256[Idx].b := Get64ColourByte(((Idx and 3) Shl 1) + Idx And 1);
        UlaPalette256[Idx].r := Get64ColourByte((Idx and 28) Shr 2);
        UlaPalette256[Idx].g := Get64ColourByte((Idx and 224) Shr 5);

     FastDrawEx.FillRect(PresetPalette.Bmp, BoxX, BoxY, BoxX+BoxSize, BoxY+BoxSize, TFColorAToTFColor(UlaPalette256[Idx]));

     BoxX:=BoxX+BoxSize;
     if (BoxX+BoxSize)> (PosPal+(BoxSize)*Boxes) Then
     Begin
         BoxX:=PosPal;
         Inc(BoxY,BoxSize);
     End;
  End;

  PosPal:=2; //X position of Palette
  BoxX:=PosPal;
  BoxY:=1;
  BoxSize:=32;
  Boxes:=8;
  For Idx := 0 to 63  Do Begin

     col:=TfSilver;

     if idx>15 then col:=TfRed;
     if idx>31 then col:=TfGray;
     if idx>47 then col:=TfBlack;
     if idx>55 then col:=TfBlue;

     if (SelectedUla64=Idx) Then
     Begin

        FastDrawEx.Rectangle(PresetPalette.Bmp, BoxX+1, BoxY+1, -1+BoxX+BoxSize, -1+BoxY+BoxSize, col);

        FastDrawEx.FillRect(PresetPalette.Bmp, BoxX+2, BoxY+3, BoxX+BoxSize-2, BoxY+BoxSize, TFColorAToTFColor(DisplayPalette64[Idx]));

     End else Begin

        FastDrawEx.Rectangle(PresetPalette.Bmp, BoxX+1, BoxY+1, -1+BoxX+BoxSize, -1+BoxY+BoxSize, col);

     End;

     FastDrawEx.FillRect(PresetPalette.Bmp, BoxX+3, BoxY+5, BoxX+BoxSize-3, BoxY+BoxSize-1, TFColorAToTFColor(DisplayPalette64[Idx]));

     BoxX:=BoxX+BoxSize;
     if (BoxX+BoxSize)> (PosPal+(BoxSize* Boxes)) Then
     Begin
         BoxX:=PosPal;
         Inc(BoxY,BoxSize);
     End;
  End;



  FastDrawEx.Rectangle(PresetPalette.Bmp, 300, 2, PresetPalette.Bmp.Width-2, PresetPalette.Bmp.Height , TfBlack);

  PresetPalette.Repaint;


End;

procedure TUlaColoursWindow.PresetPaletteMouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Xp, Yp, Cx, SelectedPreset, Idx: Integer;
  s: String;
begin

  If X > 303 then
  Begin
    // Clicked on Presets
    Xp:=X-303;
    Xp:= Xp Div 16;
    Yp:= Y Div 16;
    SelectedPreset:=Xp +((Yp)*16);
    s:=inttostr(SelectedPreset);
    Edit2.Text:='Palette Entry ' + inttostr(SelectedUla64) +' set to color '+ s +' ('+ GetRGB(SelectedPreset)+').';
    DisplayPalette64[SelectedUla64]:= UlaPalette256[SelectedPreset];
    DrawPresets;
  End else if X<256 then Begin
    // Ula64 entry Selected
    Xp:= X Div 32;
    Yp:= Y Div 32;
    SelectedUla64:=Xp +((Yp)*8);
    Cx:=SelectedUla64;
    Idx:=FindIndex(DisplayPalette64[SelectedUla64]);
    s:=inttostr(Cx)+' > ';
    if Cx>47 then
    Begin
      s:=s+'FLASH 1; BRIGHT 1; ';
      Cx:=Cx-48;
    end;
    if Cx>31 then
    Begin
      s:=s+'FLASH 1; BRIGHT 0; ';
      Cx:=Cx-32;
    end;
    if Cx>15 then
    Begin
      s:=s+'FLASH 0; BRIGHT 1; ';
      Cx:=Cx-16;
    end;
    if Cx>7 then begin
      s:=s+'PAPER '+ inttostr(Cx-8)+'; ';
      Cx:=Cx-8;
    end else Begin
      s:=s+'INK '+ inttostr(Cx)+'; ';
    end;
    Edit2.Text:=s + ' (' + GetRGB(Idx)+')';
    DrawPresets;
  End;

end;



function TUlaColoursWindow.FindIndex(Clor: TFColorA): Integer;
var
  Idx : Integer;

Begin

  For Idx := 0 To 255 Do Begin
    if (TFColorAToTColor(UlaPalette256[Idx])=TFColorAToTColor(Clor)) Then
    Begin
      Result := Idx;
      Exit;
    End;
  Result:=256;
 End;
End;
procedure TUlaColoursWindow.Button4Click(Sender: TObject);
var
Idx,x: Integer;
begin
x:=4;
    For Idx := 0 To 63 Do Begin
        //Temp := Random(256);


        DisplayPalette64[Idx]:= UlaPalette256[idx*x];
    End;
    DrawPresets;


     Idx:=Combobox1.Items.indexof(Combobox1.Text);
     if Idx<0 then Begin
       Combobox1.Items.Add( Combobox1.Text);
     end Else Begin
       ShowMessage('Preset already exists in the list. Please use a different name.');
     end;
end;

procedure TUlaColoursWindow.Button3Click(Sender: TObject);
var
Idx: Integer;
begin
Idx:=Combobox1.Items.indexof(Combobox1.Text);
     if Idx>-1 then Begin
       ComboBox1.Items.Delete(Idx);
     end Else Begin
       ShowMessage('Preset not found in the list.');
     end;

end;




procedure TUlaColoursWindow.WriteULAPresets;
var
  UINI:         TStringList;
  tempPalette:  String;
  Idx:          Integer;
begin
  UINI := TStringlist.Create;
  tempPalette:='';
  For Idx := 0 To ComboBox1.Items.Count Do Begin

  End;

  //INIWrite('Palette', 'Combo', DisplayPalette64[SelectedUla64]);


end;

procedure TUlaColoursWindow.Button1Click(Sender: TObject);
begin
Close;
end;

procedure TUlaColoursWindow.Button2Click(Sender: TObject);
Var
  Idx: Integer;
  NewCode: TStringlist;
  NewCodePresent: Boolean;
  CurLineNum: Integer;
  TempStr:String;
begin
        CurLineNum:= StrToInt(Edit1.Text);
        NewCode := TStringlist.Create;
        TempStr := IntToStr(CurLineNum)+' RESTORE '+IntToStr(CurLineNum)+': FOR F=0 TO 63: READ A: OUT 48955, F: OUT 65339, A: NEXT F: DATA ';
        For Idx := 0 To 63 Do Begin
          TempStr := TempStr + IntToStr(Ord(FindIndex(DisplayPalette64[Idx])))+',';
        End;
        NewCode.Add(Copy(TempStr, 1, Length(TempStr)-1));

        AddCodeWindow.ClearCode;
        AddCodeWindow.AddCode(NewCode);
        CentreFormOnForm(AddCodeWindow, Self);
        ShowWindow(AddCodeWindow, True);
        NewCode.Clear;
end;

procedure TUlaColoursWindow.ComboBox1Change(Sender: TObject);
Var
Idx, Temp,x: Integer;
Const
RawData: array[0..63] of Byte = (
  $00, $02, $18, $1B, $C0, $C3, $D8, $DB, $00, $02, $18, $1B, $C0, $C3, $D8,
  $DB, $00, $03, $1C, $1F, $E0, $E3, $FC, $FF, $00, $03, $1C, $1F, $E0, $E3,
  $FC, $FF, $DB, $D8, $C3, $C0, $1B, $18, $02, $00, $DB, $D8, $C3, $C0, $1B,
  $18, $02, $00, $FF, $FC, $E3, $E0, $1F, $1C, $03, $00, $FF, $FC, $E3, $E0,
  $1F, $1C, $03, $00
);        //for testing purposes
begin
x:=4;
    For Idx := 0 To 63 Do Begin
        Temp := Random(256);


        DisplayPalette64[Idx]:= UlaPalette256[idx*x];
    End;
    DrawPresets;
end;

function TUlaColoursWindow.GetRGB(Clor: Integer): String;
var
r,g,b:integer;

begin
//((G*32)+(R*4)+B)

g:=Clor Div 32;
Clor:= Clor-(g*32);
r:=Clor Div 4;
Clor:= Clor-(r*4);
b:=Clor;
Result:='RGB:' + inttostr(r) + inttostr(g) + inttostr(b);
end;

end.
