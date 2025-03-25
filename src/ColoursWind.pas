unit ColoursWind;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FastIMG, CheckLst, ExtCtrls, FastDrawEx, FastDIB, FastDraw, Math,
  Utility;

type
  TColoursWindow = class(TForm)
    Label1: TLabel;
    Bevel1: TThemeBevel;
    CheckListBox1: TCheckListBox;
    FastIMG1: TFastIMG;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    ButtonApply: TButton;
    ComboBox1: TComboBox;
    CheckBox3: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    Procedure SetUpList;
    Procedure DrawColours;
    Procedure DrawCheck(Bmp: TFastDIB; X, Y: Integer);

  end;
    function IsValidHexColor(HexColor: string): Boolean;
var
  ColoursWindow: TColoursWindow;
  ColorIndex: integer;

Const

  COLBOXW = 32;
  COLBOXH = 20;

implementation

{$R *.DFM}

Uses BASinMain, FastCore;


function IsValidHexColor( HexColor: string): Boolean;
var
  HexDigits: set of Char;
  i: Integer;
begin
  // Initialize set of valid hexadecimal characters
  HexDigits := ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

  // Check if the string starts with '#' and has exactly 6 characters
  if (Length(HexColor) <> 7) or (HexColor[1] <> '#') then
  begin
    Result := False;
    Exit;
  end;

  // Check if all characters after '#' are valid hexadecimal digits
  for i := 2 to Length(HexColor) do
  begin
    if not (HexColor[i] in HexDigits) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TColoursWindow.FormShow(Sender: TObject);
begin
  CheckBox3.Checked:=Opt_CursorBlinking;
  SetupList;
  DrawColours;
end;

Procedure TColoursWindow.SetupList;
Begin
  CheckListBox1.Checked[0]  := Opt_HighlightKeywords;
  CheckListBox1.Checked[1]  := Opt_HighlightFunctions;
  CheckListBox1.Checked[2]  := Opt_HighlightComments;
  CheckListBox1.Checked[3]  := Opt_HighlightSymbols;
  CheckListBox1.Checked[4]  := Opt_HighlightVars;
  CheckListBox1.Checked[5]  := Opt_HighlightVarsUnDef;
  CheckListBox1.Checked[6]  := Opt_HighlightNumbers;
  CheckListBox1.Checked[7]  := Opt_HighlightLineNums;
  CheckListBox1.Checked[8]  := Opt_HighlightStrings;
  CheckListBox1.Checked[9]  := True;
  CheckListBox1.Checked[10] := True;
  CheckListBox1.Checked[11] := True;
  CheckListBox1.Checked[12] := True;
End;

Procedure TColoursWindow.DrawColours;
Var
  Idx, Clr: Integer;
  Italic, Bold: Boolean;
  ColorBoxW,ColorBoxH,ColorBoxCount: Integer;
Begin
  ColorBoxW:=COLBOXW;
  ColorBoxH:=COLBOXH;
  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 24);
  FastIMG1.Bmp.Clear(TfBtnFace);
  //FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width -1, FastIMG1.Bmp.Height -1, TfBlack);
  For Idx := 0 To 7 Do Begin
     FastDrawEx.FillRect (FastIMG1.Bmp, 2+(Idx * ColorBoxW), 2, (Idx * ColorBoxW)+ColorBoxW, ColorBoxH, TfBlack);
     FastDrawEx.FillRect (FastIMG1.Bmp, 3+(Idx * ColorBoxW), 3, (Idx * ColorBoxW)+ColorBoxW-1, ColorBoxH-1, TFColorAToTFColor(DisplayPalette[Idx]));
     FastDrawEx.FillRect (FastIMG1.Bmp, 2+(Idx * ColorBoxW), 2+ColorBoxH, (Idx * ColorBoxW)+ ColorBoxW, ColorBoxH*2, TfBlack);
     FastDrawEx.FillRect (FastIMG1.Bmp, 3+(Idx * ColorBoxW), 3+ColorBoxH, (Idx * ColorBoxW)+ ColorBoxW-1, ColorBoxH*2-1, TFColorAToTFColor(DisplayPalette[Idx+8]));
     FastDrawEx.FillRect (FastIMG1.Bmp, 2+(Idx * ColorBoxW), 2+ColorBoxH*2, (Idx * ColorBoxW)+ ColorBoxW, ColorBoxH*3, TfBlack);
     FastDrawEx.FillRect (FastIMG1.Bmp, 3+(Idx * ColorBoxW), 3+ColorBoxH*2, (Idx * ColorBoxW)+ ColorBoxW-1, ColorBoxH*3-1, TFColorAToTFColor(DisplayPalette[Idx+24]));

  End;

  If CheckListBox1.ItemIndex <> -1 Then Begin
     If CheckListBox1.ItemIndex in [9, 10] Then Begin
        CheckBox2.Enabled := False;
        CheckBox1.Enabled := False;
     End Else Begin
        CheckBox2.Enabled := True;
        CheckBox1.Enabled := True;
     End;
     Case CheckListBox1.ItemIndex Of
        0: Begin Clr := Opt_KeywordsColour; Italic := Opt_KeywordsItalic; Bold := Opt_KeywordsBold; End;
        1: Begin Clr := Opt_FunctionsColour; Italic := Opt_FunctionsItalic; Bold := Opt_FunctionsBold; End;
        2: Begin Clr := Opt_CommentsColour; Italic := Opt_CommentsItalic; Bold := Opt_CommentsBold; End;
        3: Begin Clr := Opt_SymbolsColour; Italic := Opt_SymbolsItalic; Bold := Opt_SymbolsBold; End;
        4: Begin Clr := Opt_VarsColour; Italic := Opt_VarsItalic; Bold := Opt_VarsBold; End;
        5: Begin Clr := Opt_VarsUnDefColour; Italic := Opt_VarsUnDefItalic; Bold := Opt_VarsUnDefBold; End;
        6: Begin Clr := Opt_NumbersColour; Italic := Opt_NumbersItalic; Bold := Opt_NumbersBold; End;
        7: Begin Clr := Opt_LineNumsColour; Italic := Opt_LineNumsItalic; Bold := Opt_LineNumsBold; End;
        8: Begin Clr := Opt_StringsColour; Italic := Opt_StringsItalic; Bold := Opt_StringsBold; End;
        9: Begin Clr := Opt_Foreground; Italic := False; Bold := False; End;
       10: Begin Clr := Opt_Background; Italic := False; Bold := False; End;
       11: Begin Clr := Opt_CursorColor1; Italic := False; Bold := False; End;
       12: Begin Clr := Opt_CursorColor2; Italic := False; Bold := False; End;

     End;
     CheckBox1.Checked := Bold;
     CheckBox2.Checked := Italic;
     If CheckListBox1.Checked[CheckListBox1.ItemIndex] Then Begin
        If Clr < 8 Then Begin
           DrawCheck(FastIMG1.Bmp, (Clr * ColorBoxW) + ColorBoxW div 2, ColorBoxH div 2-2);
        End Else Begin
           If Clr < 16 Then Begin
               DrawCheck(FastIMG1.Bmp, ((Clr -8) * ColorBoxW) + ColorBoxW div 2, ColorBoxH+ColorBoxH div 2 -2);
           End Else Begin
               DrawCheck(FastIMG1.Bmp, ((Clr -24) * ColorBoxW) + ColorBoxW div 2, (ColorBoxH*2)+ColorBoxH div 2 -2);
           End;
        End;
     End Else Begin
        AlphaFillRect(FastIMG1.Bmp, 1, 1, FastIMG1.Bmp.Width -2, FastIMG1.Bmp.AbsHeight -2, TFBtnFace);
        AlphaFillRect(FastIMG1.Bmp, 1, 1, FastIMG1.Bmp.Width -2, FastIMG1.Bmp.AbsHeight -2, TFBtnFace);
        CheckBox2.Enabled := False;
        CheckBox1.Enabled := False;
     End;
  End Else Begin
     AlphaFillRect(FastIMG1.Bmp, 1, 1, FastIMG1.Bmp.Width -2, FastIMG1.Bmp.AbsHeight -2, TFBtnFace);
     AlphaFillRect(FastIMG1.Bmp, 1, 1, FastIMG1.Bmp.Width -2, FastIMG1.Bmp.AbsHeight -2, TFBtnFace);
     CheckBox2.Enabled := False;
     CheckBox1.Enabled := False;
  End;
  FastIMG1.Repaint;
  BASinOutput.RepaintBASIC(True);
End;

Procedure TColoursWindow.DrawCheck(Bmp: TFastDIB; X, Y: Integer);
Var
  Xp, Yp: Integer;
Const
  CheckArray: Array[0..8, 0..8] of Byte =
     ((0, 0, 0, 0, 0, 0, 0, 2, 0),
      (0, 0, 0, 0, 0, 0, 2, 1, 2),
      (0, 2, 0, 0, 0, 2, 1, 1, 2),
      (2, 1, 2, 0, 2, 1, 1, 1, 2),
      (2, 1, 1, 2, 1, 1, 1, 2, 0),
      (2, 1, 1, 1, 1, 1, 2, 0, 0),
      (0, 2, 1, 1, 1, 2, 0, 0, 0),
      (0, 0, 2, 1, 2, 0, 0, 0, 0),
      (0, 0, 0, 2, 0, 0, 0, 0, 0));
Begin
  For Xp := 0 to 8 Do
     For Yp := 0 To 8 Do
        If CheckArray[Yp, Xp] = 1 Then
           Bmp.Pixels24[Bmp.AbsHeight - (Yp+Y), Xp+X] := TfWhite
        Else If CheckArray[Yp, Xp] = 2 Then
           Bmp.Pixels24[Bmp.AbsHeight - (Yp+Y), Xp+X] := TfBlack;

End;

procedure TColoursWindow.CheckListBox1ClickCheck(Sender: TObject);
begin
  CheckListBox1.Checked[9] := True;
  CheckListBox1.Checked[10] := True;
  Case CheckListBox1.ItemIndex Of
     0: Opt_HighlightKeywords := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     1: Opt_HighlightFunctions := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     2: Opt_HighlightComments := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     3: Opt_HighlightSymbols := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     4: Opt_HighlightVars := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     5: Opt_HighlightVarsUnDef := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     6: Opt_HighlightNumbers := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     7: Opt_HighlightLineNums := CheckListBox1.Checked[CheckListBox1.ItemIndex];
     8: Opt_HighlightStrings := CheckListBox1.Checked[CheckListBox1.ItemIndex];
  End;
  DrawColours;
end;

procedure TColoursWindow.CheckListBox1Click(Sender: TObject);
begin
  DrawColours;
end;

procedure TColoursWindow.FormCreate(Sender: TObject);
begin
  {ClientWidth := 259 + 16;
  Button1.SetBounds(8, ClientHeight - (Button1.Height +8), Button1.Width, Button1.Height);
  CheckBox2.SetBounds(ClientWidth - (Max(CheckBox2.Width, CheckBox1.Width) + 8), ClientHeight - (CheckBox2.Height + 8), CheckBox2.Width, CheckBox2.Height);
  CheckBox1.SetBounds(CheckBox2.Left, CheckBox2.Top - (CheckBox1.Height + 4), CheckBox1.Width, CheckBox1.Height);
  FastIMG1.SetBounds(8, CheckBox1.Top - 67, 259, 63);
  Label2.SetBounds(8, FastIMG1.Top - (Label2.Height + 4), Label2.Width, Label2.Height);
  Label1.SetBounds(8, 4, Label1.Width, Label1.Height);
  Bevel1.SetBounds(8, Label1.Height + 8, ClientWidth - 16, 2);
  Button2.SetBounds(Button1.Left + Button1.Width + 4, Button1.Top, Button2.Width, Button1.Height);
  CheckListBox1.SetBounds(8, Bevel1.Top + 10, ClientWidth - 16, Label2.Top - (Bevel1.Top + 14));
}
  ComboBox1.ItemIndex:=0;
  
end;

procedure TColoursWindow.CheckBox1Click(Sender: TObject);
begin
  Case CheckListBox1.ItemIndex Of
     0: Opt_KeywordsBold := CheckBox1.Checked;
     1: Opt_FunctionsBold := CheckBox1.Checked;
     2: Opt_CommentsBold := CheckBox1.Checked;
     3: Opt_SymbolsBold := CheckBox1.Checked;
     4: Opt_VarsBold := CheckBox1.Checked;
     5: Opt_VarsUnDefBold := CheckBox1.Checked;
     6: Opt_NumbersBold := CheckBox1.Checked;
     7: Opt_LineNumsBold := CheckBox1.Checked;
     8: Opt_StringsBold := CheckBox1.Checked;
  End;
  DrawColours;
end;

procedure TColoursWindow.CheckBox2Click(Sender: TObject);
begin
  Case CheckListBox1.ItemIndex Of
     0: Opt_KeywordsItalic := CheckBox2.Checked;
     1: Opt_FunctionsItalic := CheckBox2.Checked;
     2: Opt_CommentsItalic := CheckBox2.Checked;
     3: Opt_SymbolsItalic := CheckBox2.Checked;
     4: Opt_VarsItalic := CheckBox2.Checked;
     5: Opt_VarsUnDefItalic := CheckBox2.Checked;
     6: Opt_NumbersItalic := CheckBox2.Checked;
     7: Opt_LineNumsItalic := CheckBox2.Checked;
     8: Opt_StringsItalic := CheckBox2.Checked;
  End;
  DrawColours;
end;

procedure TColoursWindow.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  X := X Div COLBOXW;
  If X > 7 then X := 7;
  Y := Y Div COLBOXH;
  If Y > 2 then Y := 2;


  If Y <2 Then X:= X+ 8*Y;
  If (Y=2) Then X:=X+24;
  ColorIndex:=X;
  if X>23 Then Begin
    ButtonApply.Enabled:=True;
    Edit1.Enabled:=True;
    Case X of

      24:    Edit1.Text:=ColorToHex(TFCol24);
      25:    Edit1.Text:=ColorToHex(TFCol25);
      26:    Edit1.Text:=ColorToHex(TFCol26);
      27:    Edit1.Text:=ColorToHex(TFCol27);
      28:    Edit1.Text:=ColorToHex(TFCol28);
      29:    Edit1.Text:=ColorToHex(TFCol29);
      30:    Edit1.Text:=ColorToHex(TFCol30);
      31:    Edit1.Text:=ColorToHex(TFCol31);

    End;

    End Else Begin
    ButtonApply.Enabled:=False;
    Edit1.Enabled:=False;
  End;

  Case CheckListBox1.ItemIndex Of
     0: Opt_KeywordsColour := X;
     1: Opt_FunctionsColour := X;
     2: Opt_CommentsColour := X;
     3: Opt_SymbolsColour := X;
     4: Opt_VarsColour := X;
     5: Opt_VarsUnDefColour := X;
     6: Opt_NumbersColour := X;
     7: Opt_LineNumsColour := X;
     8: Opt_StringsColour := X;
     9: Opt_Foreground := X;
    10: Begin Opt_Background := X; BASinOutput.SetDark; End;
    11: Opt_CursorColor1 := X;
    12: Opt_CursorColor2 := X;
  End;
  DrawColours;
end;

procedure TColoursWindow.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TColoursWindow.Button2Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_editor_fonts.html#syntax'), HH_DISPLAY_TOPIC, 0);

end;

procedure TColoursWindow.ButtonApplyClick(Sender: TObject);
begin
   if Not IsValidHexColor(Edit1.Text) Then Begin
     Edit1.Text:='#000000';
     Exit;
   End;
   case ColorIndex of
      24: TFCol24:=HexToColor(Edit1.Text);
      25: TFCol25:=HexToColor(Edit1.Text);
      26: TFCol26:=HexToColor(Edit1.Text);
      27: TFCol27:=HexToColor(Edit1.Text);
      28: TFCol28:=HexToColor(Edit1.Text);
      29: TFCol29:=HexToColor(Edit1.Text);
      30: TFCol30:=HexToColor(Edit1.Text);
      31: TFCol31:=HexToColor(Edit1.Text);
   End;

     BuildPalette([TFSpecBlack, TFSpecBlue,  TFSpecRed,  TFSpecMagenta,  TFSpecGreen,  TFSpecCyan,  TFSpecYellow,  TFSpecWhite,
                TFSpecBlack, TFSpecBlueB, TFSpecRedB, TFSpecMagentaB, TFSpecGreenB, TFSpecCyanB, TFSpecYellowB, TFSpecWhiteB,
                TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,
                TFCol24,TFCol25,TFCol26,TFCol27,TFCol28,TFCol29,TFCol30,TFCol31]);
     DrawColours;

end;


procedure TColoursWindow.ComboBox1Change(Sender: TObject);
begin
 Case ComboBox1.ItemIndex of
    0,1:
        Begin
        Opt_Background:=15       ;
        Opt_Foreground:=0        ;
        Opt_StringsItalic:=false     ;
        Opt_Stringsbold:=false       ;
        Opt_StringsColour:=0     ;
        Opt_HighlightStrings:=true  ;
        Opt_LineNumsItalic:=false    ;
        Opt_LineNumsbold:=false      ;
        Opt_LineNumsColour:=0    ;
        Opt_HighlightLineNums:=true ;
        Opt_NumbersItalic:=false     ;
        Opt_Numbersbold:=false       ;
        Opt_NumbersColour:=0     ;
        Opt_HighlightNumbers:=true  ;
        Opt_VarsUnDefItalic:=false   ;
        Opt_VarsUnDefbold:=false     ;
        Opt_VarsUnDefColour:=2   ;
        Opt_HighlightVarsUnDef:=true;
        Opt_VarsItalic:=false        ;
        Opt_Varsbold:=false          ;
        Opt_VarsColour:=1        ;
        Opt_HighlightVars:=true     ;
        Opt_SymbolsItalic:=false     ;
        Opt_Symbolsbold:=false       ;
        Opt_SymbolsColour:=0     ;
        Opt_HighlightSymbols:=true ;
        Opt_CommentsItalic:=true    ;
        Opt_Commentsbold:=false      ;
        Opt_CommentsColour:=6    ;
        Opt_HighlightComments:=true ;
        Opt_FunctionsItalic:=false   ;
        Opt_Functionsbold:=true     ;
        Opt_FunctionsColour:=0   ;
        Opt_HighlightFunctions:=true;
        Opt_KeywordsItalic:=false    ;
        Opt_Keywordsbold:=true      ;
        Opt_KeywordsColour:=0    ;
        Opt_HighlightKeywords:=true;
        Opt_TFCol31:='#FF7F27';
        Opt_TFCol30:='#2F9F7F';
        Opt_TFCol29:='#5374D7';
        Opt_TFCol28:='#AA2E63';
        Opt_TFCol27:='#AA66AA';
        Opt_TFCol26:='#994444';
        Opt_TFCol25:='#4646DC';
        Opt_TFCol24:='#555555';

        End;
        2:
        Begin
        Opt_Background:=7          ;
        Opt_Foreground:=0          ;
        Opt_StringsItalic:=false       ;
        Opt_Stringsbold:=false         ;
        Opt_StringsColour:=0       ;
        Opt_HighlightStrings:=true    ;
        Opt_LineNumsItalic:=false      ;
        Opt_LineNumsbold:=false        ;
        Opt_LineNumsColour:=0      ;
        Opt_HighlightLineNums:=true   ;
        Opt_NumbersItalic:=false       ;
        Opt_Numbersbold:=false         ;
        Opt_NumbersColour:=0       ;
        Opt_HighlightNumbers:=true    ;
        Opt_VarsUnDefItalic:=false     ;
        Opt_VarsUnDefbold:=false       ;
        Opt_VarsUnDefColour:=2     ;
        Opt_HighlightVarsUnDef:=true  ;
        Opt_VarsItalic:=false          ;
        Opt_Varsbold:=false            ;
        Opt_VarsColour:=1          ;
        Opt_HighlightVars:=true       ;
        Opt_SymbolsItalic:=false       ;
        Opt_Symbolsbold:=false         ;
        Opt_SymbolsColour:=0       ;
        Opt_HighlightSymbols:=true    ;
        Opt_CommentsItalic:=true      ;
        Opt_Commentsbold:=false        ;
        Opt_CommentsColour:=1      ;
        Opt_HighlightComments:=true   ;
        Opt_FunctionsItalic:=false     ;
        Opt_Functionsbold:=true       ;
        Opt_FunctionsColour:=0     ;
        Opt_HighlightFunctions:=true  ;
        Opt_KeywordsItalic:=false      ;
        Opt_Keywordsbold:=true        ;
        Opt_KeywordsColour:=0      ;
        Opt_HighlightKeywords:=true   ;

      End;
        3:
        Begin
        Opt_Background:=24          ;
        Opt_Foreground:=7          ;
        Opt_StringsItalic:=false       ;
        Opt_Stringsbold:=false         ;
        Opt_StringsColour:=30       ;
        Opt_HighlightStrings:=true    ;
        Opt_LineNumsItalic:=false      ;
        Opt_LineNumsbold:=false        ;
        Opt_LineNumsColour:=31      ;
        Opt_HighlightLineNums:=true   ;
        Opt_NumbersItalic:=false       ;
        Opt_Numbersbold:=false         ;
        Opt_NumbersColour:=7       ;
        Opt_HighlightNumbers:=true    ;
        Opt_VarsUnDefItalic:=false     ;
        Opt_VarsUnDefbold:=false       ;
        Opt_VarsUnDefColour:=28     ;
        Opt_HighlightVarsUnDef:=true  ;
        Opt_VarsItalic:=false          ;
        Opt_Varsbold:=false            ;
        Opt_VarsColour:=27          ;
        Opt_HighlightVars:=true      ;
        Opt_SymbolsItalic:=false       ;
        Opt_Symbolsbold:=false         ;
        Opt_SymbolsColour:=27       ;
        Opt_HighlightSymbols:=true    ;
        Opt_CommentsItalic:=true      ;
        Opt_Commentsbold:=false        ;
        Opt_CommentsColour:=29      ;
        Opt_HighlightComments:=true   ;
        Opt_FunctionsItalic:=false     ;
        Opt_Functionsbold:=true       ;
        Opt_FunctionsColour:=27     ;
        Opt_HighlightFunctions:=true  ;
        Opt_KeywordsItalic:=false      ;
        Opt_Keywordsbold:=true        ;
        Opt_KeywordsColour:=25      ;
        Opt_HighlightKeywords:=true   ;

        Opt_TFCol31:='#6A625F';
        Opt_TFCol30:='#4FDFAF';
        Opt_TFCol29:='#B39487';
        Opt_TFCol28:='#6A5EA3';
        Opt_TFCol27:='#A45A70';
        Opt_TFCol26:='#BFB290';
        Opt_TFCol25:='#57ABFF';
        Opt_TFCol24:='#08080A';
      End;

        4:
        Begin
        Opt_Background:=26          ;
        Opt_Foreground:=24          ;
        Opt_StringsItalic:=false       ;
        Opt_Stringsbold:=false         ;
        Opt_StringsColour:=30       ;
        Opt_HighlightStrings:=true    ;
        Opt_LineNumsItalic:=false      ;
        Opt_LineNumsbold:=false        ;
        Opt_LineNumsColour:=29      ;
        Opt_HighlightLineNums:=true   ;
        Opt_NumbersItalic:=false       ;
        Opt_Numbersbold:=false         ;
        Opt_NumbersColour:=24       ;
        Opt_HighlightNumbers:=true    ;
        Opt_VarsUnDefItalic:=false     ;
        Opt_VarsUnDefbold:=false       ;
        Opt_VarsUnDefColour:=30    ;
        Opt_HighlightVarsUnDef:=true  ;
        Opt_VarsItalic:=false          ;
        Opt_Varsbold:=false            ;
        Opt_VarsColour:=25          ;
        Opt_HighlightVars:=true      ;
        Opt_SymbolsItalic:=false       ;
        Opt_Symbolsbold:=false         ;
        Opt_SymbolsColour:=31       ;
        Opt_HighlightSymbols:=true    ;
        Opt_CommentsItalic:=true      ;
        Opt_Commentsbold:=false        ;
        Opt_CommentsColour:=7      ;
        Opt_HighlightComments:=true   ;
        Opt_FunctionsItalic:=false     ;
        Opt_Functionsbold:=true       ;
        Opt_FunctionsColour:=2     ;
        Opt_HighlightFunctions:=true  ;
        Opt_KeywordsItalic:=false      ;
        Opt_Keywordsbold:=true        ;
        Opt_KeywordsColour:=27      ;
        Opt_HighlightKeywords:=true   ;

        Opt_TFCol31:='#6A825F';
        Opt_TFCol30:='#2F9F7F';
        Opt_TFCol29:='#B39487';
        Opt_TFCol28:='#AA2E63';
        Opt_TFCol27:='#A45A70';
        Opt_TFCol26:='#EBE9D9';
        Opt_TFCol25:='#276BCF';
        Opt_TFCol24:='#58484A';
      End;
    End;

      TFCol24:=HexToColor(Opt_TFCol24);
  TFCol25:=HexToColor(Opt_TFCol25);
  TFCol26:=HexToColor(Opt_TFCol26);
  TFCol27:=HexToColor(Opt_TFCol27);
  TFCol28:=HexToColor(Opt_TFCol28);
  TFCol29:=HexToColor(Opt_TFCol29);
  TFCol30:=HexToColor(Opt_TFCol30);
  TFCol31:=HexToColor(Opt_TFCol31);

    BuildPalette([TFSpecBlack, TFSpecBlue,  TFSpecRed,  TFSpecMagenta,  TFSpecGreen,  TFSpecCyan,  TFSpecYellow,  TFSpecWhite,
                TFSpecBlack, TFSpecBlueB, TFSpecRedB, TFSpecMagentaB, TFSpecGreenB, TFSpecCyanB, TFSpecYellowB, TFSpecWhiteB,
                TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,TFCol24,
                TFCol24,TFCol25,TFCol26,TFCol27,TFCol28,TFCol29,TFCol30,TFCol31]);
    DrawColours;
    BASinOutput.SetDark;
    DrawColours;
end;

procedure TColoursWindow.CheckBox3Click(Sender: TObject);
begin
        BasinOutput.CursorState:=True;
        BasinOutput.Timer1Timer(Sender);
        Opt_CursorBlinking:=CheckBox3.Checked;
        BasinOutput.Timer1.Enabled:=Opt_CursorBlinking;

end;

end.
