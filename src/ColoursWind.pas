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
    procedure FormShow(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure SetUpList;
    Procedure DrawColours;
    Procedure DrawCheck(Bmp: TFastDIB; X, Y: Integer);
  end;

var
  ColoursWindow: TColoursWindow;

implementation

{$R *.DFM}

Uses BASinMain, FastCore;

procedure TColoursWindow.FormShow(Sender: TObject);
begin
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
End;

Procedure TColoursWindow.DrawColours;
Var
  Idx, Clr: Integer;
  Italic, Bold: Boolean;
Begin
  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 24);
  FastIMG1.Bmp.Clear(TfBtnFace);
  FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width -1, FastIMG1.Bmp.Height -1, TfBlack);
  For Idx := 0 To 7 Do Begin
     FastDrawEx.Rectangle(FastIMG1.Bmp, (Idx * 32)+2, 2, (Idx * 32)+32, 30, TfBlack);
     FastDrawEx.FillRect(FastIMG1.Bmp, (Idx*32)+3, 3, (Idx*32)+31, 30, TFColorAToTFColor(DisplayPalette[Idx]));
     FastDrawEx.Rectangle(FastIMG1.Bmp, (Idx * 32)+2, 32, (Idx * 32)+32, 60, TfBlack);
     FastDrawEx.FillRect(FastIMG1.Bmp, (Idx*32)+3, 34, (Idx*32)+31, 60, TFColorAToTFColor(DisplayPalette[Idx+8]));
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
     End;
     CheckBox1.Checked := Bold;
     CheckBox2.Checked := Italic;
     If CheckListBox1.Checked[CheckListBox1.ItemIndex] Then Begin
        If Clr < 8 Then Begin
           DrawCheck(FastIMG1.Bmp, (Clr * 32) + 13, 13);
        End Else Begin
           DrawCheck(FastIMG1.Bmp, ((Clr -8) * 32) + 13, 43);
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
  ClientWidth := 259 + 16;
  Button1.SetBounds(8, ClientHeight - (Button1.Height +8), Button1.Width, Button1.Height);
  CheckBox2.SetBounds(ClientWidth - (Max(CheckBox2.Width, CheckBox1.Width) + 8), ClientHeight - (CheckBox2.Height + 8), CheckBox2.Width, CheckBox2.Height);
  CheckBox1.SetBounds(CheckBox2.Left, CheckBox2.Top - (CheckBox1.Height + 4), CheckBox1.Width, CheckBox1.Height);
  FastIMG1.SetBounds(8, CheckBox1.Top - 67, 259, 63);
  Label2.SetBounds(8, FastIMG1.Top - (Label2.Height + 4), Label2.Width, Label2.Height);
  Label1.SetBounds(8, 4, Label1.Width, Label1.Height);
  Bevel1.SetBounds(8, Label1.Height + 8, ClientWidth - 16, 2);
  Button2.SetBounds(Button1.Left + Button1.Width + 4, Button1.Top, Button2.Width, Button1.Height);
  CheckListBox1.SetBounds(8, Bevel1.Top + 10, ClientWidth - 16, Label2.Top - (Bevel1.Top + 14));
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
  X := X Div 32;
  If X > 7 then X := 7;
  If Y > FastIMG1.Height Div 2 Then Inc(X, 8);
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
  End;
  DrawColours;
end;

procedure TColoursWindow.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TColoursWindow.Button2Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_editor_fonts.html#syntax'), HH_DISPLAY_TOPIC, 0);

end;

end.
