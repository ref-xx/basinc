unit FindWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type

  TFindType = (ftBASIC, ftASM);

  TFindForm = class(TForm)
    Button2: TButton;
    Button1: TButton;
    Label1: TLabel;
    ComboBox1: TComboBox;
    RadioGroup1: TRadioGroup;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RadioGroup2: TRadioGroup;
    Button3: TButton;
    CheckBox3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FindType: TFindType;
    Procedure DoFind(Sender: TObject);
  end;

var
  FindForm: TFindForm;
  LastFindTerm: AnsiString;

implementation

{$R *.DFM}

Uses Utility, Evaluate, ROMUtils, FastCore, InputUtils, ReplaceWindow, BasinMain, AsmEditor;

procedure TFindForm.FormCreate(Sender: TObject);
begin
  Button1.SetBounds(ClientWidth - 8 - Button1.Width, ClientHeight - 8 - Button1.Height, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - 4 - Button2.Width, Button1.Top, Button2.Width, Button2.Height);
  Button3.SetBounds(8, Button1.Top, Button3.Width, Button1.Height);
  RadioGroup2.ItemIndex := 1;
end;

procedure TFindForm.ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then Begin
     Key := 0;
     DoFind(Self);
  End Else
     Button2.Enabled := ComboBox1.Text <> '';
end;

Procedure TFindForm.DoFind(Sender: TObject);
Var
  FindTerm: AnsiString;
  Index, FindPos: Integer;
  Eval: TExpression;
  Found: TFindResult;
Begin
  FindTerm := ComboBox1.Text;
  Index := ComboBox1.Items.IndexOf(FindTerm);
  If Index >= 0 Then
     Combobox1.Items.Delete(Index);
  ComboBox1.Items.Insert(0, FindTerm);
  If CheckBox2.Checked Then Begin
     // The find term is an expression.
     Eval.Expression := FindTerm;
     Eval.SyntaxChecked := False;
     EvaluateExpr(Eval);
     Case Eval.ResultType of
        0: // String Result
           Begin
              FindTerm := Eval.ResultStr;
           End;
        1: // Numeric Result
           Begin
              // Alter the findterm to return a AnsiString, and go get it.
              // As no error was generated the first time, and it was a valid
              // numeric result, it won't return numerics or errors this time.
              Eval.Expression := 'STR$('+FindTerm+')';
              Eval.SyntaxChecked := False;
              EvaluateExpr(Eval);
              If Eval.ResultType <> 2 Then
                 FindTerm := Eval.ResultStr
              Else Begin
                 // Just in case :-)
                 MessageBox(Handle, pChar(Copy(Eval.ResultStr, 3, 9999)), pChar('Find error'), MB_OK or MB_ICONWARNING);
                 Exit;
              End;
           End;
        2: // Error
           Begin
              MessageBox(Handle, pChar(Copy(Eval.ResultStr, 3, 9999)), pChar('Find error'), MB_OK or MB_ICONWARNING);
              Exit;
           End;
     End;
  End;

  LastFindText := FindTerm;

  // Now that FindTerm actually contains what we want, we go find it.

  If FindType = ftBASIC Then Begin

     BASinOutput.UpdateMenu;

     If (Sender <> nil) And (RadioGroup2.ItemIndex = 0) Then Begin
        FindPos := 1;
     End Else
        FindPos := BASinOutput.CursOffset +1;

     If RadioGroup1.ItemIndex = 0 Then
        Found := BASinOutput.FindNextForward(FindTerm, FindPos, CheckBox1.Checked, CheckBox3.Checked)
     Else
        Found := BASinOutput.FindNextBackward(FindTerm, FindPos, CheckBox1.Checked, CheckBox3.Checked);

     If Found.Position <> 0 Then Begin
        // Found something, so bring it to the editor.

        BASinOutput.FindAndActivateLine(BASinOutput.CursLineNum, BASinOutput.CursStatementNum);
        BASinOutput.UpdateCursorPos(Found.Position, False);

        BASinOutput.RepaintBASIC(False);


        BASinOutput.AdjustCursorPoint;
        BASinOutput.MakeCursorVisible;

     End Else Begin
        // Notify that no more instances were found.
        MessageBox(Handle, pChar(#39+FindTerm+#39+' Not found.'), pChar('Find Finished.'), MB_OK or MB_ICONINFORMATION);
        Exit;
     End;

  End Else Begin

     If (Sender <> nil) And (RadioGroup2.ItemIndex = 0) Then Begin
        AsmFindPos := Point(1, 1);
     End Else
        AsmFindPos := Point(AsmEditorWindow.CurFile^.CursorColumn +1, AsmEditorWindow.CurFile^.CursorLine);

     If RadioGroup1.ItemIndex = 0 Then
        Found := AsmEditorWindow.FindNextForward(FindTerm, AsmFindPos, CheckBox1.Checked)
     Else
        Found := AsmEditorWindow.FindNextBackward(FindTerm, AsmFindPos, CheckBox1.Checked);

     If Found.LineNum <> 0 Then Begin
        // Found something, so bring it to the editor.
        AsmFindPos := Point(Found.Position, Found.LineNum);
        AsmEditorWindow.CursorToLine(Found.LineNum, Found.Position, '');
        AsmEditorWindow.MakeCursorVisible;
        AsmEditorWindow.UpdateCursorPos(AsmFindPos, False);
        AsmEditorWindow.RepaintASM(True);
     End Else Begin
        // Notify that no more instances were found.
        MessageBox(Handle, pChar(#39+FindTerm+#39+' Not found.'), pChar('Find Finished.'), MB_OK or MB_ICONINFORMATION);
        Exit;
     End;

  End;

  Close;

End;

procedure TFindForm.Button2Click(Sender: TObject);
begin
  DoFind(Self);
end;

procedure TFindForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFindForm.FormShow(Sender: TObject);
begin
  If FindType = ftASM Then
     RadioGroup2.Items[0] := 'Start of Assembly'
  Else
     RadioGroup2.Items[0] := 'Start of BASIC';

  If ReplaceForm.Visible Then Begin
     Top := ReplaceForm.Top;
     Left := ReplaceForm.Left;
     ReplaceForm.Close;
  End;

  Button2.Enabled := ComboBox1.Text <> '';

end;

procedure TFindForm.Button3Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_find.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TFindForm.ComboBox1Change(Sender: TObject);
begin

  Button2.Enabled := ComboBox1.Text <> '';

end;

end.
