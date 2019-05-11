unit ReplaceWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type

  TReplaceType = (RtBASIC, RtASM);

  TReplaceForm = class(TForm)
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    ComboBox2: TComboBox;
    RadioGroup2: TRadioGroup;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button2: TButton;
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ReplaceType: TReplaceType;
    Procedure DoReplace(Sender: TObject);
  end;

var
  ReplaceForm: TReplaceForm;
  NumReplaced: Integer;

implementation

{$R *.DFM}

Uses Evaluate, ROMUtils, FastCore, InputUtils, BASSupport, MessageBox, FindWindow, BasinMain, ASMEditor, Utility;

procedure TReplaceForm.FormCreate(Sender: TObject);
begin
  Button1.SetBounds(ClientWidth - 4 - Button1.Width, ClientHeight - 4 - Button1.Height, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - 4 - Button2.Width, Button1.Top, Button2.Width, Button2.Height);
  RadioGroup1.ItemIndex := 1;
  RadioGroup2.ItemIndex := 0;
end;

procedure TReplaceForm.ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then Begin
     DoReplace(Self);
     Key := 0;
  End Else Begin
     Button2.Enabled := ComboBox1.Text <> '';
     Button3.Enabled := ComboBox1.Text <> '';
  End;
end;

Procedure TReplaceForm.DoReplace(Sender: TObject);
Var
  FindTerm, ReplaceTerm, TempStr: AnsiString;
  FindIndex, ReplacePos, ReplaceIndex: Integer;
  Eval: TExpression;
  Found: TFindResult;
Label
  ReplaceItBASIC, ReplaceItAsm;
Begin
  ReplaceTerm := ComboBox2.Text;
  ReplaceIndex := ComboBox2.Items.IndexOf(ReplaceTerm);
  If ReplaceIndex >= 0 Then
     Combobox2.Items.Delete(ReplaceIndex);
  ComboBox2.Items.Insert(0, ReplaceTerm);
  FindTerm := ComboBox1.Text;
  FindIndex := ComboBox1.Items.IndexOf(FindTerm);
  If FindIndex >= 0 Then
     Combobox1.Items.Delete(FindIndex);
  ComboBox1.Items.Insert(0, FindTerm);

  If CheckBox2.Checked Then Begin
     // The find term is an expression.
     Eval.Expression := FindTerm;
     Eval.SyntaxChecked := False;
     EvaluateExpr(Eval);
     Case Eval.ResultType of
        0: // AnsiString Result
           Begin
              FindTerm := Eval.ResultStr;
           End;
        1: // Numeric Result
           Begin
              // Alter the findterm to return a AnsiString, and go get it.
              // As no error was generated the first time, and it was a valid
              // numeric result, it won't return numerics or errors this time.
              Eval.Expression := 'STR$('+ReplaceTerm+')';
              Eval.SyntaxChecked := False;
              EvaluateExpr(Eval);
              If Eval.ResultType <> 2 Then
                 FindTerm := Eval.ResultStr
              Else Begin
                 // Just in case :-)
                 Windows.MessageBox(Handle, pChar(Copy(Eval.ResultStr, 3, 999999)), pChar('Search Term error'), MB_OK or MB_ICONWARNING);
                 Exit;
              End;
           End;
        2: // Error
           Begin
              Windows.MessageBox(Handle, pChar(Copy(Eval.ResultStr, 3, 999999)), pChar('Search Term error'), MB_OK or MB_ICONWARNING);
              Exit;
           End;
     End;
  End;

  If CheckBox3.Checked Then Begin
     // The Replace Term is an expression.
     // Very similar to the above routine - yes, I *am* that lazy :)
     Eval.Expression := ReplaceTerm;
     Eval.SyntaxChecked := False;
     EvaluateExpr(Eval);
     Case Eval.ResultType of
        0: Begin
              ReplaceTerm := Eval.ResultStr;
           End;
        1: Begin
              Eval.Expression := 'STR$('+ReplaceTerm+')';
              Eval.SyntaxChecked := False;
              EvaluateExpr(Eval);
              If Eval.ResultType <> 2 Then
                 ReplaceTerm := Eval.ResultStr
              Else Begin
                 Windows.MessageBox(Handle, pChar(Copy(Eval.ResultStr, 3, 999999)), pChar('Replace Term error'), MB_OK or MB_ICONWARNING);
                 Exit;
              End;
           End;
        2: Begin
              Windows.MessageBox(Handle, pChar(Copy(Eval.ResultStr, 3, 999999)), pChar('Replace Term error'), MB_OK or MB_ICONWARNING);
              Exit;
           End;
     End;
  End;

  LastFindText := FindTerm;
  BASinOutput.UpdateMenu;

  Hide;

  If ReplaceType = RtBASIC Then Begin

     // Now that FindTerm and ReplaceTerm actually contain what we want, we use a ROMUtils routine
     // to find it, and replace.

     If (Sender <> nil) And (RadioGroup1.ItemIndex = 0) Then Begin
        ReplacePos := 1;
     End Else If Sender = Button2 Then Begin
        // Put up a message box to wait.
        MessageForm.DoMessage('Busy', 'Searching'#13'Please Wait...');
        Application.ProcessMessages;
        ReplacePos := 1;
     End Else
        ReplacePos := BASinOutput.CursOffset;

     NumReplaced := 0;

     // Last chance - so save an Undo point.

     SaveEmulationState(UndoState);

     // Loop here for a Replace All operation.

     ReplaceItBASIC:

     If (Sender <> nil) and (RadioGroup2.ItemIndex = 0) Then
        Found := BASinOutput.FindNextForward(FindTerm, ReplacePos, CheckBox1.Checked, CheckBox3.Checked)
     Else
        Found := BASinOutput.FindNextBackward(FindTerm, ReplacePos, CheckBox1.Checked, CheckBox3.Checked);

     If Found.Position <> 0 Then Begin
        // Found a match, so replace.

        BASinOutput.FindAndActivateLine(BASinOutput.CursLineNum, BASinOutput.CursStatementNum);
        BASinOutput.CursOffset := Found.Position;
        BASinOutput.UpdateCursorPos(Found.Position, False);
        ReplacePos := BASinOutput.CursOffset +1;

        BASinOutput.BASICMem := Copy(BASinOutput.BASICMem, 1, BASinOutput.CursOffset -1)+ReplaceTerm+Copy(BASinOutput.BASICMem, Integer(BASinOutput.CursOffset) + Length(FindTerm), 999999);
        BASinOutput.LastLineBuffer := BASinOutput.BASICMem;
        BASinOutput.RepaintBASIC(False);
        BASinOutput.MakeCursorVisible;
        Inc(NumReplaced);

        If Sender = Button2 Then Goto ReplaceItBASIC;

     End Else Begin
        // Notify that no more instances were found.
        If Sender <> Button2 Then Begin
           Windows.MessageBox(Handle, pChar(#39+FindTerm+#39+' Not found.'), pChar('Replace Finished.'), MB_OK or MB_ICONINFORMATION);
           Show;
           Exit;
        End Else Begin
           // We're at the end of a ReplaceAll() operation.
           MessageForm.ClearMessage;
           Hide;
           If NumReplaced > 0 Then
              Windows.MessageBox(Handle, pChar('Replaced '+IntToStr(NumReplaced)+' Instances of '+#13#39+FindTerm+#39), pChar('Replace Finished.'), MB_OK or MB_ICONINFORMATION)
           Else
              Windows.MessageBox(Handle, pChar(#39+FindTerm+#39+' Not found.'), pChar('Replace Finished.'), MB_OK or MB_ICONINFORMATION);
        End;
     End;

     BASinOutput.RepaintBASIC(False);
     BASinOutput.MakeCursorVisible;

  End Else Begin

     If (Sender <> nil) And (RadioGroup1.ItemIndex = 0) Then Begin
        AsmReplacePos := Point(1, 1);
     End Else If Sender = Button2 Then Begin
        // Put up a message box to wait.
        MessageForm.DoMessage('Busy', 'Searching'#13'Please Wait...');
        Application.ProcessMessages;
        AsmReplacePos := Point(1, 1);
     End Else
        AsmReplacePos := Point(AsmEditorWindow.CurFile^.CursorColumn, AsmEditorWindow.CurFile^.CursorLine);

     NumReplaced := 0;

     // Loop here for a Replace All operation.

     ReplaceItAsm:

     If (Sender <> nil) and (RadioGroup2.ItemIndex = 0) Then
        Found := AsmEditorWindow.FindNextForward(FindTerm, AsmReplacePos, CheckBox1.Checked)
     Else
        Found := AsmEditorWindow.FindNextBackward(FindTerm, AsmReplacePos, CheckBox1.Checked);

     If Found.Position <> 0 Then Begin
        // Found a match, so replace.

        AsmReplacePos := Point(Found.Position, Found.LineNum);
        AsmEditorWindow.UpdateCursorPos(AsmReplacePos, False);
        AsmReplacePos := Point(AsmEditorWindow.CurFile^.CursorColumn +1, AsmEditorWindow.CurFile^.CursorLine);

        TempStr := AsmEditorWindow.CurFile^.AsmStrs[Found.LineNum -1];
        TempStr := Copy(TempStr, 1, Found.Position -1) + ReplaceTerm + Copy(TempStr, Found.Position + Length(FindTerm), 999999);
        AsmEditorWindow.CurFile^.AsmStrs[Found.LineNum -1] := TempStr;
        AsmEditorWindow.ChangeLine(Found.LineNum -1);

        AsmEditorWindow.CurFile^.CursorLine := Found.LineNum;
        AsmEditorWindow.CurFile^.CursorColumn := Found.Position + Length(ReplaceTerm);
        AsmEditorWindow.CurFile^.EditorSelStart := Point(Found.Position, Found.LineNum);
        AsmEditorWindow.CurFile^.EditorSelEnd := Point(Found.Position, Found.LineNum);

        AsmEditorWindow.RepaintASM(False);
        AsmEditorWindow.MakeCursorVisible;
        AsmEditorWindow.CurFile^.Changed := True;
        AsmEditorWindow.UpdateCaption;
        AsmEditorWindow.BuildLabelList;
        Inc(NumReplaced);

        If Sender = Button2 Then Goto ReplaceItAsm;

     End Else Begin
        // Notify that no more instances were found.
        If Sender <> Button2 Then Begin
           Windows.MessageBox(Handle, pChar(#39+FindTerm+#39+' Not found.'), pChar('Replace Finished.'), MB_OK or MB_ICONINFORMATION);
           Show;
           Exit;
        End Else Begin
           // We're at the end of a ReplaceAll() operation.
           MessageForm.ClearMessage;
           Hide;
           If NumReplaced > 0 Then
              Windows.MessageBox(Handle, pChar('Replaced '+IntToStr(NumReplaced)+' Instances of '+#13#39+FindTerm+#39), pChar('Replace Finished.'), MB_OK or MB_ICONINFORMATION)
           Else
              Windows.MessageBox(Handle, pChar(#39+FindTerm+#39+' Not found.'), pChar('Replace Finished.'), MB_OK or MB_ICONINFORMATION);
        End;
     End;

     AsmEditorWindow.RepaintASM(False);
     AsmEditorWindow.MakeCursorVisible;

  End;

  Close;

End;

procedure TReplaceForm.Button2Click(Sender: TObject);
begin
  If Sender = Button2 Then NumReplaced := 0;
  DoReplace(Sender);
end;

procedure TReplaceForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TReplaceForm.FormShow(Sender: TObject);
begin
  If FindForm.Visible Then Begin
     Top := FindForm.Top;
     Left := FindForm.Left;
     FindForm.Close;
  End;
  If ReplaceType = rtBASIC then
     RadioGroup1.Items[0] := 'Start of BASIC'
  Else
     RadioGroup1.Items[0] := 'Start of Assembly';
  Button2.Enabled := ComboBox1.Text <> '';
  Button3.Enabled := ComboBox1.Text <> '';
end;

procedure TReplaceForm.Button4Click(Sender: TObject);
begin
  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_replace.html'), HH_DISPLAY_TOPIC, 0);
end;

procedure TReplaceForm.ComboBox1Change(Sender: TObject);
begin
  Button2.Enabled := ComboBox1.Text <> '';
  Button3.Enabled := ComboBox1.Text <> '';
end;

end.
