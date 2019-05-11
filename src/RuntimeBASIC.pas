unit RuntimeBASIC;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Menus, ClipBrd;

type
  TRunTimeWindow = class(TForm)
    Button1:  TButton;
    Button2:  TButton;
    Button4:  TButton;
    Button5:  TButton;
    Button6:  TButton;
    MainMenu1: TMainMenu;
    View1: TMenuItem;
    Edit1: TMenuItem;
    ShowBreakpoints1: TMenuItem;
    ShowVariables1: TMenuItem;
    ShowSysVars1: TMenuItem;
    ShowWatches1: TMenuItem;
    ToggleBreakpoint1: TMenuItem;
    SendSelectedtoEdit1: TMenuItem;
    N1: TMenuItem;
    CopySelected1: TMenuItem;
    N2: TMenuItem;
    Close1: TMenuItem;
    Run1: TMenuItem;
    Run2: TMenuItem;
    Goto1: TMenuItem;
    N3: TMenuItem;
    SingleStepStatement1: TMenuItem;
    NextLine1: TMenuItem;
    RunTo1: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToggleBreakpoint2: TMenuItem;
    SendSelectedtoEdit2: TMenuItem;
    Goto2: TMenuItem;
    RunTo2: TMenuItem;
    ListBox1: TCheckListBox;
    procedure FormShow(Sender: TObject);
    Procedure ClearDebugPoints;
    procedure BuildBASICWindow(LineNumber: DWord);
    procedure FindAndActivateLine(LineNumber: DWord; Statement: Byte);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpdateRuntimeButtons;
    procedure ListBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    Procedure RunProgram(Line: Word);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ListBox1DblClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { Private declarations }
    procedure OnEnterMenuLoop(var Message: TMessage); message WM_ENTERMENULOOP;
  public
    { Public declarations }
     AbortStatement,
     SingleStep:    Boolean;
     NextLine:      Boolean;
     Running:       Boolean;
     RunningAck:    Boolean;
     LineSel,
     NXTLIN,
     GOTOStatement,
     GOTOLine,
     RunStatement,
     RunLine:       DWord;
  end;

Procedure SetGOTOPoint(Line, Statement: DWord);

var
  RunTimeWindow: TRunTimeWindow;

implementation

{$R *.DFM}

Uses BASinMain, FastCore, ROMUtils, BASSupport, InputUtils, Breakpoints, SysVars, Utility;

procedure TRunTimeWindow.FormCreate(Sender: TObject);
begin
  RunningAck := Not Running;
  RunLine := 65536;
  GOTOStatement := 1;

  ListBox1.SetBounds(4, 4, ClientWidth -8, ClientHeight - Button1.Height - 12);
  Button1.SetBounds(Button1.Left, ClientHeight - Button1.Height - 4, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left + Button1.Width + 4, Button1.Top, Button2.Width, Button2.Height);
  Button5.SetBounds(Button2.Left + Button2.Width + 4, Button1.Top, Button5.Width, Button5.Height);
  Button6.SetBounds(Button5.Left + Button5.Width + 4, Button1.Top, Button6.Width, Button6.Height);
  Button4.SetBounds(ClientWidth - Button4.Width - 4, Button1.Top, Button4.Width, Button4.Height);

  ListBox1.DoubleBuffered := True;

end;

procedure TRunTimeWindow.FormShow(Sender: TObject);
begin
  BuildBASICWindow(0);
  UpdateRUNTimeButtons;
end;

Procedure TRunTimeWindow.ClearDebugPoints;
Var
  F: Integer;
Begin
  AbortStatement := False;
  GOTOStatement := 1;
  Runline := 65536;
  RunStatement := 0;
  NextLine := False;
  SingleStep := False;
  BreakLines := '';
  BreakpointsList[0] := '';
  For F := 0 to 255 Do BreakArray[F].Valid := False;
  For F := 1 to 128 Do
     BreakpointsList[0] := BreakpointsList[0] + #0;
  For F := 1 To 9999 Do
     BreakpointsList[F] := BreakpointsList[0];
  UpdateRunTimeButtons;
End;

Procedure TRunTimeWindow.BuildBASICWindow(LineNumber: DWord);
Var
  FirstLine, InString: Boolean;
  CurLineNum, CurStatement, TempWord, LinePos, CurAddress: Word;
  CurLine, BASICStr, TempStr: String;
Begin

  ListBox1.Items.BeginUpdate;
  ListBox1.Items.Clear;
  CurAddress := GetWord(@Memory[PROG]);

  If (CurAddress = 0) or (CurAddress = GetWord(@Memory[VARS])) Then Begin
     ListBox1.Enabled := False;
     ListBox1.Items.EndUpdate;
     Exit;
  End;

  BreakLines := '';

  Repeat

     // Line Numbers are stored big-endian
     TempWord := GetWord(@Memory[CurAddress]);
     CurLineNum := (TempWord Shr 8) + ((TempWord and 255) Shl 8);
     TempStr := IntToStr(CurLineNum);
     While Length(TempStr) < 4 Do TempStr := ' '+TempStr;
     CurLine := TempStr +' ';

     // Now Address the Length
     Inc(CurAddress, 2);
     // Get the line length (includes terminal #13)
     TempWord := GetWord(@Memory[CurAddress]);
     Inc(CurAddress, 2);
     TempStr := '';
     TempStr := GetMemoryString(CurAddress, TempWord-1);

     // and now address the next line for later.
     Inc(CurAddress, TempWord);

     // Now Detokenise the line we just got.
     BASICStr := '';
     BASICStr := DetokeniseLine(TempStr);

     // Process for #14 5 byte number codes, and then Escape characters.
     BASICStr := CurLine+InsertEscapes(BASICStr);

     // Now we have a line, let's prettify it a bit.
     // By breaking up multistatement lines.

     FirstLine := True;
     InString := False;
     LinePos := 1;
     CurStatement := 1;
     While LinePos < Length(BASICStr)+1 Do Begin
        If BASICStr[LinePos] = '"' Then InString := Not InString;
        If Not InString Then Begin
           If (BASICStr[LinePos] = ':') or (Copy(BASICStr, LinePos, 4) = 'THEN') Then Begin
              // Not a string, and a colon found. Split.
              // If it's the first line, it doesn't need aligning past
              // the line number.
              If BASICStr[LinePos] <> ':' Then Inc(LinePos, 3);
              If Not Firstline Then Begin
                 ListBox1.Items.Add('    ' + Copy(BASICStr, 1, LinePos));
                 ListBox1.ItemEnabled[ListBox1.Items.Count-1] := False;
                 BASICStr := Copy(BASICStr, LinePos+1, 9999);
                 LinePos := 1;
                 If BreakpointsList[CurLineNum][CurStatement] <> #0 then
                    BreakLines := BreakLines + BreakpointsList[CurLineNum][CurStatement]
                 Else
                    BreakLines := BreakLines + #0;
                 Inc(CurStatement);
              End Else Begin
                 ListBox1.Items.Add(Copy(BASICStr, 1, LinePos));
                 ListBox1.ItemEnabled[ListBox1.Items.Count-1] := False;
                 BASICStr := Copy(BASICStr, LinePos+1, 9999);
                 FirstLine := False;
                 LinePos := 1;
                 If BreakpointsList[CurLineNum][CurStatement] <> #0 then
                    BreakLines := BreakLines + BreakpointsList[CurLineNum][CurStatement]
                 Else
                    BreakLines := BreakLines + #0;
                 Inc(CurStatement);
              End;
           End;
        End;
        Inc(LinePos);
     End;
     If Not Firstline Then
        ListBox1.Items.Add('    ' + Copy(BASICStr, 1, LinePos))
     Else
        ListBox1.Items.Add(Copy(BASICStr, 1, LinePos));
     ListBox1.ItemEnabled[ListBox1.Items.Count-1] := False;
     If BreakpointsList[CurLineNum][CurStatement] <> #0 then
        BreakLines := BreakLines + BreakpointsList[CurLineNum][CurStatement]
     Else
        BreakLines := BreakLines + #0;

  Until CurAddress >= GetWord(@Memory[VARS]);

  // Having grabbed the program listing, now find the specified line number

  FindAndActivateLine(LineNumber, 0);

  ListBox1.Enabled := (ListBox1.Items.Count > 0);

  If Not BPs_Updating Then BreakpointsWindow.BuildBreakpointsList;
  ListBox1.Items.EndUpdate;
  UpdateRunTimeButtons;

End;

Procedure TRunTimeWindow.FindAndActivateLine(LineNumber: DWord; Statement: Byte);
Var
  TempStr: String;
Begin

  // Find the specified line number.
  // if this is zero, then get the current line number pointed to by the program
  // cursor. If *this* is zero, then list from the first line.

  If ListBox1.Items.Count > 0 Then Begin

     If LineNumber = 0 Then Begin

        If Not Running Then
           LineNumber := GetWord(@Memory[E_PPC])
        Else Begin
           LineNumber := GetWord(@Memory[PPC]);
           If Statement = 0 Then
              Statement := Memory[SUBPPC];
        End;

        If LineNumber = 0 Then Begin
           LineNumber := StrToInt(Copy(ListBox1.Items[0], 1, 4));
           Statement := 0;
        End;

     End Else If LineNumber = 65535 Then Begin

           LineNumber := GetWord(@Memory[PPC]);
           If Statement = 0 Then
              Statement := Memory[SUBPPC];
     End;

     TempStr := IntToStr(LineNumber);
     TempStr := Copy('    ', 1, 4-(Length(TempStr)))+TempStr;

     // Fortunately, all lines start with a 4 character number, padded with
     // spaces, so it should be relatively easy to find.

     LineNumber := 0;
     While LineNumber < DWord(ListBox1.Items.Count) Do Begin
        If Copy(ListBox1.Items[LineNumber], 1, 4) = TempStr Then Break;
        Inc(LineNumber);
     End;

     // However, if we failed to do so, make sure that we jump to line
     // zero, like the spectrum does. the LIST behaviour of choosing the
     // nearest line can be done away with - LIST will do that for us :-)

     If LineNumber = ListBox1.Items.Count Then Begin
        LineNumber := 0;
        Statement := 0;
     End;

     // Now find the statement

     If Statement > 0 Then Begin
        Dec(Statement); // Statements start at 1.
        While Statement > 0 Do Begin
           Dec(Statement);
           Inc(LineNumber);
           If ListBox1.Items.Count <= LineNumber Then Begin
              LineNumber := ListBox1.Items.Count -1;
              Break;
           End;
           If Copy(ListBox1.Items[LineNumber], 1, 4) <> '    ' Then Begin
              Dec(LineNumber);
              Break;
           End;
        End;
     End;

     If ListBox1.Tag <> LineNumber Then Begin

        If ListBox1.Tag < ListBox1.Items.Count Then
           If ListBox1.Tag >= 0 Then ListBox1.Checked[ListBox1.Tag] := False;
        ListBox1.Checked[LineNumber] := True;
        ListBox1.Tag := LineNumber;

     End;

     If (ListBox1.ItemRect(LineNumber).Top < 0) or
        (ListBox1.ItemRect(LineNumber).Top > ListBox1.Height-20) Then
        ListBox1.TopIndex := LineNumber;

  End;

End;

procedure TRunTimeWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  If Not EmuRunning Then EmuRunning := True;
  UpdateRunTimeButtons;
end;

Procedure TRunTimeWindow.UpdateRuntimeButtons;
Begin
  Case Running of
     True:
        Begin
           If EmuRunning Then Begin
              Button1.Caption := 'Pause';
              Button1.Enabled := True;
              Button2.Enabled := False;
              Button4.Enabled := False;
              Button5.Enabled := False;
              Button6.Enabled := False;
           End Else Begin
              Button1.Caption := 'Continue';
              Button1.Enabled := GetWord(@Memory[PROG]) <> GetWord(@Memory[VARS]);
              Button2.Enabled := Button1.Enabled;
              Button4.Enabled := (ListBox1.ItemIndex >= 0) and Button1.Enabled;
              Button5.Enabled := Button1.Enabled;
              Button6.Enabled := Button4.Enabled;
           End;
        End;
     False:
        Begin
           If EmuRunning Then
              Button1.Caption := 'Run'
           Else
              Button1.Caption := 'Continue';
           Button1.Enabled := GetWord(@Memory[PROG]) <> GetWord(@Memory[VARS]);
           Button2.Enabled := Button1.Enabled;
           Button4.Enabled := (ListBox1.ItemIndex >= 0) and Button1.Enabled;
           Button5.Enabled := Button1.Enabled;
           Button6.Enabled := Button4.Enabled;
        End;
  End;
  RunningAck := Running;

  // Update the RUN menu items too
  Run2.Enabled := Button1.Enabled;
  GoTo1.Enabled := Button4.Enabled;
  SingleStepStatement1.Enabled := Button2.Enabled;
  NextLine1.Enabled := Button5.Enabled;
  RunTo1.Enabled := Button4.Enabled;
  RunTo2.Enabled := Button4.Enabled;
  Goto2.Enabled := Button4.Enabled;

  // Some Menu Items in the main window are closely related to these.
  With BASinOutPut Do Begin
     Run2.Enabled := Button1.Enabled;
     Run2.Caption := Button1.Caption;
     Goto1.Enabled := Button1.Enabled;
     SingleStepStatement1.Enabled := Button2.Enabled;
     StepToNext1.Enabled := Button5.Enabled;
     RunTo1.Enabled := Button1.Enabled;
  End;

  PostMessage(BASinOutput.Handle, WM_UPDATECURSOR, 0, 0);

End;

procedure TRunTimeWindow.ListBox1Click(Sender: TObject);
begin
  UpdateRunTimeButtons;
end;

procedure TRunTimeWindow.Button2Click(Sender: TObject);
begin
  // Single Step.
  // Firstly, the boolean var SingleStep is set true - stop the emulation
  // After the current statement has executed. Then the program is started...
  If Not Running Then Begin
     If ListBox1.Items.Count = 0 Then BuildBASICWindow(0);
     RunStatement := 1;
     RunLine := StrToIntDef(Copy(ListBox1.Items[0], 1, 4), 0);
  End Else Begin
     SingleStep := True;
  End;
  RunProgram(65535);
end;

Procedure TRunTimeWindow.RunProgram(Line: Word);
Begin
  // A Real headache.
  // First, if the emulation has been paused, it's a safe bet that we're already
  // in runtime. Otherwise, start the program, by calling the RUN ROM routine.
  If Not EmuRunning Then Begin
     If Running Then Begin
        EmuRunning := True;
        UpdateRunTimeButtons;
     End Else Begin
        If Line = 65535 Then Begin
           PutEditLine('RUN');
           BufferToken(13);
        End Else Begin
           PutEditLine('GO TO'+IntToStr(Line));
           BufferToken(13);
        End;
     End;
  End Else Begin
     // Emulation is running - so we need to execute a ROM routine.
     // GOTO would be best, as RUN clears variables.
     If Line = 65535 Then Begin
        PutEditLine('RUN');
        BufferToken(13);
     End Else Begin
        PutEditLine('GO TO'+IntToStr(Line));
        BufferToken(13);
     End;
  End;
  ListBox1.Repaint;
End;

procedure TRunTimeWindow.Button1Click(Sender: TObject);
begin
  If Not EmuRunning or Not Running Then
     RunProgram(65535)
  Else Begin
     // To stop, simply simulate a single step operation.
     // then we *know* we'll stop between statements.
     SingleStep := True;
  End;
end;

procedure TRunTimeWindow.Button5Click(Sender: TObject);
Var
  TempLine: DWord;
begin
  // Step Over. Basically, Set a RUN-TO point as the next statement/line, and
  // start the RUN TO process.
  If LineSel < ListBox1.Items.Count -1 Then Begin
     If Copy(listBox1.Items[LineSel+1], 1, 4) <> '    ' Then Begin
        // Next Statement is a line.
        RunStatement := 1;
        RunLine := StrToInt(Copy(ListBox1.Items[LineSel+1], 1, 4));
     End Else Begin
        // It's a new statement, so we have to find it.
        TempLine := LineSel +1;
        RunStatement := 1;
        While Copy(ListBox1.Items[Templine], 1, 4) = '    ' Do Begin
           Dec(Templine);
           Inc(RunStatement);
        End;
        RunLine := StrToInt(Copy(ListBox1.Items[TempLine], 1, 4));
     End;
     RunProgram(65535);
  End;
end;

procedure TRunTimeWindow.Button6Click(Sender: TObject);
Var
  TempLine: DWord;
begin
  // Continue Execution until the selected statement or line
  // is about to be executed.
  RunStatement := 1;
  TempLine := ListBox1.ItemIndex;
  While Copy(ListBox1.Items[Templine], 1, 4) = '    ' Do Begin
     Dec(Templine);
     Inc(RunStatement);
  End;
  RunLine := StrToInt(Copy(ListBox1.Items[Templine], 1, 4));
  RunProgram(65535);
end;

procedure TRunTimeWindow.Button4Click(Sender: TObject);
Var
  TempLine: Integer;
  Statement: Byte;
  Line: Word;
begin
  // GO TO - this begins (or resumes) emulation/execution
  // at the line/Statement highlighted.

  Statement := 1;
  TempLine := ListBox1.ItemIndex;
  While Copy(ListBox1.Items[Templine], 1, 4) = '    ' Do Begin
     Dec(Templine);
     Inc(Statement);
  End;
  Line := StrToInt(Copy(ListBox1.Items[Templine], 1, 4));

  SetGOTOPoint(Line, Statement);

End;

Procedure SetGOTOPoint(Line, Statement: DWord);
Var
  TempVal, TempLine, NextLine: DWord;
Begin

  If RunTimeWindow.Running Then Begin

     // At this point, the Emulation is paused - the only way that
     // can happen is during program execution, so we simply poke.
     // Find the line, and set the runtime loop to point to it.

     TempLine := GetLineAddress(Line, Statement+1, GetWord(@Memory[PROG]));

     If TempLine > 0 Then Begin

        // At this point, TempLine holds the address of the line
        // to jump to.
        // First, before we modify to point at the first valid keyword,
        // we need to update the sysvar NXTLIN to point to the line after.

        TempVal := TempLine;
        Repeat
           Inc(TempVal);
        Until (Memory[TempVal] = 13) or (TempVal = GetWord(@Memory[VARS]));
        PutWord(@Memory[NXTLIN], TempVal+1);

        NextLine := (Memory[TempVal] Shl 8)+Memory[TempVal+1];

        // Now perform the aforementioned modification on TempLine.
        // If the Statement = 1 then we need to skip the line number
        // and length (4 bytes)

        If Statement = 1 Then Inc(Templine, 4);

        // Now find the first valid keyword, and update A with it,
        // as the preceding RST 20h would have done.

        While Memory[TempLine] < $CE Do Inc(TempLine);
        Registers.A := Memory[TempLine];

        // now update the system variables with the new
        // line number and statement offset.

        PutWord(@Memory[PPC], Line);
        Memory[SUBPPC] := Statement;
        PutWord(@MEMORY[CH_ADD], TempLine);

        // Finally, set the program position to the new line.

        RunTimeWindow.FindAndActivateLine(65535, 0);

        // and restart execution, if the emulation is *not* paused.
        // this is so that we can single step from any point we choose.

        CurKeyDown := 0;
        KeyBuffer := '';
        If EmuRunning Then RunTimeWindow.RunProgram(Line) Else EmuRunning := True;
        Exit;

     End;

  End Else Begin

     // We're sitting at the edit prompt,so we need to utilise a ROM trap.
     // Setting GOTOStatement to <> 1 will trap and jump to that statement later on.

     RunTimeWindow.GOTOLine := Line;
     PutEditLine('GO TO '+IntToStr(Line));
     BufferToken(13);
     RunTimeWindow.GOTOStatement := Statement;

  End;

end;

procedure TRunTimeWindow.ListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
Var
  CurBrColor: TColor;
begin
	With (Control as TCheckListBox).Canvas Do Begin
     CurBrColor := Brush.Color;
     Brush.Color := ClWindow;
     FillRect(Classes.Rect(0, Rect.Top, Rect.Left, Rect.Bottom));
     Brush.Color := CurBrColor;
     Font.Color := ClBlack;
     If BreakLines[Index+1] <> #0 Then Begin
        If BreakArray[Byte(Breaklines[Index+1])].Enabled Then
           Brush.color := ClRed
        Else
           Brush.Color := ClMaroon;
     End;
     FillRect(Classes.Rect(0, Rect.Top, Rect.Right, Rect.Bottom));
     TextOut(Rect.Left, Rect.Top, (Control as TCheckListBox).Items[Index]);
     Brush.Color := ClLime;
        If (Control as  TCheckListBox).Checked[Index] Then Begin
           Ellipse(4, Rect.Top+4, Rect.Left-2, Rect.Top+12);
           LineSel := Index;
        End Else
           Ellipse(-4, Rect.Top+4, -2, Rect.Top+12);
	end;
  Font.Color := ClWindow;
end;

procedure TRunTimeWindow.ListBox1DblClick(Sender: TObject);
Var
  MousePoint: TPoint;
  TempLine, Statement, Line: DWord;
begin
  // ItemIndex is the item we double clicked.
  // But test with PtInRect() just in case.
  If ListBox1.ItemIndex <> -1 Then Begin
     Windows.GetCursorPos(MousePoint);
     If (PtInRect(ListBox1.ItemRect(ListBox1.ItemIndex), ListBox1.ScreenToClient(MousePoint))) or
        (Sender = Nil) Then Begin
        // Get the line and Statement number
        Statement := 1;
        TempLine := ListBox1.ItemIndex;
        While Copy(ListBox1.Items[Templine], 1, 4) = '    ' Do Begin
           Dec(Templine);
           Inc(Statement);
        End;
        Line := StrToInt(Copy(ListBox1.Items[Templine], 1, 4));
        // Now set or unset the breakpoint.
        BreakpointsWindow.ToggleBreakpoint(Line, ListBox1.ItemIndex+1, Statement);
        ListBox1.Repaint;
     End;
  End;
end;

procedure TRunTimeWindow.OnEnterMenuLoop(var Message: TMessage);
Var
  IsList, Selected: Boolean;
Begin
  IsList := ListBox1.Items.Count > 0;
  Selected := IsLIst and (ListBox1.ItemIndex > -1);
  ToggleBreakpoint1.Enabled := Selected;
  ToggleBreakpoint2.Enabled := Selected;
  RunTo2.Enabled := Selected;
  Goto2.Enabled := Selected;
  CopySelected1.Enabled := Selected;
  SendSelectedToEdit1.Enabled := Selected and not Running;
  SendSelectedToEdit2.Enabled := Selected and not Running;
End;

procedure TRunTimeWindow.MenuItemClick(Sender: TObject);
Var
  Line: Word;
  Statement: Byte;
  TempStr: String;
begin
  Case (Sender as TComponent).Tag of
     1:
        Begin // Show Breakpoints
           BASinOutput.MenuItemClick(BASinOutput.Breakpoints1);
        End;
     2:
        Begin // Show Variables
           BASinOutput.MenuItemClick(BASinOutput.Variables1);
        End;
     3:
        Begin // Show System Variables
           BASinOutput.MenuItemClick(BASinOutput.SystemVariables1);
        End;
     4:
        Begin // Show Watches
           BASinOutput.MenuItemClick(BASinOutput.Watches1);
        End;
     5:
        Begin // Close
           Close;
        End;
     6:
        Begin // Toggle Breakpoint
           ListBox1DblClick(Nil);
        End;
     8:
        Begin // Copy selected
           If ListBox1.ItemIndex <> -1 Then
              TempStr := ListBox1.Items[ListBox1.ItemIndex];
              While Copy(TempStr, 1, 1) = ' ' Do
                 TempStr := Copy(TempStr, 2, 9999);
              ClipBoard.SetTextBuf(PChar(TempStr));
        End;
     9:
        Begin // Send Selected to Edit
           Statement := 1;
           Line := ListBox1.ItemIndex;
           While Copy(ListBox1.Items[Line], 1, 4) = '    ' Do Begin
              Dec(Line);
              Inc(Statement);
           End;
           SendLineToEditor(StrToInt(Copy(ListBox1.Items[Line], 1, 4)), Statement);
        End;
  End;
end;

procedure TRunTimeWindow.PopupMenu1Popup(Sender: TObject);
Var
  Msg: TMessage;
begin
  OnEnterMenuLoop(Msg);
end;

end.


