unit Breakpoints;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, ImgList, ExtCtrls, Evaluate;

type
  TBreakpoint =     Record
     Break:         Boolean; // If true, the breakpoint will stop program execution when it fires.
     Valid:         Boolean; // True = "live" though may be disabled, False = Available for allocation.
     Enabled:       Boolean; // True = Will be tested.
     LogExpression: Boolean; // The log text is an expression, and should be evaluated before logging.
     PassCount:     DWord;   // If it fires, this keeps a counter.
     Line:          Dword;
     Statement:     DWord;   // Line and statement number (1..x) within that line.
     Log:           AnsiString;  // *not* a text AnsiString - a text expression.
     Condition:     AnsiString;  // optional expression. If result > 0 then breakpoint will break execution.
  End;

  PBreakpoint =     ^TBreakpoint;

  TBreakpointsWindow = class(TForm)
    ListView1:      TListView;
    PopupMenu1:     TPopupMenu;
    AddBreakpoint1: TMenuItem;
    N1:             TMenuItem;
    EnableAll1:     TMenuItem;
    DisableAll1:    TMenuItem;
    DeleteAll1:     TMenuItem;
    ENabled1:       TMenuItem;
    Delete1:        TMenuItem;
    ViewSource1:    TMenuItem;
    Properties1:    TMenuItem;
    Button2:        TButton;
    Button3:        TButton;
    Button4:        TButton;
    Button5:        TButton;
    Button1:        TButton;
    procedure       PopupMenu1Popup(Sender: TObject);
    procedure       ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure       Enabled1Click(Sender: TObject);
    procedure       Delete1Click(Sender: TObject);
    procedure       ViewSource1Click(Sender: TObject);
    procedure       Properties1Click(Sender: TObject);
    procedure       AddBreakpoint1Click(Sender: TObject);
    procedure       EnableAll1Click(Sender: TObject);
    procedure       DisableAll1Click(Sender: TObject);
    procedure       DeleteAll1Click(Sender: TObject);
    procedure       FormCreate(Sender: TObject);
    procedure       Button2Click(Sender: TObject);
    procedure       ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure       ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure       Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Function        CreateBreakpoint(Enabled: Boolean; Line, Statement: DWord; Condition: AnsiString): Word;
    Function        ToggleBreakpoint(Line: Word; Statement: Byte): Integer;
    Procedure       UpdateListBreakpoint(BPNumber: Byte);
    Procedure       BuildBreakpointsList;
    procedure       GetBPProperties(Index: Integer);
  end;

var
  BreakpointsWindow: TBreakpointsWindow;
  BreakpointsList:   Array[0..9999] of ShortString;
  BreakArray:        Array[0..255] of TBreakpoint;
  BreakExpressions:  Array[0..255] of TExpression;
  BPs_Updating:      Boolean;

  Function           TestBreakPoint(BP: Byte): Boolean;


implementation

{$R *.DFM}

Uses FastCore, ROMUtils, InputUtils, BreakpointProperties, LogWind, Utility, BasinMain;

Function  TBreakpointsWindow.CreateBreakpoint(Enabled: Boolean; Line, Statement: DWord; Condition: AnsiString): Word;
Var
  F: Word;
Begin
  For F := 1 To 256 Do Begin
     If F < 256 Then
        If Not BreakArray[F].Valid Then Begin
           BreakArray[F].Valid := True;
           BreakArray[F].Break := True;
           BreakArray[F].Enabled := Enabled;
           BreakArray[F].Line := Line;
           BreakArray[F].Statement := Statement;
           BreakArray[F].Condition := Condition;
           BreakArray[F].PassCount := 0;
           BreakExpressions[F].Expression := Condition;
           BreakExpressions[F].SyntaxChecked := False;
           BreakExpressions[F].Tokenised := '';
           Break;
        End;
  End;
  If F = 256 Then F := 65535;
  Result := F;
End;

Function TestBreakPoint(BP: Byte): Boolean;
Var
  LogText: AnsiString;
  Expr: TExpression;
  Breakpoint: PBreakPoint;
Begin
  // Test a breakpoint's condition. PPC and SUBPPC must already be matching - the
  // conditions only test on the breakpoint's specified line.
  Breakpoint := @BreakArray[BP];
  Result := Breakpoint.Enabled;
  If Result Then Begin
     If Breakpoint.condition <> '' Then Begin
        EvaluateExpr(BreakExpressions[BP]);
        Result := (BreakExpressions[BP].ResultType = 1) and (BreakExpressions[BP].ResultNum <> 0);
     End;
     // Inc the "hit count"
     Inc(Breakpoint.PassCount);
     // Now log, if the bp is enabled and any conditions have been met.
     If Result Then
        If Breakpoint.Log <> '' Then Begin
           LogText := IntToStr(BreakPoint.Line)+':'+IntToStr(Breakpoint.Statement)+' ';
           While Length(LogText) < 9 Do LogText := ' '+LogText;
           If Breakpoint.LogExpression Then Begin
              // The log text is an expression to be evaluated now
              Expr.Expression := Breakpoint.Log;
              Expr.SyntaxChecked := False;
              EvaluateExpr(Expr);
              Case Expr.ResultType of
                 0: Begin // AnsiString Result
                       LogText := LogText + Expr.ResultStr;
                    End;
                 1: Begin // Numeric Result
                       LogText := LogText + FloatToStrEx(Expr.ResultNum);
                    End;
                 2: Begin // Error :)
                       LogText := LogText + '['+Copy(Expr.ResultStr, 3, 9999)+']';
                    End;
              End;
              Log(LogText);
           End Else
              Log(BreakPoint.Log);
        End;
     // And finally, does this one actually break, or just pretend to?
     Result := Breakpoint.Break;
     If BreakpointsWindow.Visible Then
        BreakpointsWindow.UpdateListBreakpoint(BP);
  End;
End;

Procedure TBreakpointsWindow.BuildBreakpointsList;
Var
  F, Idx: Integer;
  LI: TListItem;
  Statement: Byte;
  Line: Word;
  BreakLine: AnsiString;
Begin

  // Calls the RunTimeBasic Listing's routines for creating a program
  // List. This is slow, but as this is only called when the breakpoints
  // are added, removed or enabled/disabled, it should be ok.

  BPs_Updating := True;

  ListView1.Items.BeginUpdate;
  ListView1.Items.Clear;

  For F := 0 To 255 Do Begin
     If BreakArray[F].Valid Then Begin
        LI := ListView1.Items.Add;
        // Find the line number
        Statement := BreakArray[F].Statement;
        Line := BreakArray[F].Line;
        // Now Build the listview entry.
        BreakLine := BASinOutput.GetSourceLine(Line, Statement);
        If Breakline <> '' Then Begin
           LI.SubItems.Add(IntToStr(Line)+':'+IntToStr(Statement));
           Idx := 1;
           While BreakLine[Idx] = ' ' Do Inc(Idx);
           LI.SubItems.Add(Copy(BreakLine, Idx, 9999));
           LI.SubItems.Add(BreakArray[F].Condition);
           LI.SubItems.Add(IntToStr(BreakArray[F].PassCount));
           If BreakArray[F].Enabled Then
              LI.Checked := True
           Else
              LI.Checked := False;
        End Else Begin
           ListView1.Items.Delete(ListView1.Items.Count -1);
        End;
     End;
  End;

  ListView1.Items.EndUpdate;
  ListView1.Repaint;

  Button4.Enabled := ListView1.Selected <> Nil;
  Button5.Enabled := Button4.Enabled;

  BPs_Updating := False;

End;

procedure TBreakpointsWindow.PopupMenu1Popup(Sender: TObject);
begin
  // Enable/Disable/Hide relevant items.
  // Itemindex specifies if we clicked a breakpoint or not.
  If ListView1.Selected <> nil Then Begin
     AddBreakpoint1.Visible := False;
     EnableAll1.Visible := False;
     DeleteAll1.Visible := False;
     DisableAll1.Visible := False;
     N1.Visible := False;
     Enabled1.Visible := True;
     Enabled1.Checked := BreakArray[ListView1.Selected.Index+1].Enabled;
     Delete1.Visible := True;
     ViewSource1.Visible := True;
     Properties1.visible := True;
  End Else Begin
     EnableAll1.Enabled := ListView1.Items.Count > 0;
     DeleteAll1.Enabled := ListView1.Items.Count > 0;
     DisableAll1.Enabled := ListView1.Items.Count > 0;
     AddBreakpoint1.Visible := True;
     EnableAll1.Visible := True;
     DeleteAll1.Visible := True;
     DisableAll1.Visible := True;
     N1.Visible := True;
     Enabled1.Visible := False;
     Delete1.Visible := False;
     ViewSource1.Visible := False;
     Properties1.visible := False;
  End;
end;

procedure TBreakpointsWindow.ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  // Use this event to test if the checked property is changed.
  If Not BPs_Updating Then
     If Item.SubItems.Count <> 0 Then
        If Item.Checked <> BreakArray[Item.Index+1].Enabled Then Begin
           BreakArray[Item.Index+1].Enabled := Item.Checked;
           BASinOutput.RepaintBASIC(True);
        End;
end;

procedure TBreakpointsWindow.Enabled1Click(Sender: TObject);
begin
  If ListView1.Selected <> nil Then Begin
     Enabled1.Checked := Not Enabled1.Checked;
     ListView1.Selected.Checked := Enabled1.Checked;
  End;
end;

procedure TBreakpointsWindow.Delete1Click(Sender: TObject);
Var
  BPNum: Word;
begin
  // first, get the breakpoint number
  BPNum := ListView1.Selected.Index+1;
  // Now toggle it to remove it from the list.
  ToggleBreakpoint(BreakArray[BpNum].Line, BreakArray[BpNum].Statement);
  BASinOutput.RepaintBASIC(True);
end;

procedure TBreakpointsWindow.ViewSource1Click(Sender: TObject);
Var
  BPNum: Word;
begin
  // Opens the runtime window, and moves the list to show the breakpoint.
  If ListView1.Selected <> nil Then Begin
     BPNum := ListView1.Selected.Index+1;
     BASinOutput.BringToFront;
     // Now get the breakpoint listview number.
     BASinOutput.FindAndActivateLine(BreakArray[BPNum].Line, BreakArray[BPNum].Statement);
  End;
end;

procedure TBreakpointsWindow.Properties1Click(Sender: TObject);
Var
  MousePos: TPoint;
begin
  Windows.GetCursorPos(MousePos);
  CentreForm(BPProperties, MousePos.X, MousePos.Y);
  If ListView1.Selected <> nil Then
     BPProperties.GetBP(ListView1.Selected.Index+1);
  BPProperties.Caption := 'Breakpoint Properties';
  ShowWindow(BPProperties, True);
  BuildBreakpointslist;
  BASinOutput.RepaintBASIC(True);
end;

procedure TBreakpointsWindow.GetBPProperties(Index: Integer);
Var
  MousePos: TPoint;
begin
  Windows.GetCursorPos(MousePos);
  CentreForm(BPProperties, MousePos.X, MousePos.Y);
  BPProperties.GetBP(Index);
  BPProperties.Caption := 'Breakpoint Properties';
  ShowWindow(BPProperties, True);
  BuildBreakpointslist;
  BASinOutput.RepaintBASIC(True);
end;

Procedure TBreakpointsWindow.UpdateListBreakpoint(BPNumber: Byte);
Var
  F: Integer;
  BreakPoint: PBreakpoint;
Begin
  For F := 1 to ListView1.Items.Count do
     If F = BPNumber Then Begin
        Breakpoint := @BreakArray[BPNumber];
        ListView1.Items.BeginUpdate;
        ListView1.Items[F-1].SubItems[0] := IntToStr(Breakpoint.Line)+':'+IntToStr(Breakpoint.Statement);
        ListView1.Items[F-1].SubItems[2] := BreakPoint.Condition;
        ListView1.Items[F-1].SubItems[3] := IntToStr(Breakpoint.PassCount);
        ListView1.Items.EndUpdate;
        Break;
     End;
End;

Function TBreakpointsWindow.ToggleBreakpoint(Line: Word; Statement: Byte): Integer;   //removed unused paramater - ardafix
Var
  NewBP, F: Word;
Begin
  Result := 65535;
  If BreakpointsList[Line][Statement] = #0 Then Begin // Does not already exist, so add it
     NewBP := CreateBreakpoint(True, Line, Statement, '');
     If NewBP < 65535 Then Begin
        BreakpointsList[Line][Statement] := AnsiChar(AnsiChar(Byte(NewBP)));
        Result := NewBP;
     End;
  End Else Begin // Remove it
     NewBP := Ord(BreakpointsList[Line][Statement]);
     For F := NewBP To 254 Do Begin
        BreakArray[F] := BreakArray[F+1];
        BreakPointsList[BreakArray[F].Line][BreakArray[F].Statement] := AnsiChar(Ord(BreakPointsList[BreakArray[F].Line][BreakArray[F].Statement]) -1);
     End;
     BreakpointsList[Line][Statement] := #0;
  End;
  BuildBreakpointsList;
  BASinOutput.RepaintBASIC(True);
End;

procedure TBreakpointsWindow.AddBreakpoint1Click(Sender: TObject);
Var
  NewBP: Byte;
  MousePos: TPoint;
begin
  Windows.GetCursorPos(MousePos);
  CentreForm(BPProperties, MousePos.X, MousePos.Y);
  If Sender = nil Then Begin
     If BreakPointsList[BASinOutput.CursLineNum][BASinOutput.CursStatementNum] = #0 Then
        NewBP := CreateBreakPoint(True, BASinOutput.CursLineNum, BASinOutput.CursStatementNum, '')
     Else
        NewBP := Ord(BreakpointsList[BASinOutput.CursLineNum][BASinOutput.CursStatementNum]);
  End Else
     NewBP := CreateBreakpoint(True, 0, 1, '');
  BreakpointsList[BASinOutput.CursLineNum][BASinOutput.CursStatementNum] := AnsiChar(Byte(NewBP));
  BPProperties.GetBP(NewBP);
  BPProperties.Caption := 'Add Breakpoint';
  ShowWindow(BPProperties, True);
  If Not BPProperties.Cancelled Then Begin
     BASinOutput.RepaintBASIC(True);
     BuildBreakpointslist;
  End Else Begin
     BreakArray[NewBP].Valid := False;
     BuildBreakpointslist;
  End;
end;

procedure TBreakpointsWindow.EnableAll1Click(Sender: TObject);
Var
  F: Byte;
begin
  For F := 0 to 255 Do
     BreakArray[F].Enabled := True;
  BuildBreakpointslist;
  BASinOutput.RepaintBASIC(True);
end;

procedure TBreakpointsWindow.DisableAll1Click(Sender: TObject);
Var
  F: Byte;
begin
  For F := 0 to 255 Do
     BreakArray[F].Enabled := False;
  BuildBreakpointslist;
  BASinOutput.RepaintBASIC(True);
end;

procedure TBreakpointsWindow.DeleteAll1Click(Sender: TObject);
Var
  F: Word;
begin
  BreakpointsList[0] := '';
  For F := 0 to 255 Do BreakArray[F].Valid := False;
  For F := 1 to 128 Do
     BreakpointsList[0] := BreakpointsList[0] + #0;
  For F := 1 To 9999 Do
     BreakpointsList[F] := BreakpointsList[0];
  BuildBreakpointslist;
  BASinOutput.RepaintBASIC(True);
end;

procedure TBreakpointsWindow.FormCreate(Sender: TObject);
begin
  ListView1.DoubleBuffered := True;
  ListView1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - 22 - Button2.Height);
  Button2.SetBounds(ClientWidth - 8 - Button2.Width, ClientHeight - 8 - BUtton2.Height, Button2.Width, Button2.Height);
  Button3.SetBounds(8, ClientHeight - 8 - Button3.Height, Button3.Width, Button3.Height);
  Button4.SetBounds(Button3.Left + Button3.Width + 4, Button3.Top, Button4.Width, Button4.Height);
  Button5.SetBounds(Button4.Left + Button4.Width + 4, Button3.Top, Button5.Width, Button5.Height);
  Button1.SetBounds(Button2.Left - Button1.Width - 4, Button3.Top, Button1.Width, Button2.Height)

end;

procedure TBreakpointsWindow.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TBreakpointsWindow.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  Button4.Enabled := ListView1.Selected <> Nil;
  Button5.Enabled := Button4.Enabled;
end;

procedure TBreakpointsWindow.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_DELETE Then
     If ListView1.Selected <> nil Then
        Delete1Click(nil);
end;

procedure TBreakpointsWindow.Button1Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_breakpoints.html'), HH_DISPLAY_TOPIC, 0);

end;

end.






