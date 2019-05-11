unit CommandHistory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TCommandWindow = class(TForm)
    ListBox1: TListBox;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure UpdateCommandList;
    Procedure UpdateButton;
    Function  CanInsert(Testing: Boolean): Boolean;
  end;

  Procedure EditLineToHistory;
  Procedure RetrieveHistory(Direction: Integer);

var
  CommandWindow: TCommandWindow;
  CommandHistoryList: TStringlist;
  CommandHistoryPos : Byte;

implementation

{$R *.DFM}

Uses ROMUtils, InputUtils, BASSupport, FastCore, BASinMain, Utility;

Procedure EditLineToHistory;
Var
  EditLine: String;
  AtPos: Integer;
Begin
  EditLine := InsertEscapes(DetokeniseLine(GetEditLine, False));
  AtPos := CommandHistoryList.IndexOf(EditLine);
  If AtPos <> -1 Then CommandHistoryList.Delete(AtPos);
  If CommandHistoryList.Count > 128 Then CommandHistorylist.Delete(0);
  CommandHistoryList.Add(EditLine);
  CommandHistoryPos := CommandHistoryList.Count -1;
  If CommandWindow.Visible Then CommandWindow.UpdateCommandList;
End;

Procedure RetrieveHistory(Direction: Integer);
Begin
  If Editing and (CommandHistorylist.Count > 0) Then Begin
     If Direction = 1 Then Begin
        Inc(CommandHistoryPos);
        If CommandHistoryPos > CommandHistoryList.Count -1 Then
           CommandHistoryPos := 0;
        PutEditLine(FormatEscapes(CommandHistorylist[CommandHistoryPos]), Memory);
     End Else Begin
        PutEditLine(FormatEscapes(CommandHistorylist[CommandHistoryPos]), Memory);
        Inc(CommandHistoryPos, Direction);
        If CommandHistoryPos = 255 Then
           CommandHistoryPos := CommandHistoryList.Count -1;
     End;
  End;
End;

procedure TCommandWindow.FormCreate(Sender: TObject);
begin
  ListBox1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - 24 - Button1.Height);
  Button1.SetBounds(ClientWidth - 8 - Button1.Width, ClientHeight - 8 - Button1.Height, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - 4 - Button2.Width, ClientHeight - 8 - Button2.Height, Button2.Width, Button2.Height);
  Button3.SetBounds(8, ListBox1.Top + ListBox1.Height + 8, Button3.Width, Button1.Height);
end;

Procedure TCommandWindow.UpdateCommandList;
Var
  F: Integer;
Begin
  F := 0;
  ListBox1.Items.BeginUpdate;
  While F < CommandHistoryList.Count Do Begin
     If F < ListBox1.Items.Count Then
        ListBox1.Items[F] := CommandHistoryList[F]
     Else
        ListBox1.Items.Add(CommandHistoryList[F]);
     Inc(F);
  End;
  ListBox1.Items.EndUpdate;
End;

procedure TCommandWindow.Button1Click(Sender: TObject);
begin
  Close;
end;

Function TCommandWindow.CanInsert(Testing: Boolean): Boolean;
Var
  LineStr: String;
  Done, IsDirect: Boolean;
  LineNum, Index, LinePos, LineStart: Integer;
begin
  IsDirect := False;
  // a kludge to catch the empty string
  If BASinOutput.BASICMem = '' Then Begin
     BASinOutput.BASICMem := #13;
     BASinOutput.CursLineStart := 1;
  End;
  If Editing and (ListBox1.ItemIndex >= 0) Then Begin
     // Do not allow Direct Commands to be typed when editing a line.
     // First, find the start of the current line. It will either be the start of the text (position 1) or the last #13.
     Index := BASinOutput.CursLineStart;
     While (Index > 1) and (BASinOutput.BASICMem[Index] <> #13) Do Dec(Index);
     While BASinOutput.BASICMem[Index] = #13 Do Inc(Index);
     LineStart := Index;
     If LineStart > Length(BASinOutput.BASICMem) Then
        LineStart := Length(BASinOutput.BASICMem);
     // We're at the first character of the line.
     // If it's a number, then it *could* be a line, but might be a calculation.
     If BASinOutput.BASICMem[Index] in ['0'..'9'] Then Begin
        LineNum := 0;
        LinePos := Index;
        While (Index < Length(BASinOutput.BASICMem)) and (BASinOutput.BASICMem[Index] in ['0'..'9']) Do Begin
           LineNum := (LineNum * 10)+Ord(BASinOutput.BASICMem[Index])-48;
           Inc(Index);
        End;
        // Now skip any whitespace until we reach a new token.
        While (Index < Length(BASinOutput.BASICMem)) and (BASinOutput.BASICMem[Index] <= ' ') Do
           Inc(Index);
        // So what are we looking at? if it's a letter, then it's a line. Otherwise, it's a direct command.
        If Index < Length(BASinOutput.BASICMem) Then Begin
           If BASinOutput.BASICMem[Index] in ['A'..'Z', 'a'..'z'] Then Begin
              // does the line that we're editing exist elsewhere in the BASin text?
              Index := 1;
              Done := False;
              LineStr := IntToStr(LineNum);
              While Not Done Do begin
                 If BASinOutput.BASICMem[Index] = LineStr[1] Then Begin
                    If Index <> LinePos Then Begin
                       If Copy(BASinOutput.BASICMem, Index, Length(LineStr)) = LineStr Then
                          Done := True; // A Match! This means we're typing a replacement line, not editing an existing one.
                    End;
                 End;
                 Inc(Index);
                 If Index = Length(BASinOutput.BASICMem) +1 Then
                    Done := True;
              End;
              If Index <= Length(BASinOutput.BASICMem) Then
                 IsDirect := True;
           End Else
              IsDirect := True;
        End Else
           IsDirect := True;
     End Else Begin
        // It's a direct command, or a calculation starting with a var.
        IsDirect := True;
     End;
     If Not IsDirect Then Begin
        If Not BASinOutput.BASICChanged Then Begin
           // If we've moved onto a line, but not changed it, then we can move off it for the insertion.
           IsDirect := True;
           If Not Testing Then Begin
              Repeat
                 Inc(BASinOutput.CursOffset);
              Until BASinOutput.BASICMem[BASinOutput.CursOffset] = #13;
              If BASinOutput.BASICMem[BASinOutput.CursOffset +1] <> #13 Then
                 BASinOutput.BASICMem := Copy(BASinOutput.BASICMem, 1, BASinOutput.CursOffset) + #13 + Copy(BASinOutput.BASICMem, BASinOutput.CursOffset +1, 999999);
              Inc(BASinOutput.CursOffset);
           End;
        End;
     End Else Begin
        If Not Testing Then Begin
           // Remove the current direct command, ready to insert the new one.
           While BASinOutput.BASICMem[LineStart] <> #13 Do
              BASinOutput.BASICMem := Copy(BASinOutput.BASICMem, 1, LineStart -1) + Copy(BASinOutput.BASICMem, LineStart +1, 999999);
           BASinOutput.CursOffset := LineStart;
        End;
     End;
  End;
  Result := IsDirect;
End;

procedure TCommandWindow.Button2Click(Sender: TObject);
Begin
  If CanInsert(False) Then Begin
     // Send the characters to the editor!
     BASinOutput.PerformTokenIn(FormatEscapes(CommandHistoryList[ListBox1.ItemIndex]));
     Inc(BASinOutput.CursOffset, Length(FormatEscapes(CommandHistoryList[ListBox1.ItemIndex])));
     BASinOutput.RepaintBASIC(True);
     BASinOutput.BringToFront;
  End;
end;

procedure TCommandWindow.FormShow(Sender: TObject);
begin
  UpdateCommandList;
  UpdateButton;
end;

procedure TCommandWindow.ListBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateButton;
end;

Procedure TCommandWindow.UpdateButton;
Begin
  Button2.Enabled := Editing and (ListBox1.ItemIndex >= 0) and CanInsert(True);
End;

procedure TCommandWindow.ListBox1DblClick(Sender: TObject);
begin
  Button2Click(nil);
end;

procedure TCommandWindow.ListBox1Click(Sender: TObject);
begin
  UpdateButton;
end;

procedure TCommandWindow.Button3Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_command_history.html'), HH_DISPLAY_TOPIC, 0);

end;

Initialization

CommandHistoryList := TStringlist.Create;

Finalization

CommandHistoryList.Free;

end.


