unit AddCode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
    TAddCodeWindow = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OkayPressed: Boolean;
    Procedure ClearCode;
    Procedure AddCode(Code: TStringlist);
    Function  PasteLines: Boolean;
  end;

var
  AddCodeWindow: TAddCodeWindow;

implementation

uses BasinMain, Sound, Utility, LogWind, FastCore;

{$R *.DFM}

procedure TAddCodeWindow.FormCreate(Sender: TObject);
begin
  Button2.SetBounds(ClientWidth - Button2.Width - 8, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Left - Button1.Width - 8, ClientHeight - Button1.Height - 8, Button1.Width, Button1.Height);
  Button3.SetBounds(8, Button1.Top, Button3.Width, Button3.Height);
  Memo1.SetBounds(8, Memo1.Top, ClientWidth - 16, ClientHeight - Button1.Height - Memo1.Top - 16);
end;

Procedure TAddCodeWindow.ClearCode;
Begin
  Memo1.Lines.Clear;
End;

Procedure TAddCodeWindow.AddCode(Code: TStringlist);
Var
  CurLine, Line: String;
  Idx, Idx2: Integer;
Begin

  Idx := 0;
  CurLine := '';

  While Idx < Code.Count Do Begin

     Line := Code[Idx];

     // Remove leading whitespace.
     While (Line <> '') and (Line[1] <= ' ') Do
        Line := Copy(Line, 2, 999999);

     If Line = '' Then Begin
        Inc(Idx);
        Continue; // 1.82fix for empty line pase
     End;

     If Line[1] in ['0'..'9'] Then Begin
        // a new line.
        If CurLine <> '' Then
           Memo1.Lines.Add(CurLine);
        CurLine := Line;
     End Else Begin

        If CurLine <> '' Then Begin
           Idx2 := Length(CurLine);
           While (Idx2 > 1) and (CurLine[Idx2] <= ' ') Do
              Dec(Idx2);
           If (CurLine[Idx2] in ['\', ':']) or (Lowercase((Copy(CurLine, Idx2-3, 4))) = 'then') Then
              CurLine := CurLine + Line
           Else Begin
              Memo1.Lines.Add(CurLine);
              CurLine := Line;
           End;
        End Else
           CurLine := Line;

     End;

     Inc(Idx);

  End;

  If CurLine <> '' Then
     Memo1.Lines.Add(CurLine);

End;

procedure TAddCodeWindow.Button2Click(Sender: TObject);
begin
  OkayPressed := False;
  Close;
end;

procedure TAddCodeWindow.Button1Click(Sender: TObject);
begin
  OkayPressed := True;
  Close;
end;

procedure TAddCodeWindow.FormShow(Sender: TObject);
begin
  OkayPressed := False;
end;

procedure TAddCodeWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  If Not OkayPressed Then

     Memo1.Lines.Clear

  Else Begin

     OkayPressed := False;
     If Not PasteLines Then
        Action := CaNone;

  End;

End;

Function TAddCodeWindow.PasteLines: Boolean;
Var
  X, Idx: Integer;
  Ignore: Boolean;
  BASICBackUp, LineStr: String;
Begin
  Result := True;
  Ignore := Not Opt_OverwriteProtect;
  If Memo1.Lines.Count > 0 Then Begin
     For X := 0 To Memo1.Lines.Count -1 Do Begin
        BASICBackup := BASinOutput.BASICMem;
        If Not BASinOutput.AddLine(Memo1.Lines[0]) Then
           If BASinOutput.OverWriteCursor Then Begin
              Idx := 1;
              LineStr := '';
              While Memo1.Lines[0][Idx] in ['0'..'9'] Do Begin
                 LineStr := LineStr + Memo1.Lines[0][Idx];
                 Inc(Idx);
              End;
              If Not Ignore Then
                 Idx := MessageDlg('Line '+ LineStr + ' Will be overwritten with new code.'+#13+'Continue overwrite?', mtWarning, [mbYes, mbYesToAll, mbCancel], 0)
              Else
                 Idx := mrYes;

              If Idx = mrCancel Then Begin
                 BASinOutput.BASICMem := BASICBackup;
                 Break;
              End;

              If Idx = 10 Then Ignore := True;
              If (Idx = 10) or (Idx = mrYes) Then Begin
                 BASinOutput.OverwriteCursor := True;
                 BASinOutput.AddLine(Memo1.Lines[0]);
              End;
           End Else Begin
              // Syntax error, or somesuch. Remaining lines (and the offending line)
              // will still be in the AddCode window, so we'll re-show it.
              If Registers.EmuRunning then Begin
                 // Bailing out here will let the emulation commence for direct commands.
                 // And the Add Code window will resume when the Editor pops up and finds more lines to go.
                 Memo1.Lines.Delete(0);
                 Exit;
              End Else Begin
                 ShowWindow(LogWindow, False);
                 MakeSound(3);
                 BASinOutput.OverWriteCursor := False;
                 BASinOutput.RepaintBASIC(True);
                 Result := False;
                 Exit;
              End;
           End;
        Memo1.Lines.Delete(0);
     End;
     BASinOutput.OverwriteCursor := False;
     BASinOutput.RepaintBASIC(True);
  End;
end;

procedure TAddCodeWindow.Button3Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_add_code.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
