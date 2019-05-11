unit LogWind;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CommDlg;

type
  TLogWindow = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  Procedure Log(Text: String);
  Procedure LogTextExpression(Line, Statement: Integer; Expression: String);

var
  LogWindow: TLogWindow;

implementation

{$R *.DFM}

Uses Parser, Evaluate, BASinMain, Utility;

Procedure Log(Text: String); 
Begin
  LogWindow.Memo1.Lines.Add(TimeToStr(Time)+' '+Text);
End;

Procedure LogTextExpression(Line, Statement: Integer; Expression: String); 
Var
  ParseError: TParseError;
  Res: TExpression;
  LogError: Boolean;
  LogText: String;
Begin
  While Copy(Expression, 1, 1) = ' ' Do Expression := Copy(Expression, 2, 999999);
  LogError := False;
  ParseError := ParseInputLine(IntToStr(Line)+' PRINT '+Expression);
  If ParseError.Error <> '' Then Begin
     If ParseError.ErrorCode = -1 Then Begin
        If ParseError.Error[Length(ParseError.Error)] = '.' Then
           ParseError.Error := Copy(ParseError.Error, 1, Length(ParseError.Error)-1);
        LogText := ParseError.Error+' in Statement '+IntToStr(ParseError.Statement);
        LogError := True;
     End Else Begin
        If ParseError.Error[Length(ParseError.Error)-1] = ',' Then
           ParseError.Error := Copy(ParseError.Error, 1, Length(ParseError.Error)-2);
        LogText := ParseError.Error;
        If ParseError.ErrorCode < 0 Then LogError := True;
     End;
  End;
  If Not LogError Then Begin
     Res.Expression := Expression;
     Res.SyntaxChecked := False;
     EvaluateExpr(Res);
     
     If Res.ResultType = 0 Then
        LogText := Res.ResultStr
     Else
        If Res.ResultType = 1 Then
           LogText := FloatToStrEx(Res.ResultNum)
        Else
           If Res.ResultType = 2 Then
              LogText := Copy(Res.ResultStr, 3, 999999);
     LogText := '['+IntToStr(Line)+':'+IntToStr(Statement)+'] '+LogText;
  End;
  Log(LogText);
End;

procedure TLogWindow.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TLogWindow.Button2Click(Sender: TObject);
var
  ofn: TOpenFileName;
  szFile: array[0..MAX_PATH] of Char;
  Result: string;
Begin
  FillChar(ofn, SizeOf(TOpenFileName), 0);
  ofn.lStructSize := SizeOf(TOpenFileName);
  ofn.hwndOwner := Handle;
  ofn.lpstrFile := szFile;
  ofn.nMaxFile := SizeOf(szFile);
  ofn.lpstrTitle := PChar('Save Log text as...');
  ofn.lpstrInitialDir := PChar('.\');
  ofn.lpstrFilter := PChar('Log Textfile (*.log)'#0'*.log'#0);
  ofn.Flags := 6150;
  if GetSaveFileName(ofn) then Begin
     Result := StrPas(ofn.lpstrFile);
     Memo1.Lines.SaveToFile(Result);
  End;
end;

procedure TLogWindow.FormCreate(Sender: TObject);
begin
  Memo1.DoubleBuffered := True;
  Memo1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - 16 - Button1.Height - 8);
  Button3.SetBounds(ClientWidth - Button3.Width - 8, ClientHeight - Button3.Height - 8, Button3.Width, Button3.Height);
  Button1.SetBounds(8, Button3.Top, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Width + 12, Button1.Top, Button2.Width, Button2.Height);
  Button4.SetBounds(Button3.Left - Button4.Width - 4, button3.Top, Button4.Width, Button3.Height);
end;

procedure TLogWindow.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TLogWindow.Button4Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_log.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
