unit EvaluateBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TEvaluationWindow = class(TForm)
    Label1:    TLabel;
    ComboBox1: TComboBox;
    Label2:    TLabel;
    Memo1:     TMemo;
    Button1:   TButton;
    Button2:   TButton;
    Button3:   TButton;
    Button4: TButton;
    procedure  ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure  Button1Click(Sender: TObject);
    procedure  Button2Click(Sender: TObject);
    procedure  ComboBox1Change(Sender: TObject);
    procedure  Button3Click(Sender: TObject);
    procedure  FormShow(Sender: TObject);
    procedure  FormCreate(Sender: TObject);
    procedure  FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EvaluationWindow: TEvaluationWindow;

implementation

{$R *.DFM}

Uses Evaluate, Watches, BASinMain, Utility;

Procedure TEvaluationWindow.ComboBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
  ExprResult: TExpression;
  Expr: AnsiString;
  Index: Integer;
begin
  If Key = VK_RETURN Then Begin
     Button1.Caption := 'Stop';
     button2.Enabled := False;
     Key := 0;
     Expr := ComboBox1.Text;
     Memo1.Lines.Clear;
     If Expr <> '' Then Begin
        Index := ComboBox1.Items.IndexOf(Expr);
        If Index >= 0 Then
           Combobox1.Items.Delete(Index);
        ComboBox1.Items.Insert(0, Expr);
        ExprResult.Expression := Expr;
        ExprResult.SyntaxChecked := False;
        EvaluateExpr(ExprResult);
        If ExprResult.ResultType = 0 Then Begin
           // AnsiString result
           Memo1.Lines.BeginUpdate;
           Memo1.Lines.Clear;
           Memo1.Lines.Add('"'+ExprResult.ResultStr+'"');
           Memo1.Lines.EndUpdate;
        End Else If ExprResult.ResultType = 1 Then Begin
           // Numeric result
           Memo1.Lines.BeginUpdate;
           Memo1.Lines.Clear;
           Memo1.Lines.Add(FloatToStrEx(ExprResult.ResultNum));
           Memo1.Lines.EndUpdate;
        End Else Begin
           // Error
           Memo1.Lines.BeginUpdate;
           Memo1.Lines.Clear;
           Memo1.Lines.Add(Copy(ExprResult.ResultStr, 3, 9999));
           Memo1.Lines.EndUpdate;
        End;
        Combobox1.Text := Expr;
        ComboBox1.SelectAll;
     End;
     Button2.Enabled := True;
     Button1.Caption := 'Evaluate';
  End;
end;

procedure TEvaluationWindow.Button1Click(Sender: TObject);
Var
  Key: Word;
begin
  If Evaluating Then Begin
     EvalAbort := True;
     Button1.Caption := 'Evaluate';
     button2.Enabled := True;
  End Else Begin
     Key := VK_RETURN;
     ComboBox1KeyDown(Nil, Key, []);
  End;
end;

procedure TEvaluationWindow.Button2Click(Sender: TObject);
begin
  WatchWindow.CreateWatch(True, 0, 0, 0, 0, ComboBox1.Text, '', False);
end;

procedure TEvaluationWindow.ComboBox1Change(Sender: TObject);
begin
  Button1.Enabled := ComboBox1.Text <> '';
  Button2.Enabled := ComboBox1.Text <> '';
end;

procedure TEvaluationWindow.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TEvaluationWindow.FormShow(Sender: TObject);
begin
  Combobox1.Text := '';
  Button1.Caption := 'Evaluate';
  Button1.Enabled := False;
  Button2.Enabled := False;
  Memo1.Lines.Clear;
end;

procedure TEvaluationWindow.FormCreate(Sender: TObject);
begin
  ComboBox1.SetBounds(8, ComboBox1.Top, ClientWidth - 16, ComboBox1.Height);
  Memo1.SetBounds(8, Memo1.Top, ClientWidth - 16, ClientHeight - 16 - Button1.Height - Memo1.Top);
  Button2.SetBounds(12 + Button1.Width, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(8, Button2.Top, Button1.Width, Button1.Height);
  Button3.SetBounds(ClientWidth - 8 - Button3.Width, Button2.Top, Button3.Width, Button3.Height);
  Button4.SetBounds(Button3.Left - Button4.Width - 4, Button3.Top, Button4.Width, Button3.Height);
  ComboBox1Change(nil);
end;

procedure TEvaluationWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EvalAbort := True;
end;

procedure TEvaluationWindow.Button4Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_evaluator.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
