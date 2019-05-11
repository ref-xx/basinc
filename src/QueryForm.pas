unit QueryForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Math;

type
  TQueryWindow = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ResultText: String;
    procedure GetQuery(WindowCap, Query, OkayCap, CancelCap: String; Default: Array of String);
  end;

var
  QueryWindow: TQueryWindow;

implementation

{$R *.DFM}

Uses Utility;

Procedure TQueryWindow.GetQuery(WindowCap, Query, OkayCap, CancelCap: String; Default: Array of String);
Var
  F: Integer;
  MousePos: TPoint;
begin
  GetCursorPos(MousePos);
  CentreForm(QueryWindow, MousePos.X, MousePos.Y);
  Caption := WindowCap;
  Label1.Caption := Query;
  If OkayCap = '' Then
     Button1.Visible := False
  Else Begin
     Button1.Caption := OkayCap;
     Button1.Visible := True;
  End;
  Button2.Caption := CancelCap;
  If high(Default) <= 0 Then Begin
     // Only one default text?
     If High(Default) >= 0 Then Begin
        Edit1.Visible := True;
        Combobox1.Visible := False;
        If High(Default) = 0 Then
           Edit1.Text := Default[0];
        Edit1.SelectAll;
        Edit1.TabOrder := 0;
     End Else Begin
        ComboBox1.Visible := False;
     End;
  End Else Begin
     // Populate the combobox with a list of stuff.
     Edit1.Visible := False;
     Combobox1.Visible := True;
     Combobox1.Items.BeginUpdate;
     For F := 1 To High(Default) Do
        ComboBox1.Items.Add(Default[F]);
     ComboBox1.Items.Endupdate;
     Combobox1.Text := Default[0];
     Combobox1.Selectall;
     Combobox1.TabOrder := 0;
  End;
  Width := Max(Canvas.TextWidth(Query)+24, 200);
  ShowWindow(QueryWindow, True);
End;

procedure TQueryWindow.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then Begin
     Key := 0;
     If Edit1.Visible Then
        ResultText := Edit1.Text
     Else
        ResultText := ComboBox1.Text;
     Close;
  End;
end;

procedure TQueryWindow.Button2Click(Sender: TObject);
begin
  ResultText := '';
  Close;
end;

procedure TQueryWindow.Button1Click(Sender: TObject);
begin
  If Edit1.Visible Then
     ResultText := Edit1.Text
  Else
     ResultText := ComboBox1.Text;
  Close;
end;

procedure TQueryWindow.FormCreate(Sender: TObject);
begin
  Label1.SetBounds(8, 8, Label1.Width, Label1.Height);
  ComboBox1.SetBounds(8, 8 + Label1.Height + 4, ClientWidth - 16, ComboBox1.Height);
  Edit1.SetBounds(ComboBox1.Left, ComboBox1.Top, ComboBox1.Width, ComboBox1.Height);
  Button2.SetBounds(ClientWidth - Button2.Width - 8, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Left - 4 - Button1.Width, Button2.Top, Button1.Width, Button1.Height);
end;

end.
