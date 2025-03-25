unit BreakpointProperties;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Breakpoints, ComCtrls;

type
  TBPProperties = class(TForm)
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    TabSheet2: TTabSheet;
    Label4: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Edit4: TEdit;
    CheckBox2: TCheckBox;
    Button3: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Cancelled: Boolean;
    Procedure GetBP(Index: Byte);
  end;

var
  BPProperties: TBPProperties;
  BPNumber: Byte;
  CurrentBP: TBreakpoint;

implementation

{$R *.DFM}

Uses Evaluate, BASinMain, Utility;

Procedure TBPProperties.GetBP(Index: Byte);
Begin
  CurrentBP.Valid := BreakArray[Index].Valid;
  CurrentBP.Break := BreakArray[Index].Break;
  CurrentBP.Enabled := BreakArray[Index].Enabled;
  CurrentBP.LogExpression := BreakArray[Index].LogExpression;
  CurrentBP.PassCount := BreakArray[Index].PassCount;
  CurrentBP.Line := BreakArray[Index].Line;
  CurrentBP.Statement := BreakArray[Index].Statement;
  CurrentBP.Condition := BreakArray[Index].Condition;
  CurrentBP.Log := BreakArray[Index].Log;

  CheckBox1.Checked := CurrentBP.Enabled;
  Edit1.Text := IntToStr(CurrentBP.Line);
  Edit2.Text := IntToStr(CurrentBP.Statement);
  Edit3.Text := CurrentBP.Condition;
  Edit4.Text := CurrentBP.Log;
  CheckBox2.Checked := CurrentBP.LogExpression;
  CheckBox3.Checked := CurrentBP.Break;
  BPNumber := Index;
End;

procedure TBPProperties.CheckBox1Click(Sender: TObject);
begin
  Label1.Enabled    := CheckBox1.Checked;
  Edit1.Enabled     := CheckBox1.Checked;
  Label2.Enabled    := CheckBox1.Checked;
  Edit2.Enabled     := CheckBox1.Checked;
  Label3.Enabled    := CheckBox1.Checked;
  Edit3.Enabled     := CheckBox1.Checked;
  CheckBox2.Enabled := CheckBox1.Checked;
  Label4.Enabled    := CheckBox1.Checked;
  Edit4.Enabled     := CheckBox1.Checked;
  CurrentBP.Enabled := CheckBox1.Checked;
  CheckBox3.Enabled := CheckBox1.Checked;
end;

procedure TBPProperties.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
  LineNum: Integer;
begin
  If Key = VK_RETURN Then Begin
     Key := 0;
     LineNum := StrToIntDef(Edit1.Text, -1);
     If (LineNum < 1) or (LineNum > 9999) Then
        MessageBox(Handle, pChar('Line Numbers must be integer between 1 and 9999 inclusive.'), pChar('Line Number error'), MB_OK or MB_ICONWARNING)
     Else
        Button1Click(nil);
  End;
end;

procedure TBPProperties.Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
  StatementNum: Integer;
begin
  If Key = VK_RETURN Then Begin
     Key := 0;
     StatementNum := StrToIntDef(Edit2.Text, -1);
     If (StatementNum < 1) or (StatementNum > 128) Then
        MessageBox(Handle, pChar('Statement Numbers must be integer between 1 and 128 inclusive.'), pChar('Statement error'), MB_OK or MB_ICONWARNING)
     Else
        Button1Click(nil);
  End;
end;

procedure TBPProperties.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TBPProperties.Button1Click(Sender: TObject);
Var
  LineNum,
  StatementNum: Integer;
Begin

  LineNum := Round(EvaluateNum(Edit1.Text, -1));
  If (LineNum < 1) or (LineNum > 9999) Then Begin
     MessageBox(Handle, pChar('Line Numbers must be integer between 1 and 9999 inclusive.'), pChar('Line Number error'), MB_OK or MB_ICONWARNING);
     Exit;
  End;

  StatementNum := Round(EvaluateNum(Edit2.Text, -1));
  If (StatementNum < 1) or (StatementNum > 128) Then Begin
     MessageBox(Handle, pChar('Statement Numbers must be integer between 1 and 128 inclusive.'), pChar('Statement error'), MB_OK or MB_ICONWARNING);
     Exit;
  End;

  CurrentBP.Valid := True;
  CurrentBP.Break := CheckBox3.Checked;
  CurrentBP.Enabled := CheckBox1.Checked;
  CurrentBP.LogExpression := Checkbox2.Checked;
  CurrentBP.PassCount := 0;
  CurrentBP.Line := LineNum;
  CurrentBP.Statement := StatementNum;
  CurrentBP.Condition := Edit3.Text;
  CurrentBP.Log := Edit4.Text;

  // Check to see if the line number or statement have changed - if so, we need to
  // remove the old breakpoint before continuing.

  If (BreakArray[BPNumber].Line <> CurrentBP.Line) or
     (BreakArray[BPNumber].Statement <> CurrentBP.Statement) Then Begin
     BreakpointsList[BreakArray[BPNumber].Line][BreakArray[BPNumber].Statement] := #0;
     BreakpointsList[CurrentBP.Line][CurrentBP.Statement] := Chr(BPNumber);
  End;

  BreakArray[BPNumber].Valid := CurrentBP.Valid;
  BreakArray[BPNumber].Break := CurrentBP.Break;
  BreakArray[BPNumber].Enabled := CurrentBP.Enabled;
  BreakArray[BPNumber].LogExpression := CurrentBP.LogExpression;
  BreakArray[BPNumber].PassCount := CurrentBP.PassCount;
  BreakArray[BPNumber].Line := CurrentBP.Line;
  BreakArray[BPNumber].Statement := CurrentBP.Statement;
  BreakArray[BPNumber].Condition := CurrentBP.Condition;
  BreakArray[BPNumber].Log := CurrentBP.Log;

  Cancelled := False;
  Close;

end;

procedure TBPProperties.FormCreate(Sender: TObject);
begin
  Button2.SetBounds(ClientWidth - Button2.Width - 8, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Left - Button1.Width - 8, Button2.Top, Button1.Width, Button1.Height);
  Button3.SetBounds(8, ClientHeight - 8 - Button1.Height, Button3.Width, Button1.Height);
  PageControl1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - Button1.Height - 22);
end;

procedure TBPProperties.FormShow(Sender: TObject);
begin
  Cancelled := True;
  PageControl1.ActivePageIndex := 0;
end;

procedure TBPProperties.Button3Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_breakpoint_properties.html'), HH_DISPLAY_TOPIC, 0);

end;

end.

