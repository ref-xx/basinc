unit RenumWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Utility;

type
  TRenumberForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Bevel1: TThemeBevel;
    Label5: TLabel;
    Bevel2: TThemeBevel;
    CheckBox1: TCheckBox;
    Label6: TLabel;
    Edit3: TEdit;
    Label7: TLabel;
    Edit4: TEdit;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     OkayPressed: Boolean;
     RenumStart, RenumStep, RenumRangeStart, RenumRangeEnd, RenumLineCount: Word;
  end;

var
  RenumberForm: TRenumberForm;

implementation

{$R *.DFM}

Uses ROMUtils, FastCore, Evaluate, BasinMain;

procedure TRenumberForm.FormShow(Sender: TObject);
begin
  OkayPressed := False;
  If Not BASinOutput.TokeniseEditText(True) Then Begin
     Close;
     Exit;
  End;
  If GetWord(@Memory[PROG]) = GetWord(@Memory[VARS]) Then Begin
     Edit1.Text := '';                                
     Edit2.Text := '';
     Label1.Enabled := False;
     Label2.Enabled := False;
     Label4.Enabled := False;
     Label5.Enabled := False;
     Label6.Enabled := False;
     Label7.Enabled := False;
     Edit1.Enabled := False;
     Edit2.Enabled := False;
     Edit3.Enabled := False;
     Edit4.Enabled := False;
     CheckBox1.Enabled := False;
     Button1.Enabled := False;
  End Else Begin
     CheckBox1.Enabled := True;
     Edit1.Text := IntToStr(RenumStart);
     Edit2.Text := IntToStr(RenumStep);
     Label1.Enabled := True;
     Label2.Enabled := True;
     Label4.Enabled := True;
     Label5.Enabled := True;
     Edit1.Enabled := True;
     Edit2.Enabled := True;
     Button1.Enabled := True;
     CheckBox1Click(nil);
  End;
end;

procedure TRenumberForm.Button1Click(Sender: TObject);
Var
  TempVal, TempVal2: Integer;
Begin
  OkayPressed := False;
     TempVal := Round(EvaluateNum(Edit1.Text, -1));
     If (TempVal < 1) or (TempVal > 9999) Then Begin
        MessageBeep(MB_ICONEXCLAMATION);
        Edit1.SetFocus;
        Exit;
     End;
     If Round(EvaluateNum(Edit2.Text, -1)) = -1 Then Begin
        MessageBeep(MB_ICONEXCLAMATION);
        Edit2.SetFocus;
        Exit;
     End;
     RenumLineCount := GetProgramLineCount;
     RenumStart := Round(EvaluateNum(Edit1.Text, -1));
     RenumStep := Round(EvaluateNum(Edit2.Text, -1));
     If CheckBox1.Checked Then Begin
        RenumRangeStart := 0;
        RenumRangeEnd := 9999;
     End Else Begin
        TempVal := Round(EvaluateNum(Edit3.Text, -1));
        If (TempVal < 1) or (TempVal > 9999) Then Begin
           MessageBeep(MB_ICONEXCLAMATION);
           Edit3.SetFocus;
           Exit;
        End;
        TempVal2 := Round(EvaluateNum(Edit4.Text, -1));
        If (TempVal2 < 1) or (TempVal2 > 9999) Then Begin
           MessageBeep(MB_ICONEXCLAMATION);
           Edit4.SetFocus;
           Exit;
        End;
        RenumRangeStart := TempVal;
        RenumRangeEnd := TempVal2;
     End;
     If RenumRangeStart > RenumRangeEnd Then Begin
        MessageBox(Handle, PChar('The current Range start value'#13'must be less than the Range End value.'), PChar('Invalid Renumber'), MB_OK or MB_ICONWARNING);
        Exit;
     End;
     If (RenumStart + (RenumStep * RenumLineCount)) > 9999 Then Begin
        MessageBox(Handle, PChar('The current Start and Step values will'#13'generate line numbers greater than'#13'9999.'), PChar('Invalid Renumber'), MB_OK or MB_ICONWARNING);
        Exit;
     End;
     OkayPressed := True;
  Close;
end;

procedure TRenumberForm.Button2Click(Sender: TObject);
begin
  OkayPressed := False;
  Close;
end;

procedure TRenumberForm.FormCreate(Sender: TObject);
begin
  RenumStart := 10;
  RenumStep := 10;
  RenumRangeStart := 1;
  RenumRangeEnd := 9999;
end;

procedure TRenumberForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  If OkayPressed Then Begin
     // Renumber the program
     RenumberBASICEx(RenumRangeStart, RenumRangeEnd, RenumStart, RenumStep);
     BASinOutput.TokeniseEditText(False);
     BASinOutput.GetBASIC;
  End;
end;

procedure TRenumberForm.CheckBox1Click(Sender: TObject);
begin
  Label6.Enabled := Not CheckBox1.Checked;
  Label7.Enabled := Not CheckBox1.Checked;
  Edit3.Enabled := Not CheckBox1.Checked;
  Edit4.Enabled := Not CheckBox1.Checked;
end;

procedure TRenumberForm.Button3Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_renumber.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
