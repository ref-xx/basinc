unit ErrorWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Languages;

type
  TErrorForm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    CheckBox1: TCheckBox;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
     Procedure PrepareContinue;
  public
    { Public declarations }
     Procedure ShowError(IsError: Boolean);
  end;

var
  ErrorForm: TErrorForm;
  ValidErrorPoint: Boolean;
  ContinueReady: Boolean;
  PleaseContinue: Boolean;
  CurHint: String;

implementation

{$R *.DFM}

Uses ROMUtils, FastCore, InputUtils, Utility, BasinMain;

Procedure TErrorForm.ShowError(IsError: Boolean);
Var
  HW: THintWindow;
  DispRect: TRect;
Begin
  CentreFormOnForm(Self, BASinOutput);
  HW := THintWindow.Create(Self);
  If LastError > 42 Then LastError := 43;
  If LastErrorLine = 65534 Then LastErrorLine := 0;
  If Opt_Language='Türkçe' Then Begin
       Caption := ErrorAddressesTR[LastError].Desc+', '+IntToStr(LastErrorLine)+':'+IntToStr(LastErrorStatement);
       CurHint := ErrorsTR[LastError][3]+#13#13+'Situation: '+ErrorsTR[LastError][4];
   end else if Opt_Language='English' Then Begin
       Caption := ErrorAddresses[LastError].Desc+', '+IntToStr(LastErrorLine)+':'+IntToStr(LastErrorStatement);
       CurHint := Errors[LastError][3]+#13#13+'Situation: '+Errors[LastError][4];
   end else if Opt_Language='Deutsch' Then Begin
       //Caption := ErrorAddressesDE[LastError].Desc+', '+IntToStr(LastErrorLine)+':'+IntToStr(LastErrorStatement);
       //CurHint := ErrorsDE[LastError][3]+#13#13+'Situation: '+ErrorsDE[LastError][4];
   end else Begin
       Caption := ErrorAddresses[LastError].Desc+', '+IntToStr(LastErrorLine)+':'+IntToStr(LastErrorStatement);
       CurHint := Errors[LastError][3]+#13#13+'Situation: '+Errors[LastError][4];

   End;

  Label1.Caption := CurHint;
  DispRect := HW.CalcHintRect(Label1.Width, Label1.Caption, nil);
  HW.Free;
  Self.ClientHeight := DispRect.Bottom + 12 + CheckBox1.Height + 4 + Button1.Height;
  Self.ClientWidth := DispRect.Right + 24 + Image1.Width;
  Label1.SetBounds(48, 8, DispRect.Right, DispRect.Bottom);
  CheckBox1.Checked := ErrorAddresses[LastError].Notify;
  ControlEmulation(False);
  ValidErrorPoint := IsError;
  ContinueReady := False;
  Button1.Enabled := IsError and (LastErrorLine > 0);
  ShowWindow(Self, True);
End;

procedure TErrorForm.Button1Click(Sender: TObject);
begin
  If ValidErrorPoint Then Begin
     PrepareContinue;
     PleaseContinue := True;
  End;
  Close;
end;

procedure TErrorForm.Button2Click(Sender: TObject);
begin
  If ValidErrorPoint Then Begin
     PrepareContinue;
     PutWord(@Registers.E, GetWord(@Memory[K_CUR]));
     Registers.PC := $12A9;
     ValidErrorPoint := False;
  End;
  Close;
end;

procedure TErrorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ErrorAddresses[LastError].Notify := CheckBox1.Checked;
  If ValidErrorPoint And Not ContinueReady Then PrepareContinue;
end;

Procedure TErrorForm.PrepareContinue;
Var
  ERRN, F: Byte;
  Line, Statement: Integer;
Begin
  ErrN := Memory[ERR_NR];
  Inc(ErrN);
  If ErrN <> 0 Then Begin
     // CONTINUE starts execution at the current statement, unless it's a BREAK, in which case it
     // carries on from the next statement. Note - if you BREAK on the last line of a program, it
     // will refuse to CONTINUE.
     If (ErrN = 9) or (ErrN = $15) Then Begin
        Line := GetWord(@Memory[PPC]);
        Statement := Memory[SUBPPC] +1;
        BASinOutput.GetSourcePos(Line, Statement);
        PutWord(@Memory[PPC], Line);
        Memory[SUBPPC] := Statement;
     End;
     PutWord(@Registers.C, 3);
     PutWord(@Registers.E, OSPCC);
     PutWord(@Registers.L, NSPPC);
     If Memory[NSPPC] and 128 = 128 Then PutWord(@Registers.L, GetWord(@Registers.C)+GetWord(@Registers.L));
     For F := 1 to 3 Do Begin
        Memory[GetWord(@Registers.E)] := Memory[GetWord(@Registers.L)];
        Dec(Registers.E);
        Dec(Registers.L);
     End;
  End;
  // Now some jiggery-pokery to get us to skip the error-print routine.
  // No sense in mucking about with it if it's been reported :-)
  Memory[NSPPC] := $FF;
  Memory[FLAGS] := Memory[FLAGS] and 247;
  ContinueReady := True;
End;

procedure TErrorForm.FormCreate(Sender: TObject);
begin
  Label1.SetBounds(48, 8, ClientWidth - 48, Label1.Height);
  Button2.SetBounds(ClientWidth - Button2.Width - 8, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Left - Button1.Width - 4, Button2.Top, Button1.Width, Button1.Height);
  CheckBox1.SetBounds(8, ClientHeight - CheckBox1.Height - 8, CheckBox1.Width, CheckBox1.Height);
  Button3.SetBounds(Button1.Left - Button3.Width - 4, ClientHeight - 8 - Button1.Height, Button3.Width, Button1.Height);
end;

procedure TErrorForm.FormShow(Sender: TObject);
begin
  PleaseContinue := False;
  Button2.SetFocus;
end;

procedure TErrorForm.Button3Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_error.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
