unit GrabParms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Utility,ThemeBevelUnit;

type
  TUDGGrabWindow = class(TForm)
    Label1: TLabel;
    Bevel2: TThemeBevel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label6: TLabel;
    Bevel1: TThemeBevel;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Cancelled: Boolean;
    Val1: Integer;
    Val2: Integer;
    Val3: Integer;
    Val4: Integer;
    WindowType: Integer;
    FileSpec: AnsiString;
  end;

var
  UDGGrabWindow: TUDGGrabWindow;

implementation

{$R *.DFM}

Uses Evaluate, BASinMain;

procedure TUDGGrabWindow.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TUDGGrabWindow.FormShow(Sender: TObject);
begin
  Cancelled := True;
  Case WindowType of
     0: // Grab from memory
        Begin
           Label1.Visible := True;
           Bevel2.Visible := True;
           Label2.Visible := True;
           Label3.Visible := True;
           Edit1.Visible := True;
           Edit2.Visible := True;
           Label4.Top := Edit2.Top + 44;
           Bevel1.Top := Label4.Top + Label4.Height + 3;
           Edit3.Top := Bevel1.Top + 12;
           Edit4.Top := Edit3.Top + Edit3.Height + 6;
           Label5.Top := Edit3.Top + 4;
           Label6.Top := Edit4.Top + 4;
           Label4.Visible := True;
           Bevel1.Visible := True;
           Label5.Visible := True;
           Label6.Visible := True;
           Edit3.Visible := True;
           Edit4.Visible := True;
           ClientHeight := Edit4.Top + Edit4.Height + Button2.Height + 20;
        End;
     1: // Send To memory
        Begin
           Label1.Visible := True;
           Bevel2.Visible := True;
           Label2.Visible := True;
           Label3.Visible := True;
           Edit1.Visible := True;
           Edit2.Visible := True;
           Label4.Visible := False;
           Bevel1.Visible := False;
           Label5.Visible := False;
           Label6.Visible := False;
           Edit3.Visible := False;
           Edit4.Visible := False;
           ClientHeight := Edit2.Top + Edit2.Height + Button2.Height + 20;
        End;
     2: // Get from binary file
        Begin
           Label1.Visible := False;
           Bevel2.Visible := False;
           Label2.Visible := False;
           Label3.Visible := False;
           Edit1.Visible := False;
           Edit2.Visible := False;
           Label4.Top := Label1.Top;
           Bevel1.Top := Bevel2.Top;
           Edit3.Top := Edit1.Top;
           Edit4.Top := Edit2.Top;
           Label5.Top := Label2.Top;
           Label6.Top := Label3.Top;
           Label4.Visible := True;
           Bevel1.Visible := True;
           Label5.Visible := True;
           Label6.Visible := True;
           Edit3.Visible := True;
           Edit4.Visible := True;
           ClientHeight := Edit2.Top + Edit2.Height + Button2.Height + 20;
        End;
  End;
end;

procedure TUDGGrabWindow.Button1Click(Sender: TObject);
begin

  // Check for valid address.
  Val1 := Round(EvaluateNum(Edit1.Text, -1));
  If (Val1 < 0) or (Val1 > 65535) or (Val1+8 > 65535) Then Begin
     MessageBox(Handle, pChar('Valid addresses are in the range 0 to 655227'), pChar('Invalid Address'), MB_OK or MB_ICONWARNING);
     Edit1.SetFocus;
     Exit;
  End;
  // Check for valid number of UDGs and also correct amount
  Val2 := Round(EvaluateNum(Edit2.Text, -1));
  If Val2 < 1 Then Begin
     MessageBox(Handle, pChar('Number of graphics must be a minimum of 1'), pChar('Invalid Graphic Count'), MB_OK or MB_ICONWARNING);
     Edit2.SetFocus;
     Exit;
  End;

  If Edit3.Visible Then Begin
     // If we're grabbing from memory, then check that the parameters are > 0
     Val3 := Round(EvaluateNum(Edit3.Text, -1));
     If Val3 < 1 Then Begin
        MessageBox(Handle, pChar('Valid Data formats must be 1 byte or more.'), pChar('Invalid Data Format'), MB_OK or MB_ICONWARNING);
        Edit3.SetFocus;
        Exit;
     End;
     Val4 := Round(EvaluateNum(Edit4.Text, -1));
     If Val4 < 1 Then Begin
        MessageBox(Handle, pChar('Valid Data formats must be 1 byte or more.'), pChar('Invalid Data Format'), MB_OK or MB_ICONWARNING);
        Edit4.SetFocus;
        Exit;
     End;
  End;

  Cancelled := False;
  Close;

end;

procedure TUDGGrabWindow.Button3Click(Sender: TObject);
begin

  If Copy(Label1.Caption, 1, 4) = 'Grab' Then
     BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_udg_grab.html'), HH_DISPLAY_TOPIC, 0)
  Else
     BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_udg_send.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
