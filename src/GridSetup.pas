unit GridSetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Utility;

type
  TGridSetUpWindow = class(TForm)
    Label5: TLabel;
    Bevel1: TThemeBevel;
    Label6: TLabel;
    Edit4: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Button3: TButton;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    Label4: TLabel;
    Edit5: TEdit;
    Label7: TLabel;
    Edit6: TEdit;
    Label8: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    EditLeft,
    EditTop,
    EditType,
    EditWidth,
    Address,
    NumChars,
    EditHeight: Integer;

    Cancelled: Boolean;
  end;

var
  GridSetUpWindow: TGridSetUpWindow;

implementation

{$R *.DFM}

Uses UDGEdit, Evaluate, BASinMain;

procedure TGridSetUpWindow.FormShow(Sender: TObject);
begin

  If EditType = 0 Then Begin
     EditWidth := UDGWindow.EditWidth;
     EditHeight := UDGWindow.EditHeight;
     Caption := 'Editing Grid Setup';
     Label5.Caption := 'Grid Size Options';
     Label6.Visible := True;
     Edit2.Visible := True;
     Label2.Visible := True;
     Edit4.Visible := True;
     Label1.Visible := False;
     Label3.Visible := False;
     Label8.Visible := False;
     Edit6.Visible := False;
     Label7.Visible := False;
     Edit5.Visible := False;
     Edit1.Visible := False;
     Edit3.Visible := False;
     Label4.Visible := False;
     ComboBox1.Visible := False;
     ClientHeight := Edit2.Top + Edit2.Height + Button1.Height + 24;
     Edit4.SetFocus;
  End Else
     If EditType = 1 Then Begin

        Caption := 'Set Grid Size';
                Label6.Visible := True;
        Edit2.Visible := True;
        Label2.Visible := True;
        Edit4.Visible := True;
        Label7.Visible := False;
        Label8.Visible := False;
        Edit6.Visible := False;
        Edit5.Visible := False;
        Label5.Caption := 'Grid Size Options';
        Label1.Visible := False;
        Label3.Visible := False;
        Edit1.Visible := False;
        Edit3.Visible := False;
        ComboBox1.Visible := False;
        Label4.Visible := False;
        ClientHeight := Edit2.Top + Edit2.Height + Button1.Height + 24;
        Edit4.SetFocus;
     End Else If EditType = 2 Then Begin
        Label5.Caption := 'Selection Options';
        Caption := 'Set Selection';
        Label7.Visible := False;
        Label8.Visible := False;
        Edit6.Visible := False;
        Edit5.Visible := False;
        Label1.Visible := True;
        Label3.Visible := True;
        Edit1.Visible := True;
        Edit3.Visible := True;
        Edit1.Text := IntToStr(EditLeft);
        Edit3.Text := IntToStr(EditTop);

        Label6.Visible := True;
        Edit2.Visible := True;
        Label2.Visible := True;
        Edit4.Visible := True;

        ComboBox1.Visible := True;
        Label4.Visible := True;
        ClientHeight := ComboBox1.Top + ComboBox1.Height + Button1.Height + 24;
        Edit1.SetFocus;
     End Else If EditType = 3 Then Begin
        EditWidth := UDGWindow.EditWidth;
        EditHeight := UDGWindow.EditHeight;
        Caption := 'Character Count Setup';
        Label5.Caption := 'Character Set Options';
        Label1.Visible := False;
        Label3.Visible := False;
        Edit1.Visible := False;
        Edit3.Visible := False;
        Label4.Visible := False;
        ComboBox1.Visible := False;
        Label7.Visible := True;
        Edit5.Visible := True;
        Label8.Visible := True;
        Edit6.Visible := True;

        Label6.Visible := False;
        Edit2.Visible := False;
        Label2.Visible := False;
        Edit4.Visible := False;

        Edit5.Text := IntToStr(NumChars);
        Edit6.Text := IntToStr(Address);
        ClientHeight := Edit6.Top + Edit6.Height + Button1.Height + 24;
        Edit5.SetFocus;


     End;

  Edit4.Text := IntToStr(EditWidth);
  Edit2.Text := IntToStr(EditHeight);

  Cancelled := True;

end;

procedure TGridSetUpWindow.Button2Click(Sender: TObject);
begin

  Close;

end;

procedure TGridSetUpWindow.Edit4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then
     Button1Click(Nil);
end;

procedure TGridSetUpWindow.Button1Click(Sender: TObject);
Var
  Val1, Val2: Integer;
begin
  // Check for valid grid sizes

  Val1 := Round(EvaluateNum(Edit4.Text, -1));
  Val2 := Round(EvaluateNum(Edit2.Text, -1));
  NumChars := Round(EvaluateNum(Edit5.Text, -1));
  Address := Round(EvaluateNum(Edit6.Text, -1));

  If EditType = 0 Then Begin
     If (Val1 < 1) or (Val1 > 16) Then Begin
        MessageBox(Handle, pChar('Valid grid sizes are in the range 1 to 15'), pChar('Invalid grid size'), MB_OK or MB_ICONWARNING);
        Edit4.SetFocus;
        Exit;
     End;
     If (Val2 < 1) or (Val2 > 16) Then Begin
        MessageBox(Handle, pChar('Valid grid sizes are in the range 1 to 15'), pChar('Invalid grid size'), MB_OK or MB_ICONWARNING);
        Edit2.SetFocus;
        Exit;
     End;
     EditWidth := Val1;
     EditHeight := Val2;
  End Else If EditType = 1 Then Begin
     If (Val1 < 1) or (Val1 > 255) Then Begin
        MessageBox(Handle, pChar('Valid grid X-Sizes are in the range 1 to 255'), pChar('Invalid grid size'), MB_OK or MB_ICONWARNING);
        Edit4.SetFocus;
        Exit;
     End;
     If (Val2 < 1) or (Val2 > 191) Then Begin
        MessageBox(Handle, pChar('Valid grid Y-Sizes are in the range 1 to 191'), pChar('Invalid grid size'), MB_OK or MB_ICONWARNING);
        Edit2.SetFocus;
        Exit;
     End;
     EditWidth := Val1;
     EditHeight := Val2;
  End Else If EditType = 2 Then Begin
     If (Val1 < 1) or (Val1 > 256) Then Begin
        MessageBox(Handle, pChar('Valid selection widths are in the range 1 to 256 pixels'), pChar('Invalid selection size'), MB_OK or MB_ICONWARNING);
        Edit4.SetFocus;
        Exit;
     End;
     If (Val2 < 1) or (Val2 > 192) Then Begin
        MessageBox(Handle, pChar('Valid selection heights are in the range 1 to 192'), pChar('Invalid selection size'), MB_OK or MB_ICONWARNING);
        Edit2.SetFocus;
        Exit;
     End;
     EditWidth := Val1;
     EditHeight := Val2;
     Val1 := Round(EvaluateNum(Edit1.Text, -1));
     Val2 := Round(EvaluateNum(Edit3.Text, -1));
     If (Val1 < 0) or (Val1 > 255) Then Begin
        MessageBox(Handle, pChar('Valid selection Left coordinates are in'#13'the range 0 to 255 pixels'), pChar('Invalid selection offset'), MB_OK or MB_ICONWARNING);
        Edit1.SetFocus;
        Exit;
     End;
     If (Val2 < 0) or (Val2 > 191) Then Begin
        MessageBox(Handle, pChar('Valid selection Top coordinates are in'#13'the range 0 to 191 pixels'), pChar('Invalid selection offset'), MB_OK or MB_ICONWARNING);
        Edit3.SetFocus;
        Exit;
     End;
     EditLeft := Val1;
     EditTop := Val2;
  End;

  Cancelled := False;
  Close;

end;

procedure TGridSetUpWindow.Button3Click(Sender: TObject);
begin
  If EditType = 0 Then
     HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_udg_editor.html#gridsetup'), HH_DISPLAY_TOPIC, 0)
  Else If EditType = 1 Then
     HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_scr_editor.html#gridsetup'), HH_DISPLAY_TOPIC, 0);

end;

procedure TGridSetUpWindow.FormCreate(Sender: TObject);
begin

  ComboBox1.ItemIndex := 0;

end;

end.
