unit WatchProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Watches, ExtCtrls;

type
  TWatchProperties = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Notebook1: TNotebook;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox3: TCheckBox;
    Label3: TLabel;
    ComboBox2: TComboBox;
    Label4: TLabel;
    ComboBox3: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    Edit2: TEdit;
    ComboBox4: TComboBox;
    Button3: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OkayPressed: Boolean;
    Procedure GetWatch(Index: Byte);
  end;

var
  WatchProperties: TWatchProperties;
  WatchNumber: Byte;
  CurrentWatch: TWatch;

implementation

{$R *.DFM}

Uses SysVars, ROMUtils, Evaluate, BASinMain, Utility;

Procedure TWatchProperties.GetWatch(Index: Byte);
Begin

  CurrentWatch.Valid := WatchArray[Index].Valid;
  CurrentWatch.Enabled := WatchArray[Index].Enabled;
  CurrentWatch.Expression := WatchArray[Index].Expression;
  CurrentWatch.CanBreak := WatchArray[Index].CanBreak;
  CurrentWatch.WatchType := WatchArray[Index].WatchType;
  CurrentWatch.wTypeIndex := WatchArray[Index].wTypeIndex;
  CurrentWatch.wAddress := WatchArray[Index].wAddress;
  CurrentWatch.wSize := WatchArray[Index].wSize;
  CurrentWatch.wVarName := WatchArray[Index].wVarName;

  ComboBox1.ItemIndex := CurrentWatch.WatchType;

  CheckBox1.Checked := CurrentWatch.Enabled;
  Edit1.Text := CurrentWatch.Expression;
  CheckBox3.Checked := CurrentWatch.CanBreak;

  ComboBox2.Text := CurrentWatch.wVarName;
  ComboBox3.ItemIndex := CurrentWatch.wTypeIndex;
  ComboBox4.ItemIndex := CurrentWatch.wSize;
  Edit2.Text := IntToStr(CurrentWatch.wAddress);
  NoteBook1.PageIndex := CurrentWatch.WatchType;

  WatchNumber := Index;

End;

procedure TWatchProperties.CheckBox1Click(Sender: TObject);
begin
  Label2.Enabled := CheckBox1.Checked;
  ComboBox1.Enabled := CheckBox1.Checked;
  Notebook1.Enabled := CheckBox1.Checked;
  Label1.Enabled := CheckBox1.Checked;
  Edit1.Enabled := CheckBox1.Checked;
  CheckBox3.Enabled := CheckBox1.Checked;
  Label3.Enabled := CheckBox1.Checked;
  ComboBox2.Enabled := CheckBox1.Checked;
  Label4.Enabled := CheckBox1.Checked;
  ComboBox3.Enabled := CheckBox1.Checked;
  Label5.Enabled := CheckBox1.Checked;
  Label6.Enabled := CheckBox1.Checked;
  Edit2.Enabled := CheckBox1.Checked;
  ComboBox4.Enabled := CheckBox1.Checked;
end;

procedure TWatchProperties.Button2Click(Sender: TObject);
begin
  OkayPressed := False;
  Close;
end;

procedure TWatchProperties.Button1Click(Sender: TObject);
Var
  Expr: TExpression;
begin
  If ComboBox1.ItemIndex = 3 Then Begin
     // If this is an address watch, then we check the edit text for errors.
     // exactly the same way we do for every other edit box :)
     // I ought to  write a proc for this...
     Expr.Expression := Edit2.Text;
     Expr.SyntaxChecked := False;
     EvaluateExpr(Expr);

     If Expr.ResultType = 1 Then Begin
        // Check the result
        If (Expr.ResultNum > 65535) or (Expr.ResultNum < 0) Then Begin
           MessageBox(Handle, pChar('Value Must be 0 to 65535'), pChar('Range error'), MB_OK or MB_ICONWARNING);
           Exit;
        End Else
           WatchArray[WatchNumber].wAddress := Word(Round(Expr.ResultNum));
     End Else Begin
        If Expr.ResultType = 0 Then
           MessageBox(Handle, pChar('Numeric value required'), pChar('Type error'), MB_OK or MB_ICONWARNING)
        Else
           MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 9999)), pChar('Input Error'), MB_OK or MB_ICONWARNING);
        Exit;
     End;
  End;
  WatchArray[WatchNumber].Enabled := CheckBox1.Checked;
  WatchArray[WatchNumber].Expression := Edit1.Text;
  WatchExpressions[WatchNumber].Expression := Edit1.Text;
  WatchExpressions[WatchNumber].SyntaxChecked := False;
  WatchExpressions[WatchNumber].Tokenised := '';
  WatchArray[WatchNumber].CanBreak := CheckBox3.Checked;
  WatchArray[WatchNumber].LastResult := '';
  WatchArray[WatchNumber].Valid := True;
  WatchArray[WatchNumber].WatchType := ComboBox1.ItemIndex;
  WatchArray[WatchNumber].wTypeIndex := ComboBox3.ItemIndex;
  WatchArray[WatchNumber].wVarName := ComboBox2.Text;
  WatchArray[WatchNumber].wSize := ComboBox4.ItemIndex;
  OkayPressed := True;
  Close;
end;

procedure TWatchProperties.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then Begin
     Button1Click(nil);
     Key := 0;
  End;
end;

procedure TWatchProperties.ComboBox1Change(Sender: TObject);
Var
  List: TStringList;
begin
  Case ComboBox1.ItemIndex Of
     0: Begin // Expression - no processing needed.
        End;
     1: Begin // Variable - Update the combobox's list with variable names.
           List := TStringlist.Create;
           GetVarsList(List);
           ComboBox2.Items.Clear;
           ComboBox2.Items.AddStrings(List);
           List.Free;
        End;
     2: Begin // SYSVAR - No Processing needed, it's done in FormCreate().
        End;
     3: Begin // Memory Address - No Processing needed.
        End;
  End;
  NoteBook1.PageIndex := ComboBox1.ItemIndex;
end;

procedure TWatchProperties.FormCreate(Sender: TObject);
Var
  F: Integer;
begin
  ComboBox1.Itemindex := 0;
  ComboBox3.Items.Clear;
  For F := 0 to 70 Do
     ComboBox3.Items.Add(SystemVariables[F].Name);
  ComboBox3.ItemIndex := 0;
  ComboBox2.Text := '';
end;

procedure TWatchProperties.FormShow(Sender: TObject);
begin
  OkayPressed := False;
end;

procedure TWatchProperties.Button3Click(Sender: TObject);
begin
  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_watch_properties.html'), HH_DISPLAY_TOPIC, 0);
end;

end.
