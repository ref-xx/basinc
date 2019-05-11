unit SpecialVarEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSpecialVarsWindow = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Notebook1: TNotebook;
    ListBox1: TListBox;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Button2: TButton;
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpdateFORParameter(Index: Byte; Text: String);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GetArray(VarAddress: Word);
    procedure GetFORVar(VarAddress: Word);
  end;

var
  SpecialVarsWindow: TSpecialVarsWindow;
  ArrayVarAddress, ArrayVarSize: Word;
  EditType: Byte;
  EditWidth, EditIndex: Word;
  ArrayBackup: String;

implementation

Uses FastCore, ROMUtils, BASSupport, Evaluate, VarsWindow, Utility;

{$R *.DFM}

Procedure TSpecialVarsWindow.GetArray(VarAddress: Word);
Var
  SaveDWord, TempDWord: DWord;
  TempByte: Byte;
  NumItems, OrgAddress, TempWord: Word;
  TempValue: Extended;
  VarName, TempStr: String;
Begin
  ListBox1.Items.BeginUpdate;
  ListBox1.Items.Clear;

  OrgAddress := VarAddress;
  If Memory[VarAddress] and 224 in [128, 192] Then Begin
     VarName := Chr((Memory[VarAddress] and 31)+96);
     If Memory[VarAddress] and 224 = 192 Then
        VarName := VarName +'$('
     Else
        VarName := VarName + '(';
     TempByte := Memory[VarAddress+3];
     SaveDWord := 1;
     Inc(VarAddress, 4);
     While TempByte > 0 Do Begin
        TempWord := GetWord(@Memory[VarAddress]);
        SaveDWord := SaveDWord * TempWord;
        If TempByte > 1 Then
           VarName := VarName + IntToStr(TempWord) + ','
        Else
           VarName := VarName + IntToStr(TempWord) + ')';
        Dec(TempByte);
        Inc(VarAddress, 2);
     End;
     // Save the start of the array values for later
     ArrayVarAddress := VarAddress;
     If Memory[OrgAddress] and 224 = 128 then Begin
        // Numeric Array.
        // Get the parameters, and the items.
        While SaveDWord > 0 Do Begin
           TempByte := Memory[VarAddress];
           TempDWord := GetDWord(@Memory[VarAddress+1]);
           TempValue := Byte5ToFloat(TempByte, TempDWord);
           ListBox1.Items.Add(FloatToStrEx(TempValue));
           Inc(VarAddress, 5);
           Dec(SaveDWord);
        End;
        Caption := 'Edit Numeric Array';
        EditType := 1;
        ArrayVarSize := VarAddress-ArrayVarAddress;
     End Else Begin
        // String Array.
        // Get the items. This is slightly different, in that
        // we group by the last DIM value - Tempword, from the code above.
        TempStr := '';
        While SaveDWord > 0 Do Begin
           TempStr := TempStr + Chr(Memory[VarAddress]);
           Inc(VarAddress);
           Dec(SaveDWord);
        End;
        ArrayVarSize := VarAddress-ArrayVarAddress;
        While TempStr <> '' Do Begin
           ListBox1.Items.Add(InsertEscapes('"'+Copy(TempStr, 1, TempWord)+'"'));
           TempStr := Copy(TempStr, TempWord+1, 99999);
        End;
        Caption := 'Edit String Array';
        EditType := 0;
        EditWidth := TempWord;
     End;
     GroupBox1.Caption := ' Values for '+VarName+' ';
  End;

  // Now, resize up to a maximum of 10 items - this may not work for all
  // XP themes, at least not properly.
  NumItems := ListBox1.Items.Count;
  If NumItems > 10 Then NumItems := 10;
  Height := (NumItems*ListBox1.ItemHeight)+96;
  ListBox1.Items.EndUpdate;
  ArrayBackup := GetMemoryString(ArrayVarAddress, ArrayVarSize, Memory);

  NoteBook1.PageIndex := 1;

End;

procedure TSpecialVarsWindow.ListBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  IRect: TRect;
begin
  If ListBox1.ItemIndex > -1 Then Begin
     IRect := ListBox1.ItemRect(ListBox1.ItemIndex);
     Edit1.SetBounds(ListBox1.Left+IRect.Left+1,
                     ListBox1.Top+IRect.Top,
                    (IRect.Right - IRect.Left)+2,
                    (IRect.Bottom - IRect.Top)+4);
     Edit1.Text := ListBox1.Items[ListBox1.ItemIndex];
     Edit1.SelectAll;
     Edit1.Visible := True;
     Edit1.SetFocus;
     EditIndex := ListBox1.ItemIndex;
  End Else
     Edit1.Visible := False;
end;

procedure TSpecialVarsWindow.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
   Expr: TExpression;
  TypeStr: String;
  VarAddress: Word;
  F: Word;
begin
  // This edit box is exclusively for the use of the VarArrays editor.
  If Key = VK_RETURN Then Begin
     Key := 0;
     If EditType = 1 Then Begin
        // A numeric value is required.
        If Edit1.Text <> '' Then Begin
            Expr.Expression := Edit1.Text;
           Expr.SyntaxChecked := False;
           EvaluateExpr(Expr);
           If Expr.ResultType = 1 Then Begin
              TypeStr := FloatTo5Byte(Expr.ResultNum);
              // Find the variable
              VarAddress := ArrayVarAddress + (5 * EditIndex);
              // Insert the value.
              Memory[VarAddress] := Ord(TypeStr[1]);
              PutDWord(@Memory[VarAddress+1], GetDWord(@TypeStr[2]));
              ListBox1.Items[EditIndex] := FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])));
              Edit1.Text := ListBox1.Items[EditIndex];
              VariablesWindow.BuildVarsList;
           End Else Begin
              If Expr.ResultType = 0 Then
                 MessageBox(Handle, pChar('Numeric value required'), pChar('Type error'), MB_OK or MB_ICONWARNING)
              Else
                 MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 9999)), pChar('Input Error'), MB_OK or MB_ICONWARNING);
           End;
        End;
     End Else Begin
        // A string value.
        If Edit1.Text <> '' Then Begin
            Expr.Expression := Edit1.Text;
           Expr.SyntaxChecked := False;
           EvaluateExpr(Expr);
           If Expr.ResultType = 0 Then Begin
              // Got a string result - Make sure it's the right size.
              While Length(Expr.ResultStr) < EditWidth Do Expr.ResultStr := Expr.ResultStr + ' ';
              While Length(Expr.ResultStr) > EditWidth Do Expr.ResultStr := Copy(Expr.ResultStr, 1, Length(Expr.ResultStr)-1);
              // Now insert into memory. no need to make room.
              VarAddress := ArrayVarAddress + (EditWidth * EditIndex);
              For F := 1 to Length(Expr.ResultStr) Do
                 Memory[VarAddress+(f-1)] := Ord(Expr.ResultStr[F]);
              ListBox1.Items[EditIndex] := '"'+GetMemoryString(VarAddress, EditWidth, Memory)+'"';
              Edit1.Text := ListBox1.Items[EditIndex];
              VariablesWindow.BuildVarsList;
           End Else Begin
              If Expr.ResultType = 1 Then
                 MessageBox(Handle, pChar('String value required'), pChar('Type error'), MB_OK or MB_ICONWARNING)
              Else
                 MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 9999)), pChar('Input Error'), MB_OK or MB_ICONWARNING);
           End;
        End;
     End;
     // Update the listbox and edit box with the new value.
     Edit1.SetFocus;
     Edit1.SelectAll;
  End;
end;

procedure TSpecialVarsWindow.ListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
Var
  RCT: TRect;
  EditVisible: Boolean;
begin
  // This little lot makes the edit box follow the listbox as it scrolls.
  (Control As TListBox).Canvas.FillRect(Rect);
  (Control As TListBox).Canvas.TextOut(Rect.Left, Rect.Top, ListBox1.Items[Index]);
  If Index = EditIndex Then Begin
     Rct := ListBox1.ItemRect(EditIndex);
     EditVisible := ListBox1.ItemIndex <> -1;
     If (Rct.Bottom <= 0) or (Rct.Bottom >= ListBox1.Height) Then EditVisible := False;
     Edit1.SetBounds(ListBox1.Left+Rct.Left+1,
                     ListBox1.Top+Rct.Top,
                    (Rct.Right - Rct.Left)+2,
                    (Rct.Bottom - Rct.Top)+4);
     If Edit1.Visible <> EditVisible Then Edit1.Visible := EditVisible;
  End Else Begin
     Rct := ListBox1.ItemRect(EditIndex);
     EditVisible := ListBox1.ItemIndex <> -1;
     If (Rct.Bottom <= 0) or (Rct.Bottom >= ListBox1.Height) Then EditVisible := False;
     If Edit1.Visible <> EditVisible Then Edit1.Visible := EditVisible;
  End;
end;

Procedure TSpecialVarsWindow.GetFORVar(VarAddress: Word);
Begin
  ArrayVarAddress := VarAddress;
  ArrayVarSize := 19;
  Caption := 'Edit FOR Variable';
  ArrayBackup := GetMemoryString(ArrayVarAddress, ArrayVarSize, Memory);
  GroupBox1.Caption := ' Parameters for '+Chr((Memory[VarAddress] and 31)+96)+' ';
  // START/Current value
  Edit2.Text := FloatToStrEx(Byte5ToFloat(Memory[VarAddress+1], GetDWord(@Memory[VarAddress+2])));
  Inc(VarAddress, 6);
  // TO
  Edit3.Text := FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])));
  Inc(VarAddress, 5);
  // STEP
  Edit4.Text := FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])));
  Inc(VarAddress, 5);
  // Line
  Edit5.Text := IntToStr(GetWord(@Memory[VarAddress]));
  // Statement
  Edit6.Text := IntToStr(Memory[VarAddress+2]);
  SetBounds(Left, Top, 250, 248);
  NoteBook1.PageIndex := 2;
End;

procedure TSpecialVarsWindow.Edit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var
  Edit: TEdit;
begin
  // Checks the input for the EDIT boxes, in the FOR editor.
  If Key = VK_RETURN Then Begin
     Key := 0;
     Edit := Sender as TEdit;
     If Edit.Text <>'' Then Begin
        // This section updates the variable as the user
        // presses return in each of the edit boxes.
        If Sender = Edit2 Then UpdateFORParameter(1, Edit.Text);
        If Sender = Edit3 Then UpdateFORParameter(2, Edit.Text);
        If Sender = Edit4 Then UpdateFORParameter(3, Edit.Text);
        If Sender = Edit5 Then UpdateFORParameter(4, Edit.Text);
        If Sender = Edit6 Then UpdateFORParameter(5, Edit.Text);
        VariablesWindow.BuildVarsList;
        Edit.SelectAll;
        Edit.SetFocus;
     End;
  End;
end;

Procedure TSpecialVarsWindow.UpdateFORParameter(Index: Byte; Text: String);
Var
  Size, Address: Word;
  Expr: TExpression;
  Number: Extended;
  TempStr: String;
Begin
  Expr.Expression := Text;
  Expr.SyntaxChecked := False;
  EvaluateExpr(Expr);
  If Expr.ResultType <> 1 Then Begin
     If Expr.ResultType = 0 Then
        MessageBox(Handle, pChar('Numeric value required'), pChar('Type error'), MB_OK or MB_ICONWARNING)
     Else
        MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 9999)), pChar('Input Error'), MB_OK or MB_ICONWARNING);
  End Else Begin
     Number := Expr.ResultNum;
     If Index = 1 Then Begin Size := 5; Address := ArrayVarAddress+1;  End; // [5] Current
     If Index = 2 Then Begin Size := 5; Address := ArrayVarAddress+6;  End; // [5] TO
     If Index = 3 Then Begin Size := 5; Address := ArrayVarAddress+11; End; // [5] STEP
     If Index = 4 Then Begin Size := 2; Address := ArrayVarAddress+16; End; // [2] Line
     If Index = 5 Then Begin Size := 1; Address := ArrayVarAddress+18; End; // [1] Statement
     Case Size of
        1: Begin
              Memory[Address] := Byte(Round(Number));
           End;
        2: Begin
              PutWord(@Memory[Address], Word(Round(Number)));
           End;
        5: Begin
              TempStr := FloatTo5Byte(Number);
              Memory[Address] := Ord(TempStr[1]);
              PutDWord(@Memory[Address+1], GetDWord(@TempStr[2]));
           End;
     End;
  End;
End;

procedure TSpecialVarsWindow.Button1Click(Sender: TObject);
Var
  Address, F: Word;
begin
  F := 1;
  For Address := ArrayVarAddress To ArrayVarAddress + ArrayVarSize -1 Do Begin
     Memory[Address] := Ord(ArrayBackup[F]);
     Inc(F);
  End;
  Close;
end;

procedure TSpecialVarsWindow.Button2Click(Sender: TObject);
begin
  // Close the window if an array edit,
  // appy changes if it's a FOR edit.
  If NoteBook1.PageIndex = 2 Then Begin
     // FOR Var
     UpdateFORParameter(1, Edit2.Text);
     UpdateFORParameter(2, Edit3.Text);
     UpdateFORParameter(3, Edit4.Text);
     UpdateFORParameter(4, Edit5.Text);
     UpdateFORParameter(5, Edit6.Text);
  End;
  Close;
end;

procedure TSpecialVarsWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  VariablesWindow.BuildVarsList;
end;

procedure TSpecialVarsWindow.FormCreate(Sender: TObject);
begin
  GroupBox1.SetBounds(4, 4, ClientWidth - 8, ClientHeight - (Button1.Height + 12));
  Button1.SetBounds(Width - 12 - Button1.Width, GroupBox1.Top + GroupBox1.Height + 4, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - (Button2.Width+ 4), Button1.Top, Button1.Width, Button1.Height);
end;

end.

