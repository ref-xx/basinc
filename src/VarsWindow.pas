unit VarsWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Menus;

type
  TVariablesWindow = class(TForm)
    ListView1: TListView;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    Edit1: TMenuItem;
    WatchthisVariable1: TMenuItem;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WatchthisVariable1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure BuildVarsList;
    Function  IsVariable(Name: String): Boolean;
  end;

var
  VariablesWindow: TVariablesWindow;
  VarAddresses:    Array[0..1024] of Word;
  VarNames:        TStringList;

implementation

{$R *.DFM}

Uses FastCore, ROMUtils, BASSupport, Watches, QueryForm, Evaluate, SpecialVarEdit, Utility,
     BasinMain;




Procedure TVariablesWindow.BuildVarsList;
Var
  Done: Boolean;
  TempByte: Byte;
  ardaTemp, VarAddress, TempWord: Word;
  VarCount, TempDWord, SaveDWord: DWord;
  TempValue: Extended;
  LI: TListItem;
  TempStr: String;
Begin

  // This is similar to the SaveBAS proc's section on saving Runtime variables,
  // but populates the main listview instead.

  VarCount := 0;
  ListView1.Items.BeginUpdate;

  If ProgStateFlag <> PS_Reset Then Begin
     VarAddress := GetWord(@Memory[VARS]);
     Done := False;
     While Not Done Do Begin
        VarAddresses[VarCount] := VarAddress;
        If Memory[VarAddress] = 128 Then Begin
           Done := True;
        End Else Begin
           Inc(VarCount);
           If VarCount <= DWord(ListView1.Items.Count) Then
              LI := ListView1.Items[VarCount-1]
           Else
              LI := ListView1.Items.Add;

           LI.SubItems.Clear;

           Case (Memory[VarAddress] and 224) of
              64:
                 Begin
                    // String
                    TempWord := GetWord(@Memory[VarAddress+1]);
                    LI.Caption := Chr((Memory[VarAddress] and 31)+96) + '$';
                    LI.SubItems.Add('String');
                    LI.SubItems.Add('"'+InsertEscapes(GetMemoryString(VarAddress+3, TempWord, Memory))+'"');
                    Inc(VarAddress, TempWord+3);
                 End;
              96:
                 Begin
                    // Simple Numeric
                    TempByte := Memory[VarAddress+1];
                    TempDWord := GetDWord(@Memory[VarAddress+2]);
                    TempValue := Byte5ToFloat(TempByte, TempDWord);
                    LI.Caption := Chr((Memory[VarAddress] and 31)+96);
                    LI.SubItems.Add('Numeric');
                    LI.SubItems.Add(FloatToStrEx(TempValue));
                    Inc(VarAddress, 6);
                 End;
              160:
                 Begin
                    // Complex Numeric
                    TempStr := Chr((Memory[VarAddress] and 31)+96);
                    Inc(VarAddress);
                    While Memory[VarAddress] and 128 = 0 Do Begin
                       TempStr := TempStr + LowerCase(Chr(Memory[VarAddress]));
                       Inc(VarAddress);
                    End;
                    LI.Caption := TempStr + LowerCase(Chr(Memory[VarAddress] and 127));
                    LI.SubItems.Add('Numeric');
                    TempByte := Memory[VarAddress+1];
                    TempDWord := GetDWord(@Memory[VarAddress+2]);
                    TempValue := Byte5ToFloat(TempByte, TempDWord);
                    LI.SubItems.Add(FloatToStrEx(TempValue));
                    Inc(VarAddress, 6);
                 End;
              128:
                 Begin
                    // Numeric Array
                    LI.Caption := Chr((Memory[VarAddress] and 31)+96);
                    TempStr := 'NumArray (';
                    TempByte := Memory[VarAddress+3];
                    SaveDWord := 1;
                    Inc(VarAddress, 4);
                    While TempByte > 0 Do Begin
                       TempWord := GetWord(@Memory[VarAddress]);
                       If TempByte > 1 Then
                          TempStr := TempStr + IntToStr(TempWord) + ', '
                       Else
                          TempStr := TempStr + IntToStr(TempWord) + ')';
                       SaveDWord := SaveDWord * TempWord;
                       Dec(TempByte);
                       Inc(VarAddress, 2);
                    End;
                    LI.SubItems.Add(TempStr);
                    TempStr := '';
                    While SaveDWord > 0 Do Begin
                       TempByte := Memory[VarAddress];
                       TempDWord := GetDWord(@Memory[VarAddress+1]);
                       TempValue := Byte5ToFloat(TempByte, TempDWord);
                       TempStr := TempStr + FloatToStrEx(TempValue);
                       If SaveDWord > 1 Then TempStr := TempStr + ', ';
                       Inc(VarAddress, 5);
                       Dec(SaveDWord);
                    End;
                    LI.SubItems.Add(TempStr);
                 End;
              192:
                 Begin
                    // String Array
                    LI.Caption := Chr((Memory[VarAddress] and 31)+96)+'$';
                    TempStr := 'StrArray (';
                    TempByte := Memory[VarAddress+3];
                    SaveDWord := 1;
                    Inc(VarAddress, 4);
                    While TempByte > 0 Do Begin
                       ardaTemp:=GetWord(@Memory[E_LINE]);
                       TempWord := GetWord(@Memory[VarAddress]);
                       SaveDWord := SaveDWord * TempWord;

                       If TempByte > 1 Then
                          TempStr := TempStr + IntToStr(TempWord) + ', '
                       Else
                          TempStr := TempStr + IntToStr(TempWord) + ')';
                       Dec(TempByte);
                       Inc(VarAddress, 2);

                       If VarAddress + 2 > GetWord(@Memory[E_LINE]) Then Begin   // ardafix Prevent invalid access
                         Done := True;
                         break;
                       End;

                    End;
                    LI.SubItems.Add(TempStr);
                    TempStr := '"';
                    While SaveDWord > 0 Do Begin
                    If VarAddress + 1 > GetWord(@Memory[E_LINE]) Then Begin   // ardafix 1795 Prevent invalid access
                        Done := True;
                        SaveDWord := 1;
                       End else begin
                        TempStr := TempStr + Chr(Memory[VarAddress]);
                    End;
                       If SaveDWord = 1 Then TempStr := TempStr + '"';
                       Inc(VarAddress);
                       Dec(SaveDWord);
                    End;
                    TempStr := InsertEscapes(TempStr);
                    LI.SubItems.Add(TempStr);
                 End;
              224:
                 Begin
                    // FOR Variable
                    LI.Caption := Chr((Memory[VarAddress] and 31)+96);
                    LI.SubItems.Add('FOR Var');
                    // START
                    TempStr := FloatToStrEx(Byte5ToFloat(Memory[VarAddress+1], GetDWord(@Memory[VarAddress+2])))+' TO ';
                    Inc(VarAddress, 6);
                    // TO
                    TempStr := TempStr + FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])))+' STEP ';
                    Inc(VarAddress, 5);
                    // STEP
                    TempStr := TempStr + FloatToStrEx(Byte5ToFloat(Memory[VarAddress], GetDWord(@Memory[VarAddress+1])))+' [';
                    Inc(VarAddress, 5);
                    // LINE/STATEMENT
                    TempStr := TempStr + IntToStr(GetWord(@Memory[VarAddress]))+':';
                    TempStr := TempStr + IntToStr(Memory[VarAddress+2])+']';
                    Inc(VarAddress, 3);
                    LI.SubItems.Add(TempStr);
                 End;
           Else
              Begin
                 Done := True;
              End;
           End;
        End;
     End;
  End;

  While DWord(ListView1.Items.Count) > VarCount Do
     ListView1.Items.Delete(ListView1.Items.Count-1);

  ListView1.Items.EndUpdate;

  PopupMenu1Popup(nil);

End;

Function TVariablesWindow.IsVariable(Name: String): Boolean;
Var
  Idx: Integer;
  NewName: String;
Begin
  Result := False;
  NewName := '';
  For Idx := 1 To Length(Name) Do
     If Name[Idx] in [#36, #48..#57, #65..#90] Then
        NewName := NewName + Name[Idx];
  If ProgStateFlag <> PS_Reset Then Begin
     Idx := 0;
     Result := False;
     While Idx < ListView1.Items.Count Do Begin
        Result := NewName = UpperCase(ListView1.Items[Idx].Caption);
        If Result Then Break;
        Inc(Idx);
     End;
  End;
End;

procedure TVariablesWindow.FormShow(Sender: TObject);
begin
 if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;

  BuildVarsList;
end;

procedure TVariablesWindow.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TVariablesWindow.FormCreate(Sender: TObject);
begin

  ListView1.DoubleBuffered := True;
  ListView1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - 24 - Button1.Height - ListView1.Top);
  Button2.SetBounds(8, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Width + 12, Button2.Top, Button1.Width, Button1.Height);
  Button3.SetBounds(ClientWidth - 8 - Button3.Width, Button1.Top,Button3.Width, Button3.Height);
  Button4.SetBounds(Button3.Left - Button4.Width - 4, Button3.Top, Button4.Width, Button3.Height);
  if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;


end;

procedure TVariablesWindow.PopupMenu1Popup(Sender: TObject);
begin
  Edit1.Enabled := ListView1.Items.Count > 0;
  WatchThisVariable1.Enabled := ListView1.Items.Count > 0;
  Button1.Enabled := ListView1.Selected <> nil;
  Button2.Enabled := ListView1.Selected <> nil;
end;

procedure TVariablesWindow.ListView1Click(Sender: TObject);
begin
  PopupMenu1PopUp(nil);
end;

procedure TVariablesWindow.Button2Click(Sender: TObject);
begin
  If Sender <> nil Then Begin
     If ListView1.Selected <> nil Then Begin
        WatchWindow.CreateWatch(True, 1, 0, 0, 0, '', Listview1.Selected.Caption, False);
        If WatchWindow.Visible Then WatchWindow.BuildWatchList;
     End;
  End Else Begin

     WatchWindow.CreateWatch(True, 1, 0, 0, 0, '', Listview1.Items[BASinOutput.WatchVariable1.ImageIndex].Caption, False);
     If WatchWindow.Visible Then WatchWindow.BuildWatchList Else ShowWindow(WatchWindow, False);
  End;
end;

procedure TVariablesWindow.WatchthisVariable1Click(Sender: TObject);
begin
  Button2Click(Self);
end;


procedure TVariablesWindow.Button1Click(Sender: TObject);
Var
  LastInput, OldStr, TypeStr: String;
  VarAddress, F: Word;
  Expr: TExpression;
  Dist, Index: Integer;
  MousePos: TPoint;
Label
  GetInputStr,
  GetInputNum;
begin
  Windows.GetCursorPos(MousePos);
  CentreForm(SpecialVarsWindow, MousePos.X, MousePos.Y);
  // Edit the variable.
  // First, what type is it?

  If Sender <> Nil Then
     Index := ListView1.Selected.Index
  Else
     Index := BASinOutput.EditVariable1.ImageIndex;

  VarAddress := VarAddresses[Index];

  If Memory[VarAddress] and 224 in [96, 160] Then Begin
     // Easy enough - just a number.
     LastInput := ListView1.Items[Index].SubItems[1];
     GetInputNum:
     QueryWindow.GetQuery('Edit Numeric', 'New value for '+#39+ListView1.Items[Index].Caption+#39, 'Okay', 'Cancel', [LastInput]);
     LastInput := QueryWindow.ResultText;
     If QueryWindow.ResultText <> '' Then Begin
         Expr.Expression := QueryWindow.ResultText;
        Expr.SyntaxChecked := False;
        EvaluateExpr(Expr);
        If Expr.ResultType = 1 Then Begin
           TypeStr := FloatTo5Byte(Expr.ResultNum);
           If Memory[VarAddress] and 224 = 96 Then
              // Simple Numeric
              Inc(VarAddress)
           Else Begin
              // Complex Numeric
              Repeat
                 Inc(VarAddress);
              Until Memory[VarAddress] and 128 = 128;
              Inc(VarAddress);
           End;
           Memory[VarAddress] := Ord(TypeStr[1]);
           PutDWord(@Memory[VarAddress+1], GetDWord(@TypeStr[2]));
           BuildVarsList;
        End Else Begin
           If Expr.ResultType = 0 Then
              MessageBox(Handle, pChar('Numeric value required'), pChar('Type error'), MB_OK or MB_ICONWARNING)
           Else
              MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 999999)), pChar('Input Error'), MB_OK or MB_ICONWARNING);
           Goto GetInputNum;
        End;
     End;
  End Else If Memory[VarAddress] and 224 = 64 Then Begin
     // More complex - a string.
     LastInput := GetMemoryString(VarAddress+3, Memory[VarAddress+1], Memory);
     OldStr := LastInput;
     LastInput := '"'+LastInput+'"';
     GetInputStr:
     QueryWindow.GetQuery('Edit String', 'New value for '+#39+ListView1.Items[Index].Caption+#39, 'Okay', 'Cancel', [LastInput]);
     LastInput := QueryWindow.ResultText;
     If QueryWindow.ResultText <> '' Then Begin
         Expr.Expression := QueryWindow.ResultText;
        Expr.SyntaxChecked := False;
        EvaluateExpr(Expr);
        If Expr.ResultType = 0 Then Begin
           // Got a string result
           // Need to shift everything along, depending on the length of the new string.
           Dist := Length(Expr.ResultStr)-Length(OldStr);
           If Sender <> nil Then
              MoveSpectrumMemory(VarAddresses[ListView1.Selected.Index+1], Dist)
           Else
              MoveSpectrumMemory(VarAddresses[BASinOutput.EditVariable1.ImageIndex+1], Dist);
           If Length(Expr.ResultStr) > 0 Then
              For F := VarAddress+3 To VarAddress+2+Length(Expr.ResultStr) Do
                 Memory[F] := Ord(Expr.ResultStr[F-(VarAddress+2)]);
           PutWord(@Memory[VarAddress+1], Length(Expr.ResultStr));
           BuildVarsList;
        End Else Begin
           If Expr.ResultType = 1 Then
              MessageBox(Handle, pChar('String value required'), pChar('Type error'), MB_OK or MB_ICONWARNING)
           Else
              MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 999999)), pChar('Input Error'), MB_OK or MB_ICONWARNING);
           Goto GetInputStr;
        End;
     End;
  End Else If Memory[VarAddress] and 224 in [128, 192] Then Begin
     // An Array of numbers or strings.
     // We need to open a new window which will allow us to edit a list.
     SpecialVarsWindow.GetArray(VarAddress);
     ShowWindow(SpecialVarsWindow, True);
  End Else Begin
     // A FOR variable - and this also requires a custom editor.
     SpecialVarsWindow.GetFORVar(VarAddress);
     ShowWindow(SpecialVarsWindow, True);
  End;
  UpdateWatches;
end;

procedure TVariablesWindow.Edit1Click(Sender: TObject);
begin
  Button1Click(Self);
end;

procedure TVariablesWindow.Button4Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_variables.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
