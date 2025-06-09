unit HexEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ClipBrd, Forms, Dialogs,
  Grids, MPHexEditor, StdCtrls, FastIMG, FastDIB, ExtCtrls, Menus;

type
  THexWindow = class(TForm)
    MPHexEditor1: TMPHexEditor;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Button3: TButton;
    ComboBox1: TComboBox;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    CopyasHex1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MPHexEditor1Change(Sender: TObject);
    procedure MPHexEditor1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MPHexEditor1TopLeftChanged(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure CopyasHex1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GetMemory(Address, Count: DWord; WinCap: String);
    procedure Refresh();
  end;

var
  HexWindow: THexWindow;
  UndoBuffer: String;
  StartAddr,
  DataSize: DWord;

implementation

{$R *.DFM}

Uses FastCore, ROMUtils, VarsWindow, Display, BASinMain, Utility;

Procedure THexWindow.GetMemory(Address, Count: DWord; WinCap: String);
Begin
  MPHexEditor1.SelectAll;
  MPHexEditor1.DeleteSelection;
  MPHexEditor1.AppendBuffer(@Memory[Address], Count);
  MPHexEditor1.ResetSelection(True);
  UndoBuffer := GetMemoryString(Address, Count, Memory);
  StartAddr := Address;
  DataSize := Count;
  Caption := WinCap;
End;

procedure THexWindow.FormCreate(Sender: TObject);
begin
  ComboBox1.SetBounds(8, 8, ComboBox1.Width, ComboBox1.Height);
  Button3.SetBounds(ComboBox1.Width + 12, 8, Button3.Width, ComboBox1.Height);
  Button4.SetBounds(Button3.Left + 4 + Button3.Width, Button3.Top, Button4.Width, Button3.Height);
  MPHexEditor1.SetBounds(8, ComboBox1.Top + ComboBox1.Height + 8, ClientWidth - 16, ClientHeight - 24 - Button1.Height - ComboBox1.Height -8);
  Button1.SetBounds(ClientWidth - 8 - Button1.Width, ClientHeight - 8 - Button1.Height, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - 4 - Button2.Width, ClientHeight - 8 - Button2.Height, Button2.Width, Button2.Height);
  Panel1.SetBounds(8, Button2.Top, Button2.Left - 16, Button2.Height);
  Button5.SetBounds(ClientWidth - 8 - Button5.Width, 8, Button5.Width, Button4.Height);
  Button6.SetBounds(ClientWidth - 8 - Button5.Width - 8 - Button6.Width, 8, Button6.Width, Button4.Height);
  CheckBox1.SetBounds(ClientWidth - 8 - Button5.Width - 8 - Button6.Width-8-CheckBox1.Width, 8, CheckBox1.Width, Button4.Height);
  if Opt_ToolFontSize>0 Then MPHexEditor1.Font.Size:=Opt_ToolFontSize;
end;

procedure THexWindow.FormResize(Sender: TObject);
Var
  LetterW, CurValue: Integer;
begin


  if Opt_ToolFontSize>0 Then Begin
     Canvas.Font.Size:=Opt_ToolFontSize;
     LetterW := Canvas.TextWidth('a');
     //LetterW :=Opt_ToolFontSize;
     MPHexEditor1.GutterWidth := LetterW*6;
     End else Begin
     LetterW := Canvas.TextWidth('a');
  End;

  CurValue := MPHexEditor1.ClientWidth - MPHexEditor1.GutterWidth - (LetterW * 2);
  CurValue := CurValue Div 4;
  If (CurValue Div LetterW) > 0 Then MPHexEditor1.BytesPerRow := CurValue Div LetterW;
end;

procedure THexWindow.Button2Click(Sender: TObject);
Var
  F: DWord;
begin
  // Apply the changes
  For F := StartAddr To StartAddr + DataSize -1 Do
     Memory[F] := Byte(MPHexEditor1.Data[F-StartAddr]);
  Close;
end;

procedure THexWindow.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure THexWindow.FormShow(Sender: TObject);
Var
  Bool: Boolean;
begin
  if Opt_ToolFontSize>0 Then MPHexEditor1.Font.Size:=Opt_ToolFontSize;
  Bool := False;
  If (DataSize = 0) or (DataSize = 65536) Then Begin
     ComboBox1.Visible := True;
     Button3.Visible := True;
     Button4.Visible := True;
     GetMemory(0, 65536, 'Memory Viewer');
     MPHexEditor1.SetBounds(8, ComboBox1.Top + ComboBox1.Height + 8, ClientWidth - 16, ClientHeight - 24 - Button1.Height - ComboBox1.Height -8);
  End Else Begin
     MPHexEditor1.SetBounds(8, Button5.Top + Button5.Height + 8, ClientWidth - 16, ClientHeight - Button5.Height - Button2.Height - 32);
     ComboBox1.Visible := False;
     Button3.Visible := False;
     Button4.Visible := False;
  End;
  ComboBox1.Text := IntToStr(MPHexEditor1.GetTopLeftPosition(Bool));
end;

procedure THexWindow.MPHexEditor1Change(Sender: TObject);
begin
  If (DataSize = 0) or (DataSize = 65536) Then Begin
     If MPHexEditor1.GetCursorPos > 16383 Then Begin
        If Memory[MPHexEditor1.GetCursorPos] <> Byte(MPHexEditor1.Data[MPHexEditor1.GetCursorPos]) Then Begin
           Memory[MPHexEditor1.GetCursorPos] := Byte(MPHexEditor1.Data[MPHexEditor1.GetCursorPos]);
           If MPHexEditor1.GetCursorPos < 23296 Then Begin
              FastCore.UpdateDisplay;
              UpdateBASinDisplay;
           End;
        End;
     End;
     Label1.Caption := 'Address: '+IntToStr(MPHexEditor1.GetCursorPos) + '   Value: '+IntToStr(Memory[MPHexEditor1.GetCursorPos])+ '  Word: '+IntToStr(GetWord(@Memory[MPHexEditor1.GetCursorPos]))
  End Else
     Label1.Caption := 'Position: '+IntToStr(MPHexEditor1.GetCursorPos) + '   Value: '+IntToStr(Memory[MPHexEditor1.GetCursorPos + StartAddr])+ '  Word: '+IntToStr(GetWord(@Memory[MPHexEditor1.GetCursorPos + StartAddr]));
end;

procedure THexWindow.MPHexEditor1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  MPHexEditor1.ReadOnlyView := False;
  If (DataSize = 0) or (DataSize = 65536) Then
     If MPHexEditor1.GetCursorPos < 16384 Then
        MPHexEditor1.ReadOnlyView := True;
  MPHexEditor1Change(nil);
end;

procedure THexWindow.MPHexEditor1TopLeftChanged(Sender: TObject);
begin
  MPHexEditor1.ReadOnlyView := False;
  If (DataSize = 0) or (DataSize = 65536) Then
     If MPHexEditor1.GetCursorPos < 16384 Then
        MPHexEditor1.ReadOnlyView := True;
  MPHexEditor1Change(nil);
end;

procedure THexWindow.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then
     If Sender = ComboBox1 Then
        Button3Click(nil)
     Else
        Button4Click(nil);
end;

procedure THexWindow.Button3Click(Sender: TObject);
Var
  NewPos, Idx: Integer;
  Str: String;
  Bool: Boolean;
begin

  Bool := False;
  Str := Uppercase(ComboBox1.Text);
  NewPos := StrToIntDef(Str, -1);
  If Pos(':', Str) <> 0 Then
     NewPos := GetLineAddress(StrToIntDef(Copy(Str, 1, Pos(':', Str)-1), 0), StrToIntDef(Copy(Str, Pos(':', Str)+1, 9999), 0), GetWord(@Memory[PROG]));

  If VariablesWindow.ListView1.Items.Count > 0 Then Begin
     For Idx := 0 To VariablesWindow.Listview1.Items.Count -1 Do
        If Str = Uppercase(VariablesWindow.Listview1.Items[Idx].Caption) Then Begin
           NewPos := VarAddresses[Idx];
           Break;
        End;
  End;

  If Str = 'DEFADD'  Then NewPos := GetWord(@Memory[DEFADD]);
  If Str = 'STRMS'   Then NewPos := STRMS;
  If Str = 'CHARS'   Then NewPos := GetWord(@Memory[CHARS]);
  If Str = 'ERR_SP'  Then NewPos := GetWord(@Memory[ERR_SP]);
  If Str = 'LIST_SP' Then NewPos := GetWord(@Memory[LIST_SP]);
  If Str = 'VARS'    Then NewPos := GetWord(@Memory[VARS]);
  If Str = 'DEST'    Then NewPos := GetWord(@Memory[DEST]);
  If Str = 'CHANS'   Then NewPos := GetWord(@Memory[CHANS]);
  If Str = 'CURCHL'  Then NewPos := GetWord(@Memory[CURCHL]);
  If Str = 'PROG'    Then NewPos := GetWord(@Memory[PROG]);
  If Str = 'NXTLIN'  Then NewPos := GetWord(@Memory[NXTLIN]);
  If Str = 'DATADD'  Then NewPos := GetWord(@Memory[DATADD]);
  If Str = 'E_LINE'  Then NewPos := GetWord(@Memory[E_LINE]);
  If Str = 'K_CUR'   Then NewPos := GetWord(@Memory[K_CUR]);
  If Str = 'CH_ADD'  Then NewPos := GetWord(@Memory[CH_ADD]);
  If Str = 'X_PTR'   Then NewPos := GetWord(@Memory[X_PTR]);
  If Str = 'WORKSP'  Then NewPos := GetWord(@Memory[WORKSP]);
  If Str = 'STKBOT'  Then NewPos := GetWord(@Memory[STKBOT]);
  If Str = 'STKEND'  Then NewPos := GetWord(@Memory[STKEND]);
  If Str = 'MEM'     Then NewPos := GetWord(@Memory[MEM]);
  If Str = 'T_ADDR'  Then NewPos := GetWord(@Memory[T_ADDR]);
  If Str = 'UDG'     Then NewPos := GetWord(@Memory[UDG]);
  If Str = 'DF_CC'   Then NewPos := GetWord(@Memory[DF_CC]);
  If Str = 'DF_CCL'  Then NewPos := GetWord(@Memory[DF_CCL]);
  If Str = 'MEMBOT'  Then NewPos := MEMBOT;
  If Str = 'RAMTOP'  Then NewPos := GetWord(@Memory[RAMTOP]);
  If Str = 'P_RAMT'  Then NewPos := GetWord(@Memory[P_RAMT]);

  If NewPos <> -1 Then Begin
     MPHexEditor1.Seek(NewPos, 0);
     ComboBox1.Text := IntToStr(MPHexEditor1.GetTopLeftPosition(Bool));
     MPHexEditor1.SetFocus;
  End;

end;

procedure THexWindow.ComboBox1Click(Sender: TObject);
Var
  Idx: Integer;
begin
  // Fill the combobox.
  ComboBox1.Items.BeginUpdate;
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add('- System Variables');
  ComboBox1.Items.Add('DEFADD');
  ComboBox1.Items.Add('STRMS');
  ComboBox1.Items.Add('CHARS');
  ComboBox1.Items.Add('ERR_SP');
  ComboBox1.Items.Add('LIST_SP');
  ComboBox1.Items.Add('VARS');
  ComboBox1.Items.Add('DEST');
  ComboBox1.Items.Add('CHANS');
  ComboBox1.Items.Add('CURCHL');
  ComboBox1.Items.Add('PROG');
  ComboBox1.Items.Add('NXTLIN');
  ComboBox1.Items.Add('DATADD');
  ComboBox1.Items.Add('E_LINE');
  ComboBox1.Items.Add('K_CUR');
  ComboBox1.Items.Add('CH_ADD');
  ComboBox1.Items.Add('X_PTR');
  ComboBox1.Items.Add('WORKSP');
  ComboBox1.Items.Add('STKBOT');
  ComboBox1.Items.Add('STKEND');
  ComboBox1.Items.Add('MEM');
  ComboBox1.Items.Add('T_ADDR');
  ComboBox1.Items.Add('UDG');
  ComboBox1.Items.Add('DF_CC');
  ComboBox1.Items.Add('DF_CCL');
  ComboBox1.Items.Add('MEMBOT');
  ComboBox1.Items.Add('RAMTOP');
  ComboBox1.Items.Add('P_RAMT');
  If VariablesWindow.ListView1.Items.Count > 0 Then Begin
     ComboBox1.Items.Add('- Variables');
     For Idx := 0 To VariablesWindow.Listview1.Items.Count -1 Do
        ComboBox1.Items.Add(VariablesWindow.Listview1.Items[Idx].Caption);
  End;
  ComboBox1.Items.EndUpdate;
end;

procedure THexWindow.ComboBox1Change(Sender: TObject);
begin
  If ComboBox1.Items[ComboBox1.ItemIndex] = '- System Variables' Then ComboBox1.ItemIndex := 1;
  If ComboBox1.Items[ComboBox1.ItemIndex] = '- Variables' Then ComboBox1.ItemIndex := 1;
end;

procedure THexWindow.Button4Click(Sender: TObject);
Var
  FoundPos: Integer;
begin
  FoundPos := MPHexEditor1.Find(PChar(ComboBox1.Text), Length(PChar(ComboBox1.Text)), MPHexeditor1.GetCursorPos +1, DataSize, True, True);
  If FoundPos = -1 Then
     FoundPos := MPHexEditor1.Find(PChar(ComboBox1.Text), Length(PChar(ComboBox1.Text)), 0, MPHexeditor1.GetCursorPos -1, True, True);
  If FoundPos = -1 Then
     MessageBox(Handle, pChar('Could not find any more instances of'#13#39+ComboBox1.Text+#39), pChar('Find Finished'), MB_ICONINFORMATION or MB_OK)
  Else Begin
     MPHexEditor1.Seek(FoundPos, 0);
     MPHexEditor1.SetFocus;
  End;
end;

procedure THexWindow.FormClose(Sender: TObject; var Action: TCloseAction);
Var
  F: Integer;
  Changed: Boolean;
begin

  checkbox1.Checked:=false;
  timer1.Enabled:=false;
  Changed := False;

  For F := StartAddr To StartAddr + DataSize -1 Do
     If Memory[F] <> Byte(MPHexEditor1.Data[F-StartAddr]) Then
        If F > 16383 Then
           Changed := True;

  If Changed Then Begin

     // Save changes to memory?

     Case MessageDlg('You have made changes to the Memory.'+#13+'Do you want to store those changes?', mtWarning, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
           Begin
              For F := StartAddr To StartAddr + DataSize -1 Do
                 Memory[F] := Byte(MPHexEditor1.Data[F-StartAddr]);
              FastCore.UpdateDisplay;
              UpdateBASinDisplay;
           End;
        mrCancel:
           Action := caNone;
     End;

  End;

end;

procedure THexWindow.Button5Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_memory_viewer.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure THexWindow.Button6Click(Sender: TObject);
begin
  Refresh();
end;

procedure THexWindow.Refresh();
Var
  cpos,pos: Integer;
  Bool: Boolean;

begin
  pos:=MPHexEditor1.GetTopLeftPosition(Bool);
  //GetMemory(0, 65536, 'Memory Viewer');
  MPHexEditor1.SelectAll;
  MPHexEditor1.ReplaceSelection(@Memory[0], 65536);
  MPHexEditor1.ResetSelection(True);
  MPHexEditor1.SetTopLeftPosition(pos,Bool);
  //MPHexEditor1.
end;


procedure THexWindow.Timer1Timer(Sender: TObject);
begin
Refresh;
end;

procedure THexWindow.CheckBox1Click(Sender: TObject);
begin
timer1.Enabled:=CheckBox1.Checked;

end;

procedure THexWindow.Copy1Click(Sender: TObject);
var
TempStr: String;
begin
  TempStr:= MPHexEditor1.SelectionAsText;
  ClipBoard.SetTextBuf(PChar(TempStr));
end;

procedure THexWindow.CopyasHex1Click(Sender: TObject);
var
TempStr: String;
begin
    TempStr:= MPHexEditor1.SelectionAsHex;
    ClipBoard.SetTextBuf(PChar(TempStr));
end;

end.
