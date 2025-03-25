unit Basinet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus, ShellAPI;

type

    TBasinetWindow = class(TForm)
    BtnClose: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    TxtSnipName: TEdit;
    TxtSnipDesc: TEdit;
    BtnLogin: TButton;
    Memo_Snippet: TMemo;
    Memo_RawCode: TMemo;
    Panel1: TPanel;
    BtnPaste: TButton;
    BtnDelete: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Panel2: TPanel;
    BtnSave: TButton;
    BtnShare: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox2: TGroupBox;
    ListView1: TListView;
    GroupBox3: TGroupBox;
    ListView2: TListView;
    GroupBox4: TGroupBox;
    MemoDisclaimer: TMemo;
    Button1: TButton;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Button3: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox7: TGroupBox;
    Button2: TButton;
    Label6: TLabel;
    Edit3: TEdit;
    Label7: TLabel;
    Edit4: TEdit;
    Button4: TButton;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TxtSnipNameChange(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LatestVersionAvailable: String;
    AddingNewSnippet: Boolean;
    MyWorkerThread: TThread;
    Procedure ClearSnippet;
    Procedure AddingSnippet;
    Procedure AddSnippet(Code: TStringlist);
    Procedure PopulateList;
    Procedure SaveList;
    function MergeStrings(Dest, Source: TStringlist): TStringlist;
    function FindSnip(ID: String):Integer;
    Procedure DisableAll;
    Procedure RenumberSnippet;
    Procedure FetchSnippet;
    Procedure AnnounceSnippet;




  end;

var
  //idttp: TIdHTTP;
  BasinetWindow: TBasinetWindow;
  SNIPS: TStringList;
  SelectedID: String;

  const
 INET_USERAGENT    = 'Mozilla/4.0, Basinc Library (Windows; en-US)';
 INET_TIMEOUT_SECS = 3;
 INET_REDIRECT_MAX = 10;
 DEFAULT_SNIP_NAME = 'New Snippet';
implementation

uses BasinMain, Sound, Utility, LogWind, FastCore, BASSupport,AddCode;

{$R *.DFM}


Procedure TBasinetWindow.PopulateList;
Var
  F, Result: Integer;
  LI: TListItem;


Begin

  ListView1.Items.Clear;

  SNIPS := TStringlist.Create;
  If FileExists(BASinDIR+'\BasinCSnips.dat') Then
     SNIPS.LoadFromFile(BASinDIR+'\BasinCSnips.dat');

  Result := 0;
  F := 1;
  ListView1.Items.Clear;
  While (Result < SNIPS.Count-2) Do Begin
        While (Result < SNIPS.Count-2) and (SNIPS[Result] <> '[SNIP]') Do Inc(Result);
         if (Result < SNIPS.Count-2) Then Begin
             ListView1.Items.BeginUpdate;

             LI := ListView1.Items.Add;
             LI.Caption := inttostr(F);
             LI.SubItems.Add(SNIPS[Result+2]);
             LI.SubItems.Add(SNIPS[Result+3]);
             LI.SubItems.Add('You');
             LI.SubItems.Add('Local');
             LI.SubItems.Add(SNIPS[Result+1]);
             Inc(F);
             Result := Result +4;
             ListView1.Items.EndUpdate;
         End;
  End;

End;






Procedure TBasinetWindow.SaveList;
Var
  Snippet: TStringList;
  timestamp: Longint;
  j : integer;
Begin
timestamp := Round((Now() - 25569.0 {Unix start date in Delphi terms} ) * 86400);
  SNIPS := TStringlist.Create;
  If FileExists(BASinDIR+'\BasinCSnips.dat') Then
     SNIPS.LoadFromFile(BASinDIR+'\BasinCSnips.dat');

  Snippet := TStringlist.Create;
  Snippet.Add('[SNIP]');
  Snippet.Add(inttostr(10 + Random(80)) +inttostr(timestamp));
  Snippet.Add(TxtSnipName.Text);
  Snippet.Add(TxtSnipDesc.Text);
  Snippet.Add('[RESERVE]');
  Snippet.Add('ID,OWNER,STATUS');
  Snippet.Add('[CODE]');
  Snippet.Add(Memo_RawCode.Text);
  Snippet.Add('[ENDS]');

  for j := 0 to -1 + Snippet.Count do SNIPS.Add(Snippet[j]);

  SNIPS.SaveToFile(BASinDIR+'\BasinCSnips.dat');
  Snippet.Free;

  PopulateList;

End;



Procedure TBasinetWindow.ClearSnippet;
Begin
  Memo_Snippet.Lines.Clear;
  Memo_RawCode.Lines.Clear;
  GroupBox1.Caption := 'Snippet Details';
End;

Procedure TBasinetWindow.AddingSnippet;
Begin
  BtnPaste.Enabled := False;
  AddingNewSnippet := True;
  TxtSnipName.Text := DEFAULT_SNIP_NAME;
  TxtSnipDesc.Text := 'Undefined Snippet';
  GroupBox1.Caption := 'Adding New Snippet';
  GroupBox1.Tag := 0;
  BtnSave.Enabled := True;
  TxtSnipName.Enabled:=True;
  TxtSnipDesc.Enabled:=True;
  Panel2.Visible:=True;

End;


Procedure TBasinetWindow.AddSnippet(Code: TStringlist);
Var
  CurLine, Line: String;
  Idx, Idx2: Integer;
Begin
  Memo_Snippet.Clear;
  Idx := 0;
  CurLine := '';

  While Idx < Code.Count Do Begin

     Line := Code[Idx];

     // Remove leading whitespace.
     While (Line <> '') and (Line[1] <= ' ') Do
        Line := Copy(Line, 2, 999999);

     If Line[1] in ['0'..'9'] Then Begin
        // a new line.
        If CurLine <> '' Then
           Memo_Snippet.Lines.Add(CurLine);
        CurLine := Line;
     End Else Begin

        If CurLine <> '' Then Begin
           Idx2 := Length(CurLine);
           While (Idx2 > 1) and (CurLine[Idx2] <= ' ') Do
              Dec(Idx2);
           If (CurLine[Idx2] in ['\', ':']) or (Lowercase((Copy(CurLine, Idx2-3, 4))) = 'then') Then
              CurLine := CurLine + Line
           Else Begin
              Memo_Snippet.Lines.Add(CurLine);
              CurLine := Line;
           End;
        End Else
           CurLine := Line;

     End;

     Inc(Idx);

  End;

  If CurLine <> '' Then
     Memo_Snippet.Lines.Add(CurLine);

End;



procedure TBasinetWindow.FormCreate(Sender: TObject);
begin
Panel2.Top:=Panel1.Top;
Panel2.Left:=Panel1.Left;
Panel2.Width:=Panel1.Width;
Panel2.Height:=Panel1.Height;
if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;

DisableAll;



PopulateList;
if Visible Then TxtSnipName.SetFocus;
SelectedID := '0';
end;

Procedure TBasinetWindow.DisableAll;
Begin
CheckBox1.Enabled:=False;
CheckBox2.Enabled:=False;
BtnPaste.Enabled:=False;
BtnDelete.Enabled:=False;
BtnSave.Enabled:=False;
//BtnShare.Enabled:=False;
Edit1.Enabled:=False;
Edit2.Enabled:=False;
Panel2.Visible:=False;
TxtSnipName.Enabled:=False;
TxtSnipDesc.Enabled:=False;

End;



procedure TBasinetWindow.BtnCloseClick(Sender: TObject);
begin
   Close;
end;



procedure TBasinetWindow.BtnSaveClick(Sender: TObject);
begin
If TxtSnipName.Text<>DEFAULT_SNIP_NAME Then Begin
SaveList;
End Else Begin
TxtSnipName.Setfocus;
End;


end;

procedure TBasinetWindow.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
Begin
   If ListView1.Selected <> nil Then Begin
        FetchSnippet;
   End;
End;



Procedure TBasinetWindow.AnnounceSnippet;
Begin

End;

Procedure TBasinetWindow.FetchSnippet;
var
Idx: Integer;
TempStr: String;
NewLines: TStringList;
BASIC: String;
begin
        If ListView1.Selected <> nil Then Begin
                 TxtSnipName.Text := ListView1.Selected.SubItems[0];
                 TxtSnipDesc.Text := ListView1.Selected.SubItems[1];
                 SelectedID := ListView1.Selected.SubItems[4];
                 BtnDelete.Enabled := True;

                 Idx:=FindSnip(SelectedID);
                 If (Idx<>-1) Then Begin
                        //Found the entry
                        While(True) Do Begin
                                if (SNIPS[Idx]='[CODE]') Then Begin
                                        Inc(Idx);
                                        TempStr := FormatEscapes(SNIPS[Idx]);
                                        NewLines := TStringlist.Create;
	                                Repeat
	                                	BASIC := Copy(TempStr, 1, Pos(#13, TempStr)-1);
	                                	TempStr :=  Copy(TempStr, Pos(#13, TempStr)+1, 999999);
	                                	If BASIC <> '' Then
	                                	   NewLines.Add(InsertEscapes(BASIC));
	                                Until Pos(#13, TempStr) = 0;

	                                If TempStr <> '' Then
		                                NewLines.Add(TempStr);

                                                AddingNewSnippet := False;
                                                AddSnippet(NewLines);
	                                        NewLines.Free;

                                                DisableAll;
                                                GroupBox1.Caption := 'Snippet Review';
                                                CheckBox1.Enabled:=True;
 						CheckBox2.Enabled:=True;
						BtnPaste.Enabled:=True;
						BtnDelete.Enabled:=True;
						Edit1.Enabled:=True;
						Edit2.Enabled:=True;
                                                TxtSnipName.Enabled:=True;
                                                TxtSnipDesc.Enabled:=True;

                                                If (CheckBox2.Checked) Then RenumberSnippet;
                                                Break;
                                        End Else Begin
                                                Inc(Idx);
                                        End;
                         End;

                End;
                Memo_Snippet.SelStart :=0;
                Memo_Snippet.SelLength :=1;

        End Else Begin
                 DisableAll;
                 SelectedID := '0';
        End;
End;



procedure TBasinetWindow.TxtSnipNameChange(Sender: TObject);
begin
//DisableAll;
//Panel2.Visible:=True;
//BtnSave.Enabled := True;
end;

function TBasinetWindow.MergeStrings(Dest,
  Source: TStringlist): TStringlist;
var j : integer;
begin
   for j := 0 to -1 + Source.Count do
     if Dest.IndexOf(Source[j]) = -1 then
       Dest.Add(Source[j]) ;

   Result := Dest;
end;

procedure TBasinetWindow.BtnDeleteClick(Sender: TObject);
var
 Idx: Integer;
begin
If (SelectedID<>'0') Then Begin
        Idx:=FindSnip(SelectedID);
        If (Idx<>-1) Then Begin
          //Found the entry
          While(True) Do Begin
                if (SNIPS[Idx]<>'[ENDS]') Then Begin SNIPS.Delete(Idx); End Else Begin SNIPS.Delete(Idx); Break; End;
          End;
            SNIPS.SaveToFile(BASinDIR+'\BasinCSnips.dat');
            DisableAll;
            PopulateList;
        End;
End;

end;

function TBasinetWindow.FindSnip(ID: String): Integer;
var
LastSnip: Integer;
Begin
     Result:=0;
     LastSnip:=-1;
     While (Result < SNIPS.Count-2) Do Begin
        If (SNIPS[Result]='[SNIP]') Then Begin
            LastSnip:=Result;
        End Else Begin
            If (SNIPS[Result]=ID) Then Begin
                Result:= LastSnip;
                Exit;
            End;
        End;
        Inc(Result);
     End;
     Result:=-1;

end;

Procedure TBasinetWindow.RenumberSnippet;
var
LineIndex,Step,Idx,CurrentLineNO, CropStart,CropEnd,j,KeyLen : Integer;
Line, Keyword: String;
LineBuffer: Array[0..9999] of Integer;
const
Keywords: Array[0..3] of String = ('GO TO ','GO SUB ','RESTORE ','RUN ');
begin
Edit1.Color:= clWindow;
Idx:=0;
LineIndex := Strtoint(Edit1.Text);
Step :=  Strtoint(Edit2.Text);

//PASS 1 Simple Renumber
While Idx < Memo_Snippet.Lines.Count Do Begin
    Line := Memo_Snippet.Lines[Idx];
    While (Line <> '') and (Line[1] <= ' ') Do
        Line := Copy(Line, 2, 999999);
    CurrentLineNO := strtoint(Copy(Line, 1, Pos(' ', Line)-1));
    LineBuffer[CurrentLineNo]:=LineIndex;
    Line := Inttostr(LineIndex)+' '+ Copy(Line, Pos(' ', Line)+1, 999999);
    Memo_Snippet.Lines[Idx]:=Line;
    LineIndex:=LineIndex+Step;
    Inc(Idx);
End;

//PASS 2 GOTO & GO SUB & RESTORE & RUN Renumber
For j:=0 To 3 Do Begin
Keyword:=Keywords[j];
KeyLen:=Length(Keyword);
Idx:=0;
While Idx < Memo_Snippet.Lines.Count Do Begin
   Line := Memo_Snippet.Lines[Idx];

   CropEnd := 1;
   While (CropEnd<length(Line)) Do Begin
     CropStart :=  Pos(Keyword, Copy(Line, CropEnd, 999999));
     if CropStart>0 Then Begin
          CropStart := CropStart + KeyLen;
          CropEnd := (CropEnd-1) + CropStart ;
          While (Line[CropEnd] in ['0'..'9']) Do Inc(CropEnd);
          
          if CropEnd>CropStart Then Begin
             //We have some numbers

             CurrentLineNO := StrToInt(Copy(Line, CropStart, CropEnd-CropStart));
             if (CurrentLineNO>0) Then Begin
               if (LineBuffer[CurrentLineNO]>0) Then Begin
                   Memo_Snippet.Lines[Idx]:= Copy(Line, 1, CropStart-1) + IntToStr(LineBuffer[CurrentLineNO]) + Copy(Line, CropEnd, 999999);
               End Else Begin
                   While ((CurrentLineNO<10000) And (LineBuffer[CurrentLineNO]=0)) Do Inc(CurrentLineNO);
                   if (CurrentLineNO=9999) And (LineBuffer[CurrentLineNO]=0) Then Begin
                      Edit1.Color:= clYellow;
                   End Else Begin
                      Memo_Snippet.Lines[Idx]:= Copy(Line, 1, CropStart-1) + IntToStr(LineBuffer[CurrentLineNO]) + Copy(Line, CropEnd, 999999);
                   End;
               End;
             End;
          End;
      End;
      Break;
   End;
   Inc(Idx);

End; //While
End; //For

end;

procedure TBasinetWindow.BtnPasteClick(Sender: TObject);
Var
Result: Boolean;
begin
If BASinOutput.Running Then Begin
Result:=MessageDlg('Cannot insert code while emulation is running!'+#13+#13+'Stop the running program first.', mtWarning, [mbOK], 0) = mrOK;

Exit;
End;
AddCodeWindow.Memo1.Lines := Memo_Snippet.Lines;
AddCodeWindow.PasteLines;

end;

procedure TBasinetWindow.CheckBox2Click(Sender: TObject);
begin
Edit1.Color:= clWindow;
FetchSnippet;
end;

procedure TBasinetWindow.Edit2Change(Sender: TObject);
begin
 //CheckBox1.Checked:=False;
 CheckBox2.Checked:=False;
 if (Edit2.Text='id') Then BasinetWindow.GroupBox2.Caption:=SessionID;


end;

procedure TBasinetWindow.Button1Click(Sender: TObject);
begin
    GroupBox4.Visible:=False;
end;

procedure TBasinetWindow.Button4Click(Sender: TObject);
begin
  GroupBox6.Visible:=True;
end;

procedure TBasinetWindow.FormShow(Sender: TObject);
begin
if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;
if Opt_ToolFontSize>0 Then Memo_Snippet.Font.Size:=Opt_ToolFontSize;
Edit3.Text:= SessionID;
end;

end.

