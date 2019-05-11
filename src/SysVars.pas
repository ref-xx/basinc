
unit SysVars;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus;

type
  TSysVar = Record Bytes: Byte; Name: String; Address: Word; Desc: String; End;

  TSysVarsWindow = class(TForm)
    Panel1: TPanel;
    ListView1: TListView;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    EditthisSysVar1: TMenuItem;
    RefreshtheList1: TMenuItem;
    WatchthisSysVar1: TMenuItem;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListView1InfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
    procedure Button1Click(Sender: TObject);
    procedure EditthisSysVar1Click(Sender: TObject);
    procedure RefreshtheList1Click(Sender: TObject);
    procedure WatchthisSysVar1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CanUpdate: Boolean;
    Procedure Populate;
    procedure UpdateSysVars(UpdateAddr: Word);
    Function  GetSysVar(Index: Integer): String;
  end;

var
  SysVarsWindow: TSysVarsWindow;

Const

  SystemVariables: Array[0..70] of TSysVar =
   ((Bytes:8;  Name:'KSTATE';   Address:23552;	Desc:'Used in reading the keyboard.'),
	  (Bytes:1;  Name:'LAST_K';   Address:23560;	Desc:'Stores newly pressed key.'),
    (Bytes:1;  Name:'REPDEL';   Address:23561;	Desc:'Time (in 50ths of a second, 60ths of a second in N. America) that a key must be held down before it repeats. This starts off at 35, but you can POKE in other values.'),
	  (Bytes:1;  Name:'REPPER';   Address:23562;	Desc:'Delay (in 50ths of a second in 60ths of a second in N. America) between successive repeats of a key held down; initially 5.'),
	  (Bytes:2;  Name:'DEFADD';   Address:23563;	Desc:'Address of arguments of user defined function if one is being evaluated; otherwise 0.'),
	  (Bytes:1;  Name:'K_DATA';   Address:23565;	Desc:'Stores 2nd byte of colour controls entered from the keyboard.'),
	  (Bytes:2;  Name:'TVDATA';   Address:23566;	Desc:'Stores bytes of colour, AT and TAB controls going to television.'),
	  (Bytes:38; Name:'STRMS';    Address:23568;	Desc:'Addresses of channels attached to streams.'),
	  (Bytes:2;  Name:'CHARS';    Address:23606;	Desc:'256 less than address of character set (which starts with space and carries on to the copyright symbol). Normally in ROM, but you can set up your own in RAM and make CHARS point to it.'),
	  (Bytes:1;  Name:'RASP';     Address:23608;	Desc:'Length of warning buzz.'),
	  (Bytes:1;  Name:'PIP';      Address:23609;	Desc:'Length of keyboard click.'),
	  (Bytes:1;  Name:'ERR_NR';   Address:23610;	Desc:'1 less than the report code. Starts off at 255 (for 1) so PEEK 23610 gives 255.'),
	  (Bytes:1;  Name:'FLAGS';    Address:23611;	Desc:'Various flags to control the BASIC system.'),
	  (Bytes:1;  Name:'TV_FLAG';  Address:23612;	Desc:'Flags associated with the television.'),
	  (Bytes:2;  Name:'ERR_SP';   Address:23613;	Desc:'Address of item on machine stack to be used as error return.'),
	  (Bytes:2;  Name:'LIST_SP';  Address:23615;	Desc:'Address of return address from automatic listing.'),
	  (Bytes:1;  Name:'MODE';     Address:23617;	Desc:'Specifies K, L, C. E or G cursor.'),
	  (Bytes:2;  Name:'NEWPPC';   Address:23618;	Desc:'Line to be jumped to.'),
	  (Bytes:1;  Name:'NSPPC';    Address:23620;	Desc:'Statement number in line to be jumped to. Poking first NEWPPC and then NSPPC forces a jump to a specified statement in a line.'),
	  (Bytes:2;  Name:'PPC';      Address:23621;	Desc:'Line number of statement currently being executed.'),
	  (Bytes:1;  Name:'SUBPPC';   Address:23623;	Desc:'Number within line of statement being executed.'),
	  (Bytes:1;  Name:'BORDCR';   Address:23624;	Desc:'Border colour * 8; also contains the attributes normally used for the lower half of the screen.'),
	  (Bytes:2;  Name:'E_PPC';    Address:23625;	Desc:'Number of current line (with program cursor).'),
	  (Bytes:2;  Name:'VARS';     Address:23627;	Desc:'Address of variables.'),
	  (Bytes:2;  Name:'DEST';     Address:23629;	Desc:'Address of variable in assignment.'),
	  (Bytes:2;  Name:'CHANS';    Address:23631;	Desc:'Address of channel data.'),
	  (Bytes:2;  Name:'CURCHL';   Address:23633;	Desc:'Address of information currently being used for input and output.'),
	  (Bytes:2;  Name:'PROG';     Address:23635;	Desc:'Address of BASIC program.'),
	  (Bytes:2;  Name:'NXTLIN';   Address:23637;	Desc:'Address of next line in program.'),
	  (Bytes:2;  Name:'DATADD';   Address:23639;	Desc:'Address of terminator of last DATA item.'),
	  (Bytes:2;  Name:'E_LINE';   Address:23641;	Desc:'Address of command being typed in.'),
	  (Bytes:2;  Name:'K_CUR';    Address:23643;	Desc:'Address of cursor.'),
	  (Bytes:2;  Name:'CH_ADD';   Address:23645;	Desc:'Address of the next character to be interpreted; the character after the argument of PEEK, or the NEWLINE at the end of a POKE statement.'),
	  (Bytes:2;  Name:'X_PTR';    Address:23647;	Desc:'Address of the character after the ? marker.'),
	  (Bytes:2;  Name:'WORKSP';   Address:23649;	Desc:'Address of temporary work space.'),
	  (Bytes:2;  Name:'STKBOT';   Address:23651;	Desc:'Address of bottom of calculator stack.'),
	  (Bytes:2;  Name:'STKEND';   Address:23653;	Desc:'Address of start of spare space.'),
	  (Bytes:1;  Name:'BREG';     Address:23655;	Desc:'Calculator'#39's b register.'),
	  (Bytes:2;  Name:'MEM';      Address:23656;	Desc:'Address of area used for calculator'#39's memory. (Usually MEMBOT, but not always.)'),
	  (Bytes:1;  Name:'FLAGS2';   Address:23658;	Desc:'More flags.'),
	  (Bytes:1;  Name:'DF_SZ';    Address:23659;	Desc:'The number of lines (including one blank line) in the lower part of the screen.'),
	  (Bytes:2;  Name:'S_TOP';    Address:23660;	Desc:'The number of the top program line in automatic listings.'),
 	  (Bytes:2;  Name:'OLDPPC';   Address:23662;	Desc:'Line number to which CONTINUE jumps.'),
 	  (Bytes:1;  Name:'OSPCC';    Address:23664;	Desc:'Number within line of statement to which CONTINUE jumps.'),
 	  (Bytes:1;  Name:'FLAGX';    Address:23665;	Desc:'Various flags.'),
	  (Bytes:2;  Name:'STRLEN';   Address:23666;	Desc:'Length of string type destination in assignment.'),
	  (Bytes:2;  Name:'T_ADDR';   Address:23668;	Desc:'Address of next item in syntax table (very unlikely to be useful).'),
	  (Bytes:2;  Name:'SEED';     Address:23670;	Desc:'The seed for RND. This is the variable that is set by RANDOMIZE.'),
    (Bytes:3;  Name:'FRAMES';   Address:23672;	Desc:'3 byte (least significant first), frame counter. Incremented every 20ms.'),
	  (Bytes:2;  Name:'UDG';      Address:23675;	Desc:'Address of 1st user defined graphic You can change this for instance to save space by having fewer user defined graphics.'),
	  (Bytes:1;  Name:'COORDSX';  Address:23677;	Desc:'x-coordinate of last point plotted.'),
	  (Bytes:1;  Name:'COORDSY';  Address:23678;	Desc:'y-coordinate of last point plotted.'),
	  (Bytes:1;  Name:'P_POSN';   Address:23679;	Desc:'33 column number of printer position'),
	  (Bytes:1;  Name:'PR_CC';    Address:23680; Desc:'Less significant byte of address of next position for LPRINT to print at (in printer buffer).'),
	  (Bytes:1;  Name:'UNUSED1';  Address:23681;	Desc:'Not used.'),
	  (Bytes:2;  Name:'ECHO_E';   Address:23682;	Desc:'33 column number and 24 line number (in lower half) of end of input buffer.'),
	  (Bytes:2;  Name:'DF_CC';    Address:23684;	Desc:'Address in display file of PRINT position.'),
	  (Bytes:2;  Name:'DF_CCL';   Address:23686;	Desc:'Like DF CC for lower part of screen.'),
	  (Bytes:1;  Name:'S_POSN';   Address:23688;	Desc:'33 column number for PRINT position'),
	  (Bytes:1;  Name:'PRPOSN';   Address:23689;	Desc:'24 line number for PRINT position.'),
	  (Bytes:2;  Name:'SPOSNL';   Address:23690;	Desc:'Like S POSN for lower part'),
	  (Bytes:1;  Name:'SCR_CT';   Address:23692;	Desc:'Counts scrolls: it is always 1 more than the number of scrolls that will be done before stopping with scroll? If you keep poking this with a number bigger than 1 (say 255), the screen will scroll on and on without asking you.'),
	  (Bytes:1;  Name:'ATTR_P';   Address:23693;	Desc:'Permanent current colours, etc (as set up by colour statements).'),
	  (Bytes:1;  Name:'MASP_P';   Address:23694;	Desc:'Used for transparent colours, etc. Any bit that is 1 shows that the corresponding attribute bit is taken not from ATTR P, but from what is already on the screen.'),
	  (Bytes:1;  Name:'ATTR_T';   Address:23695;	Desc:'Temporary current colours, etc (as set up by colour items).'),
	  (Bytes:1;  Name:'MASK_T';   Address:23696;	Desc:'Like MASK P, but temporary.'),
	  (Bytes:1;  Name:'P_FLAG';   Address:23697;	Desc:'More flags.'),
	  (Bytes:30; Name:'MEMBOT';   Address:23698;	Desc:'Calculator'#39's memory area; used to store numbers that cannot conveniently be put on the calculator stack.'),
	  (Bytes:2;  Name:'UNUSED2';  Address:23728;	Desc:'Not used.'),
	  (Bytes:2;  Name:'RAMTOP';   Address:23730;	Desc:'Address of last byte of BASIC system area.'),
	  (Bytes:2;  Name:'P_RAMT';   Address:23732;	Desc:'Address of last byte of physical RAM.'));

implementation

{$R *.DFM}

Uses FastCore, QueryForm, Evaluate, HexEdit, Watches, Utility, ROMUtils, BASinMain;

procedure TSysVarsWindow.FormCreate(Sender: TObject);
begin
  Panel1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - 26 - Button1.Height);
  ListView1.SetBounds(0, 0, Panel1.Width, Panel1.Height);
  Button1.SetBounds(ClientWidth - Button1.Width - 8, ClientHeight - 8 - Button1.Height, Button1.Width, Button1.Height);
  Button2.SetBounds(8 + ComboBox1.Width +4, ClientHeight - 8 - Button2.Height, Button2.Width, Button2.Height);
  ComboBox1.SetBounds(8, Button2.Top, ComboBox1.Width, ComboBox1.Height);
  Button3.SetBounds(Button1.Left - button3.Width - 4, Button1.Top, Button3.Width, Button1.Height);
  ComboBox1.ItemIndex := 1;
  Timer1.Enabled := False;
  CanUpdate := True;
  ListView1.DoubleBuffered := True;
end;

Procedure TSysVarsWindow.Populate;
Var
  F: Integer;
  LI: TListItem;
Begin
  ListView1.Items.BeginUpdate;
  F := 0;
  While F < 71 Do Begin
     LI := ListView1.Items.Add;
     LI.Caption := SystemVariables[F].Name;
     LI.SubItems.Add(IntToStr(SystemVariables[F].Address)+' ['+IntToStr(SystemVariables[F].Bytes)+']');
     LI.SubItems.Add('');
     Inc(F);
  End;
  ListView1.Items.EndUpdate;
End;

Procedure TSysVarsWindow.UpdateSysVars(UpdateAddr: Word);
Var
  F: Integer;
Begin
  If ListView1.Items.Count = 0 Then Populate;
  If UpdateAddr = 0 Then Begin
     F := 0;
     ListView1.Items.BeginUpdate;
     While F < 71 Do Begin
        ListView1.Items[F].SubItems[1] := GetSysVar(F);
        Inc(F);
     End;
     ListView1.Items.EndUpdate;
  End Else Begin
     // Update an individual Sysvar.
  End;
End;

Function  TSysVarsWindow.GetSysVar(Index: Integer): String;
Var
  G, H: Integer;
Begin
  Case SystemVariables[Index].Bytes of
     1: Begin // Single Byte
           Result := IntToStr(Memory[SystemVariables[Index].Address]);
        End;
     2: Begin // Word value (probably an address)
           Result := IntToStr(GetWord(@Memory[SystemVariables[Index].Address]))+' ('+IntToStr(Memory[SystemVariables[Index].Address])+','+IntToStr(Memory[SystemVariables[Index].Address+1])+')';
        End;
     3: Begin // FRAMES
           Result := IntToStr(GetDWord(@Memory[SystemVariables[Index].Address]) And $FFFFFF);
        End;
    38: Begin // STREAMS - Special as it's words.
           Result := '';
           H := SystemVariables[Index].Address;
           For G := 1 To SystemVariables[Index].Bytes Div 2 Do Begin
              If G < SystemVariables[Index].Bytes Div 2 Then
                 Result := Result + IntToStr(GetWord(@Memory[H])) + ', '
              Else
                 Result := Result + IntToStr(GetWord(@Memory[H]));
              Inc(H, 2);
           End;
        End;
     Else
        // All the rest :-)
        Begin
           Result := '';
           For G := 1 to SystemVariables[Index].Bytes Do
              If G < SystemVariables[Index].Bytes Then
                 Result := Result + IntToStr(Memory[SystemVariables[Index].Address + G -1]) + ', '
              Else
                 Result := Result + IntToStr(Memory[SystemVariables[Index].Address + G -1]);
        End;
  End;
End;

procedure TSysVarsWindow.FormShow(Sender: TObject);
begin
  UpdateSysVars(0);
end;

procedure TSysVarsWindow.Timer1Timer(Sender: TObject);
begin
  UpdateSysVars(0);
end;

procedure TSysVarsWindow.ComboBox1Change(Sender: TObject);
begin
  Case ComboBox1.ItemIndex of
     0: Begin
           Timer1.Enabled := False;
           CanUpdate := False;
        End;
     1: Begin
           Timer1.Enabled := False;
           CanUpdate := True;
        End;
     2: Begin
           Timer1.Enabled := True;
           Timer1.Interval := 1000;
           CanUpdate := True;
        End;
     3: Begin
           Timer1.Enabled := True;
           Timer1.Interval := 5000;
           CanUpdate := True;
        End;
     4: Begin
           Timer1.Enabled := True;
           Timer1.Interval := 10000;
           CanUpdate := True;
        End;
     5: Begin
           Timer1.Enabled := True;
           Timer1.Interval := 30000;
           CanUpdate := True;
        End;
     6: Begin
           Timer1.Enabled := True;
           Timer1.Interval := 60000;
           CanUpdate := True;
        End;
  End;
end;

procedure TSysVarsWindow.ListView1InfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
begin
  InfoTip := Item.Caption + ': '+SystemVariables[Item.Index].Desc;
end;

procedure TSysVarsWindow.Button1Click(Sender: TObject);
begin
  Close;
End;

procedure TSysVarsWindow.EditthisSysVar1Click(Sender: TObject);
Var
  F: Integer;
  LastInput, InputCaption: String;
  InputMin, InputMax: DWord;
  Expr: TExpression;
  MousePos: TPoint;
Label
  GetInputNum;
begin
  If ListView1.Selected <> nil Then Begin
     F := ListView1.Selected.Index;
     If SystemVariables[F].Bytes in [1, 2, 3] Then Begin
        Case SystemVariables[F].Bytes of
           1: Begin // Single Byte
                 LastInput := IntToStr(Memory[SystemVariables[F].Address]);
                 InputCaption := SystemVariables[F].Name;
                 InputMin := 0;
                 InputMax := 255;
              End;
           2: Begin // Word Value
                 LastInput := IntToStr(GetWord(@Memory[SystemVariables[F].Address]));
                 InputCaption := SystemVariables[F].Name;
                 InputMin := 0;
                 InputMax := 65535;
              End;
           3: Begin // 3Byte (FRAMES)
                 LastInput := IntToStr(GetDWord(@Memory[SystemVariables[F].Address]) And $FFFFFF);
                 InputCaption := SystemVariables[F].Name;
                 InputMin := 0;
                 InputMax := $FFFFFF;
              End;
        End;
        GetInputNum:
        QueryWindow.GetQuery(InputCaption, 'New value:', 'Okay', 'Cancel', [LastInput]);
        LastInput := QueryWindow.ResultText;
        If QueryWindow.ResultText <> '' Then Begin
           Expr.Expression := QueryWindow.ResultText;
           Expr.SyntaxChecked := False;
           EvaluateExpr(Expr);
           If Expr.ResultType = 1 Then Begin
              // Check the result
              If (Expr.ResultNum > InputMax) or (Expr.ResultNum < InputMin) Then Begin
                 MessageBox(Handle, pChar('Value Must be '+IntToStr(InputMin)+' to '+IntToStr(InputMax)), pChar('Range error'), MB_OK or MB_ICONWARNING);
                 Goto GetInputNum;
              End;
              // Now write the result in.
              Case SystemVariables[F].Bytes of
                 1: Begin // Single Byte
                       Memory[SystemVariables[F].Address] := Byte(Round(Expr.ResultNum));
                    End;
                 2: Begin // Word Value
                       PutWord(@Memory[SystemVariables[F].Address], Word(Round(Expr.ResultNum)));
                    End;
                 3: Begin // 3-Byte (FRAMES)
                       PutDWord(@Memory[SystemVariables[F].Address], (DWord(Round(Expr.ResultNum)) And $FFFFFF)+(Memory[SystemVariables[F].Address+3] Shl 24));
                    End;
              End;
              UpdateSysVars(0);
           End Else Begin
              If Expr.ResultType = 0 Then
                 MessageBox(Handle, pChar('Numeric value required'), pChar('Type error'), MB_OK or MB_ICONWARNING)
              Else
                 MessageBox(Handle, pChar(Copy(Expr.ResultStr, 3, 9999)), pChar('Input Error'), MB_OK or MB_ICONWARNING);
              Goto GetInputNum;
           End;
        End;
     End Else Begin
        // Complex multibyte types
        Windows.GetCursorPos(MousePos);
        CentreForm(HexWindow, MousePos.X, MousePos.Y);
        HexWindow.GetMemory(SystemVariables[F].Address, SystemVariables[F].Bytes, SystemVariables[F].Name);
        ShowWindow(HexWindow, True);
        UpdateSysVars(0);
     End;
  End;
  UpdateWatches;
end;

procedure TSysVarsWindow.RefreshtheList1Click(Sender: TObject);
begin
  UpdateSysVars(0);
end;

procedure TSysVarsWindow.WatchthisSysVar1Click(Sender: TObject);
begin
  If ListView1.Selected.Index <> -1 Then Begin
     WatchWindow.CreateWatch(True, 2, 0, ListView1.Selected.Index, 0, '', '', False);
     WatchWindow.BuildWatchList;
  End;
end;

procedure TSysVarsWindow.Button3Click(Sender: TObject);
begin
  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_system_variables.html'), HH_DISPLAY_TOPIC, 0);
end;

end.
