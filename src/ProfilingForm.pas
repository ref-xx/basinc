unit ProfilingForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TProfileForm = class(TForm)
    Button1: TButton;
    ListView1: TListView;
    CheckBox1: TCheckBox;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button3: TButton;
    Button4: TButton;
    Timer1: TTimer;
    Button5: TButton;
    procedure Button2Click(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SelectedList: TStringlist;
    ShowingSelected: Boolean;
    Procedure BuildProfileTree;
  end;

var
  ProfileForm: TProfileForm;

implementation

{$R *.dfm}

Uses Profiling, BASinMain, FastCore, Utility;

procedure TProfileForm.FormShow(Sender: TObject);
begin
  if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;
  SelectedList.Clear;
  ShowingSelected := False;
  BuildProfileTree;

end;

Procedure TProfileForm.BuildProfileTree;
Var
  Str: AnsiString;
  NewItem: TListItem;
  Frames, Secs: Extended;
  TotalTStates, TStates: Int64;
  Idx, LineNum, StatementNum: Integer;
Begin

  if ProfilingEnabled then Begin
        ProfileForm.Caption:='Profile Results - (Profiling Enabled)';
        button4.caption:='Stop';
  end Else begin
        ProfileForm.Caption:='Profile Results - (Profiling Disabled)';
        button4.caption:='New';
  end;

  // Generate profiling information from the current results.

  ListView1.Items.BeginUpdate;
  ListView1.Items.Clear;

  If (SelectedList.Count = 0) or (Not ShowingSelected) Then Begin
     SelectedList.Clear;
     SelectedList.AddStrings(ProfileBASICList);
     Button2.Caption := 'Show Selected';
     Button2.Enabled := False;
  End Else Begin
     Button2.Caption := 'Show All';
     Button2.Enabled := True;
  End;

  If SelectedList.Count > 0 Then Begin

     TotalTStates := 0;
     For Idx := 0 To SelectedList.Count -1 Do Begin

        Str := SelectedList[Idx];
        LineNum := StrToInt(Copy(Str, 1, Pos(':', Str)-1));
        Str := Copy(Str, Pos(':', Str)+1, 99999);
        StatementNum := StrToInt(Copy(Str, 1, Pos(#255, Str)-1));
        Str := Copy(Str, Pos(#255, Str)+1, 99999);

        If CheckBox1.Checked Then Begin
           If TotalTStates < ProfileArray[(LineNum*255)+StatementNum-1] Then
              TotalTStates := ProfileArray[(LineNum*255)+StatementNum-1];
        End Else
           Inc(TotalTStates, ProfileArray[(LineNum*255)+StatementNum-1]);
     End;

     For Idx := 0 To SelectedList.Count -1 Do Begin

        Str := SelectedList[Idx];
        LineNum := StrToInt(Copy(Str, 1, Pos(':', Str)-1));
        Str := Copy(Str, Pos(':', Str)+1, 99999);
        StatementNum := StrToInt(Copy(Str, 1, Pos(#255, Str)-1));
        Str := Copy(Str, Pos(#255, Str)+1, 99999);

        TStates := Round((ProfileArray[(LineNum*255)+StatementNum -1]/TotalTStates)*100);
        If TStates = 0 Then
           TStates := 1;

        NewItem := ListView1.Items.Add;
        NewItem.SubItems.Add(IntToStr(LineNum)+':'+IntToStr(StatementNum));
        NewItem.SubItems.Add(Str);
        If ProfileArray[(LineNum*255)+(StatementNum -1)] = 0 Then
           NewItem.SubItems.Add('')
        Else
           NewItem.SubItems.Add(IntToStr(TStates));

     End;

  End;

  Label1.Caption := 'Project "' + CurProjectFilename + '"';

  Frames := TotalTStates/Opt_CPUSpeed;
  Secs := Frames/50;
  Str := '';
  If TotalTStates > 0 Then Begin
     If Secs >= 1 Then
        Str := Str + FloatToStrF(Secs, ffFixed, 18, 1) + ' Seconds (';
     If Frames >= 1 Then
        Str := Str + FloatToStrF(Frames, ffFixed, 18, 1)+ ' Frames'
     Else
        Str := Str + IntToStr(TotalTStates)+' Ts';
     If Secs >= 1 Then
        Str := Str + ')';
  End Else
     Str := 'No timing info available';

  If CheckBox1.Checked Then
     Label2.Caption := 'Max Time: ' + Str
  Else
     Label2.Caption := 'Total Time: ' + Str;
  Label3.Caption := '';

  ListView1.Items.EndUpdate;

End;

procedure TProfileForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TProfileForm.FormCreate(Sender: TObject);
begin
  if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;
  Button1.SetBounds(ClientWidth - Button1.Width - 8, ClientHeight - Button1.Height - 8, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - Button2.Width - Button3.Width - 12, Button1.Top, Button2.Width, Button2.Height);
  Button3.SetBounds(Button1.Left - Button3.Width - 4, Button1.Top, Button3.Width, Button1.Height);
  Button5.SetBounds(Button1.Left - Button2.Width - Button3.Width - Button5.Width - 16, Button1.Top, Button5.Width, Button1.Height);
  Button4.SetBounds(Button1.Left - Button2.Width - Button3.Width - Button5.Width - Button4.Width - 20, Button1.Top, Button4.Width, Button1.Height);



  Label1.SetBounds(8, 8, ClientWidth - 16, Label1.Height);
  Label2.SetBounds(8, Label1.Top + Label1.Height + 8, ClientWidth - 24, Label2.Height);
  Label3.SetBounds(8, Label2.Top + Label2.Height + 8, ClientWidth - 24, Label3.Height);
  ListView1.SetBounds(8, Label3.Top + Label3.Height + 8, ClientWidth - 16, ClientHeight - 16 - Button1.Height - Label1.Height - Label2.Height - Label3.height - 32);
  CheckBox1.SetBounds(8, Button1.Top + 4, CheckBox1.Width, CheckBox1.Height);
  SelectedList := TStringlist.Create;

   Constraints.MinWidth:= Button1.Width + Button2.Width + Button3.Width + Button5.Width + Button4.Width + 30 + CheckBox1.Width;
   Constraints.MinHeight:= ListView1.Top * 3;
end;

procedure TProfileForm.FormResize(Sender: TObject);
begin

  // Stretch the list headers

  ListView1.Columns[2].Width := ListView1.ClientWidth - (ListView1.Columns[0].Width + ListView1.Columns[1].Width);
  Label2.SetBounds(8, Label1.Top + Label1.Height + 8, ClientWidth - 24, Label2.Height);
  Label3.SetBounds(8, Label2.Top + Label2.Height + 8, ClientWidth - 24, Label3.Height);

end;

procedure TProfileForm.ListView1AdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
Var
  ItemRect: TRect;

begin

  ListView1.Canvas.Pen.Color := ClBlack;
  ListView1.Canvas.Pen.Style := PsClear;
  ListView1.Canvas.Brush.Style := BsSolid;


  If Item.Selected Then
     ListView1.Canvas.Brush.Color := ClWhite
  Else
     ListView1.Canvas.Brush.Color := ClNavy;

  If Item.SubItems[2] = '' Then
     ListView1.Canvas.Font.Color := ClGrayText
  Else Begin
     ItemRect := Item.DisplayRect(drLabel);
     InflateRect(ItemRect, -1, -1);
     ItemRect.Right := ItemRect.Left + StrToIntDef(Item.SubItems[2], 0) +1;
     ItemRect.Bottom := ItemRect.Bottom + 1;
     ListView1.Canvas.Rectangle(ItemRect);
     ListView1.Canvas.Font.Color := ClWindowText;
  End;
  ListView1.Canvas.Pen.Style := PsSolid;
  ListView1.Canvas.Brush.Style := BsSolid;
  ListView1.Canvas.Brush.Color := ClWindow;



end;

procedure TProfileForm.CheckBox1Click(Sender: TObject);
begin

  BuildProfileTree;

end;

procedure TProfileForm.FormDestroy(Sender: TObject);
begin
  SelectedList.Free;
end;

procedure TProfileForm.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
Var
  Str: AnsiString;
  TotalTStates: Int64;
  Secs, Frames: Extended;
  Idx, LineNum, StatementNum: Integer;
begin
  If (ListView1.SelCount = 0) and (Button2.Caption = 'Show Selected') Then
     Button2.Enabled := False
  Else Begin
     Button2.Enabled := True;
  End;

  If ListView1.SelCount > 0 Then Begin

     TotalTStates := 0;
     For Idx := 0 To ListView1.Items.Count -1 Do Begin

        If ListView1.Items[Idx].Selected Then Begin
           Str := ListView1.Items[Idx].SubItems[0];
           LineNum := StrToInt(Copy(Str, 1, Pos(':', Str)-1));
           Str := Copy(Str, Pos(':', Str)+1, 99999);
           StatementNum := StrToInt(Str);
           Inc(TotalTStates, ProfileArray[(LineNum*255)+StatementNum -1]);
        End;

     End;

     Frames := TotalTStates/Opt_CPUSpeed;
     Secs := Frames/50;
     Str := '';
     If TotalTStates > 0 Then Begin
        If Secs >= 1 Then
           Str := Str + FloatToStrF(Secs, ffFixed, 18, 1) + ' Seconds (';
        If Frames >= 1 Then
           Str := Str + FloatToStrF(Frames, ffFixed, 18, 1)+ ' Frames'
        Else
           Str := Str + IntToStr(TotalTStates)+' Ts';
        If Secs >= 1 Then
           Str := Str + ')';
     End Else
        Str := 'No timing info available';

     Label3.Caption := 'Selected: ' + Str;

  End Else

     Label3.Caption := '';

end;

procedure TProfileForm.Button2Click(Sender: TObject);
Var
  Idx: Integer;
begin
  If Button2.Caption = 'Show Selected' Then Begin
     If ListView1.SelCount <> 0 Then Begin
        SelectedList.Clear;
        For Idx := 0 To ListView1.Items.Count -1 Do
           If ListView1.Items[Idx].Selected Then
              SelectedList.Add(ProfileBASIClist[Idx]);
        ShowingSelected := True;
        BuildProfileTree;
     End;
  End Else Begin
     SelectedList.Clear;
     ShowingSelected := False;
     BuildProfileTree;
  End;
end;

procedure TProfileForm.Button3Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_profiling_results.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TProfileForm.Button4Click(Sender: TObject);
begin
  //added 1.69+ arda
  if ProfilingEnabled then begin
        ProfilingEnabled:=false;
  end else Begin
          NewProfile;
          ListView1.Items.Clear;
          ProfilingEnabled := true;
  end;
  buildprofiletree;
end;

procedure TProfileForm.Timer1Timer(Sender: TObject);
begin
  // realtime update - added 1.69+ arda
  // disabled (not necessarily useful)
if ProfilingEnabled and Registers.EmuRunning then Begin
    buildprofiletree;
end;
end;

procedure TProfileForm.Button5Click(Sender: TObject);
begin
    //added 1.69+ arda
    //manual refresh button
  if ProfilingEnabled then buildprofiletree;
end;

end.
