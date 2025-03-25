unit Watches;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, Evaluate;

type
  TWatch = record
     Valid: Boolean;
     Enabled: Boolean;
     WatchType,                  // Declares the Watch type. 0 - Expression, 1 - SysVar, 2 - Variable, 3 - Memory Address
     wTypeIndex: Byte;           // Declares the SYSVAR index
     wVarName: AnsiString;           // The variable name to watch.
     wAddress: Word;             // The Address to watch,if necessary.
     wSize: Byte;                // The size of the result (byte, word, DWord, custom - 1,2,3,4 .. 38 etc)
     LastResult: AnsiString;         // Used to determine if the expression has changed.
     Expression: AnsiString;         // The Expression to evaluate, or the variable name
     CanBreak: Boolean;          // Can it break execution? It will break if the expression returns non-zero, numeric.
  End;

  PWatch = ^TWatch;

  TWatchWindow = class(TForm)
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    AddWatch1: TMenuItem;
    N1: TMenuItem;
    EnableAll1: TMenuItem;
    DisableAll1: TMenuItem;
    DeleteAll1: TMenuItem;
    ENabled1: TMenuItem;
    Delete1: TMenuItem;
    Properties1: TMenuItem;
    Refresh1: TMenuItem;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure AddWatch1Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ENabled1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure EnableAll1Click(Sender: TObject);
    procedure DisableAll1Click(Sender: TObject);
    procedure DeleteAll1Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Function  CreateWatch(Enabled: Boolean;
                          WatchType, wSize, wTypeIndex: Byte;
                          wAddress: Word;
                          Expression, wVarName: AnsiString;
                          CanBreak: Boolean): Byte;
    procedure BuildWatchList;
    procedure UpdateWatchEntry(Index: Integer);
  end;

var
  WatchWindow: TWatchWindow;
  WatchArray: Array[0..255] of TWatch;
  WatchExpressions: Array[0..255] of TExpression;
  WatchList: AnsiString;
  Watches_Updating: Boolean;

  function UpdateWatches: Boolean;

implementation

{$R *.DFM}

Uses WatchProps, SysVars, FastCore, ROMUtils, Utility, LogWind, BASinMain;

Function TWatchWindow.CreateWatch(Enabled: Boolean; WatchType, wSize, wTypeIndex: Byte; wAddress: Word; Expression, wVarName: AnsiString; CanBreak: Boolean): Byte;
Var
  F: Byte;
Begin
  For F := 0 To 255 Do
     If Not WatchArray[F].Valid Then Begin
        WatchArray[F].Enabled := Enabled;
        WatchArray[F].WatchType := WatchType;
        WatchArray[F].wTypeIndex := wTypeIndex;
        WatchArray[F].wAddress := wAddress;
        WatchArray[F].wSize := wSize;
        WatchArray[F].Expression := Expression;
        WatchArray[F].CanBreak := CanBreak;
        WatchArray[F].LastResult := '';
        WatchArray[F].wVarName := wVarName;
        WatchExpressions[F].Expression := Expression;
        WatchExpressions[F].SyntaxChecked := False;
        WatchExpressions[F].Tokenised := '';
        WatchArray[F].Valid := True;
        Break;
     End;
  Result := F;
End;

Procedure TWatchWindow.BuildWatchList;
Var
  F: Byte;
  LI: TListItem;
Begin

  Watches_Updating := True;
  ListView1.Items.BeginUpdate;

  WatchList := '';
  ListView1.Items.Clear;
  For F := 0 To 255 Do
     If WatchArray[F].Valid Then Begin
        LI := ListView1.Items.Add;
        Case WatchArray[F].WatchType of
           0: Begin // Expression
                 LI.SubItems.Add(WatchArray[F].Expression);
              End;
           1: Begin // Variable
                 LI.SubItems.Add('Var: '+WatchArray[F].wVarName);
              End;
           2: Begin // SYSVAR
                 LI.SubItems.Add('SysVar: '+SystemVariables[WatchArray[F].wTypeIndex].Name);
              End;
           3: Begin // Memory Address
                 Case WatchArray[F].wSize Of
                    0: LI.SubItems.Add('Mem Byte ['+IntToStr(WatchArray[F].wAddress)+']');
                    1: LI.SubItems.Add('Mem Word ['+IntToStr(WatchArray[F].wAddress)+']');
                    2: LI.SubItems.Add('Mem DWord ['+IntToStr(WatchArray[F].wAddress)+']');
                 End;
              End;
        End;
        LI.Checked := WatchArray[F].Enabled;
        If WatchArray[F].Enabled Then
           LI.SubItems.Add('')
        Else
           LI.SubItems.Add('Watch Disabled');
        WatchList := WatchList + AnsiChar(F);
     End;

  UpdateWatches;
  ListView1.Items.EndUpdate;
  Watches_Updating := False;

  Button4.Enabled := ListView1.Selected <> Nil;
  Button5.Enabled := Button4.Enabled;

End;

Function UpdateWatches: Boolean;
Var
  F: Byte;
  WatchIdx: Integer;
  CurWatch: PWatch;
Begin
  Result := False;
  For F := 1 To Length(WatchList) Do Begin
     WatchIdx := Ord(WatchList[F]);
     CurWatch := @WatchArray[WatchIdx];
     If (CurWatch.CanBreak or WatchWindow.Visible) and CurWatch.Enabled Then Begin
        Case CurWatch.WatchType Of
           0: Begin // Expression Watch
                 EvaluateExpr(WatchExpressions[WatchIdx]);
                 Case WatchExpressions[WatchIdx].ResultType of
                    0: Begin // AnsiString result
                          CurWatch.LastResult := WatchExpressions[WatchIdx].ResultStr;
                       End;
                    1: Begin // Numeric Result
                          CurWatch.LastResult := FloatToStrEx(WatchExpressions[WatchIdx].ResultNum);
                          If CurWatch.CanBreak Then
                             If WatchExpressions[WatchIdx].ResultType = 1 Then
                                If WatchExpressions[WatchIdx].ResultNum <> 0 Then
                                   Result := True;
                       End;
                    2: Begin // Error in evaluation
                          CurWatch.LastResult := Copy(WatchExpressions[WatchIdx].ResultStr, 3, 999999);
                          Log('Watch ['+CurWatch.Expression+'] : '+WatchExpressions[WatchIdx].ResultStr);
                       End;
                 End;
              End;
           1: Begin // Variable Watch
                 CurWatch.LastResult := GetVariable(CurWatch.wVarName, 0);
              End;
           2: Begin // SYSVAR Watch
                 CurWatch.LastResult := SysVarsWindow.GetSysVar(CurWatch.wTypeIndex);
              End;
           3: Begin // Address Watch
                 Case CurWatch.wSize of
                    0: CurWatch.LastResult := IntToStr(Memory[CurWatch.wAddress]);
                    1: CurWatch.LastResult := IntToStr(GetWord(@Memory[CurWatch.wAddress]));
                    2: CurWatch.LastResult := IntToStr(GetDWord(@Memory[CurWatch.wAddress]));
                 End;
              End;
        End;
        If WatchWindow.Visible Then
           WatchWindow.UpdateWatchEntry(F);
     End;
  End;
End;

Procedure TWatchWindow.UpdateWatchEntry(Index: Integer);
Var
  Watch: PWatch;
  LI: TListItem;
Begin
  Watches_Updating := True;
  If (Index < 256) and (Index > 0) Then Begin
     Watch := @WatchArray[Ord(WatchList[Index])];
     If Watch.Valid Then Begin
        LI := ListView1.Items[Index-1];
        If Watch.Enabled Then
           LI.SubItems[1] := Watch.LastResult
        Else
           LI.SubItems[1] := 'Watch Disabled';
     End;
  End;
  Watches_Updating := False;
End;

procedure TWatchWindow.FormCreate(Sender: TObject);
begin
  if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;
  ListView1.DoubleBuffered := True;
  ListView1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - 24 - Button2.Height);
  Button2.SetBounds(ClientWidth - 8 - Button2.Width, ClientHeight - 8 - Button2.Height, Button2.Width, Button2.Height);
  Button6.SetBounds(Button2.Left - Button6.Width - 4, Button2.Top, Button6.Width, Button2.Height);
  Button1.SetBounds(Button6.Left - Button1.Width - 4, Button6.Top, Button1.Width, Button1.Height);
  Button7.SetBounds(Button1.Left - Button7.Width - 4, Button1.Top, Button7.Width, Button1.Height);
  Button3.SetBounds(8, ClientHeight - 8 - Button3.Height, Button3.Width, Button3.Height);
  Button4.SetBounds(Button3.Left + Button3.Width + 4, Button3.Top, Button4.Width, Button4.Height);
  Button5.SetBounds(Button4.Left + Button4.Width + 4, Button3.Top, Button5.Width, Button5.Height);


  Watchlist := '';
end;

procedure TWatchWindow.FormShow(Sender: TObject);
begin
  if Opt_ToolFontSize>0 Then ListView1.Font.Size:=Opt_ToolFontSize;
  BuildWatchList;
end;

procedure TWatchWindow.PopupMenu1Popup(Sender: TObject);
begin
  // Enable/Disable/Hide relevant items.
  // Itemindex specifies if we clicked a watch or not.
  If ListView1.Selected <> nil Then Begin
     AddWatch1.Visible := False;
     EnableAll1.Visible := False;
     DeleteAll1.Visible := False;
     DisableAll1.Visible := False;
     N1.Visible := False;
     Enabled1.Visible := True;
     Enabled1.Checked := WatchArray[Ord(WatchList[ListView1.Selected.Index+1])].Enabled;
     Delete1.Visible := True;
     Properties1.visible := True;
     Refresh1.Visible := True;
  End Else Begin
     EnableAll1.Enabled := ListView1.Items.Count > 0;
     DeleteAll1.Enabled := ListView1.Items.Count > 0;
     DisableAll1.Enabled := ListView1.Items.Count > 0;
     AddWatch1.Visible := True;
     EnableAll1.Visible := True;
     DeleteAll1.Visible := True;
     DisableAll1.Visible := True;
     N1.Visible := True;
     Enabled1.Visible := False;
     Delete1.Visible := False;
     Properties1.visible := False;
     Refresh1.Visible := True;
  End;
end;

procedure TWatchWindow.AddWatch1Click(Sender: TObject);
Var
  NewWatch: Byte;                                                                                                     
  MousePos: TPoint;
begin
  Windows.GetCursorPos(MousePos);
  CentreForm(WatchProperties, MousePos.X, MousePos.Y);
  NewWatch := CreateWatch(True, 0, 0, 0, 0, '', '', False);
  WatchProperties.GetWatch(NewWatch);
  WatchProperties.Caption := 'Add Watch';
  ShowWindow(WatchProperties, True);
  If Not WatchProperties.OkayPressed Then WatchArray[NewWatch].Valid := False;
  BuildWatchlist;
end;

procedure TWatchWindow.ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  // Use this event to test if the checked property is changed.
  If Not Watches_Updating Then
     If Item.SubItems.Count <> 0 Then
        If Item.Checked <> WatchArray[Ord(WatchList[Item.Index+1])].Enabled Then Begin
           WatchArray[Ord(WatchList[Item.Index+1])].Enabled := Item.Checked;
           UpdateWatchEntry(Item.Index+1);
        End;
end;

procedure TWatchWindow.Enabled1Click(Sender: TObject);
begin
  If ListView1.Selected <> nil Then Begin
     Enabled1.Checked := Not Enabled1.Checked;
     ListView1.Selected.Checked := Enabled1.Checked;
  End;
end;

procedure TWatchWindow.Delete1Click(Sender: TObject);
begin
  If ListView1.Selected <> nil Then Begin
     WatchArray[Ord(WatchList[ListView1.Selected.Index+1])].Valid := False;
     BuildWatchList;
  End;
end;

procedure TWatchWindow.Properties1Click(Sender: TObject);
Var
  MousePos: TPoint;
begin
  Windows.GetCursorPos(MousePos);
  CentreForm(WatchProperties, MousePos.X, MousePos.Y);
  If ListView1.Selected <> nil Then Begin
     WatchProperties.GetWatch(Ord(WatchList[ListView1.Selected.Index+1]));
     WatchProperties.Caption := 'Watch Properties';
     ShowWindow(WatchProperties, True);
     BuildWatchlist;
  End;
end;

procedure TWatchWindow.EnableAll1Click(Sender: TObject);
Var
  F: Byte;
begin
  For F := 1 to Length(WatchList) Do
     WatchArray[Ord(WatchList[F])].Enabled := True;
  BuildWatchList;
end;

procedure TWatchWindow.DisableAll1Click(Sender: TObject);
Var
  F: Byte;
begin
  For F := 1 to Length(WatchList) Do
     WatchArray[Ord(WatchList[F])].Enabled := False;
  BuildWatchList;
end;

procedure TWatchWindow.DeleteAll1Click(Sender: TObject);
Var
  F: Byte;
begin
  For F := 1 to Length(WatchList) Do
     WatchArray[Ord(WatchList[F])].Valid := False;
  BuildWatchList;
end;

procedure TWatchWindow.Refresh1Click(Sender: TObject);
begin
  UpdateWatches;
end;

procedure TWatchWindow.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TWatchWindow.ListView1DblClick(Sender: TObject);
begin
  Properties1Click(Sender);
end;

procedure TWatchWindow.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  Button4.Enabled := ListView1.Selected <> Nil;
  Button5.Enabled := Button4.Enabled;
end;

procedure TWatchWindow.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_DELETE Then
     If ListView1.Selected <> nil Then
        Delete1Click(nil);
end;

procedure TWatchWindow.Button6Click(Sender: TObject);
begin
  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_watch.html'), HH_DISPLAY_TOPIC, 0);
end;

procedure TWatchWindow.Button7Click(Sender: TObject);
var
  i: Integer;
begin
      for i := Low(WatchArray) to High(WatchArray) do
      begin
         WatchArray[i].Valid := False;
      end;
      BuildWatchList;
end;

end.
