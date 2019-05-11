unit BinaryForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Math, ExtCtrls, ComCtrls, Menus, Utility;

type
  TBinaryType = (btDecimal, btHex, btREM, btBASIC, btMemory);

  TBinaryWindow = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label6: TLabel;
    Bevel4: TThemeBevel;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    DATADecimal1: TMenuItem;
    DATAHex1: TMenuItem;
    REMstatement1: TMenuItem;
    ExtractBASIC1: TMenuItem;
    SendtoMemory1: TMenuItem;
    Panel2: TPanel;
    ListView1: TListView;
    Button4: TButton;
    Label2: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Edit3: TEdit;
    Label4: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Panel3: TPanel;
    Label7: TLabel;
    Label10: TLabel;
    Edit5: TEdit;
    CheckBox2: TCheckBox;
    Edit8: TEdit;
    CheckBox3: TCheckBox;
    Panel5: TPanel;
    Label9: TLabel;
    Panel4: TPanel;
    Label8: TLabel;
    Edit6: TEdit;
    Button5: TButton;
    SendToManager1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Button4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DATADecimal1Click(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BinaryFiles, CodeToAdd: TStringlist;
    Options: Array of TStringlist;
    LastSelected: Integer;
    Procedure PopulateListBox;
    Function  ValidateInput(Item: Integer): Boolean;
    Procedure BuildOptions(ChangingTo: Integer);
    Procedure AddBinary(Name, Binary: String);
    Procedure AddBinaryEx(Name, Binary: String; BinaryType: TBinaryType; Address: Word);
    Procedure RemoveBinary(Index: Integer);
    Procedure ClearBinaries;
  end;

var
  BinaryWindow: TBinaryWindow;

Const

  Descs: Array[0..4] of String = ('DATA Dec', 'DATA Hex', 'REM', 'Basic', 'Memory');

implementation

{$R *.DFM}

Uses Filing, FastCore, InputUtils, ROMUtils, BASSupport, AddCode,
     BasinMain, Display, UDGOptions, Evaluate, Binaries;

Procedure TBinaryWindow.AddBinary(Name, Binary: String);
Var
  Idx, Num: Integer;
Begin

  BinaryFiles.Add(Name+'|'+Binary);
  Num := Length(Options);
  SetLength(Options, Num +1);
  Options[Num] := TStringlist.Create;
  Options[Num].Add(Chr(0));
  For Idx := 1 To 4 Do Options[Num].Add('0');
  Options[Num].Add(Chr(0));
  Options[Num].Add(Chr(0));

End;

Procedure TBinaryWindow.AddBinaryEx(Name, Binary: String; BinaryType: TBinaryType; Address: Word);
Var
  Idx, Num: Integer;
Begin

  BinaryFiles.Add(Name+'|'+Binary);
  Num := Length(Options);
  SetLength(Options, Num +1);
  Options[Num] := TStringlist.Create;
  Options[Num].Add(Chr(Integer(BinaryType)));
  For Idx := 1 To 3 Do Options[Num].Add('0');
  Options[Num].Add(IntToStr(Address));
  Options[Num].Add(Chr(0));
  Options[Num].Add(Chr(0));

End;

Procedure TBinaryWindow.RemoveBinary(Index: Integer);
Var
  Idx: Integer;
Begin

  BinaryFiles.Delete(Index);
  If Index < Length(Options) -1 Then Begin
     For Idx := Index To Length(Options) -2 Do
        Options[Idx] := Options[Idx +1];
     SetLength(Options, Length(Options) -1);
  End;

End;

Procedure TBinaryWindow.ClearBinaries;
Var
  Idx: Integer;
Begin

  If Length(Options) > 0 Then
     For Idx := 0 To Length(Options) -1 Do
        Options[Idx].Free;

  SetLength(Options, 0);
  BinaryFiles.Clear;

End;

procedure TBinaryWindow.Button1Click(Sender: TObject);
begin

  Close;

end;

procedure TBinaryWindow.Button3Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_import_binary_file.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TBinaryWindow.FormCreate(Sender: TObject);
begin

  Button3.SetBounds(8, ClientHeight - Button3.Height - 8, Button3.Width, Button3.Height);
  Button1.SetBounds(ClientWidth - Button1.Width - 8, Button3.Top, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - Button2.Width - 4, Button1.Top, Button2.Width, Button2.Height);
  Panel1.SetBoundS(8, Button1.Top - Panel1.Height - 8, ClientWidth - 16, Panel1.Height);
  Panel3.SetBounds(Panel1.Left, Panel1.Top, Panel1.Width, Panel1.Height);
  Panel4.SetBounds(Panel1.Left, Panel1.Top, Panel1.Width, Panel1.Height);
  Panel5.SetBounds(Panel1.Left, Panel1.Top, Panel1.Width, Panel1.Height);
  Label6.SetBounds(8, Panel1.Top - Label6.Height - Bevel4.Height - 8, Label6.Width, Label6.Height);
  Bevel4.SetBounds(8, Label6.Top + Label6.Height + 4, ClientWidth - 16, 2);
  Panel2.SetBounds(Panel2.Left, Panel2.Top, ClientWidth - 16, Panel1.Top - Label6.Height - Bevel4.Height - Panel2.Top - 16);
  Button5.SetBounds(Button3.Left + Button3.Width + 4, Button3.Top, Button5.Width, Button5.Height);

  LastSelected := -1;
  BinaryFiles := TStringList.Create;
  CodeToAdd := TStringlist.Create;

end;

Procedure TBinaryWindow.PopulateListBox;
Var
  Idx: Integer;
  NewItem: TListItem;
Begin

  ListView1.Items.BeginUpdate;
  ListView1.Items.Clear;

  For Idx := 0 To BinaryFiles.Count -1 Do Begin

     NewItem := ListView1.Items.Add;
     NewItem.Caption := Utility.ShrinkFilename(Copy(BinaryFiles[Idx], 1, Pos('|', BinaryFiles[Idx]) -1), ListView1.Columns[0].Width);
     NewItem.SubItems.Add(Descs[Ord(Options[Idx].Strings[0][1])]);

  End;

  ListView1.Items.EndUpdate;

  If BinaryFiles.Count > 0 Then Begin

     Button2.Enabled := True;
     LastSelected := -1;
     ListView1.Selected := ListView1.Items[0];
     ListView1.SetFocus;

     BuildOptions(Ord(Options[ListView1.Selected.Index].Strings[0][1]));

  End Else Begin

     Button5.Enabled := False;
     Button2.Enabled := False;
     Button4.Visible := False;
     Panel5.Visible := True;
     Panel5.BringToFront;
     Label9.Caption := 'There are no more binaries to import.';
     Label9.SetBounds((Panel5.Width Div 2) - (Label9.Width Div 2), (Panel5.Height Div 2) - (Label9.Height Div 2), Label9.Width, Label9.Height);

  End;

End;

procedure TBinaryWindow.FormResize(Sender: TObject);
Var
  Idx: Integer;
  ItemRect: TRect;
  ItemSize: Integer;
begin

  ListView1.Columns[1].Width := Canvas.TextWidth(ListView1.Columns[1].Caption)+32;
  ListView1.Columns[0].Width := ListView1.ClientWidth - ListView1.Columns[1].Width;

  ListView1.Items.BeginUpdate;

  For Idx := 0 To BinaryFiles.Count -1 Do Begin

     ListView1.Items[Idx].Caption := Utility.ShrinkFilename(Copy(BinaryFiles[Idx], 1, Pos('|', BinaryFiles[Idx]) -1), ListView1.Columns[0].Width);
     If ListView1.Items[Idx].SubItems.Count > 0 Then
        ListView1.Items[Idx].SubItems[0] := (Descs[Ord(Options[Idx].Strings[0][1])]);

  End;

  If ListView1.Selected <> Nil Then Begin

     ItemRect := ListView1.Selected.DisplayRect(drBounds);
     ItemRect.TopLeft := ScreenToClient(ListView1.ClientToScreen(ItemRect.TopLeft));
     ItemRect.BottomRight := ScreenToClient(ListView1.ClientToScreen(ItemRect.BottomRight));
     ItemSize := ItemRect.Bottom - ItemRect.Top;
     Button4.SetBounds(ItemRect.Right - ItemSize -1 - Panel2.Left, ItemRect.Top - Panel2.Top, ItemSize, ItemSize);

  End;

  Label9.SetBounds((Panel5.Width Div 2) - (Label9.Width Div 2), (Panel5.Height Div 2) - (Label9.Height Div 2), Label9.Width, Label9.Height);
  Panel1.Top := Bevel4.Top + 4;
  Panel3.Top := Bevel4.Top + 4;
  Panel4.Top := Bevel4.Top + 4;
  Panel5.Top := Bevel4.Top + 4;
  ListView1.Items.EndUpdate;

end;

procedure TBinaryWindow.FormShow(Sender: TObject);
begin

  LastSelected := -1;
  PopulateListBox;

end;

procedure TBinaryWindow.FormDestroy(Sender: TObject);
Var
  Idx: Integer;
begin

  For Idx := 0 To BinaryFiles.Count -1 Do
     Options[Idx].Free;
  BinaryFiles.Free;
  CodeToAdd.Free;

end;

procedure TBinaryWindow.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
Var
  ItemRect: TRect;
  ItemSize: Integer;
begin

  If Selected and (Item <> Nil) Then Begin

     ItemRect := Item.DisplayRect(drBounds);
     ItemRect.TopLeft := ScreenToClient(ListView1.ClientToScreen(ItemRect.TopLeft));
     ItemRect.BottomRight := ScreenToClient(ListView1.ClientToScreen(ItemRect.BottomRight));
     ItemSize := ItemRect.Bottom - ItemRect.Top;
     Button4.SetBounds(ItemRect.Right - ItemSize -1 - Panel2.Left, ItemRect.Top - Panel2.Top, ItemSize, ItemSize);
     Button4.Visible := Selected;
     Button5.Enabled := True;

     BuildOptions(Ord(Options[ListView1.Selected.Index].Strings[0][1]));
     LastSelected := Item.Index;

  End Else Begin

     Button5.Enabled := False;
     Button4.Visible := False;
     Panel5.Visible := True;
     Panel5.BringToFront;
     Label9.Caption := 'Select a binary object to set parameters.';
     Label9.SetBounds((Panel5.Width Div 2) - (Label9.Width Div 2), (Panel5.Height Div 2) - (Label9.Height Div 2), Label9.Width, Label9.Height);

  End;

end;

procedure TBinaryWindow.PopupMenu1Popup(Sender: TObject);
begin

  DATADecimal1.Checked := False;
  DATAHex1.Checked := False;
  REMstatement1.Checked := False;
  ExtractBASIC1.Checked := False;
  SendtoMemory1.Checked := False;

  Case Ord(Options[ListView1.Selected.Index].Strings[0][1]) of
     0: DATADecimal1.Checked := true;
     1: DATAHex1.Checked := True;
     2: REMstatement1.Checked := True;
     3: ExtractBASIC1.Checked := True;
     4: SendtoMemory1.Checked := True;
  End;

end;

procedure TBinaryWindow.Button4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  CPos: TPoint;
begin

  Button4.Default := False;
  Windows.GetCursorPos(CPos);
  PopUpMenu1.Popup(CPos.X, CPos.Y);

end;

procedure TBinaryWindow.DATADecimal1Click(Sender: TObject);
begin

  BuildOptions((Sender as TMenuItem).Tag);
  Options[ListView1.Selected.Index].Strings[0] := Chr((Sender as TMenuItem).Tag);
  FormResize(nil);

end;

procedure TBinaryWindow.ListView1AdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
Var
  ItemRect: TRect;
  ItemSize: Integer;
begin

  If ListView1.Selcount > 0 Then Begin
     ItemRect := ListView1.Selected.DisplayRect(drBounds);
     Button4.Visible := (ItemRect.Bottom > 17) and (ItemRect.Top < ListView1.ClientHeight);
  End;

  ItemRect := Item.DisplayRect(drBounds);

  If Button4.Visible Then Begin

     If ListView1.Selected.Index = Item.Index Then Begin

        ItemRect.TopLeft := ScreenToClient(ListView1.ClientToScreen(ItemRect.TopLeft));
        ItemRect.BottomRight := ScreenToClient(ListView1.ClientToScreen(ItemRect.BottomRight));
        ItemSize := ItemRect.Bottom - ItemRect.Top;
        Button4.SetBounds(ItemRect.Right - ItemSize -1 - Panel2.Left, ItemRect.Top - Panel2.Top, ItemSize, ItemSize);

     End;

  End;

end;

Procedure TBinaryWindow.BuildOptions(ChangingTo: Integer);
Var
  BinaryType, Index: Integer;
Begin

  If ListView1.Selected <> Nil Then Begin

     If LastSelected <> -1 Then Begin

        Index := LastSelected;
        BinaryType := Ord(Options[Index].Strings[0][1]);

        Case BinaryType of

           0, 1: Begin // DATA Dec/Hex

                 Options[Index].Strings[1] := Edit1.Text;                       // Bytes Per Line
                 Options[Index].Strings[2] := Edit3.Text;                       // Start line number
                 Options[Index].Strings[3] := Edit2.Text;                       // Step
                 Options[Index].Strings[4] := Edit4.Text;                       // Address
                 Options[Index].Strings[5] := Chr(Byte(CheckBox1.Checked));     // Add POKEs

              End;

           2: Begin // REM statement

                 Options[Index].Strings[2] := Edit5.Text;                       // Line Number
                 Options[Index].Strings[4] := Edit8.Text;                       // Address
                 Options[Index].Strings[5] := Chr(Byte(CheckBox2.Checked));     // Copy Stub
                 Options[Index].Strings[6] := Chr(Byte(CheckBox3.Checked));     // Force Jump

              End;

           4: Begin // Memory

                 Options[Index].Strings[4] := Edit6.Text;                       // Address

              End;

        End;

     End;

     Label6.Enabled := True;
     Index := ListView1.Selected.Index;
     BinaryType := ChangingTo;

     Case BinaryType of

        0, 1: Begin // DATA Dec/Hex

              Edit1.Text := Options[Index].Strings[1];                          // Bytes Per Line
              Edit3.Text := Options[Index].Strings[2];                          // Start line number
              Edit2.Text := Options[Index].Strings[3];                          // Step
              Edit4.Text := Options[Index].Strings[4];                          // Address
              CheckBox1.Checked := Boolean(Ord(Options[Index].Strings[5][1]));  // Add POKEs
              Label5.Enabled := CheckBox1.Checked;
              Edit4.Enabled := CheckBox1.Checked;

              Panel1.Visible := True;
              Panel3.Visible := False;
              Panel4.Visible := False;
              Panel5.Visible := False;

           End;

        2: Begin // REM statement

              Edit5.Text := Options[Index].Strings[2];                          // Line Number
              Edit8.Text := Options[Index].Strings[4];                          // Address
              CheckBox2.Checked := Boolean(Ord(Options[Index].Strings[5][1]));  // Copy Stub
              CheckBox3.Checked := Boolean(Ord(Options[Index].Strings[6][1]));  // Force Jump
              CheckBox3.Enabled := CheckBox2.Checked;
              Edit8.Enabled := CheckBox2.Checked;
              Label10.Enabled := CheckBox2.Checked;

              Panel1.Visible := False;
              Panel3.Visible := True;
              Panel4.Visible := False;
              Panel5.Visible := False;

           End;

        4: Begin // Memory

              Edit6.Text := Options[Index].Strings[4];   // Address
              Panel1.Visible := False;
              Panel3.Visible := False;
              Panel4.Visible := True;
              Panel5.Visible := False;

           End;

        3: Begin // BASIC extractor

              Panel1.Visible := False;
              Panel3.Visible := False;
              Panel4.Visible := False;
              Panel5.Visible := True;
              Label9.Caption := 'No options available for this import type.';
              Label9.SetBounds((Panel5.Width Div 2) - (Label9.Width Div 2), (Panel5.Height Div 2) - (Label9.Height Div 2), Label9.Width, Label9.Height);

           End;

     End;

  End Else Begin

     Panel1.Visible := False;
     Panel3.Visible := False;
     Panel4.Visible := False;
     Panel5.Visible := False;
     Label6.Enabled := False;

  End;

  CheckBox2Click(Nil);

End;

Function TBinaryWindow.ValidateInput(Item: Integer): Boolean;
Var
  Value: Integer;
  Error: Boolean;
  ErrorControl: TWinControl;
Begin

  Error := False;

  Case Ord(Options[Item].Strings[0][1]) of

     0, 1: Begin // DATA Dec/Hex
           Value := Round(EvaluateNum(Options[Item].Strings[1], -1));
           If (Value < 1) or (Value > 255) Then Begin
              MessageBox(Handle, pChar('A valid Byte Count ranges from 1 to 255'), pChar('Invalid Byte count'), MB_OK or MB_ICONWARNING);
              ErrorControl := Edit1;
              Error := True;
           End Else Begin
              Options[Item].Strings[1] := IntToStr(Value);
              Value := Round(EvaluateNum(Options[Item].Strings[3], -1));
              If (Value < 1) or (Value > 255) Then Begin
                 MessageBox(Handle, pChar('Valid Step Values range from 1 to 255'), pChar('Invalid Step'), MB_OK or MB_ICONWARNING);
                 ErrorControl := Edit2;
                 Error := True;
              End Else Begin
                 Options[Item].Strings[3] := IntToStr(Value);
                 Value := Round(EvaluateNum(Options[Item].Strings[2], -1));
                 If (Value < 1) or (Value > 9999) Then Begin
                    MessageBox(Handle, pChar('Valid Start Lines range from 1 to 9999'), pChar('Invalid Start Line'), MB_OK or MB_ICONWARNING);
                    ErrorControl := Edit3;
                    Error := True;
                 End Else If CheckBox1.Checked Then Begin
                    Options[Item].Strings[2] := IntToStr(Value);
                    Value := Round(EvaluateNum(Options[Item].Strings[4], -1));
                    If (Value < 16384) or (Value > 65535) Then Begin
                       MessageBox(Handle, pChar('Valid Addresses range from 16384 to 65535'), pChar('Invalid Address'), MB_OK or MB_ICONWARNING);
                       ErrorControl := Edit4;
                       Error := True;
                    End Else
                       Options[Item].Strings[4] := IntToStr(Value);
                 End;
              End;
           End;
           Options[Item].Strings[5] := Chr(Byte(CheckBox1.Checked));
           If Error Then Begin
              Panel1.Visible := True;
              Panel3.Visible := False;
              Panel4.Visible := False;
              Panel5.Visible := False;
              ErrorControl.SetFocus;
           End;
        End;

     2: Begin // REM Statement
           Value := Round(EvaluateNum(Options[Item].Strings[2], -1));
           If (Value < 1) or (Value > 9999) Then Begin
              MessageBox(Handle, pChar('Valid Line Numbers range from 1 to 9999'), pChar('Invalid Line Number'), MB_OK or MB_ICONWARNING);
              ErrorControl := Edit5;
              Error := True;
           End Else Begin
              Options[Item].Strings[2] := IntToStr(Value);
              Value := Round(EvaluateNum(Options[Item].Strings[4], -1));
              If (Value < 16384) or (Value > 65535) Then Begin
                 MessageBox(Handle, pChar('Valid Addresses range from 16384 to 65535'), pChar('Invalid Address'), MB_OK or MB_ICONWARNING);
                 ErrorControl := Edit8;
                 Error := True;
              End Else
                 Options[Item].Strings[4] := IntToStr(Value);
           End;
           Options[Item].Strings[5] := Chr(Byte(CheckBox2.Checked));
           Options[Item].Strings[6] := Chr(Byte(CheckBox3.Checked));
           If Error Then Begin
              Panel1.Visible := False;
              Panel3.Visible := True;
              Panel4.Visible := False;
              Panel5.Visible := False;
              ErrorControl.SetFocus;
           End;
        End;

     4: Begin // Memory
           Value := Round(EvaluateNum(Options[Item].Strings[4], -1));
           If (Value < 16384) or (Value > 65535) Then Begin
              MessageBox(Handle, pChar('Valid Addresses range from 16384 to 65535'), pChar('Invalid Address'), MB_OK or MB_ICONWARNING);
              Panel1.Visible := False;
              Panel3.Visible := False;
              Panel4.Visible := True;
              Panel5.Visible := False;
              Edit6.SetFocus;
           End Else
           Options[Item].Strings[4] := IntToStr(Value);
        End;

  End;

  Result := Not Error;

End;

procedure TBinaryWindow.CheckBox2Click(Sender: TObject);
begin

  Label10.Enabled := CheckBox2.Checked;
  Edit8.Enabled := CheckBox2.Checked;
  CheckBox3.Enabled := CheckBox2.Checked;

  Label5.Enabled := CheckBox1.Checked;
  Edit4.Enabled := CheckBox1.Checked;

end;

procedure TBinaryWindow.Button5Click(Sender: TObject);
begin

  If ListView1.Selected <> nil Then Begin

     RemoveBinary(ListView1.Selected.Index);
     PopulateListBox;

  End;

end;

procedure TBinaryWindow.Button2Click(Sender: TObject);
Var
  Idx: Integer;
  NewCode: TStringlist;
  NewCodePresent: Boolean;
begin

  NewCode := TStringlist.Create;
  LastSelected := ListView1.Selected.Index;
  BuildOptions(ListView1.Selected.Index);

  For Idx := 0 To BinaryFiles.Count -1 Do Begin

     If ValidateInput(0) Then Begin
        NewCodePresent := True;
        Case Ord(Options[0].Strings[0][1]) Of

           0: Begin

                 BinaryToDATADec(Copy(BinaryFiles[0], Pos('|', BinaryFiles[0]) +1, 999999),
                                 NewCode,
                                 Boolean(Options[0].Strings[5][1]),
                                 StrToInt(Options[0].Strings[4]),
                                 StrToInt(Options[0].Strings[2]),
                                 StrToInt(Options[0].Strings[3]),
                                 StrToInt(Options[0].Strings[1]));

              End;

           1: Begin

                 BinaryToDATAHex(Copy(BinaryFiles[0], Pos('|', BinaryFiles[0]) +1, 999999),
                                 NewCode,
                                 Boolean(Options[0].Strings[5][1]),
                                 StrToInt(Options[0].Strings[4]),
                                 StrToInt(Options[0].Strings[2]),
                                 StrToInt(Options[0].Strings[3]),
                                 StrToInt(Options[0].Strings[1]));

              End;

           2: Begin

                 BinaryToREM(Copy(BinaryFiles[0], Pos('|', BinaryFiles[0])+1, 999999),
                             NewCode,
                             Boolean(Options[0].Strings[5][1]),
                             Boolean(Options[0].Strings[6][1]),
                             StrToInt(Options[0].Strings[4]),
                             StrToInt(Options[0].Strings[2]));

              End;

           3: Begin

                 BinaryToBASIC(Copy(BinaryFiles[0], Pos('|', BinaryFiles[0])+1, 999999),
                               NewCode);

              End;

           4: Begin

                 NewCodePresent := False;
                 BinaryToMemory(Copy(BinaryFiles[0], Pos('|', BinaryFiles[0])+1, 999999),
                                StrToInt(Options[0].Strings[4]));

              End;

        End;

        RemoveBinary(0);
        PopulateListBox;

     End Else Begin

        NewCode.Free;
        Exit;

     End;

     If NewCodePresent Then Begin
        AddCodeWindow.ClearCode;
        AddCodeWindow.AddCode(NewCode);
        CentreFormOnForm(AddCodeWindow, Self);
        ShowWindow(AddCodeWindow, True);
        NewCode.Clear;
     End;

  End;

  NewCode.Free;
  ClearBinaries;
  Close;

end;

end.
