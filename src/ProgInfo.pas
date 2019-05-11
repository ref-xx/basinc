unit ProgInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Utility;

type
  TProgInfoForm = class(TForm)
    Label1: TLabel;
    Bevel1: TThemeBevel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure GatherProgInfo;
  end;

var
  ProgInfoForm: TProgInfoForm;

implementation

{$R *.DFM}

Uses BASinMain, FastCore, ROMUtils, VarsWindow;

procedure TProgInfoForm.FormShow(Sender: TObject);
begin

  GatherProgInfo;

end;

Procedure TProgInfoForm.GatherProgInfo;
Var
  Bytes, BytesUsed, VarsSpace, ProgSpace,
  Idx, NumLines, NumStatements: Integer;
  InString, REMCommand: Boolean;
  CaptionStr: String;
Begin

  Label1.Caption := 'Information for "'+CurProjectName+'"';

  Idx := 1;
  NumLines := 1;
  NumStatements := 1;
  InString := False;
  REMCommand := False;

  If (BASinOutput.BASICMem <> '#13') and (BASinOutput.BASICMem <> '') Then Repeat

     If BASinOutput.BASICMem[Idx] = '"' Then
        InString := Not InString;

     If Not InString Then
        If BASinOutput.BASICMem[Idx] in ['R', 'r'] Then
           If (UpperCase(Copy(BASinOutput.BASICMem, Idx, 4)) = 'REM ') Then
              REMCommand := True;

     If ((BASinOutput.BASICMem[Idx] = ':') and Not InString and Not REMCommand) or
        (Not Instring and Not REMCommand and (BASinOutput.BASICMem[Idx -1] in ['n', 'N']) and (Uppercase(Copy(BASinOutput.BASICMem, Idx -4, 5)) = 'THEN ')) Then
           Inc(NumStatements);

     If BASinOutput.BASICMem[Idx] = #13 Then
        If BASinOutput.BASICMem[Idx +1] <> #13 Then Begin
           Inc(NumLines);
           Inc(NumStatements);
           REMCommand := False;
           InString := False;
        End;

     Inc(Idx);

  Until Idx >= Length(BASinOutput.BASICMem);

  CaptionStr := IntToStr(NumStatements) + ' Statements in '+IntToStr(NumLines)+' lines.'+#13;

  ProgSpace := GetWord(@Memory[VARS]) - GetWord(@Memory[PROG]);
  If ProgSpace > 1024 Then
     CaptionStr := CaptionStr + 'Program uses ' + FloatToStrF(ProgSpace/1024, ffFixed, 18, 1) + ' Kilobytes (' + IntToStr(ProgSpace) + ' Bytes).'+#13
  Else
     CaptionStr := CaptionStr + 'Program uses ' + IntToStr(ProgSpace) + ' Bytes.'+#13;

  VariablesWindow.BuildVarsList;
  VarsSpace := (GetWord(@Memory[E_LINE])-GetWord(@Memory[VARS])) -1;
  If VarsSpace > 0 Then Begin
     CaptionStr := CaptionStr + IntToStr(VariablesWindow.ListView1.Items.Count) + ' Variables declared, using ';
     If VarsSpace > 1024 Then
        CaptionStr := CaptionStr + FloatToStrF(VarsSpace/1024, ffFixed, 18, 1) + ' Kilobytes (' + IntToStr(VarsSpace) + ' Bytes).'+#13
     Else
        CaptionStr := CaptionStr + IntToStr(VarsSpace) + ' Bytes.'+#13;
  End Else
     CaptionStr := CaptionStr + 'No variables declared'+#13;

  CaptionStr := CaptionStr + #13;

  BytesUsed := GetWord(@Memory[E_LINE])-GetWord(@Memory[PROG])-1;
  If BytesUsed = 0 Then
     CaptionStr := 'No program in memory.'+#13#13
  Else Begin
     If BytesUsed > 1024 Then
        CaptionStr := CaptionStr + 'Used a total of '+ FloatToStrF(BytesUsed/1024, ffFixed, 18, 1) + ' Kilobytes (' + IntToStr(BytesUsed) + ' Bytes).'
     Else
        CaptionStr := CaptionStr + 'Used a total of ' + IntToStr(BytesUsed) + ' Bytes.';
  End;

  Bytes := GetWord(@Memory[RAMTOP])-GetWord(@Memory[STKEND]);
  If Bytes > 1024 Then
     CaptionStr := CaptionStr + #13 + FloatToStrF(Bytes/1024, ffFixed, 18, 1) + ' Kilobytes (' + IntToStr(Bytes) + ' Bytes) free for BASIC.'
  Else
     CaptionStr := CaptionStr + #13 + IntToStr(Bytes) + ' Bytes free for BASIC.';

  CaptionStr := CaptionStr + #13;

  CheckFor128kCommands;

  If BytesUsed > 0 Then
     If ProgramIs128k Then
        CaptionStr := CaptionStr + #13 + 'Program is 128k only.'
     Else
        If Not RAMDiskNeedsInit Then
           CaptionStr := CaptionStr + #13 + 'RAMDisk in use, program may not run in 48k.'
        Else
           If UsesUDGsTU Then
              CaptionStr := CaptionStr + #13 + 'Program is 48k Only.'
           Else
              CaptionStr := CaptionStr + #13 + 'Program is both 128k and 48k compatible.';

  Label2.Caption := CaptionStr;

End;

procedure TProgInfoForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TProgInfoForm.Button2Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/program_information.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TProgInfoForm.FormCreate(Sender: TObject);
begin

  Button2.SetBounds(ClientWidth - Button2.Width - 8, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Left - Button1.Width - 8, Button2.Top, Button1.Width, Button1.Height);

end;

end.
