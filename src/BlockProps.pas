unit BlockProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Utility;

type
  TBlockProperties = class(TForm)
    Label1: TLabel;
    Bevel1: TThemeBevel;
    Notebook1: TNotebook;
    Label2: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Edit7: TEdit;
    Edit8: TEdit;
    Label5: TLabel;
    Bevel2: TThemeBevel;
    Label6: TLabel;
    Bevel3: TThemeBevel;
    Label9: TLabel;
    Edit4: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Cancelled: Boolean;
    CurIndex: Integer;
    CurBlock: String;
  end;

var
  BlockProperties: TBlockProperties;

implementation

uses Tapes, FastCore, BASinMain;

{$R *.DFM}

procedure TBlockProperties.FormShow(Sender: TObject);
Var
  TempStr: String;
  TempWord: Word;
begin

  // This dialog can only be shown by the tape image editor
  // so get the current block from there, and get properties from it.

  Cancelled := True;

  If TapeWindow.ListView1.Selected <> Nil Then Begin
     CurIndex := TapeWindow.ListView1.Selected.Index;
     TempStr := TapeBlocks[CurIndex];
     CurBlock := TempStr;
     Case TempStr[4] of
        #0:
           Begin
              NoteBook1.PageIndex := 0;
              Edit1.Text := Copy(TempStr, 5, 10);
              TempWord := GetWord(@TempStr[17]);
              If TempWord And $8000 = 0 Then Begin
                 CheckBox1.Checked := True;
                 Edit2.Text := IntToStr(TempWord and $7FFF);
              End Else Begin
                 CheckBox1.Checked := False;
                 Edit2.Text := '';
              End;
              CheckBox1Click(Self);
              Label1.Caption := 'Program: Block';
           End;
        #1:
           Begin
              NoteBook1.PageIndex := 2;
              Edit4.Text := Copy(TempStr, 5, 10);
              Label1.Caption := 'Number array: Block';
           End;
        #2:
           Begin
              NoteBook1.PageIndex := 2;
              Edit4.Text := Copy(TempStr, 5, 10);
              Label1.Caption := 'Character array: Block';
           End;
        #3:
           Begin
              NoteBook1.PageIndex := 1;
              Edit3.Text := Copy(TempStr, 5, 10);
              TempWord := GetWord(@TempStr[17]);
              Edit7.Text := IntToStr(TempWord);
              TempWord := GetWord(@TempStr[15]);
              Edit8.Text := IntToStr(TempWord);
              Label1.Caption := 'Bytes: Block';
           End;
     End;
  End;

  Case NoteBook1.PageIndex of

        0: NoteBook1.Height := Label2.Height + Edit1.Height + Label6.Height + Bevel3.Height + CheckBox1.Height + Label3.Height + Edit2.Height + 32;
        1: NoteBook1.Height := Label2.Height + Edit1.Height + Label6.Height + Bevel3.Height + CheckBox1.Height + Label3.Height + Edit2.Height + 32;
        2: NoteBook1.Height := Label9.Height + Edit4.Height + 12;

  End;

  ClientHeight := Label1.Top + Label1.Height + 4 + Bevel1.Height + 4 + NoteBook1.Height + Button1.Height + 8;

end;

procedure TBlockProperties.CheckBox1Click(Sender: TObject);
Var
  TempWord: Word;
begin
  If CheckBox1.Checked Then Begin
     TempWord := GetWord(@CurBlock[17]);
     TempWord := TempWord And $7FFF;
     Edit2.Text := IntToStr(TempWord And $7FFF);
     Edit2.Enabled := True;
     Label3.Enabled := True;
  End Else Begin
     Edit2.Text := '';
     Edit2.Enabled := False;
     Label3.Enabled := False;
  End;
end;

procedure TBlockProperties.Button1Click(Sender: TObject);
begin
  Cancelled := False;
  Close;
end;

procedure TBlockProperties.Button2Click(Sender: TObject);
begin
  Cancelled := True;
  Close;
end;

procedure TBlockProperties.FormClose(Sender: TObject; var Action: TCloseAction);
Var
  TempStr: String;
  TempWord: Word;
begin
  If Not Cancelled Then Begin // Apply the changes
     Case CurBlock[4] of
        #0: // Program block
           Begin
              TempStr := Copy(Edit1.Text, 1, 10);
              While Length(TempStr) < 10 do TempStr := TempStr + ' ';
              CurBlock := Copy(CurBlock, 1, 4)+TempStr+Copy(CurBlock, 15, $FFFF);
              If CheckBox1.Checked Then Begin
                 If StrToIntDef(Edit2.Text, -1) <> -1 Then Begin
                    TempWord := StrToInt(Edit2.Text) and $7FFF;
                    PutWord(@CurBlock[17], TempWord);
                 End Else Begin
                    Action := caNone;
                 End;
              End Else Begin
                 TempWord := GetWord(@CurBlock[17]);
                 TempWord := TempWord Or $8000;
                 PutWord(@CurBlock[17], TempWord);
              End;
           End;
        #3: // CODE block
           Begin
              TempStr := Copy(Edit3.Text, 1, 10);
              While Length(TempStr) < 10 do TempStr := TempStr + ' ';
              CurBlock := Copy(CurBlock, 1, 4)+TempStr+Copy(CurBlock, 15, $FFFF);
              If StrToIntDef(Edit7.Text, -1) <> -1 Then Begin
                 TempWord := StrToInt(Edit7.Text) and $FFFF;
                 PutWord(@CurBlock[17], TempWord);
              End Else Begin
                 Action := caNone;
              End;
           End;
     #1,#2: // Variable block
           Begin
              TempStr := Copy(Edit4.Text, 1, 10);
              While Length(TempStr) < 10 do TempStr := TempStr + ' ';
              CurBlock := Copy(CurBlock, 1, 4)+TempStr+Copy(CurBlock, 15, $FFFF);
           End;
     End;
     TapeBlocks[CurIndex] := CurBlock;
     RecalcChecksum(CurIndex);
  End;
end;

procedure TBlockProperties.Button3Click(Sender: TObject);
begin
  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_tape_block_properties.html'), HH_DISPLAY_TOPIC, 0);
end;

end.


