unit UDGOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Utility;

type
  TUDGNew = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label5: TLabel;
    Bevel1: TBevel;
    Label6: TLabel;
    Edit4: TEdit;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NumGFX,
    WidthGFX,
    HeightGFX: DWord;
    OkayPressed: Boolean;
  end;

var
  UDGNew: TUDGNew;

implementation

{$R *.DFM}

Uses Evaluate, UDGEdit, BASinMain;

procedure TUDGNew.Button1Click(Sender: TObject);
Var
  TempVal: Integer;
Begin
  TempVal := Round(EvaluateNum(Edit4.Text, -1));
  If (TempVal < 1) or (TempVal > 256) Then Begin
     MessageBeep(MB_ICONEXCLAMATION);
     Edit4.SetFocus;
     Exit;
  End;
  NumGFX := TempVal;
  TempVal := Round(EvaluateNum(Edit2.Text, -1));
  If (TempVal < 1) or (TempVal > 9999) Then Begin
     MessageBeep(MB_ICONEXCLAMATION);
     Edit2.SetFocus;
     Exit;
  End;
  WidthGFX := TempVal;
  TempVal := Round(EvaluateNum(Edit1.Text, -1));
  If (TempVal < 1) or (TempVal > 9999) Then Begin
     MessageBeep(MB_ICONEXCLAMATION);
     Edit1.SetFocus;
     Exit;
  End;
  HeightGFX := TempVal;
  OkayPressed := True;
  Close;
end;

procedure TUDGNew.Button2Click(Sender: TObject);
begin
  OkayPressed := False;
  Close;
end;

procedure TUDGNew.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  If Not OkayPressed Then NumGFX := 0;
end;

procedure TUDGNew.FormShow(Sender: TObject);
begin
  OkayPressed := False;
end;

procedure TUDGNew.Button3Click(Sender: TObject);
begin
  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_udg_create_new.html'), HH_DISPLAY_TOPIC, 0)
end;

end.
