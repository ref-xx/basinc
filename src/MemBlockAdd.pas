unit MemBlockAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMemBlockWindow = class(TForm)
    Label1:   TLabel;
    Label2:   TLabel;
    Edit1:    TEdit;
    Edit2:    TEdit;
    Button1:  TButton;
    Button2:  TButton;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    StartAddr, DataLen: Word;
  end;

var
  MemBlockWindow: TMemBlockWindow;

implementation

{$R *.DFM}

Uses Evaluate, BASinMain, Utility;

procedure TMemBlockWindow.FormShow(Sender: TObject);
begin
  Edit1.Text := '';
  Edit2.Text := '';
end;

procedure TMemBlockWindow.Button1Click(Sender: TObject);
begin
  If Round(EvaluateNum(Edit1.Text, -1)) = -1 Then Begin
     MessageBeep(MB_ICONEXCLAMATION);
     Edit1.SetFocus;
     Exit;
  End;
  If Round(EvaluateNum(Edit2.Text, -1)) = -1 Then Begin
     MessageBeep(MB_ICONEXCLAMATION);
     Edit2.SetFocus;
     Exit;
  End;
  StartAddr := Round(EvaluateNum(Edit1.Text, -1));
  DataLen := Round(EvaluateNum(Edit2.Text, -1));
  Close;
end;

procedure TMemBlockWindow.Button2Click(Sender: TObject);
begin
  StartAddr := 0;
  DataLen := 0;
  Close;
end;

procedure TMemBlockWindow.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then Begin
     Key := 0;
     Button1Click(Self);
  End;
end;

procedure TMemBlockWindow.Button3Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_add_memory_block.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
