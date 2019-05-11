unit ConsoleOutput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Utility;

type
  TConsoleOutForm = class(TForm)
    Edit1: TEdit;
    Clear: TButton;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConsoleOutForm: TConsoleOutForm;

implementation

{$R *.DFM}

procedure TConsoleOutForm.Edit1Change(Sender: TObject);
var
x: byte;
begin
  for x:=1 to length(edit1.text) do begin
      ConsoleAddon[x]:=ord(edit1.text[x]);

  end;
end;

procedure TConsoleOutForm.ClearClick(Sender: TObject);
var
x: byte;
begin
  for x:=1 to length(edit1.text) do begin
  ConsoleAddon[x]:=ord(' ');
  edit1.clear();
  end;
end;

procedure TConsoleOutForm.Button1Click(Sender: TObject);
begin
ConsoleOutForm.memo1.Clear;
end;

end.











