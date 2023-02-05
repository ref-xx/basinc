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
    Button2: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
 If (length(edit1.text)>255) Then Exit;

  for x:=1 to length(edit1.text) do begin //as slot 0 is reserved by Index Variable, it starts from 1.
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

procedure TConsoleOutForm.Button2Click(Sender: TObject);
begin

   ConsoleOutForm.memo1.Clear;
   ConsoleOutForm.memo1.Lines.add ('SimpleCON Help -- Please See examples in snippets and examples/basinC folder');
   ConsoleOutForm.memo1.Lines.add (' ');
   ConsoleOutForm.memo1.Lines.add ('SimpleCon Has 255bytes of memory and 1 byte of index to point those 255bytes. Simple Con has two ports: 1259 and 1515. To point a memory location use OUT 1259,Index. PORT 1515 can be used reading or writing data from/to that pointer.');
   ConsoleOutForm.memo1.Lines.add (' ');
   ConsoleOutForm.memo1.Lines.add ('Use OUT 1515,Data to read memory location previously set by Port 1259. Port 1515 AUTOMATICALLY increases port1259 index after you set or read from it.');
   ConsoleOutForm.memo1.Lines.add (' ');
   ConsoleOutForm.memo1.Lines.add ('Simplecon 255byte Memory is viewed or updated using Console I/O window single line textbox. To copy console line into console log, set index to 0 using OUT 1259,0. This will feed the text into log, and moves index to 1.');
    //MessageBox(Handle, pChar('message', pChar('SimpleCon V2 Help 1/3'), MB_OK or MB_ICONINFORMATION)
end;

procedure TConsoleOutForm.FormCreate(Sender: TObject);
var
x: byte;
begin
  
end;

end.











