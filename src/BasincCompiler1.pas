unit BasincCompiler1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormCompiler1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Button5: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCompiler1: TFormCompiler1;

implementation

uses
  ROMUtils, FastCore, Filing, BasinMain, Utility;

{$R *.dfm}

procedure TFormCompiler1.FormShow(Sender: TObject);
begin
     Memo1.Text:= 'Ready to compile.';
     If GetWord(@Memory[PROG]) = GetWord(@Memory[VARS]) Then Begin
        Memo1.Text:= 'There is currently no BASIC program in memory.';
        Exit;


     End;
end;

procedure TFormCompiler1.Button1Click(Sender: TObject);
begin
   // Load MCODER3 loader and part 1, jump to $4BDB and run.
   ControlEmulation(False);
   Filename := BASinDir+'\48.Rom';
   LoadROM(Memory); //disable all traps.
   CopyMemory(@Memory[23536], @MCODER3_PART0_LOADER[0], SizeOf(MCODER3_PART0_LOADER));

   //Trap Mcoder entry point
   Romutils.Old07f5:= GetWord(@Memory[$07f5]);
   PutWord(@Memory[$07f5], $00ED);
   Registers.SP:=$5BF2;
   Registers.PC:=$5c00;
   Memo1.Text:= Memo1.Text+ #13#10 +'Compiling...';
   ControlEmulation(True);

end;

end.
