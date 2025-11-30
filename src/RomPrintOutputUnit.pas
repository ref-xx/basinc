unit RomPrintOutputUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FastCore, ROMUtils;

type
  TRomPrintOutputWindow = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure InitSpectrumScreen;
    procedure ScrollOneUp;
    procedure SetTrap;
    procedure RemoveTrap;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RomPrintOutputWindow: TRomPrintOutputWindow;

implementation

{$R *.dfm}

procedure TRomPrintOutputWindow.FormCreate(Sender: TObject);
begin
     InitSpectrumScreen;
     Memo1.ReadOnly := True;
     Memo1.WordWrap := False;
     
end;

procedure TRomPrintOutputWindow.ScrollOneUp;
begin
  with RomPrintOutputWindow.Memo1.Lines do
  begin
    if Count > 0 then
    begin
      Delete(0);                       // ilk satiri sil
      Add(StringOfChar(' ', 32));      // en alta bos satir ekle (32 sütunluk)
    end;
  end;
end;


procedure TRomPrintOutputWindow.InitSpectrumScreen;
var
  r: Integer;
  s: string;
begin
  RomPrintOutputWindow.Memo1.Font.Name := 'Courier New'; // monospaced
  RomPrintOutputWindow.Memo1.Font.Size := 10;

  RomPrintOutputWindow.Memo1.Lines.BeginUpdate;
  try
    RomPrintOutputWindow.Memo1.Clear;
    s := StringOfChar(' ', 32); // 32 cols
    for r := 0 to 23 do
      RomPrintOutputWindow.Memo1.Lines.Add(s); // 24 rows
  finally
    RomPrintOutputWindow.Memo1.Lines.EndUpdate;
  end;
end;

procedure TRomPrintOutputWindow.Button1Click(Sender: TObject);
begin
InitSpectrumScreen;
end;

procedure TRomPrintOutputWindow.Button2Click(Sender: TObject);
begin
            RomUtils.TrapPrint:=Not RomUtils.TrapPrint;

            if RomUtils.TrapPrint Then Begin

                SetTrap;

                button2.Caption:='Stop Capture';

                //ShowWindow(RomPrintOutputWindow, False);

            End else begin
                    RemoveTrap;

                    button2.Caption:='Start Capture';

                   // RomPrintOutputWindow.Close;

            End;
end;

Procedure TRomPrintOutputWindow.SetTrap;
Begin
                putWord(@Rom48k[$0BC5], $00ED); //ROM TRAP SET ON PRINT
                putWord(@Memory[$0BC5], $00ED);

                putWord(@Rom48k[$0DAF], $00ED); //ROM TRAP SET ON CLS
                putWord(@Memory[$0DAF], $00ED);

                putWord(@Rom48k[$0E3B], $00ED); //ROM TRAP SET ON SCROLL?
                putWord(@Memory[$0E3B], $00ED);
                RomPrintOutputWindow.Caption:='Screen Text [Capturing...]';
                RomUtils.TrapPrint:=True;

End;

procedure TRomPrintOutputWindow.RemoveTrap;
Begin
                    putWord(@Rom48k[$0BC5], $25EB); //ROMTRAP REMOVE see above.
                    putWord(@Memory[$0BC5], $25EB);

                    putWord(@Rom48k[$0DAF], $0021);  //ROM TRAP unSET ON CLS
                    putWord(@Memory[$0DAF], $0021);

                    putWord(@Rom48k[$0E3B], $E021);  //ROM TRAP unSET ON SCROLL?
                    putWord(@Memory[$0E3B], $E021);
                    RomPrintOutputWindow.Caption:='Screen Text Capture [Inactive]';
                    RomUtils.TrapPrint:=False;
End;

procedure TRomPrintOutputWindow.FormShow(Sender: TObject);
begin
 if RomUtils.TrapPrint Then Begin
  button2.Caption:='Stop Capture';
 End else begin
 button2.Caption:='Start Capture';
  End;
end;

procedure TRomPrintOutputWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
      RemoveTrap;
end;

procedure TRomPrintOutputWindow.Button3Click(Sender: TObject);
begin
     RomPrintOutputWindow.Close;
end;

end.
