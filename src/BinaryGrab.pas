unit BinaryGrab;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Utility;

type
  TBinaryGrabWindow = class(TForm)
    Label5: TLabel;
    Bevel1: TThemeBevel;
    Label6: TLabel;
    Edit4: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Button3: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BlockAddress,
    BlockSize: Integer;
    Cancelled: Boolean;
  end;

var
  BinaryGrabWindow: TBinaryGrabWindow;

implementation

{$R *.DFM}

Uses UDGEdit, Evaluate, BASinMain;

procedure TBinaryGrabWindow.FormShow(Sender: TObject);
begin

  Cancelled := True;

end;

procedure TBinaryGrabWindow.Button2Click(Sender: TObject);
begin

  Close;

end;

procedure TBinaryGrabWindow.Edit4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  If Key = VK_RETURN Then
     Button1Click(Nil);

end;

procedure TBinaryGrabWindow.Button1Click(Sender: TObject);
Var
  Val1, Val2: Integer;
begin
  // Check for valid grid sizes
  Val1 := Round(EvaluateNum(Edit4.Text, -1));
  If (Val1 < 0) or (Val1 > 65535) Then Begin
     MessageBox(Handle, pChar('Valid block addresses are in the range 0 to 65535'), pChar('Invalid Address'), MB_OK or MB_ICONWARNING);
     Edit4.SetFocus;
     Exit;
  End;

  Val2 := Round(EvaluateNum(Edit2.Text, -1));
  If (Val2 < 1) or (Val2 > 65535) Then Begin
     MessageBox(Handle, pChar('Valid block sizes are in the range 1 to 65535 bytes'), pChar('Invalid block size'), MB_OK or MB_ICONWARNING);
     Edit2.SetFocus;
     Exit;
  End;

  BlockAddress := Val1;
  BlockSize := Val2;
  Cancelled := False;
  Close;

end;

procedure TBinaryGrabWindow.Button3Click(Sender: TObject);
begin
  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_block_grabber.html'), HH_DISPLAY_TOPIC, 0)
end;

end.

