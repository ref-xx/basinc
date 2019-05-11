unit MessageBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMessageForm = class(TForm)
    Label1: TLabel;
  private
    { Private declarations }
    RunningEmu: Boolean;
  public
    { Public declarations }
    Procedure DoMessage(Cap, Text: String);
    Procedure ClearMessage;
  end;

var
  MessageForm: TMessageForm;

implementation

uses BasinMain, FastCore, Utility;

{$R *.DFM}

Procedure TMessageForm.DoMessage(Cap, Text: String);
Begin
  Caption := Cap;
  Label1.Caption := Text;
  RunningEmu := Registers.EmuRunning;
  ControlEmulation(False);
  BASinOutput.Enabled := False;
  CentreFormOnForm(Self, nil);
  Show;
End;

Procedure TMessageForm.ClearMessage;
Begin
  BASinOutput.Enabled := True;
  ControlEmulation(RunningEmu);
  Close;
End;

end.
