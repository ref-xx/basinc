unit notes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TNotesWindow = class(TForm)
    Memo1: TMemo;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    Changed,
    Edited: Boolean;
    Memo: AnsiString;

  end;

var
  NotesWindow: TNotesWindow;

implementation

{$R *.DFM}

procedure TNotesWindow.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
If (Key = 90) and (Shift = [ssAlt, ssCtrl]) then begin
     Close;
End;



end;

end.
