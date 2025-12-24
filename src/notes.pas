unit notes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Utility;

type
  TNotesWindow = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

uses BasinMain;

{$R *.DFM}

procedure TNotesWindow.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
If (Key = 90) and (Shift = [ssAlt, ssCtrl]) then begin
     Close;
End;



end;

procedure TNotesWindow.FormCreate(Sender: TObject);
begin
if Opt_ToolFontSize>0 Then Memo1.Font.Size:=Opt_ToolFontSize;
end;

procedure TNotesWindow.FormShow(Sender: TObject);
begin
if Opt_ToolFontSize>0 Then Memo1.Font.Size:=Opt_ToolFontSize;
end;

procedure TNotesWindow.Button2Click(Sender: TObject);
begin
BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_project_notes.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TNotesWindow.Button1Click(Sender: TObject);
begin
   Close;
end;

end.
