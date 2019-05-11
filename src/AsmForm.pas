unit AsmForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, Utility;

type
  TAssembleForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label5: TLabel;
    Bevel3: TThemeBevel;
    Label4: TLabel;
    Edit2: TEdit;
    CheckBox2: TCheckBox;
    Label6: TLabel;
    Bevel4: TThemeBevel;
    Label7: TLabel;
    Edit3: TEdit;
    Label8: TLabel;
    Edit4: TEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     BytesPerLine, StartAt, Step: Integer;
     IncludePOKEs: Boolean;
     Cancelled: Boolean;
  end;

var
  AssembleForm: TAssembleForm;

implementation

{$R *.DFM}

Uses Z80Assembler, CPUDisplay, AsmEditor, FastCore;

procedure TAssembleForm.Button2Click(Sender: TObject);
begin
  Cancelled := True;
  Close;
end;

procedure TAssembleForm.Button1Click(Sender: TObject);
Var
  Value: Integer;
begin

  Cancelled := False;

  Value := CPUWindow.GetValue(Edit2, 128, 'Bytes per line');
  If Value = -1 Then Begin
     Edit2.SetFocus;
     Exit;
  End;
  BytesPerLine := Value;

  Value := CPUWindow.GetValue(Edit3, 9000, 'Starting Line');
  If Value = -1 Then Begin
     Edit2.SetFocus;
     Exit;
  End;
  StartAt := Value;

  Value := CPUWindow.GetValue(Edit4, 100, 'Step');
  If Value < 1 Then Begin
     Edit2.SetFocus;
     Exit;
  End;
  Step := Value;

  IncludePOKES := Checkbox2.Checked;

  Close;

end;

procedure TAssembleForm.FormShow(Sender: TObject);
begin
  Cancelled := True;
end;

end.
