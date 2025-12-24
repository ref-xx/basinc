unit BasicMergeWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TBasicMergeForm = class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    CheckBox4: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    CheckBox5: TCheckBox;
    Label3: TLabel;
    Edit3: TEdit;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OkayPressed, ProcessData, ProcessPrint, RemoveRemarks: Boolean;
    MergeStart, MergeEnd, MergeMax: Word;
  end;

var
  BasicMergeForm: TBasicMergeForm;

implementation
Uses ROMUtils, BasinMain, Utility;


{$R *.dfm}
procedure TBasicMergeForm.Button1Click(Sender: TObject);
begin
  // User cancelled the dialog
  OkayPressed := False;
  ModalResult := mrCancel;
end;

procedure TBasicMergeForm.Button2Click(Sender: TObject);
var
  LStart, LEnd: Integer;
begin
  // Reset warning color each time user tries again
  CheckBox4.Color := clBtnFace;

  // Safety confirmation must be checked
  if not CheckBox4.Checked then
  begin
    CheckBox4.Color := clRed; // Visual warning
    Exit;
  end;

  // Read options from UI into form fields
  RemoveRemarks := CheckBox5.Checked; // Remove REM lines
  ProcessData   := CheckBox1.Checked; // Join DATA statements
  ProcessPrint  := CheckBox2.Checked; // Further process PRINT / DATA

  // Parse line range
  LStart := StrToIntDef(Trim(Edit1.Text), 0);
  LEnd   := StrToIntDef(Trim(Edit2.Text), 9999);

  // Clamp to sensible word range (Spectrum line numbers)
  if LStart < 0 then LStart := 0;
  if LEnd   < 0 then LEnd   := 0;
  if LStart > 9999 then LStart := 9999;
  if LEnd   > 9999 then LEnd   := 9999;

  // Ensure start <= end
  if LStart > LEnd then
  begin
    // swap values
    LStart := LStart xor LEnd;
    LEnd   := LStart xor LEnd;
    LStart := LStart xor LEnd;
  end;

  MergeMax:= StrToIntDef(Trim(Edit3.Text), 249);

  MergeStart := LStart;
  MergeEnd   := LEnd;

  OkayPressed := True;
  ModalResult := mrOk;
end;

procedure TBasicMergeForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  SkipDATA, JoinSame, KeepBreakpoints: Boolean;
begin
  // Only run when user pressed Minify! (OK)
  if not OkayPressed then
    Exit;

  // Provide a default max line length if not set elsewhere
  if MergeMax < 32 then
    MergeMax := 249; // Typical ZX Spectrum BASIC line length limit

  // Remove all REM comments if requested
  if RemoveRemarks then
    RemoveCommentsFromBasic;

  // Map UI options to MergeBASICLines flags
  SkipDATA        := not CheckBox2.Checked; // If "Further process PRINT and DATA" is off, skip DATA
  JoinSame        := CheckBox1.Checked;     // Join DATA statements
  KeepBreakpoints := CheckBox3.Checked;     // Do not touch breakpoint lines

  // Perform the actual merge/minify on the BASIC source
  MergeBASICLines(
    MergeMax,
    MergeStart,
    MergeEnd,
    SkipDATA,
    JoinSame,
    KeepBreakpoints
  );

  // Re-tokenise and push back to the editor / program window
  BASinOutput.TokeniseEditText(False);
  //PostMessage(BASinOutput.Handle, WM_UPDATEPROGRAM, 0, 0);
  BASinOutput.GetBASIC;
end;


procedure TBasicMergeForm.Button3Click(Sender: TObject);
begin
 BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_clean_code.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
