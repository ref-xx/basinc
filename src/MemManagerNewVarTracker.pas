unit MemManagerNewVarTracker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TMemManagerVarTracker = class(TForm)
    Edit1: TEdit;
    LabelName: TLabel;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit3: TEdit;
    ButtonGetCursor: TButton;
    TrackBarSize: TTrackBar;
    Button1: TButton;
    CheckTrack: TCheckBox;
    Button2: TButton;
    procedure TrackBarSizeChange(Sender: TObject);
    procedure ButtonGetCursorClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FDefaultAddress: Integer;
  public
    TrackerName: String;
    TrackerAddress: Integer;
    TrackerSize: Integer;
    TrackerEnabled: Boolean;
    function Execute(ADefaultAddress: Integer): Boolean;
  end;

var
  MemManagerVarTracker: TMemManagerVarTracker;

implementation

{$R *.dfm}

function TMemManagerVarTracker.Execute(ADefaultAddress: Integer): Boolean;
begin
  FDefaultAddress := ADefaultAddress;
  Edit2.Text := IntToStr(ADefaultAddress);
  TrackBarSize.Position := 1;
  Edit3.Text := '1';
  CheckTrack.Checked := True;
  ActiveControl := Edit1;
  Edit1.SelectAll;
  Result := ShowModal = mrOk;
end;

procedure TMemManagerVarTracker.TrackBarSizeChange(Sender: TObject);
begin
  Edit3.Text := IntToStr(TrackBarSize.Position);
end;

procedure TMemManagerVarTracker.ButtonGetCursorClick(Sender: TObject);
begin
  Edit2.Text := IntToStr(FDefaultAddress);
end;

procedure TMemManagerVarTracker.Button1Click(Sender: TObject);
var
  AddressValue, SizeValue: Integer;
begin
  TrackerName := Trim(Edit1.Text);
  AddressValue := StrToIntDef(Trim(Edit2.Text), -1);
  SizeValue := StrToIntDef(Trim(Edit3.Text), -1);

  if TrackerName = '' then
  begin
    ShowMessage('Please enter a variable name.');
    Edit1.SetFocus;
    Exit;
  end;

  if (SizeValue < 1) or (SizeValue > 8) then
  begin
    ShowMessage('Variable size must be between 1 and 8 bytes.');
    Edit3.SetFocus;
    Exit;
  end;

  if (AddressValue < 0) or ((AddressValue + SizeValue) > 65536) then
  begin
    ShowMessage('The variable address and size must fit in memory (0-65535).');
    Edit2.SetFocus;
    Exit;
  end;

  TrackerAddress := AddressValue;
  TrackerSize := SizeValue;
  TrackerEnabled := CheckTrack.Checked;
  ModalResult := mrOk;
end;

procedure TMemManagerVarTracker.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
