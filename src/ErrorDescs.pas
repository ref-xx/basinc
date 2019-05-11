unit ErrorDescs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TErrorDescriptions = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ErrorDescriptions: TErrorDescriptions;

implementation

{$R *.DFM}

Uses ROMUtils, Utility, BASinMain;

procedure TErrorDescriptions.FormCreate(Sender: TObject);
begin
  ComboBox1.SetBounds(ClientWidth - 8 - ComboBox1.Width, 8, ComboBox1.Width, ComboBox1.Height);
  Label1.SetBounds(8, 12, Label1.Width, Label1.Height);
  Label2.SetBounds(8, ComboBox1.Top + ComboBox1.Height + 8, ClientWidth -16, ClientHeight - ComboBox1.Height - Button2.Height- 32);
  Button2.SetBounds(ClientWidth - 8 - Button2.Width, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Left - Button1.Width - 4, Button2.Top, Button1.Width, Button2.Height);
end;

procedure TErrorDescriptions.FormShow(Sender: TObject);
begin
  ComboBox1.ItemIndex := LastError;
  ComboBox1Change(nil);
end;

procedure TErrorDescriptions.ComboBox1Change(Sender: TObject);
begin
  If ComboBox1.ItemIndex <> -1 Then
     Label2.Caption := Errors[ComboBox1.ItemIndex][3]+#13#13+'Situation: '+Errors[ComboBox1.ItemIndex][4]
  Else
     Label2.Caption := '';
end;

procedure TErrorDescriptions.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TErrorDescriptions.Button1Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_error_help.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
