unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FastIMG, FastDIB, TransparentPanel;

type
  TAboutBox = class(TForm)
    FastIMG2: TFastIMG;
    Panel1: TPanel;
    Label6: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}

Uses Utility;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  //ClientWidth := FastIMG1.Width + (FastIMG1.Left *2);
 // ClientHeight := FastIMG1.Height + (FastIMG1.Top * 2);
  Button1.SetBounds(ClientWidth - Button1.Width - 8, ClientHeight - Button1.Height - 8, Button1.Width, Button1.Height);
end;

procedure TAboutBox.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormShow(Sender: TObject);
begin
  Label1.Caption := ReleaseBuild;
  Label6.Caption := 'BasinC is distributed as Freeware. ' + ReleaseDate;

end;




end.
