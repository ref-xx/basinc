unit GOSUB;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TGOSUBWindow = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure PushOne(Line, Statement: Integer);
    Procedure PopOne;
    Procedure ClearStack;
    Procedure BuildStack;
  end;

var
  GOSUBWindow: TGOSUBWindow;

implementation

{$R *.DFM}

Uses ROMUtils, FastCore, BasinMain, Utility;

Procedure TGOSUBWindow.PushOne(Line, Statement: Integer);
Begin
  ListBox1.Items.Insert(0, IntToStr(Line)+':'+IntToStr(Statement));
  If ListBox1.Items.Count > 1 Then
     Label2.Caption := IntToStr(ListBox1.Items.Count)+' Items'
  Else
     If ListBox1.Items.Count = 1 Then
        Label2.Caption := '1 Item'
     Else
        Label2.Caption := 'No Items';
  Button2.Enabled := ListBox1.ItemIndex <> -1;
End;

Procedure TGOSUBWindow.PopOne;
Begin
  ListBox1.Items.Delete(0);
  If ListBox1.Items.Count > 1 Then
     Label2.Caption := IntToStr(ListBox1.Items.Count)+' Items'
  Else
     Label2.Caption := '1 Item';
  Button2.Enabled := ListBox1.ItemIndex <> -1;
End;

Procedure TGOSUBWindow.ClearStack;
Begin
  ListBox1.Items.BeginUpdate;
  ListBox1.Items.Clear;
  Label2.Caption := '0 Items';
  ListBox1.Items.EndUpdate;
  Button2.Enabled := ListBox1.ItemIndex <> -1;
End;

Procedure TGOSUBWindow.BuildStack;
Var
  Ptr, Idx: Integer;
Begin
  ListBox1.Items.BeginUpdate;
  ListBox1.Items.Clear;
  Ptr := GOSUBStackPos -1;
  For Idx := 1 to GOSUBStackSize Do Begin
     ListBox1.Items.Add(IntToStr(GetWord(@Memory[Ptr]))+':'+IntToStr(Memory[Ptr+2] -1));
     Inc(Ptr, 3);
  End;
  If ListBox1.Items.Count > 1 Then
     Label2.Caption := IntToStr(ListBox1.Items.Count)+' Items'
  Else
     Label2.Caption := '1 Item';
  Button2.Enabled := ListBox1.ItemIndex <> -1;
  ListBox1.Items.EndUpdate;
End;

procedure TGOSUBWindow.FormCreate(Sender: TObject);
begin
  Label1.SetBounds(8, 8, Label1.Width, Label1.Height);
  Label2.SetBounds(Label1.Left + Label1.Width + 8, Label1.Top, ClientWidth - 24 - Label1.Width, Label1.Height); 
  Button1.SetBounds(ClientWidth - (Button1.Width + 8), ClientHeight - (Button1.Height + 8), Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - (Button2.Width + 4), Button1.Top, Button2.Width, Button2.Height);
  ListBox1.SetBounds(8, Label1.Top + Label1.Height + 4, ClientWidth - 16, ClientHeight - 28 - Button1.Height - Label1.Height);
  Button2.Enabled := ListBox1.ItemIndex <> -1;
  Button3.SetBounds(8, Button1.Top, Button3.Width, Button1.Height);
end;

procedure TGOSUBWindow.FormShow(Sender: TObject);
begin
  BuildStack;
end;

procedure TGOSUBWindow.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TGOSUBWindow.Button2Click(Sender: TObject);
begin
  If ListBox1.ItemIndex <> -1 Then
     BASinOutput.FindandActivateLine(StrToInt(Copy(ListBox1.Items[ListBox1.ItemIndex], 1, Pos(':', ListBox1.Items[ListBox1.ItemIndex])-1)), StrToInt(Copy(ListBox1.Items[ListBox1.ItemIndex], Pos(':', ListBox1.Items[ListBox1.ItemIndex])+1, 999999)));
end;

procedure TGOSUBWindow.ListBox1Click(Sender: TObject);
begin
  Button2.Enabled := ListBox1.ItemIndex <> -1;
end;

procedure TGOSUBWindow.Button3Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_gosub_stack.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
