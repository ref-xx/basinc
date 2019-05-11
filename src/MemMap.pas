unit MemMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FastIMG, FastDIB, StdCtrls, ExtCtrls;

const
  UM_MEASUREFONTS = WM_USER;

type
  TMemMapWindow = class(TForm)
    FastIMG1: TFastIMG;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Timer1: TTimer;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1MeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
     Procedure UMMeasureFonts( Var msg: TMessage ); message
UM_MEASUREFONTS;

  public
    { Public declarations }
    SelAddrMin,
    SelAddrMax: DWord;
    Procedure DrawMemoryMap;
    Procedure GetSelectedRange;
  end;

var
  MemMapWindow: TMemMapWindow;

implementation

{$R *.DFM}

Uses FastCore, ROMUtils, BASinMain, Utility;

procedure TMemMapWindow.FormCreate(Sender: TObject);
begin
  FastIMG1.Bmp.SetSize(256, 256, 32);
  
  ComboBox1.ItemIndex := 0;

  PostMessage( handle, UM_MEASUREFONTS, 0, 0 );

  SelAddrMin := 0;
  SelAddrMax := 0;
end;

Procedure TMemMapWindow.DrawMemoryMap;
Var
  Bits, SMax, SMin: Pointer;
  MemAccPtr: Pointer;
Begin

  // Draw from the display file onwards.
  // nice greyscales for the data values...

  GetSelectedRange;
  SMax := @Memory[SelAddrMax];
  SMin := @Memory[SelAddrMin];
  Bits := Pointer(FastIMG1.Bmp.Bits);
  MemAccPtr := Pointer(@MemAccess[65280]);

  Asm

     Pushad                  // Uses esi, edi, eax, ebx, ecx, edx

     mov edi, bits           // The bitmap surface
     lea esi, Memory[65280]  // The emulated memory, at the start of the first row (65535-255)
     lea ecx, BitSetCount[0] // A LUT containing greyscale colours (32bit) for all possible 256 byte values
     xor eax, eax            // ensure eax is zero, for the look-up
     mov dh, 255             // 256 rows
     mov dl, 255             // 255 columns

  @NextPixel:

     mov al, [esi]           // Get the byte from memory
     mov ebx, [ecx+eax*4]    // Load ebx with the pixel colour for this byte

     cmp esi, SMax           // Have we hit the top of the highlight?
     jnc @NormalPixel        // and branch if this is a normal pixel

     cmp esi, SMin           // Are we still within the highlighted area?
     jc  @NormalPixel        // if not, it's a normal pixel, so branch

     or  ebx,  $00FF0000     // This pixel is highlighted, so modify the colour to make it red.

  @NormalPixel:

     mov eax, MemAccPtr
     mov al, [eax]
     test al, MemRead
     je  @NotRead

     or ebx, $0000FF00

  @NotRead:

     test al, MemWrite
     je @NotWritten

     or ebx, $00FF0000

  @NotWritten:

     test al, MovePC
     je @NotPC

     mov ebx, $000000FF

  @NotPC:

     xor eax, eax
     mov [edi], ebx          // Fill the pixel (32bit)
     add edi, 4              // and address the next

     sub dl, 1               // decrement the column counter
     inc esi                 // and increment the address
     inc MemAccPtr
     jnc @NextPixel          // loop if there's more columns to do

     sub esi, 512            // Drop back by two rows - we're at the end of a row now, and we want the previous row's start address
     sub MemAccPtr, 512
     sub dh, 1               // Decrement the row counter
     jnc @NextPixel          // dl is already at 255, so just loop if there's more rows to do

     popad                   // Restore registers

  End;

  FastIMG1.Repaint;

End;

procedure TMemMapWindow.FormShow(Sender: TObject);
begin
  DrawMemoryMap;
  Timer1.Enabled := True;
  
end;

Procedure TMemMapWindow.GetSelectedRange;
Begin
  Button1.Enabled := ComboBox1.ItemIndex = 11;
  Case ComboBox1.ItemIndex of
     0: // None
        Begin
           SelAddrMin := 0;
           SelAddrMax := 0;
        End;
     1: // ROM
        Begin
           SelAddrMin := 0;
           SelAddrMax := 16384;
        End;
     2: // Display File
        Begin
           SelAddrMin := 16384;
           SelAddrMax := 22528;
        End;
     3: // Attributes
        Begin
           SelAddrMin := 22528;
           SelAddrMax := 23296;
        End;
     4: // Printer Buffer
        Begin
           SelAddrMin := 23296;
           SelAddrMax := 23552;
        End;
     5: // System Variables
        Begin
           SelAddrMin := 23552;
           SelAddrMax := 23734;
        End;
     6: // Channels
        Begin
           SelAddrMin := 23734;
           SelAddrMax := GetWord(@Memory[PROG]);
        End;
     7: // BASIC Program
        Begin
           SelAddrMin := GetWord(@Memory[PROG]);
           SelAddrMax := GetWord(@Memory[VARS]);
        End;
     8: // Variables
        Begin
           SelAddrMin := GetWord(@Memory[VARS]);
           SelAddrMax := GetWord(@Memory[E_LINE]);
        End;
     9: // Edit Line
        Begin
           SelAddrMin := GetWord(@Memory[E_LINE]);
           SelAddrMax := GetWord(@Memory[WORKSP]);
        End;
    10: // WorkSpace
        Begin
           SelAddrMin := GetWord(@Memory[WORKSP]);
           SelAddrMax := GetWord(@Memory[STKBOT]);
        End;
    11: // Calculator Stack
        Begin
           SelAddrMin := GetWord(@Memory[STKBOT]);
           SelAddrMax := GetWord(@Memory[STKEND]);
        End;
    12: // Spare Space
        Begin
           SelAddrMin := GetWord(@Memory[STKEND]);
           SelAddrMax := Registers.SP;
        End;
    13: // Machine Stack
        Begin
           SelAddrMin := Registers.SP;
           SelAddrMax := GOSUBStackPos;
        End;
    14: // Gosub Stack
        Begin
           SelAddrMin := GOSUBStackPos;
           SelAddrMax := GOSUBStackPos+(GOSUBStackSize*3);
        End;
    15: // UDGs
        Begin
           SelAddrMin := GetWord(@Memory[UDG]);
           SelAddrMax := 65535;
        End;
  End;
End;

procedure TMemMapWindow.Timer1Timer(Sender: TObject);
begin
  If Not Registers.EmuRunning Then
     DrawMemoryMap;
end;

procedure TMemMapWindow.ComboBox1Change(Sender: TObject);
begin
  DrawMemoryMap;
end;

procedure TMemMapWindow.ComboBox1MeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
begin
  If FastIMG1.Bmp.Width <> 0 Then
     DrawMemoryMap;
  Height := (Control As TCombobox).ItemHeight+4;

end;

procedure TMemMapWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
end;

procedure TMemMapWindow.Button1Click(Sender: TObject);
Var
  Idx: Integer;
begin
  For Idx := SelAddrMin to SelAddrMax -1 Do Memory[Idx] := 0;
  DrawMemoryMap;
end;

procedure TMemMapWindow.Button2Click(Sender: TObject);
begin

  HtmlHelp(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_memory_map.html'), HH_DISPLAY_TOPIC, 0);

end;

procedure TMemMapWindow.ComboBox1DrawItem(Control: TWinControl; Index:
Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  cb: TCombobox;
begin
  cb:= Control As TCombobox;
  cb.Canvas.FillRect( rect );
  If (index >= 0) and (index < cb.Items.Count) Then Begin
    If odComboBoxEdit In State Then Begin
      // Draw the edit portion of the control, use the controls
      // design-time font for this since the edit control is
      // fixed height and drawing a bunch of symbols if the selected
      // font is Symbol etc. is not very informative for the user.
      cb.Canvas.Font := cb.Font;
      If odSelected In State Then
       cb.Canvas.Font.Color := clHighlightText;
    End
    Else Begin
      cb.Canvas.Font.Name := cb.Items[index];
      cb.Canvas.Font.Size := 10;
    End;
    cb.Canvas.TextRect( Rect, rect.left+2, rect.top+2, cb.Items[index]
);
  End;
end;

procedure TMemMapWindow.UMMeasureFonts(var msg: TMessage);
var
  i: Integer;
begin
  // use form canvas for measurements
  canvas.font.size := 10;
  For i := 0 To combobox1.items.count - 1 Do Begin
    canvas.font.name := combobox1.items[i];
    combobox1.perform( CB_SETITEMHEIGHT,
                       i,
                       canvas.TextHeight(combobox1.items[i]));
  End; { For }
 
end;


end.
