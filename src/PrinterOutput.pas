unit PrinterOutput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Utility,
  StdCtrls, Buttons, FastIMG, ExtCtrls, FastDIB, Math, FastCore, Menus;

type
  TPrinterForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Panel1: TPanel;
    FastIMG1: TFastIMG;
    ScrollBar1: TScrollBar;
    BitBtn3: TBitBtn;
    PopupMenu1: TPopupMenu;
    DisplayAreaBW1: TMenuItem;
    DisplayAndAttrsColour1: TMenuItem;
    BitBtn4: TBitBtn;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure DisplayAreaBW1Click(Sender: TObject);
    procedure DisplayAndAttrsColour1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    ScrollBox1: TNewScrollBox;
    procedure ScrollBox1VScroll(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType);
  public
    { Public declarations }
    function  PrinterIn: Byte;
    Function  RenderAsDIB: TFastDIB;
    procedure PrinterOut(Value: Byte);
    Procedure SetPrinterPixel(XPos: Integer);
    Procedure BytesToPrinter(Address, Count: Word);
    procedure DrawPrinterOutput;
    procedure InitPrinter(LoadBin: Boolean);
    procedure NewLine;
  end;

var
  PrinterForm: TPrinterForm;
  PrinterArray: Array Of Byte;
  PrinterMotorState: DWord;
  PrinterEncoderState: DWord;
  PrinterXPos: Integer;
  PrinterStylusActive: Boolean;
  PrinterPageWidth: Integer;
  PrinterBase: Integer;
  PrinterUpdateTs: DWord;
  PrinterTs: DWord;

  procedure UpdatePrinter;

implementation

uses BasinMain, Filing, ROMUtils, Sound, Printing;

{$R *.DFM}

procedure TPrinterForm.FormCreate(Sender: TObject);
Var
  MinWidth: Integer;
begin

  MinWidth := 256 + 16 + 64 + GetSystemMetrics(SM_CXVSCROLL) + (GetSystemMetrics(SM_CXEDGE) *2);
  ClientWidth := MinWidth;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;

  BitBtn1.SetBounds(8, ClientHeight - BitBtn1.Height - 8, BitBtn1.Width, BitBtn1.Height);
  BitBtn2.SetBounds(BitBtn1.Width + 12, ClientHeight - BitBtn2.Height - 8, BitBtn2.Width, BitBtn2.Height);
  Button1.SetBounds(ClientWidth - Button1.Width - 8, BitBtn1.Top, Button1.Width, Button1.Height);
  Button2.SetBounds(Button1.Left - Button2.Width - 4, Button1.Top, Button2.Width, Button1.Height);
  BitBtn4.SetBounds(BitBtn2.Left + BitBtn2.Width + 8, BitBtn2.Top, BitBtn4.Width, BitBtn4.Height);
  BitBtn3.SetBounds(BitBtn4.Left + BitBtn4.Width + 8, BitBtn4.Top, BitBtn3.Width, BitBtn3.Height);
  
  Panel1.SetBounds(8, 8, ClientWidth - 16, ClientHeight - Button1.Height - 24);

  ScrollBox1 := TNewScrollBox.Create(Self);
  ScrollBox1.Parent := Panel1;
  ScrollBox1.SetBounds(0, 0, Panel1.ClientWidth, Panel1.ClientHeight);
  ScrollBox1.Anchors := [AkLeft, AkTop, AkBottom, AkRight];
  ScrollBox1.AutoScroll := False;
  ScrollBox1.OnVerticalScroll := ScrollBox1VScroll;
  ScrollBox1.SendToBack;
  ScrollBox1.VertScrollBar.Visible := True;

  ScrollBar1.Parent := ScrollBox1;
  ScrollBar1.Align := AlRight;

  FastIMG1.SetBounds(ScrollBox1.Left + GetSystemMetrics(SM_CXEDGE), ScrollBox1.Top + GetSystemMetrics(SM_CYEDGE), 320, ScrollBox1.ClientHeight);
  FastIMG1.Anchors := [AkLeft, AkBottom];

  InitPrinter(True);

end;

Procedure TPrinterForm.InitPrinter(LoadBin: Boolean);
Var
  FS: TFileStream;
Begin

  PrinterPageWidth := 256;
  PrinterUpdateTs := 64;
  PrinterTs := 0;
  SetLength(PrinterArray, 0);
  NewLine;
  PrinterBase := 0;

  If LoadBin and Opt_SavePrinting Then Begin

     If FileExists(BASinDir + '\Printer.bin') Then Begin

        FS := TFileStream.Create(BASinDir + '\Printer.bin', fmOpenRead or fmShareDenyNone);
        SetLength(PrinterArray, FS.Size);
        FS.Read(PrinterArray[0], FS.Size);
        PrinterBase := ((Length(PrinterArray) Div PrinterPageWidth) * PrinterPageWidth) - PrinterPageWidth;
        ScrollBox1.VertScrollBar.Range := PrinterBase Div PrinterPageWidth;
        ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Range - FastIMG1.Bmp.AbsHeight;
        FS.Free;

     End;

  End;

End;

procedure TPrinterForm.ScrollBox1VScroll(Sender: TObject; Pos: SmallInt; EventType: TVScrollEventType);
begin

  If EventType = vsThumbTrack Then
     ScrollBox1.VertScrollBar.Position := ScrollBox1.ScrollInfo.nTrackPos
  Else
     ScrollBox1.VertScrollBar.Position := ScrollBox1.ScrollInfo.nPos;  
  DrawPrinterOutput;

end;

procedure TPrinterForm.FormResize(Sender: TObject);
begin

  If ScrollBox1 <> nil Then Begin

     If FastIMG1 <> nil Then Begin
        FastIMG1.SetBounds(ScrollBox1.Left + GetSystemMetrics(SM_CXEDGE), ScrollBox1.Top + GetSystemMetrics(SM_CYEDGE), 320, ScrollBox1.ClientHeight);
        If FastIMG1.Bmp <> nil Then
           FastIMG1.Bmp.SetSize(320, FastIMG1.Height, 32);
     End;

     If ScrollBox1.ClientHeight < ScrollBox1.VertScrollBar.Range Then
        ScrollBar1.Visible := False
     Else
        ScrollBar1.Visible := True;

     DrawPrinterOutput;

  End;

end;

procedure TPrinterForm.FormDestroy(Sender: TObject);
Var
  FS: TFileStream;
begin

  ScrollBox1.Free;

  If FileExists(BASinDir + '\Printer.bin') Then
     DeleteFile(BASinDir + '\Printer.bin');

  If Opt_SavePrinting Then Begin

     If Not FileExists(BASinDir + '\Printer.bin') Then Begin
        FS := TFileStream.Create(BASinDir + '\Printer.bin', fmCreate or fmShareDenyNone);
        FS.Write(PrinterArray[0], Length(PrinterArray));
        FS.Free;
     End;

  End;

end;

Procedure TPrinterForm.DrawPrinterOutput;
Var
  InkByte: Byte;
	F, X: Integer;
	ViewingPos: Integer;
	PrinterLines: Integer;
	YPos: Integer;
	Offset, YOff: Integer;
  InkColour: TFColorA;
Begin

	FastIMG1.Bmp.Clear(TfSilver);

  For F := 0 To Min(64, FastIMG1.Bmp.AbsHeight -1) Do Begin
     InkColour := FRGBA(F+128, F+128, F+128, 0);
     For X := 0 To FastIMG1.Bmp.Width -1 Do
        FastIMG1.Bmp.Pixels32[FastIMG1.Bmp.AbsHeight - F -1, X] := InkColour;
  End;

  InkByte := 0;
  InkColour := FastDIB.FRGBA(InkByte, InkByte, InkByte, 0);

	ViewingPos := ScrollBox1.VertScrollBar.Position;
  PrinterLines := Max((Length(PrinterArray) Div PrinterPageWidth), 0);

	YPos := 0;
  	If PrinterLines < FastIMG1.Bmp.absHeight Then Begin
		ViewingPos := 0;
		YPos := FastIMG1.Bmp.AbsHeight - PrinterLines -1;
	End;

	While (YPos < FastIMG1.Bmp.AbsHeight) and (ViewingPos < PrinterLines) Do Begin

		Offset := ViewingPos * PrinterPageWidth;
		Yoff := FastIMG1.Bmp.AbsHeight - YPos -1;

		For F := 0 To PrinterPageWidth -1 Do
			If PrinterArray[F+Offset] = 1 Then
				FastIMG1.Bmp.Pixels32[YOff, F+32] := InkColour
        Else
			   If PrinterArray[F+Offset] > 15 Then
				   FastIMG1.Bmp.Pixels32[YOff, F+32] := DisplayPalette[PrinterArray[F+Offset] - 16];


		Inc(YPos);
		Inc(ViewingPos);

	End;

	FastIMG1.Repaint;
  Application.ProcessMessages;

End;

Procedure TPrinterForm.SetPrinterPixel(XPos: Integer);
Begin

  If XPos = -1 Then XPos := PrinterPageWidth;
  PrinterArray[PrinterBase + XPos] := 1;

End;

Procedure TPrinterForm.NewLine;
Var
	DumpHeight: Integer;
Begin

	// Sets up a new line of gfx

	DumpHeight := Length(PrinterArray) Div PrinterPageWidth;
	SetLength(PrinterArray, (DumpHeight +1) * PrinterPageWidth);
	PrinterBase := DumpHeight * PrinterPageWidth;
  ScrollBox1.VertScrollBar.Range := DumpHeight;

	If DumpHeight > FastIMG1.Height Then
     If ScrollBox1.VertScrollBar.Position >= ScrollBox1.VertScrollBar.Range - FastIMG1.Height -1 Then Begin
        // Autoscroll the paper to keep track of the printing
        ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Range - FastIMG1.Height;
        DrawPrinterOutput;
     End;

End;

Function TPrinterForm.PrinterIn: Byte;
Begin

  // Return:
  // Bit 0 Encoder disk state
  // Bit 6 unset if the Printer is present (Window is visible)
  // Bit 7 set if the stylus is over paper

  Result := PrinterEncoderState;
  If Not Visible Then Result := Result Or 64;
  If PrinterXPos <= PrinterPageWidth Then Result := Result Or 128;

End;

procedure TPrinterForm.PrinterOut(Value: Byte);
Begin

  If Opt_FastPrinting and (Registers.PC = $0F24) Then Begin

     PrinterXPos := 0;
     BytesToPrinter(GetWord(@Registers.L) -1, Registers.C);
     PutWord(@Registers.L, GetWord(@Registers.L) + Registers.C -1);
     Registers.C := 1;
     Registers.B := 1;
     DrawPrinterOutput;

  End Else Begin

     If PrinterMotorState = 0 Then
        PrinterTs := 0;

     PrinterMotorState := 1 - ((Value and 4) Div 4);
     PrinterStylusActive := (Value and 128) = 128;

     If PrinterMotorState = 0 Then Begin
        PrinterTs := 0;
        DrawPrinterOutput;
     End;

  End;

End;

Procedure UpdatePrinter;
Begin

  If PrinterMotorState = 1 Then Begin

     While PrinterTs >= PrinterUpdateTs Do Begin

        // Flip the encoder state
        PrinterEncoderState := 1 - PrinterEncoderState;

        // If the encoder is on, and the stylus is down, then plot a point.
        If PrinterEncoderState = 1 Then Begin
           If PrinterStylusActive Then Begin
              If PrinterXPos <= PrinterPageWidth Then
                 If PrinterXPos > 128 Then
                    PrinterForm.SetPrinterPixel(PrinterXPos -2)
                 Else
                    PrinterForm.SetPrinterPixel(PrinterXPos -1);
           End;
           Inc(PrinterXPos);
        End;

        // If we reach the edge of the page, then add a new line.
        If PrinterXPos >= PrinterPageWidth Then Begin
           If PrinterEncoderState = 1 Then
              If PrinterXPos = PrinterPageWidth Then Begin
                 PrinterForm.NewLine;
                 Dec(PrinterBase, PrinterPageWidth); // Don't print to this new line yet!
              End;
           If PrinterXPos > PrinterPageWidth + 128 Then Begin
              PrinterXPos := 0;
              Inc(PrinterBase, PrinterPageWidth); // *now* we can print - we've waited for the carriage to return.
           End;
           PrinterForm.DrawPrinterOutput;
        End;

        Dec(PrinterTs, PrinterUpdateTs);

     End;

  End;

End;

Procedure TPrinterForm.BytesToPrinter(Address, Count: Word);
Var
  Bit: Byte;
Begin
  While Count > 0 Do Begin
     For Bit := 7 Downto 0 Do Begin
        If Memory[Address] and (1 shl Bit) <> 0 Then
           SetPrinterPixel(PrinterXPos);
        Inc(PrinterXPos);
        If PrinterXPos >= PrinterPageWidth Then Begin
           NewLine;
           PrinterXPos := 0;
        End;
     End;
     Inc(Address);
     Dec(Count);
  End;
End;

Function TPrinterForm.RenderAsDIB: TFastDIB;
Var
  ClrTable: TFColorTable;
  ViewingPos, F, YPos: Integer;
Begin

  // Render the current print output as a two-colour (but 8bpp) DIB

  Result := TFastDIB.Create;
	Result.SetSize(256, (Length(PrinterArray) Div PrinterPageWidth) +1, 8);
  ClrTable[0].r := $FF;
  ClrTable[0].g := $FF;
  ClrTable[0].b := $FF;
  ClrTable[1].r := 0;
  ClrTable[1].g := 0;
  ClrTable[1].b := 0;

  For F := 16 To 31 Do Begin
     ClrTable[F].r := DisplayPalette[F -16].r;
     ClrTable[F].g := DisplayPalette[F -16].g;
     ClrTable[F].b := DisplayPalette[F -16].b;
  End;

  Result.Colors := @ClrTable;
  Result.UpdateColors;
  Result.ClearB(0);

	ViewingPos := 0;
	For YPos := Result.Height -1 Downto 0 Do Begin
	   For F := 0 To 255 Do
		   If PrinterArray[(ViewingPos * PrinterPageWidth)+F] = 1 Then
			   Result.Pixels8[YPos, F] := 1
        Else
           If PrinterArray[(ViewingPos * PrinterPageWidth)+F] > 15 Then
              Result.Pixels8[YPos, F] := PrinterArray[(ViewingPos * PrinterPageWidth)+F];
     Inc(ViewingPos);
  End;

End;

procedure TPrinterForm.BitBtn2Click(Sender: TObject);
Var
  BMP: TBitmap;
  Output: TFastDIB;
  Idx, F, ViewingPos, YPos: Integer;
  Filename: String;
begin

  // New page - save the bitmap if necessary

  If Length(PrinterArray) > 0 Then Begin

     Idx := MessageDlgPos('Would you like to save the'#13'Printed output as a bitmap file?', mtWarning, [mbYes, mbNo, mbCancel], 0, Left + Width Div 2, Top + Height Div 2);
     Case Idx Of

        mrYes:
           Begin

			      BMP := TBitmap.Create;
			      BMP.Width := 320;
			      BMP.Height := Length(PrinterArray) Div PrinterPageWidth;

			      // Convert the bin file to a bitmap.
			      Output := TFastDIB.Create;
			      Output.SetSize(320, BMP.Height, 24);
			      Output.Clear(TfSilver);

			      ViewingPos := 0;
			      For YPos := Output.Height -1 Downto 0 Do Begin
				      For F := 0 To 255 Do
					      If PrinterArray[(ViewingPos * PrinterPageWidth)+F] = 1 Then
						      Output.Pixels24[YPos, F+32] := TfBlack
                    Else
                       If PrinterArray[(ViewingPos * PrinterPageWidth)+F] > 15 Then
                          Output.Pixels24[Ypos, F+32] := FRGBn(DisplayPalette[PrinterArray[(ViewingPos * PrinterPageWidth)+F] -16]);
				      Inc(ViewingPos);
			      End;

			      // And draw to the BMP.
			      Output.Draw(BMP.Canvas.Handle, 0, 0);
			      Output.Free;

              Filename := OpenFile(Handle, 'Save Printer Output', [FTBmp], '', True, False);
              If Filename = '' Then Exit;

              If FileExists(Filename) Then DeleteFile(Filename);
              If FileExists(Filename) Then Begin
                 MessageBox(Handle, pChar('Could not save the file'#13+ExtractFilename(Filename)+' - another'#13'application may be using this file.'), pChar('Save Error'), MB_OK or MB_ICONWARNING);
                 Exit;
              End;

              BMP.SaveToFile(Filename);
			      BMP.Free;

           End;

        mrCancel:
           Begin
              Exit;
           End;

     End;

  End;

  InitPrinter(False);
  FormResize(nil);
  DrawPrinterOutput;

end;

procedure TPrinterForm.Button1Click(Sender: TObject);
begin

  Close;

end;

procedure TPrinterForm.BitBtn1Click(Sender: TObject);
begin

  PrintForm.Combobox1.ItemIndex := 0;
  CentreFormOnForm(PrintForm, nil);
  ShowWindow(PrintForm, True);

end;

procedure TPrinterForm.DisplayAreaBW1Click(Sender: TObject);
Var
  Addr: Word;
  Y: Integer;
begin

  // Copy the screen display to the printer, ignoring ATTRs

  ClearSoundBuffers;

  For Y := 0 To 191 Do Begin
     PrinterXPos := 0;
     Addr := ScreenAddresses[Y]+16384;
     BytesToPrinter(Addr, 32);
     DrawPrinterOutput;
  End;

  NewLine;
  DrawPrinterOutput;

end;

procedure TPrinterForm.DisplayAndAttrsColour1Click(Sender: TObject);
Var
  ByteVal, AttrVal, ClrFg, ClrBg: Byte;
  Addr: Word;
  Y, X, Z: Integer;
begin

  // Copy the screen display to the printer, in Colour

  ClearSoundBuffers;

  For Y := 0 To 191 Do Begin
     PrinterXPos := 0;
     Addr := ScreenAddresses[Y]+16384;
     For X := 0 To 31 Do Begin
        ByteVal := Memory[Addr + X];
        AttrVal := Memory[AttrAddresses[(Addr + X) - 16384] + 16384];
        ClrFg := 16 + (AttrVal And 7);
        ClrBg := 16 + ((AttrVal Shr 3) And 7);
        If AttrVal and 64 = 64 Then Begin
           Inc(ClrFg, 8);
           Inc(ClrBg, 8);
        End;
        For Z := 7 Downto 0 Do
           If ByteVal and (1 Shl Z) = 0 Then
              PrinterArray[PrinterBase + (X * 8) + (7 - Z)] := ClrBg
           Else
              PrinterArray[PrinterBase + (X * 8) + (7 - Z)] := ClrFg;
     End;
     
     NewLine;
     DrawPrinterOutput;

  End;

  NewLine;
  DrawPrinterOutput;

end;

procedure TPrinterForm.BitBtn3Click(Sender: TObject);
Var
  Pt: TPoint;
begin

  Pt := ClientToScreen(Point(BitBtn3.Left, BitBtn3.Top + BitBtn3.Height));
  PopUpMenu1.PopUp(Pt.x, Pt.y);

end;

procedure TPrinterForm.BitBtn4Click(Sender: TObject);
begin

  NewLine;
  DrawPrinterOutput;

end;

procedure TPrinterForm.Button2Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_zx_printer_output.html'), HH_DISPLAY_TOPIC, 0);

end;

end.
