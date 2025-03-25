unit Printing;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FastIMG, ExtCtrls, StdCtrls, Printers, FastDIB, FastDraw, FastDrawEx,
  Mask, ComCtrls, WinSpool, FastFiles, Buttons, FastSize, Math, Utility;

type
  TPrintForm = class(TForm)
    Label27: TLabel;
    Bevel12: TThemeBevel;
    FastIMG1: TFastIMG;
    Label1: TLabel;
    Bevel1: TThemeBevel;
    Shape1: TShape;
    Timer1: TTimer;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    UpDown1: TUpDown;
    MaskEdit1: TMaskEdit;
    Label3: TLabel;
    Button3: TButton;
    Label4: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    PrintDialog1: TPrintDialog;
    Label5: TLabel;
    MaskEdit2: TMaskEdit;
    UpDown2: TUpDown;
    CheckBox2: TCheckBox;
    Button4: TButton;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MaskEdit1Change(Sender: TObject);
    procedure UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
    procedure MaskEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MaskEdit2Change(Sender: TObject);
    procedure MaskEdit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpDown2ChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    PageWidth: Integer;
    PageHeight: Integer;
    PageMarginLeft: Integer;
    PageMarginTop: Integer;
    PageScale: Integer;
    PageMultipleColumns: Boolean;
    NumPreviewPages: Integer;
    CurPreviewPage: Integer;
    UpdatingText: Boolean;
    Initialising: Boolean;
    PageColumnWrap: Integer;
    Procedure RefreshPrinter;
    Procedure MakePreview;
    Procedure SetPreviewSize;
    Procedure DrawPreview(PageNum: Integer);
    Procedure RenderPage(DIB: TFastDIB; var PageNum: Integer);
    Function  RenderScreen: TFastDIB;
    Function  RenderBASIC(WrapAt: Integer): TFastDIB;
    Procedure OpenPrinterProperties;
  end;

var
  PrintForm: TPrintForm;
  Printer: TPrinter;

implementation

Uses BASinMain, ROMUtils, PrinterOutput, Sound, FastCore, Display, VarsWindow;

{$R *.DFM}

procedure TPrintForm.FormShow(Sender: TObject);
begin

  Initialising := True;

  ClearSoundBuffers;
  CurPreviewPage := 1;
  MaskEdit1.Text := IntToStr(PageScale);
  MaskEdit2.Text := IntToStr(PageColumnWrap);
  CheckBox1.Checked := PageMultipleColumns;
  CurPreviewPage := 1;
  NumPreviewPages := 1;

  Initialising := False;

  PageScale := 30000;
  MakePreview;
  Timer1.Enabled := True;

end;

Procedure TPrintForm.MakePreview;
Begin

  If Initialising Then Exit;

  Cursor := crHourglass;

  RefreshPrinter;

  // Set the paper size, and resize the preview DIB accordingly.

  SetPreviewSize;

  // And draw the preview.

  DrawPreview(CurPreviewPage);

  Cursor := crDefault;

End;

Procedure TPrintForm.RefreshPrinter;
Begin

  Printer.Refresh;
  Printer.Free;
  Printer := TPrinter.Create;
  PageWidth := Printer.PageWidth;
  PageHeight := Printer.PageHeight;

End;

Procedure TPrintForm.SetPreviewSize;
Var
  pW, pH: Integer;
Begin

  If PageWidth > PageHeight Then Begin

     // Landscape printing - re-arrange the form accordingly

     Pw := 300;
     Ph := Round(300 * (PageHeight / PageWidth));

  End Else Begin

     // Portrait printing

     Pw := Round(300 * (PageWidth / PageHeight));
     Ph := 300;

  End;

  If ComboBox1.ItemIndex in [0, 2] Then Begin
     Label5.Enabled := False;
     MaskEdit2.Enabled := False;
     UpDown2.Enabled := False;
     CheckBox2.Enabled := ComboBox1.ItemIndex = 2;
  End Else Begin
     Label5.Enabled := True;
     MaskEdit2.Enabled := True;
     UpDown2.Enabled := True;
     CheckBox2.Enabled := False;
  End;

  ClientHeight := FastIMG1.Top + ph + Label1.Height + Bevel1.Height + ComboBox1.Height + MaskEdit1.Height + CheckBox1.Height + Button1.Height + SpeedButton1.Height + 56;
  FastIMG1.SetBounds(8 + ((ClientWidth - 24) Div 2) - (Pw Div 2), FastIMG1.Top, Pw, Ph);
  Shape1.SetBounds(FastIMG1.Left + 8, FastIMG1.Top + 8, FastIMG1.Width, FastIMG1.Height);
  FastIMG1.Bmp.SetSize(Pw, Ph, 24);



End;

Procedure TPrintForm.DrawPreview(PageNum: Integer);
Var
  PrevDIB, StretchDIB: TFastDIB;
  ClrTable: TFColorTable;
Begin

  // First clear the preview DIB

  FastIMG1.Bmp.Clear(TFWhite);
  FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width -1, FastIMG1.Bmp.AbsHeight -1, TfBlack);
  FastIMG1.Bmp.Pixels24[0, 0] := TFBlack;

  // Create a temporary DIB which will hold a 1:1 image of the printed page

  PrevDIB := TFastDIB.Create;
  PrevDIB.SetSize(PageWidth, PageHeight, 32);

  ClrTable[0].r := $FF;
  ClrTable[0].g := $FF;
  ClrTable[0].b := $FF;

  ClrTable[1].r := 0;
  ClrTable[1].g := 0;
  ClrTable[1].b := 0;

  PrevDIB.Colors := @ClrTable;
  PrevDIB.UpdateColors;

  // Now call the renderer for this surface

  RenderPage(PrevDIB, PageNum);

  StretchDIB := TFastDIB.Create;
  StretchDIB.SetSize(FastIMG1.Bmp.Width -2, FastIMG1.Bmp.AbsHeight -2, 32);
  Bilinear(PrevDIB, StretchDIB);
  StretchDIB.Draw(FastIMG1.Bmp.hDc, 1, 1);
  StretchDIB.Free;

  PrevDIB.Free;

  CurPreviewPage := PageNum;
  Label4.Caption := 'Page '+IntToStr(CurPreviewPage)+'/'+IntToStr(NumPreviewPages);
  SpeedButton2.Enabled := Not (CurPreviewPage = NumPreviewPages);
  SpeedButton1.Enabled := Not (CurPreviewPage = 1);

  FastIMG1.Repaint;

End;

Procedure TPrintForm.RenderPage(DIB: TFastDIB; var PageNum: Integer);
Var
  PixelsPerMillimetre: Integer;
  RangeY, MarginX, MarginY: Integer;
  GrabHeight, DumpHeight, OffsetX, OffsetY: Integer;
  NumColumns: Integer;
  ColumnSize, ColumnWrap: Integer;
  OnePageSize: Integer;
  FromDIB: TFastDIB;
  ColumnCount, PageCount: Integer;
  MultipleColumns: Boolean;
Begin

  If Initialising Then Exit;

  DIB.Clear(TFWhite);

  If ComboBox1.ItemIndex = 0 Then Begin
     // ZX Printer output
     FromDIB := PrinterForm.RenderAsDIB;
     MultipleColumns := PageMultipleColumns;
     ColumnWrap := 32;
  End Else If ComboBox1.ItemIndex = 1 Then Begin
     // BASIC listing
     FromDIB := RenderBASIC(PageColumnWrap);
     ColumnWrap := FromDIB.Width Div 8;
     MultipleColumns := PageMultipleColumns;
  End Else Begin
     // The Screen display
     FromDIB := RenderScreen;
     ColumnWrap := FromDIB.Width Div 8;
     MultipleColumns := False;
  End;

  // Render a page at full size. This is used by the printing itself and the preview DIB
  // Figure out how big the page actually is in pixels

  PixelsPerMillimetre := Round(GetDeviceCaps(Printer.Handle, LogPixelsY)/25.4);
  MarginY := PageMarginTop * PixelsPerMillimetre;
  MarginX := PageMarginLeft* PixelsPerMillimetre;
  RangeY := PageHeight - (2 * MarginY);

  // Sanity check the scaling - can't get *too* large!

  If Round(ColumnWrap * 8 * (PageScale/100)) > PageWidth - (MarginX * 2) Then Begin
     PageScale := Round((PageWidth - (MarginX * 2)) / (ColumnWrap * 8) * 100);
     MaskEdit1.Text := IntToStr(PageScale);
  End;

  If ComboBox1.ItemIndex = 2 Then
     If FromDIB.Height * (PageScale/100) > PageHeight - (MarginY * 2) Then Begin
        PageScale := Round(Int((PageHeight - (MArginY * 2)) / (FromDIB.Height)) * 100);
        MaskEdit1.Text := IntToStr(PageScale);
     End;

  // How many lines in a column?

  If PageScale = 0 Then PageScale := 1;
  ColumnSize := Round((RangeY / (8 * (PageScale/100))));

  If MultipleColumns Then Begin

     // Multiple columns - each column is 32 chars wide, with 4 chars between.
     // Unless this is the BASIC listing, where it's a user defined width.

     NumColumns := 1;
     OffsetX := MarginX + Round((PageScale/100) * (ColumnWrap * 8));
     While OffsetX < PageWidth - MarginX Do Begin
        Inc(OffsetX, Round((PageScale/100) * ((ColumnWrap +4) * 8)));
        If OffsetX < PageWidth - MarginX Then
           Inc(numColumns);
     End;

     OnePageSize := (ColumnSize * 8) * NumColumns;

  End Else Begin

     OnePageSize := ColumnSize * 8;

  End;

  NumPreviewPages := (FromDIB.AbsHeight Div OnePageSize) +1;

  If NumPreviewPages < PageNum Then
     PageNum := NumPreviewPages;

  // Now we know how large a chunk of the bitmap a page takes, we can advance to the correct page.

  OffsetY := 0;
  PageCount := 1;
  While True Do Begin

     If PageCount = PageNum Then Break;

     If OffsetY + OnePageSize < FromDIB.AbsHeight -1 Then Begin
        Inc(OffsetY, OnePageSize);
        Inc(PageCount);
     End Else
        Break;

  End;

  // Now OffsetY is pointing at the place to render. Time to render the page.

  OffsetX := MarginX;
  ColumnCount := 0;
  While True Do Begin

     // If we're at the last page, then don't grab the whole DIB - we get corruption that way.

     DumpHeight := Round(ColumnSize * (8 * (PageScale/100)));
     GrabHeight := ColumnSize * 8;

     If GrabHeight + OffsetY > FromDIB.AbsHeight Then Begin

        DumpHeight := Round((FromDIB.AbsHeight - OffsetY -1) * (PageScale/100));
        GrabHeight := FromDIB.AbsHeight - OffsetY -1;

     End;

     // Copy and stretch to the new DIB

     FromDIB.StretchRect(DIB.hDc, OffsetX, MarginY, Round(ColumnWrap * (8 * (PageScale/100))), DumpHeight, 0, OffsetY, ColumnWrap * 8, GrabHeight);

     // And process multiple columns if necessary.

     If Not MultipleColumns Then
        Break
     Else Begin
        Inc(OffSetX, Round((ColumnWrap + 4) * (8 * (PageScale/100))));
        Inc(OffSetY, ColumnSize * 8);
        If OffSetY > FromDIB.AbsHeight -1 Then Break;
        Inc(ColumnCount);
        If ColumnCount = NumColumns Then Break;
     End;

  End;

  // All Done!

  FromDIB.Free;

End;

procedure TPrintForm.Timer1Timer(Sender: TObject);
Var
  Ptr: TPrinter;
begin

  Ptr := TPrinter.Create;
  If (Ptr.PageWidth <> PageWidth) or (Ptr.PageHeight <> PageHeight) Then
     MakePreview;
  Ptr.Free;

end;

procedure TPrintForm.Button3Click(Sender: TObject);
begin

  Timer1.Enabled := False;
  OpenPrinterProperties;
  MakePreview;
  Timer1.Enabled := True;

end;

Procedure TPrintForm.OpenPrinterProperties;
var
  FPrinterHandle: THandle;
  aDevice, Driver, Port: array[0..255] of char;
  DevMode: PDeviceMode;
  StubDevMode: TDeviceMode;
  DeviceMode: THandle;
  PrinterInfoLen: DWord;
  PrinterInfo2: pPrinterInfo2;
  ppi: THandle;
Begin
  Printer.GetPrinter(aDevice, Driver, Port, fPrinterHandle);
  if OpenPrinter(aDevice,FPrinterHandle,nil) then begin
     DeviceMode := GlobalAlloc(GHND, DocumentProperties(0, FPrinterHandle, ADevice, StubDevMode, StubDevMode, 0));
     if DeviceMode <> 0 then begin
        DevMode := GlobalLock(DeviceMode);
        If DocumentProperties(Handle, FPrinterHandle, ADevice, DevMode^, DevMode^, DM_IN_PROMPT or DM_OUT_BUFFER) = IDOK Then Begin
           GetPrinter(fPrinterHandle, 2, nil, 0, @PrinterInfoLen);
           ppi := GlobalAlloc(GHND, PrinterInfoLen);
           if ppi <> 0 then begin
              PrinterInfo2 := GlobalLock(ppi);
              PrinterInfo2^.pDevmode := DevMode;
              SetPrinter(fPrinterHandle, 2, PrinterInfo2, 0);
              GlobalUnlock(ppi);
              GlobalFree(ppi);
           End;
           ClosePrinter(fPrinterHandle);
        End;
        GlobalUnlock(DeviceMode);
        GlobalFree(DeviceMode);
     end;
  end;
End;

procedure TPrintForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  Timer1.Enabled := False;

end;

procedure TPrintForm.FormCreate(Sender: TObject);
begin

  Initialising := True;

  PageMultipleColumns := True;
  PageMarginLeft := 20;
  PageMarginTop := 20;
  PageScale := 100;
  PageColumnWrap := 32;

  ComboBox1.ItemIndex := 0;

  Initialising := False;

end;

procedure TPrintForm.MaskEdit1Change(Sender: TObject);
begin

  // Numerals only in here!

  If MaskEdit1.Text <> '' Then
     If StrToIntDef(MaskEdit1.Text, -1) = -1 Then
        MaskEdit1.Text := IntToStr(PageScale)
     Else
        PageScale := StrToIntDef(MaskEdit1.Text, 100);

end;

procedure TPrintForm.UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  If Direction = UpdUp Then
     Inc(PageScale)
  Else
     If PageScale > 0 Then
        Dec(PageScale);
  If PageScale = 0 Then PageScale := 1;
  MaskEdit1.Text := IntToStr(PageScale);
  MakePreview;
end;

procedure TPrintForm.MaskEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then Begin
     MakePreview;
     MaskEdit1.SelectAll;
     Key := 0;
  End;
end;

procedure TPrintForm.CheckBox1Click(Sender: TObject);
begin

  PageMultipleColumns := CheckBox1.Checked;
  MakePreview;

end;

procedure TPrintForm.ComboBox1Change(Sender: TObject);
begin

  PageScale := 3000;
  MakePreview;

end;

procedure TPrintForm.SpeedButton1Click(Sender: TObject);
begin
  Dec(CurPreviewPage);
  MakePreview;
end;

procedure TPrintForm.SpeedButton2Click(Sender: TObject);
begin
  Inc(CurPreviewPage);
  MakePreview;
end;

procedure TPrintForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TPrintForm.Button1Click(Sender: TObject);
Var
  Idx, Start, Finish, PageCount: Integer;
  ClrTable: TFColorTable;
  PrintDIB: TFastDIB;
begin

  // Do the print! - Set up and call the Print Dialog

  If ActiveControl = MaskEdit1 Then Begin
     MakePreview;
     MaskEdit1.SelectAll;
     Exit;
  End;

  If ActiveControl = MaskEdit2 Then Begin
     MakePreview;
     MaskEdit2.SelectAll;
     Exit;
  End;

  If ActiveControl = ComboBox1 Then Begin
     MakePreview;
     Exit;
  End;

  PrintDialog1.FromPage := 1;
  PrintDialog1.MinPage := 1;
  PrintDialog1.ToPage := NumPreviewPages;
  PrintDialog1.MaxPage := NumPreviewPages;

  If NumPreviewPages > 1 Then
     PrintDialog1.Options := PrintDialog1.Options + [poSelection]
  Else
     PrintDialog1.Options := PrintDialog1.Options - [poSelection];

  If PrintDialog1.Execute Then Begin

     If PrintDialog1.PrintRange = prAllPages Then Begin
        Start := 1;
        Finish := NumPreviewPages;
     End Else If PrintDialog1.PrintRange = prSelection Then Begin
        Start := CurPreviewPage;
        Finish := CurPreviewPage;
     End Else Begin
        Start := PrintDialog1.FromPage;
        Finish := PrintDialog1.ToPage;
     End;

     // For each page...

     Printer.BeginDoc;

     For PageCount := 1 To PrintDialog1.Copies Do Begin

        Idx := Start;
        While Idx <= Finish Do Begin

           PrintDIB := TFastDIB.Create;
           PrintDIB.SetSize(PageWidth, PageHeight, 32);

           ClrTable[0].r := $FF;
           ClrTable[0].g := $FF;
           ClrTable[0].b := $FF;

           ClrTable[1].r := 0;
           ClrTable[1].g := 0;
           ClrTable[1].b := 0;

           PrintDIB.Colors := @ClrTable;
           PrintDIB.UpdateColors;

           // Now call the renderer for this surface - does this look familiar?

           RenderPage(PrintDIB, Idx);

           // And send it to the printer

           PrintDIB.Stretch(Printer.Canvas.Handle, 0, 0, PageWidth -1, PageHeight -1);
           PrintDIB.Free;

           If Idx <> Finish Then
              Printer.NewPage
           Else
              If PageCount <> PrintDialog1.Copies Then
                 Printer.NewPage;

           Inc(Idx);
        
        End;

     End;

     Printer.EndDoc;

  End;

end;

// How about Rendering the Screen?

Function TPrintForm.RenderScreen: TFastDIB;
Begin

  Result := TFastDIB.Create;
  If Not CheckBox2.Checked Then Begin
     Result.SetSize(FastCore.Display.Width, FastCore.Display.Height, 32);
     FastCore.Display.Draw(Result.hDc, 0, 0);
  End Else Begin
     Result.SetSize(DisplayWindow.DisplayIMG.Bmp.Width, DisplayWindow.DisplayIMG.Bmp.AbsHeight, 32);
     DisplayWindow.DisplayIMG.Bmp.Draw(Result.hDc, 0, 0);
  End;

End;

// Last but not least - Printing your editor text. This will generate a COLOUR bitmap.

Function TPrintForm.RenderBASIC(WrapAt: Integer): TFastDIB;
Var
  MarginX, Idx, Idx2, Clr, XOffset, YOffset, TempVal, MaxLen, KeywordCutOff: Integer;
  InString, REMCommand, Ita, Bld: Boolean;
  Text, TempStr, ExpStr, Token: String;
  BASICList: TStringlist;
  Addr: Word;
Begin

  BASIClist := TStringlist.Create;

  // Need to do this twice - a waste, but what the hell. First, calculate how big a bitmap we need.
  // Then actually grab the text and colour it with the user's syntax highlighting settings.

  Addr := GetWord(@Memory[PROG]);

  While Addr <= GetWord(@Memory[VARS]) Do Begin

     Text := '';
     TempStr := '';
     // Get the line number
     Text := IntToStr((Memory[Addr] Shl 8) + Memory[Addr +1]);
     // And pad it
     While Length(Text) < 4 Do
        Text := ' ' + Text;
     // Now get the meat of the line
     TempVal := GetWord(@Memory[Addr +2]);
     TempStr := ROMUtils.DeTokeniseLine(GetMemoryString(Addr +4, TempVal -1, Memory), False);
     // And convert to text
     Text := Text + ' ' + TempStr;
     // Now parse for line seperators - the Wrap point, the ":" and THEN
     InString := False;
     REMCommand := False;
     Idx := 5;
     While True Do Begin
        If Idx = WrapAt Then Begin
           If Not InString and Not REMCommand Then
              While Text[Idx] in ['0'..'9', 'A'..'Z', 'a'..'z'] Do
                 Dec(Idx);
           BASICList.Add(Copy(Text, 1, Idx));
           Text := '     ' + Copy(Text, Idx +1, 999999);
           Idx := 5;
        End;
        Case Text[Idx] Of
           '"':  InString := Not InString;
           'R':  If Not InString Then
                    If Lowercase(Copy(Text, Idx, 4)) = 'rem ' Then
                       REMCommand := True;
           'T':  If Not InString and Not REMCommand Then
                    If Lowercase(Copy(Text, Idx, 5)) = 'then ' Then Begin
                       BASICList.Add(Copy(Text, 1, Idx + 3));
                       Text := '    ' + Copy(Text, Idx + 4, 999999);
                       Idx := 4;
                    End;
           ':':  If Not InString and Not REMCommand Then Begin
                    BASICList.Add(Copy(Text, 1, Idx));
                    Text := '    ' + Copy(Text, Idx +1, 999999);
                    Idx := 4;
                 End;
        End;
        inc(Idx);
        If Idx > Length(Text) Then Begin
           BASICList.Add(Text);
           Break;
        End;
     End;

     // And address the next line
     Inc(Addr, TempVal + 4);

  End;

  MaxLen := 0;
  For Idx := 0 To BASICList.Count -1 Do
     If Length(BASICList[Idx]) > MaxLen Then
        MaxLen := Length(BASICList[Idx]);

  // Now create the DIB.

  Result := TFastDIB.Create;
  Result.SetSize(MaxLen * 8, BASICList.Count * 8, 32);
  Result.Clear(TfWhite);

  // And render the DIB, using syntax highlighting. This involves iterating through BASIC memory
  // once more, identifying and drawing tokens.

  YOffSet := -8;
  Addr := GetWord(@Memory[PROG]);

  If ProgramIs128k Then
     KeywordCutOff := $A2
  Else
     KeywordCutOff := $A4;

  While Opt_SyntaxHighlight and (Addr < GetWord(@Memory[VARS])) Do Begin

     XOffset := 0;
     Inc(YOffset, 8);

     // Start with a lineNumber

     Text := '';
     Text := IntToStr((Memory[Addr] Shl 8) + Memory[Addr +1]);
     While Length(Text) < 4 Do
        Text := ' ' + Text;
     SpecTextToDIB(Result, XOffset, YOffset, Text, Opt_LineNumsColour, -1, 0, Opt_LineNumsItalic, false);
     If Opt_LineNumsBold Then
        SpecTextToDIB(Result, XOffset+1, YOffset, Text, Opt_LineNumsColour, -1, 0, Opt_LineNumsItalic, false);

     Inc(XOffset, 32);
     TempVal := GetWord(@Memory[Addr +2]);
     Text := GetMemoryString(Addr +4, TempVal -1, Memory);

     // Now get the line token by token

     Idx := 1;
     While Idx <= Length(Text) Do Begin

        Case Text[Idx] of

           #0..#31:
              Begin
                 // Hexchars here
                 Inc(Idx);
              End;
           #32:
              Begin
                 // Space - just skip
                 Inc(XOffset, 8);
                 Inc(Idx);
              End;
           #33..#47, #58..#64, #91..#96, #123..#127:
              Begin
                 // Symbols
                 If Text[Idx] = '"' Then Begin
                    // This quote (") mark is the start of a string, so process the whole string.
                    TempStr := '"';
                    Inc(Idx);
                    While Text[Idx] <> '"' Do Begin
                       TempStr := TempStr + Text[Idx];
                       Inc(Idx);
                    End;
                    TempStr := TempStr + '"';
                    // Expand any tokens in the string
                    Inc(Idx);
                    Idx2 := 2;
                    ExpStr := '"';
                    While TempStr[Idx2] <> '"' Do Begin
                       If TempStr[Idx2] > Chr(KeywordCutOff) Then Begin
                          Token := SpaceKeywords[Ord(TempStr[Idx2]) - $A3];
                          If Token[1] in ['A'..'Z'] Then
                             If TempStr[Idx2] > #$C4 Then
                                If ExpStr[Length(ExpStr)] <> ' ' Then
                                   Token := ' ' + Token;
                          ExpStr := ExpStr + Token;
                       End Else
                          ExpStr := ExpStr + TempStr[Idx2];
                       Inc(Idx2);
                    End;
                    ExpStr := ExpStr + '"';
                    While XOffset + (8 * (Length(ExpStr))) > Result.Width Do Begin
                       // Wrap ?
                       TempStr := Copy(ExpStr, 1, (Result.Width - XOffset) Div 8);
                       ExpStr := '     ' + Copy(ExpStr, Length(TempStr) +1, 999999);
                       SpecTextToDIB(Result, XOffset, YOffset, TempStr, Opt_StringsColour, -1, 0, Opt_StringsItalic, false);
                       If Opt_StringsBold Then
                          SpecTextToDIB(Result, XOffset+1, YOffset, TempStr, Opt_StringsColour, -1, 0, Opt_StringsItalic, false);
                       XOffset := 0;
                       Inc(YOffset, 8);
                    End;
                    SpecTextToDIB(Result, XOffset, YOffset, ExpStr, Opt_StringsColour, -1, 0, Opt_StringsItalic, false);
                    If Opt_StringsBold Then
                       SpecTextToDIB(Result, XOffset+1, YOffset, ExpStr, Opt_StringsColour, -1, 0, Opt_StringsItalic, false);
                    Inc(XOffset, Length(ExpStr) * 8);
                 End Else Begin
                    // Normal Token
                    SpecTextToDIB(Result, XOffset, YOffset, Text[Idx], Opt_SymbolsColour, -1, 0, Opt_SymbolsItalic, False);
                    If Opt_SymbolsBold Then
                       SpecTextToDIB(Result, XOffset +1, YOffset, Text[Idx], Opt_SymbolsColour, -1, 0, Opt_SymbolsItalic, False);
                    Inc(XOffset, 8);
                    If Text[Idx] = #58 Then Begin
                       // ":" seperator - so wrap.
                       XOffset := 32;
                       Inc(YOffset, 8);
                    End;
                    Inc(Idx);
                 End;
              End;
           #48..#57:
              Begin
                 // A number - run through and gather all chars up to the $0E number marker,
                 ExpStr := '';
                 While Text[Idx] <> #14 Do Begin
                    ExpStr := ExpStr + Text[Idx];
                    Inc(Idx);
                 End;
                 // and then skip over the five bytes
                 Inc(Idx, 6);
                 While XOffset + (8 * (Length(ExpStr))) > Result.Width Do Begin
                    // Wrap ?
                    TempStr := Copy(ExpStr, 1, (Result.Width - XOffset) Div 8);
                    ExpStr := '     ' + Copy(ExpStr, Length(TempStr) +1, 999999);
                    SpecTextToDIB(Result, XOffset, YOffset, TempStr, Opt_NumbersColour, -1, 0, Opt_NumbersItalic, false);
                    If Opt_NumbersBold Then
                       SpecTextToDIB(Result, XOffset +1, YOffset, TempStr, Opt_NumbersColour, -1, 0, Opt_NumbersItalic, false);
                    XOffset := 0;
                    Inc(YOffset, 8);
                 End;
                 SpecTextToDIB(Result, XOffset, YOffset, ExpStr, Opt_NumbersColour, -1, 0, Opt_NumbersItalic, false);
                 If Opt_NumbersBold Then
                    SpecTextToDIB(Result, XOffset +1, YOffset, ExpStr, Opt_NumbersColour, -1, 0, Opt_NumbersItalic, false);
                 Inc(XOffset, Length(ExpStr) * 8);
              End;
           #65..#90, #97..#122:
              Begin
                 // Text, not in quotes... so must be a variable. Gather all alphanumerics and
                 // a possible trailing "$" char.
                 ExpStr := '';
                 While Text[Idx] in [#36, #48..#57, #65..#90, #92..#122] Do Begin
                    ExpStr := ExpStr + Text[Idx];
                    Inc(Idx);
                 End;
                 If VariablesWindow.IsVariable(Uppercase(ExpStr)) Then Begin
                    Ita := Opt_VarsItalic;
                    Clr := Opt_VarsColour;
                    Bld := Opt_VarsBold;
                 End Else Begin
                    Ita := Opt_VarsUnDefItalic;
                    Clr := Opt_VarsUnDefColour;
                    Bld := Opt_VarsUnDefBold;
                 End;
                 While XOffset + (8 * (Length(ExpStr))) > Result.Width Do Begin
                    // Wrap ?
                    TempStr := Copy(ExpStr, 1, (Result.Width - XOffset) Div 8);
                    ExpStr := '     ' + Copy(ExpStr, Length(TempStr) +1, 999999);
                    SpecTextToDIB(Result, XOffset, YOffset, TempStr, Clr, -1, 0, Ita, false);
                    If Bld Then
                       SpecTextToDIB(Result, XOffset +1, YOffset, TempStr, Clr, -1, 0, Ita, false);
                    XOffset := 0;
                    Inc(YOffset, 8);
                 End;
                 SpecTextToDIB(Result, XOffset, YOffset, ExpStr, Clr, -1, 0, Ita, false);
                 If Bld Then
                    SpecTextToDIB(Result, XOffset +1, YOffset, ExpStr, Clr, -1, 0, Ita, false);
                 Inc(XOffset, Length(ExpStr) * 8);
              End;
           #163..#255:
              Begin
                 // A Keyword or Function token. Some of these are coloured differently.
                 If Text[Idx] in [#199..#201] Then Begin
                    // The <=, >= and <> are symbols
                    Clr := Opt_SymbolsColour;
                    Ita := Opt_SymbolsItalic;
                    Bld := Opt_SymbolsBold;
                 End Else Begin
                    Clr := Opt_KeywordsColour;
                    Ita := Opt_KeywordsItalic;
                    Bld := Opt_KeywordsBold;
                 End;
                 Token := SpaceKeywords[Ord(Text[Idx]) - $A3];
                 If Token[1] in ['A'..'Z'] Then
                    If (Text[Idx] > #$C4) or (Text[Idx] in [#163, #164]) Then
                       If Text[Idx -1] <> ' ' Then
                          Token := ' ' + Token;
                 // Tokens do not wrap mid-text, so wrap before the token if necessary
                 If XOffset + (Length(Token) * 8) > Result.Width Then Begin
                    XOffset := 0;
                    Inc(YOffset, 8);
                    If Token[1] <> ' ' Then Token := ' ' + Token;
                    Token := '    ' + Token;
                 End;
                 SpecTextToDIB(Result, XOffset, YOffset, Token, Clr, -1, 0, Ita, False);
                 If Bld Then
                    SpecTextToDIB(Result, XOffset +1, YOffset, Token, Clr, -1, 0, Ita, False);
                 Inc(XOffset, 8 * (Length(Token)));
                 // Now - is it a REM, DEF FN or FN token?
                 If Text[Idx] = #234 Then Begin
                    // REM - gather all the rest of the chars in this line and draw them.
                    TempStr := '';
                    While Idx <= Length(Text) Do Begin
                       TempStr := TempStr + Text[Idx];
                       Inc(Idx);
                    End;
                    // Process as for strings, to expand tokens in the REM
                    Idx2 := 2;
                    ExpStr := '"';
                    While Idx2 <= Length(TempStr) Do Begin
                       If TempStr[Idx2] > #$A4 Then Begin
                          Token := SpaceKeywords[Ord(TempStr[Idx2]) - $A3];
                          If Token[1] in ['A'..'Z'] Then
                             If TempStr[Idx2] > #$C4 Then
                                If ExpStr[Length(ExpStr)] <> ' ' Then
                                   Token := ' ' + Token;
                          ExpStr := ExpStr + Token;
                       End Else
                          If TempStr[Idx2] >= ' ' Then
                             ExpStr := ExpStr + TempStr[Idx2] // HEXCHARS!
                          Else
                             If TempStr[Idx2] = #14 Then
                                Inc(Idx2, 5);
                       Inc(Idx2);
                    End;
                    ExpStr := Copy(ExpStr, 2, 999999);
                    While XOffset + (8 * (Length(ExpStr))) > Result.Width Do Begin
                       // Wrap ?
                       TempStr := Copy(ExpStr, 1, (Result.Width - XOffset) Div 8);
                       ExpStr := '     ' + Copy(ExpStr, Length(TempStr) +1, 999999);
                       SpecTextToDIB(Result, XOffset, YOffset, TempStr, Opt_CommentsColour, -1, 0, Opt_CommentsItalic, false);
                       If Opt_CommentsBold Then
                          SpecTextToDIB(Result, XOffset +1, YOffset, TempStr, Opt_CommentsColour, -1, 0, Opt_CommentsItalic, false);
                       XOffset := 0;
                       Inc(YOffset, 8);
                    End;
                    SpecTextToDIB(Result, XOffset, YOffset, ExpStr, Opt_CommentsColour, -1, 0, Opt_CommentsItalic, false);
                    If Opt_CommentsBold Then
                       SpecTextToDIB(Result, XOffset +1, YOffset, ExpStr, Opt_CommentsColour, -1, 0, Opt_CommentsItalic, false);
                    Inc(XOffset, Length(ExpStr) * 8);
                 End Else
                    If Text[Idx] in [#168, #206] Then Begin
                       // DEF FN or FN. Grab all up to "(" and send.
                       ExpStr := '';
                       While Text[Idx] <> '(' Do Begin
                          ExpStr := ExpStr + Text[Idx];
                          Inc(Idx);
                       End;
                       While XOffset + (8 * (Length(ExpStr))) > Result.Width Do Begin
                          // Wrap ?
                          TempStr := Copy(ExpStr, 1, (Result.Width - XOffset) Div 8);
                          ExpStr := '     ' + Copy(ExpStr, Length(TempStr) +1, 999999);
                          SpecTextToDIB(Result, XOffset, YOffset, TempStr, Opt_FunctionsColour, -1, 0, Opt_FunctionsItalic, false);
                          If Opt_FunctionsBold Then
                             SpecTextToDIB(Result, XOffset +1, YOffset, TempStr, Opt_FunctionsColour, -1, 0, Opt_FunctionsItalic, false);
                          XOffset := 0;
                          Inc(YOffset, 8);
                       End;
                       SpecTextToDIB(Result, XOffset, YOffset, ExpStr, Opt_FunctionsColour, -1, 0, Opt_FunctionsItalic, false);
                       If Opt_FunctionsBold Then
                          SpecTextToDIB(Result, XOffset +1, YOffset, ExpStr, Opt_FunctionsColour, -1, 0, Opt_FunctionsItalic, false);
                       Inc(XOffset, Length(ExpStr) * 8);
                    End Else
                       If Text[Idx] = #203 Then Begin
                          // THEN - so wrap.
                          XOffset := 32;
                          Inc(YOffset, 8);
                          Inc(Idx);
                       End Else
                          // Just step over the token and continue.
                          Inc(Idx);
              End;
        Else
           Inc(Idx);
        End;
     End;

     // And address the next line
     Inc(Addr, TempVal + 4);

  End;

  If Not Opt_SyntaxHighlight Then Begin
     YOffset := 0;
     For Idx := 0 To BASICList.Count -1 Do Begin
        SpecTextToDIB(Result, 0, YOffset, BASICList[Idx], 0, 7, 1, false, false);
        Inc(YOffset, 8);
     End;
  End;

  BASICList.Free;

End;

procedure TPrintForm.MaskEdit2Change(Sender: TObject);
begin

  // Numerals only in here!

  If MaskEdit2.Text <> '' Then
     If StrToIntDef(MaskEdit2.Text, -1) = -1 Then
        MaskEdit2.Text := IntToStr(PageColumnWrap)
     Else
        PageColumnWrap := Max(StrToIntDef(MaskEdit2.Text, 32), 32);
        
  MaskEdit2.Text := IntToStr(PageColumnWrap);

end;

procedure TPrintForm.MaskEdit2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = VK_RETURN Then Begin
     MakePreview;
     MaskEdit2.SelectAll;
     Key := 0;
  End;
end;

procedure TPrintForm.UpDown2ChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  If Direction = UpdUp Then
     Inc(PageColumnWrap)
  Else
     If PageColumnWrap > 0 Then
        Dec(PageColumnWrap);
  If PageColumnWrap = 0 Then PageColumnWrap := 1;
  MaskEdit2.Text := IntToStr(PageColumnWrap);
  MakePreview;
end;

procedure TPrintForm.CheckBox2Click(Sender: TObject);
begin

  MakePreview;

end;

procedure TPrintForm.Button4Click(Sender: TObject);
begin

  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_print_preview.html'), HH_DISPLAY_TOPIC, 0);

end;

Initialization

  Printer := TPrinter.Create;

Finalization

  Printer.Free;

end.
