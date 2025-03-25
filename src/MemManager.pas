unit MemManager;

interface

// memManager
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ClipBrd,  CompressorUnit, OptimizerUnit, Forms, Dialogs, Math, ShellAPI, AxCtrls,
  FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, Filing, FastDraw, FastDrawEx, FastFiles, FastSize, FastFX,
  Utility, ROMUtils,AnimPreview, Grids, MPHexEditor, Menus, BinaryForm,FastCore,basic, BasinMain, BinaryGrab, BlockUnit;

type


  MSlot = record
     Location: integer;
     Length: integer;
     data: array of Byte;
  End;

    MBlock = record
     Valid: Boolean;
     Enabled: Boolean;
     BlockType,                  // Declares the block type. 0 - data, 1 - screen,etc.. see BuildManList
     State,                      // 0-raw, 1-packed(zx0) , 2-packed(z80) with embedded depacker
     wBlockIndex: integer;       // Declares the listindex
     FileName: String;       // The filename or blockname
     wAddress: integer;          // The Start Address of the block
     wLength: integer;           // The length of the block
     dAddress: integer;          // Unpack Destination Address
     Modified: Byte;             // Used to determine if the block has modified by something. 0 - n/a, 1 - no, 2 - modified (yes)
     CanBreak: Boolean;          // Can it break execution? It will break if the block is modified, numeric.
     Slot: Mslot;
  End;

  pMBlock = ^MBlock;

  

  TMemManagerForm = class(TForm)
    FastIMG1: TFastIMG;
    Button1: TButton;
    txtaddress: TEdit;
    MPHexEditor1: TMPHexEditor;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ImportBinary1: TMenuItem;
    Grabcurrentscreen1: TMenuItem;
    Grabfrommemory1: TMenuItem;
    N1: TMenuItem;
    Savethismanagement1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    View1: TMenuItem;
    Scale1: TMenuItem;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    Insertintomemory1: TMenuItem;
    Rewrite1: TMenuItem;
    Remove1: TMenuItem;
    Properties1: TMenuItem;
    N2: TMenuItem;
    ImportFile1: TMenuItem;
    DeclareNewBlock1: TMenuItem;
    Deselect1: TMenuItem;
    N3: TMenuItem;
    Tools1: TMenuItem;
    SplitSelected1: TMenuItem;
    NewBlockfromCurrentState1: TMenuItem;
    Putto1: TMenuItem;
    CHARS1: TMenuItem;
    SCREEN1: TMenuItem;
    UDG1: TMenuItem;
    RAMTOP1: TMenuItem;
    ListView1: TListView;
    Compresszx01: TMenuItem;
    Compressandembeddepacker1: TMenuItem;
    Rename1: TMenuItem;
    N4: TMenuItem;
    VARS1: TMenuItem;
    Edit2: TMenuItem;
    N491921: TMenuItem;
    NextFreeAddress1: TMenuItem;
    N5: TMenuItem;
    Close1: TMenuItem;
    N6: TMenuItem;
    SetType1: TMenuItem;
    Screen2: TMenuItem;
    Data1: TMenuItem;
    Code1: TMenuItem;
    UDG2: TMenuItem;
    Chars2: TMenuItem;
    Map1: TMenuItem;
    Beeps1: TMenuItem;
    AYPSG1: TMenuItem;
    Pack1: TMenuItem;
    PopupMenu2: TPopupMenu;
    CreateBlockFromSelectedArea1: TMenuItem;
    Jumpto1: TMenuItem;
    GridToggle1: TMenuItem;
    ApplyEditstoMemory1: TMenuItem;
    ApplyEditstoMemory2: TMenuItem;
    N7: TMenuItem;
    ViewasHex1: TMenuItem;
    Copy2: TMenuItem;
    SendtoBinaryGrabber1: TMenuItem;
    Button3: TButton;
    Button4: TButton;
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
     Procedure DrawRuler;
     Procedure HexCursor;
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ContextMenuClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);

    procedure DrawBlock(Address, Length: Integer; Text: String; Colr: byte; Locked: Boolean);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FastIMG1Click(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure ListView1Edited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure ListView1DblClick(Sender: TObject);



  private
    { Private declarations }
  public
    { Public declarations }
      mousedown:                        boolean;
      ManagerBin: Array[0..1023] of MSlot;
      HexShown: Integer;

      Procedure UpdateManEntry(Index: Integer);
      Procedure BuildManList;
      Function CreateMBlock(FileName:string; wAddress, wLength, BlockType, wBlockIndex: integer;  CanBreak, Enabled: Boolean ; DataChunk: TByteArray ): Byte;
      Function DuplicateMBlock(index: integer): Byte;
      Procedure LoadFileToByteArray;
      function StringToTBytes(Value: string):  TByteArray;
      

      Procedure PackZX0(quick: Boolean; dataToPack: Array Of Byte); //inputs zx0input and outputs zx0output
  end;

var
  zx0input,
  zx0output:           Array[0..65535] of Byte;
  zx0Status,
  zx0iLength,
  zx0iPointer,
  zx0oLength,
  zx0oPointer:          Integer;

const

MAXSLOTS: Integer = 1023;

tfPremiereLocked:  TFColor  = (b:140; g:140; r:189);
tfPremiereLockedheader:  TFColor  = (b:130; g:130; r:147);

tfPremiereunLocked:  TFColor  = (b:140; g:189; r:140);
tfPremiereunLockedheader:  TFColor  = (b:130; g:150; r:130);

tfPremiereWarn:  TFColor  = (b:140; g:190; r:187);
tfPremiereWarnheader:  TFColor  = (b:130; g:150; r:150);

tfPremiereBgk:  TFColor  = (b:187; g:189; r:187);
tfPremiereRed:  TFColor  = (b:0; g:0; r:130);

var
      MousePos:                          TPoint;

      Man_Updating:                      boolean;
      ManList: AnsiString;

      MemManagerForm: TMemManagerForm;
      ManArray: Array[0..1023] of MBlock;

implementation

{$R *.DFM}


Procedure TmemmanagerForm.PackZX0(quick: Boolean; dataToPack: Array Of Byte);
var
Idelta: TIntegerArray;
Iblock: TBlock;
zx0c: TCompressor;
zx0o: TOptimizer;
i: Integer;
Begin
       //if quick then Result:=dataToPack;
        SetLength(Idelta,1);
        Idelta[0]:=0;
        zx0iLength:=Length(dataToPack);
        For i := 0 To zx0iLength-1 Do Begin
          zx0input[i]:=dataToPack[i];
        End;


        Iblock:=zx0o.Optimize(0,32640);

        
        zx0ipointer:=0;

        zx0c.Compress(Iblock,0,false,false,dataToPack, Idelta);

End;
//optimize(input TByteArray, 0,   2176 ya da 32640)

Function TmemmanagerForm.DuplicateMBlock(index: integer): Byte;
Var
  F: Integer;
  Free: boolean;
Begin
  free:=false;
  For F := 0 To MAXSLOTS Do
  Begin
    If Not ManArray[F].Valid Then Begin
        ManArray[F]:= ManArray[index];
        ManArray[F].FileName:='D_'+ManArray[F].FileName;
        free:= true;
        Break;
    End;
  End;
  if not free then F:=MAXSLOTS; //all slots are full!
  Result := F;
End;

Function TmemmanagerForm.CreateMBlock( FileName:string; wAddress, wLength, BlockType, wBlockIndex: integer; CanBreak, Enabled: Boolean;   DataChunk: TByteArray ): Byte;
Var
  F,i: Integer;
  Free: boolean;
  Begin
  free:=false;
  For F := 0 To MAXSLOTS Do
     If Not ManArray[F].Valid Then Begin
        ManArray[F].Enabled := Enabled;
        ManArray[F].wAddress := wAddress;
        ManArray[F].FileName := FileName;
        ManArray[F].wLength := wLength;
        ManArray[F].BlockType := BlockType;
        ManArray[F].CanBreak := CanBreak;
        ManArray[F].Modified := 0;
        ManArray[F].wBlockIndex := wBlockIndex;
        ManArray[F].State := 0;
        ManArray[F].Valid := True;
        free:= true;
        SetLength(ManArray[F].Slot.data,wLength);
        //CopyMemory(@ManArray[F].Slot.data[0],MPHexEditor1.SelectionAsText , wLength);
        //CopyMemory(@ManArray[F].Slot.data[0],@DataChunk[wAddress], wLength);
        for i := wAddress to wAddress+wLength-1 do
            ManArray[F].Slot.data[i-wAddress] := DataChunk[i];
        Break;
     End;

  if not free then f:=MAXSLOTS; //all slots are full!
  Result := F;
End;



Procedure TmemmanagerForm.BuildManList;
Var
  F: Integer;
  LI: TListItem;
  packS: String;
Begin

  Man_Updating := True;
  ListView1.Items.BeginUpdate;

  ManList := '';
  ListView1.Items.Clear;
  For F := 0 To MAXSLOTS Do
     If ManArray[F].Valid Then Begin
        LI := ListView1.Items.Add;

        LI.SubItems.add(ManArray[f].Filename);

                Case ManArray[F].modified of
           0:   Begin
                packS:='';
                End;
           1:   Begin
                packS:='(Packed)';
                End;
           2:   Begin
                PackS:='(Packed+Depacker)';
                End;
        End;
        
        Case ManArray[F].BlockType of

           0: Begin
                 LI.SubItems.Add('Raw'+PackS);
              End;
           1: Begin
                 LI.SubItems.Add('Screen'+PackS);
              End;
           2: Begin
                 LI.SubItems.Add('Code'+PackS);
              End;
           3: Begin
                 LI.SubItems.Add('Udg'+PackS);
              End;
           4: Begin
                 LI.SubItems.Add('Chars'+PackS);
              End;
           5: Begin
                 LI.SubItems.Add('Map'+PackS);
              End;
           6: Begin
                 LI.SubItems.Add('Beeps'+PackS);
              End;
           7: Begin
                 LI.SubItems.Add('AY'+PackS);
              End;
        End;


        if  ManArray[f].wAddress >-1  then LI.SubItems.add(inttostr(ManArray[f].waddress)) Else LI.SubItems.add('n/a') ;
        LI.SubItems.add(inttostr(ManArray[f].wlength));



        Case ManArray[F].modified of

           0: Begin // NA
                 LI.SubItems.Add('Not in memory');
              End;
           1: Begin // No
                 LI.SubItems.Add('Intact');
              End;
           2: Begin // Modified
                 LI.SubItems.Add('Modified');
              End;

        End;

        LI.Checked := ManArray[F].Enabled;
        ManList := ManList + AnsiChar(F);

     End;


  ListView1.Items.EndUpdate;
  Man_Updating := False;

  //Button4.Enabled := ListView1.Selected <> Nil;
  //Button5.Enabled := Button4.Enabled;

End;

Procedure TmemmanagerForm.DrawBlock(Address, Length: Integer; Text: String; Colr: byte;Locked: Boolean);
var
 birim, blen, bstart, btop, bbottom, bboy : integer;
 ft: TFont;
 Col: TFColor;
 Scole: TfColor;

Begin

   // stacked if-elses... Case-of doesn't work on this occasion?!
   //if you manage, please fix this :D

    if  Colr=0 then begin
      col:= tfPremiereLocked;
      Scole:= tfPremiereLockedHeader;
    end else if colr=1 then begin
      col:= tfPremiereUnLocked;
      Scole:= tfPremiereUnLockedHeader;
    end else if colr=2 then begin
       col:= tfPremiereWarn;
      Scole:= tfPremiereWarnHeader;
    end;

    ft := TFont.Create;
    ft.Name := 'Arial';
    ft.size:= 8;
    btop:=6;
    bboy:=fastimg1.Bmp.absheight-4;
    birim:=(65535 div (FastIMG1.Bmp.Width-4));
    bstart:=2+(Address div birim);
    blen:=bstart+(length div birim)-1;
    bbottom:=4;

    //Draw Shadow&highlight
    rectangle(fastimg1.bmp,bstart,bbottom-1, blen,bboy-1,tfBlack);
    rectangle(fastimg1.bmp,bstart,bbottom+1, blen-1,bboy-1,tfWhite);

    //Draw  Block and the header
    fillrect(FastIMG1.Bmp,bstart+1,Btop,blen-1,bboy,col);
    fillrect(FastIMG1.Bmp,bstart+1,Btop,blen-1,bboy div 4 ,scole);

    //Draw Heading
    fasttextrectNOFMT(fastimg1.bmp,ft,Bstart+2,Btop,blen-1,bboy div 4,text);



end;

procedure TMemManagerForm.FormResize(Sender: TObject);
var
LetterW, CurValue: Integer;
begin
  LetterW := Canvas.TextWidth('a');
  CurValue := MPHexEditor1.ClientWidth - MPHexEditor1.GutterWidth - (LetterW * 2);
  CurValue := CurValue Div 5;
  If (CurValue Div LetterW) > 0 Then MPHexEditor1.BytesPerRow := (CurValue Div LetterW);


   FastIMG1.SetBounds(ListView1.left,ListView1.left+ ListView1.top + listview1.Height ,  (mphexeditor1.left+mphexeditor1.Width)-listview1.left , 80);

   FastIMG1.Bmp.SetSize(fastimg1.width , fastimg1.height , 24);
   FastIMG1.Bmp.Clear(tfGray);
   FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width , FastIMG1.Bmp.Height , tfGray);
   FastDrawEx.Rectangle(FastIMG1.Bmp, 1, 1, FastIMG1.Bmp.Width-2 , FastIMG1.Bmp.Height-2 , tfWhite);

  drawruler;
  
  FastIMG1.Repaint;
end;

procedure TMemManagerForm.Button1Click(Sender: TObject);
Var
  Bytes, BytesUsed: Integer;
begin
Bytes := GetWord(@Memory[RAMTOP])-GetWord(@Memory[STKEND]);
BytesUsed := GetWord(@Memory[E_LINE])-GetWord(@Memory[PROG])-1;
SpecTextToDIB(FastIMG1.Bmp, 100, FastIMG1.Height - 20, IntToStr(BytesUsed) + ',' + IntToStr(Bytes), 7, 0, 1, False, False) ;
//DrawRuler;
FastIMG1.Repaint;
end;


Procedure TMemManagerForm.DrawRuler;
Var

  X: Integer;
  Dbirim: Double;
  birim: integer;
  Btop,per16K: integer; // Block top
  BytesUsed: Integer;
 Begin
    Btop:=6;
    Dbirim:= 65535/(FastIMG1.Bmp.Width-4);
    birim:= Round(Dbirim);


    //clear all
    FillRect(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width , FastIMG1.Bmp.AbsHeight , tfPremiereBgk);


    //Add RAM PAGE guides
    per16K:=  16384 div birim;
    X:=0;
    while X <= (FastIMG1.Width - 100) do
    Begin
      Line(FastIMG1.Bmp, 2+X, 0, 2+X, FastIMG1.Height, TfBlack);
      Inc(X,per16K);
    End;
    //txtaddress.Text := IntToStr (per16K);



    //put boxes


    //put rom
    DrawBlock(0,16384,'ROM',0,True);

    //put screen
    DrawBlock(16384,6144,'Screen$',2,False);
    DrawBlock(22528,768,'Attr',2,False);

    //Bytes := GetWord(@Memory[RAMTOP])-GetWord(@Memory[STKEND]);
    BytesUsed := GetWord(@Memory[E_LINE])-GetWord(@Memory[PROG])-1;

    //
    DrawBlock(GetWord(@Memory[PROG]),BytesUsed,'Basic',0,True);
    DrawBlock(GetWord(@Memory[UDG]),768,'UDG',1,False);
    DrawBlock(GetWord(@Memory[VARS]),181,'Vars',1,True);




  // Draw the mouse position
  If (MousePos.X >=2) and (MousePos.X < FastIMG1.Bmp.Width-2) Then
   Begin
     Line(FastIMG1.Bmp, MousePos.X, 2, MousePos.X, FastIMG1.Bmp.Height-2, tfPremiereRed);
     txtaddress.Text:= IntToStr((birim* (MousePos.X-2))) ;

    end;


  // add borders
    FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width-1 , FastIMG1.Bmp.absHeight-1 , tfBlack);
    //FastDrawEx.Rectangle(FastIMG1.Bmp, 1, 1, FastIMG1.Bmp.Width-2 , FastIMG1.Bmp.Height-2 , tfWhite);

  FastIMG1.Repaint;


end;


procedure TMemManagerForm.FastIMG1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

  begin
MousePos := Point(X, Y);
DrawRuler;

If MouseDown Then Begin

         HexCursor;

end;


end;


procedure TMemManagerForm.HexCursor;
var
 newpos: integer;
begin
if HexShown<>9999 Then Begin
      MPHexEditor1.SelectAll;
  MPHexEditor1.DeleteSelection;
  MPHexEditor1.AppendBuffer(@Memory[0], 65535);
  MPHexEditor1.ResetSelection(True);
  HexShown:=9999;
End;

   newpos:=strtoint(txtaddress.Text);
 If (NewPos > -1) and (newpos<65536) Then Begin
     MPHexEditor1.Seek(NewPos, 0);

     MPHexEditor1.SetFocus;
  End;

end;



procedure TMemManagerForm.FormShow(Sender: TObject);
begin
  HexShown:=9999;
  MPHexEditor1.SelectAll;
  MPHexEditor1.DeleteSelection;
  MPHexEditor1.AppendBuffer(@Memory[0], 65535);
  MPHexEditor1.ResetSelection(True);
end;

procedure TMemManagerForm.FastIMG1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);


begin
MouseDown := True;
end;

procedure TMemManagerForm.FastIMG1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
MouseDown := false;
end;

procedure TMemManagerForm.ContextMenuClick(Sender: TObject);
var

idx: Integer;
SelS: String;
packd: TByteArray;
output: TByteArray;
begin
 Case (Sender As TComponent).Tag Of
    12: //duplicate
      Begin
        If ListView1.Selected <> nil Then Begin
             DuplicateMBlock(Ord(ManList[ListView1.Selected.Index+1]));
             BuildManList;
        End;
      End;
    13: //rename
      Begin
      if ListView1.Selected <> nil then
        begin
        // Start editing the caption of the selected item
        ListView1.Selected.EditCaption;
        end;
      End;
     14: //delete
      Begin
        If ListView1.Selected <> nil Then Begin
        ManArray[Ord(ManList[ListView1.Selected.Index+1])].Valid := False;
        BuildManList;
        End;
      End;
      19: //Pack ZX0
       Begin
         If ListView1.Selected <> nil Then Begin
           //packd:=ArrayToBytes();

           //zx0buffer:=ManArray[Ord(ManList[ListView1.Selected.Index+1])].Slot.data;
           zx0Status:=0;
           PackZX0(false,ManArray[Ord(ManList[ListView1.Selected.Index+1])].Slot.data);
           if zx0Status=1 Then output:=ArrayToTbytes(zx0output,zx0oLength);

           CreateMblock(ManArray[Ord(ManList[ListView1.Selected.Index+1])].FileName,ManArray[Ord(ManList[ListView1.Selected.Index+1])].wAddress,zx0oLength,0,0,false,true,output);
           BuildManList;
         End;

       End;
      20: //Send to Binary Grabber
       Begin
         If ListView1.Selected <> nil Then Begin
           if HexShown <> ListView1.Selected.Index+1 Then Begin
           MPHexEditor1.SelectAll;
           MPHexEditor1.DeleteSelection;
           MPHexEditor1.AppendBuffer(@ManArray[Ord(ManList[ListView1.Selected.Index+1])].Slot.data[0], Length(ManArray[Ord(ManList[ListView1.Selected.Index+1])].Slot.data));
           MPHexEditor1.ResetSelection(True);
           HexShown:=ListView1.Selected.Index+1;
           End;
         End;
       End;

     21: //Send to Binary Grabber
       Begin
         If ListView1.Selected <> nil Then Begin

              BinaryWindow.ClearBinaries;
              SelS := '';
              For Idx := 0 To ManArray[Ord(ManList[ListView1.Selected.Index+1])].wLength-1 Do
                 SelS := SelS + Chr(ManArray[Ord(ManList[ListView1.Selected.Index+1])].slot.data[Idx]);
              BinaryWindow.AddBinary('Memory Block ('+IntToStr(ManArray[Ord(ManList[ListView1.Selected.Index+1])].wAddress)+', '+IntToStr(ManArray[Ord(ManList[ListView1.Selected.Index+1])].wLength)+')', SelS);
              CentreFormOnForm(BinaryWindow, Self);
              ShowWindow(BinaryWindow, True);
         End;
       End;
     50..57: //set Type
     Begin
        If ListView1.Selected <> nil Then Begin

           Idx:=(Sender As TComponent).Tag-50;
           ManArray[Ord(ManList[ListView1.Selected.Index+1])].BlockType:=Idx;
           BuildManList;
        End;
     End;
     80: //HexEditor Create Block
      Begin
          Sels:=MPHexEditor1.SelectionAsText;
          if SelS<>'' Then Begin
             idx:=CreateMblock('Selection.bin',0,MPHexEditor1.SelCount,0,0,false,true,StringToTBytes(SelS));
             BuildManList;
          End;
      End;

      85: //Copy
      Begin
          Sels:=MPHexEditor1.SelectionAsText;
          if SelS<>'' Then ClipBoard.SetTextBuf(PChar(Sels));
      End;
    End;

end;

function TMemManagerForm.StringToTBytes( Value: string):  TByteArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Value));
  for i := 1 to Length(Value) do
    Result[i - 1] := Ord(Value[i]);
end;



procedure TMemManagerForm.PopupMenu1Popup(Sender: TObject);
begin
     Insertintomemory1.Enabled := ListView1.SelCount > 0;
     Rewrite1.Enabled := ListView1.SelCount > 0;
     Putto1.Enabled := ListView1.SelCount > 0;

     NewBlockfromCurrentState1.Enabled := ListView1.SelCount > 0;
     Remove1.Enabled := ListView1.SelCount > 0;
     Properties1.Enabled := ListView1.SelCount > 0;

     ImportFile1.Enabled := ListView1.SelCount < 1;

     
end;

procedure TMemManagerForm.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  // Use this event to test if the checked property is changed.
  If Not Man_Updating Then
     If Item.SubItems.Count <> 0 Then Begin
        If Item.Checked <> ManArray[Ord(ManList[Item.Index+1])].Enabled Then Begin
           ManArray[Ord(ManList[Item.Index+1])].Enabled := Item.Checked;
           UpdateManEntry(Item.Index+1);
        End;

     End;

end;



Procedure TMemManagerForm.UpdateManEntry(Index: Integer);
Var
  Man: pmblock;
  LI: TListItem;
Begin
  Man_Updating := True;
  If (Index < MAXSLOTS) and (Index > 0) Then Begin
     Man := @ManArray[Ord(ManList[Index])];
     If Man.Valid Then Begin
        LI := ListView1.Items[Index-1];

     End;
  End;
  Man_Updating := False;
End;



procedure TMemManagerForm.FastIMG1Click(Sender: TObject);
begin
 HexCursor;
end;

procedure TMemManagerForm.MenuClick(Sender: TObject);
var
Idx: Integer;
begin
  Case (Sender As TComponent).Tag Of
    40: //Import File
      Begin
         LoadFileToByteArray;
      End;
    41: //Grab from screen
      Begin
           Idx:=CreateMblock('untitled.scr',16384,6912,1,0,False,True,ArrayToTBytes(Memory,0));
           BuildManList;
      End;

    42:
        Begin // Memory grabber
           CentreFormOnForm(BinaryGrabWindow, Self);
           ShowWindow(BinaryGrabWindow, True);
           If Not BinaryGrabWindow.Cancelled Then Begin
           if BinaryGrabWindow.BlockAddress<16384 Then Begin
              Idx:= CreateMblock('RomGrab.bin',BinaryGrabWindow.BlockAddress,BinaryGrabWindow.BlockSize,0,0,False,True,ArrayToTBytes(Memory,0));
              End Else Begin
              Idx:= CreateMblock('RamGrab.bin',BinaryGrabWindow.BlockAddress,BinaryGrabWindow.BlockSize,0,0,False,True,ArrayToTBytes(Memory,0));

              End;
              BuildManList;
           End;
        End;
    43: //Create zx0 Depacker
      Begin
         LoadFileToByteArray;
      End;

  End;
end;

procedure TMemManagerForm.ListView1Edited(Sender: TObject; Item: TListItem;
  var S: String);
var
f: Integer;
k: String;
begin
 If Not Man_Updating Then
     If Item.SubItems.Count <> 0 Then Begin
           f:=Ord(ManList[Item.Index+1]);
           k :=S; //to avoid Access Violation
           ManArray[f].FileName:=k;
           BuildManList;
     End;
end;

procedure TMemManagerForm.ListView1DblClick(Sender: TObject);
begin
 if ListView1.Selected <> nil then
  begin

        if HexShown <> ListView1.Selected.Index+1 Then Begin
           MPHexEditor1.SelectAll;
           MPHexEditor1.DeleteSelection;
           MPHexEditor1.AppendBuffer(@ManArray[Ord(ManList[ListView1.Selected.Index+1])].Slot.data[0], Length(ManArray[Ord(ManList[ListView1.Selected.Index+1])].Slot.data));
           MPHexEditor1.ResetSelection(True);
           HexShown:=ListView1.Selected.Index+1;
        End;
  end;
end;

Procedure TMemManagerForm.LoadFileToByteArray;
var
  FileStream: TFileStream;
  Size: Integer;
  FileName: string;
  FTypes: TBASICFiles;
  fileContents: Array of byte;
begin

  FTypes := [ FTBin, FTCh8, FTScr,FTSpecCHR,FTAssembly,FTAll ];
  If Filename = '' Then Filename := OpenFile(Handle, 'Save Screen Image as...', FTypes, '', False, True);
  If Filename = '' Then Exit;
   Size := FileSizeX(FileName);
   if  (Size >= 0) and (Size < 131072) Then Begin
         FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
           try
            // Set the length of the byte array to match the file size
            SetLength(fileContents, Size);

            // Read the contents of the file into the byte array
            FileStream.ReadBuffer(fileContents[0], Size);
          finally
            // Free the TFileStream instance
            FileStream.Free;
            CreateMblock(ExtractFileName(FileName),0,Size,0,0,False,True,ArrayToTBytes(fileContents,0));
            BuildManList;
          end;
   End
   Else
   Begin
        ShowMessage('File is empty or too big. (Max Block Size: 128kb)');
   End;


end;


end.
