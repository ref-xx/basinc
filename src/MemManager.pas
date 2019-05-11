unit MemManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math, ShellAPI, AxCtrls,
  FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, FastDraw, FastDrawEx, FastFiles, FastSize, FastFX, GraphicEx,
  Utility, ROMUtils,AnimPreview, Grids, MPHexEditor, Menus;

type
  TMBlock = record
     Valid: Boolean;
     Enabled: Boolean;
     BlockType,                  // Declares the block type. 0 - data, 1 - screen,etc.. see BuildManList
     wBlockIndex: integer;       // Declares the listindex
     FileName: AnsiString;       // The filename or blockname
     wAddress: integer;          // The Start Address of the block
     wLength: integer;           // The length of the block
     Modified: Byte;             // Used to determine if the block has modified by something. 0 - n/a, 1 - no, 2 - modified (yes)
     CanBreak: Boolean;          // Can it break execution? It will break if the block is modified, numeric.
  End;

  TmSlots = record
     Location: integer;
     Length: integer;
     data: string;
  End;

  pMBlock = ^TMBlock;

  

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
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
     Procedure DrawRuler;
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Insertintomemory1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Remove1Click(Sender: TObject);

    procedure DrawBlock(Address, Length: Integer; Text: String; Colr: byte; Locked: Boolean);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);

  private
    { Private declarations }
  public
    { Public declarations }
      mousedown:                        boolean;
      ManagerBin: Array[0..255] of TmSlots;

      Procedure UpdateManEntry(Index: Integer);
      Procedure BuildManList;
      Function CreateMBlock(FileName:string; wAddress, wLength, BlockType, wBlockIndex: integer;  CanBreak, Enabled: Boolean): Byte;

  end;

const

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
      ManArray: Array[0..255] of TMBlock;

implementation
uses FastCore,basic, BasinMain;
{$R *.DFM}


Function TmemmanagerForm.CreateMBlock(FileName:string; wAddress, wLength, BlockType, wBlockIndex: integer; CanBreak, Enabled: Boolean): Byte;
Var
  F: Byte;
  Free: boolean;
  Begin
  free:=false;
  For F := 0 To 254 Do
     If Not ManArray[F].Valid Then Begin
        ManArray[F].Enabled := Enabled;
        ManArray[F].wAddress := wAddress;
        ManArray[F].FileName := FileName;
        ManArray[F].wLength := wLength;
        ManArray[F].BlockType := BlockType;
        ManArray[F].CanBreak := CanBreak;
        ManArray[F].Modified := 0;
        ManArray[F].wBlockIndex := wBlockIndex;
        ManArray[F].Valid := True;
        free:= true;
        Break;
     End;

  if not free then f:=255; //all slots are full!
  Result := F;
End;



Procedure TmemmanagerForm.BuildManList;
Var
  F: Byte;
  LI: TListItem;
Begin

  Man_Updating := True;
  ListView1.Items.BeginUpdate;

  ManList := '';
  ListView1.Items.Clear;
  For F := 0 To 255 Do
     If ManArray[F].Valid Then Begin
        LI := ListView1.Items.Add;

        LI.SubItems.add(ManArray[f].Filename);

        
        Case ManArray[F].BlockType of

           0: Begin
                 LI.SubItems.Add('Raw');
              End;
           1: Begin
                 LI.SubItems.Add('Screen');
              End;
           2: Begin
                 LI.SubItems.Add('Code');
              End;
           3: Begin
                 LI.SubItems.Add('Udg');
              End;
           4: Begin
                 LI.SubItems.Add('Chars');
              End;
           5: Begin
                 LI.SubItems.Add('Map');
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
    birim:=(65535 div (FastIMG1.Bmp.Width-2));
    bstart:=4+(Address div birim);
    blen:=bstart+(length div birim)-1;
    bbottom:=fastimg1.Bmp.absheight-(btop+(bboy-btop)) ;

    //Draw Shadow&hilight
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
  Status: String;
begin
Bytes := GetWord(@Memory[RAMTOP])-GetWord(@Memory[STKEND]);
BytesUsed := GetWord(@Memory[E_LINE])-GetWord(@Memory[PROG])-1;
SpecTextToDIB(FastIMG1.Bmp, 100, FastIMG1.Height - 20, IntToStr(BytesUsed) + ',' + IntToStr(Bytes), 7, 0, 1, False, False) ;
//DrawRuler;
FastIMG1.Repaint;
end;


Procedure TMemManagerForm.DrawRuler;
Var
  PosInString: Boolean;
  X, Xp, LineX, StringLen, LineStart,
  Len, Count, NonAsciiMod, Idx: Integer;
  birim: integer;
  Btop: integer; // Block top

  Bytes, BytesUsed: Integer;
 Begin
    Btop:=6;
    birim:=(65535 div (FastIMG1.Bmp.Width-2));


    //clear all
    FillRect(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width , FastIMG1.Bmp.AbsHeight , tfPremiereBgk);


    //put boxes


    //put rom
    DrawBlock(0,16384,'ROM',0,True);

    //put screen
    DrawBlock(16384,6144,'Screen$',2,False);
    DrawBlock(22528,768,'Attr',2,False);

    //Bytes := GetWord(@Memory[RAMTOP])-GetWord(@Memory[STKEND]);
    BytesUsed := GetWord(@Memory[E_LINE])-GetWord(@Memory[PROG])-1;

    DrawBlock(PROG,bytesused,'Basic',0,True);
    DrawBlock(63200,768,'UDG',1,False);
    DrawBlock(23552,181,'Vars',1,True);




     // Draw the ruler tics and their numbers
  For X := 0 To (FastIMG1.Width - 2) Div (8*Opt_FontScale) - 1 Do Begin
     LineX := 2 + (X * 8 * Opt_FontScale);
     If Count Mod 8 = 0 Then Begin
        Len := 5;
       { SmallTextOut(FastIMG1.Bmp, IntToStr(Count), LineX - ((Length(IntToStr(Count))*4) Div 2) +1, FastIMG1.Height - 15, TFColorToTFColorA(TfSpecBlack));
     }End Else
        Len := 3;
     If Count Mod 2 = 0 Then Inc(Len, 2);
     Line(FastIMG1.Bmp, LineX, 0, LineX, Len, TfBlack);

        {Xp := 8 * Opt_FontScale;
        FillRect(FastIMG1.Bmp, LineX+1, 12, LineX+Xp, 14, TFGreen);
        If Count Mod 32 = 0 Then Begin
           SmallTextOut(FastIMG1.Bmp, ' ', LineX+(XP Div 2)-1, 10, TFColorToTFColorA(TfSpecRed));
        End;
              }

     Inc(Count);
  End;





  // Draw the mouse position
  If (MousePos.X >= 2) and (MousePos.X < FastIMG1.Bmp.Width-2) Then
   Begin
     Line(FastIMG1.Bmp, MousePos.X, 2, MousePos.X, FastIMG1.Bmp.Height-4, tfPremiereRed);
     txtaddress.Text:= IntToStr((birim* (MousePos.X-2))) ;

    end;


  // add borders
    FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width-1 , FastIMG1.Bmp.absHeight-1 , tfBlack);
    //FastDrawEx.Rectangle(FastIMG1.Bmp, 1, 1, FastIMG1.Bmp.Width-2 , FastIMG1.Bmp.Height-2 , tfWhite);

  FastIMG1.Repaint;


end;


procedure TMemManagerForm.FastIMG1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  var
  newpos: integer;
  begin
MousePos := Point(X, Y);
DrawRuler;

If MouseDown Then Begin

newpos:=strtoint(txtaddress.Text);
 If (NewPos > -1) and (newpos<65536) Then Begin
     MPHexEditor1.Seek(NewPos, 0);

     MPHexEditor1.SetFocus;
  End;

end;


end;





procedure TMemManagerForm.Button2Click(Sender: TObject);
Var
Lw1: TListItem;
idx: integer;
begin

idx:=CreateMblock('Gnome.scr',16384,6912,1,0,false,true);
idx:=CreateMblock('attribs.bin',22528,768,1,0,false,true);

BuildManList;


end;

procedure TMemManagerForm.FormShow(Sender: TObject);
begin
  MPHexEditor1.SelectAll;
  MPHexEditor1.DeleteSelection;
  MPHexEditor1.AppendBuffer(@Memory[0], 65535);
  MPHexEditor1.ResetSelection(True);
end;

procedure TMemManagerForm.FastIMG1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
newpos: integer;
begin
MouseDown := True;
end;

procedure TMemManagerForm.FastIMG1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
MouseDown := false;
end;

procedure TMemManagerForm.Insertintomemory1Click(Sender: TObject);
begin
        If ListView1.Selected <> nil Then Begin

                //listview1.Items(ListView1.Selected.Index+1)

        end;
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

procedure TMemManagerForm.Remove1Click(Sender: TObject);
begin
If ListView1.Selected <> nil Then Begin
     ManArray[Ord(ManList[ListView1.Selected.Index+1])].Valid := False;
     BuildManList;
  End;
end;

procedure TMemManagerForm.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  // Use this event to test if the checked property is changed.
  If Not Man_Updating Then
     If Item.SubItems.Count <> 0 Then
        If Item.Checked <> ManArray[Ord(ManList[Item.Index+1])].Enabled Then Begin
           ManArray[Ord(ManList[Item.Index+1])].Enabled := Item.Checked;
           UpdateManEntry(Item.Index+1);
        End;
end;



Procedure TMemManagerForm.UpdateManEntry(Index: Integer);
Var
  Man: pmblock;
  LI: TListItem;
Begin
  Man_Updating := True;
  If (Index < 255) and (Index > 0) Then Begin
     Man := @ManArray[Ord(ManList[Index])];
     If Man.Valid Then Begin
        LI := ListView1.Items[Index-1];

     End;
  End;
  Man_Updating := False;
End;

end.
