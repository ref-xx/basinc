unit MemManager;

interface

// memManager
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ClipBrd, Forms, Dialogs, Math, ShellAPI, AxCtrls,
  FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, Filing, FastDraw, FastDrawEx, FastFiles, FastSize, FastFX,Display,
  Utility, ROMUtils,AnimPreview, Grids, MPHexEditor, Menus, BinaryForm,FastCore,basic, BasinMain, BinaryGrab, BlockUnit,zx0packer;

type


  // 1. PROJECT HEADER
  TManProjectHeader = packed record
    Signature: Array[0..3] of Char; // 'BMM1'
    BlockCount: Integer;
    Reserved: Array[0..31] of Byte; // Future project-wide settings
  end;

  // 2. FILE BLOCK METADATA (Fixed Size for Disk Storage)
  TManBlockMeta = packed record
    Enabled: Boolean;
    BlockType: Byte;
    State: Byte;
    wBlockIndex: Integer;
    FileName: ShortString; // 256 bytes
    wAddress: Integer;
    wLength: Integer;
    dAddress: Integer;
    Modified: Byte;        // Legacy/GUI status
    CanBreak: Boolean;
    Status: Word;          // Stores MS_INMEMORY, MS_TRACKED, etc.
    wPage:   Word;          // Stores memory page for 128k
    Reserved: Array[0..13] of Byte; // NEW: Padding for future features (BMM2, BMM3...)
  end;

  MSlot = record
     Location: integer;
     Length: integer;
     data: TByteArray; //data: array of Byte;
  End;

    MBlock = record
     Valid: Boolean;
     Enabled: Boolean;
     BlockType: Byte;                // Declares the block type. 0 - data, 1 - screen,etc.. see BuildManList
     State: Byte;                     // 0-raw, 1-packed(zx0) , 2-packed(z80) with embedded depacker
     wBlockIndex: integer;       // Declares the listindex
     FileName: String;       // The filename or blockname
     wAddress: integer;          // The Start Address of the block
     wLength: integer;           // The length of the block
     dAddress: integer;          // Unpack Destination Address
     Modified: Byte;             // Used to determine if the block has modified by something. 0 - n/a, 1 - no, 2 - modified (yes)
     CanBreak: Boolean;          // Can it break execution? It will break if the block is modified, numeric.
     Status: Word;
     wPage:   Word;
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
    N1: TMenuItem;
    Savethismanagement1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    Insertintomemory1: TMenuItem;
    Rewrite1: TMenuItem;
    Remove1: TMenuItem;
    Properties1: TMenuItem;
    N2: TMenuItem;
    Deselect1: TMenuItem;
    N3: TMenuItem;
    NewBlockfromCurrentState1: TMenuItem;
    Putto1: TMenuItem;
    CHARS1: TMenuItem;
    SCREEN1: TMenuItem;
    UDG1: TMenuItem;
    RAMTOP1: TMenuItem;
    ListView1: TListView;
    Rename1: TMenuItem;
    N4: TMenuItem;
    VARS1: TMenuItem;
    Edit2: TMenuItem;
    N491921: TMenuItem;
    NextFreeAddress1: TMenuItem;
    N5: TMenuItem;
    Close1: TMenuItem;
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
    OpenManagement1: TMenuItem;
    CutBlock1: TMenuItem;
    CopyBlock1: TMenuItem;
    PasteBlock1: TMenuItem;
    N8: TMenuItem;
    N327681: TMenuItem;
    N9: TMenuItem;
    Blocks1: TMenuItem;
    Importbinaryfromfile1: TMenuItem;
    Grabcurrentscreen2: TMenuItem;
    Grabfrommemory2: TMenuItem;
    Createzx0depackerblock1: TMenuItem;
    N6: TMenuItem;
    SplitSelected3: TMenuItem;
    N12: TMenuItem;
    New1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    Lift1: TMenuItem;
    ToggleROMView1: TMenuItem;
    zx0Depacker1: TMenuItem;
    Packed1: TMenuItem;
    Compresszx01: TMenuItem;
    Compressandembeddepacker1: TMenuItem;
    Duplicate1: TMenuItem;
    N13: TMenuItem;
    oggleTrack1: TMenuItem;
    Lock1: TMenuItem;
    PasteasText1: TMenuItem;
    GrabBasic1: TMenuItem;
    ClearManagement1: TMenuItem;
    ExportAddressestoListing1: TMenuItem;
    N14: TMenuItem;
    Help1: TMenuItem;
    Feedback1: TMenuItem;
    HelponMemoryManagerWindow1: TMenuItem;
    RewriteAllBlocks1: TMenuItem;
    Refresh1: TMenuItem;
    OpenRecent1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
     Procedure DrawLayout;
     Procedure HexCursor;
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X,   Y: Integer);
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
    procedure txtaddressKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListView1Click(Sender: TObject);
    procedure ToggleROMView1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Feedback1Click(Sender: TObject);
    procedure HelponMemoryManagerWindow1Click(Sender: TObject);





  private
    { Private declarations }
    FClipboardBlock: MBlock;
    FClipboardValid: Boolean;

    //Rom View
    FZoomMode: Boolean; // False: Full (0-65535), True: RAM Only (16384-65535)
    FViewStart: Integer;  // def: 0
    FViewLength: Integer; // def: 65536

    // Drag & Drop Degiskenleri
    FDragActive: Boolean;       // Su an sürükleme yapiliyor mu?
    FDragBlockIdx: Integer;     // Sürüklenen blogun ManArray indeksi
    FDragOffset: Integer;       // Tiklama noktasi ile blok baslangici arasindaki fark
    FDragCurrentAddr: Integer;  // Sürükleme esnasindaki geçici adres
    FDragMinLimit: Integer;     // Sola gidebilecegi en son nokta
    FDragMaxLimit: Integer;     // Saga gidebilecegi en son nokta
    procedure LiftBlockFromLayout;
    // Helpers to calculate scale and offset based on Zoom Mode
    procedure GetViewParams(out StartAddr: Integer; out BytesPerPixel: Double);
    function AddrToPixel(Addr: Integer): Integer;
    function PixelToAddr(X: Integer): Integer;
    procedure PerformZoom(ZoomIn: Boolean);
    procedure ChangeBlockLength(BlockIndex: Integer; NewLength: Integer);
    procedure CheckChanges;
    procedure ExportMetaData;

  public
    { Public declarations }
      mousedown:                        boolean;
      ManagerBin: Array[0..1023] of MSlot;
      HexShown,NewCounter: Integer;

      Procedure UpdateManEntry(Index: Integer);
      Procedure BuildManList;
      Function CreateMBlock(FileName:string; wAddress, wLength, BlockType, SetStatusBits, wPage: integer;  CanBreak, Enabled: Boolean ; DataChunk: TByteArray ): Byte;
      Function DuplicateMBlock(index: integer): Byte;
      Procedure LoadFileToByteArray;
      function StringToTBytes(Value: string):  TByteArray;
      function PackArrayToTBytes(Value:  TZX0ByteArray): TByteArray ;
      procedure SaveManagementProject(FileName: String);
      procedure LoadManagementProject(FileName: String; AppendData: Boolean);
      procedure InsertBlockAtCursor;
      //Procedure PackZX0(quick: Boolean; dataToPack: Array Of Byte); //inputs zx0input and outputs zx0output
      Procedure AddMemBlock(BlockName: String; Address: Integer; Data: TByteArray);
      // Public Helper Functions for External Access
    function GetSelectedBlockData: TByteArray;
    function GetSelectedBlockName: String;
    function GetSelectedBlockAddress: Integer;
      
  end;

var
  zx0input,
  zx0output:           Array[0..65535] of Byte;
  zx0Status,
  zx0iLength,
  zx0iPointer,
  zx0oLength,
  zx0oPointer:          Integer;
    FCurrentAddr: Integer;  // O anki seçili adresi hafizada tutmak için
  FLastMouseX: Integer;   // Farenin son X pozisyonunu takip etmek için

const
  // Status Bitleri
  MS_STORED    = 0;   // Idle
  MS_INMEMORY  = 1;   // Location adresine yüklenmis
  MS_TRACKED   = 2;   // Bellekte izleniyor
  MS_COMPRESSED= 4;   // ZX0 ile sikisik
  MS_CHANGED   = 8;   // Bellekte bozulmus/degismis
  MS_INUSE     = 16;  // Export'a dahil
  MS_WDECODER  = 32;  // Depacker ekli
  MS_MODIFIED  = 64;  // Hex editörde degismis
  
 const
  MB_RAW     = 0;
  MB_SCREEN  = 1;
  MB_CODE    = 2;
  MB_UDG     = 3;
  MB_CHARS   = 4;
  MB_MAP     = 5;
  MB_BEEPS   = 6;
  MB_AY      = 7;
  MB_ZX0     = 8;  //decompressor - not relocatable
  MB_PACKED  = 9;  //packed data only

MAXSLOTS: Integer = 1023;
BMM_SIGNATURE = 'BMM1'; // Signature: Basin Memory Manager Version 1

tfPremiereLocked:  TFColor  = (b:140; g:140; r:189);
tfPremiereLockedheader:  TFColor  = (b:130; g:130; r:147);

tfPremiereunLocked:  TFColor  = (b:140; g:189; r:140);
tfPremiereunLockedheader:  TFColor  = (b:130; g:150; r:130);

tfPremiereWarn:  TFColor  = (b:140; g:190; r:187);
tfPremiereWarnheader:  TFColor  = (b:130; g:150; r:150);

tfPremiereBgk:  TFColor  = (b:187; g:189; r:187);
tfPremiereRed:  TFColor  = (b:0; g:0; r:130);

tfPremiereSelected:       TFColor = (b:240; g:160; r:80);  // Bright Blue
tfPremiereSelectedHeader: TFColor = (b:200; g:120; r:60);  // Darker Blue Header

var
      MousePos:                          TPoint;

      Man_Updating:                      boolean;
      ManList: AnsiString;

      MemManagerForm: TMemManagerForm;
      ManArray: Array[0..1023] of MBlock;

implementation

uses Tapes, Binaries,AddCode;

{$R *.DFM}


procedure TMemManagerForm.CheckChanges;
var
  i, k: Integer;
  IsChanged: Boolean;
  MemByte, BlockByte: Byte;
begin
  // Tüm slotlari tara
  for i := 0 to MAXSLOTS do
  begin
    // Sadece geçerli, bellekte olan (INMEMORY) ve takipli (TRACKED) bloklari kontrol et
    if ManArray[i].Valid and 
       ((ManArray[i].Status and MS_INMEMORY) <> 0) and 
       ((ManArray[i].Status and MS_TRACKED) <> 0) then
    begin
       IsChanged := False;
       
       // Blok verisi var mi ve adresi geçerli mi?
       if (Length(ManArray[i].Slot.data) > 0) and 
          (ManArray[i].wAddress >= 0) and 
          ((ManArray[i].wAddress + ManArray[i].wLength) <= 65536) then
       begin
          // Byte byte karsilastirma
          for k := 0 to ManArray[i].wLength - 1 do
          begin
             // Ana Hafizadaki Veri
             MemByte := Memory[ManArray[i].wAddress + k];
             
             // Bloktaki Orijinal Veri
             BlockByte := ManArray[i].Slot.data[k];
             
             if MemByte <> BlockByte then
             begin
                IsChanged := True;
                Break; // Ilk farkta çik, hepsini taramaya gerek yok
             end;
          end;
       end;

       // Duruma göre Status bitini güncelle
       if IsChanged then
          ManArray[i].Status := ManArray[i].Status or MS_CHANGED // Set Changed Flag
       else
          ManArray[i].Status := ManArray[i].Status and (not MS_CHANGED); // Clear Changed Flag
    end;
  end;
  
  // Arayüzü güncelle (Liste ikonlari veya durum metinleri degismis olabilir)
  BuildManList;
end;


procedure TMemManagerForm.ChangeBlockLength(BlockIndex: Integer; NewLength: Integer);
var
  CurrentLen: Integer;
begin

  if (BlockIndex < 0) or (BlockIndex > MAXSLOTS) or (not ManArray[BlockIndex].Valid) then
    Exit;
  if NewLength < 0 then NewLength := 0;
  if NewLength > 65536 then NewLength := 65536;
  CurrentLen := Length(ManArray[BlockIndex].Slot.data);
  if CurrentLen = NewLength then Exit;
  SetLength(ManArray[BlockIndex].Slot.data, NewLength);

  if NewLength > CurrentLen then
  begin
     FillChar(ManArray[BlockIndex].Slot.data[CurrentLen], NewLength - CurrentLen, 0);
  end;

  ManArray[BlockIndex].wLength := NewLength;
  ManArray[BlockIndex].Modified := 0;

  BuildManList;
  DrawLayout;
end;


function TMemManagerForm.GetSelectedBlockData: TByteArray;
var
  Idx: Integer;
begin
  SetLength(Result, 0); // Default empty

  if ListView1.Selected <> nil then
  begin
    // Find the real index in ManArray
    Idx := Ord(ManList[ListView1.Selected.Index + 1]);
    
    if ManArray[Idx].Valid then
    begin
       // Return the dynamic array content
       Result := ManArray[Idx].Slot.data;
    end;
  end;
end;

function TMemManagerForm.GetSelectedBlockName: String;
var
  Idx: Integer;
begin
  Result := '';
  if ListView1.Selected <> nil then
  begin
    Idx := Ord(ManList[ListView1.Selected.Index + 1]);
    if ManArray[Idx].Valid then
       Result := ManArray[Idx].FileName;
  end;
end;

function TMemManagerForm.GetSelectedBlockAddress: Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if ListView1.Selected <> nil then
  begin
    Idx := Ord(ManList[ListView1.Selected.Index + 1]);
    if ManArray[Idx].Valid then
       Result := ManArray[Idx].wAddress;
  end;
end;

procedure TMemManagerForm.GetViewParams(out StartAddr: Integer; out BytesPerPixel: Double);
begin
  StartAddr := FViewStart;
  
  if (FastIMG1.Bmp.Width - 4) > 0 then
    BytesPerPixel := FViewLength / (FastIMG1.Bmp.Width - 4)
  else
    BytesPerPixel := 1;
end;

   {
procedure TMemManagerForm.GetViewParams(out StartAddr: Integer; out BytesPerPixel: Double);
var
  ViewLen: Integer;
begin
  if FZoomMode then
  begin
    StartAddr := 16384;
    ViewLen := 49152; // 65536 - 16384
  end
  else
  begin
    StartAddr := 0;
    ViewLen := 65536;
  end;

  if (FastIMG1.Bmp.Width - 4) > 0 then
    BytesPerPixel := ViewLen / (FastIMG1.Bmp.Width - 4)
  else
    BytesPerPixel := 1;
end;
 }
function TMemManagerForm.AddrToPixel(Addr: Integer): Integer;
var
  StartAddr: Integer;
  BPP: Double;
begin
  GetViewParams(StartAddr, BPP);
  // Formula: 2 + (Address - Offset) / Scale
  Result := 2 + Round((Addr - StartAddr) / BPP);
end;

function TMemManagerForm.PixelToAddr(X: Integer): Integer;
var
  StartAddr: Integer;
  BPP: Double;
begin
  GetViewParams(StartAddr, BPP);
  // Formula: Offset + (Pixel - 2) * Scale
  Result := StartAddr + Round((X - 2) * BPP);
  
  // Clamp results
  if Result < 0 then Result := 0;
  if Result > 65535 then Result := 65535;
end;

Procedure TMemManagerForm.AddMemBlock(BlockName: String; Address: Integer; Data: TByteArray);
Var
  RealIdx: Integer;
  k: Integer;
Begin
  // 1. Create the block using the internal function
  // Parameters: Name, Addr, Len, Type(0=Data), Index(0), Page(0), Break(False), Enabled(True), Data
  RealIdx := CreateMBlock(BlockName, Address, Length(Data), 0, 0, 0, False, True, Data);

  // 2. Update the Memory Manager Lists and Visuals
  BuildManList;
  DrawLayout;

  // 3. Find and Select the newly created block in ListView
  // We need to match the Real Index returned by CreateMBlock with the ListView items
  if RealIdx < MAXSLOTS then
  begin
     ListView1.Selected := nil;
     for k := 0 to ListView1.Items.Count - 1 do
     begin
        // Check if this List Item corresponds to our new Block Index
        if (k + 1 <= Length(ManList)) and (Ord(ManList[k + 1]) = RealIdx) then
        begin
           ListView1.Items[k].Selected := True;
           ListView1.Items[k].Focused := True;
           ListView1.Items[k].MakeVisible(False);
           Break;
        end;
     end;
  end;

  // 4. Ensure Memory Manager window is visible
  if not Self.Visible then Self.Show;
  Self.BringToFront;
End;


procedure TMemManagerForm.LiftBlockFromLayout;
var
  SelIdx, RealIdx: Integer;
  i: Integer;
  IsMemoryDirty: Boolean;
  StartAddr, EndAddr: Integer;
begin
  if ListView1.Selected = nil then Exit;

  SelIdx := ListView1.Selected.Index;
  RealIdx := Ord(ManList[SelIdx + 1]);

  if ManArray[RealIdx].Valid then  Begin
     ManArray[RealIdx].Status :=0;
     //ManArray[RealIdx].State:=0;
     StartAddr := ManArray[RealIdx].wAddress;
     EndAddr :=  ManArray[RealIdx].wLength ;

        // A. Clear the old memory area (Fill with 0)
        if (StartAddr >= 0) and ((StartAddr + EndAddr) <= 65536) then
           FillChar(Memory[StartAddr], EndAddr, 0);

     BuildManList;
     DrawLayout;
  End;
End;


procedure TMemManagerForm.InsertBlockAtCursor;
var
  SelIdx, RealIdx: Integer;
  i: Integer;
  IsMemoryDirty: Boolean;
  StartAddr, EndAddr: Integer;
begin
  if ListView1.Selected = nil then Exit;

  SelIdx := ListView1.Selected.Index;
  RealIdx := Ord(ManList[SelIdx + 1]);




  if ManArray[RealIdx].Valid then
  begin
    StartAddr := FCurrentAddr;
    EndAddr := FCurrentAddr + ManArray[RealIdx].wLength - 1;

    if (ManArray[RealIdx].BlockType=8) And (StartAddr<>ManArray[RealIdx].wAddress) then begin
       ShowMessage('Fixed blocks are not relocatable. Use "Insert in place" instead or change the type.');
       Exit;
     End;


    // Safety check for memory bounds
    if EndAddr > 65535 then
    begin
       ShowMessage('Block does not fit at this address (exceeds 65535).');
       Exit;
    end;

    // 1. CHECK MEMORY FOR COLLISIONS
    IsMemoryDirty := False;
    for i := StartAddr to EndAddr do
    begin
      if Memory[i] <> 0 then
      begin
        IsMemoryDirty := True;
        Break;
      end;
    end;

    // 2. ASK USER IF DIRTY
    if IsMemoryDirty then
    begin
      if MessageDlg('The target memory area is not empty. Overwrite?', 
                    mtWarning, [mbOK, mbCancel], 0) = mrCancel then
      begin
         Exit; // Cancel operation
      end;
    end;

    // 3. UPDATE BLOCK
    ManArray[RealIdx].wAddress := StartAddr;
    
    // Set Status: It is now logically "In Memory"
    ManArray[RealIdx].Status := ManArray[RealIdx].Status or MS_INMEMORY;

    // Mark as Modified logic (optional, dependent on your UX)
    ManArray[RealIdx].Modified := 1;

    // 4. ACTUALLY WRITE TO MEMORY (Optional but logical for "Insert")
    // If "Insert into cursor" implies writing the data to the emulated RAM:
    if Length(ManArray[RealIdx].Slot.data) > 0 then
       CopyMemory(@Memory[StartAddr], @ManArray[RealIdx].Slot.data[0], ManArray[RealIdx].wLength);

    BuildManList;
    DrawLayout;
        if StartAddr < 23296 then begin
          FastCore.UpdateDisplay;
          UpdateBASinDisplay;
        End;
    
    if (SelIdx >= 0) and (SelIdx < ListView1.Items.Count) then
    begin
       ListView1.Items[SelIdx].Selected := True;
       ListView1.Items[SelIdx].MakeVisible(False);
    end;
  end;
end;

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
        ManArray[F].Status:=ManArray[F].Status And (Not MS_INMEMORY);

        free:= true;
        Break;
    End;
  End;
  if not free then F:=MAXSLOTS; //all slots are full!
  Result := F;
End;

Function TmemmanagerForm.CreateMBlock( FileName:string; wAddress, wLength, BlockType, SetStatusBits, wPage: integer; CanBreak, Enabled: Boolean;   DataChunk: TByteArray ): Byte;
Var
  F: Integer;
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
        ManArray[F].wBlockIndex := 0;
        ManArray[F].State := 0;
        ManArray[F].Valid := True;
        ManArray[F].wPage := wPage;
        ManArray[F].Status := SetStatusBits;
        free:= true;

        SetLength(ManArray[F].Slot.data, wLength);
        if wLength > 0 then
           Move(DataChunk[0], ManArray[F].Slot.data[0], wLength);
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
 

  SavedRealIndex: Integer;
  ItemToSelect: TListItem;
Begin

  SavedRealIndex := -1;
  if ListView1.Selected <> nil then
  begin

     if (ListView1.Selected.Index + 1) <= Length(ManList) then
        SavedRealIndex := Ord(ManList[ListView1.Selected.Index + 1]);
  end;

  Man_Updating := True;
  ListView1.Items.BeginUpdate;

  ManList := '';
  ListView1.Items.Clear;
  
  ItemToSelect := nil;

  For F := 0 To MAXSLOTS Do
     If ManArray[F].Valid Then Begin
        LI := ListView1.Items.Add;

        if F = SavedRealIndex then
           ItemToSelect := LI;

        packS:='';
        LI.SubItems.add(ManArray[f].Filename);

        LI.Checked := ManArray[F].Enabled;
        
        Case ManArray[F].BlockType of
           0: LI.SubItems.Add('Raw'+PackS);
           1: LI.SubItems.Add('Screen'+PackS);
           2: LI.SubItems.Add('Code'+PackS);
           3: LI.SubItems.Add('Udg'+PackS);
           4: LI.SubItems.Add('Chars'+PackS);
           5: LI.SubItems.Add('Map'+PackS);
           6: LI.SubItems.Add('Beeps'+PackS);
           7: LI.SubItems.Add('AY'+PackS);
           8: Begin
                 LI.SubItems.Add('Fixed'+PackS);
                 LI.Checked:=False; 
              End;
           9: LI.SubItems.Add('Packed'+PackS);
        End;

        if  ManArray[f].wAddress >-1  then LI.SubItems.add(inttostr(ManArray[f].waddress)) Else LI.SubItems.add('n/a') ;
        LI.SubItems.add(inttostr(ManArray[f].wlength));


        packS := 'Lifted ';

        // Check Bits
        if (ManArray[F].Status and MS_INMEMORY) <> 0 then
           packS := 'Placed '; // In Memory

        if (ManArray[F].Status and MS_TRACKED) <> 0 then
           packS := packS + '[T] '; // Tracked

        if (ManArray[F].Status and MS_COMPRESSED) <> 0 then
           packS := packS + '[P] '; // Compressed (ZX0)

        if (ManArray[F].Status and MS_CHANGED) <> 0 then
           packS := packS + '[!] '; // Changed/Dirty in RAM

        if (ManArray[F].Status and MS_INUSE) <> 0 then
           packS := packS + '[E] '; // Export Included

        if (ManArray[F].Status and MS_WDECODER) <> 0 then
           packS := packS + '[X] '; // With Decoder

        if (ManArray[F].Status and MS_MODIFIED) <> 0 then
           packS := packS + '[*] '; // Modified in Editor

        packS := Trim(packS);
        
        LI.SubItems.Add(packS);
        LI.SubItems.add(inttostr(ManArray[f].wPage));

        ManList := ManList + AnsiChar(F);
     End;

  ListView1.Items.EndUpdate;
  Man_Updating := False;

  if ItemToSelect <> nil then
  begin
     ListView1.Selected := ItemToSelect;
     ItemToSelect.Focused := True;
     ItemToSelect.MakeVisible(False);
  end;

End;

Procedure TmemmanagerForm.DrawBlock(Address, Length: Integer; Text: String; Colr: byte; Locked: Boolean);
var
  bstart, blen, btop, bboy : integer;
  ft: TFont;
  Col, Scole: TFColor;
  // View Params
  StartAddr: Integer;
  BPP: Double;
  RealEnd: Integer;
Begin
    // Renk atamalari (Ayni kaliyor)
    if  Colr=0 then begin
      col:= tfPremiereLocked;
      Scole:= tfPremiereLockedHeader;
    end else if colr=1 then begin
      col:= tfPremiereUnLocked;
      Scole:= tfPremiereUnLockedHeader;
    end else if colr=2 then begin
       col:= tfPremiereWarn;
      Scole:= tfPremiereWarnHeader;
    end else if Colr = 3 then begin  // Selected
      col := tfPremiereSelected;
      Scole := tfPremiereSelectedHeader;
    end;

    ft := TFont.Create;
    ft.Name := 'Arial';
    ft.size:= 8;
    btop:=6;
    bboy:=fastimg1.Bmp.absheight-4;

    GetViewParams(StartAddr, BPP);
    if (Address + Length) < StartAddr then Exit;
    
    btop:=6;
    bboy:=fastimg1.Bmp.absheight-4;

    // Use Helper for Start Position
    bstart := AddrToPixel(Address);
    
    // Calculate Pixel Length based on Scale
    // Note: We calculate the End Pixel and subtract Start Pixel to handle scaling correctly
    blen := Round(Length / BPP);
    if blen < 1 then blen := 1;

    // CLIP LEFT: If block starts before view, trim it
    if bstart < 2 then 
    begin
       blen := blen - (2 - bstart);
       bstart := 2;
    end;

    // --- Drawing logic (Rectangle, FillRect, Text) stays the same ---
    // Ensure we don't draw outside right edge (FastDraw handles clipping usually, but good to know)
    
    rectangle(fastimg1.bmp, bstart, 4-1, bstart + blen, bboy-1, tfBlack); 
    rectangle(fastimg1.bmp, bstart, 4+1, bstart + blen-1, bboy-1, tfWhite); 

    fillrect(FastIMG1.Bmp, bstart+1, Btop, bstart + blen-1, bboy, col);
    fillrect(FastIMG1.Bmp, bstart+1, Btop, bstart + blen-1, bboy div 4 , scole);

    fasttextrectNOFMT(fastimg1.bmp, ft, Bstart+2, Btop, bstart + blen-1, bboy div 4, text);
    
    ft.Free; 
       {
    // --- DÜZELTME BURADA BASLIYOR ---
    // Önce orani Double olarak hesapliyoruz
    if (FastIMG1.Bmp.Width - 4) > 0 then
      Ratio := (FastIMG1.Bmp.Width - 4) / 65536.0
    else
      Ratio := 1;

    // Adresi pixel'e çeviriyoruz (Adres * Oran)
    bstart := 2 + Round(Address * Ratio);
    
    // Genisligi pixel'e çeviriyoruz ve -1 diyoruz cunku örnegin 0'dan baslayip 16384 byte koyacaksan 0-16383'de biter
    blen := Round((Length-1) * Ratio);
    if blen < 1 then blen := 1; // En az 1 pixel genislik olsun

    // --- DÜZELTME BITIYOR ---

    // Çizim islemleri (Koordinatlar artik daha hassas)
    // Gölge ve Highlight
    rectangle(fastimg1.bmp, bstart, 4-1, bstart + blen, bboy-1, tfBlack); // Gölge
    rectangle(fastimg1.bmp, bstart, 4+1, bstart + blen-1, bboy-1, tfWhite); // Highlight

    // Blogu çiz
    fillrect(FastIMG1.Bmp, bstart+1, Btop, bstart + blen-1, bboy, col);
    fillrect(FastIMG1.Bmp, bstart+1, Btop, bstart + blen-1, bboy div 4 , scole);

    // Yaziyi yaz
    fasttextrectNOFMT(fastimg1.bmp, ft, Bstart+2, Btop, bstart + blen-1, bboy div 4, text);
    
    ft.Free; // Fontu serbest birakmayi unutma    }
End;

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

  DrawLayout;
  
  FastIMG1.Repaint;
end;

procedure TMemManagerForm.Button1Click(Sender: TObject);
Var
  Bytes, BytesUsed: Integer;
begin
  MemManagerForm.Close;  
end;

Procedure TMemManagerForm.DrawLayout;
Var
  X, i: Integer;
  LineX: Integer;
  BytesUsed: Integer;
  SelectedRealIndex: Integer; 
  BlockColor: Byte;
  
  // View Params
  ViewStart: Integer;
  DummyBPP: Double;
 Begin
    GetViewParams(ViewStart, DummyBPP); // Only need ViewStart for clipping check

    // 0. DETERMINE SELECTED BLOCK INDEX
    SelectedRealIndex := -1;
    if ListView1.Selected <> nil then
       SelectedRealIndex := Ord(ManList[ListView1.Selected.Index + 1]);

    // 2. CLEAR
    FillRect(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width , FastIMG1.Bmp.AbsHeight , tfPremiereBgk);

    // 3. 16KB GUIDES
    X := 0;
    while X < 4 do 
    Begin
      // Use AddrToPixel helper
      LineX := AddrToPixel(16384 * X);
      
      if (LineX >= 0) and (LineX < FastIMG1.Width) then
        Line(FastIMG1.Bmp, LineX, 0, LineX, FastIMG1.Height, TfBlack);
      Inc(X);
    End;

    // UDG Line
    LineX := AddrToPixel(GetWord(@Memory[UDG]));
    if (LineX >= 0) and (LineX < FastIMG1.Width) then
      Line(FastIMG1.Bmp, LineX, 0, LineX, FastIMG1.Height div 3, tfPremiereSelected);

    // Attribute/Screen End Line (22528)
    LineX := AddrToPixel(22528);
    if (LineX >= 0) and (LineX < FastIMG1.Width) then
      Line(FastIMG1.Bmp, LineX, 0, LineX, FastIMG1.Height div 3, tfPremiereSelected);

    // RAMTOP Line
    LineX := AddrToPixel(GetWord(@Memory[RAMTOP]));
    if (LineX >= 0) and (LineX < FastIMG1.Width) then
      Line(FastIMG1.Bmp, LineX, 0, LineX, FastIMG1.Height div 3, tfPremiereSelected);

    // --- STACK WARNING BLOCK ---
    LineX := AddrToPixel(GetWord(@Memory[RAMTOP]) - 100); // Start
    i     := AddrToPixel(GetWord(@Memory[RAMTOP]));       // End

    if i <= LineX then i := LineX + 1;
    if (LineX < FastIMG1.Width) and (i > 0) then
       FillRect(FastIMG1.Bmp, LineX, 0, i, FastIMG1.Height div 3, tfRed);

    // 4. DRAW SYSTEM BLOCKS 
    // Only draw ROM if we are in Full Mode (ViewStart == 0)
    if ViewStart = 0 then DrawBlock(0,16384,'ROM',0,True);
    
    BytesUsed := GetWord(@Memory[E_LINE])-GetWord(@Memory[PROG])-1;
    DrawBlock(GetWord(@Memory[PROG]),BytesUsed,'Basic',0,True);

    // DRAW USER BLOCKS
    for i := 0 to MAXSLOTS do
    begin
      if ManArray[i].Valid then
      begin
        if (ManArray[i].Status and MS_INMEMORY) <> 0 then
        begin
             if ManArray[i].Enabled then BlockColor := 1 else BlockColor := 0; 
             if i = SelectedRealIndex then BlockColor := 3;

             if FDragActive and (i = FDragBlockIdx) then
                DrawBlock(FDragCurrentAddr, ManArray[i].wLength, ManArray[i].FileName, BlockColor, False)
             else
                DrawBlock(ManArray[i].wAddress, ManArray[i].wLength, ManArray[i].FileName, BlockColor, False);
        end;
      end;
    end;
    
    // RED CURSOR
    LineX := AddrToPixel(FCurrentAddr);
    If (LineX >= 2) and (LineX < FastIMG1.Bmp.Width - 2) Then
       Line(FastIMG1.Bmp, LineX, 2, LineX, FastIMG1.Bmp.Height-2, tfPremiereRed);
    
    txtaddress.Text := IntToStr(FCurrentAddr);
    FastDrawEx.Rectangle(FastIMG1.Bmp, 0, 0, FastIMG1.Bmp.Width-1 , FastIMG1.Bmp.absHeight-1 , tfBlack);
    FastIMG1.Repaint;
end;

procedure TMemManagerForm.FastIMG1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Ratio: Double;
  RawAddr, TargetAddr: Integer;
  
  // Dynamic Limits
  GapStart, GapEnd: Integer;
  
  // Iterator variables
  k, ObsStart, ObsEnd: Integer;
  
  // System Pointers
  ProgStart, VarStart, UDGStart: Integer;
begin
  MousePos := Point(X, Y);

  if MouseDown then
  begin
      if (FastIMG1.Bmp.Width - 4) > 0 then
         Ratio := 49152.0 / (FastIMG1.Bmp.Width - 4)
      else
         Ratio := 1;

      if FDragActive then
      begin
          // --- BLOCK DRAGGING LOGIC (Same as before) ---
          RawAddr := PixelToAddr(X);
          //RawAddr := Round((X - 2) * Ratio);
          if RawAddr < 0 then RawAddr := 0;
          if RawAddr > 65535 then RawAddr := 65535;

          GapStart := 0;
          GapEnd := 65536;

          ProgStart := GetWord(@Memory[PROG]);
          UDGStart := GetWord(@Memory[UDG]);
          
          // System Obstacles
          // ROM
          ObsStart := 0; ObsEnd := 16384;
          if ObsEnd <= RawAddr then begin if ObsEnd > GapStart then GapStart := ObsEnd; end;
          if ObsStart >= RawAddr then begin if ObsStart < GapEnd then GapEnd := ObsStart; end;

          // Screen
          ObsStart := 16384; ObsEnd := 23296; 
          if ObsEnd <= RawAddr then begin if ObsEnd > GapStart then GapStart := ObsEnd; end;
          if ObsStart >= RawAddr then begin if ObsStart < GapEnd then GapEnd := ObsStart; end;
          
          // Basic
          ObsStart := ProgStart; ObsEnd := VarStart; 
          if (ObsEnd > ObsStart) then
          begin
             if ObsEnd <= RawAddr then begin if ObsEnd > GapStart then GapStart := ObsEnd; end;
             if ObsStart >= RawAddr then begin if ObsStart < GapEnd then GapEnd := ObsStart; end;
          end;

          // UDG
          ObsStart := UDGStart; ObsEnd := UDGStart + 768;
          if ObsEnd <= RawAddr then begin if ObsEnd > GapStart then GapStart := ObsEnd; end;
          if ObsStart >= RawAddr then begin if ObsStart < GapEnd then GapEnd := ObsStart; end;
          
          // User Blocks
          for k := 0 to MAXSLOTS do
          begin
             if (k <> FDragBlockIdx) and ManArray[k].Valid and ((ManArray[k].Status and MS_INMEMORY) <> 0) then
             begin
                ObsStart := ManArray[k].wAddress;
                ObsEnd := ObsStart + ManArray[k].wLength;
                
                if ObsEnd <= RawAddr then 
                   if ObsEnd > GapStart then GapStart := ObsEnd;

                if ObsStart >= RawAddr then 
                   if ObsStart < GapEnd then GapEnd := ObsStart;
             end;
          end;

          if GapStart >= GapEnd then
          begin
               if Abs(RawAddr - GapStart) < Abs(RawAddr - GapEnd) then
                  GapEnd := 65536 
               else
                  GapStart := 0; 
          end;

          TargetAddr := RawAddr - FDragOffset;

          if (TargetAddr + ManArray[FDragBlockIdx].wLength) > GapEnd then
             TargetAddr := GapEnd - ManArray[FDragBlockIdx].wLength;
          
          if TargetAddr < GapStart then TargetAddr := GapStart;
          
          if (GapEnd - GapStart) < ManArray[FDragBlockIdx].wLength then
             TargetAddr := GapStart; 

          FDragCurrentAddr := TargetAddr;
          txtaddress.Text := IntToStr(FDragCurrentAddr);
      end
      else
      begin
          // --- CURSOR SELECTION LOGIC ---
          if (ssShift in Shift) then
          begin
             // Precision Mode: 1 pixel = 1 byte movement (relative)
             FCurrentAddr := FCurrentAddr + (X - FLastMouseX);
          end
          else
          begin
             // Normal Mode: Snap to nearest multiple of 8
             //RawAddr := Round((X - 2) * Ratio);
             RawAddr := PixelToAddr(X);
             FCurrentAddr := ((RawAddr + 4) div 8) * 8;
          end;

          if FCurrentAddr < 0 then FCurrentAddr := 0;
          if FCurrentAddr > 65535 then FCurrentAddr := 65535;
          
          txtaddress.Text := IntToStr(FCurrentAddr);
      end;

      FLastMouseX := X;
      DrawLayout;
      if not FDragActive then HexCursor;
  end;
end;


procedure TMemManagerForm.HexCursor;
var
 newpos: integer;
begin
if HexShown<>9999 Then Begin
      MPHexEditor1.SelectAll;
  MPHexEditor1.DeleteSelection;
  MPHexEditor1.AppendBuffer(@Memory[0], 65536);
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
  FViewStart := 0;
  FViewLength := 65536;
  HexShown:=9999;
  MPHexEditor1.SelectAll;
  MPHexEditor1.DeleteSelection;
  MPHexEditor1.AppendBuffer(@Memory[0], 65536);
  MPHexEditor1.ResetSelection(True);
end;


procedure TMemManagerForm.FastIMG1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, RealIdx, RawAddr: Integer;
  Ratio: Double;
begin
if Button = mbRight then Exit;
  MouseDown := True;
  FLastMouseX := X;
  FDragActive := False;

  if (FastIMG1.Bmp.Width - 4) > 0 then
    Ratio := 65536.0 / (FastIMG1.Bmp.Width - 4)
  else
    Ratio := 1;

  if not (ssShift in Shift) then
  begin
      // Calculate raw address based on pixels
      //RawAddr := Round((X - 2) * Ratio);
      RawAddr := PixelToAddr(X);

      // Snap to nearest multiple of 8


      ListView1.Selected := nil;

      for i := 0 to ListView1.Items.Count - 1 do
      begin
         if (i + 1) <= Length(ManList) then
         begin
             RealIdx := Ord(ManList[i + 1]);

             if ManArray[RealIdx].Valid and ((ManArray[RealIdx].Status and MS_INMEMORY) <> 0) then
             begin
                // Check if cursor is inside the block
                if (RawAddr >= ManArray[RealIdx].wAddress) and
                   (RawAddr < (ManArray[RealIdx].wAddress + ManArray[RealIdx].wLength)) then
                begin
                   // Always select the item, even if locked
                   ListView1.Items[i].Selected := True;
                   ListView1.Items[i].Focused := True;
                   ListView1.Items[i].MakeVisible(False);

                   // ONLY start dragging if Enabled is true
                   if ManArray[RealIdx].Enabled then
                   begin
                       FDragActive := True;
                       FDragBlockIdx := RealIdx;
                       FDragOffset := RawAddr - ManArray[RealIdx].wAddress;
                       FDragCurrentAddr := ManArray[RealIdx].wAddress;
                   end;

                   Break;
                end;
             end;
         end;
      end;
      if ListView1.Selected = nil Then
      Begin
      FCurrentAddr := ((RawAddr + 4) div 8) * 8;
      if FCurrentAddr < 0 then FCurrentAddr := 0;
      if FCurrentAddr > 65535 then FCurrentAddr := 65535;
      End;
  end;

  DrawLayout;
end;




procedure TMemManagerForm.FastIMG1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  IdxInList: Integer;
  OldAddr, Len: Integer;

  // Collision Detection Variables
  TargetStart, TargetEnd: Integer;
  ObsStart, ObsEnd: Integer;
  k: Integer;
  IsCollision: Boolean;
  
  // System Pointers
  SysProg, SysELine, SysUDG: Integer;
begin
  if FDragActive then
  begin
     Len := ManArray[FDragBlockIdx].wLength;
     TargetStart := FDragCurrentAddr;
     TargetEnd := TargetStart + Len;
     IsCollision := False;

     // --- 1. COLLISION CHECKING START ---
     
     // A. Check against User Blocks
     for k := 0 to MAXSLOTS do
     begin
        // Skip self, invalid blocks, or blocks not in memory
        if (k <> FDragBlockIdx) and ManArray[k].Valid and ((ManArray[k].Status and MS_INMEMORY) <> 0) then
        begin
           ObsStart := ManArray[k].wAddress;
           ObsEnd := ObsStart + ManArray[k].wLength;

           // Check Intersection: (StartA < EndB) and (StartB < EndA)
           if (TargetStart < ObsEnd) and (ObsStart < TargetEnd) then
           begin
              IsCollision := True;
              Break;
           end;
        end;
     end;

     // B. Check against System Blocks (If no user collision found yet)
     if not IsCollision then
     begin
        // Load System Pointers from Memory
        SysProg := GetWord(@Memory[PROG]);   // Start of BASIC
        SysELine := GetWord(@Memory[E_LINE]); // End of Variables/Current Line
        SysUDG := GetWord(@Memory[UDG]);      // Start of UDG

        // 1. Check ROM (0 - 16384)
        ObsStart := 0; ObsEnd := 16384;
        if (TargetStart < ObsEnd) and (ObsStart < TargetEnd) then IsCollision := True;

        // 2. Check BASIC + VARS (PROG to E_LINE)
        if not IsCollision then
        begin
           ObsStart := SysProg; ObsEnd := SysELine;
           if (ObsEnd > ObsStart) then // Validity check
              if (TargetStart < ObsEnd) and (ObsStart < TargetEnd) then IsCollision := True;
        end;

        // 3. Check UDG (Fixed length 768)
        if not IsCollision then
        begin
           ObsStart := SysUDG; ObsEnd := SysUDG + 768;
           if (TargetStart < ObsEnd) and (ObsStart < TargetEnd) then IsCollision := True;
        end;
     end;
     // --- COLLISION CHECKING END ---

     
     // --- 2. DECISION: COMMIT OR REVERT ---
     if IsCollision then
     begin
        // COLLISION DETECTED:
        // Do nothing to Memory.
        // Do not change ManArray address.
        // Just repaint. The block snaps back visually because we didn't update the record.
        // Optional: Beep or small visual cue
        Beep; 
     end
     else
     begin
        // NO COLLISION: COMMIT MOVE
        
        OldAddr := ManArray[FDragBlockIdx].wAddress;

        // A. Clear the old memory area (Fill with 0)
        if (OldAddr >= 0) and ((OldAddr + Len) <= 65536) then
           FillChar(Memory[OldAddr], Len, 0);

        // B. Update the block to the new address
        ManArray[FDragBlockIdx].wAddress := FDragCurrentAddr;
        ManArray[FDragBlockIdx].Modified := 2;

        // C. Write the block data to the NEW memory location
        if (Len > 0) and (Length(ManArray[FDragBlockIdx].Slot.data) > 0) then
        begin
           if (FDragCurrentAddr + Len) <= 65536 then
              Move(ManArray[FDragBlockIdx].Slot.data[0], Memory[FDragCurrentAddr], Len);
        end;
        HexShown:=-1;
        HexCursor;
        if FDragCurrentAddr < 23296 then begin
          FastCore.UpdateDisplay;
          UpdateBASinDisplay;
        End;
        // D. Update ListView UI
        if ListView1.Selected <> nil then
           ListView1.Selected.SubItems[2] := IntToStr(FDragCurrentAddr)
        else
           BuildManList; 
     end;

     // Reset Drag State
     FDragActive := False;
     FDragBlockIdx := -1;
     
     // Redraw to show final state (Reverted or Moved)
     DrawLayout;
  end;

  MouseDown := false;
end;


procedure TMemManagerForm.ContextMenuClick(Sender: TObject);
var
  idx, f,i, k, PastePos, lentowrite: Integer;
  SelS, sAddr, S: String;
  packd: TZX0ByteArray;
  output: TByteArray;
  SourceIdx: Integer;

 MyData: TByteArray;
 RawData, PackedData, Depacker: TZX0ByteArray;
 FinalData, Bytes: TByteArray;
 UnpackTargetAddr, CodeLoadAddr: Word;
 Ms: TMemoryStream;
begin
 Case (Sender As TComponent).Tag Of

 10: // Cut
      Begin
        if ListView1.Selected <> nil then
        begin
           SourceIdx := Ord(ManList[ListView1.Selected.Index + 1]);

           // 1. Copy logic (Deep Copy)
           FClipboardBlock := ManArray[SourceIdx];
           // Important: Duplicate the dynamic array content
           if Length(ManArray[SourceIdx].Slot.data) > 0 then
           begin
              SetLength(FClipboardBlock.Slot.data, Length(ManArray[SourceIdx].Slot.data));
              Move(ManArray[SourceIdx].Slot.data[0], FClipboardBlock.Slot.data[0], Length(ManArray[SourceIdx].Slot.data));
           end
           else
              SetLength(FClipboardBlock.Slot.data, 0);
              
           FClipboardValid := True;

           // 2. Delete logic
           ManArray[SourceIdx].Valid := False;
           SetLength(ManArray[SourceIdx].Slot.data, 0); // Free original memory
           
           BuildManList;
        end;
      End;

    11: // Copy
      Begin
        if ListView1.Selected <> nil then
        begin
           SourceIdx := Ord(ManList[ListView1.Selected.Index + 1]);

           // Perform Deep Copy to internal clipboard
           FClipboardBlock := ManArray[SourceIdx];
           
           if Length(ManArray[SourceIdx].Slot.data) > 0 then
           begin
              SetLength(FClipboardBlock.Slot.data, Length(ManArray[SourceIdx].Slot.data));
              Move(ManArray[SourceIdx].Slot.data[0], FClipboardBlock.Slot.data[0], Length(ManArray[SourceIdx].Slot.data));
           end
           else
              SetLength(FClipboardBlock.Slot.data, 0);

           FClipboardValid := True;
        end;
      End;

    12: // Paste
      Begin
        if FClipboardValid then
        begin
           // Find a free slot
           f := -1;
           for k := 0 to MAXSLOTS do
           begin
              if not ManArray[k].Valid then
              begin
                 f := k;
                 Break;
              end;
           end;

           if f <> -1 then
           begin
              // Copy from clipboard to new slot
              ManArray[f] := FClipboardBlock;
              
              // Ensure uniqueness
              ManArray[f].FileName := 'Copy_' + ManArray[f].FileName; 
              ManArray[f].Valid := True;

              // Deep copy the data array
              if Length(FClipboardBlock.Slot.data) > 0 then
              begin
                 SetLength(ManArray[f].Slot.data, Length(FClipboardBlock.Slot.data));
                 Move(FClipboardBlock.Slot.data[0], ManArray[f].Slot.data[0], Length(FClipboardBlock.Slot.data));
              end;

              // --- CHECK SOURCE COMPONENT ---
              // If the menu was triggered from FastIMG1 (The Visual Ruler),
              // update the address to the current Red Cursor position.
              if PopupMenu1.PopupComponent = FastIMG1 then
              begin
                 ManArray[f].wAddress := FCurrentAddr;
                 // Ensure the block is marked as "In Memory" since we explicitly placed it
                 ManArray[f].Status := ManArray[f].Status or MS_INMEMORY;
                 ManArray[f].Modified := 1;   // paste as in memory
              end else begin
                 ManArray[f].Modified := 0;   // paste as not in memory
                 ManArray[f].Status:= 0;
                 //ManArray[F].State := 0;

              end;
              // ------------------------------

              BuildManList;
              DrawLayout; // Refresh the visual representation
           end
           else
              ShowMessage('No free slots available.');
        end;
      End;

    13: // Rename
      Begin
        if ListView1.Selected <> nil then
        begin
           // 1. Gerçek Indexi bul
           f := Ord(ManList[ListView1.Selected.Index + 1]);

           // 2. Mevcut ismi al
           SelS := ManArray[f].FileName;

           // 3. Kullaniciya sor
           if InputQuery('Rename Block', 'Enter new filename:', SelS) then
           begin
              // 4. Veri tabanini güncelle
              ManArray[f].FileName := SelS;
              ManArray[f].Modified := 2; // Degisti olarak isaretle

              // 5. Listeyi güncelle (Column 1 = SubItems[0])
              ListView1.Selected.SubItems[0] := SelS;
              
              // 6. Çizimi güncelle (Blok üzerinde isim yaziyorsa degissin diye)
              DrawLayout; 
           end;
        end;
      End;
     14: //delete
      Begin
        If ListView1.Selected <> nil Then Begin
        ManArray[Ord(ManList[ListView1.Selected.Index+1])].Valid := False;
        BuildManList;
        End;
      End;
      15: //duplicate
      Begin
        If ListView1.Selected <> nil Then Begin
             DuplicateMBlock(Ord(ManList[ListView1.Selected.Index+1]));
             BuildManList;
        End;
      End;
      16: // Change Length (Properties / Resize)
      Begin
         if ListView1.Selected <> nil then
         begin
            idx := Ord(ManList[ListView1.Selected.Index + 1]);
            if (ManArray[idx].Status And MS_INMEMORY)<>0 Then Begin
               MessageDlg('You cannot resize blocks already in memory.'#13#10'Lift the block first.', mtWarning, [mbOk], 0);
               Exit;
            End;
            if ManArray[idx].Valid then
            begin
               SelS := IntToStr(ManArray[idx].wLength);
               if InputQuery('Change Block Length', 'Enter new length (bytes):', SelS) then
               begin
                  f := StrToIntDef(SelS, -1); // f degiskenini NewLen olarak kullaniyoruz

                  if (f >= 0) and (f <= 65536) then
                  begin
                     if f < ManArray[idx].wLength then
                     begin
                        if MessageDlg('Reducing the block size will truncate (cut) the data at the end.'#13#10'Are you sure?',
                                      mtWarning, [mbYes, mbNo], 0) = mrNo then
                        begin
                           Exit;
                        end;
                     end;

                     ChangeBlockLength(idx, f);
                     HexShown:=-1;
                     HexCursor;
                  end
                  else
                  begin
                     ShowMessage('Invalid length! Please enter a value between 0 and 65536.');
                  end;
               end;
            end;
         end;

      End;

      17:    //toggle track
      Begin
         if ListView1.Selected <> nil then
         begin
            idx := Ord(ManList[ListView1.Selected.Index + 1]);
            if (ManArray[idx].Status And MS_INMEMORY)=0 Then Begin
               MessageDlg('You cannot track blocks unless they are in memory.'#13#10'Insert the block first.', mtWarning, [mbOk], 0);
               Exit;
            End;
            //toggle the bit
            if (ManArray[idx].Status and MS_TRACKED)<>0 Then ManArray[idx].Status:=ManArray[idx].Status and (not MS_TRACKED) Else ManArray[idx].Status:=ManArray[idx].Status or MS_TRACKED;
            BuildManList;
         End;
      End;


      18: // Pack with embedded depacker (SFX)
      Begin
         if ListView1.Selected <> nil then
         begin
            idx := Ord(ManList[ListView1.Selected.Index + 1]);
            if ManArray[idx].Valid then
            begin

                begin
                   // --- 0. ASK FOR LOCATION ---
                   // Default to current location
                   sAddr := txtaddress.text;  //IntToStr(ManArray[idx].wAddress);
                   if not InputQuery('Create Self-Extracting Block', 'Enter the START address where this SFX block will be loaded:', sAddr) then
                      Exit; // Cancel if user presses Cancel
                      
                   CodeLoadAddr := StrToIntDef(sAddr, ManArray[idx].wAddress);

                   // --- A. GET DATA & COMPRESS ---
                   SetLength(RawData, Length(ManArray[idx].Slot.data));
                   if Length(RawData) > 0 then 
                      Move(ManArray[idx].Slot.data[0], RawData[0], Length(RawData));
                   
                   PackedData := PackZX0(RawData);

                   // --- B. PREPARE ADDRESSES ---
                   // UnpackTargetAddr: Where the data goes AFTER decompression (Original Address)
                   UnpackTargetAddr := ManArray[idx].wAddress;

                   // --- C. PREPARE & RELOCATE DEPACKER ---
                   // Relocate the depacker code to run from CodeLoadAddr
                   Depacker := RelocateDepacker(CodeLoadAddr);

                   // --- D. PATCH HEADER ---
                   // DE Register -> Unpack Target Address (Destination)
                   Depacker[1] := UnpackTargetAddr and $FF;
                   Depacker[2] := (UnpackTargetAddr shr 8) and $FF;

                   // HL Register -> Start of Compressed Data (Source)
                   // Formula: CodeLoadAddr + HeaderSize
                   Depacker[4] := (CodeLoadAddr + Length(Depacker)) and $FF;
                   Depacker[5] := ((CodeLoadAddr + Length(Depacker)) shr 8) and $FF;

                   // --- E. COMBINE (Header + Packed Data) ---
                   SetLength(FinalData, Length(Depacker) + Length(PackedData));

                   // Copy Header
                   Move(Depacker[0], FinalData[0], Length(Depacker));

                   // Copy Packed Data
                   if Length(PackedData) > 0 then
                      Move(PackedData[0], FinalData[Length(Depacker)], Length(PackedData));

                   // --- F. CREATE BLOCK ---
                   // Create at the new CodeLoadAddr location
                   // Type MB_CODE (2) is appropriate since it is executable code now.
                   // MB_ZX0 is for raw compressed data. You can decide which one fits better.
                   CreateMBlock(ManArray[idx].FileName + '.sfx', 
                                CodeLoadAddr, 
                                Length(FinalData), 
                                MB_ZX0, 0, 0, False, True, FinalData);

                   BuildManList;
                   DrawLayout;
                end;
            end;
         end;
      End;

      19: // Pack ZX0 (Only Raw Compression)
      Begin
         if ListView1.Selected <> nil then
         begin
            // 1. Get Block Index
            idx := Ord(ManList[ListView1.Selected.Index + 1]);

            if ManArray[idx].Valid then
            begin


                begin
                   // Convert current block data to ZX0 input format
                   // Assuming ManArray data is TByteArray (or similar)
                   SetLength(RawData, Length(ManArray[idx].Slot.data));
                   if Length(RawData) > 0 then
                      Move(ManArray[idx].Slot.data[0], RawData[0], Length(RawData));

                   // 3. Compress!
                   PackedData := PackZX0(RawData);

                   // 4. Convert back to internal format
                   SetLength(MyData, Length(PackedData));
                   if Length(PackedData) > 0 then
                      Move(PackedData[0], MyData[0], Length(PackedData));

                   // 5. Create New Block with Compressed Data
                   CreateMBlock(ManArray[idx].FileName + '.zx0',
                                ManArray[idx].wAddress, // Same address? Or 0?
                                Length(MyData),
                                MB_PACKED, MS_COMPRESSED, 0, False, True, MyData);



                   BuildManList;
                   DrawLayout;
                end;
            end;
         end;
      End;


      20: //view
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

       25: //toggle lock
       Begin
         If ListView1.Selected <> nil Then
         Begin
            idx := Ord(ManList[ListView1.Selected.Index + 1]);
            if ManArray[idx].Valid then
            begin
               ListView1.Selected.Checked:= not ListView1.Selected.Checked;
            End;
         End;

       End;

     50..59: //set Type
     Begin
        If ListView1.Selected <> nil Then Begin

           Idx:=(Sender As TComponent).Tag-50;
           ManArray[Ord(ManList[ListView1.Selected.Index+1])].BlockType:=Idx;
           BuildManList;
        End;
     End;
     80: // HexEditor Create Block
      Begin
          Sels := MPHexEditor1.SelectionAsText;
          if SelS <> '' Then
          Begin
             // Calculate the lowest address regardless of selection direction (Forward or Backward)
             if MPHexEditor1.SelStart < MPHexEditor1.SelEnd then
                SourceIdx := MPHexEditor1.SelStart
             else
                SourceIdx := MPHexEditor1.SelEnd;

             idx := CreateMblock('Selection.bin', SourceIdx, MPHexEditor1.SelCount, 0, 0, 0, false, true, StringToTBytes(SelS));
             BuildManList;
          End;
      End;
      83: // Revert to Memory (Discard Hex Edits)
      Begin
         // 1. Check if a block is currently shown in Hex Editor
         if (HexShown > 0) and (HexShown <= Length(ManList)) then
         begin
             // Find Real Index
             f := Ord(ManList[HexShown]);
             
             if ManArray[f].Valid then
             begin
                 // Reload data FROM Block TO Editor
                 MPHexEditor1.SelectAll;
                 MPHexEditor1.DeleteSelection;
                 
                 if Length(ManArray[f].Slot.data) > 0 then
                    MPHexEditor1.AppendBuffer(@ManArray[f].Slot.data[0], Length(ManArray[f].Slot.data));
                 
                 MPHexEditor1.ResetSelection(True);
                 
                 // Clear Modified Flag since we reverted
                 ManArray[f].Status := ManArray[f].Status and (not MS_MODIFIED);
                 
                 ShowMessage('Reverted to original memory state.');
             end;
         end
         else
            ShowMessage('No block is currently loaded in the Hex Editor.');
      End;

    84: // Apply Edits to Memory (Save Hex Edits)
      Begin
         // 1. Check if a block is currently shown
         if (HexShown > 0) and (HexShown <= Length(ManList)) then
         begin
             f := Ord(ManList[HexShown]);
             
             if ManArray[f].Valid then
             begin

                 begin
                    Ms := TMemoryStream.Create;
                    try
                       MPHexEditor1.SaveToStream(Ms);
                       Ms.Position := 0;
                       
                       // Resize Block Data
                       SetLength(ManArray[f].Slot.data, Ms.Size);
                       
                       // Copy Data
                       if Ms.Size > 0 then
                          Ms.ReadBuffer(ManArray[f].Slot.data[0], Ms.Size);
                          
                       // Update Length Info
                       ManArray[f].wLength := Ms.Size;
                       
                       // Mark as Modified
                       ManArray[f].Status := ManArray[f].Status or MS_MODIFIED;
                       ManArray[f].Modified := 2; // General modified flag
                       
                       ShowMessage('Edits applied to memory block.');
                       
                       BuildManList; // Refresh list to show status/size changes
                       DrawLayout;
                    finally
                       Ms.Free;
                    end;
                 end;
             end;
         end
         else
            ShowMessage('No block is currently loaded in the Hex Editor.');
      End;


      85: //Copy
      Begin
          Sels:=MPHexEditor1.SelectionAsText;
          if SelS<>'' Then ClipBoard.SetTextBuf(PChar(Sels));
      End;

      86: // Paste Text into Hex Editor
      Begin
         if Clipboard.HasFormat(CF_TEXT) then
         begin
                S := Clipboard.AsText;
                if S <> '' then
                begin
                   SetLength(Bytes, Length(S));
                   for i := 1 to Length(S) do
                      Bytes[i-1] := Ord(S[i]);
                   PastePos := MPHexEditor1.SelStart;
                   if MPHexEditor1.SelCount > 0 then
                   begin
                      MPHexEditor1.DeleteSelection;
                      PastePos := MPHexEditor1.SelStart; 
                   end;
                   MPHexEditor1.InsertBuffer(PChar(@Bytes[0]), Length(Bytes), PastePos, 'Paste Text');
                end;

         end;
      End;


      90: // Insert into cursor
      Begin
        SaveEmulationState(UndoState);
        InsertBlockAtCursor;
        HexShown:=-1;
        HexCursor;

      End;
      91: //lift
      Begin
        LiftBlockFromLayout;
        HexShown:=-1;
        HexCursor;
      
      End;
      92: // Insert into cursor
      Begin
        if ListView1.Selected = nil then Exit;
        FCurrentAddr := ManArray[Ord(ManList[ListView1.Selected.Index + 1])].wAddress;
        HexShown:=-1;
        HexCursor;
        InsertBlockAtCursor;

      End;
      93: //Rewrite All Changed Blocks
      Begin
        for i := 0 to MAXSLOTS do
        begin
          if ManArray[i].Valid and
             ((ManArray[i].Status and MS_TRACKED) <> 0) and
             ((ManArray[i].Status and MS_CHANGED) <> 0) and
             ((ManArray[i].Status and MS_INMEMORY) <> 0) then
          begin
             LenToWrite := ManArray[i].wLength;

             if LenToWrite > Length(ManArray[i].Slot.data) then
                LenToWrite := Length(ManArray[i].Slot.data);

             if (LenToWrite > 0) and
                (ManArray[i].wAddress >= 0) and
                ((ManArray[i].wAddress + LenToWrite) <= 65536) then
             begin
                Move(ManArray[i].Slot.data[0], Memory[ManArray[i].wAddress], LenToWrite);
                ManArray[i].Status := ManArray[i].Status and (not MS_CHANGED);
             end;
          end;
        end;

        BuildManList;
        DrawLayout;
      End;

      30..37: // Send to preset locations
      Begin
        if ListView1.Selected = nil then Exit;

        // Get current block index and info
        f := Ord(ManList[ListView1.Selected.Index + 1]);

        // Define variable for target address (-1 means not valid yet)
        idx := -1; 

        Case (Sender As TComponent).Tag Of
          30: idx := GetWord(@Memory[CHARS]);    // CHARS
          31: idx := 16384;                      // SCREEN
          32: idx := GetWord(@Memory[UDG]);      // UDG
          34: idx := GetWord(@Memory[RAMTOP]);   // RAMTOP
          37: idx := 32768;                      // Fixed 32768
          35: idx := 49152;                      // Fixed 49152
          
          33: // 0 REM (Scan for 'X' placeholders)
            Begin
               // Start scanning from BASIC PROG area up to E_LINE (End of BASIC)
               // We are looking for a sequence of 'X' (0x58) matching the block length.
               SourceIdx := 0; // Used as counter for consecutive 'X's
               for k := GetWord(@Memory[PROG]) to GetWord(@Memory[E_LINE]) do
               begin
                  if Memory[k] = $58 then // 'X' character
                  begin
                     Inc(SourceIdx);
                     if SourceIdx >= ManArray[f].wLength then
                     begin
                        // Found enough space. Start address is current k - length + 1
                        idx := k - ManArray[f].wLength + 1;
                        Break;
                     end;
                  end
                  else
                  begin
                     SourceIdx := 0; // Reset counter if sequence is broken
                  end;
               end;

               if idx = -1 then
                  ShowMessage('Could not find a suitable REM line with "X" placeholders for this block size.');
            End;

          36: // Next Free Address
            Begin
               // Find the highest end-address of currently active blocks
               idx := GetWord(@Memory[E_LINE]); // Default start point if no blocks exist
               
               for k := 0 to MAXSLOTS do
               begin
                  if (k <> f) and ManArray[k].Valid and ((ManArray[k].Status and MS_INMEMORY) <> 0) then
                  begin
                     // If this block ends later than our current candidate address, update it
                     if (ManArray[k].wAddress + ManArray[k].wLength) > idx then
                        idx := ManArray[k].wAddress + ManArray[k].wLength;
                  end;
               end;
            End;
        End;

        // --- EXECUTE MOVE IF VALID ---
        if idx <> -1 then
        begin
           // 1. Collision Check
           // Check if the new range [idx, idx + length] overlaps with any EXISTING user block
           SourceIdx := 0; // Using SourceIdx as a flag: 0=Safe, 1=Collision
           
           for k := 0 to MAXSLOTS do
           begin
              if (k <> f) and ManArray[k].Valid and ((ManArray[k].Status and MS_INMEMORY) <> 0) then
              begin
                 // Check intersection
                 // A_Start < B_End AND A_End > B_Start
                 if (idx < (ManArray[k].wAddress + ManArray[k].wLength)) and 
                    ((idx + ManArray[f].wLength) > ManArray[k].wAddress) then
                 begin
                    SourceIdx := 1;
                    Break;
                 end;
              end;
           end;

           if SourceIdx = 1 then
           begin
              ShowMessage('Cannot move block: The target location overlaps with another block.');
           end
           else
           begin
              // 2. Perform Move
              // Optional: Clear old memory location
              if (ManArray[f].wAddress >= 0) and (ManArray[f].wAddress + ManArray[f].wLength <= 65536) then
                 FillChar(Memory[ManArray[f].wAddress], ManArray[f].wLength, 0);

              // Update Block Info
              ManArray[f].wAddress := idx;
              ManArray[f].Status := ManArray[f].Status or MS_INMEMORY;
              ManArray[f].Modified := 2;

              // Write Data to New Memory Location
              if Length(ManArray[f].Slot.data) > 0 then
              begin
                 if (idx + ManArray[f].wLength) <= 65536 then
                    Move(ManArray[f].Slot.data[0], Memory[idx], ManArray[f].wLength);
              end;

              // Update UI
              BuildManList;
              DrawLayout;
              
              // Re-select the item to update properties view
              if ListView1.Selected <> nil then
                 ListView1.Selected.MakeVisible(False);
           end;
        end;
      End;



      // End Case below
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

function TMemManagerForm.PackArrayToTBytes(Value: TZX0ByteArray): TByteArray ;
begin
  // 1. Hedef diziyi boyutlandir
  SetLength(Result, Length(Value));

  // 2. Eger veri varsa, hafizadan hafizaya kopyala
  if Length(Value) > 0 then
    Move(Value[0], Result[0], Length(Value));
end;



procedure TMemManagerForm.PopupMenu1Popup(Sender: TObject);
var
  HasSelection: Boolean;
begin
     // 1. Seçim Durumunu Kontrol Et
     HasSelection := (ListView1.Selected <> nil);

     // 2. Seçim Gerektiren Islemler
     Insertintomemory1.Enabled         := HasSelection; // Move/Insert into cursor
     Rewrite1.Enabled                  := HasSelection; // Rewrite to location
     Putto1.Enabled                    := HasSelection; // Send to preset location (Parent Menu)
     Lift1.Enabled                     := HasSelection; // Lift
     SetType1.Enabled                  := HasSelection; // Change Type (Parent Menu)
     
     Deselect1.Enabled                 := HasSelection;
     NewBlockfromCurrentState1.Enabled := HasSelection; // Duplicate
     
     Pack1.Enabled                     := HasSelection; // Pack zx0
     SendtoBinaryGrabber1.Enabled      := HasSelection; // Grab...
     
     ViewasHex1.Enabled                := HasSelection;
     // Edit2.Enabled                  := HasSelection; // Edit in Editor (Genelde henüz implemente edilmediyse False kalir)
     
     CutBlock1.Enabled                 := HasSelection;
     CopyBlock1.Enabled                := HasSelection;
     
     Remove1.Enabled                   := HasSelection; // Delete
     Rename1.Enabled                   := HasSelection;
     Properties1.Enabled               := HasSelection;
     Lock1.Enabled                     := HasSelection;
     oggleTrack1.Enabled               := HasSelection;
     // 3. Clipboard Durumuna Göre Islemler
     // FClipboardValid degiskeni Cut/Copy yapildiginda True olur
     PasteBlock1.Enabled               := FClipboardValid;
end;

procedure TMemManagerForm.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin

  if Change = ctState then DrawLayout;
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
  Idx, i: Integer;
  SaveDlg: TSaveDialog;
  OpenDlg: TOpenDialog;
  Res: Integer;
  ShouldAppend: Boolean;
   sInput, sAddr: String;
  iSize: Integer;
  LenToWrite: Integer;
  EmptyData: TByteArray;
   packd: TZX0ByteArray;
  TargetAddr: Word;
begin
  Case (Sender As TComponent).Tag Of
     99:
     Begin
       MemManagerForm.Close;
       
     End;
     49: // Clear Management (New)
      Begin
        if ListView1.Items.Count > 0 then
        begin
          if MessageDlg('Are you sure you want to clear the entire project? This will clear the blocks list but memory contents will not be modified.', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
             // Iterate through all slots and invalidate them
             for i := 0 to MAXSLOTS do
             begin
               ManArray[i].Valid := False;
               SetLength(ManArray[i].Slot.data, 0); // Release memory
             end;
             BuildManList; // Refresh UI
          end;
        end;
      End;

    40: //Import File
      Begin
         LoadFileToByteArray;
      End;
    41: //Grab from screen
      Begin
           Idx:=CreateMblock('untitled.scr',16384,6912,1,0,0,false,True,ArrayToTBytes(Memory,6912,16384));
           BuildManList;
      End;

    42:
        Begin // Memory grabber
           BinaryGrabWindow.BlockAddress := FCurrentAddr;
           BinaryGrabWindow.BlockSize := 1024;
           BinaryGrabWindow.updateBoxes;
           

           CentreFormOnForm(BinaryGrabWindow, Self);
           ShowWindow(BinaryGrabWindow, True);
           If Not BinaryGrabWindow.Cancelled Then Begin
           if BinaryGrabWindow.BlockAddress<16384 Then Begin
              Idx:= CreateMblock('RomGrab.bin',BinaryGrabWindow.BlockAddress,BinaryGrabWindow.BlockSize,0,0,0,false,True,ArrayToTBytes(Memory,0,FCurrentAddr));
              End Else Begin
              Idx:= CreateMblock('RamGrab.bin',BinaryGrabWindow.BlockAddress,BinaryGrabWindow.BlockSize,0,0,0,false,True,ArrayToTBytes(Memory,0,FCurrentAddr));

              End;
              BuildManList;
           End;
        End;
43: // Create zx0 Depacker
      Begin
         TargetAddr := 0;

         // 1. Determine Target Address
         // If context menu opened on Ruler (FastIMG1), use Cursor Position
         if PopupMenu1.PopupComponent = FastIMG1 then
         begin
             TargetAddr := FCurrentAddr;
         end
         else
         begin
             // Else ask user
             sAddr := '32768';
             if InputQuery('Create Depacker', 'Fixed Memory Address:', sAddr) then
                TargetAddr := StrToIntDef(sAddr, 0);
         end;

         // 2. Create Block if Address is Valid
         if (TargetAddr > 0) and (TargetAddr <= 65535) then
         begin
             // Generate Relocated Code
             packd := RelocateDepacker(TargetAddr);

             // Create Memory Block (Size is 80 bytes for std depacker)
             CreateMBlock('zx0depacker.bin', TargetAddr, 81, 8, 0, 0, False, True, packarraytotbytes(packd) );
             
             BuildManList;
             DrawLayout;
         end;
      End;




      44:
				begin
				  SaveDlg := TSaveDialog.Create(Self);
				  try
					 SaveDlg.Title := 'Save Memory Manager Project';
					 SaveDlg.Filter := 'Basin Manager Project (*.bmm)|*.bmm|All Files (*.*)|*.*';
					 SaveDlg.DefaultExt := 'bmm';
					
					 if SaveDlg.Execute then
					 begin
					  SaveManagementProject(SaveDlg.FileName);
					 end;
				  finally
					 SaveDlg.Free;
				  end;
				end;
        45:  //Open Management
        Begin
			  OpenDlg := TOpenDialog.Create(Self);
			  try
				OpenDlg.Filter := 'Basin Manager Project (*.bmm)|*.bmm|All Files (*.*)|*.*';
				
				if OpenDlg.Execute then
				begin
				  ShouldAppend := False;
				  
				  // Check if there is existing data
				  if ListView1.Items.Count > 0 then
				  begin
					Res := MessageDlg('Do you want to append to the current project?'#13#10 +
									  'Yes: Add to current list'#13#10 +
									  'No: Clear and load'#13#10 +
									  'Cancel: Abort', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
									  
					if Res = mrCancel then Exit;
					if Res = mrYes then ShouldAppend := True;
				  end;
				  
				  LoadManagementProject(OpenDlg.FileName, ShouldAppend);
				end;
			  finally
				OpenDlg.Free;
			  end;

        End;
        46: //Grab basic
        Begin
           iSize:= GetWord(@Memory[STKEND])-23296;
           Idx:=CreateMblock('BasicBackup',23296,iSize,MB_ZX0,0,0,false,True,ArrayToTBytes(Memory,iSize,23296));
           BuildManList;

        End;

        50:
        Begin
            ExportMetaData;
        End;
        51:
        Begin
                 CheckChanges;  //this also refreshes management list
                 DrawLayout;
                 HexShown:=-1;
                 HexCursor;
        End;

        100: // Create a new empty Block
      Begin
          sInput := '768'; // Default value
          if InputQuery('Create New Block', 'Enter block size (bytes):', sInput) then
          begin
             iSize := StrToIntDef(sInput, 0);

             // Validate size (Must be between 1 and 64KB)
             if (iSize > 0) and (iSize <= 65536) then
             begin
                // Prepare dynamic array with Zeros
                SetLength(EmptyData, iSize);
                FillChar(EmptyData[0], iSize, 0);

                // Create block at current Red Cursor position
                // BlockType 0 = Data, Enabled = True
                Inc(NewCounter);
                CreateMBlock('NewBlock'+IntToStr(NewCounter)+'.bin', FCurrentAddr, iSize, 0, 0, 0, False, True, EmptyData);

                BuildManList;
             end
             else
             begin
                ShowMessage('Invalid size! Please enter a value between 1 and 65536.');
             end;
          end;
      End;
      End;




  End;

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
           DrawLayout;
     End;
     S:='';
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
  DrawLayout; 
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
  If Filename = '' Then Filename := OpenFile(Handle, 'Import Binary', FTypes, '', False, True);
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
            CreateMblock(ExtractFileName(FileName),16384,Size,MB_RAW,0,0,false,True,ArrayToTBytes(fileContents,0,0));
            BuildManList;
          end;
   End
   Else
   Begin
        ShowMessage('File is empty or too big. (Max Block Size: 128kb)');
   End;


end;

procedure TMemManagerForm.LoadManagementProject(FileName: String; AppendData: Boolean);
var
  FileStream: TFileStream;
  ProjHeader: TManProjectHeader;
  BlockMeta: TManBlockMeta;
  i, k, FreeSlot: Integer;
  SlotsFull: Boolean;
begin
  if not FileExists(FileName) then Exit;

  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FileStream.ReadBuffer(ProjHeader, SizeOf(TManProjectHeader));

    // VERSION CHECK LOGIC
    if ProjHeader.Signature = 'BMM1' then
    begin
       // Correct version, proceed
    end
    else if ProjHeader.Signature = 'BMM2' then
    begin
       // Future logic for BMM2 (e.g., read a larger record)
       // For now, treat as error or fallback
    end
    else
    begin
      MessageDlg('Unknown file format.', mtError, [mbOK], 0);
      Exit;
    end;

    if not AppendData then
    begin
      for i := 0 to MAXSLOTS do
      begin
        ManArray[i].Valid := False;
        SetLength(ManArray[i].Slot.data, 0);
      end;
    end;

    SlotsFull := False;
    for i := 0 to ProjHeader.BlockCount - 1 do
    begin
      // Read BMM1 Structure
      FileStream.ReadBuffer(BlockMeta, SizeOf(TManBlockMeta));

      FreeSlot := -1;
      for k := 0 to MAXSLOTS do
      begin
        if not ManArray[k].Valid then
        begin
          FreeSlot := k;
          Break;
        end;
      end;

      if FreeSlot = -1 then
      begin
        SlotsFull := True;
        Break;
      end;

      with ManArray[FreeSlot] do
      begin
        Valid       := True;
        Enabled     := BlockMeta.Enabled;
        BlockType   := BlockMeta.BlockType;
        State       := BlockMeta.State;
        wBlockIndex := BlockMeta.wBlockIndex;
        FileName    := String(BlockMeta.FileName);
        wAddress    := BlockMeta.wAddress;
        wLength     := BlockMeta.wLength;
        dAddress    := BlockMeta.dAddress;
        Modified    := BlockMeta.Modified;
        CanBreak    := BlockMeta.CanBreak;
        Status      := BlockMeta.Status; // Load Status
        wPage       := BlockMeta.wPage;

        if wLength > 0 then
        begin
          SetLength(Slot.data, wLength);
          FileStream.ReadBuffer(Slot.data[0], wLength);
        end
        else
          SetLength(Slot.data, 0);
      end;
    end;
  finally
    FileStream.Free;
    BuildManList;
    DrawLayout; // Redraw
  end;
end;

procedure TMemManagerForm.SaveManagementProject(FileName: String);
var
  FileStream: TFileStream;
  ProjHeader: TManProjectHeader;
  BlockMeta: TManBlockMeta;
  i, ValidCount: Integer;
begin
  ValidCount := 0;
  for i := 0 to MAXSLOTS do
    if ManArray[i].Valid then Inc(ValidCount);

  // Prepare Header
  FillChar(ProjHeader, SizeOf(ProjHeader), 0);
  ProjHeader.Signature := BMM_SIGNATURE; // 'BMM1'
  ProjHeader.BlockCount := ValidCount;

  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteBuffer(ProjHeader, SizeOf(TManProjectHeader));

    for i := 0 to MAXSLOTS do
    begin
      if ManArray[i].Valid then
      begin
        // Prepare Metadata
        FillChar(BlockMeta, SizeOf(BlockMeta), 0); // Clear reserved bytes (crucial for future compatibility)
        
        BlockMeta.Enabled     := ManArray[i].Enabled;
        BlockMeta.BlockType   := ManArray[i].BlockType;
        BlockMeta.State       := ManArray[i].State;
        BlockMeta.wBlockIndex := ManArray[i].wBlockIndex;
        BlockMeta.FileName    := ShortString(ManArray[i].FileName);
        BlockMeta.wAddress    := ManArray[i].wAddress;
        BlockMeta.wLength     := ManArray[i].wLength;
        BlockMeta.dAddress    := ManArray[i].dAddress;
        BlockMeta.Modified    := ManArray[i].Modified;
        BlockMeta.CanBreak    := ManArray[i].CanBreak;
        BlockMeta.Status      := ManArray[i].Status; // Save the Status word
        BlockMeta.wPage       := ManArray[i].wPage;

        FileStream.WriteBuffer(BlockMeta, SizeOf(TManBlockMeta));

        if Length(ManArray[i].Slot.data) > 0 then
           FileStream.WriteBuffer(ManArray[i].Slot.data[0], Length(ManArray[i].Slot.data));
      end;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TMemManagerForm.txtaddressKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  NewAddr: Integer;
begin
  // Check if the pressed key is ENTER
  if Key = VK_RETURN then
  begin
    // 1. Convert text to integer safely (Default to -1 if invalid)
    NewAddr := StrToIntDef(Trim(txtaddress.Text), -1);

    // 2. Validate range (Spectrum memory is 0 - 65535)
    if (NewAddr >= 0) and (NewAddr <= 65535) then
    begin
      // Update the internal tracking variable
      FCurrentAddr := NewAddr;

      // Update the Visual Map (Draws the red line at the new FCurrentAddr)
      DrawLayout;

      // Update the Hex Editor (Existing function moves cursor based on txtaddress)
      HexCursor;
    end
    else
    begin
      // If invalid, revert text to the last known valid address
      txtaddress.Text := IntToStr(FCurrentAddr);
      Beep;
    end;

    // Suppress the Windows "Ding" sound for the Enter key
    Key := 0;
  end;
end;



procedure TMemManagerForm.ListView1Click(Sender: TObject);
begin
     DrawLayout;
end;

procedure TMemManagerForm.ToggleROMView1Click(Sender: TObject);
begin
  FZoomMode := not FZoomMode;
  ToggleRomView1.Checked:=FZoomMode;
  HexShown:=-1;
  hexcursor;

  DrawLayout;
end;

procedure TMemManagerForm.PerformZoom(ZoomIn: Boolean);
var
  NewLen, NewStart: Integer;
begin

  if ZoomIn then
    NewLen := FViewLength div 2
  else
    NewLen := FViewLength * 2;

  if NewLen < 256 then NewLen := 256; 

  if NewLen > 65536 then NewLen := 65536;


  NewStart := FCurrentAddr - (NewLen div 2);


  if NewStart < 0 then NewStart := 0;

  if (NewStart + NewLen) > 65536 then 
     NewStart := 65536 - NewLen;

  FViewStart := NewStart;
  FViewLength := NewLen;
  
  DrawLayout;
end;

procedure TMemManagerForm.Button3Click(Sender: TObject); // Zoom IN (+)
begin
  PerformZoom(True);
end;

procedure TMemManagerForm.Button4Click(Sender: TObject); // Zoom OUT (-)
begin
  PerformZoom(False);
end;

procedure TMemManagerForm.Button2Click(Sender: TObject);
begin
     CheckChanges;  //this also refreshes management list
     DrawLayout;
     HexShown:=-1;
     HexCursor;
end;

procedure TMemManagerForm.ExportMetaData;
var
  i: Integer;
  IncludeFilenames: Boolean;
  DataStr: String;
  FirstItem: Boolean;
  Res: Integer;
begin
  // 1. Check if emulation is running
  if BASinOutput.Running then
  begin
    MessageDlg('Cannot insert code while emulation is running!' + #13 + #13 + 
               'Stop the running program first.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // 2. Ask user preference for filenames
  Res := MessageDlg('Include filenames in DATA statements?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  if Res = mrCancel then Exit;
  IncludeFilenames := (Res = mrYes);

  // 3. Build the DATA string
  DataStr := '9965 DATA ';
  FirstItem := True;

  for i := 0 to MAXSLOTS do
  begin
    // Check validity
    if ManArray[i].Valid then
    begin
      // Logic: Process only UNCHECKED items (Enabled = False) as requested
      // If the checkbox in the list corresponds to .Enabled, then unchecked means Enabled=False.
      if not ManArray[i].Enabled then 
      begin
        if not FirstItem then
          DataStr := DataStr + ','; // Add comma separator

        if IncludeFilenames then
        begin
          // Add "Filename", Address, Length
          DataStr := DataStr + '"' + ManArray[i].FileName + '",' + 
                     IntToStr(ManArray[i].wAddress) + ',' + 
                     IntToStr(ManArray[i].wLength);
        end
        else
        begin
          // Add Address, Length only
          DataStr := DataStr + IntToStr(ManArray[i].wAddress) + ',' + 
                     IntToStr(ManArray[i].wLength);
        end;

        FirstItem := False;
      end;
    end;
  end;

  // 4. Check if anything was generated
  if FirstItem then // Still true means no items were found
  begin
    ShowMessage('No unchecked blocks found to export.');
    Exit;
  end;

  // 5. Send to Editor
  // Prepare the AddCodeWindow
  AddCodeWindow.Memo1.Lines.Clear;
  AddCodeWindow.Memo1.Lines.Add(DataStr);
  
  // Trigger the paste mechanism
  AddCodeWindow.PasteLines;
end;


procedure TMemManagerForm.Feedback1Click(Sender: TObject);
begin
    ShellExecute(0, 'open', PChar('https://github.com/ref-xx/basinc/issues'), nil, nil, SW_SHOWNORMAL);

end;

procedure TMemManagerForm.HelponMemoryManagerWindow1Click(Sender: TObject);
begin
  BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/window_memory_manager.html'), HH_DISPLAY_TOPIC, 0);
end;

end.
