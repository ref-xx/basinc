unit CheqEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ToolWin, ComCtrls, FastIMG, ExtCtrls, ThemeBevelUnit,
  Buttons, StdCtrls, CheqMap;

type
  TFormCheqEdit = class(TForm)
    FastIMG1: TFastIMG;
    CheqMainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    View1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Saveas1: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Export1: TMenuItem;
    N3: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Grid1: TMenuItem;
    Image1: TMenuItem;
    EnableColor1: TMenuItem;
    N4: TMenuItem;
    PsuedoFlash1: TMenuItem;
    N5: TMenuItem;
    Import1: TMenuItem;
    Open2: TMenuItem;
    Invert1: TMenuItem;
    N6: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Clear1: TMenuItem;
    SelectNone1: TMenuItem;
    N7: TMenuItem;
    CoolBar2: TCoolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Bevel1: TThemeBevel;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Bevel2: TThemeBevel;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    Bevel4: TThemeBevel;
    TimerPsuedoFlash: TTimer;
    ThemeBevel1: TThemeBevel;
    N8: TMenuItem;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    N9: TMenuItem;
    SetPageColors1: TMenuItem;
    DefaultPageColors1: TMenuItem;
    CharacterSet1: TMenuItem;
    SetDimensions1: TMenuItem;
    StatusBar1: TStatusBar;
    Cut1: TMenuItem;
    SpeedButton10: TSpeedButton;
    Crop1: TMenuItem;
    N11: TMenuItem;
    CanvasPanel: TPanel;
    OverlayPanel: TPanel;
    ScrollHorizontal: TScrollBar;
    ScrollVertical: TScrollBar;
    PalettePanel: TPanel;
    PaletteImage: TFastIMG;
    GraphButton: TSpeedButton;
    ColorToolbar: TPanel;
    Ink0: TSpeedButton;
    Ink1: TSpeedButton;
    Ink2: TSpeedButton;
    Ink3: TSpeedButton;
    Ink4: TSpeedButton;
    Ink5: TSpeedButton;
    Ink6: TSpeedButton;
    Ink7: TSpeedButton;
    Paper0: TSpeedButton;
    Paper1: TSpeedButton;
    Paper2: TSpeedButton;
    Paper3: TSpeedButton;
    Paper4: TSpeedButton;
    Paper5: TSpeedButton;
    Paper6: TSpeedButton;
    Paper7: TSpeedButton;
    BrightButton: TSpeedButton;
    FlashButton: TSpeedButton;
    PaintButton: TSpeedButton;
    HoverInk: TShape;
    HoverPaper: TShape;
    InkLabel: TLabel;
    PaperLabel: TLabel;
    ExportButtons: TPanel;
    SaveBasicButton: TButton;
    SaveImageButton: TButton;
    ImportImage: TImage;
    ImportColorCheck: TCheckBox;
    ImportInfo: TLabel;
    BrowseImageButton: TButton;
    ImportNowButton: TButton;
    CloseImportButton: TButton;
    DimensionPanel: TPanel;
    DimensionLabel: TLabel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    ApplyDimensionsButton: TButton;
    CancelDimensionsButton: TButton;
    GraphOpenDialog: TOpenDialog;
    GraphSaveDialog: TSaveDialog;
    ImageOpenDialog: TOpenDialog;
    ExportSaveDialog: TSaveDialog;
    CloseExportButton: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    NumberingGroup: TGroupBox;
    StartLabel: TLabel;
    StepLabel: TLabel;
    StartEdit: TEdit;
    StepEdit: TEdit;
    SkipEmptyCheck: TCheckBox;
    TabSheet2: TTabSheet;
    MethodGroup: TGroupBox;
    SpaceLabel: TLabel;
    SpaceCombo: TComboBox;
    CompressedCheck: TCheckBox;
    CompressionTab: TTabSheet;
    CompressionGroup: TGroupBox;
    CompressionMethodLabel: TLabel;
    CompressionCombo: TComboBox;
    CompressionChunkLabel: TLabel;
    CompressionChunkEdit: TEdit;
    CompressionDataOnlyCheck: TCheckBox;
    CompressionHint: TLabel;
    CHRCheck: TCheckBox;
    ColorCheck: TCheckBox;
    ExportInkCheck: TCheckBox;
    ExportPaperCheck: TCheckBox;
    ExportBrightCheck: TCheckBox;
    ExportFlashCheck: TCheckBox;
    ControlColorCheck: TCheckBox;
    TabSheet3: TTabSheet;
    PageColorGroup: TGroupBox;
    ExportInkLabel: TLabel;
    ExportPaperLabel: TLabel;
    InitColorsCheck: TCheckBox;
    ExportInkEdit: TEdit;
    ExportPaperEdit: TEdit;
    ExportBrightCheck2: TCheckBox;
    ExportFlashCheck2: TCheckBox;
    TabSheet4: TTabSheet;
    BridgeGroup: TGroupBox;
    BridgeHint: TLabel;
    BridgeCombo: TComboBox;
    BridgeButton: TButton;
    GroupBox3: TGroupBox;
    ExportSelectionCheck: TCheckBox;
    TabSheet5: TTabSheet;
    CodeMemo: TMemo;
    GenerateButton: TButton;
    CopyCodeButton: TButton;
    SendBasicButton: TButton;
    JoinFullCheck: TCheckBox;
    Help1: TMenuItem;
    GraphTileEditorHelp1: TMenuItem;
    CharsInformation1: TMenuItem;
    N10: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    MoveLeft1: TMenuItem;
    MoveRight1: TMenuItem;
    CHARS1: TMenuItem;
    UDG1: TMenuItem;
    SpeedButton12: TSpeedButton;
    N12: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OverlayPanelResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuClick(Sender: TObject);
    procedure ToolbarClick(Sender: TObject);
    procedure ColorButtonClick(Sender: TObject);
    procedure FlagButtonClick(Sender: TObject);
    procedure GraphButtonClick(Sender: TObject);
    procedure PaletteImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FastIMG1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollChange(Sender: TObject);
    procedure TimerPsuedoFlashTimer(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure ExportOptionsChange(Sender: TObject);
    procedure CopyCodeButtonClick(Sender: TObject);
    procedure SendBasicButtonClick(Sender: TObject);
    procedure SaveBasicButtonClick(Sender: TObject);
    procedure SaveImageButtonClick(Sender: TObject);
    procedure BridgeButtonClick(Sender: TObject);
    procedure CloseOverlayClick(Sender: TObject);
    procedure BrowseImageButtonClick(Sender: TObject);
    procedure ImportNowButtonClick(Sender: TObject);
    procedure ApplyDimensionsButtonClick(Sender: TObject);
  private
    FShuttingDown: Boolean;
    FMap: TCheqMap;
    FZoom, FBrush, FInk, FPaper, FBright, FFlash: Integer;
    FLastInk, FLastPaper, FLastBright, FLastFlash: Integer;
    FFilename, FClipboard: AnsiString;
    FDrawing, FErasing, FSelecting, FHasSelection, FFlashPhase: Boolean;
    FSelection, FSelectionPixels: TRect;
    FSelectionGraph: Boolean;
    FAnchor, FLastPoint, FHover, FHoverPixels: TPoint;
    FUpdating, FNewDimensions: Boolean;
    FExportLines: TStringList;
    function EffectiveArea: TRect;
    function EffectivePixels: TRect;
    procedure NormalizeSelection;
    procedure ExpandSelectionToCells;
    function ConfirmDiscard: Boolean;
    function ExecuteFileDialog(Dialog: TOpenDialog): Boolean;
    function SaveDocument(SaveAs: Boolean): Boolean;
    procedure FinishStroke;
    procedure DrawCanvas;
    procedure DrawPalette;
    procedure RefreshEditor;
    procedure UpdateLayout;
    procedure CenterOverlayGroups;
    procedure UpdateCharacterInfo;
    procedure UpdateScrollbars;
    procedure ShowOverlay(Which: Integer);
    procedure SyncExportColors;
    procedure SetPageColors(Attribute: Byte);
    procedure PaintAt(X, Y: Integer);
    procedure UpdateSelection(X, Y: Integer);
    procedure ShowDimensions(NewDocument: Boolean);
    function ExportOptions: TCheqExportOptions;
    function ExportDocument: TCheqMap;
    function GenerateCode: Boolean;
    function GlyphRow(Character, Row: Integer): Byte;
    procedure RenderBitmap(Bitmap: TBitmap);
  public
    destructor Destroy; override;
    property Document: TCheqMap read FMap;
    function CanCloseDocument: Boolean;
    procedure RefreshLanguage;
  end;

var
  FormCheqEdit: TFormCheqEdit;

implementation

{$R *.dfm}

uses Math, Clipbrd, FastDIB, FastDraw, FastCore, Utility, ROMUtils,
  BasinMain, BinaryForm, BASSupport, Filing, JPEG, CommDlg, Display,
  Languages, UDGEdit, SysVars;

function SpectrumColor(Index, Bright: Integer): TColor;
var Level: Integer;
begin
  if Bright = 1 then Level := 255 else Level := 205;
  Result := RGB(Ord((Index and 2) <> 0) * Level,
    Ord((Index and 4) <> 0) * Level, Ord((Index and 1) <> 0) * Level);
end;

procedure TFormCheqEdit.FormCreate(Sender: TObject);
begin
  { The fixed-size groups remain editable side by side in the DFM.
    Only their background panel fills the running window. }
  FUpdating := True;
  try
    AutoScroll := False;
    OverlayPanel.Visible := False;
    GroupBox1.Visible := False;
    GroupBox2.Visible := False;
    OverlayPanel.Align := alClient;
  finally FUpdating := False; end;
  FMap := TCheqMap.Create;
  FExportLines := TStringList.Create;
  FZoom := 2; FBrush := -1;
  FInk := -1; FPaper := -1; FBright := -1; FFlash := -1;
  FLastInk := 0; FLastPaper := 7; FLastBright := 0; FLastFlash := 0;
  FHover := Point(0, 0);
  SpaceCombo.ItemIndex := 1;
  CompressionCombo.ItemIndex := 0;
  BridgeCombo.ItemIndex := 0;
  SyncExportColors;
  RefreshEditor;
end;

destructor TFormCheqEdit.Destroy;
begin
  { VCL can send layout/scroll notifications while destroying child
    controls, after OnDestroy has released the document. }
  FShuttingDown := True;
  if Assigned(TimerPsuedoFlash) then TimerPsuedoFlash.Enabled := False;
  FinishStroke;
  if FormCheqEdit = Self then FormCheqEdit := nil;
  inherited Destroy;
end;

procedure TFormCheqEdit.FormDestroy(Sender: TObject);
begin
  FShuttingDown := True;
  FUpdating := True;
  FreeAndNil(FExportLines);
  FreeAndNil(FMap);
end;

procedure TFormCheqEdit.FormShow(Sender: TObject);
begin
  Constraints.MinWidth := 460;
  Constraints.MinHeight := 440;
  RefreshEditor;
end;

procedure TFormCheqEdit.FormActivate(Sender: TObject);
begin
  RefreshEditor;
end;

procedure TFormCheqEdit.RefreshLanguage;
begin
  RefreshEditor;
end;

procedure TFormCheqEdit.UpdateCharacterInfo;
var Address: Integer;
begin
  Address := GetWord(@Memory[CHARS]);
  CHARS1.Caption := Format('CHARS = %d ($%s)', [Address, IntToHex(Address,4)]);
  if ((Address+256) and $FFFF) < 16384 then
    CHARS1.Caption := CHARS1.Caption + ' - Uneditable: Change CHARS address';
  Address := GetWord(@Memory[UDG]);
  UDG1.Caption := Format('UDG = %d ($%s)', [Address, IntToHex(Address,4)]);
end;

procedure TFormCheqEdit.FormResize(Sender: TObject);
begin
  if FShuttingDown or (FMap = nil) then Exit;
  UpdateLayout;
  DrawPalette;
  DrawCanvas;
end;

procedure TFormCheqEdit.OverlayPanelResize(Sender: TObject);
begin
  CenterOverlayGroups;
end;

procedure TFormCheqEdit.CenterOverlayGroups;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  GroupBox1.Left := (OverlayPanel.ClientWidth - GroupBox1.Width) div 2;
  GroupBox1.Top := (OverlayPanel.ClientHeight - GroupBox1.Height) div 2;
  GroupBox2.Left := (OverlayPanel.ClientWidth - GroupBox2.Width) div 2;
  GroupBox2.Top := (OverlayPanel.ClientHeight - GroupBox2.Height) div 2;
end;

procedure TFormCheqEdit.FinishStroke;
begin
  if FDrawing and Assigned(FMap) then FMap.EndEdit;
  FDrawing := False; FSelecting := False;
  if Assigned(FastIMG1) then
    if FastIMG1.HandleAllocated then
      if GetCapture = FastIMG1.Handle then ReleaseCapture;
end;

procedure TFormCheqEdit.FormDeactivate(Sender: TObject);
begin
  if FShuttingDown then Exit;
  FinishStroke;
end;

procedure TFormCheqEdit.FormHide(Sender: TObject);
begin
  if FShuttingDown then Exit;
  FinishStroke;
  TimerPsuedoFlash.Enabled := False;
end;

function TFormCheqEdit.ConfirmDiscard: Boolean;
var Answer: Integer;
begin
  Result := True;
  if FShuttingDown or (FMap = nil) then Exit;
  FinishStroke;
  if not FMap.Dirty then Exit;
  Answer := MessageDlg('Save changes in ' + GraphTileMapEditorName(Opt_Language) + '?', mtConfirmation, mbYesNoCancel, 0);
  if Answer = mrCancel then Result := False
  else if Answer = mrYes then Result := SaveDocument(False);
end;

procedure TFormCheqEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FinishStroke;
  CanClose := True;
end;

function TFormCheqEdit.CanCloseDocument: Boolean;
begin
  Result := ConfirmDiscard;
end;

function TFormCheqEdit.ExecuteFileDialog(Dialog: TOpenDialog): Boolean;
var Ofn: TOpenFileName; FileBuffer: array[0..65534] of Char;
  Filter, Directory: String; ErrorCode: DWORD;
begin
  { Match Filing.OpenFile's native Windows dialog path. Delphi 7's
    Execute installs a VCL hook, which forces the legacy dialog UI.
    Keep filters and options on the existing designer components. }
  if Assigned(DisplayWindow) then DisplayWindow.WantsFront := False;
  FillChar(Ofn, SizeOf(Ofn), 0);
  FillChar(FileBuffer, SizeOf(FileBuffer), 0);
  StrPLCopy(FileBuffer, Dialog.FileName, High(FileBuffer));
  Filter := StringReplace(Dialog.Filter, '|', #0, [rfReplaceAll]) + #0#0;
  Directory := Dialog.InitialDir;
  Ofn.lStructSize := SizeOf(Ofn);
  Ofn.hwndOwner := Handle;
  Ofn.lpstrFile := FileBuffer;
  Ofn.nMaxFile := SizeOf(FileBuffer);
  Ofn.lpstrFilter := PChar(Filter);
  Ofn.nFilterIndex := Dialog.FilterIndex;
  if Dialog.DefaultExt <> '' then Ofn.lpstrDefExt := PChar(Dialog.DefaultExt);
  if Dialog.Title <> '' then Ofn.lpstrTitle := PChar(Dialog.Title);
  if Directory <> '' then Ofn.lpstrInitialDir := PChar(Directory);
  Ofn.Flags := OFN_EXPLORER or OFN_ENABLESIZING or OFN_NOCHANGEDIR;
  if ofHideReadOnly in Dialog.Options then Ofn.Flags := Ofn.Flags or OFN_HIDEREADONLY;
  if ofFileMustExist in Dialog.Options then Ofn.Flags := Ofn.Flags or OFN_FILEMUSTEXIST;
  if ofPathMustExist in Dialog.Options then Ofn.Flags := Ofn.Flags or OFN_PATHMUSTEXIST;
  if ofOverwritePrompt in Dialog.Options then Ofn.Flags := Ofn.Flags or OFN_OVERWRITEPROMPT;
  if Dialog is TSaveDialog then Result := CommDlg.GetSaveFileName(Ofn)
  else Result := CommDlg.GetOpenFileName(Ofn);
  if Result then
  begin
    Dialog.FileName := StrPas(FileBuffer);
    Dialog.FilterIndex := Ofn.nFilterIndex;
  end
  else
  begin
    ErrorCode := CommDlgExtendedError;
    if ErrorCode <> 0 then
      raise EOSError.CreateFmt('Could not open the file dialog (Windows error %d).', [ErrorCode]);
  end;
end;

function TFormCheqEdit.SaveDocument(SaveAs: Boolean): Boolean;
var Stream: TFileStream; Name, TempName: String;
  TempBuffer: array[0..MAX_PATH] of Char;
begin
  Result := False;
  Name := FFilename;
  if SaveAs or (Name = '') then
  begin
    GraphSaveDialog.FileName := Name;
    if not ExecuteFileDialog(GraphSaveDialog) then Exit;
    Name := GraphSaveDialog.FileName;
  end;
  TempName := '';
  try
    if GetTempFileName(PChar(ExtractFilePath(ExpandFileName(Name))), 'sbg', 0, TempBuffer) = 0 then
      RaiseLastOSError;
    TempName := TempBuffer;
    Stream := TFileStream.Create(TempName, fmCreate);
    try FMap.SaveToStream(Stream); finally Stream.Free; end;
    if not MoveFileEx(PChar(TempName), PChar(Name), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
      RaiseLastOSError;
    FFilename := Name; FMap.MarkSaved;
    Result := True; RefreshEditor;
  except
    on E: Exception do begin DeleteFile(TempName); MessageDlg(E.Message, mtError, [mbOK], 0); end;
  end;
end;

procedure TFormCheqEdit.ShowDimensions(NewDocument: Boolean);
begin
  FinishStroke;
  FNewDimensions := NewDocument;
  WidthEdit.Text := IntToStr(FMap.Width); HeightEdit.Text := IntToStr(FMap.Height);
  DimensionPanel.Visible := True;
  DimensionPanel.BringToFront;
  WidthEdit.SetFocus;
end;

procedure TFormCheqEdit.ApplyDimensionsButtonClick(Sender: TObject);
var W, H: Integer;
begin
  try
    W := StrToInt(WidthEdit.Text); H := StrToInt(HeightEdit.Text);
    if (W < 2) or (H < 2) or (W > 256) or (H > 256) then
      raise ERangeError.Create('Enter dimensions between 2 x 2 and 256 x 256 character cells.');
    if FNewDimensions then
    begin
      if not ConfirmDiscard then Exit;
      FMap.NewMap(W, H); FFilename := '';
    end
    else FMap.Resize(W, H);
    FHasSelection := False;
    DimensionPanel.Visible := False;
    SyncExportColors;
    RefreshEditor;
  except on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0); end;
end;

function TFormCheqEdit.EffectiveArea: TRect;
begin
  if FHasSelection then Result := FSelection
  else Result := Rect(0, 0, FMap.Width, FMap.Height);
end;

function TFormCheqEdit.EffectivePixels: TRect;
begin
  if FHasSelection then Result := FSelectionPixels
  else Result := Rect(0,0,FMap.Width*2,FMap.Height*2);
end;

procedure TFormCheqEdit.NormalizeSelection;
begin
  if not FHasSelection then Exit;
  FSelectionPixels := FMap.NormalizeSelectionPixels(FSelectionPixels,FBrush<0);
  FSelection := CheqCellArea(FSelectionPixels);
  FSelectionGraph := (FBrush<0) and FMap.AreaIsGraph(FSelection);
end;

procedure TFormCheqEdit.ExpandSelectionToCells;
begin
  if not FHasSelection then Exit;
  FSelectionPixels := Rect(FSelection.Left*2,FSelection.Top*2,FSelection.Right*2,FSelection.Bottom*2);
  DrawCanvas;
end;

procedure TFormCheqEdit.MenuClick(Sender: TObject);
var Stream: TFileStream; Item: TMenuItem; I, DX, DY: Integer;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  Item := Sender as TMenuItem;
  { Standard edit shortcuts belong to the focused export/dimension field. }
  if (GroupBox1.Visible or GroupBox2.Visible or DimensionPanel.Visible) and
    (ActiveControl is TCustomEdit) then
  begin
    if Item = Copy1 then begin ActiveControl.Perform(WM_COPY, 0, 0); Exit; end;
    if Item = Cut1 then begin ActiveControl.Perform(WM_CUT, 0, 0); Exit; end;
    if Item = Paste1 then begin ActiveControl.Perform(WM_PASTE, 0, 0); Exit; end;
    if Item = Undo1 then begin ActiveControl.Perform(WM_UNDO, 0, 0); Exit; end;
    if Item = Redo1 then Exit;
    if Item = Clear1 then begin ActiveControl.Perform(WM_KEYDOWN, VK_DELETE, 0); Exit; end;
  end;
  FinishStroke;
  NormalizeSelection;
  try
    if (Item = Help1) or (Item = CharsInformation1) then UpdateCharacterInfo
    else if Item = GraphTileEditorHelp1 then
      BASinOutput.HtmlHelpOnline(Application.Handle,
        PChar(BASinDir+'\BASin.chm::/topics/window_graph_tile_map_editor.html'), HH_DISPLAY_TOPIC, 0)
    else if (Item = CHARS1) and (((GetWord(@Memory[CHARS])+256) and $FFFF) < 16384) then
    begin
      ShowWindow(SysVarsWindow, False);
      for I := 0 to SysVarsWindow.ListView1.Items.Count-1 do
        if SysVarsWindow.ListView1.Items[I].Caption = 'CHARS' then
        begin
          SysVarsWindow.ListView1.Items[I].Selected := True;
          SysVarsWindow.ListView1.Items[I].Focused := True;
          SysVarsWindow.ListView1.Items[I].MakeVisible(False);
          Break;
        end;
    end
    else if (Item = CHARS1) or (Item = UDG1) then
    begin
      UDGWindow.ImportCharacterSet(Item = UDG1);
      ShowWindow(UDGWindow, False);
    end
    else if Item = Open1 then ShowDimensions(True)
    else if Item = Open2 then
    begin
      if not ConfirmDiscard then Exit;
      if not ExecuteFileDialog(GraphOpenDialog) then Exit;
      Stream := TFileStream.Create(GraphOpenDialog.FileName, fmOpenRead or fmShareDenyWrite);
      try FMap.LoadFromStream(Stream); finally Stream.Free; end;
      FFilename := GraphOpenDialog.FileName; FHasSelection := False;
      SyncExportColors;
    end
    else if Item = Save1 then SaveDocument(False)
    else if Item = Saveas1 then SaveDocument(True)
    else if Item = Exit1 then Close
    else if Item = Undo1 then begin FMap.Undo; FHasSelection := False; SyncExportColors; end
    else if Item = Redo1 then begin FMap.Redo; FHasSelection := False; SyncExportColors; end
    else if (Item = Copy1) or (Item = Cut1) then
    begin
      if FHasSelection and FSelectionGraph then
      begin
        FClipboard := FMap.CopyPixels(FSelectionPixels);
        if Item = Cut1 then FMap.EditGraphPixels(FSelectionPixels,False);
      end
      else
      begin
        FClipboard := FMap.CopyArea(EffectiveArea);
        if Item = Cut1 then FMap.Clear(EffectiveArea);
      end;
    end
    else if Item = Paste1 then
    begin
      if FClipboard = '' then Exit;
      if Copy(FClipboard,1,4) = 'GPX1' then
      begin
        if FHasSelection then FMap.PastePixels(FClipboard,FSelectionPixels.Left,FSelectionPixels.Top)
        else FMap.PastePixels(FClipboard,FHoverPixels.X,FHoverPixels.Y);
      end
      else if FHasSelection then FMap.PasteArea(FClipboard, FSelection.Left, FSelection.Top)
      else FMap.PasteArea(FClipboard, FHover.X, FHover.Y);
    end
    else if Item = Clear1 then
    begin
      if FHasSelection and FSelectionGraph then FMap.EditGraphPixels(FSelectionPixels,False)
      else FMap.Clear(EffectiveArea);
    end
    else if Item = Crop1 then
    begin
      if not FHasSelection then Exit;
      ExpandSelectionToCells;
      FMap.Crop(FSelection); FHasSelection := False;
    end
    else if Item = SelectNone1 then begin FHasSelection := False; SpeedButton10.Down := False; end
    else if Item = Invert1 then
    begin
      if FHasSelection and FSelectionGraph then FMap.EditGraphPixels(FSelectionPixels,True)
      else FMap.Invert(EffectiveArea);
    end
    else if (Item = MoveUp1) or (Item = MoveDown1) or
      (Item = MoveLeft1) or (Item = MoveRight1) then
    begin
      DX := 0; DY := 0;
      if Item = MoveUp1 then DY := -1 else if Item = MoveDown1 then DY := 1
      else if Item = MoveLeft1 then DX := -1 else DX := 1;
      if FMap.Colored then ExpandSelectionToCells;
      FMap.ShiftPixels(EffectivePixels,DX,DY);
    end
    else if Item = Grid1 then Grid1.Checked := not Grid1.Checked
    else if Item = CharacterSet1 then CharacterSet1.Checked := not CharacterSet1.Checked
    else if Item = EnableColor1 then begin FMap.SetColored(not FMap.Colored); ColorCheck.Checked := FMap.Colored; end
    else if Item = PsuedoFlash1 then PsuedoFlash1.Checked := not PsuedoFlash1.Checked
    else if Item = ZoomIn1 then FZoom := Min(8, FZoom + 1)
    else if Item = ZoomOut1 then FZoom := Max(1, FZoom - 1)
    else if Item = SetDimensions1 then ShowDimensions(False)
    else if Item = SetPageColors1 then SetPageColors(CheqAttribute(FLastInk, FLastPaper, FLastBright, FLastFlash))
    else if Item = DefaultPageColors1 then SetPageColors(56)
    else if Item = Export1 then ShowOverlay(1)
    else if Item = Import1 then ShowOverlay(2);
    RefreshEditor;
  except on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0); end;
end;

procedure TFormCheqEdit.ToolbarClick(Sender: TObject);
var Button: TSpeedButton;
begin
  Button := Sender as TSpeedButton;
  if Button = SpeedButton1 then MenuClick(Open1)
  else if Button = SpeedButton2 then MenuClick(Open2)
  else if Button = SpeedButton3 then MenuClick(Save1)
  else if Button = SpeedButton4 then MenuClick(Cut1)
  else if Button = SpeedButton5 then MenuClick(Copy1)
  else if Button = SpeedButton6 then MenuClick(Paste1)
  else if Button = SpeedButton7 then MenuClick(Export1)
  else if Button = SpeedButton8 then MenuClick(Clear1)
  else if Button = SpeedButton9 then MenuClick(Grid1)
  else if Button = SpeedButton11 then MenuClick(EnableColor1)
  else if Button = SpeedButton12 then MenuClick(CharacterSet1)
  else if Button = SpeedButton14 then MenuClick(Undo1)
  else if Button = SpeedButton15 then MenuClick(Redo1)
  else if Button = SpeedButton10 then begin FinishStroke; DrawCanvas; end;
end;

procedure TFormCheqEdit.ColorButtonClick(Sender: TObject);
var B: TSpeedButton;
begin
  B := Sender as TSpeedButton;
  if B.Tag < 8 then
  begin
    if B.Down then begin FInk := B.Tag; FLastInk := FInk; end else FInk := -1;
  end
  else
  begin
    if B.Down then begin FPaper := B.Tag - 8; FLastPaper := FPaper; end else FPaper := -1;
  end;
end;

procedure TFormCheqEdit.FlagButtonClick(Sender: TObject);
var B: TSpeedButton; State: Integer; Name: String;
begin
  B := Sender as TSpeedButton;
  if B = BrightButton then begin State := FBright; Name := 'BRIGHT'; end
  else begin State := FFlash; Name := 'FLASH'; end;
  if State < 0 then State := 1 else if State = 1 then State := 0 else State := -1;
  B.Down := State >= 0;
  if State < 0 then B.Caption := Name + ': keep' else B.Caption := Name + ': ' + IntToStr(State);
  if B = BrightButton then
  begin FBright := State; if State >= 0 then FLastBright := State; end
  else begin FFlash := State; if State >= 0 then FLastFlash := State; end;
end;

procedure TFormCheqEdit.GraphButtonClick(Sender: TObject);
begin
  FBrush := -1;
  GraphButton.Down := True;
  NormalizeSelection;
  DrawPalette; DrawCanvas;
end;

procedure TFormCheqEdit.PaletteImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Index, Columns, Limit: Integer;
begin
  if Button <> mbLeft then Exit;
  Columns := Max(1, PaletteImage.Width div 24);
  Index := (Y div 24) * Columns + X div 24;
  if ProgramIs128k then Limit := 19 else Limit := 21;
  if Index < 96 then FBrush := 32 + Index
  else if Index < 96 + Limit then FBrush := 144 + Index - 96 else Exit;
  GraphButton.Down := False;
  NormalizeSelection;
  DrawPalette; DrawCanvas;
end;

function TFormCheqEdit.GlyphRow(Character, Row: Integer): Byte;
var Address: Integer;
begin
  Result := 0;
  if (Character >= 144) and (Character <= 164) then
  begin
    Address := GetWord(@Memory[UDG]) + (Character - 144) * 8 + Row;
    if (Address >= 0) and (Address <= 65535) then Result := Memory[Address];
  end
  else if (Character >= 32) and (Character <= 127) then
    Result := Memory[(GetWord(@Memory[CHARS]) + Character*8 + Row) and $FFFF]
  else if (Character >= 128) and (Character <= 143) then
    Result := EditorChars[(Character - 32) * 8 + Row + 1];
end;

procedure TFormCheqEdit.DrawPalette;
var I, X, Y, PX, PY, C, Columns, Count: Integer; Col: TColor;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  if not PalettePanel.Visible then Exit;
  Columns := Max(1, PaletteImage.Width div 24);
  if ProgramIs128k then Count := 115 else Count := 117;
  PalettePanel.Height := Max(64, ((Count + Columns - 1) div Columns) * 24 + 8);
  PaletteImage.Height := Max(1, PalettePanel.ClientHeight - 8);
  PaletteImage.Bmp.SetSize(Max(1, PaletteImage.Width), Max(1, PaletteImage.Height), 32);
  PaletteImage.Bmp.Clear(TColorToTFColor(clBtnFace));
  for I := 0 to Count - 1 do
  begin
    X := (I mod Columns) * 24; Y := (I div Columns) * 24;
    if I < 96 then C := 32 + I else C := 144 + I - 96;
    for PY := 0 to 19 do
      for PX := 0 to 19 do
      begin
        if C = FBrush then Col := clHighlight else Col := clWhite;
        if (PX >= 2) and (PX < 18) and (PY >= 2) and (PY < 18) then
          if (GlyphRow(C, (PY - 2) div 2) and (128 shr ((PX - 2) div 2))) <> 0 then
            if C = FBrush then Col := clHighlightText else Col := clBlack;
        if (X + PX < PaletteImage.Bmp.Width) and (Y + PY < PaletteImage.Bmp.Height) then
          PaletteImage.Bmp.Pixels32[PaletteImage.Bmp.Height - 1 - Y - PY, X + PX] := TFColorToTFColorA(TColorToTFColor(Col));
      end;
  end;
  PaletteImage.Repaint;
end;

procedure TFormCheqEdit.UpdateScrollbars;
var W, H: Integer;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  W := Max(1, FastIMG1.Width div (8 * FZoom)); H := Max(1, FastIMG1.Height div (8 * FZoom));
  ScrollHorizontal.Max := Max(0, FMap.Width - W);
  ScrollVertical.Max := Max(0, FMap.Height - H);
  ScrollHorizontal.LargeChange := W; ScrollVertical.LargeChange := H;
  ScrollHorizontal.Enabled := ScrollHorizontal.Max > 0;
  ScrollVertical.Enabled := ScrollVertical.Max > 0;
end;

procedure TFormCheqEdit.UpdateLayout;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  OverlayPanel.Visible := GroupBox1.Visible or GroupBox2.Visible;
  ColorToolbar.Visible := FMap.Colored and not GroupBox1.Visible and not GroupBox2.Visible;
  PalettePanel.Visible := CharacterSet1.Checked and not GroupBox1.Visible and not GroupBox2.Visible;
  PalettePanel.Top := CoolBar2.Top + CoolBar2.Height;
  if PalettePanel.Visible then ColorToolbar.Top := PalettePanel.Top + PalettePanel.Height
  else ColorToolbar.Top := CoolBar2.Top + CoolBar2.Height;
  CanvasPanel.Visible := not GroupBox1.Visible and not GroupBox2.Visible;
  if OverlayPanel.Visible then OverlayPanel.BringToFront;
  CenterOverlayGroups;
  PaletteImage.SetBounds(96, 4, Max(1, PalettePanel.ClientWidth - 100),
    Max(1, PalettePanel.ClientHeight - 8));
  DimensionPanel.Left := (ClientWidth - DimensionPanel.Width) div 2;
  DimensionPanel.Top := (ClientHeight - DimensionPanel.Height) div 2;
  UpdateScrollbars;
end;

procedure TFormCheqEdit.RefreshEditor;
var Name: String;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  NormalizeSelection;
  Undo1.Enabled := FMap.CanUndo; Redo1.Enabled := FMap.CanRedo;
  SpeedButton14.Enabled := Undo1.Enabled; SpeedButton15.Enabled := Redo1.Enabled;
  Paste1.Enabled := FClipboard <> ''; SpeedButton6.Enabled := Paste1.Enabled;
  Crop1.Enabled := FHasSelection;
  EnableColor1.Checked := FMap.Colored; SpeedButton11.Down := FMap.Colored;
  SpeedButton9.Down := Grid1.Checked;
  SpeedButton12.Down := CharacterSet1.Checked;
  TimerPsuedoFlash.Enabled := FMap.Colored and PsuedoFlash1.Checked and Visible;
  if FFilename = '' then Name := 'Untitled' else Name := ExtractFileName(FFilename);
  if FMap.Dirty then Name := Name + ' *';
  Caption := Name + ' - ' + GraphTileMapEditorName(Opt_Language);
  UpdateCharacterInfo;
  StatusBar1.SimpleText := Format('%d x %d cells / %d x %d GRAPH pixels',
    [FMap.Width, FMap.Height, FMap.Width * 2, FMap.Height * 2]);
  if FMap.IsGraph then StatusBar1.SimpleText := StatusBar1.SimpleText + ' | GRAPH'
  else StatusBar1.SimpleText := StatusBar1.SimpleText + ' | Tilemap';
  UpdateLayout; DrawPalette; DrawCanvas;
end;

procedure TFormCheqEdit.DrawCanvas;
var X, Y, CX, CY, PX, PY, C, A, Ink, Paper, B, V, CellSize, GridSize, GX, GY: Integer;
    Col, InkColor, PaperColor, GridColor: TFColorA; IsGrid: Boolean;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  if not CanvasPanel.Visible or (FastIMG1.Width < 1) or (FastIMG1.Height < 1) then Exit;
  FastIMG1.Bmp.SetSize(FastIMG1.Width, FastIMG1.Height, 32);
  FastIMG1.Bmp.Clear(TColorToTFColor(clAppWorkSpace));
  CellSize := 8 * FZoom;
  if (FBrush < 0) and (not FHasSelection or FSelectionGraph) then GridSize := CellSize div 2
  else GridSize := CellSize;
  GridColor := TFColorToTFColorA(TColorToTFColor(clGray));
  for CY := 0 to (FastIMG1.Height - 1) div CellSize do
  begin
    Y := ScrollVertical.Position + CY;
    if Y >= FMap.Height then Break;
    for CX := 0 to (FastIMG1.Width - 1) div CellSize do
    begin
      X := ScrollHorizontal.Position + CX;
      if X >= FMap.Width then Break;
      C := FMap.Characters[X, Y];
      if FMap.Colored then A := FMap.Attributes[X, Y] else A := FMap.PageAttribute;
      Ink := A and 7; Paper := (A shr 3) and 7; B := (A shr 6) and 1;
      if FMap.Colored and PsuedoFlash1.Checked and FFlashPhase and ((A and 128) <> 0) then
      begin V := Ink; Ink := Paper; Paper := V; end;
      InkColor := TFColorToTFColorA(TColorToTFColor(SpectrumColor(Ink, B)));
      PaperColor := TFColorToTFColorA(TColorToTFColor(SpectrumColor(Paper, B)));
      for PY := 0 to CellSize - 1 do
      begin
        if CY * CellSize + PY >= FastIMG1.Height then Break;
        V := GlyphRow(C, PY div FZoom);
        for PX := 0 to CellSize - 1 do
        begin
          if CX * CellSize + PX >= FastIMG1.Width then Break;
          if (V and (128 shr (PX div FZoom))) <> 0 then Col := InkColor else Col := PaperColor;
          IsGrid := Grid1.Checked and (((PX mod GridSize) = 0) or ((PY mod GridSize) = 0));
          if IsGrid then Col := GridColor;
          GX := X*CellSize+PX; GY := Y*CellSize+PY;
          if FHasSelection and (GX >= FSelectionPixels.Left*CellSize div 2) and
            (GX < FSelectionPixels.Right*CellSize div 2) and
            (GY >= FSelectionPixels.Top*CellSize div 2) and (GY < FSelectionPixels.Bottom*CellSize div 2) then
            if (GX < FSelectionPixels.Left*CellSize div 2+2) or
              (GX >= FSelectionPixels.Right*CellSize div 2-2) or
              (GY < FSelectionPixels.Top*CellSize div 2+2) or
              (GY >= FSelectionPixels.Bottom*CellSize div 2-2) then
              Col := TFColorToTFColorA(TColorToTFColor(clRed));
          FastIMG1.Bmp.Pixels32[FastIMG1.Height - 1 - (CY * CellSize + PY), CX * CellSize + PX] := Col;
        end;
      end;
    end;
  end;
  FastIMG1.Repaint;
end;

procedure TFormCheqEdit.UpdateSelection(X, Y: Integer);
var P: TPoint;
begin
  P := Point(EnsureRange(ScrollHorizontal.Position*2 + X div (FZoom * 4), 0, FMap.Width*2 - 1),
    EnsureRange(ScrollVertical.Position*2 + Y div (FZoom * 4), 0, FMap.Height*2 - 1));
  FSelectionPixels := Rect(Min(FAnchor.X, P.X), Min(FAnchor.Y, P.Y),
    Max(FAnchor.X, P.X) + 1, Max(FAnchor.Y, P.Y) + 1);
  FHasSelection := True;
  NormalizeSelection;
  DrawCanvas;
end;

procedure TFormCheqEdit.PaintAt(X, Y: Integer);
var P: TPoint; DX, DY, SX, SY, Error, E: Integer;
begin
  if (X < 0) or (Y < 0) or (X >= FastIMG1.Width) or (Y >= FastIMG1.Height) then Exit;
  P := Point(ScrollHorizontal.Position + X div (FZoom * 8), ScrollVertical.Position + Y div (FZoom * 8));
  if (P.X >= FMap.Width) or (P.Y >= FMap.Height) then Exit;
  if FBrush < 0 then P := Point(ScrollHorizontal.Position * 2 + X div (FZoom * 4),
    ScrollVertical.Position * 2 + Y div (FZoom * 4));
  if FLastPoint.X < 0 then FLastPoint := P;
  DX := Abs(P.X - FLastPoint.X); DY := -Abs(P.Y - FLastPoint.Y);
  if FLastPoint.X < P.X then SX := 1 else SX := -1;
  if FLastPoint.Y < P.Y then SY := 1 else SY := -1;
  Error := DX + DY;
  while True do
  begin
    FMap.Paint(FLastPoint.X, FLastPoint.Y, FBrush, FErasing, PaintButton.Down, FInk, FPaper, FBright, FFlash);
    if (FLastPoint.X = P.X) and (FLastPoint.Y = P.Y) then Break;
    E := Error * 2;
    if E >= DY then begin Error := Error + DY; Inc(FLastPoint.X, SX); end;
    if E <= DX then begin Error := Error + DX; Inc(FLastPoint.Y, SY); end;
  end;
  DrawCanvas;
end;

procedure TFormCheqEdit.FastIMG1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FShuttingDown or (FMap = nil) then Exit;
  if not (Button in [mbLeft, mbRight]) then Exit;
  if (X<0) or (Y<0) or (X>=FastIMG1.Width) or (Y>=FastIMG1.Height) then Exit;
  FinishStroke;
  FAnchor := Point(ScrollHorizontal.Position + X div (FZoom * 8), ScrollVertical.Position + Y div (FZoom * 8));
  if (FAnchor.X >= FMap.Width) or (FAnchor.Y >= FMap.Height) then Exit;
  SetCapture(FastIMG1.Handle);
  if SpeedButton10.Down or (ssShift in Shift) then
  begin
    FAnchor := Point(ScrollHorizontal.Position*2+X div (FZoom*4),
      ScrollVertical.Position*2+Y div (FZoom*4));
    FSelecting := True; UpdateSelection(X, Y);
  end
  else
  begin
    FMap.BeginEdit; FDrawing := True; FErasing := Button = mbRight;
    FLastPoint := Point(-1, -1); PaintAt(X, Y);
  end;
end;

procedure TFormCheqEdit.FastIMG1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var A: Byte;
begin
  if FShuttingDown or (FMap = nil) then Exit;
  FHover := Point(ScrollHorizontal.Position + X div (FZoom * 8), ScrollVertical.Position + Y div (FZoom * 8));
  FHoverPixels := Point(ScrollHorizontal.Position*2+X div (FZoom*4),ScrollVertical.Position*2+Y div (FZoom*4));
  A := FMap.Attributes[FHover.X, FHover.Y];
  HoverInk.Brush.Color := SpectrumColor(A and 7, (A shr 6) and 1);
  HoverPaper.Brush.Color := SpectrumColor((A shr 3) and 7, (A shr 6) and 1);
  if FSelecting then UpdateSelection(X, Y)
  else if FDrawing then
    if not ((ssLeft in Shift) or (ssRight in Shift)) then begin FinishStroke; RefreshEditor; end
    else PaintAt(X, Y);
end;

procedure TFormCheqEdit.FastIMG1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FShuttingDown or (FMap = nil) then Exit;
  if FDrawing then PaintAt(X, Y);
  FinishStroke;
  RefreshEditor;
end;

procedure TFormCheqEdit.ScrollChange(Sender: TObject);
begin
  if not FShuttingDown and Assigned(FMap) then DrawCanvas;
end;

procedure TFormCheqEdit.TimerPsuedoFlashTimer(Sender: TObject);
begin
  if FShuttingDown or (FMap = nil) then Exit;
  FFlashPhase := not FFlashPhase;
  DrawCanvas;
end;

procedure TFormCheqEdit.SetPageColors(Attribute: Byte);
var Answer: Integer;
begin
  Answer := mrNo;
  if not FMap.IsEmpty then
  begin
    Answer := MessageDlg('Apply these page colors to cells using the previous page colors?',
      mtConfirmation, mbYesNoCancel, 0);
    if Answer = mrCancel then Exit;
  end
  else Answer := mrYes;
  FMap.SetPageColors(Attribute, Answer = mrYes);
  SyncExportColors;
end;

procedure TFormCheqEdit.SyncExportColors;
var A: Byte;
begin
  FUpdating := True;
  try
    A := FMap.PageAttribute;
    ExportInkEdit.Text := IntToStr(A and 7);
    ExportPaperEdit.Text := IntToStr((A shr 3) and 7);
    ExportBrightCheck2.Checked := (A and 64) <> 0;
    ExportFlashCheck2.Checked := (A and 128) <> 0;
  finally FUpdating := False; end;
end;

procedure TFormCheqEdit.ShowOverlay(Which: Integer);
begin
  FinishStroke;
  GroupBox1.Visible := Which = 1; GroupBox2.Visible := Which = 2;
  if Which = 1 then
  begin
    GroupBox1.BringToFront; ColorCheck.Checked := FMap.Colored;
    ExportOptionsChange(nil);
  end
  else if Which = 2 then GroupBox2.BringToFront;
  DimensionPanel.Visible := False;
  UpdateLayout;
end;

procedure TFormCheqEdit.CloseOverlayClick(Sender: TObject);
begin
  if Sender = CancelDimensionsButton then DimensionPanel.Visible := False
  else ShowOverlay(0);
  RefreshEditor;
end;

function TFormCheqEdit.ExportOptions: TCheqExportOptions;
var I, P: Integer;
begin
  Result := CheqDefaultExportOptions;
  Result.Start := StrToInt(StartEdit.Text); Result.Step := StrToInt(StepEdit.Text);
  Result.SkipEmpty := SkipEmptyCheck.Checked;
  Result.JoinFull := JoinFullCheck.Checked;
  Result.InitColors := InitColorsCheck.Checked; Result.Compressed := CompressedCheck.Checked;
  Result.CompressionMethod := TCheqCompressionMethod(Max(0, CompressionCombo.ItemIndex));
  if Result.Compressed then Result.CompressionChunkSize := StrToInt(CompressionChunkEdit.Text);
  Result.CompressionDataOnly := CompressionDataOnlyCheck.Checked;
  Result.UseCHR := CHRCheck.Checked; Result.Colored := ColorCheck.Checked;
  if Result.Compressed and (Result.CompressionMethod in [cmLetters,cmGraph,cmGraphCut]) then
    Result.Colored := False;
  Result.ControlColors := ControlColorCheck.Checked;
  Result.Ink := ExportInkCheck.Checked; Result.Paper := ExportPaperCheck.Checked;
  Result.Bright := ExportBrightCheck.Checked; Result.Flash := ExportFlashCheck.Checked;
  Result.SpaceMethod := TCheqSpaceMethod(Max(0, SpaceCombo.ItemIndex));
  I := StrToInt(ExportInkEdit.Text); P := StrToInt(ExportPaperEdit.Text);
  if (I < 0) or (I > 7) or (P < 0) or (P > 7) then raise ERangeError.Create('INK and PAPER must be between 0 and 7.');
  Result.PageAttribute := CheqAttribute(I, P, Ord(ExportBrightCheck2.Checked), Ord(ExportFlashCheck2.Checked));
end;

function TFormCheqEdit.ExportDocument: TCheqMap;
begin
  if ExportSelectionCheck.Checked then
  begin
    if not FHasSelection then raise EConvertError.Create('Select an area on the canvas before exporting the selection.');
    ExpandSelectionToCells;
    Result := FMap.ExtractArea(FSelection);
  end
  else Result := FMap.ExtractArea(Rect(0, 0, FMap.Width, FMap.Height));
end;

procedure TFormCheqEdit.ExportOptionsChange(Sender: TObject);
var ExportWidth: Integer; BasicEnabled, GraphOnly, UsesColor: Boolean; M: TCheqMap;
begin
  if FShuttingDown or FUpdating or (FMap = nil) then Exit;
  FExportLines.Clear;
  CodeMemo.Text := 'Click Generate to preview the current export options.';
  BridgeCombo.Enabled := FMap.Colored;
  if not FMap.Colored then BridgeCombo.ItemIndex := 0;
  ExportSelectionCheck.Enabled := FHasSelection;
  if not FHasSelection then ExportSelectionCheck.Checked := False;
  M := ExportDocument;
  try
    GraphOnly := M.IsGraph;
    if (BridgeCombo.ItemIndex >= 5) and not M.IsGraph then BridgeCombo.ItemIndex := 0;
  finally M.Free; end;
  CompressedCheck.Enabled := GraphOnly;
  if not GraphOnly then CompressedCheck.Checked := False;
  CompressionCombo.Enabled := CompressedCheck.Checked;
  CompressionChunkEdit.Enabled := CompressedCheck.Checked;
  CompressionDataOnlyCheck.Enabled := CompressedCheck.Checked;
  UsesColor := not CompressedCheck.Checked or (CompressionCombo.ItemIndex in [0,4]);
  ColorCheck.Enabled := UsesColor;
  ExportInkCheck.Enabled := UsesColor and ColorCheck.Checked;
  ExportPaperCheck.Enabled := ExportInkCheck.Enabled;
  ExportBrightCheck.Enabled := ExportInkCheck.Enabled;
  ExportFlashCheck.Enabled := ExportInkCheck.Enabled;
  ControlColorCheck.Enabled := ExportInkCheck.Enabled and not CompressedCheck.Checked;
  CHRCheck.Enabled := not CompressedCheck.Checked;
  SpaceCombo.Enabled := not CompressedCheck.Checked;
  JoinFullCheck.Enabled := not CompressedCheck.Checked;
  ExportWidth := FMap.Width;
  if ExportSelectionCheck.Checked and FHasSelection then ExportWidth := FSelection.Right - FSelection.Left;
  BasicEnabled := ExportWidth <= 32;
  GenerateButton.Enabled := BasicEnabled; CopyCodeButton.Enabled := BasicEnabled;
  SendBasicButton.Enabled := BasicEnabled; SaveBasicButton.Enabled := BasicEnabled;
  if not BasicEnabled then CodeMemo.Text := 'This area is wider than 32 cells. Use Bridge, or select an area up to 32 cells wide and enable Export selection.';
end;

function TFormCheqEdit.GenerateCode: Boolean;
var I: Integer; M: TCheqMap;
begin
  Result := False;
  try
    M := ExportDocument;
    try M.GenerateBasic(ExportOptions, FExportLines); finally M.Free; end;
    CodeMemo.Lines.BeginUpdate;
    try
      CodeMemo.Clear;
      for I := 0 to FExportLines.Count - 1 do CodeMemo.Lines.Add(InsertEscapes(FExportLines[I]));
    finally CodeMemo.Lines.EndUpdate; end;
    Result := True;
  except
    on E: Exception do begin FExportLines.Clear; CodeMemo.Text := E.Message; MessageDlg(E.Message, mtError, [mbOK], 0); end;
  end;
end;

procedure TFormCheqEdit.GenerateButtonClick(Sender: TObject);
begin
  GenerateCode;
end;

procedure TFormCheqEdit.CopyCodeButtonClick(Sender: TObject);
begin
  if GenerateCode then Clipboard.AsText := CodeMemo.Text;
end;

procedure TFormCheqEdit.SendBasicButtonClick(Sender: TObject);
var I, Number: Integer; Previous: TEmulationState; Overlap: Boolean;
begin
  if not GenerateCode then Exit;
  if BASinOutput.Running or Registers.EmuRunning then
  begin MessageDlg('Stop the BASIC program before inserting code.', mtInformation, [mbOK], 0); Exit; end;
  Overlap := False;
  for I := 0 to FExportLines.Count - 1 do
  begin
    Number := StrToInt(Copy(FExportLines[I], 1, Pos(' ', FExportLines[I]) - 1));
    if BASinOutput.LineExists(Number) >= 0 then Overlap := True;
  end;
  if Overlap then
    if MessageDlg('Replace existing BASIC lines with the same numbers?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  SaveEmulationState(Previous);
  BASinOutput.AddUndo;
  try
    for I := 0 to FExportLines.Count - 1 do
      if not BASinOutput.AddLine(InsertEscapes(FExportLines[I])) then
        raise EConvertError.CreateFmt('BASIC rejected exported line %d. No code was inserted.', [I + 1]);
    BASinOutput.GetBASIC;
    BASinOutput.RepaintBASIC(True);
    StatusBar1.SimpleText := Format('%d BASIC lines inserted.', [FExportLines.Count]);
  except
    on E: Exception do
    begin
      LoadEmulationState(Previous, True); BASinOutput.GetBASIC; BASinOutput.RepaintBASIC(True);
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TFormCheqEdit.SaveBasicButtonClick(Sender: TObject);
begin
  if not GenerateCode then Exit;
  ExportSaveDialog.DefaultExt := 'bas'; ExportSaveDialog.Filter := 'BASIC text (*.bas)|*.bas';
  if ExecuteFileDialog(ExportSaveDialog) then CodeMemo.Lines.SaveToFile(ExportSaveDialog.FileName);
end;

procedure TFormCheqEdit.RenderBitmap(Bitmap: TBitmap);
var X, Y, PX, PY, C, A, B: Integer; M: TCheqMap;
begin
  M := ExportDocument;
  try
  Bitmap.PixelFormat := pf24bit; Bitmap.Width := M.Width * 8; Bitmap.Height := M.Height * 8;
  for Y := 0 to M.Height - 1 do
    for X := 0 to M.Width - 1 do
    begin
      C := M.Characters[X, Y];
      if M.Colored then A := M.Attributes[X, Y] else A := M.PageAttribute;
      B := (A shr 6) and 1;
      for PY := 0 to 7 do for PX := 0 to 7 do
        if (GlyphRow(C, PY) and (128 shr PX)) <> 0 then
          Bitmap.Canvas.Pixels[X * 8 + PX, Y * 8 + PY] := SpectrumColor(A and 7, B)
        else Bitmap.Canvas.Pixels[X * 8 + PX, Y * 8 + PY] := SpectrumColor((A shr 3) and 7, B);
    end;
  finally M.Free; end;
end;

procedure TFormCheqEdit.SaveImageButtonClick(Sender: TObject);
var Bitmap: TBitmap;
begin
  ExportSaveDialog.DefaultExt := 'bmp'; ExportSaveDialog.Filter := 'Bitmap image (*.bmp)|*.bmp';
  if not ExecuteFileDialog(ExportSaveDialog) then Exit;
  Bitmap := TBitmap.Create;
  try
    try RenderBitmap(Bitmap); Bitmap.SaveToFile(ExportSaveDialog.FileName);
    except on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0); end;
  finally Bitmap.Free; end;
end;

procedure TFormCheqEdit.BridgeButtonClick(Sender: TObject);
var Method: TCheqBridgeMethod; Data: AnsiString; Index, Stride: Integer; M: TCheqMap;
begin
  try
    M := ExportDocument;
    try
    Method := TCheqBridgeMethod(Max(0, BridgeCombo.ItemIndex));
    if not M.Colored then Method := cbCharacters;
    Data := M.BridgeData(Method);
    BinaryWindow.AddBinary('Graph Tile Map ' + IntToStr(M.Width) + 'x' + IntToStr(M.Height) + '.bin', Data);
    Index := BinaryWindow.BinaryFiles.Count - 1;
    if Method in [cbHiResDecimal, cbHiResBinary] then
    begin
      if Method = cbHiResDecimal then Stride := M.Width * 2
      else Stride := (M.Width * 2 + 7) div 8;
      BinaryWindow.Options[Index][1] := IntToStr(Stride);
    end;
    BinaryWindow.Show; BinaryWindow.BringToFront;
    finally M.Free; end;
  except on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0); end;
end;

procedure TFormCheqEdit.BrowseImageButtonClick(Sender: TObject);
begin
  if not ExecuteFileDialog(ImageOpenDialog) then Exit;
  try
    ImportImage.Picture.LoadFromFile(ImageOpenDialog.FileName);
    ImportInfo.Caption := Format('%s (%d x %d). Pixels map directly to GRAPH pixels; larger images are cropped.',
      [ExtractFileName(ImageOpenDialog.FileName), ImportImage.Picture.Width, ImportImage.Picture.Height]);
    ImportNowButton.Enabled := True;
  except on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0); end;
end;

procedure TFormCheqEdit.ImportNowButtonClick(Sender: TObject);
var Bitmap: TBitmap; X, Y, DX, DY, I, J, P, Ink, Bri, Bits, A, C, Best, D, D2: Integer;
    Colors: array[0..3] of TColor; Counts: array[0..15] of Integer;
  function Distance(C1, C2: TColor): Integer;
  begin
    Result := Sqr(Integer(GetRValue(C1)) - GetRValue(C2)) +
      Sqr(Integer(GetGValue(C1)) - GetGValue(C2)) + Sqr(Integer(GetBValue(C1)) - GetBValue(C2));
  end;
begin
  if ImportImage.Picture.Graphic = nil then Exit;
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit; Bitmap.Width := ImportImage.Picture.Width; Bitmap.Height := ImportImage.Picture.Height;
    Bitmap.Canvas.Draw(0, 0, ImportImage.Picture.Graphic);
    FMap.BeginEdit;
    try
      for Y := 0 to FMap.Height - 1 do
        for X := 0 to FMap.Width - 1 do FMap.SetCell(X, Y, 128, FMap.PageAttribute);
      for Y := 0 to FMap.Height - 1 do
        for X := 0 to FMap.Width - 1 do
        begin
          if (X * 2 >= Bitmap.Width) or (Y * 2 >= Bitmap.Height) then Continue;
          FillChar(Counts, SizeOf(Counts), 0);
          for DY := 0 to 1 do for DX := 0 to 1 do
          begin
            I := DY * 2 + DX; Colors[I] := clWhite;
            if (X * 2 + DX < Bitmap.Width) and (Y * 2 + DY < Bitmap.Height) then
              Colors[I] := ColorToRGB(Bitmap.Canvas.Pixels[X * 2 + DX, Y * 2 + DY]);
            Best := MaxInt; J := 0;
            for C := 0 to 15 do
            begin
              D := Distance(Colors[I], SpectrumColor(C and 7, C div 8));
              if D < Best then begin Best := D; J := C; end;
            end;
            Inc(Counts[J]);
          end;
          Bits := 0; A := FMap.PageAttribute;
          if ImportColorCheck.Checked then
          begin
            P := 0;
            for J := 1 to 15 do if Counts[J] > Counts[P] then P := J;
            Ink := P;
            for J := 0 to 15 do
              if (J <> P) and ((Ink = P) or (Counts[J] > Counts[Ink])) then Ink := J;
            Bri := P div 8; P := P and 7; Ink := Ink and 7;
            if Ink = P then if P = 0 then Ink := 7 else Ink := 0;
            if GetRValue(SpectrumColor(P, Bri)) + GetGValue(SpectrumColor(P, Bri)) + GetBValue(SpectrumColor(P, Bri)) >
              GetRValue(SpectrumColor(Ink, Bri)) + GetGValue(SpectrumColor(Ink, Bri)) + GetBValue(SpectrumColor(Ink, Bri)) then
            begin J := P; P := Ink; Ink := J; end;
            A := CheqAttribute(Ink, P, Bri, 0);
            for I := 0 to 3 do
            begin
              D := Distance(Colors[I], SpectrumColor(Ink, Bri)); D2 := Distance(Colors[I], SpectrumColor(P, Bri));
              if D < D2 then Bits := Bits or (1 shl ((I div 2) * 2 + 1 - I mod 2));
            end;
          end
          else for I := 0 to 3 do
            if GetRValue(Colors[I]) < 127 then Bits := Bits or (1 shl ((I div 2) * 2 + 1 - I mod 2));
          FMap.SetCell(X, Y, 128 + Bits, A);
        end;
      FMap.SetColored(ImportColorCheck.Checked);
      FMap.EndEdit;
    except FMap.CancelEdit; raise; end;
    FHasSelection := False; FBrush := -1; GraphButton.Down := True;
    ShowOverlay(0); RefreshEditor;
  finally Bitmap.Free; end;
end;

procedure TFormCheqEdit.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FShuttingDown or (FMap = nil) then Exit;
  if Key = VK_ESCAPE then
  begin
    if FDrawing then begin FMap.CancelEdit; FDrawing := False; end;
    FinishStroke;
    if DimensionPanel.Visible then DimensionPanel.Visible := False
    else if GroupBox1.Visible or GroupBox2.Visible then ShowOverlay(0)
    else begin FHasSelection := False; SpeedButton10.Down := False; end;
    RefreshEditor; Key := 0;
  end;
end;

end.
