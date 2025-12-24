unit BasinCTips;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GraphicEx, Utility,
  ComCtrls;

type
  TTipItem = class
  public
    ImageFile: string;
    FontSize: Integer;
    GroupName: string;
    GroupIndex: Integer;
    Text: string;
    OrderIndex: Integer;
  end;

  TFormBasinCTips = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Panel1: TPanel;
    btnNext: TButton;
    btnWhatsNew: TButton;
    btnClose: TButton;
    btnPrev: TButton;
    ChkShowTips: TCheckBox;
    Memo1: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnWhatsNewClick(Sender: TObject);
    procedure ChkShowTipsClick(Sender: TObject);
  private
    FTips: TList;
    FRootTips: TList;
    FLoaded: Boolean;
    FCurrentTip: TTipItem;
    FResumeRootAfterNews: TTipItem;
    FDefaultMemoFontSize: Integer;

    procedure EnsureLoaded;
    procedure LoadTips;
    procedure ClearTips;

    function TipsFilePath: string;
    function TipsImagePath(const ImageName: string): string;

    function FindTip(const GroupName: string; GroupIndex: Integer): TTipItem;
    function FirstTipInGroup(const GroupName: string): TTipItem;
    function RootTipFor(ATip: TTipItem): TTipItem;
    function NextRootTipAfter(ARoot: TTipItem): TTipItem;
    function PrevRootTipBefore(ARoot: TTipItem): TTipItem;

    procedure DisplayTip(ATip: TTipItem);
    procedure DisplayTipText(const S: string);
    procedure LoadTipImage(const ImageFile: string);
    procedure UpdateButtons;

    procedure ShowRandomRootTip;
    procedure ShowNextTip;
    procedure ShowPrevTip;
    procedure ShowWhatsNew;

  public
    procedure ShowOnStartupIfEnabled;
  end;

var
  FormBasinCTips: TFormBasinCTips;

implementation

{$R *.dfm}

const
  CTipSettingsSection = 'Tips';
  CTipShowOnStartupKey = 'ShowTipsOnStartup';
  CTipsFileName = 'basinctips.ini';
  CTipsFolderName = 'tips';
  CNewsGroupName = 'news';

function StartsWith(const S, Prefix: string): Boolean;
begin
  Result := (Length(S) >= Length(Prefix)) and (Copy(S, 1, Length(Prefix)) = Prefix);
end;

function IsAbsolutePath(const S: string): Boolean;
begin
  Result := (Pos(':\', S) = 2) or StartsWith(S, '\\') or StartsWith(S, '/');
end;

function ReplaceBR(const S: string): string;
begin
  Result := StringReplace(S, '[br]', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
end;

function SplitTipLine(const Line: string; out Img, FontSz, Group, Idx, Text: string): Boolean;
var
  i, CommaCount, LastPos: Integer;
  Parts: array[0..4] of string;
begin
  Result := False;
  Img := '';
  FontSz := '';
  Group := '';
  Idx := '';
  Text := '';

  CommaCount := 0;
  LastPos := 1;
  for i := 1 to Length(Line) do
  begin
    if Line[i] = ',' then
    begin
      if CommaCount <= 3 then
      begin
        Parts[CommaCount] := Trim(Copy(Line, LastPos, i - LastPos));
        Inc(CommaCount);
        LastPos := i + 1;
      end
      else
        Break;
    end;
  end;

  if CommaCount < 4 then
    Exit;

  Parts[4] := Copy(Line, LastPos, MaxInt);

  Img := Parts[0];
  FontSz := Parts[1];
  Group := Parts[2];
  Idx := Parts[3];
  Text := Parts[4];
  Result := True;
end;


procedure TFormBasinCTips.FormCreate(Sender: TObject);
begin
  Randomize;

  FTips := TList.Create;
  FRootTips := TList.Create;
  FLoaded := False;
  FCurrentTip := nil;
  FResumeRootAfterNews := nil;

  Memo1.ReadOnly := True;
  Memo1.WordWrap := True;
  Memo1.ScrollBars := ssVertical;
  
  FDefaultMemoFontSize := Memo1.Font.Size;

  Image1.Center := True;
  Image1.Proportional := True;
  Image1.Stretch := True;

  ChkShowTips.Checked := Opt_EnableBasincTips;

  EnsureLoaded;
  UpdateButtons;
end;

procedure TFormBasinCTips.FormDestroy(Sender: TObject);
begin
  ClearTips;
  FreeAndNil(FTips);
  FreeAndNil(FRootTips);
end;

procedure TFormBasinCTips.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Persist user choice into global option; actual INI write happens in SaveOptions
  Opt_EnableBasincTips := ChkShowTips.Checked;
end;

procedure TFormBasinCTips.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormBasinCTips.btnNextClick(Sender: TObject);
begin
  ShowNextTip;
end;

procedure TFormBasinCTips.btnPrevClick(Sender: TObject);
begin
  ShowPrevTip;
end;

procedure TFormBasinCTips.btnWhatsNewClick(Sender: TObject);
begin
  ShowWhatsNew;
end;

procedure TFormBasinCTips.ChkShowTipsClick(Sender: TObject);
begin
  Opt_EnableBasincTips := ChkShowTips.Checked;
  //WriteSettingBool(CTipSettingsSection, CTipShowOnStartupKey, ChkShowTips.Checked);
end;

procedure TFormBasinCTips.ShowOnStartupIfEnabled;
begin
  // Sync checkbox with current option before evaluating visibility
  ChkShowTips.Checked := Opt_EnableBasincTips;

  // Already visible (e.g. user opened it before the startup timer fired) -> do nothing to avoid modal error
  if Visible then
    Exit;

  if not ChkShowTips.Checked then
    Exit;

  EnsureLoaded;
  if FRootTips.Count = 0 then
    Exit;

  ShowRandomRootTip;
  FormStyle := fsStayOnTop;   // Ensure it is above all windows
  ShowModal;
  FormStyle := fsNormal;      // Restore after closing
end;

procedure TFormBasinCTips.EnsureLoaded;
begin
  if FLoaded then
    Exit;
  LoadTips;
  FLoaded := True;
end;

procedure TFormBasinCTips.ClearTips;
var
  i: Integer;
begin
  for i := 0 to FTips.Count - 1 do
    TObject(FTips[i]).Free;
  FTips.Clear;
  FRootTips.Clear;
  FCurrentTip := nil;
  FResumeRootAfterNews := nil;
end;

function TFormBasinCTips.TipsFilePath: string;
begin
  Result := IncludeTrailingPathDelimiter(BASinDir) + CTipsFileName;
end;

function TFormBasinCTips.TipsImagePath(const ImageName: string): string;
begin
  if Trim(ImageName) = '' then Begin Result:=''; Exit; End;


  if IsAbsolutePath(ImageName) then
    Result := ImageName
  else
    Result := IncludeTrailingPathDelimiter(BASinDir) + CTipsFolderName + '\' + ImageName;
end;

procedure TFormBasinCTips.LoadTips;
var
  Lines: TStringList;
  i, FontSize, GroupIndex: Integer;
  Line, Img, FontSz, Group, Idx, Text: string;
  Tip: TTipItem;
begin
  ClearTips;

  Lines := TStringList.Create;
  try
    if FileExists(TipsFilePath) then
      Lines.LoadFromFile(TipsFilePath)
    else
    begin
      Lines.Add('; Auto-generated default tips file (missing basinctips.ini)');
      Lines.Add('BasinCMini.png,12,nav,0,Welcome to BasinC![br][br]We cannot found the tips file. Please check your installation.\\.');
      try
        Lines.SaveToFile(TipsFilePath);
      except
        // ignore (read-only install or permissions)
      end;
    end;

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line = '' then
        Continue;
      if StartsWith(Line, ';') or StartsWith(Line, '#') or StartsWith(Line, '//') then
        Continue;

      if not SplitTipLine(Line, Img, FontSz, Group, Idx, Text) then
        Continue;

      FontSize := StrToIntDef(Trim(FontSz), 0);
      if FontSize <= 0 then
        FontSize := FDefaultMemoFontSize;

      GroupIndex := StrToIntDef(Trim(Idx), 0);

      Tip := TTipItem.Create;
      Tip.ImageFile := Trim(Img);
      Tip.FontSize := FontSize;
      Tip.GroupName := LowerCase(Trim(Group));
      Tip.GroupIndex := GroupIndex;
      Tip.Text := Text;
      Tip.OrderIndex := i;
      FTips.Add(Tip);
    end;

    for i := 0 to FTips.Count - 1 do
    begin
      Tip := TTipItem(FTips[i]);
      if (Tip.GroupIndex = 0) and (not SameText(Tip.GroupName, CNewsGroupName)) then
        FRootTips.Add(Tip);
    end;
  finally
    Lines.Free;
  end;
end;

function TFormBasinCTips.FindTip(const GroupName: string; GroupIndex: Integer): TTipItem;
var
  i: Integer;
  Tip: TTipItem;
begin
  Result := nil;
  for i := 0 to FTips.Count - 1 do
  begin
    Tip := TTipItem(FTips[i]);
    if SameText(Tip.GroupName, GroupName) and (Tip.GroupIndex = GroupIndex) then
     Begin Result:=Tip; Exit; End;
  end;
end;

function TFormBasinCTips.FirstTipInGroup(const GroupName: string): TTipItem;
begin
  Result := FindTip(GroupName, 0);
end;

function TFormBasinCTips.RootTipFor(ATip: TTipItem): TTipItem;
begin
  if ATip = nil then begin
    Result:=Nil;
    Exit;
  End;
  Result := FindTip(ATip.GroupName, 0);
  if Result = nil then
    Result := ATip;
end;

function TFormBasinCTips.NextRootTipAfter(ARoot: TTipItem): TTipItem;
var
  i, RootIndex: Integer;
begin
  Result := nil;
  if FRootTips.Count = 0 then
    Exit;

  if ARoot = nil then Begin Result:=TTipItem(FRootTips[0]); Exit; End;

  RootIndex := -1;
  for i := 0 to FRootTips.Count - 1 do
    if TTipItem(FRootTips[i]) = ARoot then
    begin
      RootIndex := i;
      Break;
    end;

  if RootIndex < 0 then Begin Result:=TTipItem(FRootTips[0]); Exit; End;

  Inc(RootIndex);
  if RootIndex >= FRootTips.Count then
    RootIndex := 0;
  Result := TTipItem(FRootTips[RootIndex]);
end;

function TFormBasinCTips.PrevRootTipBefore(ARoot: TTipItem): TTipItem;
var
  i, RootIndex: Integer;
begin
  Result := nil;
  if FRootTips.Count = 0 then
    Exit;

  // If we don't know current root, go to last one by default
  if ARoot = nil then
  begin
    Result := TTipItem(FRootTips[FRootTips.Count - 1]);
    Exit;
  end;

  RootIndex := -1;
  for i := 0 to FRootTips.Count - 1 do
    if TTipItem(FRootTips[i]) = ARoot then
    begin
      RootIndex := i;
      Break;
    end;

  if RootIndex < 0 then
  begin
    Result := TTipItem(FRootTips[0]);
    Exit;
  end;

  Dec(RootIndex);
  if RootIndex < 0 then
    RootIndex := FRootTips.Count - 1;
  Result := TTipItem(FRootTips[RootIndex]);
end;

procedure TFormBasinCTips.LoadTipImage(const ImageFile: string);
var
  Path: string;
begin
  Image1.Picture.Assign(nil);
  Path := TipsImagePath(ImageFile);
  if (Path <> '') and FileExists(Path) then
  begin
    try
      Image1.Picture.LoadFromFile(Path);
    except
      Image1.Picture.Assign(nil);
    end;
  end;
end;

procedure TFormBasinCTips.DisplayTipText(const S: string);
begin
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Lines.Text := ReplaceBR(S);
    Memo1.SelStart := 0;
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TFormBasinCTips.DisplayTip(ATip: TTipItem);
begin
  if ATip = nil then
    Exit;

  FCurrentTip := ATip;

  if SameText(ATip.GroupName, CNewsGroupName) then
    Label1.Caption := 'What'#39's New'
  else
    Label1.Caption := 'BasinC Tip of the Day';

  Memo1.Font.Size := ATip.FontSize;
  LoadTipImage(ATip.ImageFile);
  DisplayTipText(ATip.Text);

  UpdateButtons;
end;

procedure TFormBasinCTips.UpdateButtons;
var
  HasNews: Boolean;
begin
  btnNext.Enabled := (FRootTips.Count > 0);
  btnPrev.Enabled := (FCurrentTip <> nil) and (FRootTips.Count > 0);

  HasNews := (FirstTipInGroup(CNewsGroupName) <> nil);
  btnWhatsNew.Enabled := HasNews;
end;

procedure TFormBasinCTips.ShowRandomRootTip;
var
  Index: Integer;
begin
  EnsureLoaded;
  if FRootTips.Count = 0 then
    Exit;

  Index := Random(FRootTips.Count);
  DisplayTip(TTipItem(FRootTips[Index]));
end;

procedure TFormBasinCTips.ShowPrevTip;
var
  Root: TTipItem;
  BaseRoot: TTipItem;
begin
  if FCurrentTip = nil then
    Exit;

  // Always jump to the previous tip group's root (index 0)
  if SameText(FCurrentTip.GroupName, CNewsGroupName) and (FResumeRootAfterNews <> nil) then
    BaseRoot := RootTipFor(FResumeRootAfterNews)
  else
    BaseRoot := RootTipFor(FCurrentTip);

  Root := PrevRootTipBefore(BaseRoot);
  if Root <> nil then
    DisplayTip(Root);


end;

procedure TFormBasinCTips.ShowNextTip;
var
  NextTip: TTipItem;
  Root: TTipItem;
  NextIndex: Integer;
begin
  EnsureLoaded;

  if FRootTips.Count = 0 then
    Exit;

  if FCurrentTip = nil then
  begin
    DisplayTip(TTipItem(FRootTips[0]));
    Exit;
  end;

  if SameText(FCurrentTip.GroupName, CNewsGroupName) then
  begin
    NextIndex := FCurrentTip.GroupIndex + 1;
    NextTip := FindTip(CNewsGroupName, NextIndex);
    if NextTip <> nil then
    begin
      DisplayTip(NextTip);
      Exit;
    end;

    Root := FResumeRootAfterNews;
    FResumeRootAfterNews := nil;
    DisplayTip(NextRootTipAfter(Root));
    Exit;
  end;

  NextIndex := FCurrentTip.GroupIndex + 1;
  NextTip := FindTip(FCurrentTip.GroupName, NextIndex);
  if NextTip <> nil then
  begin
    DisplayTip(NextTip);
    Exit;
  end;

  Root := RootTipFor(FCurrentTip);
  DisplayTip(NextRootTipAfter(Root));
end;

procedure TFormBasinCTips.ShowWhatsNew;
var
  News: TTipItem;
begin
  EnsureLoaded;

  News := FirstTipInGroup(CNewsGroupName);
  if News = nil then
  begin
    MessageDlg('No '#39'news'#39' tips found in ' + CTipsFileName + '.', mtInformation, [mbOK], 0);
    Exit;
  end;

  if (FCurrentTip <> nil) and (not SameText(FCurrentTip.GroupName, CNewsGroupName)) then
    FResumeRootAfterNews := RootTipFor(FCurrentTip)
  else if FResumeRootAfterNews = nil then
    FResumeRootAfterNews := nil;

  DisplayTip(News);
end;

function FindSectionLine(Lines: TStringList; const Section: string): Integer;
var
  i: Integer;
  Target: string;
begin
  Result := -1;
  Target := '[' + LowerCase(Section) + ']';
  for i := 0 to Lines.Count - 1 do
    if LowerCase(Trim(Lines[i])) = Target then Begin Result:=i; Exit; End;
end;

function FindKeyLineInSection(Lines: TStringList; SectionLine: Integer; const Key: string): Integer;
var
  i: Integer;
  L, Prefix: string;
begin
  Result := -1;
  Prefix := LowerCase(Key) + '=';

  i := SectionLine + 1;
  while i < Lines.Count do
  begin
    L := Trim(Lines[i]);
    if (L <> '') and (L[1] = '[') then
      Break;
    if LowerCase(Copy(L, 1, Length(Prefix))) = Prefix then Begin result:=i; Exit; End;
    Inc(i);
  end;
end;


end.
