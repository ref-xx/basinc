unit Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CheckLst, ExtCtrls, Buttons, FastIMG, FastDIB, FastDraw, FastSize,
  FolderBrowser, FileCtrl, Menus, Math, Utility, Languages,ThemeBevelUnit;

type

  TListlessComboBox = class(TCustomComboBox)
     CaptureControl: TControl;
     Procedure DropDown; Override;
     procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  End;

  TOptionsWindow = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Bevel1: TThemeBevel;
    CheckBox2: TCheckBox;
    Bevel3: TThemeBevel;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label5: TLabel;
    ComboBox2: TComboBox;
    Bevel4: TThemeBevel;
    Bevel5: TThemeBevel;
    Label8: TLabel;
    ComboBox3: TComboBox;
    CheckBox7: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label9: TLabel;
    Bevel6: TThemeBevel;
    CheckBox8: TCheckBox;
    CheckBox11: TCheckBox;
    Label11: TLabel;
    Bevel7: TThemeBevel;
    Label12: TLabel;
    Bevel8: TThemeBevel;
    ComboBox5: TComboBox;
    Label13: TLabel;
    Label14: TLabel;
    ComboBox6: TComboBox;
    CheckBox13: TCheckBox;
    TrackBar1: TTrackBar;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    TrackBar2: TTrackBar;
    Label18: TLabel;
    Bevel2: TThemeBevel;
    Label2: TLabel;
    CheckListBox1: TCheckListBox;
    CheckBox10: TCheckBox;
    CheckBox15: TCheckBox;
    Label19: TLabel;
    ComboBox7: TComboBox;
    Label20: TLabel;
    Bevel9: TThemeBevel;
    TrackBar3: TTrackBar;
    Label21: TLabel;
    Label22: TLabel;
    CheckBox9: TCheckBox;
    Label23: TLabel;
    Bevel10: TThemeBevel;
    CheckBox14: TCheckBox;
    CheckBox16: TCheckBox;
    ComboBox8: TComboBox;
    Label24: TLabel;
    Label25: TLabel;
    ComboBox9: TComboBox;
    Label26: TLabel;
    Bevel11: TThemeBevel;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    Label27: TLabel;
    Bevel12: TThemeBevel;
    CheckBox20: TCheckBox;
    Label29: TLabel;
    Label30: TLabel;
    TrackBar4: TTrackBar;
    Label31: TLabel;
    Bevel13: TThemeBevel;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    Label32: TLabel;
    Bevel14: TThemeBevel;
    TrackBar5: TTrackBar;
    Label33: TLabel;
    Label34: TLabel;
    TabSheet7: TTabSheet;
    Label28: TLabel;
    Bevel15: TThemeBevel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    ComboBox4: TComboBox;
    Label10: TLabel;
    FastIMG1: TFastIMG;
    Label35: TLabel;
    Bevel16: TThemeBevel;
    Label36: TLabel;
    Bevel17: TThemeBevel;
    Label37: TLabel;
    FolderBrowser1: TFolderBrowser;
    TreeView1: TTreeView;
    HelpBtn: TButton;
    Label38: TLabel;
    ComboBox10: TComboBox;
    CheckBox23: TCheckBox;
    Label39: TLabel;
    CheckBox24: TCheckBox;
    Button4: TButton;
    ThemeBevel1: TThemeBevel;
    Label40: TLabel;
    ButtonReset: TButton;
    Label41: TLabel;
    ThemeBevel2: TThemeBevel;
    CheckBox25: TCheckBox;
    Label42: TLabel;
    ThemeBevel3: TThemeBevel;
    ComboBox11: TComboBox;
    Label43: TLabel;
    Button3: TButton;
    CheckBox12: TCheckBox;
    CheckBox26: TCheckBox;
    CheckBox27: TCheckBox;
    Button5: TButton;
    CheckBox28: TCheckBox;
    CheckBox29: TCheckBox;
    CheckBox30: TCheckBox;
    TrackBar6: TTrackBar;
    CheckBox31: TCheckBox;
    ThemeBevel5: TThemeBevel;
    Label45: TLabel;
    CheckBox32: TCheckBox;
    CheckBox33: TCheckBox;
    ComboBox12: TComboBox;
    Label52: TLabel;
    chkOpt_OnlineHelp: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox34: TCheckBox;
    TabSheet8: TTabSheet;
    Edit2: TEdit;
    SpeedButton2: TSpeedButton;
    Label46: TLabel;
    ThemeBevel6: TThemeBevel;
    EditAIQueryURL: TEdit;
    Label47: TLabel;
    Label48: TLabel;
    Edit4: TEdit;
    SpeedButton4: TSpeedButton;
    Label49: TLabel;
    Edit5: TEdit;
    SpeedButton5: TSpeedButton;
    Label50: TLabel;
    TrackBar7: TTrackBar;
    Label51: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    ThemeBevel7: TThemeBevel;
    Label55: TLabel;
    RadioAI1: TRadioButton;
    RadioAI2: TRadioButton;
    EditAPIKEY: TEdit;
    cbAIModelSelect: TComboBox;
    btnAImodelFetch: TButton;
    TrackBar8: TTrackBar;
    Button6: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure BtnFontSelectClick(Sender: TObject);
    Procedure GatherFonts;
    Function  FileLook(Filespec: string; Node: TTreeNode): boolean;
    Procedure RenderFontPreview;
    procedure ComboBox10Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Enter(Sender: TObject);
    procedure TreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ComboBox4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure TrackBar7Change(Sender: TObject);
    procedure btnAImodelFetchClick(Sender: TObject);
    procedure TrackBar8Change(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    ComboBoxL: TListLessComboBox;
    SizingTree: Boolean;
    Function  GetTreeNodeHeight(Node: TTreeNode): Integer;

  public
    { Public declarations }
  end;

var
  OptionsWindow: TOptionsWindow;
  TempAspect, TempIntScale: Boolean;
  InternalFontList: TStringlist;
  FontFilename: String;
  FontEnabled: Boolean;
  IconID : string='000304050607080910111215407302';
  FileExtensions : TStringList;

const
   ISAUnknown = 0;
   ISAClosedFolder = 1;
   ISAOpenFolder = 2;

   CFrameTimeMs: array[0..10] of Integer = (
    1000, //  1 FPS
    200,  //  5 FPS
    83,   // 12 FPS
    42,   // 24 FPS
    19,   // 50 FPS (forced 19 ms)
    17,   // 60 FPS
    14,   // 70 FPS
    13,   // 80 FPS
    10,   // 100 FPS
    4,    // 250 FPS
    1     // 1000 FPS
  );
    CFPSValues: array[0..10] of Integer = (
    1,    // pos 0
    5,    // pos 1
    12,   // pos 2
    24,   // pos 3
    50,   // pos 4
    60,   // pos 5
    70,   // pos 6
    80,   // pos 7
    100,  // pos 8
    250,  // pos 9
    1000  // pos 10
  );


implementation

{$R *.DFM}

Uses ShellAPI,FastCore, Filing, ROMUtils, BasinMain, Display, ColoursWind, Sound, Tapes, LogWind, ConsoleOutput;

function AskGemini(TargetUrl, ApiKey, UserPrompt, SystemPrompt: PChar): PChar; stdcall; external 'BasinAI.dll';
function GetModelList(ApiKey: PChar): PChar; stdcall; external 'BasinAI.dll';

Procedure TListLessComboBox.DropDown;
Begin
  // Nothing Doing!
  If Assigned(OnDropDown) Then
     OnDropDown(Self);
End;

procedure TListLessComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;
  if Message.NotifyCode=CBN_DROPDOWN then begin
     CaptureControl := GetCaptureControl;
     PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
  end;
  if Message.NotifyCode=CBN_CLOSEUP then begin
     SetCaptureControl(CaptureControl);
  End;
end;

procedure TOptionsWindow.FormShow(Sender: TObject);
Var
  i,Ci,Idx: Integer;
begin

  SizingTree := False;

  TempAspect   := Opt_MaintainAspect;
  TempIntScale := Opt_IntegerScaling;

  // Programming Aids

  CheckBox30.Checked:= Opt_Indenting ; //arda
  TrackBar6.Position:= Opt_IndentSize;

  CheckBox1.Checked   := Opt_AutoLoadSession;
  CheckBox2.Checked   := Opt_ShowingSyntax;
  CheckBox3.Checked   := Opt_AutoList;
  CheckBox12.Checked  := Opt_SyntaxHighlight;
  ComboBox1.ItemIndex := Ord(Opt_GraphicsMethod);
  ComboBox7.ItemIndex := Ord(Opt_AutoBracket);
  CheckBox15.Checked  := Opt_Predictive;
  CheckBox24.Checked  := Opt_64Colours;
  CheckBox25.Checked  := Opt_FastResets;
  CheckBox28.Checked  := Opt_KMouse;
  CheckBox29.Checked  := Opt_ConsoleAddon;
  CheckBox31.Checked  := Opt_AllowMultipleInstances;
  //CheckBox35.Checked  := Opt_CheckUpdates;
  chkOpt_OnlineHelp.Checked:=Opt_OnlineHelp;
  CheckBox34.Checked:= Opt_ShowRemCommands;
  
  If Not Opt_OverwriteProtect Then
     ComboBox9.ItemIndex := 0
  Else
     If Opt_ProtectNewOnly Then
        ComboBox9.ItemIndex := 2
     Else
        ComboBox9.ItemIndex := 1;

  // Error Notification

  For Idx := 0 To 43 Do
     CheckListBox1.Checked[Idx] := ErrorAddresses[Idx].Notify;

  CheckBox10.Checked := Opt_CursorToError;

  // Printer Options

  CheckBox21.Checked := Opt_SavePrinting;
  CheckBox22.Checked := Opt_FastPrinting;

    // CPU Speed setting

  Trackbar4.Position := Opt_CPUSpeed;

  // Scaling Options

  ComboBox4.ItemIndex := Opt_FontScale -1;
  CheckBox4.Checked := Opt_MaintainAspect;
  CheckBox5.Checked := Opt_IntegerScaling;
  CheckBox11.Checked := Opt_8BitStretch;
  ComboBox2.ItemIndex := Ord(Opt_RenderMethod);

       Ci:=0;
       for i := 0 to ComboBox12.Items.Count - 1 do
       begin
           if ComboBox12.Items[i] = IntToStr(Opt_ToolFontSize) then
            begin
              Ci := i;
           end;
       end;
       ComboBox12.ItemIndex := Ci;

  ComboBox2Change(Nil);

  // Frameskip

  TrackBar3.Position := Opt_Frameskip;
  CheckBox9.Checked := Opt_AutoFrameSkip;

  // Embellishments

  CheckBox6.Checked := Opt_Scanlines;
  CheckBox8.Checked := Opt_ClipCorners;

  // Snapshots

  ComboBox3.ItemIndex := Opt_z80Version;
  ComboBox10.ItemIndex := Opt_Always128k;

  // .BAS Files
  CheckBox27.Checked := Opt_LoadAutoStart;
  CheckBox26.Checked := Opt_Autostart;
  CheckBox7.Checked := Opt_SavePretty;
  CheckBox33.Checked := Opt_ShowNotes; //arda

  // Tape Images

  CheckBox17.Checked := Opt_TapeTrapLOAD;
  CheckBox18.Checked := Opt_TapeTrapSAVE;
  CheckBox19.Checked := Opt_TapeRewind;

  //AutoBackup
  CheckBox32.Checked := Opt_AutoBackup;
  trackbar8.Position:= opt_AutoBackInterval;


  // Languages

  if Opt_Language ='English'  then Combobox11.Itemindex:=0;
  if Opt_Language ='Türkçe' then   Combobox11.Itemindex:=1;
  if Opt_Language ='Spanish' then   Combobox11.Itemindex:=2;

  // Sound Settings

  Case Opt_SoundFrequency of
     11025: ComboBox5.ItemIndex := 0;
     22050: ComboBox5.ItemIndex := 1;
     44100: ComboBox5.ItemIndex := 2;
  End;

  If Opt_SoundBits = 8 Then
     ComboBox6.ItemIndex := 0
  Else
     ComboBox6.ItemIndex := 1;

  CheckBox13.Checked := Opt_SoundStereo <> 0;

  TrackBar1.Position := Opt_NumSoundBuffers;
  TrackBar2.Position := Opt_SoundLatency;
  TrackBar5.Position := Opt_SoundVolume;

  CheckBox14.Checked := Opt_SoundEnabled;
  ComboBox8.ItemIndex := Ord(Not Opt_KeyClick48k);
  CheckBox16.Checked := Opt_EditorSounds;
  CheckBox23.Checked := Opt_DSoundSynch;

  CheckBox16Click(nil);
  TrackBar1Change(nil);
  TrackBar2Change(nil);
  TrackBar3Change(nil);

  EditAPIKEY.Text := Opt_BYOK_APIKEY;
  cbAIModelSelect.Text := Opt_SelectedAIModel;

  Edit2.Text:= Opt_ExternalExec;

  Edit1.Text := Opt_EditorFontFolder;
  FontFilename := Opt_EditorFontFilename;
  FontEnabled := Opt_EditorCustomFont;
  GatherFonts;

  RenderFontPreview;

  if FileExists(ExtractFilePath(Application.ExeName) + 'pasmo.exe') then Opt_AsmPasmoAvailable:= True;
  if Opt_AsmPasmoAvailable then Edit4.Text:=ExtractFilePath(Application.ExeName) + 'pasmo.exe' Else Edit4.Text:='<pasmo.exe is not found>';

end;

procedure TOptionsWindow.FormCreate(Sender: TObject);
begin

  Button2.SetBounds(ClientWidth - Button2.Width -8, ClientHeight - Button2.Height - 8, Button2.Width, Button2.Height);
  Button1.SetBounds(Button2.Left - Button1.Width - 4, Button2.Top, Button1.Width, Button1.Height);
  PageControl1.SetBounds(8, 8, ClientWidth -16, ClientHeight - Button2.Height - 24);

  ComboBoxL := TListLessComboBox.Create(TabSheet7);
  ComboBoxL.Parent := TabSheet7;
  ComboBoxL.SetBounds(Edit1.Left, Label37.Top + 16, Edit1.Width + SpeedButton1.Width + 4, Edit1.Height);
  ComboBoxL.OnDropDown := Button4Click;
  ComboBoxL.OnKeyDown := ComboBox4KeyDown;
  ComboBoxL.BringToFront;
  ComboBoxL.Visible := True;
  ComboBoxL.Show;

  Combobox11.Items.Add( 'English' );
  Combobox11.Items.Add( 'Tï¿½rkï¿½e');
   Combobox11.Items.Add( 'Spanish');
    Combobox11.Items.Add( 'Deutsch');

  TreeView1.SetBounds(ComboBoxL.Left, ComboBoxL.Top + ComboBoxL.Height + 2, ComboBoxL.Width, TreeView1.Height);
  PageControl1.ActivePage := TabSheet1;


end;

Function TOptionsWindow.GetTreeNodeHeight(Node: TTreeNode): Integer;
Var
  Idx: Integer;
  Rct: TRect;
Begin

  // Calculate the height of a treenode

  Result := 0;
  If Node.HasChildren and Node.Expanded and Node.IsVisible Then Begin
     For Idx := 0 To Node.Count -1 Do
        Result := Result + GetTreeNodeHeight(Node.Item[Idx]);
  End Else Begin
     Rct := Node.DisplayRect(False);
     Result := Rct.Bottom - Rct.Top;
  End;

End;

procedure TOptionsWindow.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TOptionsWindow.Button1Click(Sender: TObject);
Var
  Idx: Integer;
  fh: THandle;
begin

  // Programming Aids



  Opt_AutoLoadSession := CheckBox1.Checked;
  Opt_ShowingSyntax := CheckBox2.Checked;
  Opt_AutoList:= CheckBox3.Checked;
  Opt_AutoBracket := TBracketMethod(ComboBox7.ItemIndex);
  Opt_SyntaxHighlight := CheckBox12.Checked;
  Opt_GraphicsMethod := TGraphicsMethod(ComboBox1.ItemIndex);
  Opt_Predictive := CheckBox15.Checked;
  Opt_64Colours := CheckBox24.Checked;
  Opt_OnlineHelp:= chkOpt_OnlineHelp.Checked;

    Opt_AllowMultipleInstances := CheckBox31.Checked;  //arda
    Opt_ConsoleAddon := CheckBox29.Checked;    //arda
    if (not Opt_ConsoleAddon)then ConsoleOutForm.Close;
    Opt_Indenting:=  CheckBox30.Checked; // arda flist
    Opt_IndentSize:= TrackBar6.Position; //arda
    Opt_FastResets := CheckBox25.Checked; //arda
    Opt_KMouse := CheckBox28.Checked;     //arda
    Opt_ShowNotes := CheckBox33.Checked; //arda
    //Opt_CheckUpdates := CheckBox35.Checked;   //arda --removed 1.8
    Opt_ShowRemCommands:= CheckBox34.Checked; //arda 1.81

  Opt_BYOK_APIKEY := Trim(EditAPIKEY.Text);
  if cbAIModelSelect.ItemIndex >= 0 then
    Opt_SelectedAIModel := cbAIModelSelect.Items[cbAIModelSelect.ItemIndex]
  else
    Opt_SelectedAIModel := Trim(cbAIModelSelect.Text);
    
  // CPU Speed options

  Opt_CPUSpeed := TrackBar4.Position;
  SpeedBackup := Opt_CPUSpeed; //used for returning back to this value after Rem Speed x execution
  Opt_EmulationSpeed := CFrameTimeMs[TrackBar7.Position];


  Case ComboBox9.ItemIndex Of
     0: Begin
           Opt_OverwriteProtect := False;
           Opt_ProtectNewOnly := False;
        End;
     1: Begin
           Opt_OverwriteProtect := True;
           Opt_ProtectNewOnly := False;
        End;
     2: Begin
           Opt_OverwriteProtect := True;
           Opt_ProtectNewOnly := True;
        End;
  End;

  // Error Notification

  For Idx := 0 To 43 Do
     ErrorAddresses[Idx].Notify := CheckListBox1.Checked[Idx];

  Opt_CursorToError := CheckBox10.Checked;

  // Printer Options

  Opt_FastPrinting := Checkbox22.Checked;
  Opt_SavePrinting := Checkbox21.Checked;

  // Scaling Options

  Opt_FontScale := ComboBox4.ItemIndex +1;
  Opt_MaintainAspect := TempAspect;
  Opt_IntegerScaling := TempIntScale;
  Opt_RenderMethod := TRenderMethod(ComboBox2.ItemIndex);
  Opt_8BitStretch := CheckBox11.Checked;

  // FrameSkip

  Opt_FrameSkip := TrackBar3.Position;
  Opt_AutoFrameSkip := CheckBox9.Checked;

  // Embellishments

  Opt_Scanlines := CheckBox6.Checked;
  Opt_ClipCorners := CheckBox8.Checked;

  // Snapshots

  Opt_z80Version := ComboBox3.ItemIndex;
  Opt_Always128k := ComboBox10.ItemIndex;

  // .BAS Files

  Opt_LoadAutoStart := CheckBox27.Checked;
  Opt_Autostart := CheckBox26.Checked;
  Opt_SavePretty := CheckBox7.Checked;

  // Tape Images

  Opt_TapeTrapLOAD := CheckBox17.Checked;
  Opt_TapeTrapSAVE := CheckBox18.Checked;
  Opt_TapeRewind := CheckBox19.Checked;

  //autobackup
  Opt_AutoBackup := CheckBox32.Checked;
  Opt_AutoBackInterval:=trackbar8.Position;

  basinoutput.Timer3.Interval:=(1+(Opt_AutoBackInterval*3))*60000;
  basinoutput.Timer3.enabled:= Opt_AutoBackup;

  // Sound Settings

  Case ComboBox5.ItemIndex of
     0: Opt_SoundFrequency := 11025;
     1: Opt_SoundFrequency := 22050;
     2: Opt_SoundFrequency := 44100;
  End;

  //Languages
  Opt_Language:=ComboBox11.items[ ComboBox11.ItemIndex];
  SetLanguage(Opt_Language);

  //multiple instances    arda 1.69+

  //following is a dirty hack but it saves us some waiting
  //this dummy file will be checked at startup
  //because using iniread on basin.dpr causes a crash

  if Opt_AllowMultipleInstances then begin

    fh:=CreateFileW(PWideChar(WideString((basindir + '\allowinstance.opt'))), GENERIC_READ,FILE_SHARE_READ,nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL,0);
    CloseHandle(fh);
  end else begin
    DeleteFile(basindir + '\allowinstance.opt');
  end;

  //end arda
  
  If ComboBox6.ItemIndex = 0 Then
     Opt_SoundBits := 8
  Else
     Opt_SoundBits := 16;

  Opt_SoundStereo := Ord(CheckBox13.Checked);

  Opt_NumSoundBuffers := TrackBar1.Position;
  Opt_SoundLatency := TrackBar2.Position;
  Opt_SoundVolume := TrackBar5.Position;
  Opt_DSoundSynch := CheckBox23.Checked;

  Opt_SoundEnabled := CheckBox14.Checked;
  SoundAvailable := Opt_SoundEnabled; // not sure if this would be here
  if (Opt_EmulationSpeed=19) then Begin
   if Opt_SoundEnabled Then SoundAvailable:=True;
   End Else SoundAvailable:=False;   // if speed is not 19, then sound is muted

  Opt_KeyClick48k := ComboBox8.ItemIndex = 0;
  Opt_EditorSounds := CheckBox16.Checked;

  Opt_EditorFontFilename := FontFilename;
  Opt_EditorCustomFont := Integer(Treeview1.Selected.Data) <> 0;
  Opt_EditorFontFolder := Edit1.Text;
  Opt_ExternalExec:=Edit2.Text;

  if ComboBox12.ItemIndex=0 Then Opt_ToolFontSize:=0 else Opt_ToolFontSize:=StrToInt(ComboBox12.Items[ComboBox12.ItemIndex]) ;

  AlterSoundSettings;
  ResetSound;
  DisplayWindow.InitScaleDIBs;
  DisplayWindow.FormResize(Nil);
  BASinOutput.RepaintBASIC(True);
  Close;

end;

procedure TOptionsWindow.ComboBox2Change(Sender: TObject);
begin
  If ComboBox2.ItemIndex >= 2 Then Begin
     If ComboBox2.Tag < 2 Then Begin
        TempAspect := CheckBox4.Checked;
        TempIntScale := CheckBox5.Checked;
        CheckBox4.Checked := True;
        CheckBox5.Checked := True;
        CheckBox4.Enabled := False;
        CheckBox5.Enabled := False;
     End;
  End Else Begin
     CheckBox4.Checked := TempAspect;
     CheckBox5.Checked := TempIntScale;
     CheckBox4.Enabled := True;
     CheckBox5.Enabled := True;
    CheckBox11.Enabled := ComboBox2.ItemIndex = 0;
  End;
  ComboBox2.Tag := ComboBox2.ItemIndex;
end;

procedure TOptionsWindow.CheckBox4Click(Sender: TObject);
begin
  TempAspect := CheckBox4.Checked;
end;

procedure TOptionsWindow.CheckBox5Click(Sender: TObject);
begin
  TempIntScale := CheckBox5.Checked;
end;

procedure TOptionsWindow.Button3Click(Sender: TObject);
begin
  CentreForm(ColoursWindow, Left + Width, Top + Height);
  ShowWindow(ColoursWindow, True);
end;

procedure TOptionsWindow.TrackBar1Change(Sender: TObject);
begin
  If TrackBar1.Position <> 1 Then
     Label16.Caption := IntToStr(TrackBar1.Position) + ' Frames'
  Else
     Label16.Caption := IntToStr(TrackBar1.Position) + ' Frame';
end;

procedure TOptionsWindow.TrackBar6Change(Sender: TObject);
begin
  If TrackBar6.Position <> 1 Then
     CheckBox30.Caption := 'Indent ' + IntToStr(TrackBar6.Position) + ' Spaces'
  Else
     CheckBox30.Caption := 'Indent ' + IntToStr(TrackBar6.Position) + ' Space';
end;

procedure TOptionsWindow.TrackBar2Change(Sender: TObject);
begin
  If TrackBar2.Position <> 1 Then
     Label18.Caption := IntToStr(TrackBar2.Position) + ' Frames'
  Else
     Label18.Caption := IntToStr(TrackBar2.Position) + ' Frame';
end;

procedure TOptionsWindow.TrackBar3Change(Sender: TObject);
begin
  If TrackBar3.Position <> 1 Then
     Label21.Caption := IntToStr(TrackBar3.Position) + ' Frames'
  Else
     Label21.Caption := IntToStr(TrackBar3.Position) + ' Frame';
end;

procedure TOptionsWindow.CheckBox16Click(Sender: TObject);
begin
  Label24.Enabled := Checkbox16.Checked;
  ComboBox8.Enabled := CheckBox16.Checked;
end;

procedure TOptionsWindow.BtnFontSelectClick(Sender: TObject);
begin

  FolderBrowser1.Title := 'Browse for Fonts folder';
  FolderBrowser1.Folder := Edit1.Text;
  If FolderBrowser1.Execute Then Begin
     Edit1.Text := FolderBrowser1.Folder;
     GatherFonts;
  End;

end;

Procedure TOptionsWindow.GatherFonts;
Begin

  TreeView1.Items.BeginUpdate;

  TreeView1.Items.Clear;
  InternalFontList.Clear;

  TreeView1.Items.Add(Nil, 'Default Sinclair');
  TreeView1.Items[0].Data := 0;
  InternalFontList.Add('Default Sinclair');
  FileLook(Edit1.Text + '\' + '*.*', nil);
  If (ComboBoxL.Text = '') or (ComboBoxL.Text = 'Default Sinclair') Then Begin
     ComboBoxL.Text := 'Default Sinclair';
     TreeView1.Selected := TreeView1.Items[0];
  End;
  TreeView1.Items.EndUpdate;
  RenderFontPreview;

End;

Function TOptionsWindow.FileLook(Filespec: string; Node: TTreeNode): boolean;
var
  TempNode, ParentNode: TTreeNode;
  validres, CodeLength: integer;
  SearchRec: TSearchRec;
  DirPath, FullName, Flname, Ext, Filename: string;
  FStream: TFilestream;
begin

  DirPath := ExtractFilePath(FileSpec);
  Result := DirectoryExists(DirPath);
  If not Result then exit;
  Flname := ExtractFileName(FileSpec);
  validres := FindFirst(FileSpec, faAnyFile, SearchRec);

  while validres = 0 do begin

     If (SearchRec.Name[1] <> '.') then begin {not a dotted directory}
	      FullName:=DirPath + LowerCase(SearchRec.Name);
        {add folder/file as child of current Node}
        If SearchRec.Attr and faDirectory = 0 Then Begin
           // This is a file, so get it's attributes, and test to see if we want to include it.
           Filename := FullName;
           Ext := Lowercase(ExtractFileExt(Filename));

           FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
           CodeLength := FStream.Size;
           If Ext = '.bsc' Then
              CodeLength := CodeLength - 17;

           FStream.Free;

           If CodeLength = 768 Then Begin

              InternalFontList.Add(Filename);
              TempNode := TreeView1.Items.AddChild(Node, SearchRec.Name);
              TempNode.Data := pointer(InternalFontList.Count -1);
              ParentNode := TempNode;
              If lowercase(FullName) = Lowercase(FontFilename) Then Begin

                 Repeat

                    ParentNode := ParentNode.Parent;
                    If ParentNode <> nil Then
                       ParentNode.Expand(False);

                 Until ParentNode = nil;

                 ComboBoxL.Text := TempNode.Text;
                 TempNode.MakeVisible;
                 TempNode.Selected := True;
                 TreeView1.Selected := TempNode;

              End;

           End;

        End Else Begin // Is a directory

           TempNode := TreeView1.Items.AddChild(Node, SearchRec.Name);
           TempNode.ImageIndex := ISAClosedFolder;
           TempNode.SelectedIndex := ISAOpenFolder;
           TempNode.Data := Pointer(0);

           FileLook(FullName+'\'+Flname, TempNode);

        end;
     end;

     validres:=FindNext(SearchRec);  {continue scanning current
                                      folder for files and other folders}
  end;
end;



Procedure TOptionsWindow.RenderFontPreview;
Var
  X, Y, Idx: Integer;
  TempDIB: TFastDIB;
Begin

  FastIMG1.Bmp.SetSize(242, 150, 32);
  FastIMG1.Bmp.Clear(TFSpecWhite);

  TempDIB := TFastDIB.Create;
  TempDIB.SetSize(8, 8, 32);

  For X := 0 To 241 Do Begin
     FastIMG1.Bmp.Pixels32[0, X] := DisplayPalette[0];
     FastIMG1.Bmp.Pixels32[149, X] := DisplayPalette[0];
  End;

  For Y := 0 To 149 Do Begin
     FastIMG1.Bmp.Pixels32[Y, 0] := DisplayPalette[0];
     FastIMG1.Bmp.Pixels32[Y, 241] := DisplayPalette[0];
  End;

  X := 2; Y := 3;
  For Idx := 32 To 127 Do Begin

     TempDIB.Clear(TFSpecWhite);
     SpecTextToDIB(TempDIB, 0, 1, Chr(Idx), 0, -1, 0, False, False);
     TempDIB.Stretch(FastIMG1.Bmp.hDc, X * 2, (Y * 2)-2, 16, 16);

     Inc(X, 9);
     If X > (FastIMG1.Bmp.Width DIv 2) -8 Then Begin
        X := 3;
        Inc(Y, 9);
     End;

  End;

  TempDIB.Free;
  FastIMG1.Repaint;

End;

procedure TOptionsWindow.ComboBox10Click(Sender: TObject);
begin

  If (Sender = nil ) or Not SizingTree Then
     If TreeView1.Selected <> nil Then
        If Not TreeView1.Selected.HasChildren Then Begin
           FontFilename := InternalFontList[Integer(TreeView1.Selected.Data)];
           LoadEditorFont(Handle, FontFilename, Integer(TreeView1.Selected.Data) <> 0);
           ComboBoxL.Text := TreeView1.Selected.Text;
           RenderFontPreview;
           TreeView1.Hide;
           ReleaseCapture;
        End;

  SizingTree := False;

end;

procedure TOptionsWindow.Button4Click(Sender: TObject);
Var
  Idx, Ht: Integer;
  Rct: TRect;
begin

  Ht := TreeView1.Height - TreeView1.ClientHeight;
  For Idx := 0 To TreeView1.Items.Count -1 Do
     If TreeView1.Items[Idx].IsVisible Then Begin
        Rct := TreeView1.Items[Idx].DisplayRect(True);
        Ht := Ht + Rct.Bottom - Rct.Top;
     End;

  Ht := Min(Ht, TabSheet7.ClientHeight - 16 - TreeView1.Top);

  TreeView1.SetBounds(ComboBoxL.Left, ComboBoxL.Top + ComboBoxL.Height + 2, ComboBoxL.Width, Ht);

  TreeView1.Visible := True;
  TreeView1.Selected.MakeVisible;
  TreeView1.SetFocus;
  SetCaptureControl(TreeView1);
  SizingTree := False;

end;

procedure TOptionsWindow.TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  XResult: integer;
  XP: TPoint;
begin
  Windows.GetCursorPos(XP);
  XResult:=SendMessage(TreeView1.Handle, WM_NCHITTEST, 0, integer(smallpoint(XP.X, XP.Y)));
  if (GetCapture=TreeView1.Handle) and (XResult in [HTVSCROLL, HTHSCROLL]) then begin
     ReleaseCapture;
     mouse_event(MOUSEEVENTF_LEFTDOWN, XP.X, XP.Y, 0, 0);
     Application.ProcessMessages;
     SetCapture(TreeView1.Handle);
  end else
     if XResult<>HTCLIENT then begin
        ReleaseCapture;
        TreeView1.Visible:=false;
     end else
        inherited MouseDown(Button, Shift, X, Y);
end;

procedure TOptionsWindow.FormDestroy(Sender: TObject);
begin

  ComboBoxL.Free;

end;

procedure TOptionsWindow.TreeView1Enter(Sender: TObject);
begin

  SetCaptureControl(TreeView1);

end;

procedure TOptionsWindow.TreeView1Expanded(Sender: TObject; Node: TTreeNode);
Var
  Idx, Ht: Integer;
  Rct: TRect;
begin

  Ht := TreeView1.Height - TreeView1.ClientHeight;
  For Idx := 0 To TreeView1.Items.Count -1 Do
     If TreeView1.Items[Idx].IsVisible Then Begin
        Rct := TreeView1.Items[Idx].DisplayRect(True);
        Ht := Ht + Rct.Bottom - Rct.Top;
     End;

  Ht := Min(Ht, TabSheet7.ClientHeight - 16 - TreeView1.Top);
  SizingTree := True;
  TreeView1.SetBounds(ComboBoxL.Left, ComboBoxL.Top + ComboBoxL.Height + 2, ComboBoxL.Width, Ht);

end;

procedure TOptionsWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LoadEditorFont(Handle, Opt_EditorFontFilename, Opt_EditorCustomFont);
end;

procedure TOptionsWindow.TreeView1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SizingTree := False;
end;

procedure TOptionsWindow.ComboBox4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  PostMessage(TreeView1.Handle, WM_KEYDOWN, Key, 0);
  ComboBox10Click(nil);
end;

procedure TOptionsWindow.HelpBtnClick(Sender: TObject);
begin

  Case PageControl1.ActivePageIndex of
     0: BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_basin.html'), HH_DISPLAY_TOPIC, 0);
     1: BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_editor_fonts.html'), HH_DISPLAY_TOPIC, 0);
     2: BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_error_reporting.html'), HH_DISPLAY_TOPIC, 0);
     3: BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_emulation.html'), HH_DISPLAY_TOPIC, 0);
     4: BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_display.html'), HH_DISPLAY_TOPIC, 0);
     5: BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_sound.html'), HH_DISPLAY_TOPIC, 0);
     6: BasinOutput.HtmlHelpOnline(Application.Handle, PChar(BASinDir+'\BASin.chm::/topics/options_files.html'), HH_DISPLAY_TOPIC, 0);
  End;


end;

procedure TOptionsWindow.TrackBar4Change(Sender: TObject);
begin
     Label30.Caption := FloatToStrF(TrackBar4.Position * 50 / 1000000, ffFixed, 18, 2)+' mHz';
  Label39.Caption := IntToStr(TrackBar4.Position) + ' Ts/Frame';
end;

procedure TOptionsWindow.ButtonResetClick(Sender: TObject);
begin
     TrackBar4.Position := 69888;
     TrackBar7.Position := 4;
end;

procedure TOptionsWindow.Button5Click(Sender: TObject);
begin
TrackBar4.Position := 69888*2;
end;

procedure TOptionsWindow.SpeedButton2Click(Sender: TObject);
var Filename: String;
begin
// point an emulator
     Filename := OpenFile(Handle, 'Please select an executable file', [FTExec], '', False, False);
     If Filename = '' Then
        Exit;
     Edit2.Text := Filename;
end;

procedure TOptionsWindow.SpeedButton3Click(Sender: TObject);
begin
     Filename := OpenFile(Handle, 'Please select zxbc.exe file', [FTExec], 'zxbc.exe', False, False);
     if not SameText(ExtractFileName(Filename), 'zxbc.exe') then
        Exit;
     //Edit3.Text := ExtractFilePath(Filename);
end;

procedure TOptionsWindow.SpeedButton4Click(Sender: TObject);
begin
     Filename := OpenFile(Handle, 'Please select pasmo.exe file', [FTExec], 'pasmo.exe', False, False);
     if not SameText(ExtractFileName(Filename), 'pasmo.exe') then
        Exit;
     Edit4.Text := ExtractFilePath(Filename);
end;

procedure TOptionsWindow.SpeedButton5Click(Sender: TObject);
begin
     Filename := OpenFile(Handle, 'Please select an text editor executable file', [FTExec], '', False, False);
     If Filename = '' Then
        Exit;
     Edit5.Text := Filename;
end;

procedure TOptionsWindow.TrackBar7Change(Sender: TObject);
var
  fps: Integer;
begin
  fps := CFPSValues[TrackBar7.Position];

  if fps = 50 then
    Label53.Caption := 'Sound Available'
  else if fps=1000 then
    Label53.Caption := 'Fast PC?!'
  else
    Label53.Caption := 'Sound Disabled';

  // FPS label
  Label51.Caption := IntToStr(fps) + ' Fps';
end;


procedure TOptionsWindow.btnAImodelFetchClick(Sender: TObject);
var
  ApiKey: String;
  RawList: PChar;
  ListStr: String;
  TempList: TStringList;
begin
  ApiKey := Trim(EditAPIKEY.Text);
  if ApiKey = '' then
  begin
    ShowMessage('Please enter your API key.');
    Exit;
  end;

  cbAIModelSelect.Clear;
  cbAIModelSelect.Text := 'Loading models...';
  Application.ProcessMessages;

  try
    RawList := GetModelList(PChar(ApiKey));

    if RawList = nil then
      ListStr := 'Error: No answer'
    else
      ListStr := StrPas(RawList);

    if (Pos('Error:', ListStr) = 1) or (Trim(ListStr) = '') then
    begin
      ShowMessage('Hata: ' + ListStr);
      cbAIModelSelect.Text := '';
      Exit;
    end;

    TempList := TStringList.Create;
    try
      TempList.Text := ListStr;
      cbAIModelSelect.Items.Assign(TempList);

      if cbAIModelSelect.Items.Count > 0 then
        cbAIModelSelect.ItemIndex := 0
      else
        cbAIModelSelect.Text := 'Model not found';
    finally
      TempList.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('DLL error: ' + E.Message);
      cbAIModelSelect.Text := '';
    end;
  end;
end;


procedure TOptionsWindow.TrackBar8Change(Sender: TObject);
begin

    Checkbox32.Caption:='Backup as .BAS every '+inttostr(1+(trackbar8.Position*3)) +' minutes';
    end;


procedure TOptionsWindow.Button6Click(Sender: TObject);
begin

 ShellExecute(0, 'open', PChar(BasinOutput.GetAutoBackupDir), nil, nil, SW_SHOWNORMAL);
end;



Initialization

  InternalFontList := TStringlist.Create;

Finalization

  InternalFontList.Free;

end.

