unit Sound;

interface

Uses Windows, Classes, MMSystem, DirectSound, Math;

Type

  TAYRegisters  = Record  R0, R1, R2, R3, R4, R5, R6, R7,
                          R8, R9, R10, R11, R12, R13, R14, R15,
                          NoiseOutput, EnvVolume, WhiteNoiseCounter,
                          InternalR6, EnvMode, EnvelopeClock,
                          ChAOutput, ChBOutput, ChCOutput: Byte;

                          ChACounter, ChBCounter, ChCCounter,
                          FinalChanA, FinalChanB, FinalChanC,
                          EnvCounter: Word;

                          AccChanA, AccChanB, AccChanC,
                          RandomSeed1, RandomSeed2:    DWord;
                  End;

  TShadowAYRegisters =
                  Record  R0, R1, R2, R3, R4, R5, R6, R7,
                          R8, R9, R10, R11, R12, R13, R14, R15: Byte;
                  End;

Var

  EarBit:           DWord;
  SoundTs:          DWord;
  SoundAvailable:   Boolean;
  SoundBUfferSize:  DWord;
  SubBufferSize:    DWord;
  NumSubBuffers:    DWord;
  CurSubBuffer:     DWord;
  WriteBufferPos:   DWord;
  DXBufferPos:      DWord;
  SampleTS:         DWord;
  DSoundCaps:       TDSCaps;
  LastSample:       Word;
  SampleCount:      DWord;
  PrivateBuffer:    Array[0..262144] of Byte;

  {DXSound Objects}

  WaveFormat:       TWaveFormatEx;
  DSObject:         IDirectSound = nil;
  BufferDesc:       TDSBufferDesc;
  PrimaryBuffer,
  SecondaryBuffer:  IDirectSoundBuffer;

  {Sample Streams}

  MS_48kClick,
  MS_128kClick,
  MS_Error,
  MS_OK:            TMemoryStream;

  {AY Objects}

  AYVolumes:        Array[0..15] of Word;
  AYRegisters:      TAYRegisters;
  ShadowAYRegs:     TShadowAYRegisters;
  AYRegSelected:    Byte;
  AYTs:             DWord;
  AYSampleTs:       DWord;

  {Non-DSound synch vars}

  BASinFrequency,
  LastSynchError,
  LastSynchPos,
  SynchPos,
  NoDSoundBufferLen: Int64;

  Procedure         INITSound;
  Procedure         CheckSoundDeviceCaps;
  Procedure         AlterSoundSettings;
  Function          SetFormat(Hz, Bits: DWord; Stereo, Primary: Boolean): HResult;
  Procedure         CloseSound;
  Procedure         ClearSoundBuffers;
  Procedure         BufferSoundSample;
  Procedure         WriteBuffer;
  Procedure         ResetSound;

  Procedure         CreateEditorSounds;
  procedure         MakeSound(SoundType: Integer);

  Procedure         UpdateAY;
  Procedure         UpdateAYState;
  Procedure         CreateSample;

Const

  env_DECAY  = 0;
  env_ATTACK = 1;
  env_OFF    = 2;
  env_HOLD   = 3;

  AYBaseVolumes: Array[0..15] of Word = (   0,  108,  159,  223,  335,  511,  703, 1119,
                                         1343, 2143, 2943, 3679, 4655, 5759, 6911, 8191);
  AYRealVolumes: Array[0..15] of Word = (   0,   86,  126,  177,  257,  378,  520,  869,
                                         1080, 1772, 2435, 3201, 4201, 5218, 6705, 8191);
  AYSpecVolumes: Array[0..15] of Word = (   0,  112,  168,  238,  346,  506,  694,  1121,
                                         1385, 2168, 2889, 3685, 5672, 5630, 6948, 8191);

implementation

Uses PrinterOutput, BASinMain, FastCore, Utility;

Procedure INITSound;
Var
  DXError: HResult;
  F: Integer;
Begin

  QueryPerformanceFrequency(BASinFrequency);
  If BASinFrequency = 0 Then Opt_DSoundSynch := True;

  For F := 0 To 15 Do
     AYVolumes[F] := Round(AYSpecVolumes[F]*0.9765625);

  AYRegSelected := 0;
  FillChar(AYRegisters, 16, 0);

  SoundAvailable := False;

  //If Not Opt_SoundEnabled Then Exit;

  DXError := DirectSoundCreate(Nil, DSObject, Nil);

  If DXError = DS_OK Then Begin

     DXError := DSObject.SetCooperativeLevel(BASinOutput.Handle, DSSCL_PRIORITY);

     If DXError = DS_OK Then Begin

        CheckSoundDeviceCaps;

        BufferDesc.dwSize := SizeOf(TDSBufferDesc);
        BufferDesc.dwFlags := DSBCAPS_PRIMARYBUFFER or DSBCAPS_GETCURRENTPOSITION2;

        DXError := DSObject.CreateSoundBuffer(BufferDesc, PrimaryBuffer, Nil);

        If DXError = DS_OK Then Begin

           SoundAvailable := True;
           AlterSoundSettings;

        End Else Begin

           MessageBox(BASinOutput.Handle, pChar('DirectSound could not create the Primary Buffer.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);

        End;

     End Else Begin

        MessageBox(BASinOutput.Handle, pChar('DirectSound could not set the cooperative level.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);

     End;

  End Else Begin

     MessageBox(BASinOutput.Handle, pChar('DirectSound could not be initialised.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);
     Opt_SoundEnabled:=False;
  End;

  LastSample := 0;
  SampleCount := 0;

End;

Procedure CheckSoundDeviceCaps;
Begin

  If Not SoundAvailable Then Exit;

  DSoundCaps.dwSize := SizeOf(TDSCaps);
  DSObject.GetCaps(DSoundCaps);

  If 11025 > Opt_SoundFrequency Then
     Opt_SoundFrequency := 11025;

  If DSoundCaps.dwMaxSecondarySampleRate < Opt_SoundFrequency Then
     Opt_SoundFrequency := DSoundCaps.dwMaxSecondarySampleRate;

  If (Opt_SoundBits = 16) and Not (DSoundCaps.dwFlags and DSCAPS_Secondary16Bit <> 0) Then
     If DSoundCaps.dwFlags or DSCAPS_Secondary8Bit <> 0 Then
        Opt_SoundBits := 8
     Else Begin
        MessageBox(BASinOutput.Handle, pChar('DirectSound does not support 8 or 16 bit Sound.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);
        Exit;
     End;

  If (Opt_SoundBits = 8) and Not (DSoundCaps.dwFlags or DSCAPS_Secondary8bit <> 0) Then
     If DSoundCaps.dwFlags or DSCAPS_Secondary16Bit <> 0 Then
        Opt_SoundBits := 16
     Else Begin
        MessageBox(BASinOutput.Handle, pChar('DirectSound does not support 8 or 16 bit sound.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);
        Exit;
     End;

  If (Opt_SoundStereo <> 0) and Not (DSoundCaps.dwFlags or DSCAPS_SecondaryStereo <> 0) Then
     If DSoundCaps.dwFlags or DSCAPS_SecondaryMono <> 0 Then
        Opt_SoundStereo := 0
     Else Begin
        MessageBox(BASinOutput.Handle, pChar('DirectSound does not support mono or stereo sound.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);
        Exit;
     End;

  If (Opt_SoundStereo = 0) and Not (DSoundCaps.dwFlags or DSCAPS_SecondaryMono <> 0) Then
     If DSoundCaps.dwFlags or DSCAPS_SecondaryStereo <> 0 Then
        Opt_SoundStereo := 1
     Else Begin
        MessageBox(BASinOutput.Handle, pChar('DirectSound does not support mono or stereo sound.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);
        Exit;
     End;

End;

Procedure AlterSoundSettings;
Var
  DXError: HResult;
Begin

  If SoundAvailable Then Begin

     SoundAvailable := False;

     DXError := SetFormat(Opt_SoundFrequency, Opt_SoundBits, Opt_SoundStereo <> 0, True);

     If DXError = DS_OK Then Begin

        DXError := SetFormat(Opt_SoundFrequency, Opt_SoundBits, Opt_SoundStereo <> 0, False);

        If DXError = DS_OK Then Begin

           DXError := SecondaryBuffer.Play(0, 0, DSBPLAY_LOOPING);

           If DXError = DS_OK Then Begin

              SoundAvailable := True;
              ResetSound;
              PrimaryBuffer.Play(0, 0, DSBPLAY_LOOPING);

           End Else Begin

              MessageBox(BASinOutput.Handle, pChar('DirectSound could not start playing.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);

           End;

        End Else Begin

           MessageBox(BASinOutput.Handle, pChar('DirectSound could not create the Secondary Buffer.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);

        End;

     End Else Begin

        MessageBox(BASinOutput.Handle, pChar('DirectSound could not set the Primary Buffer format.'#13'Sound is unavailable.'), pChar('DirectSound Error'), MB_OK or MB_ICONWARNING);

     End;

  End Else

     If DSObject = nil Then

        If Opt_SoundEnabled Then Begin

           InitSound;
           AlterSoundSettings;

        End;

End;

Function SetFormat(Hz, Bits: DWord; Stereo, Primary: Boolean): HResult;
Begin

  With WaveFormat Do begin

     wFormatTag := WAVE_FORMAT_PCM;
     If Stereo Then
        nChannels := 2
     Else
        nChannels := 1;
     nSamplesPerSec := Hz;
     wBitsPerSample := Bits;
     nBlockAlign := wBitsPerSample * nChannels div 8;
     nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
     cbSize := 0;

  End;

  If Primary Then Begin

     Result := PrimaryBuffer.SetFormat(WaveFormat);

  End Else Begin

     SampleTs := Round((Opt_CPUSpeed*50)/Hz);
     AYSampleTs := 16;
     SubBufferSize := Round(Opt_CPUSpeed/SampleTs);

     If Opt_SoundStereo <> 0 Then
        SubBufferSize := SubBufferSize *2;

     If Opt_SoundBits = 16 Then
        SubBufferSize := SubBufferSize *2;

     NumSubBuffers := Opt_NumSoundBuffers;
     SoundBufferSize :=  SubBufferSize * NumSubBuffers;

     BufferDesc.dwFlags := DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_GLOBALFOCUS;
     BufferDesc.dwBufferBytes := SoundBufferSize;
     BufferDesc.lpwfxFormat := @WaveFormat;

     SecondaryBuffer := nil;
     Result := DSObject.CreateSoundBuffer(BufferDesc, SecondaryBuffer, Nil);

     NoDSoundBufferLen := Round(SubBufferSize * (BASinFrequency / (hz * (Opt_SoundBits Div 8) * WaveFormat.nChannels)));

     //(SubBufferSize * (BASinFrequency Div Hz) Div (2 * WaveFormat.nChannels));

  End;

End;

Procedure CloseSound;
Begin

  If SecondaryBuffer <> Nil Then
     SecondaryBuffer := Nil;

  If PrimaryBuffer <> Nil Then
     PrimaryBuffer := Nil;

  If DSObject <> Nil then
     DSObject := Nil;

End;

Procedure ClearSoundBuffers;
Var
  Silence: Word;
  BytesLocked1, BytesLocked2: DWord;
  Pointer1, Pointer2: Pointer;
  DXError: HResult;

Begin

  If Not SoundAvailable or (SecondaryBuffer = nil) Then Exit;

  Silence := GetWord(@PrivateBuffer[0]);

  DXError := SecondaryBuffer.Lock(0, SoundBufferSize, Pointer1, BytesLocked1, Pointer2, BytesLocked2, 0);

  If DXError = DS_OK Then Begin

     BytesLocked1 := BytesLocked1 Shr 2;

     asm
        push eax
        push ecx
        push edi
        mov  ax,  Silence
        mov  edi, [Pointer1]
        shl  eax, 16
        mov  ecx, BytesLocked1
        mov  ax,  Silence

     @Loop:
        mov  [edi], eax
        add  edi, 4
        dec  ecx
        jnz  @loop

        pop  edi
        pop  ecx
        pop  eax
     end;

     SecondaryBuffer.Unlock(Pointer1, BytesLocked1, Pointer2, BytesLocked2);

     If DXError <> DS_OK Then Begin

        MessageBox(BASinOutput.Handle, pChar('DirectSound Could not unlock the Secondary Buffer.'#13'Sound may be corrupted.'), pChar('DirectSound Error'),  MB_OK or MB_ICONWARNING);

     End;

  End Else Begin

     MessageBox(BASinOutput.Handle, pChar('DirectSound Could not lock the Secondary Buffer.'#13'Sound is now unavailable.'), pChar('DirectSound Error'),  MB_OK or MB_ICONWARNING);

     SoundAvailable := False;

  End;

  LastSynchPos := 0;

End;

Procedure ResetSound;
Begin
  If SoundAvailable Then
     If SecondaryBuffer <> nil Then Begin
        ClearSoundBuffers;
        CurSubBuffer := Opt_SoundLatency;
        WriteBufferPos := 0;
        SoundTs := 0;
        AYTs := 0;
     End;
  CreateEditorSounds;
  QueryPerformanceCounter(LastSynchPos);
  LastSynchError := 0;
End;

Procedure BufferSoundSample;
Var
  Sample: Word;
Begin

  If Not SoundAvailable Then Exit;

  Dec(SoundTs, SampleTs);

  Sample := 0;

  If Opt_SoundEnabled Then Begin

     CreateSample;
     Sample := Round(((EarBit * 8192) + AYRegisters.AccChanA + AYRegisters.AccChanB + AYRegisters.AccChanC) * (Opt_SoundVolume/30000));
     Sample := Min(Opt_SoundVolume, Sample);

     If Opt_SoundBits = 8 Then
        Sample := Sample Shr 8;

  End;

  If Opt_SoundStereo <> 0 Then Begin

     If Opt_SoundBits = 16 Then Begin

        PrivateBuffer[WriteBufferPos] := Sample and 255;
        PrivateBuffer[WriteBufferPos +1] := Sample shr 8;
        PrivateBuffer[WriteBufferPos +2] := Sample and 255;
        PrivateBuffer[WriteBufferPos +3] := Sample shr 8;
        Inc(WriteBufferPos, 4);

     End Else Begin

        PrivateBuffer[WriteBufferPos] := Sample and 255;
        PrivateBuffer[WriteBufferPos +1] := Sample and 255;
        Inc(WriteBufferPos, 2);

     End;

  End Else Begin

     If Opt_SoundBits = 16 Then Begin

        PrivateBuffer[WriteBufferPos] := Sample and 255;
        PrivateBuffer[WriteBufferPos +1] := Sample shr 8;
        Inc(WriteBufferPos, 2);

     End Else Begin

        PrivateBuffer[WriteBufferPos] := Sample and 255;
        Inc(WriteBufferPos);

     End;

  End;

  If WriteBufferPos >= SubBufferSize Then
     WriteBuffer;

End;

Procedure WriteBuffer;
Var
  DXError: HResult;
  BytesLocked1, BytesLocked2: DWord;
  Pointer1, Pointer2: Pointer;
  BufferPtr: PByte;
  SecondaryPos, Buffer, BufferNow, SynchBuffer: DWord;
Begin

  If Not SoundAvailable Then Exit;

  Inc(CurSubBuffer);
  If CurSubBuffer >= NumSubBuffers Then
     CurSubBuffer := 0;

  SynchBuffer := (CurSubBuffer + NumSubBuffers - (Opt_SoundLatency -1)) mod NumSubBuffers;

  DXError := SecondaryBuffer.Lock(CurSubBuffer * SubBufferSize, SubBufferSize, Pointer1, BytesLocked1, Pointer2, BytesLocked2, 0);

  If DXError = DS_OK Then Begin

     BufferPtr := @PrivateBuffer[0];
     CopyMemory(Pointer1, BufferPtr, BytesLocked1);
     Inc(BufferPtr, BytesLocked1);
     CopyMemory(Pointer2, BufferPtr, BytesLocked2);
     SecondaryBuffer.Unlock(Pointer1, BytesLocked1, Pointer2, BytesLocked2);

     WriteBufferPos := 0;
     Buffer := (SynchBuffer +(Opt_SoundLatency -1)) mod NumSubBuffers;

     Repeat

        If Opt_DSoundSynch Then Begin

           SecondaryBuffer.GetCurrentPosition(nil, @SecondaryPos);
           BufferNow := SecondaryPos Div SubBufferSize;

           If Buffer < SynchBuffer then begin

              If (BufferNow >= SynchBuffer) or (BufferNow <= Buffer) Then
                 Break;

           End Else Begin

              If (BufferNow = SynchBuffer) and (BufferNow <= Buffer) Then
                 Break;

           End;

           Sleep(1);

        End Else Begin

           QueryPerformanceCounter(SynchPos);

           If (SynchPos - LastSynchPos) >= NoDSoundBufferLen Then Begin
              LastSynchPos := SynchPos;
              Break;
           End;

           Sleep(1);

        End;

      Until Registers.EmuRunning = False;

  End Else Begin

  End;

End;

procedure MakeSound(SoundType: Integer); 
Begin
  sndPlaySound(nil, 0);
  Case SoundType of
     1:
        Begin
           If Opt_KeyClick48k Then
              sndPlaySound(MS_48kClick.Memory, SND_MEMORY or SND_ASYNC)
           Else
              sndPlaySound(MS_128kClick.Memory, SND_MEMORY or SND_ASYNC);
        End;
     2:
        Begin
           sndPlaySound(MS_Ok.Memory, SND_MEMORY or SND_ASYNC);
        End;
     3:
        Begin
           sndPlaySound(MS_Error.Memory, SND_MEMORY or SND_ASYNC);
        End;
  End;
end;

procedure CreateEditorSounds;
var
  WaveFormatEx : TWaveFormatEx;
  idx, rpt, TempInt, DataCount, RiffCount, SoundType: integer;
  SoundValue, SilenceValue: Word;
  MS: TMemoryStream;
const
  Mono: Word = $0001;
  RiffId : string = 'RIFF';
  WaveId : string = 'WAVE';
  FmtId : string = 'fmt ';
  DataId : string = 'data';
begin

  { Key click is 300x(76, 29) }
  { OK  sound is  80x(76, 29),  80x(0, 0) repeated 50 times}
  { Err Sound is 326x(76, 29), 326x(0, 0) repeated 32 times}

  With WaveFormatEx Do Begin

     wFormatTag := WAVE_FORMAT_PCM;
     nChannels := 2;
     nSamplesPerSec := 44100;
     wBitsPerSample := 16;
     nBlockAlign := (nChannels * wBitsPerSample) div 8;
     nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
     cbSize := 0;

  End;

  For SoundType := 0 To 3 Do Begin

     MS := TMemoryStream.Create;

     With MS Do Begin

        Seek(0, soFromBeginning);
        MS.Size := 0;

        {Calculate length of sound data and of file data}
        If SoundType = 0 Then DataCount := 600;
        If SoundType = 1 Then DataCount := 600;
        If SoundType = 2 Then DataCount := 100*80*2;
        If SoundType = 3 Then DataCount := 50*326*2;

        RiffCount := Length(WaveId)
                   + Length(FmtId) + SizeOf(DWord) + SizeOf(TWaveFormatEx)
                   + Length(DataId) + SizeOf(DWord) + DataCount; // file data

        {write out the wave header}
        Write(RiffId[1], 4);                            // 'RIFF'
        Write(RiffCount, SizeOf(DWord));                // file data size
        Write(WaveId[1], Length(WaveId));               // 'WAVE'
        Write(FmtId[1], Length(FmtId));                 // 'fmt '
        TempInt := SizeOf(TWaveFormatEx);
        Write(TempInt, SizeOf(DWord));                  // TWaveFormat data size
        Write(WaveFormatEx, SizeOf(TWaveFormatEx));     // WaveFormatEx record
        Write(DataId[1], Length(DataId));               // 'data'
        Write(DataCount, SizeOf(DWord));                // sound data size

        {calculate and write out the tone signal}       // now the data values
        SoundValue := Opt_SoundVolume;
        SilenceValue := 0;

        Case SoundType of
           0: Begin // 48k Keyclick
                 For Idx := 0 To 24 Do
                    Write(SoundValue, SizeOf(Word));
                 For Idx := 25 To 299 Do
                    Write(SilenceValue, SizeOf(Word));
                 MS_48kClick := MS;
              End;
           1: Begin // 128k Keyclick
                 For Idx := 0 To 299 Do
                    Write(SoundValue, SizeOf(Word));
                 MS_128kClick := MS;
              End;
           2: Begin // Ok
                 For rpt := 0 To 99 Do Begin
                    For Idx := 0 To 79 Do
                       Write(SoundValue, SizeOf(Word));
                    For Idx := 0 To 79 Do
                       Write(SilenceValue, SizeOf(Word));
                 End;
                 MS_Ok := MS;
              End;
           3: Begin // Error
                 For rpt := 0 To 50 Do Begin
                    For Idx := 0 To 325 Do
                       Write(SoundValue, SizeOf(Word));
                    For Idx := 0 To 325 Do
                       Write(SilenceValue, SizeOf(Word));
                 End;
                 MS_Error := MS;
              End;
        end;
     End;
  End;

End;

// AY Procedures

Procedure UpdateAYState;
Begin
  Dec(AYTs, AYSampleTs);
  UpdateAY;
End;

procedure UpdateAY;
Asm
            push esi
            push ebx

            lea  esi,AYRegisters
            xor  [esi].TAYRegisters.EnvelopeClock, 1
            je   @NoEnvelope

            inc  [esi].TAYRegisters.EnvCounter

            // we only handle the envelope clock if a channel is using it
            test dword ptr [esi].TAYRegisters.R8, $101010
            je   @NoEnvelope

            // reached the envelope period?
            mov  ax, Word Ptr [esi].TAYRegisters.R11
            cmp  [esi].TAYRegisters.EnvCounter,ax
            jc   @NoEnvelope

            // call the envelope handler
            // let's first bring the vector table into the data cache
            // whilst evaluating the correct vector offset
            mov  ebx, dword [@ExecEnvVectors]

            mov  al, [esi].TAYRegisters.R13
            mov  [esi].TAYRegisters.EnvCounter, 0
            and  eax, 15
            mov  eax, dword [@ExecEnvVectors+(eax*4)]
            call eax

@NoEnvelope:
            inc [esi].TAYRegisters.ChACounter
            mov ax, Word Ptr [esi].TAYRegisters.R0
            cmp [esi].TAYRegisters.ChACounter,ax
            jc  @HandleChB
@ChA_Reset: mov [esi].TAYRegisters.ChACounter,0
            xor [esi].TAYRegisters.ChAOutput,1

@HandleChB: inc [esi].TAYRegisters.ChBCounter
            mov ax, Word Ptr [esi].TAYRegisters.R2
            cmp [esi].TAYRegisters.ChBCounter,ax
            jc  @HandleChC
@ChB_Reset: mov [esi].TAYRegisters.ChBCounter,0
            xor [esi].TAYRegisters.ChBOutput,1

@HandleChC: inc [esi].TAYRegisters.ChCCounter
            mov ax, Word Ptr [esi].TAYRegisters.R4
            cmp [esi].TAYRegisters.ChCCounter,ax
            jc  @HandleWhiteNoise
@ChC_Reset: mov [esi].TAYRegisters.ChCCounter,0
            xor [esi].TAYRegisters.ChCOutput,1

@HandleWhiteNoise:
            inc [esi].TAYRegisters.WhiteNoiseCounter
            mov al,[esi].TAYRegisters.InternalR6
            cmp [esi].TAYRegisters.WhiteNoiseCounter,al
            jnc @WN_Reset

            pop ebx
            pop esi
            ret

@WN_Reset:  mov al,[esi].TAYRegisters.R6
            mov [esi].TAYRegisters.WhiteNoiseCounter,0
            and al,31
            jne @NewNoise
            inc al
@NewNoise:  mov [esi].TAYRegisters.InternalR6,al

            mov  eax, [esi].TAYRegisters.RandomSeed1
            mov  ebx, eax
            mov  ecx, eax
            mov  edx, [esi].TAYRegisters.RandomSeed2
            shr  ebx, 15
            shr  ecx, 13
            and  ebx, 1
            and  ecx, 1
            and  edx, 1
            xor  edx, ebx
            xor  edx, ecx
            shr  edx, 1
            rcr  [esi].TAYRegisters.RandomSeed1,1
            mov  al, 0
            adc  al, 0
            rcr  [esi].TAYRegisters.RandomSeed2,1
            mov  [esi].TAYRegisters.NoiseOutput,al

            pop  ebx
            pop  esi
            ret

@ExecEnvVectors:
            dd  @ExecEnv0,  @ExecEnv0,  @ExecEnv0,  @ExecEnv0
            dd  @ExecEnv4,  @ExecEnv4,  @ExecEnv4,  @ExecEnv4
            dd  @ExecEnv8,  @ExecEnv9,  @ExecEnv10, @ExecEnv11
            dd  @ExecEnv12, @ExecEnv13, @ExecEnv14, @ExecEnv15

@ExecEnv0:  cmp [esi].TAYRegisters.EnvMode, env_OFF
            je  @Env_Exit
            cmp [esi].TAYRegisters.EnvVolume,0
            je  @Set_OFF
            dec [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv4:  cmp [esi].TAYRegisters.EnvMode, env_OFF
            je  @Env_Exit
            cmp [esi].TAYRegisters.EnvVolume,15
            je  @Set_OFF
            inc [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv8:  cmp [esi].TAYRegisters.EnvVolume,0
            je  @Set_DECAY
            dec [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv9:  cmp [esi].TAYRegisters.EnvMode, env_OFF
            je  @Env_Exit
            cmp [esi].TAYRegisters.EnvVolume,0
            je  @Set_OFF
            dec [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv10: cmp [esi].TAYRegisters.EnvMode, env_DECAY
		       jne @Env10_1

            cmp [esi].TAYRegisters.EnvVolume,0
		       je  @Set_ATTACK
		       dec [esi].TAYRegisters.EnvVolume
		       ret

@Env10_1:   cmp [esi].TAYRegisters.EnvVolume,15
            je  @Set_DECAY
            inc [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv11: cmp [esi].TAYRegisters.EnvMode, env_HOLD
            je  @Env_Exit
            cmp [esi].TAYRegisters.EnvVolume,0
            je  @Set_HOLD
            dec [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv12: cmp [esi].TAYRegisters.EnvVolume,15
            je  @Set_ATTACK
            inc [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv13: cmp [esi].TAYRegisters.EnvMode, env_HOLD
            je  @Env_Exit
            cmp [esi].TAYRegisters.EnvVolume,15
            je  @Set_HOLD
            inc [esi].TAYRegisters.EnvVolume
            ret

@ExecEnv14: cmp [esi].TAYRegisters.EnvMode, env_ATTACK
		       jne @Env14_1

      	    cmp [esi].TAYRegisters.EnvVolume,15
            je  @Set_DECAY
	          inc [esi].TAYRegisters.EnvVolume
		       ret

@Env14_1:   cmp [esi].TAYRegisters.EnvVolume,0
		       je  @Set_ATTACK
            dec [esi].TAYRegisters.EnvVolume
		       ret

@ExecEnv15: cmp [esi].TAYRegisters.EnvMode, env_OFF
            je  @Env_Exit
            cmp [esi].TAYRegisters.EnvVolume,15
            je  @Set_OFF
            inc [esi].TAYRegisters.EnvVolume
@Env_Exit:  ret

@Set_OFF:    mov [esi].TAYRegisters.EnvMode, env_OFF
             mov [esi].TAYRegisters.EnvVolume,0
             ret
@Set_ATTACK: mov [esi].TAYRegisters.EnvMode, env_ATTACK
             mov [esi].TAYRegisters.EnvVolume,0
             ret
@Set_DECAY:  mov [esi].TAYRegisters.EnvMode, env_DECAY
             mov [esi].TAYRegisters.EnvVolume,15
             ret
@Set_HOLD:   mov [esi].TAYRegisters.EnvMode, env_HOLD
             mov [esi].TAYRegisters.EnvVolume,15
             ret
End;

Procedure CreateSample;
Asm
              push  esi
              push  edx
              push  ebx
              lea   esi, AYRegisters
              mov   ah, [esi].TAYRegisters.R7
              mov   cl, [esi].TAYRegisters.R8
              mov   al, [esi].TAYRegisters.ChAOutput
              call  @MixOutput
              mov   [esi].TAYRegisters.AccChanA, ecx

              shr   ah, 1
              mov   cl, [esi].TAYRegisters.R9
              mov   al, [esi].TAYRegisters.ChBOutput
              call  @MixOutput
              mov   [esi].TAYRegisters.AccChanB, ecx

              shr   ah, 1
              mov   cl, [esi].TAYRegisters.R10
              mov   al, [esi].TAYRegisters.ChCOutput
              call  @MixOutput
              mov   [esi].TAYRegisters.AccChanC, ecx

              pop   ebx
              pop   edx
              pop   esi
              ret

@MixOutput:   mov   bl, ah
              mov   bh, ah
              shr   bl, 3
              or    al, bh   	                        // (ToneA OR ToneEnable)
              or    bl, [esi].TAYRegisters.NoiseOutput  // (Noise OR NoiseEnable)
              and   al, bl   	                        // al = channel's final output
              shr   al, 1                               // ensure we only work on the bit 0 result
              jnc   @NoMixerOutput

              test  cl, 16
              jz    @Skip                               // jump if using fixed volume
              mov   cl, [esi].TAYRegisters.EnvVolume	   // else using envelope volume

@Skip:        and   ecx, 15                             // = 0-15
              mov   cx, Word [AYVolumes+ecx*2]
              ret

@NoMixerOutput:
              xor   ecx, ecx
              ret
End;

end.
