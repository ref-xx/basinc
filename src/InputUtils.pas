unit InputUtils;

interface

Uses Windows, Classes, SysUtils;

type

  TKSShift = (kssShift, kssCtrl, kssAlt, kssHankaku, kssReserve1, kssReserve2);
  TKSShiftSet = set of TKSShift;



	// Input control Procs

	Procedure ClearKeys;
	Function  KeyToToken(Key: Word; Shift: TShiftState): Byte;
	Procedure BufferToken(Token: Byte);
	Procedure SendNextToken;
	Procedure BufferKey(KeyBit, Key: Byte);
	Procedure BufferKeySequence(Keys: Array of Byte);
	Procedure UnBufferKey;
	Procedure BuildKeyPorts(Key: Word; KeyBit: Byte);
	Procedure AlterShift(Shifted: Boolean);
	Procedure AlterSymbShift(SymbShifted: Byte);
	Procedure SetCapslock;
        Procedure ReadKempstonMouse;
  Function  GetShiftStr(Sh: TKSShiftSet) : string;
  Function  GetCharFromVKey(vkey: Word): string;

Var

  VKStr:            array[0..255] of string;
  i:                integer;

	CurKeyDown:	      Byte;
	KeyState: 			TKeyBoardState;
	ReadBufferCount:	DWord;
  LastKey:          Word;
  InputDelete:      Boolean;

	LogText,
	TokenBuffer,
	KeyBuffer:			String;

implementation

Uses Fastcore, ROMUtils, BASinMain, Utility, LogWind;

Procedure ClearKeys;
Begin
  Ports[$FEFE] := 255;
  Ports[$FDFE] := 255;
  Ports[$FBFE] := 255;
  Ports[$F7FE] := 255;
  Ports[$EFFE] := 255;
  Ports[$DFFE] := 255;
  Ports[$BFFE] := 255;
  Ports[$7FFE] := 255;
  KeyBuffer := '';
  InputDelete := False;
End;

Procedure BuildKeyPorts(Key: Word; KeyBit: Byte);
Var
  PortByte: Byte;
  KeyName: AnsiChar;
  Port: ^Byte;
  Shifted: Boolean;
  SymbShifted: Byte;
Begin

  SymbShifted := GetKeyState(VK_SHIFT);
  Shifted := SymbShifted > 1;
  SymbShifted := GetKeyState(VK_CONTROL);
  If SymbShifted > 1 Then
     SymbShifted := 0
  Else
     SymbShifted := 1;

  Case Key of
     VK_SHIFT:
        Begin
           If KeyBit = 0 Then AlterShift(True) Else AlterShift(False);
        End;
     VK_BACK, VK_DELETE:
        Begin
           If KeyBit = 0 Then AlterShift(True) Else AlterShift(False);
           Key := 48;
        End;
     VK_LEFT:
        Begin
           If KeyBit = 0 Then AlterShift(True) Else AlterShift(False);
           Key := 53;
        End;
     VK_RIGHT:
        Begin
           If KeyBit = 0 Then AlterShift(True) Else AlterShift(False);
           Key := 56;
        End;
     VK_UP:
        Begin
           If KeyBit = 0 Then AlterShift(True) Else AlterShift(False);
           Key := 55;
        End;
     VK_DOWN:
        Begin
           If KeyBit = 0 Then AlterShift(True) Else AlterShift(False);
           Key := 54;
        End;
     VK_CONTROL:
        Begin
           AlterSymbShift(KeyBit);
           Exit;
        End;
     VK_NUMPAD0: Key := 48;
     VK_NUMPAD1: Key := 49;
     VK_NUMPAD2: Key := 50;
     VK_NUMPAD3: Key := 51;
     VK_NUMPAD4: Key := 52;
     VK_NUMPAD5: Key := 53;
     VK_NUMPAD6: Key := 54;
     VK_NUMPAD7: Key := 55;
     VK_NUMPAD8: Key := 56;
     VK_NUMPAD9: Key := 57;
     VK_MULTIPLY:
        Begin
           AlterShift(False);
           AlterSymbShift(KeyBit);
           Key := 66;
        End;
     VK_ADD:
        Begin
           AlterShift(False);
           AlterSymbShift(KeyBit);
           Key := 75;
        End;
     VK_SUBTRACT:
        Begin
           AlterShift(False);
           AlterSymbShift(KeyBit);
           Key := 74;
        End;
     VK_DIVIDE:
        Begin
           AlterShift(False);
           AlterSymbShift(KeyBit);
           Key := 86;
        End;
     VK_DECIMAL:
        Begin
           AlterShift(False);
           AlterSymbShift(KeyBit);
           Key := 77;
        End;

     // Special Shifted Keys

     187: // + =
        Begin
           If Shifted or (SymbShifted = 0) Then Begin
              Key := 75;
              BuildKeyPorts(76, 1);
           End Else Begin
              Key := 76;
              BuildKeyPorts(75, 1);
           End;
           AlterShift(False);
           AlterSymbShift(KeyBit);
        End;
     189: // _ -
        Begin
           If Shifted or (SymbShifted = 0) Then Begin
              Key := 48;
              BuildKeyPorts(74, 1);
           End Else Begin
              Key := 74;
              BuildKeyPorts(48, 1);
           End;
           AlterShift(False);
           AlterSymbShift(KeyBit);
        End;
     190: // . >
        Begin
           If Shifted or (SymbShifted = 0) Then Begin
              Key := Ord('T');
              BuildKeyPorts(Ord('M'), 1);
           End Else Begin
              Key := Ord('M');
              BuildKeyPorts(Ord('T'), 1);
           End;
           AlterShift(False);
           AlterSymbShift(KeyBit);
        End;
     191: // ? /
        Begin
           If Shifted or (SymbShifted = 0) Then Begin
              Key := 67;
              BuildKeyPorts(86, 1);
           End Else Begin
              Key := 86;
              BuildKeyPorts(67, 1);
           End;
           AlterShift(False);
           AlterSymbShift(KeyBit);
        End;
     188: // , <
        Begin
           If Shifted or (SymbShifted = 0) Then Begin
              Key := 82;
              BuildKeyPorts(78, 1);
           End Else Begin
              Key := 78;
              BuildKeyPorts(82, 1);
           End;
           AlterShift(False);
           AlterSymbShift(KeyBit);
        End;
     192:  // @ '
        Begin
           If Shifted or (SymbShifted = 0) Then Begin
              Key := 50;
              BuildKeyPorts(55, 1);
           End Else Begin
              Key := 55;
              BuildKeyPorts(50, 1);
           End;
           AlterShift(False);
           AlterSymbShift(KeyBit);
        End;
     222: // # ~ (~ is an extend mode char, needs buffering.)
        Begin
           If Not Shifted and (SymbShifted = 1) Then Begin
              AlterSymbShift(KeyBit);
              Key := 51;
           End Else Begin
              If KeyBit = 0 Then Begin
                 BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 65, 1, 65, 1, 255]);
                 Exit;
              End Else Exit;
           End;
        End;
     186: // ; :
        Begin
           If Shifted or (SymbShifted = 0) Then Begin
              Key := 90;
              BuildKeyPorts(79, 1);
           End Else Begin
              Key := 79;
              BuildKeyPorts(90, 1);
           End;
           AlterShift(False);
           AlterSymbShift(KeyBit);
        End;

     // Extend Mode Keys

     219: // [ {
        Begin
           If Not Shifted and (SymbShifted = 1) Then Begin
              If KeyBit = 0 Then Begin
                 BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 89, 1, 89, 1, 255]);
                 Exit;
              End Else Exit;
           End Else Begin
              If KeyBit = 0 Then Begin
                 BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 70, 1, 70, 1, 255]);
                 Exit;
              End Else Exit;
           End;
        End;

     220: // | \
        Begin
           If Not Shifted and (SymbShifted = 1) Then Begin
              If KeyBit = 0 Then Begin
                 BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 68, 1, 68, 1, 255]);
                 Exit;
              End Else Exit;
           End Else Begin
              If KeyBit = 0 Then Begin
                 BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 83, 1, 83, 1, 255]);
                 Exit;
              End Else Exit;
           End;
        End;

     221: // ] }
        Begin
           If Not Shifted and (SymbShifted = 1) Then Begin
              If KeyBit = 0 Then Begin
                 BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 85, 1, 85, 1, 255]);
                 Exit;
              End Else Exit;
           End Else Begin
              If KeyBit = 0 Then Begin
                 BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 71, 1, 71, 1, 255]);
                 Exit;
              End Else Exit;
           End;
        End;

     223: // (C) symbol
        Begin
           If KeyBit = 0 Then Begin
              BufferKeySequence([0, 16, 0, 255, 1, 16, 0, 80, 1, 80, 1, 255]);
              Exit;
           End Else Exit;
        End;

  End;
  If Key = 255 Then Key := 17;

  

  KeyName := Chr(Key And 255);
  Case KeyName of
     Chr(16),'Z',     'X', 'C', 'V': Port := @Ports[$FEFE];
     'A',    'S',     'D', 'F', 'G': Port := @Ports[$FDFE];
     'Q',    'W',     'E', 'R', 'T': Port := @Ports[$FBFE];
     '1',    '2',     '3', '4', '5': Port := @Ports[$F7FE];
     '0',    '9',     '8', '7', '6': Port := @Ports[$EFFE];
     'P',    'O',     'I', 'U', 'Y': Port := @Ports[$DFFE];
     Chr(13),'L',     'K', 'J', 'H': Port := @Ports[$BFFE];
     ' ',    Chr(17), 'M', 'N', 'B': Port := @Ports[$7FFE];
  Else
     Port := @Ports[0];
  End;
  PortByte := Port^;
  Case KeyBit Of
     0: Case KeyName of
           Chr(16), 'A', 'Q', '1', '0', 'P', Chr(13), ' ':     PortByte := PortByte And -2;
           'Z',     'S', 'W', '2', '9', 'O', 'L',     Chr(17): PortByte := PortByte And -3;
           'X',     'D', 'E', '3', '8', 'I', 'K',     'M':     PortByte := PortByte And -5;
           'C',     'F', 'R', '4', '7', 'U', 'J',     'N':     PortByte := PortByte And -9;
           'V',     'G', 'T', '5', '6', 'Y', 'H',     'B':     PortByte := PortByte And -17;
        End;
     1: Case KeyName of
           Chr(16), 'A', 'Q', '1', '0', 'P', Chr(13), ' ':     PortByte := PortByte Or 1;
           'Z',     'S', 'W', '2', '9', 'O', 'L',     Chr(17): PortByte := PortByte Or 2;
           'X',     'D', 'E', '3', '8', 'I', 'K',     'M':     PortByte := PortByte Or 4;
           'C',     'F', 'R', '4', '7', 'U', 'J',     'N':     PortByte := PortByte Or 8;
           'V',     'G', 'T', '5', '6', 'Y', 'H',     'B':     PortByte := PortByte Or 16;
        End;
  End;
  Port^ := PortByte;

   If TokenBuffer = '' Then
     If (Ports[$FEFE] = 255) And (Ports[$FDFE] = 255) And (Ports[$FBFE] = 255) And (Ports[$F7FE] = 255)
        And (Ports[$EFFE] = 255) And (Ports[$DFFE] = 255) And (Ports[$BFFE] = 255) And (Ports[$7FFE] = 255) Then
           Memory[FLAGS] := Memory[FLAGS] And 223;
           
End;

Procedure AlterShift(Shifted: Boolean);
Begin
  If Shifted Then Ports[$FEFE] := Ports[$FEFE] and -2 Else Ports[$FEFE] := Ports[$FEFE] Or 1;
End;

Procedure AlterSymbShift(SymbShifted: Byte);
Begin
  If SymbShifted = 0 Then Ports[$7FFE] := Ports[$7FFE] And -3 Else Ports[$7FFE] := Ports[$7FFE] Or 2;
End;

Function GetCharFromVKey(vkey: Word): string;
var
  keystate: TKeyboardState;
  retcode: Integer;
begin
  Win32Check(GetKeyboardState(keystate));
  SetLength(Result, 2);
  retcode := ToAsciiEx(vkey, MapVirtualKey(vkey, 0), keystate, @Result[1], 0, GetKeyboardLayout(0));
  case retcode of
    0: Result := '';
    1: SetLength(Result, 1);
    2: ;
    else
      Result := '';
  end;
end;

Function KeyToToken(Key: Word; Shift: TShiftState): Byte;
Var
	Caps,
	Shifted,
  CtrlDown,
  GFXMode: Boolean;
  KeyChar: String;
begin

	Result := $FF; // $FF is an invalid token, and is returned for invalid keys.

	Shifted := ssShift in Shift;
        CtrlDown := ssCtrl in Shift;
	GFXMode := Memory[MODE] = 2;
	Caps := (DWord(GetKeyState(VK_CAPITAL) and 1) = 1);

	// Caps Lock can be set in any mode, and Graphics likewise.
	// Shift is ignored in all modes, as the shift state has already
	// been determined.

  // First, check for "Special" keys, such as Return, Backspace, Delete etc.
  // If not a special key, then use localisation to convert to ascii.

	Case Key Of

     VK_NUMLOCK:
        Begin
           If opt_GraphicsMethod = gmNumLock Then Begin
              Result := $0F;
              Exit;
           End;
        End;

     VK_SCROLL:
        Begin
           If opt_GraphicsMethod = gmScrollLock Then Begin
              Result := $0F;
              Exit;
           End;
        End;

     VK_ESCAPE:
        Begin
           If InInput and BASinOutput.Running Then Begin
              BufferToken(226);
              Result := 13;
           End Else
              Result := 32;
           If Registers.HaltEmu Then Begin
              Registers.HaltEmu := False;
              Inc(Registers.PC);
           End;
           If ProgStateFlag = PS_Paused then // Cancel any PAUSE command.
              PutWord(@Registers.C, 0);
           Exit;
        End;

		VK_CAPITAL:
			Begin
				// Set the capslock by sending a $06 (128k Manual is *wrong* there)
				// but test that the speccy matches the PC first.
				If Memory[FLAGS2] And 8 = 8 Then Begin
					// Caps is on
					If Not Caps Then Result := $06 Else Exit;
				End Else Begin
					// Caps is off
					If Caps Then Result := $06 Else Exit;
				End;
       	End;

		VK_RETURN:
        Begin
           // If the user presses Return in EDIT mode, then
           // a direct command or program change is about to occur,
           // so set up an UNDO.
           If (Not BASinOutput.Running) and ((GetWord(@Memory[WORKSP])-GetWord(@Memory[E_LINE])) > 2) Then Begin
              SaveEmulationState(UndoState);
              SaveEmulationState(BREAKState);
           End;
           Result := Key;
        End;
        VK_SPACE:
                Begin
                        Result := Key;
                End;

        VK_CONTROL:
        Begin
           // For those of you lucky enough to own an english keyboard and XP/Nt,
           // Graphics mode can be a modifier on Alt-GR.
           If (opt_GraphicsMethod = gmAltGr) and (DWord(GetASyncKeyState(VK_RMENU)) <> 0) Then Begin
              // Put Graphics mode on if it isn't already
              // Alt-Gr comes through as a CONTROL key for some reason.
              If Memory[MODE] <> 2 Then Result := $0F;
              Exit;
           End;
        End;

		VK_SHIFT:
			Begin
				// No Shift available, it's read previously.
				Exit;
			End;

		// Editing Keys. Available wherever.

		VK_F1: // Edit, Blue
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $11  // Blue Paper
					Else
						Result := $19; // Blue Ink
				End Else
					Result := $00;    // Context Help
			End;
		VK_F2: // Red, Graphics On/Off
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $12  // Red Paper
					Else
						Result := $1A; // Red Ink
				End Else
					Result := $07;    // Edit
			End;
		VK_F3: // Inverse Video, Magenta
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $13  // Magenta Paper
					Else
						Result := $1B; // Magenta Ink
				End Else
					Result := $05;    // Inverse Video
			End;
		VK_F4: // True Video, Green
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $14  // Green Paper
					Else
						Result := $1C; // Green Ink
				End Else
					Result := $04;    // True Video
			End;
		VK_F5: // Bright On, Cyan
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $15  // Cyan Paper
					Else
						Result := $1D; // Cyan Ink
				End Else
					Result := $03;    // Bright On
			End;
		VK_F6: // Bright Off, Yellow
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $16  // Yellow Paper
					Else
						Result := $1E; // Yellow Ink
				End Else
					Result := $02;    // Bright Off
			End;
		VK_F7: // Flash On, White
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $17  // White Paper
					Else
						Result := $1F; // White Ink
				End Else
					Result := $01;    // Flash On
			End;
		VK_F8: // Flash Off, Black
			Begin
				If GFXMode Then Begin
					If Shifted Then
						Result := $10  // Black Paper
					Else
						Result := $18; // Black Ink
				End Else
              Result := $00;    // Flash Off
			End;

		VK_BACK:
			Begin
				Result := $0C;
			End;
     VK_DELETE:
        Begin
           Result := $0E;
           InputDelete := True;
        End;

		VK_LEFT:
			Begin
				Result := $08;
			End;
		VK_RIGHT:
			Begin
				Result := $09;
			End;
		VK_DOWN:
			Begin
           If TokenBuffer <> '' Then Exit;
           Result := EditCursorMove(1);
			End;
     VK_UP:
			Begin
           If TokenBuffer <> '' Then Exit;
           Result := EditCursorMove(0);
 			End;
		VK_HOME:
			Begin
           If TokenBuffer <> '' Then Exit;
				If Editing Then Result := EditCursorMove(2);
			End;
		VK_END:
			Begin
           If TokenBuffer <> '' Then Exit;
				If Editing Then Result := EditCursorMove(3);
			End;
		VK_PRIOR:
			Begin // Page Up
				If Editing Then
              If Shifted Then
                 Result := EditCursorMove(4)
              Else
                 Result := $0B;
			End;
		VK_NEXT:
			Begin // Page Down
				If Editing Then
              If Shifted Then
                 Result := EditCursorMove(5)
              Else
                 Result := $0A;
			End;
	End;

	// Allow special keys to bail out now.

	If Result <> $FF Then Exit;

	// Now test the keys and supply the emulation with the correct tokens.
	// First, the non-graphics mode chars.

	If Memory[MODE] <> 2 Then	Begin

     KeyChar := GetCharFromVKey(Key);
     If KeyChar = '' Then Exit;
     Key := Ord(KeyChar[1]);

		Case Key Of

			// Alphabetics

			65..90: // A..Z
				Begin
					If Memory[MODE] <> 2 Then Begin
						If Not Shifted And Not Caps Then Inc(Key, 32);
					End Else Begin
						If Key < $56 Then
							Inc(Key, $4F)
						Else
                    Key := $FF;
           	End;
					Result := Key;
				End;

        97..122: // a..z
				Begin
					If Memory[MODE] <> 2 Then Begin
						If Shifted or Caps Then Dec(Key, 32);
					End Else Begin
						If Key < $56 Then
							Inc(Key, $4F)
						Else
                    Key := $FF;
           	End;
					Result := Key;
				End;
     Else

        If Key = 96 Then Key := 127;
        If Key = 163 Then Key := 96;
        If Key > 126 Then Key := 127;
        Result := Key;

		End;

	End Else Begin

		// now a few keys function differently in Graphics mode,
		// Alphas give UDGs (A..U) and Numbers give Checkerboard chars.
		// Numbers can be shifted.

		Case Key Of

			// Alphabetic UDGs

			49: // 1
				Begin
					If Not Shifted Then
						Result := $81
					Else
						Result := $8E;
				End;
			50: // 2
				Begin
					If Not Shifted Then
						Result := $82
					Else
						Result := $8D;
				End;
			51: // 3
				Begin
					If Not Shifted Then
						Result := $83
					Else
						Result := $8C;
				End;
			52: // 4
				Begin
					If Not Shifted Then
						Result := $84
					Else
						Result := $8B;
				End;
			53: // 5
				Begin
					If Not Shifted Then
						Result := $85
					Else
						Result := $8A;
				End;
			54: // 6
				Begin
					If Not Shifted Then
						Result := $86
					Else
						Result := $89;
				End;
			55: // 7
				Begin
					If Not Shifted Then
						Result := $87
					Else
						Result := $88;
				End;
			56: // 8
				Begin
					If Not Shifted Then
						Result := $80
					Else
						Result := $8F;
				End;

			65..90:
				Begin
					If (ProgramIs128k And (Key < $54)) or (Not ProgramIs128k and (Key <$56)) Then
						Inc(Key, $4F) // Add for UDG
					Else
                 Key := $FF;
					Result := Key;
          	End;

     Else

        Result := $FF;

		End;

	End;

end;

Procedure BufferToken(Token: Byte);
Begin
  If Not Registers.IntsEnabled Then
     TokenBuffer := Chr(Token)
  Else
	   TokenBuffer := TokenBuffer + Chr(Token);
	If Length(TokenBuffer) > 5 Then TokenBuffer := Copy(TokenBuffer, 1, 5);
	If Token <> $0F Then
     If (opt_GraphicsMethod = gmAltGr) and (GetKeyState(VK_RMENU) < 0) Then Begin
        If Chr(Token) in ['a'..'u'] Then
           CurKeyDown := Token + 47
        Else
           If ProgramIs128k Then Begin
              If Chr(Token) in ['A'..'S'] Then
                 CurKeyDown := Token + 79;
           End Else Begin
              If Chr(Token) in ['A'..'U'] Then
                 CurKeyDown := Token + 79;
           End;
     End Else
        CurKeyDown := Token;
End;

Procedure SendNextToken;
Begin
	If Memory[FLAGS] and 32 <> 32 Then Begin
		If TokenBuffer <> '' Then Begin
     	// Set FLAGS Bit 5
        Memory[FLAGS] := Memory[FLAGS] or 32;
        // POKE LAST K with next token
        Memory[LAST_K] := Ord(TokenBuffer[1]);

        TokenBuffer := Copy(TokenBuffer, 2, 9999);
        Exit;
     End;
	End;
End;

Procedure BufferKey(KeyBit, Key: Byte);
Begin
  If Length(KeyBuffer) < 240 Then KeyBuffer := KeyBuffer+Chr(KeyBit)+Chr(Key);
End;

Procedure BufferKeySequence(Keys: Array of Byte);
Var
  F: Integer;
  S: String;
Begin
  S := '';
  For F := 0 To High(Keys) Do S := S + Chr(Keys[F]);
  KeyBuffer := KeyBuffer + S;
End;

Procedure UnBufferKey;
Var
  Key: Word;
  KeyBit: Byte;
Begin
  If ReadBufferCount = 0 Then Begin
     Key := Ord(KeyBuffer[2]);
     KeyBit := Ord(KeyBuffer[1]);
     KeyBuffer := Copy(KeyBuffer, 3, 99999);
     BuildKeyPorts(Key, KeyBit);
     ReadBufferCount := 2;
  End;
End;

Procedure SetCapslock;
Var
  Lock,
	Caps: Boolean;
Begin
	// This is usually called in response to a WM_ACTIVATE message,
	// to synchronise the CAPSLOCK state of the Spectrum with that of
	// the PC.
	Caps := DWord(GetKeyState(VK_CAPITAL) and 1) = 1;
	If Memory[FLAGS2] And 8 = 8 Then Begin
		// Spectrum Caps is on
		If Not Caps Then Begin
        // PC Caps is off, so update Spectrum Caps
        Memory[FLAGS2] := Memory[FLAGS2] and 247;
     End;
	End Else Begin
		// Spectrum Caps is off
		If Caps Then Begin
        // PC Caps is on, so synchronise the two.
        Memory[FLAGS2] := Memory[FLAGS2] or 8;
     End;
	End;
  // If selected, the graphics mode needs to be updated
  // to match the current numlock/scroll lock status.
  If opt_GraphicsMethod <> gmAltGr Then Begin
     If opt_GraphicsMethod = gmNumLock Then
	      Lock := DWord(GetKeyState(VK_NUMLOCK) and 1) = 1
     Else If opt_GraphicsMethod = gmScrollLock Then
	      Lock := DWord(GetKeyState(VK_SCROLL) and 1) = 1;
     If Lock <> (Memory[MODE] = 2) Then
        TokenBuffer := TokenBuffer + Chr($0F);
  End;
End;

Procedure ReadKempstonMouse;
var
MyPoint : TPoint;
begin
Windows.GetCursorPos(MyPoint);
  mypoint.x:=mypoint.x div displayscale;
  mypoint.y:=mypoint.y div displayscale;

  if (oldMousepos.x<>Mypoint.x) or (oldMousepos.y<>Mypoint.y) then begin
     dx:=dx+(mypoint.x - oldmousepos.x);
     dy:=dy-(mypoint.y - oldmousepos.y);
     dx:=dx and 255;
     dy:=dy and 255;
     KempMouseX := dx;
     KempMouseY := dy;
     oldmousepos.x:=mypoint.x;
     oldmousepos.y:=mypoint.y;
  end;

end;

Function GetShiftStr(Sh : TKSShiftSet) : string;
Var
  I: TKSShift;
const
  KSShiftStr: array[TKSShift] of string = ('Shift', 'Ctrl', 'Alt', 'Hankaku', 'Reserve1', 'Reserve2');
begin
  Result := '';
  For I := kssShift to kssReserve2 do
    If I In Sh Then Result := Result + KSShiftStr[I] + ', ';
  If Length(Result) > 2 then SetLength(Result, Length(Result) - 2); // trim last ', ';
  Result := '[' + Result + ']';
end;

initialization

  {fill virtual-key constants array}
  {values from Oct 1999 MSDN CD}
  VKStr[0] := 'Not Used';
  VKStr[1] := 'VK_LBUTTON';
  VKStr[2] := 'VK_RBUTTON';
  VKStr[3] := 'VK_CANCEL';
  VKStr[4] := 'VK_MBUTTON';
  VKStr[5] := 'Undefined';
  VKStr[6] := 'Undefined';
  VKStr[7] := 'Undefined';
  VKStr[8] := 'VK_BACK';
  VKStr[9] := 'VK_TAB';
  VKStr[10] := 'Undefined';
  VKStr[11] := 'Undefined';
  VKStr[12] := 'VK_CLEAR';
  VKStr[13] := 'VK_RETURN';
  VKStr[14] := 'Undefined';
  VKStr[15] := 'Undefined';
  VKStr[16] := 'VK_SHIFT';
  VKStr[17] := 'VK_CONTROL';
  VKStr[18] := 'VK_MENU';
  VKStr[19] := 'VK_PAUSE';
  VKStr[20] := 'VK_CAPITAL';
  for i:= 21{ $15} to 26{ $1A} do VKStr[i] := 'Undefined';
  VKStr[27] := 'VK_ESCAPE';
  for i:= 28{ $1C} to 31{ $1F} do VKStr[i] := 'Undefined';
  VKStr[32] := 'VK_SPACE';
  VKStr[33] := 'VK_PRIOR';
  VKStr[34] := 'VK_NEXT';
  VKStr[35] := 'VK_END';
  VKStr[36] := 'VK_HOME';
  VKStr[37] := 'VK_LEFT';
  VKStr[38] := 'VK_UP';
  VKStr[39] := 'VK_RIGHT';
  VKStr[40] := 'VK_DOWN';
  VKStr[41] := 'VK_SELECT';
  VKStr[42] := 'VK_PRINT';
  VKStr[43] := 'VK_EXECUTE';
  VKStr[44] := 'VK_SNAPSHOT';
  VKStr[45] := 'VK_INSERT';
  VKStr[46] := 'VK_DELETE';
  VKStr[47] := 'VK_HELP';
  VKStr[48] := 'VK_0';
  VKStr[49] := 'VK_1';
  VKStr[50] := 'VK_2';
  VKStr[51] := 'VK_3';
  VKStr[52] := 'VK_4';
  VKStr[53] := 'VK_5';
  VKStr[54] := 'VK_6';
  VKStr[55] := 'VK_7';
  VKStr[56] := 'VK_8';
  VKStr[57] := 'VK_9';
  for i:= 56{ $3A} to 64{ $40} do VKStr[i] := 'Undefined';
  VKStr[65] := 'VK_A';
  VKStr[66] := 'VK_B';
  VKStr[67] := 'VK_C';
  VKStr[68] := 'VK_D';
  VKStr[69] := 'VK_E';
  VKStr[70] := 'VK_F';
  VKStr[71] := 'VK_G';
  VKStr[72] := 'VK_H';
  VKStr[73] := 'VK_I';
  VKStr[74] := 'VK_J';
  VKStr[75] := 'VK_K';
  VKStr[76] := 'VK_L';
  VKStr[77] := 'VK_M';
  VKStr[78] := 'VK_N';
  VKStr[79] := 'VK_O';
  VKStr[80] := 'VK_P';
  VKStr[81] := 'VK_Q';
  VKStr[82] := 'VK_R';
  VKStr[83] := 'VK_S';
  VKStr[84] := 'VK_T';
  VKStr[85] := 'VK_U';
  VKStr[86] := 'VK_V';
  VKStr[87] := 'VK_W';
  VKStr[88] := 'VK_X';
  VKStr[89] := 'VK_Y';
  VKStr[90] := 'VK_Z';
  VKStr[91] := 'VK_LWIN';
  VKStr[92] := 'VK_RWIN';
  VKStr[93] := 'VK_APPS';
  VKStr[94] := 'Undefined';
  VKStr[95] := 'Undefined';
  VKStr[96] := 'VK_NUMPAD0';
  VKStr[97] := 'VK_NUMPAD1';
  VKStr[98] := 'VK_NUMPAD2';
  VKStr[99] := 'VK_NUMPAD3';
  VKStr[100] := 'VK_NUMPAD4';
  VKStr[101] := 'VK_NUMPAD5';
  VKStr[102] := 'VK_NUMPAD6';
  VKStr[103] := 'VK_NUMPAD7';
  VKStr[104] := 'VK_NUMPAD8';
  VKStr[105] := 'VK_NUMPAD9';
  VKStr[106] := 'VK_MULTIPLY';
  VKStr[107] := 'VK_ADD';
  VKStr[108] := 'VK_SEPARATOR';
  VKStr[109] := 'VK_SUBTRACT';
  VKStr[110] := 'VK_DECIMAL';
  VKStr[111] := 'VK_DIVIDE';
  VKStr[112] := 'VK_F1';
  VKStr[113] := 'VK_F2';
  VKStr[114] := 'VK_F3';
  VKStr[115] := 'VK_F4';
  VKStr[116] := 'VK_F5';
  VKStr[117] := 'VK_F6';
  VKStr[118] := 'VK_F7';
  VKStr[119] := 'VK_F8';
  VKStr[120] := 'VK_F9';
  VKStr[121] := 'VK_F10';
  VKStr[122] := 'VK_F11';
  VKStr[123] := 'VK_F12';
  VKStr[124] := 'VK_F13';
  VKStr[125] := 'VK_F14';
  VKStr[126] := 'VK_F15';
  VKStr[127] := 'VK_F16';
  VKStr[128] := 'VK_F17';
  VKStr[129] := 'VK_F18';
  VKStr[130] := 'VK_F19';
  VKStr[131] := 'VK_F20';
  VKStr[132] := 'VK_F21';
  VKStr[133] := 'VK_F22';
  VKStr[134] := 'VK_F23';
  VKStr[135] := 'VK_F24';
  for i:= 136{ $88} to 143{ $8F} do VKStr[i] := 'Unassigned';
  VKStr[144] := 'VK_NUMLOCK';
  VKStr[145] := 'VK_SCROLL';
  for i:= 146{ $92} to 185{ $B9} do VKStr[i] := 'Unassigned';
  VKStr[186] := 'VK_OEM_1';      // ;:
  VKStr[187] := 'VK_OEM_PLUS';   // =+
  VKStr[188] := 'VK_OEM_COMMA';  // ,<
  VKStr[189] := 'VK_OEM_MINUS';  // -_
  VKStr[190] := 'VK_OEM_PERIOD'; // .>
  VKStr[191] := 'VK_OEM_2';    // /?
  VKStr[192] := 'VK_OEM_3';    // '@
  for i:= 193{ $C1} to 218{ $DA} do VKStr[i] := 'Unassigned';
  VKStr[219] := 'VK_OEM_4';     // [{
  VKStr[220] := 'VK_OEM_5';     // \|
  VKStr[221] := 'VK_OEM_6';     // ]}
  VKStr[222] := 'VK_OEM_7';     // #~
  VKStr[223] := 'VK_OEM_8';     // `¬
  VKStr[224] := 'VK_F17';       // on Olivetti extended keyboard
  VKStr[225] := 'VK_OEM_AX';
  VKStr[226] := 'VK_OEM_102';
  VKStr[227] := 'VK_ICO_HELP';  // on Olivetti extended keyboard
  VKStr[228] := 'VK_ICO_00';    // on Olivetti extended keyboard
  VKStr[229] := 'VK_PROCESSKEY';
  VKStr[230] := 'VK_ICO_CLEAR'; // on Olivetti extended keyboard
  VKStr[231] := 'Unassigned';
  VKStr[232] := 'Unassigned';
  VKStr[233] := 'VK_OEM_RESET';   // only used by Nokia
  VKStr[234] := 'VK_OEM_JUMP';    // only used by Nokia
  VKStr[235] := 'VK_OEM_PA1';     // only used by Nokia
  VKStr[236] := 'VK_OEM_PA2';     // only used by Nokia
  VKStr[237] := 'VK_OEM_PA3';     // only used by Nokia
  VKStr[238] := 'VK_OEM_WSCTRL';  // only used by Nokia
  VKStr[239] := 'VK_OEM_CUSEL';   // only used by Nokia
  VKStr[240] := 'VK_OEM_ATTN';    // only used by Nokia
  VKStr[241] := 'VK_OEM_FINNISH'; // only used by Nokia
  VKStr[242] := 'VK_OEM_COPY';    // only used by Nokia
  VKStr[243] := 'VK_OEM_AUTO';    // only used by Nokia
  VKStr[244] := 'VK_OEM_ENLW';    // only used by Nokia
  VKStr[245] := 'VK_OEM_BACKTAB'; // only used by Nokia
  VKStr[246] := 'VK_ATTN';
  VKStr[247] := 'VK_CRSEL';
  VKStr[248] := 'VK_EXSEL';
  VKStr[249] := 'VK_EREOF';
  VKStr[250] := 'VK_PLAY';
  VKStr[251] := 'VK_ZOOM';
  VKStr[252] := 'VK_NONAME';
  VKStr[253] := 'VK_PA1';
  VKStr[254] := 'VK_OEM_CLEAR';
  VKStr[255] := 'Not Used';


end.
