library BasinCGPU;

{$mode delphi}{$H+}

uses
  Windows, SysUtils, BasinGpuCore;

function DXR_Create(HostWindow: HWND; BackgroundFileName, MaskFileName: PAnsiChar;
  out Renderer: Pointer): LongInt; stdcall;
var
  Context: TBasinGpuContext;
begin
  Result := -1;
  Renderer := nil;
  try
    if (BackgroundFileName = nil) or (MaskFileName = nil) then
      raise Exception.Create('No back or mask bitmap path given');
    Context := TBasinGpuContext.Create(HostWindow,
      AnsiString(BackgroundFileName), AnsiString(MaskFileName));
    Renderer := Pointer(Context);
    Result := 0;
  except
    on E: Exception do
      if GetGlobalError = '' then
        SetGlobalError(AnsiString(E.Message));
  end;
end;

function DXR_UpdateFrame(Renderer, Pixels: Pointer; BorderRGBA: LongWord): LongInt; stdcall;
begin
  Result := -1;
  try
    if Renderer = nil then
      raise Exception.Create('GPU renderer context nil');
    TBasinGpuContext(Renderer).UpdateFrame(Pixels, BorderRGBA);
    Result := 0;
  except
    on E: Exception do
      SetGlobalError(AnsiString(E.Message));
  end;
end;

function DXR_SetFilter(Renderer: Pointer; FilterID: LongWord): LongInt; stdcall;
begin
  Result := -1;
  try
    if Renderer = nil then
      raise Exception.Create('GPU renderer context nil');
    TBasinGpuContext(Renderer).SetFilter(FilterID);
    Result := 0;
  except
    on E: Exception do
      SetGlobalError(AnsiString(E.Message));
  end;
end;

function DXR_Resize(Renderer: Pointer; Width, Height: LongWord): LongInt; stdcall;
begin
  Result := -1;
  try
    if Renderer = nil then
      raise Exception.Create('GPU renderer context nil');
    TBasinGpuContext(Renderer).Resize(Width, Height);
    Result := 0;
  except
    on E: Exception do
      SetGlobalError(AnsiString(E.Message));
  end;
end;

function DXR_Render(Renderer: Pointer): LongInt; stdcall;
begin
  Result := -1;
  try
    if Renderer = nil then
      raise Exception.Create('GPU renderer context nil');
    TBasinGpuContext(Renderer).Render;
    Result := 0;
  except
    on E: Exception do
      SetGlobalError(AnsiString(E.Message));
  end;
end;

procedure DXR_Destroy(Renderer: Pointer); stdcall;
begin
  try
    if Renderer <> nil then
      TBasinGpuContext(Renderer).Free;
  except
    on E: Exception do
      SetGlobalError(AnsiString(E.Message));
  end;
end;

function DXR_GetLastError(Buffer: PAnsiChar; BufferSize: LongInt): LongInt; stdcall;
var
  Msg: AnsiString;
begin
  Msg := GetGlobalError;
  Result := Length(Msg);
  if (Buffer <> nil) and (BufferSize > 0) then
    lstrcpynA(Buffer, PAnsiChar(Msg), BufferSize);
end;

exports
  DXR_Create name 'DXR_Create',
  DXR_UpdateFrame name 'DXR_UpdateFrame',
  DXR_SetFilter name 'DXR_SetFilter',
  DXR_Resize name 'DXR_Resize',
  DXR_Render name 'DXR_Render',
  DXR_Destroy name 'DXR_Destroy',
  DXR_GetLastError name 'DXR_GetLastError';

begin
end.
