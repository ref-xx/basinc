unit BasinGpuRenderer;

interface

uses Windows, SysUtils;

const
  GPU_FILTER_NONE = 0;
  GPU_FILTER_LOTTES_CRT = 1;

type
  TDXRCreate = function(HostWindow: HWND; BackgroundFileName, MaskFileName: PChar;
    var Renderer: Pointer): LongInt; stdcall;
  TDXRUpdateFrame = function(Renderer: Pointer; Pixels: Pointer;
    BorderRGBA: LongWord): LongInt; stdcall;
  TDXRSetFilter = function(Renderer: Pointer; FilterID: LongWord): LongInt; stdcall;
  TDXRResize = function(Renderer: Pointer; Width, Height: LongWord): LongInt; stdcall;
  TDXRRender = function(Renderer: Pointer): LongInt; stdcall;
  TDXRDestroy = procedure(Renderer: Pointer); stdcall;
  TDXRGetLastError = function(Buffer: PChar; BufferSize: LongInt): LongInt; stdcall;

function GPUEnsureLoaded(const DLLFileName: String): Boolean;
function GPUCreate(HostWindow: HWND; const BackgroundFileName, MaskFileName: String;
  var Renderer: Pointer): Boolean;
function GPUUpdateFrame(Renderer: Pointer; Pixels: Pointer;
  BorderRGBA: LongWord): Boolean;
function GPUSetFilter(Renderer: Pointer; FilterID: LongWord): Boolean;
function GPUResize(Renderer: Pointer; Width, Height: LongWord): Boolean;
function GPURender(Renderer: Pointer): Boolean;
procedure GPUDestroy(var Renderer: Pointer);
function GPUGetLastError: String;
procedure GPUUnload;

implementation

var
  GPUModule: HMODULE = 0;
  DXRCreate: TDXRCreate = nil;
  DXRUpdateFrame: TDXRUpdateFrame = nil;
  DXRSetFilter: TDXRSetFilter = nil;
  DXRResize: TDXRResize = nil;
  DXRRender: TDXRRender = nil;
  DXRDestroy: TDXRDestroy = nil;
  DXRGetLastError: TDXRGetLastError = nil;
  LoaderError: String = '';

procedure ClearFunctions;
begin
  @DXRCreate := nil;
  @DXRUpdateFrame := nil;
  @DXRSetFilter := nil;
  @DXRResize := nil;
  @DXRRender := nil;
  @DXRDestroy := nil;
  @DXRGetLastError := nil;
end;

function GPUEnsureLoaded(const DLLFileName: String): Boolean;
begin
  Result := GPUModule <> 0;
  if Result then Exit;

  LoaderError := '';
  GPUModule := LoadLibrary(PChar(DLLFileName));
  if GPUModule = 0 then begin
    LoaderError := 'BasinCGPU.dll yuklenemedi. Windows hata kodu: ' +
      IntToStr(GetLastError);
    Exit;
  end;

  @DXRCreate := GetProcAddress(GPUModule, 'DXR_Create');
  @DXRUpdateFrame := GetProcAddress(GPUModule, 'DXR_UpdateFrame');
  @DXRSetFilter := GetProcAddress(GPUModule, 'DXR_SetFilter');
  @DXRResize := GetProcAddress(GPUModule, 'DXR_Resize');
  @DXRRender := GetProcAddress(GPUModule, 'DXR_Render');
  @DXRDestroy := GetProcAddress(GPUModule, 'DXR_Destroy');
  @DXRGetLastError := GetProcAddress(GPUModule, 'DXR_GetLastError');

  Result := Assigned(DXRCreate) and Assigned(DXRUpdateFrame) and
    Assigned(DXRSetFilter) and Assigned(DXRResize) and
    Assigned(DXRRender) and Assigned(DXRDestroy) and
    Assigned(DXRGetLastError);
  if not Result then begin
    LoaderError := 'BasinCGPU.dll gerekli exportlari icermiyor.';
    FreeLibrary(GPUModule);
    GPUModule := 0;
    ClearFunctions;
  end;
end;

function GPUCreate(HostWindow: HWND; const BackgroundFileName, MaskFileName: String;
  var Renderer: Pointer): Boolean;
begin
  Renderer := nil;
  Result := Assigned(DXRCreate) and
    (DXRCreate(HostWindow, PChar(BackgroundFileName), PChar(MaskFileName), Renderer) = 0) and
    (Renderer <> nil);
end;

function GPUUpdateFrame(Renderer: Pointer; Pixels: Pointer;
  BorderRGBA: LongWord): Boolean;
begin
  Result := Assigned(DXRUpdateFrame) and (Renderer <> nil) and (Pixels <> nil) and
    (DXRUpdateFrame(Renderer, Pixels, BorderRGBA) = 0);
end;

function GPUSetFilter(Renderer: Pointer; FilterID: LongWord): Boolean;
begin
  Result := Assigned(DXRSetFilter) and (Renderer <> nil) and
    (DXRSetFilter(Renderer, FilterID) = 0);
end;

function GPUResize(Renderer: Pointer; Width, Height: LongWord): Boolean;
begin
  Result := Assigned(DXRResize) and (Renderer <> nil) and
    (DXRResize(Renderer, Width, Height) = 0);
end;

function GPURender(Renderer: Pointer): Boolean;
begin
  Result := Assigned(DXRRender) and (Renderer <> nil) and
    (DXRRender(Renderer) = 0);
end;

procedure GPUDestroy(var Renderer: Pointer);
begin
  if Assigned(DXRDestroy) and (Renderer <> nil) then
    DXRDestroy(Renderer);
  Renderer := nil;
end;

function GPUGetLastError: String;
var
  Buffer: array[0..1023] of Char;
begin
  Result := LoaderError;
  if Assigned(DXRGetLastError) then begin
    FillChar(Buffer, SizeOf(Buffer), 0);
    DXRGetLastError(@Buffer[0], SizeOf(Buffer));
    if Buffer[0] <> #0 then Result := String(Buffer);
  end;
end;

procedure GPUUnload;
begin
  ClearFunctions;
  if GPUModule <> 0 then begin
    FreeLibrary(GPUModule);
    GPUModule := 0;
  end;
end;

initialization

finalization
  GPUUnload;

end.
