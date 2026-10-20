unit BasinGpuCore;

{$mode delphi}{$H+}

interface

uses
  Windows, SysUtils, Classes, FPImage, FPReadPNG, FPReadJPEG, D3D11Mini;

type
  TBasinGpuContext = class
  private
    FHostWnd: HWND;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FCanvasWidth: Integer;
    FCanvasHeight: Integer;
    FSwapChain: Pointer;
    FDevice: Pointer;
    FImmediateContext: Pointer;
    FRenderTargetView: Pointer;
    FBackgroundTexture: Pointer;
    FBackgroundView: Pointer;
    FMaskTexture: Pointer;
    FMaskView: Pointer;
    FScreenTexture: Pointer;
    FScreenView: Pointer;
    FBorderTexture: Pointer;
    FBorderView: Pointer;
    FVertexShader: Pointer;
    FPixelShaders: array[0..1] of Pointer;
    FFilter: Cardinal;
    FLinearSampler: Pointer;
    FPointSampler: Pointer;
    FCriticalSection: TRTLCriticalSection;
    FLastError: AnsiString;
    procedure SetError(const Msg: AnsiString);
    procedure CheckHR(HR: HRESULT; const Operation: AnsiString);
    procedure CreateDeviceResources;
    procedure CreateRenderTarget;
    procedure CreateShaders;
    procedure CreateImageTexture(const FileName: AnsiString;
      out Texture, View: Pointer);
    procedure CreateFrameTextures;
    procedure ReleaseRenderTarget;
    procedure ReleaseResources;
  public
    constructor Create(AHostWnd: HWND; const BackgroundFileName,
      MaskFileName: AnsiString);
    destructor Destroy; override;
    procedure Resize(AWidth, AHeight: Cardinal);
    procedure UpdateFrame(Pixels: Pointer; BorderRGBA: Cardinal);
    procedure SetFilter(FilterID: Cardinal);
    procedure Render;
    property LastError: AnsiString read FLastError;
  end;

procedure SetGlobalError(const Msg: AnsiString);
function GetGlobalError: AnsiString;

implementation

type
  ID3DBlob = interface(IUnknown)
    ['{8BA5FB08-5195-40E2-AC58-0D989C3A0102}']
    function GetBufferPointer: Pointer; stdcall;
    function GetBufferSize: SIZE_T; stdcall;
  end;

  TD3DCompileFn = function(
    SrcData: Pointer;
    SrcDataSize: SIZE_T;
    SourceName: PAnsiChar;
    Defines: Pointer;
    IncludeHandler: Pointer;
    EntryPoint: PAnsiChar;
    Target: PAnsiChar;
    Flags1: Cardinal;
    Flags2: Cardinal;
    out Code: ID3DBlob;
    out ErrorMessages: ID3DBlob): HRESULT; stdcall;

const
  VertexShaderSource: AnsiString =
    'struct VSOutput {' + #10 +
    '  float4 position : SV_POSITION;' + #10 +
    '  float2 texCoord : TEXCOORD0;' + #10 +
    '};' + #10 +
    'VSOutput mainVS(uint vertexId : SV_VertexID) {' + #10 +
    '  VSOutput output;' + #10 +
    '  float2 uv = float2((vertexId << 1) & 2, vertexId & 2);' + #10 +
    '  output.texCoord = uv;' + #10 +
    '  output.position = float4(uv * float2(2.0, -2.0) + float2(-1.0, 1.0), 0.0, 1.0);' + #10 +
    '  return output;' + #10 +
    '}' + #10;

  { Timothy Lottes' public-domain CRT pipeline, adapted from the right panel
    of the Shadertoy demo for the Spectrum image and its solid border. }
  PixelShaderSource: AnsiString =
    'Texture2D backgroundTexture : register(t0);' + #10 +
    'Texture2D maskTexture : register(t1);' + #10 +
    'Texture2D screenTexture : register(t2);' + #10 +
    'Texture2D borderTexture : register(t3);' + #10 +
    'SamplerState linearSampler : register(s0);' + #10 +
    'SamplerState pointSampler : register(s1);' + #10 +
    'static const float2 overlayOrigin = float2(263.0, 176.0);' + #10 +
    'static const float2 overlaySize = float2(926.0, 802.0);' + #10 +
    'static const float2 imageOrigin = float2(79.0, 113.0);' + #10 +
    'static const float2 imageSize = float2(768.0, 576.0);' + #10 +
    'static const float2 emulatedRes = overlaySize / 3.0;' + #10 +
    'float3 RawPixel(float2 pixel) {' + #10 +
    '  float3 color = borderTexture.SampleLevel(pointSampler, float2(0.5, 0.5), 0).rgb;' + #10 +
    '  if (all(pixel >= imageOrigin) && all(pixel < imageOrigin + imageSize)) {' + #10 +
    '    float2 sourcePixel = floor((pixel - imageOrigin) / 3.0);' + #10 +
    '    color = screenTexture.SampleLevel(pointSampler, (sourcePixel + 0.5) / float2(256.0, 192.0), 0).rgb;' + #10 +
    '  }' + #10 +
    '  return color;' + #10 +
    '}' + #10 +
    'float ToLinear1(float c) { return c <= 0.04045 ? c / 12.92 : pow(max((c + 0.055) / 1.055, 0.0), 2.4); }' + #10 +
    'float3 ToLinear(float3 c) { return float3(ToLinear1(c.r), ToLinear1(c.g), ToLinear1(c.b)); }' + #10 +
    'float ToSrgb1(float c) { return c < 0.0031308 ? c * 12.92 : 1.055 * pow(max(c, 0.0), 0.41666) - 0.055; }' + #10 +
    'float3 ToSrgb(float3 c) { return float3(ToSrgb1(c.r), ToSrgb1(c.g), ToSrgb1(c.b)); }' + #10 +
    'float3 Fetch(float2 pos, float2 off) {' + #10 +
    '  float2 texel = floor(pos * emulatedRes + off);' + #10 +
    '  if (any(texel < 0.0) || any(texel >= emulatedRes)) return float3(0.0, 0.0, 0.0);' + #10 +
    '  return ToLinear(RawPixel((texel + 0.5) * 3.0));' + #10 +
    '}' + #10 +
    'float2 Dist(float2 pos) { pos *= emulatedRes; return -((pos - floor(pos)) - 0.5); }' + #10 +
    'float Gaus(float pos, float scale) { return exp2(scale * pos * pos); }' + #10 +
    'float3 Horz3(float2 pos, float off) {' + #10 +
    '  float3 b = Fetch(pos, float2(-1.0, off));' + #10 +
    '  float3 c = Fetch(pos, float2( 0.0, off));' + #10 +
    '  float3 d = Fetch(pos, float2( 1.0, off));' + #10 +
    '  float dst = Dist(pos).x;' + #10 +
    '  float wb = Gaus(dst - 1.0, -3.0);' + #10 +
    '  float wc = Gaus(dst, -3.0);' + #10 +
    '  float wd = Gaus(dst + 1.0, -3.0);' + #10 +
    '  return (b * wb + c * wc + d * wd) / (wb + wc + wd);' + #10 +
    '}' + #10 +
    'float3 Horz5(float2 pos, float off) {' + #10 +
    '  float3 a = Fetch(pos, float2(-2.0, off));' + #10 +
    '  float3 b = Fetch(pos, float2(-1.0, off));' + #10 +
    '  float3 c = Fetch(pos, float2( 0.0, off));' + #10 +
    '  float3 d = Fetch(pos, float2( 1.0, off));' + #10 +
    '  float3 e = Fetch(pos, float2( 2.0, off));' + #10 +
    '  float dst = Dist(pos).x;' + #10 +
    '  float wa = Gaus(dst - 2.0, -3.0);' + #10 +
    '  float wb = Gaus(dst - 1.0, -3.0);' + #10 +
    '  float wc = Gaus(dst, -3.0);' + #10 +
    '  float wd = Gaus(dst + 1.0, -3.0);' + #10 +
    '  float we = Gaus(dst + 2.0, -3.0);' + #10 +
    '  return (a * wa + b * wb + c * wc + d * wd + e * we) / (wa + wb + wc + wd + we);' + #10 +
    '}' + #10 +
    'float Scan(float2 pos, float off) { return Gaus(Dist(pos).y + off, -8.0); }' + #10 +
    'float3 Tri(float2 pos) {' + #10 +
    '  return Horz3(pos, -1.0) * Scan(pos, -1.0) +' + #10 +
    '         Horz5(pos,  0.0) * Scan(pos,  0.0) +' + #10 +
    '         Horz3(pos,  1.0) * Scan(pos,  1.0);' + #10 +
    '}' + #10 +
    'float2 Warp(float2 pos) {' + #10 +
    '  pos = pos * 2.0 - 1.0;' + #10 +
    '  pos *= float2(1.0 + pos.y * pos.y / 32.0, 1.0 + pos.x * pos.x / 24.0);' + #10 +
    '  return pos * 0.5 + 0.5;' + #10 +
    '}' + #10 +
    'float3 ShadowMask(float2 pos) {' + #10 +
    '  pos.x += pos.y * 3.0;' + #10 +
    '  float3 mask = float3(0.5, 0.5, 0.5);' + #10 +
    '  float phase = frac(pos.x / 6.0);' + #10 +
    '  if (phase < 0.333) mask.r = 1.5;' + #10 +
    '  else if (phase < 0.666) mask.g = 1.5;' + #10 +
    '  else mask.b = 1.5;' + #10 +
    '  return mask;' + #10 +
    '}' + #10 +
    'float3 LottesCRT(float2 overlayPixel, float2 outputPixel) {' + #10 +
    '  float2 warped = Warp(overlayPixel / overlaySize);' + #10 +
    '  return ToSrgb(Tri(warped) * ShadowMask(outputPixel));' + #10 +
    '}' + #10 +
    'float4 Compose(float4 position, float2 texCoord, bool crt) {' + #10 +
    '  float4 background = backgroundTexture.Sample(linearSampler, texCoord);' + #10 +
    '  float2 overlayPixel = texCoord * float2(1440.0, 1200.0) - overlayOrigin;' + #10 +
    '  if (all(overlayPixel >= 0.0) && all(overlayPixel < overlaySize)) {' + #10 +
    '    float3 maskColor = maskTexture.Sample(linearSampler, texCoord).rgb;' + #10 +
    '    float frameReady = borderTexture.SampleLevel(pointSampler, float2(0.5, 0.5), 0).a;' + #10 +
    '    if (frameReady > 0.5 && any(maskColor > 0.0)) {' + #10 +
    '      float3 emulatorPixel = crt ? LottesCRT(overlayPixel, position.xy) : RawPixel(overlayPixel);' + #10 +
    '      float3 dark = 2.0 * emulatorPixel * maskColor;' + #10 +
    '      float3 light = 1.0 - 2.0 * (1.0 - emulatorPixel) * (1.0 - maskColor);' + #10 +
    '      background.rgb = saturate(lerp(dark, light, step(0.5, maskColor)));' + #10 +
    '    }' + #10 +
    '  }' + #10 +
    '  return float4(background.rgb, 1.0);' + #10 +
    '}' + #10 +
    'float4 mainPlain(float4 position : SV_POSITION, float2 texCoord : TEXCOORD0) : SV_TARGET {' + #10 +
    '  return Compose(position, texCoord, false);' + #10 +
    '}' + #10 +
    'float4 mainLottes(float4 position : SV_POSITION, float2 texCoord : TEXCOORD0) : SV_TARGET {' + #10 +
    '  return Compose(position, texCoord, true);' + #10 +
    '}' + #10;

var
  GlobalError: AnsiString = '';

function HResultText(HR: HRESULT): AnsiString;
begin
  Result := '0x' + AnsiString(IntToHex(Cardinal(HR), 8));
end;

procedure SetGlobalError(const Msg: AnsiString);
begin
  GlobalError := Msg;
  OutputDebugStringA(PAnsiChar('BasinCGPU: ' + Msg));
end;

function GetGlobalError: AnsiString;
begin
  Result := GlobalError;
end;

function LoadCompiler(out ModuleHandle: HMODULE; out CompileFn: TD3DCompileFn): Boolean;
var
  ProcAddress: Pointer;
begin
  Result := False;
  ModuleHandle := LoadLibraryA('d3dcompiler_47.dll');
  if ModuleHandle = 0 then
    ModuleHandle := LoadLibraryA('d3dcompiler_43.dll');
  if ModuleHandle = 0 then
    Exit;
  ProcAddress := GetProcAddress(ModuleHandle, 'D3DCompile');
  if ProcAddress = nil then
  begin
    FreeLibrary(ModuleHandle);
    ModuleHandle := 0;
    Exit;
  end;
  CompileFn := TD3DCompileFn(ProcAddress);
  Result := True;
end;

function CompileShader(const Source, EntryPoint, Target: AnsiString): ID3DBlob;
var
  CompilerModule: HMODULE;
  CompileFn: TD3DCompileFn;
  CodeBlob: ID3DBlob;
  ErrorBlob: ID3DBlob;
  HR: HRESULT;
  ErrorText: AnsiString;
begin
  Result := nil;
  CodeBlob := nil;
  ErrorBlob := nil;
  if not LoadCompiler(CompilerModule, CompileFn) then
    raise Exception.Create('d3dcompiler_47.dll veya d3dcompiler_43.dll bulunamadi');
  try
    HR := CompileFn(PAnsiChar(Source), Length(Source), 'BasinCGPU', nil, nil,
      PAnsiChar(EntryPoint), PAnsiChar(Target), 0, 0, CodeBlob, ErrorBlob);
    if HR < 0 then
    begin
      if ErrorBlob <> nil then
        SetString(ErrorText, PAnsiChar(ErrorBlob.GetBufferPointer), ErrorBlob.GetBufferSize)
      else
        ErrorText := 'HLSL derleme hatasi: ' + HResultText(HR);
      raise Exception.Create(String(ErrorText));
    end;
    Result := CodeBlob;
    CodeBlob := nil;
  finally
    ErrorBlob := nil;
    CodeBlob := nil;
    { Keep d3dcompiler loaded: ID3DBlob vtables live in this module. }
  end;
end;

procedure TBasinGpuContext.SetError(const Msg: AnsiString);
begin
  FLastError := Msg;
  SetGlobalError(Msg);
end;

procedure TBasinGpuContext.CheckHR(HR: HRESULT; const Operation: AnsiString);
begin
  if HR < 0 then
    raise Exception.Create(String(Operation + ' basarisiz: ' + HResultText(HR)));
end;

constructor TBasinGpuContext.Create(AHostWnd: HWND; const BackgroundFileName,
  MaskFileName: AnsiString);
var
  R: TRect;
begin
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
  FHostWnd := AHostWnd;
  FFilter := 1;
  if not IsWindow(FHostWnd) then
    raise Exception.Create('Gecersiz GPU host pencere handle degeri');
  GetClientRect(FHostWnd, R);
  FWidth := Cardinal(R.Right - R.Left);
  FHeight := Cardinal(R.Bottom - R.Top);
  if FWidth = 0 then FWidth := 1;
  if FHeight = 0 then FHeight := 1;
  try
    FLastError := 'CreateDeviceResources';
    CreateDeviceResources;
    FLastError := 'CreateShaders';
    CreateShaders;
    FLastError := 'CreateBackgroundTexture';
    CreateImageTexture(BackgroundFileName, FBackgroundTexture, FBackgroundView);
    FLastError := 'CreateMaskTexture';
    CreateImageTexture(MaskFileName, FMaskTexture, FMaskView);
    FLastError := 'CreateFrameTextures';
    CreateFrameTextures;
    FLastError := 'InitialRender';
    Render;
    FLastError := '';
    SetGlobalError('');
  except
    on E: Exception do
    begin
      SetError(FLastError + ': ' + AnsiString(E.Message));
      ReleaseResources;
      raise;
    end;
  end;
end;

procedure TBasinGpuContext.CreateDeviceResources;
var
  Desc: TDXGISwapChainDesc;
  HR: HRESULT;
begin
  FillChar(Desc, SizeOf(Desc), 0);
  Desc.BufferDesc.Width := FWidth;
  Desc.BufferDesc.Height := FHeight;
  Desc.BufferDesc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
  Desc.SampleDesc.Count := 1;
  Desc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
  Desc.BufferCount := 2;
  Desc.OutputWindow := FHostWnd;
  Desc.Windowed := True;
  Desc.SwapEffect := DXGI_SWAP_EFFECT_DISCARD;

  HR := D3D11CreateDeviceAndSwapChain(nil, D3D_DRIVER_TYPE_HARDWARE, 0,
    D3D11_CREATE_DEVICE_BGRA_SUPPORT, nil, 0, D3D11_SDK_VERSION, @Desc,
    FSwapChain, FDevice, nil, FImmediateContext);
  if HR < 0 then
    HR := D3D11CreateDeviceAndSwapChain(nil, D3D_DRIVER_TYPE_WARP, 0,
      D3D11_CREATE_DEVICE_BGRA_SUPPORT, nil, 0, D3D11_SDK_VERSION, @Desc,
      FSwapChain, FDevice, nil, FImmediateContext);
  CheckHR(HR, 'D3D11 device/swap chain olusturma');
  CreateRenderTarget;
end;

procedure TBasinGpuContext.CreateRenderTarget;
var
  BackBuffer: Pointer;
begin
  BackBuffer := nil;
  CheckHR(SwapChainGetBuffer(FSwapChain, 0, IID_ID3D11Texture2D, BackBuffer),
    'Swap chain back buffer alma');
  try
    CheckHR(DeviceCreateRenderTargetView(FDevice, BackBuffer, nil,
      FRenderTargetView), 'Render target view olusturma');
  finally
    ComRelease(BackBuffer);
  end;
end;

procedure TBasinGpuContext.CreateShaders;
var
  VertexBlob: ID3DBlob;
  PixelBlob: ID3DBlob;
  ShaderBuffer: Pointer;
  ShaderSize: SIZE_T;
  SamplerDesc: TD3D11SamplerDesc;
  FilterID: Integer;
  EntryPoint: AnsiString;
begin
  FLastError := 'CompileVertexShader';
  VertexBlob := CompileShader(VertexShaderSource, 'mainVS', 'vs_4_0');
  try
    FLastError := 'GetVertexShaderBuffer';
    ShaderBuffer := VertexBlob.GetBufferPointer;
    FLastError := 'GetVertexShaderSize';
    ShaderSize := VertexBlob.GetBufferSize;
    FLastError := 'CreateVertexShader';
    CheckHR(DeviceCreateVertexShader(FDevice, ShaderBuffer,
      ShaderSize, nil, FVertexShader), 'Vertex shader olusturma');
  finally
    VertexBlob := nil;
  end;

  for FilterID := 0 to 1 do
  begin
    if FilterID = 0 then EntryPoint := 'mainPlain'
    else EntryPoint := 'mainLottes';
    FLastError := 'CompilePixelShader_' + EntryPoint;
    PixelBlob := CompileShader(PixelShaderSource, EntryPoint, 'ps_4_0');
    try
      ShaderBuffer := PixelBlob.GetBufferPointer;
      ShaderSize := PixelBlob.GetBufferSize;
      FLastError := 'CreatePixelShader_' + EntryPoint;
      CheckHR(DeviceCreatePixelShader(FDevice, ShaderBuffer,
        ShaderSize, nil, FPixelShaders[FilterID]),
        'Pixel shader olusturma');
    finally
      PixelBlob := nil;
    end;
  end;

  FLastError := 'CreateSamplerState';
  FillChar(SamplerDesc, SizeOf(SamplerDesc), 0);
  SamplerDesc.Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
  SamplerDesc.AddressU := D3D11_TEXTURE_ADDRESS_CLAMP;
  SamplerDesc.AddressV := D3D11_TEXTURE_ADDRESS_CLAMP;
  SamplerDesc.AddressW := D3D11_TEXTURE_ADDRESS_CLAMP;
  SamplerDesc.MaxLOD := 3.402823466E+38;
  CheckHR(DeviceCreateSamplerState(FDevice, @SamplerDesc, FLinearSampler),
    'Linear sampler state olusturma');

  SamplerDesc.Filter := D3D11_FILTER_MIN_MAG_MIP_POINT;
  CheckHR(DeviceCreateSamplerState(FDevice, @SamplerDesc, FPointSampler),
    'Point sampler state olusturma');
end;

procedure TBasinGpuContext.CreateImageTexture(const FileName: AnsiString;
  out Texture, View: Pointer);
var
  Image: TFPMemoryImage;
  Pixels: array of Byte;
  X, Y, Offset: Integer;
  C: TFPColor;
  Desc: TD3D11Texture2DDesc;
  InitialData: TD3D11SubresourceData;
begin
  if not FileExists(String(FileName)) then
    raise Exception.Create('Arka plan dosyasi bulunamadi: ' + String(FileName));

  Image := TFPMemoryImage.Create(0, 0);
  try
    Image.LoadFromFile(String(FileName));
    if (Image.Width = 0) or (Image.Height = 0) then
      raise Exception.Create('Arka plan goruntusu gecersiz');
    if FCanvasWidth = 0 then
    begin
      if (Image.Width <> 1440) or (Image.Height <> 1200) then
        raise Exception.Create('Arka plan 1440x1200 boyutunda olmali');
      FCanvasWidth := Image.Width;
      FCanvasHeight := Image.Height;
    end
    else if (Image.Width <> FCanvasWidth) or (Image.Height <> FCanvasHeight) then
      raise Exception.Create('Mask ve arka plan goruntulerinin boyutlari ayni olmali');

    SetLength(Pixels, Image.Width * Image.Height * 4);
    Offset := 0;
    for Y := 0 to Image.Height - 1 do
      for X := 0 to Image.Width - 1 do
      begin
        C := Image.Colors[X, Y];
        Pixels[Offset] := C.Red shr 8;
        Pixels[Offset + 1] := C.Green shr 8;
        Pixels[Offset + 2] := C.Blue shr 8;
        Pixels[Offset + 3] := C.Alpha shr 8;
        Inc(Offset, 4);
      end;

    FillChar(Desc, SizeOf(Desc), 0);
    Desc.Width := Image.Width;
    Desc.Height := Image.Height;
    Desc.MipLevels := 1;
    Desc.ArraySize := 1;
    Desc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
    Desc.SampleDesc.Count := 1;
    Desc.Usage := D3D11_USAGE_IMMUTABLE;
    Desc.BindFlags := D3D11_BIND_SHADER_RESOURCE;

    FillChar(InitialData, SizeOf(InitialData), 0);
    InitialData.SysMem := @Pixels[0];
    InitialData.SysMemPitch := Image.Width * 4;
    InitialData.SysMemSlicePitch := Length(Pixels);

    CheckHR(DeviceCreateTexture2D(FDevice, @Desc, @InitialData,
      Texture), 'Goruntu texture olusturma');
    CheckHR(DeviceCreateShaderResourceView(FDevice, Texture, nil,
      View), 'Goruntu shader resource view olusturma');
  finally
    Image.Free;
  end;
end;

procedure TBasinGpuContext.CreateFrameTextures;
var
  Desc: TD3D11Texture2DDesc;
  InitialData: TD3D11SubresourceData;
  InitialScreen: array[0..(256 * 192 * 4) - 1] of Byte;
  InitialBorder: array[0..3] of Byte;
begin
  FillChar(InitialScreen, SizeOf(InitialScreen), 0);
  FillChar(InitialBorder, SizeOf(InitialBorder), 0);
  FillChar(Desc, SizeOf(Desc), 0);
  Desc.Width := 256;
  Desc.Height := 192;
  Desc.MipLevels := 1;
  Desc.ArraySize := 1;
  Desc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
  Desc.SampleDesc.Count := 1;
  Desc.Usage := D3D11_USAGE_DEFAULT;
  Desc.BindFlags := D3D11_BIND_SHADER_RESOURCE;

  FillChar(InitialData, SizeOf(InitialData), 0);
  InitialData.SysMem := @InitialScreen[0];
  InitialData.SysMemPitch := 256 * 4;
  InitialData.SysMemSlicePitch := SizeOf(InitialScreen);
  CheckHR(DeviceCreateTexture2D(FDevice, @Desc, @InitialData, FScreenTexture),
    'Emulator screen texture olusturma');
  CheckHR(DeviceCreateShaderResourceView(FDevice, FScreenTexture, nil, FScreenView),
    'Emulator screen shader resource view olusturma');

  Desc.Width := 1;
  Desc.Height := 1;
  FillChar(InitialData, SizeOf(InitialData), 0);
  InitialData.SysMem := @InitialBorder[0];
  InitialData.SysMemPitch := 4;
  InitialData.SysMemSlicePitch := 4;
  CheckHR(DeviceCreateTexture2D(FDevice, @Desc, @InitialData, FBorderTexture),
    'Border color texture olusturma');
  CheckHR(DeviceCreateShaderResourceView(FDevice, FBorderTexture, nil, FBorderView),
    'Border color shader resource view olusturma');
end;

procedure TBasinGpuContext.ReleaseRenderTarget;
begin
  if FImmediateContext <> nil then
    ContextOMSetRenderTargets(FImmediateContext, 0, nil, nil);
  ComRelease(FRenderTargetView);
end;

procedure TBasinGpuContext.ReleaseResources;
begin
  if FImmediateContext <> nil then
  begin
    ContextClearState(FImmediateContext);
    ContextFlush(FImmediateContext);
  end;
  ReleaseRenderTarget;
  ComRelease(FPointSampler);
  ComRelease(FLinearSampler);
  ComRelease(FPixelShaders[1]);
  ComRelease(FPixelShaders[0]);
  ComRelease(FVertexShader);
  ComRelease(FBorderView);
  ComRelease(FBorderTexture);
  ComRelease(FScreenView);
  ComRelease(FScreenTexture);
  ComRelease(FMaskView);
  ComRelease(FMaskTexture);
  ComRelease(FBackgroundView);
  ComRelease(FBackgroundTexture);
  ComRelease(FImmediateContext);
  ComRelease(FDevice);
  ComRelease(FSwapChain);
end;

procedure TBasinGpuContext.Resize(AWidth, AHeight: Cardinal);
begin
  if (AWidth = 0) or (AHeight = 0) then Exit;
  if (AWidth = FWidth) and (AHeight = FHeight) then Exit;
  EnterCriticalSection(FCriticalSection);
  try
    try
      ReleaseRenderTarget;
      CheckHR(SwapChainResizeBuffers(FSwapChain, 0, AWidth, AHeight, 0, 0),
        'Swap chain resize');
      FWidth := AWidth;
      FHeight := AHeight;
      CreateRenderTarget;
    except
      on E: Exception do SetError(AnsiString(E.Message));
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TBasinGpuContext.UpdateFrame(Pixels: Pointer; BorderRGBA: Cardinal);
var
  BorderPixel: Cardinal;
  UnboundViews: array[0..3] of Pointer;
begin
  if Pixels = nil then
    raise Exception.Create('Emulator frame buffer nil');

  EnterCriticalSection(FCriticalSection);
  try
    if (FImmediateContext = nil) or (FScreenTexture = nil) or
      (FBorderTexture = nil) then
      raise Exception.Create('GPU frame texture hazir degil');

    FillChar(UnboundViews, SizeOf(UnboundViews), 0);
    ContextPSSetShaderResources(FImmediateContext, 0, 4, @UnboundViews[0]);
    ContextUpdateSubresource(FImmediateContext, FScreenTexture, 0, nil,
      Pixels, 256 * 4, 256 * 192 * 4);
    BorderPixel := BorderRGBA;
    ContextUpdateSubresource(FImmediateContext, FBorderTexture, 0, nil,
      @BorderPixel, 4, 4);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TBasinGpuContext.SetFilter(FilterID: Cardinal);
begin
  if FilterID > High(FPixelShaders) then
    raise Exception.Create('Gecersiz GPU filter ID');
  EnterCriticalSection(FCriticalSection);
  try
    FFilter := FilterID;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TBasinGpuContext.Render;
var
  Viewport: TD3D11Viewport;
  ClearColor: array[0..3] of Single;
  RenderTarget: Pointer;
  ShaderResources: array[0..3] of Pointer;
  Samplers: array[0..1] of Pointer;
  HR: HRESULT;
begin
  EnterCriticalSection(FCriticalSection);
  try
    try
      if (FSwapChain = nil) or (FRenderTargetView = nil) then Exit;

      FillChar(Viewport, SizeOf(Viewport), 0);
      Viewport.Width := FWidth;
      Viewport.Height := FHeight;
      Viewport.MinDepth := 0.0;
      Viewport.MaxDepth := 1.0;

      ClearColor[0] := 0.0;
      ClearColor[1] := 0.0;
      ClearColor[2] := 0.0;
      ClearColor[3] := 1.0;
      RenderTarget := FRenderTargetView;
      ShaderResources[0] := FBackgroundView;
      ShaderResources[1] := FMaskView;
      ShaderResources[2] := FScreenView;
      ShaderResources[3] := FBorderView;
      Samplers[0] := FLinearSampler;
      Samplers[1] := FPointSampler;

      ContextOMSetRenderTargets(FImmediateContext, 1, @RenderTarget, nil);
      ContextRSSetViewports(FImmediateContext, 1, @Viewport);
      ContextClearRenderTargetView(FImmediateContext, FRenderTargetView, @ClearColor[0]);
      ContextIASetPrimitiveTopology(FImmediateContext, D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
      ContextVSSetShader(FImmediateContext, FVertexShader);
      ContextPSSetShader(FImmediateContext, FPixelShaders[FFilter]);
      ContextPSSetShaderResources(FImmediateContext, 0, 4, @ShaderResources[0]);
      ContextPSSetSamplers(FImmediateContext, 0, 2, @Samplers[0]);
      ContextDraw(FImmediateContext, 3, 0);

      HR := SwapChainPresent(FSwapChain, 1, 0);
      if HR < 0 then SetError('Present basarisiz: ' + HResultText(HR));
    except
      on E: Exception do SetError(AnsiString(E.Message));
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

destructor TBasinGpuContext.Destroy;
begin
  EnterCriticalSection(FCriticalSection);
  try
    ReleaseResources;
  finally
    LeaveCriticalSection(FCriticalSection);
    DeleteCriticalSection(FCriticalSection);
  end;
  inherited Destroy;
end;

end.
