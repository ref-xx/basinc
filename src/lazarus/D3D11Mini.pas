unit D3D11Mini;

{$mode delphi}{$H+}
{$packrecords c}

interface

uses
  Windows;

type
  UINT = Cardinal;
  SIZE_T = Cardinal;
  D3D_FEATURE_LEVEL = Cardinal;
  DXGI_FORMAT = Cardinal;

  TDXGISampleDesc = record
    Count: UINT;
    Quality: UINT;
  end;

  TDXGIRational = record
    Numerator: UINT;
    Denominator: UINT;
  end;

  TDXGIModeDesc = record
    Width: UINT;
    Height: UINT;
    RefreshRate: TDXGIRational;
    Format: DXGI_FORMAT;
    ScanlineOrdering: Cardinal;
    Scaling: Cardinal;
  end;

  TDXGISwapChainDesc = record
    BufferDesc: TDXGIModeDesc;
    SampleDesc: TDXGISampleDesc;
    BufferUsage: UINT;
    BufferCount: UINT;
    OutputWindow: HWND;
    Windowed: BOOL;
    SwapEffect: Cardinal;
    Flags: UINT;
  end;
  PDXGISwapChainDesc = ^TDXGISwapChainDesc;

  TD3D11Texture2DDesc = record
    Width: UINT;
    Height: UINT;
    MipLevels: UINT;
    ArraySize: UINT;
    Format: DXGI_FORMAT;
    SampleDesc: TDXGISampleDesc;
    Usage: Cardinal;
    BindFlags: UINT;
    CPUAccessFlags: UINT;
    MiscFlags: UINT;
  end;
  PD3D11Texture2DDesc = ^TD3D11Texture2DDesc;

  TD3D11SubresourceData = record
    SysMem: Pointer;
    SysMemPitch: UINT;
    SysMemSlicePitch: UINT;
  end;
  PD3D11SubresourceData = ^TD3D11SubresourceData;

  TD3D11SamplerDesc = record
    Filter: Cardinal;
    AddressU: Cardinal;
    AddressV: Cardinal;
    AddressW: Cardinal;
    MipLODBias: Single;
    MaxAnisotropy: UINT;
    ComparisonFunc: Cardinal;
    BorderColor: array[0..3] of Single;
    MinLOD: Single;
    MaxLOD: Single;
  end;
  PD3D11SamplerDesc = ^TD3D11SamplerDesc;

  TD3D11Viewport = record
    TopLeftX: Single;
    TopLeftY: Single;
    Width: Single;
    Height: Single;
    MinDepth: Single;
    MaxDepth: Single;
  end;
  PD3D11Viewport = ^TD3D11Viewport;

  TPointerArray = array[0..255] of Pointer;
  PPointerArray = ^TPointerArray;

const
  D3D11_SDK_VERSION = 7;
  D3D_DRIVER_TYPE_HARDWARE = 1;
  D3D_DRIVER_TYPE_WARP = 5;
  D3D11_CREATE_DEVICE_BGRA_SUPPORT = $20;

  DXGI_FORMAT_R8G8B8A8_UNORM = 28;
  DXGI_USAGE_RENDER_TARGET_OUTPUT = $20;
  DXGI_SWAP_EFFECT_DISCARD = 0;

  D3D11_USAGE_DEFAULT = 0;
  D3D11_USAGE_IMMUTABLE = 1;
  D3D11_BIND_SHADER_RESOURCE = $8;
  D3D11_FILTER_MIN_MAG_MIP_POINT = 0;
  D3D11_FILTER_MIN_MAG_MIP_LINEAR = $15;
  D3D11_TEXTURE_ADDRESS_CLAMP = 3;
  D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST = 4;

  IID_ID3D11Texture2D: TGUID = '{6F15AAF2-D208-4E89-9AB4-489535D34F9C}';

function D3D11CreateDeviceAndSwapChain(
  pAdapter: Pointer;
  DriverType: Cardinal;
  Software: HMODULE;
  Flags: UINT;
  pFeatureLevels: Pointer;
  FeatureLevels: UINT;
  SDKVersion: UINT;
  pSwapChainDesc: PDXGISwapChainDesc;
  out SwapChain: Pointer;
  out Device: Pointer;
  pFeatureLevel: Pointer;
  out ImmediateContext: Pointer): HRESULT; stdcall; external 'd3d11.dll';

function VMethod(Obj: Pointer; Index: Integer): Pointer;
procedure ComRelease(var Obj: Pointer);

function SwapChainPresent(SwapChain: Pointer; SyncInterval, Flags: UINT): HRESULT;
function SwapChainGetBuffer(SwapChain: Pointer; Buffer: UINT; const IID: TGUID; out Surface: Pointer): HRESULT;
function SwapChainResizeBuffers(SwapChain: Pointer; BufferCount, Width, Height: UINT; NewFormat: DXGI_FORMAT; Flags: UINT): HRESULT;

function DeviceCreateTexture2D(Device: Pointer; Desc: PD3D11Texture2DDesc; InitialData: PD3D11SubresourceData; out Texture: Pointer): HRESULT;
function DeviceCreateShaderResourceView(Device, Resource, Desc: Pointer; out View: Pointer): HRESULT;
function DeviceCreateRenderTargetView(Device, Resource, Desc: Pointer; out View: Pointer): HRESULT;
function DeviceCreateVertexShader(Device, ByteCode: Pointer; ByteCodeLength: SIZE_T; ClassLinkage: Pointer; out Shader: Pointer): HRESULT;
function DeviceCreatePixelShader(Device, ByteCode: Pointer; ByteCodeLength: SIZE_T; ClassLinkage: Pointer; out Shader: Pointer): HRESULT;
function DeviceCreateSamplerState(Device: Pointer; Desc: PD3D11SamplerDesc; out Sampler: Pointer): HRESULT;

procedure ContextPSSetShaderResources(Context: Pointer; StartSlot, NumViews: UINT; Views: PPointer);
procedure ContextPSSetShader(Context, Shader: Pointer);
procedure ContextPSSetSamplers(Context: Pointer; StartSlot, NumSamplers: UINT; Samplers: PPointer);
procedure ContextUpdateSubresource(Context, Resource: Pointer; Subresource: UINT;
  DstBox, SrcData: Pointer; SrcRowPitch, SrcDepthPitch: UINT);
procedure ContextVSSetShader(Context, Shader: Pointer);
procedure ContextDraw(Context: Pointer; VertexCount, StartVertexLocation: UINT);
procedure ContextIASetPrimitiveTopology(Context: Pointer; Topology: Cardinal);
procedure ContextOMSetRenderTargets(Context: Pointer; NumViews: UINT; Views: PPointer; DepthStencilView: Pointer);
procedure ContextRSSetViewports(Context: Pointer; NumViewports: UINT; Viewports: PD3D11Viewport);
procedure ContextClearRenderTargetView(Context, RenderTargetView: Pointer; Color: PSingle);
procedure ContextClearState(Context: Pointer);
procedure ContextFlush(Context: Pointer);

implementation

type
  TReleaseFn = function(Self: Pointer): Cardinal; stdcall;
  TPresentFn = function(Self: Pointer; SyncInterval, Flags: UINT): HRESULT; stdcall;
  TGetBufferFn = function(Self: Pointer; Buffer: UINT; const IID: TGUID; out Surface: Pointer): HRESULT; stdcall;
  TResizeBuffersFn = function(Self: Pointer; BufferCount, Width, Height: UINT; NewFormat: DXGI_FORMAT; Flags: UINT): HRESULT; stdcall;

  TCreateTexture2DFn = function(Self: Pointer; Desc: PD3D11Texture2DDesc; InitialData: PD3D11SubresourceData; out Texture: Pointer): HRESULT; stdcall;
  TCreateViewFn = function(Self, Resource, Desc: Pointer; out View: Pointer): HRESULT; stdcall;
  TCreateShaderFn = function(Self, ByteCode: Pointer; ByteCodeLength: SIZE_T; ClassLinkage: Pointer; out Shader: Pointer): HRESULT; stdcall;
  TCreateSamplerFn = function(Self: Pointer; Desc: PD3D11SamplerDesc; out Sampler: Pointer): HRESULT; stdcall;

  TSetResourcesFn = procedure(Self: Pointer; StartSlot, Count: UINT; Resources: PPointer); stdcall;
  TUpdateSubresourceFn = procedure(Self, Resource: Pointer; Subresource: UINT;
    DstBox, SrcData: Pointer; SrcRowPitch, SrcDepthPitch: UINT); stdcall;
  TSetShaderFn = procedure(Self, Shader, ClassInstances: Pointer; NumClassInstances: UINT); stdcall;
  TDrawFn = procedure(Self: Pointer; VertexCount, StartVertexLocation: UINT); stdcall;
  TSetTopologyFn = procedure(Self: Pointer; Topology: Cardinal); stdcall;
  TOMSetRenderTargetsFn = procedure(Self: Pointer; NumViews: UINT; Views: PPointer; DepthStencilView: Pointer); stdcall;
  TRSSetViewportsFn = procedure(Self: Pointer; NumViewports: UINT; Viewports: PD3D11Viewport); stdcall;
  TClearRenderTargetViewFn = procedure(Self, RenderTargetView: Pointer; Color: PSingle); stdcall;
  TNoArgProcedureFn = procedure(Self: Pointer); stdcall;

function VMethod(Obj: Pointer; Index: Integer): Pointer;
begin
  Result := PPointerArray(PPointer(Obj)^)[Index];
end;

procedure ComRelease(var Obj: Pointer);
var
  Fn: TReleaseFn;
begin
  if Obj <> nil then
  begin
    Fn := TReleaseFn(VMethod(Obj, 2));
    Fn(Obj);
    Obj := nil;
  end;
end;

function SwapChainPresent(SwapChain: Pointer; SyncInterval, Flags: UINT): HRESULT;
var Fn: TPresentFn;
begin
  Fn := TPresentFn(VMethod(SwapChain, 8));
  Result := Fn(SwapChain, SyncInterval, Flags);
end;

function SwapChainGetBuffer(SwapChain: Pointer; Buffer: UINT; const IID: TGUID; out Surface: Pointer): HRESULT;
var Fn: TGetBufferFn;
begin
  Fn := TGetBufferFn(VMethod(SwapChain, 9));
  Result := Fn(SwapChain, Buffer, IID, Surface);
end;

function SwapChainResizeBuffers(SwapChain: Pointer; BufferCount, Width, Height: UINT; NewFormat: DXGI_FORMAT; Flags: UINT): HRESULT;
var Fn: TResizeBuffersFn;
begin
  Fn := TResizeBuffersFn(VMethod(SwapChain, 13));
  Result := Fn(SwapChain, BufferCount, Width, Height, NewFormat, Flags);
end;

function DeviceCreateTexture2D(Device: Pointer; Desc: PD3D11Texture2DDesc; InitialData: PD3D11SubresourceData; out Texture: Pointer): HRESULT;
var Fn: TCreateTexture2DFn;
begin
  Fn := TCreateTexture2DFn(VMethod(Device, 5));
  Result := Fn(Device, Desc, InitialData, Texture);
end;

function DeviceCreateShaderResourceView(Device, Resource, Desc: Pointer; out View: Pointer): HRESULT;
var Fn: TCreateViewFn;
begin
  Fn := TCreateViewFn(VMethod(Device, 7));
  Result := Fn(Device, Resource, Desc, View);
end;

function DeviceCreateRenderTargetView(Device, Resource, Desc: Pointer; out View: Pointer): HRESULT;
var Fn: TCreateViewFn;
begin
  Fn := TCreateViewFn(VMethod(Device, 9));
  Result := Fn(Device, Resource, Desc, View);
end;

function DeviceCreateVertexShader(Device, ByteCode: Pointer; ByteCodeLength: SIZE_T; ClassLinkage: Pointer; out Shader: Pointer): HRESULT;
var Fn: TCreateShaderFn;
begin
  Fn := TCreateShaderFn(VMethod(Device, 12));
  Result := Fn(Device, ByteCode, ByteCodeLength, ClassLinkage, Shader);
end;

function DeviceCreatePixelShader(Device, ByteCode: Pointer; ByteCodeLength: SIZE_T; ClassLinkage: Pointer; out Shader: Pointer): HRESULT;
var Fn: TCreateShaderFn;
begin
  Fn := TCreateShaderFn(VMethod(Device, 15));
  Result := Fn(Device, ByteCode, ByteCodeLength, ClassLinkage, Shader);
end;

function DeviceCreateSamplerState(Device: Pointer; Desc: PD3D11SamplerDesc; out Sampler: Pointer): HRESULT;
var Fn: TCreateSamplerFn;
begin
  Fn := TCreateSamplerFn(VMethod(Device, 23));
  Result := Fn(Device, Desc, Sampler);
end;

procedure ContextPSSetShaderResources(Context: Pointer; StartSlot, NumViews: UINT; Views: PPointer);
var Fn: TSetResourcesFn;
begin
  Fn := TSetResourcesFn(VMethod(Context, 8));
  Fn(Context, StartSlot, NumViews, Views);
end;

procedure ContextPSSetShader(Context, Shader: Pointer);
var Fn: TSetShaderFn;
begin
  Fn := TSetShaderFn(VMethod(Context, 9));
  Fn(Context, Shader, nil, 0);
end;

procedure ContextPSSetSamplers(Context: Pointer; StartSlot, NumSamplers: UINT; Samplers: PPointer);
var Fn: TSetResourcesFn;
begin
  Fn := TSetResourcesFn(VMethod(Context, 10));
  Fn(Context, StartSlot, NumSamplers, Samplers);
end;

procedure ContextUpdateSubresource(Context, Resource: Pointer; Subresource: UINT;
  DstBox, SrcData: Pointer; SrcRowPitch, SrcDepthPitch: UINT);
var Fn: TUpdateSubresourceFn;
begin
  Fn := TUpdateSubresourceFn(VMethod(Context, 48));
  Fn(Context, Resource, Subresource, DstBox, SrcData, SrcRowPitch, SrcDepthPitch);
end;

procedure ContextVSSetShader(Context, Shader: Pointer);
var Fn: TSetShaderFn;
begin
  Fn := TSetShaderFn(VMethod(Context, 11));
  Fn(Context, Shader, nil, 0);
end;

procedure ContextDraw(Context: Pointer; VertexCount, StartVertexLocation: UINT);
var Fn: TDrawFn;
begin
  Fn := TDrawFn(VMethod(Context, 13));
  Fn(Context, VertexCount, StartVertexLocation);
end;

procedure ContextIASetPrimitiveTopology(Context: Pointer; Topology: Cardinal);
var Fn: TSetTopologyFn;
begin
  Fn := TSetTopologyFn(VMethod(Context, 24));
  Fn(Context, Topology);
end;

procedure ContextOMSetRenderTargets(Context: Pointer; NumViews: UINT; Views: PPointer; DepthStencilView: Pointer);
var Fn: TOMSetRenderTargetsFn;
begin
  Fn := TOMSetRenderTargetsFn(VMethod(Context, 33));
  Fn(Context, NumViews, Views, DepthStencilView);
end;

procedure ContextRSSetViewports(Context: Pointer; NumViewports: UINT; Viewports: PD3D11Viewport);
var Fn: TRSSetViewportsFn;
begin
  Fn := TRSSetViewportsFn(VMethod(Context, 44));
  Fn(Context, NumViewports, Viewports);
end;

procedure ContextClearRenderTargetView(Context, RenderTargetView: Pointer; Color: PSingle);
var Fn: TClearRenderTargetViewFn;
begin
  Fn := TClearRenderTargetViewFn(VMethod(Context, 50));
  Fn(Context, RenderTargetView, Color);
end;

procedure ContextClearState(Context: Pointer);
var Fn: TNoArgProcedureFn;
begin
  Fn := TNoArgProcedureFn(VMethod(Context, 110));
  Fn(Context);
end;

procedure ContextFlush(Context: Pointer);
var Fn: TNoArgProcedureFn;
begin
  Fn := TNoArgProcedureFn(VMethod(Context, 111));
  Fn(Context);
end;

end.
