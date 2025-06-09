unit TransparentPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

Type
  TTransparentPanel = Class(TPanel)
  Private
    Procedure SetParent(AParent:TWinControl); Override;
    Procedure WMEraseBkGnd(Var Message:TWMEraseBkGnd); Message WM_EraseBkGnd;
  Protected
    Procedure CreateParams(Var Params:TCreateParams); Override;
    Procedure Paint; Override;
  Public
    Constructor Create(AOwner:TComponent); Override;
    Procedure Invalidate; Override;
  End;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PBGoodies', [TTransparentPanel]);
end;

Constructor TTransparentPanel.Create(AOwner:TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle:= ControlStyle - [csOpaque];
End;

Procedure TTransparentPanel.CreateParams(Var Params:TCreateParams);
Begin
  Inherited CreateParams(Params);
  Params.ExStyle:= Params.ExStyle or WS_EX_TRANSPARENT;
End;

Procedure TTransparentPanel.Paint;
Begin
  {Nothing Doing}
End;

Procedure TTransparentPanel.WMEraseBkGnd(Var Message:TWMEraseBkGnd);
Begin
  Message.Result:= 1;
End;

Procedure TTransparentPanel.SetParent(AParent:TWinControl);
Begin
  Inherited SetParent(AParent);
  If Parent <> Nil then
    SetWindowLong(Parent.Handle, GWL_STYLE,
       GetWindowLong(Parent.Handle, GWL_STYLE)
          And Not WS_ClipChildren);
End;

Procedure TTransparentPanel.Invalidate;
Var
  Rect :TRect;
Begin
  Rect:= BoundsRect;
  If (Parent <> Nil) and Parent.HandleAllocated then
    InvalidateRect(Parent.Handle, @Rect, True)
  Else
    Inherited Invalidate;
End;

end.



