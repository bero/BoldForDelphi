{ Global compiler directives }
{$include bold.inc}
unit BoldAggeregates;

interface

uses
  BoldBase;

type
  TBoldAggregateableObject = class;
  TBoldContainedObject = class;
  TBoldAggregatedObject = class;

  {-- TBoldAggregateableObject --}
  TBoldAggregateableObject = class(TBoldRefCountedObject, IInterface)
    function IInterface._AddRef = NonDelegatingAddRef;
    function IInterface._Release = NonDelegatingRelease;
    function IInterface.QueryInterface = NonDelegatingQueryInterface;
  protected
    FController: Pointer;
    function GetController: IInterface;
    function QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} IId: TGUID; out Obj): HResult; override;
    function _AddRef: Integer; override;
    function _Release: Integer; override;
    function NonDelegatingQueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; virtual; {$IFDEF BOLD_INTF_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
    function NonDelegatingAddRef: Integer; virtual; {$IFDEF BOLD_INTF_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
    function NonDelegatingRelease: Integer; virtual; {$IFDEF BOLD_INTF_CDECL}cdecl{$ELSE}stdcall{$ENDIF};
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  {-- TBoldContainedObject --}
  TBoldContainedObject = class(TBoldInterfacedObject)
  protected
    FController: Pointer;
    function GetController: IInterface;
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  {-- TBoldAggregatedObject --}
  TBoldAggregatedObject = class(TBoldContainedObject)
  protected
    function QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} IId: TGUID; out Obj): HResult; override;
  end;

implementation

{-- TBoldAggregateableObject --------------------------------------------------}

constructor TBoldAggregateableObject.Create(const Controller: IInterface);
begin
  FController := Pointer(Controller);
end;

function TBoldAggregateableObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

function TBoldAggregateableObject.QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  if Assigned(FController) then
    Result := IInterface(FController).QueryInterface(IID, Obj)
  else
    Result := NonDelegatingQueryInterface(IID, Obj);
end;

function TBoldAggregateableObject._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IInterface(FController)._AddRef
  else
    Result := NonDelegatingAddRef;
end;

function TBoldAggregateableObject._Release: Integer;
begin
  if Assigned(FController) then
    Result := IInterface(FController)._Release
  else
    Result := NonDelegatingRelease;
end;

function TBoldAggregateableObject.NonDelegatingQueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TBoldAggregateableObject.NonDelegatingAddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TBoldAggregateableObject.NonDelegatingRelease: Integer;
begin
  Result := inherited _Release;
end;

{-- TBoldContainedObject ------------------------------------------------------}

constructor TBoldContainedObject.Create(const Controller: IInterface);
begin
  FController := Pointer(Controller);
end;

function TBoldContainedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

function TBoldContainedObject._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TBoldContainedObject._Release: Integer;
begin
  Result := IInterface(FController)._Release;
end;

{-- TBoldAggregatedObject -----------------------------------------------------}

function TBoldAggregatedObject.QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  Result := IInterface(FController).QueryInterface(IID, Obj);
end;

end.

