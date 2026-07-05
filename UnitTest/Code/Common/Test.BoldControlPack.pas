unit Test.BoldControlPack;

{ DUnitX tests for BoldControlPack - follower/renderer display exception contract }

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  BoldElements,
  BoldControlPack,
  BoldStringControlPack,
  BoldExceptionHandlers,
  BoldAttributes;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldControlPack = class
  private
    FOwnerForm: TComponent;       // plays the role of the form owning everything
    FOwningComponent: TComponent; // plays the role of the Bold-aware control
    FRenderer: TBoldAsStringRenderer;
    FController: TBoldStringFollowerController;
    FFollower: TBoldFollower;
    FElement: TBAString;
    FHandlerCalled: Boolean;
    FRaiseInGetAsString: Boolean;
    function RaisingGetAsString(aFollower: TBoldFollower): string;
    procedure HandlerDisplayException(E: Exception; Component: TComponent; Elem: TBoldElement);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // TBoldFollower.Display exception contract:
    // - no TBoldExceptionHandler handles it -> exception must propagate
    // - a TBoldExceptionHandler handled it  -> exception must be swallowed
    [Test]
    [Category('Quick')]
    procedure TestDisplayExceptionPropagatesWhenUnhandled;
    [Test]
    [Category('Quick')]
    procedure TestDisplayExceptionSwallowedWhenHandled;
  end;

implementation

{ TTestBoldControlPack }

function TTestBoldControlPack.RaisingGetAsString(aFollower: TBoldFollower): string;
begin
  // Only raise inside the test body; Setup/TearDown also exercise the renderer
  // (SetElementAndMakeCurrent, follower cleanup) and must not raise there.
  if FRaiseInGetAsString then
    raise Exception.Create('Renderer failure');
  Result := 'ok';
end;

procedure TTestBoldControlPack.HandlerDisplayException(E: Exception; Component: TComponent; Elem: TBoldElement);
begin
  FHandlerCalled := True;
end;

procedure TTestBoldControlPack.SetUp;
begin
  FHandlerCalled := False;
  FRaiseInGetAsString := False;
  FOwnerForm := TComponent.Create(nil);
  FOwningComponent := TComponent.Create(FOwnerForm);
  FRenderer := TBoldAsStringRenderer.Create(FOwnerForm);
  FRenderer.OnGetAsString := RaisingGetAsString;
  FController := TBoldStringFollowerController.Create(FOwningComponent);
  FController.Renderer := FRenderer;
  FElement := TBAString.Create;
  FElement.AsString := 'value';
  FFollower := TBoldFollower.Create(FOwningComponent, FController);
  FFollower.SetElementAndMakeCurrent(FElement, True);
end;

procedure TTestBoldControlPack.TearDown;
begin
  FRaiseInGetAsString := False;
  FreeAndNil(FFollower);
  FreeAndNil(FController);
  FreeAndNil(FElement);
  FreeAndNil(FOwnerForm); // also frees FOwningComponent, FRenderer and any handler
end;

procedure TTestBoldControlPack.TestDisplayExceptionPropagatesWhenUnhandled;
var
  Raised: Boolean;
begin
  // No TBoldExceptionHandler exists, so HandleDisplayException returns False.
  // The display exception must reach the caller (decorated with the component path).
  Raised := False;
  FRaiseInGetAsString := True;
  FFollower.MarkValueOutOfDate;
  try
    FFollower.Display;
  except
    on E: Exception do
    begin
      Raised := True;
      Assert.Contains(E.Message, 'Renderer failure',
        'The original exception message must be preserved');
    end;
  end;
  Assert.IsTrue(Raised,
    'Display must re-raise a display exception when no exception handler is present');
end;

procedure TTestBoldControlPack.TestDisplayExceptionSwallowedWhenHandled;
var
  Handler: TBoldExceptionHandler;
begin
  // A TBoldExceptionHandler owned by the same owner as the controller's
  // owning component handles the exception -> Display must NOT re-raise.
  Handler := TBoldExceptionHandler.Create(FOwnerForm);
  Handler.OnDisplayException := HandlerDisplayException;

  FRaiseInGetAsString := True;
  FFollower.MarkValueOutOfDate;
  try
    FFollower.Display;
  except
    on E: Exception do
      Assert.Fail('Display must not re-raise when the exception handler handled it, but raised: ' + E.Message);
  end;
  Assert.IsTrue(FHandlerCalled, 'The OnDisplayException handler must have been invoked');
end;

end.
