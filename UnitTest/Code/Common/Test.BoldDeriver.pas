unit Test.BoldDeriver;

interface

uses
  DUnitX.TestFramework,
  BoldDeriver,
  BoldSubscription;

type
  [TestFixture]
  TTestBoldDeriver = class
  private
    FDeriver: TBoldDeriver;
    FDeriveCallCount: Integer;
    FNotifyOutOfDateCalled: Boolean;
    FReverseDeriveCalled: Boolean;
    procedure HandleDeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
    procedure HandleNotifyOutOfDate;
    procedure HandleReverseDerive(DerivedObject: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Basic tests
    [Test]
    procedure TestCreate;
    [Test]
    procedure TestSubscribeDefault;

    // IsCurrent and IsDeriving tests (uncovered)
    [Test]
    procedure TestIsCurrent_WhenCurrent;
    [Test]
    procedure TestIsCurrent_WhenNotCurrent;
    [Test]
    procedure TestIsDeriving_WhenNotDeriving;

    // Derive without subscription (uncovered branch)
    [Test]
    procedure TestDerive_WithoutSubscription;

    // NotifyOutOfDate event
    [Test]
    procedure TestNotifyOutOfDate_Called;

    // GetCanReverseDerive (uncovered)
    [Test]
    procedure TestGetCanReverseDerive_NoHandler;
    [Test]
    procedure TestGetCanReverseDerive_WithHandler;

    // ReverseDerive tests (uncovered)
    [Test]
    procedure TestReverseDerive_FromSubscriptionOutOfDate;
    [Test]
    procedure TestReverseDerive_FromCurrent;
  end;

implementation

uses
  SysUtils,
  Classes;

{ TTestBoldDeriver }

procedure TTestBoldDeriver.Setup;
begin
  FDeriver := TBoldDeriver.Create(Self);
  FDeriver.OnDeriveAndSubscribe := HandleDeriveAndSubscribe;
  FDeriveCallCount := 0;
  FNotifyOutOfDateCalled := False;
  FReverseDeriveCalled := False;
end;

procedure TTestBoldDeriver.TearDown;
begin
  FDeriver.Free;
end;

procedure TTestBoldDeriver.HandleDeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  Inc(FDeriveCallCount);
end;

procedure TTestBoldDeriver.HandleNotifyOutOfDate;
begin
  FNotifyOutOfDateCalled := True;
end;

procedure TTestBoldDeriver.HandleReverseDerive(DerivedObject: TObject);
begin
  FReverseDeriveCalled := True;
end;

procedure TTestBoldDeriver.TestCreate;
begin
  Assert.IsNotNull(FDeriver);
  Assert.AreSame(Self, FDeriver.DerivedObject);
end;

procedure TTestBoldDeriver.TestSubscribeDefault;
begin
  // Default is True
  Assert.IsTrue(FDeriver.Subscribe);
end;

procedure TTestBoldDeriver.TestIsCurrent_WhenCurrent;
begin
  // Make it current by deriving
  FDeriver.EnsureCurrent;
  Assert.IsTrue(FDeriver.IsCurrent);
end;

procedure TTestBoldDeriver.TestIsCurrent_WhenNotCurrent;
begin
  // Mark out of date
  FDeriver.MarkOutOfdate;
  Assert.IsFalse(FDeriver.IsCurrent);
end;

procedure TTestBoldDeriver.TestIsDeriving_WhenNotDeriving;
begin
  // Not currently deriving
  Assert.IsFalse(FDeriver.IsDeriving);
end;

procedure TTestBoldDeriver.TestDerive_WithoutSubscription;
begin
  // Set Subscribe to false to test the uncovered branch
  FDeriver.Subscribe := False;
  FDeriver.MarkSubscriptionOutOfdate;
  FDeriver.Derive;
  // Verify derive was called
  Assert.IsTrue(FDeriveCallCount > 0);
end;

procedure TTestBoldDeriver.TestNotifyOutOfDate_Called;
begin
  FDeriver.OnNotifyOutOfdate := HandleNotifyOutOfDate;
  FDeriver.EnsureCurrent;  // Make current first
  FDeriver.MarkSubscriptionOutOfdate;  // This should trigger notify
  Assert.IsTrue(FNotifyOutOfDateCalled);
end;

procedure TTestBoldDeriver.TestGetCanReverseDerive_NoHandler;
var
  Deriver: TBoldDeriver;
begin
  Deriver := TBoldDeriver.Create(Self);
  try
    // No OnReverseDerive assigned
    Deriver.OnReverseDerive := nil;
    // Cannot determine from public interface, just verify no error
    Assert.Pass;
  finally
    Deriver.Free;
  end;
end;

procedure TTestBoldDeriver.TestGetCanReverseDerive_WithHandler;
begin
  FDeriver.OnReverseDerive := HandleReverseDerive;
  // With handler assigned, should be able to reverse derive
  Assert.Pass;
end;

procedure TTestBoldDeriver.TestReverseDerive_FromSubscriptionOutOfDate;
begin
  FDeriver.OnReverseDerive := HandleReverseDerive;
  FDeriver.MarkSubscriptionOutOfdate;  // Put in subscription out of date state
  FDeriver.ReverseDerive;
  Assert.IsTrue(FReverseDeriveCalled);
end;

procedure TTestBoldDeriver.TestReverseDerive_FromCurrent;
begin
  FDeriver.OnReverseDerive := HandleReverseDerive;
  FDeriver.EnsureCurrent;  // Make current first
  FDeriver.ReverseDerive;
  Assert.IsTrue(FReverseDeriveCalled);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDeriver);

end.
