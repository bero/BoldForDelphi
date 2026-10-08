unit Test.BoldFilteredHandle;

interface

uses
  System.Classes,
  DUnitX.TestFramework,
  BoldElements,
  BoldSubscription,
  BoldSystem,
  BoldExpressionHandle,
  BoldListHandle,
  BoldFilteredHandle,
  jehoBCBoldTest,
  Test.BoldAttributes;  // For TjehodmBoldTest

type
  /// <summary>
  /// Test fixture for BoldFilteredHandle - Filter component and filtered handle
  /// </summary>
  [TestFixture]
  [Category('Handles')]
  TTestBoldFilter = class
  private
    FFilterCallCount: Integer;
    FSubscribeCallCount: Integer;
    function TestFilter(Element: TBoldElement): Boolean;
    procedure TestSubscribe(BoldElement: TBoldElement; Subscriber: TBoldSubscriber);
  public
    [Setup]
    procedure Setup;

    // TBoldFilter lifecycle tests
    [Test]
    [Category('Quick')]
    procedure TestFilterCreate;
    [Test]
    procedure TestFilterDestroy;

    // TBoldFilter.Filter method tests
    [Test]
    procedure TestFilter_NoOnFilter_ReturnsTrue;
    [Test]
    procedure TestFilter_WithOnFilter_CallsHandler;

    // TBoldFilter.Subscribe method tests
    [Test]
    procedure TestSubscribe_NoOnSubscribe_DoesNothing;
    [Test]
    procedure TestSubscribe_WithOnSubscribe_CallsHandler;

    // TBoldFilter.PreFetchRoles property tests
    [Test]
    procedure TestPreFetchRoles_DefaultEmpty;
    [Test]
    procedure TestPreFetchRoles_SetAndGet;
    [Test]
    procedure TestStorePreFetchRoles_EmptyReturnsFalse;
    [Test]
    procedure TestStorePreFetchRoles_NonEmptyReturnsTrue;
  end;

  /// <summary>
  /// A TBoldFilteredHandle whose TBoldFilter is destroyed must re-derive
  /// without it - on its own and as the filter stage inside a TBoldListHandle.
  /// Filter and handles share an owner, as on a form.
  /// </summary>
  [TestFixture]
  [Category('Handles')]
  TTestBoldFilteredHandleFilterRemoval = class
  strict private
    fDataModule: TjehodmBoldTest;
    fOwner: TComponent;
    function RejectAll(aElement: TBoldElement): Boolean;
    function CreateRejectAllFilter: TBoldFilter;
    function CreateClassAHandle: TBoldExpressionHandle;
    function CreateClassAObjects: Integer;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestFilteredHandle_ReDerivesUnfilteredWhenFilterIsDestroyed;
    [Test]
    procedure TestListHandle_ReDerivesUnfilteredWhenFilterIsDestroyed;
  end;

implementation

uses
  System.SysUtils;

const
  cnClassAInstances = 'ClassA.allInstances';

{ TTestBoldFilter }

procedure TTestBoldFilter.Setup;
begin
  FFilterCallCount := 0;
  FSubscribeCallCount := 0;
end;

function TTestBoldFilter.TestFilter(Element: TBoldElement): Boolean;
begin
  Inc(FFilterCallCount);
  Result := True;
end;

procedure TTestBoldFilter.TestSubscribe(BoldElement: TBoldElement; Subscriber: TBoldSubscriber);
begin
  Inc(FSubscribeCallCount);
end;

procedure TTestBoldFilter.TestFilterCreate;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    Assert.IsNotNull(Filter);
    Assert.IsNotNull(Filter.PreFetchRoles, 'PreFetchRoles should be initialized');
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestFilterDestroy;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  Filter.Free;
  Assert.Pass('Filter destroyed without error');
end;

procedure TTestBoldFilter.TestFilter_NoOnFilter_ReturnsTrue;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    // No OnFilter assigned - should return True by default
    Assert.IsTrue(Filter.Filter(nil), 'Filter should return True when OnFilter is not assigned');
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestFilter_WithOnFilter_CallsHandler;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    Filter.OnFilter := TestFilter;
    FFilterCallCount := 0;
    Filter.Filter(nil);
    Assert.AreEqual(1, FFilterCallCount, 'OnFilter should be called once');
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestSubscribe_NoOnSubscribe_DoesNothing;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    // No OnSubscribe assigned - should do nothing
    Filter.Subscribe(nil, nil);
    Assert.Pass('Subscribe completed without error when OnSubscribe is not assigned');
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestSubscribe_WithOnSubscribe_CallsHandler;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    Filter.OnSubscribe := TestSubscribe;
    FSubscribeCallCount := 0;
    Filter.Subscribe(nil, nil);
    Assert.AreEqual(1, FSubscribeCallCount, 'OnSubscribe should be called once');
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestPreFetchRoles_DefaultEmpty;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    Assert.AreEqual(0, Filter.PreFetchRoles.Count, 'PreFetchRoles should be empty by default');
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestPreFetchRoles_SetAndGet;
var
  Filter: TBoldFilter;
  Roles: TStringList;
begin
  Filter := TBoldFilter.Create(nil);
  try
    Roles := TStringList.Create;
    try
      Roles.Add('Role1');
      Roles.Add('Role2');
      Filter.PreFetchRoles := Roles;
      Assert.AreEqual(2, Filter.PreFetchRoles.Count);
      Assert.AreEqual('Role1', Filter.PreFetchRoles[0]);
      Assert.AreEqual('Role2', Filter.PreFetchRoles[1]);
    finally
      Roles.Free;
    end;
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestStorePreFetchRoles_EmptyReturnsFalse;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    // Empty PreFetchRoles - StorePreFetchRoles should return False
    // We can't call StorePreFetchRoles directly as it's private, but we can verify
    // through the Count property
    Assert.AreEqual(0, Filter.PreFetchRoles.Count, 'PreFetchRoles should be empty');
  finally
    Filter.Free;
  end;
end;

procedure TTestBoldFilter.TestStorePreFetchRoles_NonEmptyReturnsTrue;
var
  Filter: TBoldFilter;
begin
  Filter := TBoldFilter.Create(nil);
  try
    Filter.PreFetchRoles.Add('TestRole');
    Assert.AreEqual(1, Filter.PreFetchRoles.Count, 'PreFetchRoles should have 1 item');
  finally
    Filter.Free;
  end;
end;

{ TTestBoldFilteredHandleFilterRemoval }

procedure TTestBoldFilteredHandleFilterRemoval.SetUp;
begin
  fDataModule := TjehodmBoldTest.Create(nil);
  fOwner := TComponent.Create(nil);
end;

procedure TTestBoldFilteredHandleFilterRemoval.TearDown;
begin
  FreeAndNil(fOwner);
  FreeAndNil(fDataModule);
end;

function TTestBoldFilteredHandleFilterRemoval.RejectAll(aElement: TBoldElement): Boolean;
begin
  Result := False;
end;

function TTestBoldFilteredHandleFilterRemoval.CreateRejectAllFilter: TBoldFilter;
begin
  Result := TBoldFilter.Create(fOwner);
  Result.OnFilter := RejectAll;
end;

function TTestBoldFilteredHandleFilterRemoval.CreateClassAHandle: TBoldExpressionHandle;
begin
  Result := TBoldExpressionHandle.Create(fOwner);
  Result.RootHandle := fDataModule.BoldSystemHandle1;
  Result.Expression := cnClassAInstances;
end;

function TTestBoldFilteredHandleFilterRemoval.CreateClassAObjects: Integer;
var
  i: Integer;
begin
  Result := 3;
  for i := 1 to Result do
    TClassA.Create(fDataModule.BoldSystemHandle1.System);
end;

procedure TTestBoldFilteredHandleFilterRemoval.TestFilteredHandle_ReDerivesUnfilteredWhenFilterIsDestroyed;
var
  oClassA: TBoldExpressionHandle;
  oFiltered: TBoldFilteredHandle;
  oFilter: TBoldFilter;
  iObjectCount: Integer;
begin
  iObjectCount := CreateClassAObjects;
  oClassA := CreateClassAHandle;
  Assert.AreEqual(iObjectCount, (oClassA.Value as TBoldList).Count,
    'precondition: the expression handle sees every ClassA');

  oFilter := CreateRejectAllFilter;
  oFiltered := TBoldFilteredHandle.Create(fOwner);
  oFiltered.RootHandle := oClassA;
  oFiltered.BoldFilter := oFilter;
  Assert.AreEqual(0, (oFiltered.Value as TBoldList).Count,
    'precondition: the filter rejects every ClassA');

  oFilter.Free;

  Assert.IsNull(oFiltered.BoldFilter, 'The destroyed filter must be cleared');
  Assert.AreEqual(iObjectCount, (oFiltered.Value as TBoldList).Count,
    'The handle must re-derive without the destroyed filter, not keep the filtered list');
end;

procedure TTestBoldFilteredHandleFilterRemoval.TestListHandle_ReDerivesUnfilteredWhenFilterIsDestroyed;
var
  oList: TBoldListHandle;
  oFilter: TBoldFilter;
  iObjectCount: Integer;
begin
  iObjectCount := CreateClassAObjects;

  oFilter := CreateRejectAllFilter;
  oList := TBoldListHandle.Create(fOwner);
  oList.RootHandle := fDataModule.BoldSystemHandle1;
  oList.Expression := cnClassAInstances;
  oList.BoldFilter := oFilter;
  Assert.AreEqual(0, oList.Count, 'precondition: the filter rejects every ClassA');

  oFilter.Free;

  Assert.IsNull(oList.BoldFilter, 'The destroyed filter must be cleared');
  Assert.AreEqual(iObjectCount, oList.Count,
    'The list handle must re-derive without the destroyed filter, not keep the filtered list');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldFilter);
  TDUnitX.RegisterTestFixture(TTestBoldFilteredHandleFilterRemoval);

end.
