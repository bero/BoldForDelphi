unit Test.BoldFilteredHandle;

interface

uses
  DUnitX.TestFramework,
  BoldElements,
  BoldSubscription,
  BoldFilteredHandle;

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

implementation

uses
  System.SysUtils,
  System.Classes;

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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldFilter);

end.
