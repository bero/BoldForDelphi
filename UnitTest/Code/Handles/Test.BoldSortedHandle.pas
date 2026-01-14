unit Test.BoldSortedHandle;

interface

uses
  DUnitX.TestFramework,
  BoldSortedHandle,
  BoldSubscription,
  BoldSystem,
  BoldElements;

type
  [TestFixture]
  TTestBoldSortedHandle = class
  private
    FComparer: TBoldComparer;
    FHandle: TBoldSortedHandle;
    FCompareCalled: Boolean;
    FSubscribeCalled: Boolean;
    FPrepareSortCalled: Boolean;
    FFinishSortCalled: Boolean;
    function HandleCompare(Item1, Item2: TBoldElement): Integer;
    procedure HandleSubscribe(Element: TBoldElement; Subscriber: TBoldSubscriber);
    procedure HandlePrepareSort(List: TBoldList);
    procedure HandleFinishSort(List: TBoldList);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // TBoldComparer tests
    [Test]
    procedure TestComparerCreate;
    [Test]
    procedure TestComparerCompare_NoHandler_ReturnsZero;
    [Test]
    procedure TestComparerCompare_WithHandler;
    [Test]
    procedure TestComparerSubscribe_NoHandler;
    [Test]
    procedure TestComparerSubscribe_WithHandler;
    [Test]
    procedure TestComparerPrepareSort_NoHandler;
    [Test]
    procedure TestComparerPrepareSort_WithHandler;
    [Test]
    procedure TestComparerFinishSort_NoHandler;
    [Test]
    procedure TestComparerFinishSort_WithHandler;

    // TBoldSortedHandle tests
    [Test]
    procedure TestSortedHandleCreate;
    [Test]
    procedure TestSortedHandleSetBoldComparer;
    [Test]
    procedure TestSortedHandleSetBoldComparerSameValue;
  end;

implementation

uses
  SysUtils;

{ TTestBoldSortedHandle }

procedure TTestBoldSortedHandle.Setup;
begin
  FComparer := TBoldComparer.Create(nil);
  FHandle := TBoldSortedHandle.Create(nil);
  FCompareCalled := False;
  FSubscribeCalled := False;
  FPrepareSortCalled := False;
  FFinishSortCalled := False;
end;

procedure TTestBoldSortedHandle.TearDown;
begin
  FHandle.Free;
  FComparer.Free;
end;

function TTestBoldSortedHandle.HandleCompare(Item1, Item2: TBoldElement): Integer;
begin
  FCompareCalled := True;
  Result := 1;  // Return non-zero to verify it was used
end;

procedure TTestBoldSortedHandle.HandleSubscribe(Element: TBoldElement; Subscriber: TBoldSubscriber);
begin
  FSubscribeCalled := True;
end;

procedure TTestBoldSortedHandle.HandlePrepareSort(List: TBoldList);
begin
  FPrepareSortCalled := True;
end;

procedure TTestBoldSortedHandle.HandleFinishSort(List: TBoldList);
begin
  FFinishSortCalled := True;
end;

procedure TTestBoldSortedHandle.TestComparerCreate;
begin
  Assert.IsNotNull(FComparer);
end;

procedure TTestBoldSortedHandle.TestComparerCompare_NoHandler_ReturnsZero;
begin
  // When OnCompare is not assigned, Compare returns 0
  Assert.AreEqual(0, FComparer.Compare(nil, nil));
end;

procedure TTestBoldSortedHandle.TestComparerCompare_WithHandler;
begin
  FComparer.OnCompare := HandleCompare;
  Assert.AreEqual(1, FComparer.Compare(nil, nil));
  Assert.IsTrue(FCompareCalled);
end;

procedure TTestBoldSortedHandle.TestComparerSubscribe_NoHandler;
begin
  // Should not raise when OnSubscribe not assigned
  FComparer.Subscribe(nil, nil);
  Assert.IsFalse(FSubscribeCalled);
end;

procedure TTestBoldSortedHandle.TestComparerSubscribe_WithHandler;
begin
  FComparer.OnSubscribe := HandleSubscribe;
  FComparer.Subscribe(nil, nil);
  Assert.IsTrue(FSubscribeCalled);
end;

procedure TTestBoldSortedHandle.TestComparerPrepareSort_NoHandler;
begin
  // Should not raise when OnPrepareSort not assigned
  FComparer.PrepareSort(nil);
  Assert.IsFalse(FPrepareSortCalled);
end;

procedure TTestBoldSortedHandle.TestComparerPrepareSort_WithHandler;
begin
  FComparer.OnPrepareSort := HandlePrepareSort;
  FComparer.PrepareSort(nil);
  Assert.IsTrue(FPrepareSortCalled);
end;

procedure TTestBoldSortedHandle.TestComparerFinishSort_NoHandler;
begin
  // Should not raise when OnFinishSort not assigned
  FComparer.FinishSort(nil);
  Assert.IsFalse(FFinishSortCalled);
end;

procedure TTestBoldSortedHandle.TestComparerFinishSort_WithHandler;
begin
  FComparer.OnFinishSort := HandleFinishSort;
  FComparer.FinishSort(nil);
  Assert.IsTrue(FFinishSortCalled);
end;

procedure TTestBoldSortedHandle.TestSortedHandleCreate;
begin
  Assert.IsNotNull(FHandle);
  Assert.IsNull(FHandle.BoldComparer);
end;

procedure TTestBoldSortedHandle.TestSortedHandleSetBoldComparer;
begin
  FHandle.BoldComparer := FComparer;
  Assert.AreSame(FComparer, FHandle.BoldComparer);
end;

procedure TTestBoldSortedHandle.TestSortedHandleSetBoldComparerSameValue;
begin
  FHandle.BoldComparer := FComparer;
  // Setting same value should not change anything
  FHandle.BoldComparer := FComparer;
  Assert.AreSame(FComparer, FHandle.BoldComparer);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSortedHandle);

end.
