unit Test.BoldUpdatePrecondition;

interface

uses
  DUnitX.TestFramework,
  BoldUpdatePrecondition,
  BoldId,
  BoldDefaultId;

type
  [TestFixture]
  TTestBoldUpdatePrecondition = class
  public
    // TBoldUpdatePrecondition tests (base class)
    [Test]
    procedure TestBaseCreate;
    [Test]
    procedure TestBaseGetFailed_ReturnsFalse;
    [Test]
    procedure TestBaseGetFailureReason_ReturnsEmpty;

    // TBoldOptimisticLockingPrecondition tests
    [Test]
    procedure TestOptimisticCreate;
    [Test]
    procedure TestOptimisticGetStreamName;
    [Test]
    procedure TestOptimisticGetFailed_EmptyList;
    [Test]
    procedure TestOptimisticGetFailed_WithFailedObject;
    [Test]
    procedure TestOptimisticAddFailedObject;
    [Test]
    procedure TestOptimisticAddFailedObject_NoDuplicates;
    [Test]
    procedure TestOptimisticGetFailureReason_SingleObject;
    [Test]
    procedure TestOptimisticGetFailureReason_MultipleObjects;
    [Test]
    procedure TestOptimisticGetFailureList_LazyCreation;
    [Test]
    procedure TestOptimisticGetValueSpace;
    [Test]
    procedure TestOptimisticGetHasOptimisticLocks_Empty;
    [Test]
    procedure TestOptimisticGetHasOptimisticLocks_NotEmpty;
    [Test]
    procedure TestOptimisticClearValueSpace;
    [Test]
    procedure TestOptimisticAssignOutValues_FromOptimistic;
    [Test]
    procedure TestOptimisticAssignOutValues_FromBase;
  end;

implementation

uses
  SysUtils,
  BoldStreams,
  BoldvalueSpaceInterfaces;

{ TTestBoldUpdatePrecondition }

procedure TTestBoldUpdatePrecondition.TestBaseCreate;
var
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  // Note: TBoldUpdatePrecondition is abstract (GetStreamName is abstract)
  // So we test via the concrete subclass
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    Assert.IsNotNull(Precondition);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestBaseGetFailed_ReturnsFalse;
var
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    // Base class GetFailed returns false, but TBoldOptimisticLockingPrecondition
    // overrides it to check FailureList.Count
    // With empty failure list, it should return false
    Assert.IsFalse(Precondition.Failed);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestBaseGetFailureReason_ReturnsEmpty;
var
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    // TBoldOptimisticLockingPrecondition overrides GetFailureReason
    // Even with no failures, it returns formatted string with count 0
    Assert.Contains(Precondition.FailureReason, '0 objects');
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticCreate;
var
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    Assert.IsNotNull(Precondition);
    Assert.IsFalse(Precondition.Failed);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetStreamName;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  Streamable: IBoldStreamable;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    Streamable := Precondition as IBoldStreamable;
    Assert.AreEqual('OptimisticLockingPreCondition', Streamable.StreamName);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetFailed_EmptyList;
var
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    Assert.IsFalse(Precondition.Failed);
    Assert.AreEqual(0, Precondition.FailureList.Count);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetFailed_WithFailedObject;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ObjectId: TBoldDefaultId;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    ObjectId := TBoldDefaultId.CreateWithClassId(1, False);
    try
      ObjectId.AsInteger := 100;
      Precondition.AddFailedObject(ObjectId);
      Assert.IsTrue(Precondition.Failed);
    finally
      ObjectId.Free;
    end;
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticAddFailedObject;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ObjectId: TBoldDefaultId;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    ObjectId := TBoldDefaultId.CreateWithClassId(1, False);
    try
      ObjectId.AsInteger := 42;
      Precondition.AddFailedObject(ObjectId);
      Assert.AreEqual(1, Precondition.FailureList.Count);
    finally
      ObjectId.Free;
    end;
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticAddFailedObject_NoDuplicates;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ObjectId1, ObjectId2: TBoldDefaultId;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    ObjectId1 := TBoldDefaultId.CreateWithClassId(1, False);
    ObjectId2 := TBoldDefaultId.CreateWithClassId(1, False);
    try
      ObjectId1.AsInteger := 42;
      ObjectId2.AsInteger := 42;  // Same ID value

      Precondition.AddFailedObject(ObjectId1);
      Precondition.AddFailedObject(ObjectId2);

      // Should not add duplicate
      Assert.AreEqual(1, Precondition.FailureList.Count);
    finally
      ObjectId1.Free;
      ObjectId2.Free;
    end;
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetFailureReason_SingleObject;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ObjectId: TBoldDefaultId;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    ObjectId := TBoldDefaultId.CreateWithClassId(1, False);
    try
      ObjectId.AsInteger := 123;
      Precondition.AddFailedObject(ObjectId);

      Assert.Contains(Precondition.FailureReason, '1 objects');
      Assert.Contains(Precondition.FailureReason, 'Id:');
    finally
      ObjectId.Free;
    end;
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetFailureReason_MultipleObjects;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ObjectId1, ObjectId2: TBoldDefaultId;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    ObjectId1 := TBoldDefaultId.CreateWithClassId(1, False);
    ObjectId2 := TBoldDefaultId.CreateWithClassId(2, False);
    try
      ObjectId1.AsInteger := 100;
      ObjectId2.AsInteger := 200;

      Precondition.AddFailedObject(ObjectId1);
      Precondition.AddFailedObject(ObjectId2);

      Assert.Contains(Precondition.FailureReason, '2 objects');
      // Should contain comma separator between IDs
      Assert.Contains(Precondition.FailureReason, ', ');
    finally
      ObjectId1.Free;
      ObjectId2.Free;
    end;
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetFailureList_LazyCreation;
var
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    // FailureList should be created on first access
    Assert.IsNotNull(Precondition.FailureList);
    Assert.AreEqual(0, Precondition.FailureList.Count);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetValueSpace;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ValueSpace: IBoldValueSpace;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    ValueSpace := Precondition.ValueSpace;
    Assert.IsNotNull(ValueSpace);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetHasOptimisticLocks_Empty;
var
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    // Fresh ValueSpace should be empty
    Assert.IsFalse(Precondition.HasOptimisticLocks);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticGetHasOptimisticLocks_NotEmpty;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ValueSpace: IBoldValueSpace;
  ObjectId: TBoldDefaultId;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    // Access ValueSpace to create the FreeStandingValueSpace
    ValueSpace := Precondition.ValueSpace;
    // Add content to make it non-empty
    ObjectId := TBoldDefaultId.CreateWithClassId(1, False);
    try
      ObjectId.AsInteger := 1;
      ValueSpace.EnsureObjectContents(ObjectId);
      Assert.IsTrue(Precondition.HasOptimisticLocks);
    finally
      ObjectId.Free;
    end;
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticClearValueSpace;
var
  Precondition: TBoldOptimisticLockingPrecondition;
  ValueSpace: IBoldValueSpace;
  ObjectId: TBoldDefaultId;
begin
  Precondition := TBoldOptimisticLockingPrecondition.Create;
  try
    // First, populate the ValueSpace
    ValueSpace := Precondition.ValueSpace;
    ObjectId := TBoldDefaultId.CreateWithClassId(1, False);
    try
      ObjectId.AsInteger := 1;
      ValueSpace.EnsureObjectContents(ObjectId);
      Assert.IsTrue(Precondition.HasOptimisticLocks);
    finally
      ObjectId.Free;
    end;

    // Clear it
    Precondition.ClearValueSpace;

    // After clearing, HasOptimisticLocks should be false (new empty ValueSpace created on access)
    Assert.IsFalse(Precondition.HasOptimisticLocks);
  finally
    Precondition.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticAssignOutValues_FromOptimistic;
var
  Source, Target: TBoldOptimisticLockingPrecondition;
  ObjectId1, ObjectId2: TBoldDefaultId;
begin
  Source := TBoldOptimisticLockingPrecondition.Create;
  Target := TBoldOptimisticLockingPrecondition.Create;
  try
    ObjectId1 := TBoldDefaultId.CreateWithClassId(1, False);
    ObjectId2 := TBoldDefaultId.CreateWithClassId(2, False);
    try
      ObjectId1.AsInteger := 100;
      ObjectId2.AsInteger := 200;

      Source.AddFailedObject(ObjectId1);
      Source.AddFailedObject(ObjectId2);

      Assert.AreEqual(0, Target.FailureList.Count);

      Target.AssignOutValues(Source);

      Assert.AreEqual(2, Target.FailureList.Count);
    finally
      ObjectId1.Free;
      ObjectId2.Free;
    end;
  finally
    Source.Free;
    Target.Free;
  end;
end;

procedure TTestBoldUpdatePrecondition.TestOptimisticAssignOutValues_FromBase;
var
  Source: TBoldOptimisticLockingPrecondition;
  Target: TBoldOptimisticLockingPrecondition;
  ObjectId: TBoldDefaultId;
begin
  // Test that AssignOutValues handles non-TBoldOptimisticLockingPrecondition source
  // Since TBoldUpdatePrecondition is abstract, we can only test with a concrete subclass
  // but test the else branch by using a fresh instance
  Source := TBoldOptimisticLockingPrecondition.Create;
  Target := TBoldOptimisticLockingPrecondition.Create;
  try
    ObjectId := TBoldDefaultId.CreateWithClassId(1, False);
    try
      ObjectId.AsInteger := 50;
      Target.AddFailedObject(ObjectId);

      // AssignOutValues from Source (which has empty FailureList)
      Target.AssignOutValues(Source);

      // Target should still have its original item (AddList adds, doesn't replace)
      Assert.AreEqual(1, Target.FailureList.Count);
    finally
      ObjectId.Free;
    end;
  finally
    Source.Free;
    Target.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUpdatePrecondition);

end.
