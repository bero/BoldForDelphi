unit Test.BoldFreeStandingValues;

interface

uses
  DUnitX.TestFramework,
  BoldFreeStandingValues,
  BoldId;

type
  [TestFixture]
  [Category('FreeStandingValues')]
  TTestBoldFreeStandingValues = class
  public
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId1Empty;
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId2Empty;
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId1WithOneId;
    [Test]
    [Category('Quick')]
    procedure TestObjectIdRefPairGetId1And2WithTwoIds;
  end;

implementation

{ TTestBoldFreeStandingValues }

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId1Empty;
var
  IdRefPair: TBFSObjectIdRefPair;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    // Empty pair should return nil for Id1
    Assert.IsNull(IdRefPair.Id1, 'Id1 should be nil when no IDs set');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId2Empty;
var
  IdRefPair: TBFSObjectIdRefPair;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    // Empty pair should return nil for Id2
    Assert.IsNull(IdRefPair.Id2, 'Id2 should be nil when no IDs set');
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId1WithOneId;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1: TBoldInternalObjectId;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    try
      IdRefPair.SetFromIds(Id1, nil);
      Assert.IsNotNull(IdRefPair.Id1, 'Id1 should not be nil');
      Assert.IsNull(IdRefPair.Id2, 'Id2 should be nil when only Id1 set');
    finally
      Id1.Free;
    end;
  finally
    IdRefPair.Free;
  end;
end;

procedure TTestBoldFreeStandingValues.TestObjectIdRefPairGetId1And2WithTwoIds;
var
  IdRefPair: TBFSObjectIdRefPair;
  Id1, Id2: TBoldInternalObjectId;
begin
  IdRefPair := TBFSObjectIdRefPair.Create;
  try
    Id1 := TBoldInternalObjectId.CreateWithClassId(1, False);
    Id2 := TBoldInternalObjectId.CreateWithClassId(2, False);
    try
      IdRefPair.SetFromIds(Id1, Id2);
      Assert.IsNotNull(IdRefPair.Id1, 'Id1 should not be nil');
      Assert.IsNotNull(IdRefPair.Id2, 'Id2 should not be nil');
    finally
      Id1.Free;
      Id2.Free;
    end;
  finally
    IdRefPair.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldFreeStandingValues);

end.
