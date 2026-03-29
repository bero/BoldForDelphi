unit Test.BoldUndoHandler;

{ DUnitX tests for BoldUndoHandler - Undo/Redo functionality }

interface

uses
  SysUtils,
  Classes,
  DUnitX.TestFramework,
  BoldDefs,
  BoldSystem,
  BoldDomainElement,
  BoldAttributes,
  BoldId,  // includes TBoldInternalObjectId
  BoldSubscription,
  BoldUndoHandler,
  BoldUndoInterfaces,
  BoldFreeStandingValues,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldElements,
  BoldHandles,
  BoldSystemHandle,
  BoldModel,
  BoldTestModel;

type
  { Isolated tests for TBoldUndoBlockList and TBoldUndoBlock - no database required }
  [TestFixture]
  [Category('UndoHandler')]
  TTestBoldUndoBlockListIsolated = class
  private
    FBlockList: TBoldUndoBlockList;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // TBoldUndoBlockList tests
    [Test]
    procedure TestBlockListCountInitiallyZero;
    [Test]
    procedure TestBlockListIsEmptyInitially;
    [Test]
    procedure TestAddBlockIncreasesCount;
    [Test]
    procedure TestGetBlockByIndex;
    [Test]
    procedure TestGetBlockByName;
    [Test]
    procedure TestIndexOfByName;
    [Test]
    procedure TestIndexOfByBlock;
    [Test]
    procedure TestIndexOfNonExistentReturnsMinusOne;
    [Test]
    procedure TestRemoveBlock;
    [Test]
    procedure TestRenameBlock;
    [Test]
    procedure TestClearRemovesAllBlocks;
    [Test]
    procedure TestGetContainsChangesWhenEmpty;
    [Test]
    procedure TestGetContainsChangesWithEmptyBlock;
    [Test]
    procedure TestAddBlockDuplicateNameRaises;
    [Test]
    procedure TestGetAssertedBlockByNameRaisesWhenNotFound;
    [Test]
    procedure TestGetAssertedBlockByIndexRaisesWhenOutOfRange;
    [Test]
    procedure TestAssertedIndexOfRaisesWhenNotFound;
    [Test]
    procedure TestMergeAllMergesBlocks;
    [Test]
    procedure TestMoveBlockSameIndex;
    [Test]
    procedure TestCanMoveBlockSameIndex;
    [Test]
    procedure TestCanMergeBlockSameIndex;
    [Test]
    procedure TestRenameBlockToExistingNameRaises;

    // TBoldUndoBlock tests
    [Test]
    procedure TestBlockNameProperty;
    [Test]
    procedure TestBlockCaptionProperty;
    [Test]
    procedure TestBlockCreatedProperty;
    [Test]
    procedure TestBlockContainsChangesInitiallyFalse;
    [Test]
    procedure TestBlockObjectCountInitiallyZero;
    [Test]
    procedure TestBlockGetIndex;
    [Test]
    procedure TestBlockSetFSValueSpace;
    [Test]
    procedure TestBlockValueExistsReturnsFalseWhenEmpty;
    [Test]
    procedure TestBlockGetFSValueSpaceCreatesIfNil;
  end;

  [TestFixture]
  [Category('UndoHandler')]
  TTestBoldUndoHandler = class
  private
    FUndoHandler: TBoldUndoHandler;
    FSomeClassList: TSomeClassList;
    FAPersistentClassList: TAPersistentClassList;
    FATransientClassList: TATransientClassList;
    FFSValueSpace: TBoldFreeStandingValueSpace;
    function GetSystem: TBoldSystem;
    function GetUndoHandler: TBoldUndoHandler;
    procedure RefreshSystem;
    procedure UpdateDatabase;
    // Helper methods for verification
    procedure StoreValue(const Member: TBoldMember);
    function GetStoredValueOfMember(const Member: TBoldMember): IBoldValue;
    procedure VerifyState(const Element: TBoldDomainElement; const State: TBoldValuePersistenceState);
    procedure VerifyIsInUndoArea(aBlock: TBoldUndoBlock; Member: TBoldMember; MemberValue: IBoldValue);
    procedure VerifyIsInRedoArea(Member: TBoldMember; Value: TBoldFreeStandingValue);
    function IdCompare(Item1, Item2: TBoldElement): Integer;
    procedure FetchClassSorted(const aSystem: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
    procedure SetSimpleConfiguration;
    procedure SetTransientConfiguration;
  public
    [SetupFixture]
    procedure SetUpFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // Object creation undo tests
    [Test]
    [Category('Quick')]
    procedure TestNewObjectRecordedWithCorrectExistenceState;

    [Test]
    [Category('Quick')]

    procedure TestDeletedObjectRecordedWithCorrectExistenceState;

    // Attribute modification tests
    [Test]
    [Category('Quick')]
    procedure TestModifyAttributeRecordsOldValue;

    [Test]
    [Category('Quick')]
    procedure TestUndoRestoresAttributeValue;

    // Undo/Redo cycle tests
    [Test]
    [Category('Quick')]

    procedure TestUndoObjectCreation;

    [Test]
    [Category('Quick')]
    procedure TestRedoObjectCreation;

    // Tests migrated from maan_Modify

    [Test]
    [Category('Quick')]
    procedure TestModifyPersistentAttributeCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyPersistentAttributeModified;

    [Test]
    [Category('Quick')]
    procedure TestModifyTransientAttribute;

    [Test]
    [Category('Quick')]

    procedure TestCreatePersistentObjectRecordsUndoState;

    [Test]
    [Category('Quick')]

    procedure TestCreateTransientObjectRecordsUndoState;

    [Test]
    [Category('Quick')]

    procedure TestDeleteTransientObjectRecordsUndoState;

    [Test]
    [Category('Quick')]
    procedure TestModifyEmbeddedRoleCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyEmbeddedRoleTransient;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleInsertCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleDeleteCurrent;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleInsertTransient;

    [Test]
    [Category('Quick')]
    procedure TestModifyNonEmbeddedRoleDeleteTransient;

    // Tests migrated from maan_Undo

    [Test]
    [Category('Quick')]
    procedure TestUndoTransientAttribute;

    [Test]
    [Category('Quick')]
    procedure TestUndoModifiedAttribute;

    [Test]
    [Category('Quick')]
    procedure TestUndoEmbeddedRoleModified;

    [Test]
    [Category('Quick')]
    procedure TestUndoEmbeddedRoleTransient;

    // Additional modify test
    [Test]
    [Category('Quick')]
    procedure TestModifyEmbeddedRoleModified;

    property System: TBoldSystem read GetSystem;
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
  end;

  { Tests for indirect link associations (many-to-many via link class) }
  [TestFixture]
  [Category('UndoHandler')]
  TTestBoldUndoHandlerIndirectLinks = class
  private
    FUndoHandler: TBoldUndoHandler;
    FSomeClassList: TSomeClassList;
    FClassWithLinkList: TClassWithLinkList;
    FFSValueSpace: TBoldFreeStandingValueSpace;
    function GetSystem: TBoldSystem;
    function GetUndoHandler: TBoldUndoHandler;
    procedure RefreshSystem;
    procedure UpdateDatabase;
    function IdCompare(Item1, Item2: TBoldElement): Integer;
    procedure FetchClassSorted(const aSystem: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
    procedure SetConfigurationForIndirectSingle;
    procedure VerifyState(const Element: TBoldDomainElement; const State: TBoldValuePersistenceState);
    procedure VerifyObjectInBlock(Block: TBoldUndoBlock; aId: TBoldObjectId; State: TBoldExistenceState);
  public
    [SetupFixture]
    procedure SetUpFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // Indirect multi-link tests (part/partof via Tpartpartof)
    [Test]
    [Category('Quick')]

    procedure TestIndirectMultiModifyInsertCurrent;

    [Test]
    [Category('Quick')]
    procedure TestIndirectMultiModifyDeleteCurrent;

    [Test]
    [Category('Quick')]

    procedure TestIndirectMultiUndoInsert;

    [Test]
    [Category('Quick')]
    procedure TestIndirectMultiUndoDelete;

    // Indirect single-link tests (one/many via LinkClass)
    [Test]
    [Category('Quick')]

    procedure TestIndirectSingleModifySingleRoleCurrent;

    [Test]
    [Category('Quick')]

    procedure TestIndirectSingleModifyMultiRoleInsertCurrent;

    [Test]
    [Category('Quick')]
    procedure TestIndirectSingleModifyMultiRoleDeleteCurrent;

    [Test]
    [Category('Quick')]

    procedure TestIndirectSingleUndoSingleRoleModified;

    property System: TBoldSystem read GetSystem;
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
  end;

  { Transient tests for TBoldUndoHandler basic API - no database required }
  [TestFixture]
  [Category('UndoHandler')]
  TTestBoldUndoHandlerTransient = class
  private
    FSystemHandle: TBoldSystemHandle;
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    function GetSystem: TBoldSystem;
    function GetUndoHandler: TBoldUndoHandler;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestUndoHandlerExists;
    [Test]
    procedure TestUndoHandlerEnabled;
    [Test]
    procedure TestSetCheckPoint;
  end;

implementation

uses
  dmBoldTest,
  dmModel1,
  maan_UndoRedoTestCaseUtils;

type
  // Cracker class to access protected members for testing
  TBoldUndoBlockListCracker = class(TBoldUndoBlockList);

{ TTestBoldUndoBlockListIsolated }

procedure TTestBoldUndoBlockListIsolated.SetUp;
begin
  // Create block list with nil handler - works for isolated tests
  // that don't need full undo handler functionality
  FBlockList := TBoldUndoBlockList.Create(nil);
end;

procedure TTestBoldUndoBlockListIsolated.TearDown;
begin
  FreeAndNil(FBlockList);
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockListCountInitiallyZero;
begin
  Assert.AreEqual(0, FBlockList.Count, 'Count should be 0 initially');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockListIsEmptyInitially;
begin
  Assert.IsTrue(FBlockList.IsEmpty, 'IsEmpty should be True initially');
end;

procedure TTestBoldUndoBlockListIsolated.TestAddBlockIncreasesCount;
var
  BlockName: string;
begin
  BlockName := 'Block1';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Caption1');
  Assert.AreEqual(1, FBlockList.Count, 'Count should be 1 after adding block');
  Assert.IsFalse(FBlockList.IsEmpty, 'IsEmpty should be False after adding block');
end;

procedure TTestBoldUndoBlockListIsolated.TestGetBlockByIndex;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'TestBlock';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'TestCaption');
  Block := FBlockList.BlockByIndex[0];
  Assert.IsNotNull(Block, 'BlockByIndex[0] should return block');
  Assert.AreEqual('TestBlock', Block.BlockName, 'Block name should match');
end;

procedure TTestBoldUndoBlockListIsolated.TestGetBlockByName;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'NamedBlock';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Caption');
  Block := FBlockList.BlockByName['NamedBlock'];
  Assert.IsNotNull(Block, 'BlockByName should find block');
  Assert.AreEqual('NamedBlock', Block.BlockName, 'Block name should match');
end;

procedure TTestBoldUndoBlockListIsolated.TestIndexOfByName;
var
  BlockName1, BlockName2: string;
begin
  BlockName1 := 'First';
  BlockName2 := 'Second';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName1, 'Cap1');
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName2, 'Cap2');
  Assert.AreEqual(0, FBlockList.IndexOf('First'), 'IndexOf First should be 0');
  Assert.AreEqual(1, FBlockList.IndexOf('Second'), 'IndexOf Second should be 1');
end;

procedure TTestBoldUndoBlockListIsolated.TestIndexOfByBlock;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'BlockForIndex';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.AreEqual(0, FBlockList.IndexOf(Block), 'IndexOf(Block) should return 0');
end;

procedure TTestBoldUndoBlockListIsolated.TestIndexOfNonExistentReturnsMinusOne;
begin
  Assert.AreEqual(-1, FBlockList.IndexOf('NonExistent'), 'IndexOf non-existent should return -1');
end;

procedure TTestBoldUndoBlockListIsolated.TestRemoveBlock;
var
  BlockName: string;
begin
  BlockName := 'ToRemove';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.AreEqual(1, FBlockList.Count, 'Count should be 1 before remove');
  FBlockList.RemoveBlock('ToRemove');
  Assert.AreEqual(0, FBlockList.Count, 'Count should be 0 after remove');
end;

procedure TTestBoldUndoBlockListIsolated.TestRenameBlock;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'OldName';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  FBlockList.RenameBlock('OldName', 'NewName');
  Block := FBlockList.BlockByName['NewName'];
  Assert.IsNotNull(Block, 'Block should be found by new name');
  Assert.AreEqual('NewName', Block.BlockName, 'Block name should be updated');
  Assert.IsNull(FBlockList.BlockByName['OldName'], 'Old name should not find block');
end;

procedure TTestBoldUndoBlockListIsolated.TestClearRemovesAllBlocks;
var
  BlockName1, BlockName2: string;
begin
  BlockName1 := 'B1';
  BlockName2 := 'B2';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName1, 'C1');
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName2, 'C2');
  Assert.AreEqual(2, FBlockList.Count, 'Count should be 2 before clear');
  TBoldUndoBlockListCracker(FBlockList).Clear;
  Assert.AreEqual(0, FBlockList.Count, 'Count should be 0 after clear');
  Assert.IsTrue(FBlockList.IsEmpty, 'IsEmpty should be True after clear');
end;

procedure TTestBoldUndoBlockListIsolated.TestGetContainsChangesWhenEmpty;
begin
  // When empty, should return false (iterates and finds no blocks with changes)
  Assert.IsFalse(FBlockList.ContainsChanges, 'ContainsChanges should be False when empty');
end;

procedure TTestBoldUndoBlockListIsolated.TestGetContainsChangesWithEmptyBlock;
var
  BlockName: string;
begin
  BlockName := 'EmptyBlock';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  // New block has no changes
  Assert.IsFalse(FBlockList.ContainsChanges, 'ContainsChanges should be False with empty block');
end;

procedure TTestBoldUndoBlockListIsolated.TestAddBlockDuplicateNameRaises;
var
  BlockName: string;
  Cracker: TBoldUndoBlockListCracker;
begin
  Cracker := TBoldUndoBlockListCracker(FBlockList);
  BlockName := 'Duplicate';
  Cracker.AddBlock(BlockName, 'Cap1');
  Assert.WillRaise(
    procedure
    var
      TempName: string;
    begin
      TempName := 'Duplicate';
      Cracker.AddBlock(TempName, 'Cap2');
    end,
    EBold);
end;

procedure TTestBoldUndoBlockListIsolated.TestGetAssertedBlockByNameRaisesWhenNotFound;
var
  Cracker: TBoldUndoBlockListCracker;
begin
  Cracker := TBoldUndoBlockListCracker(FBlockList);
  Assert.WillRaise(
    procedure
    begin
      Cracker.AssertedBlockByName['NonExistent'];
    end,
    EBold);
end;

procedure TTestBoldUndoBlockListIsolated.TestGetAssertedBlockByIndexRaisesWhenOutOfRange;
var
  Cracker: TBoldUndoBlockListCracker;
begin
  Cracker := TBoldUndoBlockListCracker(FBlockList);
  Assert.WillRaise(
    procedure
    begin
      Cracker.AssertedBlockByIndex[0];
    end,
    EBold);
end;

procedure TTestBoldUndoBlockListIsolated.TestAssertedIndexOfRaisesWhenNotFound;
var
  Cracker: TBoldUndoBlockListCracker;
begin
  Cracker := TBoldUndoBlockListCracker(FBlockList);
  Assert.WillRaise(
    procedure
    begin
      Cracker.AssertedIndexOf('NonExistent');
    end,
    EBold);
end;

procedure TTestBoldUndoBlockListIsolated.TestMergeAllMergesBlocks;
var
  BlockName1, BlockName2, BlockName3: string;
begin
  BlockName1 := 'B1';
  BlockName2 := 'B2';
  BlockName3 := 'B3';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName1, 'C1');
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName2, 'C2');
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName3, 'C3');
  Assert.AreEqual(3, FBlockList.Count, 'Count should be 3 before merge');
  FBlockList.MergeAll;
  Assert.AreEqual(1, FBlockList.Count, 'Count should be 1 after MergeAll');
end;

procedure TTestBoldUndoBlockListIsolated.TestMoveBlockSameIndex;
var
  BlockName: string;
begin
  BlockName := 'OnlyBlock';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  // Moving to same index should be no-op
  FBlockList.MoveBlock(0, 0);
  Assert.AreEqual(1, FBlockList.Count, 'Count should still be 1');
  Assert.AreEqual('OnlyBlock', FBlockList.BlockByIndex[0].BlockName, 'Block should still be there');
end;

procedure TTestBoldUndoBlockListIsolated.TestCanMoveBlockSameIndex;
var
  BlockName: string;
begin
  BlockName := 'TestBlock';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.IsTrue(FBlockList.CanMoveBlock(0, 0), 'CanMoveBlock(0, 0) should return True');
end;

procedure TTestBoldUndoBlockListIsolated.TestCanMergeBlockSameIndex;
var
  BlockName: string;
begin
  BlockName := 'TestBlock';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.IsTrue(FBlockList.CanMergeBlock(0, 0), 'CanMergeBlock(0, 0) should return True');
end;

procedure TTestBoldUndoBlockListIsolated.TestRenameBlockToExistingNameRaises;
var
  BlockName1, BlockName2: string;
begin
  BlockName1 := 'First';
  BlockName2 := 'Second';
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName1, 'C1');
  TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName2, 'C2');
  Assert.WillRaise(
    procedure
    begin
      FBlockList.RenameBlock('First', 'Second');
    end,
    EBold);
end;

// TBoldUndoBlock tests

procedure TTestBoldUndoBlockListIsolated.TestBlockNameProperty;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'MyBlockName';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.AreEqual('MyBlockName', Block.BlockName, 'BlockName property should return correct name');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockCaptionProperty;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'Block';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'MyCaption');
  Assert.AreEqual('MyCaption', Block.Caption, 'Caption property should return correct caption');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockCreatedProperty;
var
  BlockName: string;
  Block: TBoldUndoBlock;
  BeforeCreate, AfterCreate: TDateTime;
begin
  BeforeCreate := Now;
  BlockName := 'Block';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  AfterCreate := Now;
  Assert.IsTrue(Block.Created >= BeforeCreate, 'Created should be >= time before creation');
  Assert.IsTrue(Block.Created <= AfterCreate, 'Created should be <= time after creation');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockContainsChangesInitiallyFalse;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'Block';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.IsFalse(Block.ContainsChanges, 'ContainsChanges should be False for new block');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockObjectCountInitiallyZero;
var
  BlockName: string;
  Block: TBoldUndoBlock;
begin
  BlockName := 'Block';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.AreEqual(0, Block.ObjectCount, 'ObjectCount should be 0 for new block');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockGetIndex;
var
  BlockName1, BlockName2, BlockName3: string;
  Block1, Block2, Block3: TBoldUndoBlock;
begin
  BlockName1 := 'B1';
  BlockName2 := 'B2';
  BlockName3 := 'B3';
  Block1 := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName1, 'C1');
  Block2 := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName2, 'C2');
  Block3 := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName3, 'C3');
  Assert.AreEqual(0, Block1.Index, 'Block1.Index should be 0');
  Assert.AreEqual(1, Block2.Index, 'Block2.Index should be 1');
  Assert.AreEqual(2, Block3.Index, 'Block3.Index should be 2');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockSetFSValueSpace;
var
  BlockName: string;
  Block: TBoldUndoBlock;
  NewVS: TBoldFreeStandingValueSpace;
begin
  BlockName := 'Block';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  Assert.IsFalse(Block.ContainsChanges, 'ContainsChanges should be False initially');
  NewVS := TBoldFreeStandingValueSpace.Create;
  Block.FSValueSpace := NewVS;
  Assert.IsTrue(Block.ContainsChanges, 'ContainsChanges should be True after setting FSValueSpace');
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockValueExistsReturnsFalseWhenEmpty;
var
  BlockName: string;
  Block: TBoldUndoBlock;
  ObjectId: TBoldObjectId;
begin
  BlockName := 'Block';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  ObjectId := TBoldInternalObjectId.CreateWithClassID(0, True);
  try
    Assert.IsFalse(Block.ValueExists(ObjectId, 0), 'ValueExists should return False for empty block');
  finally
    ObjectId.Free;
  end;
end;

procedure TTestBoldUndoBlockListIsolated.TestBlockGetFSValueSpaceCreatesIfNil;
var
  BlockName: string;
  Block: TBoldUndoBlock;
  VS: TBoldFreeStandingValueSpace;
begin
  BlockName := 'Block';
  Block := TBoldUndoBlockListCracker(FBlockList).AddBlock(BlockName, 'Cap');
  // FSValueSpace is nil initially when created without one
  VS := Block.FSValueSpace;
  Assert.IsNotNull(VS, 'FSValueSpace getter should create value space if nil');
end;

{ TTestBoldUndoHandler }

procedure TTestBoldUndoHandler.SetUpFixture;
begin
  EnsureBoldTestDM;
  Assert.IsNotNull(BoldTestDM, 'BoldTestDM should be created');
  Assert.IsNotNull(BoldTestDM.BoldSystemHandle1.System, 'System should be active');
end;

procedure TTestBoldUndoHandler.TearDownFixture;
begin
  // Clean up DataModule at the end of all tests
  CloseBoldTestDM;
end;

procedure TTestBoldUndoHandler.SetUp;
begin
  if not BoldTestDM.BoldSystemHandle1.Active then
    BoldTestDM.BoldSystemHandle1.Active := True;

  FUndoHandler := BoldTestDM.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler;
  FUndoHandler.Enabled := True;

  FSomeClassList := TSomeClassList.Create;
  FAPersistentClassList := TAPersistentClassList.Create;
  FATransientClassList := TATransientClassList.Create;
  FFSValueSpace := TBoldFreeStandingValueSpace.Create;
end;

procedure TTestBoldUndoHandler.TearDown;
begin
  FreeAndNil(FSomeClassList);
  FreeAndNil(FAPersistentClassList);
  FreeAndNil(FATransientClassList);
  FreeAndNil(FFSValueSpace);

  if Assigned(BoldTestDM) and BoldTestDM.BoldSystemHandle1.Active then
  begin
    BoldTestDM.BoldSystemHandle1.System.Discard;
    BoldTestDM.BoldSystemHandle1.Active := False;
  end;
end;

function TTestBoldUndoHandler.GetSystem: TBoldSystem;
begin
  Result := BoldTestDM.BoldSystemHandle1.System;
end;

function TTestBoldUndoHandler.GetUndoHandler: TBoldUndoHandler;
begin
  Result := FUndoHandler;
end;

function TTestBoldUndoHandler.IdCompare(Item1, Item2: TBoldElement): Integer;
var
  i1, i2: Integer;
begin
  i1 := StrToInt(TBoldObject(Item1).BoldObjectLocator.AsString);
  i2 := StrToInt(TBoldObject(Item2).BoldObjectLocator.AsString);
  if i1 = i2 then
    Result := 0
  else if i1 < i2 then
    Result := -1
  else
    Result := 1;
end;

procedure TTestBoldUndoHandler.FetchClassSorted(const aSystem: TBoldSystem;
  const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
begin
  FetchClass(aSystem, aList, ObjClass);
  aList.Sort(IdCompare);
end;

procedure TTestBoldUndoHandler.RefreshSystem;
begin
  UpdateDatabase;
  BoldTestDM.BoldSystemHandle1.Active := False;
  FSomeClassList.Clear;
  FAPersistentClassList.Clear;
  FATransientClassList.Clear;
  BoldTestDM.BoldSystemHandle1.Active := True;
  FUndoHandler := BoldTestDM.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler;
  FUndoHandler.Enabled := True;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
end;

procedure TTestBoldUndoHandler.UpdateDatabase;
begin
  BoldTestDM.BoldSystemHandle1.UpdateDatabase;
end;

procedure TTestBoldUndoHandler.SetSimpleConfiguration;
begin
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].parent := FSomeClassList[0];
  FSomeClassList[3].parent := FSomeClassList[2];
end;

procedure TTestBoldUndoHandler.SetTransientConfiguration;
begin
  RefreshSystem;
  GenerateObjects(System, 'APersistentClass', 2);
  RefreshSystem;
  FetchEnsuredClass(System, FAPersistentClassList, TAPersistentClass);
  CreateATransientClass(System, nil);
  CreateATransientClass(System, nil);
  FetchClassSorted(System, FATransientClassList, TATransientClass);
  FATransientClassList[0].many.Add(FAPersistentClassList[0]);
  FATransientClassList[1].many.Add(FAPersistentClassList[1]);
end;

procedure TTestBoldUndoHandler.StoreValue(const Member: TBoldMember);
var
  oc: TBoldFreeStandingObjectContents;
  MemberId: TBoldMemberId;
begin
  (FFSValueSpace as IBoldValueSpace).EnsureObjectContents(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc.ApplyObjectContents(Member.OwningObject.AsIBoldObjectContents[bdepContents], False, False);
  MemberId := TBoldMemberID.Create(Member.BoldMemberRTInfo.Index);
  try
    oc.EnsureMember(MemberId, Member.AsIBoldValue[bdepContents].ContentName);
    oc.ValueByIndex[MemberId.MemberIndex].AssignContent(Member.AsIBoldValue[bdepContents]);
  finally
    FreeAndNil(MemberId);
  end;
end;

function TTestBoldUndoHandler.GetStoredValueOfMember(const Member: TBoldMember): IBoldValue;
var
  oc: TBoldFreeStandingObjectContents;
begin
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  Result := oc.ValueByIndex[Member.BoldMemberRTInfo.Index];
end;

procedure TTestBoldUndoHandler.VerifyState(const Element: TBoldDomainElement;
  const State: TBoldValuePersistenceState);
var
  CurState: TBoldValuePersistenceState;
begin
  if Element is TBoldObject then
    CurState := (Element as TBoldObject).BoldPersistenceState
  else if Element is TBoldMember then
    CurState := (Element as TBoldMember).BoldPersistenceState
  else
    raise EBold.Create('VerifyState: unsupported element type');
  Assert.AreEqual(State, CurState,
    Format('%s state should be %d but was %d', [Element.DisplayName, Ord(State), Ord(CurState)]));
end;

procedure TTestBoldUndoHandler.VerifyIsInUndoArea(aBlock: TBoldUndoBlock;
  Member: TBoldMember; MemberValue: IBoldValue);
var
  OldValue: IBoldValue;
  ValueFound: Boolean;
begin
  ValueFound := aBlock.ValueExists(
    Member.OwningObject.BoldObjectLocator.BoldObjectID,
    Member.BoldMemberRTInfo.Index,
    OldValue);
  Assert.IsTrue(ValueFound, Format('%s should be in undo area', [Member.DisplayName]));
  Assert.IsNotNull(OldValue, Format('%s old value should not be nil', [Member.DisplayName]));
end;

procedure TTestBoldUndoHandler.VerifyIsInRedoArea(Member: TBoldMember;
  Value: TBoldFreeStandingValue);
var
  ValueInBlock: IBoldValue;
  Found: Boolean;
begin
  ValueInBlock := nil;
  Found := UndoHandler.RedoBlocks.CurrentBlock.ValueExists(
    Member.OwningObject.BoldObjectLocator.BoldObjectID,
    Member.BoldMemberRTInfo.Index,
    ValueInBlock);
  Assert.IsTrue(Found, Format('%s should be in redo area', [Member.DisplayName]));
  Assert.IsNotNull(ValueInBlock, Format('%s redo value should not be nil', [Member.DisplayName]));
end;

// Object creation undo tests

procedure TTestBoldUndoHandler.TestNewObjectRecordedWithCorrectExistenceState;
var
  NewObject: TSomeClass;
  oc: TBoldFreeStandingObjectContents;
begin
  // Create a new persistent object
  NewObject := CreateSomeClass(System, nil, True);

  // Get the object contents from the undo block
  oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(
    NewObject.BoldObjectLocator.BoldObjectID);

  Assert.IsNotNull(oc, 'Object contents should be recorded in undo block');
  Assert.AreEqual(besNotCreated, oc.BoldExistenceState,
    'New object should be recorded with besNotCreated state for proper undo');
end;

procedure TTestBoldUndoHandler.TestDeletedObjectRecordedWithCorrectExistenceState;
var
  NewObject: TSomeClass;
  ObjectId: TBoldObjectId;
  oc: TBoldFreeStandingObjectContents;
begin
  // Create and save an object first
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;

  // Get the first object and delete it
  NewObject := FSomeClassList[0];
  ObjectId := NewObject.BoldObjectLocator.BoldObjectID.Clone;
  try
    UndoHandler.SetCheckPoint('BeforeDelete');
    NewObject.Delete;

    // Get the object contents from the undo block
    oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);

    Assert.IsNotNull(oc, 'Deleted object contents should be recorded in undo block');
    Assert.AreEqual(besExisting, oc.BoldExistenceState,
      'Deleted object should be recorded with besExisting state for proper undo');
  finally
    ObjectId.Free;
  end;
end;

// Attribute modification tests

procedure TTestBoldUndoHandler.TestModifyAttributeRecordsOldValue;
var
  Obj: TSomeClass;
  OldValue: string;
  RecordedValue: IBoldValue;
begin
  // Create and save an object
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;

  Obj := FSomeClassList[0];
  OldValue := Obj.aString;

  UndoHandler.SetCheckPoint('BeforeModify');
  Obj.aString := 'ModifiedValue';

  // Verify the old value is recorded in undo block
  Assert.IsTrue(
    UndoHandler.UndoBlocks.CurrentBlock.ValueExists(
      Obj.BoldObjectLocator.BoldObjectID,
      Obj.M_aString.BoldMemberRTInfo.Index,
      RecordedValue),
    'Old value should be recorded in undo block');
end;

procedure TTestBoldUndoHandler.TestUndoRestoresAttributeValue;
var
  Obj: TSomeClass;
  OldValue: string;
  ObjectId: TBoldObjectId;
begin
  // Create and save an object
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;

  Obj := FSomeClassList[0];
  OldValue := Obj.aString;
  ObjectId := Obj.BoldObjectLocator.BoldObjectID.Clone;
  try
    UndoHandler.SetCheckPoint('BeforeModify');
    Obj.aString := 'ModifiedValue';

    Assert.AreEqual('ModifiedValue', Obj.aString, 'Value should be modified');

    // Undo the change
    UndoHandler.UndoLatest;

    // Re-fetch the object (it may have been recreated)
    Obj := System.Locators.ObjectByID[ObjectId] as TSomeClass;

    Assert.AreEqual(OldValue, Obj.aString, 'Undo should restore original value');
  finally
    ObjectId.Free;
  end;
end;

// Undo/Redo cycle tests

procedure TTestBoldUndoHandler.TestUndoObjectCreation;
var
  ParentObj, ChildObj: TSomeClass;
  ParentId, ChildId: TBoldObjectId;
  BlockName: string;
begin
  // Create a parent object and save it
  ParentObj := CreateSomeClass(System, nil, True);
  UpdateDatabase;
  ParentId := ParentObj.BoldObjectLocator.BoldObjectID.Clone;
  try
    // Set checkpoint and create a child object linked to parent
    BlockName := UndoHandler.SetCheckPoint('CreateChild');
    ChildObj := CreateSomeClass(System, nil, True);
    ChildId := ChildObj.BoldObjectLocator.BoldObjectID.Clone;
    try
      ChildObj.parent := ParentObj;

      Assert.AreEqual(1, ParentObj.child.Count, 'Parent should have 1 child');

      // Undo the child creation - child was never persisted, so undo should just remove from memory
      UndoHandler.UndoBlock(BlockName);

      // Re-fetch parent
      ParentObj := System.Locators.ObjectByID[ParentId] as TSomeClass;
      ChildObj := System.Locators.ObjectByID[ChildId] as TSomeClass;

      Assert.IsTrue(ParentObj.BoldExistenceState = besExisting, 'Parent should still exist');
      Assert.IsNull(ChildObj, 'Child should be deleted after undo');
      Assert.AreEqual(0, ParentObj.child.Count, 'Parent should have no children after undo');
    finally
      ChildId.Free;
    end;
  finally
    ParentId.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestRedoObjectCreation;
var
  ParentObj, ChildObj: TSomeClass;
  ParentId, ChildId: TBoldObjectId;
  BlockName: string;
begin
  // Create a parent object and save it
  ParentObj := CreateSomeClass(System, nil, True);
  UpdateDatabase;
  ParentId := ParentObj.BoldObjectLocator.BoldObjectID.Clone;
  try
    // Set checkpoint and create a child object linked to parent
    BlockName := UndoHandler.SetCheckPoint('CreateChild');
    ChildObj := CreateSomeClass(System, nil, True);
    ChildId := ChildObj.BoldObjectLocator.BoldObjectID.Clone;
    try
      ChildObj.parent := ParentObj;

      // Undo
      UndoHandler.UndoBlock(BlockName);

      // Redo
      UndoHandler.RedoBlock(BlockName);

      // Re-fetch objects
      ParentObj := System.Locators.ObjectByID[ParentId] as TSomeClass;
      ChildObj := System.Locators.ObjectByID[ChildId] as TSomeClass;

      Assert.IsTrue(ParentObj.BoldExistenceState = besExisting, 'Parent should exist after redo');
      Assert.IsTrue(ChildObj.BoldExistenceState = besExisting, 'Child should exist after redo');
      Assert.AreEqual(1, ParentObj.child.Count, 'Parent should have 1 child after redo');
    finally
      ChildId.Free;
    end;
  finally
    ParentId.Free;
  end;
end;

// Tests migrated from maan_Modify

procedure TTestBoldUndoHandler.TestCreatePersistentObjectRecordsUndoState;
var
  NewObject: TSomeClass;
  oc: TBoldFreeStandingObjectContents;
begin
  RefreshSystem;
  NewObject := CreateSomeClass(System, nil, True);
  oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(
    NewObject.BoldObjectLocator.BoldObjectID);
  Assert.IsNotNull(oc, 'oc should be assigned after CreateSomeClass');
  Assert.AreEqual(besNotCreated, oc.BoldExistenceState,
    'oc.BoldExistenceState should be besNotCreated');
end;

procedure TTestBoldUndoHandler.TestCreateTransientObjectRecordsUndoState;
var
  NewObject: TATransientClass;
  oc: TBoldFreeStandingObjectContents;
begin
  RefreshSystem;
  NewObject := CreateATransientClass(System, nil);
  Assert.AreEqual(besExisting, NewObject.BoldExistenceState,
    'NewObject.BoldExistenceState should be besExisting');
  oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(
    NewObject.BoldObjectLocator.BoldObjectID);
  Assert.IsNotNull(oc, 'oc should be assigned for transient object');
  Assert.AreEqual(besNotCreated, oc.BoldExistenceState,
    'oc.BoldExistenceState should be besNotCreated for transient');
end;

procedure TTestBoldUndoHandler.TestDeleteTransientObjectRecordsUndoState;
var
  NewObject: TATransientClass;
  oc: TBoldFreeStandingObjectContents;
  oid: TBoldObjectId;
begin
  RefreshSystem;
  NewObject := CreateATransientClass(System, nil);
  Assert.AreEqual(besExisting, NewObject.BoldExistenceState,
    'NewObject.BoldExistenceState should be besExisting before Delete');
  oid := NewObject.BoldObjectLocator.BoldObjectID.Clone;
  try
    UndoHandler.SetCheckPoint('Delete NewObject');
    NewObject.Delete;
    oc := UndoHandler.UndoBlocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(oid);
    Assert.IsNotNull(oc, 'oc should be assigned after Delete transient');
    Assert.AreEqual(besExisting, oc.BoldExistenceState,
      'oc.BoldExistenceState should be besExisting after Delete transient');
  finally
    oid.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestModifyPersistentAttributeCurrent;
var
  NewObject: TSomeClass;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  NewObject := FSomeClassList[0];
  VerifyState(NewObject.M_aString, bvpsCurrent);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString,
    GetStoredValueOfMember(NewObject.M_aString));
  VerifyState(NewObject.M_aString, bvpsModified);
end;

procedure TTestBoldUndoHandler.TestModifyPersistentAttributeModified;
var
  NewObject: TSomeClass;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  NewObject := FSomeClassList[0];
  NewObject.aString := NewObject.aString + '123';
  VerifyState(NewObject.M_aString, bvpsModified);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '456';
  VerifyState(NewObject.M_aString, bvpsModified);
end;

procedure TTestBoldUndoHandler.TestModifyTransientAttribute;
var
  NewObject: TATransientClass;
begin
  RefreshSystem;
  NewObject := CreateATransientClass(System, nil);
  VerifyState(NewObject.M_aString, bvpsTransient);
  StoreValue(NewObject.M_aString);
  UndoHandler.SetCheckPoint('Block1');
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString,
    GetStoredValueOfMember(NewObject.M_aString));
  VerifyState(NewObject.M_aString, bvpsTransient);
end;

procedure TTestBoldUndoHandler.TestModifyEmbeddedRoleCurrent;
var
  ObjA, ObjB, ObjA2: TSomeClass;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjB := FSomeClassList[1];
  ObjA2 := FSomeClassList[2];
  ObjB.M_parent.EnsureContentsCurrent;
  VerifyState(ObjB.M_parent, bvpsCurrent);

  // ObjA.child Current
  ObjA.child.EnsureContentsCurrent;
  ObjA2.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  Assert.IsTrue(ObjB.parent = ObjA, 'ObjB.parent should be ObjA');
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(ObjB.parent = ObjA2, 'ObjB.parent should now be ObjA2');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent,
    GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsFalse(ObjA.child.Includes(ObjB), 'ObjA.child should not include ObjB');
  VerifyState(ObjA2.M_child, bvpsCurrent);
  Assert.IsTrue(ObjA2.child.Includes(ObjB), 'ObjA2.child should include ObjB');
end;

procedure TTestBoldUndoHandler.TestModifyEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;
begin
  // EmbeddedRole.child Transient
  GenerateObjects(System, 'APersistentClass', 2);
  RefreshSystem;
  FetchEnsuredClass(System, FAPersistentClassList, TAPersistentClass);
  ObjB1 := FAPersistentClassList[0];
  ObjB2 := FAPersistentClassList[1];
  ObjA1 := CreateATransientClass(System, nil);
  ObjA2 := CreateATransientClass(System, nil);
  ObjA1.many.Add(ObjB1);
  ObjA2.many.Add(ObjB2);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  UndoHandler.SetCheckPoint('ModifyEmbeddedTransient');
  StoreValue(ObjB1.M_one);
  Assert.IsTrue(ObjB1.one = ObjA1, 'ObjB1.one should be ObjA1');
  ObjB1.one := ObjA2; // modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  Assert.IsTrue(ObjB2.one = ObjA2, 'ObjB2.one should be ObjA2');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one,
    GetStoredValueOfMember(ObjB1.M_one));
  Assert.IsFalse(ObjA1.many.Includes(ObjB1), 'ObjA1.many should not include ObjB1');
  Assert.IsTrue(ObjA2.many.Includes(ObjB1), 'ObjA2.many should include ObjB1');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleInsertCurrent;
var
  ObjA, ObjB: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
begin
  SetSimpleConfiguration;
  // Change setup: ObjB.parent points to ObjA2 (index 3), not ObjA (index 0)
  FSomeClassList[1].parent := FSomeClassList[3];
  FSomeClassList[3].parent := FSomeClassList[2];
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjBLocator := FSomeClassList.Locators[1];
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);

  // ObjB.parent current - insert ObjB into ObjA.child
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  ObjB.parent; // ensure current
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Add(ObjB); // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue((ObjB.M_parent as TBoldObjectReference).Locator = ObjA.BoldObjectLocator,
    'ObjB.parent should point to ObjA');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent,
    GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsTrue(ObjA.child.Includes(ObjB), 'ObjA.child should include ObjB');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleDeleteCurrent;
var
  ObjA, ObjB: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjBLocator := FSomeClassList.Locators[1];
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);

  // ObjB.parent current
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  ObjB.parent; // ensure current
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Remove(ObjB); // modify-delete
  Assert.IsTrue(ObjB.parent = nil, 'ObjB.parent should be nil after remove');
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent,
    GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsFalse(ObjA.child.Includes(ObjB), 'ObjA.child should not include ObjB');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleInsertTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;
begin
  SetTransientConfiguration;
  ObjB1 := FAPersistentClassList[0];
  ObjB2 := FAPersistentClassList[1];
  ObjA1 := FATransientClassList[0];
  ObjA2 := FATransientClassList[1];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  StoreValue(ObjB2.M_one);
  UndoHandler.SetCheckPoint('Check1');
  Assert.IsTrue(ObjB2.one = ObjA2, 'ObjB2.one should be ObjA2');
  ObjA1.many.Add(ObjB2); // modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjB2.M_one, bvpsTransient);
  Assert.IsTrue(ObjB2.one = ObjA1, 'ObjB2.one should now be ObjA1');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB2.M_one,
    GetStoredValueOfMember(ObjB2.M_one));
  Assert.IsTrue(ObjA1.many.Includes(ObjB2), 'ObjA1.many should include ObjB2');
  Assert.IsFalse(ObjA2.many.Includes(ObjB2), 'ObjA2.many should not include ObjB2');
end;

procedure TTestBoldUndoHandler.TestModifyNonEmbeddedRoleDeleteTransient;
var
  ObjA1: TATransientClass;
  ObjB1: TAPersistentClass;
begin
  SetTransientConfiguration;
  ObjA1 := FATransientClassList[0];
  ObjB1 := FAPersistentClassList[0];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  StoreValue(ObjB1.M_one);
  UndoHandler.SetCheckPoint('Check1');
  ObjA1.many.Remove(ObjB1); // modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  Assert.IsTrue(ObjB1.one = nil, 'ObjB1.one should be nil after remove');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one,
    GetStoredValueOfMember(ObjB1.M_one));
  Assert.IsFalse(ObjA1.many.Includes(ObjB1), 'ObjA1.many should not include ObjB1');
end;

// Tests migrated from maan_Undo

procedure TTestBoldUndoHandler.TestUndoTransientAttribute;
var
  TransientObject: TATransientClass;
  TransientObjectId: TBoldObjectId;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  // Transient attribute undo
  TransientObject := CreateATransientClass(System, nil);
  TransientObjectId := TransientObject.BoldObjectLocator.BoldObjectId.Clone;
  try
    UndoHandler.SetCheckPoint('UndoAttribute');
    StoreValue(TransientObject.M_aString);
    TransientObject.aString := TransientObject.aString + '123';
    VerifyState(TransientObject.M_aString, bvpsTransient);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;

    TransientObject := System.Locators.ObjectByID[TransientObjectId] as TATransientClass;
    Assert.IsTrue(TransientObject.ValuesAreEqual(
      TransientObject.M_aString.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(TransientObject.M_aString), 'aString'),
      'Value should be restored after undo');
    VerifyState(TransientObject.M_aString, bvpsTransient);
  finally
    TransientObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestUndoModifiedAttribute;
var
  SomeObject: TSomeClass;
  SomeObjectId: TBoldObjectId;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  SomeObject := FSomeClassList[0];
  SomeObjectId := SomeObject.BoldObjectLocator.BoldObjectId.Clone;
  try
    StoreValue(SomeObject.M_aString);
    VerifyState(SomeObject.M_aString, bvpsCurrent);
    SomeObject.aString := SomeObject.aString + '123';
    VerifyState(SomeObject.M_aString, bvpsModified);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;

    SomeObject := System.Locators.ObjectById[SomeObjectId] as TSomeClass;
    VerifyState(SomeObject.M_aString, bvpsCurrent);
    Assert.IsTrue(SomeObject.ValuesAreEqual(
      SomeObject.M_aString.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(SomeObject.M_aString), 'aString'),
      'Value should be restored after undo');
  finally
    SomeObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestUndoEmbeddedRoleModified;
var
  ObjA1, ObjA2, ObjB1, ObjB2: TSomeClass;
  ObjA1Id, ObjA2Id, ObjB1Id, ObjB2Id: TBoldObjectId;
  FSValue: TBFSObjectIdRef;

  procedure Prepare;
  begin
    RefreshSystem;
    FSomeClassList[1].parent := FSomeClassList[0];
    FSomeClassList[3].parent := FSomeClassList[2];
    RefreshSystem;
    ObjA1 := FSomeClassList[0];
    ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
    ObjB1 := FSomeClassList[1];
    ObjB1Id := ObjB1.BoldObjectLocator.BoldObjectId.Clone;
    ObjA2 := FSomeClassList[2];
    ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
    ObjB2 := FSomeClassList[3];
    ObjB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
  end;

  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TSomeClass;
    ObjB1 := System.Locators.ObjectById[ObjB1Id] as TSomeClass;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TSomeClass;
  end;

begin
  SetSimpleConfiguration;
  Prepare;

  // NonEmbeddedRole.parent invalid, ObjA1 invalid, ObjA2 invalid
  FSValue := TBFSObjectIdRef.Create;
  try
    StoreValue(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.parent := ObjA2; // modify
    VerifyState(ObjA1.M_child, bvpsInvalid);
    VerifyState(ObjA2.M_child, bvpsInvalid);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;
    SetObjectsFromIds;

    VerifyState(ObjB1.M_parent, bvpsCurrent);
    Assert.IsTrue(ObjB1.ValuesAreEqual(
      ObjB1.M_parent.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(ObjB1.M_parent), 'parent'),
      'Parent should be restored after undo');
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    VerifyState(ObjA2.M_child, bvpsInvalid);
    VerifyState(ObjA1.M_child, bvpsInvalid);
  finally
    FreeAndNil(FSValue);
    ObjA1Id.Free;
    ObjA2Id.Free;
    ObjB1Id.Free;
    ObjB2Id.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestUndoEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB2: TAPersistentClass;
  ObjA1Id, ObjA2Id, ObjB2Id: TBoldObjectId;
  FSValueBeforeUndo: TBFSObjectIdRef;
  BlockName: string;

  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TATransientClass;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TATransientClass;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TAPersistentClass;
  end;

begin
  SetTransientConfiguration;
  ObjB2 := FAPersistentClassList[1];
  ObjB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
  ObjA1 := FATransientClassList[0];
  ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
  ObjA2 := FATransientClassList[1];
  ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
  FSValueBeforeUndo := TBFSObjectIdRef.Create;
  try
    BlockName := UndoHandler.SetCheckPoint;
    StoreValue(ObjB2.M_one);
    ObjB2.one := ObjA1; // modify
    VerifyState(ObjB2.M_one, bvpsTransient);
    VerifyState(ObjA1.M_many, bvpsTransient);
    VerifyState(ObjA2.M_many, bvpsTransient);
    FSValueBeforeUndo.AssignContent(ObjB2.M_one.AsIBoldValue[bdepContents]);

    UndoHandler.UndoLatest;
    System.AssertLinkIntegrity;
    SetObjectsFromIds;

    VerifyState(ObjB2.M_one, bvpsTransient);
    Assert.IsTrue(ObjB2.ValuesAreEqual(
      ObjB2.M_one.AsIBoldValue[bdepContents],
      GetStoredValueOfMember(ObjB2.M_one), 'one'),
      'One should be restored after undo');
    VerifyIsInRedoArea(ObjB2.M_one, FSValueBeforeUndo);
    Assert.IsFalse(ObjA1.many.Includes(ObjB2), 'ObjA1.many should not include ObjB2 after undo');
    Assert.IsTrue(ObjA2.many.Includes(ObjB2), 'ObjA2.many should include ObjB2 after undo');
  finally
    FreeAndNil(FSValueBeforeUndo);
    ObjA1Id.Free;
    ObjA2Id.Free;
    ObjB2Id.Free;
  end;
end;

procedure TTestBoldUndoHandler.TestModifyEmbeddedRoleModified;
var
  ObjA, ObjB, ObjA2, ObjA3: TSomeClass;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjB := FSomeClassList[1];
  ObjA2 := FSomeClassList[2];
  ObjA3 := FSomeClassList[3];

  // Verify initial state: ObjB.parent = ObjA (from SetSimpleConfiguration)
  ObjB.M_parent.EnsureContentsCurrent;
  Assert.IsTrue(ObjB.parent = ObjA, 'ObjB.parent should be ObjA initially');

  // Put ObjB.parent into Modified state: change to ObjA2
  ObjB.parent := ObjA2;
  VerifyState(ObjB.M_parent, bvpsModified);

  UndoHandler.SetCheckPoint('ModifyEmbeddedModified');

  // Now modify again from already-Modified state
  ObjA.child.EnsureContentsCurrent;
  ObjA2.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsCurrent);

  StoreValue(ObjB.M_parent);
  Assert.IsTrue(ObjB.parent = ObjA2, 'ObjB.parent should be ObjA2 before second modify');
  ObjB.parent := ObjA3; // modify from already-modified state
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert.IsTrue(ObjB.parent = ObjA3, 'ObjB.parent should be ObjA3 after modify');
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent,
    GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert.IsFalse(ObjA.child.Includes(ObjB), 'ObjA.child should not include ObjB');
  VerifyState(ObjA2.M_child, bvpsCurrent);
  Assert.IsFalse(ObjA2.child.Includes(ObjB), 'ObjA2.child should not include ObjB');
end;

{ TTestBoldUndoHandlerIndirectLinks }

procedure TTestBoldUndoHandlerIndirectLinks.SetUpFixture;
begin
  EnsureBoldTestDM;
  Assert.IsNotNull(BoldTestDM, 'BoldTestDM should be created');
  Assert.IsNotNull(BoldTestDM.BoldSystemHandle1.System, 'System should be active');
end;

procedure TTestBoldUndoHandlerIndirectLinks.TearDownFixture;
begin
  CloseBoldTestDM;
end;

procedure TTestBoldUndoHandlerIndirectLinks.SetUp;
begin
  // Deactivate to discard all in-memory objects, then reactivate for clean state
  if BoldTestDM.BoldSystemHandle1.Active then
  begin
    BoldTestDM.BoldSystemHandle1.System.Discard;
    BoldTestDM.BoldSystemHandle1.Active := False;
  end;
  BoldTestDM.BoldSystemHandle1.Active := True;

  FUndoHandler := BoldTestDM.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler;
  FUndoHandler.Enabled := True;

  FSomeClassList := TSomeClassList.Create;
  FClassWithLinkList := TClassWithLinkList.Create;
  FFSValueSpace := TBoldFreeStandingValueSpace.Create;
end;

procedure TTestBoldUndoHandlerIndirectLinks.TearDown;
begin
  FreeAndNil(FSomeClassList);
  FreeAndNil(FClassWithLinkList);
  FreeAndNil(FFSValueSpace);

  if Assigned(BoldTestDM) and BoldTestDM.BoldSystemHandle1.Active then
  begin
    BoldTestDM.BoldSystemHandle1.System.Discard;
    BoldTestDM.BoldSystemHandle1.Active := False;
  end;
end;

function TTestBoldUndoHandlerIndirectLinks.GetSystem: TBoldSystem;
begin
  Result := BoldTestDM.BoldSystemHandle1.System;
end;

function TTestBoldUndoHandlerIndirectLinks.GetUndoHandler: TBoldUndoHandler;
begin
  Result := FUndoHandler;
end;

function TTestBoldUndoHandlerIndirectLinks.IdCompare(Item1, Item2: TBoldElement): Integer;
var
  i1, i2: Integer;
begin
  i1 := StrToInt(TBoldObject(Item1).BoldObjectLocator.AsString);
  i2 := StrToInt(TBoldObject(Item2).BoldObjectLocator.AsString);
  if i1 = i2 then Result := 0
  else if i1 < i2 then Result := -1
  else Result := 1;
end;

procedure TTestBoldUndoHandlerIndirectLinks.FetchClassSorted(const aSystem: TBoldSystem;
  const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
begin
  FetchClass(aSystem, aList, ObjClass);
  aList.Sort(IdCompare);
end;

procedure TTestBoldUndoHandlerIndirectLinks.RefreshSystem;
begin
  UpdateDatabase;
  BoldTestDM.BoldSystemHandle1.Active := False;
  FSomeClassList.Clear;
  FClassWithLinkList.Clear;
  BoldTestDM.BoldSystemHandle1.Active := True;
  FUndoHandler := BoldTestDM.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler;
  FUndoHandler.Enabled := True;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
  FetchClassSorted(System, FClassWithLinkList, TClassWithLink);
end;

procedure TTestBoldUndoHandlerIndirectLinks.UpdateDatabase;
begin
  BoldTestDM.BoldSystemHandle1.UpdateDatabase;
end;

procedure TTestBoldUndoHandlerIndirectLinks.SetConfigurationForIndirectSingle;
begin
  GenerateObjects(System, 'ClassWithLink', 4);
  UpdateDatabase;
  FetchClassSorted(System, FClassWithLinkList, TClassWithLink);
  FClassWithLinkList[1].one := FClassWithLinkList[0];
  FClassWithLinkList[3].one := FClassWithLinkList[2];
end;

procedure TTestBoldUndoHandlerIndirectLinks.VerifyState(
  const Element: TBoldDomainElement; const State: TBoldValuePersistenceState);
var
  CurState: TBoldValuePersistenceState;
begin
  if Element is TBoldObject then
    CurState := (Element as TBoldObject).BoldPersistenceState
  else if Element is TBoldMember then
    CurState := (Element as TBoldMember).BoldPersistenceState
  else
    raise EBold.Create('VerifyState: unsupported element type');
  Assert.AreEqual(State, CurState,
    Format('%s state should be %d but was %d', [Element.DisplayName, Ord(State), Ord(CurState)]));
end;

procedure TTestBoldUndoHandlerIndirectLinks.VerifyObjectInBlock(
  Block: TBoldUndoBlock; aId: TBoldObjectId; State: TBoldExistenceState);
var
  fsObjectContents: TBoldFreeStandingObjectContents;
begin
  fsObjectContents := Block.FSValueSpace.GetFSObjectContentsByObjectId(aId);
  Assert.IsNotNull(fsObjectContents,
    Format('Object %s should be in undo block', [aId.AsString]));
  Assert.AreEqual(State, fsObjectContents.BoldExistenceState,
    Format('Object %s existence state should be %d but was %d',
      [aId.AsString, Ord(State), Ord(fsObjectContents.BoldExistenceState)]));
end;

// Indirect multi-link tests (part/partof via Tpartpartof link class)

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectMultiModifyInsertCurrent;
var
  ObjB, ObjA2: TSomeClass;
  aLinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId: TBoldObjectId;
begin
  // Create objects with part/partof links: ObjB.part has ObjA but NOT ObjA2
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].part.Add(FSomeClassList[0]);
  FSomeClassList[3].part.Add(FSomeClassList[0]);
  FSomeClassList[3].part.Add(FSomeClassList[2]);
  UpdateDatabase;
  RefreshSystem;

  ObjB := FSomeClassList[1];
  ObjA2 := FSomeClassList[2];

  ObjB.M_part.EnsureContentsCurrent;
  VerifyState(ObjB.M_part, bvpsCurrent);

  Assert.IsFalse(ObjB.part.Includes(ObjA2), 'ObjB.part should not include ObjA2 initially');
  ObjB.part.Add(ObjA2); // modify-insert
  aLinkObjectLocator := ObjB.partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  try
    // The new link object should be recorded as besNotCreated in undo block
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besNotCreated);
    Assert.IsTrue(ObjB.part.Includes(ObjA2), 'ObjB.part should include ObjA2 after insert');
    Assert.IsTrue(Assigned(System.Locators.ObjectById[aLinkObjectId]),
      'Link object should exist');
  finally
    aLinkObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectMultiModifyDeleteCurrent;
var
  ObjB, ObjA2: TSomeClass;
  aLinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId: TBoldObjectId;
begin
  // Create objects with part/partof links: ObjB.part has both ObjA and ObjA2
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].part.Add(FSomeClassList[0]);
  FSomeClassList[1].part.Add(FSomeClassList[2]);
  FSomeClassList[3].part.Add(FSomeClassList[0]);
  FSomeClassList[3].part.Add(FSomeClassList[2]);
  UpdateDatabase;
  RefreshSystem;

  ObjB := FSomeClassList[1];
  ObjA2 := FSomeClassList[2];

  ObjB.M_part.EnsureContentsCurrent;
  VerifyState(ObjB.M_part, bvpsCurrent);
  Assert.IsTrue(ObjB.part.Includes(ObjA2), 'ObjB.part should include ObjA2 initially');

  aLinkObjectLocator := ObjB.M_partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  try
    ObjB.part.RemoveByIndex(ObjB.part.IndexOf(ObjA2)); // modify-delete
    Assert.IsFalse(ObjB.part.Includes(ObjA2), 'ObjB.part should not include ObjA2 after delete');
    // The deleted link object should be recorded as besExisting in undo block
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
    Assert.IsTrue(
      (not Assigned(System.Locators.ObjectById[aLinkObjectId])) or
      (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besDeleted),
      'Link object should be deleted or nil');
  finally
    aLinkObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectMultiUndoInsert;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  ObjAId, ObjBId, ObjA2Id: TBoldObjectId;
  ALinkObject: Tpartpartof;
  ALinkObjectId: TBoldObjectId;
begin
  // Create 4 objects, save IDs before RefreshSystem
  ObjA := CreateSomeClass(System, nil, True);
  ObjB := CreateSomeClass(System, nil, True);
  ObjA2 := CreateSomeClass(System, nil, True);
  CreateSomeClass(System, nil, True); // ObjB2 - not used directly
  UpdateDatabase;
  // Set part links: ObjB.part has ObjA only (not ObjA2)
  ObjB.part.Add(ObjA);
  UpdateDatabase;
  ObjAId := ObjA.BoldObjectLocator.BoldObjectId.Clone;
  ObjBId := ObjB.BoldObjectLocator.BoldObjectId.Clone;
  ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
  try
    // Discard and re-fetch by ID to simulate RefreshSystem
    System.DiscardPersistent;
    ObjA := System.EnsuredLocatorByID[ObjAId].EnsuredBoldObject as TSomeClass;
    ObjB := System.EnsuredLocatorByID[ObjBId].EnsuredBoldObject as TSomeClass;
    ObjA2 := System.EnsuredLocatorByID[ObjA2Id].EnsuredBoldObject as TSomeClass;
    FUndoHandler := System.UndoHandler as TBoldUndoHandler;
    FUndoHandler.Enabled := True;

    ObjB.M_part.EnsureContentsCurrent;
    ObjA2.M_partof.EnsureContentsCurrent;
    VerifyState(ObjB.M_part, bvpsCurrent);

    Assert.IsTrue(ObjB.part.Includes(ObjA), 'ObjB.part should include ObjA');
    Assert.IsFalse(ObjB.part.Includes(ObjA2), 'ObjB.part should not include ObjA2');

    UndoHandler.SetCheckPoint('UndoIndirectMulti');
    ObjB.part.Add(ObjA2); // modify-insert
    Assert.IsTrue(ObjB.part.Includes(ObjA2), 'ObjB.part should include ObjA2 after insert');
    ALinkObject := ObjB.partpartpartof.BoldObjects[ObjB.part.IndexOf(ObjA2)];
    ALinkObjectId := ALinkObject.BoldObjectLocator.BoldObjectId.Clone;
    try
      VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besNotCreated);
      System.AssertLinkIntegrity;

      // Undo the insert
      UndoHandler.UndoLatest;
      System.AssertLinkIntegrity;
      ObjB := System.Locators.ObjectById[ObjBId] as TSomeClass;
      ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
      Assert.IsFalse(ObjB.part.Includes(ObjA2), 'ObjB.part should not include ObjA2 after undo');
      Assert.IsFalse(ObjA2.partof.Includes(ObjB), 'ObjA2.partof should not include ObjB after undo');

      // Redo the insert
      UndoHandler.RedoLatest;
      ObjB := System.Locators.ObjectById[ObjBId] as TSomeClass;
      ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
      Assert.IsTrue(ObjA2.partof.Includes(ObjB), 'ObjA2.partof should include ObjB after redo');
      Assert.IsTrue(ObjB.part.Includes(ObjA2), 'ObjB.part should include ObjA2 after redo');
    finally
      ALinkObjectId.Free;
    end;
  finally
    ObjAId.Free;
    ObjBId.Free;
    ObjA2Id.Free;
  end;
end;

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectMultiUndoDelete;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  ObjAId, ObjBId, ObjA2Id: TBoldObjectId;
  ALinkObjectId: TBoldObjectId;
begin
  // Create objects with part/partof links: ObjB.part has both ObjA and ObjA2
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClassSorted(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].part.Add(FSomeClassList[0]);
  FSomeClassList[1].part.Add(FSomeClassList[2]);
  FSomeClassList[3].part.Add(FSomeClassList[0]);
  FSomeClassList[3].part.Add(FSomeClassList[2]);
  UpdateDatabase;
  RefreshSystem;

  ObjA := FSomeClassList[0];
  ObjAId := ObjA.BoldObjectLocator.BoldObjectId.Clone;
  ObjB := FSomeClassList[1];
  ObjBId := ObjB.BoldObjectLocator.BoldObjectId.Clone;
  ObjA2 := FSomeClassList[2];
  ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
  try
    ObjB.M_part.EnsureContentsCurrent;
    VerifyState(ObjB.M_part, bvpsCurrent);

    UndoHandler.SetCheckPoint('UndoIndirectMultiDel');
    ALinkObjectId := ObjB.M_partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)].BoldObjectId.Clone;
    try
      ObjB.part.RemoveByIndex(ObjB.part.IndexOf(ObjA2)); // modify-delete
      Assert.IsFalse(ObjB.part.Includes(ObjA2), 'ObjB.part should not include ObjA2 after delete');
      VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besExisting);

      // Undo the delete
      UndoHandler.UndoLatest;
      System.AssertLinkIntegrity;
      ObjB := System.Locators.ObjectById[ObjBId] as TSomeClass;
      ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
      Assert.IsTrue(ObjA2.partof.Includes(ObjB), 'ObjA2.partof should include ObjB after undo');
      Assert.IsTrue(ObjB.part.Includes(ObjA2), 'ObjB.part should include ObjA2 after undo');

      // Redo the delete
      UndoHandler.RedoLatest;
      System.AssertLinkIntegrity;
      ObjB := System.Locators.ObjectById[ObjBId] as TSomeClass;
      ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
      Assert.IsFalse(ObjA2.partof.Includes(ObjB), 'ObjA2.partof should not include ObjB after redo');
      Assert.IsFalse(ObjB.part.Includes(ObjA2), 'ObjB.part should not include ObjA2 after redo');
    finally
      ALinkObjectId.Free;
    end;
  finally
    ObjAId.Free;
    ObjBId.Free;
    ObjA2Id.Free;
  end;
end;

// Indirect single-link tests (one/many via LinkClass)

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectSingleModifySingleRoleCurrent;
var
  ObjA, ObjB, ObjA2: TClassWithLink;
  aLinkObjectId, newLinkObjectId: TBoldObjectId;
begin
  SetConfigurationForIndirectSingle;
  RefreshSystem;

  ObjA := FClassWithLinkList[0];
  ObjB := FClassWithLinkList[1];
  ObjA2 := FClassWithLinkList[2];

  ObjB.M_one.EnsureContentsCurrent;
  VerifyState(ObjB.M_one, bvpsCurrent);
  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);

  Assert.IsTrue(ObjB.one = ObjA, 'ObjB.one should be ObjA before modify');

  aLinkObjectId := ObjB.oneLinkClass.BoldObjectLocator.BoldObjectId.Clone;
  try
    ObjB.one := ObjA2; // modify single role
    Assert.IsTrue(ObjB.one = ObjA2, 'ObjB.one should be ObjA2 after modify');

    newLinkObjectId := ObjB.oneLinkClass.BoldObjectLocator.BoldObjectId.Clone;
    try
      // Old link object should be deleted and recorded as besExisting
      VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
      Assert.IsTrue(
        (not Assigned(System.Locators.ObjectById[aLinkObjectId])) or
        (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besDeleted),
        'Old link object should be deleted');
      // New link object recorded as besNotCreated
      VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, newLinkObjectId, besNotCreated);
    finally
      newLinkObjectId.Free;
    end;
  finally
    aLinkObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectSingleModifyMultiRoleInsertCurrent;
var
  ObjA, ObjB: TClassWithLink;
  aLinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId: TBoldObjectId;
begin
  SetConfigurationForIndirectSingle;
  // ObjB.one = nil (remove link from setup)
  FClassWithLinkList[1].one := nil;
  RefreshSystem;

  ObjA := FClassWithLinkList[0];
  ObjB := FClassWithLinkList[1];

  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);

  ObjB.M_one.EnsureContentsCurrent;
  VerifyState(ObjB.M_one, bvpsCurrent);

  ObjA.many.Add(ObjB); // modify-insert via multi role
  Assert.IsTrue(ObjB.one = ObjA, 'ObjB.one should be ObjA after insert');

  aLinkObjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexOfLocator(ObjB.BoldObjectLocator)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  try
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besNotCreated);
    Assert.IsTrue(ObjA.many.Includes(ObjB), 'ObjA.many should include ObjB');
  finally
    aLinkObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectSingleModifyMultiRoleDeleteCurrent;
var
  ObjA, ObjB: TClassWithLink;
  aLinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId: TBoldObjectId;
begin
  SetConfigurationForIndirectSingle;
  RefreshSystem;

  ObjA := FClassWithLinkList[0];
  ObjB := FClassWithLinkList[1];

  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);
  Assert.IsTrue(ObjA.many.Includes(ObjB), 'ObjA.many should include ObjB initially');

  aLinkObjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexOfLocator(ObjB.BoldObjectLocator)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  try
    ObjA.many.RemoveByIndex(ObjA.many.IndexOfLocator(ObjB.BoldObjectLocator)); // modify-delete
    Assert.IsFalse(ObjA.many.Includes(ObjB), 'ObjA.many should not include ObjB after delete');
    Assert.IsTrue(ObjB.one = nil, 'ObjB.one should be nil after delete');
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
    Assert.IsTrue(
      (not Assigned(System.Locators.ObjectById[aLinkObjectId])) or
      (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besDeleted),
      'Link object should be deleted');
  finally
    aLinkObjectId.Free;
  end;
end;

procedure TTestBoldUndoHandlerIndirectLinks.TestIndirectSingleUndoSingleRoleModified;
var
  ObjA1, ObjA2, ObjB1: TClassWithLink;
  ObjA1Id, ObjA2Id, ObjB1Id: TBoldObjectId;
  aLinkClassId, newLinkClassId: TBoldObjectId;
begin
  SetConfigurationForIndirectSingle;
  RefreshSystem;

  ObjA1 := FClassWithLinkList[0];
  ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
  ObjB1 := FClassWithLinkList[1];
  ObjB1Id := ObjB1.BoldObjectLocator.BoldObjectId.Clone;
  ObjA2 := FClassWithLinkList[2];
  ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
  try
    Assert.IsTrue(ObjB1.one = ObjA1, 'ObjB1.one should be ObjA1 initially');
    aLinkClassId := ObjB1.oneLinkClass.BoldObjectLocator.BoldObjectId.Clone;
    try
      UndoHandler.SetCheckPoint;
      ObjB1.one := ObjA2; // modify
      Assert.IsTrue(
        (not Assigned(System.Locators.ObjectById[aLinkClassId])) or
        (System.Locators.ObjectById[aLinkClassId].BoldExistenceState = besDeleted),
        'Old link should be deleted after modify');
      VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkClassId, besExisting);

      newLinkClassId := ObjB1.oneLinkClass.BoldObjectLocator.BoldObjectId.Clone;
      try
        VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, newLinkClassId, besNotCreated);

        // Undo
        UndoHandler.UndoLatest;
        ObjA1 := System.Locators.ObjectById[ObjA1Id] as TClassWithLink;
        ObjB1 := System.Locators.ObjectById[ObjB1Id] as TClassWithLink;
        Assert.IsTrue(Assigned(System.Locators.ObjectById[aLinkClassId]),
          'Old link should be restored after undo');
        Assert.IsTrue(not Assigned(System.Locators.ObjectById[newLinkClassId]),
          'New link should be removed after undo');
        Assert.IsTrue(ObjB1.one = ObjA1, 'ObjB1.one should be ObjA1 after undo');

        // Redo
        UndoHandler.RedoLatest;
        ObjB1 := System.Locators.ObjectById[ObjB1Id] as TClassWithLink;
        ObjA2 := System.Locators.ObjectById[ObjA2Id] as TClassWithLink;
        Assert.IsTrue(ObjB1.one = ObjA2, 'ObjB1.one should be ObjA2 after redo');
      finally
        newLinkClassId.Free;
      end;
    finally
      aLinkClassId.Free;
    end;
  finally
    ObjA1Id.Free;
    ObjA2Id.Free;
    ObjB1Id.Free;
  end;
end;

{ TTestBoldUndoHandlerTransient }

procedure TTestBoldUndoHandlerTransient.SetUp;
begin
  Ensuredm_Model;
  FSystemTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FSystemTypeInfoHandle.BoldModel := dm_Model1.BoldModel1;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FSystemTypeInfoHandle;
  FSystemHandle.Active := True;
end;

procedure TTestBoldUndoHandlerTransient.TearDown;
begin
  if Assigned(FSystemHandle) then
  begin
    if FSystemHandle.Active then
    begin
      FSystemHandle.System.Discard;
      FSystemHandle.Active := False;
    end;
  end;
  FreeAndNil(FSystemHandle);
  FreeAndNil(FSystemTypeInfoHandle);
end;

function TTestBoldUndoHandlerTransient.GetSystem: TBoldSystem;
begin
  Result := FSystemHandle.System;
end;

function TTestBoldUndoHandlerTransient.GetUndoHandler: TBoldUndoHandler;
begin
  Result := GetSystem.UndoHandler as TBoldUndoHandler;
end;

procedure TTestBoldUndoHandlerTransient.TestUndoHandlerExists;
begin
  Assert.IsNotNull(GetSystem.UndoHandler, 'UndoHandler should exist on system');
  Assert.IsTrue(GetSystem.UndoHandler is TBoldUndoHandler, 'UndoHandler should be TBoldUndoHandler');
end;

procedure TTestBoldUndoHandlerTransient.TestUndoHandlerEnabled;
begin
  GetUndoHandler.Enabled := True;
  Assert.IsTrue(GetUndoHandler.Enabled, 'UndoHandler should be enabled');

  GetUndoHandler.Enabled := False;
  Assert.IsFalse(GetUndoHandler.Enabled, 'UndoHandler should be disabled');
end;

procedure TTestBoldUndoHandlerTransient.TestSetCheckPoint;
var
  CheckPointName: string;
begin
  GetUndoHandler.Enabled := True;
  CheckPointName := GetUndoHandler.SetCheckPoint('TestCheckPoint');
  Assert.IsNotEmpty(CheckPointName, 'CheckPoint name should not be empty');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUndoBlockListIsolated);
  TDUnitX.RegisterTestFixture(TTestBoldUndoHandler);
  TDUnitX.RegisterTestFixture(TTestBoldUndoHandlerIndirectLinks);
  TDUnitX.RegisterTestFixture(TTestBoldUndoHandlerTransient);

end.
