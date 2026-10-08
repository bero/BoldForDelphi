unit Test.BoldPMTransactionGuards;

{ Transaction guards in the SQL persistence mappers.

  PMCreate, PMUpdate and PMDelete write to several tables and must refuse to
  run outside a database transaction, and TBoldSystemSQLMapper.StartTransaction
  must raise when the database still reports no transaction afterwards.
  Without these guards a failure half-way through a save leaves the rows of the
  earlier statements committed - the dangling BOLD_ID corruption. }

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  Delphi.Mocks,
  BoldDBInterfaces,
  BoldPMappersSQL,
  BoldPMappersDefault;

type
  { Calls the object mapper's PM methods directly on the SQLite test database
    with no transaction open. }
  [TestFixture]
  [Category('PMapper')]
  TTestBoldPMTransactionGuards = class
  strict private
    function SystemMapper: TBoldSystemSQLMapper;
    function ObjectMapperByName(const aClassName: string): TBoldObjectDefaultMapper;
    procedure EnsureNoTransaction;
    procedure AssertRaisesNotInTransaction(const aProc: TProc; const aOperation: string);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestPMCreateRequiresTransaction;
    [Test]
    procedure TestPMUpdateRequiresTransaction;
    [Test]
    procedure TestPMDeleteRequiresTransaction;
  end;

  { Exposes the protected transaction methods of the SQL system mapper. }
  TTestableSystemMapper = class(TBoldSystemDefaultMapper)
  public
    procedure StartTransactionForTest;
    procedure CommitForTest;
  end;

  { Builds a system mapper over a mocked IBoldDatabase, so the test decides
    what InTransaction reports after StartTransaction. }
  [TestFixture]
  [Category('PMapper')]
  TTestBoldSystemSQLMapperStartTransaction = class
  strict private
    fMockDb: TMock<IBoldDatabase>;
    fInTransaction: Boolean;
    fMapper: TTestableSystemMapper;
    function GetMockDatabase: IBoldDatabase;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestStartTransactionRaisesWhenDatabaseStartedNone;
    [Test]
    procedure TestStartTransactionIsCommittedWhenDatabaseStartedOne;
  end;

implementation

uses
  System.Rtti,
  BoldDefs,
  BoldId,
  BoldDefaultId,
  BoldFreeStandingValues,
  BoldSystem,
  BoldTestModel,
  maan_UndoRedoBase;

const
  cnSomeClass = 'SomeClass';

{ TTestBoldPMTransactionGuards }

procedure TTestBoldPMTransactionGuards.SetUp;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
end;

procedure TTestBoldPMTransactionGuards.TearDown;
begin
  if Assigned(dmUndoRedo) then
  begin
    if dmUndoRedo.BoldSystemHandle1.Active then
    begin
      dmUndoRedo.BoldSystemHandle1.System.Discard;
      dmUndoRedo.BoldSystemHandle1.Active := False;
    end;
    // A PM method that ran without the guard has written to the database
    // outside any transaction - free the datamodule so the next EnsureDM
    // starts from a fresh database.
    FreeAndNil(dmUndoRedo);
  end;
end;

function TTestBoldPMTransactionGuards.SystemMapper: TBoldSystemSQLMapper;
begin
  Result := dmUndoRedo.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper;
end;

function TTestBoldPMTransactionGuards.ObjectMapperByName(const aClassName: string): TBoldObjectDefaultMapper;
var
  TopSortedIndex: Integer;
begin
  TopSortedIndex := dmUndoRedo.BoldSystemHandle1.System.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[aClassName].TopSortedIndex;
  Result := SystemMapper.ObjectPersistenceMappers[TopSortedIndex] as TBoldObjectDefaultMapper;
end;

procedure TTestBoldPMTransactionGuards.EnsureNoTransaction;
begin
  if SystemMapper.Database.InTransaction then
    SystemMapper.Database.RollBack;
  Assert.IsFalse(SystemMapper.Database.InTransaction, 'precondition: no transaction may be open');
end;

procedure TTestBoldPMTransactionGuards.AssertRaisesNotInTransaction(const aProc: TProc; const aOperation: string);
begin
  try
    aProc;
  except
    on E: EBold do
    begin
      Assert.IsTrue(Pos('Not in transaction', E.Message) > 0,
        aOperation + ' raised EBold for another reason: ' + E.Message);
      Exit;
    end;
    on E: Exception do
      Assert.Fail(aOperation + ' raised ' + E.ClassName + ' instead of the transaction guard: ' + E.Message);
  end;
  Assert.Fail(aOperation + ' ran outside a transaction without raising');
end;

procedure TTestBoldPMTransactionGuards.TestPMCreateRequiresTransaction;
var
  ObjectMapper: TBoldObjectDefaultMapper;
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIDTranslationList;
  ValueSpace: TBoldFreeStandingValueSpace;
  NewId: TBoldDefaultId;
begin
  ObjectMapper := ObjectMapperByName(cnSomeClass);
  EnsureNoTransaction;
  ObjectIdList := TBoldObjectIdList.Create;
  TranslationList := TBoldIDTranslationList.Create;
  ValueSpace := TBoldFreeStandingValueSpace.Create;
  NewId := TBoldDefaultId.CreateWithClassId(ObjectMapper.TopSortedIndex, True);
  try
    NewId.AsInteger := -99999;
    ObjectIdList.Add(NewId);
    AssertRaisesNotInTransaction(
      procedure
      begin
        ObjectMapper.PMCreate(ObjectIdList, ValueSpace, TranslationList);
      end, 'PMCreate');
  finally
    NewId.Free;
    ValueSpace.Free;
    TranslationList.Free;
    ObjectIdList.Free;
  end;
end;

procedure TTestBoldPMTransactionGuards.TestPMUpdateRequiresTransaction;
var
  Obj: TSomeClass;
  ObjectMapper: TBoldObjectDefaultMapper;
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIDTranslationList;
  ValueSpace: TBoldFreeStandingValueSpace;
begin
  Obj := TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  ObjectMapper := ObjectMapperByName(cnSomeClass);
  EnsureNoTransaction;
  ObjectIdList := TBoldObjectIdList.Create;
  TranslationList := TBoldIDTranslationList.Create;
  ValueSpace := TBoldFreeStandingValueSpace.Create;
  try
    ObjectIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
    AssertRaisesNotInTransaction(
      procedure
      begin
        ObjectMapper.PMUpdate(ObjectIdList, ValueSpace, nil, TranslationList);
      end, 'PMUpdate');
  finally
    ValueSpace.Free;
    TranslationList.Free;
    ObjectIdList.Free;
  end;
end;

procedure TTestBoldPMTransactionGuards.TestPMDeleteRequiresTransaction;
var
  Obj: TSomeClass;
  ObjectMapper: TBoldObjectDefaultMapper;
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIDTranslationList;
  ValueSpace: TBoldFreeStandingValueSpace;
begin
  Obj := TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  ObjectMapper := ObjectMapperByName(cnSomeClass);
  EnsureNoTransaction;
  ObjectIdList := TBoldObjectIdList.Create;
  TranslationList := TBoldIDTranslationList.Create;
  ValueSpace := TBoldFreeStandingValueSpace.Create;
  try
    ObjectIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
    AssertRaisesNotInTransaction(
      procedure
      begin
        ObjectMapper.PMDelete(ObjectIdList, ValueSpace, nil, TranslationList);
      end, 'PMDelete');
  finally
    ValueSpace.Free;
    TranslationList.Free;
    ObjectIdList.Free;
  end;
end;

{ TTestableSystemMapper }

procedure TTestableSystemMapper.StartTransactionForTest;
begin
  StartTransaction(nil);
end;

procedure TTestableSystemMapper.CommitForTest;
begin
  Commit(nil);
end;

{ TTestBoldSystemSQLMapperStartTransaction }

procedure TTestBoldSystemSQLMapperStartTransaction.SetUp;
begin
  EnsureDM;
  fInTransaction := False;
  fMockDb := TMock<IBoldDatabase>.Create;
  fMockDb.Setup.WillReturn(True).When.GetIsSQLBased;
  fMockDb.Setup.WillExecute(
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    begin
      Result := TValue.From<Boolean>(fInTransaction);
    end).When.GetInTransaction;
  fMapper := TTestableSystemMapper.CreateFromMold(dmUndoRedo.BoldModel1.MoldModel,
    dmUndoRedo.BoldModel1.TypeNameDictionary, nil,
    dmUndoRedo.BoldDatabaseAdapterFireDAC1.SQLDatabaseConfig, GetMockDatabase);
end;

procedure TTestBoldSystemSQLMapperStartTransaction.TearDown;
begin
  FreeAndNil(fMapper);
end;

function TTestBoldSystemSQLMapperStartTransaction.GetMockDatabase: IBoldDatabase;
begin
  Result := fMockDb.Instance;
end;

procedure TTestBoldSystemSQLMapperStartTransaction.TestStartTransactionRaisesWhenDatabaseStartedNone;
begin
  // StartTransaction is accepted but InTransaction stays False, as with an
  // adapter whose StartTransaction silently failed. The mapper must not go on
  // as if it owned a transaction.
  fMockDb.Setup.WillExecute(
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    begin
      Result := TValue.Empty;
    end).When.StartTransaction;
  try
    fMapper.StartTransactionForTest;
    Assert.Fail('StartTransaction returned although the database reports no transaction');
  except
    on E: EBold do
      Assert.IsTrue(Pos('Failed to start transaction', E.Message) > 0, 'Unexpected EBold: ' + E.Message);
  end;
end;

procedure TTestBoldSystemSQLMapperStartTransaction.TestStartTransactionIsCommittedWhenDatabaseStartedOne;
begin
  fMockDb.Setup.WillExecute(
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    begin
      fInTransaction := True;
      Result := TValue.Empty;
    end).When.StartTransaction;
  fMockDb.Setup.Expect.Once.When.Commit;
  fMapper.StartTransactionForTest;
  Assert.IsTrue(fInTransaction, 'precondition: the mocked database started a transaction');
  fMapper.CommitForTest;
  fMockDb.Verify('the mapper must commit the transaction it started');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldPMTransactionGuards);
  TDUnitX.RegisterTestFixture(TTestBoldSystemSQLMapperStartTransaction);

end.
