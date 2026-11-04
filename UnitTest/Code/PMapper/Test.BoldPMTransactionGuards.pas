unit Test.BoldPMTransactionGuards;

{ Transaction guards in the SQL persistence mappers.

  PMCreate, PMUpdate and PMDelete write to several tables and must refuse to
  run outside a database transaction, and TBoldSystemSQLMapper.StartTransaction
  must raise when the database still reports no transaction afterwards.
  Without these guards a failure half-way through a save leaves the rows of the
  earlier statements committed - the dangling BOLD_ID corruption.

  Shared between DUnitX and FPCUnit. The guard fixture runs on dmUndoRedo
  (FireDAC under Delphi, Zeos under Free Pascal); the StartTransaction fixture
  uses a hand-written fake database, as Delphi-Mocks needs Delphi's extended
  RTTI. }

interface

uses
  SysUtils,
  Classes,
  BoldTestFramework,
  BoldBase,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldId,
  BoldFreeStandingValues,
  BoldPMappersSQL,
  BoldPMappersDefault;

type
  TGuardedCall = procedure of object;

  { Calls the object mapper's PM methods directly on the SQLite test database
    with no transaction open. }
  {$IFNDEF FPC}
  [Category('PMapper')]
  {$ENDIF}
  TTestBoldPMTransactionGuards = class(TBoldTestCase)
  strict private
    // What the guarded calls work on; Free Pascal 3.2.2 has no anonymous
    // methods, so the calls are methods and their arguments fields.
    fObjectMapper: TBoldObjectDefaultMapper;
    fObjectIdList: TBoldObjectIdList;
    fTranslationList: TBoldIDTranslationList;
    fValueSpace: TBoldFreeStandingValueSpace;
    function SystemMapper: TBoldSystemSQLMapper;
    function ObjectMapperByName(const aClassName: string): TBoldObjectDefaultMapper;
    procedure EnsureNoTransaction;
    procedure AssertRaisesNotInTransaction(aCall: TGuardedCall; const aOperation: string);
    procedure CallPMCreate;
    procedure CallPMUpdate;
    procedure CallPMDelete;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPMCreateRequiresTransaction;
    procedure TestPMUpdateRequiresTransaction;
    procedure TestPMDeleteRequiresTransaction;
  end;

  { Exposes the protected transaction methods of the SQL system mapper. }
  TTestableSystemMapper = class(TBoldSystemDefaultMapper)
  public
    procedure StartTransactionForTest;
    procedure CommitForTest;
  end;

  { An IBoldDatabase for the StartTransaction fixture: StartTransaction opens a
    transaction only when StartsTransaction is set, and Commit is counted.
    Every other member returns a default value or does nothing, as the loose
    Delphi-Mocks mock it replaces did. Not reference counted: the test frees
    it after the mapper that holds it. }
  TFakeTransactionDatabase = class(TBoldNonRefCountedObject, IBoldDatabase)
  private
    fSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    fStartsTransaction: Boolean;
    fInTransaction: Boolean;
    fCommitCount: Integer;
    procedure StartTransaction;
    procedure Commit;
    procedure RollBack;
    procedure Open;
    procedure Close;
    procedure Reconnect;
    function GetInTransaction: Boolean;
    function GetIsSQLBased: Boolean;
    procedure AllTableNames(Pattern: String; SystemTables: Boolean; TableNameList: TStrings);
    procedure SetlogInPrompt(NewValue: Boolean);
    function GetLogInPrompt: Boolean;
    procedure SetKeepConnection(NewValue: Boolean);
    function GetKeepConnection: Boolean;
    function GetConnected: Boolean;
    function SupportsDefaultColumnValues: Boolean;
    function GetQuery: IBoldQuery;
    procedure ReleaseQuery(var Query: IBoldQuery);
    function GetExecQuery: IBoldExecQuery;
    procedure ReleaseExecQuery(var Query: IBoldExecQuery);
    function GetTable: IBoldTable;
    procedure ReleaseTable(var Table: IBoldTable);
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
    function TableExists(const TableName: String): Boolean;
    function GetIndexDescriptions(const TableName: String): TBoldIndexDescriptionArray;
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    function GetIsExecutingQuery: Boolean;
    procedure CreateDatabase(DropExisting: boolean = true);
    procedure DropDatabase;
    function DatabaseExists: boolean;
    function CreateAnotherDatabaseConnection: IBoldDatabase;
    procedure ReleaseAnotherDatabaseConnection(var ADatabase: IBoldDatabase);
    function GetImplementor: TObject;
  public
    constructor Create(ASQLDatabaseConfig: TBoldSQLDatabaseConfig);
    property StartsTransaction: Boolean read fStartsTransaction write fStartsTransaction;
    property InTransaction: Boolean read fInTransaction;
    property CommitCount: Integer read fCommitCount;
  end;

  { Builds a system mapper over the fake database, so the test decides what
    InTransaction reports after StartTransaction. }
  {$IFNDEF FPC}
  [Category('PMapper')]
  {$ENDIF}
  TTestBoldSystemSQLMapperStartTransaction = class(TBoldTestCase)
  strict private
    fDatabase: TFakeTransactionDatabase;
    fMapper: TTestableSystemMapper;
    function GetFakeDatabase: IBoldDatabase;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStartTransactionRaisesWhenDatabaseStartedNone;
    procedure TestStartTransactionIsCommittedWhenDatabaseStartedOne;
  end;

implementation

uses
  BoldDefs,
  BoldDefaultId,
  BoldSystem,
  BoldTestModel,
  maan_UndoRedoBase;

const
  cnSomeClass = 'SomeClass';

{ TTestBoldPMTransactionGuards }

procedure TTestBoldPMTransactionGuards.SetUp;
begin
  inherited;
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
  fObjectIdList := TBoldObjectIdList.Create;
  fTranslationList := TBoldIDTranslationList.Create;
  fValueSpace := TBoldFreeStandingValueSpace.Create;
end;

procedure TTestBoldPMTransactionGuards.TearDown;
begin
  FreeAndNil(fValueSpace);
  FreeAndNil(fTranslationList);
  FreeAndNil(fObjectIdList);
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
  inherited;
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

procedure TTestBoldPMTransactionGuards.AssertRaisesNotInTransaction(aCall: TGuardedCall; const aOperation: string);
begin
  try
    aCall;
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

procedure TTestBoldPMTransactionGuards.CallPMCreate;
begin
  fObjectMapper.PMCreate(fObjectIdList, fValueSpace, fTranslationList);
end;

procedure TTestBoldPMTransactionGuards.CallPMUpdate;
begin
  fObjectMapper.PMUpdate(fObjectIdList, fValueSpace, nil, fTranslationList);
end;

procedure TTestBoldPMTransactionGuards.CallPMDelete;
begin
  fObjectMapper.PMDelete(fObjectIdList, fValueSpace, nil, fTranslationList);
end;

procedure TTestBoldPMTransactionGuards.TestPMCreateRequiresTransaction;
var
  NewId: TBoldDefaultId;
begin
  fObjectMapper := ObjectMapperByName(cnSomeClass);
  EnsureNoTransaction;
  NewId := TBoldDefaultId.CreateWithClassId(fObjectMapper.TopSortedIndex, True);
  try
    NewId.AsInteger := -99999;
    fObjectIdList.Add(NewId);
    AssertRaisesNotInTransaction(CallPMCreate, 'PMCreate');
  finally
    NewId.Free;
  end;
end;

procedure TTestBoldPMTransactionGuards.TestPMUpdateRequiresTransaction;
var
  Obj: TSomeClass;
begin
  Obj := TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  fObjectMapper := ObjectMapperByName(cnSomeClass);
  EnsureNoTransaction;
  fObjectIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
  AssertRaisesNotInTransaction(CallPMUpdate, 'PMUpdate');
end;

procedure TTestBoldPMTransactionGuards.TestPMDeleteRequiresTransaction;
var
  Obj: TSomeClass;
begin
  Obj := TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  fObjectMapper := ObjectMapperByName(cnSomeClass);
  EnsureNoTransaction;
  fObjectIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
  AssertRaisesNotInTransaction(CallPMDelete, 'PMDelete');
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

{ TFakeTransactionDatabase }

constructor TFakeTransactionDatabase.Create(ASQLDatabaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create;
  fSQLDatabaseConfig := ASQLDatabaseConfig;
end;

procedure TFakeTransactionDatabase.StartTransaction;
begin
  if fStartsTransaction then
    fInTransaction := True;
end;

procedure TFakeTransactionDatabase.Commit;
begin
  inc(fCommitCount);
end;

procedure TFakeTransactionDatabase.RollBack;
begin
end;

procedure TFakeTransactionDatabase.Open;
begin
end;

procedure TFakeTransactionDatabase.Close;
begin
end;

procedure TFakeTransactionDatabase.Reconnect;
begin
end;

function TFakeTransactionDatabase.GetInTransaction: Boolean;
begin
  Result := fInTransaction;
end;

function TFakeTransactionDatabase.GetIsSQLBased: Boolean;
begin
  Result := True;
end;

procedure TFakeTransactionDatabase.AllTableNames(Pattern: String; SystemTables: Boolean; TableNameList: TStrings);
begin
end;

procedure TFakeTransactionDatabase.SetlogInPrompt(NewValue: Boolean);
begin
end;

function TFakeTransactionDatabase.GetLogInPrompt: Boolean;
begin
  Result := False;
end;

procedure TFakeTransactionDatabase.SetKeepConnection(NewValue: Boolean);
begin
end;

function TFakeTransactionDatabase.GetKeepConnection: Boolean;
begin
  Result := False;
end;

function TFakeTransactionDatabase.GetConnected: Boolean;
begin
  Result := False;
end;

function TFakeTransactionDatabase.SupportsDefaultColumnValues: Boolean;
begin
  Result := False;
end;

function TFakeTransactionDatabase.GetQuery: IBoldQuery;
begin
  Result := nil;
end;

procedure TFakeTransactionDatabase.ReleaseQuery(var Query: IBoldQuery);
begin
  Query := nil;
end;

function TFakeTransactionDatabase.GetExecQuery: IBoldExecQuery;
begin
  Result := nil;
end;

procedure TFakeTransactionDatabase.ReleaseExecQuery(var Query: IBoldExecQuery);
begin
  Query := nil;
end;

function TFakeTransactionDatabase.GetTable: IBoldTable;
begin
  Result := nil;
end;

procedure TFakeTransactionDatabase.ReleaseTable(var Table: IBoldTable);
begin
  Table := nil;
end;

function TFakeTransactionDatabase.SupportsTableCreation: Boolean;
begin
  Result := False;
end;

procedure TFakeTransactionDatabase.ReleaseCachedObjects;
begin
end;

function TFakeTransactionDatabase.TableExists(const TableName: String): Boolean;
begin
  Result := False;
end;

function TFakeTransactionDatabase.GetIndexDescriptions(const TableName: String): TBoldIndexDescriptionArray;
begin
  Result := nil;
end;

function TFakeTransactionDatabase.GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
begin
  Result := fSQLDatabaseConfig;
end;

function TFakeTransactionDatabase.GetIsExecutingQuery: Boolean;
begin
  Result := False;
end;

procedure TFakeTransactionDatabase.CreateDatabase(DropExisting: boolean);
begin
end;

procedure TFakeTransactionDatabase.DropDatabase;
begin
end;

function TFakeTransactionDatabase.DatabaseExists: boolean;
begin
  Result := False;
end;

function TFakeTransactionDatabase.CreateAnotherDatabaseConnection: IBoldDatabase;
begin
  Result := nil;
end;

procedure TFakeTransactionDatabase.ReleaseAnotherDatabaseConnection(var ADatabase: IBoldDatabase);
begin
  ADatabase := nil;
end;

function TFakeTransactionDatabase.GetImplementor: TObject;
begin
  Result := Self;
end;

{ TTestBoldSystemSQLMapperStartTransaction }

procedure TTestBoldSystemSQLMapperStartTransaction.SetUp;
var
  Config: TBoldSQLDatabaseConfig;
begin
  inherited;
  EnsureDM;
  Config := dmUndoRedo.BoldPersistenceHandleDB1.DatabaseAdapter.SQLDatabaseConfig;
  fDatabase := TFakeTransactionDatabase.Create(Config);
  fMapper := TTestableSystemMapper.CreateFromMold(dmUndoRedo.BoldModel1.MoldModel,
    dmUndoRedo.BoldModel1.TypeNameDictionary, nil, Config, GetFakeDatabase);
end;

procedure TTestBoldSystemSQLMapperStartTransaction.TearDown;
begin
  FreeAndNil(fMapper);
  FreeAndNil(fDatabase); // after the mapper, which holds it
  inherited;
end;

function TTestBoldSystemSQLMapperStartTransaction.GetFakeDatabase: IBoldDatabase;
begin
  Result := fDatabase;
end;

procedure TTestBoldSystemSQLMapperStartTransaction.TestStartTransactionRaisesWhenDatabaseStartedNone;
begin
  // StartTransaction is accepted but InTransaction stays False, as with an
  // adapter whose StartTransaction silently failed. The mapper must not go on
  // as if it owned a transaction.
  fDatabase.StartsTransaction := False;
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
  fDatabase.StartsTransaction := True;
  fMapper.StartTransactionForTest;
  Assert.IsTrue(fDatabase.InTransaction, 'precondition: the fake database started a transaction');
  fMapper.CommitForTest;
  Assert.AreEqual(1, fDatabase.CommitCount, 'the mapper must commit the transaction it started');
end;

initialization
  BoldRegisterTest(TTestBoldPMTransactionGuards);
  BoldRegisterTest(TTestBoldSystemSQLMapperStartTransaction);

end.
