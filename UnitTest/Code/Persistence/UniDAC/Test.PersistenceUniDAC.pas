{$include bold.inc}
unit Test.PersistenceUniDAC;

interface

uses
  DUnitX.TestFramework,
  BoldTestCaseUniDAC;

type
  [TestFixture]
  [Category('Persistence')]
  TTestPersistenceUniDAC = class(TBoldTestCaseUniDAC)
  public
    [Test]
    [Category('Slow')]
    procedure TestCreateObject;
    [Test]
    [Category('Slow')]
    procedure TestPersistAndReload;
    [Test]
    [Category('DB')]
    procedure TestReleaseQueryCommitsStartedReadTransaction;
    [Test]
    [Category('DB')]
    procedure TestExecSQLKeepsOwnershipOfOwnReadTransaction;
    [Test]
    [Category('DB')]
    procedure TestReopenKeepsOwnershipOfOwnReadTransaction;
    [Test]
    [Category('DB')]
    procedure TestDestroyFreesCachedQueries;
  end;

implementation

uses
  System.SysUtils,
  BoldUniDACInterfaces,
  BoldSystem,
  BoldId,
  BoldDBInterfaces;

{ TTestPersistenceUniDAC }

procedure TTestPersistenceUniDAC.TestCreateObject;
var
  Obj: TBoldObject;
begin
  Obj := CreateObject('TestClass');
  Assert.IsNotNull(Obj, 'Object should be created');
  Assert.AreEqual('TestClass', Obj.BoldClassTypeInfo.ExpressionName);
end;

procedure TTestPersistenceUniDAC.TestPersistAndReload;
var
  Obj: TBoldObject;
  ObjId: TBoldObjectId;
  ReloadedObj: TBoldObject;
begin
  // Create object and set attribute
  Obj := CreateObject('TestClass');
  SetAttributeAsString(Obj, 'Name', 'TestValue');

  // Save to database
  UpdateDatabase;

  // Get the ID before refresh
  ObjId := Obj.BoldObjectLocator.BoldObjectID.Clone;
  try
    // Refresh to reload from database
    RefreshSystem;

    // Find the object again and verify
    ReloadedObj := FindObjectById(ObjId);
    Assert.IsNotNull(ReloadedObj, 'Object should be reloaded from database');
    AssertAttributeEquals(ReloadedObj, 'Name', 'TestValue');
  finally
    ObjId.Free;
  end;
end;

procedure TTestPersistenceUniDAC.TestReleaseQueryCommitsStartedReadTransaction;
var
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  DbInterface := UniDACAdapter.DatabaseInterface;
  UniConnection.ExecSQL('DROP TABLE IF EXISTS Bold_TxRelease');
  UniConnection.ExecSQL('CREATE TABLE Bold_TxRelease (ID INT)');

  // A caller that releases a query back to the cache without closing it
  // first must not leave the read transaction the query started behind:
  // its row locks stay on the shared connection until it is recycled.
  Query := DbInterface.GetQuery;
  Query.UseReadTransactions := True;
  Query.SQLText := 'SELECT ID FROM Bold_TxRelease';
  Query.Open;
  Assert.IsTrue(DbInterface.InTransaction,
    'Opening with UseReadTransactions should have started a read transaction');

  DbInterface.ReleaseQuery(Query);
  Assert.IsFalse(DbInterface.InTransaction,
    'Releasing the query must commit the read transaction it started');

  UniConnection.ExecSQL('DROP TABLE Bold_TxRelease');
end;

procedure TTestPersistenceUniDAC.TestExecSQLKeepsOwnershipOfOwnReadTransaction;
var
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
  ExecIntf: IBoldExecQuery;
begin
  DbInterface := UniDACAdapter.DatabaseInterface;
  UniConnection.ExecSQL('DROP TABLE IF EXISTS Bold_TxExec');
  UniConnection.ExecSQL('CREATE TABLE Bold_TxExec (ID INT)');

  Query := DbInterface.GetQuery;
  try
    Query.UseReadTransactions := True;
    Query.SQLText := 'SELECT ID FROM Bold_TxExec';
    Query.Open;
    Assert.IsTrue(DbInterface.InTransaction,
      'Opening with UseReadTransactions should have started a read transaction');

    // Assigning new SQL closes the dataset behind the wrapper, so the read
    // transaction is still open and still owned by this query when ExecSQL
    // runs. ExecSQL must keep that ownership and commit at the end instead
    // of dropping the flag because the connection is "already in a
    // transaction" - that is exactly how the transaction gets orphaned.
    Query.SQLText := 'DELETE FROM Bold_TxExec';
    Assert.IsTrue(DbInterface.InTransaction,
      'Read transaction should still be open after reassigning the SQL');

    // The very same wrapper also implements IBoldExecQuery, so this is the
    // ExecSQL path of the query that owns the open transaction.
    ExecIntf := Query as IBoldExecQuery;
    try
      ExecIntf.ExecSQL;
    finally
      ExecIntf := nil;
    end;
    Assert.IsFalse(DbInterface.InTransaction,
      'ExecSQL must commit the read transaction this query started');
  finally
    DbInterface.ReleaseQuery(Query);
  end;

  UniConnection.ExecSQL('DROP TABLE Bold_TxExec');
end;

procedure TTestPersistenceUniDAC.TestReopenKeepsOwnershipOfOwnReadTransaction;
var
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  DbInterface := UniDACAdapter.DatabaseInterface;
  UniConnection.ExecSQL('DROP TABLE IF EXISTS Bold_TxReopen');
  UniConnection.ExecSQL('CREATE TABLE Bold_TxReopen (ID INT)');

  Query := DbInterface.GetQuery;
  try
    Query.UseReadTransactions := True;
    Query.SQLText := 'SELECT ID FROM Bold_TxReopen';
    Query.Open;
    Assert.IsTrue(DbInterface.InTransaction,
      'Opening with UseReadTransactions should have started a read transaction');

    // Reopen without closing: the connection is already in the transaction
    // this very query started, so Open must not hand ownership away - Close
    // below is the only thing that can commit it.
    Query.SQLText := 'SELECT ID FROM Bold_TxReopen';
    Query.Open;

    Query.Close;
    Assert.IsFalse(DbInterface.InTransaction,
      'Close must commit the read transaction this query started');
  finally
    DbInterface.ReleaseQuery(Query);
  end;

  UniConnection.ExecSQL('DROP TABLE Bold_TxReopen');
end;

function CurrentAllocatedBlocks: Int64;
var
  st: TMemoryManagerState;
  i: Integer;
begin
  GetMemoryManagerState(st);
  Result := Int64(st.AllocatedMediumBlockCount) + Int64(st.AllocatedLargeBlockCount);
  for i := Low(st.SmallBlockTypeStates) to High(st.SmallBlockTypeStates) do
    Result := Result + Int64(st.SmallBlockTypeStates[i].AllocatedBlockCount);
end;

procedure TTestPersistenceUniDAC.TestDestroyFreesCachedQueries;

  procedure UseAndDestroyWrapper;
  var
    Wrapper: TBoldUniDACConnection;
    DB: IBoldDatabase;
    Query: IBoldQuery;
    ExecQuery: IBoldExecQuery;
  begin
    // A second wrapper on the fixture's TUniConnection; the wrapper does not
    // own the component, so only the wrapper and its cache go away here.
    Wrapper := TBoldUniDACConnection.Create(UniConnection, UniDACAdapter.SQLDatabaseConfig);
    DB := Wrapper; // the public surface; the wrapper is not reference counted
    try
      Query := DB.GetQuery;
      DB.ReleaseQuery(Query);          // now sits in the wrapper's query cache
      ExecQuery := DB.GetExecQuery;
      DB.ReleaseExecQuery(ExecQuery);  // now sits in the wrapper's exec-query cache
    finally
      DB := nil;
      Wrapper.Free;
    end;
  end;

var
  Before, After: Int64;
begin
  // First pass absorbs one-time allocations of the DAC layer; the second
  // pass must be allocation-neutral: destroying the wrapper has to free the
  // query and exec query it cached, not hand them back to the cache.
  UseAndDestroyWrapper;
  Before := CurrentAllocatedBlocks;
  UseAndDestroyWrapper;
  After := CurrentAllocatedBlocks;
  Assert.AreEqual(Before, After,
    'Destroying a TBoldUniDACConnection must free its cached query and exec query ' +
    '(allocated blocks grew by ' + IntToStr(After - Before) + ')');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceUniDAC);

end.
