{$include bold.inc}
unit Test.PersistenceUniDAC;

{$IFDEF UniDAC}

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
  end;

implementation

uses
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

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceUniDAC);

{$ELSE}

interface

implementation

{$ENDIF}

end.
