unit Test.BoldPersistence;

{ Integration tests for Bold database operations with transaction rollback.

  These tests verify database-level operations work correctly with
  transaction rollback for automatic cleanup. They don't require
  generated business classes - they work directly with the database.

  Prerequisites:
  - UnitTest.ini must be configured (any database engine)
}

interface

uses
  DUnitX.TestFramework,
  BoldDBInterfaces,
  BoldTestCaseFireDAC;

type
  { Tests for database connection and transaction operations.
    Uses TBoldTestCaseFireDAC as base but doesn't activate the system. }
  [TestFixture]
  [Category('Integration')]
  [Category('Database')]
  TTestBoldDatabaseOperations = class
  private
    FAdapter: TObject;  // TBoldDatabaseAdapterFireDAC
    FDatabase: IBoldDatabase;
    FInTransaction: Boolean;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDatabaseConnected;

    [Test]
    procedure TestTransactionStartCommit;

    [Test]
    procedure TestTransactionStartRollback;

    [Test]
    procedure TestQueryOpenClose;

    [Test]
    procedure TestQueryWithData;

    [Test]
    procedure TestExecQueryInsert;

    [Test]
    procedure TestTransactionIsolatesChanges;
  end;

implementation

uses
  SysUtils,
  Classes,
  BoldDatabaseAdapterFireDAC,
  BoldTestDatabaseConfig,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

{ TTestBoldDatabaseOperations }

procedure TTestBoldDatabaseOperations.SetUp;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  ExecQuery: IBoldExecQuery;
begin
  FInTransaction := False;

  // Create connection directly (bypassing Bold system)
  Connection := TFDConnection.Create(nil);
  Connection.DriverName := 'SQLite';
  Connection.Params.Values['Database'] := ':memory:';
  Connection.LoginPrompt := False;

  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  Adapter.Connection := Connection;
  FAdapter := Adapter;

  // Open connection
  Connection.Open;
  FDatabase := Adapter.DatabaseInterface;

  // Create a test table
  ExecQuery := FDatabase.GetExecQuery;
  try
    ExecQuery.SQLText := 'CREATE TABLE IF NOT EXISTS TestTable (ID INTEGER PRIMARY KEY, Name TEXT, Value INTEGER)';
    ExecQuery.ExecSQL;
  finally
    FDatabase.ReleaseExecQuery(ExecQuery);
  end;

  // Start transaction for test isolation
  FDatabase.StartTransaction;
  FInTransaction := True;
end;

procedure TTestBoldDatabaseOperations.TearDown;
var
  Adapter: TBoldDatabaseAdapterFireDAC;
  Connection: TFDConnection;
begin
  // Rollback any pending transaction
  if FInTransaction and Assigned(FDatabase) then
  begin
    try
      if FDatabase.InTransaction then
        FDatabase.Rollback;
    except
      // Ignore errors during cleanup
    end;
    FInTransaction := False;
  end;

  FDatabase := nil;

  // Cleanup adapter and connection
  if Assigned(FAdapter) then
  begin
    Adapter := TBoldDatabaseAdapterFireDAC(FAdapter);
    Connection := Adapter.Connection;
    Adapter.Free;
    Connection.Free;
    FAdapter := nil;
  end;
end;

procedure TTestBoldDatabaseOperations.TestDatabaseConnected;
begin
  Assert.IsNotNull(FDatabase, 'Database interface should not be nil');
  Assert.IsTrue(FDatabase.Connected, 'Database should be connected');
end;

procedure TTestBoldDatabaseOperations.TestTransactionStartCommit;
begin
  // We're already in a transaction from SetUp
  Assert.IsTrue(FDatabase.InTransaction, 'Should be in transaction');

  // Commit it
  FDatabase.Commit;
  FInTransaction := False;
  Assert.IsFalse(FDatabase.InTransaction, 'Should not be in transaction after commit');

  // Start a new one for the next test
  FDatabase.StartTransaction;
  FInTransaction := True;
  Assert.IsTrue(FDatabase.InTransaction, 'Should be in new transaction');
end;

procedure TTestBoldDatabaseOperations.TestTransactionStartRollback;
begin
  Assert.IsTrue(FDatabase.InTransaction, 'Should be in transaction');

  // Rollback
  FDatabase.Rollback;
  FInTransaction := False;
  Assert.IsFalse(FDatabase.InTransaction, 'Should not be in transaction after rollback');

  // Start a new one
  FDatabase.StartTransaction;
  FInTransaction := True;
end;

procedure TTestBoldDatabaseOperations.TestQueryOpenClose;
var
  Query: IBoldQuery;
begin
  Query := FDatabase.GetQuery;
  try
    Query.SQLText := 'SELECT * FROM TestTable';
    Query.Open;
    // Eof should be True for empty table
    Assert.IsTrue(Query.Eof, 'Table should be empty');
    Query.Close;
  finally
    FDatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldDatabaseOperations.TestQueryWithData;
var
  Query: IBoldQuery;
  ExecQuery: IBoldExecQuery;
begin
  // Insert test data
  ExecQuery := FDatabase.GetExecQuery;
  try
    ExecQuery.SQLText := 'INSERT INTO TestTable (ID, Name, Value) VALUES (1, ''Test'', 100)';
    ExecQuery.ExecSQL;
  finally
    FDatabase.ReleaseExecQuery(ExecQuery);
  end;

  // Query the data
  Query := FDatabase.GetQuery;
  try
    Query.SQLText := 'SELECT * FROM TestTable WHERE ID = 1';
    Query.Open;

    Assert.IsFalse(Query.Eof, 'Should have data');
    Assert.AreEqual(1, Query.FieldByName('ID').AsInteger, 'ID should be 1');
    Assert.AreEqual('Test', Query.FieldByName('Name').AsString, 'Name should be Test');
    Assert.AreEqual(100, Query.FieldByName('Value').AsInteger, 'Value should be 100');

    Query.Close;
  finally
    FDatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldDatabaseOperations.TestExecQueryInsert;
var
  ExecQuery: IBoldExecQuery;
  Query: IBoldQuery;
begin
  ExecQuery := FDatabase.GetExecQuery;
  try
    ExecQuery.SQLText := 'INSERT INTO TestTable (ID, Name, Value) VALUES (2, ''Another'', 200)';
    ExecQuery.ExecSQL;
    Assert.AreEqual(1, ExecQuery.RowsAffected, 'Should affect 1 row');
  finally
    FDatabase.ReleaseExecQuery(ExecQuery);
  end;

  // Verify insert
  Query := FDatabase.GetQuery;
  try
    Query.SQLText := 'SELECT COUNT(*) AS Cnt FROM TestTable WHERE ID = 2';
    Query.Open;
    Assert.AreEqual(1, Query.FieldByName('Cnt').AsInteger, 'Should find 1 row');
    Query.Close;
  finally
    FDatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldDatabaseOperations.TestTransactionIsolatesChanges;
var
  ExecQuery: IBoldExecQuery;
  Query: IBoldQuery;
begin
  // Insert data in transaction
  ExecQuery := FDatabase.GetExecQuery;
  try
    ExecQuery.SQLText := 'INSERT INTO TestTable (ID, Name, Value) VALUES (99, ''WillRollback'', 999)';
    ExecQuery.ExecSQL;
  finally
    FDatabase.ReleaseExecQuery(ExecQuery);
  end;

  // Verify it exists before rollback
  Query := FDatabase.GetQuery;
  try
    Query.SQLText := 'SELECT COUNT(*) AS Cnt FROM TestTable WHERE ID = 99';
    Query.Open;
    Assert.AreEqual(1, Query.FieldByName('Cnt').AsInteger, 'Should exist before rollback');
    Query.Close;
  finally
    FDatabase.ReleaseQuery(Query);
  end;

  // Rollback
  FDatabase.Rollback;
  FInTransaction := False;

  // Start new transaction (needed for next query)
  FDatabase.StartTransaction;
  FInTransaction := True;

  // Verify data is gone after rollback
  Query := FDatabase.GetQuery;
  try
    Query.SQLText := 'SELECT COUNT(*) AS Cnt FROM TestTable WHERE ID = 99';
    Query.Open;
    Assert.AreEqual(0, Query.FieldByName('Cnt').AsInteger, 'Should NOT exist after rollback');
    Query.Close;
  finally
    FDatabase.ReleaseQuery(Query);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDatabaseOperations);

end.
