unit BoldDatabaseOperationsTestCase;

{ Integration tests for Bold database operations with transaction rollback.

  They work directly with IBoldDatabase, IBoldQuery and IBoldExecQuery on a
  private in-memory SQLite database, with no Bold system and no generated
  business classes: connection, transactions, queries and exec queries. Each
  test runs in a transaction that TearDown rolls back.

  The tests are written once here, against an abstract database. A runner
  registers a subclass that opens the database with its adapter:
    Delphi:      Test.BoldPersistence (FireDAC), TTestBoldDatabaseOperations
    Free Pascal: Zeos\Test.BoldPersistenceZeos, TTestBoldDatabaseOperationsZeos }

interface

uses
  BoldTestFramework,
  BoldDBInterfaces;

type
  TBoldDatabaseOperationsTestCase = class(TBoldTestCase)
  private
    FDatabase: IBoldDatabase;
    FInTransaction: Boolean;
  protected
    { Creates a connection and a database adapter for a new, empty in-memory
      SQLite database, opens it and returns the adapter's interface. }
    function OpenDatabase: IBoldDatabase; virtual; abstract;
    { Frees what OpenDatabase created. }
    procedure CloseDatabase; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDatabaseConnected;
    procedure TestTransactionStartCommit;
    procedure TestTransactionStartRollback;
    procedure TestQueryOpenClose;
    procedure TestQueryWithData;
    procedure TestExecQueryInsert;
    procedure TestTransactionIsolatesChanges;
  end;

implementation

{ TBoldDatabaseOperationsTestCase }

procedure TBoldDatabaseOperationsTestCase.SetUp;
var
  ExecQuery: IBoldExecQuery;
begin
  inherited;
  FInTransaction := False;
  FDatabase := OpenDatabase;

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

procedure TBoldDatabaseOperationsTestCase.TearDown;
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
  CloseDatabase;
  inherited;
end;

procedure TBoldDatabaseOperationsTestCase.TestDatabaseConnected;
begin
  Assert.IsNotNull(FDatabase, 'Database interface should not be nil');
  Assert.IsTrue(FDatabase.Connected, 'Database should be connected');
end;

procedure TBoldDatabaseOperationsTestCase.TestTransactionStartCommit;
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

procedure TBoldDatabaseOperationsTestCase.TestTransactionStartRollback;
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

procedure TBoldDatabaseOperationsTestCase.TestQueryOpenClose;
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

procedure TBoldDatabaseOperationsTestCase.TestQueryWithData;
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

procedure TBoldDatabaseOperationsTestCase.TestExecQueryInsert;
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

procedure TBoldDatabaseOperationsTestCase.TestTransactionIsolatesChanges;
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

end.
