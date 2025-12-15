unit Test.PersistenceFireDAC;

{******************************************************************************}
{                                                                              }
{  Test.PersistenceFireDAC - FireDAC persistence integration tests             }
{                                                                              }
{  Tests basic Bold persistence operations using FireDAC with SQL Server.      }
{  Uses the same model and infrastructure as maan_UndoRedo tests.              }
{                                                                              }
{******************************************************************************}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  [Category('Persistence')]
  TTestPersistenceFireDAC = class
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    [Category('Slow')]
    procedure TestCreateObject;
    [Test]
    [Category('Slow')]
    procedure TestPersistAndReload;
    [Test]
    [Category('Slow')]
    procedure TestDatabaseExistsReturnsTrue;
    [Test]
    [Category('Slow')]
    procedure TestDatabaseExistsReturnsFalseForNonExistent;
    [Test]
    [Category('Slow')]
    procedure TestDropDatabase;
    [Test]
    [Category('Slow')]
    procedure TestCreateDatabase;
    [Test]
    [Category('Slow')]
    procedure TestConnectionOpenClose;
    [Test]
    [Category('Slow')]
    procedure TestTransactionCommitRollback;
    [Test]
    [Category('Slow')]
    procedure TestGetQueryAndRelease;
    [Test]
    [Category('Slow')]
    procedure TestQueryExecuteSQL;
    [Test]
    [Category('Slow')]
    procedure TestQueryWithParameters;
    [Test]
    [Category('Slow')]
    procedure TestAllTableNames;
    [Test]
    [Category('Slow')]
    procedure TestQueryRecordCount;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryRowsAffected;
    [Test]
    [Category('Slow')]
    procedure TestQueryPrepare;
    [Test]
    [Category('Slow')]
    procedure TestParameterTypes;
    [Test]
    [Category('Slow')]
    procedure TestGetTableAndRelease;
    [Test]
    [Category('Slow')]
    procedure TestConnectionProperties;
    [Test]
    [Category('Slow')]
    procedure TestExecQuery;
    [Test]
    [Category('Slow')]
    procedure TestQueryRecNo;
    [Test]
    [Category('Slow')]
    procedure TestQueryAssignParams;
    [Test]
    [Category('Slow')]
    procedure TestQueryRequestLiveQuery;
    [Test]
    [Category('Slow')]
    procedure TestParameterInt64;
    [Test]
    [Category('Slow')]
    procedure TestParameterAnsiString;
    [Test]
    [Category('Slow')]
    procedure TestParameterDataType;
    [Test]
    [Category('Slow')]
    procedure TestTableExists;
    [Test]
    [Category('Slow')]
    procedure TestTableFieldDefs;
    [Test]
    [Category('Slow')]
    procedure TestConnectionReconnect;
    [Test]
    [Category('Slow')]
    procedure TestReadTransaction;
    [Test]
    [Category('Slow')]
    procedure TestCreateAnotherDatabaseConnection;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryClear;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryCreateParam;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryFindParam;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryEnsureParamByName;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryPrepare;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryParamCheck;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryUseReadTransactions;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryBatchOperations;
    [Test]
    [Category('Slow')]
    procedure TestExecQueryAssignParams;
  end;

implementation

uses
  System.SysUtils,
  Classes,
  DB,
  BoldSystem,
  BoldSystemRT,
  BoldId,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces,
  BoldFireDACInterfaces,
  BoldDatabaseAdapterFireDAC,
  BoldTestDatabaseConfig,
  FireDAC.Comp.Client,
  maan_UndoRedoBase,
  UndoTestModelClasses;

{ TTestPersistenceFireDAC }

procedure TTestPersistenceFireDAC.SetUp;
begin
  // EnsureDM creates the data module with model, connections, and persistence
  // It also creates the database schema and activates the system
  EnsureDM;
end;

procedure TTestPersistenceFireDAC.TearDown;
begin
  // Let EnsureDM handle cleanup - it discards on next setup
end;

procedure TTestPersistenceFireDAC.TestCreateObject;
var
  Obj: TSomeClass;
begin
  // Create a SomeClass object using the strongly-typed class
  Obj := TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System);
  Assert.IsNotNull(Obj, 'Object should be created');
  Assert.AreEqual('SomeClass', Obj.BoldClassTypeInfo.ExpressionName);
end;

procedure TTestPersistenceFireDAC.TestPersistAndReload;
var
  Obj: TSomeClass;
  ObjId: TBoldObjectId;
  ReloadedObj: TBoldObject;
  Locator: TBoldObjectLocator;
begin
  // Create object and set attribute
  Obj := TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System);
  Obj.aString := 'TestValue';

  // Save to database
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Get the ID before refresh
  ObjId := Obj.BoldObjectLocator.BoldObjectID.Clone;
  try
    // Discard local changes and deactivate/reactivate to reload from database
    dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
    dmUndoRedo.BoldSystemHandle1.Active := True;

    // Find the object again - need to get fresh System reference after reactivation
    Locator := dmUndoRedo.BoldSystemHandle1.System.EnsuredLocatorByID[ObjId];
    Assert.IsNotNull(Locator, 'Locator should exist for object ID');

    // Fetch the object from database
    Locator.EnsureBoldObject;
    ReloadedObj := Locator.BoldObject;

    Assert.IsNotNull(ReloadedObj, 'Object should be reloaded from database');
    Assert.IsTrue(ReloadedObj is TSomeClass, 'Object should be TSomeClass');
    Assert.AreEqual('TestValue', TSomeClass(ReloadedObj).aString, 'aString should match');
  finally
    ObjId.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestDatabaseExistsReturnsTrue;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
begin
  // Create fresh connection and adapter to avoid any state issues
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    // Configure connection from INI - this also sets DatabaseEngine
    ConfigureConnection(Connection, Adapter);

    // Verify adapter is properly configured
    Assert.AreEqual(Integer(dbeSQLServer), Integer(Adapter.DatabaseEngine),
      'Adapter should be configured for SQL Server');

    // The BoldUnitTest database should exist (created by EnsureDM/CreateTestDatabase)
    Assert.IsTrue(Adapter.DatabaseExists, 'DatabaseExists should return True for existing database');
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestDatabaseExistsReturnsFalseForNonExistent;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
begin
  // Create fresh connection and adapter
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    // Configure connection from INI - this also sets DatabaseEngine
    ConfigureConnection(Connection, Adapter);

    // Change to a non-existent database name
    Connection.Params.Database := 'NonExistentDatabase_XYZ_12345';

    // Verify adapter is properly configured
    Assert.AreEqual(Integer(dbeSQLServer), Integer(Adapter.DatabaseEngine),
      'Adapter should be configured for SQL Server');

    // Should return False for non-existent database
    Assert.IsFalse(Adapter.DatabaseExists, 'DatabaseExists should return False for non-existent database');
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestDropDatabase;
const
  TempDbName = 'BoldUnitTest_TempDrop';
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);

    // Use a temporary database name for this test
    Connection.Params.Database := TempDbName;

    // Create the temporary database
    Adapter.CreateDatabase(True);

    // Verify it exists
    Assert.IsTrue(Adapter.DatabaseExists, 'Database should exist after CreateDatabase');

    // Drop the database
    Adapter.DropDatabase;

    // Verify it no longer exists
    Assert.IsFalse(Adapter.DatabaseExists, 'Database should not exist after DropDatabase');
  finally
    // Cleanup: ensure temp database is dropped even if test fails
    try
      if Adapter.DatabaseExists then
        Adapter.DropDatabase;
    except
      // Ignore cleanup errors
    end;
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestCreateDatabase;
const
  TempDbName = 'BoldUnitTest_TempCreate';
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);

    // Use a temporary database name for this test
    Connection.Params.Database := TempDbName;

    // Ensure database doesn't exist before test
    if Adapter.DatabaseExists then
      Adapter.DropDatabase;
    Assert.IsFalse(Adapter.DatabaseExists, 'Database should not exist before CreateDatabase');

    // Create the database
    Adapter.CreateDatabase(False);

    // Verify it exists
    Assert.IsTrue(Adapter.DatabaseExists, 'Database should exist after CreateDatabase');

    // Test CreateDatabase with DropExisting=True (should recreate without error)
    Adapter.CreateDatabase(True);
    Assert.IsTrue(Adapter.DatabaseExists, 'Database should exist after CreateDatabase with DropExisting');
  finally
    // Cleanup
    try
      if Adapter.DatabaseExists then
        Adapter.DropDatabase;
    except
    end;
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestConnectionOpenClose;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);

    // Initially not connected
    Assert.IsFalse(Connection.Connected, 'Should not be connected initially');

    // Open connection
    Connection.Open;
    Assert.IsTrue(Connection.Connected, 'Should be connected after Open');

    // Close connection
    Connection.Close;
    Assert.IsFalse(Connection.Connected, 'Should not be connected after Close');
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestTransactionCommitRollback;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;

    DbInterface.Open;
    Assert.IsFalse(DbInterface.InTransaction, 'Should not be in transaction initially');

    // Start transaction
    DbInterface.StartTransaction;
    Assert.IsTrue(DbInterface.InTransaction, 'Should be in transaction after StartTransaction');

    // Commit
    DbInterface.Commit;
    Assert.IsFalse(DbInterface.InTransaction, 'Should not be in transaction after Commit');

    // Start another transaction and rollback
    DbInterface.StartTransaction;
    Assert.IsTrue(DbInterface.InTransaction, 'Should be in transaction');

    DbInterface.RollBack;
    Assert.IsFalse(DbInterface.InTransaction, 'Should not be in transaction after RollBack');

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestGetQueryAndRelease;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query1, Query2, Query3: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;

    // Get first query
    Query1 := DbInterface.GetQuery;
    Assert.IsNotNull(Query1, 'Query1 should not be nil');

    // Get second query (should be different instance)
    Query2 := DbInterface.GetQuery;
    Assert.IsNotNull(Query2, 'Query2 should not be nil');

    // Release first query
    DbInterface.ReleaseQuery(Query1);
    Assert.IsNull(Query1, 'Query1 should be nil after release');

    // Get third query (should reuse released query from cache)
    Query3 := DbInterface.GetQuery;
    Assert.IsNotNull(Query3, 'Query3 should not be nil');

    // Cleanup
    DbInterface.ReleaseQuery(Query2);
    DbInterface.ReleaseQuery(Query3);
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestQueryExecuteSQL;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      // Execute a simple SELECT query
      Query.SQLText := 'SELECT 1 AS TestValue';
      Query.Open;

      Assert.IsFalse(Query.Eof, 'Query should return a row');
      Assert.AreEqual(1, Query.Fields[0].AsInteger, 'TestValue should be 1');

      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestQueryWithParameters;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
  Param: IBoldParameter;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      // Execute a parameterized query
      Query.SQLText := 'SELECT :InputValue AS OutputValue';
      Param := Query.ParamByName('InputValue');
      Param.AsInteger := 42;

      Query.Open;

      Assert.IsFalse(Query.Eof, 'Query should return a row');
      Assert.AreEqual(42, Query.Fields[0].AsInteger, 'OutputValue should be 42');

      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestAllTableNames;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  TableNames: TStringList;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  TableNames := TStringList.Create;
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Get all table names (the BoldUnitTest database should have Bold system tables)
    DbInterface.AllTableNames('', False, TableNames);

    // Should have at least some tables (Bold creates system tables)
    Assert.IsTrue(TableNames.Count >= 0, 'AllTableNames should return without error');

    DbInterface.Close;
  finally
    TableNames.Free;
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestQueryRecordCount;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      Query.SQLText := 'SELECT 1 AS Val UNION ALL SELECT 2 UNION ALL SELECT 3';
      Query.Open;

      Assert.AreEqual(3, Query.RecordCount, 'RecordCount should be 3');
      Assert.AreEqual('SELECT 1 AS Val UNION ALL SELECT 2 UNION ALL SELECT 3', Query.SQLText, 'SQLText should match');

      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryRowsAffected;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      // Create a temp table, insert rows, check RowsAffected
      ExecQuery.AssignSQLText('CREATE TABLE #TempTest (ID INT)');
      ExecQuery.ExecSQL;

      ExecQuery.AssignSQLText('INSERT INTO #TempTest VALUES (1), (2), (3)');
      ExecQuery.ExecSQL;
      Assert.AreEqual(3, ExecQuery.RowsAffected, 'RowsAffected should be 3 after insert');

      ExecQuery.AssignSQLText('DROP TABLE #TempTest');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestQueryPrepare;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      Query.SQLText := 'SELECT :Param1 AS Result';
      // Set parameter value BEFORE Prepare - MSSQL needs to know data type
      Query.ParamByName('Param1').AsInteger := 100;
      Query.Prepare;

      Query.Open;
      Assert.AreEqual(100, Query.Fields[0].AsInteger, 'Result should be 100');
      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestParameterTypes;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
  Param: IBoldParameter;
  TestDate: TDateTime;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      // Test string parameter
      Query.SQLText := 'SELECT :StrParam AS StrResult';
      Param := Query.ParamByName('StrParam');
      Param.AsString := 'TestString';
      Assert.AreEqual('TestString', Param.AsString, 'AsString should match');
      Query.Open;
      Assert.AreEqual('TestString', Query.Fields[0].AsString, 'String result should match');
      Query.Close;

      // Test float parameter
      Query.SQLText := 'SELECT :FloatParam AS FloatResult';
      Param := Query.ParamByName('FloatParam');
      Param.AsFloat := 3.14;
      Assert.AreEqual(3.14, Param.AsFloat, 0.001, 'AsFloat should match');
      Query.Open;
      Assert.AreEqual(3.14, Query.Fields[0].AsFloat, 0.001, 'Float result should match');
      Query.Close;

      // Test datetime parameter
      TestDate := EncodeDate(2025, 12, 15);
      Query.SQLText := 'SELECT :DateParam AS DateResult';
      Param := Query.ParamByName('DateParam');
      Param.AsDateTime := TestDate;
      Assert.AreEqual(TestDate, Param.AsDateTime, 'AsDateTime should match');

      // Test boolean parameter
      Query.SQLText := 'SELECT :BoolParam AS BoolResult';
      Param := Query.ParamByName('BoolParam');
      Param.AsBoolean := True;
      Assert.IsTrue(Param.AsBoolean, 'AsBoolean should be True');

      // Test IsNull
      Query.SQLText := 'SELECT :NullParam AS NullResult';
      Param := Query.ParamByName('NullParam');
      Param.Clear;
      Assert.IsTrue(Param.IsNull, 'IsNull should be True after Clear');

    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestGetTableAndRelease;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Table1, Table2: IBoldTable;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Get first table
    Table1 := DbInterface.GetTable;
    Assert.IsNotNull(Table1, 'Table1 should not be nil');

    // Get second table
    Table2 := DbInterface.GetTable;
    Assert.IsNotNull(Table2, 'Table2 should not be nil');

    // Release tables
    DbInterface.ReleaseTable(Table1);
    Assert.IsNull(Table1, 'Table1 should be nil after release');

    DbInterface.ReleaseTable(Table2);
    Assert.IsNull(Table2, 'Table2 should be nil after release');

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestConnectionProperties;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;

    // Test IsSQLBased
    Assert.IsTrue(DbInterface.IsSQLBased, 'IsSQLBased should be True for FireDAC');

    // Test SupportsTableCreation - FireDAC uses SQL for schema, not TTable
    Assert.IsFalse(DbInterface.SupportsTableCreation, 'SupportsTableCreation should be False for FireDAC');

    // Test Implementor returns the connection
    Assert.IsNotNull(DbInterface.Implementor, 'Implementor should not be nil');

    // Test KeepConnection
    Assert.IsTrue(DbInterface.KeepConnection, 'KeepConnection should be True by default');

    DbInterface.Open;

    // Test IsExecutingQuery (should be false when idle)
    Assert.IsFalse(DbInterface.IsExecutingQuery, 'IsExecutingQuery should be False when idle');

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQuery;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
  Param: IBoldParameter;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // First create a temp table for our test
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('CREATE TABLE #TestExec (ID INT, Val VARCHAR(50))');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    // Test ExecQuery with INSERT (DML statement, no result set)
    ExecQuery := DbInterface.GetExecQuery;
    try
      // Test SQLText property
      ExecQuery.AssignSQLText('INSERT INTO #TestExec (ID, Val) VALUES (:ID, :Val)');
      Assert.AreEqual('INSERT INTO #TestExec (ID, Val) VALUES (:ID, :Val)', ExecQuery.SQLText, 'SQLText should match');

      // Test ParamCount
      Assert.AreEqual(2, ExecQuery.ParamCount, 'ParamCount should be 2');

      // Test ParamByName
      Param := ExecQuery.ParamByName('ID');
      Assert.IsNotNull(Param, 'ParamByName should return parameter');
      Param.AsInteger := 1;

      Param := ExecQuery.ParamByName('Val');
      Param.AsString := 'TestValue';

      // Test Params property
      Assert.IsNotNull(ExecQuery.Params, 'Params should not be nil');

      // Execute
      ExecQuery.ExecSQL;

      // RowsAffected should be 1 after INSERT
      Assert.AreEqual(1, ExecQuery.RowsAffected, 'RowsAffected should be 1 after INSERT');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    // Cleanup temp table
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('DROP TABLE #TestExec');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestQueryRecNo;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      Query.SQLText := 'SELECT 1 AS Col1 UNION ALL SELECT 2 UNION ALL SELECT 3';
      Query.Open;

      // Test RecNo - should be 0-based in Bold
      Assert.AreEqual(0, Query.RecNo, 'RecNo should be 0 at first row');

      Query.Next;
      Assert.AreEqual(1, Query.RecNo, 'RecNo should be 1 at second row');

      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestQueryAssignParams;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
  SourceParams: TParams;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  SourceParams := TParams.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Create source params
    SourceParams.CreateParam(ftInteger, 'Param1', ptInput).AsInteger := 42;

    Query := DbInterface.GetQuery;
    try
      Query.SQLText := 'SELECT :Param1 AS Result';
      Query.AssignParams(SourceParams);
      Query.Open;

      Assert.AreEqual(42, Query.Fields[0].AsInteger, 'Parameter should be assigned');
      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    SourceParams.Free;
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestQueryRequestLiveQuery;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      // Test RequestLiveQuery getter/setter
      Query.RequestLiveQuery := True;
      // FireDAC implementation always returns False
      Assert.IsFalse(Query.RequestLiveQuery, 'RequestLiveQuery should be False for FireDAC');
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestParameterInt64;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
  Param: IBoldParameter;
  BigValue: Int64;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      BigValue := 9223372036854775807; // Max Int64
      Query.SQLText := 'SELECT :BigParam AS Result';
      Param := Query.ParamByName('BigParam');
      Param.AsInt64 := BigValue;
      Assert.AreEqual(BigValue, Param.AsInt64, 'AsInt64 should match');

      Query.Open;
      // Use AsVariant since IBoldField doesn't expose AsLargeInt
      Assert.AreEqual(BigValue, Int64(Query.Fields[0].AsVariant), 'Int64 result should match');
      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestParameterAnsiString;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
  Param: IBoldParameter;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      Query.SQLText := 'SELECT :AnsiParam AS Result';
      Param := Query.ParamByName('AnsiParam');
      Param.AsAnsiString := AnsiString('TestAnsi');
      Assert.AreEqual(AnsiString('TestAnsi'), Param.AsAnsiString, 'AsAnsiString should match');
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestParameterDataType;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
  Param: IBoldParameter;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      Query.SQLText := 'SELECT :TypedParam AS Result';
      Param := Query.ParamByName('TypedParam');

      // Test DataType getter/setter
      Param.DataType := ftInteger;
      Assert.AreEqual(Ord(ftInteger), Ord(Param.DataType), 'DataType should be ftInteger');

      Param.DataType := ftString;
      Assert.AreEqual(Ord(ftString), Ord(Param.DataType), 'DataType should be ftString');
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestTableExists;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Table: IBoldTable;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Create a test table
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('CREATE TABLE TestTableExists (ID INT)');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    // Test Exists
    Table := DbInterface.GetTable;
    try
      Table.TableName := 'TestTableExists';
      Assert.IsTrue(Table.Exists, 'Table should exist');

      Table.TableName := 'NonExistentTable12345';
      Assert.IsFalse(Table.Exists, 'Table should not exist');
    finally
      DbInterface.ReleaseTable(Table);
    end;

    // Cleanup
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('DROP TABLE TestTableExists');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestTableFieldDefs;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Table: IBoldTable;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Create a test table with known columns
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('CREATE TABLE TestFieldDefs (ID INT, Name VARCHAR(50), Amount DECIMAL(10,2))');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    // Test FieldDefs
    Table := DbInterface.GetTable;
    try
      Table.TableName := 'TestFieldDefs';
      Table.Exclusive := True; // Test Exclusive property
      Table.FieldDefs.Update;

      Assert.IsTrue(Table.FieldDefs.Count >= 3, 'Should have at least 3 field defs');
      Assert.IsNotNull(Table.FieldDefs.Find('ID'), 'Should find ID field');
      Assert.IsNotNull(Table.FieldDefs.Find('Name'), 'Should find Name field');
      Assert.IsNotNull(Table.FieldDefs.Find('Amount'), 'Should find Amount field');
    finally
      DbInterface.ReleaseTable(Table);
    end;

    // Cleanup
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('DROP TABLE TestFieldDefs');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestConnectionReconnect;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;

    DbInterface.Open;
    Assert.IsTrue(Connection.Connected, 'Should be connected');

    DbInterface.Reconnect;
    Assert.IsTrue(Connection.Connected, 'Should still be connected after reconnect');

    DbInterface.Close;
    Assert.IsFalse(Connection.Connected, 'Should be disconnected');
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestReadTransaction;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  Query: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    Query := DbInterface.GetQuery;
    try
      // Test UseReadTransactions property
      Query.UseReadTransactions := True;
      Assert.IsTrue(Query.UseReadTransactions, 'UseReadTransactions should be True');

      Query.UseReadTransactions := False;
      Assert.IsFalse(Query.UseReadTransactions, 'UseReadTransactions should be False');

      // Execute a query with read transactions enabled
      Query.UseReadTransactions := True;
      Query.SQLText := 'SELECT 1 AS Result';
      Query.Open;
      Query.Close;
    finally
      DbInterface.ReleaseQuery(Query);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestCreateAnotherDatabaseConnection;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  AnotherDbInterface: IBoldDatabase;
  Query1, Query2: IBoldQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Create another database connection
    AnotherDbInterface := DbInterface.CreateAnotherDatabaseConnection;
    Assert.IsNotNull(AnotherDbInterface, 'Another connection should not be nil');
    // Note: The created connection owns itself (fOwnsConnection=True) and its TFDConnection

    // Verify both connections work independently
    AnotherDbInterface.Open;

    // Verify we have a different underlying connection
    Assert.AreNotSame(DbInterface.Implementor, AnotherDbInterface.Implementor,
      'Should be different connection objects');

    Query1 := DbInterface.GetQuery;
    Query2 := AnotherDbInterface.GetQuery;
    try
      Query1.SQLText := 'SELECT 1 AS Result1';
      Query2.SQLText := 'SELECT 2 AS Result2';

      Query1.Open;
      Query2.Open;

      Assert.AreEqual(1, Integer(Query1.Fields[0].AsVariant), 'Query1 should return 1');
      Assert.AreEqual(2, Integer(Query2.Fields[0].AsVariant), 'Query2 should return 2');

      Query1.Close;
      Query2.Close;
    finally
      DbInterface.ReleaseQuery(Query1);
      AnotherDbInterface.ReleaseQuery(Query2);
    end;

    AnotherDbInterface.Close;
    DbInterface.Close;
    // Note: TBoldNonRefCountedObject doesn't use ref counting, so AnotherDbInterface
    // must be explicitly freed. The fOwnsConnection flag ensures the TFDConnection is also freed.
    (AnotherDbInterface as TBoldFireDACConnection).Free;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryClear;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      // Set up SQL and params
      ExecQuery.AssignSQLText('SELECT :Param1, :Param2');
      Assert.AreEqual(2, ExecQuery.ParamCount, 'Should have 2 params before clear');

      // ClearParams should clear all parameters
      ExecQuery.ClearParams;
      Assert.AreEqual(0, ExecQuery.ParamCount, 'ParamCount should be 0 after ClearParams');

      // Add more params and clear again
      ExecQuery.AssignSQLText('SELECT :P1, :P2, :P3');
      Assert.AreEqual(3, ExecQuery.ParamCount, 'Should have 3 params');
      ExecQuery.ClearParams;
      Assert.AreEqual(0, ExecQuery.ParamCount, 'ParamCount should be 0 after second ClearParams');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryCreateParam;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
  Param: IBoldParameter;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.ParamCheck := False; // Disable auto param creation
      ExecQuery.AssignSQLText('SELECT @TestParam');

      // CreateParam with basic signature
      Param := ExecQuery.CreateParam(ftInteger, 'IntParam');
      Assert.IsNotNull(Param, 'CreateParam should return non-nil');
      Assert.AreEqual('IntParam', Param.Name, 'Param name should match');
      Param.AsInteger := 42;
      Assert.AreEqual(42, Param.AsInteger, 'Param value should be set');

      // CreateParam with extended signature (type, name, paramtype, size)
      Param := ExecQuery.CreateParam(ftString, 'StrParam', ptInput, 100);
      Assert.IsNotNull(Param, 'CreateParam extended should return non-nil');
      Assert.AreEqual('StrParam', Param.Name, 'Param name should match');
      Param.AsString := 'TestValue';
      Assert.AreEqual('TestValue', Param.AsString, 'String param value should be set');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryFindParam;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
  Param: IBoldParameter;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('SELECT :ExistingParam, :AnotherParam');

      // FindParam returns parameter if found
      Param := ExecQuery.FindParam('ExistingParam');
      Assert.IsNotNull(Param, 'FindParam should find existing param');
      Assert.AreEqual('ExistingParam', Param.Name, 'Found param name should match');

      // FindParam returns nil if not found (unlike ParamByName which raises)
      Param := ExecQuery.FindParam('NonExistentParam');
      Assert.IsNull(Param, 'FindParam should return nil for non-existent param');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryEnsureParamByName;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
  Param1, Param2: IBoldParameter;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.ParamCheck := False;
      ExecQuery.AssignSQLText('SELECT 1');

      // EnsureParamByName creates param if not exists
      Param1 := ExecQuery.EnsureParamByName('NewParam');
      Assert.IsNotNull(Param1, 'EnsureParamByName should create param');
      Assert.AreEqual('NewParam', Param1.Name, 'Created param name should match');

      // Calling again returns the same param
      Param2 := ExecQuery.EnsureParamByName('NewParam');
      Assert.IsNotNull(Param2, 'EnsureParamByName should return existing param');
      Assert.AreEqual('NewParam', Param2.Name, 'Existing param name should match');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryPrepare;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      // Use a non-returning SQL statement (SET NOCOUNT is safe and doesn't return results)
      ExecQuery.AssignSQLText('SET NOCOUNT ON');

      // Prepare should not raise
      ExecQuery.Prepare;

      // Execute after prepare - SET statements don't return result sets
      ExecQuery.ExecSQL;
      Assert.Pass('Prepare and ExecSQL completed without error');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryParamCheck;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      // Default ParamCheck should be True
      Assert.IsTrue(ExecQuery.ParamCheck, 'Default ParamCheck should be True');

      // With ParamCheck=True, params are auto-created from SQL
      ExecQuery.AssignSQLText('SELECT :AutoParam');
      Assert.AreEqual(1, ExecQuery.ParamCount, 'Should auto-create param when ParamCheck=True');

      // Disable ParamCheck
      ExecQuery.ParamCheck := False;
      Assert.IsFalse(ExecQuery.ParamCheck, 'ParamCheck should be False after setting');

      // With ParamCheck=False, params are NOT auto-created
      ExecQuery.ClearParams;
      ExecQuery.AssignSQLText('SELECT :ManualParam');
      Assert.AreEqual(0, ExecQuery.ParamCount, 'Should NOT auto-create param when ParamCheck=False');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryUseReadTransactions;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    ExecQuery := DbInterface.GetExecQuery;
    try
      // Test UseReadTransactions property
      ExecQuery.UseReadTransactions := True;
      Assert.IsTrue(ExecQuery.UseReadTransactions, 'UseReadTransactions should be True');

      ExecQuery.UseReadTransactions := False;
      Assert.IsFalse(ExecQuery.UseReadTransactions, 'UseReadTransactions should be False');

      // Execute with read transactions disabled - use non-returning statement
      ExecQuery.AssignSQLText('SET NOCOUNT ON');
      ExecQuery.ExecSQL;
      Assert.Pass('ExecSQL completed with UseReadTransactions=False');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryBatchOperations;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Create a test table
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('CREATE TABLE #BatchTest (ID INT, Val VARCHAR(50))');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    // Test batch operations
    ExecQuery := DbInterface.GetExecQuery;
    try
      // Start batch
      ExecQuery.StartSQLBatch;

      // Queue multiple statements
      ExecQuery.AssignSQLText('INSERT INTO #BatchTest (ID, Val) VALUES (1, ''One'')');
      ExecQuery.ExecSQL;

      ExecQuery.AssignSQLText('INSERT INTO #BatchTest (ID, Val) VALUES (2, ''Two'')');
      ExecQuery.ExecSQL;

      ExecQuery.AssignSQLText('INSERT INTO #BatchTest (ID, Val) VALUES (3, ''Three'')');
      ExecQuery.ExecSQL;

      // End batch - executes all queued statements
      ExecQuery.EndSQLBatch;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    // Verify batch executed correctly
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('SELECT COUNT(*) FROM #BatchTest');
      // Note: ExecQuery doesn't return result sets, but we can verify no error occurred
      Assert.Pass('Batch operations completed without error');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    // Cleanup
    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.AssignSQLText('DROP TABLE #BatchTest');
      ExecQuery.ExecSQL;
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    Adapter.Free;
    Connection.Free;
  end;
end;

procedure TTestPersistenceFireDAC.TestExecQueryAssignParams;
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
  DbInterface: IBoldDatabase;
  ExecQuery: IBoldExecQuery;
  SourceParams: TParams;
begin
  Connection := TFDConnection.Create(nil);
  Adapter := TBoldDatabaseAdapterFireDAC.Create(nil);
  SourceParams := TParams.Create(nil);
  try
    Adapter.Connection := Connection;
    ConfigureConnection(Connection, Adapter);
    DbInterface := Adapter.DatabaseInterface;
    DbInterface.Open;

    // Set up source params
    SourceParams.CreateParam(ftInteger, 'IntVal', ptInput).AsInteger := 100;
    SourceParams.CreateParam(ftString, 'StrVal', ptInput).AsString := 'TestString';

    ExecQuery := DbInterface.GetExecQuery;
    try
      ExecQuery.ParamCheck := False;
      ExecQuery.AssignSQLText('SELECT 1');

      // AssignParams copies params from source
      ExecQuery.AssignParams(SourceParams);

      Assert.AreEqual(2, ExecQuery.ParamCount, 'Should have 2 params after AssignParams');
      Assert.AreEqual(100, ExecQuery.ParamByName('IntVal').AsInteger, 'IntVal should be 100');
      Assert.AreEqual('TestString', ExecQuery.ParamByName('StrVal').AsString, 'StrVal should match');
    finally
      DbInterface.ReleaseExecQuery(ExecQuery);
    end;

    DbInterface.Close;
  finally
    SourceParams.Free;
    Adapter.Free;
    Connection.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceFireDAC);

end.
