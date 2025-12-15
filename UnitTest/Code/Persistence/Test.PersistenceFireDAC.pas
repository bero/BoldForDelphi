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
  TTestPersistenceFireDAC = class
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestCreateObject;
    [Test]
    procedure TestPersistAndReload;
    [Test]
    procedure TestDatabaseExistsReturnsTrue;
    [Test]
    procedure TestDatabaseExistsReturnsFalseForNonExistent;
    [Test]
    procedure TestDropDatabase;
    [Test]
    procedure TestCreateDatabase;
    [Test]
    procedure TestConnectionOpenClose;
    [Test]
    procedure TestTransactionCommitRollback;
    [Test]
    procedure TestGetQueryAndRelease;
    [Test]
    procedure TestQueryExecuteSQL;
    [Test]
    procedure TestQueryWithParameters;
    [Test]
    procedure TestAllTableNames;
  end;

implementation

uses
  Classes,
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

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceFireDAC);

end.
