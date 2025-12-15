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
  end;

implementation

uses
  BoldSystem,
  BoldSystemRT,
  BoldId,
  BoldSQLDatabaseConfig,
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

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceFireDAC);

end.
