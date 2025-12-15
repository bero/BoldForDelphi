unit Test.BoldPersistenceHandleDB;

interface

uses
  DUnitX.TestFramework,
  BoldPersistenceHandleDB;

type
  // Subclass to expose protected methods for testing
  TTestableBoldPersistenceHandleDB = class(TBoldPersistenceHandleDB)
  public
    procedure CallAssertSQLDatabaseconfig;
  end;

  [TestFixture]
  [Category('Persistence')]
  TTestBoldPersistenceHandleDB = class
  public
    [Test]
    [Category('Quick')]
    procedure TestGettersWithoutDatabaseAdapter;
    [Test]
    [Category('Quick')]
    procedure TestSetActiveWithoutDatabaseAdapterRaisesException;
    [Test]
    [Category('Quick')]
    procedure TestAssertSQLDatabaseconfigWithoutAdapterRaisesException;
  end;

implementation

uses
  System.SysUtils,
  BoldDefs;

{ TTestableBoldPersistenceHandleDB }

procedure TTestableBoldPersistenceHandleDB.CallAssertSQLDatabaseconfig;
begin
  AssertSQLDatabaseconfig('Test');
end;

{ TTestBoldPersistenceHandleDB }

procedure TTestBoldPersistenceHandleDB.TestGettersWithoutDatabaseAdapter;
var
  Handle: TBoldPersistenceHandleDB;
begin
  Handle := TBoldPersistenceHandleDB.Create(nil);
  try
    // Without DatabaseAdapter, these should return nil
    // Covers lines 79, 87, 106
    Assert.IsNull(Handle.CustomIndexes, 'CustomIndexes should be nil without adapter');
    Assert.IsNull(Handle.DatabaseInterface, 'DatabaseInterface should be nil without adapter');
    Assert.IsNull(Handle.SQLDatabaseConfig, 'SQLDatabaseConfig should be nil without adapter');
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldPersistenceHandleDB.TestSetActiveWithoutDatabaseAdapterRaisesException;
var
  Handle: TBoldPersistenceHandleDB;
begin
  Handle := TBoldPersistenceHandleDB.Create(nil);
  try
    // Setting Active without DatabaseAdapter should raise exception
    // Covers line 122
    Assert.WillRaise(
      procedure
      begin
        Handle.Active := True;
      end,
      EBold);
  finally
    Handle.Free;
  end;
end;

procedure TTestBoldPersistenceHandleDB.TestAssertSQLDatabaseconfigWithoutAdapterRaisesException;
var
  Handle: TTestableBoldPersistenceHandleDB;
begin
  Handle := TTestableBoldPersistenceHandleDB.Create(nil);
  try
    // Calling AssertSQLDatabaseconfig without DatabaseAdapter should raise exception
    // Covers line 71
    Assert.WillRaise(
      procedure
      begin
        Handle.CallAssertSQLDatabaseconfig;
      end,
      EBold);
  finally
    Handle.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldPersistenceHandleDB);

end.
