unit Test.BoldPersistenceHandleDB;

interface

uses
  BoldTestFramework,
  BoldPersistenceHandleDB;

type
  // Subclass to expose protected methods for testing
  TTestableBoldPersistenceHandleDB = class(TBoldPersistenceHandleDB)
  public
    procedure CallAssertSQLDatabaseconfig;
  end;

  {$IFNDEF FPC}
  [Category('Persistence')]
  {$ENDIF}
  TTestBoldPersistenceHandleDB = class(TBoldTestCase)
  private
    fRaiseHandle: TObject;
    procedure TestSetActiveWithoutDatabaseAdapterRaisesException_Raiser;
    procedure TestAssertSQLDatabaseconfigWithoutAdapterRaisesException_Raiser;
  published
    procedure TestGettersWithoutDatabaseAdapter;
    procedure TestSetActiveWithoutDatabaseAdapterRaisesException;
    procedure TestAssertSQLDatabaseconfigWithoutAdapterRaisesException;
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TTestableBoldPersistenceHandleDB }

procedure TTestableBoldPersistenceHandleDB.CallAssertSQLDatabaseconfig;
begin
  AssertSQLDatabaseconfig('Test');
end;

{ TTestBoldPersistenceHandleDB }

procedure TTestBoldPersistenceHandleDB.TestSetActiveWithoutDatabaseAdapterRaisesException_Raiser;
begin
  TBoldPersistenceHandleDB(fRaiseHandle).Active := True;
end;

procedure TTestBoldPersistenceHandleDB.TestAssertSQLDatabaseconfigWithoutAdapterRaisesException_Raiser;
begin
  TTestableBoldPersistenceHandleDB(fRaiseHandle).CallAssertSQLDatabaseconfig;
end;

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
    fRaiseHandle := Handle;
    Assert.WillRaise(TestSetActiveWithoutDatabaseAdapterRaisesException_Raiser, EBold);
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
    fRaiseHandle := Handle;
    Assert.WillRaise(TestAssertSQLDatabaseconfigWithoutAdapterRaisesException_Raiser, EBold);
  finally
    Handle.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldPersistenceHandleDB);

end.
