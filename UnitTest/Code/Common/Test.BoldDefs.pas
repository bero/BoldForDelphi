unit Test.BoldDefs;

interface

uses
  DUnitX.TestFramework,
  BoldDefs;

type
  /// <summary>
  /// Test fixture for BoldDefs - Core type definitions and exceptions
  /// </summary>
  [TestFixture]
  [Category('Common')]
  TTestBoldDefs = class
  public
    [Test]
    [Category('Quick')]
    procedure TestEBoldDatabaseErrorProperties;
    [Test]
    procedure TestEBoldDatabaseErrorInheritance;
  end;

implementation

uses
  System.SysUtils;

{ TTestBoldDefs }

procedure TTestBoldDefs.TestEBoldDatabaseErrorProperties;
var
  Ex: EBoldDatabaseError;
begin
  Ex := EBoldDatabaseError.Create('Test error');
  try
    Ex.OriginalExceptionClass := 'TOriginalException';
    Ex.OriginalExceptionMessage := 'Original message';

    Assert.AreEqual('TOriginalException', Ex.OriginalExceptionClass, 'OriginalExceptionClass should be set');
    Assert.AreEqual('Original message', Ex.OriginalExceptionMessage, 'OriginalExceptionMessage should be set');
  finally
    Ex.Free;
  end;
end;

procedure TTestBoldDefs.TestEBoldDatabaseErrorInheritance;
var
  ConnErr: EBoldDatabaseConnectionError;
  UpdateErr: EBoldDatabaseUpdateError;
  SQLErr: EBoldDatabaseSQLError;
  DeadlockErr: EBoldDatabaseDeadlockError;
  LoginErr: EBoldDatabaseLoginError;
begin
  // Verify all database errors inherit from EBoldDatabaseError
  ConnErr := EBoldDatabaseConnectionError.Create('Connection error');
  UpdateErr := EBoldDatabaseUpdateError.Create('Update error');
  SQLErr := EBoldDatabaseSQLError.Create('SQL error');
  DeadlockErr := EBoldDatabaseDeadlockError.Create('Deadlock');
  LoginErr := EBoldDatabaseLoginError.Create('Login failed');
  try
    Assert.IsTrue(ConnErr is EBoldDatabaseError, 'ConnectionError should inherit from EBoldDatabaseError');
    Assert.IsTrue(UpdateErr is EBoldDatabaseError, 'UpdateError should inherit from EBoldDatabaseError');
    Assert.IsTrue(SQLErr is EBoldDatabaseError, 'SQLError should inherit from EBoldDatabaseError');
    Assert.IsTrue(DeadlockErr is EBoldDatabaseError, 'DeadlockError should inherit from EBoldDatabaseError');
    Assert.IsTrue(LoginErr is EBoldDatabaseError, 'LoginError should inherit from EBoldDatabaseError');
  finally
    ConnErr.Free;
    UpdateErr.Free;
    SQLErr.Free;
    DeadlockErr.Free;
    LoginErr.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDefs);

end.
