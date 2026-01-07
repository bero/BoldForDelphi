unit Test.BoldDBInterfacesMock;

{ Unit tests demonstrating how to use Delphi-Mocks with Bold database interfaces.

  These tests show patterns for mocking IBoldDatabase, IBoldQuery, and IBoldField
  using the Delphi-Mocks framework instead of custom mock implementations.

  Usage:
    var
      MockDb: TMock<IBoldDatabase>;
      MockQuery: TMock<IBoldQuery>;
    begin
      MockDb := TMock<IBoldDatabase>.Create;
      MockQuery := TMock<IBoldQuery>.Create;

      // Setup return values
      MockDb.Setup.WillReturn(TValue.From<IBoldQuery>(MockQuery.Instance))
        .When.GetQuery;

      // Use mock in tests
      var Db: IBoldDatabase := MockDb.Instance;
      var Query := Db.GetQuery;
      ...
    end;
}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  BoldDBInterfaces;

type
  [TestFixture]
  [Category('Mocks')]
  TTestBoldDBInterfacesMock = class
  public
    [Test]
    [Category('Quick')]
    procedure TestMockDatabaseConnection;

    [Test]
    [Category('Quick')]
    procedure TestMockDatabaseTransaction;

    [Test]
    [Category('Quick')]
    procedure TestMockQueryReturnValue;

    [Test]
    [Category('Quick')]
    procedure TestMockFieldAccess;

    [Test]
    [Category('Quick')]
    procedure TestMockExpectations;

    [Test]
    [Category('Quick')]
    procedure TestMockWillExecute;
  end;

implementation

uses
  SysUtils,
  System.Rtti;

{ TTestBoldDBInterfacesMock }

procedure TTestBoldDBInterfacesMock.TestMockDatabaseConnection;
var
  MockDb: TMock<IBoldDatabase>;
begin
  // Test mock returning True
  MockDb := TMock<IBoldDatabase>.Create;
  MockDb.Setup.WillReturn(True).When.GetConnected;
  Assert.IsTrue(MockDb.Instance.Connected, 'Mock should return True for Connected');

  // Create new mock to test returning False (can't redefine WillReturn on same mock)
  MockDb := TMock<IBoldDatabase>.Create;
  MockDb.Setup.WillReturn(False).When.GetConnected;
  Assert.IsFalse(MockDb.Instance.Connected, 'Mock should return False for Connected');
end;

procedure TTestBoldDBInterfacesMock.TestMockDatabaseTransaction;
var
  MockDb: TMock<IBoldDatabase>;
  TransactionStarted: Boolean;
begin
  MockDb := TMock<IBoldDatabase>.Create;
  TransactionStarted := False;

  // Setup expectations
  MockDb.Setup.Expect.Once.When.StartTransaction;
  MockDb.Setup.Expect.Once.When.Commit;

  // Setup WillExecute to track calls
  MockDb.Setup.WillExecute(
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    begin
      TransactionStarted := True;
      Result := TValue.Empty;
    end
  ).When.StartTransaction;

  MockDb.Setup.WillReturn(TransactionStarted).When.GetInTransaction;

  // Execute
  MockDb.Instance.StartTransaction;
  Assert.IsTrue(TransactionStarted, 'StartTransaction should have been called');

  MockDb.Instance.Commit;

  // Verify expectations were met
  MockDb.Verify('Transaction methods should be called once each');
end;

procedure TTestBoldDBInterfacesMock.TestMockQueryReturnValue;
var
  MockDb: TMock<IBoldDatabase>;
  MockQuery: TMock<IBoldQuery>;
  Query: IBoldQuery;
begin
  MockDb := TMock<IBoldDatabase>.Create;
  MockQuery := TMock<IBoldQuery>.Create;

  // Setup mock query to return record count
  MockQuery.Setup.WillReturn(5).When.GetRecordCount;
  MockQuery.Setup.WillReturn(False).When.GetEof;

  // Setup database to return mock query
  MockDb.Setup.WillReturn(TValue.From<IBoldQuery>(MockQuery.Instance)).When.GetQuery;

  // Test
  Query := MockDb.Instance.GetQuery;
  Assert.IsNotNull(Query, 'GetQuery should return mock query');
  Assert.AreEqual(5, Query.RecordCount, 'RecordCount should be 5');
  Assert.IsFalse(Query.Eof, 'Eof should be False');
end;

procedure TTestBoldDBInterfacesMock.TestMockFieldAccess;
var
  MockQuery: TMock<IBoldQuery>;
  MockField: TMock<IBoldField>;
begin
  MockQuery := TMock<IBoldQuery>.Create;
  MockField := TMock<IBoldField>.Create;

  // Setup field mock
  MockField.Setup.WillReturn('TestName').When.GetFieldName;
  MockField.Setup.WillReturn(42).When.GetAsInteger;
  MockField.Setup.WillReturn('TestValue').When.GetAsString;

  // Setup query to return field
  MockQuery.Setup.WillReturn(TValue.From<IBoldField>(MockField.Instance))
    .When.FieldByName(It0.IsAny<string>);

  // Test
  var Field := MockQuery.Instance.FieldByName('AnyField');
  Assert.IsNotNull(Field, 'FieldByName should return mock field');
  Assert.AreEqual('TestName', Field.FieldName, 'FieldName should match');
  Assert.AreEqual(42, Field.AsInteger, 'AsInteger should be 42');
  Assert.AreEqual('TestValue', Field.AsString, 'AsString should match');
end;

procedure TTestBoldDBInterfacesMock.TestMockExpectations;
var
  MockDb: TMock<IBoldDatabase>;
  MockQuery: TMock<IBoldQuery>;
begin
  MockDb := TMock<IBoldDatabase>.Create;
  MockQuery := TMock<IBoldQuery>.Create;

  // Setup return value for GetQuery (required for function calls)
  MockDb.Setup.WillReturn(TValue.From<IBoldQuery>(MockQuery.Instance)).When.GetQuery;

  // Setup expectations for method call counts
  MockDb.Setup.Expect.Exactly(3).When.GetQuery;
  MockDb.Setup.Expect.Never.When.DropDatabase;

  // Make the expected calls
  MockDb.Instance.GetQuery;
  MockDb.Instance.GetQuery;
  MockDb.Instance.GetQuery;

  // Verify - this should pass
  MockDb.Verify('GetQuery should be called exactly 3 times');
  Assert.Pass('Mock expectations verified successfully');
end;

procedure TTestBoldDBInterfacesMock.TestMockWillExecute;
var
  MockDb: TMock<IBoldDatabase>;
  OpenCallCount: Integer;
begin
  MockDb := TMock<IBoldDatabase>.Create;
  OpenCallCount := 0;

  // Setup WillExecute to run custom code
  MockDb.Setup.WillExecute(
    function(const args: TArray<TValue>; const ReturnType: TRttiType): TValue
    begin
      Inc(OpenCallCount);
      Result := TValue.Empty;
    end
  ).When.Open;

  // Call multiple times
  MockDb.Instance.Open;
  MockDb.Instance.Open;
  MockDb.Instance.Open;

  Assert.AreEqual(3, OpenCallCount, 'Open should have been called 3 times');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDBInterfacesMock);

end.
