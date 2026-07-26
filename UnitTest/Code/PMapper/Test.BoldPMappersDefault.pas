unit Test.BoldPMappersDefault;

{ Unit tests for BoldPMappersDefault using Delphi-Mocks framework.

  Tests cover mocking patterns for IBoldQuery and IBoldField interfaces
  that are used by NewIdFromQuery and other persistence mapper methods.
}

interface

uses
  DUnitX.TestFramework,
  Delphi.Mocks,
  BoldDBInterfaces,
  BoldDefs;

type
  [TestFixture]
  [Category('PMapper')]
  TTestBoldPMappersDefault = class
  public
    [Test]
    [Category('Quick')]
    procedure TestMockQueryWithBoldDbTypeField;

    [Test]
    [Category('Quick')]
    procedure TestMockQueryWithObjectIdField;

    [Test]
    [Category('Quick')]
    procedure TestMockQueryWithMultipleFields;

    [Test]
    [Category('Quick')]
    procedure TestBoldMaxTimestampConstant;
  end;

  { Regression fixture for the PMCreate error-path leak (issue #64).
    Needs a real SQLite-backed system: the leak sits inside
    TBoldObjectDefaultMapper.PMCreate, which only runs against a fully built
    persistence mapper graph, so mocks cannot reach it. }
  [TestFixture]
  [Category('PMapper')]
  TTestBoldPMCreateErrorPath = class
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestFailedInsertDoesNotLeakPerTableLists;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  BoldSystem,
  BoldPMappersSQL,
  BoldTestModel,
  maan_UndoRedoBase,
  maan_UndoRedoTestCaseUtils;

{ TTestBoldPMappersDefault }

procedure TTestBoldPMappersDefault.TestMockQueryWithBoldDbTypeField;
var
  MockQuery: TMock<IBoldQuery>;
  MockField: TMock<IBoldField>;
begin
  // Demonstrates mocking IBoldQuery.Fields[index] for BoldDbType column
  // This pattern is used by NewIdFromQuery when BoldDbTypeColumn <> -1
  MockQuery := TMock<IBoldQuery>.Create;
  MockField := TMock<IBoldField>.Create;

  MockField.Setup.WillReturn(5).When.GetAsInteger;  // BoldDbType value

  MockQuery.Setup.WillReturn(TValue.From<IBoldField>(MockField.Instance))
    .When.GetFields(0);

  // Verify mock returns expected BoldDbType
  Assert.AreEqual(5, MockQuery.Instance.Fields[0].AsInteger, 'Field 0 should return BoldDbType=5');
end;

procedure TTestBoldPMappersDefault.TestMockQueryWithObjectIdField;
var
  MockQuery: TMock<IBoldQuery>;
  MockField: TMock<IBoldField>;
begin
  // Demonstrates mocking IBoldQuery.Fields[index] for ObjectId column
  // This pattern is used by NewIdFromQuery to get the object ID value
  MockQuery := TMock<IBoldQuery>.Create;
  MockField := TMock<IBoldField>.Create;

  MockField.Setup.WillReturn(42).When.GetAsInteger; // ObjectId value

  MockQuery.Setup.WillReturn(TValue.From<IBoldField>(MockField.Instance))
    .When.GetFields(1);

  // Verify mock returns expected ObjectId
  Assert.AreEqual(42, MockQuery.Instance.Fields[1].AsInteger, 'Field 1 should return ObjectId=42');
end;

procedure TTestBoldPMappersDefault.TestMockQueryWithMultipleFields;
var
  MockQuery: TMock<IBoldQuery>;
  MockField0, MockField1: TMock<IBoldField>;
begin
  // Demonstrates mocking multiple fields simultaneously
  // NewIdFromQuery accesses both BoldDbType and ObjectId columns
  MockQuery := TMock<IBoldQuery>.Create;
  MockField0 := TMock<IBoldField>.Create;
  MockField1 := TMock<IBoldField>.Create;

  MockField0.Setup.WillReturn(3).When.GetAsInteger;   // BoldDbType
  MockField1.Setup.WillReturn(999).When.GetAsInteger; // ObjectId

  MockQuery.Setup.WillReturn(TValue.From<IBoldField>(MockField0.Instance))
    .When.GetFields(0);
  MockQuery.Setup.WillReturn(TValue.From<IBoldField>(MockField1.Instance))
    .When.GetFields(1);

  // Verify both fields return expected values
  Assert.AreEqual(3, MockQuery.Instance.Fields[0].AsInteger, 'BoldDbType field should be 3');
  Assert.AreEqual(999, MockQuery.Instance.Fields[1].AsInteger, 'ObjectId field should be 999');
end;

procedure TTestBoldPMappersDefault.TestBoldMaxTimestampConstant;
begin
  // Verify BoldMaxTimestamp constant is defined and usable
  // NewIdFromQuery uses this to determine if a timestamped ID should be created
  Assert.AreEqual(High(Integer), BOLDMAXTIMESTAMP, 'BoldMaxTimestamp should be MaxInt');
end;

{ TTestBoldPMCreateErrorPath }

procedure TTestBoldPMCreateErrorPath.SetUp;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
end;

procedure TTestBoldPMCreateErrorPath.TearDown;
begin
  if Assigned(dmUndoRedo) then
  begin
    if dmUndoRedo.BoldSystemHandle1.Active then
    begin
      dmUndoRedo.BoldSystemHandle1.System.Discard;
      dmUndoRedo.BoldSystemHandle1.Active := False;
    end;
    // The test dropped a table behind Bold's back - free the datamodule so
    // the next EnsureDM recreates the database schema from scratch.
    FreeAndNil(dmUndoRedo);
  end;
end;

procedure TTestBoldPMCreateErrorPath.TestFailedInsertDoesNotLeakPerTableLists;
var
  Sys: TBoldSystem;
  ObjectMapper: TBoldObjectSQLMapper;
  i: Integer;
  FailCount: Integer;
  BytesBefore, BytesAfter: Int64;
begin
  // PMCreate builds a TBoldMemberPersistenceMapperList and a TStringList per
  // table and frees them only at the bottom of the loop body; when ExecSQL
  // raises, both leak - once per failed UpdateDatabase attempt.
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  ObjectMapper := dmUndoRedo.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper.
    ObjectPersistenceMappers[Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'].TopSortedIndex]
    as TBoldObjectSQLMapper;

  TSomeClass.Create(Sys);
  // Sabotage the schema so PMCreate's INSERT fails
  dmUndoRedo.FDConnection1.ExecSQL('DROP TABLE ' + ObjectMapper.MainTable.SQLName);

  // Warm-up: two failing attempts get the query pool and FireDAC error
  // machinery allocated. NOTE: only the first 4 attempts raise at all - after
  // that the broken error path leaves the pooled exec query corrupted and
  // UpdateDatabase reports success without writing anything, so the
  // measurement window below must stay inside the first 4 attempts.
  Assert.WillRaiseAny(
    procedure
    begin
      dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    end, 'UpdateDatabase must fail once the class table is gone');
  Assert.WillRaiseAny(
    procedure
    begin
      dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    end, 'second UpdateDatabase attempt must fail too');

  // Measure attempts 3 and 4 - each failing PMCreate leaks one
  // TBoldMemberPersistenceMapperList (plus indexes) and one TStringList.
  FailCount := 0;
  BytesBefore := CurrentAllocatedBytes;
  for i := 1 to 2 do
    try
      dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    except
      Inc(FailCount);  // expected - table is gone
    end;
  BytesAfter := CurrentAllocatedBytes;

  Assert.AreEqual(2, FailCount, 'both measured UpdateDatabase attempts must reach the failing INSERT');
  Assert.IsTrue(BytesAfter - BytesBefore < 400,
    Format('2 failed PMCreate calls grew heap by %d bytes - per-table MemberPMList/SQL leak on the error path',
      [BytesAfter - BytesBefore]));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldPMappersDefault);
  TDUnitX.RegisterTestFixture(TTestBoldPMCreateErrorPath);

end.
