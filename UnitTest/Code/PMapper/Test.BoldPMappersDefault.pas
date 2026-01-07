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

implementation

uses
  System.Rtti;

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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldPMappersDefault);

end.
