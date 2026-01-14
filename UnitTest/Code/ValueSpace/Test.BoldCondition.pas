unit Test.BoldCondition;

interface

uses
  DUnitX.TestFramework,
  BoldCondition;

type
  [TestFixture]
  TTestBoldCondition = class
  public
    // TBoldCondition tests (base class)
    [Test]
    procedure TestConditionCreate;
    [Test]
    procedure TestConditionMaxAnswersDefault;
    [Test]
    procedure TestConditionOffsetDefault;
    [Test]
    procedure TestConditionProperties;

    // TBoldConditionWithClass tests
    [Test]
    procedure TestConditionWithClassCreate;
    [Test]
    procedure TestConditionWithClassTimeDefault;
    [Test]
    procedure TestConditionWithClassGetStreamName;
    [Test]
    procedure TestConditionWithClassProperties;

    // TBoldSQLCondition tests
    [Test]
    procedure TestSQLConditionCreate;
    [Test]
    procedure TestSQLConditionJoinInheritedTablesDefault;
    [Test]
    procedure TestSQLConditionGetStreamName;
    [Test]
    procedure TestSQLConditionProperties;

    // TBoldRawSQLCondition tests
    [Test]
    procedure TestRawSQLConditionGetStreamName;
    [Test]
    procedure TestRawSQLConditionProperties;

    // TBoldTimestampCondition tests
    [Test]
    procedure TestTimestampConditionGetStreamName;
    [Test]
    procedure TestTimestampConditionProperties;

    // TBoldChangePointCondition tests
    [Test]
    procedure TestChangePointConditionGetStreamName;
    [Test]
    procedure TestChangePointConditionProperties;
  end;

implementation

uses
  SysUtils,
  DB,
  BoldDefs,
  BoldId,
  BoldStreams;

{ TTestBoldCondition }

procedure TTestBoldCondition.TestConditionCreate;
var
  Condition: TBoldConditionWithClass;
begin
  // TBoldCondition is abstract, test via TBoldConditionWithClass
  Condition := TBoldConditionWithClass.Create;
  try
    Assert.IsNotNull(Condition);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestConditionMaxAnswersDefault;
var
  Condition: TBoldConditionWithClass;
begin
  Condition := TBoldConditionWithClass.Create;
  try
    Assert.AreEqual(-1, Condition.MaxAnswers);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestConditionOffsetDefault;
var
  Condition: TBoldConditionWithClass;
begin
  Condition := TBoldConditionWithClass.Create;
  try
    Assert.AreEqual(-1, Condition.Offset);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestConditionProperties;
var
  Condition: TBoldConditionWithClass;
begin
  Condition := TBoldConditionWithClass.Create;
  try
    Condition.MaxAnswers := 100;
    Condition.Offset := 50;
    Condition.AvailableAnswers := 200;

    Assert.AreEqual(100, Condition.MaxAnswers);
    Assert.AreEqual(50, Condition.Offset);
    Assert.AreEqual(200, Condition.AvailableAnswers);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestConditionWithClassCreate;
var
  Condition: TBoldConditionWithClass;
begin
  Condition := TBoldConditionWithClass.Create;
  try
    Assert.IsNotNull(Condition);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestConditionWithClassTimeDefault;
var
  Condition: TBoldConditionWithClass;
begin
  Condition := TBoldConditionWithClass.Create;
  try
    Assert.AreEqual(BOLDMAXTIMESTAMP, Condition.Time);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestConditionWithClassGetStreamName;
var
  Condition: TBoldConditionWithClass;
  Streamable: IBoldStreamable;
begin
  Condition := TBoldConditionWithClass.Create;
  try
    Streamable := Condition as IBoldStreamable;
    Assert.AreEqual('ClassCondition', Streamable.StreamName);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestConditionWithClassProperties;
var
  Condition: TBoldConditionWithClass;
begin
  Condition := TBoldConditionWithClass.Create;
  try
    Condition.TopSortedIndex := 5;
    Condition.Time := 12345;

    Assert.AreEqual(5, Condition.TopSortedIndex);
    Assert.AreEqual(TBoldTimestampType(12345), Condition.Time);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestSQLConditionCreate;
var
  Condition: TBoldSQLCondition;
begin
  Condition := TBoldSQLCondition.Create;
  try
    Assert.IsNotNull(Condition);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestSQLConditionJoinInheritedTablesDefault;
var
  Condition: TBoldSQLCondition;
begin
  Condition := TBoldSQLCondition.Create;
  try
    Assert.IsTrue(Condition.JoinInheritedTables);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestSQLConditionGetStreamName;
var
  Condition: TBoldSQLCondition;
  Streamable: IBoldStreamable;
begin
  Condition := TBoldSQLCondition.Create;
  try
    Streamable := Condition as IBoldStreamable;
    Assert.AreEqual('SQLCondition', Streamable.StreamName);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestSQLConditionProperties;
var
  Condition: TBoldSQLCondition;
  Params: TParams;
begin
  Condition := TBoldSQLCondition.Create;
  Params := TParams.Create;
  try
    Condition.WhereFragment := 'Id > 10';
    Condition.OrderBy := 'Name ASC';
    Condition.JoinInheritedTables := False;
    Condition.Params := Params;

    Assert.AreEqual('Id > 10', Condition.WhereFragment);
    Assert.AreEqual('Name ASC', Condition.OrderBy);
    Assert.IsFalse(Condition.JoinInheritedTables);
    Assert.AreSame(Params, Condition.Params);
  finally
    Params.Free;
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestRawSQLConditionGetStreamName;
var
  Condition: TBoldRawSQLCondition;
  Streamable: IBoldStreamable;
begin
  Condition := TBoldRawSQLCondition.Create;
  try
    Streamable := Condition as IBoldStreamable;
    Assert.AreEqual('RawSQLCondition', Streamable.StreamName);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestRawSQLConditionProperties;
var
  Condition: TBoldRawSQLCondition;
  Params: TParams;
begin
  Condition := TBoldRawSQLCondition.Create;
  Params := TParams.Create;
  try
    Condition.SQL := 'SELECT * FROM MyTable WHERE Id = :Id';
    Condition.Params := Params;

    Assert.AreEqual('SELECT * FROM MyTable WHERE Id = :Id', Condition.SQL);
    Assert.AreSame(Params, Condition.Params);
  finally
    Params.Free;
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestTimestampConditionGetStreamName;
var
  Condition: TBoldTimestampCondition;
  Streamable: IBoldStreamable;
begin
  Condition := TBoldTimestampCondition.Create;
  try
    Streamable := Condition as IBoldStreamable;
    Assert.AreEqual('TimestampCondition', Streamable.StreamName);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestTimestampConditionProperties;
var
  Condition: TBoldTimestampCondition;
begin
  Condition := TBoldTimestampCondition.Create;
  try
    Condition.Timestamp := 99999;
    Assert.AreEqual(TBoldTimestampType(99999), Condition.Timestamp);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestChangePointConditionGetStreamName;
var
  Condition: TBoldChangePointCondition;
  Streamable: IBoldStreamable;
begin
  Condition := TBoldChangePointCondition.Create;
  try
    Streamable := Condition as IBoldStreamable;
    Assert.AreEqual('ChangePointCondition', Streamable.StreamName);
  finally
    Condition.Free;
  end;
end;

procedure TTestBoldCondition.TestChangePointConditionProperties;
var
  Condition: TBoldChangePointCondition;
  IdList: TBoldObjectIdList;
  MemberIdList: TBoldMemberIdList;
begin
  Condition := TBoldChangePointCondition.Create;
  IdList := TBoldObjectIdList.Create;
  MemberIdList := TBoldMemberIdList.Create;
  try
    Condition.IdList := IdList;
    Condition.StartTime := 1000;
    Condition.EndTime := 2000;
    Condition.MemberIdList := MemberIdList;

    Assert.AreSame(IdList, Condition.IdList);
    Assert.AreEqual(TBoldTimestampType(1000), Condition.StartTime);
    Assert.AreEqual(TBoldTimestampType(2000), Condition.EndTime);
    Assert.AreSame(MemberIdList, Condition.MemberIdList);
  finally
    MemberIdList.Free;
    IdList.Free;
    Condition.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldCondition);

end.
