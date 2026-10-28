unit Test.BoldCondition;

interface

uses
  Classes,
  BoldTestFramework,
  BoldCondition;

type
  TTestBoldCondition = class(TBoldTestCase)
  published
    // TBoldCondition tests (base class)
    procedure TestConditionCreate;
    procedure TestConditionMaxAnswersDefault;
    procedure TestConditionOffsetDefault;
    procedure TestConditionProperties;

    // TBoldConditionWithClass tests
    procedure TestConditionWithClassCreate;
    procedure TestConditionWithClassTimeDefault;
    procedure TestConditionWithClassGetStreamName;
    procedure TestConditionWithClassProperties;

    // TBoldSQLCondition tests
    procedure TestSQLConditionCreate;
    procedure TestSQLConditionJoinInheritedTablesDefault;
    procedure TestSQLConditionGetStreamName;
    procedure TestSQLConditionProperties;

    // TBoldRawSQLCondition tests
    procedure TestRawSQLConditionGetStreamName;
    procedure TestRawSQLConditionProperties;

    // TBoldTimestampCondition tests
    procedure TestTimestampConditionGetStreamName;
    procedure TestTimestampConditionProperties;

    // TBoldChangePointCondition tests
    procedure TestChangePointConditionGetStreamName;
    procedure TestChangePointConditionProperties;
  end;

  TTestBoldConditionXMLStreaming = class(TBoldTestCase)
  private
    FDataModule: TDataModule;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestXMLRoundTrip_ConditionWithClass;
    procedure TestXMLRoundTrip_SQLCondition_NoParams;
    procedure TestXMLRoundTrip_SQLCondition_WithParams;
    procedure TestXMLRoundTrip_SQLCondition_ParamValues;
    procedure TestXMLRead_SQLCondition_LegacyStringData;
    procedure TestXMLRoundTrip_TimestampCondition;
    procedure TestXMLRoundTrip_ChangePointCondition;
  end;

implementation

uses
  SysUtils,
  DB,
  BoldDefs,
  BoldId,
  BoldDefaultId,
  BoldStreams,
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  jehoBoldTestDataModule;

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
    FreeAfterTest(Condition);
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
    FreeAfterTest(Condition);
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
    FreeAfterTest(Condition);
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
    FreeAfterTest(Condition);
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
    FreeAfterTest(Condition);
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

{ TTestBoldConditionXMLStreaming }

procedure TTestBoldConditionXMLStreaming.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldConditionXMLStreaming.TearDown;
begin
  FreeAndNil(FDataModule);
end;

procedure TTestBoldConditionXMLStreaming.TestXMLRoundTrip_ConditionWithClass;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TBoldXMLDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Cond1, Cond2: TBoldConditionWithClass;
begin
  Cond1 := TBoldConditionWithClass.Create;
  try
    Cond1.MaxAnswers := 100;
    Cond1.Offset := 50;
    Cond1.AvailableAnswers := 200;
    Cond1.TopSortedIndex := 5;
    Cond1.Time := 12345;

    // Write to XML
    aDoc := TBoldXMLDocument.Create;
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'CondWithClassTest');
      aNode.WriteSubNodeObject('Condition', '', Cond1);
      anXML := aDoc.RootXML;
      aNode.Free;
      aDoc.Free;

      // Read back from XML
      aDoc := TBoldXMLDocument.Create;
      aDoc.LoadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'CondWithClassTest');
      try
        Cond2 := aNode.ReadSubNodeObject('Condition', '') as TBoldConditionWithClass;
        try
          Assert.AreEqual(100, Cond2.MaxAnswers, 'MaxAnswers mismatch');
          Assert.AreEqual(50, Cond2.Offset, 'Offset mismatch');
          Assert.AreEqual(200, Cond2.AvailableAnswers, 'AvailableAnswers mismatch');
          Assert.AreEqual(5, Cond2.TopSortedIndex, 'TopSortedIndex mismatch');
          Assert.AreEqual(TBoldTimestampType(12345), Cond2.Time, 'Time mismatch');
        finally
          Cond2.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    Cond1.Free;
  end;
end;

procedure TTestBoldConditionXMLStreaming.TestXMLRoundTrip_SQLCondition_NoParams;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TBoldXMLDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Cond1, Cond2: TBoldSQLCondition;
begin
  Cond1 := TBoldSQLCondition.Create;
  try
    Cond1.TopSortedIndex := 2;
    Cond1.WhereFragment := 'Id > 10';
    Cond1.OrderBy := 'Name ASC';
    Cond1.JoinInheritedTables := False;
    Cond1.Params := nil;

    // Write to XML
    aDoc := TBoldXMLDocument.Create;
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'SQLCondNoParamsTest');
      aNode.WriteSubNodeObject('Condition', '', Cond1);
      anXML := aDoc.RootXML;
      aNode.Free;
      aDoc.Free;

      // Read back from XML
      aDoc := TBoldXMLDocument.Create;
      aDoc.LoadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'SQLCondNoParamsTest');
      try
        Cond2 := aNode.ReadSubNodeObject('Condition', '') as TBoldSQLCondition;
        try
          Assert.AreEqual('Id > 10', Cond2.WhereFragment, 'WhereFragment mismatch');
          Assert.AreEqual('Name ASC', Cond2.OrderBy, 'OrderBy mismatch');
          Assert.IsFalse(Cond2.JoinInheritedTables, 'JoinInheritedTables mismatch');
          Assert.AreEqual(2, Cond2.TopSortedIndex, 'TopSortedIndex mismatch');
          Assert.IsNull(Cond2.Params, 'Params should be nil');
        finally
          Cond2.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    Cond1.Free;
  end;
end;

procedure TTestBoldConditionXMLStreaming.TestXMLRoundTrip_SQLCondition_WithParams;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TBoldXMLDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Cond1, Cond2: TBoldSQLCondition;
begin
  Cond1 := TBoldSQLCondition.Create;
  try
    Cond1.TopSortedIndex := 3;
    Cond1.WhereFragment := 'Name = :Name AND Id = :Id';
    Cond1.OrderBy := 'Id DESC';
    Cond1.Params := TParams.Create(nil);
    Cond1.Params.CreateParam(ftString, 'aString', ptInput).AsString := 'test value';
    Cond1.Params.CreateParam(ftInteger, 'anInt', ptInput).AsInteger := 42;

    // Write to XML
    aDoc := TBoldXMLDocument.Create;
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'SQLCondWithParamsTest');
      aNode.WriteSubNodeObject('Condition', '', Cond1);
      anXML := aDoc.RootXML;
      aNode.Free;
      aDoc.Free;

      // Read back from XML
      aDoc := TBoldXMLDocument.Create;
      aDoc.LoadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'SQLCondWithParamsTest');
      try
        Cond2 := aNode.ReadSubNodeObject('Condition', '') as TBoldSQLCondition;
        try
          Assert.AreEqual('Name = :Name AND Id = :Id', Cond2.WhereFragment, 'WhereFragment mismatch');
          Assert.AreEqual('Id DESC', Cond2.OrderBy, 'OrderBy mismatch');
          Assert.IsNotNull(Cond2.Params, 'Params should not be nil');
          Assert.AreEqual(2, Cond2.Params.Count, 'Params count mismatch');
          Assert.AreEqual('aString', Cond2.Params[0].Name, 'Param[0] name mismatch');
          Assert.AreEqual(Integer(Cond1.Params[0].DataType), Integer(Cond2.Params[0].DataType), 'Param[0] DataType mismatch');
          Assert.AreEqual(Integer(ptInput), Integer(Cond2.Params[0].ParamType), 'Param[0] ParamType mismatch');
          Assert.AreEqual('anInt', Cond2.Params[1].Name, 'Param[1] name mismatch');
          Assert.AreEqual(Integer(Cond1.Params[1].DataType), Integer(Cond2.Params[1].DataType), 'Param[1] DataType mismatch');
          Assert.AreEqual(Integer(ptInput), Integer(Cond2.Params[1].ParamType), 'Param[1] ParamType mismatch');
        finally
          Cond2.Params.Free;
          Cond2.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    Cond1.Params.Free;
    Cond1.Free;
  end;
end;

procedure TTestBoldConditionXMLStreaming.TestXMLRoundTrip_TimestampCondition;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TBoldXMLDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Cond1, Cond2: TBoldTimestampCondition;
begin
  Cond1 := TBoldTimestampCondition.Create;
  try
    Cond1.TopSortedIndex := 1;
    Cond1.Time := 42;
    Cond1.Timestamp := 99999;

    // Write to XML
    aDoc := TBoldXMLDocument.Create;
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'TimestampCondTest');
      aNode.WriteSubNodeObject('Condition', '', Cond1);
      anXML := aDoc.RootXML;
      aNode.Free;
      aDoc.Free;

      // Read back from XML
      aDoc := TBoldXMLDocument.Create;
      aDoc.LoadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'TimestampCondTest');
      try
        Cond2 := aNode.ReadSubNodeObject('Condition', '') as TBoldTimestampCondition;
        try
          Assert.AreEqual(1, Cond2.TopSortedIndex, 'TopSortedIndex mismatch');
          Assert.AreEqual(TBoldTimestampType(42), Cond2.Time, 'Time mismatch');
          Assert.AreEqual(TBoldTimestampType(99999), Cond2.Timestamp, 'Timestamp mismatch');
        finally
          Cond2.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    Cond1.Free;
  end;
end;

procedure TTestBoldConditionXMLStreaming.TestXMLRoundTrip_ChangePointCondition;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TBoldXMLDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Cond1, Cond2: TBoldChangePointCondition;
  anId: TBoldDefaultId;
begin
  anId := nil;
  Cond1 := TBoldChangePointCondition.Create;
  try
    Cond1.MaxAnswers := 10;
    Cond1.Offset := 5;
    Cond1.IdList := TBoldObjectIdList.Create;
    anId := TBoldDefaultId.CreateWithClassID(2, False);
    anId.AsInteger := 3;
    Cond1.IdList.Add(anId);
    Cond1.StartTime := 1000;
    Cond1.EndTime := 2000;
    Cond1.MemberIdList := TBoldMemberIdList.Create;
    Cond1.MemberIdList.Add(TBoldMemberId.Create(7));

    // Write to XML
    aDoc := TBoldXMLDocument.Create;
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'ChangePointCondTest');
      aNode.WriteSubNodeObject('Condition', '', Cond1);
      anXML := aDoc.RootXML;
      aNode.Free;
      aDoc.Free;

      // Read back from XML
      aDoc := TBoldXMLDocument.Create;
      aDoc.LoadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'ChangePointCondTest');
      try
        Cond2 := aNode.ReadSubNodeObject('Condition', '') as TBoldChangePointCondition;
        try
          Assert.AreEqual(10, Cond2.MaxAnswers, 'MaxAnswers mismatch');
          Assert.AreEqual(5, Cond2.Offset, 'Offset mismatch');
          Assert.AreEqual(TBoldTimestampType(1000), Cond2.StartTime, 'StartTime mismatch');
          Assert.AreEqual(TBoldTimestampType(2000), Cond2.EndTime, 'EndTime mismatch');
          Assert.IsNotNull(Cond2.IdList, 'IdList should not be nil');
          Assert.AreEqual(1, Cond2.IdList.Count, 'IdList count mismatch');
          Assert.IsNotNull(Cond2.MemberIdList, 'MemberIdList should not be nil');
          Assert.AreEqual(1, Cond2.MemberIdList.Count, 'MemberIdList count mismatch');
        finally
          if Assigned(Cond2.IdList) then
            Cond2.IdList.Free;
          if Assigned(Cond2.MemberIdList) then
            Cond2.MemberIdList.Free;
          Cond2.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    Cond1.IdList.Free;
    Cond1.MemberIdList.Free;
    Cond1.Free;
    anId.Free;
  end;
end;

{ Parameter VALUES must survive the XML round trip, not only names and types.
  AsString makes a parameter ftWideString; AsAnsiString keeps ftString. The
  numeric, boolean, date-time and null parameters guard the binary data path.
  All mismatches are collected and reported together. }
procedure TTestBoldConditionXMLStreaming.TestXMLRoundTrip_SQLCondition_ParamValues;
const
  cUnicode = #$00C5'sa '#$0436#$6F22;  // A-ring outside ASCII, Cyrillic and CJK outside ANSI
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TBoldXMLDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Cond1, Cond2: TBoldSQLCondition;
  Moment: TDateTime;
  Mismatches: TStringList;
  i: Integer;

  procedure Expect(AOk: Boolean; const AWhat, AActual: string);
  begin
    if not AOk then
      Mismatches.Add(AWhat + ': got [' + AActual + ']');
  end;

begin
  Moment := EncodeDate(2026, 10, 24) + EncodeTime(12, 30, 15, 0);
  Mismatches := TStringList.Create;
  Cond1 := TBoldSQLCondition.Create;
  try
    Cond1.WhereFragment := 'Name = :aString';
    Cond1.Params := TParams.Create(nil);
    Cond1.Params.CreateParam(ftString, 'aString', ptInput).AsString := 'test value';
    Cond1.Params.CreateParam(ftWideString, 'aUnicode', ptInput).AsWideString := cUnicode;
    Cond1.Params.CreateParam(ftString, 'anAnsi', ptInput).AsAnsiString := 'ansi value';
    Cond1.Params.CreateParam(ftInteger, 'anInt', ptInput).AsInteger := 42;
    Cond1.Params.CreateParam(ftFloat, 'aFloat', ptInput).AsFloat := 1.5;
    Cond1.Params.CreateParam(ftBoolean, 'aBool', ptInput).AsBoolean := True;
    Cond1.Params.CreateParam(ftDateTime, 'aMoment', ptInput).AsDateTime := Moment;
    Cond1.Params.CreateParam(ftInteger, 'aNull', ptInput);
    Cond1.Params.CreateParam(ftString, 'anEmpty', ptInput).AsString := '';

    aDoc := TBoldXMLDocument.Create;
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'SQLCondParamValuesTest');
      aNode.WriteSubNodeObject('Condition', '', Cond1);
      anXML := aDoc.RootXML;
      aNode.Free;
      aDoc.Free;

      aDoc := TBoldXMLDocument.Create;
      aDoc.LoadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'SQLCondParamValuesTest');
      try
        Cond2 := aNode.ReadSubNodeObject('Condition', '') as TBoldSQLCondition;
        try
          Assert.AreEqual(Cond1.Params.Count, Cond2.Params.Count, 'Params count');
          for i := 0 to Cond1.Params.Count - 1 do
            Expect(Cond1.Params[i].DataType = Cond2.Params[i].DataType, Cond1.Params[i].Name + ' DataType',
              IntToStr(Ord(Cond2.Params[i].DataType)));
          Expect(Cond2.Params.ParamByName('aString').AsString = 'test value', 'aString', Cond2.Params.ParamByName('aString').AsString);
          Expect(Cond2.Params.ParamByName('aUnicode').AsString = cUnicode, 'aUnicode', Cond2.Params.ParamByName('aUnicode').AsString);
          Expect(Cond2.Params.ParamByName('anAnsi').AsAnsiString = 'ansi value', 'anAnsi', string(Cond2.Params.ParamByName('anAnsi').AsAnsiString));
          Expect(Cond2.Params.ParamByName('anInt').AsInteger = 42, 'anInt', Cond2.Params.ParamByName('anInt').AsString);
          Expect(Cond2.Params.ParamByName('aFloat').AsFloat = 1.5, 'aFloat', Cond2.Params.ParamByName('aFloat').AsString);
          Expect(Cond2.Params.ParamByName('aBool').AsBoolean, 'aBool', Cond2.Params.ParamByName('aBool').AsString);
          Expect(Abs(Cond2.Params.ParamByName('aMoment').AsDateTime - Moment) < 1 / MSecsPerDay, 'aMoment',
            DateTimeToStr(Cond2.Params.ParamByName('aMoment').AsDateTime));
          Expect(Cond2.Params.ParamByName('aNull').IsNull, 'aNull IsNull', Cond2.Params.ParamByName('aNull').AsString);
          Expect(not Cond2.Params.ParamByName('anEmpty').IsNull and (Cond2.Params.ParamByName('anEmpty').AsString = ''),
            'anEmpty is an empty, non-null string', Cond2.Params.ParamByName('anEmpty').AsString);
          Assert.IsTrue(Mismatches.Count = 0,
            Format('%d parameter mismatch(es):' + sLineBreak + '%s', [Mismatches.Count, Mismatches.Text]));
        finally
          Cond2.Params.Free;
          Cond2.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    Cond1.Params.Free;
    Cond1.Free;
    Mismatches.Free;
  end;
end;

{ XML written by older versions stored string parameters as GetData bytes:
  the ANSI text, then whatever the buffer held, base64-encoded when that
  included a #0. The payloads below are 'legacy'#0#$AA and 'ansi'#0. Reading
  such XML must give back the text before the #0. }
procedure TTestBoldConditionXMLStreaming.TestXMLRead_SQLCondition_LegacyStringData;
const
  cWidePlaceholder = 'WIDE-PLACEHOLDER';
  cAnsiPlaceholder = 'ANSI-PLACEHOLDER';
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TBoldXMLDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Cond1, Cond2: TBoldSQLCondition;

  procedure ReplaceData(const APlaceholder, ABase64: string);
  var
    NewText: string;
  begin
    NewText := StringReplace(anXML, '<Data>' + APlaceholder + '</Data>',
      '<Data dt="binary.base64">' + ABase64 + '</Data>', []);
    Assert.AreNotEqual(anXML, NewText, 'Placeholder not found in the written XML: ' + APlaceholder);
    anXML := NewText;
  end;

begin
  Cond1 := TBoldSQLCondition.Create;
  try
    Cond1.WhereFragment := 'x';
    Cond1.Params := TParams.Create(nil);
    Cond1.Params.CreateParam(ftString, 'oldWide', ptInput).AsWideString := cWidePlaceholder;
    Cond1.Params.CreateParam(ftString, 'oldAnsi', ptInput).AsAnsiString := cAnsiPlaceholder;

    aDoc := TBoldXMLDocument.Create;
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'SQLCondLegacyTest');
      aNode.WriteSubNodeObject('Condition', '', Cond1);
      anXML := aDoc.RootXML;
      aNode.Free;
      aDoc.Free;

      ReplaceData(cWidePlaceholder, 'bGVnYWN5AKo=');
      ReplaceData(cAnsiPlaceholder, 'YW5zaQA=');

      aDoc := TBoldXMLDocument.Create;
      aDoc.LoadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'SQLCondLegacyTest');
      try
        Cond2 := aNode.ReadSubNodeObject('Condition', '') as TBoldSQLCondition;
        try
          Assert.AreEqual('legacy', Cond2.Params.ParamByName('oldWide').AsString, 'oldWide');
          Assert.AreEqual(Integer(ftWideString), Integer(Cond2.Params.ParamByName('oldWide').DataType), 'oldWide DataType');
          Assert.AreEqual('ansi', string(Cond2.Params.ParamByName('oldAnsi').AsAnsiString), 'oldAnsi');
          Assert.AreEqual(Integer(ftString), Integer(Cond2.Params.ParamByName('oldAnsi').DataType), 'oldAnsi DataType');
        finally
          Cond2.Params.Free;
          Cond2.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    Cond1.Params.Free;
    Cond1.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldCondition);
  BoldRegisterTest(TTestBoldConditionXMLStreaming);

end.
