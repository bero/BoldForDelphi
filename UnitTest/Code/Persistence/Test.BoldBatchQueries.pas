unit Test.BoldBatchQueries;

{ Tests for TBoldSQLDataBaseConfig.UseBatchQueries: with the option on, the
  write statements of one UpdateDatabase are concatenated by
  TBoldBatchDataSetWrapper and sent to the server in a single round trip
  instead of one ExecSQL per statement.

  The fixture runs against the FireDAC adapter (SQLite in-memory by default,
  see UnitTest.ini). It flips UseBatchQueries on the live adapter config -
  TBoldDatabaseWrapper holds the adapter's config by reference - and frees the
  shared data module in TearDown so the flag never leaks into other fixtures. }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldDefs,
  BoldLogHandler,
  BoldSystem,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldPMappersDefault,
  BoldTestModel,
  maan_UndoRedoBase;

type
  { Captures the statements the adapter executes through BoldSQLLogHandler.
    BoldLogSQLWithParams logs one '...:SQL <n>- <first line>' header per
    executed statement, followed by indented continuation and parameter
    lines - so the number of headers is the number of round trips. }
  TBatchSQLCapture = class(TBoldLogHandler)
  private
    fLines: TStringList;
    function IsHeader(const Line: string): Boolean;
    function IsParamDump(const Line: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const s: string; LogType: TBoldLogType = ltInfo); override;
    procedure Clear; override;
    function CapturedText: string;
    { Number of executed statements (round trips) whose text contains SubText. }
    function StatementCount(const SubText: string): Integer;
    { Total number of occurrences of SubText in all executed statement text
      (parameter dump lines excluded). }
    function OccurrenceCount(const SubText: string): Integer;
  end;

  [TestFixture]
  [Category('Persistence')]
  TTestBoldBatchQueries = class
  private
    FCapture: TBatchSQLCapture;
    FSavedHandler: TBoldLogHandler;
    function GetSystem: TBoldSystem;
    function SystemMapper: TBoldSystemDefaultMapper;
    function Config: TBoldSQLDataBaseConfig;
    function Database: IBoldDatabase;
    procedure Save;
    { Discards the object space and reactivates the system so that everything
      is read back from the database. }
    procedure ReloadFromDatabase;
    function SavedSomeClassCount: Integer;
    function CountWithString(const Value: string): Integer;
    { 'id=aString' of every SomeClass instance, for assertion messages. }
    function AllSomeClassStrings: string;
    { Checks the pooled exec query out and back in and returns its
      implementor, so a test can later verify that the same instance is
      returned to the pool. }
    function PooledExecQueryImplementor: TObject;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    [Category('DB')]
    procedure TestBatchedCreateRoundTrips;
    [Test]
    [Category('DB')]
    procedure TestBatchedMixedUpdateRoundTrips;
    [Test]
    [Category('DB')]
    procedure TestBatchReleasesQueryWithParamCheckRestored;
    [Test]
    [Category('DB')]
    procedure TestFailedBatchLeavesSystemUsable;
  end;

implementation

uses
  BoldPersistenceControllerDefault;

const
  cSqlHeaderMarker = ':SQL ';
  cInvalidBatchPrefix = 'THIS IS NOT SQL;';

{ TBatchSQLCapture }

constructor TBatchSQLCapture.Create;
begin
  inherited Create;
  fLines := TStringList.Create;
end;

destructor TBatchSQLCapture.Destroy;
begin
  FreeAndNil(fLines);
  inherited;
end;

procedure TBatchSQLCapture.Log(const s: string; LogType: TBoldLogType);
begin
  fLines.Add(s);
end;

procedure TBatchSQLCapture.Clear;
begin
  fLines.Clear;
end;

function TBatchSQLCapture.CapturedText: string;
begin
  result := fLines.Text;
end;

function TBatchSQLCapture.IsHeader(const Line: string): Boolean;
begin
  result := Pos(cSqlHeaderMarker, Line) > 0;
end;

function TBatchSQLCapture.IsParamDump(const Line: string): Boolean;
begin
  // BoldLogSQLWithParams writes each parameter as an indented '[name]:value'.
  result := Copy(TrimLeft(Line), 1, 1) = '[';
end;

function TBatchSQLCapture.StatementCount(const SubText: string): Integer;
var
  i: Integer;
  Statement: string;

  procedure FlushStatement;
  begin
    if (Statement <> '') and (Pos(SubText, Statement) > 0) then
      Inc(result);
    Statement := '';
  end;

begin
  result := 0;
  Statement := '';
  for i := 0 to fLines.Count - 1 do
  begin
    if IsHeader(fLines[i]) then
      FlushStatement;
    if not IsParamDump(fLines[i]) then
      Statement := Statement + fLines[i] + sLineBreak;
  end;
  FlushStatement;
end;

function TBatchSQLCapture.OccurrenceCount(const SubText: string): Integer;
var
  i, p: Integer;
begin
  result := 0;
  for i := 0 to fLines.Count - 1 do
  begin
    if IsParamDump(fLines[i]) then
      continue;
    p := Pos(SubText, fLines[i]);
    while p > 0 do
    begin
      Inc(result);
      p := Pos(SubText, fLines[i], p + Length(SubText));
    end;
  end;
end;

{ TTestBoldBatchQueries }

procedure TTestBoldBatchQueries.SetUp;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
  Config.UseBatchQueries := True;
  FCapture := TBatchSQLCapture.Create;
  FSavedHandler := BoldSQLLogHandler;
  BoldSQLLogHandler := FCapture;
end;

procedure TTestBoldBatchQueries.TearDown;
begin
  BoldSQLLogHandler := FSavedHandler;
  FreeAndNil(FCapture);
  if Assigned(dmUndoRedo) and dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
  end;
  // The data module owns the adapter config we changed; drop it so the next
  // fixture gets a fresh one from the dfm.
  FreeAndNil(dmUndoRedo);
end;

function TTestBoldBatchQueries.GetSystem: TBoldSystem;
begin
  result := dmUndoRedo.BoldSystemHandle1.System;
end;

function TTestBoldBatchQueries.SystemMapper: TBoldSystemDefaultMapper;
begin
  result := (GetSystem.PersistenceController
    as TBoldPersistenceControllerDefault).PersistenceMapper;
end;

function TTestBoldBatchQueries.Config: TBoldSQLDataBaseConfig;
begin
  // The adapter's config is the instance TBoldDatabaseWrapper reads through
  // DatabaseWrapper.SQLDatabaseConfig, so changes here reach the batch code.
  result := dmUndoRedo.BoldDatabaseAdapterFireDAC1.SQLDatabaseConfig;
end;

function TTestBoldBatchQueries.Database: IBoldDatabase;
begin
  result := SystemMapper.Database;
end;

procedure TTestBoldBatchQueries.Save;
begin
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
end;

procedure TTestBoldBatchQueries.ReloadFromDatabase;
begin
  GetSystem.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
end;

function TTestBoldBatchQueries.SavedSomeClassCount: Integer;
begin
  ReloadFromDatabase;
  result := GetSystem.ClassByExpressionName['SomeClass'].Count;
end;

function TTestBoldBatchQueries.CountWithString(const Value: string): Integer;
begin
  result := StrToInt(GetSystem.EvaluateExpressionAsString(
    'SomeClass.allInstances->select(aString = ''' + Value + ''')->size'));
end;

function TTestBoldBatchQueries.AllSomeClassStrings: string;
var
  List: TBoldObjectList;
  i: Integer;
begin
  result := '';
  List := GetSystem.ClassByExpressionName['SomeClass'];
  for i := 0 to List.Count - 1 do
    result := result + List[i].BoldObjectLocator.BoldObjectID.AsString + '=' +
      TSomeClass(List[i]).aString + ' ';
end;

function TTestBoldBatchQueries.PooledExecQueryImplementor: TObject;
var
  Query: IBoldExecQuery;
begin
  Query := Database.GetExecQuery;
  result := Query.Implementor;
  Database.ReleaseExecQuery(Query);
end;

procedure TTestBoldBatchQueries.TestBatchedCreateRoundTrips;
var
  i: Integer;
begin
  for i := 1 to 3 do
    TSomeClass.Create(GetSystem).aString := 'Batched ' + IntToStr(i);
  FCapture.Clear;

  Save;

  Assert.IsTrue(FCapture.OccurrenceCount('INSERT INTO') >= 3,
    'Saving three new objects must execute at least three INSERTs ' +
    '(scenario precondition - fewer means the spy is not hooked up): ' +
    FCapture.CapturedText);
  Assert.AreEqual(1, FCapture.StatementCount('INSERT INTO'),
    'With UseBatchQueries all INSERTs of one UpdateDatabase must be sent in ' +
    'a single round trip: ' + FCapture.CapturedText);
  Assert.AreEqual(3, SavedSomeClassCount,
    'All three batched objects must be readable from the database');
  for i := 1 to 3 do
    Assert.AreEqual(1, CountWithString('Batched ' + IntToStr(i)),
      'Each object must carry its own parameter value, not a neighbour''s: ' +
      AllSomeClassStrings + FCapture.CapturedText);
end;

procedure TTestBoldBatchQueries.TestBatchedMixedUpdateRoundTrips;
var
  ObjA, ObjB, ObjC: TSomeClass;
  SavedMaxParams: Integer;
begin
  ObjA := TSomeClass.Create(GetSystem);
  ObjA.aString := 'A';
  ObjB := TSomeClass.Create(GetSystem);
  ObjB.aString := 'B';
  ObjC := TSomeClass.Create(GetSystem);
  ObjC.aString := 'C';
  Save;

  // One UpdateDatabase carrying an UPDATE, a DELETE and an INSERT. A low
  // parameter ceiling forces the wrapper to flush mid-batch, exercising the
  // ExecuteBatch re-arm path as well as the final flush in EndSQLBatch.
  ObjA.aString := 'A updated';
  ObjB.Delete;
  TSomeClass.Create(GetSystem).aString := 'D';
  SavedMaxParams := Config.MaxBatchQueryParams;
  Config.MaxBatchQueryParams := 6;
  try
    FCapture.Clear;
    Save;
  finally
    Config.MaxBatchQueryParams := SavedMaxParams;
  end;

  Assert.IsTrue(FCapture.OccurrenceCount('UPDATE ') > 0,
    'UPDATE expected: ' + FCapture.CapturedText);
  Assert.IsTrue(FCapture.OccurrenceCount('DELETE FROM') > 0,
    'DELETE expected: ' + FCapture.CapturedText);
  Assert.IsTrue(FCapture.OccurrenceCount('INSERT INTO') > 0,
    'INSERT expected: ' + FCapture.CapturedText);

  Assert.AreEqual(3, SavedSomeClassCount,
    'A, C and D must remain after the mixed batched save');
  Assert.AreEqual(1, CountWithString('A updated'),
    'The batched UPDATE must have been applied: ' + AllSomeClassStrings + FCapture.CapturedText);
  Assert.AreEqual(0, CountWithString('B'),
    'The batched DELETE must have been applied: ' + AllSomeClassStrings + FCapture.CapturedText);
  Assert.AreEqual(1, CountWithString('D'),
    'The batched INSERT must have been applied: ' + AllSomeClassStrings + FCapture.CapturedText);
  Assert.IsNotNull(ObjC, 'reference kept alive for readability');
end;

procedure TTestBoldBatchQueries.TestBatchReleasesQueryWithParamCheckRestored;
var
  Query: IBoldExecQuery;
begin
  TSomeClass.Create(GetSystem).aString := 'ParamCheck';
  Save;

  // The batch query is checked out of the connection's pool with
  // ParamCheck := false and must come back with ParamCheck := true, since
  // TBoldMemberSQLMapper.ValueToQuery names its parameters differently
  // depending on that flag.
  Query := Database.GetExecQuery;
  try
    Assert.IsTrue(Query.ParamCheck,
      'A pooled exec query must have ParamCheck restored after batch use');
  finally
    Database.ReleaseExecQuery(Query);
  end;
end;

procedure TTestBoldBatchQueries.TestFailedBatchLeavesSystemUsable;
var
  Pooled: TObject;
  Query: IBoldExecQuery;
begin
  TSomeClass.Create(GetSystem).aString := 'First';
  Save;
  Pooled := PooledExecQueryImplementor;

  // Make the batch itself fail at execution: the invalid prefix is prepended
  // to the concatenated statement by ExecuteBatch.
  TSomeClass.Create(GetSystem).aString := 'Second';
  Config.BatchQueryBegin := cInvalidBatchPrefix;
  try
    Assert.WillRaiseAny(Save, 'A batch starting with invalid SQL must fail');
  finally
    Config.BatchQueryBegin := '';
  end;

  // The failed batch must have been torn down completely: the pooled query
  // is back in the pool, and the retry saves without inheriting parameters
  // or SQL from the failed attempt.
  Query := Database.GetExecQuery;
  try
    Assert.AreSame(Pooled, Query.Implementor,
      'FailSQLBatch must return the batch query to the pool');
    Assert.IsTrue(Query.ParamCheck, 'and restore ParamCheck');
    Assert.AreEqual(0, Query.ParamCount, 'and leave no parameters behind');
  finally
    Database.ReleaseExecQuery(Query);
  end;

  Save;
  Assert.AreEqual(2, SavedSomeClassCount,
    'The retried save must persist the object the failed batch dropped');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldBatchQueries);

end.
