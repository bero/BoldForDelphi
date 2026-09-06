unit BoldPersistenceScenarios;

{******************************************************************************}
{                                                                              }
{  BoldPersistenceScenarios - adapter-neutral persistence scenarios            }
{                                                                              }
{  The scenarios behind the #88 (PMCreate row reset) and #80 (UseBatchQueries) }
{  regression tests, written once against the code-built test system of        }
{  TBoldTestCasePersistence so that the same bodies run on every adapter the   }
{  build has: TTestPersistenceScenariosFireDAC in the default configuration,   }
{  TTestPersistenceScenariosUniDAC under DebugUniDAC. The fixtures are thin -  }
{  one [Test] per scenario that forwards here.                                 }
{                                                                              }
{******************************************************************************}

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  BoldSystemHandle,
  BoldSystem,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldAbstractDatabaseAdapter,
  BoldLogHandler,
  Test.BoldBatchQueries;

type
  TBoldPersistenceScenarios = class
  private
    FSystemHandle: TBoldSystemHandle;
    FAdapter: TBoldAbstractDatabaseAdapter;
    FCapture: TBatchSQLCapture;
    FSavedHandler: TBoldLogHandler;
    function System: TBoldSystem;
    function Config: TBoldSQLDataBaseConfig;
    function Database: IBoldDatabase;
    function NewTestObject(const AName: string): TBoldObject;
    procedure Save;
    procedure ReloadFromDatabase;
    function SavedObjectCount: Integer;
    function CountWithName(const AName: string): Integer;
    function AllNames: string;
    function PooledExecQueryImplementor: TObject;
  public
    constructor Create(ASystemHandle: TBoldSystemHandle; AAdapter: TBoldAbstractDatabaseAdapter);
    destructor Destroy; override;

    { #88: three new objects of one class saved in one UpdateDatabase, inserted
      row by row, must each keep their own attribute values. }
    procedure ThreeObjectsKeepDistinctValues;
    { #80: with UseBatchQueries the INSERTs of one save go in one round trip
      and every object still carries its own values. }
    procedure BatchedCreateRoundTrips;
    { #80: update, delete and insert in one save with a forced mid-batch flush. }
    procedure BatchedMixedUpdateRoundTrips;
    { #80: the pooled exec query comes back with ParamCheck restored. }
    procedure BatchReleasesQueryWithParamCheckRestored;
    { #80: a failed batch returns its query to the pool and the retry saves. }
    procedure FailedBatchLeavesSystemUsable;
  end;

implementation

uses
  BoldPersistenceControllerDefault;

const
  cTestClass = 'TestClass';
  cNameAttribute = 'Name';
  cInvalidBatchPrefix = 'THIS IS NOT SQL;';

{ TBoldPersistenceScenarios }

constructor TBoldPersistenceScenarios.Create(ASystemHandle: TBoldSystemHandle;
  AAdapter: TBoldAbstractDatabaseAdapter);
begin
  inherited Create;
  FSystemHandle := ASystemHandle;
  FAdapter := AAdapter;
  FCapture := TBatchSQLCapture.Create;
  FSavedHandler := BoldSQLLogHandler;
  BoldSQLLogHandler := FCapture;
end;

destructor TBoldPersistenceScenarios.Destroy;
begin
  BoldSQLLogHandler := FSavedHandler;
  FreeAndNil(FCapture);
  inherited;
end;

function TBoldPersistenceScenarios.System: TBoldSystem;
begin
  result := FSystemHandle.System;
end;

function TBoldPersistenceScenarios.Config: TBoldSQLDataBaseConfig;
begin
  // The adapter's config is the instance the database wrapper reads, so
  // changes here reach the batch and insert code. The fixture recreates the
  // adapter for every test, so nothing needs restoring.
  result := FAdapter.SQLDatabaseConfig;
end;

function TBoldPersistenceScenarios.Database: IBoldDatabase;
begin
  result := (System.PersistenceController as TBoldPersistenceControllerDefault)
    .PersistenceMapper.Database;
end;

function TBoldPersistenceScenarios.NewTestObject(const AName: string): TBoldObject;
begin
  result := System.CreateNewObjectByExpressionName(cTestClass);
  result.BoldMemberByExpressionName[cNameAttribute].AsString := AName;
end;

procedure TBoldPersistenceScenarios.Save;
begin
  FSystemHandle.UpdateDatabase;
end;

procedure TBoldPersistenceScenarios.ReloadFromDatabase;
begin
  System.Discard;
  FSystemHandle.Active := False;
  FSystemHandle.Active := True;
end;

function TBoldPersistenceScenarios.SavedObjectCount: Integer;
begin
  ReloadFromDatabase;
  result := System.ClassByExpressionName[cTestClass].Count;
end;

function TBoldPersistenceScenarios.CountWithName(const AName: string): Integer;
var
  List: TBoldObjectList;
  i: Integer;
begin
  result := 0;
  List := System.ClassByExpressionName[cTestClass];
  for i := 0 to List.Count - 1 do
    if List[i].BoldMemberByExpressionName[cNameAttribute].AsString = AName then
      Inc(result);
end;

function TBoldPersistenceScenarios.AllNames: string;
var
  List: TBoldObjectList;
  i: Integer;
begin
  result := '';
  List := System.ClassByExpressionName[cTestClass];
  for i := 0 to List.Count - 1 do
    result := result + List[i].BoldObjectLocator.BoldObjectID.AsString + '=' +
      List[i].BoldMemberByExpressionName[cNameAttribute].AsString + ' ';
end;

function TBoldPersistenceScenarios.PooledExecQueryImplementor: TObject;
var
  Query: IBoldExecQuery;
begin
  Query := Database.GetExecQuery;
  result := Query.Implementor;
  Database.ReleaseExecQuery(Query);
end;

procedure TBoldPersistenceScenarios.ThreeObjectsKeepDistinctValues;
const
  cValues: array[1..3] of string = ('First', 'Second', 'Third');
var
  i: Integer;
begin
  // Row-by-row inserts are where PMCreate's per-row parameter reset matters
  // (on FireDAC a reset through the detached TParams view gave the third row
  // the second row's values). Server engines insert multi-row by default, so
  // the limit is forced to 1 on every adapter.
  Config.MultiRowInsertLimit := 1;
  for i := Low(cValues) to High(cValues) do
    NewTestObject(cValues[i]);
  Save;

  Assert.AreEqual(3, SavedObjectCount, 'All three objects must be persisted: ' + AllNames);
  for i := Low(cValues) to High(cValues) do
    Assert.AreEqual(1, CountWithName(cValues[i]),
      'Object ' + IntToStr(i) + ' must carry its own value, not a neighbour''s. Reloaded: ' + AllNames);
end;

procedure TBoldPersistenceScenarios.BatchedCreateRoundTrips;
var
  i: Integer;
begin
  Config.UseBatchQueries := True;
  for i := 1 to 3 do
    NewTestObject('Batched ' + IntToStr(i));
  FCapture.Clear;

  Save;

  Assert.IsTrue(FCapture.OccurrenceCount('INSERT INTO') >= 3,
    'Saving three new objects must execute at least three INSERTs ' +
    '(scenario precondition - fewer means the spy is not hooked up): ' + FCapture.CapturedText);
  Assert.AreEqual(1, FCapture.StatementCount('INSERT INTO'),
    'With UseBatchQueries all INSERTs of one UpdateDatabase must be sent in ' +
    'a single round trip: ' + FCapture.CapturedText);
  Assert.AreEqual(3, SavedObjectCount, 'All three batched objects must be readable from the database');
  for i := 1 to 3 do
    Assert.AreEqual(1, CountWithName('Batched ' + IntToStr(i)),
      'Each object must carry its own parameter value, not a neighbour''s: ' +
      AllNames + FCapture.CapturedText);
end;

procedure TBoldPersistenceScenarios.BatchedMixedUpdateRoundTrips;
var
  ObjA, ObjB: TBoldObject;
begin
  Config.UseBatchQueries := True;
  ObjA := NewTestObject('A');
  ObjB := NewTestObject('B');
  NewTestObject('C');
  Save;

  // One UpdateDatabase carrying an UPDATE, a DELETE and an INSERT. A low
  // parameter ceiling forces the wrapper to flush mid-batch, exercising the
  // ExecuteBatch re-arm path as well as the final flush in EndSQLBatch.
  ObjA.BoldMemberByExpressionName[cNameAttribute].AsString := 'A updated';
  ObjB.Delete;
  NewTestObject('D');
  Config.MaxBatchQueryParams := 6;
  FCapture.Clear;
  Save;

  Assert.IsTrue(FCapture.OccurrenceCount('UPDATE ') > 0, 'UPDATE expected: ' + FCapture.CapturedText);
  Assert.IsTrue(FCapture.OccurrenceCount('DELETE FROM') > 0, 'DELETE expected: ' + FCapture.CapturedText);
  Assert.IsTrue(FCapture.OccurrenceCount('INSERT INTO') > 0, 'INSERT expected: ' + FCapture.CapturedText);

  Assert.AreEqual(3, SavedObjectCount, 'A, C and D must remain after the mixed batched save: ' + AllNames);
  Assert.AreEqual(1, CountWithName('A updated'), 'The batched UPDATE must have been applied: ' + AllNames);
  Assert.AreEqual(0, CountWithName('B'), 'The batched DELETE must have been applied: ' + AllNames);
  Assert.AreEqual(1, CountWithName('D'), 'The batched INSERT must have been applied: ' + AllNames);
end;

procedure TBoldPersistenceScenarios.BatchReleasesQueryWithParamCheckRestored;
var
  Query: IBoldExecQuery;
begin
  Config.UseBatchQueries := True;
  NewTestObject('ParamCheck');
  Save;

  // The batch query is checked out of the connection's pool with
  // ParamCheck := false and must come back with ParamCheck := true, since
  // TBoldMemberSQLMapper.ValueToQuery names its parameters differently
  // depending on that flag.
  Query := Database.GetExecQuery;
  try
    Assert.IsTrue(Query.ParamCheck, 'A pooled exec query must have ParamCheck restored after batch use');
  finally
    Database.ReleaseExecQuery(Query);
  end;
end;

procedure TBoldPersistenceScenarios.FailedBatchLeavesSystemUsable;
var
  Pooled: TObject;
  Query: IBoldExecQuery;
begin
  Config.UseBatchQueries := True;
  NewTestObject('First');
  Save;
  Pooled := PooledExecQueryImplementor;

  // Make the batch itself fail at execution: the invalid prefix is prepended
  // to the concatenated statement by ExecuteBatch.
  NewTestObject('Second');
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
    Assert.AreSame(Pooled, Query.Implementor, 'FailSQLBatch must return the batch query to the pool');
    Assert.IsTrue(Query.ParamCheck, 'and restore ParamCheck');
    Assert.AreEqual(0, Query.ParamCount, 'and leave no parameters behind');
  finally
    Database.ReleaseExecQuery(Query);
  end;

  Save;
  Assert.AreEqual(2, SavedObjectCount, 'The retried save must persist the object the failed batch dropped: ' + AllNames);
end;

end.
