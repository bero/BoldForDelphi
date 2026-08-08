unit Test.BoldSqlParameterization;

{ Tests that persistence-mapper SQL uses query parameters instead of literal
  ids. Literal id lists make every statement unique text, which on SQL Server
  costs a compile per statement and floods the plan cache with single-use
  ad-hoc plans. }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldDefs,
  BoldLogHandler,
  BoldSystem,
  BoldId,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldPMappersDefault,
  BoldTestModel,
  maan_UndoRedoBase;

type
  { Captures every SQL statement the DB adapter executes, by hooking the
    global BoldSQLLogHandler used by BoldLogSQLWithParams. }
  TSQLCaptureLogHandler = class(TBoldLogHandler)
  private
    fLines: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const s: string; LogType: TBoldLogType = ltInfo); override;
    procedure Clear; override;
    function CapturedText: string;
    function ContainsText(const SubText: string): Boolean;
    { All captured statements (first line + continuation/param lines) that
      contain SubText, concatenated. Empty if no statement matched. }
    function StatementsContaining(const SubText: string): string;
    { Number of DISTINCT executed statement texts containing SubText.
      Timestamps, statement counters and parameter-value dump lines are
      stripped, so only the SQL text itself decides distinctness - the same
      criterion a server-side plan cache keys on. }
    function DistinctStatementCount(const SubText: string): Integer;
  end;

  [TestFixture]
  [Category('PMapper')]
  TTestBoldSqlParameterization = class
  private
    FCapture: TSQLCaptureLogHandler;
    FSavedHandler: TBoldLogHandler;
    function GetSystem: TBoldSystem;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestMultilinkFetchUsesParams;
    [Test]
    procedure TestDeleteUsesParams;
  end;

  { Fragment-level tests for the IN-list bucket padding: id lists are padded
    up to fixed sizes (10, 50, 100, 250, 500) by repeating the last id, so
    fetches of different list sizes produce identical SQL text and SQL Server
    can reuse a handful of cached plans instead of compiling per fetch. }
  [TestFixture]
  [Category('PMapper')]
  TTestBoldIdListWhereFragment = class
  private
    FMapper: TBoldObjectDefaultMapper;
    FQuery: IBoldExecQuery;
    FSavedMaxParams: Integer;
    function SystemMapper: TBoldSystemDefaultMapper;
    function Config: TBoldSQLDataBaseConfig;
    function MakeIdList(Count: Integer): TBoldObjectIdList;
    function BuildFragment(IdList: TBoldObjectIdList): string;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestSingleIdNotPadded;
    [Test]
    procedure TestTwelveIdsPaddedToBucket50;
    [Test]
    procedure TestPaddedTailRepeatsLastId;
    [Test]
    procedure Test250IdsFillBucketExactly;
    [Test]
    procedure TestNoPaddingWhenBucketExceedsMaxParams;
    [Test]
    procedure TestLegacyLargeListStaysLiteral;
  end;

implementation

uses
  BoldDefaultId,
  BoldPersistenceControllerDefault;

{ TSQLCaptureLogHandler }

constructor TSQLCaptureLogHandler.Create;
begin
  inherited Create;
  fLines := TStringList.Create;
end;

destructor TSQLCaptureLogHandler.Destroy;
begin
  FreeAndNil(fLines);
  inherited;
end;

procedure TSQLCaptureLogHandler.Log(const s: string; LogType: TBoldLogType);
begin
  fLines.Add(s);
end;

procedure TSQLCaptureLogHandler.Clear;
begin
  fLines.Clear;
end;

function TSQLCaptureLogHandler.CapturedText: string;
begin
  result := fLines.Text;
end;

function TSQLCaptureLogHandler.ContainsText(const SubText: string): Boolean;
begin
  result := Pos(SubText, CapturedText) > 0;
end;

function TSQLCaptureLogHandler.StatementsContaining(const SubText: string): string;
var
  i: Integer;
  Statement: string;

  procedure FlushIfMatching;
  begin
    if Pos(SubText, Statement) > 0 then
      result := result + Statement;
    Statement := '';
  end;

begin
  result := '';
  Statement := '';
  for i := 0 to fLines.Count - 1 do
  begin
    // BoldLogSQLWithParams starts each statement with a 'SQL <n>- ...' line;
    // continuation and parameter lines are indented.
    if Pos(':SQL ', fLines[i]) > 0 then
      FlushIfMatching;
    Statement := Statement + fLines[i] + sLineBreak;
  end;
  FlushIfMatching;
end;

function TSQLCaptureLogHandler.DistinctStatementCount(const SubText: string): Integer;
var
  i, p: Integer;
  Line, Statement: string;
  Distinct: TStringList;

  procedure FlushStatement;
  begin
    if (Statement <> '') and (Pos(SubText, Statement) > 0) then
      Distinct.Add(Statement);
    Statement := '';
  end;

begin
  Distinct := TStringList.Create;
  try
    Distinct.Sorted := True;
    Distinct.Duplicates := dupIgnore;
    Statement := '';
    for i := 0 to fLines.Count - 1 do
    begin
      Line := fLines[i];
      p := Pos(':SQL ', Line);
      if p > 0 then
      begin
        FlushStatement;
        // Strip '<timestamp>:SQL <counter>- ' so only the SQL text remains;
        // the counter area holds only digits and spaces, so the first '- '
        // after the marker delimits it.
        Line := Copy(Line, p + Length(':SQL '), MaxInt);
        p := Pos('- ', Line);
        if p > 0 then
          Line := Copy(Line, p + 2, MaxInt);
        Statement := Line;
      end
      else if Copy(TrimLeft(Line), 1, 1) <> '[' then // skip param value dumps
        Statement := Statement + sLineBreak + TrimLeft(Line);
    end;
    FlushStatement;
    result := Distinct.Count;
  finally
    Distinct.Free;
  end;
end;

{ TTestBoldSqlParameterization }

procedure TTestBoldSqlParameterization.SetUp;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
  FCapture := TSQLCaptureLogHandler.Create;
  FSavedHandler := BoldSQLLogHandler;
  BoldSQLLogHandler := FCapture;
end;

procedure TTestBoldSqlParameterization.TearDown;
begin
  BoldSQLLogHandler := FSavedHandler;
  FreeAndNil(FCapture);
  if Assigned(dmUndoRedo) and dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
  end;
  FreeAndNil(dmUndoRedo);
end;

function TTestBoldSqlParameterization.GetSystem: TBoldSystem;
begin
  result := dmUndoRedo.BoldSystemHandle1.System;
end;

procedure TTestBoldSqlParameterization.TestMultilinkFetchUsesParams;
var
  Parent, Child1, Child2: TSomeClass;
  ParentIdString: string;
begin
  Parent := TSomeClass.Create(GetSystem);
  Child1 := TSomeClass.Create(GetSystem);
  Child2 := TSomeClass.Create(GetSystem);
  Child1.parent := Parent;
  Child2.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  ParentIdString := Parent.BoldObjectLocator.BoldObjectID.AsString;

  // Force the child multilink to be refetched from the database through
  // TBoldNonEmbeddedLinkDefaultMapper.PMFetch.
  Parent.child.Invalidate;
  FCapture.Clear;
  Assert.AreEqual(2, Parent.child.Count, 'Both children must be refetched');

  Assert.IsTrue(FCapture.ContainsText('SELECT'),
    'The invalidated multilink must be refetched with a SELECT ' +
    '(scenario precondition - no capture means the spy is not hooked up)');
  Assert.IsTrue(FCapture.ContainsText(':ID1'),
    'Multilink fetch must bind the owning ids as parameters (:ID1). ' +
    'Literal ids make every fetch unique SQL text: ' + FCapture.CapturedText);
  Assert.IsFalse(FCapture.ContainsText('= ' + ParentIdString),
    'Multilink fetch must not inline the owning id as a literal: ' +
    FCapture.CapturedText);
end;

procedure TTestBoldSqlParameterization.TestDeleteUsesParams;
var
  ObjA, ObjB: TSomeClass;
  DeleteStatements: string;
begin
  ObjA := TSomeClass.Create(GetSystem);
  ObjB := TSomeClass.Create(GetSystem);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  ObjA.Delete;
  ObjB.Delete;
  FCapture.Clear;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Other statements in the update flow (object fetches etc.) may already be
  // parameterized, so the assertion must look inside the DELETEs only.
  DeleteStatements := FCapture.StatementsContaining('DELETE FROM');
  Assert.IsTrue(DeleteStatements <> '',
    'Deleting saved objects must execute DELETE statements ' +
    '(scenario precondition - no capture means the spy is not hooked up)');
  Assert.IsTrue(Pos(':ID1', DeleteStatements) > 0,
    'Delete must bind the ids as parameters (:ID1). Literal id lists make ' +
    'every delete unique SQL text: ' + DeleteStatements);
end;

{ TTestBoldIdListWhereFragment }

function TTestBoldIdListWhereFragment.SystemMapper: TBoldSystemDefaultMapper;
begin
  result := (dmUndoRedo.BoldSystemHandle1.System.PersistenceController
    as TBoldPersistenceControllerDefault).PersistenceMapper;
end;

function TTestBoldIdListWhereFragment.Config: TBoldSQLDataBaseConfig;
begin
  result := SystemMapper.SQLDataBaseConfig;
end;

procedure TTestBoldIdListWhereFragment.SetUp;
var
  TopSortedIndex: Integer;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
  TopSortedIndex := dmUndoRedo.BoldSystemHandle1.System.BoldSystemTypeInfo.
    ClassTypeInfoByExpressionName['SomeClass'].TopSortedIndex;
  FMapper := SystemMapper.ObjectPersistenceMappers[TopSortedIndex]
    as TBoldObjectDefaultMapper;
  FQuery := SystemMapper.GetExecQuery;
  FQuery.ParamCheck := true;
  FSavedMaxParams := Config.MaxParamsInIdList;
end;

procedure TTestBoldIdListWhereFragment.TearDown;
begin
  Config.MaxParamsInIdList := FSavedMaxParams;
  SystemMapper.ReleaseExecQuery(FQuery);
  // The pooled wrapper dies with the datamodule below; the interface field
  // must not keep a reference or the next Setup releases freed memory.
  FQuery := nil;
  if Assigned(dmUndoRedo) and dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
  end;
  FreeAndNil(dmUndoRedo);
end;

function TTestBoldIdListWhereFragment.MakeIdList(Count: Integer): TBoldObjectIdList;
var
  i: Integer;
  Id: TBoldDefaultId;
begin
  result := TBoldObjectIdList.Create;
  for i := 1 to Count do
  begin
    Id := TBoldDefaultId.CreateWithClassId(FMapper.TopSortedIndex, True);
    Id.AsInteger := i;
    result.AddAndAdopt(Id);
  end;
end;

function TTestBoldIdListWhereFragment.BuildFragment(IdList: TBoldObjectIdList): string;
begin
  FQuery.ClearParams;
  result := FMapper.IdListSegmentToWhereFragment(IdList, 0, IdList.Count - 1, true, FQuery);
end;

procedure TTestBoldIdListWhereFragment.TestSingleIdNotPadded;
var
  IdList: TBoldObjectIdList;
begin
  Config.MaxParamsInIdList := 500;
  IdList := MakeIdList(1);
  try
    Assert.AreEqual(' = :ID1', BuildFragment(IdList),
      'Single id must keep the equality form without padding');
    Assert.AreEqual(1, FQuery.ParamCount);
  finally
    IdList.Free;
  end;
end;

procedure TTestBoldIdListWhereFragment.TestTwelveIdsPaddedToBucket50;
var
  IdList: TBoldObjectIdList;
  Fragment: string;
begin
  Config.MaxParamsInIdList := 500;
  IdList := MakeIdList(12);
  try
    Fragment := BuildFragment(IdList);
    Assert.AreEqual(50, FQuery.ParamCount,
      'A 12-id list must be padded to the 50 bucket so all lists of ' +
      '11..50 ids produce identical SQL text: ' + Fragment);
    Assert.IsTrue(Pos(':ID50', Fragment) > 0,
      'Fragment must reference the padded params: ' + Fragment);
  finally
    IdList.Free;
  end;
end;

procedure TTestBoldIdListWhereFragment.TestPaddedTailRepeatsLastId;
var
  IdList: TBoldObjectIdList;
begin
  Config.MaxParamsInIdList := 500;
  IdList := MakeIdList(12);
  try
    BuildFragment(IdList);
    Assert.AreEqual(50, FQuery.ParamCount);
    // Params 13..50 must repeat the last real id (12) - duplicates inside
    // an IN list are semantically harmless.
    Assert.AreEqual(12, FQuery.Param[12].AsInteger, 'Param ID13 must repeat the last id');
    Assert.AreEqual(12, FQuery.Param[49].AsInteger, 'Param ID50 must repeat the last id');
    Assert.AreEqual(12, FQuery.Param[11].AsInteger, 'Param ID12 is the last real id');
    Assert.AreEqual(1, FQuery.Param[0].AsInteger, 'Param ID1 must keep its real value');
  finally
    IdList.Free;
  end;
end;

procedure TTestBoldIdListWhereFragment.Test250IdsFillBucketExactly;
var
  IdList: TBoldObjectIdList;
  Fragment: string;
begin
  Config.MaxParamsInIdList := 500;
  IdList := MakeIdList(250);
  try
    Fragment := BuildFragment(IdList);
    Assert.AreEqual(250, FQuery.ParamCount,
      'A 250-id list hits the 250 bucket exactly - no padding');
    Assert.IsTrue(Pos(':ID250', Fragment) > 0, Fragment);
    Assert.IsTrue(Pos('in (:ID1,', Fragment) > 0,
      'Fragment must be fully parameterized: ' + Copy(Fragment, 1, 60));
  finally
    IdList.Free;
  end;
end;

procedure TTestBoldIdListWhereFragment.TestNoPaddingWhenBucketExceedsMaxParams;
var
  IdList: TBoldObjectIdList;
begin
  // Legacy default config: MaxParamsInIdList = 20. 15 ids would pad to the
  // 50 bucket, which exceeds the limit - so the list stays unpadded rather
  // than growing beyond what the engine config allows.
  Config.MaxParamsInIdList := 20;
  IdList := MakeIdList(15);
  try
    BuildFragment(IdList);
    Assert.AreEqual(15, FQuery.ParamCount,
      'Padding must never push the param count beyond MaxParamsInIdList');
  finally
    IdList.Free;
  end;
end;

procedure TTestBoldIdListWhereFragment.TestLegacyLargeListStaysLiteral;
var
  IdList: TBoldObjectIdList;
  Fragment: string;
begin
  Config.MaxParamsInIdList := 20;
  IdList := MakeIdList(250);
  try
    Fragment := BuildFragment(IdList);
    Assert.AreEqual(0, FQuery.ParamCount,
      'A list above MaxParamsInIdList must keep the literal path');
    Assert.IsTrue(Pos(':ID', Fragment) = 0, 'No params expected: ' + Copy(Fragment, 1, 60));
    Assert.IsTrue(Pos('in (1, 2,', Fragment) > 0,
      'Literal ids expected: ' + Copy(Fragment, 1, 60));
  finally
    IdList.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSqlParameterization);
  TDUnitX.RegisterTestFixture(TTestBoldIdListWhereFragment);

end.
