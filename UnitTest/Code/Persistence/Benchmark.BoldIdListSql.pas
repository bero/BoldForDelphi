unit Benchmark.BoldIdListSql;

{******************************************************************************}
{                                                                              }
{  Benchmark.BoldIdListSql - id-list SQL parameterization benchmarks           }
{                                                                              }
{  Measurement companion to the IN-list parameterization and bucket padding    }
{  work: MaxParamsInIdList=500 for SQL Server, AllowParms on multilink fetch   }
{  and delete, and padding of parameterized id lists to fixed bucket sizes.    }
{                                                                              }
{  Two angles, following the [BENCH] pattern of Benchmark.BoldFetch (timings   }
{  reported to stdout, counts asserted - never wall-clock):                    }
{                                                                              }
{  1. On the suite's default engine (SQLite in-memory): fetches with id lists  }
{     of varying sizes, reporting per-size wall time (proves the padding       }
{     overhead is negligible) and asserting the DISTINCT generated statement   }
{     count collapses to one text per bucket - the deterministic proxy for     }
{     what a server-side plan cache keys on.                                   }
{                                                                              }
{  2. On SQL Server only (opt-in via UnitTest.ini Engine=SQLServer): clears    }
{     the database-scoped plan cache, runs hundreds of fetches of varying      }
{     sizes and offsets, then counts the ad-hoc plans actually cached via      }
{     sys.dm_exec_cached_plans, softly asserting the count stays bounded.      }
{                                                                              }
{******************************************************************************}

interface

uses
  DUnitX.TestFramework,
  BoldSystem,
  BoldSystemRT,
  BoldId,
  BoldTestCaseFireDAC;

type
  [TestFixture]
  [Category('Benchmark')]
  TBoldIdListSqlBenchmark = class(TBoldTestCaseFireDAC)
  private
    procedure RaiseMaxParamsInIdList;
    procedure SeedObjects(ACount: integer);
    function FetchExtent: TBoldObjectList;
    function FetchRange(AList: TBoldObjectList; AOffset, ACount: integer): Int64;
  protected
    procedure ConfigureModel; override;
  public
    [Test]
    procedure IdListFetch_DistinctStatementsPerBucket;
    [Test]
    procedure SqlServer_PlanCacheStaysBounded;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  FireDAC.Comp.Client,
  BoldAttributes,
  BoldDBInterfaces,
  BoldLogHandler,
  BoldPersistenceControllerDefault,
  BoldTestDatabaseConfig,
  Test.BoldSqlParameterization;

const
  cSeedCount = 300;
  cFetchSizes: array[0..9] of integer = (1, 2, 5, 10, 12, 25, 50, 100, 150, 250);
  // '= :ID1' plus one text per bucket reached: 10, 50, 100 and 250.
  cExpectedDistinctFetchTexts = 5;

{ TBoldIdListSqlBenchmark }

procedure TBoldIdListSqlBenchmark.ConfigureModel;
var
  vModel: TStringList;
begin
  vModel := TStringList.Create;
  try
    vModel.Add('VERSION 19');
    vModel.Add('(Model');
    vModel.Add(#9'"BenchIdModel"');
    vModel.Add(#9'"BusinessClassesRoot"');
    vModel.Add(#9'""');
    vModel.Add(#9'""');
    vModel.Add(#9'"_Boldify.boldified=True,Bold.DelphiName=<Name>,Bold.UnitName=BenchIdModel,Bold.RootClass=BusinessClassesRoot"');
    vModel.Add(#9'(Classes');
    vModel.Add(#9#9'(Class');
    vModel.Add(#9#9#9'"BusinessClassesRoot"');
    vModel.Add(#9#9#9'"<NONE>"');
    vModel.Add(#9#9#9'TRUE');
    vModel.Add(#9#9#9'FALSE');
    vModel.Add(#9#9#9'""');
    vModel.Add(#9#9#9'""');
    vModel.Add(#9#9#9'"_Boldify.autoCreated=True,persistence=persistent,Bold.TableName=<Prefix>_OBJECT"');
    vModel.Add(#9#9#9'(Attributes');
    vModel.Add(#9#9#9')');
    vModel.Add(#9#9#9'(Methods');
    vModel.Add(#9#9#9')');
    vModel.Add(#9#9')');
    vModel.Add(#9#9'(Class');
    vModel.Add(#9#9#9'"BenchIdClass"');
    vModel.Add(#9#9#9'"BusinessClassesRoot"');
    vModel.Add(#9#9#9'TRUE');
    vModel.Add(#9#9#9'FALSE');
    vModel.Add(#9#9#9'""');
    vModel.Add(#9#9#9'""');
    vModel.Add(#9#9#9'"persistence=persistent"');
    vModel.Add(#9#9#9'(Attributes');
    vModel.Add(#9#9#9#9'(Attribute');
    vModel.Add(#9#9#9#9#9'"BenchName"');
    vModel.Add(#9#9#9#9#9'"String"');
    vModel.Add(#9#9#9#9#9'FALSE');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'2');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'"persistence=Persistent"');
    vModel.Add(#9#9#9#9')');
    vModel.Add(#9#9#9#9'(Attribute');
    vModel.Add(#9#9#9#9#9'"BenchValue"');
    vModel.Add(#9#9#9#9#9'"Integer"');
    vModel.Add(#9#9#9#9#9'FALSE');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'2');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'"persistence=Persistent"');
    vModel.Add(#9#9#9#9')');
    vModel.Add(#9#9#9')');
    vModel.Add(#9#9#9'(Methods');
    vModel.Add(#9#9#9')');
    vModel.Add(#9#9')');
    vModel.Add(#9')');
    vModel.Add(#9'(Associations');
    vModel.Add(#9')');
    vModel.Add(')');
    Model.SetFromModelAsString(vModel);
  finally
    vModel.Free;
  end;
end;

procedure TBoldIdListSqlBenchmark.RaiseMaxParamsInIdList;
begin
  // The suite default engine config caps parameterized id lists at 20; raise
  // it to the SQL Server production value so full-size lists parameterize.
  // Must be re-applied after every RefreshSystem (fresh persistence mapper).
  (System.PersistenceController as TBoldPersistenceControllerDefault).
    PersistenceMapper.SQLDataBaseConfig.MaxParamsInIdList := 500;
end;

procedure TBoldIdListSqlBenchmark.SeedObjects(ACount: integer);
var
  vClassTypeInfo: TBoldClassTypeInfo;
  vObject: TBoldObject;
  vNameIndex, vValueIndex: integer;
  i: integer;
begin
  vClassTypeInfo := GetClassTypeInfo('BenchIdClass');
  Assert.IsNotNull(vClassTypeInfo, 'BenchIdClass not found');
  vNameIndex := vClassTypeInfo.MemberIndexByExpressionName['BenchName'];
  vValueIndex := vClassTypeInfo.MemberIndexByExpressionName['BenchValue'];
  for i := 1 to ACount do
  begin
    vObject := System.CreateNewObjectFromClassTypeInfo(vClassTypeInfo, True);
    (vObject.BoldMembers[vNameIndex] as TBAString).AsString := 'N' + IntToStr(i);
    (vObject.BoldMembers[vValueIndex] as TBAInteger).AsInteger := i;
  end;
end;

function TBoldIdListSqlBenchmark.FetchExtent: TBoldObjectList;
begin
  result := System.ClassByExpressionName['BenchIdClass'];
  Assert.AreEqual(cSeedCount, result.Count, 'Extent count');
end;

function TBoldIdListSqlBenchmark.FetchRange(AList: TBoldObjectList;
  AOffset, ACount: integer): Int64;
var
  vSub: TBoldObjectList;
  vWatch: TStopwatch;
  i: integer;
begin
  // Locator-only sublist: nothing is materialized until the bulk fetch, so
  // the timed call issues exactly one id-list fetch of ACount ids.
  vSub := TBoldObjectList.Create;
  try
    for i := AOffset to AOffset + ACount - 1 do
      vSub.AddLocator(AList.Locators[i]);
    vWatch := TStopwatch.StartNew;
    System.FetchMembersWithObjects(vSub, TBoldMemberIdList(nil));
    result := vWatch.ElapsedMilliseconds;
  finally
    vSub.Free;
  end;
end;

procedure TBoldIdListSqlBenchmark.IdListFetch_DistinctStatementsPerBucket;
var
  vCapture: TSQLCaptureLogHandler;
  vSavedHandler: TBoldLogHandler;
  vList: TBoldObjectList;
  vMs, vTotalMs: Int64;
  vDistinct, i: integer;
begin
  SeedObjects(cSeedCount);
  UpdateDatabase;

  vCapture := TSQLCaptureLogHandler.Create;
  vSavedHandler := BoldSQLLogHandler;
  BoldSQLLogHandler := vCapture;
  try
    vTotalMs := 0;
    WriteLn(Format('[BENCH] id-list fetch, %d seeded objects, engine=%s',
      [cSeedCount, GetTestDatabaseEngine]));
    for i := Low(cFetchSizes) to High(cFetchSizes) do
    begin
      // Fresh object space per size so every fetch really hits the database
      // with the full id list instead of skipping already-loaded objects.
      RefreshSystem;
      RaiseMaxParamsInIdList;
      vList := FetchExtent;
      vMs := FetchRange(vList, 0, cFetchSizes[i]);
      Inc(vTotalMs, vMs);
      WriteLn(Format('[BENCH]   fetch %3d ids (padded to bucket): %d ms',
        [cFetchSizes[i], vMs]));
    end;
    vDistinct := vCapture.DistinctStatementCount(':ID1');
    WriteLn(Format(
      '[BENCH]   total fetch time: %d ms, distinct parameterized fetch texts: %d',
      [vTotalMs, vDistinct]));
    Assert.AreEqual(cExpectedDistinctFetchTexts, vDistinct,
      'Id lists of sizes 1..250 must collapse to one statement text per ' +
      'padding bucket - this count is what a plan cache keys on');
  finally
    BoldSQLLogHandler := vSavedHandler;
    vCapture.Free;
  end;
end;

procedure TBoldIdListSqlBenchmark.SqlServer_PlanCacheStaysBounded;
const
  cRounds = 30; // 30 rounds x 10 sizes = 300 fetches
  cPlanCountBound = 20;
var
  vQuery: TFDQuery;
  vList: TBoldObjectList;
  vWatch: TStopwatch;
  vElapsedMs: Int64;
  vPlanCount, vRound, i, vOffset: integer;
begin
  if not SameText(GetTestDatabaseEngine, 'SQLServer') then
  begin
    WriteLn('[BENCH] SqlServer_PlanCacheStaysBounded skipped ' +
      '(UnitTest.ini Engine is not SQLServer)');
    Exit;
  end;

  SeedObjects(cSeedCount);
  UpdateDatabase;

  // Dev-server only: drop this database's plan cache so the count below
  // reflects exactly what this benchmark caused to be compiled.
  FDConnection.ExecSQL('ALTER DATABASE SCOPED CONFIGURATION CLEAR PROCEDURE_CACHE');

  vWatch := TStopwatch.StartNew;
  for vRound := 1 to cRounds do
    for i := Low(cFetchSizes) to High(cFetchSizes) do
    begin
      RefreshSystem;
      RaiseMaxParamsInIdList;
      vList := FetchExtent;
      // Vary the offset so successive fetches carry different id values;
      // plan reuse must come from identical TEXT, not identical values.
      vOffset := (vRound * 7) mod (cSeedCount - cFetchSizes[i] + 1);
      FetchRange(vList, vOffset, cFetchSizes[i]);
    end;
  vElapsedMs := vWatch.ElapsedMilliseconds;

  vQuery := TFDQuery.Create(nil);
  try
    vQuery.Connection := FDConnection;
    vQuery.SQL.Text :=
      'SELECT COUNT(*) FROM sys.dm_exec_cached_plans cp ' +
      'CROSS APPLY sys.dm_exec_sql_text(cp.plan_handle) st ' +
      'WHERE st.text LIKE ''%BenchIdClass%'' AND cp.objtype = ''Adhoc''';
    vQuery.Open;
    vPlanCount := vQuery.Fields[0].AsInteger;
  finally
    vQuery.Free;
  end;

  WriteLn(Format(
    '[BENCH] SQL Server: %d fetches of 10 sizes in %d ms, ad-hoc plans cached for BenchIdClass: %d',
    [cRounds * Length(cFetchSizes), vElapsedMs, vPlanCount]));
  Assert.IsTrue(vPlanCount < cPlanCountBound, Format(
    'Plan cache must stay bounded (< %d) when id lists are parameterized ' +
    'and padded; got %d single-use plans', [cPlanCountBound, vPlanCount]));
end;

initialization
  TDUnitX.RegisterTestFixture(TBoldIdListSqlBenchmark);

end.
