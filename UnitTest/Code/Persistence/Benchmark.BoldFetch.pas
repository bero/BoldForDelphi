unit Benchmark.BoldFetch;

{******************************************************************************}
{                                                                              }
{  Benchmark.BoldFetch - Fetch/materialization performance benchmarks          }
{                                                                              }
{  Measures the cost of converting SQL result rows into TBoldObjects in        }
{  memory (the ValueFromField / value-space / proxy path), isolated from       }
{  real database I/O by using the SQLite in-memory engine (UnitTest.ini).      }
{                                                                              }
{  Each benchmark: seeds N objects of a wide class (20 attributes),            }
{  persists them, restarts the object space, then times:                       }
{    Phase 1  class-extent fetch (PMFetchClassWithCondition -> per-row         }
{             ValuesFromFieldsByMemberList -> ValueFromField per member)       }
{    Phase 2  touching all members in memory (lazy member instantiation,       }
{             no database access)                                              }
{                                                                              }
{  Timings are written to stdout with a [BENCH] prefix. The assertions keep    }
{  these runnable as ordinary tests; the numbers are the actual deliverable.   }
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
  TBoldFetchBenchmark = class(TBoldTestCaseFireDAC)
  private
    function ResolveMemberIndices(AClassTypeInfo: TBoldClassTypeInfo): TArray<integer>;
    procedure SeedObjects(ACount: integer; const AClassName: string);
    procedure RunFetchBenchmark(ABaseCount, ASubCount: integer);
  protected
    procedure ConfigureModel; override;
  public
    [Test]
    procedure Fetch_10k;
    [Test]
    procedure Fetch_10k_WithSubclasses;
    [Test]
    procedure Fetch_100k;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  BoldAttributes;

const
  cStringAttrCount = 8;
  cIntAttrCount = 6;
  cFloatAttrCount = 2;
  cDateAttrCount = 2;
  cBoolAttrCount = 2;
  cAttrCount = cStringAttrCount + cIntAttrCount + cFloatAttrCount +
    cDateAttrCount + cBoolAttrCount; // 20 members per object

{ TBoldFetchBenchmark }

procedure TBoldFetchBenchmark.ConfigureModel;
var
  vModel: TStringList;

  procedure AddAttributeLines(const AName, AType: string);
  begin
    vModel.Add(#9#9#9#9'(Attribute');
    vModel.Add(#9#9#9#9#9'"' + AName + '"');
    vModel.Add(#9#9#9#9#9'"' + AType + '"');
    vModel.Add(#9#9#9#9#9'FALSE');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'2');
    vModel.Add(#9#9#9#9#9'""');
    vModel.Add(#9#9#9#9#9'"persistence=Persistent"');
    vModel.Add(#9#9#9#9')');
  end;

  procedure AddBenchAttributes;
  var
    i: integer;
  begin
    for i := 1 to cStringAttrCount do
      AddAttributeLines('Str' + IntToStr(i), 'String');
    for i := 1 to cIntAttrCount do
      AddAttributeLines('Int' + IntToStr(i), 'Integer');
    for i := 1 to cFloatAttrCount do
      AddAttributeLines('Flt' + IntToStr(i), 'Float');
    for i := 1 to cDateAttrCount do
      AddAttributeLines('Dat' + IntToStr(i), 'DateTime');
    for i := 1 to cBoolAttrCount do
      AddAttributeLines('Bool' + IntToStr(i), 'Boolean');
  end;

  procedure BeginClass(const AName, ASuperClass, ATaggedValues: string);
  begin
    vModel.Add(#9#9'(Class');
    vModel.Add(#9#9#9'"' + AName + '"');
    vModel.Add(#9#9#9'"' + ASuperClass + '"');
    vModel.Add(#9#9#9'TRUE');
    vModel.Add(#9#9#9'FALSE');
    vModel.Add(#9#9#9'""');
    vModel.Add(#9#9#9'""');
    vModel.Add(#9#9#9'"' + ATaggedValues + '"');
    vModel.Add(#9#9#9'(Attributes');
  end;

  procedure EndClass;
  begin
    vModel.Add(#9#9#9')');
    vModel.Add(#9#9#9'(Methods');
    vModel.Add(#9#9#9')');
    vModel.Add(#9#9')');
  end;

begin
  vModel := TStringList.Create;
  try
    vModel.Add('VERSION 19');
    vModel.Add('(Model');
    vModel.Add(#9'"BenchModel"');
    vModel.Add(#9'"BusinessClassesRoot"');
    vModel.Add(#9'""');
    vModel.Add(#9'""');
    vModel.Add(#9'"_Boldify.boldified=True,Bold.DelphiName=<Name>,Bold.UnitName=BenchModel,Bold.RootClass=BusinessClassesRoot"');
    vModel.Add(#9'(Classes');

    BeginClass('BusinessClassesRoot', '<NONE>',
      '_Boldify.autoCreated=True,persistence=persistent,Bold.TableName=<Prefix>_OBJECT');
    EndClass;

    BeginClass('BenchClass', 'BusinessClassesRoot', 'persistence=persistent');
    AddBenchAttributes;
    EndClass;

    BeginClass('BenchSub', 'BenchClass', 'persistence=persistent');
    AddAttributeLines('SubStr1', 'String');
    AddAttributeLines('SubInt1', 'Integer');
    EndClass;

    vModel.Add(#9')');
    vModel.Add(#9'(Associations');
    vModel.Add(#9')');
    vModel.Add(')');

    Model.SetFromModelAsString(vModel);
  finally
    vModel.Free;
  end;
end;

function TBoldFetchBenchmark.ResolveMemberIndices(
  AClassTypeInfo: TBoldClassTypeInfo): TArray<integer>;
var
  i, vPos: integer;

  procedure Put(const AName: string);
  begin
    Result[vPos] := AClassTypeInfo.MemberIndexByExpressionName[AName];
    Assert.IsTrue(Result[vPos] <> -1, 'Member not found: ' + AName);
    Inc(vPos);
  end;

begin
  SetLength(Result, cAttrCount);
  vPos := 0;
  for i := 1 to cStringAttrCount do
    Put('Str' + IntToStr(i));
  for i := 1 to cIntAttrCount do
    Put('Int' + IntToStr(i));
  for i := 1 to cFloatAttrCount do
    Put('Flt' + IntToStr(i));
  for i := 1 to cDateAttrCount do
    Put('Dat' + IntToStr(i));
  for i := 1 to cBoolAttrCount do
    Put('Bool' + IntToStr(i));
end;

procedure TBoldFetchBenchmark.SeedObjects(ACount: integer;
  const AClassName: string);
var
  vClassTypeInfo: TBoldClassTypeInfo;
  vIndices: TArray<integer>;
  vObject: TBoldObject;
  vBaseDate: TDateTime;
  i, a, vMemberPos: integer;
begin
  vClassTypeInfo := GetClassTypeInfo(AClassName);
  Assert.IsNotNull(vClassTypeInfo, 'Class not found: ' + AClassName);
  vIndices := ResolveMemberIndices(vClassTypeInfo);
  vBaseDate := EncodeDate(2026, 1, 1);
  for i := 1 to ACount do
  begin
    vObject := System.CreateNewObjectFromClassTypeInfo(vClassTypeInfo, True);
    vMemberPos := 0;
    for a := 1 to cStringAttrCount do
    begin
      (vObject.BoldMembers[vIndices[vMemberPos]] as TBAString).AsString :=
        'V' + IntToStr(i) + '_' + IntToStr(a);
      Inc(vMemberPos);
    end;
    for a := 1 to cIntAttrCount do
    begin
      (vObject.BoldMembers[vIndices[vMemberPos]] as TBAInteger).AsInteger :=
        i * 100 + a;
      Inc(vMemberPos);
    end;
    for a := 1 to cFloatAttrCount do
    begin
      (vObject.BoldMembers[vIndices[vMemberPos]] as TBAFloat).AsFloat :=
        i + a / 10;
      Inc(vMemberPos);
    end;
    for a := 1 to cDateAttrCount do
    begin
      (vObject.BoldMembers[vIndices[vMemberPos]] as TBADateTime).AsDateTime :=
        vBaseDate + (i mod 365) + a / 24;
      Inc(vMemberPos);
    end;
    for a := 1 to cBoolAttrCount do
    begin
      (vObject.BoldMembers[vIndices[vMemberPos]] as TBABoolean).AsBoolean :=
        Odd(i + a);
      Inc(vMemberPos);
    end;
  end;
end;

procedure TBoldFetchBenchmark.RunFetchBenchmark(ABaseCount, ASubCount: integer);
var
  vWatch: TStopwatch;
  vSeedMs, vSaveMs, vIdMs, vFetchMs, vTouchMs: Int64;
  vTotal, vFetchedCount, i, m, vNullCount: integer;
  vList: TBoldObjectList;
  vObject: TBoldObject;
  vIndices: TArray<integer>;
  vClassTypeInfo: TBoldClassTypeInfo;
begin
  vTotal := ABaseCount + ASubCount;

  // --- Seed and persist (not the measured path, but reported for context) ---
  vWatch := TStopwatch.StartNew;
  SeedObjects(ABaseCount, 'BenchClass');
  if ASubCount > 0 then
    SeedObjects(ASubCount, 'BenchSub');
  vSeedMs := vWatch.ElapsedMilliseconds;

  vWatch := TStopwatch.StartNew;
  UpdateDatabase;
  vSaveMs := vWatch.ElapsedMilliseconds;

  // --- Fresh object space; the in-memory database survives because the ---
  // --- connection stays open until TearDown                            ---
  RefreshSystem;

  // --- Phase 1: fetch the class extent (object ids only) ---
  vWatch := TStopwatch.StartNew;
  vList := System.ClassByExpressionName['BenchClass'];
  vFetchedCount := vList.Count;
  vIdMs := vWatch.ElapsedMilliseconds;
  Assert.AreEqual(vTotal, vFetchedCount, 'Extent count after fetch');

  // --- Phase 2: bulk-fetch all objects with their default members      ---
  // --- (SpanFetch-style block SELECTs -> ValuesFromFieldsByMemberList  ---
  // --- per row). This is the materialization path under measurement.   ---
  vWatch := TStopwatch.StartNew;
  System.FetchMembersWithObjects(vList, TBoldMemberIdList(nil));
  vFetchMs := vWatch.ElapsedMilliseconds;

  // --- Phase 3: touch every member in memory (no database access) ---
  vClassTypeInfo := GetClassTypeInfo('BenchClass');
  vIndices := ResolveMemberIndices(vClassTypeInfo);
  vNullCount := 0;
  vWatch := TStopwatch.StartNew;
  for i := 0 to vList.Count - 1 do
  begin
    vObject := vList[i];
    for m := 0 to High(vIndices) do
      if (vObject.BoldMembers[vIndices[m]] as TBoldAttribute).IsNull then
        Inc(vNullCount);
  end;
  vTouchMs := vWatch.ElapsedMilliseconds;

  Assert.AreEqual(0, vNullCount, 'No fetched attribute should be null');
  Assert.AreEqual('V',
    Copy((vList[0].BoldMembers[vIndices[0]] as TBAString).AsString, 1, 1),
    'Spot check of fetched value');

  WriteLn(Format(
    '[BENCH] %d objects (%d base + %d sub), %d attributes each',
    [vTotal, ABaseCount, ASubCount, cAttrCount]));
  WriteLn(Format(
    '[BENCH]   seed (in-memory create+assign): %d ms', [vSeedMs]));
  WriteLn(Format(
    '[BENCH]   UpdateDatabase (write path):    %d ms', [vSaveMs]));
  WriteLn(Format(
    '[BENCH]   fetch extent (ids only):        %d ms', [vIdMs]));
  WriteLn(Format(
    '[BENCH]   BULK FETCH objects w/members:   %d ms  (%.2f us/object, %.2f us/value)',
    [vFetchMs, vFetchMs * 1000 / vTotal, vFetchMs * 1000 / (vTotal * cAttrCount)]));
  WriteLn(Format(
    '[BENCH]   touch all members in memory:    %d ms  (%.2f us/object)',
    [vTouchMs, vTouchMs * 1000 / vTotal]));
end;

procedure TBoldFetchBenchmark.Fetch_10k;
begin
  RunFetchBenchmark(10000, 0);
end;

procedure TBoldFetchBenchmark.Fetch_10k_WithSubclasses;
begin
  RunFetchBenchmark(5000, 5000);
end;

procedure TBoldFetchBenchmark.Fetch_100k;
begin
  // Takes ~40 s (dominated by seeding the database), so it only runs on
  // explicit request. Enable with:  set BOLD_BENCH_FULL=1
  if GetEnvironmentVariable('BOLD_BENCH_FULL') <> '1' then
  begin
    WriteLn('[BENCH] Fetch_100k skipped (set BOLD_BENCH_FULL=1 to run)');
    Exit;
  end;
  RunFetchBenchmark(100000, 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TBoldFetchBenchmark);

end.
