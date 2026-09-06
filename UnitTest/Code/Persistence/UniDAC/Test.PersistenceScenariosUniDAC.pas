unit Test.PersistenceScenariosUniDAC;

{ The adapter-neutral persistence scenarios (BoldPersistenceScenarios) run on
  the UniDAC adapter (DebugUniDAC configuration). Twin:
  Test.PersistenceScenariosFireDAC. The default configurations compile the
  empty stub in ..\UniDACStubs instead. }

interface

uses
  DUnitX.TestFramework,
  BoldTestCaseUniDAC,
  BoldPersistenceScenarios;

type
  [TestFixture]
  [Category('Persistence')]
  TTestPersistenceScenariosUniDAC = class(TBoldTestCaseUniDAC)
  private
    FScenarios: TBoldPersistenceScenarios;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
    [Test]
    [Category('DB')]
    procedure ThreeObjectsKeepDistinctValues;
    [Test]
    [Category('DB')]
    procedure BatchedCreateRoundTrips;
    [Test]
    [Category('DB')]
    procedure BatchedMixedUpdateRoundTrips;
    [Test]
    [Category('DB')]
    procedure BatchReleasesQueryWithParamCheckRestored;
    [Test]
    [Category('DB')]
    procedure FailedBatchLeavesSystemUsable;
  end;

implementation

uses
  System.SysUtils;

procedure TTestPersistenceScenariosUniDAC.SetUp;
begin
  inherited;
  FScenarios := TBoldPersistenceScenarios.Create(SystemHandle, DatabaseAdapter);
end;

procedure TTestPersistenceScenariosUniDAC.TearDown;
begin
  FreeAndNil(FScenarios);
  inherited;
end;

procedure TTestPersistenceScenariosUniDAC.ThreeObjectsKeepDistinctValues;
begin
  FScenarios.ThreeObjectsKeepDistinctValues;
end;

procedure TTestPersistenceScenariosUniDAC.BatchedCreateRoundTrips;
begin
  FScenarios.BatchedCreateRoundTrips;
end;

procedure TTestPersistenceScenariosUniDAC.BatchedMixedUpdateRoundTrips;
begin
  FScenarios.BatchedMixedUpdateRoundTrips;
end;

procedure TTestPersistenceScenariosUniDAC.BatchReleasesQueryWithParamCheckRestored;
begin
  FScenarios.BatchReleasesQueryWithParamCheckRestored;
end;

procedure TTestPersistenceScenariosUniDAC.FailedBatchLeavesSystemUsable;
begin
  FScenarios.FailedBatchLeavesSystemUsable;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceScenariosUniDAC);

end.
