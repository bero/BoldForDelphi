unit Test.PersistenceScenariosFireDAC;

{ The adapter-neutral persistence scenarios (BoldPersistenceScenarios) run on
  the FireDAC adapter. Twin: UniDAC\Test.PersistenceScenariosUniDAC. }

interface

uses
  DUnitX.TestFramework,
  BoldTestCaseFireDAC,
  BoldPersistenceScenarios;

type
  [TestFixture]
  [Category('Persistence')]
  TTestPersistenceScenariosFireDAC = class(TBoldTestCaseFireDAC)
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

procedure TTestPersistenceScenariosFireDAC.SetUp;
begin
  inherited;
  FScenarios := TBoldPersistenceScenarios.Create(SystemHandle, DatabaseAdapter);
end;

procedure TTestPersistenceScenariosFireDAC.TearDown;
begin
  FreeAndNil(FScenarios);
  inherited;
end;

procedure TTestPersistenceScenariosFireDAC.ThreeObjectsKeepDistinctValues;
begin
  FScenarios.ThreeObjectsKeepDistinctValues;
end;

procedure TTestPersistenceScenariosFireDAC.BatchedCreateRoundTrips;
begin
  FScenarios.BatchedCreateRoundTrips;
end;

procedure TTestPersistenceScenariosFireDAC.BatchedMixedUpdateRoundTrips;
begin
  FScenarios.BatchedMixedUpdateRoundTrips;
end;

procedure TTestPersistenceScenariosFireDAC.BatchReleasesQueryWithParamCheckRestored;
begin
  FScenarios.BatchReleasesQueryWithParamCheckRestored;
end;

procedure TTestPersistenceScenariosFireDAC.FailedBatchLeavesSystemUsable;
begin
  FScenarios.FailedBatchLeavesSystemUsable;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceScenariosFireDAC);

end.
