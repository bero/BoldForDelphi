unit Test.PersistenceScenariosZeos;

{ The adapter-neutral persistence scenarios (BoldPersistenceScenarios) run on
  the Zeos adapter under Free Pascal. Twins: Test.PersistenceScenariosFireDAC
  and UniDAC\Test.PersistenceScenariosUniDAC on Delphi. }

interface

uses
  BoldTestFramework,
  BoldTestCaseZeos,
  BoldPersistenceScenarios;

type
  TTestPersistenceScenariosZeos = class(TBoldTestCaseZeos)
  private
    FScenarios: TBoldPersistenceScenarios;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ThreeObjectsKeepDistinctValues;
    procedure BatchedCreateRoundTrips;
    procedure BatchedMixedUpdateRoundTrips;
    procedure BatchReleasesQueryWithParamCheckRestored;
    procedure FailedBatchLeavesSystemUsable;
  end;

implementation

uses
  SysUtils;

procedure TTestPersistenceScenariosZeos.SetUp;
begin
  inherited;
  FScenarios := TBoldPersistenceScenarios.Create(SystemHandle, DatabaseAdapter);
end;

procedure TTestPersistenceScenariosZeos.TearDown;
begin
  FreeAndNil(FScenarios);
  inherited;
end;

procedure TTestPersistenceScenariosZeos.ThreeObjectsKeepDistinctValues;
begin
  FScenarios.ThreeObjectsKeepDistinctValues;
end;

procedure TTestPersistenceScenariosZeos.BatchedCreateRoundTrips;
begin
  FScenarios.BatchedCreateRoundTrips;
end;

procedure TTestPersistenceScenariosZeos.BatchedMixedUpdateRoundTrips;
begin
  FScenarios.BatchedMixedUpdateRoundTrips;
end;

procedure TTestPersistenceScenariosZeos.BatchReleasesQueryWithParamCheckRestored;
begin
  FScenarios.BatchReleasesQueryWithParamCheckRestored;
end;

procedure TTestPersistenceScenariosZeos.FailedBatchLeavesSystemUsable;
begin
  FScenarios.FailedBatchLeavesSystemUsable;
end;

initialization
  BoldRegisterTest(TTestPersistenceScenariosZeos);

end.
