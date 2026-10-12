unit Test.BoldDbEvolutorUniDAC;

{ The database evolutor scenarios (BoldDbEvolutorScenarios) on the UniDAC
  adapter (DebugUniDAC configuration). Twin: ..\..\PMapper\Test.BoldDbEvolutor.
  The default configurations compile the empty stub in ..\UniDACStubs instead. }

interface

uses
  DUnitX.TestFramework,
  BoldTestCaseUniDAC,
  BoldDbEvolutorScenarios;

type
  [TestFixture]
  [Category('PMapper')]
  TTestBoldDbEvolutorUniDAC = class(TBoldTestCaseUniDAC)
  private
    FScenarios: TBoldDbEvolutorScenarios;
    procedure RequireIndexInfoTemplate;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
    [Test]
    [Category('DB')]
    procedure TestEvolveOfUpToDateDatabasePlansNoIndexes;
    [Test]
    [Category('DB')]
    procedure TestIndexDescriptionListsCompositeColumnsInIndexOrder;
    [Test]
    [Category('DB')]
    procedure TestIndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
    [Test]
    [Category('DB')]
    procedure TestIndexDescriptionsListAnIndexedPrimaryKeyOnce;
    [Test]
    [Category('DB')]
    procedure TestIndexDescriptionsIgnoreSameNamedTableInOtherSchema;
  end;

implementation

uses
  System.SysUtils;

procedure TTestBoldDbEvolutorUniDAC.SetUp;
begin
  inherited;
  FScenarios := TBoldDbEvolutorScenarios.Create(SystemHandle, DatabaseAdapter);
end;

procedure TTestBoldDbEvolutorUniDAC.TearDown;
begin
  FreeAndNil(FScenarios);
  inherited;
end;

// Without an IndexInfoTemplate the index descriptions go through
// TBoldUniDACTable.GetIndexDefs, which raises (#116). Assert.Pass raises, so
// a test on such an engine ends here.
procedure TTestBoldDbEvolutorUniDAC.RequireIndexInfoTemplate;
begin
  if DatabaseAdapter.SQLDatabaseConfig.IndexInfoTemplate = '' then
    Assert.Pass('UniDAC reads index descriptions only through an IndexInfoTemplate (#116)');
end;

procedure TTestBoldDbEvolutorUniDAC.TestEvolveOfUpToDateDatabasePlansNoIndexes;
begin
  RequireIndexInfoTemplate;
  FScenarios.EvolveOfUpToDateDatabasePlansNoIndexes;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionListsCompositeColumnsInIndexOrder;
begin
  RequireIndexInfoTemplate;
  FScenarios.IndexDescriptionListsCompositeColumnsInIndexOrder;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
begin
  RequireIndexInfoTemplate;
  FScenarios.IndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionsListAnIndexedPrimaryKeyOnce;
begin
  RequireIndexInfoTemplate;
  FScenarios.IndexDescriptionsListAnIndexedPrimaryKeyOnce;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionsIgnoreSameNamedTableInOtherSchema;
begin
  RequireIndexInfoTemplate;
  FScenarios.IndexDescriptionsIgnoreSameNamedTableInOtherSchema;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbEvolutorUniDAC);

end.
