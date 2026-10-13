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

procedure TTestBoldDbEvolutorUniDAC.TestEvolveOfUpToDateDatabasePlansNoIndexes;
begin
  FScenarios.EvolveOfUpToDateDatabasePlansNoIndexes;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionListsCompositeColumnsInIndexOrder;
begin
  FScenarios.IndexDescriptionListsCompositeColumnsInIndexOrder;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
begin
  FScenarios.IndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionsListAnIndexedPrimaryKeyOnce;
begin
  FScenarios.IndexDescriptionsListAnIndexedPrimaryKeyOnce;
end;

procedure TTestBoldDbEvolutorUniDAC.TestIndexDescriptionsIgnoreSameNamedTableInOtherSchema;
begin
  FScenarios.IndexDescriptionsIgnoreSameNamedTableInOtherSchema;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbEvolutorUniDAC);

end.
