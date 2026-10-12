unit Test.BoldDbEvolutor;

{ The database evolutor scenarios (BoldDbEvolutorScenarios) on the FireDAC
  adapter and the configured test engine. Twin:
  ..\Persistence\UniDAC\Test.BoldDbEvolutorUniDAC. }

interface

uses
  DUnitX.TestFramework,
  BoldTestCaseFireDAC,
  BoldDbEvolutorScenarios;

type
  [TestFixture]
  [Category('PMapper')]
  TTestBoldDbEvolutor = class(TBoldTestCaseFireDAC)
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

procedure TTestBoldDbEvolutor.SetUp;
begin
  inherited;
  FScenarios := TBoldDbEvolutorScenarios.Create(SystemHandle, DatabaseAdapter);
end;

procedure TTestBoldDbEvolutor.TearDown;
begin
  FreeAndNil(FScenarios);
  inherited;
end;

procedure TTestBoldDbEvolutor.TestEvolveOfUpToDateDatabasePlansNoIndexes;
begin
  FScenarios.EvolveOfUpToDateDatabasePlansNoIndexes;
end;

procedure TTestBoldDbEvolutor.TestIndexDescriptionListsCompositeColumnsInIndexOrder;
begin
  FScenarios.IndexDescriptionListsCompositeColumnsInIndexOrder;
end;

procedure TTestBoldDbEvolutor.TestIndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
begin
  FScenarios.IndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
end;

procedure TTestBoldDbEvolutor.TestIndexDescriptionsListAnIndexedPrimaryKeyOnce;
begin
  FScenarios.IndexDescriptionsListAnIndexedPrimaryKeyOnce;
end;

procedure TTestBoldDbEvolutor.TestIndexDescriptionsIgnoreSameNamedTableInOtherSchema;
begin
  FScenarios.IndexDescriptionsIgnoreSameNamedTableInOtherSchema;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbEvolutor);

end.
