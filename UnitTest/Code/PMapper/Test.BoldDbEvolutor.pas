unit Test.BoldDbEvolutor;

{ The database evolutor scenarios (BoldDbEvolutorScenarios) on the runner's
  database (TBoldTestCaseDB): FireDAC and the configured test engine under
  Delphi, Zeos on in-memory SQLite under Free Pascal. Twin:
  ..\Persistence\UniDAC\Test.BoldDbEvolutorUniDAC. }

interface

uses
  BoldTestFramework,
  BoldTestCaseDB,
  BoldDbEvolutorScenarios;

type
  {$IFNDEF FPC}
  [Category('PMapper')]
  {$ENDIF}
  TTestBoldDbEvolutor = class(TBoldTestCaseDB)
  private
    FScenarios: TBoldDbEvolutorScenarios;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFNDEF FPC}
    [Category('DB')]
    {$ENDIF}
    procedure TestEvolveOfUpToDateDatabasePlansNoIndexes;
    {$IFNDEF FPC}
    [Category('DB')]
    {$ENDIF}
    procedure TestIndexDescriptionListsCompositeColumnsInIndexOrder;
    {$IFNDEF FPC}
    [Category('DB')]
    {$ENDIF}
    procedure TestIndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
    {$IFNDEF FPC}
    [Category('DB')]
    {$ENDIF}
    procedure TestIndexDescriptionsListAnIndexedPrimaryKeyOnce;
    {$IFNDEF FPC}
    [Category('DB')]
    {$ENDIF}
    procedure TestIndexDescriptionsIgnoreSameNamedTableInOtherSchema;
  end;

implementation

uses
  SysUtils;

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
  BoldRegisterTest(TTestBoldDbEvolutor);

end.
