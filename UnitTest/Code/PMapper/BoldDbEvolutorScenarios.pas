unit BoldDbEvolutorScenarios;

{ The database evolutor scenarios, written once against the code-built test
  system of TBoldTestCasePersistence so that the same bodies run on every
  adapter the build has: TTestBoldDbEvolutor in the default configuration,
  TTestBoldDbEvolutorUniDAC under DebugUniDAC. The fixtures are thin - one
  [Test] per scenario that forwards here.

  The evolutor reads the indexes that exist in the database through the
  engine's IndexInfoTemplate and plans a CREATE INDEX for every model index it
  cannot find there. On a database that was just created from the model it
  must plan none - otherwise an evolve fails on, or duplicates, indexes that
  already exist. }

interface

uses
  BoldSystemHandle,
  BoldDBInterfaces,
  BoldAbstractDatabaseAdapter;

type
  TBoldDbEvolutorScenarios = class
  private
    FSystemHandle: TBoldSystemHandle;
    FAdapter: TBoldAbstractDatabaseAdapter;
    function Database: IBoldDatabase;
    procedure ExecSQL(const ASQL: string);
    function IndexedColumnsOf(const aTableName, aIndexName: string): string;
  public
    constructor Create(ASystemHandle: TBoldSystemHandle; AAdapter: TBoldAbstractDatabaseAdapter);
    procedure EvolveOfUpToDateDatabasePlansNoIndexes;
    procedure IndexDescriptionListsCompositeColumnsInIndexOrder;
    procedure IndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
    procedure IndexDescriptionsListAnIndexedPrimaryKeyOnce;
    procedure IndexDescriptionsIgnoreSameNamedTableInOtherSchema;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  DUnitX.TestFramework,
  BoldAbstractPersistenceHandleDB,
  BoldSQLDatabaseConfig,
  BoldPersistenceControllerDefault,
  BoldPMappersDefault,
  BoldPSDescriptionsSQL,
  BoldDbEvolutor;

const
  cNoSuchIndex = '<no such index>';

{ TBoldDbEvolutorScenarios }

constructor TBoldDbEvolutorScenarios.Create(ASystemHandle: TBoldSystemHandle;
  AAdapter: TBoldAbstractDatabaseAdapter);
begin
  inherited Create;
  FSystemHandle := ASystemHandle;
  FAdapter := AAdapter;
end;

function TBoldDbEvolutorScenarios.Database: IBoldDatabase;
begin
  result := FAdapter.DatabaseInterface;
end;

procedure TBoldDbEvolutorScenarios.ExecSQL(const ASQL: string);
var
  Query: IBoldExecQuery;
begin
  Query := Database.GetExecQuery;
  try
    Query.AssignSQLText(ASQL);
    Query.ExecSQL;
  finally
    Database.ReleaseExecQuery(Query);
  end;
end;

// The evolutor asks for the indexes of a table by its upper-cased name.
function TBoldDbEvolutorScenarios.IndexedColumnsOf(const aTableName, aIndexName: string): string;
var
  Description: TBoldIndexDescription;
begin
  Result := cNoSuchIndex;
  for Description in Database.GetIndexDescriptions(UpperCase(aTableName)) do
    if SameText(Description.IndexName, aIndexName) then
      Result := UpperCase(Description.IndexedColumns);
end;

procedure TBoldDbEvolutorScenarios.EvolveOfUpToDateDatabasePlansNoIndexes;
var
  Evolutor: TBoldDataBaseEvolutor;
  DbScript, MappingScript: TStringList;
  Line, PlannedIndexes: string;
begin
  // CalculateScript refuses a persistence handle that a running system uses.
  FSystemHandle.System.Discard;
  FSystemHandle.Active := False;

  DbScript := nil;
  MappingScript := nil;
  Evolutor := TBoldDataBaseEvolutor.Create(FSystemHandle.PersistenceHandle as TBoldAbstractPersistenceHandleDB);
  try
    DbScript := TStringList.Create;
    MappingScript := TStringList.Create;
    Evolutor.CalculateScript;
    Evolutor.GenerateScript(DbScript, MappingScript);

    PlannedIndexes := '';
    for Line in DbScript do
      if StartsText('CREATE', Line) and ContainsText(Line, ' INDEX ') then
        PlannedIndexes := PlannedIndexes + sLineBreak + Line;
    Assert.AreEqual('', PlannedIndexes,
      'An evolve of the database just created from the model planned indexes that already exist');
  finally
    MappingScript.Free;
    DbScript.Free;
    Evolutor.Free;
  end;
end;

procedure TBoldDbEvolutorScenarios.IndexDescriptionListsCompositeColumnsInIndexOrder;
begin
  // Reversed against the table's column order, so a result in column order fails.
  ExecSQL('CREATE INDEX IX_EVOLVE_COMPOSITE ON TestClass (BOLD_TYPE, BOLD_ID)');
  Assert.AreEqual('BOLD_TYPE;BOLD_ID', IndexedColumnsOf('TestClass', 'IX_EVOLVE_COMPOSITE'),
    'The columns of a composite index, in index order and separated as the evolutor compares them');
end;

// The evolutor needs the primary key to move data between tables, and the
// structure validator matches indexes by name as well as by columns.
procedure TBoldDbEvolutorScenarios.IndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
var
  ModelPrimary: TBoldSQLIndexDescription;
  Description: TBoldIndexDescription;
  Primary: string;
begin
  ModelPrimary := (FSystemHandle.PersistenceHandle as TBoldAbstractPersistenceHandleDB).PersistenceControllerDefault.
    PersistenceMapper.PSSystemDescription.SQLTablesList.ItemsBySQLName['TESTCLASS'].PrimaryIndex;
  Primary := cNoSuchIndex;
  for Description in Database.GetIndexDescriptions('TESTCLASS') do
    if Description.IsPrimary then
      Primary := UpperCase(Description.IndexName + '(' + Description.IndexedColumns + ')');
  Assert.AreEqual(UpperCase(ModelPrimary.GeneratedName + '(' + ModelPrimary.IndexedFields + ')'), Primary,
    'The primary key of TestClass');
end;

// Unlike an INTEGER key, a BIGINT key is backed by an index on SQLite too; it
// must be reported once, as the primary key.
procedure TBoldDbEvolutorScenarios.IndexDescriptionsListAnIndexedPrimaryKeyOnce;
const
  cTable = 'EVOLVE_BIGINT_KEY';
var
  Description: TBoldIndexDescription;
  Found: string;
begin
  ExecSQL('DROP TABLE IF EXISTS ' + cTable);
  ExecSQL('CREATE TABLE ' + cTable + ' (ID BIGINT NOT NULL, CONSTRAINT PK_' + cTable + ' PRIMARY KEY (ID))');
  try
    Found := '';
    for Description in Database.GetIndexDescriptions(cTable) do
      if SameText(Description.IndexedColumns, 'ID') then
        Found := Found + BoolToStr(Description.IsPrimary, True) + ';';
    Assert.AreEqual('True;', Found, 'The descriptions on column ID, by IsPrimary');
  finally
    ExecSQL('DROP TABLE ' + cTable);
  end;
end;

procedure TBoldDbEvolutorScenarios.IndexDescriptionsIgnoreSameNamedTableInOtherSchema;
const
  cOtherSchema = 'bold_evolve_other';
begin
  // Bold's SQL names tables without a schema, so only the table that the
  // search path resolves to is Bold's. PostgreSQL is the engine whose
  // IndexInfoTemplate reads the catalog of all schemas.
  if FAdapter.DatabaseEngine <> dbePostgres then
  begin
    Assert.Pass('PostgreSQL only');
    Exit;
  end;

  ExecSQL('DROP SCHEMA IF EXISTS ' + cOtherSchema + ' CASCADE');
  ExecSQL('CREATE SCHEMA ' + cOtherSchema);
  try
    ExecSQL('CREATE TABLE ' + cOtherSchema + '.TestClass (BOLD_ID INTEGER, OTHER_COLUMN INTEGER)');
    ExecSQL('CREATE INDEX IX_OTHER_SCHEMA ON ' + cOtherSchema + '.TestClass (OTHER_COLUMN)');

    Assert.AreEqual('BOLD_ID', IndexedColumnsOf('TestClass', 'IX_TestClass_BOLD_ID'),
      'The primary index of Bold''s own TestClass');
    Assert.AreEqual(cNoSuchIndex, IndexedColumnsOf('TestClass', 'IX_OTHER_SCHEMA'),
      'An index of a TestClass in another schema');
  finally
    ExecSQL('DROP SCHEMA ' + cOtherSchema + ' CASCADE');
  end;
end;

end.
