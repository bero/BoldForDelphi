unit Test.BoldDbEvolutor;

{ The database evolutor on the configured test engine.

  The evolutor reads the indexes that exist in the database through the
  engine's IndexInfoTemplate and plans a CREATE INDEX for every model index it
  cannot find there. On a database that was just created from the model it
  must plan none - otherwise an evolve fails on, or duplicates, indexes that
  already exist. }

interface

uses
  DUnitX.TestFramework,
  BoldTestCaseFireDAC;

type
  [TestFixture]
  [Category('PMapper')]
  TTestBoldDbEvolutor = class(TBoldTestCaseFireDAC)
  strict private
    function IndexedColumnsOf(const aTableName, aIndexName: string): string;
  public
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
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  BoldAbstractPersistenceHandleDB,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldPersistenceControllerDefault,
  BoldPMappersDefault,
  BoldPSDescriptionsSQL,
  BoldDbEvolutor;

const
  cNoSuchIndex = '<no such index>';

// The evolutor asks for the indexes of a table by its upper-cased name.
function TTestBoldDbEvolutor.IndexedColumnsOf(const aTableName, aIndexName: string): string;
var
  Description: TBoldIndexDescription;
begin
  Result := cNoSuchIndex;
  for Description in DatabaseAdapter.DatabaseInterface.GetIndexDescriptions(UpperCase(aTableName)) do
    if SameText(Description.IndexName, aIndexName) then
      Result := UpperCase(Description.IndexedColumns);
end;

procedure TTestBoldDbEvolutor.TestEvolveOfUpToDateDatabasePlansNoIndexes;
var
  Evolutor: TBoldDataBaseEvolutor;
  DbScript, MappingScript: TStringList;
  Line, PlannedIndexes: string;
begin
  // CalculateScript refuses a persistence handle that a running system uses.
  SystemHandle.System.Discard;
  SystemHandle.Active := False;

  DbScript := nil;
  MappingScript := nil;
  Evolutor := TBoldDataBaseEvolutor.Create(SystemHandle.PersistenceHandle as TBoldAbstractPersistenceHandleDB);
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

procedure TTestBoldDbEvolutor.TestIndexDescriptionListsCompositeColumnsInIndexOrder;
begin
  // Reversed against the table's column order, so a result in column order fails.
  FDConnection.ExecSQL('CREATE INDEX IX_EVOLVE_COMPOSITE ON TestClass (BOLD_TYPE, BOLD_ID)');
  Assert.AreEqual('BOLD_TYPE;BOLD_ID', IndexedColumnsOf('TestClass', 'IX_EVOLVE_COMPOSITE'),
    'The columns of a composite index, in index order and separated as the evolutor compares them');
end;

// The evolutor needs the primary key to move data between tables, and the
// structure validator matches indexes by name as well as by columns.
procedure TTestBoldDbEvolutor.TestIndexDescriptionsFlagThePrimaryKeyUnderItsModelName;
var
  ModelPrimary: TBoldSQLIndexDescription;
  Description: TBoldIndexDescription;
  Primary: string;
begin
  ModelPrimary := (SystemHandle.PersistenceHandle as TBoldAbstractPersistenceHandleDB).PersistenceControllerDefault.
    PersistenceMapper.PSSystemDescription.SQLTablesList.ItemsBySQLName['TESTCLASS'].PrimaryIndex;
  Primary := cNoSuchIndex;
  for Description in DatabaseAdapter.DatabaseInterface.GetIndexDescriptions('TESTCLASS') do
    if Description.IsPrimary then
      Primary := UpperCase(Description.IndexName + '(' + Description.IndexedColumns + ')');
  Assert.AreEqual(UpperCase(ModelPrimary.GeneratedName + '(' + ModelPrimary.IndexedFields + ')'), Primary,
    'The primary key of TestClass');
end;

// Unlike an INTEGER key, a BIGINT key is backed by an index on SQLite too; it
// must be reported once, as the primary key.
procedure TTestBoldDbEvolutor.TestIndexDescriptionsListAnIndexedPrimaryKeyOnce;
const
  cTable = 'EVOLVE_BIGINT_KEY';
var
  Description: TBoldIndexDescription;
  Found: string;
begin
  FDConnection.ExecSQL('DROP TABLE IF EXISTS ' + cTable);
  FDConnection.ExecSQL('CREATE TABLE ' + cTable + ' (ID BIGINT NOT NULL, CONSTRAINT PK_' + cTable + ' PRIMARY KEY (ID))');
  try
    Found := '';
    for Description in DatabaseAdapter.DatabaseInterface.GetIndexDescriptions(cTable) do
      if SameText(Description.IndexedColumns, 'ID') then
        Found := Found + BoolToStr(Description.IsPrimary, True) + ';';
    Assert.AreEqual('True;', Found, 'The descriptions on column ID, by IsPrimary');
  finally
    FDConnection.ExecSQL('DROP TABLE ' + cTable);
  end;
end;

procedure TTestBoldDbEvolutor.TestIndexDescriptionsIgnoreSameNamedTableInOtherSchema;
const
  cOtherSchema = 'bold_evolve_other';
begin
  // Bold's SQL names tables without a schema, so only the table that the
  // search path resolves to is Bold's. PostgreSQL is the engine whose
  // IndexInfoTemplate reads the catalog of all schemas.
  if DatabaseAdapter.DatabaseEngine <> dbePostgres then
  begin
    Assert.Pass('PostgreSQL only');
    Exit;
  end;

  FDConnection.ExecSQL('DROP SCHEMA IF EXISTS ' + cOtherSchema + ' CASCADE');
  FDConnection.ExecSQL('CREATE SCHEMA ' + cOtherSchema);
  try
    FDConnection.ExecSQL('CREATE TABLE ' + cOtherSchema + '.TestClass (BOLD_ID INTEGER, OTHER_COLUMN INTEGER)');
    FDConnection.ExecSQL('CREATE INDEX IX_OTHER_SCHEMA ON ' + cOtherSchema + '.TestClass (OTHER_COLUMN)');

    Assert.AreEqual('BOLD_ID', IndexedColumnsOf('TestClass', 'IX_TestClass_BOLD_ID'),
      'The primary index of Bold''s own TestClass');
    Assert.AreEqual(cNoSuchIndex, IndexedColumnsOf('TestClass', 'IX_OTHER_SCHEMA'),
      'An index of a TestClass in another schema');
  finally
    FDConnection.ExecSQL('DROP SCHEMA ' + cOtherSchema + ' CASCADE');
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbEvolutor);

end.
