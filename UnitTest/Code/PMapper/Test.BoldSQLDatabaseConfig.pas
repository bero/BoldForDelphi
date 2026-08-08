unit Test.BoldSQLDatabaseConfig;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestBoldSQLDatabaseConfig = class
  private
    FChangeCount: Integer;
    procedure HandleChange(Sender: TObject);
  public
    // Create and defaults
    [Test] [Category('Quick')]
    procedure TestCreate_DefaultValues;
    [Test] [Category('Quick')]
    procedure TestCreate_ReservedWordsPopulated;

    // InitializeDbEngineSettings - per engine
    [Test] [Category('Quick')]
    procedure TestInitialize_SQLServer;
    [Test] [Category('Quick')]
    procedure TestInitialize_SQLServer_MaxParamsInIdList;
    [Test] [Category('Quick')]
    procedure TestInitialize_Postgres;
    [Test] [Category('Quick')]
    procedure TestInitialize_MySQL;
    [Test] [Category('Quick')]
    procedure TestInitialize_InterbaseDialect3;
    [Test] [Category('Quick')]
    procedure TestInitialize_InterbaseDialect1;
    [Test] [Category('Quick')]
    procedure TestInitialize_Oracle;
    [Test] [Category('Quick')]
    procedure TestInitialize_DBISAM;
    [Test] [Category('Quick')]
    procedure TestInitialize_Advantage;
    [Test] [Category('Quick')]
    procedure TestInitialize_Paradox;
    [Test] [Category('Quick')]
    procedure TestInitialize_Informix;
    [Test] [Category('Quick')]
    procedure TestInitialize_GenericANSI;
    [Test] [Category('Quick')]
    procedure TestInitialize_Unknown;

    // Template expansion - Drop queries
    [Test] [Category('Quick')]
    procedure TestGetDropColumnQuery;
    [Test] [Category('Quick')]
    procedure TestGetDropColumnQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetDropIndexQuery;
    [Test] [Category('Quick')]
    procedure TestGetDropIndexQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetDropTableQuery;
    [Test] [Category('Quick')]
    procedure TestGetDropTableQuery_EmptyTemplate;

    // Template expansion - Database queries
    [Test] [Category('Quick')]
    procedure TestGetCreateDatabaseQuery;
    [Test] [Category('Quick')]
    procedure TestGetCreateDatabaseQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetDropDatabaseQuery;
    [Test] [Category('Quick')]
    procedure TestGetDropDatabaseQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetDatabaseExistsQuery;
    [Test] [Category('Quick')]
    procedure TestGetDatabaseExistsQuery_EmptyTemplate;

    // Template expansion - Exists queries
    [Test] [Category('Quick')]
    procedure TestGetColumnExistsQuery;
    [Test] [Category('Quick')]
    procedure TestGetColumnExistsQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetTableExistsQuery;
    [Test] [Category('Quick')]
    procedure TestGetTableExistsQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetIndexExistsQuery;
    [Test] [Category('Quick')]
    procedure TestGetIndexExistsQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetIndexColumnExistsQuery;
    [Test] [Category('Quick')]
    procedure TestGetIndexColumnExistsQuery_EmptyTemplate;
    [Test] [Category('Quick')]
    procedure TestGetIfColumnNotExistsQuery;
    [Test] [Category('Quick')]
    procedure TestGetIfColumnNotExistsQuery_EmptyTemplate;

    // Template expansion - IndexInfo
    [Test] [Category('Quick')]
    procedure TestGetIndexInfoQuery;

    // GetColumnTypeForString / Unicode / Ansi
    [Test] [Category('Quick')]
    procedure TestGetColumnTypeForString_NoLimit;
    [Test] [Category('Quick')]
    procedure TestGetColumnTypeForString_WithinLimit;
    [Test] [Category('Quick')]
    procedure TestGetColumnTypeForString_ExceedsLimit;
    [Test] [Category('Quick')]
    procedure TestGetColumnTypeForUnicodeString_NoLimit;
    [Test] [Category('Quick')]
    procedure TestGetColumnTypeForUnicodeString_ExceedsLimit;
    [Test] [Category('Quick')]
    procedure TestGetColumnTypeForAnsiString_NoLimit;
    [Test] [Category('Quick')]
    procedure TestGetColumnTypeForAnsiString_ExceedsLimit;

    // CorrectlyQuotedDefaultValue
    [Test] [Category('Quick')]
    procedure TestCorrectlyQuotedDefaultValue_NoQuote;
    [Test] [Category('Quick')]
    procedure TestCorrectlyQuotedDefaultValue_WithQuote;

    // EffectiveSQLForNotNull
    [Test] [Category('Quick')]
    procedure TestEffectiveSQLForNotNull_Normal;
    [Test] [Category('Quick')]
    procedure TestEffectiveSQLForNotNull_Empty;

    // IsSQLServerEngine
    [Test] [Category('Quick')]
    procedure TestIsSQLServerEngine;

    // SystemTablePrefix empty fallback
    [Test] [Category('Quick')]
    procedure TestSystemTablePrefix_EmptyFallback;

    // SQLforNotNull empty marker
    [Test] [Category('Quick')]
    procedure TestSQLforNotNull_EmptyUsesMarker;

    // Assign / AssignConfig
    [Test] [Category('Quick')]
    procedure TestAssignConfig;
    [Test] [Category('Quick')]
    procedure TestAssign;

    // Change notification
    [Test] [Category('Quick')]
    procedure TestOnChange;
  end;

implementation

uses
  SysUtils,
  db,
  BoldDefs,
  BoldSQLDatabaseConfig;

{ TTestBoldSQLDatabaseConfig }

procedure TTestBoldSQLDatabaseConfig.TestCreate_DefaultValues;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual(250, Cfg.FetchBlockSize);
    Assert.AreEqual(255, Cfg.DefaultStringLength);
    Assert.AreEqual(20, Cfg.MaxParamsInIdList);
    Assert.AreEqual(18, Cfg.MaxIndexNameLength);
    Assert.AreEqual(-1, Cfg.MaxDbIdentifierLength);
    Assert.AreEqual(-1, Cfg.LongStringLimit);
    Assert.AreEqual(1, Cfg.MultiRowInsertLimit);
    Assert.IsFalse(Cfg.UseSQL92Joins);
    Assert.IsFalse(Cfg.SingleIndexOrderedLinks);
    Assert.IsFalse(Cfg.UseBatchQueries);
    Assert.IsFalse(Cfg.UseParamsForInteger);
    Assert.IsFalse(Cfg.UseParamsForEmptyString);
    Assert.IsTrue(Cfg.AllowMetadataChangesInTransaction);
    Assert.IsTrue(Cfg.SupportsConstraintsInCreateTable);
    Assert.IsTrue(Cfg.SupportsStringDefaultValues);
    Assert.IsFalse(Cfg.QuoteNonStringDefaultValues);
    Assert.AreEqual('INTEGER', Cfg.ColumnTypeForInteger);
    Assert.AreEqual('SMALLINT', Cfg.ColumnTypeForSmallInt);
    Assert.AreEqual('BIGINT', Cfg.ColumnTypeForInt64);
    Assert.AreEqual('VARCHAR(%d)', Cfg.ColumnTypeForString);
    Assert.AreEqual('NVARCHAR(%d)', Cfg.ColumnTypeForUnicodeString);
    Assert.AreEqual('BLOB', Cfg.ColumnTypeForBlob);
    Assert.AreEqual('DOUBLE PRECISION', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('DATE', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('NOT NULL', Cfg.SQLforNotNull);
    Assert.AreEqual('NULL', Cfg.SQLforNull);
    Assert.AreEqual('BOLD', Cfg.SystemTablePrefix);
    Assert.AreEqual(DEFAULTNAME, Cfg.DefaultSystemMapper);
    Assert.AreEqual(DEFAULTNAME, Cfg.DefaultObjectMapper);
    Assert.AreEqual(Ord(ftBlob), Ord(Cfg.FieldTypeForBlob));
    Assert.AreEqual('DROP TABLE <TableName>', Cfg.DropTableTemplate);
    Assert.AreEqual('ALTER TABLE <TableName> DROP <ColumnName>', Cfg.DropColumnTemplate);
    Assert.AreEqual('DROP INDEX <IndexName>', Cfg.DropIndexTemplate);
    Assert.AreEqual('CREATE DATABASE <DatabaseName>', Cfg.CreateDatabaseTemplate);
    Assert.AreEqual('DROP DATABASE <DatabaseName>', Cfg.DropDatabaseTemplate);
    Assert.IsTrue(Cfg.EvolveDropsUnknownIndexes);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestCreate_ReservedWordsPopulated;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.IsTrue(Cfg.ReservedWords.Count > 0);
    Assert.IsTrue(Pos('SELECT', UpperCase(Cfg.ReservedWords.Text)) > 0);
    Assert.IsTrue(Pos('TABLE', UpperCase(Cfg.ReservedWords.Text)) > 0);
  finally
    Cfg.Free;
  end;
end;

// --- InitializeDbEngineSettings ---

procedure TTestBoldSQLDatabaseConfig.TestInitialize_SQLServer;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Assert.AreEqual('DATETIME', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('DATETIME', Cfg.ColumnTypeForDate);
    Assert.AreEqual('DATETIME', Cfg.ColumnTypeForTime);
    Assert.AreEqual('VARBINARY(MAX)', Cfg.ColumnTypeForBlob);
    Assert.AreEqual('DECIMAL (28,10)', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('DECIMAL (28,10)', Cfg.ColumnTypeForCurrency);
    Assert.AreEqual('VARCHAR(MAX)', Cfg.ColumnTypeForText);
    Assert.AreEqual('NVARCHAR(MAX)', Cfg.ColumnTypeForUnicodeText);
    Assert.AreEqual('UNIQUEIDENTIFIER', Cfg.ColumnTypeForGUID);
    Assert.AreEqual('BIGINT', Cfg.ColumnTypeForInt64);
    Assert.AreEqual(128, Cfg.MaxDbIdentifierLength);
    Assert.AreEqual(128, Cfg.MaxIndexNameLength);
    Assert.AreEqual(4000, Cfg.LongStringLimit);
    Assert.AreEqual(1000, Cfg.MultiRowInsertLimit);
    Assert.IsTrue(Cfg.IfTemplate <> '');
    Assert.IsTrue(Cfg.ColumnExistsTemplate <> '');
    Assert.IsTrue(Cfg.TableExistsTemplate <> '');
    Assert.IsTrue(Cfg.IndexExistsTemplate <> '');
    Assert.IsTrue(Cfg.IndexInfoTemplate <> '');
    Assert.AreEqual('BEGIN TRANSACTION', Cfg.SqlScriptStartTransaction);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_SQLServer_MaxParamsInIdList;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    // SQL Server compiles a new plan for every distinct IN-list text; raising the
    // param ceiling above FetchBlockSize (250) keeps full fetch blocks on the
    // parameterized path so statements stay byte-identical and plans reusable.
    Assert.AreEqual(500, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_Postgres;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbePostgres);
    Assert.AreEqual('BYTEA', Cfg.ColumnTypeForBlob);
    Assert.AreEqual('NUMERIC', Cfg.ColumnTypeForCurrency);
    Assert.AreEqual('TIMESTAMP', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('NUMERIC', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('TIME', Cfg.ColumnTypeForTime);
    Assert.AreEqual('UUID', Cfg.ColumnTypeForGUID);
    Assert.AreEqual(63, Cfg.MaxIndexNameLength);
    Assert.AreEqual(63, Cfg.MaxDbIdentifierLength);
    Assert.AreEqual(1000, Cfg.MultiRowInsertLimit);
    Assert.IsTrue(Cfg.ColumnExistsTemplate <> '');
    Assert.IsTrue(Cfg.IndexColumnExistsTemplate <> '');
    Assert.IsTrue(Cfg.DatabaseExistsTemplate <> '');
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_MySQL;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeMySQL);
    Assert.AreEqual('LONGBLOB', Cfg.ColumnTypeForBlob);
    Assert.AreEqual('DECIMAL(18,4)', Cfg.ColumnTypeForCurrency);
    Assert.AreEqual('DATETIME', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('DOUBLE', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('LONGTEXT', Cfg.ColumnTypeForText);
    Assert.AreEqual('CHAR(36)', Cfg.ColumnTypeForGUID);
    Assert.AreEqual(64, Cfg.MaxIndexNameLength);
    Assert.AreEqual(65535, Cfg.LongStringLimit);
    Assert.AreEqual(1000, Cfg.MultiRowInsertLimit);
    Assert.IsTrue(Cfg.ColumnExistsTemplate <> '');
    Assert.IsTrue(Cfg.TableExistsTemplate <> '');
    Assert.IsTrue(Cfg.IndexExistsTemplate <> '');
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_InterbaseDialect3;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeInterbaseSQLDialect3);
    Assert.AreEqual('TIMESTAMP', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('TIMESTAMP', Cfg.ColumnTypeForDate);
    Assert.AreEqual('TIMESTAMP', Cfg.ColumnTypeForTime);
    Assert.AreEqual(31, Cfg.MaxIndexNameLength);
    Assert.AreEqual(31, Cfg.MaxDbIdentifierLength);
    Assert.AreEqual('INT64', Cfg.ColumnTypeForInt64);
    Assert.AreEqual('VARCHAR(32765)', Cfg.ColumnTypeForText);
    Assert.IsTrue(Cfg.AllowMetadataChangesInTransaction);
    Assert.IsTrue(Cfg.IfTemplate <> '');
    Assert.IsTrue(Cfg.IndexInfoTemplate <> '');
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_InterbaseDialect1;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeInterbaseSQLDialect1);
    Assert.AreEqual(31, Cfg.MaxIndexNameLength);
    Assert.AreEqual(31, Cfg.MaxDbIdentifierLength);
    Assert.AreEqual('INT64', Cfg.ColumnTypeForInt64);
    Assert.AreEqual('VARCHAR(32765)', Cfg.ColumnTypeForText);
    Assert.IsTrue(Cfg.AllowMetadataChangesInTransaction);
    Assert.IsTrue(Cfg.IfTemplate <> '');
    Assert.IsTrue(Cfg.ColumnExistsTemplate <> '');
    Assert.IsTrue(Cfg.TableExistsTemplate <> '');
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_Oracle;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeOracle);
    Assert.AreEqual('VARCHAR2(%d)', Cfg.ColumnTypeForString);
    Assert.AreEqual('NUMBER', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('NUMBER(18,4)', Cfg.ColumnTypeForCurrency);
    Assert.AreEqual('CLOB', Cfg.ColumnTypeForText);
    Assert.AreEqual('NVARCHAR2(%d)', Cfg.ColumnTypeForUnicodeString);
    Assert.AreEqual('NCLOB', Cfg.ColumnTypeForUnicodeText);
    Assert.AreEqual('BLOB', Cfg.ColumnTypeForBlob);
    Assert.AreEqual('TIMESTAMP', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('NUMBER(10)', Cfg.ColumnTypeForInteger);
    Assert.AreEqual('NUMBER(5)', Cfg.ColumnTypeForSmallInt);
    Assert.AreEqual('NUMBER(19)', Cfg.ColumnTypeForInt64);
    Assert.AreEqual('RAW(16)', Cfg.ColumnTypeForGUID);
    Assert.AreEqual(4000, Cfg.LongStringLimit);
    Assert.AreEqual(30, Cfg.MaxIndexNameLength);
    Assert.AreEqual(1, Cfg.MultiRowInsertLimit);
    Assert.IsFalse(Cfg.AllowMetadataChangesInTransaction);
    Assert.IsFalse(Cfg.SupportsStringDefaultValues);
    Assert.AreEqual(#1, Cfg.EmptyStringMarker);
    Assert.IsTrue(Cfg.IfTemplate <> '');
    Assert.IsTrue(Cfg.ColumnExistsTemplate <> '');
    Assert.IsTrue(Cfg.TableExistsTemplate <> '');
    Assert.IsTrue(Cfg.IndexExistsTemplate <> '');
    Assert.IsTrue(Cfg.IndexInfoTemplate <> '');
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_DBISAM;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeDBISAM);
    Assert.AreEqual('DATE', Cfg.ColumnTypeForDate);
    Assert.AreEqual('TIME', Cfg.ColumnTypeForTime);
    Assert.AreEqual('TIMESTAMP', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('FLOAT', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('FLOAT', Cfg.ColumnTypeForCurrency);
    Assert.AreEqual(250, Cfg.DefaultStringLength);
    // Should have 'Description' in reserved words
    Assert.IsTrue(Pos('Description', Cfg.ReservedWords.Text) > 0);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_Advantage;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeAdvantage);
    Assert.AreEqual('CONSTRAINT NOT NULL', Cfg.SQLforNotNull);
    Assert.AreEqual('SHORT', Cfg.ColumnTypeForSmallInt);
    Assert.AreEqual('NUMERIC', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('NUMERIC', Cfg.ColumnTypeForCurrency);
    Assert.IsFalse(Cfg.SupportsConstraintsInCreateTable);
    Assert.IsTrue(Cfg.QuoteNonStringDefaultValues);
    Assert.IsFalse(Cfg.SupportsStringDefaultValues);
    Assert.AreEqual('CHAR(%d)', Cfg.ColumnTypeForString);
    Assert.IsTrue(Cfg.StoreEmptyStringsAsNULL);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_Paradox;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeParadox);
    Assert.AreEqual(Ord(dbgTable), Ord(Cfg.DBGenerationMode));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_Informix;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeInformix);
    Assert.AreEqual(18, Cfg.MaxIndexNameLength);
    Assert.AreEqual(18, Cfg.MaxDbIdentifierLength);
    Assert.AreEqual('MONEY', Cfg.ColumnTypeForCurrency);
    Assert.AreEqual('NUMERIC', Cfg.ColumnTypeForFloat);
    Assert.AreEqual('DATETIME YEAR TO FRACTION', Cfg.ColumnTypeForDateTime);
    Assert.AreEqual('DATETIME YEAR TO DAY', Cfg.ColumnTypeForDate);
    Assert.AreEqual('DATETIME HOUR TO FRACTION', Cfg.ColumnTypeForTime);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_GenericANSI;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeGenericANSISQL92);
    Assert.AreEqual('DATE', Cfg.ColumnTypeForDate);
    Assert.AreEqual('TIME', Cfg.ColumnTypeForTime);
    Assert.AreEqual('TIMESTAMP', Cfg.ColumnTypeForDateTime);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_Unknown;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // dbeUnknown does NOT call SetInitialValues
    Cfg.ColumnTypeForBlob := 'CUSTOM_BLOB';
    Cfg.InitializeDbEngineSettings(dbeUnknown);
    Assert.AreEqual('CUSTOM_BLOB', Cfg.ColumnTypeForBlob);
  finally
    Cfg.Free;
  end;
end;

// --- Template expansion: Drop queries ---

procedure TTestBoldSQLDatabaseConfig.TestGetDropColumnQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('ALTER TABLE MyTable DROP MyCol', Cfg.GetDropColumnQuery('MyTable', 'MyCol'));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropColumnQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.DropColumnTemplate := '';
    Assert.WillRaise(
      procedure begin Cfg.GetDropColumnQuery('T', 'C'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropIndexQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('DROP INDEX IX_Test', Cfg.GetDropIndexQuery('MyTable', 'IX_Test'));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropIndexQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.DropIndexTemplate := '';
    Assert.WillRaise(
      procedure begin Cfg.GetDropIndexQuery('T', 'I'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropTableQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('DROP TABLE MyTable', Cfg.GetDropTableQuery('MyTable'));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropTableQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.DropTableTemplate := '';
    Assert.WillRaise(
      procedure begin Cfg.GetDropTableQuery('T'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

// --- Template expansion: Database queries ---

procedure TTestBoldSQLDatabaseConfig.TestGetCreateDatabaseQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('CREATE DATABASE TestDB', Cfg.GetCreateDatabaseQuery('TestDB'));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetCreateDatabaseQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.CreateDatabaseTemplate := '';
    Assert.WillRaise(
      procedure begin Cfg.GetCreateDatabaseQuery('DB'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropDatabaseQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('DROP DATABASE TestDB', Cfg.GetDropDatabaseQuery('TestDB'));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropDatabaseQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.DropDatabaseTemplate := '';
    Assert.WillRaise(
      procedure begin Cfg.GetDropDatabaseQuery('DB'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDatabaseExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    var Q := Cfg.GetDatabaseExistsQuery('MyDB');
    Assert.IsTrue(Pos('MyDB', Q) > 0);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDatabaseExistsQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // Default has empty DatabaseExistsTemplate
    Assert.WillRaise(
      procedure begin Cfg.GetDatabaseExistsQuery('DB'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

// --- Template expansion: Exists queries ---

procedure TTestBoldSQLDatabaseConfig.TestGetColumnExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    var Q := Cfg.GetColumnExistsQuery('MyTable', 'MyCol');
    Assert.IsTrue(Pos('MyTable', Q) > 0);
    Assert.IsTrue(Pos('MyCol', Q) > 0);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnExistsQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.WillRaise(
      procedure begin Cfg.GetColumnExistsQuery('T', 'C'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetTableExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    var Q := Cfg.GetTableExistsQuery('MyTable');
    Assert.IsTrue(Pos('MyTable', Q) > 0);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetTableExistsQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.WillRaise(
      procedure begin Cfg.GetTableExistsQuery('T'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    var Q := Cfg.GetIndexExistsQuery('MyTable', 'IX_Test');
    Assert.IsTrue(Pos('IX_Test', Q) > 0);
    Assert.IsTrue(Pos('MyTable', Q) > 0);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexExistsQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.WillRaise(
      procedure begin Cfg.GetIndexExistsQuery('T', 'I'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexColumnExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    var Q := Cfg.GetIndexColumnExistsQuery('MyTable', 'MyCol');
    Assert.IsTrue(Pos('MyTable', Q) > 0);
    Assert.IsTrue(Pos('MyCol', Q) > 0);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexColumnExistsQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.WillRaise(
      procedure begin Cfg.GetIndexColumnExistsQuery('T', 'C'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIfColumnNotExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    var Q := Cfg.GetIfColumnNotExistsQuery('MyTable', 'MyCol', 'ALTER TABLE MyTable ADD MyCol INT');
    Assert.IsTrue(Pos('NOT EXISTS', Q) > 0);
    Assert.IsTrue(Pos('MyTable', Q) > 0);
    Assert.IsTrue(Pos('MyCol', Q) > 0);
    Assert.IsTrue(Pos('ALTER TABLE MyTable ADD MyCol INT', Q) > 0);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIfColumnNotExistsQuery_EmptyTemplate;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // Default has empty IfTemplate and ColumnExistsTemplate
    Assert.WillRaise(
      procedure begin Cfg.GetIfColumnNotExistsQuery('T', 'C', 'SQL'); end,
      EBold);
  finally
    Cfg.Free;
  end;
end;

// --- IndexInfo ---

procedure TTestBoldSQLDatabaseConfig.TestGetIndexInfoQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    var Q := Cfg.GetIndexInfoQuery('MyTable');
    Assert.IsTrue(Pos('MyTable', Q) > 0);
  finally
    Cfg.Free;
  end;
end;

// --- GetColumnTypeForString ---

procedure TTestBoldSQLDatabaseConfig.TestGetColumnTypeForString_NoLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // LongStringLimit = -1 (default), always uses ColumnTypeForString
    Assert.AreEqual('VARCHAR(100)', Cfg.GetColumnTypeForString(100));
    Assert.AreEqual('VARCHAR(10000)', Cfg.GetColumnTypeForString(10000));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnTypeForString_WithinLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.LongStringLimit := 4000;
    Assert.AreEqual('VARCHAR(255)', Cfg.GetColumnTypeForString(255));
    Assert.AreEqual('VARCHAR(4000)', Cfg.GetColumnTypeForString(4000));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnTypeForString_ExceedsLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.LongStringLimit := 4000;
    Assert.AreEqual('VARCHAR(MAX)', Cfg.GetColumnTypeForString(4001));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnTypeForUnicodeString_NoLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('NVARCHAR(200)', Cfg.GetColumnTypeForUnicodeString(200));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnTypeForUnicodeString_ExceedsLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.LongStringLimit := 4000;
    Assert.AreEqual('NVARCHAR(MAX)', Cfg.GetColumnTypeForUnicodeString(4001));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnTypeForAnsiString_NoLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('VARCHAR(100)', Cfg.GetColumnTypeForAnsiString(100));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnTypeForAnsiString_ExceedsLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.LongStringLimit := 4000;
    Assert.AreEqual('VARCHAR(MAX)', Cfg.GetColumnTypeForAnsiString(4001));
  finally
    Cfg.Free;
  end;
end;

// --- CorrectlyQuotedDefaultValue ---

procedure TTestBoldSQLDatabaseConfig.TestCorrectlyQuotedDefaultValue_NoQuote;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('42', Cfg.CorrectlyQuotedDefaultValue('42'));
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestCorrectlyQuotedDefaultValue_WithQuote;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.QuoteNonStringDefaultValues := True;
    Assert.AreEqual('''42''', Cfg.CorrectlyQuotedDefaultValue('42'));
  finally
    Cfg.Free;
  end;
end;

// --- EffectiveSQLForNotNull ---

procedure TTestBoldSQLDatabaseConfig.TestEffectiveSQLForNotNull_Normal;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.AreEqual('NOT NULL', Cfg.EffectiveSQLForNotNull);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestEffectiveSQLForNotNull_Empty;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.SQLforNotNull := '';
    // Setting to '' stores '<Empty>' marker, EffectiveSQLForNotNull returns ''
    Assert.AreEqual('', Cfg.EffectiveSQLForNotNull);
  finally
    Cfg.Free;
  end;
end;

// --- IsSQLServerEngine ---

procedure TTestBoldSQLDatabaseConfig.TestIsSQLServerEngine;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Assert.IsFalse(Cfg.IsSQLServerEngine);
    Cfg.Engine := dbeSQLServer;
    Assert.IsTrue(Cfg.IsSQLServerEngine);
  finally
    Cfg.Free;
  end;
end;

// --- SystemTablePrefix ---

procedure TTestBoldSQLDatabaseConfig.TestSystemTablePrefix_EmptyFallback;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.SystemTablePrefix := '';
    Assert.AreEqual('BOLD', Cfg.SystemTablePrefix);
  finally
    Cfg.Free;
  end;
end;

// --- SQLforNotNull empty marker ---

procedure TTestBoldSQLDatabaseConfig.TestSQLforNotNull_EmptyUsesMarker;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.SQLforNotNull := '';
    // Internal value is '<Empty>' marker, published property returns it
    Assert.AreEqual('<Empty>', Cfg.SQLforNotNull);
    // But EffectiveSQLForNotNull translates it back to ''
    Assert.AreEqual('', Cfg.EffectiveSQLForNotNull);
  finally
    Cfg.Free;
  end;
end;

// --- Assign / AssignConfig ---

procedure TTestBoldSQLDatabaseConfig.TestAssignConfig;
var
  Src, Dst: TBoldSQLDataBaseConfig;
begin
  Src := TBoldSQLDataBaseConfig.Create;
  Dst := TBoldSQLDataBaseConfig.Create;
  try
    Src.InitializeDbEngineSettings(dbeSQLServer);
    Src.Engine := dbeSQLServer;
    Dst.AssignConfig(Src);

    Assert.AreEqual(Src.ColumnTypeForBlob, Dst.ColumnTypeForBlob);
    Assert.AreEqual(Src.ColumnTypeForDateTime, Dst.ColumnTypeForDateTime);
    Assert.AreEqual(Src.ColumnTypeForFloat, Dst.ColumnTypeForFloat);
    Assert.AreEqual(Src.ColumnTypeForCurrency, Dst.ColumnTypeForCurrency);
    Assert.AreEqual(Src.ColumnTypeForString, Dst.ColumnTypeForString);
    Assert.AreEqual(Src.ColumnTypeForUnicodeString, Dst.ColumnTypeForUnicodeString);
    Assert.AreEqual(Src.ColumnTypeForText, Dst.ColumnTypeForText);
    Assert.AreEqual(Src.ColumnTypeForUnicodeText, Dst.ColumnTypeForUnicodeText);
    Assert.AreEqual(Src.ColumnTypeForInteger, Dst.ColumnTypeForInteger);
    Assert.AreEqual(Src.ColumnTypeForInt64, Dst.ColumnTypeForInt64);
    Assert.AreEqual(Src.ColumnTypeForGUID, Dst.ColumnTypeForGUID);
    Assert.AreEqual(Src.LongStringLimit, Dst.LongStringLimit);
    Assert.AreEqual(Src.MaxIndexNameLength, Dst.MaxIndexNameLength);
    Assert.AreEqual(Src.MaxDbIdentifierLength, Dst.MaxDbIdentifierLength);
    Assert.AreEqual(Src.MultiRowInsertLimit, Dst.MultiRowInsertLimit);
    Assert.AreEqual(Src.DropColumnTemplate, Dst.DropColumnTemplate);
    Assert.AreEqual(Src.DropIndexTemplate, Dst.DropIndexTemplate);
    Assert.AreEqual(Src.IndexInfoTemplate, Dst.IndexInfoTemplate);
    Assert.AreEqual(Src.IfTemplate, Dst.IfTemplate);
    Assert.AreEqual(Src.ColumnExistsTemplate, Dst.ColumnExistsTemplate);
    Assert.AreEqual(Src.TableExistsTemplate, Dst.TableExistsTemplate);
    Assert.AreEqual(Src.IndexExistsTemplate, Dst.IndexExistsTemplate);
    Assert.AreEqual(Src.SqlScriptStartTransaction, Dst.SqlScriptStartTransaction);
    Assert.AreEqual(Ord(Src.Engine), Ord(Dst.Engine));
  finally
    Src.Free;
    Dst.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestAssign;
var
  Src, Dst: TBoldSQLDataBaseConfig;
begin
  Src := TBoldSQLDataBaseConfig.Create;
  Dst := TBoldSQLDataBaseConfig.Create;
  try
    Src.InitializeDbEngineSettings(dbePostgres);
    Src.Engine := dbePostgres;
    Dst.Assign(Src);
    Assert.AreEqual(Src.ColumnTypeForBlob, Dst.ColumnTypeForBlob);
    Assert.AreEqual(Src.ColumnTypeForDateTime, Dst.ColumnTypeForDateTime);
    Assert.AreEqual(Ord(Src.Engine), Ord(Dst.Engine));
  finally
    Src.Free;
    Dst.Free;
  end;
end;

// --- OnChange notification ---

procedure TTestBoldSQLDatabaseConfig.HandleChange(Sender: TObject);
begin
  Inc(FChangeCount);
end;

procedure TTestBoldSQLDatabaseConfig.TestOnChange;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  FChangeCount := 0;
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.OnChange := HandleChange;
    Cfg.ColumnTypeForBlob := 'CUSTOM_BLOB';
    Assert.AreEqual(1, FChangeCount);
    // Setting same value again should NOT trigger change
    Cfg.ColumnTypeForBlob := 'CUSTOM_BLOB';
    Assert.AreEqual(1, FChangeCount);
    // Different value triggers change
    Cfg.ColumnTypeForFloat := 'REAL';
    Assert.AreEqual(2, FChangeCount);
  finally
    Cfg.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSQLDatabaseConfig);

end.
