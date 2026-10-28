unit Test.BoldSQLDatabaseConfig;

interface

uses
  BoldTestFramework;

type
  TTestBoldSQLDatabaseConfig = class(TBoldTestCase)
  private
    FChangeCount: Integer;
    fRaiseCfg: TObject;
    procedure TestGetDropColumnQuery_EmptyTemplate_Raiser;
    procedure TestGetDropIndexQuery_EmptyTemplate_Raiser;
    procedure TestGetDropTableQuery_EmptyTemplate_Raiser;
    procedure TestGetCreateDatabaseQuery_EmptyTemplate_Raiser;
    procedure TestGetDropDatabaseQuery_EmptyTemplate_Raiser;
    procedure TestGetDatabaseExistsQuery_EmptyTemplate_Raiser;
    procedure TestGetColumnExistsQuery_EmptyTemplate_Raiser;
    procedure TestGetTableExistsQuery_EmptyTemplate_Raiser;
    procedure TestGetIndexExistsQuery_EmptyTemplate_Raiser;
    procedure TestGetIndexColumnExistsQuery_EmptyTemplate_Raiser;
    procedure TestGetIfColumnNotExistsQuery_EmptyTemplate_Raiser;
    procedure HandleChange(Sender: TObject);
  published
    // Create and defaults
    procedure TestCreate_DefaultValues;
    procedure TestCreate_ReservedWordsPopulated;

    // InitializeDbEngineSettings - per engine
    procedure TestInitialize_SQLServer;
    procedure TestInitialize_SQLServer_MaxParamsInIdList;
    procedure TestInitialize_SQLServer_MaxParamsInIdListFollowsFetchBlockSize;
    procedure TestInitialize_SQLServer_MaxParamsInIdListCappedAtEngineLimit;
    procedure TestInitialize_SetsEngine;
    procedure TestSetFetchBlockSize_SQLServer_RaisesMaxParamsInIdList;
    procedure TestSetFetchBlockSize_OtherEngine_KeepsMaxParamsInIdList;
    procedure TestSetEngine_SQLServer_RaisesMaxParamsInIdListToFetchBlockSize;
    procedure TestSetEngine_SQLServer_DefaultBlockSizeRaisesToBlockSize;
    procedure TestSetEngine_SQLServer_CapsAtEngineLimit;
    procedure TestSetEngine_OtherEngine_KeepsMaxParamsInIdList;
    procedure TestSetMaxParamsInIdList_SQLServer_NeverBelowFetchBlockSize;
    procedure TestMaxParamsInIdListZero_SwitchesParameterizationOff;
    procedure TestInitialize_Postgres;
    procedure TestInitialize_MySQL;
    procedure TestInitialize_InterbaseDialect3;
    procedure TestInitialize_InterbaseDialect1;
    procedure TestInitialize_Oracle;
    procedure TestInitialize_DBISAM;
    procedure TestInitialize_Advantage;
    procedure TestInitialize_Paradox;
    procedure TestInitialize_Informix;
    procedure TestInitialize_GenericANSI;
    procedure TestInitialize_Unknown;

    // Template expansion - Drop queries
    procedure TestGetDropColumnQuery;
    procedure TestGetDropColumnQuery_EmptyTemplate;
    procedure TestGetDropIndexQuery;
    procedure TestGetDropIndexQuery_EmptyTemplate;
    procedure TestGetDropTableQuery;
    procedure TestGetDropTableQuery_EmptyTemplate;

    // Template expansion - Database queries
    procedure TestGetCreateDatabaseQuery;
    procedure TestGetCreateDatabaseQuery_EmptyTemplate;
    procedure TestGetDropDatabaseQuery;
    procedure TestGetDropDatabaseQuery_EmptyTemplate;
    procedure TestGetDatabaseExistsQuery;
    procedure TestGetDatabaseExistsQuery_EmptyTemplate;

    // Template expansion - Exists queries
    procedure TestGetColumnExistsQuery;
    procedure TestGetColumnExistsQuery_EmptyTemplate;
    procedure TestGetTableExistsQuery;
    procedure TestGetTableExistsQuery_EmptyTemplate;
    procedure TestGetIndexExistsQuery;
    procedure TestGetIndexExistsQuery_EmptyTemplate;
    procedure TestGetIndexColumnExistsQuery;
    procedure TestGetIndexColumnExistsQuery_EmptyTemplate;
    procedure TestGetIfColumnNotExistsQuery;
    procedure TestGetIfColumnNotExistsQuery_EmptyTemplate;

    // Template expansion - IndexInfo
    procedure TestGetIndexInfoQuery;

    // GetColumnTypeForString / Unicode / Ansi
    procedure TestGetColumnTypeForString_NoLimit;
    procedure TestGetColumnTypeForString_WithinLimit;
    procedure TestGetColumnTypeForString_ExceedsLimit;
    procedure TestGetColumnTypeForUnicodeString_NoLimit;
    procedure TestGetColumnTypeForUnicodeString_ExceedsLimit;
    procedure TestGetColumnTypeForAnsiString_NoLimit;
    procedure TestGetColumnTypeForAnsiString_ExceedsLimit;

    // CorrectlyQuotedDefaultValue
    procedure TestCorrectlyQuotedDefaultValue_NoQuote;
    procedure TestCorrectlyQuotedDefaultValue_WithQuote;

    // EffectiveSQLForNotNull
    procedure TestEffectiveSQLForNotNull_Normal;
    procedure TestEffectiveSQLForNotNull_Empty;

    // IsSQLServerEngine
    procedure TestIsSQLServerEngine;

    // SystemTablePrefix empty fallback
    procedure TestSystemTablePrefix_EmptyFallback;

    // SQLforNotNull empty marker
    procedure TestSQLforNotNull_EmptyUsesMarker;

    // Assign / AssignConfig
    procedure TestAssignConfig;
    procedure TestAssign;

    // Change notification
    procedure TestOnChange;

    // Generated evolution script
    procedure TestSQLServerEvolutionScript_OneDeclarePerBatch;
  end;

implementation

uses
  SysUtils,
  Classes,
  db,
  BoldDefs,
  BoldSQLDatabaseConfig,
  BoldDbEvolutorScript;

{ TTestBoldSQLDatabaseConfig }

procedure TTestBoldSQLDatabaseConfig.TestGetDropColumnQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetDropColumnQuery('T', 'C');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropIndexQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetDropIndexQuery('T', 'I');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropTableQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetDropTableQuery('T');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetCreateDatabaseQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetCreateDatabaseQuery('DB');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDropDatabaseQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetDropDatabaseQuery('DB');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDatabaseExistsQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetDatabaseExistsQuery('DB');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetColumnExistsQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetColumnExistsQuery('T', 'C');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetTableExistsQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetTableExistsQuery('T');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexExistsQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetIndexExistsQuery('T', 'I');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexColumnExistsQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetIndexColumnExistsQuery('T', 'C');
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIfColumnNotExistsQuery_EmptyTemplate_Raiser;
begin
  TBoldSQLDataBaseConfig(fRaiseCfg).GetIfColumnNotExistsQuery('T', 'C', 'SQL');
end;

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

procedure TTestBoldSQLDatabaseConfig.TestInitialize_SQLServer_MaxParamsInIdListFollowsFetchBlockSize;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // A dfm-streamed FetchBlockSize above 500 must not push every full fetch
    // block back to literal SQL: the ceiling follows the block size.
    Cfg.FetchBlockSize := 600;
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Assert.AreEqual(600, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_SQLServer_MaxParamsInIdListCappedAtEngineLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // SQL Server allows 2100 parameters per statement; the ceiling stays below it.
    Cfg.FetchBlockSize := 3000;
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Assert.AreEqual(2000, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestInitialize_SetsEngine;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Assert.IsTrue(Cfg.Engine = dbeSQLServer, 'Engine must record the engine the settings were initialized for');
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestSetFetchBlockSize_SQLServer_RaisesMaxParamsInIdList;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // FetchBlockSize set at runtime, after the engine settings ran.
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Cfg.FetchBlockSize := 600;
    Assert.AreEqual(600, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestSetFetchBlockSize_OtherEngine_KeepsMaxParamsInIdList;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // Only SQL Server opted in to full-block parameterization; other engines
    // keep the default ceiling whatever the block size.
    Cfg.InitializeDbEngineSettings(dbePostgres);
    Cfg.FetchBlockSize := 600;
    Assert.AreEqual(20, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestSetEngine_SQLServer_RaisesMaxParamsInIdListToFetchBlockSize;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // dfm order: the config streams before the adapter's DatabaseEngine, and
    // under csLoading InitializeDbEngineSettings never runs - the Engine
    // setter is the last write and must apply the ceiling itself.
    Cfg.FetchBlockSize := 600;
    Cfg.Engine := dbeSQLServer;
    Assert.AreEqual(600, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestSetEngine_SQLServer_DefaultBlockSizeRaisesToBlockSize;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // A dfm that stores neither FetchBlockSize nor MaxParamsInIdList still
    // gets its full 250-id blocks parameterized on SQL Server.
    Cfg.Engine := dbeSQLServer;
    Assert.AreEqual(250, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestSetEngine_SQLServer_CapsAtEngineLimit;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.FetchBlockSize := 3000;
    Cfg.Engine := dbeSQLServer;
    Assert.AreEqual(2000, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestSetEngine_OtherEngine_KeepsMaxParamsInIdList;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.FetchBlockSize := 600;
    Cfg.Engine := dbePostgres;
    Assert.AreEqual(20, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestSetMaxParamsInIdList_SQLServer_NeverBelowFetchBlockSize;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // A stored MaxParamsInIdList streams after FetchBlockSize; a value below
    // the block size would silently put every full block back on literal SQL.
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Cfg.FetchBlockSize := 600;
    Cfg.MaxParamsInIdList := 500;
    Assert.AreEqual(600, Cfg.MaxParamsInIdList);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestMaxParamsInIdListZero_SwitchesParameterizationOff;
var
  Cfg: TBoldSQLDataBaseConfig;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    // 0 is the explicit off-switch (a bad parameterized plan is worked around
    // by forcing literal id lists); no write path may raise it to the floor.
    Cfg.FetchBlockSize := 600;
    Cfg.MaxParamsInIdList := 0;
    Cfg.Engine := dbeSQLServer;
    Assert.AreEqual(0, Cfg.MaxParamsInIdList, 'Engine setter must not raise an explicit 0');
    Cfg.FetchBlockSize := 700;
    Assert.AreEqual(0, Cfg.MaxParamsInIdList, 'FetchBlockSize setter must not raise an explicit 0');
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Cfg.MaxParamsInIdList := 0;
    Assert.AreEqual(0, Cfg.MaxParamsInIdList, 'Writing 0 after the engine settings must stick');
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetDropColumnQuery_EmptyTemplate_Raiser, EBold);
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetDropIndexQuery_EmptyTemplate_Raiser, EBold);
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetDropTableQuery_EmptyTemplate_Raiser, EBold);
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetCreateDatabaseQuery_EmptyTemplate_Raiser, EBold);
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetDropDatabaseQuery_EmptyTemplate_Raiser, EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetDatabaseExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
  Q: string;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Q := Cfg.GetDatabaseExistsQuery('MyDB');
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetDatabaseExistsQuery_EmptyTemplate_Raiser, EBold);
  finally
    Cfg.Free;
  end;
end;

// --- Template expansion: Exists queries ---

procedure TTestBoldSQLDatabaseConfig.TestGetColumnExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
  Q: string;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Q := Cfg.GetColumnExistsQuery('MyTable', 'MyCol');
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetColumnExistsQuery_EmptyTemplate_Raiser, EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetTableExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
  Q: string;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Q := Cfg.GetTableExistsQuery('MyTable');
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetTableExistsQuery_EmptyTemplate_Raiser, EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
  Q: string;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Q := Cfg.GetIndexExistsQuery('MyTable', 'IX_Test');
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetIndexExistsQuery_EmptyTemplate_Raiser, EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIndexColumnExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
  Q: string;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Q := Cfg.GetIndexColumnExistsQuery('MyTable', 'MyCol');
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetIndexColumnExistsQuery_EmptyTemplate_Raiser, EBold);
  finally
    Cfg.Free;
  end;
end;

procedure TTestBoldSQLDatabaseConfig.TestGetIfColumnNotExistsQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
  Q: string;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Q := Cfg.GetIfColumnNotExistsQuery('MyTable', 'MyCol', 'ALTER TABLE MyTable ADD MyCol INT');
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
    fRaiseCfg := Cfg;
    Assert.WillRaise(TestGetIfColumnNotExistsQuery_EmptyTemplate_Raiser, EBold);
  finally
    Cfg.Free;
  end;
end;

// --- IndexInfo ---

procedure TTestBoldSQLDatabaseConfig.TestGetIndexInfoQuery;
var
  Cfg: TBoldSQLDataBaseConfig;
  Q: string;
begin
  Cfg := TBoldSQLDataBaseConfig.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    Q := Cfg.GetIndexInfoQuery('MyTable');
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

// --- Generated evolution script ---

procedure TTestBoldSQLDatabaseConfig.TestSQLServerEvolutionScript_OneDeclarePerBatch;
const
  cDeclare = 'DECLARE @CONSTRAINTNAME';
var
  Cfg: TBoldSQLDataBaseConfig;
  EvolutionScript: TBoldDataBaseEvolutorScript;
  Lines: TStringList;
  i, DeclaresInBatch, DeclaresTotal: Integer;
begin
  // Every SQL Server column drop declares @CONSTRAINTNAME, and SQL Server
  // refuses a second DECLARE of a variable within one batch. A script that
  // drops two columns must therefore separate the drops with GO.
  Cfg := TBoldSQLDataBaseConfig.Create;
  EvolutionScript := TBoldDataBaseEvolutorScript.Create;
  Lines := TStringList.Create;
  try
    Cfg.InitializeDbEngineSettings(dbeSQLServer);
    EvolutionScript.DropColumn('PERSON', 'NICKNAME');
    EvolutionScript.DropColumn('PERSON', 'SHOESIZE');
    EvolutionScript.GenerateScript(Lines, Cfg);
    DeclaresInBatch := 0;
    DeclaresTotal := 0;
    for i := 0 to Lines.Count - 1 do
      if SameText(Trim(Lines[i]), 'GO') then
        DeclaresInBatch := 0
      else if Pos(cDeclare, Lines[i]) > 0 then
      begin
        Inc(DeclaresInBatch);
        Inc(DeclaresTotal);
        Assert.AreEqual(1, DeclaresInBatch, 'Two drops share one batch:' + sLineBreak + Lines.Text);
      end;
    Assert.AreEqual(2, DeclaresTotal, Lines.Text);
  finally
    Lines.Free;
    EvolutionScript.Free;
    Cfg.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldSQLDatabaseConfig);

end.
