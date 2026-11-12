unit Test.BoldZeosAdapter;

{ Tests for the Zeos database adapter (BoldZeosInterfaces,
  BoldDatabaseAdapterZeos), driven through Bold's IBoldDatabase interfaces.

  Free Pascal only: Delphi uses FireDAC, so this unit is listed in
  FPC/UnitTestFPC.lpr and not in UnitTest.dpr. Every test gets its own shared
  in-memory SQLite database, which disappears with its last connection. }

interface

uses
  Classes,
  SysUtils,
  BoldTestFramework,
  ZConnection,
  BoldDBInterfaces,
  BoldDatabaseAdapterZeos;

type
  TTestBoldZeosAdapter = class(TBoldTestCase)
  private
    fConnection: TZConnection;
    fAdapter: TBoldDatabaseAdapterZeos;
    fDatabase: IBoldDatabase;
    procedure Exec(const SQL: string);
    function CountRows(const TableName: string): Integer;
    procedure ExecInvalidSQL;
    function ReadIdThroughAnotherConnection(var Other: IBoldDatabase): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParameterTypesRoundTrip;
    procedure TestRollbackDiscardsAndCommitKeeps;
    procedure TestAnotherConnectionSharesTheInMemoryDatabase;
    procedure TestAllTableNamesMatchesPattern;
    procedure TestInvalidSQLRaisesBoldDatabaseError;
    procedure TestBatchOfStatementsRunsEveryStatement;
    procedure TestParamValuesSurviveBoldBatchSequence;
    procedure TestClearParamsThenSameSQLRecreatesTheParams;
    procedure TestBlobParameterStaysABlob;
    procedure TestCurrencyParameterRoundTrip;
    procedure TestCurrencyFieldAssignedToAParameter;
  end;

implementation

uses
  Variants,
  Db,
  BoldDefs,
  BoldSQLDatabaseConfig;

const
  { A shared in-memory SQLite database, as dmBoldTest uses on Delphi: with
    these open flags every connection of the process that opens the same
    file: name sees the same database. The name must not carry
    ?mode=memory&cache=shared: Zeos reads the ? part as its own URL
    parameters and silently opens a file on disk instead. }
  SQLITE_OPEN_READWRITE = $02;
  SQLITE_OPEN_CREATE = $04;
  SQLITE_OPEN_URI = $40;
  SQLITE_OPEN_MEMORY = $80;
  SQLITE_OPEN_SHAREDCACHE = $20000;

var
  DatabaseCounter: Integer = 0;

procedure TTestBoldZeosAdapter.SetUp;
begin
  inherited;
  inc(DatabaseCounter);
  fConnection := TZConnection.Create(nil);
  fConnection.Protocol := 'sqlite';
  fConnection.Database := 'file:zeosadaptertest' + IntToStr(DatabaseCounter);
  fConnection.Properties.Values['SQLiteOpen_Flags'] := IntToStr(SQLITE_OPEN_READWRITE or
    SQLITE_OPEN_CREATE or SQLITE_OPEN_URI or SQLITE_OPEN_MEMORY or SQLITE_OPEN_SHAREDCACHE);
  fAdapter := TBoldDatabaseAdapterZeos.Create(nil);
  fAdapter.DatabaseEngine := dbeGenericANSISQL92;
  fAdapter.Connection := fConnection;
  fDatabase := fAdapter.DatabaseInterface;
  fDatabase.Open;
end;

procedure TTestBoldZeosAdapter.TearDown;
begin
  fDatabase := nil;
  FreeAndNil(fAdapter);
  FreeAndNil(fConnection);
  inherited;
end;

procedure TTestBoldZeosAdapter.Exec(const SQL: string);
var
  Query: IBoldExecQuery;
begin
  Query := fDatabase.GetExecQuery;
  try
    Query.AssignSQLText(SQL);
    Query.ExecSQL;
  finally
    fDatabase.ReleaseExecQuery(Query);
  end;
end;

function TTestBoldZeosAdapter.CountRows(const TableName: string): Integer;
var
  Query: IBoldQuery;
begin
  Query := fDatabase.GetQuery;
  try
    Query.AssignSQLText('select count(*) as n from ' + TableName);
    Query.Open;
    Result := Query.FieldByName('n').AsInteger;
    Query.Close;
  finally
    fDatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldZeosAdapter.ExecInvalidSQL;
begin
  Exec('select * from no_such_table');
end;

procedure TTestBoldZeosAdapter.TestParameterTypesRoundTrip;
const
  Text = 'Bold ' + #$00E5#$00E4#$00F6 + ' ' + #$20AC;
  Big: Int64 = 9007199254740993; // 2^53 + 1, not representable as a Double
var
  ExecQuery: IBoldExecQuery;
  Query: IBoldQuery;
  Blob: TBoldBlobData;
begin
  // Built byte by byte: in mode DelphiUnicode a #255 literal is a WideChar.
  SetLength(Blob, 4);
  Blob[1] := AnsiChar(0);
  Blob[2] := AnsiChar(1);
  Blob[3] := AnsiChar(255);
  Blob[4] := AnsiChar(66);
  Exec('create table t (id integer primary key, s varchar(100), big bigint, f double precision, b blob)');

  ExecQuery := fDatabase.GetExecQuery;
  try
    ExecQuery.AssignSQLText('insert into t (id, s, big, f, b) values (:id, :s, :big, :f, :b)');
    ExecQuery.ParamByName('id').AsInteger := 1;
    ExecQuery.ParamByName('s').AsString := Text;
    ExecQuery.ParamByName('big').AsInt64 := Big;
    ExecQuery.ParamByName('f').AsFloat := 1.25;
    ExecQuery.ParamByName('b').AsBlob := Blob;
    ExecQuery.ExecSQL;
    Assert.AreEqual(1, ExecQuery.RowsAffected, 'one row inserted');
  finally
    fDatabase.ReleaseExecQuery(ExecQuery);
  end;

  Query := fDatabase.GetQuery;
  try
    Query.AssignSQLText('select s, big, f, b from t where id = :id');
    Query.ParamByName('id').AsInteger := 1;
    Query.Open;
    Assert.IsFalse(Query.Eof, 'the row is found');
    Assert.AreEqual(Text, Query.FieldByName('s').AsString, 'Unicode text');
    Assert.AreEqual(Big, Query.FieldByName('big').AsInt64, 'Int64 beyond Double precision');
    Assert.AreEqual(1.25, Query.FieldByName('f').AsFloat, 0, 'float');
    Assert.IsTrue(Blob = Query.FieldByName('b').AsBlob, 'blob bytes, including #0 and #255');
    Query.Close;
  finally
    fDatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldZeosAdapter.TestRollbackDiscardsAndCommitKeeps;
begin
  Exec('create table t (id integer)');
  fDatabase.StartTransaction;
  Exec('insert into t values (1)');
  fDatabase.RollBack;
  Assert.AreEqual(0, CountRows('t'), 'the rolled back insert is gone');

  fDatabase.StartTransaction;
  Exec('insert into t values (2)');
  fDatabase.Commit;
  Assert.AreEqual(1, CountRows('t'), 'the committed insert is kept');
  Assert.IsFalse(fDatabase.InTransaction, 'no transaction left open');
end;

{ Kept apart from the release of the other connection: Free Pascal holds the
  results of interface functions (CreateAnotherDatabaseConnection, GetQuery,
  FieldByName) in hidden temporaries until the end of the routine, and the
  database wrappers are not reference counted. Releasing the connection in the
  same routine would leave temporaries pointing at freed memory, and their
  release at the end of the routine crashes. }
function TTestBoldZeosAdapter.ReadIdThroughAnotherConnection(var Other: IBoldDatabase): Integer;
var
  Query: IBoldQuery;
begin
  Other := fDatabase.CreateAnotherDatabaseConnection;
  Other.Open;
  Query := Other.GetQuery;
  try
    Query.AssignSQLText('select id from t');
    Query.Open;
    if Query.Eof then
      Result := -1
    else
      Result := Query.FieldByName('id').AsInteger;
    Query.Close;
  finally
    Other.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldZeosAdapter.TestAnotherConnectionSharesTheInMemoryDatabase;
var
  Other: IBoldDatabase;
  Id: Integer;
begin
  Exec('create table t (id integer)');
  Exec('insert into t values (42)');
  Other := nil;
  try
    Id := ReadIdThroughAnotherConnection(Other);
  finally
    fDatabase.ReleaseAnotherDatabaseConnection(Other);
  end;
  Assert.AreEqual(42, Id, 'the other connection sees the row written by the first one');
end;

procedure TTestBoldZeosAdapter.TestAllTableNamesMatchesPattern;
var
  Names: TStringList;
begin
  Exec('create table BOLD_A (id integer)');
  Exec('create table BOLD_B (id integer)');
  Exec('create table OTHER (id integer)');
  Names := TStringList.Create;
  try
    fDatabase.AllTableNames('bold_*', false, Names);
    Names.Sort;
    Assert.AreEqual('BOLD_A,BOLD_B', Names.CommaText, 'case-insensitive * pattern');
    Names.Clear;
    fDatabase.AllTableNames('', false, Names);
    Assert.AreEqual(3, Names.Count, 'an empty pattern lists every table');
  finally
    Names.Free;
  end;
end;

procedure TTestBoldZeosAdapter.TestInvalidSQLRaisesBoldDatabaseError;
begin
  Assert.WillRaise(ExecInvalidSQL, EBoldDatabaseError, 'a Zeos error surfaces as EBoldDatabaseError');
  Assert.IsFalse(fDatabase.InTransaction, 'the read transaction of the failed statement is rolled back');
end;

procedure TTestBoldZeosAdapter.TestBatchOfStatementsRunsEveryStatement;
var
  ExecQuery: IBoldExecQuery;
begin
  // SQLite prepares only the first statement of a string; the adapter runs
  // each statement of a batch and binds its own parameters.
  Exec('create table t (id integer not null)');
  ExecQuery := fDatabase.GetExecQuery;
  try
    ExecQuery.AssignSQLText('insert into t values (:a); insert into t values (:b)');
    ExecQuery.ParamByName('a').AsInteger := 1;
    ExecQuery.ParamByName('b').AsInteger := 2;
    ExecQuery.ExecSQL;
  finally
    fDatabase.ReleaseExecQuery(ExecQuery);
  end;
  Assert.AreEqual(2, CountRows('t'), 'both statements of the batch ran');
end;

procedure TTestBoldZeosAdapter.TestParamValuesSurviveBoldBatchSequence;
var
  ExecQuery: IBoldExecQuery;
  Query: IBoldQuery;
begin
  // TBoldBatchDataSetWrapper.ExecuteBatch: parameters are created while
  // ParamCheck is off and the SQL is in BeginUpdate; EndUpdate and then
  // ParamCheck := true follow. Zeos with ParamCheck off would drop them.
  Exec('create table t (id integer not null)');
  ExecQuery := fDatabase.GetExecQuery;
  try
    ExecQuery.ParamCheck := false;
    ExecQuery.SQLStrings.BeginUpdate;
    ExecQuery.CreateParam(ftInteger, 'P1').AsInteger := 42;
    ExecQuery.SQLStrings.Text := 'insert into t values (:P1)';
    ExecQuery.SQLStrings.EndUpdate;
    ExecQuery.ParamCheck := true;
    ExecQuery.ExecSQL;
  finally
    fDatabase.ReleaseExecQuery(ExecQuery);
  end;
  Query := fDatabase.GetQuery;
  try
    Query.AssignSQLText('select id from t');
    Query.Open;
    Assert.IsFalse(Query.Eof, 'the row was inserted');
    Assert.AreEqual(42, Query.FieldByName('id').AsInteger, 'the parameter kept its value');
    Query.Close;
  finally
    fDatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldZeosAdapter.TestClearParamsThenSameSQLRecreatesTheParams;
const
  cInsert = 'insert into t (id) values (:p0_0);';
var
  ExecQuery: IBoldExecQuery;
begin
  // TBoldDbCopy's CalcBatchSize, once per batch: ClearParams, ParamCheck on,
  // then the INSERT. With a batch of one row the INSERT text does not change,
  // and Zeos parses the markers only when the SQL changes, so the cleared
  // parameters did not come back: ParamCount 0, and the copy loop, which
  // fills ParamCount parameters per statement, never moved on.
  Exec('create table t (id integer not null)');
  ExecQuery := fDatabase.GetExecQuery;
  try
    ExecQuery.ClearParams;
    ExecQuery.ParamCheck := true;
    ExecQuery.SQLText := cInsert;
    Assert.AreEqual(1, ExecQuery.ParamCount, 'the first INSERT has its parameter');
    ExecQuery.Param[0].AsInteger := 1;
    ExecQuery.ExecSQL;

    ExecQuery.ClearParams;
    ExecQuery.ParamCheck := true;
    ExecQuery.SQLText := cInsert;
    Assert.AreEqual(1, ExecQuery.ParamCount, 'the same INSERT again gets its parameter back');
    ExecQuery.Param[0].AsInteger := 2;
    ExecQuery.ExecSQL;
  finally
    fDatabase.ReleaseExecQuery(ExecQuery);
  end;
  Assert.AreEqual(2, CountRows('t'), 'both rows were inserted');
end;

procedure TTestBoldZeosAdapter.TestBlobParameterStaysABlob;
var
  ExecQuery: IBoldExecQuery;
  Param: IBoldParameter;
begin
  // SetAsBlob marked the parameter ftBlob and then stored the bytes with
  // AsBytes, which turns a Zeos parameter into ftVarBytes. The SQL log treats
  // only blob types as blobs, read AsString of the bytes and Zeos raised
  // "Unsupported parameter type, SQLType: stUnicodeString/stBytes".
  ExecQuery := fDatabase.GetExecQuery;
  try
    ExecQuery.AssignSQLText('update t set b = :b');
    Param := ExecQuery.ParamByName('b');
    Param.AsBlob := 'blob bytes';
    Assert.AreEqual(Ord(ftBlob), Ord(Param.DataType), 'the parameter is still a blob');
    Assert.IsTrue(Param.AsBlob = 'blob bytes', 'and holds the bytes');
    BoldLogSQL(ExecQuery); // must not raise; logs the blob as a placeholder
  finally
    Param := nil;
    fDatabase.ReleaseExecQuery(ExecQuery);
  end;
end;

procedure TTestBoldZeosAdapter.TestCurrencyParameterRoundTrip;
const
  Price: Currency = 120.5;
  // Four decimals, the most a currency has: the Double on the way must not round it
  Exact: Currency = 1234.5678;
var
  ExecQuery: IBoldExecQuery;
  Query: IBoldQuery;
begin
  // Bold's column type for a currency attribute is DOUBLE PRECISION, and
  // TBoldPMCurrency writes it with Param.AsCurrency. Zeos binds a currency
  // parameter for SQLite as its scaled 64-bit integer (1205000), which only
  // reads back right from a column declared CURRENCY or MONEY.
  Exec('create table t (id integer primary key, c double precision, d double precision, ' +
    'e double precision, v double precision)');
  ExecQuery := fDatabase.GetExecQuery;
  try
    ExecQuery.AssignSQLText('insert into t (id, c, d, e, v) values (:id, :c, :d, :e, :v)');
    ExecQuery.ParamByName('id').AsInteger := 1;
    ExecQuery.ParamByName('c').AsCurrency := Price;
    ExecQuery.ParamByName('d').AsBCD := Price;
    ExecQuery.ParamByName('e').AsCurrency := Exact;
    ExecQuery.ParamByName('v').AsVariant := Exact;
    ExecQuery.ExecSQL;
  finally
    fDatabase.ReleaseExecQuery(ExecQuery);
  end;

  Query := fDatabase.GetQuery;
  try
    Query.AssignSQLText('select c, d, e, v, typeof(c) as ctype from t where id = 1');
    Query.Open;
    Assert.AreEqual(120.5, Query.FieldByName('c').AsFloat, 0, 'AsCurrency, read as a float');
    Assert.IsTrue(Query.FieldByName('c').AsCurrency = Price, 'AsCurrency, read as a currency');
    Assert.AreEqual(120.5, Query.FieldByName('d').AsFloat, 0, 'AsBCD, read as a float');
    Assert.IsTrue(Query.FieldByName('e').AsCurrency = Exact, 'four decimals survive');
    Assert.IsTrue(Query.FieldByName('v').AsCurrency = Exact, 'a currency in a Variant');
    Assert.AreEqual('real', Query.FieldByName('ctype').AsString, 'stored as a REAL, as FireDAC''s ends up');
    Query.Close;
  finally
    fDatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldZeosAdapter.TestCurrencyFieldAssignedToAParameter;
var
  Source, Check: IBoldQuery;
  ExecQuery: IBoldExecQuery;
begin
  // AssignFieldValue copies a field into a parameter, as DbCopy and the
  // update of old data do. A field of a column declared MONEY is a currency
  // in Zeos, and must not be bound as its scaled integer either.
  Exec('create table src (id integer primary key, m money)');
  Exec('insert into src (id, m) values (1, 120.5)');
  Exec('create table t (id integer primary key, c double precision)');
  Source := fDatabase.GetQuery;
  ExecQuery := fDatabase.GetExecQuery;
  try
    Source.AssignSQLText('select m from src where id = 1');
    Source.Open;
    // Zeos reports the MONEY column as ftBCD, whose Variant is a currency
    Assert.AreEqual(varCurrency, Integer(VarType(Source.FieldByName('m').AsVariant)), 'the source value is a currency');
    ExecQuery.AssignSQLText('insert into t (id, c) values (1, :c)');
    ExecQuery.ParamByName('c').AssignFieldValue(Source.FieldByName('m'));
    ExecQuery.ExecSQL;
    Source.Close;
  finally
    fDatabase.ReleaseExecQuery(ExecQuery);
    fDatabase.ReleaseQuery(Source);
  end;

  Check := fDatabase.GetQuery;
  try
    Check.AssignSQLText('select c from t where id = 1');
    Check.Open;
    Assert.AreEqual(120.5, Check.FieldByName('c').AsFloat, 0, 'the copied value');
    Check.Close;
  finally
    fDatabase.ReleaseQuery(Check);
  end;
end;

initialization
  BoldRegisterTest(TTestBoldZeosAdapter);

end.
