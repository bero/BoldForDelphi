unit Test.BoldDbCopy;

{ DUnitX tests for BoldDbCopy - control character stripping (H9) }

interface

uses
  SysUtils,
  DUnitX.TestFramework,
  BoldDbCopy;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbCopyFireDACTuning = class
  public
    [Test]
    [Category('Quick')]
    procedure TestSourceQueryTuningAppliesToFireDAC;
  end;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbCopy = class
  public
    [Test]
    [Category('Quick')]
    procedure TestStripControlCharsRemovesTrailingControlChar;
    [Test]
    [Category('Quick')]
    procedure TestStripControlCharsKeepsPlainText;
  end;

implementation

uses
  BoldDBInterfaces,
  BoldFireDACInterfaces,
  BoldSQLDatabaseConfig,
  FireDAC.Comp.Client,
  FireDAC.Stan.Option;

procedure TTestBoldDbCopyFireDACTuning.TestSourceQueryTuningAppliesToFireDAC;
var
  FDConnection: TFDConnection;
  Config: TBoldSQLDataBaseConfig;
  BoldConnection: TBoldFireDACConnection;
  Database: IBoldDataBase;
  Query: IBoldQuery;
  FDQuery: TFDQuery;
begin
  // Follow-up to the UniDAC-optional change (#51): the bulk-copy read tuning
  // (streaming, forward-only, read-only, batch-sized fetches) existed only in
  // the UniDAC branch, leaving the open-source FireDAC path correct but
  // untuned - unbounded row retention on large source tables.
  Config := TBoldSQLDataBaseConfig.Create;
  FDConnection := TFDConnection.Create(nil);
  BoldConnection := TBoldFireDACConnection.Create(FDConnection, Config);
  try
    Database := BoldConnection;
    Query := Database.GetQuery;
    TBoldDbCopy.TuneSourceQuery(Query);
    TBoldDbCopy.SetSourceFetchRows(Query, 123);
    FDQuery := Query.AsDataSet as TFDQuery;
    Assert.IsTrue(FDQuery.FetchOptions.Unidirectional, 'source must be forward-only');
    Assert.IsTrue(FDQuery.UpdateOptions.ReadOnly, 'source must be read-only');
    Assert.IsTrue(FDQuery.FetchOptions.Mode = fmOnDemand, 'source must stream on demand');
    Assert.AreEqual(123, FDQuery.FetchOptions.RowsetSize, 'fetch batch must match the insert batch');
  finally
    // release in the finally so a failing assertion does not leak the query
    if Assigned(Query) then
      Database.ReleaseQuery(Query);
    Query := nil;
    Database := nil;
    BoldConnection.Free;
    FDConnection.Free;
    Config.Free;
  end;
end;

procedure TTestBoldDbCopy.TestStripControlCharsRemovesTrailingControlChar;
begin
  // Regression for H9: 'for x := Length(s)-1 downto 1' never examined the
  // LAST character (Delphi strings are 1-based), so a trailing control char
  // (e.g. #0) survived - exactly the byte PostgreSQL then rejects with
  // 'invalid byte sequence for encoding', feeding the batch retry loop.
  Assert.AreEqual('abc', TBoldDbCopy.StripControlChars('abc'#0),
    'trailing control character must be removed');
  Assert.AreEqual('abc', TBoldDbCopy.StripControlChars(#7'a'#13'b'#10'c'#0),
    'control characters in all positions must be removed');
  Assert.AreEqual('', TBoldDbCopy.StripControlChars(#0),
    'a single control character must be removed');
end;

procedure TTestBoldDbCopy.TestStripControlCharsKeepsPlainText;
begin
  Assert.AreEqual('abc def', TBoldDbCopy.StripControlChars('abc def'),
    'plain text must pass through unchanged');
  Assert.AreEqual('', TBoldDbCopy.StripControlChars(''),
    'empty string must pass through');
  Assert.AreEqual('åäö€', TBoldDbCopy.StripControlChars('åäö€'),
    'non-ASCII printable characters must pass through');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbCopy);
  TDUnitX.RegisterTestFixture(TTestBoldDbCopyFireDACTuning);

end.
