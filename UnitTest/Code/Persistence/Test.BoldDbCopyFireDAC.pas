unit Test.BoldDbCopyFireDAC;

{ The FireDAC side of BoldDbCopy: the bulk-copy read tuning of a FireDAC source
  query. Delphi only, like the FireDAC adapter it checks; the adapter-neutral
  BoldDbCopy tests are in Test.BoldDbCopy. }

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbCopyFireDACTuning = class
  public
    [Test]
    procedure TestSourceQueryTuningAppliesToFireDAC;
  end;

implementation

uses
  BoldDBInterfaces,
  BoldDbCopy,
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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbCopyFireDACTuning);

end.
