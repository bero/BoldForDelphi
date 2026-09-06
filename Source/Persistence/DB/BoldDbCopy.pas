unit BoldDbCopy;

interface

uses
  Classes,
  System.SysUtils,
  BoldAbstractPersistenceHandleDB,
  BoldDBInterfaces,
  BoldThreadSafeQueue,
  System.TimeSpan;

type
  TBoldDbCopy = class;

  TBoldDbCopyProgressEvent = procedure(Sender: TBoldDbCopy; AProgress: integer) of object;
  TBoldDbCopyLogEvent = procedure(Sender: TBoldDbCopy; const aStatus: string) of object;

  TBoldDbCopy = class(TComponent)
  private
    fDestinationPersistenceHandle: TBoldAbstractPersistenceHandleDB;
    fSourcePersistenceHandle: TBoldAbstractPersistenceHandleDB;
    fThreadCount: integer;
    fThreadList: TThreadList;
    fStartTime: TDateTime;
    fTableQueue: TBoldThreadSafeStringQueue;
    fTotalTables: integer;
    fAllTables: TStringList;
    fTotalRecords: integer;
    FOnComplete: TNotifyEvent;
    fProgressEvent: TBoldDbCopyProgressEvent;
    fErrors: TStringList;
    procedure RecordError(const AMessage: string);
    function GetHasErrors: Boolean;
    procedure SetDestinationPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
    procedure SetSourcePersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
    procedure SetThreadCount(const Value: integer);
    procedure DoOnComplete;
    procedure ProcessTables;
    procedure SetOnComplete(const Value: TNotifyEvent);
  protected
    procedure DoOnProgress(AProcessedRecords: Integer);
  public
    class function StripControlChars(const s: string): string;
    // Provider-specific read tuning for the bulk-copy source: streaming
    // (no full-result buffering), forward-only, read-only, batch-sized
    // fetches. Public so the tuning is unit-testable per adapter.
    class procedure TuneSourceConnection(const ADatabase: IBoldDatabase);
    class procedure TuneSourceQuery(const AQuery: IBoldQuery);
    class procedure SetSourceFetchRows(const AQuery: IBoldQuery; ARows: Integer);
    procedure Run;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property TableQueue: TBoldThreadSafeStringQueue read fTableQueue;
    property SourcePersistenceHandle: TBoldAbstractPersistenceHandleDB read fSourcePersistenceHandle write SetSourcePersistenceHandle;
    property DestinationPersistenceHandle: TBoldAbstractPersistenceHandleDB read fDestinationPersistenceHandle write SetDestinationPersistenceHandle;
    property ThreadCount: integer read fThreadCount write SetThreadCount;
    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
    property OnProgress: TBoldDbCopyProgressEvent read fProgressEvent write fProgressEvent;
    property TotalTables: integer read fTotalTables;
    property TotalRecords: integer read fTotalRecords;
    { Worker failures. A worker that raises is logged and recorded here; the
      run still completes (OnComplete fires), so check HasErrors afterwards. }
    property Errors: TStringList read fErrors;
    property HasErrors: Boolean read GetHasErrors;
  end;

  EBoldDbCopy = class(Exception);

implementation

uses
  // VCL
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Option,
  System.Character,
  System.DateUtils,
  System.Math,
  System.Types, // Remove inline hint H2443
  Winapi.ActiveX,

  {$IFDEF UniDAC} Uni,{$ENDIF}

  // Bold
  BoldDefs,
  BoldLogHandler,
  BoldPMappersDefault,
  BoldPSDescriptionsSQL;
{ TBoldDbCopy }

class procedure TBoldDbCopy.TuneSourceConnection(const ADatabase: IBoldDatabase);
begin
  {$IFDEF UniDAC}
  if ADatabase.Implementor is TUniConnection then
    TUniConnection(ADatabase.Implementor).SpecificOptions.Values['ApplicationIntent'] := 'aiReadOnly';
  {$ENDIF}
  // FireDAC understands ApplicationIntent for the MSSQL driver only - an
  // unknown parameter would fail the connect on other drivers.
  if (ADatabase.Implementor is TFDConnection) and
     SameText(TFDConnection(ADatabase.Implementor).DriverName, 'MSSQL') then
    TFDConnection(ADatabase.Implementor).Params.Values['ApplicationIntent'] := 'ReadOnly';
end;

class procedure TBoldDbCopy.TuneSourceQuery(const AQuery: IBoldQuery);
begin
  {$IFDEF UniDAC}
  if AQuery.AsDataSet is TUniQuery then
  begin
    TUniQuery(AQuery.AsDataSet).SpecificOptions.Values['FetchAll'] := 'false';
    TUniQuery(AQuery.AsDataSet).SpecificOptions.Values['SQL Server.FetchAll'] := 'false';
    TUniQuery(AQuery.AsDataSet).UniDirectional := true;
    TUniQuery(AQuery.AsDataSet).ReadOnly := true;
  end;
  {$ENDIF}
  if AQuery.AsDataSet is TFDQuery then
  begin
    TFDQuery(AQuery.AsDataSet).FetchOptions.Mode := fmOnDemand;
    TFDQuery(AQuery.AsDataSet).FetchOptions.Unidirectional := True;
    TFDQuery(AQuery.AsDataSet).UpdateOptions.ReadOnly := True;
  end;
end;

class procedure TBoldDbCopy.SetSourceFetchRows(const AQuery: IBoldQuery; ARows: Integer);
begin
  {$IFDEF UniDAC}
  if AQuery.AsDataSet is TUniQuery then
    TUniQuery(AQuery.AsDataSet).FetchRows := ARows;
  {$ENDIF}
  if AQuery.AsDataSet is TFDQuery then
    TFDQuery(AQuery.AsDataSet).FetchOptions.RowsetSize := ARows;
end;

class function TBoldDbCopy.StripControlChars(const s: string): string;
begin
  Result := s;
  // Delphi strings are 1-based: the last character is at Length(Result).
  // Starting at Length-1 left a trailing control char (e.g. #0) in place -
  // the exact byte PostgreSQL rejects with 'invalid byte sequence'.
  for var x := Length(Result) downto 1 do
    if Result[x].IsControl then
      Delete(Result, x, 1);
end;

procedure TBoldDbCopy.AfterConstruction;
begin
  inherited;
  fTableQueue := TBoldThreadSafeStringQueue.Create('TableQueue');
  ThreadCount := 4;
  fThreadList := TThreadList.Create;
  fAllTables := TStringList.Create;
  fErrors := TStringList.Create;
end;

procedure TBoldDbCopy.BeforeDestruction;
begin
  fTableQueue.free;
  fAllTables.free;
  fThreadList.Free;
  fErrors.Free;
  inherited;
end;

procedure TBoldDbCopy.RecordError(const AMessage: string);
begin
  BoldLog.Log(AMessage, ltError);
  // Called from the worker threads
  TMonitor.Enter(fErrors);
  try
    fErrors.Add(AMessage);
  finally
    TMonitor.Exit(fErrors);
  end;
end;

function TBoldDbCopy.GetHasErrors: Boolean;
begin
  TMonitor.Enter(fErrors);
  try
    result := fErrors.Count > 0;
  finally
    TMonitor.Exit(fErrors);
  end;
end;

procedure TBoldDbCopy.DoOnComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(self);
end;

procedure TBoldDbCopy.DoOnProgress(AProcessedRecords: Integer);
begin
  if Assigned(fProgressEvent) then
    fProgressEvent(self, AProcessedRecords);
end;

procedure TBoldDbCopy.ProcessTables;

var
  SourceQuery: IBoldQuery;
  MultiRowInsertLimit: Integer;
  MaxBatchQueryParams: Integer;
  DestinationTable: TBoldSQLTableDescription;
  DestinationQuery: IBoldExecQuery;
  Columns: string;
  RemainingRecords: integer;
  ParamIndex: integer;

  function CalcBatchSize: integer;
  begin
    var sl := TStringList.Create;
    var sl2 := TStringList.Create;
    var ParamCount:= 0;
    Result := Min(RemainingRecords, MultiRowInsertLimit);
    for var _j := 0 to Result-1 do
    begin
      for var i := 0 to DestinationTable.ColumnsList.Count-1 do
      begin
        sl.Add(':p'+ _j.ToString + '_' + i.ToString);
        inc(ParamCount);
      end;
      sl2.Add(Trim('(' + Trim(sl.CommaText) + ')'));
      sl.Clear;
      if ParamCount >= MaxBatchQueryParams then
      begin
        Result := _j;
        break;
      end;
    end;
    SetSourceFetchRows(SourceQuery, result);
    sl2.QuoteChar := ' ';
    sl2.StrictDelimiter := false;
    var Values := sl2.DelimitedText;
    var InsertSql := Format('insert into %s (%s) values %s;', [DestinationTable.SQLName, Columns, Values]);
    DestinationQuery.ClearParams;
    DestinationQuery.ParamCheck := true;
    DestinationQuery.SQLText := InsertSql;
    // No explicit Prepare here: the parameters have no types yet (they get
    // them from AssignFieldValue in the row loop) and FireDAC refuses to
    // prepare typeless parameters. Both adapters prepare on the first Execute.
    ParamIndex := 0;
    sl.free;
    sl2.free;
  end;

begin
  var InsertSql: string;
  var Values: string;
  var Field: IBoldField;
  var SourceDatabaseInterface := SourcePersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection;
  TuneSourceConnection(SourceDatabaseInterface);
  SourceDatabaseInterface.Open;
  // Get the query from the connection tuned above - it was fetched from a
  // separate anonymous connection before, making the read-only tuning dead
  // and mismatching the ReleaseQuery in the finally block.
  SourceQuery := SourceDatabaseInterface.GetQuery;
  SourceQuery.UseReadTransactions := false;
  TuneSourceQuery(SourceQuery);
  var DatabaseInterface := DestinationPersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection;
  DatabaseInterface.Open;
  DestinationQuery := DatabaseInterface.GetExecQuery;
  var TestQuery := DatabaseInterface.GetQuery;
  var sl := TStringList.Create;
  var sl2 := TStringList.Create;
  try
    repeat
      var SourceTableName := fTableQueue.Dequeue;
      if SourceTableName = '' then
        exit;
      var SourceRecordCount := fAllTables.Values[SourceTableName].ToInteger;
      var DestinationPersistenceMapper := DestinationPersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
      MultiRowInsertLimit := DestinationPersistenceMapper.SQLDataBaseConfig.MultiRowInsertLimit;
      MaxBatchQueryParams := DestinationPersistenceMapper.SQLDataBaseConfig.MaxBatchQueryParams;
      DestinationTable := DestinationPersistenceMapper.AllTables.ItemsBySQLName[SourceTableName];
      Columns := DestinationTable.ColumnsList.ToString;
      var SelectSql := Format('select %s from %s', [Columns, SourceTableName]);
//      MultiRowInsertLimit := 20;
//      MaxBatchQueryParams := 10;
      BoldLog.LogHeader := Format('Loading from %s', [SourceTableName]);
      SetSourceFetchRows(SourceQuery, MultiRowInsertLimit);
      SourceQuery.SQLText := SelectSql;
      SourceQuery.Open;
      var i,j: integer;
      sl.Clear;
      sl2.Clear;
      j := SourceRecordCount;
      BoldLog.LogHeader := Format('%d records loaded from %s', [j, SourceTableName]);
      BoldLog.Log(Format('%d records in table %s', [j, DestinationTable.SQLName]));
      BoldLog.ProgressMax := j;

            TestQuery.SQLText := 'select count(*) from ' + SourceTableName;
            TestQuery.Open;
            if TestQuery.Fields[0].AsInteger > 0 then
            begin
              DestinationQuery.SQLText := 'delete from ' + SourceTableName;
              DestinationQuery.ExecSQL;
              TestQuery.Close;
              TestQuery.Open;
            end;
            TestQuery.Close;
            if DatabaseInterface.InTransaction then
              DatabaseInterface.Commit;

      DatabaseInterface.StartTransaction;
      RemainingRecords := SourceRecordCount;

      var ProcessedRecords := 0;
      var vRecNo := 0;
      var s: string;
      var Bytes: TBytes;
      var Batch := CalcBatchSize;
      repeat
        begin
          if RemainingRecords <= batch then
            Batch := CalcBatchSize;
          repeat
            if ParamIndex = DestinationQuery.ParamCount then
              break;
            for i := 0 to SourceQuery.FieldCount-1 do
            begin
              var Param := DestinationQuery.Param[ParamIndex];
              inc(ParamIndex);
              Param.AssignFieldValue(SourceQuery.Fields[i]);
              if Param.DataType = ftWideMemo then
              begin
                s := StripControlChars(Trim(SourceQuery.Fields[i].AsString));
                Bytes := TEncoding.UTF8.GetBytes(s);
                Param.AsString := TEncoding.UTF8.GetString(Bytes);
              end;
            end;
            SourceQuery.Next;
            inc(vRecNo);
            inc(ProcessedRecords);
            dec(RemainingRecords);
          until RemainingRecords = 0;
          Assert(ParamIndex = DestinationQuery.ParamCount);
          try
            DestinationQuery.ExecSQL;
            ParamIndex := 0;
            DatabaseInterface.Commit;
            DatabaseInterface.StartTransaction;
            DoOnProgress(vRecNo);
            vRecNo := 0;
            BoldLog.Progress := ProcessedRecords;
            BoldLog.LogHeader := Format('%d/%d records processed in table %s', [ProcessedRecords,j, DestinationTable.SQLName]);
          except
            // The old 'invalid byte sequence for encoding' skip stems from the
            // row-by-row era and could never work for batches: 'continue'
            // neither reset ParamIndex nor advanced the source, so the same
            // statement re-executed forever (and PostgreSQL aborts the
            // transaction on error, failing everything after it). The stray
            // control characters that caused it are stripped correctly now;
            // any remaining batch failure must surface, not silently skip
            // rows the closing record-count assertion would flag anyway.
            DatabaseInterface.RollBack;
            raise;
          end;
        end;
      until RemainingRecords = 0;
      if DatabaseInterface.InTransaction then
        DatabaseInterface.Commit;

      TestQuery.SQLText := 'select count(*) from ' + SourceTableName;
      TestQuery.Open;
      Assert(TestQuery.Fields[0].AsInteger = SourceRecordCount);
      if DatabaseInterface.InTransaction then
        DatabaseInterface.Commit;

    until fTableQueue.Empty;
  finally
    sl.free;
    sl2.Free;
    SourceDatabaseInterface.ReleaseQuery(SourceQuery);
    DatabaseInterface.ReleaseExecQuery(DestinationQuery);
    // TestQuery was obtained from the destination connection - releasing it
    // to the source put a destination-bound query into the source's cache.
    DatabaseInterface.ReleaseQuery(TestQuery);
    SourceDatabaseInterface.Close;
    DatabaseInterface.Close;
    // Both wrappers came from CreateAnotherDatabaseConnection and are not
    // reference counted - they used to leak here, one pair per worker.
    SourcePersistenceHandle.DatabaseInterface.ReleaseAnotherDatabaseConnection(SourceDatabaseInterface);
    DestinationPersistenceHandle.DatabaseInterface.ReleaseAnotherDatabaseConnection(DatabaseInterface);
    BoldLog.Log(Format('Thread %d completed', [TThread.CurrentThread.ThreadID]));
  end;
end;

function SortByDescendingRowCount(List: TStringList; Index1,
  Index2: Integer): Integer;
var
  i, j: integer;
begin
  i := List.ValueFromIndex[Index1].ToInteger;
  j := List.ValueFromIndex[Index2].ToInteger;
  result := j - i;
end;

procedure TBoldDbCopy.Run;
var
  SourcePersistenceMapper: TBoldSystemDefaultMapper;
  DestinationPersistenceMapper: TBoldSystemDefaultMapper;
begin
  fStartTime := now;
  SourcePersistenceHandle.Active := true;
  DestinationPersistenceHandle.Active := true;
  SourcePersistenceMapper := SourcePersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
  DestinationPersistenceMapper := DestinationPersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
  var SourceTable: TBoldSQLTableDescription;
  var DestinationTable: TBoldSQLTableDescription;
  var SourceColumn: TBoldSQLDescriptionElement;
  var DestinationColumn: TBoldSQLDescriptionElement;
  for SourceTable in SourcePersistenceMapper.AllTables do
  begin
    DestinationTable := DestinationPersistenceMapper.AllTables.ItemsBySQLName[SourceTable.SQLName];
    if DestinationTable = nil then
      raise EBoldDbCopy.CreateFmt('Table %s not found in destination database.', [SourceTable.SQLName]);
    for SourceColumn in SourceTable.ColumnsList do
    begin
      DestinationColumn := DestinationTable.ColumnsList.ItemsBySQLName[SourceColumn.SQLName];
      if DestinationColumn = nil then
        raise EBoldDbCopy.CreateFmt('Column %s not found in table %s in destination database.', [SourceColumn.SQLName, DestinationTable.SQLName]);
    end;
  end;

  var SourceQuery: IBoldQuery;
  SourceQuery :=  SourcePersistenceHandle.DatabaseInterface.GetQuery;
  try
    for SourceTable in SourcePersistenceMapper.AllTables do
    begin
      var SelectSql := Format('select count(*) from %s', [SourceTable.SQLName]);
      SourceQuery.SQLText := SelectSql;
      SourceQuery.Open;
      if SourceQuery.Fields[0].AsInteger = 0 then
        continue;
      inc(fTotalRecords, SourceQuery.Fields[0].AsInteger);
      fAllTables.Values[SourceTable.SQLName] := SourceQuery.Fields[0].AsString;
      SourceQuery.Close;
    end;
  finally
    SourcePersistenceHandle.DatabaseInterface.ReleaseQuery(SourceQuery);
  end;
  fAllTables.CustomSort(SortByDescendingRowCount);
//  var q := vTables.IndexOfName('Person');
//    vTables.Move(q, 0);
  for var i := 0 to fAllTables.Count-1 do
    fTableQueue.Enqueue(fAllTables.Names[i]);
  fTotalTables := fTableQueue.count;
  for var I := 0 to ThreadCount-1 do
  begin
    var Thread := TThread.CreateAnonymousThread(procedure
     begin
       CoInitialize(nil);
       try
         try
           ProcessTables;
         except
           // The worker is a FreeOnTerminate thread: an exception that leaves
           // it is lost, the run would complete and look successful. Record it
           // so the caller can see the copy failed.
           on E: Exception do
             RecordError(Format('%s: %s', [E.ClassName, E.Message]));
         end;
       finally
         CoUninitialize;
         fThreadList.Remove(TThread.CurrentThread);
         if fThreadList.LockList.Count = 0 then
         begin
           var Duration := TTimeSpan.Subtract(now, fStartTime);
           BoldLog.Log(Format('Operation completed after %s', [Duration.ToString]));
           DoOnComplete;
         end;
         fThreadList.UnlockList;
       end;
     end);
    fThreadList.Add(Thread);
    Thread.Start;
  end;
end;

procedure TBoldDbCopy.SetDestinationPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
begin
  fDestinationPersistenceHandle := Value;
end;

procedure TBoldDbCopy.SetOnComplete(const Value: TNotifyEvent);
begin
  FOnComplete := Value;
end;

procedure TBoldDbCopy.SetSourcePersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
begin
  fSourcePersistenceHandle := Value;
end;

procedure TBoldDbCopy.SetThreadCount(const Value: integer);
begin
  fThreadCount := Value;
end;

end.

