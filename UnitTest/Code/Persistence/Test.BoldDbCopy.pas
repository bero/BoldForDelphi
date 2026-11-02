unit Test.BoldDbCopy;

{ Tests for BoldDbCopy - control character stripping (H9) and an end-to-end
  copy, shared between DUnitX and FPCUnit. The end-to-end copy runs on
  dmUndoRedo: FireDAC under Delphi, Zeos under Free Pascal. The FireDAC query
  tuning is in Test.BoldDbCopyFireDAC (Delphi only). }

interface

uses
  SysUtils,
  SyncObjs,
  BoldTestFramework,
  BoldDBInterfaces,
  BoldDbCopy;

type
  {$IFNDEF FPC}
  [Category('Persistence')]
  {$ENDIF}
  TTestBoldDbCopy = class(TBoldTestCase)
  published
    procedure TestStripControlCharsRemovesTrailingControlChar;
    procedure TestStripControlCharsKeepsPlainText;
  end;

  { Copies the test database into a second in-memory database with a real
    TBoldDbCopy run (Run spawns ThreadCount worker threads, each with its own
    source and destination connection) and checks what arrived. }
  {$IFNDEF FPC}
  [Category('Persistence')]
  {$ENDIF}
  TTestBoldDbCopyEndToEnd = class(TBoldTestCase)
  private
    FDone: TEvent;
    FSharedCacheSQLite: Boolean;
    procedure HandleComplete(Sender: TObject);
    function RowCount(const ADatabase: IBoldDatabase; const ATable: string): Integer;
    function StringsOf(const ADatabase: IBoldDatabase; const ASql: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFNDEF FPC}
    [Category('DB')]
    {$ENDIF}
    procedure TestCopiesAllRowsToTheDestination;
  end;

implementation

uses
  BoldTestModel,
  maan_UndoRedoBase;

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

{ TTestBoldDbCopyEndToEnd }

procedure TTestBoldDbCopyEndToEnd.SetUp;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
  // Destination: the data module's second persistence handle, pointed at a
  // second database with a freshly created (empty) schema. The connector
  // knows how: a second in-memory database on SQLite, a sibling of the test
  // database on server engines.
  FSharedCacheSQLite := OpenUndoRedoSecondDatabase;
  dmUndoRedo.BoldPersistenceHandleDB2.CreateDataBaseSchema;
  FDone := TEvent.Create(nil, True, False, '');
end;

procedure TTestBoldDbCopyEndToEnd.TearDown;
begin
  FreeAndNil(FDone);
  if Assigned(dmUndoRedo) then
  begin
    if dmUndoRedo.BoldPersistenceHandleDB2.Active then
      dmUndoRedo.BoldPersistenceHandleDB2.Active := False;
    if dmUndoRedo.BoldSystemHandle1.Active then
    begin
      dmUndoRedo.BoldSystemHandle1.System.Discard;
      dmUndoRedo.BoldSystemHandle1.Active := False;
    end;
  end;
  // The data module's second connection was re-pointed; drop the module so
  // the next fixture gets a fresh one from the dfm.
  FreeAndNil(dmUndoRedo);
end;

procedure TTestBoldDbCopyEndToEnd.HandleComplete(Sender: TObject);
begin
  FDone.SetEvent;
end;

function TTestBoldDbCopyEndToEnd.RowCount(const ADatabase: IBoldDatabase; const ATable: string): Integer;
var
  Query: IBoldQuery;
begin
  Query := ADatabase.GetQuery;
  try
    Query.SQLText := 'select count(*) from ' + ATable;
    Query.Open;
    result := Query.Fields[0].AsInteger;
    Query.Close;
  finally
    ADatabase.ReleaseQuery(Query);
  end;
end;

function TTestBoldDbCopyEndToEnd.StringsOf(const ADatabase: IBoldDatabase; const ASql: string): string;
var
  Query: IBoldQuery;
begin
  result := '';
  Query := ADatabase.GetQuery;
  try
    Query.SQLText := ASql;
    Query.Open;
    while not Query.Eof do
    begin
      result := result + Query.Fields[0].AsString + '|';
      Query.Next;
    end;
    Query.Close;
  finally
    ADatabase.ReleaseQuery(Query);
  end;
end;

procedure TTestBoldDbCopyEndToEnd.TestCopiesAllRowsToTheDestination;
const
  cValues: array[1..3] of string = ('copy one', 'copy two', 'copy three');
var
  i: Integer;
  DbCopy: TBoldDbCopy;
  Source, Destination: IBoldDatabase;
  SourceObjects, SourceStrings: string;
begin
  for i := Low(cValues) to High(cValues) do
    TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System).aString := cValues[i];
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Source := dmUndoRedo.BoldPersistenceHandleDB1.DatabaseAdapter.DatabaseInterface;
  Destination := dmUndoRedo.BoldPersistenceHandleDB2.DatabaseAdapter.DatabaseInterface;
  // CreateDataBaseSchema in SetUp closed the destination connection again.
  // FireDAC reconnects on the first query; Zeos does not, so open it here.
  if not Destination.Connected then
    Destination.Open;
  SourceObjects := IntToStr(RowCount(Source, 'BOLD_OBJECT'));
  SourceStrings := StringsOf(Source, 'select aString from SomeClass order by aString');
  Assert.AreEqual(0, RowCount(Destination, 'SomeClass'),
    'precondition: the destination schema starts empty');

  DbCopy := TBoldDbCopy.Create(nil);
  try
    DbCopy.SourcePersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB1;
    DbCopy.DestinationPersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    // SQLite's shared-cache mode (the in-memory test database) refuses
    // concurrent writers with SQLITE_LOCKED, so one worker there; the
    // multi-worker path is exercised on server engines.
    if FSharedCacheSQLite then
      DbCopy.ThreadCount := 1
    else
      DbCopy.ThreadCount := 2;
    DbCopy.OnComplete := HandleComplete;
    DbCopy.Run;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(30000)),
      'the copy workers must complete and call OnComplete');
    // OnComplete fires from inside the last worker; the anonymous threads
    // finish and free themselves right after it.
    Sleep(200);

    Assert.IsFalse(DbCopy.HasErrors, 'the workers must not fail: ' + DbCopy.Errors.Text);
    Assert.IsTrue(DbCopy.TotalTables > 0, 'the run must have found tables to copy');
    Assert.AreEqual(SourceObjects, IntToStr(RowCount(Destination, 'BOLD_OBJECT')),
      'every BOLD_OBJECT row must have been copied');
    Assert.AreEqual(Length(cValues), RowCount(Destination, 'SomeClass'),
      'every SomeClass row must have been copied');
    Assert.AreEqual(SourceStrings,
      StringsOf(Destination, 'select aString from SomeClass order by aString'),
      'the copied rows must carry the source values');
  finally
    DbCopy.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldDbCopy);
  BoldRegisterTest(TTestBoldDbCopyEndToEnd);

end.
