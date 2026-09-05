unit Test.BoldDbValidator;

{ DUnitX tests for BoldDbValidator - remedy list thread safety (H8) }

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  DUnitX.TestFramework,
  BoldDbValidator;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbValidator = class
  public
    [Test]
    [Category('Quick')]
    procedure TestAddRemedyThreadSafety;
  end;

  { Drives a real TBoldDbValidator run: Execute activates the persistence
    handle, spawns ThreadCount validator threads that each open their own
    database connection, and the last thread to finish calls OnComplete. }
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbValidatorEndToEnd = class
  private
    FDone: TEvent;
    procedure HandleComplete(Sender: TObject);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    [Category('DB')]
    procedure TestValidatorThreadsCompleteAndFreeThemselves;
  end;

implementation

uses
  BoldDBInterfaces,
  maan_UndoRedoBase;

type
  { A validator thread that does one query on its own connection and stops.
    Counts its live instances so a test can tell whether the threads were
    freed - FastMM block counts cannot be used for that: FireDAC retains a few
    blocks per thread that opens a connection until its manager finalizes. }
  TEndToEndValidatorThread = class(TBoldDbValidatorThread)
  private
    class var FInstanceCount: Integer;
  protected
    procedure Validate; override;
  public
    constructor Create(AValidator: TBoldDbValidator); override;
    destructor Destroy; override;
    class property InstanceCount: Integer read FInstanceCount;
  end;

  TEndToEndDbValidator = class(TBoldDbValidator)
  protected
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  public
    function RunningThreadCount: Integer;
  end;

  // CreateValidatorThread is abstract; the hammer test never spawns
  // validator threads, it drives AddRemedy from plain TThreads instead.
  TTestableDbValidator = class(TBoldDbValidator)
  protected
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  end;

function TTestableDbValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  result := nil;
end;

procedure TTestBoldDbValidator.TestAddRemedyThreadSafety;
const
  cThreads = 8;
  cAddsPerThread = 200000;
var
  Validator: TTestableDbValidator;
  Threads: array[0..cThreads - 1] of TThread;
  StartSignal: TEvent;
  i: Integer;
begin
  // Regression for H8 (0b52cef): up to ThreadCount validator threads add to
  // the shared remedy TList<String> with no synchronization. Concurrent Add
  // races lose entries or corrupt the list. With the lock in place the final
  // count is exact and deterministic. The start barrier is essential: without
  // it thread startup latency serializes the workloads and hides the race.
  Validator := TTestableDbValidator.Create(nil);
  StartSignal := TEvent.Create(nil, True, False, '');
  try
    for i := 0 to cThreads - 1 do
    begin
      Threads[i] := TThread.CreateAnonymousThread(
        procedure
        var
          j: Integer;
        begin
          StartSignal.WaitFor(INFINITE);
          for j := 1 to cAddsPerThread do
            Validator.AddRemedy('remedy');
        end);
      Threads[i].FreeOnTerminate := False;
    end;
    for i := 0 to cThreads - 1 do
      Threads[i].Start;
    StartSignal.SetEvent;  // release all threads at once
    for i := 0 to cThreads - 1 do
    begin
      Threads[i].WaitFor;
      Threads[i].Free;
    end;
    Assert.AreEqual(cThreads * cAddsPerThread, Validator.Remedy.Count,
      'concurrent AddRemedy must not lose entries');
  finally
    Validator.Free;
    StartSignal.Free;
  end;
end;

{ TEndToEndValidatorThread }

constructor TEndToEndValidatorThread.Create(AValidator: TBoldDbValidator);
begin
  AtomicIncrement(FInstanceCount);
  inherited;
end;

destructor TEndToEndValidatorThread.Destroy;
begin
  inherited;
  AtomicDecrement(FInstanceCount);
end;

procedure TEndToEndValidatorThread.Validate;
var
  Query: IBoldQuery;
begin
  Query := Database.GetQuery;
  try
    Query.SQLText := 'SELECT 1 AS Res';
    Query.Open;
    Query.Close;
  finally
    Database.ReleaseQuery(Query);
  end;
end;

{ TEndToEndDbValidator }

function TEndToEndDbValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  result := TEndToEndValidatorThread.Create(Self);
end;

function TEndToEndDbValidator.RunningThreadCount: Integer;
begin
  result := ThreadList.LockList.Count;
  ThreadList.UnlockList;
end;

{ TTestBoldDbValidatorEndToEnd }

procedure TTestBoldDbValidatorEndToEnd.SetUp;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
  // The validator activates and deactivates its persistence handle from a
  // worker thread, so it gets the data module's second handle, pointed at
  // the same database (as OpenSystem2 does), and system 1 stays untouched.
  dmUndoRedo.FDConnection2.Params.Values['Database'] :=
    dmUndoRedo.FDConnection1.Params.Values['Database'];
  FDone := TEvent.Create(nil, True, False, '');
end;

procedure TTestBoldDbValidatorEndToEnd.TearDown;
begin
  FreeAndNil(FDone);
  if Assigned(dmUndoRedo) and dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
  end;
  FreeAndNil(dmUndoRedo);
end;

procedure TTestBoldDbValidatorEndToEnd.HandleComplete(Sender: TObject);
begin
  FDone.SetEvent;
end;

procedure TTestBoldDbValidatorEndToEnd.TestValidatorThreadsCompleteAndFreeThemselves;
const
  cThreads = 2;
var
  Validator: TEndToEndDbValidator;
  Waited: Integer;
begin
  // Each validator thread opens its own connection through
  // CreateAnotherDatabaseConnection and releases it at the end of Execute
  // (#92); the thread object itself must go too (#93).
  Validator := TEndToEndDbValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := cThreads;
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator threads must complete and call OnComplete');
    Assert.AreEqual(0, Validator.RunningThreadCount,
      'every thread must have left the validator''s thread list');

    // OnComplete fires from inside the last thread; the threads free
    // themselves after that, so give them a moment.
    Waited := 0;
    while (TEndToEndValidatorThread.InstanceCount > 0) and (Waited < 5000) do
    begin
      Sleep(50);
      Inc(Waited, 50);
    end;
    Assert.AreEqual(0, TEndToEndValidatorThread.InstanceCount,
      'validator threads must free themselves after completion');
  finally
    Validator.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbValidator);
  TDUnitX.RegisterTestFixture(TTestBoldDbValidatorEndToEnd);

end.
