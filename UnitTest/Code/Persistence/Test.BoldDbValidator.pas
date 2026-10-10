unit Test.BoldDbValidator;

{ DUnitX tests for BoldDbValidator - remedy list thread safety (H8) }

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  DUnitX.TestFramework,
  BoldDbValidator,
  BoldDbDataValidator;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbValidator = class
  public
    [Test]
    [Category('Quick')]
    procedure TestAddRemedyThreadSafety;
    [Test]
    [Category('Quick')]
    procedure TestCorruptObjectsActionDefaultsToDelete;
    [Test]
    [Category('Quick')]
    procedure TestCompletionMessage;
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
    procedure HandleCompleteThenRaise(Sender: TObject);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    [Category('DB')]
    procedure TestValidatorThreadsCompleteAndFreeThemselves;
    [Test]
    [Category('DB')]
    procedure TestDataValidatorThreadEndsCleanly;
    [Test]
    [Category('DB')]
    procedure TestInsertRemedyPutsScriptSeparatorOnItsOwnLine;
    [Test]
    [Category('DB')]
    procedure TestValidatorThreadStartsAfterConstructionAndListing;
    [Test]
    [Category('DB')]
    procedure TestWorkerExceptionIsReported;
    [Test]
    [Category('DB')]
    procedure TestWorkerExceptionIsLoggedAsError;
    [Test]
    [Category('DB')]
    procedure TestBrokenClassDoesNotStopTheOthers;
    [Test]
    [Category('DB')]
    procedure TestRerunReportsOnlyItsOwnResults;
    [Test]
    [Category('DB')]
    procedure TestOnCompleteExceptionIsLoggedAndLogEnded;
    [Test]
    [Category('DB')]
    procedure TestFailingRelationValidationIsReported;
    [Test]
    [Category('DB')]
    procedure TestFailingLinkObjectValidationIsReported;
  private
    procedure RunWithTableDropped(const ExpressionName: string; TestTypes: TBoldDBDataValidatorTestTypes);
  end;

implementation

uses
  System.StrUtils,
  FireDAC.Comp.Client,
  BoldDefs,
  BoldCoreConsts,
  BoldLogHandler,
  BoldLogReceiverInterface,
  BoldSystem,
  BoldDBInterfaces,
  BoldPMappersSQL,
  BoldTestModel,
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

  { A real data validator thread that checks existence in the parent table, so
    it opens a query on its own connection, and records how it ended: the
    exception Execute left in FatalException and one raised while it is
    destroyed. }
  TRecordingDataValidatorThread = class(TBoldDbDataValidatorThread)
  private
    class var FInstanceCount: Integer;
    class var FFatalError: string;
    class var FDestroyError: string;
  public
    constructor Create(AValidator: TBoldDbValidator); override;
    destructor Destroy; override;
    class procedure Reset;
    class property InstanceCount: Integer read FInstanceCount;
    class property FatalError: string read FFatalError;
    class property DestroyError: string read FDestroyError;
  end;

  TRecordingDataValidator = class(TBoldDbDataValidator)
  protected
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  end;

  { Records what Validate finds when it starts: whether the constructor has
    finished and whether the validator lists the thread. The constructor sleeps
    after the inherited one, so a thread started inside it gets well ahead.
    Validate then waits for the listing, so even a thread started too early
    ends cleanly - Execute asserts the listing. }
  TStartupValidatorThread = class(TBoldDbValidatorThread)
  private
    FConstructed: Boolean;
    class var FInstanceCount: Integer;
    class var FValidated: Boolean;
    class var FSawConstructed: Boolean;
    class var FSawListed: Boolean;
  protected
    procedure Validate; override;
  public
    constructor Create(AValidator: TBoldDbValidator); override;
    destructor Destroy; override;
    class procedure Reset;
    class property InstanceCount: Integer read FInstanceCount;
    class property Validated: Boolean read FValidated;
    class property SawConstructed: Boolean read FSawConstructed;
    class property SawListed: Boolean read FSawListed;
  end;

  TStartupDbValidator = class(TBoldDbValidator)
  protected
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  public
    function Lists(Thread: TThread): Boolean;
  end;

  { A validator thread whose Validate raises, and that records the exception
    Execute left in FatalException. }
  TRaisingValidatorThread = class(TBoldDbValidatorThread)
  private
    class var FInstanceCount: Integer;
    class var FFatalError: string;
  protected
    procedure Validate; override;
  public
    constructor Create(AValidator: TBoldDbValidator); override;
    destructor Destroy; override;
    class procedure Reset;
    class property InstanceCount: Integer read FInstanceCount;
    class property FatalError: string read FFatalError;
  end;

  TRaisingDbValidator = class(TBoldDbValidator)
  protected
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  end;

  { Keeps every line BoldLog sends it, from any thread, and counts the log
    sessions that were ended. }
  TCapturingLogReceiver = class(TBoldLogReceiver)
  private
    FLines: TStringList;
    FEndLogCount: Integer;
  protected
    procedure Log(const s: string; LogType: TBoldLogType); override;
    procedure EndLog; override;
  public
    constructor Create;
    destructor Destroy; override;
    function HasLine(LogType: TBoldLogType; const Part: string): Boolean;
    function HasExactLine(const s: string): Boolean;
    function Text: string;
    property EndLogCount: Integer read FEndLogCount;
  end;

{ TRaisingValidatorThread }

constructor TRaisingValidatorThread.Create(AValidator: TBoldDbValidator);
begin
  AtomicIncrement(FInstanceCount);
  inherited;
end;

destructor TRaisingValidatorThread.Destroy;
begin
  if Assigned(FatalException) then
    FFatalError := FatalException.ClassName;
  inherited;
  AtomicDecrement(FInstanceCount);
end;

procedure TRaisingValidatorThread.Validate;
begin
  raise EBold.Create('boom');
end;

class procedure TRaisingValidatorThread.Reset;
begin
  FInstanceCount := 0;
  FFatalError := '';
end;

{ TRaisingDbValidator }

function TRaisingDbValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  result := TRaisingValidatorThread.Create(Self);
end;

{ TCapturingLogReceiver }

constructor TCapturingLogReceiver.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TCapturingLogReceiver.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TCapturingLogReceiver.Log(const s: string; LogType: TBoldLogType);
begin
  TMonitor.Enter(FLines);
  try
    FLines.Add(Format('%d|%s', [Ord(LogType), Trim(s)]));
  finally
    TMonitor.Exit(FLines);
  end;
end;

function TCapturingLogReceiver.HasLine(LogType: TBoldLogType; const Part: string): Boolean;
var
  i: Integer;
begin
  result := False;
  TMonitor.Enter(FLines);
  try
    for i := 0 to FLines.Count - 1 do
      if StartsText(Format('%d|', [Ord(LogType)]), FLines[i]) and ContainsText(FLines[i], Part) then
        Exit(True);
  finally
    TMonitor.Exit(FLines);
  end;
end;

function TCapturingLogReceiver.HasExactLine(const s: string): Boolean;
var
  i: Integer;
begin
  result := False;
  TMonitor.Enter(FLines);
  try
    for i := 0 to FLines.Count - 1 do
      if SameText(Copy(FLines[i], Pos('|', FLines[i]) + 1, MaxInt), s) then
        Exit(True);
  finally
    TMonitor.Exit(FLines);
  end;
end;

procedure TCapturingLogReceiver.EndLog;
begin
  AtomicIncrement(FEndLogCount);
end;

function TCapturingLogReceiver.Text: string;
begin
  TMonitor.Enter(FLines);
  try
    result := FLines.Text;
  finally
    TMonitor.Exit(FLines);
  end;
end;

{ TStartupValidatorThread }

constructor TStartupValidatorThread.Create(AValidator: TBoldDbValidator);
begin
  AtomicIncrement(FInstanceCount);
  inherited;
  Sleep(200);
  FConstructed := True;
end;

destructor TStartupValidatorThread.Destroy;
begin
  inherited;
  AtomicDecrement(FInstanceCount);
end;

procedure TStartupValidatorThread.Validate;
var
  Waited: Integer;
begin
  FSawConstructed := FConstructed;
  FSawListed := (Validator as TStartupDbValidator).Lists(Self);
  FValidated := True;
  Waited := 0;
  while not (Validator as TStartupDbValidator).Lists(Self) and (Waited < 5000) do
  begin
    Sleep(1);
    Inc(Waited);
  end;
end;

class procedure TStartupValidatorThread.Reset;
begin
  FInstanceCount := 0;
  FValidated := False;
  FSawConstructed := False;
  FSawListed := False;
end;

{ TStartupDbValidator }

function TStartupDbValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  result := TStartupValidatorThread.Create(Self);
end;

function TStartupDbValidator.Lists(Thread: TThread): Boolean;
begin
  result := ThreadList.LockList.IndexOf(Thread) >= 0;
  ThreadList.UnlockList;
end;

{ TRecordingDataValidatorThread }

constructor TRecordingDataValidatorThread.Create(AValidator: TBoldDbValidator);
begin
  AtomicIncrement(FInstanceCount);
  inherited;
  ValidatorTestTypes := [ttExistenceInParentTest];
end;

destructor TRecordingDataValidatorThread.Destroy;
begin
  if Assigned(FatalException) then
    FFatalError := FatalException.ClassName;
  try
    try
      inherited;
    except
      on E: Exception do
        FDestroyError := E.ClassName;
    end;
  finally
    AtomicDecrement(FInstanceCount);
  end;
end;

class procedure TRecordingDataValidatorThread.Reset;
begin
  FInstanceCount := 0;
  FFatalError := '';
  FDestroyError := '';
end;

{ TRecordingDataValidator }

function TRecordingDataValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  result := TRecordingDataValidatorThread.Create(Self);
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

procedure TTestBoldDbValidator.TestCorruptObjectsActionDefaultsToDelete;
var
  Validator: TBoldDbDataValidator;
begin
  // Without a choice the data validator keeps suggesting DELETEs for corrupt
  // objects, as it did before the action could be chosen.
  Validator := TBoldDbDataValidator.Create(nil);
  try
    Assert.AreEqual(Ord(caDelete), Ord(Validator.CorruptObjectsAction));
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidator.TestCompletionMessage;
var
  Validator: TTestableDbValidator;
begin
  // The wording shown when a validation run ends - an error outranks remedies,
  // since the remedies of a failed run are incomplete.
  Validator := TTestableDbValidator.Create(nil);
  try
    Assert.AreEqual('Database validated OK', Validator.CompletionMessage);
    Validator.AddRemedy('DELETE FROM SOMETABLE WHERE BOLD_ID IN (1);');
    Assert.AreEqual('Database validated found problems', Validator.CompletionMessage);
    Validator.AddError('EBold: boom');
    Assert.AreEqual('Database validation failed, see log', Validator.CompletionMessage);
  finally
    Validator.Free;
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

procedure TTestBoldDbValidatorEndToEnd.HandleCompleteThenRaise(Sender: TObject);
begin
  FDone.SetEvent;
  raise EBold.Create('raised by OnComplete');
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
    Assert.AreEqual(0, Validator.Errors.Count, 'the run reported errors:' + sLineBreak + Validator.Errors.Text);
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestDataValidatorThreadEndsCleanly;
var
  Validator: TRecordingDataValidator;
  Waited: Integer;
begin
  // Execute releases the thread's own connection when validation is done.
  // Neither Execute's cleanup nor the thread's destructor may use it after
  // that - the query the data validator opened on it included.
  TRecordingDataValidatorThread.Reset;
  Validator := TRecordingDataValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.ClassesToValidate := 'SomeClass';
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator thread must complete and call OnComplete');

    Waited := 0;
    while (TRecordingDataValidatorThread.InstanceCount > 0) and (Waited < 5000) do
    begin
      Sleep(50);
      Inc(Waited, 50);
    end;
    Assert.AreEqual(0, TRecordingDataValidatorThread.InstanceCount, 'the validator thread must be destroyed');
    Assert.IsTrue((TRecordingDataValidatorThread.FatalError = '') and (TRecordingDataValidatorThread.DestroyError = ''),
      Format('Execute ended with [%s], the destructor raised [%s]',
        [TRecordingDataValidatorThread.FatalError, TRecordingDataValidatorThread.DestroyError]));
    // Execute catches what Validate and its cleanup raise into Errors, so a
    // clean end needs that list to be empty as well.
    Assert.AreEqual(0, Validator.Errors.Count, 'the run reported errors:' + sLineBreak + Validator.Errors.Text);
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestInsertRemedyPutsScriptSeparatorOnItsOwnLine;
var
  Validator: TBoldDbDataValidator;
  SomeObject: TSomeClass;
  SystemMapper: TBoldSystemSQLMapper;
  OwnTable, ParentTable, ObjectId, OldSeparator: string;
  Remedy: TStringList;
  i, InsertAt: Integer;
begin
  // A SomeClass row whose parent table row is gone makes a validator set to
  // caInsert suggest an INSERT for the parent table. Script tools only honour
  // a batch separator such as GO on a line of its own, so it must not trail
  // the INSERT.
  SystemMapper := dmUndoRedo.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper;
  for i := 0 to SystemMapper.ObjectPersistenceMappers.Count - 1 do
    if Assigned(SystemMapper.ObjectPersistenceMappers[i]) and
      SameText(SystemMapper.ObjectPersistenceMappers[i].ExpressionName, 'SomeClass') then
    begin
      OwnTable := (SystemMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper).MainTable.SQLName;
      ParentTable := (SystemMapper.ObjectPersistenceMappers[i].SuperClass as TBoldObjectSQLMapper).MainTable.SQLName;
    end;
  Assert.AreNotEqual('', ParentTable, 'precondition: SomeClass has a parent table');

  SomeObject := TSomeClass.Create(dmUndoRedo.BoldSystemHandle1.System);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  ObjectId := SomeObject.BoldObjectLocator.BoldObjectID.AsString;
  Assert.AreEqual(1, dmUndoRedo.FDConnection1.ExecSQL('DELETE FROM ' + ParentTable + ' WHERE BOLD_ID = ' + ObjectId),
    'precondition: the parent table holds the new object');

  OldSeparator := dmUndoRedo.BoldPersistenceHandleDB2.SQLDataBaseConfig.SqlScriptSeparator;
  dmUndoRedo.BoldPersistenceHandleDB2.SQLDataBaseConfig.SqlScriptSeparator := 'GO';
  Validator := TBoldDbDataValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.ClassesToValidate := 'SomeClass';
    Validator.ValidatorTestTypes := [ttExistenceInParentTest];
    Validator.CorruptObjectsAction := caInsert;
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator thread must complete and call OnComplete');

    Remedy := Validator.Remedy;
    InsertAt := -1;
    for i := 0 to Remedy.Count - 1 do
      if StartsText('INSERT INTO', Remedy[i]) then
      begin
        InsertAt := i;
        Break;
      end;
    Assert.IsTrue(InsertAt >= 0, 'expected an INSERT remedy:' + sLineBreak + Remedy.Text);
    Assert.IsFalse(EndsText('GO', Remedy[InsertAt]), 'GO trails the INSERT:' + sLineBreak + Remedy.Text);
    Assert.IsTrue(InsertAt + 1 < Remedy.Count, 'no separator after the INSERT:' + sLineBreak + Remedy.Text);
    Assert.AreEqual('GO', Remedy[InsertAt + 1]);
  finally
    Validator.Free;
    dmUndoRedo.BoldPersistenceHandleDB2.SQLDataBaseConfig.SqlScriptSeparator := OldSeparator;
    dmUndoRedo.FDConnection1.ExecSQL('DELETE FROM ' + OwnTable + ' WHERE BOLD_ID = ' + ObjectId);
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestValidatorThreadStartsAfterConstructionAndListing;
var
  Validator: TStartupDbValidator;
  Waited: Integer;
begin
  // A validator thread may only run once its constructors have finished -
  // descendants set up what Validate uses there - and once the validator
  // lists it, which Execute asserts when it ends.
  TStartupValidatorThread.Reset;
  Validator := TStartupDbValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator thread must complete and call OnComplete');
    Waited := 0;
    while (TStartupValidatorThread.InstanceCount > 0) and (Waited < 5000) do
    begin
      Sleep(50);
      Inc(Waited, 50);
    end;
    Assert.IsTrue(TStartupValidatorThread.Validated, 'Validate must have run');
    Assert.IsTrue(TStartupValidatorThread.SawConstructed and TStartupValidatorThread.SawListed,
      Format('Validate started with the constructor finished: %s, listed by the validator: %s',
        [BoolToStr(TStartupValidatorThread.SawConstructed, True), BoolToStr(TStartupValidatorThread.SawListed, True)]));
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestWorkerExceptionIsReported;
var
  Validator: TRaisingDbValidator;
  Waited: Integer;
begin
  // An exception in a worker must end up in Errors instead of disappearing
  // into the thread's FatalException, and the run must still complete.
  TRaisingValidatorThread.Reset;
  Validator := TRaisingDbValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator must complete and call OnComplete');
    Waited := 0;
    while (TRaisingValidatorThread.InstanceCount > 0) and (Waited < 5000) do
    begin
      Sleep(50);
      Inc(Waited, 50);
    end;
    Assert.AreEqual('', TRaisingValidatorThread.FatalError, 'Execute must catch the exception');
    Assert.AreEqual(1, Validator.Errors.Count, Validator.Errors.Text);
    Assert.IsTrue(ContainsText(Validator.Errors[0], 'EBold') and ContainsText(Validator.Errors[0], 'boom'),
      Validator.Errors[0]);
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestWorkerExceptionIsLoggedAsError;
var
  Validator: TRaisingDbValidator;
  Capture: TCapturingLogReceiver;
  Receiver: IBoldLogReceiver;
  Waited: Integer;
begin
  // The log of a run must show a worker's exception as an error and must not
  // claim that the validation finished.
  TRaisingValidatorThread.Reset;
  Capture := TCapturingLogReceiver.Create;
  // The interface keeps the receiver alive after unregistering it.
  Receiver := Capture;
  BoldLog.RegisterLogReceiver(Receiver);
  Validator := TRaisingDbValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator must complete and call OnComplete');
    Waited := 0;
    while (TRaisingValidatorThread.InstanceCount > 0) and (Waited < 5000) do
    begin
      Sleep(50);
      Inc(Waited, 50);
    end;
    Assert.IsTrue(Capture.HasLine(ltError, 'boom'), 'no error line for the exception:' + sLineBreak + Capture.Text);
    Assert.IsFalse(Capture.HasExactLine(sDBValidationDone), 'a failed run reported as finished:' + sLineBreak + Capture.Text);
  finally
    Validator.Free;
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestBrokenClassDoesNotStopTheOthers;
const
  cClasses: array[0..2] of string = ('SomeClass', 'Book', 'Topic');
var
  Validator: TBoldDbDataValidator;
  SystemMapper: TBoldSystemSQLMapper;
  Mappers: array[0..2] of TBoldObjectSQLMapper;
  Indexes: array[0..2] of Integer;
  Mapper: TBoldObjectSQLMapper;
  Orphan: TBoldObject;
  Remedy: string;
  i, j, Index: Integer;
begin
  // One worker validates three classes. The table of the middle one (by mapper
  // order) is gone, so validating it raises; the outer two each have an object
  // without its parent row. Whichever way the worker walks the queue, a class
  // with a remedy comes after the broken one - and must still be validated.
  SystemMapper := dmUndoRedo.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper;
  for i := 0 to High(cClasses) do
  begin
    Mappers[i] := nil;
    Indexes[i] := -1;
    for j := 0 to SystemMapper.ObjectPersistenceMappers.Count - 1 do
      if Assigned(SystemMapper.ObjectPersistenceMappers[j]) and
        SameText(SystemMapper.ObjectPersistenceMappers[j].ExpressionName, cClasses[i]) then
      begin
        Mappers[i] := SystemMapper.ObjectPersistenceMappers[j] as TBoldObjectSQLMapper;
        Indexes[i] := j;
      end;
    Assert.IsNotNull(Mappers[i], 'precondition: a mapper for ' + cClasses[i]);
  end;
  // Order the three by their position among the system's mappers.
  for i := 0 to High(Mappers) - 1 do
    for j := i + 1 to High(Mappers) do
      if Indexes[j] < Indexes[i] then
      begin
        Mapper := Mappers[i];
        Mappers[i] := Mappers[j];
        Mappers[j] := Mapper;
        Index := Indexes[i];
        Indexes[i] := Indexes[j];
        Indexes[j] := Index;
      end;

  for i in [0, 2] do
  begin
    Orphan := dmUndoRedo.BoldSystemHandle1.System.CreateNewObjectByExpressionName(Mappers[i].ExpressionName);
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    Assert.AreEqual(1, dmUndoRedo.FDConnection1.ExecSQL(
      'DELETE FROM ' + (Mappers[i].SuperClass as TBoldObjectSQLMapper).MainTable.SQLName +
      ' WHERE BOLD_ID = ' + Orphan.BoldObjectLocator.BoldObjectID.AsString),
      'precondition: the parent table holds the new ' + Mappers[i].ExpressionName);
  end;
  dmUndoRedo.FDConnection1.ExecSQL('DROP TABLE ' + Mappers[1].MainTable.SQLName);

  Validator := TBoldDbDataValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.ClassesToValidate := Mappers[0].ExpressionName + ',' + Mappers[1].ExpressionName + ',' +
      Mappers[2].ExpressionName;
    Validator.ValidatorTestTypes := [ttExistenceInParentTest];
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator must complete and call OnComplete');

    Remedy := Validator.Remedy.Text;
    for i in [0, 2] do
      Assert.IsTrue(ContainsText(Remedy, 'class ' + Mappers[i].ExpressionName + ' missing parent'),
        Mappers[i].ExpressionName + ' was not validated:' + sLineBreak + Remedy);
    Assert.IsTrue(ContainsText(Validator.Errors.Text, Mappers[1].ExpressionName),
      'no error for ' + Mappers[1].ExpressionName + ':' + sLineBreak + Validator.Errors.Text);
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestRerunReportsOnlyItsOwnResults;
var
  Validator: TRaisingDbValidator;
  Run, Waited: Integer;
begin
  // The DB actions keep their validator and execute it again for every run.
  // A run must report its own errors and remedies only, not those of earlier
  // runs.
  Validator := TRaisingDbValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.OnComplete := HandleComplete;
    for Run := 1 to 2 do
    begin
      TRaisingValidatorThread.Reset;
      FDone.ResetEvent;
      Validator.Execute;
      Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
        Format('run %d must complete and call OnComplete', [Run]));
      Waited := 0;
      while (TRaisingValidatorThread.InstanceCount > 0) and (Waited < 5000) do
      begin
        Sleep(50);
        Inc(Waited, 50);
      end;
      if Run = 1 then
        Validator.AddRemedy('-- left over from run 1');
    end;
    Assert.AreEqual(1, Validator.Errors.Count, 'errors of run 2:' + sLineBreak + Validator.Errors.Text);
    Assert.AreEqual(0, Validator.Remedy.Count, 'remedies of run 2:' + sLineBreak + Validator.Remedy.Text);
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestOnCompleteExceptionIsLoggedAndLogEnded;
var
  Validator: TRaisingDbValidator;
  Capture: TCapturingLogReceiver;
  Receiver: IBoldLogReceiver;
  Waited: Integer;
begin
  // Completing a run calls code the validator does not control - here the
  // caller's OnComplete raises. The exception must be logged instead of ending
  // in the thread's FatalException, and the log session must still be closed.
  TRaisingValidatorThread.Reset;
  Capture := TCapturingLogReceiver.Create;
  // The interface keeps the receiver alive after unregistering it.
  Receiver := Capture;
  BoldLog.RegisterLogReceiver(Receiver);
  Validator := TRaisingDbValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.OnComplete := HandleCompleteThenRaise;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator must complete and call OnComplete');
    Waited := 0;
    while (TRaisingValidatorThread.InstanceCount > 0) and (Waited < 5000) do
    begin
      Sleep(50);
      Inc(Waited, 50);
    end;
    Assert.AreEqual('', TRaisingValidatorThread.FatalError, 'the OnComplete exception ended in FatalException');
    Assert.IsTrue(Capture.HasLine(ltError, 'raised by OnComplete'),
      'no error line for the OnComplete exception:' + sLineBreak + Capture.Text);
    Assert.IsTrue(Capture.EndLogCount > 0, 'the log session was not ended');
  finally
    Validator.Free;
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.RunWithTableDropped(const ExpressionName: string;
  TestTypes: TBoldDBDataValidatorTestTypes);
var
  Validator: TBoldDbDataValidator;
  SystemMapper: TBoldSystemSQLMapper;
  Table: string;
  i: Integer;
begin
  // The table of the class is gone, so the selected test fails on its query
  // after it has started collecting ids. The failure must be reported - and
  // the run log of this exe must not show a leaked list afterwards.
  SystemMapper := dmUndoRedo.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper;
  Table := '';
  for i := 0 to SystemMapper.ObjectPersistenceMappers.Count - 1 do
    if Assigned(SystemMapper.ObjectPersistenceMappers[i]) and
      SameText(SystemMapper.ObjectPersistenceMappers[i].ExpressionName, ExpressionName) then
      Table := (SystemMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper).MainTable.SQLName;
  Assert.AreNotEqual('', Table, 'precondition: a table for ' + ExpressionName);
  dmUndoRedo.FDConnection1.ExecSQL('DROP TABLE ' + Table);

  Validator := TBoldDbDataValidator.Create(nil);
  try
    Validator.PersistenceHandle := dmUndoRedo.BoldPersistenceHandleDB2;
    Validator.ThreadCount := 1;
    Validator.ClassesToValidate := ExpressionName;
    Validator.ValidatorTestTypes := TestTypes;
    Validator.OnComplete := HandleComplete;
    Validator.Execute;
    Assert.AreEqual(Ord(wrSignaled), Ord(FDone.WaitFor(10000)),
      'the validator must complete and call OnComplete');
    Assert.IsTrue(ContainsText(Validator.Errors.Text, 'Validating ' + ExpressionName + ' failed'),
      'no error for ' + ExpressionName + ':' + sLineBreak + Validator.Errors.Text);
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldDbValidatorEndToEnd.TestFailingRelationValidationIsReported;
begin
  // ValidateRelations creates its id list before the relation query fails.
  RunWithTableDropped('SomeClass', [ttRelationTest]);
end;

procedure TTestBoldDbValidatorEndToEnd.TestFailingLinkObjectValidationIsReported;
begin
  // ValidateLinkObjects creates its id list before the link object query fails.
  RunWithTableDropped('topicbook', [ttLinkObjectTest]);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbValidator);
  TDUnitX.RegisterTestFixture(TTestBoldDbValidatorEndToEnd);

end.
