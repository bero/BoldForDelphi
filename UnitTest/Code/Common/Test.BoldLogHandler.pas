unit Test.BoldLogHandler;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestBoldLogHandler = class
  public
    // Create and basics
    [Test] [Category('Quick')]
    procedure TestLog;
    [Test] [Category('Quick')]
    procedure TestLogFmt;

    // Indent / Dedent
    [Test] [Category('Quick')]
    procedure TestIndentDedent;
    [Test] [Category('Quick')]
    procedure TestDedentFloor;
    [Test] [Category('Quick')]
    procedure TestLogIndent;
    [Test] [Category('Quick')]
    procedure TestLogDedent;
    [Test] [Category('Quick')]
    procedure TestLogFmtDedent;

    // Separator deduplication
    [Test] [Category('Quick')]
    procedure TestSeparator;
    [Test] [Category('Quick')]
    procedure TestSeparatorDedup;

    // Clear / Show / Hide / Sync
    [Test] [Category('Quick')]
    procedure TestClear;
    [Test] [Category('Quick')]
    procedure TestShow;
    [Test] [Category('Quick')]
    procedure TestHide;
    [Test] [Category('Quick')]
    procedure TestSync;

    // StartLog / EndLog
    [Test] [Category('Quick')]
    procedure TestStartLogEndLog;

    // Progress
    [Test] [Category('Quick')]
    procedure TestProgressStep;
    [Test] [Category('Quick')]
    procedure TestProgressMax;
    [Test] [Category('Quick')]
    procedure TestSetProgress;
    [Test] [Category('Quick')]
    procedure TestSetLogHeader;

    // Interrupt
    [Test] [Category('Quick')]
    procedure TestProcessInterruption_NotInterrupted;
    [Test] [Category('Quick')]
    procedure TestInterruptProcess;
    [Test] [Category('Quick')]
    procedure TestProcessInterruption_HandledOnce;

    // RegisterLogReceiver / UnregisterLogReceiver
    [Test] [Category('Quick')]
    procedure TestRegisterAndUnregisterLogReceiver;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldLogHandler,
  BoldLogReceiverInterface;

type
  TTestLogReceiver = class(TBoldLogReceiver)
  public
    LastLogMessage: string;
    LastLogType: TBoldLogType;
    ClearCalled: Boolean;
    ShowCalled: Boolean;
    HideCalled: Boolean;
    SyncCalled: Boolean;
    StartLogSession: string;
    EndLogCalled: Boolean;
    ProgressStepCalled: Boolean;
    LastProgressMax: Integer;
    LastProgress: Integer;
    LastLogHeader: string;
    ProcessInterruptionCalled: Boolean;
    procedure Reset;
  protected
    procedure Log(const s: string; LogType: TBoldLogType); override;
    procedure Clear; override;
    procedure Show; override;
    procedure Hide; override;
    procedure Sync; override;
    procedure StartLog(const SessionName: String); override;
    procedure EndLog; override;
    procedure ProgressStep; override;
    procedure SetProgressMax(const Value: integer); override;
    procedure SetProgress(const Value: integer); override;
    procedure SetLogHeader(const Value: string); override;
    procedure ProcessInterruption; override;
  end;

procedure TTestLogReceiver.Reset;
begin
  LastLogMessage := '';
  LastLogType := ltInfo;
  ClearCalled := False;
  ShowCalled := False;
  HideCalled := False;
  SyncCalled := False;
  StartLogSession := '';
  EndLogCalled := False;
  ProgressStepCalled := False;
  LastProgressMax := 0;
  LastProgress := 0;
  LastLogHeader := '';
  ProcessInterruptionCalled := False;
end;

procedure TTestLogReceiver.Log(const s: string; LogType: TBoldLogType);
begin
  LastLogMessage := s;
  LastLogType := LogType;
end;

procedure TTestLogReceiver.Clear;
begin
  ClearCalled := True;
end;

procedure TTestLogReceiver.Show;
begin
  ShowCalled := True;
end;

procedure TTestLogReceiver.Hide;
begin
  HideCalled := True;
end;

procedure TTestLogReceiver.Sync;
begin
  SyncCalled := True;
end;

procedure TTestLogReceiver.StartLog(const SessionName: String);
begin
  StartLogSession := SessionName;
end;

procedure TTestLogReceiver.EndLog;
begin
  EndLogCalled := True;
end;

procedure TTestLogReceiver.ProgressStep;
begin
  ProgressStepCalled := True;
end;

procedure TTestLogReceiver.SetProgressMax(const Value: integer);
begin
  LastProgressMax := Value;
end;

procedure TTestLogReceiver.SetProgress(const Value: integer);
begin
  LastProgress := Value;
end;

procedure TTestLogReceiver.SetLogHeader(const Value: string);
begin
  LastLogHeader := Value;
end;

procedure TTestLogReceiver.ProcessInterruption;
begin
  ProcessInterruptionCalled := True;
end;

{ TTestBoldLogHandler }

// All tests use BoldLog (the global singleton) because
// TBoldLogReceiverSubscriber subscribes to BoldLog, not to local instances.

procedure TTestBoldLogHandler.TestLog;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Log('Hello', ltInfo);
    Assert.AreEqual('Hello', Receiver.LastLogMessage);
    Assert.AreEqual(Ord(ltInfo), Ord(Receiver.LastLogType));

    BoldLog.Log('Warning msg', ltWarning);
    Assert.AreEqual('Warning msg', Receiver.LastLogMessage);
    Assert.AreEqual(Ord(ltWarning), Ord(Receiver.LastLogType));
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestLogFmt;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.LogFmt('Count: %d', [42], ltInfo);
    Assert.AreEqual('Count: 42', Receiver.LastLogMessage);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestIndentDedent;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Log('Level0');
    Assert.AreEqual('Level0', Receiver.LastLogMessage);

    BoldLog.Indent;
    BoldLog.Log('Level1');
    Assert.AreEqual('  Level1', Receiver.LastLogMessage);

    BoldLog.Indent;
    BoldLog.Log('Level2');
    Assert.AreEqual('    Level2', Receiver.LastLogMessage);

    BoldLog.Dedent;
    BoldLog.Log('Back1');
    Assert.AreEqual('  Back1', Receiver.LastLogMessage);

    BoldLog.Dedent;
    BoldLog.Log('Back0');
    Assert.AreEqual('Back0', Receiver.LastLogMessage);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestDedentFloor;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Dedent;
    BoldLog.Dedent;
    BoldLog.Dedent;
    BoldLog.Log('StillZero');
    Assert.AreEqual('StillZero', Receiver.LastLogMessage);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestLogIndent;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.LogIndent('Starting');
    Assert.AreEqual('Starting', Receiver.LastLogMessage);
    BoldLog.Log('Indented');
    Assert.AreEqual('  Indented', Receiver.LastLogMessage);
    BoldLog.Dedent;
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestLogDedent;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Indent;
    BoldLog.LogDedent('Ending');
    Assert.AreEqual('Ending', Receiver.LastLogMessage);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestLogFmtDedent;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Indent;
    BoldLog.LogFmtDedent('Done: %s', ['OK'], ltInfo);
    Assert.AreEqual('Done: OK', Receiver.LastLogMessage);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestSeparator;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Log('Something');
    BoldLog.Separator;
    Assert.AreEqual(Ord(ltSeparator), Ord(Receiver.LastLogType));
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestSeparatorDedup;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    // After a Separator, calling Separator again should be deduped
    BoldLog.Log('Text');
    BoldLog.Separator;
    Assert.AreEqual(Ord(ltSeparator), Ord(Receiver.LastLogType));

    // Second consecutive Separator should be skipped
    Receiver.LastLogType := ltInfo;
    BoldLog.Separator;
    Assert.AreEqual(Ord(ltInfo), Ord(Receiver.LastLogType), 'Duplicate separator should be skipped');
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestClear;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Clear;
    Assert.IsTrue(Receiver.ClearCalled);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestShow;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Show;
    Assert.IsTrue(Receiver.ShowCalled);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestHide;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Hide;
    Assert.IsTrue(Receiver.HideCalled);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestSync;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Sync;
    Assert.IsTrue(Receiver.SyncCalled);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestStartLogEndLog;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.StartLog('TestSession');
    Assert.AreEqual('TestSession', Receiver.StartLogSession);

    BoldLog.EndLog;
    Assert.IsTrue(Receiver.EndLogCalled);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestProgressStep;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.ProgressStep;
    Assert.IsTrue(Receiver.ProgressStepCalled);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestProgressMax;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.ProgressMax := 100;
    Assert.AreEqual(100, Receiver.LastProgressMax);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestSetProgress;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.Progress := 50;
    Assert.AreEqual(50, Receiver.LastProgress);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestSetLogHeader;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.LogHeader := 'MyHeader';
    Assert.AreEqual('MyHeader', Receiver.LastLogHeader);
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestProcessInterruption_NotInterrupted;
begin
  Assert.IsFalse(BoldLog.ProcessInterruption);
end;

procedure TTestBoldLogHandler.TestInterruptProcess;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.InterruptProcess;
    Assert.IsTrue(Pos('abort', LowerCase(Receiver.LastLogMessage)) > 0);
    Assert.IsTrue(BoldLog.ProcessInterruption);
    // Reset interrupted state via StartLog
    BoldLog.StartLog('Reset');
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestProcessInterruption_HandledOnce;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  try
    BoldLog.InterruptProcess;

    // First call logs 'Process stopped'
    Assert.IsTrue(BoldLog.ProcessInterruption);
    Assert.IsTrue(Pos('stopped', LowerCase(Receiver.LastLogMessage)) > 0);

    // Second call should NOT log again (fInterruptHandled = true)
    Receiver.LastLogMessage := '';
    Assert.IsTrue(BoldLog.ProcessInterruption);
    Assert.AreEqual('', Receiver.LastLogMessage, 'Should not log stopped twice');

    // Reset interrupted state
    BoldLog.StartLog('Reset');
  finally
    BoldLog.UnregisterLogReceiver(Receiver);
  end;
end;

procedure TTestBoldLogHandler.TestRegisterAndUnregisterLogReceiver;
var
  Receiver: TTestLogReceiver;
begin
  Receiver := TTestLogReceiver.Create;
  BoldLog.RegisterLogReceiver(Receiver);
  BoldLog.Log('Before');
  Assert.AreEqual('Before', Receiver.LastLogMessage);

  BoldLog.UnregisterLogReceiver(Receiver);
  // After unregister, receiver should not get events
  Receiver.LastLogMessage := '';
  BoldLog.Log('After');
  Assert.AreEqual('', Receiver.LastLogMessage, 'Should not receive after unregister');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldLogHandler);

end.
