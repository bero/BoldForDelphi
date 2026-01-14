unit Test.BoldStubs;

interface

uses
  DUnitX.TestFramework,
  BoldStubs;

type
  [TestFixture]
  TTestBoldStubs = class
  public
    [Test]
    procedure TestTraceLogAssigned;
    [Test]
    procedure TestTraceLogReturnsInstance;
    [Test]
    procedure TestTraceLogReturnsSameInstance;
    [Test]
    procedure TestTraceLogSystemMessage;
    [Test]
    procedure TestTraceLogSystemMessageWithFormat;
    [Test]
    procedure TestTraceLogLogFileDir;
    [Test]
    procedure TestInSpanFetch_ReturnsFalse;
    [Test]
    procedure TestPrefetchDerivedMember_DoesNotRaise;
  end;

implementation

uses
  SysUtils;

{ TTestBoldStubs }

procedure TTestBoldStubs.TestTraceLogAssigned;
begin
  Assert.IsTrue(TraceLogAssigned);
end;

procedure TTestBoldStubs.TestTraceLogReturnsInstance;
begin
  Assert.IsNotNull(TraceLog);
end;

procedure TTestBoldStubs.TestTraceLogReturnsSameInstance;
var
  Log1, Log2: TTraceLog;
begin
  Log1 := TraceLog;
  Log2 := TraceLog;
  Assert.AreSame(Log1, Log2);
end;

procedure TTestBoldStubs.TestTraceLogSystemMessage;
begin
  // Should not raise, delegates to BoldLog
  TraceLog.SystemMessage('Test message', ekInfo);
  TraceLog.SystemMessage('Warning message', ekWarning);
  TraceLog.SystemMessage('Error message', ekError);
  TraceLog.SystemMessage('Debug message', ekDebug);
  Assert.Pass;
end;

procedure TTestBoldStubs.TestTraceLogSystemMessageWithFormat;
begin
  // Should not raise, delegates to BoldLog
  TraceLog.SystemMessage('Test %s %d', ['string', 42], ekInfo);
  TraceLog.SystemMessage('Warning %d', [123], ekWarning);
  Assert.Pass;
end;

procedure TTestBoldStubs.TestTraceLogLogFileDir;
var
  Dir: string;
begin
  Dir := TraceLog.LogFileDir;
  Assert.IsNotEmpty(Dir);
  // Should return path of executable
  Assert.AreEqual(ExtractFilePath(ParamStr(0)), Dir);
end;

procedure TTestBoldStubs.TestInSpanFetch_ReturnsFalse;
begin
  Assert.IsFalse(InSpanFetch);
end;

procedure TTestBoldStubs.TestPrefetchDerivedMember_DoesNotRaise;
begin
  // Should not raise - it's a no-op stub
  PrefetchDerivedMember(nil);
  Assert.Pass;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldStubs);

end.
