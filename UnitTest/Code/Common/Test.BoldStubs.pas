unit Test.BoldStubs;

interface

uses
  BoldTestFramework,
  BoldStubs;

type
  TTestBoldStubs = class(TBoldTestCase)
  published
    procedure TestTraceLogAssigned;
    procedure TestTraceLogReturnsInstance;
    procedure TestTraceLogReturnsSameInstance;
    procedure TestTraceLogSystemMessage;
    procedure TestTraceLogSystemMessageWithFormat;
    procedure TestTraceLogLogFileDir;
    procedure TestInSpanFetch_ReturnsFalse;
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
  BoldRegisterTest(TTestBoldStubs);

end.
