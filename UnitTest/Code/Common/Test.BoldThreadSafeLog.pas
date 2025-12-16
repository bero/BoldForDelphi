unit Test.BoldThreadSafeLog;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldThreadSafeLog = class
  private
    FTempDir: string;
    function GetTempLogFile(const Suffix: string): string;
    function ReadLogFile(const FileName: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestFileLoggingCreate;
    [Test]
    procedure TestFileLoggingTrace;
    [Test]
    procedure TestFileLoggingTraceWithArgs;
    [Test]
    procedure TestFileLoggingIncludeThreadId;
    [Test]
    procedure TestFileLoggingIncludeDate;
    [Test]
    procedure TestFileLoggingMaxSize;
    [Test]
    procedure TestFileLoggingOpenClose;
    [Test]
    procedure TestBoldLoggerCreate;
    [Test]
    procedure TestBoldLoggerLog;
    [Test]
    procedure TestBoldLoggerLogError;
    [Test]
    procedure TestBoldLoggerLogFmt;
    [Test]
    procedure TestGlobalBoldLog;
    [Test]
    procedure TestBoldLoggerLogThread;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  BoldThreadSafeLog;

{ TTestBoldThreadSafeLog }

procedure TTestBoldThreadSafeLog.Setup;
begin
  FTempDir := TPath.Combine(TPath.GetTempPath, 'BoldLogTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);
end;

procedure TTestBoldThreadSafeLog.TearDown;
begin
  BoldDoneLog;
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

function TTestBoldThreadSafeLog.GetTempLogFile(const Suffix: string): string;
begin
  Result := TPath.Combine(FTempDir, 'test_' + Suffix + '.log');
end;

function TTestBoldThreadSafeLog.ReadLogFile(const FileName: string): string;
begin
  if TFile.Exists(FileName) then
    Result := TFile.ReadAllText(FileName, TEncoding.UTF8)
  else
    Result := '';
end;

procedure TTestBoldThreadSafeLog.TestFileLoggingCreate;
var
  Log: TFileLogging;
  LogFile: string;
begin
  LogFile := GetTempLogFile('create');
  Log := TFileLogging.Create(LogFile, 1024 * 1024, True);
  try
    Assert.AreEqual(LogFile, Log.FileName);
    Assert.IsTrue(Log.Open);
    Assert.IsTrue(TFile.Exists(LogFile));
  finally
    Log.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestFileLoggingTrace;
var
  Log: TFileLogging;
  LogFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('trace');
  Log := TFileLogging.Create(LogFile, 1024 * 1024, True);
  try
    Log.Trace('Test message');
    Log.Free;
    Log := nil;

    Content := ReadLogFile(LogFile);
    Assert.Contains(Content, 'Test message');
  finally
    Log.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestFileLoggingTraceWithArgs;
var
  Log: TFileLogging;
  LogFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('trace_args');
  Log := TFileLogging.Create(LogFile, 1024 * 1024, True);
  try
    Log.Trace('Value is %d and string is %s', [42, 'hello']);
    Log.Free;
    Log := nil;

    Content := ReadLogFile(LogFile);
    Assert.Contains(Content, 'Value is 42 and string is hello');
  finally
    Log.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestFileLoggingIncludeThreadId;
var
  Log: TFileLogging;
  LogFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('threadid');
  Log := TFileLogging.Create(LogFile, 1024 * 1024, True);
  try
    Log.IncludeThreadId := True;
    Log.Trace('With thread ID');
    Log.Free;
    Log := nil;

    Content := ReadLogFile(LogFile);
    Assert.Contains(Content, 'ThreadID=');
  finally
    Log.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestFileLoggingIncludeDate;
var
  Log: TFileLogging;
  LogFile: string;
  Content: string;
  Today: string;
begin
  LogFile := GetTempLogFile('date');
  Log := TFileLogging.Create(LogFile, 1024 * 1024, True);
  try
    Log.IncludeDate := True;
    Log.Trace('With date');
    Log.Free;
    Log := nil;

    Content := ReadLogFile(LogFile);
    Today := FormatDateTime('yyyy-mm-dd', Now);
    Assert.Contains(Content, Today);
  finally
    Log.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestFileLoggingMaxSize;
var
  Log: TFileLogging;
  LogFile: string;
  i: Integer;
begin
  LogFile := GetTempLogFile('maxsize');
  // Small max size to trigger flush
  Log := TFileLogging.Create(LogFile, 500, True);
  try
    // Write enough to exceed max size
    for i := 1 to 20 do
      Log.Trace('Line %d: This is a test message that adds content', [i]);

    Log.Free;
    Log := nil;

    // File should exist and be smaller than it would be without flush
    Assert.IsTrue(TFile.Exists(LogFile));
  finally
    Log.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestFileLoggingOpenClose;
var
  Log: TFileLogging;
  LogFile: string;
begin
  LogFile := GetTempLogFile('openclose');
  Log := TFileLogging.Create(LogFile, 1024 * 1024, False);
  try
    Assert.IsFalse(Log.Open);

    Log.Open := True;
    Assert.IsTrue(Log.Open);
    Assert.IsTrue(TFile.Exists(LogFile));

    Log.Trace('After opening');

    Log.Open := False;
    Assert.IsFalse(Log.Open);
  finally
    Log.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestBoldLoggerCreate;
var
  Logger: TBoldLogger;
  LogFile, ErrorFile, ThreadFile: string;
begin
  LogFile := GetTempLogFile('logger_main');
  ErrorFile := GetTempLogFile('logger_error');
  ThreadFile := GetTempLogFile('logger_thread');

  Logger := TBoldLogger.Create(LogFile, ErrorFile, ThreadFile, 1024 * 1024);
  try
    Assert.IsTrue(TFile.Exists(LogFile));
    Assert.IsTrue(TFile.Exists(ErrorFile));
    Assert.IsTrue(TFile.Exists(ThreadFile));
  finally
    Logger.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestBoldLoggerLog;
var
  Logger: TBoldLogger;
  LogFile, ErrorFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('logger_log');
  ErrorFile := GetTempLogFile('logger_log_err');

  Logger := TBoldLogger.Create(LogFile, ErrorFile, '', 1024 * 1024);
  try
    Logger.Log('Normal log message');
    Logger.Free;
    Logger := nil;

    Content := ReadLogFile(LogFile);
    Assert.Contains(Content, 'Normal log message');
  finally
    Logger.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestBoldLoggerLogError;
var
  Logger: TBoldLogger;
  LogFile, ErrorFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('logger_err_main');
  ErrorFile := GetTempLogFile('logger_err');

  Logger := TBoldLogger.Create(LogFile, ErrorFile, '', 1024 * 1024);
  try
    Logger.LogError('Error message');
    Logger.Free;
    Logger := nil;

    Content := ReadLogFile(ErrorFile);
    Assert.Contains(Content, 'Error message');
  finally
    Logger.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestBoldLoggerLogFmt;
var
  Logger: TBoldLogger;
  LogFile, ErrorFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('logger_fmt');
  ErrorFile := GetTempLogFile('logger_fmt_err');

  Logger := TBoldLogger.Create(LogFile, ErrorFile, '', 1024 * 1024);
  try
    Logger.LogFmt('Count: %d, Name: %s', [123, 'Test']);
    Logger.Free;
    Logger := nil;

    Content := ReadLogFile(LogFile);
    Assert.Contains(Content, 'Count: 123, Name: Test');
  finally
    Logger.Free;
  end;
end;

procedure TTestBoldThreadSafeLog.TestGlobalBoldLog;
var
  LogFile, ErrorFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('global_log');
  ErrorFile := GetTempLogFile('global_err');

  BoldInitLog(LogFile, ErrorFile, '', 1024 * 1024);
  try
    BoldLog('Global log message');
    BoldLog('Formatted: %d', [999]);
    BoldLogError('Global error');
    BoldLogError('Error: %s', ['details']);
  finally
    BoldDoneLog;
  end;

  Content := ReadLogFile(LogFile);
  Assert.Contains(Content, 'Global log message');
  Assert.Contains(Content, 'Formatted: 999');

  Content := ReadLogFile(ErrorFile);
  Assert.Contains(Content, 'Global error');
  Assert.Contains(Content, 'Error: details');
end;

procedure TTestBoldThreadSafeLog.TestBoldLoggerLogThread;
var
  Logger: TBoldLogger;
  LogFile, ErrorFile, ThreadFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('logger_thread_main');
  ErrorFile := GetTempLogFile('logger_thread_err');
  ThreadFile := GetTempLogFile('logger_thread');

  Logger := TBoldLogger.Create(LogFile, ErrorFile, ThreadFile, 1024 * 1024);
  try
    Logger.LogThread('Thread activity message');
    Logger.Free;
    Logger := nil;

    Content := ReadLogFile(ThreadFile);
    Assert.Contains(Content, 'Thread activity message');
    // Thread log should include thread ID (short format)
    Assert.Contains(Content, 'TID=');
  finally
    Logger.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldThreadSafeLog);

end.
