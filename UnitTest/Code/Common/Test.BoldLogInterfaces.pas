unit Test.BoldLogInterfaces;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldLogInterfaces = class
  private
    FTempDir: string;
    function GetTempLogFile(const Suffix: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestLogManagerSingleton;
    [Test]
    procedure TestRegisterSink;
    [Test]
    procedure TestUnregisterSink;
    [Test]
    procedure TestMemorySinkBasic;
    [Test]
    procedure TestMemorySinkLevels;
    [Test]
    procedure TestMemorySinkDisabled;
    [Test]
    procedure TestMultipleSinks;
    [Test]
    procedure TestFileSink;
    [Test]
    procedure TestLogLevelMethods;
    [Test]
    procedure TestGlobalBoldLogProcedure;
    [Test]
    procedure TestMemorySinkMaxLines;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  BoldLogInterfaces,
  BoldLogSinks;

{ TTestBoldLogInterfaces }

procedure TTestBoldLogInterfaces.Setup;
begin
  FTempDir := TPath.Combine(TPath.GetTempPath, 'BoldLogTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);
  BoldLogManager.UnregisterAllSinks;
end;

procedure TTestBoldLogInterfaces.TearDown;
begin
  BoldLogManager.UnregisterAllSinks;
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

function TTestBoldLogInterfaces.GetTempLogFile(const Suffix: string): string;
begin
  Result := TPath.Combine(FTempDir, 'test_' + Suffix + '.log');
end;

procedure TTestBoldLogInterfaces.TestLogManagerSingleton;
var
  Manager1, Manager2: IBoldLogManager;
begin
  Manager1 := BoldLogManager;
  Manager2 := BoldLogManager;
  Assert.AreSame(Manager1, Manager2, 'BoldLogManager should return same instance');
end;

procedure TTestBoldLogInterfaces.TestRegisterSink;
var
  Sink: IBoldLogSink;
begin
  Assert.AreEqual(0, BoldLogManager.SinkCount);

  Sink := TBoldMemoryLogSink.Create;
  BoldLogManager.RegisterSink(Sink);

  Assert.AreEqual(1, BoldLogManager.SinkCount);
end;

procedure TTestBoldLogInterfaces.TestUnregisterSink;
var
  Sink: IBoldLogSink;
begin
  Sink := TBoldMemoryLogSink.Create;
  BoldLogManager.RegisterSink(Sink);
  Assert.AreEqual(1, BoldLogManager.SinkCount);

  BoldLogManager.UnregisterSink(Sink);
  Assert.AreEqual(0, BoldLogManager.SinkCount);
end;

procedure TTestBoldLogInterfaces.TestMemorySinkBasic;
var
  Sink: TBoldMemoryLogSink;
begin
  Sink := TBoldMemoryLogSink.Create;
  BoldLogManager.RegisterSink(Sink);

  BoldLogManager.Log('Test message');

  Assert.AreEqual(1, Sink.Lines.Count);
  Assert.Contains(Sink.Lines[0], 'Test message');
end;

procedure TTestBoldLogInterfaces.TestMemorySinkLevels;
var
  Sink: TBoldMemoryLogSink;
begin
  Sink := TBoldMemoryLogSink.Create;
  Sink.Levels := [llError]; // Only errors
  BoldLogManager.RegisterSink(Sink);

  BoldLogManager.Info('Info message');
  BoldLogManager.Warning('Warning message');
  BoldLogManager.Error('Error message');

  Assert.AreEqual(1, Sink.Lines.Count);
  Assert.Contains(Sink.Lines[0], 'Error message');
end;

procedure TTestBoldLogInterfaces.TestMemorySinkDisabled;
var
  Sink: TBoldMemoryLogSink;
begin
  Sink := TBoldMemoryLogSink.Create;
  Sink.Enabled := False;
  BoldLogManager.RegisterSink(Sink);

  BoldLogManager.Log('Should not appear');

  Assert.AreEqual(0, Sink.Lines.Count);
end;

procedure TTestBoldLogInterfaces.TestMultipleSinks;
var
  Sink1, Sink2: TBoldMemoryLogSink;
begin
  Sink1 := TBoldMemoryLogSink.Create;
  Sink2 := TBoldMemoryLogSink.Create;
  BoldLogManager.RegisterSink(Sink1);
  BoldLogManager.RegisterSink(Sink2);

  BoldLogManager.Log('Broadcast message');

  Assert.AreEqual(1, Sink1.Lines.Count);
  Assert.AreEqual(1, Sink2.Lines.Count);
  Assert.Contains(Sink1.Lines[0], 'Broadcast message');
  Assert.Contains(Sink2.Lines[0], 'Broadcast message');
end;

procedure TTestBoldLogInterfaces.TestFileSink;
var
  Sink: TBoldFileLogSink;
  LogFile: string;
  Content: string;
begin
  LogFile := GetTempLogFile('filesink');
  Sink := TBoldFileLogSink.Create(LogFile);
  BoldLogManager.RegisterSink(Sink);

  BoldLogManager.Log('File log entry');

  // Unregister to close file
  BoldLogManager.UnregisterSink(Sink);

  Content := TFile.ReadAllText(LogFile, TEncoding.UTF8);
  Assert.Contains(Content, 'File log entry');
end;

procedure TTestBoldLogInterfaces.TestLogLevelMethods;
var
  Sink: TBoldMemoryLogSink;
begin
  Sink := TBoldMemoryLogSink.Create;
  Sink.Levels := AllBoldLogLevels;
  BoldLogManager.RegisterSink(Sink);

  BoldLogManager.Trace('Trace msg');
  BoldLogManager.Debug('Debug msg');
  BoldLogManager.Info('Info msg');
  BoldLogManager.Warning('Warn msg');
  BoldLogManager.Error('Error msg');

  Assert.AreEqual(5, Sink.Lines.Count);
  Assert.Contains(Sink.Lines[0], '[TRACE]');
  Assert.Contains(Sink.Lines[1], '[DEBUG]');
  Assert.Contains(Sink.Lines[2], '[INFO]');
  Assert.Contains(Sink.Lines[3], '[WARN]');
  Assert.Contains(Sink.Lines[4], '[ERROR]');
end;

procedure TTestBoldLogInterfaces.TestGlobalBoldLogProcedure;
var
  Sink: TBoldMemoryLogSink;
begin
  Sink := TBoldMemoryLogSink.Create;
  BoldLogManager.RegisterSink(Sink);

  BoldLog('Global log call');
  BoldLog('Formatted: %d', [42], llWarning);

  Assert.AreEqual(2, Sink.Lines.Count);
  Assert.Contains(Sink.Lines[0], 'Global log call');
  Assert.Contains(Sink.Lines[1], 'Formatted: 42');
  Assert.Contains(Sink.Lines[1], '[WARN]');
end;

procedure TTestBoldLogInterfaces.TestMemorySinkMaxLines;
var
  Sink: TBoldMemoryLogSink;
  i: Integer;
begin
  Sink := TBoldMemoryLogSink.Create(5); // Max 5 lines
  BoldLogManager.RegisterSink(Sink);

  for i := 1 to 10 do
    BoldLogManager.Log('Message %d', [i]);

  Assert.AreEqual(5, Sink.Lines.Count);
  // Should have last 5 messages (6-10)
  Assert.Contains(Sink.Lines[0], 'Message 6');
  Assert.Contains(Sink.Lines[4], 'Message 10');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldLogInterfaces);

end.
