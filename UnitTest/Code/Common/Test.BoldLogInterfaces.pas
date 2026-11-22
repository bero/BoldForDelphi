unit Test.BoldLogInterfaces;

interface

uses
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldLogInterfaces = class(TBoldTestCase)
  private
    FTempDir: string;
    function GetTempLogFile(const Suffix: string): string;
    function UnreachableLogFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLogManagerSingleton;
    procedure TestRegisterSink;
    procedure TestUnregisterSink;
    procedure TestMemorySinkBasic;
    procedure TestMemorySinkLevels;
    procedure TestMemorySinkDisabled;
    procedure TestMultipleSinks;
    procedure TestFileSink;
    procedure TestLogLevelMethods;
    procedure TestGlobalBoldLogProcedure;
    procedure TestMemorySinkMaxLines;
    // The next run of the program adds to the file the last run left
    procedure TestFileSinkAppendsToAnExistingFile;
    // A file sink with several files writes to the first that opens (#130)
    procedure TestFileSinkPassesOverAFolderThatCannotBeCreated;
    // A file another sink holds, as another instance of the program does
    procedure TestFileSinkPassesOverAFileAnotherSinkHolds;
    procedure TestFileSinkRaisesWhenNoFileOpens;
    procedure TestFileSinkNeedsAFileName;
  end;

implementation

uses
  SysUtils,
  Classes,
  BoldLogInterfaces,
  BoldLogSinks;

{ TTestBoldLogInterfaces }

procedure TTestBoldLogInterfaces.SetUp;
begin
  FTempDir := BoldTestNewTempDir('BoldLogTest_');
  BoldLogManager.UnregisterAllSinks;
end;

procedure TTestBoldLogInterfaces.TearDown;
begin
  BoldLogManager.UnregisterAllSinks;
  BoldTestDeleteTree(FTempDir);
end;

function TTestBoldLogInterfaces.GetTempLogFile(const Suffix: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + ('test_' + Suffix + '.log');
end;

{ A log file whose folder cannot be created: the folder would be under a file }
function TTestBoldLogInterfaces.UnreachableLogFile: string;
var
  Blocker: string;
begin
  Blocker := GetTempLogFile('blocker');
  TFileStream.Create(Blocker, fmCreate).Free;
  Result := Blocker + PathDelim + 'folder' + PathDelim + 'test_unreachable.log';
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

  Content := BoldTestReadAllText(LogFile);
  Assert.Contains(Content, 'File log entry');
end;

procedure TTestBoldLogInterfaces.TestFileSinkAppendsToAnExistingFile;
var
  LogFile: string;
  Sink: IBoldLogSink;
  Content: string;
begin
  LogFile := GetTempLogFile('existing');
  Sink := TBoldFileLogSink.Create(LogFile);
  Sink.WriteLog('First run', llInfo, Now, 0);
  Sink := nil;
  Sink := TBoldFileLogSink.Create(LogFile);
  Sink.WriteLog('Second run', llInfo, Now, 0);
  Sink := nil;
  Content := BoldTestReadAllText(LogFile);
  Assert.Contains(Content, 'First run');
  Assert.Contains(Content, 'Second run');
end;

procedure TTestBoldLogInterfaces.TestFileSinkPassesOverAFolderThatCannotBeCreated;
var
  Unreachable, LogFile: string;
  Sink: TBoldFileLogSink;
  SinkRef: IBoldLogSink;
begin
  Unreachable := UnreachableLogFile;
  LogFile := GetTempLogFile('reachable');
  Sink := TBoldFileLogSink.Create([Unreachable, LogFile]);
  SinkRef := Sink;
  Assert.AreEqual(Unreachable, Sink.FileName, 'the first file before the first entry');
  SinkRef.WriteLog('Entry', llInfo, Now, 0);
  Assert.AreEqual(LogFile, Sink.FileName, 'the file it writes to');
  SinkRef := nil;
  Assert.Contains(BoldTestReadAllText(LogFile), 'Entry');
end;

procedure TTestBoldLogInterfaces.TestFileSinkPassesOverAFileAnotherSinkHolds;
var
  First, Second: string;
  Holder, Sink: TBoldFileLogSink;
  HolderRef, SinkRef: IBoldLogSink;
begin
  First := GetTempLogFile('program');
  Second := GetTempLogFile('program-2');
  // The first instance of the program writes to the first file and keeps it
  // open
  Holder := TBoldFileLogSink.Create([First, Second]);
  HolderRef := Holder;
  HolderRef.WriteLog('First instance', llInfo, Now, 0);
  Sink := TBoldFileLogSink.Create([First, Second]);
  SinkRef := Sink;
  SinkRef.WriteLog('Second instance', llInfo, Now, 0);
  Assert.AreEqual(First, Holder.FileName, 'the first instance''s file');
  Assert.AreEqual(Second, Sink.FileName, 'the second instance''s file');
  HolderRef := nil;
  SinkRef := nil;
  Assert.IsFalse(Pos('Second instance', BoldTestReadAllText(First)) > 0,
    'the second instance did not write to the first file: ' + BoldTestReadAllText(First));
  Assert.Contains(BoldTestReadAllText(Second), 'Second instance');
end;

procedure TTestBoldLogInterfaces.TestFileSinkRaisesWhenNoFileOpens;
var
  Sink: IBoldLogSink;
  Raised: Boolean;
begin
  Sink := TBoldFileLogSink.Create([UnreachableLogFile]);
  Raised := False;
  try
    Sink.WriteLog('Entry', llInfo, Now, 0);
  except
    Raised := True;
  end;
  Assert.IsTrue(Raised, 'no file could be opened, and the entry was not written');
end;

procedure TTestBoldLogInterfaces.TestFileSinkNeedsAFileName;
var
  Raised: string;
begin
  Raised := '';
  try
    TBoldFileLogSink.Create([]).Free;
  except
    on E: Exception do
      Raised := E.ClassName;
  end;
  Assert.AreEqual('EArgumentException', Raised, 'a file sink without a file');
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
  BoldRegisterTest(TTestBoldLogInterfaces);

end.
