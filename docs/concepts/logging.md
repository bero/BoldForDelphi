# Logging

Bold for Delphi has three logging systems for different needs. All three are in `Source/Common/Logging` and build
with Delphi and Free Pascal.

| System | Unit | Best for | Output | Thread-safe |
|--------|------|----------|--------|-------------|
| **Simple** | `BoldThreadSafeLog` | File logging | Log files | Yes |
| **Multi-sink** | `BoldLogInterfaces`, `BoldLogSinks` | Applications that need levels and several outputs | Files, console, debugger, memory | Yes |
| **Progress** | `BoldLogHandler` | Long operations with UI feedback | Log receivers such as log forms | No; use it from the main thread |

All three units declare a routine named `BoldLog`. When a unit uses more than one of them, the unit listed last in
the `uses` clause wins; qualify the name, for example `BoldThreadSafeLog.BoldLog(...)`, to choose.

## Quick Start

The simplest way to add logging to a Bold application:

```pascal
uses
  BoldThreadSafeLog;

// Initialize at application startup
BoldInitLog('app.log', 'error.log', '', 10*1024*1024);

// Log messages anywhere in your code
BoldLog('Application started');
BoldLog('Processing %d records', [RecordCount]);
BoldLogError('Database connection failed: %s', [ErrorMsg]);

// Clean up at shutdown
BoldDoneLog;
```

## Simple File Logging

The `BoldThreadSafeLog` unit writes to log files and adds a timestamp to every line.

### Setup

```pascal
uses
  BoldThreadSafeLog;

procedure InitializeLogging;
begin
  // Parameters: MainLog, ErrorLog, ThreadLog, MaxFileSize
  BoldInitLog(
    'logs/app.log',      // Main log file
    'logs/error.log',    // Error log
    'logs/thread.log',   // Thread activity log; '' for none
    10 * 1024 * 1024     // Max 10 MB per file
  );
end;

procedure FinalizeLogging;
begin
  BoldDoneLog;
end;
```

A file that grows beyond the maximum size is emptied and starts again; it is not rotated. Before `BoldInitLog` and
after `BoldDoneLog`, the logging calls do nothing.

### Usage

```pascal
// Simple message
BoldLog('User logged in');

// Formatted message
BoldLog('Processed %d of %d items', [Current, Total]);

// Error log; every entry includes the thread ID
BoldLogError('Failed to save: %s', [E.Message]);

// Thread activity log; ignored when no thread log file was given
BoldLogThread('Worker thread started');
```

### Log File Format

`app.log`:

```
2025-01-15T10:30:45.123 Application started
2025-01-15T10:30:45.456 Processing 100 records
```

`error.log`:

```
2025-01-15T10:30:46.789 Failed to connect (ThreadID=1234)
```

## Multi-Sink Logging

The `BoldLogInterfaces` and `BoldLogSinks` units send each message to every registered sink whose levels include the
message's level.

### Log Levels

```pascal
type
  TBoldLogLevel = (
    llTrace,    // Detailed tracing
    llDebug,    // Debug information
    llInfo,     // General information
    llWarning,  // Warning conditions
    llError     // Error conditions
  );
```

A new sink logs `llInfo`, `llWarning` and `llError` (`DefaultBoldLogLevels`).

### Available Sinks

| Sink | Description |
|------|-------------|
| `TBoldFileLogSink` | Writes to a file; emptied when it exceeds the maximum size (default 10 MB) |
| `TBoldConsoleLogSink` | Writes to the console (`WriteLn`) |
| `TBoldDebugLogSink` | `OutputDebugString` on Windows, visible in the IDE debugger; standard error elsewhere |
| `TBoldMemoryLogSink` | Keeps the last lines in memory, for display in the UI (default 10000 lines) |

### Basic Setup

```pascal
uses
  BoldLogInterfaces,
  BoldLogSinks;

procedure SetupLogging;
var
  FileSink: TBoldFileLogSink;
begin
  // A file sink with 5 MB max size
  FileSink := TBoldFileLogSink.Create('app.log', 5 * 1024 * 1024);
  BoldLogManager.RegisterSink(FileSink);
end;
```

The sinks are reference-counted interfaces: the log manager keeps a registered sink alive until it is unregistered.

### Multiple Sinks

```pascal
procedure SetupProductionLogging;
var
  FileSink: TBoldFileLogSink;
  ConsoleSink: TBoldConsoleLogSink;
  DebugSink: TBoldDebugLogSink;
begin
  // File: information, warnings and errors
  FileSink := TBoldFileLogSink.Create('app.log');
  FileSink.Levels := [llInfo, llWarning, llError];
  BoldLogManager.RegisterSink(FileSink);

  // Console: errors only
  ConsoleSink := TBoldConsoleLogSink.Create;
  ConsoleSink.Levels := [llError];
  BoldLogManager.RegisterSink(ConsoleSink);

  // Debug output: everything, during development
  DebugSink := TBoldDebugLogSink.Create;
  DebugSink.Levels := AllBoldLogLevels;
  BoldLogManager.RegisterSink(DebugSink);
end;
```

### Logging Messages

```pascal
uses
  BoldLogInterfaces;

procedure DoSomething;
begin
  BoldLog('Starting operation', llInfo);
  try
    // ... do work ...
    BoldLog('Processed %d items', [Count], llDebug);
  except
    on E: Exception do
      BoldLog('Operation failed: %s', [E.Message], llError);
  end;
end;
```

`BoldLogManager` also has `Trace`, `Debug`, `Info`, `Warning` and `Error` methods, for example
`BoldLogManager.Warning('Disk almost full')`.

### In-Memory Sink for UI

```pascal
var
  MemorySink: TBoldMemoryLogSink;

procedure SetupLogViewer;
begin
  // Keep the last 1000 lines in memory
  MemorySink := TBoldMemoryLogSink.Create(1000);
  BoldLogManager.RegisterSink(MemorySink);
end;

procedure RefreshLogView;
begin
  // Display in a TMemo or similar
  Memo1.Lines.Assign(MemorySink.Lines);
end;
```

## Progress Logging

The `BoldLogHandler` unit is the original Bold logging system. It logs with indentation and progress tracking, for
long operations with UI feedback. It sends its output through Bold's subscription mechanism to log receivers
(`IBoldLogReceiver`), such as a log form. Bold itself logs through it, for example while it creates a database
schema.

### Basic Usage

```pascal
uses
  BoldLogHandler;

procedure ImportData;
begin
  BoldLog.StartLog('Data Import');
  try
    BoldLog.Log('Loading file...');
    // ... load file ...

    BoldLog.Log('Processing records...');
    // ... process ...

    BoldLog.Log('Import complete');
  finally
    BoldLog.EndLog;
  end;
end;
```

### Progress Tracking

```pascal
procedure ProcessRecords(Records: TList);
var
  I: Integer;
begin
  BoldLog.ProgressMax := Records.Count;
  BoldLog.LogIndent('Processing records');
  try
    for I := 0 to Records.Count - 1 do
    begin
      BoldLog.LogFmt('Record %d of %d', [I + 1, Records.Count]);
      ProcessRecord(Records[I]);
      BoldLog.ProgressStep;  // Advance the progress
    end;
  finally
    BoldLog.Dedent;
  end;
end;
```

### Nested Operations with Indentation

```pascal
procedure ComplexOperation;
begin
  BoldLog.LogIndent('Starting complex operation');
  try
    BoldLog.Log('Phase 1: Validation');
    ValidateData;

    BoldLog.LogIndent('Phase 2: Processing');
    try
      ProcessItems;
    finally
      BoldLog.Dedent;
    end;

    BoldLog.Log('Phase 3: Cleanup');
    Cleanup;
  finally
    BoldLog.LogDedent('Operation complete');  // Dedent, then log
  end;
end;
```

Output, when `ProcessItems` logs two lines:

```
Starting complex operation
  Phase 1: Validation
  Phase 2: Processing
    Item 1 processed
    Item 2 processed
  Phase 3: Cleanup
Operation complete
```

### Log Types

```pascal
BoldLog.Log('Information message', ltInfo);
BoldLog.Log('Detailed debug info', ltDetail);
BoldLog.Log('Warning: low memory', ltWarning);
BoldLog.Log('Error: file not found', ltError);
BoldLog.Separator;  // Separator lines
```

### Other Features

- `Show`, `Hide` and `Clear` act on the log receivers, for example a log window.
- Implement `IBoldLogReceiver` (unit `BoldLogReceiverInterface`) and register it with `RegisterLogReceiver` to show
  the log in your own UI.

## Choosing the Right System

| Need | Recommendation |
|------|----------------|
| Simple file log | `BoldThreadSafeLog` |
| File log with levels | `BoldLogManager` + `TBoldFileLogSink` |
| Display in a TMemo | `BoldLogManager` + `TBoldMemoryLogSink` |
| Progress tracking and hierarchical display | `BoldLogHandler` |
| Exceptions with their call stacks | `BoldExceptionTrace`, see below |

## Call Stacks of Exceptions

`BoldExceptionTrace` (#130) writes an exception to the log with its call stack, so that an error a user
reports can be traced to the routine that raised it.

```pascal
uses
  BoldExceptionTrace;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
begin
  BoldLogException(E);           // to BoldLogManager, as an error
  Application.ShowException(E);
end;

// In FormCreate: Application.OnException := AppException;
```

Call it in the handler or `except` block that handles the exception. `BoldExceptionReport(E)` gives the
same text without logging it, and `BoldExceptionCallStack(E)` the call stack alone. `BoldLogException` never raises: a log that
cannot be written (its file locked by another instance, its folder read-only) leaves the exception to the
handler, which goes on to show it.

Bold names no stack-trace library, so the library can be exchanged without a change in Bold:

| Compiler | Where the call stack comes from | Without it |
|----------|---------------------------------|------------|
| Free Pascal / Lazarus | the RTL's own frames, up to 64 (the unit raises the RTL's `RaiseMaxFrameCount` from its default of 16); compile with `-gl` for routine, line and file | addresses only |
| Delphi | `Exception.StackTrace`, which a provider fills through the RTL's hooks (`Exception.GetExceptionStackInfoProc` and two more) | the raise address and the load address of its module, which the detailed map file resolves |

A Delphi program chooses its provider in its `.dpr`. [Stacker](https://codeberg.org/sivv/stacker) (MIT;
Win32, Win64, Linux64, macOS, Android; Delphi 12 or later) installs the hooks itself: name its unit first
in the uses clause and import its `build/Stacker.targets` in the `.dproj`, before
`CodeGear.Delphi.Targets`; a post-link step embeds the symbols in the program, so no map file is shipped.
The model editor's FMX client (`Tools/ModelEditor/FMX`) shows how to make that optional: the import and
the unit are only used when the environment variable `STACKER` points to a checkout of Stacker. The lines
that a provider writes before the raising routine (its capture routines and the RTL's raise routines) are
left out of the report.

## Thread Safety

- **Simple**: each log file is protected by a `TCriticalSection`.
- **Multi-sink**: a `TCriticalSection` protects the sink list, and the file and memory sinks have their own.
- **Progress**: `BoldLogHandler` has no locking and its receivers are usually forms. Use it from the main thread.

The simple and multi-sink logs can be called from any thread. A Delphi example (anonymous methods need Delphi):

```pascal
TThread.CreateAnonymousThread(
  procedure
  begin
    BoldLog('Background task started');
    // ... do work ...
    BoldLog('Background task completed');
  end
).Start;
```

## Source Files

All units are in `Source/Common/Logging`:

| Unit | Description |
|------|-------------|
| `BoldThreadSafeLog` | Simple file logging |
| `BoldLogInterfaces` | Log levels, the sink interface and `BoldLogManager` |
| `BoldLogSinks` | Sink implementations |
| `BoldLogHandler` | Progress logging |
| `BoldLogReceiverInterface` | `IBoldLogReceiver` and `TBoldLogReceiver` |
| `BoldExceptionTrace` | Exceptions with their call stacks |
