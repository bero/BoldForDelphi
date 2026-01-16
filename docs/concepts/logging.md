# Logging

Bold for Delphi provides flexible logging capabilities through three complementary systems, each suited for different use cases.

## Quick Start

The simplest way to add logging to your Bold application:

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

## Logging Systems Overview

| System | Best For | Output | Thread-Safe |
|--------|----------|--------|-------------|
| **Simple** (BoldThreadSafeLog) | File-based logging | Log files | Yes |
| **Multi-Sink** (BoldLogManager) | Production apps with multiple outputs | Files, Console, Debugger, Memory | Yes |
| **UI/Progress** (BoldLogHandler) | UI apps with progress tracking | UI forms | Yes |

## Simple File Logging

The `BoldThreadSafeLog` unit provides straightforward file-based logging with automatic timestamps.

### Setup

```pascal
uses
  BoldThreadSafeLog;

procedure InitializeLogging;
begin
  // Parameters: MainLog, ErrorLog, ThreadLog, MaxFileSize
  BoldInitLog(
    'logs\app.log',      // Main log file
    'logs\error.log',    // Error-specific log
    'logs\thread.log',   // Thread activity log (optional, use '' to skip)
    10 * 1024 * 1024     // Max 10 MB per file
  );
end;

procedure FinalizeLogging;
begin
  BoldDoneLog;
end;
```

### Usage

```pascal
// Simple message
BoldLog('User logged in');

// Formatted message
BoldLog('Processed %d of %d items', [Current, Total]);

// Error logging (always includes thread ID)
BoldLogError('Failed to save: %s', [E.Message]);

// Thread activity logging
BoldLogThread('Worker thread started');
```

### Log File Format

```
2025-01-15T10:30:45.123 Application started
2025-01-15T10:30:45.456 Processing 100 records
2025-01-15T10:30:46.789 Failed to connect (ThreadID=1234)
```

## Multi-Sink Logging

The `BoldLogInterfaces` and `BoldLogSinks` units provide a flexible sink-based architecture for production applications.

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

### Available Sinks

| Sink | Description |
|------|-------------|
| `TBoldFileLogSink` | Log to files with automatic size management |
| `TBoldConsoleLogSink` | Output to console (WriteLn) |
| `TBoldDebugLogSink` | OutputDebugString - visible in IDE debugger |
| `TBoldMemoryLogSink` | In-memory buffer for UI display |

### Basic Setup

```pascal
uses
  BoldLogInterfaces,
  BoldLogSinks;

procedure SetupLogging;
var
  FileSink: TBoldFileLogSink;
begin
  // Create a file sink with 5 MB max size
  FileSink := TBoldFileLogSink.Create('app.log', 5 * 1024 * 1024);
  BoldLogManager.RegisterSink(FileSink);
end;
```

### Multiple Sinks

```pascal
procedure SetupProductionLogging;
var
  FileSink: TBoldFileLogSink;
  ConsoleSink: TBoldConsoleLogSink;
  DebugSink: TBoldDebugLogSink;
begin
  // File: All levels
  FileSink := TBoldFileLogSink.Create('app.log');
  FileSink.Levels := [llInfo, llWarning, llError];
  BoldLogManager.RegisterSink(FileSink);

  // Console: Errors only
  ConsoleSink := TBoldConsoleLogSink.Create;
  ConsoleSink.Levels := [llError];
  BoldLogManager.RegisterSink(ConsoleSink);

  // Debug output: Everything during development
  DebugSink := TBoldDebugLogSink.Create;
  DebugSink.Levels := [llTrace, llDebug, llInfo, llWarning, llError];
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

### In-Memory Sink for UI

```pascal
var
  MemorySink: TBoldMemoryLogSink;

procedure SetupLogViewer;
begin
  // Keep last 1000 lines in memory
  MemorySink := TBoldMemoryLogSink.Create(1000);
  BoldLogManager.RegisterSink(MemorySink);
end;

procedure RefreshLogView;
begin
  // Display in a TMemo or similar
  Memo1.Lines.Assign(MemorySink.Lines);
end;
```

## UI Logging with Progress

The `BoldLogHandler` unit is the original Bold logging system. It provides logging with progress tracking, ideal for long-running operations with UI feedback. It uses Bold's subscription pattern to notify UI components.

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
      BoldLog.ProgressStep;  // Increment progress
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
    BoldLog.LogDedent;  // Log message and dedent
  end;
end;
```

Output:
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
BoldLog.Log('', ltSeparator);  // Visual separator line
```

## Choosing the Right System

| Need | Recommendation |
|------|----------------|
| Simple file log | BoldThreadSafeLog |
| File log with levels | BoldLogManager + TBoldFileLogSink |
| Display in TMemo | BoldLogManager + TBoldMemoryLogSink |
| Progress tracking + UI | BoldLogHandler |

### Simple File Logging (BoldThreadSafeLog)

Best for straightforward file logging without UI:

```pascal
BoldInitLog('app.log', 'error.log', '', 10*1024*1024);
BoldLog('Something happened');
```

### File Logging with Levels (BoldLogManager)

Use when you need log level filtering (Debug, Info, Warning, Error):

```pascal
var Sink := TBoldFileLogSink.Create('app.log');
Sink.Levels := [llInfo, llWarning, llError];  // Filter out Debug/Trace
BoldLogManager.RegisterSink(Sink);

BoldLog('Debug info', llDebug);    // Not written (filtered)
BoldLog('Important', llInfo);       // Written
```

### UI Display without Progress (TBoldMemoryLogSink)

Display logs in a TMemo or similar component:

```pascal
var MemorySink := TBoldMemoryLogSink.Create(1000);  // Keep 1000 lines
BoldLogManager.RegisterSink(MemorySink);

// In your UI refresh
Memo1.Lines.Assign(MemorySink.Lines);
```

### UI with Progress Tracking (BoldLogHandler)

Use when you need progress bars and hierarchical display:

- Progress bars (ProgressMax, ProgressStep)
- Hierarchical indentation (Indent/Dedent)
- Implement `IBoldLogReceiver` for custom UI components
- Show/Hide/Clear operations on log windows

## Thread Safety

All three logging systems are thread-safe:

- **Simple**: Uses `TCriticalSection` for file access
- **Multi-Sink**: Uses `TMonitor` for sink management
- **UI/Progress**: Uses subscription pattern with thread synchronization

```pascal
// Safe to call from any thread
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

| File | Description |
|------|-------------|
| `Source/Common/Logging/BoldThreadSafeLog.pas` | Simple file logging |
| `Source/Common/Logging/BoldLogInterfaces.pas` | Modern sink interfaces |
| `Source/Common/Logging/BoldLogSinks.pas` | Sink implementations |
| `Source/Common/Logging/BoldLogHandler.pas` | Legacy UI logging |
| `Source/Common/Logging/BoldLogReceiverInterface.pas` | Receiver interfaces |
