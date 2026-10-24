
{ Global compiler directives }
{$include bold.inc}
unit BoldThreadSafeLog;

{$REGION 'Documentation'}
(*
  🗒️ Unit: BoldThreadSafeLog

  📜 Description:
    BoldThreadSafeLog is a lightweight, thread-safe file logging framework for Bold for Delphi
    applications. It provides simple, reliable logging to multiple log files with automatic
    timestamp formatting, optional thread ID inclusion, and automatic file size management.
    The framework is designed for production use where basic file-based logging is required
    without the complexity of more sophisticated logging systems.

  🔖 Core Components:
    1. 📝 TFileLogging: Low-level thread-safe file writer that handles a single log file.
       - Uses TCriticalSection for thread safety
       - Supports automatic file truncation when MaxSize is exceeded
       - Configurable timestamp format (date+time or time only)
       - Optional thread ID in log entries (full or short format)
       - UTF-8 encoded output with ISO 8601 timestamps

    2. 🎯 TBoldLogger: High-level logger managing three separate log files:
       - Main log (fLog): For general application messages via Log/LogFmt
       - Error log (fErrorLog): For error messages via LogError/LogErrorFmt, includes thread ID
       - Thread log (fThreadLog): For thread activity messages via LogThread, short thread ID format

    3. 🌐 Global Procedures: Convenient access to a singleton TBoldLogger instance:
       - BoldInitLog: Initialize the global logger with file paths and max size
       - BoldDoneLog: Shutdown and free the global logger
       - BoldLog: Log to main log file
       - BoldLogError: Log to error log file
       - BoldLogThread: Log to thread activity file

  ✨ Features:
    - 🛡️ Thread Safety: All file operations protected by TCriticalSection
    - 📏 Size Management: Automatic file truncation when MaxSize exceeded (FlushStream clears file)
    - ⏰ ISO Timestamps: Uses AsISODateTimeMS/AsISOTimeMS for consistent timestamp format
    - 🧵 Thread ID Support: Optional thread ID inclusion in two formats:
      - Full: " (ThreadID=1234)"
      - Short: ":TID=1234"
    - 📁 Multiple Log Files: Separate files for general logs, errors, and thread activity
    - 🔄 Open/Close Control: Logs can be opened and closed dynamically

  🛠️ Usage:
    // Initialize logging at application startup
    BoldInitLog('app.log', 'error.log', 'thread.log', 10 * 1024 * 1024);  // 10MB max

    // Log messages
    BoldLog('Application started');
    BoldLog('Processing %d items', [Count]);
    BoldLogError('Failed to connect: %s', [ErrorMsg]);
    BoldLogThread('Worker thread started');

    // Shutdown at application exit
    BoldDoneLog;

  📋 Log Entry Format:
    With IncludeDate=True:  "2025-01-15T10:30:45.123 Message text"
    With IncludeDate=False: "10:30:45.123 Message text"
    With IncludeThreadId:   "2025-01-15T10:30:45.123 Message text (ThreadID=1234)"
    With ShortThreadId:     "10:30:45.123 Message text:TID=1234"

  ⚠️ Notes:
    - If BoldInitLog is not called, all BoldLog/BoldLogError calls are no-ops (safe to call)
    - FlushStream truncates the file to zero bytes when MaxSize is exceeded (not a rolling log)
    - Error log always includes thread ID for debugging multi-threaded issues
    - Thread log uses time-only timestamps and short thread ID format for compact output
*)
{$ENDREGION}

interface

uses
  classes,
  syncobjs;

type
  {forward declarations}
  TBoldLogger = class;
  TFileLogging = class;

  { TBoldLogger }
  TBoldLogger = class
  private
    fLog: TFileLogging;
    fErrorLog: TFileLogging;
    fThreadLog: TFileLogging;
  public
    constructor Create(const LogFileName, ErrorLogFileName, ThreadLogFileName: string; const MaxLogFileSize: integer);
    destructor Destroy; override;
    procedure Log(const Msg: string);
    procedure LogError(const Msg: string);
    procedure LogFmt(const Msg: string; const Args: array of const);
    procedure LogErrorFmt(const Msg: string; const Args: array of const);
    procedure LogThread(Msg: String);
  end;

  { TFileLogging }
  TFileLogging = class
  private
    fFileStream: TFileStream;
    fFileName: string;
    fMaxSize: LongInt;
    fLocker: TCriticalSection;
    fOpen: Boolean;
    FIncludeThreadId: Boolean;
    fIncludeDate: Boolean;
    fShortThreadId: Boolean;
    function OpenStream: Boolean;
    procedure CloseStream;
    procedure FlushStream;
    procedure SetOpen(const Value: Boolean);
  public
    constructor create(const FileName: string; const MaxSize: LongInt; const Open: Boolean); overload;
    destructor Destroy; override;
    procedure Trace(const Entry: string); overload;
    procedure Trace(const Msg: string; const Args: array of const); overload;
    property FileName: string read fFileName;
    property Open: Boolean read fOpen write setOpen;
    property IncludeThreadId: Boolean read FIncludeThreadID write FIncludeThreadId;
    property IncludeDate: Boolean read fIncludeDate write fIncludeDate;
    property ShortThreadId: Boolean read fShortThreadId write fShortThreadId;
  end;


  procedure BoldInitLog(const LogFilename, ErrorLogFileName, ThreadLogFileName: string; const MaxLogFileSize: integer);
  procedure BoldDoneLog;
  procedure BoldLog(const Msg: string); overload;
  procedure BoldLog(const Msg: string; const Args: array of const); overload;
  procedure BoldLogError(const Msg: string); overload;
  procedure BoldLogError(const Msg: string; const Args: array of const); overload;
  procedure BoldLogThread(const Msg: string);

implementation

uses
  SysUtils,
  BoldUtils,
  Windows,
  BoldDefs,
  BoldIsoDateTime;

var
  LogThreadActivities: Boolean;
  Logger: TBoldLogger;

procedure BoldInitLog(const LogFilename, ErrorLogFileName, ThreadLogFileName: string; const MaxLogFileSize: integer);
begin
  LogThreadActivities := ThreadLogFileName <> '';
  Logger := TBoldLogger.Create(LogFileName, ErrorLogFileName, ThreadLogFileName, MaxLogFileSize);
end;

procedure BoldDoneLog;
begin
  FreeAndNil(Logger);
end;

procedure BoldLog(const Msg: string); overload;
begin
  if Assigned(Logger) then
    Logger.Log(Msg);
end;

procedure BoldLog(const Msg: string; const Args: array of const); overload;
begin
  if Assigned(Logger) then
    Logger.LogFmt(Msg, Args);
end;

procedure BoldLogError(const Msg: string); overload;
begin
  if Assigned(Logger) then
    Logger.LogError(Msg);
end;

procedure BoldLogError(const Msg: string; const Args: array of const); overload;
begin
  if Assigned(Logger) then
    Logger.LogErrorFmt(Msg, Args);
end;

procedure BoldLogThread(const Msg: string);
begin
  if assigned(Logger) and LogThreadActivities then
    Logger.LogThread(Msg);
end;

{ TBoldLogger }

constructor TBoldLogger.Create(const LogFileName, ErrorLogFileName, ThreadLogFileName: string; const MaxLogFileSize: integer);
begin
  inherited Create;
  fLog := TFileLogging.Create(LogFileName, MaxLogFileSize, true);
  fErrorLog := TFileLogging.Create(ErrorLogFileName, MaxLogFileSize, true);
  fErrorLog.IncludeThreadId := True;
  if ThreadLogFileName <> '' then
  begin
    fThreadLog := TFileLogging.Create(ThreadLogFileName, MaxLogFileSize, true);
    fThreadLog.IncludeThreadId := True;
    fThreadLog.IncludeDate := false;
    fThreadLog.ShortThreadId := true;
  end else
    fThreadLog := nil;
end;

destructor TBoldLogger.Destroy;
begin
  FreeAndNil(fLog);
  FreeAndNil(fErrorLog);
  FreeAndNil(fThreadLog);
  inherited;
end;

procedure TBoldLogger.Log(const Msg: string);
begin
  if Assigned(fLog) then
    fLog.Trace(Msg);
end;

procedure TBoldLogger.LogError(const Msg: string);
begin
  if Assigned(fErrorLog) then
    fErrorLog.Trace(Msg);
end;

procedure TBoldLogger.LogErrorFmt(const Msg: string; const Args: array of const);
begin
  if Assigned(fErrorLog) then
    fErrorLog.Trace(Msg, Args);
end;

procedure TBoldLogger.LogFmt(const Msg: string; const Args: array of const);
begin
  if Assigned(fLog) then
    fLog.Trace(Msg, Args);
end;

procedure TBoldLogger.LogThread(Msg: String);
begin
  fThreadLog.Trace(Msg);
end;

{ TFileLogging }

constructor TFileLogging.create(const FileName: string; const MaxSize: LongInt; const Open: Boolean);
begin
  inherited Create;
  fFileName := FileName;
  fIncludeDate := true;
  fMaxSize := MaxSize;
  fLocker:= TCriticalSection.Create;
  FIncludeThreadId := false;
  fLocker.Acquire;
  try
    self.Open := Open;
  finally
    fLocker.Release;
  end;
end;

destructor TFileLogging.Destroy;
begin
  fLocker.Acquire;
  try
    CloseStream;
  finally
    fLocker.Release;
  end;      
  FreeAndNil(fLocker);
  inherited;
end;

function TFileLogging.OpenStream: Boolean;
var
  OpenMode: Word;
begin
  try
    if not FileExists(fFileName) then
      try
        fFileStream := TFileStream.Create(fFileName, fmCreate);
      finally
        FreeAndNil(fFileStream);
      end;
    OpenMode := fmOpenWrite or fmShareDenyWrite;
    fFileStream := TFileStream.Create(fFileName, OpenMode);
    FFileStream.Seek(0, soFromEnd);
    Result := true
  except
    Result := false;
  end;
end;

procedure TFileLogging.Trace(const Entry: string);
var
  line: string;
  Bytes: TBytes;
begin
  fLocker.Acquire;
  try
    if Open then
    begin
      if (fMaxSize > 0) and (fFileStream.Size > fMaxSize) then
        FlushStream;
      if IncludeDate then
        Line := AsISODateTimeMS(now)
      else
        Line := AsISOTimeMS(now);

      Line := Line + ' ' + Entry;

      if IncludeThreadId then
      begin
        if ShortThreadId then
          line := line + Format(':TID=%d', [GetCurrentThreadID])
        else
          line := line + Format(' (ThreadID=%d)', [GetCurrentThreadID]);
      end;

      Line := Line + BOLDCRLF;
      Bytes := TEncoding.UTF8.GetBytes(line);
      fFileStream.write(Bytes, Length(Bytes));
    end;
  finally
    fLocker.Release;
  end;
end;

procedure TFileLogging.CloseStream;
begin
  FreeAndNil(fFileStream);
end;

procedure TFileLogging.FlushStream;
begin
  fFileStream.Size := 0;
end;

procedure TFileLogging.SetOpen(const Value: Boolean);
begin
  fLocker.Acquire;
  try
    if (Value <> fOpen) then
    begin
      fOpen := Value;
      if Value then
        fOpen := OpenStream
      else
        CloseStream;
    end;
  finally
    fLocker.Release;
  end;
end;

procedure TFileLogging.Trace(const Msg: string;
  const Args: array of const);
begin
  self.Trace(Format(Msg, Args));
end;

end.
