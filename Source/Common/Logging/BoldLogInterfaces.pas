
{ Global compiler directives }
{$include bold.inc}
unit BoldLogInterfaces;

{$REGION 'Documentation'}
(*
  🗒️ Unit: BoldLogInterfaces

  📜 Description:
    Defines interfaces for a pluggable logging system where applications log against
    abstractions without knowing the concrete destination. Log sinks (file, database,
    console, etc.) can be registered at runtime.

  🔖 Core Interfaces:
    1. 📝 IBoldLogSink: Implemented by log destinations (file writer, database writer, etc.)
       - Receives log messages and writes them to the destination
       - Can filter by log level
       - Multiple sinks can be active simultaneously

    2. 🎯 IBoldLogManager: Central manager for logging
       - Register/unregister sinks
       - Dispatch log messages to all registered sinks
       - Thread-safe operations

  ✨ Features:
    - 🔌 Pluggable: Register any sink implementation at runtime
    - 📊 Multiple Sinks: Log to file AND database simultaneously
    - 🏷️ Log Levels: Filter messages by severity (Trace, Debug, Info, Warning, Error)
    - 🛡️ Thread-Safe: Safe for multi-threaded applications
    - 🎭 Decoupled: Application code doesn't know where logs go

  🛠️ Usage:
    // Register a file sink
    BoldLogManager.RegisterSink(TBoldFileLogSink.Create('app.log'));

    // Log messages (goes to all registered sinks)
    BoldLogManager.Log('Application started');
    BoldLogManager.Log('User %s logged in', [UserName], llInfo);
    BoldLogManager.Log('Connection failed', llError);

    // Or use the global shortcut
    BoldLog('Processing complete');
*)
{$ENDREGION}

interface

uses
  Classes;

type
  TBoldLogLevel = (llTrace, llDebug, llInfo, llWarning, llError);
  TBoldLogLevels = set of TBoldLogLevel;

const
  AllBoldLogLevels = [llTrace, llDebug, llInfo, llWarning, llError];
  DefaultBoldLogLevels = [llInfo, llWarning, llError];

type
  IBoldLogSink = interface
    ['{A1B2C3D4-E5F6-4A5B-8C9D-0E1F2A3B4C5D}']
    procedure WriteLog(const Msg: string; Level: TBoldLogLevel; const TimeStamp: TDateTime; ThreadID: Cardinal);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetLevels: TBoldLogLevels;
    procedure SetLevels(Value: TBoldLogLevels);
    function GetName: string;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Levels: TBoldLogLevels read GetLevels write SetLevels;
    property Name: string read GetName;
  end;

  IBoldLogManager = interface
    ['{D4C3B2A1-6F5E-B5A4-D9C8-F1E0B3A2C4D5}']
    procedure RegisterSink(const Sink: IBoldLogSink);
    procedure UnregisterSink(const Sink: IBoldLogSink);
    procedure UnregisterAllSinks;
    procedure Log(const Msg: string; Level: TBoldLogLevel = llInfo); overload;
    procedure Log(const Msg: string; const Args: array of const; Level: TBoldLogLevel = llInfo); overload;
    procedure Trace(const Msg: string); overload;
    procedure Trace(const Msg: string; const Args: array of const); overload;
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Msg: string; const Args: array of const); overload;
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
    procedure Warning(const Msg: string); overload;
    procedure Warning(const Msg: string; const Args: array of const); overload;
    procedure Error(const Msg: string); overload;
    procedure Error(const Msg: string; const Args: array of const); overload;
    function GetSinkCount: Integer;
    property SinkCount: Integer read GetSinkCount;
  end;

function BoldLogManager: IBoldLogManager;

// Global convenience procedures
procedure BoldLog(const Msg: string; Level: TBoldLogLevel = llInfo); overload;
procedure BoldLog(const Msg: string; const Args: array of const; Level: TBoldLogLevel = llInfo); overload;

function BoldLogLevelToString(Level: TBoldLogLevel): string;

implementation

uses
  System.SysUtils,
  System.SyncObjs,
  WinApi.Windows,
  System.Generics.Collections;

type
  TBoldLogManager = class(TInterfacedObject, IBoldLogManager)
  private
    FSinks: TList<IBoldLogSink>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterSink(const Sink: IBoldLogSink);
    procedure UnregisterSink(const Sink: IBoldLogSink);
    procedure UnregisterAllSinks;
    procedure Log(const Msg: string; Level: TBoldLogLevel = llInfo); overload;
    procedure Log(const Msg: string; const Args: array of const; Level: TBoldLogLevel = llInfo); overload;
    procedure Trace(const Msg: string); overload;
    procedure Trace(const Msg: string; const Args: array of const); overload;
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Msg: string; const Args: array of const); overload;
    procedure Info(const Msg: string); overload;
    procedure Info(const Msg: string; const Args: array of const); overload;
    procedure Warning(const Msg: string); overload;
    procedure Warning(const Msg: string; const Args: array of const); overload;
    procedure Error(const Msg: string); overload;
    procedure Error(const Msg: string; const Args: array of const); overload;
    function GetSinkCount: Integer;
  end;

var
  GBoldLogManager: IBoldLogManager = nil;
  GLogManagerLock: TCriticalSection = nil;

function BoldLogManager: IBoldLogManager;
begin
  if GBoldLogManager = nil then
  begin
    GLogManagerLock.Acquire;
    try
      if GBoldLogManager = nil then
        GBoldLogManager := TBoldLogManager.Create;
    finally
      GLogManagerLock.Release;
    end;
  end;
  Result := GBoldLogManager;
end;

procedure BoldLog(const Msg: string; Level: TBoldLogLevel = llInfo);
begin
  BoldLogManager.Log(Msg, Level);
end;

procedure BoldLog(const Msg: string; const Args: array of const; Level: TBoldLogLevel = llInfo);
begin
  BoldLogManager.Log(Msg, Args, Level);
end;

function BoldLogLevelToString(Level: TBoldLogLevel): string;
begin
  case Level of
    llTrace: Result := 'TRACE';
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARN';
    llError: Result := 'ERROR';
  else
    Result := 'UNKNOWN';
  end;
end;

{ TBoldLogManager }

constructor TBoldLogManager.Create;
begin
  inherited Create;
  FSinks := TList<IBoldLogSink>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TBoldLogManager.Destroy;
begin
  FLock.Acquire;
  try
    FSinks.Clear;
  finally
    FLock.Release;
  end;
  FLock.Free;
  FSinks.Free;
  inherited;
end;

procedure TBoldLogManager.RegisterSink(const Sink: IBoldLogSink);
begin
  FLock.Acquire;
  try
    if not FSinks.Contains(Sink) then
      FSinks.Add(Sink);
  finally
    FLock.Release;
  end;
end;

procedure TBoldLogManager.UnregisterSink(const Sink: IBoldLogSink);
begin
  FLock.Acquire;
  try
    FSinks.Remove(Sink);
  finally
    FLock.Release;
  end;
end;

procedure TBoldLogManager.UnregisterAllSinks;
begin
  FLock.Acquire;
  try
    FSinks.Clear;
  finally
    FLock.Release;
  end;
end;

procedure TBoldLogManager.Log(const Msg: string; Level: TBoldLogLevel);
var
  Sink: IBoldLogSink;
  TimeStamp: TDateTime;
  ThreadID: Cardinal;
  SinksCopy: TArray<IBoldLogSink>;
  i: Integer;
begin
  TimeStamp := Now;
  ThreadID := GetCurrentThreadId;

  FLock.Acquire;
  try
    SetLength(SinksCopy, FSinks.Count);
    for i := 0 to FSinks.Count - 1 do
      SinksCopy[i] := FSinks[i];
  finally
    FLock.Release;
  end;

  for Sink in SinksCopy do
  begin
    if Sink.Enabled and (Level in Sink.Levels) then
      Sink.WriteLog(Msg, Level, TimeStamp, ThreadID);
  end;
end;

procedure TBoldLogManager.Log(const Msg: string; const Args: array of const; Level: TBoldLogLevel);
begin
  Log(Format(Msg, Args), Level);
end;

procedure TBoldLogManager.Trace(const Msg: string);
begin
  Log(Msg, llTrace);
end;

procedure TBoldLogManager.Trace(const Msg: string; const Args: array of const);
begin
  Log(Msg, Args, llTrace);
end;

procedure TBoldLogManager.Debug(const Msg: string);
begin
  Log(Msg, llDebug);
end;

procedure TBoldLogManager.Debug(const Msg: string; const Args: array of const);
begin
  Log(Msg, Args, llDebug);
end;

procedure TBoldLogManager.Info(const Msg: string);
begin
  Log(Msg, llInfo);
end;

procedure TBoldLogManager.Info(const Msg: string; const Args: array of const);
begin
  Log(Msg, Args, llInfo);
end;

procedure TBoldLogManager.Warning(const Msg: string);
begin
  Log(Msg, llWarning);
end;

procedure TBoldLogManager.Warning(const Msg: string; const Args: array of const);
begin
  Log(Msg, Args, llWarning);
end;

procedure TBoldLogManager.Error(const Msg: string);
begin
  Log(Msg, llError);
end;

procedure TBoldLogManager.Error(const Msg: string; const Args: array of const);
begin
  Log(Msg, Args, llError);
end;

function TBoldLogManager.GetSinkCount: Integer;
begin
  FLock.Acquire;
  try
    Result := FSinks.Count;
  finally
    FLock.Release;
  end;
end;

initialization
  GLogManagerLock := TCriticalSection.Create;

finalization
  GBoldLogManager := nil;
  FreeAndNil(GLogManagerLock);

end.
