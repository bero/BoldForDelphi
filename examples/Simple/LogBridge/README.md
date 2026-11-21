# Bold Log Bridge Example

This example shows how to send Bold's log messages to the logging system your
application already has.

## Running it

Open `LogBridgeDemo.dproj` in Delphi and run it. It is a console program that
uses only `BoldLogInterfaces` and `BoldLogSinks`; the project enables only the
Win32 platform so far. It registers the built-in `TBoldFileLogSink`, writes a
few messages at different levels, narrows the sink to errors and back, and
writes everything to `LogBridgeDemo.log` in the working directory. It then
waits for Enter.

The demo program does not use `BoldAppLogSink.pas`. That unit is the bridge
sink to copy into your own project, as described below.

## The problem

Bold has its own log: `BoldLog(...)` and `BoldLogManager` in
`BoldLogInterfaces`. Inside the framework, `BoldLogException` (unit
`BoldExceptionTrace`) writes exceptions with their call stacks to it (#130),
and your own code can write to it as well. Your application likely has its own
logging system (file logger, database logger, etc.), and you want Bold's
messages to appear there alongside your own.

`BoldThreadSafeLog` has a `BoldLog` of its own, a plain file logger that the
propagator uses. It has nothing to do with sinks, so mind which unit comes last
in a `uses` clause that names both.

## The solution

Bold's log is interface based, with pluggable "sinks". A sink is any class that
implements `IBoldLogSink` and receives log messages. You can register several
sinks at the same time.

### How it works

```
Your Application Code          Bold Framework Code
       |                              |
       v                              v
   AppLog(...)                   BoldLog(...)
       |                              |
       v                              v
+------------------------------------------+
|           Your Log Destination           |
|      (file, database, console, etc.)     |
+------------------------------------------+
```

By creating a custom sink that forwards Bold's messages to your logging system,
all logs end up in the same place.

## Creating a custom sink

Implement the `IBoldLogSink` interface:

```pascal
type
  TMyAppLogSink = class(TInterfacedObject, IBoldLogSink)
  private
    FEnabled: Boolean;
    FLevels: TBoldLogLevels;
  public
    constructor Create;
    procedure WriteLog(const Msg: string; Level: TBoldLogLevel;
      const TimeStamp: TDateTime; ThreadID: Cardinal);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetLevels: TBoldLogLevels;
    procedure SetLevels(Value: TBoldLogLevels);
    function GetName: string;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Levels: TBoldLogLevels read GetLevels write SetLevels;
    property Name: string read GetName;
  end;

procedure TMyAppLogSink.WriteLog(const Msg: string; Level: TBoldLogLevel;
  const TimeStamp: TDateTime; ThreadID: Cardinal);
begin
  // Forward to your application's logging system
  case Level of
    llTrace, llDebug: MyAppLogger.Debug('[Bold] ' + Msg);
    llInfo:           MyAppLogger.Info('[Bold] ' + Msg);
    llWarning:        MyAppLogger.Warn('[Bold] ' + Msg);
    llError:          MyAppLogger.Error('[Bold] ' + Msg);
  end;
end;
```

## Registration

Register your sink at application startup, before any Bold operations:

```pascal
procedure InitializeLogging;
begin
  // Register the bridge sink
  BoldLogManager.RegisterSink(TMyAppLogSink.Create);
end;
```

## Log levels

Bold defines these log levels:

| Level | Description |
|-------|-------------|
| `llTrace` | Very detailed diagnostic info |
| `llDebug` | Debug information |
| `llInfo` | General information |
| `llWarning` | Warning conditions |
| `llError` | Error conditions |

You can filter which levels a sink receives. The built-in sinks start with
`DefaultBoldLogLevels` (info, warning and error), so trace and debug messages
are dropped until you widen the set:

```pascal
Sink.Levels := [llWarning, llError];  // Only warnings and errors
Sink.Levels := AllBoldLogLevels;      // Everything
```

## Multiple sinks

You can register several sinks for different purposes:

```pascal
// Log everything to a file
var FileSink: IBoldLogSink := TBoldFileLogSink.Create('bold.log');
FileSink.Levels := AllBoldLogLevels;
BoldLogManager.RegisterSink(FileSink);

// Also send errors to your application's main log
var AppSink: IBoldLogSink := TMyAppLogSink.Create;
AppSink.Levels := [llError];
BoldLogManager.RegisterSink(AppSink);
```

Declare the variables as `IBoldLogSink`, not as the class: the sinks are
reference counted.

## Files in this example

| File | What it holds |
| --- | --- |
| `BoldAppLogSink.pas` | `TBoldAppLogSink`, an example bridge sink to copy into your project |
| `LogBridgeDemo.dpr` | the console demo, with the built-in file sink |
| `LogBridgeDemo.dproj` | the Delphi project |

## See also

- [`boldloginterfaces.pas`](../../../Source/Common/Logging/boldloginterfaces.pas)
  (unit `BoldLogInterfaces`): `IBoldLogSink`, `BoldLogManager`, `BoldLog`
- [`boldlogsinks.pas`](../../../Source/Common/Logging/boldlogsinks.pas)
  (unit `BoldLogSinks`): the file, console, debug and memory sinks
