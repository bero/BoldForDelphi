# Bold Log Bridge Example

This example demonstrates how to integrate Bold's logging system with your application's existing logging infrastructure.

## The Problem

Bold framework has internal logging calls (`BoldLog(...)`) for diagnostics, warnings, and errors. Your application likely has its own logging system (file logger, database logger, etc.). You want Bold's log messages to appear in your application's logs alongside your own messages.

## The Solution

Bold uses an interface-based logging system with pluggable "sinks". A sink is any class that implements `IBoldLogSink` and receives log messages. You can register multiple sinks simultaneously.

### How It Works

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

By creating a custom sink that forwards Bold's messages to your logging system, all logs end up in the same place.

## Creating a Custom Sink

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

## Log Levels

Bold defines these log levels:

| Level | Description |
|-------|-------------|
| `llTrace` | Very detailed diagnostic info |
| `llDebug` | Debug information |
| `llInfo` | General information |
| `llWarning` | Warning conditions |
| `llError` | Error conditions |

You can filter which levels your sink receives:

```pascal
Sink.Levels := [llWarning, llError];  // Only warnings and errors
Sink.Levels := AllBoldLogLevels;       // Everything
```

## Multiple Sinks

You can register multiple sinks for different purposes:

```pascal
// Log everything to file
BoldLogManager.RegisterSink(TBoldFileLogSink.Create('bold.log'));

// Also send errors to your app's main log
var AppSink := TMyAppLogSink.Create;
AppSink.Levels := [llError];
BoldLogManager.RegisterSink(AppSink);
```

## Files in This Example

- `BoldAppLogSink.pas` - Example sink implementation
- `LogBridgeDemo.dpr` - Console demo showing the concept
- `LogBridgeDemo.dproj` - Delphi project file

## See Also

- `Source/Common/Logging/BoldLogInterfaces.pas` - Interface definitions
- `Source/Common/Logging/BoldLogSinks.pas` - Built-in sink implementations
