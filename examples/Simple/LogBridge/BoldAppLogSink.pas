unit BoldAppLogSink;

{$REGION 'Documentation'}
(*
  Unit: BoldAppLogSink

  Description:
    Example sink that bridges Bold's logging to an application's logging system.
    This demonstrates how to integrate Bold logs with your existing log infrastructure.

  Usage:
    1. Copy this unit to your project
    2. Replace the WriteLog implementation with calls to your actual logging system
    3. Register the sink at application startup:

       BoldLogManager.RegisterSink(TBoldAppLogSink.Create);

  Example with AttracsTraceLog:
    procedure TBoldAppLogSink.WriteLog(...);
    begin
      case Level of
        llTrace, llDebug: TraceLog.Trace('[Bold] ' + Msg);
        llInfo:           TraceLog.SystemMessage('[Bold] ' + Msg, ekInfo);
        llWarning:        TraceLog.SystemMessage('[Bold] ' + Msg, ekWarning);
        llError:          TraceLog.SystemMessage('[Bold] ' + Msg, ekError);
      end;
    end;
*)
{$ENDREGION}

interface

uses
  BoldLogInterfaces;

type
  /// <summary>
  /// Example sink that forwards Bold log messages to an application's logging system.
  /// Modify WriteLog to integrate with your specific logging infrastructure.
  /// </summary>
  TBoldAppLogSink = class(TInterfacedObject, IBoldLogSink)
  private
    FEnabled: Boolean;
    FLevels: TBoldLogLevels;
    FName: string;
    FPrefix: string;
  public
    constructor Create(const APrefix: string = '[Bold]');
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
    property Prefix: string read FPrefix write FPrefix;
  end;

implementation

uses
  SysUtils;

{ TBoldAppLogSink }

constructor TBoldAppLogSink.Create(const APrefix: string);
begin
  inherited Create;
  FEnabled := True;
  FLevels := DefaultBoldLogLevels; // Info, Warning, Error
  FName := 'AppLogSink';
  FPrefix := APrefix;
end;

procedure TBoldAppLogSink.WriteLog(const Msg: string; Level: TBoldLogLevel;
  const TimeStamp: TDateTime; ThreadID: Cardinal);
var
  FormattedMsg: string;
begin
  // Format the message with prefix
  if FPrefix <> '' then
    FormattedMsg := FPrefix + ' ' + Msg
  else
    FormattedMsg := Msg;

  // TODO: Replace this with your application's logging calls
  // Examples:
  //
  // For AttracsTraceLog:
  //   case Level of
  //     llTrace, llDebug: TraceLog.Trace(FormattedMsg);
  //     llInfo:           TraceLog.SystemMessage(FormattedMsg, ekInfo);
  //     llWarning:        TraceLog.SystemMessage(FormattedMsg, ekWarning);
  //     llError:          TraceLog.SystemMessage(FormattedMsg, ekError);
  //   end;
  //
  // For CodeSite:
  //   CodeSite.Send(FormattedMsg);
  //
  // For Log4D:
  //   case Level of
  //     llTrace: Logger.Trace(FormattedMsg);
  //     llDebug: Logger.Debug(FormattedMsg);
  //     llInfo:  Logger.Info(FormattedMsg);
  //     llWarning: Logger.Warn(FormattedMsg);
  //     llError: Logger.Error(FormattedMsg);
  //   end;

  // Default implementation: write to console (for demo purposes)
  WriteLn(Format('%s [%s] %s',
    [FormatDateTime('hh:nn:ss.zzz', TimeStamp),
     BoldLogLevelToString(Level),
     FormattedMsg]));
end;

function TBoldAppLogSink.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TBoldAppLogSink.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

function TBoldAppLogSink.GetLevels: TBoldLogLevels;
begin
  Result := FLevels;
end;

procedure TBoldAppLogSink.SetLevels(Value: TBoldLogLevels);
begin
  FLevels := Value;
end;

function TBoldAppLogSink.GetName: string;
begin
  Result := FName;
end;

end.
