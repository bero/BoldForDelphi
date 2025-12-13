{ Global compiler directives }
{$include bold.inc}
unit BoldStubs;

(*
  BoldStubs - Central stub implementations for Attracs-specific functionality
  ============================================================================

  PURPOSE:
  This unit provides stub implementations that allow Bold to compile with
  the ATTRACS define without requiring actual Attracs-specific units.

  STUB STRATEGY:
  Each stub follows one of these strategies:

  1. DO NOTHING - Function/procedure has no effect
     Used for: Performance tracking, SpanFetch optimization
     Reason: These are optional optimizations that can be safely skipped

  2. DELEGATE TO BOLDLOG - Logging calls are forwarded to Bold logger
     Used for: TraceLog
     Reason: Logging should work; Bold logger provides equivalent functionality

  3. RETURN SAFE DEFAULT - Functions return values that disable optional features
     Used for: InSpanFetch returns False
     Reason: Ensures code paths that check for SpanFetch behave correctly

  ORIGINAL ATTRACS UNITS AND THEIR STUBS:
  ----------------------------------------
  AttracsTraceLog         -> BoldTraceLog        : Delegates to BoldLog
  AttracsSpanFetchManager -> BoldSpanFetchManager: Returns False / does nothing
  AttracsSpanFetch        -> BoldSpanFetch       : Empty stub
  AttracsDefs             -> BoldStubDefs        : Empty stub (type definitions)
  AttracsPerformance      -> BoldPerformanceStub : Empty stub

  TO ENABLE FULL ATTRACS FUNCTIONALITY:
  Place actual Attracs units earlier in the search path than the Stubs folder.
  The source files will then use the real implementations instead of these stubs.
*)

interface

uses
  BoldElements;

type
  { TEventKind - Log event severity levels }
  TEventKind = (ekInfo, ekWarning, ekError, ekDebug);

  { TTraceLog - Logging interface that delegates to BoldLogHandler }
  TTraceLog = class
  public
    procedure SystemMessage(const Msg: string; EventKind: TEventKind); overload;
    procedure SystemMessage(const Fmt: string; const Args: array of const; EventKind: TEventKind = ekInfo); overload;
    function LogFileDir: string;
  end;

{ TraceLog functions - Strategy: DELEGATE TO BOLDLOG }
function TraceLogAssigned: Boolean;
function TraceLog: TTraceLog;

{ SpanFetch functions - Strategy: RETURN SAFE DEFAULT / DO NOTHING }
function InSpanFetch: Boolean;
procedure PrefetchDerivedMember(Element: TBoldElement);

implementation

uses
  SysUtils,
  BoldDefs,
  BoldLogHandler;

var
  GTraceLog: TTraceLog = nil;

{ Helper function to map TEventKind to TBoldLogType }
function EventKindToLogType(EventKind: TEventKind): TBoldLogType;
begin
  case EventKind of
    ekInfo: Result := ltInfo;
    ekWarning: Result := ltWarning;
    ekError: Result := ltError;
    ekDebug: Result := ltDetail;
  else
    Result := ltInfo;
  end;
end;

{ TraceLog implementation - delegates to BoldLog }

function TraceLogAssigned: Boolean;
begin
  Result := True; // Always available since we delegate to BoldLog
end;

function TraceLog: TTraceLog;
begin
  if GTraceLog = nil then
    GTraceLog := TTraceLog.Create;
  Result := GTraceLog;
end;

procedure TTraceLog.SystemMessage(const Msg: string; EventKind: TEventKind);
begin
  BoldLog.Log(Msg, EventKindToLogType(EventKind));
end;

procedure TTraceLog.SystemMessage(const Fmt: string; const Args: array of const; EventKind: TEventKind);
begin
  BoldLog.LogFmt(Fmt, Args, EventKindToLogType(EventKind));
end;

function TTraceLog.LogFileDir: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

{ SpanFetch implementation - returns safe defaults / does nothing }

function InSpanFetch: Boolean;
begin
  Result := False; // Not in span fetch when using stubs
end;

procedure PrefetchDerivedMember(Element: TBoldElement);
begin
  // Do nothing - SpanFetch optimization not available in stub mode
end;

initialization

finalization
  GTraceLog.Free;

end.
