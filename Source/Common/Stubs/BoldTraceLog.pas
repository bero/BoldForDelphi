{ Global compiler directives }
{$include bold.inc}
unit BoldTraceLog;

{
  BoldTraceLog - Wrapper unit for AttracsTraceLog compatibility
  See BoldStubs.pas for documentation on stub strategy.

  Strategy: DELEGATE TO BOLDLOG
  All logging calls are forwarded to Bold's standard logging system.
}

interface

uses
  BoldStubs;

type
  TEventKind = BoldStubs.TEventKind;
  TTraceLog = BoldStubs.TTraceLog;

function TraceLogAssigned: Boolean;
function TraceLog: TTraceLog;

implementation

function TraceLogAssigned: Boolean;
begin
  Result := BoldStubs.TraceLogAssigned;
end;

function TraceLog: TTraceLog;
begin
  Result := BoldStubs.TraceLog;
end;

end.
