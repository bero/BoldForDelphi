{ Global compiler directives }
{$include bold.inc}
unit BoldPerformanceStub;

{
  BoldPerformanceStub - Wrapper unit for AttracsPerformance compatibility
  See BoldStubs.pas for documentation on stub strategy.

  Strategy: DO NOTHING
  Performance tracking is optional and safely skipped.
  All methods are no-ops that return safe default values.
}

interface

type
  TPerformanceMeasurement = record
  public
    WhatMeasured: string;
    WhatMeasuredParameter: string;
    class function ReStart: TPerformanceMeasurement; static;
    function AcceptableTimeForSmallComputation: Boolean;
    procedure Trace;
  end;

implementation

{ TPerformanceMeasurement }

class function TPerformanceMeasurement.ReStart: TPerformanceMeasurement;
begin
  Result := Default(TPerformanceMeasurement);
end;

function TPerformanceMeasurement.AcceptableTimeForSmallComputation: Boolean;
begin
  Result := False; // Never log
end;

procedure TPerformanceMeasurement.Trace;
begin
  // Do nothing
end;

end.
