{ Global compiler directives }
{$include bold.inc}
unit BoldPerformanceStub;

{
  BoldPerformanceStub - Wrapper unit for AttracsPerformance compatibility
  See BoldStubs.pas for documentation on stub strategy.

  Strategy: DO NOTHING
  Performance tracking is optional and safely skipped.
}

interface

type
  TPerformanceMeasurement = class
  public
    class function ReStart : TPerformanceMeasurement;
  end;

implementation

{ TPerformanceMeasurement }

class function TPerformanceMeasurement.ReStart: TPerformanceMeasurement;
begin

end;

end.
