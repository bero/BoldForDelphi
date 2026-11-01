unit Test.BoldDbParameter;

{ Tests for TBoldDbParameter (BoldDBInterfaces), the IBoldParameter for
  libraries whose parameters are DB.TParam. No database is needed. }

interface

uses
  Classes,
  SysUtils,
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('Persistence')]
  {$ENDIF}
  TTestBoldDbParameter = class(TBoldTestCase)
  published
    procedure TestInt64RoundTripsBeyond32Bits;
  end;

implementation

uses
  Db,
  BoldDBInterfaces;

procedure TTestBoldDbParameter.TestInt64RoundTripsBeyond32Bits;
const
  Big: Int64 = 9007199254740993; // 2^53 + 1; its low 32 bits are 1
var
  Params: TParams;
  Parameter: IBoldParameter;
begin
  Params := TParams.Create(nil);
  try
    Parameter := TBoldDbParameter.Create(Params.CreateParam(ftLargeint, 'p', ptInput), nil);
    Parameter.AsInt64 := Big;
    Assert.AreEqual(Big, Parameter.AsInt64, 'AsInt64 must keep all 64 bits');
  finally
    Parameter := nil;
    Params.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldDbParameter);

end.
