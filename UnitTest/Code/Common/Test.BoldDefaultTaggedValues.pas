unit Test.BoldDefaultTaggedValues;

interface

uses
  BoldTestFramework,
  BoldDefaultTaggedValues;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldDefaultTaggedValues = class(TBoldTestCase)
  published
    procedure TestTVIsFalse;
  end;

implementation

{ TTestBoldDefaultTaggedValues }

procedure TTestBoldDefaultTaggedValues.TestTVIsFalse;
begin
  Assert.IsTrue(TVIsFalse('False'), 'TVIsFalse should return True for "False"');
  Assert.IsTrue(TVIsFalse('false'), 'TVIsFalse should return True for "false" (case insensitive)');
  Assert.IsFalse(TVIsFalse('True'), 'TVIsFalse should return False for "True"');
  Assert.IsFalse(TVIsFalse(''), 'TVIsFalse should return False for empty string');
end;

initialization
  BoldRegisterTest(TTestBoldDefaultTaggedValues);

end.
