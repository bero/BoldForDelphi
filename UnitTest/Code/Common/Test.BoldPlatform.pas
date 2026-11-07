unit Test.BoldPlatform;

{ BoldPlatform: the operating-system calls of the Bold runtime, one
  implementation per platform. BoldUnicodeCompareLen uses CompareStringEx with
  Delphi on Windows and the RTL's locale-aware comparison elsewhere; every
  implementation must give the same three results. Non-ASCII case folding is
  left out, because under Free Pascal it follows the process locale. }

interface

uses
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldPlatform = class(TBoldTestCase)
  published
    procedure TestEqualStringsCompareEqual;
    procedure TestOrderGivesMinusOneOrOne;
    procedure TestCaseInsensitiveIgnoresCase;
    procedure TestOnlyTheFirstNCharactersCount;
  end;

implementation

uses
  BoldPlatform;

procedure TTestBoldPlatform.TestEqualStringsCompareEqual;
begin
  Assert.AreEqual(0, BoldUnicodeCompareLen(True, 'abc', 'abc', 3));
  Assert.AreEqual(0, BoldUnicodeCompareLen(False, 'abc', 'abc', 3));
end;

procedure TTestBoldPlatform.TestOrderGivesMinusOneOrOne;
begin
  // Exactly -1 and 1, not a difference of character codes
  Assert.AreEqual(-1, BoldUnicodeCompareLen(True, 'abc', 'abx', 3));
  Assert.AreEqual(1, BoldUnicodeCompareLen(True, 'abx', 'abc', 3));
  Assert.AreEqual(-1, BoldUnicodeCompareLen(False, 'abc', 'ABX', 3));
  Assert.AreEqual(1, BoldUnicodeCompareLen(False, 'ABX', 'abc', 3));
end;

procedure TTestBoldPlatform.TestCaseInsensitiveIgnoresCase;
begin
  Assert.AreEqual(0, BoldUnicodeCompareLen(False, 'Bold', 'bOLD', 4));
  Assert.AreNotEqual(0, BoldUnicodeCompareLen(True, 'Bold', 'bOLD', 4));
end;

procedure TTestBoldPlatform.TestOnlyTheFirstNCharactersCount;
begin
  Assert.AreEqual(0, BoldUnicodeCompareLen(True, 'abcX', 'abcY', 3));
  Assert.AreEqual(-1, BoldUnicodeCompareLen(True, 'abcX', 'abcY', 4));
end;

initialization
  BoldRegisterTest(TTestBoldPlatform);

end.
