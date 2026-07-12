unit Test.BoldDbCopy;

{ DUnitX tests for BoldDbCopy - control character stripping (H9) }

interface

uses
  SysUtils,
  DUnitX.TestFramework,
  BoldDbCopy;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbCopy = class
  public
    [Test]
    [Category('Quick')]
    procedure TestStripControlCharsRemovesTrailingControlChar;
    [Test]
    [Category('Quick')]
    procedure TestStripControlCharsKeepsPlainText;
  end;

implementation

procedure TTestBoldDbCopy.TestStripControlCharsRemovesTrailingControlChar;
begin
  // Regression for H9: 'for x := Length(s)-1 downto 1' never examined the
  // LAST character (Delphi strings are 1-based), so a trailing control char
  // (e.g. #0) survived - exactly the byte PostgreSQL then rejects with
  // 'invalid byte sequence for encoding', feeding the batch retry loop.
  Assert.AreEqual('abc', TBoldDbCopy.StripControlChars('abc'#0),
    'trailing control character must be removed');
  Assert.AreEqual('abc', TBoldDbCopy.StripControlChars(#7'a'#13'b'#10'c'#0),
    'control characters in all positions must be removed');
  Assert.AreEqual('', TBoldDbCopy.StripControlChars(#0),
    'a single control character must be removed');
end;

procedure TTestBoldDbCopy.TestStripControlCharsKeepsPlainText;
begin
  Assert.AreEqual('abc def', TBoldDbCopy.StripControlChars('abc def'),
    'plain text must pass through unchanged');
  Assert.AreEqual('', TBoldDbCopy.StripControlChars(''),
    'empty string must pass through');
  Assert.AreEqual('åäö€', TBoldDbCopy.StripControlChars('åäö€'),
    'non-ASCII printable characters must pass through');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbCopy);

end.
