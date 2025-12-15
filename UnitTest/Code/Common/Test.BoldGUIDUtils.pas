unit Test.BoldGUIDUtils;

interface

uses
  DUnitX.TestFramework,
  BoldGUIDUtils;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldGUIDUtils = class
  public
    [Test]
    [Category('Quick')]
    procedure TestBoldCreateGUIDAsString;
  end;

implementation

uses
  System.SysUtils;

{ TTestBoldGUIDUtils }

procedure TTestBoldGUIDUtils.TestBoldCreateGUIDAsString;
var
  GUID1, GUID2: string;
begin
  // Test BoldCreateGUIDAsString - covers StringFromCLSID line 27
  GUID1 := BoldCreateGUIDAsString;
  Assert.IsNotEmpty(GUID1, 'GUID should not be empty');
  Assert.AreEqual(38, Length(GUID1), 'GUID with brackets should be 38 chars');
  Assert.AreEqual('{', GUID1[1], 'GUID should start with {');
  Assert.AreEqual('}', GUID1[38], 'GUID should end with }');

  // Test with StripBrackets = True
  GUID2 := BoldCreateGUIDAsString(True);
  Assert.AreEqual(36, Length(GUID2), 'GUID without brackets should be 36 chars');
  Assert.AreNotEqual('{', GUID2[1], 'Stripped GUID should not start with {');

  // Ensure each call generates a unique GUID
  Assert.AreNotEqual(GUID1, '{' + GUID2 + '}', 'Each call should generate unique GUID');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldGUIDUtils);

end.
