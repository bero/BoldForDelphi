unit Test.BoldSSExcept;

interface

uses
  DUnitX.TestFramework,
  BoldSSExcept;

type
  /// <summary>
  /// Test fixture for BoldSSExcept - Parser exception classes
  /// </summary>
  [TestFixture]
  [Category('ObjectSpace')]
  TTestBoldSSExcept = class
  public
    [Test]
    [Category('Quick')]
    procedure TestCreate;
    [Test]
    procedure TestCreateName;
    [Test]
    procedure TestCreateLong;
    [Test]
    procedure TestCreateLongLongNameLen;
    [Test]
    procedure TestIdProperty;
    [Test]
    procedure TestPositionProperty;
  end;

implementation

uses
  System.SysUtils;

{ TTestBoldSSExcept }

procedure TTestBoldSSExcept.TestCreate;
var
  Ex: SSException;
begin
  Ex := SSException.Create(SSExceptionLexError, 'Test error message');
  try
    Assert.AreEqual('Test error message', Ex.Message, 'Message should match');
    Assert.AreEqual(Ord(SSExceptionLexError), Ord(Ex.Id), 'Id should be set');
  finally
    Ex.Free;
  end;
end;

procedure TTestBoldSSExcept.TestCreateName;
var
  Ex: SSException;
begin
  Ex := SSException.CreateName(SSExceptionLexFileOpen, 'Cannot open file: %s', 'test.txt');
  try
    Assert.AreEqual('Cannot open file: test.txt', Ex.Message, 'Message should be formatted');
    Assert.AreEqual(Ord(SSExceptionLexFileOpen), Ord(Ex.Id), 'Id should be set');
    Assert.AreEqual(0, Ex.Position, 'Position should be 0');
  finally
    Ex.Free;
  end;
end;

procedure TTestBoldSSExcept.TestCreateLong;
var
  Ex: SSException;
begin
  Ex := SSException.CreateLong(SSExceptionYaccParse, 'Error at position %ld', 42);
  try
    Assert.AreEqual('Error at position 42', Ex.Message, 'Message should replace %ld with integer');
    Assert.AreEqual(Ord(SSExceptionYaccParse), Ord(Ex.Id), 'Id should be set');
    Assert.AreEqual(0, Ex.Position, 'Position should be 0');
  finally
    Ex.Free;
  end;
end;

procedure TTestBoldSSExcept.TestCreateLongLongNameLen;
var
  Ex: SSException;
begin
  Ex := SSException.CreateLongLongNameLen(SSExceptionYaccElement,
    'Error at line %ld, col %ld near "%s"', 10, 25, 'TestTokenName', 9);
  try
    Assert.Contains(Ex.Message, '10', 'Message should contain first long');
    Assert.Contains(Ex.Message, '25', 'Message should contain second long');
    Assert.Contains(Ex.Message, 'TestToken', 'Message should contain truncated name');
    Assert.AreEqual(Ord(SSExceptionYaccElement), Ord(Ex.Id), 'Id should be set');
    Assert.AreEqual(25, Ex.Position, 'Position should be second long value');
  finally
    Ex.Free;
  end;
end;

procedure TTestBoldSSExcept.TestIdProperty;
var
  Ex: SSException;
begin
  Ex := SSException.Create(SSExceptionOutOfMemory, 'Out of memory');
  try
    Assert.AreEqual(SSExceptionOutOfMemory, Ex.Id, 'Id should be SSExceptionOutOfMemory');
  finally
    Ex.Free;
  end;
end;

procedure TTestBoldSSExcept.TestPositionProperty;
var
  Ex: SSException;
begin
  Ex := SSException.CreateLongLongNameLen(SSExceptionLexError,
    'At %ld:%ld token "%s"', 5, 123, 'identifier', 10);
  try
    Assert.AreEqual(123, Ex.Position, 'Position should be set from second long');
  finally
    Ex.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSSExcept);

end.
