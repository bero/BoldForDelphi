unit Test.BoldBase64;

interface

uses
  DUnitX.TestFramework,
  BoldBase64;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldBase64 = class
  private
    FBase64: TBase64;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [Category('Quick')]
    procedure TestCreate_DefaultFilterDecodeInput;

    [Test]
    procedure TestEncodeData_EmptyString;
    [Test]
    procedure TestEncodeData_SingleChar;
    [Test]
    procedure TestEncodeData_TwoChars;
    [Test]
    procedure TestEncodeData_ThreeChars;
    [Test]
    procedure TestEncodeData_StandardString;
    [Test]
    procedure TestEncodeData_BinaryData;

    [Test]
    procedure TestDecodeData_EmptyString;
    [Test]
    procedure TestDecodeData_StandardString;
    [Test]
    procedure TestDecodeData_WithPadding;
    [Test]
    procedure TestDecodeData_InvalidLength;
    [Test]
    procedure TestDecodeData_InvalidCharacter;
    [Test]
    procedure TestDecodeData_FilteredInput;

    [Test]
    procedure TestRoundTrip_SimpleString;
    [Test]
    procedure TestRoundTrip_BinaryData;
    [Test]
    procedure TestRoundTrip_AllBytes;

    [Test]
    procedure TestDecodeData_DataLeftError;
    [Test]
    procedure TestDecodeData_PaddingError;

    { Position-specific decode error tests }
    [Test]
    procedure TestDecodeData_InvalidFirstChar;
    [Test]
    procedure TestDecodeData_InvalidSecondChar;
    [Test]
    procedure TestDecodeData_DataLeftFourthByte;
    [Test]
    procedure TestDecodeData_InvalidFourthChar;
  end;

implementation

uses
  System.SysUtils,
  BoldDefs;

{ TTestBoldBase64 }

procedure TTestBoldBase64.Setup;
begin
  FBase64 := TBase64.Create;
end;

procedure TTestBoldBase64.TearDown;
begin
  FBase64.Free;
end;

procedure TTestBoldBase64.TestCreate_DefaultFilterDecodeInput;
begin
  Assert.IsTrue(FBase64.FilterdecodeInput, 'Default FilterdecodeInput should be True');
end;

procedure TTestBoldBase64.TestEncodeData_EmptyString;
var
  Output: string;
  ResultCode: Byte;
begin
  ResultCode := FBase64.EncodeData('', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string(''), Output);
end;

procedure TTestBoldBase64.TestEncodeData_SingleChar;
var
  Output: string;
  ResultCode: Byte;
begin
  // Single char 'M' should encode to 'TQ==' (with padding)
  ResultCode := FBase64.EncodeData('M', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('TQ=='), Output);
end;

procedure TTestBoldBase64.TestEncodeData_TwoChars;
var
  Output: string;
  ResultCode: Byte;
begin
  // Two chars 'Ma' should encode to 'TWE=' (with single padding)
  ResultCode := FBase64.EncodeData('Ma', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('TWE='), Output);
end;

procedure TTestBoldBase64.TestEncodeData_ThreeChars;
var
  Output: string;
  ResultCode: Byte;
begin
  // Three chars 'Man' should encode to 'TWFu' (no padding)
  ResultCode := FBase64.EncodeData('Man', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('TWFu'), Output);
end;

procedure TTestBoldBase64.TestEncodeData_StandardString;
var
  Output: string;
  ResultCode: Byte;
begin
  // Standard test string
  ResultCode := FBase64.EncodeData('Hello, World!', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('SGVsbG8sIFdvcmxkIQ=='), Output);
end;

procedure TTestBoldBase64.TestEncodeData_BinaryData;
var
  Input: TBoldAnsiString;
  Output: string;
  ResultCode: Byte;
begin
  // Binary data with null bytes
  Input := AnsiChar(#0) + AnsiChar(#1) + AnsiChar(#255);
  ResultCode := FBase64.EncodeData(Input, Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('AAH/'), Output);
end;

procedure TTestBoldBase64.TestDecodeData_EmptyString;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  ResultCode := FBase64.DecodeData('', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
end;

procedure TTestBoldBase64.TestDecodeData_StandardString;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  ResultCode := FBase64.DecodeData('SGVsbG8sIFdvcmxkIQ==', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('Hello, World!'), string(Output));
end;

procedure TTestBoldBase64.TestDecodeData_WithPadding;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  // Single padding
  ResultCode := FBase64.DecodeData('TWE=', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('Ma'), string(Output));

  // Double padding
  ResultCode := FBase64.DecodeData('TQ==', Output);
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('M'), string(Output));
end;

procedure TTestBoldBase64.TestDecodeData_InvalidLength;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  ResultCode := FBase64.DecodeData('ABC', Output); // Length not multiple of 4
  Assert.AreEqual(Byte(BASE64_LENGTH), ResultCode);
end;

procedure TTestBoldBase64.TestDecodeData_InvalidCharacter;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  ResultCode := FBase64.DecodeData('AB$D', Output); // $ is invalid
  Assert.AreEqual(Byte(BASE64_INVALID), ResultCode);
end;

procedure TTestBoldBase64.TestDecodeData_FilteredInput;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  // With filtering enabled (default), invalid chars should be stripped
  FBase64.FilterdecodeInput := True;
  ResultCode := FBase64.DecodeData('TW Fu', Output); // Space should be filtered
  Assert.AreEqual(Byte(BASE64_OK), ResultCode);
  Assert.AreEqual(string('Man'), string(Output));
end;

procedure TTestBoldBase64.TestRoundTrip_SimpleString;
var
  Original: TBoldAnsiString;
  Encoded: string;
  Decoded: TBoldAnsiString;
begin
  Original := 'The quick brown fox jumps over the lazy dog.';
  Assert.AreEqual(Byte(BASE64_OK), FBase64.EncodeData(Original, Encoded));
  Assert.AreEqual(Byte(BASE64_OK), FBase64.DecodeData(Encoded, Decoded));
  Assert.AreEqual(string(Original), string(Decoded));
end;

procedure TTestBoldBase64.TestRoundTrip_BinaryData;
var
  Original: TBoldAnsiString;
  Encoded: string;
  Decoded: TBoldAnsiString;
  I: Integer;
begin
  // Create string with all possible byte values 0..255
  SetLength(Original, 256);
  for I := 0 to 255 do
    Original[I + 1] := AnsiChar(I);

  Assert.AreEqual(Byte(BASE64_OK), FBase64.EncodeData(Original, Encoded));
  Assert.AreEqual(Byte(BASE64_OK), FBase64.DecodeData(Encoded, Decoded));
  Assert.AreEqual(Length(Original), Length(Decoded));
  for I := 1 to Length(Original) do
    Assert.AreEqual(Integer(Ord(Original[I])), Integer(Ord(Decoded[I])), Format('Byte %d mismatch', [I - 1]));
end;

procedure TTestBoldBase64.TestRoundTrip_AllBytes;
var
  Original: TBoldAnsiString;
  Encoded: string;
  Decoded: TBoldAnsiString;
begin
  Original := AnsiChar(#0) + AnsiChar(#127) + AnsiChar(#128) + AnsiChar(#255);
  Assert.AreEqual(Byte(BASE64_OK), FBase64.EncodeData(Original, Encoded));
  Assert.AreEqual(Byte(BASE64_OK), FBase64.DecodeData(Encoded, Decoded));
  Assert.AreEqual(string(Original), string(Decoded));
end;

procedure TTestBoldBase64.TestDecodeData_DataLeftError;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  // '=' in wrong position (not at end-1 when followed by non-pad)
  // Crafting input where first pad is not at position Length-1
  ResultCode := FBase64.DecodeData('TW==AAAA', Output);
  Assert.AreEqual(Byte(BASE64_DATALEFT), ResultCode);
end;

procedure TTestBoldBase64.TestDecodeData_PaddingError;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  // Padding followed by non-pad character
  ResultCode := FBase64.DecodeData('TW=A', Output);
  Assert.AreEqual(Byte(BASE64_PADDING), ResultCode);
end;

{ Position-specific decode error tests }

procedure TTestBoldBase64.TestDecodeData_InvalidFirstChar;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  // Invalid char '$' at position 1 (first byte of quartet)
  ResultCode := FBase64.DecodeData('$BCD', Output);
  Assert.AreEqual(Byte(BASE64_INVALID), ResultCode);
end;

procedure TTestBoldBase64.TestDecodeData_InvalidSecondChar;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  // Invalid char '$' at position 2 (second byte of quartet)
  ResultCode := FBase64.DecodeData('A$CD', Output);
  Assert.AreEqual(Byte(BASE64_INVALID), ResultCode);
end;

procedure TTestBoldBase64.TestDecodeData_DataLeftFourthByte;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  // Pad '=' at position 4 but more data follows (i <> InputLength)
  ResultCode := FBase64.DecodeData('AAA=AAAA', Output);
  Assert.AreEqual(Byte(BASE64_DATALEFT), ResultCode);
end;

procedure TTestBoldBase64.TestDecodeData_InvalidFourthChar;
var
  Output: TBoldAnsiString;
  ResultCode: Byte;
begin
  FBase64.FilterdecodeInput := False;
  // Invalid char '$' at position 4 (fourth byte of quartet)
  ResultCode := FBase64.DecodeData('AAA$', Output);
  Assert.AreEqual(Byte(BASE64_INVALID), ResultCode);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldBase64);

end.
