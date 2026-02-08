unit Test.BoldNameExpander;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestBoldNameExpander = class
  public
    // BoldExpandName function (covers all 3 expander creation paths)
    [Test] [Category('Quick')]
    procedure TestBoldExpandName_SQL;
    [Test] [Category('Quick')]
    procedure TestBoldExpandName_Delphi;
    [Test] [Category('Quick')]
    procedure TestBoldExpandName_Expression;

    // BoldExpandPrefix
    [Test] [Category('Quick')]
    procedure TestBoldExpandPrefix_ReplacesTag;
    [Test] [Category('Quick')]
    procedure TestBoldExpandPrefix_NoTag;

    // TBoldAbstractNameExpander.ExpandName
    [Test] [Category('Quick')]
    procedure TestExpandName_ExactMatch;
    [Test] [Category('Quick')]
    procedure TestExpandName_Embedded;
    [Test] [Category('Quick')]
    procedure TestExpandName_Multiple;
    [Test] [Category('Quick')]
    procedure TestExpandName_NoMatch;
    [Test] [Category('Quick')]
    procedure TestExpandName_ShorterThanTag;

    // LanguageIsCaseSensitive
    [Test] [Category('Quick')]
    procedure TestLanguageIsCaseSensitive;

    // MapCharacters (base + national char conversion)
    [Test] [Category('Quick')]
    procedure TestMapCharacters_NoConversion;
    [Test] [Category('Quick')]
    procedure TestMapCharacters_WithConversion;
    [Test] [Category('Quick')]
    procedure TestMapCharacters_DefaultFlagOn;

    // GetMapCharacter - specific national characters
    [Test] [Category('Quick')]
    procedure TestMapCharacter_AccentedChars;
    [Test] [Category('Quick')]
    procedure TestMapCharacter_UnmappedChar;

    // TruncateName
    [Test] [Category('Quick')]
    procedure TestTruncateName_WithinLimit;
    [Test] [Category('Quick')]
    procedure TestTruncateName_ExceedsLimit;
    [Test] [Category('Quick')]
    procedure TestTruncateName_NoLimit;

    // ValidateName (base)
    [Test] [Category('Quick')]
    procedure TestValidateName_Valid;
    [Test] [Category('Quick')]
    procedure TestValidateName_InvalidChars;
    [Test] [Category('Quick')]
    procedure TestValidateName_TooLong;

    // SQL expander specifics
    [Test] [Category('Quick')]
    procedure TestSQLExpandName_UpperCase;
    [Test] [Category('Quick')]
    procedure TestSQLValidCharacters;

    // Delphi expander specifics
    [Test] [Category('Quick')]
    procedure TestDelphiMapCharacters_LeadingDigit;
    [Test] [Category('Quick')]
    procedure TestDelphiValidateName_Empty;
    [Test] [Category('Quick')]
    procedure TestDelphiValidateName_InvalidFirstChar;
    [Test] [Category('Quick')]
    procedure TestDelphiValidateName_Valid;
    [Test] [Category('Quick')]
    procedure TestDelphiValidCharacters;

    // Expression expander specifics
    [Test] [Category('Quick')]
    procedure TestExpressionMapCharacters_LeadingDigit;
    [Test] [Category('Quick')]
    procedure TestExpressionValidateName_Empty;
    [Test] [Category('Quick')]
    procedure TestExpressionValidateName_InvalidFirstChar;
    [Test] [Category('Quick')]
    procedure TestExpressionValidateName_Valid;
    [Test] [Category('Quick')]
    procedure TestExpressionValidCharacters;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldDefaultTaggedValues,
  BoldTaggedValueSupport,
  BoldNameExpander;

{ TTestBoldNameExpander }

// --- BoldExpandName ---

procedure TTestBoldNameExpander.TestBoldExpandName_SQL;
begin
  var Result := BoldExpandName('Table_<Name>', 'Customer', xtSQL, -1, nccDefault);
  Assert.AreEqual('Table_Customer', Result);
end;

procedure TTestBoldNameExpander.TestBoldExpandName_Delphi;
begin
  var Result := BoldExpandName('T<Name>', 'Order', xtDelphi, -1, nccDefault);
  Assert.AreEqual('TOrder', Result);
end;

procedure TTestBoldNameExpander.TestBoldExpandName_Expression;
begin
  var Result := BoldExpandName('<Name>List', 'Item', xtExpression, -1, nccDefault);
  Assert.AreEqual('ItemList', Result);
end;

// --- BoldExpandPrefix ---

procedure TTestBoldNameExpander.TestBoldExpandPrefix_ReplacesTag;
begin
  var Result := BoldExpandPrefix(TABLEPREFIXTAG + '_XFILES', 'Obj', 'SYS', -1, nccDefault);
  Assert.IsTrue(Pos('SYS', Result) > 0, 'Prefix should be inserted');
  Assert.AreEqual(0, Pos(TABLEPREFIXTAG, Result), 'Tag should be replaced');
end;

procedure TTestBoldNameExpander.TestBoldExpandPrefix_NoTag;
begin
  var Result := BoldExpandPrefix('PlainName', 'Obj', 'SYS', -1, nccDefault);
  Assert.AreEqual('PlainName', Result);
end;

// --- ExpandName ---

procedure TTestBoldNameExpander.TestExpandName_ExactMatch;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.AreEqual('Customer', Exp.ExpandName('<Name>', 'Customer'));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpandName_Embedded;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.AreEqual('TCustomer', Exp.ExpandName('T<Name>', 'Customer'));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpandName_Multiple;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.AreEqual('TFoo_Foo', Exp.ExpandName('T<Name>_<Name>', 'Foo'));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpandName_NoMatch;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.AreEqual('NoTags', Exp.ExpandName('NoTags', 'Replacement'));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpandName_ShorterThanTag;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.AreEqual('AB', Exp.ExpandName('AB', 'Replacement'));
  finally
    Exp.Free;
  end;
end;

// --- LanguageIsCaseSensitive ---

procedure TTestBoldNameExpander.TestLanguageIsCaseSensitive;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.IsFalse(Exp.LanguageIsCaseSensitive);
  finally
    Exp.Free;
  end;
end;

// --- MapCharacters ---

procedure TTestBoldNameExpander.TestMapCharacters_NoConversion;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    // nccFalse: no national char mapping, but invalid chars still replaced with _
    Assert.AreEqual('Hello_World', Exp.MapCharacters('Hello World', nccFalse));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestMapCharacters_WithConversion;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    // nccTrue: national chars are mapped to ASCII equivalents
    Assert.AreEqual('Arlig', Exp.MapCharacters(#$00C5'rlig', nccTrue)); // Å -> A
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestMapCharacters_DefaultFlagOn;
var
  Exp: TBoldDelphiNameExpander;
  SavedFlag: Boolean;
begin
  SavedFlag := BoldNameExpanderMapAnsiCharacters;
  Exp := TBoldDelphiNameExpander.Create;
  try
    BoldNameExpanderMapAnsiCharacters := True;
    Assert.AreEqual('Arlig', Exp.MapCharacters(#$00C5'rlig', nccDefault)); // Å -> A
  finally
    BoldNameExpanderMapAnsiCharacters := SavedFlag;
    Exp.Free;
  end;
end;

// --- GetMapCharacter ---

procedure TTestBoldNameExpander.TestMapCharacter_AccentedChars;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    // Test various national character mappings via MapCharacters with nccTrue
    Assert.AreEqual('a', Exp.MapCharacters(#$00E4, nccTrue)); // ä -> a
    Assert.AreEqual('A', Exp.MapCharacters(#$00C4, nccTrue)); // Ä -> A
    Assert.AreEqual('o', Exp.MapCharacters(#$00F6, nccTrue)); // ö -> o
    Assert.AreEqual('O', Exp.MapCharacters(#$00D6, nccTrue)); // Ö -> O
    Assert.AreEqual('e', Exp.MapCharacters(#$00E9, nccTrue)); // é -> e
    Assert.AreEqual('u', Exp.MapCharacters(#$00FC, nccTrue)); // ü -> u
    Assert.AreEqual('n', Exp.MapCharacters(#$00F1, nccTrue)); // ñ -> n
    Assert.AreEqual('c', Exp.MapCharacters(#$00E7, nccTrue)); // ç -> c
    Assert.AreEqual('y', Exp.MapCharacters(#$00FD, nccTrue)); // ý -> y
    Assert.AreEqual('i', Exp.MapCharacters(#$00EE, nccTrue)); // î -> i
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestMapCharacter_UnmappedChar;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    // Characters not in valid set and not in mapping table become '_'
    Assert.AreEqual('_', Exp.MapCharacters(' ', nccTrue));
  finally
    Exp.Free;
  end;
end;

// --- TruncateName ---

procedure TTestBoldNameExpander.TestTruncateName_WithinLimit;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.AreEqual('Short', Exp.TruncateName('Short', 20));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestTruncateName_ExceedsLimit;
var
  Exp: TBoldDelphiNameExpander;
  LongName, Truncated: string;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    LongName := 'ThisIsAVeryLongIdentifierNameThatExceedsTheLimit';
    Truncated := Exp.TruncateName(LongName, 10);
    Assert.AreEqual(10, Length(Truncated));
    // First 7 chars preserved (10-3=7)
    Assert.AreEqual('ThisIsA', Copy(Truncated, 1, 7));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestTruncateName_NoLimit;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.AreEqual('AnyLength', Exp.TruncateName('AnyLength', -1));
  finally
    Exp.Free;
  end;
end;

// --- ValidateName (base) ---

procedure TTestBoldNameExpander.TestValidateName_Valid;
var
  Exp: TBoldDelphiNameExpander;
  Reason: string;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.IsTrue(Exp.ValidateName('ValidName', Reason, nccFalse, -1));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestValidateName_InvalidChars;
var
  Exp: TBoldDelphiNameExpander;
  Reason: string;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.IsFalse(Exp.ValidateName('Invalid Name', Reason, nccFalse, -1));
    Assert.AreEqual('Name has invalid characters', Reason);
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestValidateName_TooLong;
var
  Exp: TBoldDelphiNameExpander;
  Reason: string;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.IsFalse(Exp.ValidateName('ThisNameIsTooLong', Reason, nccFalse, 5));
    Assert.AreEqual('Name is too long', Reason);
  finally
    Exp.Free;
  end;
end;

// --- SQL expander ---

procedure TTestBoldNameExpander.TestSQLExpandName_UpperCase;
var
  Exp: TBoldSQLNameExpander;
  SavedFlag: Boolean;
begin
  SavedFlag := BoldSQLNameExpanderUpperCaseNames;
  Exp := TBoldSQLNameExpander.Create;
  try
    BoldSQLNameExpanderUpperCaseNames := True;
    Assert.AreEqual('TABLE_CUSTOMER', Exp.ExpandName('Table_<Name>', 'Customer'));
  finally
    BoldSQLNameExpanderUpperCaseNames := SavedFlag;
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestSQLValidCharacters;
var
  Exp: TBoldSQLNameExpander;
  ValidChars: TBoldCharacterSet;
begin
  Exp := TBoldSQLNameExpander.Create;
  try
    ValidChars := Exp.ValidCharacters;
    Assert.IsTrue(AnsiChar('a') in ValidChars);
    Assert.IsTrue(AnsiChar('Z') in ValidChars);
    Assert.IsTrue(AnsiChar('0') in ValidChars);
    Assert.IsTrue(AnsiChar('_') in ValidChars);
    Assert.IsTrue(AnsiChar('"') in ValidChars);
    Assert.IsTrue(AnsiChar('''') in ValidChars);
    Assert.IsFalse(AnsiChar(' ') in ValidChars);
  finally
    Exp.Free;
  end;
end;

// --- Delphi expander ---

procedure TTestBoldNameExpander.TestDelphiMapCharacters_LeadingDigit;
var
  Exp: TBoldDelphiNameExpander;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    // Leading digit is not valid for Delphi, gets mapped
    var Result := Exp.MapCharacters('1abc', nccFalse);
    Assert.AreEqual('_abc', Result);
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestDelphiValidateName_Empty;
var
  Exp: TBoldDelphiNameExpander;
  Reason: string;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.IsFalse(Exp.ValidateName('', Reason, nccFalse, -1));
    Assert.AreEqual('Name can not be empty', Reason);
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestDelphiValidateName_InvalidFirstChar;
var
  Exp: TBoldDelphiNameExpander;
  Reason: string;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    // Leading digit: MapCharacters maps '1' -> '_', so base ValidateName
    // catches it as "invalid chars" before the first-char check runs
    Assert.IsFalse(Exp.ValidateName('1abc', Reason, nccFalse, -1));
    Assert.AreEqual('Name has invalid characters', Reason);
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestDelphiValidateName_Valid;
var
  Exp: TBoldDelphiNameExpander;
  Reason: string;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    Assert.IsTrue(Exp.ValidateName('_ValidName123', Reason, nccFalse, -1));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestDelphiValidCharacters;
var
  Exp: TBoldDelphiNameExpander;
  ValidChars: TBoldCharacterSet;
begin
  Exp := TBoldDelphiNameExpander.Create;
  try
    ValidChars := Exp.ValidCharacters;
    Assert.IsTrue(AnsiChar('a') in ValidChars);
    Assert.IsTrue(AnsiChar('Z') in ValidChars);
    Assert.IsTrue(AnsiChar('_') in ValidChars);
    Assert.IsFalse(AnsiChar(' ') in ValidChars);
    Assert.IsFalse(AnsiChar('"') in ValidChars);
  finally
    Exp.Free;
  end;
end;

// --- Expression expander ---

procedure TTestBoldNameExpander.TestExpressionMapCharacters_LeadingDigit;
var
  Exp: TBoldExpressionNameExpander;
begin
  Exp := TBoldExpressionNameExpander.Create;
  try
    var Result := Exp.MapCharacters('1abc', nccFalse);
    Assert.AreEqual('_abc', Result);
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpressionValidateName_Empty;
var
  Exp: TBoldExpressionNameExpander;
  Reason: string;
begin
  Exp := TBoldExpressionNameExpander.Create;
  try
    Assert.IsFalse(Exp.ValidateName('', Reason, nccFalse, -1));
    Assert.AreEqual('Name can not be empty', Reason);
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpressionValidateName_InvalidFirstChar;
var
  Exp: TBoldExpressionNameExpander;
  Reason: string;
begin
  Exp := TBoldExpressionNameExpander.Create;
  try
    // Leading digit: MapCharacters maps '1' -> '_', so base ValidateName
    // catches it as "invalid chars" before the first-char check runs
    Assert.IsFalse(Exp.ValidateName('1abc', Reason, nccFalse, -1));
    Assert.AreEqual('Name has invalid characters', Reason);
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpressionValidateName_Valid;
var
  Exp: TBoldExpressionNameExpander;
  Reason: string;
begin
  Exp := TBoldExpressionNameExpander.Create;
  try
    Assert.IsTrue(Exp.ValidateName('ValidExpr', Reason, nccFalse, -1));
  finally
    Exp.Free;
  end;
end;

procedure TTestBoldNameExpander.TestExpressionValidCharacters;
var
  Exp: TBoldExpressionNameExpander;
  ValidChars: TBoldCharacterSet;
begin
  Exp := TBoldExpressionNameExpander.Create;
  try
    ValidChars := Exp.ValidCharacters;
    Assert.IsTrue(AnsiChar('a') in ValidChars);
    Assert.IsTrue(AnsiChar('_') in ValidChars);
    Assert.IsFalse(AnsiChar('"') in ValidChars);
  finally
    Exp.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldNameExpander);

end.
