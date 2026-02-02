unit Test.BoldURI;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  /// Test fixture for BoldURI - URI and resource name conversion functions
  /// </summary>
  [TestFixture]
  [Category('Common')]
  TTestBoldURI = class
  public
    // ResourceNameToModelName tests - converts plural resource names to singular model names
    [Test]
    [Category('Quick')]
    procedure TestResourceNameToModelName_RemovesS;
    [Test]
    procedure TestResourceNameToModelName_RemovesSES;
    [Test]
    procedure TestResourceNameToModelName_ConvertIEStoY;
    [Test]
    procedure TestResourceNameToModelName_ShortName;
    [Test]
    procedure TestResourceNameToModelName_NoChange;

    // ModelNameToResourceName tests - converts singular model names to plural resource names
    [Test]
    procedure TestModelNameToResourceName_AddsS;
    [Test]
    procedure TestModelNameToResourceName_AddsES;
    [Test]
    procedure TestModelNameToResourceName_ConvertYtoIES;
    [Test]
    procedure TestModelNameToResourceName_EmptyString;
  end;

implementation

uses
  BoldURI;

{ TTestBoldURI }

// ResourceNameToModelName tests

procedure TTestBoldURI.TestResourceNameToModelName_RemovesS;
begin
  // Regular plural: Users -> USER
  Assert.AreEqual('USER', ResourceNameToModelName('Users'));
  Assert.AreEqual('ORDER', ResourceNameToModelName('Orders'));
  Assert.AreEqual('PRODUCT', ResourceNameToModelName('Products'));
end;

procedure TTestBoldURI.TestResourceNameToModelName_RemovesSES;
begin
  // Words ending in SES: removes last 2 chars (the 'ES')
  // Classes -> CLASS (removes 'ES' from 'SES')
  Assert.AreEqual('CLASS', ResourceNameToModelName('Classes'));
  // Addresses -> ADDRESS (removes 'ES')
  Assert.AreEqual('ADDRESS', ResourceNameToModelName('Addresses'));
end;

procedure TTestBoldURI.TestResourceNameToModelName_ConvertIEStoY;
begin
  // Words ending in IES: removes 'IES', adds 'Y'
  Assert.AreEqual('CATEGORY', ResourceNameToModelName('Categories'));
  Assert.AreEqual('COMPANY', ResourceNameToModelName('Companies'));
  Assert.AreEqual('ENTITY', ResourceNameToModelName('Entities'));
end;

procedure TTestBoldURI.TestResourceNameToModelName_ShortName;
begin
  // Short names (1-2 chars)
  Assert.AreEqual('A', ResourceNameToModelName('As'));
  Assert.AreEqual('I', ResourceNameToModelName('Is'));
end;

procedure TTestBoldURI.TestResourceNameToModelName_NoChange;
begin
  // Names not ending in S remain unchanged (uppercased)
  Assert.AreEqual('USER', ResourceNameToModelName('User'));
  Assert.AreEqual('DATA', ResourceNameToModelName('Data'));
end;

// ModelNameToResourceName tests

procedure TTestBoldURI.TestModelNameToResourceName_AddsS;
begin
  // Regular singular: User -> Users
  Assert.AreEqual('Users', ModelNameToResourceName('User'));
  Assert.AreEqual('Orders', ModelNameToResourceName('Order'));
  Assert.AreEqual('Products', ModelNameToResourceName('Product'));
end;

procedure TTestBoldURI.TestModelNameToResourceName_AddsES;
begin
  // Words ending in S: Class -> Classes
  Assert.AreEqual('Classes', ModelNameToResourceName('Class'));
  Assert.AreEqual('Addresses', ModelNameToResourceName('Address'));
end;

procedure TTestBoldURI.TestModelNameToResourceName_ConvertYtoIES;
begin
  // Words ending in Y: removes 'Y', adds 'ies'
  Assert.AreEqual('Categories', ModelNameToResourceName('Category'));
  Assert.AreEqual('Companies', ModelNameToResourceName('Company'));
  Assert.AreEqual('Entities', ModelNameToResourceName('Entity'));
end;

procedure TTestBoldURI.TestModelNameToResourceName_EmptyString;
begin
  // Empty string should return empty (or handle gracefully)
  // The function checks Length > 0, so empty returns nothing
  Assert.AreEqual('', ModelNameToResourceName(''));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldURI);

end.
