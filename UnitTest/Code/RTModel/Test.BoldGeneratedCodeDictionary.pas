unit Test.BoldGeneratedCodeDictionary;

interface

uses
  DUnitX.TestFramework,
  BoldGeneratedCodeDictionary;

type
  [TestFixture]
  [Category('RTModel')]
  TTestBoldGeneratedCodeDictionary = class
  public
    [Test]
    [Category('Quick')]
    procedure TestBoldGeneratedCodesAssigned;
    [Test]
    [Category('Quick')]
    procedure TestDescriptorByExpressionName;
  end;

implementation

{ TTestBoldGeneratedCodeDictionary }

procedure TTestBoldGeneratedCodeDictionary.TestBoldGeneratedCodesAssigned;
begin
  // BoldGeneratedCodesAssigned returns True if G_BoldGeneratedCodes is assigned
  // The initialization section creates the object, so it should be True
  Assert.IsTrue(BoldGeneratedCodesAssigned, 'BoldGeneratedCodesAssigned should return True after initialization');
end;

procedure TTestBoldGeneratedCodeDictionary.TestDescriptorByExpressionName;
var
  Descriptor: TBoldGeneratedCodeDescriptor;
begin
  // Test DescriptorByExpressionName property - covers lines 121-123
  // Search for a non-existent name should return nil
  Descriptor := GeneratedCodes.DescriptorByExpressionName['NonExistentModel'];
  Assert.IsNull(Descriptor, 'Non-existent model should return nil');

  // If there are registered models, test finding one
  if GeneratedCodes.Count > 0 then
  begin
    Descriptor := GeneratedCodes.ModelEntries[0];
    Assert.IsNotNull(GeneratedCodes.DescriptorByExpressionName[Descriptor.ExpressionName],
      'Should find descriptor by its ExpressionName');
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldGeneratedCodeDictionary);

end.
