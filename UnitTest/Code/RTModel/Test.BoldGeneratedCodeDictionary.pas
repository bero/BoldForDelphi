unit Test.BoldGeneratedCodeDictionary;

interface

uses
  BoldTestFramework,
  BoldGeneratedCodeDictionary;

type
  {$IFNDEF FPC}
  [Category('RTModel')]
  {$ENDIF}
  TTestBoldGeneratedCodeDictionary = class(TBoldTestCase)
  published
    procedure TestBoldGeneratedCodesAssigned;
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
  BoldRegisterTest(TTestBoldGeneratedCodeDictionary);

end.
