unit Test.BoldMemberTypeDictionary;

interface

uses
  BoldTestFramework,
  BoldMemberTypeDictionary;

type
  {$IFNDEF FPC}
  [Category('RTModel')]
  {$ENDIF}
  TTestBoldMemberTypeDictionary = class(TBoldTestCase)
  published
    procedure TestDescriptorsByIndex;
  end;

implementation

{ TTestBoldMemberTypeDictionary }

procedure TTestBoldMemberTypeDictionary.TestDescriptorsByIndex;
var
  Descriptor: TBoldMemberTypeDescriptor;
  i: Integer;
begin
  // BoldMemberTypes is populated during initialization by various Bold units
  Assert.IsTrue(BoldMemberTypes.Count > 0, 'BoldMemberTypes should have registered descriptors');

  // Test Descriptors[Index] property - covers lines 133-135
  for i := 0 to BoldMemberTypes.Count - 1 do
  begin
    Descriptor := BoldMemberTypes.Descriptors[i];
    Assert.IsNotNull(Descriptor);
    Assert.IsNotNull(Descriptor.MemberClass);
  end;
end;

initialization
  BoldRegisterTest(TTestBoldMemberTypeDictionary);

end.
